(** Caravan CLI entry point: REPL, one-shot agent runs, and utilities. *)

open Caravan
open Caravan.Types
open Ui
open Cmdliner

module Registry = CaravanProviders.Registry

(* ── Tool assembly ────────────────────────────────────────────────────── *)

let strict_mode () =
  Config.get_int_opt (Some "CARAVAN_STRICT_MODE") "strict_mode"
  |> Option.value ~default:0

let base_tools () =
  let base = CaravanTools.All_tools.all_tools in
  if strict_mode () = 2 then
    List.filter (fun t -> Tool.name_of_packed t <> "bash") base
  else base

(* The plugin host owns the live tool composition: built-in tools and
   MCP mounts run as plugin fibers (config: [[plugins]], defaulting to
   the classic built-ins + [[mcp.servers]] composition). *)
let host = lazy (
  let h = Plugin_host.create ~builtin_tools:base_tools () in
  Plugin_host.load h;
  h)

let all_tools () = Plugin_host.tools (Lazy.force host)

(** (Re)compose the plugin set from config. Safe to call repeatedly —
    reconciliation only touches entries that changed. *)
let init_plugins () =
  if Lazy.is_val host then Plugin_host.load (Lazy.force host)
  else ignore (Lazy.force host)

(* ── Front-end plumbing: renderer, transcript, permissions ────────────── *)

let render_opts = ref Render.{ streaming = true; quiet = false; verbose = false }
let renderer_installed = ref false

let permission_mode = ref (Config.get_permission_mode ())

(** Install the trace renderer and (if enabled) the JSONL transcript sink.
    Returns the transcript path when one was opened. *)
let setup_frontend ?(quiet = false) ?(verbose = false) () =
  if not !renderer_installed then begin
    Render.install render_opts;
    renderer_installed := true
  end;
  let is_verbose = verbose || Config.get_spinner_verbose () in
  render_opts := Render.{ streaming = Config.get_stream (); quiet; verbose = is_verbose };
  permission_mode := Config.get_permission_mode ();
  if Config.get_transcript_enabled () then
    (try Some (Trace.open_transcript ~dir:(Config.log_dir ()))
     with _ -> None)
  else None

(** Run [f] under the active tool-permission policy. *)
let with_permissions f =
  let is_mutating name =
    match Tool.find_tool (all_tools ()) name with
    | Some t -> Tool.is_mutating_packed t
    | None -> true
  in
  let describe_action name args =
    match Tool.find_tool (all_tools ()) name with
    | Some t -> Tool.describe_action_packed t args
    | None -> Printf.sprintf "Use tool '%s'" name
  in
  Effects.run_with_effects
    ~permission_policy:(Permission.policy_of_mode ~is_mutating ~describe_action !permission_mode)
    f

let on_token token =
  print_ansi (green token);
  flush stdout

(* ── Provider resolution ──────────────────────────────────────────────── *)

let resolve_provider ~provider_name ~model ~base_url =
  Registry.make_provider ?base_url ?model:(Some model) provider_name

(** Fail fast, in plain language, when the provider name is unknown. *)
let resolve_provider_or_exit ~provider_name ~model ~base_url =
  try resolve_provider ~provider_name ~model ~base_url
  with Registry.Unknown_provider msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 2

(** Unified resolution of (provider_name, model, base_url) given optional CLI overrides with README claim *)
let resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli =
  Cli_resolve.resolve
    ~default_model:Registry.default_model
    ~provider_cli ~model_cli ~base_url_cli ()

(** Build a session inside [Eio_main.run]: static + MCP tools, plus the
    config-declared delegate tool when subagents are enabled.

    The system prompt is layered (System_prompt.compose): the shipped
    base + capability-conditioned format guidance + environment
    preamble, with the user's [system] setting appended on top.
    [system_replace = true] hands the user full control instead.  The
    preamble is assembled here — once per session — so the request
    prefix stays byte-stable for prompt caching. *)
let make_session ~net ~clock ~provider_name ~model ~base_url ~system =
  let provider = resolve_provider_or_exit ~provider_name ~model ~base_url in
  Plugin_host.set_provider (Lazy.force host) provider;
  let tools = Subagents.session_tools ~net ~clock ~host:(Lazy.force host) (all_tools ()) in
  let capability = Capability.lookup model in
  (* Tool profile (M1): low-capability models get the core surface —
     every schema is a per-turn context tax, and small models choose
     badly among many options. tool_profile = "full" restores all. *)
  let tools =
    if Capability.use_core_profile ~profile:(Config.get_tool_profile ()) capability
    then begin
      let kept = List.filter
          (fun t -> List.mem (Tool.name_of_packed t) Capability.core_tool_names)
          tools in
      Trace.log "info"
        "Tool profile 'core' active for %s (%d of %d tools exposed; set tool_profile = \"full\" to override)"
        model (List.length kept) (List.length tools);
      kept
    end else tools
  in
  let sess =
    Session.create ~tools model provider
    |> (fun s -> Session.set_context_window s (Some capability.Capability.context_window))
  in
  let replace =
    Config.get_bool_opt (Some "CARAVAN_SYSTEM_REPLACE") "system_replace"
    |> Option.value ~default:false
  in
  match System_prompt.compose ~capability ?user_system:system ~replace () with
  | Some s -> Session.set_system sess s
  | None -> sess

(* ── REPL state ───────────────────────────────────────────────────────── *)

type repl_state = {
  mutable session          : Session.t;
  mutable provider_name    : string;
  mutable model            : string;
  mutable provider         : Provider.packed_provider;
  mutable base_url         : string option;
  mutable total_tokens_in  : int;
  mutable total_tokens_out : int;
  mutable pending_nudge    : string option;
}

type help_group = {
  title : string;
  commands : (string * string * string option) list;
}

let help_groups = [
  { title = "Chat";
    commands = [
      ("/agent <task>", "Let the AI work autonomously on a task",
       Some "Example: /agent summarize the files in this directory");
      ("/nudge <text>", "Queue a steering note for the next model call", None);
      ("/lisp <program>", "Evaluate a Slip expression (the model's calculator)",
       Some "Example: /lisp (mean (list 4 8 15 16 23 42))");
      ("/system [text]", "Set instructions for the AI's personality", None);
      ("/clear", "Start a fresh conversation", None);
    ] };
  { title = "Model and Provider";
    commands = [
      ("/model <name>", "Switch AI model",
       Some "Example: /model claude-sonnet-5");
      ("/provider <p> [url]", "Switch AI provider",
       Some "Example: /provider anthropic");
      ("/models", "Browse available models", None);
      ("/providers", "List supported providers and key status", None);
      ("/subagents", "Show configured subagent workers", None);
    ] };
  { title = "Safety and Tuning";
    commands = [
      ("/permissions <mode>", "Tool policy: auto | ask | readonly", None);
      ("/temp <0.0-2.0>", "Creativity level (higher = more creative)", None);
      ("/memory <n>", "How many messages to remember (0 = unlimited)", None);
      ("/summarise", "Compress conversation to save memory", None);
    ] };
  { title = "Session";
    commands = [
      ("/history", "Show conversation so far", None);
      ("/export [file]", "Save conversation to a file", None);
      ("/resume [file]", "Restore conversation from a checkpoint", None);
      ("/tools", "List available tools for the agent", None);
      ("/mcp [list|add|get|remove]", "Manage MCP server connections and tools",
       Some "Example: /mcp add github -- npx -y @modelcontextprotocol/server-github");
      ("/plugins", "List composed plugins; enable/disable by id", None);
      ("/config", "Show current settings", None);
      ("/config set <k> <v>", "Change a setting, saved to the config file",
       Some "Example: /config set permissions ask   (/config keys lists them)");
      ("/key <provider>", "Store an API key (input hidden, file 0600)", None);
    ] };
  { title = "Exit";
    commands = [
      ("/quit", "Exit Caravan", None);
    ] };
]

let print_help_grouped groups =
  List.iter (fun g ->
    println_ansi (bold (yellow (Printf.sprintf "\n  %s" g.title)));
    List.iter (fun (cmd, desc, ex) ->
      println_ansi (Printf.sprintf "    %s  %s"
        (cyan (Printf.sprintf "%-24s" cmd)) (dim desc));
      match ex with
      | Some e -> println_ansi (Printf.sprintf "      %s" (green e))
      | None -> ()
    ) g.commands
  ) groups;
  print_newline ()

(* ── Interactive input helpers (shared by wizard and slash commands) ──── *)

let read_line_default default =
  match String.trim (try input_line stdin with End_of_file -> "") with
  | "" -> default
  | s -> s

(** Read a secret without echoing it to the terminal. *)
let read_secret prompt =
  print_ansi (cyan prompt);
  flush stdout;
  let read_plain () = try String.trim (input_line stdin) with End_of_file -> "" in
  if not is_tty then read_plain ()
  else begin
    let open Unix in
    try
      let attr = tcgetattr stdin in
      tcsetattr stdin TCSANOW { attr with c_echo = false };
      let s = Fun.protect
          ~finally:(fun () -> tcsetattr stdin TCSANOW attr; print_newline ())
          read_plain
      in s
    with Unix_error _ -> read_plain ()
  end

(* ── Slash command helpers ────────────────────────────────────────────── *)

let usage cmd msg = println_ansi (red (Printf.sprintf "Usage: %s %s" cmd msg))

let confirm fmt = Printf.ksprintf (fun s -> println_ansi (yellow ("  ✓ " ^ s))) fmt

let update_float_opt st cmd name setter min_v max_v = function
  | [v_str] ->
    (match float_of_string_opt v_str with
     | Some v when v >= min_v && v <= max_v ->
       st.session <- Session.set_options st.session (setter v);
       confirm "%s → %.2f" name v
     | _ -> usage cmd (Printf.sprintf "<float %.1f-%.1f>" min_v max_v))
  | _ -> usage cmd (Printf.sprintf "<float %.1f-%.1f>" min_v max_v)

let update_int_opt st cmd name setter = function
  | [v_str] ->
    (match int_of_string_opt v_str with
     | Some v ->
       st.session <- Session.set_options st.session (setter v);
       confirm "%s → %d" name v
     | _ -> usage cmd "<int>")
  | _ -> usage cmd "<int>"

let switch_model st new_model =
  st.model <- new_model;
  let provider =
    resolve_provider_or_exit ~provider_name:st.provider_name
      ~model:new_model ~base_url:st.base_url
  in
  st.provider <- provider;
  Plugin_host.set_provider (Lazy.force host) provider;
  st.session <- Session.with_provider (Session.with_model st.session new_model) provider

let print_providers_table active =
  println_ansi (rule ~title:"Providers" ());
  List.iter (fun (e : Registry.entry) ->
    let mark = if e.name = active then green "●" else dim "○" in
    let key_status =
      if not e.requires_key then dim "local · no key needed"
      else match Registry.api_key_for e with
        | Some _ -> green "key found"
        | None ->
          (match e.key_env with
           | Some ev -> red (Printf.sprintf "key missing (set %s)" ev)
           | None -> red "key missing")
    in
    println_ansi (Printf.sprintf "  %s %s %s %s"
      mark
      (bold (Printf.sprintf "%-11s" e.name))
      (Printf.sprintf "%-24s" (dim (truncate_visible e.default_model 24)))
      key_status)
  ) Registry.entries;
  println_ansi (dim "\n  Switch with /provider <name>, or see 'caravan providers' for details.")

(* ── Slash command handling ───────────────────────────────────────────── *)

let cmd_agent net clock st rest =
    let task = String.concat " " rest |> String.trim in
    if task = "" then usage "/agent" "<task description>"
    else begin
      println_ansi (rule ~title:"Agent" ());
      println_ansi (Printf.sprintf "  %s %s" (dim "task:") (white task));
      let on_turn current max =
        let max_str = if max <= 0 then "∞" else string_of_int max in
        println_ansi (dim (Printf.sprintf "  ── turn %d/%s ──" current max_str))
      in
      let on_step step_sess =
        st.session <- step_sess;
        ignore (Session.save_checkpoint step_sess)
      in
      (try
        let stream_enabled = Config.get_stream () in
        let result =
          with_permissions (fun () ->
            if stream_enabled then
              Agent.run_stream ~on_turn ~on_step net clock st.session task ~on_token
            else
              Agent.run ~on_turn ~on_step net clock st.session task)
        in
        match result with
        | Ok (new_sess, res) ->
          st.session <- new_sess;
          ignore (Session.save_checkpoint new_sess);
          print_newline ();
          println_ansi (rule ~title:"Result" ());
          if String.trim res.value.content <> "" then begin
            println_ansi (render_markdown (String.trim res.value.content));
            print_newline ()
          end;
          println_ansi (dim ("  " ^ Monitor.format_usage res))
        | Error e ->
          ignore (Session.save_checkpoint st.session);
          Trace.error "repl-agent" "%s" e;
          println_ansi (red (Printf.sprintf "  ✗ agent: %s" e))
      with exn ->
        ignore (Session.save_checkpoint st.session);
        Trace.error "repl-agent" "%s" (Caravan_error.humanize exn);
        println_ansi (red (Printf.sprintf "  ✗ %s" (Caravan_error.humanize exn))))
    end

let cmd_lisp rest =
    let src = String.concat " " rest |> String.trim in
    if src = "" then usage "/lisp" "<program>   e.g. /lisp (sum (range 1 101))"
    else
      (match Caravan.Lisp.run_to_string src with
       | Ok out -> println_ansi (green ("  " ^ out))
       | Error e -> println_ansi (red ("  ✗ " ^ e)))

let cmd_nudge st rest =
    let text = String.concat " " rest |> String.trim in
    if text = "" then usage "/nudge" "<steering note>"
    else begin
      st.pending_nudge <- Some text;
      confirm "nudge queued — it will be injected before the next model call"
    end

let cmd_model st rest =
    (match rest with
     | [new_model] ->
       switch_model st new_model;
       confirm "Model → %s" new_model
     | _ -> usage "/model" "<model-name>")

let cmd_provider st rest =
    (match rest with
     | name :: rest ->
       (match Registry.find name with
        | None -> println_ansi (red (Registry.unknown_provider_message name))
        | Some e ->
          let base_url = if rest = [] then None else Some (String.concat "" rest) in
          st.provider_name <- e.name;
          st.base_url <- base_url;
          (* Model likely doesn't carry across providers; reset to default. *)
          let model = Registry.default_model e.name in
          st.model <- model;
          let provider =
            resolve_provider_or_exit ~provider_name:e.name ~model ~base_url in
          st.provider <- provider;
          Plugin_host.set_provider (Lazy.force host) provider;
          st.session <- Session.with_provider (Session.with_model st.session model) provider;
          confirm "Provider → %s (model %s)" e.name model)
     | [] -> usage "/provider" "<name> [url]")

let cmd_permissions rest =
    (match rest with
     | [mode] when List.mem mode ["auto"; "ask"; "readonly"] ->
       permission_mode := mode;
       confirm "Permissions → %s" mode
     | [] ->
       println_ansi (Printf.sprintf "  Permission mode: %s" (bold !permission_mode))
     | _ -> usage "/permissions" "auto | ask | readonly")

let cmd_system st rest =
    let text = String.concat " " rest |> String.trim in
    st.session <- Session.set_system st.session text;
    if text = "" then confirm "System prompt cleared"
    else confirm "System prompt set (%d chars)" (String.length text)

let cmd_memory st rest =
    (match rest with
     | [n_str] ->
       (match int_of_string_opt n_str with
        | Some n ->
          st.session <- Session.set_memory_size st.session n;
          confirm "Memory window → %s" (if n = 0 then "unlimited" else string_of_int n)
        | None -> usage "/memory" "<n>")
     | _ -> usage "/memory" "<n>")

let cmd_summarise net clock st =
    let hist = Session.history st.session in
    if hist = [] then
      println_ansi (yellow "  ⚠ Conversation history is empty; nothing to summarize.")
    else begin
      (try
         let (new_sess, summary) = Session.summarise net clock st.session in
         st.session <- new_sess;
         confirm "Context compacted";
         println_ansi (dim (Printf.sprintf "  %s" summary))
       with exn ->
         Trace.error "summarize" "%s" (Caravan_error.humanize exn);
         println_ansi (red (Printf.sprintf "  ✗ summarize: %s" (Caravan_error.humanize exn))))
    end

let cmd_clear st =
    st.session <- Session.clear st.session;
    confirm "History cleared"

let cmd_history st =
    let hist = Session.history st.session in
    if hist = [] then println_ansi (dim "  (empty history)")
    else
      List.iter (fun msg ->
        let role_str = role_to_string msg.role in
        let colour = match msg.role with
          | System -> yellow | User -> cyan | Assistant -> green | Tool _ -> magenta in
        println_ansi (Printf.sprintf "%s: %s" (bold (colour role_str)) (dim msg.content))
      ) hist

let cmd_export st rest =
    (match rest with
     | [file] ->
       (try
         let oc = open_out file in
         output_string oc (Yojson.Safe.pretty_to_string (Session.export_json st.session));
         close_out oc;
         confirm "Exported to %s" file
       with exn -> println_ansi (red (Printf.sprintf "  ✗ %s" (Caravan_error.humanize exn))))
     | [] -> print_endline (Yojson.Safe.pretty_to_string (Session.export_json st.session))
     | _ -> usage "/export" "[file]")

let cmd_resume st rest =
    let path = match rest with [] -> None | [file] -> Some file | _ -> None in
    (match Session.load_checkpoint ~provider:st.provider ~tools:(Session.tools st.session) ?path () with
     | Ok sess ->
       let turns = List.length (Session.history sess) in
       let resume_note =
         Printf.sprintf
           "[Caravan system note]: Previous task execution was interrupted. \
            The conversation history above contains all completed steps and tool outputs up to turn %d. \
            Please review the previous tool execution history and resume working on the task."
           (Session.turn_idx sess)
       in
       let sess' = Session.add_messages sess [system_msg resume_note] in
       st.session <- sess';
       confirm "Resumed session checkpoint (%d messages, turn %d)" turns (Session.turn_idx sess')
     | Error e -> println_ansi (red (Printf.sprintf "  ✗ %s" e)))

let cmd_models net st =
    (try
      let models = Provider.list_models_packed net st.provider in
      println_ansi (rule ~title:(Printf.sprintf "Models on %s" st.provider_name) ());
      List.iteri (fun i m ->
        let mark = if m = st.model then green " ● " else dim " ○ " in
        let num = cyan (Printf.sprintf "[%d]" (i + 1)) in
        println_ansi (Printf.sprintf "  %s%s%s" num mark (white m))
      ) models;
      if is_tty then begin
        println_ansi (dim "\n  Enter a number to switch, or press Enter to cancel:");
        (try
          let input = String.trim (input_line stdin) in
          if input <> "" then
            match int_of_string_opt input with
            | Some n when n >= 1 && n <= List.length models ->
              let new_model = List.nth models (n - 1) in
              switch_model st new_model;
              confirm "Switched to %s" new_model
            | _ -> println_ansi (red "  Invalid selection.")
        with End_of_file -> ())
      end
    with exn ->
      println_ansi (red ("  " ^ Caravan_error.humanize exn)))

let cmd_subagents st =
    let roster = Subagents.describe () in
    if roster = [] then begin
      println_ansi (dim "  No subagents configured.");
      println_ansi (dim "  Declare [[subagents]] tables in the config to enable the delegate tool —");
      println_ansi (dim "  see the Subagents chapter of the docs and examples/heterogeneous_agent_swarms/.")
    end else begin
      println_ansi (rule ~title:"Subagents" ());
      let installed =
        List.exists (fun t -> Tool.name_of_packed t = "delegate") (Session.tools st.session)
      in
      List.iter (fun ((cfg : Config.subagent_config), provider_status) ->
        let health =
          if String.length provider_status >= 10
             && (String.sub provider_status 0 10 = "UNRESOLVED") then red "✗"
          else if Re.execp (Re.compile (Re.str "unset")) provider_status then yellow "⚠"
          else green "●"
        in
        println_ansi (Printf.sprintf "  %s %s %s %s"
          health
          (bold (Printf.sprintf "%-14s" cfg.name))
          (Printf.sprintf "%-28s" (white (truncate_visible cfg.model 28)))
          (dim provider_status));
        if cfg.tool_names <> [] then
          println_ansi (dim (Printf.sprintf "      tools: %s" (String.concat ", " cfg.tool_names)));
        (match cfg.realm with
         | Some realm ->
           let sandbox = Plugin_host.realm_tools (Lazy.force host) ~realm in
           println_ansi (dim (Printf.sprintf "      realm: %s (%d sandbox tool%s)"
             realm (List.length sandbox)
             (if List.length sandbox = 1 then "" else "s")))
         | None -> ())
      ) roster;
      print_newline ();
      if not (Subagents.enabled ()) then
        println_ansi (yellow "  Disabled by enable_subagents = false — /config set enable_subagents true")
      else if installed then
        println_ansi (dim "  delegate tool is live in this session (governed by /permissions).")
      else
        println_ansi (yellow "  Configured but not loaded in this session — check warnings above/at startup.")
    end

let cmd_tools st =
    let tools = Session.tools st.session in
    if tools = [] then println_ansi (yellow "  No tools registered.")
    else begin
      println_ansi (rule ~title:"Tools" ());
      List.iter (fun p ->
        let name = Tool.name_of_packed p in
        let mut = if Tool.is_mutating_packed p then yellow "✎" else dim "·" in
        println_ansi (Printf.sprintf "  %s %s  %s"
          mut (cyan (Printf.sprintf "%-14s" name))
          (dim (truncate_visible (Tool.description_of_packed p) 60)))
      ) tools;
      println_ansi (dim "\n  ✎ = can modify state (governed by /permissions)")
    end

let cmd_plugins net clock st rest =
    let h = Lazy.force host in
    (match rest with
     | [] ->
       let entries = Plugin_host.entries h in
       if entries = [] then println_ansi (dim "  No plugins composed.")
       else begin
         println_ansi (rule ~title:"Plugins" ());
         List.iter (fun (e : Config.plugin_config) ->
           let fiber = Plugin_host.fiber h e.id in
           let state = match fiber with
             | Some f ->
               Format.asprintf "%a" Plugin.Fiber.pp_state (Plugin.Fiber.state f)
             | None -> if e.enabled then "not instantiated" else "disabled"
           in
           let mark = match fiber with
             | Some f when Plugin.Fiber.state f = Plugin.Fiber.Active -> green " ● "
             | Some f when Plugin.Fiber.state f = Plugin.Fiber.Failed -> red " ✗ "
             | _ -> dim " ○ "
           in
           println_ansi (Printf.sprintf "  %s%s %s"
             mark (cyan (Printf.sprintf "%-18s" e.id))
             (dim (Printf.sprintf "(%s · %s)" e.plugin state)));
           (match Option.bind fiber Plugin.Fiber.error with
            | Some exn ->
              println_ansi (red (Printf.sprintf "        %s" (Caravan_error.humanize exn)))
            | None -> ())
         ) entries;
         println_ansi
           (dim "\n  /plugins enable|disable <id> (session-only) · declared via [[plugins]] in config")
       end
     | [action; id] when action = "enable" || action = "disable" ->
       (match Plugin_host.set_enabled h ~id (action = "enable") with
        | Ok () ->
          st.session <-
            Session.with_tools st.session
              (Subagents.session_tools ~net ~clock ~host:h (all_tools ()));
          confirm "plugin '%s' %sd" id action
        | Error e -> println_ansi (red ("  ✗ " ^ e)))
     | _ -> usage "/plugins" "[enable|disable <id>]")

let cmd_mcp net clock st rest =
    let h = Lazy.force host in
    (match rest with
     | [] | ["list"] ->
       let mcp_servers = Config.get_mcp_servers () in
       if mcp_servers = [] then println_ansi (dim "  No MCP servers configured.")
       else begin
         println_ansi (rule ~title:"MCP Servers" ());
         List.iter (fun (cfg : Config.mcp_server_config) ->
           let id = "mcp:" ^ cfg.name in
           let fiber = Plugin_host.fiber h id in
           let status_mark, status_str = match fiber with
             | Some f when Plugin.Fiber.state f = Plugin.Fiber.Active -> (green "●", "active")
             | Some f when Plugin.Fiber.state f = Plugin.Fiber.Failed -> (red "✗", "failed")
             | _ -> (yellow "○", "inactive")
           in
           println_ansi (Printf.sprintf "  %s %s  %s  %s %s (%s)"
             status_mark
             (bold (Printf.sprintf "%-14s" cfg.name))
             (cyan (Printf.sprintf "%-6s" cfg.transport))
             (white cfg.command)
             (dim (String.concat " " cfg.args))
             (dim status_str))
         ) mcp_servers;
         println_ansi (dim "\n  /mcp get <name> · /mcp add <name> -- <cmd> · /mcp remove <name>")
       end
     | ["get"; name] ->
       (match Config.get_mcp_server name with
        | None -> println_ansi (red (Printf.sprintf "  ✗ MCP server '%s' not found." name))
        | Some cfg ->
          println_ansi (rule ~title:(Printf.sprintf "MCP Server: %s" cfg.name) ());
          println_ansi (kv_line "Name" cfg.name);
          println_ansi (kv_line "Transport" cfg.transport);
          println_ansi (kv_line "Command" (cfg.command ^ " " ^ String.concat " " cfg.args));
          let prefix = cfg.name ^ "_" in
          let mcp_tools = List.filter (fun t ->
            let n = Tool.name_of_packed t in
            String.length n > String.length prefix && String.sub n 0 (String.length prefix) = prefix
          ) (Session.tools st.session) in
          println_ansi (kv_line "Tools" (string_of_int (List.length mcp_tools)));
          List.iter (fun t ->
            println_ansi (Printf.sprintf "    %s  %s"
              (cyan (Tool.name_of_packed t))
              (dim (truncate_visible (Tool.description_of_packed t) 60)))
          ) mcp_tools)
     | "add" :: name :: rest_args ->
       let (command, args) =
         match rest_args with
         | "--" :: cmd :: a -> (cmd, a)
         | cmd :: a -> (cmd, a)
         | [] -> ("", [])
       in
       if command = "" then usage "/mcp add" "<name> [--transport stdio] -- <command> [args...]"
       else begin
         println_ansi (dim (Printf.sprintf "  Probing MCP server '%s'..." name));
         match Mcp.probe_server name command args with
         | Error err -> println_ansi (red (Printf.sprintf "  ✗ Probe failed: %s" err))
         | Ok (client, tools) ->
           (try client.close () with _ -> ());
           let cfg = { Config.name; transport = "stdio"; command; args } in
           (match Config.add_mcp_server cfg with
            | Ok path ->
              Plugin_host.load h;
              st.session <- Session.with_tools st.session (Subagents.session_tools ~net ~clock ~host:h (all_tools ()));
              confirm "MCP server '%s' added (%d tools registered, saved to %s)" name (List.length tools) path
            | Error e -> println_ansi (red ("  ✗ " ^ e)))
       end
     | ["remove"; name] | ["rm"; name] ->
       (match Config.delete_mcp_server name with
        | Ok path ->
          Plugin_host.load h;
          st.session <- Session.with_tools st.session (Subagents.session_tools ~net ~clock ~host:h (all_tools ()));
          confirm "MCP server '%s' removed (saved to %s)" name path
        | Error e -> println_ansi (red ("  ✗ " ^ e)))
     | _ -> usage "/mcp" "[list | get <name> | add <name> -- <cmd> [args...] | remove <name>]")

let cmd_config_set st key rest =
    let value = String.concat " " rest in
    (match Config.set_value key value with
     | Ok path ->
       confirm "%s = %s  (saved to %s)" key value path;
       (match key with
        | "provider" | "model" | "base_url" ->
          println_ansi (dim "  Applies to new sessions — use /provider or /model to switch live.")
        | "permissions" ->
          permission_mode := Config.get_permission_mode ()
        | "verbose" | "spinner.verbose" ->
          render_opts := { !render_opts with Render.verbose = Config.get_spinner_verbose () }
        | _ -> ())
     | Error e -> println_ansi (red (Printf.sprintf "  ✗ %s" e)))

let cmd_config_get key =
    (match Config.get_string key with
     | Some v -> println_ansi (kv_line key (white v))
     | None ->
       match Config.get_int key with
       | Some v -> println_ansi (kv_line key (white (string_of_int v)))
       | None ->
         match Config.get_bool key with
         | Some b -> println_ansi (kv_line key (white (string_of_bool b)))
         | None -> println_ansi (yellow (Printf.sprintf "  '%s' is not set" key)))

let cmd_config_keys () =
    println_ansi (rule ~title:"Editable keys" ());
    List.iter (fun (k, desc, accepts) ->
      println_ansi (Printf.sprintf "  %s %s %s"
        (cyan (Printf.sprintf "%-17s" k))
        (Printf.sprintf "%-42s" (dim desc))
        (dim accepts))
    ) Config.editable_keys;
    println_ansi (dim "\n  /config set <key> <value>   ·   /key <provider> to store an API key")

let cmd_key rest =
    (match rest with
     | [name] ->
       (match Registry.find name with
        | None -> println_ansi (red (Registry.unknown_provider_message name))
        | Some e when not e.requires_key ->
          println_ansi (yellow (Printf.sprintf "  %s is a local provider — no API key needed." e.name))
        | Some e ->
          let key = read_secret (Printf.sprintf "  Paste the %s API key (input hidden): " e.name) in
          if key = "" then println_ansi (yellow "  Nothing entered — key unchanged.")
          else
            (match Config.set_api_key e.name key with
             | Ok path -> confirm "API key for %s stored in %s (0600)" e.name path
             | Error err -> println_ansi (red (Printf.sprintf "  ✗ %s" err))))
     | _ -> usage "/key" "<provider>   (stores the key under [api_keys], input hidden)")

let cmd_config_show st =
    let cfg = Session.config st.session in
    let opts = cfg.options in
    println_ansi (rule ~title:"Configuration" ());
    let p k v = println_ansi (kv_line k (white v)) in
    p "Provider" st.provider_name;
    p "Model" st.model;
    p "URL" (Option.value ~default:"(default)" st.base_url);
    p "Memory" (string_of_int cfg.memory_size);
    p "Permissions" !permission_mode;
    p "System" (match cfg.system with
      | Some s -> Printf.sprintf "%s…" (truncate_visible s 40)
      | None -> "(none)");
    p "Streaming" (string_of_bool (Config.get_stream ()));
    p "Verbose" (string_of_bool (Config.get_spinner_verbose ()));
    p "Transcript" (if Config.get_transcript_enabled ()
                    then Config.log_dir () else "disabled");
    println_ansi (bold (dim "  Generation options:"));
    let po n = function
      | Some v -> println_ansi (kv_line ~key_width:12 ("  " ^ n) (white v))
      | None -> ()
    in
    po "Temp" (Option.map (Printf.sprintf "%.2f") opts.temperature);
    po "Top P" (Option.map (Printf.sprintf "%.2f") opts.top_p);
    po "Top K" (Option.map string_of_int opts.top_k);
    po "Max Tokens" (Option.map string_of_int opts.max_tokens);
    po "Seed" (Option.map string_of_int opts.seed);
    if opts.stop <> [] then
      println_ansi (kv_line ~key_width:12 "  Stop" (white (String.concat ", " opts.stop)))

let cmd_stop st rest =
    if rest = [] then begin
      st.session <- Session.set_options st.session (fun o -> { o with stop = [] });
      confirm "Stop sequences cleared"
    end else begin
      st.session <- Session.set_options st.session (fun o -> { o with stop = rest });
      confirm "Stop sequences → %s" (String.concat ", " rest)
    end


let handle_slash_command net clock st line =
  let parts = String.split_on_char ' ' (String.trim line) |> List.filter (fun s -> s <> "") in
  match parts with
  | [] -> ()

  | ["/quit"] | ["/exit"] | ["/q"] ->
    println_ansi (dim "\nGoodbye.");
    exit 0

  | ["/help"] | ["/?"] ->
    print_help_grouped help_groups

  | "/agent" :: rest -> cmd_agent net clock st rest

  | "/lisp" :: rest -> cmd_lisp rest

  | "/nudge" :: rest -> cmd_nudge st rest

  | "/model" :: rest -> cmd_model st rest

  | "/provider" :: rest -> cmd_provider st rest

  | "/permissions" :: rest -> cmd_permissions rest

  | "/system" :: rest -> cmd_system st rest

  | "/memory" :: rest -> cmd_memory st rest

  | ["/summarise"] | ["/summarize"] -> cmd_summarise net clock st

  | ["/clear"] -> cmd_clear st

  | ["/history"] -> cmd_history st

  | "/export" :: rest -> cmd_export st rest

  | "/resume" :: rest -> cmd_resume st rest

  | ["/models"] -> cmd_models net st

  | ["/providers"] ->
    print_providers_table st.provider_name

  | ["/subagents"] -> cmd_subagents st

  | ["/tools"] -> cmd_tools st

  | "/plugins" :: rest -> cmd_plugins net clock st rest

  | "/mcp" :: rest -> cmd_mcp net clock st rest

  | "/config" :: "set" :: key :: rest when rest <> [] -> cmd_config_set st key rest

  | ["/config"; "get"; key] -> cmd_config_get key

  | ["/config"; "keys"] -> cmd_config_keys ()

  | "/key" :: rest -> cmd_key rest

  | ["/config"] -> cmd_config_show st

  | "/temp"       :: rest -> update_float_opt st "/temp" "Temperature" (fun v o -> { o with temperature = Some v }) 0.0 2.0 rest
  | "/top_p"      :: rest -> update_float_opt st "/top_p" "Top P" (fun v o -> { o with top_p = Some v }) 0.0 1.0 rest
  | "/top_k"      :: rest -> update_int_opt st "/top_k" "Top K" (fun v o -> { o with top_k = Some v }) rest
  | "/max_tokens" :: rest -> update_int_opt st "/max_tokens" "Max Tokens" (fun v o -> { o with max_tokens = Some v }) rest
  | "/seed"       :: rest -> update_int_opt st "/seed" "Seed" (fun v o -> { o with seed = Some v }) rest

  | "/stop" :: rest -> cmd_stop st rest

  (* Pre-run commands, reachable from inside the REPL too — one command
     surface instead of two. They run as subprocesses so their own event
     loops don't nest inside ours. *)
  | ["/doctor"] ->
    ignore (Sys.command (Filename.quote Sys.executable_name ^ " doctor"))

  | ["/init"] ->
    ignore (Sys.command (Filename.quote Sys.executable_name ^ " init"));
    Config.reload ();
    println_ansi (dim "  Config reloaded — /provider or /model to apply changes live.")

  | ["/web"] ->
    println_ansi (yellow "  The web UI blocks a terminal, so run it in another one:");
    println_ansi (cyan "    caravan web    ")

  | cmd :: _ ->
    if String.length cmd > 0 && cmd.[0] = '/' then
      println_ansi (red (Printf.sprintf "  Unknown command: %s  (try /help)" cmd))
    else ()

(* ── REPL loop ────────────────────────────────────────────────────────── *)

(** Every REPL command, for the live completion palette (and /help). *)
let palette : Editor.command_info list =
  let c name args doc = Editor.{ name; args; doc } in
  [ c "/agent" "<task>" "run the agent autonomously on a task";
    c "/nudge" "<text>" "queue a steering note for the next model call";
    c "/lisp" "<program>" "evaluate a Slip expression, e.g. (sum (range 1 11))";
    c "/system" "[text]" "set (or clear) the system prompt";
    c "/clear" "" "start a fresh conversation";
    c "/model" "<name>" "switch model";
    c "/models" "" "browse models on this provider";
    c "/provider" "<name> [url]" "switch provider";
    c "/providers" "" "provider table with key status";
    c "/subagents" "" "configured subagent workers";
    c "/permissions" "[mode]" "tool policy: auto | ask | readonly";
    c "/temp" "<0.0-2.0>" "sampling temperature";
    c "/top_p" "<0.0-1.0>" "nucleus sampling";
    c "/top_k" "<n>" "top-k sampling";
    c "/max_tokens" "<n>" "response token cap";
    c "/seed" "<n>" "sampling seed";
    c "/stop" "[seq …]" "stop sequences (empty clears)";
    c "/memory" "<n>" "context window in messages (0 = unlimited)";
    c "/summarise" "" "compact the conversation now";
    c "/history" "" "show the conversation so far";
    c "/export" "[file]" "save the conversation as JSON";
    c "/resume" "[file]" "restore conversation from a checkpoint";
    c "/tools" "" "available tools (✎ = mutating)";
    c "/mcp" "[list|add|get|remove]" "manage MCP tool servers";
    c "/plugins" "[enable|disable <id>]" "plugin composition and lifecycle states";
    c "/config" "[set k v | get k | keys]" "show or edit settings";
    c "/key" "<provider>" "store an API key (hidden input)";
    c "/doctor" "" "run diagnostics";
    c "/init" "" "re-run the setup wizard";
    c "/web" "" "how to launch the web UI";
    c "/help" "" "all commands, grouped";
    c "/quit" "" "exit Caravan";
  ]

let repl net clock st =
  let status_line () =
    if is_tty then begin
      let turns = List.length (Session.history st.session) in
      let status = render_status_bar
        ~provider:st.provider_name
        ~model:st.model
        ~turns
        ~tokens_in:st.total_tokens_in
        ~tokens_out:st.total_tokens_out
      in
      println_ansi (Printf.sprintf "\n%s" status)
    end;
    flush stdout
  in
  let prompt_str = Printf.sprintf "%s " (bold (cyan "❯")) in
  let rec loop () =
    status_line ();
    let line_opt = Editor.read_line ~prompt:prompt_str ~commands:palette () in
    let line = match line_opt with
      | Some l -> String.trim l
      | None -> "/quit"
    in
    if line = "" then loop ()
    else if String.length line > 0 && line.[0] = '/' then begin
      handle_slash_command net clock st line;
      loop ()
    end else begin
      (* Inject any queued nudge as a system note ahead of the user turn. *)
      (match st.pending_nudge with
       | Some n ->
         st.session <- Session.add_messages st.session
           [system_msg ("[User steering note]: " ^ n)];
         st.pending_nudge <- None
       | None -> ());
      (try
        let stream_enabled = Config.get_stream () in
        let (new_sess, result) =
          with_permissions (fun () ->
            if stream_enabled then
              Session.turn_stream net clock st.session line ~on_token
            else
              Session.turn net clock st.session line)
        in
        st.session <- new_sess;
        ignore (Session.save_checkpoint new_sess);
        (match result.usage with
         | Some u ->
           st.total_tokens_in  <- st.total_tokens_in + u.prompt_tokens;
           st.total_tokens_out <- st.total_tokens_out + u.completion_tokens
         | None -> ());
        if not stream_enabled then begin
          if is_tty then
            println_ansi (Printf.sprintf "\n%s" (bold (green "Assistant:")));
          println_ansi (render_markdown result.value.content)
        end;
        if is_tty then begin
          print_newline ();
          println_ansi (dim ("  " ^ Monitor.format_usage result))
        end;
        if not is_tty && stream_enabled then print_newline ()
      with exn ->
        ignore (Session.save_checkpoint st.session);
        if is_tty then print_newline ();
        Trace.error "repl" "%s" (Caravan_error.humanize exn);
        println_ansi (red (Printf.sprintf "\n  ✗ %s" (Caravan_error.humanize exn))));
      loop ()
    end
  in
  loop ()

(* ── CLI arguments ────────────────────────────────────────────────────── *)

let provider_arg =
  let doc = "Provider to use (defaults to config provider or ollama). See $(b,caravan providers) for the list." in
  Arg.(value & opt (some string) None & info ["p"; "provider"] ~docv:"PROVIDER" ~doc)

let model_arg =
  let doc = "Model name (defaults to config model or provider default)." in
  Arg.(value & opt (some string) None & info ["m"; "model"] ~docv:"MODEL" ~doc)

let base_url_arg =
  let doc = "Base URL override for the provider API." in
  Arg.(value & opt (some string) None & info ["base-url"] ~docv:"URL" ~doc)

let system_arg =
  let doc = "System prompt for the session or completion." in
  let default = Config.get_string "system" in
  Arg.(value & opt (some string) default & info ["s"; "system"] ~docv:"PROMPT" ~doc)

let verbose_arg =
  let doc = "Show verbose tool call details and trace output." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

(* ── repl command ─────────────────────────────────────────────────────── *)

let run_repl model_cli provider_cli base_url_cli system verbose =
  init_plugins ();
  let transcript = setup_frontend ~verbose () in
  let (provider_name, model, base_url) =
    resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli
  in
  if is_tty && Config.is_first_run () then
    println_ansi (yellow "  First time here? Run 'caravan init' for guided setup.\n");
  Eio_main.run (fun env ->
    Effects.with_net env#net @@ fun () ->
    let provider = resolve_provider_or_exit ~provider_name ~model ~base_url in
    let sess = make_session ~net:env#net ~clock:env#clock
        ~provider_name ~model ~base_url ~system in
    let st = {
      session          = sess;
      provider_name;
      model;
      provider;
      base_url;
      total_tokens_in  = 0;
      total_tokens_out = 0;
      pending_nudge    = None;
    } in
    print_banner ();
    if is_tty then begin
      println_ansi (dim "  Type a message to chat, " ^ cyan "/help" ^
                    dim " for commands, " ^ cyan "/agent <task>" ^
                    dim " for autonomy.");
      println_ansi (Printf.sprintf "  %s %s %s %s"
        (dim "Model") (bold (white model)) (dim "on") (bold (white provider_name)));
      (match transcript with
       | Some path -> println_ansi (dim (Printf.sprintf "  Transcript: %s" path))
       | None -> ());
      print_newline ()
    end;
    repl env#net env#clock st
  )

let repl_cmd =
  let doc = "Start an interactive chat session (default command)." in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(const run_repl $ model_arg $ provider_arg $ base_url_arg $ system_arg $ verbose_arg)

(* ── agent command (one-shot autonomy) ────────────────────────────────── *)

let run_agent model_cli provider_cli base_url_cli system max_turns quiet json_out verbose task =
  init_plugins ();
  let quiet = quiet || json_out in
  let transcript = setup_frontend ~quiet ~verbose () in
  let (provider_name, model, base_url) =
    resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli
  in
  let exit_code = ref 0 in
  Eio_main.run (fun env ->
    Effects.with_net env#net @@ fun () ->
    let sess = make_session ~net:env#net ~clock:env#clock
        ~provider_name ~model ~base_url ~system in
    let config =
      let base = Agent.default_config () in
      { base with Agent.max_turns = Option.value ~default:base.Agent.max_turns max_turns }
    in
    let on_turn current max =
      if not quiet && is_tty then
        let max_str = if max <= 0 then "∞" else string_of_int max in
        println_ansi (dim (Printf.sprintf "  ── turn %d/%s ──" current max_str))
    in
    let stream_enabled = Config.get_stream () && not quiet && not json_out in
    let result =
      with_permissions (fun () ->
        if stream_enabled then
          Agent.run_stream ~config ~on_turn env#net env#clock sess task ~on_token
        else
          Agent.run ~config ~on_turn env#net env#clock sess task)
    in
    let mode = if json_out then Agent_output.Json else Agent_output.Plain in
    match result with
    | Ok (_sess, res) ->
      if json_out then
        print_endline (Agent_output.format_success ~mode ~result:res ~transcript)
      else begin
        if not stream_enabled then begin
          print_newline ();
          println_ansi (render_markdown (Agent_output.format_success ~mode ~result:res ~transcript))
        end else print_newline ();
        if not quiet && is_tty then
          println_ansi (dim ("  " ^ Monitor.format_usage res))
      end
    | Error e ->
      exit_code := 1;
      Trace.error "agent" "%s" e;
      let msg = Agent_output.format_error ~mode ~message:e ~transcript in
      if json_out then
        print_endline msg
      else
        Printf.eprintf "%s\n%!" msg
  );
  exit !exit_code

let agent_cmd =
  let task_arg =
    let doc = "The task for the agent to accomplish." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  let max_turns_arg =
    let doc = "Turn budget for this run (overrides config max_turns)." in
    Arg.(value & opt (some int) None & info ["max-turns"] ~docv:"N" ~doc)
  in
  let quiet_arg =
    let doc = "Suppress progress output; print only the final result." in
    Arg.(value & flag & info ["q"; "quiet"] ~doc)
  in
  let json_arg =
    let doc = "Emit the outcome as a single JSON object (implies --quiet)." in
    Arg.(value & flag & info ["json"] ~doc)
  in
  let doc = "Run an autonomous agent on a task and exit (scripting-friendly)." in
  let man = [
    `S Manpage.s_description;
    `P "Runs the full agentic loop (tools, turn budget, nudges) without \
        the interactive REPL. Exit status is 0 on completion, 1 when the \
        agent fails or exhausts its turn budget.";
    `P "Combine with $(b,--json) in pipelines: the result, token usage and \
        transcript path arrive as one JSON object on stdout.";
  ] in
  let info = Cmd.info "agent" ~doc ~man in
  Cmd.v info Term.(const run_agent $ model_arg $ provider_arg $ base_url_arg
                   $ system_arg $ max_turns_arg $ quiet_arg $ json_arg $ verbose_arg $ task_arg)

(* Alias: caravan run "<task>" *)
let run_cmd =
  let task_arg =
    let doc = "The task for the agent to accomplish." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TASK" ~doc)
  in
  let max_turns_arg =
    Arg.(value & opt (some int) None & info ["max-turns"] ~docv:"N"
           ~doc:"Turn budget for this run.")
  in
  let quiet_arg = Arg.(value & flag & info ["q"; "quiet"] ~doc:"Suppress progress output.") in
  let json_arg = Arg.(value & flag & info ["json"] ~doc:"Emit JSON outcome.") in
  let info = Cmd.info "run" ~doc:"Alias of $(b,caravan agent)." in
  Cmd.v info Term.(const run_agent $ model_arg $ provider_arg $ base_url_arg
                   $ system_arg $ max_turns_arg $ quiet_arg $ json_arg $ verbose_arg $ task_arg)

(* ── complete command ─────────────────────────────────────────────────── *)

let run_complete model_cli provider_cli base_url_cli system verbose prompt_text =
  init_plugins ();
  let _transcript = setup_frontend ~verbose () in
  let (provider_name, model, base_url) =
    resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli
  in
  Eio_main.run (fun env ->
    Effects.with_net env#net @@ fun () ->
    let sess = make_session ~net:env#net ~clock:env#clock
        ~provider_name ~model ~base_url ~system in
    (try
      let stream_enabled = Config.get_stream () in
      let (_sess, result) =
        with_permissions (fun () ->
          if stream_enabled then
            Session.turn_stream env#net env#clock sess prompt_text ~on_token
          else
            Session.turn env#net env#clock sess prompt_text)
      in
      if not stream_enabled then
        print_ansi (green result.value.content);
      print_newline ();
      if is_tty then println_ansi (dim ("  " ^ Monitor.format_usage result))
    with exn ->
      Trace.error "complete" "%s" (Caravan_error.humanize exn);
      Printf.eprintf "[Caravan] Error: %s\n%!" (Caravan_error.humanize exn);
      exit 1)
  )

let complete_cmd =
  let prompt_arg =
    let doc = "The prompt text to send." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROMPT" ~doc)
  in
  let doc = "Send a single prompt and print the response." in
  let info = Cmd.info "complete" ~doc in
  Cmd.v info Term.(const run_complete $ model_arg $ provider_arg $ base_url_arg
                   $ system_arg $ verbose_arg $ prompt_arg)

(* ── models command ───────────────────────────────────────────────────── *)

let run_models model_cli provider_cli base_url_cli =
  let (provider_name, model, base_url) =
    resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli
  in
  Eio_main.run (fun env ->
    let provider = resolve_provider_or_exit ~provider_name ~model ~base_url in
    (try
      let models = Provider.list_models_packed env#net provider in
      List.iter (fun m ->
        print_endline (if m = model then "* " ^ m else "  " ^ m)
      ) models
    with exn ->
      Printf.eprintf "[Caravan] Error: %s\n%!" (Caravan_error.humanize exn);
      exit 1)
  )

let models_cmd =
  let doc = "List models available on the chosen provider." in
  let info = Cmd.info "models" ~doc in
  Cmd.v info Term.(const run_models $ model_arg $ provider_arg $ base_url_arg)

(* ── providers command ────────────────────────────────────────────────── *)

let run_providers ladder =
  if ladder then begin
    println_ansi (bold "  Model ladder — a default for every weight class:\n");
    List.iter (fun (cls, prov, model, note) ->
      println_ansi (Printf.sprintf "  %s %s %s"
        (yellow (Printf.sprintf "%-13s" cls))
        (Printf.sprintf "%-36s" (cyan (Printf.sprintf "%s/%s" prov model)))
        (dim note))
    ) Registry.model_ladder;
    println_ansi (dim "\n  Try one: caravan -p <provider> -m <model>")
  end else begin
    println_ansi (bold "  Supported providers:\n");
    List.iter (fun (e : Registry.entry) ->
      let key_status =
        if not e.requires_key then green "ready (local)"
        else match Registry.api_key_for e with
          | Some _ -> green "key found"
          | None ->
            (match e.key_env with
             | Some ev -> yellow (Printf.sprintf "set %s" ev)
             | None -> yellow "key missing")
      in
      println_ansi (Printf.sprintf "  %s %s %s"
        (bold (Printf.sprintf "%-11s" e.name))
        (Printf.sprintf "%-22s" key_status)
        (dim e.notes));
      println_ansi (Printf.sprintf "    %s %s  %s %s"
        (dim "url:") (dim e.base_url)
        (dim "default:") (dim e.default_model))
    ) Registry.entries;
    println_ansi (dim "\n  caravan providers --ladder  shows a curated model per weight class.")
  end

let providers_cmd =
  let ladder_arg =
    let doc = "Show a curated model ladder from ~1B local weights to frontier." in
    Arg.(value & flag & info ["ladder"] ~doc)
  in
  let doc = "List supported providers, their endpoints, and key status." in
  let info = Cmd.info "providers" ~doc in
  Cmd.v info Term.(const run_providers $ ladder_arg)

(* ── init command (setup wizard) ──────────────────────────────────────── *)

let toml_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let run_init () =
  print_banner ();
  println_ansi (bold "  Let's get you set up.\n");
  println_ansi (bold (yellow "  Pick a provider:"));
  List.iteri (fun i (e : Registry.entry) ->
    let kind = match e.kind with
      | Registry.Local -> dim "local"
      | Registry.Cloud -> dim "cloud"
    in
    let key_note =
      if not e.requires_key then ""
      else match Registry.api_key_for e with
        | Some _ -> green " (key already available)"
        | None -> ""
    in
    println_ansi (Printf.sprintf "  %s %s %s %s%s"
      (cyan (Printf.sprintf "[%2d]" (i + 1)))
      (bold (Printf.sprintf "%-11s" e.name))
      kind (dim ("— " ^ e.notes)) key_note)
  ) Registry.entries;
  print_ansi (cyan "\n  Select [1-12] (default 1 · ollama): ");
  flush stdout;
  let choice =
    match int_of_string_opt (read_line_default "1") with
    | Some n when n >= 1 && n <= List.length Registry.entries ->
      List.nth Registry.entries (n - 1)
    | _ -> List.hd Registry.entries
  in
  (* Base URL: offer override for local providers. *)
  let base_url =
    match choice.kind with
    | Registry.Local ->
      print_ansi (cyan (Printf.sprintf "  Endpoint URL (default %s): " choice.base_url));
      flush stdout;
      let url = read_line_default choice.base_url in
      if url = choice.base_url then None else Some url
    | Registry.Cloud -> None
  in
  (* API key: env wins; otherwise offer to store one (0600 config). *)
  let api_key_to_store =
    if not choice.requires_key then None
    else match choice.key_env with
      | Some ev when Sys.getenv_opt ev <> None && Sys.getenv_opt ev <> Some "" ->
        println_ansi (green (Printf.sprintf "  ✓ Using %s from your environment (not stored)." ev));
        None
      | _ ->
        let key = read_secret (Printf.sprintf "  Paste your %s API key (input hidden): " choice.name) in
        if key = "" then begin
          println_ansi (yellow (Printf.sprintf
            "  ⚠ No key entered. Set %s before using this provider."
            (Option.value ~default:"the provider's API key" choice.key_env)));
          None
        end else Some key
  in
  (* Model selection: probe local providers, else registry default. *)
  let model =
    match choice.name with
    | "ollama" ->
      let selected = ref choice.default_model in
      Eio_main.run (fun env ->
        try
          let url = Option.value ~default:choice.base_url base_url in
          let provider = Registry.make_provider ~base_url:url ~model:choice.default_model "ollama" in
          let models = Provider.list_models_packed env#net provider in
          if models <> [] then begin
            println_ansi (green "\n  Connected to Ollama. Local models:");
            List.iteri (fun i m ->
              println_ansi (Printf.sprintf "  %s %s"
                (cyan (Printf.sprintf "[%d]" (i + 1))) (white m))
            ) models;
            print_ansi (cyan (Printf.sprintf "  Select model [1-%d] (default 1): " (List.length models)));
            flush stdout;
            (match int_of_string_opt (read_line_default "1") with
             | Some n when n >= 1 && n <= List.length models ->
               selected := List.nth models (n - 1)
             | _ -> selected := List.hd models)
          end
        with _ ->
          println_ansi (yellow "\n  ⚠ Could not reach Ollama at its default port.");
          println_ansi (dim "    Install & start it first: https://ollama.com  (ollama serve)"));
      !selected
    | _ ->
      print_ansi (cyan (Printf.sprintf "  Model (default %s): " choice.default_model));
      flush stdout;
      read_line_default choice.default_model
  in
  (* Write config, private to the user. *)
  let path = Config.config_path () in
  let config_dir = Filename.dirname path in
  if not (Sys.file_exists config_dir) then (try Unix.mkdir config_dir 0o700 with _ -> ());
  let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 path in
  Printf.fprintf oc "# Generated by 'caravan init' — docs: https://adukhan99.github.io/Caravan/\n";
  Printf.fprintf oc "provider = \"%s\"\n" (toml_escape choice.name);
  Printf.fprintf oc "model = \"%s\"\n" (toml_escape model);
  (match base_url with
   | Some u -> Printf.fprintf oc "base_url = \"%s\"\n" (toml_escape u)
   | None -> ());
  Printf.fprintf oc "stream = true\n";
  Printf.fprintf oc "transcript = true       # JSONL session logs in ~/.caravan/logs\n";
  Printf.fprintf oc "permissions = \"auto\"    # auto | ask | readonly\n";
  (match api_key_to_store with
   | Some k ->
     Printf.fprintf oc "\n[api_keys]\n%s = \"%s\"\n" choice.name (toml_escape k)
   | None -> ());
  close_out oc;
  (try Unix.chmod path 0o600 with _ -> ());
  println_ansi (green (Printf.sprintf "\n  ✓ Configuration saved to %s (0600)" path));
  println_ansi (dim  "    caravan            start chatting");
  println_ansi (dim  "    caravan agent \"…\"  run an autonomous task");
  println_ansi (dim  "    caravan doctor      verify everything works\n")

let init_cmd =
  let doc = "Interactive first-run setup wizard." in
  let info = Cmd.info "init" ~doc in
  Cmd.v info Term.(const run_init $ const ())

(* ── doctor command ───────────────────────────────────────────────────── *)

let run_doctor () =
  println_ansi (bold (cyan "\n  Caravan system diagnostics\n"));
  
  let checks = Doctor.run_checks
    ~find_provider:(fun n ->
      match Registry.find n with
      | Some e -> Some Doctor.{
          name = e.name;
          kind = (match e.kind with Registry.Local -> Local | Registry.Cloud -> Cloud);
          base_url = e.base_url;
          requires_key = e.requires_key;
          key_env = e.key_env;
        }
      | None -> None
    )
    ~api_key_for:(fun info ->
      let e = Option.get (Registry.find info.name) in
      Registry.api_key_for e
    )
    ~list_models:(fun info base_url ->
      let e = Option.get (Registry.find info.name) in
      let model = Config.get_string_opt (Some "CARAVAN_MODEL") "model" |> Option.value ~default:e.default_model in
      Eio_main.run (fun env ->
        let p = Registry.make_provider ?base_url ~model e.name in
        Provider.list_models_packed env#net p
      )
    )
    ~subagents_roster:(Subagents.describe ())
    ~subagents_enabled:(Subagents.enabled ())
    ()
  in

  let checks_passed = ref true in
  List.iter (fun (c : Doctor.check) ->
    let status_str = match c.severity with
      | Pass -> green "  ✓ "
      | Warn -> yellow "  ⚠ "
      | Fail -> checks_passed := false; red "  ✗ "
    in
    let hint_str = match c.hint with
      | Some h -> dim ("\n      " ^ h)
      | None -> ""
    in
    println_ansi (Printf.sprintf "%s%s: %s%s" status_str (bold c.label) c.message hint_str)
  ) checks;

  print_newline ();
  if !checks_passed then
    println_ansi (bold (green "  All diagnostics passed. Caravan is ready.\n"))
  else begin
    println_ansi (bold (yellow "  Some checks failed — see hints above.\n"));
    exit 1
  end

let doctor_cmd =
  let doc = "Run system and configuration diagnostics." in
  let info = Cmd.info "doctor" ~doc in
  Cmd.v info Term.(const run_doctor $ const ())

(* ── config command ───────────────────────────────────────────────────── *)

let run_config args =
  let path = Config.config_path () in
  match args with
  | [] | ["show"] ->
    if Sys.file_exists path then begin
      println_ansi (dim (Printf.sprintf "# %s" path));
      let ic = open_in path in
      (try
         while true do print_endline (input_line ic) done
       with End_of_file -> close_in ic)
    end else
      println_ansi (yellow (Printf.sprintf "No config file at %s (run 'caravan init')" path))
  | ["path"] -> print_endline path
  | ["get"; key] ->
    (match Config.get_string key with
     | Some v -> print_endline v
     | None ->
       match Config.get_int key with
       | Some v -> print_endline (string_of_int v)
       | None ->
         match Config.get_bool key with
         | Some b -> print_endline (string_of_bool b)
         | None -> Printf.eprintf "Key '%s' not set.\n%!" key; exit 1)
  | ["set"; key; value] ->
    (match Config.set_value key value with
     | Ok _ -> println_ansi (green (Printf.sprintf "✓ %s = %s" key value))
     | Error e -> Printf.eprintf "Error: %s\n%!" e; exit 1)
  | ["keys"] ->
    List.iter (fun (k, desc, accepts) ->
      Printf.printf "%-18s %-44s %s\n" k desc accepts
    ) Config.editable_keys
  | _ ->
    Printf.eprintf "Usage: caravan config [show|path|keys|get KEY|set KEY VALUE]\n%!";
    exit 2

let config_cmd =
  let args = Arg.(value & pos_all string [] & info [] ~docv:"ACTION") in
  let doc = "Show or edit the configuration file (show | path | get | set)." in
  let man = [
    `S Manpage.s_examples;
    `P "caravan config set model claude-sonnet-5"; `Noblank;
    `P "caravan config set permissions ask"; `Noblank;
    `P "caravan config get provider";
  ] in
  let info = Cmd.info "config" ~doc ~man in
  Cmd.v info Term.(const run_config $ args)

(* ── web command ──────────────────────────────────────────────────────── *)

let run_web model_cli provider_cli base_url_cli system port =
  init_plugins ();
  let _transcript = setup_frontend ~quiet:true () in
  let (provider_name, model, base_url) =
    resolve_cli_spec ~provider_cli ~model_cli ~base_url_cli
  in
  Web.serve ~port ~provider_name ~model
    ~make_session:(fun env ->
      make_session ~net:env#net ~clock:env#clock
        ~provider_name ~model ~base_url ~system)

let web_cmd =
  let port_arg =
    let doc = "Port to listen on (127.0.0.1 only)." in
    Arg.(value & opt int 8787 & info ["port"] ~docv:"PORT" ~doc)
  in
  let doc = "Serve a local web chat UI (single embedded page, localhost only)." in
  let info = Cmd.info "web" ~doc in
  Cmd.v info Term.(const run_web $ model_arg $ provider_arg $ base_url_arg
                   $ system_arg $ port_arg)

(* ── mcp command ──────────────────────────────────────────────────────── *)

let run_mcp_list () =
  init_plugins ();
  let mcp_servers = Config.get_mcp_servers () in
  if mcp_servers = [] then
    println_ansi (dim "No MCP servers configured.")
  else begin
    println_ansi (rule ~title:"MCP Servers" ());
    let h = Lazy.force host in
    List.iter (fun (cfg : Config.mcp_server_config) ->
      let id = "mcp:" ^ cfg.name in
      let fiber = Plugin_host.fiber h id in
      let status_mark, status_str = match fiber with
        | Some f when Plugin.Fiber.state f = Plugin.Fiber.Active -> (green "●", "active")
        | Some f when Plugin.Fiber.state f = Plugin.Fiber.Failed -> (red "✗", "failed")
        | _ -> (yellow "○", "inactive")
      in
      println_ansi (Printf.sprintf "  %s %s  %s  %s %s (%s)"
        status_mark
        (bold (Printf.sprintf "%-14s" cfg.name))
        (cyan (Printf.sprintf "%-6s" cfg.transport))
        (white cfg.command)
        (dim (String.concat " " cfg.args))
        (dim status_str))
    ) mcp_servers
  end

let run_mcp_get name =
  init_plugins ();
  match Config.get_mcp_server name with
  | None ->
    Printf.eprintf "MCP server '%s' not found.\n%!" name;
    exit 1
  | Some cfg ->
    println_ansi (rule ~title:(Printf.sprintf "MCP Server: %s" cfg.name) ());
    println_ansi (kv_line "Name" cfg.name);
    println_ansi (kv_line "Transport" cfg.transport);
    println_ansi (kv_line "Command" (cfg.command ^ " " ^ String.concat " " cfg.args));
    let h = Lazy.force host in
    let prefix = cfg.name ^ "_" in
    let tools = List.filter (fun t ->
      let n = Tool.name_of_packed t in
      String.length n > String.length prefix && String.sub n 0 (String.length prefix) = prefix
    ) (Plugin_host.tools h) in
    println_ansi (kv_line "Tools" (string_of_int (List.length tools)));
    List.iter (fun t ->
      println_ansi (Printf.sprintf "  %s  %s"
        (cyan (Tool.name_of_packed t))
        (dim (truncate_visible (Tool.description_of_packed t) 60)))
    ) tools

let run_mcp_add transport no_probe name command args =
  let transport = Option.value ~default:"stdio" transport in
  if not no_probe then begin
    println_ansi (dim (Printf.sprintf "Probing MCP server '%s' (%s %s)..." name command (String.concat " " args)));
    match Mcp.probe_server name command args with
    | Error err ->
      Printf.eprintf "Error: Probe failed for '%s': %s\n%!" name err;
      exit 1
    | Ok (client, tools) ->
      (try client.close () with _ -> ());
      println_ansi (green (Printf.sprintf "✓ Probe successful (%d tools discovered)" (List.length tools)))
  end;
  let cfg = { Config.name; transport; command; args } in
  match Config.add_mcp_server cfg with
  | Ok path ->
    println_ansi (green (Printf.sprintf "✓ Added MCP server '%s' to %s" name path))
  | Error err ->
    Printf.eprintf "Error: %s\n%!" err;
    exit 1

let run_mcp_remove name =
  match Config.delete_mcp_server name with
  | Ok path ->
    println_ansi (green (Printf.sprintf "✓ Removed MCP server '%s' from %s" name path))
  | Error err ->
    Printf.eprintf "Error: %s\n%!" err;
    exit 1

let mcp_cmd =
  let name_pos = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME" ~doc:"Server name.") in
  let cmd_pos = Arg.(required & pos 1 (some string) None & info [] ~docv:"COMMAND" ~doc:"Command to execute.") in
  let args_pos = Arg.(value & pos_right 1 string [] & info [] ~docv:"ARGS" ~doc:"Arguments to pass to command.") in
  let transport_opt = Arg.(value & opt (some string) None & info ["transport"] ~docv:"TRANSPORT" ~doc:"Transport type (stdio).") in
  let no_probe_flag = Arg.(value & flag & info ["no-probe"] ~doc:"Skip probing connection before saving.") in

  let list_cmd =
    let doc = "List configured MCP servers and their health status." in
    let info = Cmd.info "list" ~doc in
    Cmd.v info Term.(const run_mcp_list $ const ())
  in
  let get_cmd =
    let doc = "Show details and tools for an MCP server." in
    let info = Cmd.info "get" ~doc in
    Cmd.v info Term.(const run_mcp_get $ name_pos)
  in
  let add_cmd =
    let doc = "Add an MCP server configuration." in
    let man = [
      `S Manpage.s_examples;
      `P "caravan mcp add github -- npx -y @modelcontextprotocol/server-github";
      `P "caravan mcp add filesystem -- npx -y @modelcontextprotocol/server-filesystem /tmp";
    ] in
    let info = Cmd.info "add" ~doc ~man in
    Cmd.v info Term.(const run_mcp_add $ transport_opt $ no_probe_flag $ name_pos $ cmd_pos $ args_pos)
  in
  let remove_cmd =
    let doc = "Remove an MCP server configuration." in
    let info = Cmd.info "remove" ~doc in
    Cmd.v info Term.(const run_mcp_remove $ name_pos)
  in
  let rm_cmd =
    let doc = "Alias of $(b,caravan mcp remove)." in
    let info = Cmd.info "rm" ~doc in
    Cmd.v info Term.(const run_mcp_remove $ name_pos)
  in
  let info = Cmd.info "mcp" ~doc:"Manage Model Context Protocol (MCP) servers." in
  Cmd.group ~default:Term.(const run_mcp_list $ const ()) info [list_cmd; get_cmd; add_cmd; remove_cmd; rm_cmd]

(* ── Entry point ──────────────────────────────────────────────────────── *)

let () =
  let doc = "Typed agentic CLI harness and LLM orchestration framework." in
  let man = [
    `S Manpage.s_description;
    `P "Caravan is a typed, self-documenting harness for LLM agents. \
        Run it bare for an interactive REPL, or use $(b,caravan agent) \
        for scripted autonomous runs.";
    `S Manpage.s_examples;
    `P "caravan init                          # guided setup"; `Noblank;
    `P "caravan                               # chat REPL"; `Noblank;
    `P "caravan agent \"fix the failing test\"  # one-shot autonomy"; `Noblank;
    `P "caravan -p anthropic -m claude-sonnet-5"; `Noblank;
    `P "caravan providers --ladder            # model suggestions by size";
  ] in
  let info = Cmd.info "caravan" ~doc ~man ~version:Version.v in
  let default_cmd = Term.(const run_repl $ model_arg $ provider_arg $ base_url_arg $ system_arg $ verbose_arg) in
  let cmd = Cmd.group ~default:default_cmd info
    [ repl_cmd; agent_cmd; run_cmd; complete_cmd; models_cmd; providers_cmd;
      init_cmd; doctor_cmd; config_cmd; web_cmd; mcp_cmd ]
  in
  exit (Cmd.eval cmd)

