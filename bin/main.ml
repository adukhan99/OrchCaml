(** OrchCaml — Interactive TUI / REPL entry point.

    An interactive sessions manager for the OrchCaml orchestration framework.
    Talks to Ollama by default; supports OpenAI-compatible providers via flags.

    Usage:
      orchcaml                        -- start interactive REPL (Ollama default)
      orchcaml --model llama3.2       -- choose model
      orchcaml --provider openai      -- use OpenAI (needs OPENAI_API_KEY)
      orchcaml complete "question"    -- non-interactive single completion
      orchcaml models                 -- list available models
*)

open OrchCaml
open OrchCaml.Types
open OrchCaml.Config

(* ────────────────────────────────────────────────────────────────────────────
   ANSI colour helpers
   ──────────────────────────────────────────────────────────────────────────── *)

let ansi code s = Printf.sprintf "\027[%sm%s\027[0m" code s
let bold s    = ansi "1" s
let dim s     = ansi "2" s
let cyan s    = ansi "1;36" s
let green s   = ansi "1;32" s
let yellow s  = ansi "1;33" s
let magenta s = ansi "1;35" s
let red s     = ansi "1;31" s
let white s   = ansi "0;97" s
let blue s    = ansi "0;34" s

(* Store whether we have a real TTY (for colour/ANSI). *)
let is_tty = Unix.isatty Unix.stdout

let print_ansi s = if is_tty then print_string s else
  (* Strip ANSI for non-interactive use — simple regex approach *)
  let re = Re.compile (Re.seq [
    Re.char '\027'; Re.char '[';
    Re.rep (Re.compl [Re.char 'm']); Re.char 'm'
  ]) in
  print_string (Re.replace_string re ~by:"" s)

let println_ansi s = print_ansi s; print_char '\n'

(* ────────────────────────────────────────────────────────────────────────────
   Banner
   ──────────────────────────────────────────────────────────────────────────── *)

let print_banner () =
  if is_tty then begin
    println_ansi (cyan "╔═════════════════════════════════════════════════╗");
    println_ansi (cyan "║  " ^ bold (white "OrchCaml") ^ white "  v0.1  —  Typed LLM Orchestration     " ^ cyan "║");
    println_ansi (cyan "╚═════════════════════════════════════════════════╝");
    print_newline ()
  end

(* ────────────────────────────────────────────────────────────────────────────
   Slash command help
   ──────────────────────────────────────────────────────────────────────────── *)

let get_available_tools () =
  let tools_dir = "lib/tools" in
  if Sys.file_exists tools_dir && Sys.is_directory tools_dir then
    let files = Array.to_list (Sys.readdir tools_dir) in
    let ml_files = List.filter (fun f -> Filename.check_suffix f ".ml") files in
    let desc_re = Re.compile (Re.seq [
      Re.str "let description"; Re.rep Re.space; Re.char '='; Re.rep Re.space;
      Re.char '"'; Re.group (Re.rep (Re.compl [Re.char '"'])); Re.char '"'
    ]) in
    List.map (fun f ->
      let path = Filename.concat tools_dir f in
      let name = Filename.chop_suffix f ".ml" in
      let desc =
        try
          let ic = open_in path in
          let rec loop () =
            let line = input_line ic in
            match Re.exec_opt desc_re line with
            | Some g -> Re.Group.get g 1
            | None -> loop ()
          in
          let d = loop () in
          close_in ic; d
        with _ -> "No description available"
      in
      (name, desc)
    ) ml_files
  else []

let print_help () =
  println_ansi (bold (yellow " Slash Commands:"));
  let cmds = [
    "/model <name>",    "Switch the model";
    "/system <text>",   "Set the system prompt";
    "/nosystem",        "Clear the system prompt";
    "/memory <n>",      "Set context window (0 = max)";
    "/clear",           "Clear conversation history";
    "/history",         "Print conversation history";
    "/export [file]",   "Export session JSON to stdout or file";
    "/models",          "List available models";
    "/tools",           "List available tools";
    "/provider",        "Show current provider info";
    "/temp <0.0-2.0>",  "Set temperature";
    "/help",            "Show this help";
    "/quit  or  /exit", "Exit OrchCaml";
  ] in
  List.iter (fun (cmd, desc) ->
    println_ansi (Printf.sprintf "  %s  %s"
      (cyan (Printf.sprintf "%-22s" cmd))
      (dim desc))
  ) cmds;
  print_newline ()

(* ────────────────────────────────────────────────────────────────────────────
   Provider construction helpers
   ──────────────────────────────────────────────────────────────────────────── *)

let make_ollama_provider model =
  OrchCamlProviders.Ollama.make_provider ~model ()

let make_openai_provider ?base_url model =
  OrchCamlProviders.Openai.make_provider ?base_url ~model ()

(* ────────────────────────────────────────────────────────────────────────────
   REPL state
   ──────────────────────────────────────────────────────────────────────────── *)

type repl_state = {
  mutable session  : Session.t;
  mutable provider_name : string;
  mutable model    : string;
  mutable provider : Provider.packed_provider;
  mutable use_openai : bool;
  mutable openai_base : string;
}

let rebuild_session st =
  let provider =
    if st.use_openai then
      make_openai_provider ~base_url:st.openai_base st.model
    else
      make_ollama_provider st.model
  in
  st.provider <- provider;
  st.session  <- Session.create st.model provider

(* ────────────────────────────────────────────────────────────────────────────
   Token streaming callback — prints with colour, no newline
   ──────────────────────────────────────────────────────────────────────────── *)

let on_token token =
  print_ansi (green token);
  flush stdout

(* ────────────────────────────────────────────────────────────────────────────
   Slash command handler
   ──────────────────────────────────────────────────────────────────────────── *)

let handle_slash_command st line =
  let open Lwt.Syntax in
  let parts = String.split_on_char ' ' (String.trim line) in
  match parts with

  | ["/quit"] | ["/exit"] | ["/q"] ->
    println_ansi (dim "\nGoodbye.");
    exit 0

  | ["/help"] | ["/?"] ->
    print_help ();
    Lwt.return_unit

  | "/model" :: rest ->
    let new_model = String.concat " " rest |> String.trim in
    if new_model = "" then
      (println_ansi (red "Usage: /model <model-name>"); Lwt.return_unit)
    else begin
      st.model <- new_model;
      rebuild_session st;
      println_ansi (yellow (Printf.sprintf "  ✓ Model → %s" new_model));
      Lwt.return_unit
    end

  | "/system" :: rest ->
    let text = String.concat " " rest |> String.trim in
    if text = "" then
      (println_ansi (red "Usage: /system <prompt text>"); Lwt.return_unit)
    else begin
      st.session <- Session.set_system st.session text;
      println_ansi (yellow (Printf.sprintf "  ✓ System prompt set (%d chars)" (String.length text)));
      Lwt.return_unit
    end

  | ["/nosystem"] ->
    st.session <- Session.set_system st.session "";
    println_ansi (yellow "  ✓ System prompt cleared");
    Lwt.return_unit

  | "/memory" :: [n_str] ->
    (match int_of_string_opt n_str with
     | None   -> println_ansi (red "Usage: /memory <n>  (integer)"); Lwt.return_unit
     | Some n ->
       let window = if n = 0 then max_int else n in
       st.session <- Session.create
         ~config:(fun m -> { (Session.default_config m) with memory_size = window })
         st.model st.provider;
       println_ansi (yellow (Printf.sprintf "  ✓ Memory window → %s"
         (if n = 0 then "unlimited" else string_of_int n)));
       Lwt.return_unit)

  | ["/clear"] ->
    st.session <- Session.clear st.session;
    println_ansi (yellow "  ✓ History cleared");
    Lwt.return_unit

  | ["/history"] ->
    let hist = Session.history st.session in
    if hist = [] then
      println_ansi (dim "  (empty history)")
    else
      List.iter (fun msg ->
        let role_str = role_to_string msg.role in
        let colour = match msg.role with
          | System    -> yellow
          | User      -> cyan
          | Assistant -> green
          | Tool _    -> magenta
        in
        println_ansi (Printf.sprintf "%s: %s"
          (bold (colour role_str))
          (dim msg.content))
      ) hist;
    Lwt.return_unit

  | "/export" :: rest ->
    let json = Session.export_json st.session in
    let json_str = Yojson.Safe.pretty_to_string json in
    (match rest with
     | [file] ->
       let oc = open_out file in
       output_string oc json_str;
       close_out oc;
       println_ansi (yellow (Printf.sprintf "  ✓ Exported to %s" file))
     | _ ->
       print_endline json_str);
    Lwt.return_unit

  | ["/models"] ->
    Lwt.catch
      (fun () ->
        let* models = Provider.list_models_packed st.provider in
        println_ansi (bold (yellow (Printf.sprintf "  Models on %s:" st.provider_name)));
        List.iter (fun m ->
          let mark = if m = st.model then green " ✓ " else dim "   " in
          println_ansi (mark ^ white m)
        ) models;
        Lwt.return_unit)
      (fun exn ->
        let msg = match exn with
          | Failure s -> s
          | _ -> Printexc.to_string exn
        in
        println_ansi (red (Printf.sprintf "  Error: %s" msg));
        Lwt.return_unit)

  | ["/tools"] ->
    let tools = get_available_tools () in
    if tools = [] then
      println_ansi (yellow "  No tools found loosely in lib/tools/")
    else begin
      println_ansi (bold (yellow "  Available Tools:"));
      List.iter (fun (name, desc) ->
        println_ansi (Printf.sprintf "  %s  %s" 
          (cyan (Printf.sprintf "%-15s" name))
          (dim desc))
      ) tools
    end;
    Lwt.return_unit

  | ["/provider"] ->
    println_ansi (Printf.sprintf "  %s  %s  %s"
      (bold (blue "Provider:")) (white st.provider_name)
      (dim ("model=" ^ st.model)));
    Lwt.return_unit

  | "/temp" :: [v_str] ->
    (match float_of_string_opt v_str with
     | None -> println_ansi (red "Usage: /temp <float 0.0-2.0>"); Lwt.return_unit
     | Some temp ->
       st.session <- Session.create
         ~config:(fun m -> { (Session.default_config m) with options = { (Session.default_config m).options with temperature = Some temp } })
         st.model st.provider;
       println_ansi (yellow (Printf.sprintf "  ✓ Temperature → %.2f" temp));
       Lwt.return_unit)

  | cmd :: _ ->
    println_ansi (red (Printf.sprintf "  Unknown command: %s  (try /help)" cmd));
    Lwt.return_unit

  | [] -> Lwt.return_unit

(* ────────────────────────────────────────────────────────────────────────────
   The main REPL loop
   ──────────────────────────────────────────────────────────────────────────── *)

let repl st =
  let open Lwt.Syntax in
  let prompt () =
    if is_tty then
      print_ansi (Printf.sprintf "\n%s %s %s "
        (blue (Printf.sprintf "[%s/%s]" st.provider_name st.model))
        (cyan "›")
        "")
    else ();
    flush stdout
  in
  let rec loop () =
    prompt ();
    let* line_opt = Lwt_io.(read_line_opt stdin) in
    let line = match line_opt with
      | Some l -> String.trim l
      | None -> "/quit"
    in
    if line = "" then loop ()
    else if String.length line > 0 && line.[0] = '/' then begin
      let* () = handle_slash_command st line in
      loop ()
    end else begin
      (* Print assistant label *)
      if is_tty then
        println_ansi (Printf.sprintf "\n%s" (bold (green "Assistant:")));
      (* Stream response *)
      let* (new_sess, response) = Session.turn_stream st.session line ~on_token in
      st.session <- new_sess;
      if is_tty then print_newline ();
      (* If not a TTY (piped), print the full response after *)
      if not is_tty then print_endline response;
      loop ()
    end
  in
  loop ()

(* ────────────────────────────────────────────────────────────────────────────
   Subcommands
   ──────────────────────────────────────────────────────────────────────────── *)

(** [cmd_complete] — single non-interactive completion. *)
let cmd_complete ~model ~use_openai ~openai_base ~system prompt_text =
  let open Lwt.Syntax in
  let provider =
    if use_openai then make_openai_provider ~base_url:openai_base model
    else make_ollama_provider model
  in
  let sess = Session.create model provider in
  let sess = match system with Some s -> Session.set_system sess s | None -> sess in
  Lwt.catch
    (fun () ->
      let* (_sess, _result) = Session.turn_stream sess prompt_text ~on_token in
      print_newline ();
      (* Yield briefly so background async task finishes before scheduler exits *)
      Lwt_unix.sleep 0.05)
    (function
      | Lwt_stream.Closed -> Lwt.return_unit
      | exn ->
        Printf.eprintf "[OrchCaml] Error: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit)

(** [cmd_models] — list models for a provider. *)
let cmd_models ~use_openai ~openai_base ~model () =
  let open Lwt.Syntax in
  let provider =
    if use_openai then make_openai_provider ~base_url:openai_base model
    else make_ollama_provider model
  in
  Lwt.catch
    (fun () ->
      let* models = Provider.list_models_packed provider in
      List.iter (fun m ->
        print_endline (if m = model then "> " ^ m else "  " ^ m)
      ) models;
      Lwt.return_unit)
    (fun exn ->
      let msg = match exn with
        | Failure s -> s
        | _ -> Printexc.to_string exn
      in
      Printf.eprintf "[OrchCaml] Error: %s\n%!" msg;
      exit 1)

(* ────────────────────────────────────────────────────────────────────────────
   Cmdliner CLI setup
   ──────────────────────────────────────────────────────────────────────────── *)

open Cmdliner

let model_arg =
  let doc = "Model name to use. For Ollama: the model tag (e.g. llama3.2, gpt-oss:20b). \
             For OpenAI: the model ID (e.g. gpt-4o)." in
  let default = match get_string "model" with Some v -> v | None -> "gpt-oss:20b" in
  Arg.(value & opt string default & info ["m"; "model"] ~docv:"MODEL" ~doc)

let provider_arg =
  let doc = "Provider to use: 'ollama' (default, local) or 'openai'." in
  let default = match get_string "provider" with Some v -> v | None -> "ollama" in
  Arg.(value & opt string default & info ["p"; "provider"] ~docv:"PROVIDER" ~doc)

let openai_base_arg =
  let doc = "Base URL for OpenAI-compatible API. Default: https://api.openai.com/v1. \
             Override for Groq, Together, local proxies, etc." in
  let default = match get_string "base_url" with Some v -> v | None -> "https://api.openai.com/v1" in
  Arg.(value & opt string default
       & info ["base-url"] ~docv:"URL" ~doc)

let system_arg =
  let doc = "System prompt to use for the session or completion." in
  let default = get_string "system" in
  Arg.(value & opt (some string) default & info ["s"; "system"] ~docv:"PROMPT" ~doc)

let run_repl model provider_str openai_base system =
  let use_openai = provider_str = "openai" in
  let provider_name = provider_str in
  let provider =
    if use_openai then make_openai_provider ~base_url:openai_base model
    else make_ollama_provider model
  in
  let sess = Session.create model provider in
  let sess = match system with Some s -> Session.set_system sess s | None -> sess in
  let st = {
    session      = sess;
    provider_name;
    model;
    provider;
    use_openai;
    openai_base;
  } in
  print_banner ();
  if is_tty then begin
    println_ansi (Printf.sprintf "  %s %s  %s %s"
      (bold (blue "Provider:")) (white provider_name)
      (bold (blue "Model:"))    (white model));
    (match system with
     | Some s -> println_ansi (Printf.sprintf "  %s %s"
                   (bold (yellow "System:")) (dim s))
     | None -> ());
    println_ansi (dim "  Type a message and press Enter. Use /help for commands.");
    print_newline ()
  end;
  (try Lwt_main.run (repl st)
   with Lwt_stream.Closed -> ())

(* ── Interactive REPL command ── *)
let repl_cmd =
  let doc = "Start an interactive chat session (default command)." in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(const run_repl $ model_arg $ provider_arg $ openai_base_arg $ system_arg)

(* ── Single completion command ── *)
let complete_cmd =
  let prompt_arg =
    let doc = "The prompt text to send." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PROMPT" ~doc)
  in
  let run model provider_str openai_base system prompt =
    let use_openai = provider_str = "openai" in
    (try Lwt_main.run (cmd_complete
      ~model ~use_openai ~openai_base ~system prompt)
     with Lwt_stream.Closed -> ())
  in
  let doc = "Send a single prompt and print the response (non-interactive)." in
  let info = Cmd.info "complete" ~doc in
  Cmd.v info Term.(const run $ model_arg $ provider_arg $ openai_base_arg $ system_arg $ prompt_arg)

(* ── List models command ── *)
let models_cmd =
  let run model provider_str openai_base =
    let use_openai = provider_str = "openai" in
    Lwt_main.run (cmd_models ~use_openai ~openai_base ~model ())
  in
  let doc = "List available models for the chosen provider." in
  let info = Cmd.info "models" ~doc in
  Cmd.v info Term.(const run $ model_arg $ provider_arg $ openai_base_arg)

(* ────────────────────────────────────────────────────────────────────────────
   Entry point
   ──────────────────────────────────────────────────────────────────────────── *)

let () =
  let doc = "A typed LLM orchestration framework and interactive REPL." in
  let info = Cmd.info "orchcaml"
    ~doc
    ~version:"0.1.0"
    ~man:[
      `S "DESCRIPTION";
      `P "OrchCaml is a functional, type-safe LLM orchestration framework for OCaml. \
          It provides typed pipelines (Template → LLM → Parser), pluggable providers \
          (Ollama, OpenAI-compatible), conversation memory, and this interactive REPL.";
      `S "EXAMPLES";
      `P "Start an interactive session with your local Ollama:";
      `Pre "  orchcaml";
      `P "Use a specific model:";
      `Pre "  orchcaml --model llama3.2";
      `P "Non-interactive completion:";
      `Pre "  orchcaml complete \"What is a monad?\"";
      `P "List available Ollama models:";
      `Pre "  orchcaml models";
      `P "Use OpenAI:";
      `Pre "  OPENAI_API_KEY=sk-... orchcaml --provider openai --model gpt-4o";
      `P "Use a generic OpenAI-compatible API (e.g. OpenRouter):";
      `Pre "  OPENAI_API_KEY=sk-... orchcaml --provider openai --base-url https://openrouter.ai/api/v1 --model meta-llama/llama-3-8b-instruct";
    ]
  in
  (* Default to 'repl' if no subcommand is given *)
  let default_cmd = Term.(const run_repl $ model_arg $ provider_arg $ openai_base_arg $ system_arg)
  in
  let cmd = Cmd.group ~default:default_cmd info
    [ repl_cmd; complete_cmd; models_cmd ]
  in
  exit (Cmd.eval cmd)
