(** Stateful multi-turn conversation sessions. *)

open Types
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type config = {
  model               : string;
  system              : string option;
  options             : gen_options;
  memory_size         : int;
  max_tool_output_len : int option;
  auto_summarize      : bool;
} [@@deriving yojson]

let default_config model = {
  model;
  system              = None;
  options             = default_options;
  memory_size         = 40;
  max_tool_output_len = Some 1000;
  auto_summarize      = true;
}

type spinner_config = {
  enabled : bool;
  get_verb : string -> string;
}

let default_spinner_config () = {
  enabled = Config.get_spinner_enabled ();
  get_verb = fun v -> Config.pick_verb (Config.get_verbs v);
}

type t = {
  cfg      : config;
  provider : Provider.packed_provider;
  memory   : Memory.packed_memory;
  turn_idx : int;
  tools    : Tool.packed_tool list;
  spinner_cfg : spinner_config;
}

let create ?(config = fun m -> default_config m) ?(tools=[]) model provider =
  let cfg = config model in
  let window = if cfg.memory_size = 0 then max_int else cfg.memory_size in
  {
    cfg;
    provider;
    memory = Memory.Mem ((module Memory.Ring), Memory.Ring.make ~window ());
    turn_idx = 0;
    tools;
    spinner_cfg = default_spinner_config ();
  }

let set_system sess text =
  let cfg =
    if String.trim text = "" then
      { sess.cfg with system = None }
    else
      { sess.cfg with system = Some text }
  in
  { sess with cfg }

let set_memory_size sess n =
  let cfg = { sess.cfg with memory_size = n } in
  let Memory.Mem ((module M), mem) = sess.memory in
  let memory = Memory.Mem ((module M), M.set_window mem n) in
  { sess with cfg; memory }

let set_max_tool_output_len sess max_len =
  { sess with cfg = { sess.cfg with max_tool_output_len = max_len } }

let set_auto_summarize sess auto =
  { sess with cfg = { sess.cfg with auto_summarize = auto } }

let set_options sess f =
  let cfg = { sess.cfg with options = f sess.cfg.options } in
  { sess with cfg }

let with_spinner_config spinner_cfg sess =
  { sess with spinner_cfg }

let clear sess =
  let Memory.Mem ((module M), mem) = sess.memory in
  { sess with memory = Memory.Mem ((module M), M.clear mem); turn_idx = 0 }

let add_messages sess msgs =
  let Memory.Mem ((module M), mem) = sess.memory in
  let final_mem = List.fold_left M.add mem msgs in
  { sess with memory = Memory.Mem ((module M), final_mem) }

let with_provider sess provider =
  { sess with provider }

let config sess = sess.cfg
let provider sess = sess.provider
let tools sess = sess.tools
let with_tools sess tools = { sess with tools }
let turn_idx sess = sess.turn_idx

let with_model sess model =
  { sess with cfg = { sess.cfg with model } }

let history sess =
  let Memory.Mem ((module M), mem) = sess.memory in
  M.get mem

let truncation_suffix = "omitted to preserve context ...]"

(** Truncate an aged tool output.  Idempotent — an already-truncated
    message is returned unchanged, byte for byte, because this runs on
    every turn and any byte drift in an old message invalidates the
    provider's prompt-cache prefix from that point on. *)
let truncate_tool_output max_len msg =
  match msg.role with
  | Tool _ when String.length msg.content > max_len
             && not (String.ends_with ~suffix:truncation_suffix msg.content) ->
    let prefix = String.sub msg.content 0 max_len in
    let omitted = String.length msg.content - max_len in
    { msg with content = Printf.sprintf "%s\n[... %d bytes omitted to preserve context ...]" prefix omitted }
  | _ -> msg

(** How many trailing messages keep their full tool output.  The model
    reads a result in full while it is fresh; once it ages past this
    window it is truncated permanently in memory (see [stabilize]). *)
let full_output_window = 2

(** Truncate tool outputs that have aged out of the recent window —
    {b in memory}, once, so each message's serialised form is immutable
    for the rest of its life.  The previous design re-derived truncation
    per request with a moving boundary, which changed old messages'
    bytes between turns and made the request prefix uncacheable by
    construction (prompt caching is strict prefix matching).  Run before
    every provider call; idempotence of [truncate_tool_output] makes the
    repeated application byte-stable. *)
let stabilize sess =
  match sess.cfg.max_tool_output_len with
  | None -> sess
  | Some max_len ->
    let Memory.Mem ((module M), mem) = sess.memory in
    let mem' = M.map_recent mem ~keep:full_output_window (truncate_tool_output max_len) in
    { sess with memory = Memory.Mem ((module M), mem') }

(** Defensive wire-shape validation: drop [tool] messages whose
    [tool_call_id] is not introduced by a preceding assistant message.
    Ring eviction is pair-aware, but checkpoint restore ([of_json])
    reconstructs memory from arbitrary saved JSON, and strict endpoints
    hard-reject orphaned tool results — cheap insurance here beats a
    non-retriable 400 mid-run. *)
let drop_orphan_tool_results msgs =
  let (rev, _) =
    List.fold_left
      (fun (acc, known) msg ->
         match msg.role with
         | Tool id ->
           if List.mem id known then (msg :: acc, known) else (acc, known)
         | _ ->
           let known =
             match msg.tool_calls with
             | Some tcs -> List.map (fun (tc : tool_call) -> tc.id) tcs @ known
             | None -> known
           in
           (msg :: acc, known))
      ([], []) msgs
  in
  List.rev rev

(* Truncation deliberately does NOT happen here any more: rendering a
   message differently depending on its distance from the end changed
   its bytes between turns and defeated prompt caching (audit H3).
   [stabilize] now truncates in memory, exactly once per message. *)
let history_for_llm sess =
  let Memory.Mem ((module M), mem) = sess.memory in
  let hist = drop_orphan_tool_results (M.get mem) in
  match sess.cfg.system with
  | None     -> hist
  | Some sys ->
    let sm = system_msg sys in
    (match hist with
     | { role = System; _ } :: _ -> hist
     | rest -> sm :: rest)

(** Retry aggression for provider calls, from the [provider_retry]
    setting; unknown values fall back to the library default. *)
let retry_mode () =
  match Provider.Retry.of_string (Config.get_provider_retry_mode ()) with
  | Some m -> m
  | None -> Provider.Retry.default_mode

let execute_tool_calls _net clock sess tcs =
  Eio.Fiber.List.map (fun tc ->
    match Tool.find_tool sess.tools tc.name with
    | None ->
      let msg = Printf.sprintf "Tool '%s' not found in registered tools." tc.name in
      Trace.emit (Trace.Tool_not_found { name = tc.name });
      tool_msg tc.id msg
    | Some packed ->
      Trace.emit (Trace.Tool_call_start { name = tc.name; args = tc.args });
      let verb = sess.spinner_cfg.get_verb tc.name in
      let enabled = sess.spinner_cfg.enabled in
      let t0 = Unix.gettimeofday () in
      let output_str = Ui.with_spinner clock verb enabled (fun () -> Tool.dispatch packed tc.args) in
      let duration = Unix.gettimeofday () -. t0 in
      if tc.name = "finish" then
        Trace.emit (Trace.Task_finished { summary = output_str })
      else
        Trace.emit (Trace.Tool_call_end { name = tc.name; output = output_str; duration });
      tool_msg tc.id output_str
  ) tcs

let default_prompt_fn msgs =
  let format_history =
    String.concat "\n"
      (List.map (fun m ->
         Printf.sprintf "[%s]: %s" (role_to_string m.role) m.content) msgs)
  in
  "Please provide a highly concise summary of the following conversation history. " ^
  "Focus on preserving key details, facts, contexts, and instructions. " ^
  "Write ONLY the plain-text summary, with no meta-commentary, introductory text, or headers.\n\n" ^
  "Conversation History:\n" ^
  format_history

let summarise ?prompt_fn net clock sess =
  let hist = history sess in
  if hist = [] then
    (sess, "Conversation history is empty; nothing to summarize.")
  else
    let prompt =
      match prompt_fn with
      | Some f -> f hist
      | None -> default_prompt_fn hist
    in
    let verb = sess.spinner_cfg.get_verb "summarizing" in
    let enabled = sess.spinner_cfg.enabled in
    Trace.emit Trace.Summarize_start;
    let result = Ui.with_spinner clock verb enabled (fun () ->
      Provider.complete_packed ~retry_mode:(retry_mode ()) ~retry_clock:clock
        net ~model:sess.cfg.model ~options:sess.cfg.options ~tools:[] sess.provider [user_msg prompt]
    ) in
    let summary_content = String.trim result.value.content in
    Trace.emit (Trace.Summarize_end { summary = summary_content });
    let new_mem_t =
      let open Memory.Summary in
      let mem = create ~max_messages:sess.cfg.memory_size () in
      let mem_sum = compress ~complete:(fun _ -> summary_content) mem in
      Memory.Mem ((module Memory.SummaryMemory), mem_sum)
    in
    (* [turn_idx] counts turns consumed against the agent budget and is
       deliberately NOT reset here: compaction is a memory operation, not
       a new conversation.  Resetting it handed the agent a fresh budget
       on every compaction, making [max_turns] unenforceable on exactly
       the runs (confused model, long history) that most need a ceiling.
       Only [clear] — a genuine new conversation — resets the counter. *)
    let new_sess = { sess with memory = new_mem_t } in
    (new_sess, summary_content)

(** Why a conversation run ended. Threaded into the returned
    [result_with_meta.finish_reason] so callers (notably [Agent]) can tell
    a genuine [finish] tool call apart from a turn-budget stop without
    scanning history. *)
type done_reason =
  | Via_finish_tool
  | Via_max_turns
  | Via_plain_reply

let done_reason_string = function
  | Via_finish_tool -> "finish_tool"
  | Via_max_turns   -> "max_turns"
  | Via_plain_reply -> "plain_reply"

type step_outcome =
  | Continue of t
  | Done     of t * string * done_reason

(** Recover a tool call the model emitted as text (audit C2).  Applied
    only when the native [tool_calls] field is empty, the session has
    tools, and [tool_call_mode] is not "native".  The extractor itself
    enforces the whole-content and registered-tool guards; the reply is
    rewritten to carry the synthesised calls (content moves to the
    tool_calls, mirroring what a native-calling model would have sent). *)
let apply_tool_call_fallback sess (reply : chat_message) =
  match reply.tool_calls with
  | Some tcs when tcs <> [] -> reply
  | _ ->
    if sess.tools = [] || Config.get_tool_call_mode () = "native" then reply
    else
      match Tool_call_fallback.extract ~tools:sess.tools reply.content with
      | None -> reply
      | Some (tcs, format) ->
        List.iter
          (fun (tc : tool_call) ->
             Trace.emit (Trace.Tool_call_fallback { name = tc.name; format }))
          tcs;
        { reply with tool_calls = Some tcs; content = "" }

let run_turn_step ?max_turns ?on_turn ?on_step net clock sess (reply : chat_message) =
  let reply = apply_tool_call_fallback sess reply in
  let Memory.Mem ((module M), mem) = sess.memory in
  let final_memory = Memory.Mem ((module M), M.add mem reply) in
  let new_sess = { sess with memory = final_memory; turn_idx = sess.turn_idx + 1 } in
  (match on_turn with
   | Some f -> f new_sess.turn_idx (Option.value ~default:0 max_turns)
   | None -> ());
  let notify_step s =
    match on_step with
    | Some f -> (try f s with _ -> ())
    | None -> ()
  in
  match reply.tool_calls with
  | Some tcs when tcs <> [] ->
    let tool_responses = execute_tool_calls net clock new_sess tcs in
    let memory_with_tools =
      List.fold_left (fun (Memory.Mem ((module M2), m2)) r -> Memory.Mem ((module M2), M2.add m2 r)) new_sess.memory tool_responses
    in
    let sess_after_tools = { new_sess with memory = memory_with_tools } in
    
    (* Trigger summarization if explicit tool was executed or if history size threshold reached *)
    let tool_call_names = List.map (fun tc -> tc.name) tcs in
    let Memory.Mem ((module M2), mem2) = memory_with_tools in
    let compact = Compaction_policy.should_compact
      ~auto_summarize:sess.cfg.auto_summarize
      ~memory_size:sess.cfg.memory_size
      ~history_length:(M2.length mem2)
      ~tool_call_names
    in
    let sess_after_sum =
      if compact then
        let (s, _) = summarise net clock sess_after_tools in
        s
      else
        sess_after_tools
    in
    notify_step sess_after_sum;

    let has_finish = List.exists (fun tc -> tc.name = "finish") tcs in
    if has_finish then
      let finish_tool_call = List.find (fun tc -> tc.name = "finish") tcs in
      let finish_output =
        match List.find_opt (fun (m : chat_message) ->
          match m.role with Tool id -> id = finish_tool_call.id | _ -> false
        ) tool_responses with
        | Some m -> m.content
        | None -> ""
      in
      let final_content =
        if reply.content = "" then finish_output
        else reply.content ^ "\n\n" ^ finish_output
      in
      Done (sess_after_sum, final_content, Via_finish_tool)
    else
      (match max_turns with
       | Some max_t when max_t > 0 && sess_after_sum.turn_idx >= max_t ->
         Done (sess_after_sum, "Maximum turns reached without completion.", Via_max_turns)
       | _ ->
         Continue sess_after_sum)
  | _ ->
    notify_step new_sess;
    Done (new_sess, reply.content, Via_plain_reply)

let emit_assistant_reply (reply : chat_message) =
  let tool_call_names =
    match reply.tool_calls with
    | None -> []
    | Some tcs -> List.map (fun (tc : tool_call) -> tc.name) tcs
  in
  Trace.emit (Trace.Assistant_reply { content = reply.content; tool_call_names })

let rec run_conversations ?max_turns ?on_turn ?on_step net clock sess =
  let sess = stabilize sess in
  let verb = sess.spinner_cfg.get_verb "thinking" in
  let enabled = sess.spinner_cfg.enabled in
  let result = Ui.with_spinner clock verb enabled (fun () ->
    Provider.complete_packed ~retry_mode:(retry_mode ()) ~retry_clock:clock
      net ~model:sess.cfg.model ~options:sess.cfg.options ~tools:sess.tools sess.provider (history_for_llm sess)
  ) in
  emit_assistant_reply result.value;
  let outcome = run_turn_step ?max_turns ?on_turn ?on_step net clock sess result.value in
  match outcome with
  | Continue sess' -> run_conversations ?max_turns ?on_turn ?on_step net clock sess'
  | Done (sess', content, reason) ->
      (sess', { result with value = { result.value with content };
                            finish_reason = Some (done_reason_string reason);
                            turn_count = Some sess'.turn_idx })

let turn net clock sess user_input =
  let user = user_msg user_input in
  let Memory.Mem ((module M), mem) = sess.memory in
  let sess' = { sess with memory = Memory.Mem ((module M), M.add mem user) } in
  run_conversations net clock sess'

let rec run_conversations_stream ?max_turns ?on_turn ?on_step net clock sess ~on_token =
  let sess = stabilize sess in
  let verb = sess.spinner_cfg.get_verb "thinking" in
  let enabled = sess.spinner_cfg.enabled in
  let result_with_meta =
    Eio.Switch.run (fun sw ->
      let promise, resolver = Eio.Promise.create () in
      let spinner_stopped = Ui.run_spinner_until_promise sw clock verb enabled promise in
      let first_token = ref true in
      let wrapped_on_token token =
        if !first_token then begin
          first_token := false;
          Eio.Promise.resolve resolver ();
          (* Wait for the spinner to erase itself before any output —
             otherwise its erase wipes the first streamed tokens. *)
          (match spinner_stopped with
           | Some stopped -> Eio.Promise.await stopped
           | None -> ());
          Trace.emit Trace.Stream_start
        end;
        on_token token
      in
      Fun.protect
        ~finally:(fun () -> if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ())
        (fun () ->
         Provider.stream_packed ~retry_mode:(retry_mode ()) ~retry_clock:clock
           net ~model:sess.cfg.model ~options:sess.cfg.options ~tools:sess.tools
           ~on_token:wrapped_on_token sess.provider (history_for_llm sess))
    )
  in
  emit_assistant_reply result_with_meta.value;
  let outcome = run_turn_step ?max_turns ?on_turn ?on_step net clock sess result_with_meta.value in
  match outcome with
  | Continue sess' -> run_conversations_stream ?max_turns ?on_turn ?on_step net clock sess' ~on_token
  | Done (sess', content, reason) ->
      (sess', { result_with_meta with value = { result_with_meta.value with content };
                                      finish_reason = Some (done_reason_string reason);
                                      turn_count = Some sess'.turn_idx })

let turn_stream net clock sess user_input ~on_token =
  let user = user_msg user_input in
  let Memory.Mem ((module M), mem) = sess.memory in
  let sess' = { sess with memory = Memory.Mem ((module M), M.add mem user) } in
  run_conversations_stream net clock sess' ~on_token

let export_json sess =
  let Memory.Mem ((module M), mem) = sess.memory in
  `Assoc [
    ("config",   yojson_of_config sess.cfg);
    ("turn_idx", `Int sess.turn_idx);
    ("history",  M.to_json mem);
  ]

let of_json ~provider ?(tools = []) json =
  match json with
  | `Assoc _ ->
    (try
       let open Yojson.Safe.Util in
       let cfg =
         match json |> member "config" with
         | `Null ->
           (* Backwards compatibility for legacy checkpoints without full config *)
           let model = json |> member "model" |> to_string in
           let system = json |> member "system" |> to_string_option in
           { (default_config model) with system }
         | cfg_json -> config_of_yojson cfg_json
       in
       let turn_idx =
         match json |> member "turn_idx" with
         | `Int i -> i
         | _ -> 0
       in
       let history_json =
         match json |> member "history" with
         | `List _ as l -> l
         | _ -> `List []
       in
       let ring_mem = Memory.Ring.of_json history_json in
       let sess = create ~tools cfg.model provider in
       let sess = {
         sess with
         cfg;
         turn_idx;
         memory = Memory.Mem ((module Memory.Ring), ring_mem);
       } in
       Ok sess
     with exn ->
       Error (Printf.sprintf "Failed to parse session JSON: %s" (Caravan_error.humanize exn)))
  | _ -> Error "Invalid session JSON: expected JSON object"

let default_checkpoint_path () =
  let dir = Config.log_dir () in
  let rec mkdir_p path =
    if not (Sys.file_exists path) then begin
      mkdir_p (Filename.dirname path);
      (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    end
  in
  mkdir_p dir;
  Filename.concat dir "last_checkpoint.json"

let save_checkpoint ?path sess =
  let p = match path with Some path -> path | None -> default_checkpoint_path () in
  try
    let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 p in
    output_string oc (Yojson.Safe.pretty_to_string (export_json sess));
    output_char oc '\n';
    close_out oc;
    Ok p
  with exn ->
    Error (Printf.sprintf "Failed to save checkpoint to '%s': %s" p (Caravan_error.humanize exn))

let load_checkpoint ~provider ?tools ?path () =
  let p = match path with Some path -> path | None -> default_checkpoint_path () in
  if not (Sys.file_exists p) then
    Error (Printf.sprintf "Checkpoint file '%s' does not exist." p)
  else
    try
      let json = Yojson.Safe.from_file p in
      of_json ~provider ?tools json
    with exn ->
      Error (Printf.sprintf "Failed to load checkpoint from '%s': %s" p (Caravan_error.humanize exn))

let pp_history fmt sess =
  let Memory.Mem ((module M), mem) = sess.memory in
  List.iter (fun msg ->
    let role_str = role_to_string msg.role in
    Format.fprintf fmt "@[<v>[%s]: %s@]@." role_str msg.content
  ) (M.get mem)


