(** OrchCaml.Session — Stateful multi-turn conversation sessions.

    A [session] combines:
    - a [Provider.packed_provider] (the LLM backend),
    - a [Memory.Buffer.t] (conversation history),
    - metadata (model name, system prompt, generation options). *)

open Types

(** Session configuration. *)
type config = {
  model       : string;
  system      : string option;     (** Optional system prompt *)
  options     : gen_options;
  memory_size : int;               (** Sliding window size, 0 = unlimited *)
}

let default_config model = {
  model;
  system      = None;
  options     = default_options;
  memory_size = 40;
}

(** A live session. *)
type t = {
  cfg      : config;
  provider : Provider.packed_provider;
  memory   : Memory.Buffer.t;
  turn_idx : int;
  tools    : Tool.packed_tool list;
}

(** Create a new session. *)
let create ?(config = fun m -> default_config m) ?(tools=[]) model provider =
  let cfg = config model in
  let window = if cfg.memory_size = 0 then max_int else cfg.memory_size in
  {
    cfg;
    provider;
    memory = Memory.Buffer.create ~window ();
    turn_idx = 0;
    tools;
  }

(** [set_system sess text] updates the system prompt and returns the new session. *)
let set_system sess text =
  let cfg =
    if String.trim text = "" then
      { sess.cfg with system = None }
    else
      { sess.cfg with system = Some text }
  in
  { sess with cfg }

(** [clear sess] clears conversation history (not the config) and returns the new session. *)
let clear sess =
  { sess with memory = Memory.Buffer.clear sess.memory; turn_idx = 0 }

(** [history sess] returns the current message history. *)
let history sess = Memory.Buffer.get sess.memory

(* ------------------------------------------------------------------ *)
(*  Shared helpers                                                      *)
(* ------------------------------------------------------------------ *)

(** Build the full message list to send to the LLM, prepending the
    system prompt if it is not already the first message. *)
let history_for_llm sess =
  match sess.cfg.system with
  | None     -> Memory.Buffer.get sess.memory
  | Some sys ->
    let sm = system_msg sys in
    (match Memory.Buffer.get sess.memory with
     | { role = System; _ } :: _ -> Memory.Buffer.get sess.memory
     | rest -> sm :: rest)

(** [execute_tool_calls sess tcs] dispatches all requested tool calls in
    parallel-sequential order (Lwt_list.map_s) and returns the resulting
    [Tool] messages. *)
let execute_tool_calls sess tcs =
  let open Lwt.Syntax in
  Lwt_list.map_s (fun tc ->
    match List.find_opt (fun t -> Tool.name_of_packed t = tc.name) sess.tools with
    | None ->
      Lwt.return (tool_msg tc.id (Printf.sprintf "Tool %s not found" tc.name))
    | Some packed ->
      let* output_str = Tool.dispatch packed tc.args in
      Lwt.return (tool_msg tc.id output_str)
  ) tcs

(** After the LLM responds with [reply], store it in memory, dispatch
    any tool calls, and return:
    - [None]      — the loop should continue (tool calls were made)
    - [Some]      — the loop should stop (plain text reply).

    The returned session is always updated with the latest memory. *)
type step_outcome =
  | Continue of t          (** tool calls dispatched; keep looping *)
  | Done     of t * string (** terminal reply — updated session + content *)

let run_turn_step sess (reply : chat_message) =
  let open Lwt.Syntax in
  let final_memory = Memory.Buffer.add sess.memory reply in
  let new_sess = { sess with memory = final_memory; turn_idx = sess.turn_idx + 1 } in
  match reply.tool_calls with
  | Some tcs when tcs <> [] ->
    let* tool_responses = execute_tool_calls new_sess tcs in
    let memory_with_tools =
      List.fold_left Memory.Buffer.add new_sess.memory tool_responses
    in
    Lwt.return (Continue { new_sess with memory = memory_with_tools })
  | _ ->
    Lwt.return (Done (new_sess, reply.content))

(* ------------------------------------------------------------------ *)
(*  Non-streaming conversation loop                                     *)
(* ------------------------------------------------------------------ *)

let rec run_conversations sess =
  let open Lwt.Syntax in
  let* result = Provider.complete_packed ~tools:sess.tools sess.provider (history_for_llm sess) in
  let* outcome = run_turn_step sess result.value in
  match outcome with
  | Continue sess' -> run_conversations sess'
  | Done (sess', content) -> Lwt.return (sess', content)

(** [turn sess user_input] sends a user message and returns the updated
    session and the assistant response as a plain string (non-streaming). *)
let turn sess user_input =
  let user = user_msg user_input in
  let sess' = { sess with memory = Memory.Buffer.add sess.memory user } in
  run_conversations sess'

(* ------------------------------------------------------------------ *)
(*  Streaming conversation loop                                         *)
(* ------------------------------------------------------------------ *)

let rec run_conversations_stream sess ~on_token =
  let open Lwt.Syntax in
  let stream, meta_promise =
    Provider.stream_packed ~tools:sess.tools sess.provider (history_for_llm sess)
  in
  let* () = Lwt_stream.iter (fun token -> on_token token) stream in
  let* result_with_meta = meta_promise in
  let* outcome = run_turn_step sess result_with_meta.value in
  match outcome with
  | Continue sess' -> run_conversations_stream sess' ~on_token
  | Done (sess', content) -> Lwt.return (sess', content)

(** [turn_stream sess user_input ~on_token] is like [turn] but streams
    tokens via [on_token] as they arrive. Returns the updated session and
    full response when the stream is exhausted. *)
let turn_stream sess user_input ~on_token =
  let user = user_msg user_input in
  let sess' = { sess with memory = Memory.Buffer.add sess.memory user } in
  run_conversations_stream sess' ~on_token

(* ------------------------------------------------------------------ *)
(*  Export / pretty-print                                               *)
(* ------------------------------------------------------------------ *)

(** Serialise the session history to JSON. *)
let export_json sess =
  `Assoc [
    ("model",    `String sess.cfg.model);
    ("turn_idx", `Int sess.turn_idx);
    ("system",   (match sess.cfg.system with
                  | None -> `Null | Some s -> `String s));
    ("history",  Memory.Buffer.to_json sess.memory);
  ]

(** Pretty-print the session history. *)
let pp_history fmt sess =
  List.iter (fun msg ->
    let role_str = role_to_string msg.role in
    Format.fprintf fmt "@[<v>[%s]: %s@]@." role_str msg.content
  ) (Memory.Buffer.get sess.memory)
