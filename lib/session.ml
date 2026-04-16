(** OrchCaml.Session — Stateful multi-turn conversation sessions.

    A [session] combines:
    - a [Provider.packed_provider] (the LLM backend),
    - a [Memory.Buffer.t] (conversation history),
    - metadata (model name, system prompt, generation options).

    This is the main entry point for interactive use: the TUI creates
    a session and calls [turn] for each user message. *)

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
}

(** Create a new session. *)
let create ?(config = fun m -> default_config m) model provider =
  let cfg = config model in
  let window = if cfg.memory_size = 0 then max_int else cfg.memory_size in
  {
    cfg;
    provider;
    memory = Memory.Buffer.create ~window ();
    turn_idx = 0;
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

(** [turn sess user_input] sends a user message and returns the updated session and 
    the assistant response as a plain string (non-streaming). *)
let turn sess user_input =
  let open Lwt.Syntax in
  let user = user_msg user_input in
  let memory_with_user = Memory.Buffer.add sess.memory user in
  let history_for_llm =
    match sess.cfg.system with
    | None     -> Memory.Buffer.get memory_with_user
    | Some sys ->
      let sm = system_msg sys in
      (* Only add system message if not already at head *)
      (match Memory.Buffer.get memory_with_user with
       | { role = System; _ } :: _ -> Memory.Buffer.get memory_with_user
       | rest -> sm :: rest)
  in
  let* result = Provider.complete_packed sess.provider history_for_llm in
  let reply = assistant_msg result.value in
  let final_memory = Memory.Buffer.add memory_with_user reply in
  let new_sess = { sess with memory = final_memory; turn_idx = sess.turn_idx + 1 } in
  Lwt.return (new_sess, result.value)

(** [turn_stream sess user_input ~on_token] is like [turn] but streams
    tokens via [on_token] as they arrive. Returns the updated session and 
    full response when the stream is exhausted. *)
let turn_stream sess user_input ~on_token =
  let open Lwt.Syntax in
  let user = user_msg user_input in
  let memory_with_user = Memory.Buffer.add sess.memory user in
  let history_for_llm =
    match sess.cfg.system with
    | None     -> Memory.Buffer.get memory_with_user
    | Some sys ->
      (match Memory.Buffer.get memory_with_user with
       | { role = System; _ } :: _ -> Memory.Buffer.get memory_with_user
       | rest -> system_msg sys :: rest)
  in
  let stream, _meta_promise = Provider.stream_packed sess.provider history_for_llm in
  let buf = Buffer.create 4096 in
  let* () = Lwt_stream.iter (fun token ->
    Buffer.add_string buf token;
    on_token token
  ) stream in
  let full_response = Buffer.contents buf in
  let reply = assistant_msg full_response in
  let final_memory = Memory.Buffer.add memory_with_user reply in
  let new_sess = { sess with memory = final_memory; turn_idx = sess.turn_idx + 1 } in
  Lwt.return (new_sess, full_response)

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
