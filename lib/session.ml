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
  mutable cfg      : config;
  provider         : Provider.packed_provider;
  memory           : Memory.Buffer.t;
  mutable turn_idx : int;
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

(** [set_system sess text] updates the system prompt. *)
let set_system sess text =
  if String.trim text = "" then
    sess.cfg <- { sess.cfg with system = None }
  else
    sess.cfg <- { sess.cfg with system = Some text };
  ()

(** [clear sess] clears conversation history (not the config). *)
let clear sess =
  Memory.Buffer.clear sess.memory;
  sess.turn_idx <- 0

(** [history sess] returns the current message history. *)
let history sess = Memory.Buffer.get sess.memory

(** [turn sess user_input] sends a user message and returns the assistant
    response as a plain string (non-streaming). *)
let turn sess user_input =
  let open Lwt.Syntax in
  let user = user_msg user_input in
  Memory.Buffer.add sess.memory user;
  let history =
    match sess.cfg.system with
    | None     -> Memory.Buffer.get sess.memory
    | Some sys ->
      let sm = system_msg sys in
      (* Only add system message if not already at head *)
      (match Memory.Buffer.get sess.memory with
       | { role = System; _ } :: _ -> Memory.Buffer.get sess.memory
       | rest -> sm :: rest)
  in
  let* result = Provider.complete_packed sess.provider history in
  let reply = assistant_msg result.value in
  Memory.Buffer.add sess.memory reply;
  sess.turn_idx <- sess.turn_idx + 1;
  Lwt.return result.value

(** [turn_stream sess user_input ~on_token] is like [turn] but streams
    tokens via [on_token] as they arrive. Returns the full response
    when the stream is exhausted. *)
let turn_stream sess user_input ~on_token =
  let open Lwt.Syntax in
  let user = user_msg user_input in
  Memory.Buffer.add sess.memory user;
  let history =
    match sess.cfg.system with
    | None     -> Memory.Buffer.get sess.memory
    | Some sys ->
      (match Memory.Buffer.get sess.memory with
       | { role = System; _ } :: _ -> Memory.Buffer.get sess.memory
       | rest -> system_msg sys :: rest)
  in
  let stream, _meta_promise = Provider.stream_packed sess.provider history in
  let buf = Buffer.create 4096 in
  let* () = Lwt_stream.iter (fun token ->
    Buffer.add_string buf token;
    on_token token
  ) stream in
  let full_response = Buffer.contents buf in
  let reply = assistant_msg full_response in
  Memory.Buffer.add sess.memory reply;
  sess.turn_idx <- sess.turn_idx + 1;
  Lwt.return full_response

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
