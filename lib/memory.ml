(** OrchCaml.Memory — Conversation memory management.

    Provides pluggable memory backends:
    - [Buffer]: fixed sliding window of N most recent messages.
    - [Summary]: compresses history beyond a threshold using an LLM call.
    - [Noop]: no memory — each call is stateless.

    The module type [MEMORY] is a first-class interface so you can
    swap memory strategies without changing session or chain code. *)

open Types

(** The abstract memory interface. *)
module type MEMORY = sig
  type t

  (** Create a new empty memory store. *)
  val create : unit -> t

  (** Add a message to memory. *)
  val add : t -> chat_message -> unit

  (** Retrieve the current message history to prepend to the next LLM call. *)
  val get : t -> chat_message list

  (** Clear all stored messages. *)
  val clear : t -> unit

  (** Number of messages currently stored. *)
  val length : t -> int

  (** Serialise to JSON for export/save. *)
  val to_json : t -> Yojson.Safe.t

  (** Deserialise from JSON. *)
  val of_json : Yojson.Safe.t -> t
end

(** --- Buffer Memory --- *)

(** Keeps the system prompt (if any) plus the [window] most recent turns. *)
module Buffer : sig
  include MEMORY
  val create : ?window:int -> unit -> t
end = struct
  type t = {
    mutable messages : chat_message Queue.t;
    window           : int;
  }

  let create ?(window = 20) () =
    { messages = Queue.create (); window }

  let add mem msg =
    Queue.push msg mem.messages;
    (* Trim to window, but always keep system messages. *)
    while Queue.length mem.messages > mem.window do
      (* Pop from the front; discard unless it's a system message. *)
      let oldest = Queue.pop mem.messages in
      match oldest.role with
      | System ->
        (* Re-insert system message at the front — it must always be kept. *)
        let q = Queue.create () in
        Queue.push oldest q;
        Queue.transfer mem.messages q;
        Queue.transfer q mem.messages
      | _ -> ()
    done

  let get mem =
    Queue.fold (fun acc m -> acc @ [m]) [] mem.messages

  let clear mem =
    Queue.clear mem.messages

  let length mem = Queue.length mem.messages

  let to_json mem =
    `List (Queue.fold (fun acc m -> acc @ [chat_message_to_json m]) [] mem.messages)

  let of_json json =
    let msgs = Yojson.Safe.Util.to_list json |> List.map chat_message_of_json in
    let mem = create () in
    List.iter (add mem) msgs;
    mem
end

(** --- No-op Memory --- *)

(** Stateless — every call sees an empty history. *)
module Noop : MEMORY = struct
  type t = unit

  let create () = ()
  let add () _msg = ()
  let get () = []
  let clear () = ()
  let length () = 0
  let to_json () = `List []
  let of_json _ = ()
end

(** --- Summary Memory --- *)

(** When the buffer exceeds [max_messages], older messages are summarised
    into a single System message using an LLM call. *)
module Summary = struct
  type t = {
    buf          : Buffer.t;
    max_messages : int;
    mutable summary : string option;
  }

  let create ?(max_messages = 40) () =
    { buf = Buffer.create ~window:max_messages ();
      max_messages;
      summary = None }

  let add mem msg =
    Buffer.add mem.buf msg

  let get mem =
    let msgs = Buffer.get mem.buf in
    match mem.summary with
    | None   -> msgs
    | Some s ->
      (* Prepend summary as a system message. *)
      let sum_msg = system_msg ("[Conversation summary]: " ^ s) in
      sum_msg :: msgs

  let clear mem =
    Buffer.clear mem.buf;
    mem.summary <- None

  let length mem = Buffer.length mem.buf

  (** [compress ~complete mem] uses [complete] to summarise old history.
      [complete] should be a function [chat_message list -> string Lwt.t].
      Call this when [length mem >= mem.max_messages]. *)
  let compress ~complete mem =
    let open Lwt.Syntax in
    let msgs = Buffer.get mem.buf in
    let prompt = [
      system_msg "You are a summarisation assistant. Summarise the following \
                  conversation history concisely, preserving all important \
                  facts, decisions, and context. Output only the summary.";
      user_msg (String.concat "\n\n" (List.map (fun m ->
        Printf.sprintf "[%s]: %s" (role_to_string m.role) m.content) msgs));
    ] in
    let* summary = complete prompt in
    mem.summary <- Some summary;
    Buffer.clear mem.buf;
    Lwt.return_unit

  let to_json mem =
    `Assoc [
      ("messages", Buffer.to_json mem.buf);
      ("summary",  match mem.summary with
                   | None   -> `Null
                   | Some s -> `String s);
    ]

  let of_json json =
    let open Yojson.Safe.Util in
    let mem = create () in
    let msgs = json |> member "messages" |> to_list |> List.map chat_message_of_json in
    List.iter (add mem) msgs;
    mem.summary <- (match json |> member "summary" with
                    | `Null     -> None
                    | `String s -> Some s
                    | _         -> None);
    mem
end
