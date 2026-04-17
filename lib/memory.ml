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

  (** Add a message to memory. Returns the new state. *)
  val add : t -> chat_message -> t

  (** Retrieve the current message history to prepend to the next LLM call. *)
  val get : t -> chat_message list

  (** Clear all stored messages. Returns the new state. *)
  val clear : t -> t

  (** Number of messages currently stored. *)
  val length : t -> int

  (** Serialise to JSON for export/save. *)
  val to_json : t -> Yojson.Safe.t

  (** Deserialise from JSON. *)
  val of_json : Yojson.Safe.t -> t
end

(** --- Buffer Memory --- *)

(** Purely functional deque — two-list representation.
    - [front]: oldest messages, in order oldest→newest.
    - [rear] : newest messages, in *reverse* order newest→oldest.
    Invariant: [List.length rear <= List.length front + 1] (loose).
    [add] is O(1) amortised; [get] is O(n) but allocation-free. *)

(** Keeps the system prompt (if any) plus the [window] most recent turns.

    Uses a functional deque so [add] is O(1) amortised and no intermediate 
    lists are allocated on every insertion. *)
module Buffer : sig
  include MEMORY
  val create : ?window:int -> unit -> t
end = struct
  (** [front] is oldest→newest; [rear] is newest→oldest (reversed). *)
  type t = {
    system_msgs : chat_message list;  (** stored oldest→newest *)
    front       : chat_message list;
    rear        : chat_message list;
    window      : int;
    len         : int;                (** = length front + length rear *)
  }

  let create ?(window = 20) () =
    { system_msgs = []; front = []; rear = []; window; len = 0 }

  (** Rebalance when [rear] is longer than [front]: reverse [rear] onto [front]. *)
  let rebalance dq =
    match dq.front with
    | [] -> { dq with front = List.rev dq.rear; rear = [] }
    | _  -> dq

  (** Drop the oldest message (the head of [front]).  Caller must ensure
      [len > 0]. *)
  let drop_oldest dq =
    let dq = rebalance dq in
    match dq.front with
    | []     -> dq   (* should not happen if len > 0 *)
    | _ :: t -> rebalance { dq with front = t; len = dq.len - 1 }

  let add mem msg =
    match msg.role with
    | System -> { mem with system_msgs = mem.system_msgs @ [msg] }
    | _ ->
      (* Append to rear (O(1)). *)
      let dq = { mem with rear = msg :: mem.rear; len = mem.len + 1 } in
      (* Trim to window. *)
      if dq.len > dq.window then drop_oldest dq
      else dq

  (** Reconstruct ordered history: front @ rev(rear) = oldest→newest. *)
  let get mem =
    mem.system_msgs @ mem.front @ List.rev mem.rear

  let clear mem =
    { mem with system_msgs = []; front = []; rear = []; len = 0 }

  let length mem = List.length mem.system_msgs + mem.len

  let to_json mem =
    `List (List.map chat_message_to_json (get mem))

  let of_json json =
    let msgs = Yojson.Safe.Util.to_list json |> List.map chat_message_of_json in
    let mem = create () in
    List.fold_left add mem msgs
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
    summary      : string option;
  }

  let create ?(max_messages = 40) () =
    { buf = Buffer.create ~window:max_messages ();
      max_messages;
      summary = None }

  let add mem msg =
    { mem with buf = Buffer.add mem.buf msg }

  let get mem =
    let msgs = Buffer.get mem.buf in
    match mem.summary with
    | None   -> msgs
    | Some s ->
      (* Prepend summary as a system message. *)
      let sum_msg = system_msg ("[Conversation summary]: " ^ s) in
      sum_msg :: msgs

  let clear mem =
    { mem with buf = Buffer.clear mem.buf; summary = None }

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
    Lwt.return { mem with summary = Some summary; buf = Buffer.clear mem.buf }

  let to_json mem =
    `Assoc [
      ("messages", Buffer.to_json mem.buf);
      ("summary",  match mem.summary with
                   | None   -> `Null
                   | Some s -> `String s);
    ]

  let of_json json =
    let open Yojson.Safe.Util in
    let msgs = json |> member "messages" |> to_list |> List.map chat_message_of_json in
    let mem = create () in
    let mem = List.fold_left add mem msgs in
    let summary = match json |> member "summary" with
                  | `Null     -> None
                  | `String s -> Some s
                  | _         -> None
    in
    { mem with summary }
end
