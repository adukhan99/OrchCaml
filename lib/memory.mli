open Types

val with_summary : string option -> chat_message list -> chat_message list

module type MEMORY = sig
  type t
  val create   : unit -> t
  val add      : t -> chat_message -> t
  val get      : t -> chat_message list
  val clear    : t -> t
  val length   : t -> int
  val set_window : t -> int -> t

  (** [map_recent t ~keep f] applies [f] to every non-system message
      except the newest [keep].  Session uses it to truncate tool
      outputs exactly once, as they age out of the recent window, so a
      message's serialised bytes never change afterwards — the
      precondition for prompt-cache prefix stability. *)
  val map_recent : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json  : t -> Yojson.Safe.t
  val of_json  : Yojson.Safe.t -> t
end

type packed_memory = Mem : (module MEMORY with type t = 'a) * 'a -> packed_memory

module Ring : sig
  type t
  val make        : ?window:int -> unit -> t
  val create      : unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
end

module Noop : MEMORY

module Summary : sig
  type t
  val create      : ?max_messages:int -> unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
  val compress    : complete:(chat_message list -> string) -> t -> t
end

module Hierarchical : sig
  type t
  val no_op_summarizer : chat_message list -> string
  val create      : ?max_short:int -> summarizer:(chat_message list -> string) -> unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
end

module SummaryMemory : MEMORY with type t = Summary.t

