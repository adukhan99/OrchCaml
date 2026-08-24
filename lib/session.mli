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


val default_config : string -> config

type spinner_config = {
  enabled : bool;
  get_verb : string -> string;
}

val default_spinner_config : unit -> spinner_config

type t

val create : ?config:(string -> config) -> ?tools:Tool.packed_tool list -> string -> Provider.packed_provider -> t

val set_system : t -> string -> t
val set_memory_size : t -> int -> t
val set_max_tool_output_len : t -> int option -> t
val set_auto_summarize : t -> bool -> t
val set_options : t -> (gen_options -> gen_options) -> t
val with_spinner_config : spinner_config -> t -> t
val clear : t -> t
val add_messages : t -> chat_message list -> t
val with_provider : t -> Provider.packed_provider -> t
val tools : t -> Tool.packed_tool list

val with_tools : t -> Tool.packed_tool list -> t
(** Replace the session's tool set (e.g. after the plugin composition
    changed) without touching history or options. *)
val config : t -> config
val provider : t -> Provider.packed_provider
val turn_idx : t -> int
val with_model : t -> string -> t

val history : t -> chat_message list
val history_for_llm : t -> chat_message list

val run_conversations : ?max_turns:int -> ?on_turn:(int -> int -> unit) -> ?on_step:(t -> unit) -> _ Eio.Net.t -> _ Eio.Time.clock -> t -> t * chat_message result_with_meta
val run_conversations_stream : ?max_turns:int -> ?on_turn:(int -> int -> unit) -> ?on_step:(t -> unit) -> _ Eio.Net.t -> _ Eio.Time.clock -> t -> on_token:(string -> unit) -> t * chat_message result_with_meta

val turn : _ Eio.Net.t -> _ Eio.Time.clock -> t -> string -> t * chat_message result_with_meta
val turn_stream : _ Eio.Net.t -> _ Eio.Time.clock -> t -> string -> on_token:(string -> unit) -> t * chat_message result_with_meta

val summarise : ?prompt_fn:(chat_message list -> string) -> _ Eio.Net.t -> _ Eio.Time.clock -> t -> t * string

val export_json : t -> Yojson.Safe.t
val of_json : provider:Provider.packed_provider -> ?tools:Tool.packed_tool list -> Yojson.Safe.t -> (t, string) result
val save_checkpoint : ?path:string -> t -> (string, string) result
val load_checkpoint : provider:Provider.packed_provider -> ?tools:Tool.packed_tool list -> ?path:string -> unit -> (t, string) result
val pp_history : Format.formatter -> t -> unit

