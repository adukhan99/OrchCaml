(** OrchCaml.Provider — Abstract LLM backend interface.

    Every provider (Ollama, OpenAI, Anthropic, etc.) implements this
    signature. Pipelines are written against [PROVIDER], not a concrete
    module, so swapping backends is a one-liner. *)

open Types

(** The core interface that every backend must satisfy. *)
module type PROVIDER = sig

  (** Provider-specific configuration (URLs, credentials, model name, etc.) *)
  type config

  (** Human-readable provider name, e.g. "ollama" or "openai". *)
  val name : string

  (** [complete cfg history] sends [history] to the LLM and returns the
      full response string when it is ready. *)
  val complete
    :  config
    -> chat_message list
    -> string result_with_meta Lwt.t

  (** [stream cfg history] sends [history] and returns a stream of token
      chunks as they arrive. Useful for live display in the TUI. *)
  val stream
    :  config
    -> chat_message list
    -> (string Lwt_stream.t * string result_with_meta Lwt.t)

  (** [list_models cfg] returns the models available from this provider. *)
  val list_models : config -> string list Lwt.t

end

(** A packed (existential) provider — lets you store providers of different
    types in the same data structure. *)
type packed_provider =
  | Provider : (module PROVIDER with type config = 'c) * 'c -> packed_provider

let complete_packed (Provider ((module P), cfg)) msgs =
  P.complete cfg msgs

let stream_packed (Provider ((module P), cfg)) msgs =
  P.stream cfg msgs

let list_models_packed (Provider ((module P), cfg)) =
  P.list_models cfg

let name_of_packed (Provider ((module P), _)) = P.name
