(** Default system prompt and environment preamble.

    Layered prompt assembly for out-of-box competence on small and
    free-tier models: a base harness contract, a tool-call-format layer
    included only when the model's native tool calling is unreliable,
    and an environment preamble (cwd, OS, date, git state, shallow
    listing).

    The preamble must be assembled once per session — it heads the
    request prefix, and prompt caching is strict byte-prefix matching. *)

(** The base agent prompt: act via tools, verify, error discipline, the
    [finish] completion protocol. *)
val base : unit -> string

(** Tool-call format guidance for the text protocol, or [None] for
    models with reliable native tool calling. *)
val tool_format_layer : Capability.t -> string option

(** Environment context (cwd, OS, date, git branch and dirty state,
    shallow directory listing). Call once at session construction;
    never rebuild per turn. *)
val environment_preamble : unit -> string

(** [compose ?capability ?user_system ?replace ()] assembles the final
    system prompt. With [replace = true] the result is exactly
    [user_system] (possibly [None]); otherwise the shipped layers with
    [user_system] appended last. Defaults: {!Capability.conservative},
    no user text, [replace = false]. *)
val compose :
  ?capability:Capability.t ->
  ?user_system:string ->
  ?replace:bool ->
  unit ->
  string option
