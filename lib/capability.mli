(** Model capability table.

    Records what a given model can do — context window, tool-calling
    fidelity, cache semantics, known free-tier limits — so the harness
    can derive behaviour from the model in front of it instead of from
    global constants.  Lookup is by model name (case-insensitive
    substring match) with a conservative default for unknown models;
    every field is overridable from [config.toml] via
    [capabilities."pattern"] tables. *)

type tool_fidelity =
  | Native     (** reliable native tool_calls — trust the API field *)
  | Flaky      (** nominally supports tool calls but often emits them as
                   text; keep the fallback parser armed *)
  | Text_only  (** no native tool calling — text protocol only *)

type cache_kind =
  | Cache_none       (** no prompt caching *)
  | Cache_automatic  (** provider caches prefixes transparently *)
  | Cache_explicit   (** provider needs explicit breakpoints *)

type t = {
  context_window       : int;         (** tokens the model can attend to *)
  tool_calling         : tool_fidelity;
  streaming_tool_calls : bool;        (** tool-call deltas arrive over SSE *)
  cache                : cache_kind;
  requests_per_minute  : int option;  (** known free-tier ceiling, if any *)
  tokens_per_minute    : int option;
}

(** The default for an unknown model: small context, flaky tool calling.
    Wrong in that direction degrades gracefully; the opposite fails. *)
val conservative : t

(** [lookup model] resolves the capability record for [model]: built-in
    table first, then matching [capabilities."pattern"] config overrides
    patched on top, else {!conservative}. *)
val lookup : string -> t

val tool_fidelity_of_string : string -> tool_fidelity option
val tool_fidelity_to_string : tool_fidelity -> string
val cache_kind_of_string : string -> cache_kind option
val cache_kind_to_string : cache_kind -> string

(** The reduced tool surface for low-capability models; always
    includes [finish], the completion protocol. *)
val core_tool_names : string list

(** [use_core_profile ~profile cap] — whether to expose only
    {!core_tool_names}.  [profile] is the [tool_profile] config value:
    ["core"]/["full"] force it, anything else derives from capability
    (non-native tool calling, or a context window under 16k). *)
val use_core_profile : profile:string -> t -> bool

(** Cheap chars/4 token estimate — a compaction trigger, not a billing
    figure.  Slightly over-estimates code and JSON, the safe direction. *)
val estimate_tokens : string -> int

(** Estimate for one message including role framing and tool-call args. *)
val estimate_message_tokens : Types.chat_message -> int

(** Sum of {!estimate_message_tokens} over a history. *)
val estimate_history_tokens : Types.chat_message list -> int
