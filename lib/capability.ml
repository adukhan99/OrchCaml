(** Model capability table — the spine of low-cost-model adaptation.

    The provider registry knows where every backend lives; this table
    knows what the model in front of us can actually do.  Behaviour is
    derived from it rather than from global constants: which tools to
    expose, whether to activate the text tool-call fallback, when to
    compact, how the request prefix should be cached.

    Capabilities are per-model, not per-provider (OpenRouter alone spans
    the whole range), so lookup is by model name with a conservative
    default for unknown models: assume a small context and flaky tool
    calling.  Being wrong in that direction degrades gracefully; the
    opposite direction fails.

    Users can override any field from [config.toml]:

    {[
      [capabilities."my-local-model"]
      context_window = 32768
      tool_calling = "text"
    ]}

    where the table key is matched as a case-insensitive substring of
    the model name. *)

type tool_fidelity =
  | Native     (** reliable native tool_calls — trust the API field *)
  | Flaky      (** nominally supports tool calls but often emits them as
                   text; keep the fallback parser armed *)
  | Text_only  (** no native tool calling — text protocol is the only
                   channel *)

type cache_kind =
  | Cache_none       (** no prompt caching *)
  | Cache_automatic  (** provider caches prefixes transparently
                         (OpenAI, DeepSeek) — byte-stable prefixes pay
                         off with no request changes *)
  | Cache_explicit   (** provider needs explicit breakpoints
                         (Anthropic [cache_control]) *)

type t = {
  context_window       : int;         (** tokens the model can attend to *)
  tool_calling         : tool_fidelity;
  streaming_tool_calls : bool;        (** tool-call deltas arrive over SSE *)
  cache                : cache_kind;
  requests_per_minute  : int option;  (** known free-tier ceiling, if any *)
  tokens_per_minute    : int option;
}

(** The default for a model we know nothing about.  Small context and
    flaky tool calling: the failure mode of underestimating a strong
    model is mild inefficiency, the failure mode of overestimating a
    weak one is silent breakage. *)
let conservative = {
  context_window       = 8192;
  tool_calling         = Flaky;
  streaming_tool_calls = false;
  cache                = Cache_none;
  requests_per_minute  = None;
  tokens_per_minute    = None;
}

let tool_fidelity_of_string = function
  | "native"          -> Some Native
  | "flaky"           -> Some Flaky
  | "text" | "none"   -> Some Text_only
  | _                 -> None

let tool_fidelity_to_string = function
  | Native    -> "native"
  | Flaky     -> "flaky"
  | Text_only -> "text"

let cache_kind_of_string = function
  | "none"      -> Some Cache_none
  | "automatic" -> Some Cache_automatic
  | "explicit"  -> Some Cache_explicit
  | _           -> None

let cache_kind_to_string = function
  | Cache_none      -> "none"
  | Cache_automatic -> "automatic"
  | Cache_explicit  -> "explicit"

(* ── Built-in table ───────────────────────────────────────────────────── *)

(* Each entry is (substring patterns, capability).  The first entry with
   a pattern found in the lowercased model name wins, so put specific
   names before family catch-alls.  Context windows are the documented
   values at the time of writing; the config override exists precisely
   because tables like this drift. *)

let cloud_frontier ~ctx ~cache = {
  context_window       = ctx;
  tool_calling         = Native;
  streaming_tool_calls = true;
  cache;
  requests_per_minute  = None;
  tokens_per_minute    = None;
}

let builtin : (string list * t) list = [
  (* Anthropic — current generation is 1M-context; Haiku 4.5 is 200k.
     Caching needs explicit cache_control breakpoints. *)
  (["claude-fable-5"; "claude-mythos"; "claude-opus-5"; "claude-sonnet-5";
    "claude-opus-4"; "claude-sonnet-4"],
   cloud_frontier ~ctx:1_000_000 ~cache:Cache_explicit);
  (["claude-haiku"], cloud_frontier ~ctx:200_000 ~cache:Cache_explicit);
  (["claude"],       cloud_frontier ~ctx:200_000 ~cache:Cache_explicit);

  (* OpenAI — automatic prefix caching since gpt-4o. *)
  (["gpt-4o"; "gpt-4.1"; "o3"; "o4-mini"],
   cloud_frontier ~ctx:128_000 ~cache:Cache_automatic);
  (["gpt-5"], cloud_frontier ~ctx:400_000 ~cache:Cache_automatic);
  (["gpt-"],  cloud_frontier ~ctx:128_000 ~cache:Cache_automatic);

  (* DeepSeek — automatic context caching on disk, very cheap hits. *)
  (["deepseek"], cloud_frontier ~ctx:128_000 ~cache:Cache_automatic);

  (* Google *)
  (["gemini"], cloud_frontier ~ctx:1_000_000 ~cache:Cache_automatic);

  (* xAI *)
  (["grok"], cloud_frontier ~ctx:131_072 ~cache:Cache_none);

  (* Mistral hosted *)
  (["mistral-large"; "mistral-small"], cloud_frontier ~ctx:128_000 ~cache:Cache_none);

  (* Large hosted open weights: reliable native tool calling. *)
  (["llama-3.3-70b"; "llama-3.1-70b"; "llama-3.1-405b"],
   cloud_frontier ~ctx:128_000 ~cache:Cache_none);

  (* gpt-oss is tool-call tuned. *)
  (["gpt-oss"],
   { conservative with context_window = 128_000; tool_calling = Native;
                       streaming_tool_calls = true });

  (* Small local weights: the population C2/H5 exist for.  Nominal
     context windows are large but practical local deployments
     (Ollama's default num_ctx, VRAM limits) are far smaller — the
     conservative figures here are deliberate. *)
  (["llama3.2:1b"; "llama3.2:3b"; "llama-3.2-1b"; "llama-3.2-3b"],
   { conservative with context_window = 8_192; tool_calling = Flaky });
  (["llama3.2"; "llama3.1:8b"; "llama-3.1-8b"],
   { conservative with context_window = 16_384; tool_calling = Flaky });
  (["qwen3:4b"; "qwen3-4b"],
   { conservative with context_window = 32_768; tool_calling = Flaky });
  (["qwen3"; "qwen2.5"],
   { conservative with context_window = 32_768; tool_calling = Native });
]

(* ── Config overrides ─────────────────────────────────────────────────── *)

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  if n = 0 || n > h then n = 0
  else
    let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
    go 0

(** Apply one [capabilities."pattern"] TOML table over [base].  Only the
    fields present in the table are patched. *)
let apply_override fields base =
  let int_field key d =
    match Config.assoc_int_opt fields key with Some v -> v | None -> d in
  let int_opt_field key d =
    match Config.assoc_int_opt fields key with Some v -> Some v | None -> d in
  let tool_calling =
    match Config.assoc_string_opt fields "tool_calling" with
    | Some s -> (match tool_fidelity_of_string (String.lowercase_ascii s) with
                 | Some f -> f
                 | None -> base.tool_calling)
    | None -> base.tool_calling
  in
  let cache =
    match Config.assoc_string_opt fields "cache" with
    | Some s -> (match cache_kind_of_string (String.lowercase_ascii s) with
                 | Some c -> c
                 | None -> base.cache)
    | None -> base.cache
  in
  { context_window       = int_field "context_window" base.context_window;
    tool_calling;
    streaming_tool_calls =
      (match Config.assoc_bool_opt fields "streaming_tool_calls" with
       | Some b -> b | None -> base.streaming_tool_calls);
    cache;
    requests_per_minute  = int_opt_field "requests_per_minute" base.requests_per_minute;
    tokens_per_minute    = int_opt_field "tokens_per_minute" base.tokens_per_minute;
  }

(** Look up the capability record for [model].  Resolution order:
    built-in table (first matching pattern), then any matching
    [capabilities."pattern"] config override patched on top, then the
    conservative default when nothing matches. *)
let lookup model =
  let m = String.lowercase_ascii (String.trim model) in
  let base =
    match
      List.find_opt
        (fun (pats, _) -> List.exists (fun p -> contains ~needle:p m) pats)
        builtin
    with
    | Some (_, cap) -> cap
    | None -> conservative
  in
  List.fold_left
    (fun acc (pattern, fields) ->
       if contains ~needle:(String.lowercase_ascii pattern) m
       then apply_override fields acc
       else acc)
    base (Config.get_capability_overrides ())

(* ── Token estimation ─────────────────────────────────────────────────── *)

(* A chars/4 heuristic.  Deliberately dependency-free: the point is a
   sane compaction trigger and a "will this fit?" answer, not billing
   accuracy.  English prose runs ~4 chars/token; code and JSON a little
   lower, so this over-estimates slightly — the safe direction. *)

let estimate_tokens s = (String.length s + 3) / 4

(** Per-message overhead (role framing, separators) in tokens. *)
let message_overhead = 4

let estimate_message_tokens (msg : Types.chat_message) =
  let content = estimate_tokens msg.content in
  let tool_calls =
    match msg.tool_calls with
    | None -> 0
    | Some tcs ->
      List.fold_left
        (fun acc (tc : Types.tool_call) ->
           acc + estimate_tokens tc.name + estimate_tokens tc.args)
        0 tcs
  in
  message_overhead + content + tool_calls

let estimate_history_tokens msgs =
  List.fold_left (fun acc m -> acc + estimate_message_tokens m) 0 msgs
