(** Data-driven provider registry.

    One table describes every backend Caravan can talk to out of the box:
    where it lives, which environment variable holds its key, what model
    to use when the user names none, and a curated ladder of models from
    ~1B-parameter local weights to frontier systems.

    Every entry speaks the OpenAI chat-completions dialect (Anthropic and
    Gemini via their official OpenAI-compatible endpoints), so a single
    engine — [Openai_compatible] — serves them all. *)

open Caravan.Provider

type kind = Local | Cloud

type entry = {
  name          : string;
  aliases       : string list;
  kind          : kind;
  base_url      : string;        (** default; overridable via config/flag *)
  key_env       : string option; (** env var conventionally holding the key *)
  requires_key  : bool;
  default_model : string;
  (** Representative models, smallest → largest, for discovery UX. *)
  model_hints   : string list;
  notes         : string;
}

let entries : entry list = [
  { name = "ollama"; aliases = ["local"]; kind = Local;
    base_url = "http://127.0.0.1:11434";
    key_env = None; requires_key = false;
    default_model = "llama3.2";
    model_hints = ["llama3.2:1b"; "llama3.2"; "qwen3:4b"; "qwen3:8b"; "gpt-oss:20b"];
    notes = "Local models via Ollama (https://ollama.com)." };

  { name = "llama_cpp"; aliases = ["llamacpp"; "llama-cpp"]; kind = Local;
    base_url = "http://127.0.0.1:8080/v1";
    key_env = None; requires_key = false;
    default_model = "default";
    model_hints = ["(whatever GGUF the server has loaded)"];
    notes = "llama.cpp server (llama-server --port 8080)." };

  { name = "vllm"; aliases = []; kind = Local;
    base_url = "http://127.0.0.1:8000/v1";
    key_env = None; requires_key = false;
    default_model = "default";
    model_hints = ["(model passed to vllm serve)"];
    notes = "vLLM OpenAI-compatible server — the HPC workhorse." };

  { name = "lmstudio"; aliases = ["lm_studio"; "lm-studio"]; kind = Local;
    base_url = "http://127.0.0.1:1234/v1";
    key_env = None; requires_key = false;
    default_model = "default";
    model_hints = ["(model loaded in LM Studio)"];
    notes = "LM Studio local server." };

  { name = "openai"; aliases = ["oai"]; kind = Cloud;
    base_url = "https://api.openai.com/v1";
    key_env = Some "OPENAI_API_KEY"; requires_key = true;
    default_model = "gpt-4o-mini";
    model_hints = ["gpt-4o-mini"; "gpt-4o"; "o3-mini"];
    notes = "OpenAI." };

  { name = "anthropic"; aliases = ["claude"]; kind = Cloud;
    base_url = "https://api.anthropic.com/v1";
    key_env = Some "ANTHROPIC_API_KEY"; requires_key = true;
    default_model = "claude-sonnet-5";
    model_hints = ["claude-haiku-4-5"; "claude-sonnet-5"; "claude-opus-5"];
    notes = "Anthropic Claude via its OpenAI-compatible endpoint. \
             Hardcoded ladders drift: `caravan models` is authoritative." };

  { name = "groq"; aliases = []; kind = Cloud;
    base_url = "https://api.groq.com/openai/v1";
    key_env = Some "GROQ_API_KEY"; requires_key = true;
    default_model = "llama-3.3-70b-versatile";
    model_hints = ["llama-3.1-8b-instant"; "llama-3.3-70b-versatile"];
    notes = "Groq — very fast open-weight inference." };

  { name = "openrouter"; aliases = ["or"]; kind = Cloud;
    base_url = "https://openrouter.ai/api/v1";
    key_env = Some "OPENROUTER_API_KEY"; requires_key = true;
    default_model = "meta-llama/llama-3.3-70b-instruct";
    model_hints = ["(hundreds — see openrouter.ai/models)";
                   "(append :free for zero-cost variants)"];
    notes = "OpenRouter — one key, most models. Models with a :free \
             suffix cost nothing (rate-limited)." };

  { name = "cerebras"; aliases = []; kind = Cloud;
    base_url = "https://api.cerebras.ai/v1";
    key_env = Some "CEREBRAS_API_KEY"; requires_key = true;
    default_model = "llama-3.3-70b";
    model_hints = ["llama3.1-8b"; "llama-3.3-70b"];
    notes = "Cerebras — very fast inference, generous free tier." };

  { name = "github_models"; aliases = ["github"; "gh"]; kind = Cloud;
    base_url = "https://models.github.ai/inference";
    key_env = Some "GITHUB_TOKEN"; requires_key = true;
    default_model = "openai/gpt-4o-mini";
    model_hints = ["openai/gpt-4o-mini"; "meta/llama-3.3-70b-instruct"];
    notes = "GitHub Models — free with any GitHub token (rate-limited)." };

  { name = "nvidia"; aliases = ["nim"]; kind = Cloud;
    base_url = "https://integrate.api.nvidia.com/v1";
    key_env = Some "NVIDIA_API_KEY"; requires_key = true;
    default_model = "meta/llama-3.3-70b-instruct";
    model_hints = ["meta/llama-3.1-8b-instruct"; "meta/llama-3.3-70b-instruct"];
    notes = "NVIDIA NIM — free API credits for development use." };

  { name = "together"; aliases = []; kind = Cloud;
    base_url = "https://api.together.xyz/v1";
    key_env = Some "TOGETHER_API_KEY"; requires_key = true;
    default_model = "meta-llama/Llama-3.3-70B-Instruct-Turbo";
    model_hints = ["meta-llama/Llama-3.3-70B-Instruct-Turbo"];
    notes = "Together AI." };

  { name = "deepseek"; aliases = []; kind = Cloud;
    base_url = "https://api.deepseek.com/v1";
    key_env = Some "DEEPSEEK_API_KEY"; requires_key = true;
    default_model = "deepseek-chat";
    model_hints = ["deepseek-chat"; "deepseek-reasoner"];
    notes = "DeepSeek." };

  { name = "mistral"; aliases = []; kind = Cloud;
    base_url = "https://api.mistral.ai/v1";
    key_env = Some "MISTRAL_API_KEY"; requires_key = true;
    default_model = "mistral-small-latest";
    model_hints = ["mistral-small-latest"; "mistral-large-latest"];
    notes = "Mistral AI." };

  { name = "gemini"; aliases = ["google"]; kind = Cloud;
    base_url = "https://generativelanguage.googleapis.com/v1beta/openai";
    key_env = Some "GEMINI_API_KEY"; requires_key = true;
    default_model = "gemini-2.0-flash";
    model_hints = ["gemini-2.0-flash"; "gemini-2.5-pro"];
    notes = "Google Gemini via its OpenAI-compatible endpoint." };

  { name = "xai"; aliases = ["grok"]; kind = Cloud;
    base_url = "https://api.x.ai/v1";
    key_env = Some "XAI_API_KEY"; requires_key = true;
    default_model = "grok-3-mini";
    model_hints = ["grok-3-mini"; "grok-3"];
    notes = "xAI Grok." };
]

let names () = List.map (fun e -> e.name) entries

let find name =
  let n = String.lowercase_ascii (String.trim name) in
  List.find_opt (fun e -> e.name = n || List.mem n e.aliases) entries

(** Resolve an API key for [e]: env var first, then [api_keys.<name>] in
    the TOML config, then legacy [openai_api_key] for the openai entry. *)
let api_key_for (e : entry) =
  match e.key_env with
  | None -> None
  | Some env_var ->
    let legacy_key = if e.name = "openai" then Some "openai_api_key" else None in
    Caravan.Config.get_api_key ~env_var ~name:e.name ?legacy_key ()

(** A model ladder for discovery: weight-class → (provider, model, note). *)
let model_ladder = [
  ("tiny ~1B",     "ollama",    "llama3.2:1b",             "runs on a laptop CPU");
  ("small ~4B",    "ollama",    "qwen3:4b",                "fast local reasoning");
  ("medium ~20B",  "ollama",    "gpt-oss:20b",             "strong local, needs ~16GB");
  ("large ~70B",   "groq",      "llama-3.3-70b-versatile", "open weights, hosted fast");
  ("frontier",     "anthropic", "claude-sonnet-5",         "top-tier agentic coding");
  ("frontier",     "openai",    "gpt-4o",                  "general frontier");
  ("frontier",     "gemini",    "gemini-2.5-pro",          "long-context frontier");
]

exception Unknown_provider of string

let unknown_provider_message name =
  Printf.sprintf
    "Unknown provider '%s'. Supported: %s.\n\
     Use base_url in the config (or --base-url) with provider \"openai\" \
     for any other OpenAI-compatible endpoint."
    name (String.concat ", " (names ()))

(** Build a packed provider for [name].
    [base_url]/[api_key] override the registry/env/config resolution.
    @raise Unknown_provider on an unrecognised name. *)
let make_provider ?base_url ?api_key ?(model : string option) name : packed_provider =
  match find name with
  | None -> raise (Unknown_provider (unknown_provider_message name))
  | Some e ->
    let model = Option.value ~default:e.default_model model in
    let base_url = Option.value ~default:e.base_url base_url in
    let api_key = match api_key with Some _ as k -> k | None -> api_key_for e in
    (match e.name with
     | "ollama" ->
       (* Keep Ollama's URL normalisation (adds /v1 when missing). *)
       Ollama.make_provider ~base_url ~model ()
     | _ ->
       Openai_compatible.make_provider
         ~provider_name:e.name ~base_url ?api_key ~model ())

(** Effective default model for a provider name, falling back to Ollama's
    default when the provider is unknown (callers validate separately). *)
let default_model name =
  match find name with
  | Some e -> e.default_model
  | None -> "llama3.2"
