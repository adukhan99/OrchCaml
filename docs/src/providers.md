# Providers

Caravan speaks the OpenAI chat-completions dialect to every backend through
one engine (`CaravanProviders.Openai_compatible`), configured by a single
data table (`CaravanProviders.Registry`). Run `caravan providers` to see
this table live, with key status for your environment.

| Name        | Kind  | Key env var          | Default model                     |
|-------------|-------|----------------------|-----------------------------------|
| `ollama`    | local | —                    | `llama3.2`                        |
| `llama_cpp` | local | —                    | `default` (whatever is loaded)    |
| `vllm`      | local | —                    | `default`                         |
| `lmstudio`  | local | —                    | `default`                         |
| `openai`    | cloud | `OPENAI_API_KEY`     | `gpt-4o-mini`                     |
| `anthropic` | cloud | `ANTHROPIC_API_KEY`  | `claude-sonnet-5`                 |
| `groq`      | cloud | `GROQ_API_KEY`       | `llama-3.3-70b-versatile`         |
| `openrouter`| cloud | `OPENROUTER_API_KEY` | `meta-llama/llama-3.3-70b-instruct` |
| `together`  | cloud | `TOGETHER_API_KEY`   | `meta-llama/Llama-3.3-70B-Instruct-Turbo` |
| `deepseek`  | cloud | `DEEPSEEK_API_KEY`   | `deepseek-chat`                   |
| `mistral`   | cloud | `MISTRAL_API_KEY`    | `mistral-small-latest`            |
| `gemini`    | cloud | `GEMINI_API_KEY`     | `gemini-2.0-flash`                |
| `xai`       | cloud | `XAI_API_KEY`        | `grok-3-mini`                     |
| `cerebras`  | cloud | `CEREBRAS_API_KEY`   | `llama-3.3-70b`                   |
| `github_models` | cloud | `GITHUB_TOKEN`   | `openai/gpt-4o-mini`              |
| `nvidia`    | cloud | `NVIDIA_API_KEY`     | `meta/llama-3.3-70b-instruct`     |

Model names drift faster than any table: `caravan models` (live, per
provider) is authoritative, and the [free-tier guide](free-tier.md) covers
the zero-cost entries in detail.

## Key resolution order

1. The provider's environment variable (`ANTHROPIC_API_KEY`, …) — preferred;
2. `[api_keys] <provider> = "…"` in `~/.caravan/config.toml` (file is 0600);
3. legacy `openai_api_key` top-level key (openai only).

## A model for every weight class

`caravan providers --ladder` prints a curated pick per size:

| Class        | Suggestion                     | Note                        |
|--------------|--------------------------------|-----------------------------|
| tiny ~1B     | `ollama / llama3.2:1b`         | runs on a laptop CPU        |
| small ~4B    | `ollama / qwen3:4b`            | fast local reasoning        |
| medium ~20B  | `ollama / gpt-oss:20b`         | strong local, ~16 GB        |
| large ~70B   | `groq / llama-3.3-70b-versatile` | open weights, hosted fast |
| frontier     | `anthropic / claude-sonnet-5`, `openai / gpt-4o`, `gemini / gemini-2.5-pro` | |

## Notes and caveats

- **Anthropic** and **Gemini** are reached through their official
  OpenAI-compatible endpoints, so tool calling and streaming work through
  the same code path as everyone else. A few provider-specific parameters
  (e.g. Anthropic's fine-grained thinking controls) are not exposed through
  that compatibility surface; if you need them, add a bespoke provider
  module — the `PROVIDER` signature is four functions.
- **Any other OpenAI-compatible server** (text-generation-inference,
  llamafile, a lab-internal gateway): use `--base-url` (or `base_url` in
  the config) together with `-p openai`, plus `OPENAI_API_KEY` if the
  gateway wants auth.
- **TLS**: certificates are verified against the system CA store, with
  hostname checking. `CARAVAN_TLS_INSECURE=1` disables verification for
  self-signed lab endpoints (a warning is printed).
