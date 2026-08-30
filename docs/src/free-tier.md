# Getting Started for Free

You can run a working Caravan agent without paying anyone. This page
lists the zero-cost routes, roughly in order of "least setup first",
and the settings that make cheap models behave.

## Free routes

### Local models (no key, no meter)

```bash
ollama pull llama3.2        # or qwen3:4b, gpt-oss:20b …
caravan -p ollama -m llama3.2
```

Anything Ollama, llama.cpp, vLLM, or LM Studio can load works, on your
hardware, with no rate limits. Small local models are the weakest
*models* on this page but the strongest *deal*: unlimited requests.

### GitHub Models — free with a token you already have

Any GitHub account token works (`GITHUB_TOKEN`), rate-limited but free:

```bash
export GITHUB_TOKEN=ghp_…
caravan -p github_models -m openai/gpt-4o-mini
```

### OpenRouter `:free` models

One key, hundreds of models, and every model with a `:free` suffix
costs nothing (about 20 requests/minute):

```bash
export OPENROUTER_API_KEY=sk-or-…
caravan -p openrouter -m "meta-llama/llama-3.3-70b-instruct:free"
```

Browse the current list at [openrouter.ai/models](https://openrouter.ai/models?max_price=0).

### Groq, Cerebras, NVIDIA NIM, Gemini

All four offer free tiers or free development credits with a signup:

| Provider   | Sign up at            | Notes                              |
|------------|-----------------------|------------------------------------|
| `groq`     | console.groq.com      | very fast 70B-class open weights   |
| `cerebras` | cloud.cerebras.ai     | very fast, generous daily quota    |
| `nvidia`   | build.nvidia.com      | free API credits, wide catalogue   |
| `gemini`   | aistudio.google.com   | free tier on flash models          |

## Settings that matter on a free tier

Caravan derives most of this automatically from its capability table
(see below), but these are the levers:

```toml
# ~/.caravan/config.toml

# Requests are the scarce resource. Rate-limit hits (429) honour the
# server's Retry-After automatically; "high" retries hardest.
provider_retry = "medium"

# Recognise tool calls that small models emit as text (default "auto").
tool_call_mode = "auto"

# Reduced tool surface for small-context models (default "auto").
tool_profile = "auto"

# Route compaction summaries to a cheap/free model so they don't burn
# the working model's quota.
summarize_model = "meta-llama/llama-3.3-70b-instruct:free"
```

## Telling Caravan about your model

Unknown models get conservative defaults (8k context, tool calling
treated as unreliable). If you know better, say so:

```toml
[capabilities."my-local-model"]
context_window = 32768
tool_calling = "native"     # native | flaky | text
requests_per_minute = 20
```

The pattern is matched as a case-insensitive substring of the model
name; every field is optional and patches over the built-in table.

## What the harness already does for you

- A default system prompt and environment preamble, so small models
  don't waste metered requests rediscovering the working directory.
- Text tool-call recovery: a model that prints
  `{"tool": "ls", "arguments": {}}` instead of using native tool calls
  still works (auditable via `tool_call_fallback` trace events).
- Token-aware compaction sized to the model's context window, with a
  free structural tier before any summarisation call is spent.
- Byte-stable request prefixes, so providers with automatic prompt
  caching (DeepSeek, OpenAI) bill cached rates for the shared history.
- An enforceable turn budget (`max_turns`, default 24) so a confused
  model cannot silently drain a daily quota.
