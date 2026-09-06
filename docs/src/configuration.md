# Configuration

One TOML file: `~/.caravan/config.toml` (or the path in `CARAVAN_CONFIG`).
`caravan init` writes it (mode 0600). Edit it from any surface — no shell
required:

```bash
caravan config set permissions ask     # CLI
/config set permissions ask            # REPL (and /key <provider> for keys)
# web UI: ⚙ settings panel
```

## Editing the file

Every surface writes through the same schema, so a setting knows what it
accepts before anything reaches disk:

```bash
caravan config keys                    # every setting, its value, what it accepts
caravan config set max_turns 40
caravan config unset base_url          # fall back to the provider default
caravan config check                   # validate the file against the schema
caravan config edit                    # $EDITOR, restored if the result won’t parse
```

A value that a setting cannot accept is refused rather than stored:

```
$ caravan config set model 3
✓ model = "3"                          # free text stays text
$ caravan config set permisions ask
Error: unknown setting 'permisions' — did you mean 'permissions'?
$ caravan config set max_turns 99999
Error: max_turns must be between 1 and 1000 (got 99999)
```

Writes are surgical: your comments, key order, and table layout survive an
edit untouched — including `/subagents add` and `/subagents remove`, which
append and delete `[[subagents]]` blocks in place. The previous contents are
kept as `config.toml.bak`, and reading the config never modifies it.

`/config` with no arguments opens the settings as a list: arrow to one, press
Enter, and pick a value (or type one, for the free-text settings). `/config
show` keeps the old summary of the live session.

If a setting appears to do nothing, an environment variable is usually
overriding it — `config set`, `config get`, and `caravan doctor` all say so
when that is the case.

## Diagnostics

`caravan doctor` checks the config file, the provider and its key, the
transcript directory, subagents and MCP servers — and then offers to fix what
it can: arrow to a failing check, press Enter, and the change is applied and
the suite re-run.

```bash
caravan doctor           # report, then offer fixes on a terminal
caravan doctor --fix     # apply every fix that needs no input, then re-check
caravan doctor --json    # one JSON object, for CI
```

It exits non-zero when any check fails, so it can gate a script. `/doctor`
runs the same checks inside a session, against that session's own connection.

## Resolution order

1. **CLI flags** — `-p/--provider`, `-m/--model`, `--base-url`, `-s/--system`;
2. **Environment** — `CARAVAN_PROVIDER`, `CARAVAN_MODEL`, `CARAVAN_BASE_URL`,
   `CARAVAN_STREAM`, `CARAVAN_MAX_TURNS`, `CARAVAN_PERMISSIONS`,
   `CARAVAN_TRANSCRIPT`, `CARAVAN_NUDGE`, `CARAVAN_SUBAGENTS`,
   `CARAVAN_STRICT_MODE`, `CARAVAN_SPINNER`, `CARAVAN_TLS_INSECURE`,
   provider key vars (`ANTHROPIC_API_KEY`, …);
3. **TOML root keys**, then the same keys inside `[orchestrator]`;
4. **Registry / module defaults**.

## Core keys

```toml
provider    = "anthropic"          # see `caravan providers`
model       = "claude-sonnet-5"    # omit → provider default
# base_url  = "http://my-gateway:8000/v1"
system      = "You are a concise research assistant."   # appended to the shipped default
# system_replace = true  # make `system` replace the shipped default entirely

stream      = true       # stream tokens as they arrive
max_turns   = 15         # agent turn budget (default 24)
nudge       = true       # budget-awareness nudges in agent loops
permissions = "auto"     # auto | ask | readonly
provider_retry = "medium"  # provider error retry aggression: off | low | medium | high
# provider_retry_base_delay = 0.5  # base backoff seconds (exponential, cap 30s)
transcript  = true       # JSONL session logs in ~/.caravan/logs/
strict_mode = 0          # bash tool: 0 permissive, 1 single-command, 2 hidden
enable_subagents = true  # offer delegate when [[subagents]] exist

tool_call_mode = "auto"  # tool-call recognition: auto | native | text
require_finish = true    # agent runs complete only via the finish tool
tool_profile   = "auto"  # tool surface: auto (capability-driven) | core | full
# summarize_model = "…"  # cheap model for compaction summaries
```

### Model capabilities

Unknown models get conservative defaults (8k context window, tool
calling treated as unreliable). Override per model-name pattern
(case-insensitive substring match); every field is optional:

```toml
[capabilities."my-local-model"]
context_window = 32768
tool_calling = "native"        # native | flaky | text
streaming_tool_calls = true
cache = "automatic"            # none | automatic | explicit
requests_per_minute = 20
```

The capability table drives the compaction threshold, the text
tool-call fallback, the tool profile, and the system-prompt layers —
see [Getting Started for Free](free-tier.md).

`caravan config keys` (or `/config keys`) lists every editable key with its
current value, what it accepts, and when a change takes effect. It is
generated from the same schema that validates writes, so it can never
drift from what the code actually reads.

### Provider retries

When a provider call fails transiently (HTTP 5xx, 429 rate limits,
dropped connections), Caravan can retry it automatically instead of
interrupting the turn. `provider_retry` controls the aggression:

| Mode | Retries | Retries on |
|------|---------|-----------|
| `off` | 0 | never |
| `low` | 1 | 5xx + connection failures |
| `medium` (default) | 3 | 5xx + 429 + connection failures |
| `high` | unlimited | every HTTP status, including deterministic 4xx |

When the server supplies `Retry-After` (or an `x-ratelimit-reset-*`
header), that wait is honoured instead — clamped to 120s — which is
what makes 429-heavy free tiers survivable.
Otherwise backoff is exponential (`provider_retry_base_delay`, default 0.5s:
0.5s → 1s → 2s … capped at 30s). Streaming responses are only retried
before the first token reaches your terminal, so output is never
duplicated. Every retry is announced in the transcript as a
`provider_retry` event. Env overrides: `CARAVAN_PROVIDER_RETRY`,
`CARAVAN_PROVIDER_RETRY_BASE_DELAY`.

## API keys

```toml
[api_keys]
anthropic = "sk-ant-..."
groq      = "gsk_..."
```

Environment variables take precedence and are the recommended home for
secrets; the `[api_keys]` table is for hosts where a private 0600 file
beats env plumbing (cron, HPC batch scripts).

## Subagents

See [Subagents](subagents.md) for `[[subagents]]` and `[providers.*]`
tables.

## Spinner

```toml
[spinner]
enabled = true    # auto-disabled when stderr is not a TTY
verbose = false
thinking = ["Thinking", "Pondering", "Mulling"]   # arrays pick at random
```

## MCP servers

```toml
[[mcp.servers]]
name      = "filesystem"
transport = "stdio"
command   = "npx"
args      = ["-y", "@modelcontextprotocol/server-filesystem", "/home/you/ws"]
```

## Plugins

Caravan's tool composition runs on the plugin runtime
(see [Plugins](plugins.md)). With no `[[plugins]]` table the default
composition applies: the built-in tools plus one MCP mount per
`[[mcp.servers]]` entry — existing configs behave exactly as before.
Declare `[[plugins]]` entries to take control:

```toml
[[plugins]]                      # built-in tools, minus bash
plugin  = "tools.builtin"
exclude = ["bash"]

[[plugins]]                      # an MCP server as a plugin
id      = "fs"
plugin  = "tools.mcp"
name    = "filesystem"
command = "npx"
args    = ["-y", "@modelcontextprotocol/server-filesystem", "/home/you/ws"]
```

- `plugin` names a builder (`tools.builtin`, `tools.mcp`, or one an
  embedding application registered); `id` defaults to it.
- Entries merge over the defaults by `id` — redeclaring
  `tools.builtin` with `enabled = false` switches the default off.
- An optional `realm = "<name>"` field sandboxes the entry's tools
  into an isolated realm that only `[[subagents]]` workers declaring
  the same `realm` can see — details in
  [Subagents → Sandbox realms](subagents.md#sandbox-realms).
- `/plugins` in the REPL shows each entry's lifecycle state;
  `/plugins enable|disable <id>` toggles one for the session.

## Diagnostics

```bash
caravan doctor          # validity, key presence, reachability, subagents
caravan config show     # print the active file
caravan config path     # where it lives
```

A fully annotated example lives at
[`docs/example_config.toml`](https://github.com/adukhan99/Caravan/blob/main/docs/example_config.toml).
