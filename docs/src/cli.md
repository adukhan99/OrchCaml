# CLI Reference

Every command accepts `-p/--provider`, `-m/--model`, `--base-url`,
`-s/--system`. Resolution order: **CLI flag → `CARAVAN_*` env →
`~/.caravan/config.toml` → registry default**.

## Commands

| Command | Purpose |
|---------|---------|
| `caravan` | interactive REPL (default) |
| `caravan agent "<task>"` | one-shot autonomous run (`run` is an alias) |
| `caravan complete "<prompt>"` | single completion, no tools |
| `caravan web [--port N]` | local web UI (127.0.0.1 only) |
| `caravan providers [--ladder]` | provider table / model ladder |
| `caravan models` | models on the current provider |
| `caravan config show\|path\|keys\|get K\|set K V` | inspect or edit the config |
| `caravan mcp add\|remove\|list\|get` | manage Model Context Protocol (MCP) servers |
| `caravan init` | setup wizard |
| `caravan doctor` [`--fix`] [`--json`] | diagnostics, with fixes offered (exit 1 on failure) |

### `caravan mcp`

- `caravan mcp list` — list configured MCP servers and their health status
- `caravan mcp get <name>` — inspect an MCP server and its discovered tools
- `caravan mcp add <name> [--transport stdio] [--no-probe] -- <command> [args...]` — probe connection, register tools, and save to config
- `caravan mcp remove <name>` (or `rm`) — remove an MCP server configuration

### `caravan agent`

- `--max-turns N` — turn budget (default: config `max_turns`, else 24)
- `--quiet` — final result only
- `--json` — one JSON object on stdout:
  `{ok, result, turns, usage, transcript}`
- exit codes: `0` done · `1` failed / out of turns · `2` config error

## REPL slash commands

Typing `/` opens a live palette; Tab completes the command, and once one is
typed, its arguments — `/config set <Tab>` offers the settings,
`/provider <Tab>` the registry. Highlights:

| Command | Effect |
|---------|--------|
| `/agent <task>` | autonomous loop with tools |
| `/resume [path]` | restore session state from a saved checkpoint |
| `/nudge <text>` | steering note injected before the next model call |
| `/lisp <program>` | evaluate a [Slip](slip.md) expression |
| `/model` · `/models` · `/provider` · `/providers` | switching |
| `/subagents` · `/subagents add` · `/subagents remove [name]` | worker roster and CRUD |
| `/permissions [mode]` | `auto` \| `ask` \| `readonly`, live |
| `/config` · `/config keys` · `/config set k v` · `/config unset k` · `/config get k` · `/config edit` | settings |
| `/key <provider>` | store an API key (hidden input) |
| `/system` `/temp` `/top_p` `/top_k` `/max_tokens` `/seed` `/stop` | generation |
| `/memory <n>` · `/summarise` | context window / compact now |
| `/history` · `/export [file]` · `/tools` | inspect the session |
| `/mcp [list\|add [--no-probe]\|get\|remove]` | manage MCP tool servers and dynamic tool bindings |
| `/plugins [enable\|disable <id>]` | plugin composition and lifecycle states |
| `/doctor` · `/init` | pre-run commands, callable in-session |
| `/clear` · `/help` · `/quit` | housekeeping |

## Line editor

| Keys | Effect |
|------|--------|
| `alt+enter` · `ctrl+o` | insert a line break — `enter` submits |
| `ctrl+r` | search history; `enter` runs the match, `esc` cancels |
| `tab` · `↑` `↓` | complete a command or its argument |
| `↑` `↓` | move between rows, or walk history |
| `ctrl+a` `ctrl+e` · `ctrl+k` `ctrl+u` `ctrl+w` | line start/end · kill to end, to start, previous word |
| `ctrl+c` · `ctrl+d` | clear the line · exit on an empty one |

Input is multi-line and soft-wraps, and pasting is bracketed: a pasted block
arrives as **one** message and one history entry, however many lines it has.
`/help` lists these in-session.
