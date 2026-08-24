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
| `caravan init` | setup wizard |
| `caravan doctor` | diagnostics (exit 1 on failure) |

### `caravan agent`

- `--max-turns N` — turn budget (default: config `max_turns`, else 10)
- `--quiet` — final result only
- `--json` — one JSON object on stdout:
  `{ok, result, turns, usage, transcript}`
- exit codes: `0` done · `1` failed / out of turns · `2` config error

## REPL slash commands

Typing `/` opens a live palette (Tab completes). Highlights:

| Command | Effect |
|---------|--------|
| `/agent <task>` | autonomous loop with tools |
| `/resume [path]` | restore session state from a saved checkpoint |
| `/nudge <text>` | steering note injected before the next model call |
| `/lisp <program>` | evaluate a [Slip](slip.md) expression |
| `/model` · `/models` · `/provider` · `/providers` | switching |
| `/subagents` | configured worker roster |
| `/permissions [mode]` | `auto` \| `ask` \| `readonly`, live |
| `/config` · `/config set k v` · `/config get k` · `/config keys` | settings |
| `/key <provider>` | store an API key (hidden input) |
| `/system` `/temp` `/top_p` `/top_k` `/max_tokens` `/seed` `/stop` | generation |
| `/memory <n>` · `/summarise` | context window / compact now |
| `/history` · `/export [file]` · `/tools` | inspect the session |
| `/plugins [enable\|disable <id>]` | plugin composition and lifecycle states |
| `/doctor` · `/init` | pre-run commands, callable in-session |
| `/clear` · `/help` · `/quit` | housekeeping |

## Line editor

Arrows / Home / End / Delete edit the line; Up/Down recall persistent
history (`~/.caravan/history`); Ctrl-A/E jump, Ctrl-K/U kill, Ctrl-W
deletes a word, Ctrl-L clears the screen, Ctrl-C cancels the line
(session survives), Ctrl-D on an empty line exits.
