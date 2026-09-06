# Refactor Log — Config & Input UX

**Date:** 2026-09-06
**Implements:** the six-phase plan agreed with the maintainer, whose report was
that "the input config" is the roughest edge in the harness: no pop-up options
for `/config` and `/doctor`, a single-line input prompt, and unintuitive TOML
CRUD.
**Implementer:** Claude Opus 5.
**Branch:** `config-schema-and-safe-writes` off `main` @ `b6b6994`. One commit
per phase, so each stays independently reviewable.

---

## 0. The plan

| Phase | What | Status |
|---|---|---|
| **0** | Stop the data loss: pure reads, comment-preserving writes | **Done** |
| **1** | A typed setting schema as the single source of truth | **Done** |
| **5** | One command registry behind `/help`, the palette, and Tab | **Done** |
| **2** | A picker primitive (`select` / `form` / `confirm`) in `bin/` | **Done** |
| 4 | Doctor checks that carry a fix, applied from the picker | Planned |
| 3 | The input line: multi-line, bracketed paste, no fork per keystroke | Planned |

Phases 0 and 1 landed together because 1 has no user-visible value until 0
stops the file being rewritten underneath it. The remaining order puts the
editor rewrite last, against a settled API.

## 1. What was actually wrong

Measured against the built binary before any change, on a hand-written config:

1. **Reading the config rewrote it.** `load_toml` called `ensure_config_exists`
   → `ensure_orchestrator` → `write_ast`, and `write_ast` reprinted the whole
   file from an otoml AST, which carries no comment nodes. So
   `caravan config get provider` — a pure read — stripped every comment,
   re-indented and reordered the tables, and injected an `[orchestrator]`
   section the user never wrote. Every command that touched config did this.
2. **No validation on writes.** `toml_value_of_string` sniffed types, so
   `config set model 3` stored the *integer* 3 and printed a green ✓. Every
   later `get_string "model"` then returned `None` and fell back to the
   provider default — a setting that appeared saved and did nothing.
   `config set permisions ask` wrote the typo just as happily.
3. **No way to undo a setting.** There was no `unset`: a key, once written,
   could only be changed, not returned to its default.
4. **`editable_keys` was a display tuple**, consulted by the web surface as a
   whitelist and by nothing else. The types it advertised ("true | false",
   "0 | 1 | 2") were prose, not enforced anywhere.

## 2. What landed

| Change | Where |
|---|---|
| Reads never write; `[orchestrator]` resolves in memory | `Config.load_toml`, `Config.get_orchestrator` |
| Atomic writes (temp + rename) keeping `config.toml.bak` | `Config.write_config_text` |
| Comment-preserving textual splice for scalar writes | `Config.splice_scalar` and helpers |
| `unset`, and an `$EDITOR` round-trip that restores an unparseable edit | `Config.unset_toml_value`, `cmd_config_edit` |
| Typed setting schema (`Bool`/`Int`/`Float`/`Enum`/`Str`, scope, env var) | `Config.setting`, `Config.settings` |
| Validated writes with a did-you-mean for typos | `Config.set_checked`, `Config.suggest_key` |
| Schema-driven diagnostics | `Config.unknown_keys`, `Config.mistyped_keys` |
| `caravan config keys \| check \| unset \| edit`, `/config` likewise | `bin/main.ml` |
| Doctor reports unknown keys, mistyped values, and env shadowing | `lib/doctor.ml` |
| Web settings form gets choices, defaults, effect, shadow state | `bin/web.ml` |
| One `mkdir_p`, in `Config`, replacing three copies | `Config.mkdir_p` |

### 2.1 The splice

Writes locate the assignment line for a key inside its table and replace only
the value span, keeping indentation, key order, and trailing comments. The
result is re-parsed *and the value read back* before anything is written; a
splice that cannot be expressed — a multi-line value, a quoted table path, a
key inside an array-of-tables — degrades to the AST printer rather than
corrupting the file. Verified against a config with a `#` inside a quoted
string, a multi-line array, and the same key present at top level, in
`[orchestrator]`, and in `[[subagents]]`: the right one changed, and nothing
else moved.

Structural CRUD was left on the AST printer at first, and closed in phase 2
once `/subagents add` made the loss reachable from the REPL:
`append_table_array` writes a `[[subagents]]` block as text and
`remove_table_array` deletes one by walking to the next header, both verified
by re-parsing. The MCP commands still fall back to the AST printer.

### 2.2 The schema as the spine

`Config.settings` now describes each setting's type, bounds, default, the
environment variable that shadows it, and when a change takes effect.
`editable_keys` is derived from it, so the web whitelist cannot drift. The
same list drives `/config keys`, write validation, `caravan config check`,
and three new doctor checks. `model = 3` — legal TOML, read as nothing — is
now caught by the doctor and refused at the point of writing.

The env-shadow report exists because "I saved it and nothing happened" had no
diagnosis: `CARAVAN_PERMISSIONS` beat the file silently. `config set`,
`config get`, `/config keys` and `doctor` all now say so.

## 3. Verification

`dune build` warning-free; `dune runtest` green. `test/test_Caravan.ml` goes
from 82 test units to 89 — seven new ones, table-driven per AGENTS.md §7.3:

- `config_reads_never_write` — the byte-for-byte assertion that a read is pure.
- `config_orchestrator_falls_back_without_writing`
- `config_write_preserves_comments` — comments, trailing comments, table order,
  and where a brand-new key lands.
- `config_set_checked_validation` — 11 accepted values and 10 refusals in one
  unit, including the `model 3` case that motivated all of this.
- `config_unset_restores_default`
- `config_schema_diagnostics` and `config_schema_is_well_formed` — the latter
  asserts every declared default is a value its own setting would accept.

`with_tmp_config` was extended to clear the `.bak`/`.tmp` sidecars the new
writer leaves.

## 4. Phase 5 — one command registry

`help_groups` and `palette` in `bin/main.ml` were two hand-maintained lists of
the same thing, and they had drifted: `/help` no longer mentioned `/doctor`,
`/init`, `/web`, `/stop`, `/top_p`, `/top_k`, `/max_tokens` or `/seed`. Both
are gone, replaced by `bin/commands.ml` — one record per command carrying its
name, aliases, argument sketch, doc, help group, example, and a completer.

`/help` and the palette are now rendered from it, and `Editor.command_info`
was deleted in favour of `Commands.t`, so the editor completes arguments as
well as command names: the completer is called with the argument tokens typed
so far and returns candidates for the last one. `/config set <Tab>` offers the
settings, `/config set tool_profile <Tab>` offers `auto | core | full`,
`/provider <Tab>` the registry, `/permissions <Tab>` the three modes, and
`/mcp remove <Tab>` the configured servers — all read live, none of it
hand-listed.

Two smaller things fell out of having a registry: an unknown command now
suggests the nearest real one, and a *known* command used with arguments it
does not take answers with its usage line instead of "Unknown command:
/config".

## 5. Phase 2 — the picker

`bin/tty.ml` now holds the raw-mode primitives — keypress decoding, raw mode,
terminal size — that the line editor used to own privately, and `bin/picker.ml`
sits on top with `select`, `confirm`, `prompt`, `secret` and `form`. (It is
`Tty` and not `Term` because `bin/main.ml` opens Cmdliner, whose `Term` would
shadow it.)

Everything that used to print a numbered table and ask the user to retype a
number now arrows: `/config` (Enter changes a setting, with its values offered
from `Config.setting_kind` — an `Enum` is a list, a `Bool` a two-row list, an
`Int` or free-text setting an inline edit pre-filled with the current value),
`/models`, `/provider`, `/permissions`, `/key`, `/subagents remove`, and the
provider and model steps of `caravan init`. `/config show` keeps the old
session summary. Each falls back to the numbered prompt when stdin is not a
terminal, so scripts are unaffected.

`/subagents add` and `/subagents remove` are new: the REPL had no way to
declare a worker, only the web cockpit did. Both drive
`Config.editable_subagent_fields`, so the two surfaces ask for the same
fields.

Two fixes fell out of building it:

- `read_secret` read through OCaml's buffered `input_line` while the widgets
  read stdin byte by byte with `Unix.read`. A buffered channel reading
  alongside a raw one swallows whatever it read ahead, so a pasted key could
  eat the keystrokes meant for the next prompt. Secrets now read raw, through
  the same reader as everything else.
- `caravan init` overwrote an existing config without asking. It now confirms
  first, and writes through `Config.write_config_text`, so the old file is
  recoverable from `config.toml.bak`.

`Tty` caches the terminal size and drops the cache on SIGWINCH and at the
start of each widget. That was scheduled for phase 3, but the editor's redraw
called `stty size` — a fork and an exec — on every keystroke, and the function
moved in this phase anyway.

## 6. For the next phase

Phase 4 turns each doctor check's `hint` into a `fix` the picker can apply.
Phase 3 is then the last one: the line editor itself.

`bin/` is an executable, so `Commands` and `Picker` are not reachable from the
test library; their behaviour is covered by driving the built binary through a
pty. Splitting `bin` into a library plus a thin executable would make them
testable directly, and is worth doing if that layer grows further.
