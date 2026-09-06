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
| 5 | One command registry behind `/help`, the palette, and Tab | Planned |
| 2 | A picker primitive (`select` / `form` / `confirm`) in `bin/` | Planned |
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

Structural CRUD (`add_subagent`, `delete_subagent`, the MCP commands) still
goes through the AST printer and still loses comments. That is the honest
boundary of this phase; appending a `[[table]]` as text is straightforward,
deleting one is not.

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

## 4. For the next phase

Phase 5 (one command registry) is the cheapest remaining win and removes a
live drift: `help_groups` and `palette` in `bin/main.ml` are separate
hand-maintained lists, and `/help` currently omits `/doctor`, `/init`, `/web`,
`/stop`, `/top_p`, `/top_k`, `/max_tokens` and `/seed`.

Phase 2's picker should take its value lists straight from
`Config.setting_kind` — `Enum` is a list to arrow through, `Bool` a toggle,
`Int` a bounds-checked inline edit — so the settings UI is generated rather
than written. That is the whole reason the schema carries types rather than
prose.
