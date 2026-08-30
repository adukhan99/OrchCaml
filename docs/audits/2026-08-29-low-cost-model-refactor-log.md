# Refactor Log — Low-Cost & Free-Tier Model Readiness

**Date:** 2026-08-29
**Implements:** `docs/audits/2026-08-29-low-cost-model-readiness.md` (Claude Opus 5, read-only)
**Implementer:** Claude Fable 5, working from the maintainer's delegated answers.
**Branch:** `audit-fixes` off `main` @ `af58744`. One commit per tier, per the
audit's request that tiers stay independently reviewable.

---

## 1. What landed, finding by finding

| Finding | Status | Where |
|---|---|---|
| D1 capability table | **Done** | `lib/capability.ml{,i}` — per-model context window, tool fidelity, cache kind, rate limits; `[capabilities."pattern"]` config overrides; conservative default for unknowns |
| C1 default system prompt | **Done** | `lib/system_prompt.ml{,i}` — layered (base / capability-conditioned format layer / env preamble / user append); `system_replace` escape hatch |
| C2 text tool-call fallback | **Done** | `lib/tool_call_fallback.ml{,i}` + `Session.apply_tool_call_fallback`; `tool_call_mode` key; `Tool_call_fallback` trace event |
| C3 pair-aware eviction | **Done** | `Memory.Ring.drop_oldest` unit-atomic; `Session.drop_orphan_tool_results` wire guard |
| C4 budget reset | **Done** | `Session.summarise` no longer resets `turn_idx` |
| H1 tiered compaction | **Done** | `Session.compact`: structural tier (hard-truncate aged tool outputs, free) before model summarisation; task + last 4 messages kept verbatim; `summarize_model` routing |
| H2 token budgeting | **Done** | `Capability.estimate_*` (chars/4), `Session.config.context_window`, `Compaction_policy` 75%-of-window trigger |
| H3 byte-stable prefix | **Done** (see §3.1) | `MEMORY.map_recent`, `Session.stabilize`, idempotent truncation; `usage.cached_tokens` instrumentation |
| H4 Retry-After | **Done** (limiter deferred, §4) | `Provider_failure.retry_after`, typed-header parsing, `Retry.delay_for` |
| H5 finish discipline | **Done** | `Agent.require_finish` (default on), bounded plain-reply reminders, fail-fast diagnostic |
| M1 tool profiles | **Done** | `Capability.core_tool_names` / `use_core_profile`; `tool_profile` key; filtered in `make_session` with a trace note |
| M2 native Anthropic provider | **Deferred** (§4) | — |
| M3 model ladder refresh | **Done** | registry: `claude-sonnet-5` default, `claude-haiku-4-5`/`claude-sonnet-5`/`claude-opus-5` hints |
| M4 free-tier docs + providers | **Done** | `docs/src/free-tier.md`; registry: `cerebras`, `github_models`, `nvidia`; OpenRouter `:free` documented |
| M5 | Same as D1 | — |
| L1 gen_tools substring matching | **Not touched** | No new tools were added; noted for whoever adds one |

Every tier builds warning-free and passes `dune runtest` (82 tests, up
from 71 at baseline). No dependencies were added; `Caravan.opam` is
unchanged.

## 2. Maintainer answers I worked from

Only two questions came back answered; the rest were delegated to my
judgment. For the record:

- **Q1 (strict_mode)** — answered: loosen. Default is now `0`
  (permissive). The setting and both stricter modes remain.
- **Q4/M2 (native Anthropic)** — answered: only if straightforward.
  It is not straightforward (see §4); deferred.
- **Q2 (max_turns)** — my call: raised 10 → 24, *after* C4 made the
  ceiling enforceable. Rationale: weak models need more steps for the
  same work, the nudge system keeps long runs on task, and the budget
  now actually stops runaway spend. The audit suggested revisiting
  with measurements; 24 is a considered default, not a measured one —
  cheap to re-tune.
- **Q3 (system append vs replace)** — my call, matching Opus's lean:
  `system` appends; `system_replace = true` restores full user
  control. `system_replace = true` with no `system` reproduces the
  old no-prompt behaviour exactly, so nobody is trapped.
- **Q5 (text protocol: feature or fallback?)** — my call: documented
  first-class-lite. `tool_call_mode = auto|native|text` exists and is
  documented (free-tier guide + configuration.md), the recognised
  formats are specified in the C1 prompt layer, and the fallback is
  always auditable via trace events. What I did *not* do is build
  text-mode-specific prompt scaffolding beyond that layer — that is
  the part that would make it a genuine differentiator, and it should
  ride on evidence from real local-model runs.

## 3. Where I diverged from the audit, and why

The audit asked for disagreements to be stated rather than quietly
worked around. Two are worth recording.

### 3.1 H3: "truncate once, at insertion time" — implemented as "truncate once, at aging time"

Literal insertion-time truncation would cap every tool output at
`max_tool_output_len` (default 1000 bytes) *before the model ever reads
it* — `read_file` on any real file would be crippled. That cannot be
what we want while the default cap is this small.

What landed: a message keeps its full output while it is within the
newest 2 messages, and is truncated **in memory, exactly once,
idempotently** the first time it ages past that window. Consequence for
caching, stated honestly: each request's prefix is byte-identical to
the previous request *except for messages that crossed the aging
boundary that turn* (in an agentic loop, roughly the previous turn's
tool results). The cache therefore re-uses everything up to the
previous turn's tail rather than the theoretical maximum of everything
up to the current tail. That is the overwhelming majority of the win;
the residual gap closes only by giving up fresh-read fidelity, which is
the wrong trade. The regression test asserts exactly the property the
design guarantees.

### 3.2 C2: ReAct `Action:` blocks are not recognised

The audit floated ReAct as a third format behind a capability flag. I
left it out entirely: it is the format most likely to false-positive on
ordinary prose (the audit's own assessment), and the population that
needs it overlaps heavily with models that can manage the JSON form
when the C1 format layer tells them to. If field reports show
ReAct-only models mattering, the extractor has an obvious seam to add
it behind `tool_call_mode = "text"` plus a capability flag.

Minor note for completeness: `Provider_failure` gained a `retry_after`
field read from **typed response headers**. I read AGENTS.md §8's
string-scraping prohibition as targeting exception *text*; header
values are parsed strictly (HTTP-dates and arbitrary text are rejected,
not guessed at) and never travel unstructured.

## 4. Deferred, deliberately

- **M2 — native Anthropic `/v1/messages` provider.** Different message
  shape, content blocks, separate system channel, different SSE
  framing, `cache_control` placement — a real provider module, not an
  afternoon. Per the maintainer's "only if straightforward": deferred.
  Until it lands, Anthropic runs through the compatibility shim and the
  explicit-cache half of H3 stays unreachable (the byte-stable work
  already benefits automatic-caching providers). The capability table
  already records `Cache_explicit` for Claude models, so the wiring
  point exists.
- **H4's better half — client-side token-bucket limiter.** Honouring
  `Retry-After` survives the 429; a limiter seeded from
  `requests_per_minute` would avoid it. The capability table carries
  the field already; the limiter belongs in `Provider` next to `Retry`.
- **Subagent system prompts** (`subagent_config.system_prompt`
  defaults to `""`). C1 covers the main session; giving subagents the
  layered default is a small follow-up in `Subagents`/`Plugin_host`
  territory I did not want to fold into this series.
- **Cloudflare Workers AI** registry entry: its endpoint embeds an
  account id, which `Registry.entry`'s static `base_url` cannot
  express without a placeholder convention. Skipped rather than done
  badly.
- **L1** stands as recorded by the audit.

## 5. Testing notes

Per the audit's §8 list: C2 (fenced JSON executes end-to-end +
extractor rejection table), C3 (windows that land mid-unit, restored
orphan checkpoints), C4 (constantly-compacting run stops at
`max_turns` with bounded spend), H3 (stable-portion byte equality
across three recorded requests), plus H1/H2/H4/H5/M1/D1 coverage. The
`session_summarise` expect-test's output changed deliberately (the
task statement now survives compaction) and was promoted.

Two pre-existing fixtures constructed orphaned tool results by hand —
the exact wire shape C3 exists to eliminate — and were corrected to
introduce their tool calls honestly.

What is **not** covered: no live-endpoint runs. The audit's
"Verified (untested)" items (the 400-on-orphan contract, real
`Retry-After` values, actual cache-hit billing) remain confirmed only
against documented API behaviour; `usage.cached_tokens` exists
precisely so the caching claim can be checked against a real DeepSeek
or OpenAI run cheaply.

## 6. For the next audit

The capability table is the spine, as requested — C2's activation
condition, H2's threshold, M1's profile, and C1's format layer all
read from it, and unknown models degrade to the conservative default.
The built-in table will drift like every table; the config override
and `caravan models` note are the mitigation, but a periodic refresh
(M3-style) should be assumed as maintenance.
