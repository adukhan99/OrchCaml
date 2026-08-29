# Audit — Low-Cost & Free-Tier Model Readiness

**Date:** 2026-08-29
**Repo:** `Caravan` @ `16cfa17` (main, clean), v0.2.0, 128 commits
**Auditor:** Claude Opus 5, read-only pass. No code was modified.
**Intended reader:** an implementing agent (Fable 5) undertaking a long-horizon
refactor, plus the maintainer reviewing its work.

---

## 0. How to read this document

Every finding carries an **evidence** line with `file:line` citations and a
**confidence** marker. The markers are load-bearing — please honour them:

- **Verified** — I read the code path end to end and traced the consequence.
  You can act on it without re-deriving it, though you should still read the
  surrounding code before editing.
- **Verified (untested)** — the code path is as described, but the *external*
  consequence (an API returning 400, a model behaving a given way) is inferred
  from how those APIs and models generally behave. I did not run against a live
  endpoint. Confirm before building anything expensive on top.
- **Inferred** — reasoning from general knowledge, not from this repo. Treat as
  a hypothesis to test.
- **Judgment call** — a real tradeoff with no objectively correct answer. These
  are flagged for the maintainer, not for you. See §6; do not silently resolve
  them.

If you find one of my claims is wrong, that is a useful outcome — say so
plainly in your report rather than working around it. I would rather be
corrected than have you build on a bad premise.

---

## 1. Intent

### 1.1 What was asked

The maintainer's framing, close to verbatim:

> How can we make this harness more optimised for free API and low-cost model
> use? Obviously, for models with proprietary harnesses, I can't out-compete
> that, but for intentionally multivalent models, how can we get this
> extensible harness to be ready & successful 'out of the box' like Claude
> Code?

### 1.2 What I take that to mean

Three distinct goals, which the refactor should keep distinct because they pull
in different directions:

1. **Correctness under weak models.** A 1B–8B local model, or a free-tier
   hosted model, should be able to complete a simple agentic task without the
   harness silently mis-handling its output. Today several such models fail
   *invisibly* — the harness reports success having done nothing. This is a
   correctness problem, not a quality problem.

2. **Economy under metered access.** Free tiers meter requests per minute,
   tokens per minute, and tokens per day. The harness should treat requests
   and tokens as the scarce resource they are: cache-friendly prefixes, no
   redundant calls, an enforceable budget, and backoff that respects the
   server's own stated retry window.

3. **Out-of-box competence.** A user who runs `caravan init` and picks a free
   provider should get a working agent, not a construction kit. Claude Code
   feels good out of the box substantially because of a long, tuned system
   prompt and injected environment context — the half of the equation that
   is *not* the model. Caravan currently ships none of it.

### 1.3 The strategic read

Goal 3 is where the "multivalent" framing earns its keep, and I want to state
this as an intent rather than a task, because it should shape how you sequence
everything else.

Caravan cannot win by imitating a single-model harness. It can win by being the
harness that *adapts to the model in front of it*. `lib/providers/registry.ml`
already knows where every backend lives; it does not know what any of them can
do. Making that table capability-aware — context window, native tool-calling
fidelity, cache semantics, free-tier limits — and then deriving behaviour from
it (which tools to expose, when to compact, how to parse tool calls, how hard
to retry) is the structural advantage a proprietary harness cannot copy,
because it only ever has one model to serve.

Please treat the capability table (D1, §5) as the spine of the refactor, and
the individual fixes as things that hang off it. If you fix the bugs but leave
the registry capability-blind, the next cheap model to come along breaks the
harness again.

### 1.4 Explicit non-goals

- Do **not** rewrite the plugin runtime, the Slip/Lisp interpreter, the web
  cockpit, or the subagent system. They are out of scope and appear healthy.
- Do **not** chase frontier-model quality. The target is "a free model
  completes a simple task reliably," not "matches Claude Code on hard tasks."
- Do **not** add dependencies without asking. The README's "no 10 GB of
  dependencies" claim and the HPC/non-root positioning are real constraints.
- Do **not** break the single-static-binary, no-root, no-file-spew properties.

---

## 2. Coverage — what I actually read

So you know where the gaps in this audit are.

**Read fully:** `lib/agent.ml`, `lib/session.ml`, `lib/memory.ml` (Ring +
Summary), `lib/provider.ml`, `lib/providers/openai_compatible.ml`,
`lib/providers/registry.ml`, `lib/tool.ml`, `lib/prompt.ml`, `lib/parser.ml`,
`lib/compaction_policy.ml`, `lib/tools/{bash,read_file,finish,gen_tools}.ml`.

**Read partially:** `lib/config.ml` (defaults, key tables, MCP section),
`bin/main.ml` (session construction, CLI wiring, help tables), `AGENTS.md`
(section headings + §7–§12), `README.md`, CI workflow.

**Not read:** `lib/plugin.ml`, `lib/plugin_host.ml`, `lib/lisp.ml`,
`lib/mcp.ml`, `lib/ui.ml`, `bin/web.ml`, `bin/editor.ml`, `lib/trace.ml`,
`lib/redis_store.ml`, the test suite bodies (2748 lines across three files),
`lib/tools/delegate.ml`. **If your changes touch these, audit them yourself —
I have no basis for an opinion on them.** In particular I have not checked
whether the test suite already covers any of the behaviours below.

---

## 3. Critical findings

These four break the harness for the target model class. C2 and C4 in
combination mean a free-tier agent run can both do nothing and cost a lot.

---

### C1 — No default system prompt

**Confidence:** Verified.
**Evidence:** `bin/main.ml:945` reads `Config.get_string "system"`, which
returns `None` unless the user has written one. `bin/main.ml:100-105`
(`make_session`) calls `Session.set_system` only on `Some`. Subagents default
to `system_prompt = ""` (`lib/config.ml:507`). There is no fallback string
anywhere in the tree.

**What happens:** every model receives the tool JSON schemas and nothing else.
No working directory, no OS, no date, no git context, no statement of what the
agent is for, no instruction to call `finish` when done, no guidance on how to
sequence tool calls or what to do when a tool errors.

**Why it matters here specifically:** frontier models infer most of this from
the tool schemas and their own priors. Small and free models do not. They
narrate instead of acting, re-discover the working directory with repeated
`ls`/`pwd` round trips (each one a metered request), and never call `finish`
because nothing told them it exists as a protocol rather than as one tool among
fifteen.

**Intent for the fix:** ship a real default system prompt, plus a generated
environment preamble (cwd, OS, date, git branch and dirty state, a shallow
top-level listing). Two design constraints that matter downstream:

- The preamble must be assembled **once per session**, not per turn. It is the
  head of the cache prefix (see H3) and must be byte-stable.
- The prompt should be **layered**, not monolithic: a base agent prompt, plus
  a capability-conditioned section (e.g. tool-call format reminders only for
  models that need them, per D1). Do not hardcode one string.

Keep the existing `system` config key working as an *override or append*, and
decide deliberately which — I lean append, with an explicit `system_replace`
for users who want full control. Flagging as a minor judgment call.

---

### C2 — A model that emits tool calls as text silently ends the run

**Confidence:** Verified for the code path; Inferred for the prevalence across
specific models.
**Evidence:** `lib/providers/openai_compatible.ml:128` parses tool calls only
from `message.tool_calls` (and `:278` for the streaming delta). If a model puts
its tool call in `content` instead, `tool_calls` is `None`. That reaches
`lib/session.ml:284`, which returns `Done (…, Via_plain_reply)`. `Agent.is_finished`
(`lib/agent.ml:25-33`) treats a plain reply with non-empty content as
**finished**.

**What happens:** the model replies with ```` ```json {"tool": "ls", …} ````
or `<tool_call>…</tool_call>` or a ReAct `Action:` block. The harness sees prose,
declares the task complete, and returns that prose as the result. The user gets
a confident-looking answer produced by an agent that executed zero tools.

**Why it matters here specifically:** this is the *default* behaviour of a large
share of the target population — small local GGUFs, `llama3.2:1b`, and a good
fraction of OpenRouter's `:free` tier. These models were often tuned on a
text-based tool protocol, or emit malformed native calls that the server drops.
This single finding probably accounts for most "Caravan doesn't work with my
local model" reports, and it is invisible: there is no error, no warning, no
trace event distinguishing it from a genuine one-shot answer.

**Intent for the fix:** a fallback tool-call extractor. When `tool_calls` is
empty *and* the session is in agent mode *and* the content parses as a tool
invocation, synthesise the `tool_call` record and continue the loop.

- Build on what exists: `Parser.permissive_json` and `Parser.extract_code`
  (`lib/parser.ml`) already strip code fences and tolerate sloppy JSON.
- Support at least: fenced/bare JSON objects naming a known tool, and
  `<tool_call>` / `<function_call>` XML. ReAct `Action:`/`Action Input:` is a
  third format worth considering — I'd gate it behind a capability flag, since
  it is the most likely to false-positive on ordinary prose.
- **False positives are the danger.** A model legitimately *discussing* a tool
  call must not have it executed. Require the parsed name to match a registered
  tool, require the whole content to be the invocation (not a fragment inside
  prose), and emit a distinct `Trace` event so this is auditable rather than
  magic. Under `ask` permissions the user still gets the prompt, which is a
  useful safety net.
- Where this lives is a real design question. The parser is provider-agnostic,
  so `lib/session.ml` or a new `lib/tool_call_fallback.ml` is more honest than
  burying it in `openai_compatible.ml`. Your call, but keep it out of the
  provider engine if you can — other providers will need it too.

---

### C3 — Window eviction orphans tool results, producing hard API errors

**Confidence:** Verified (untested) — the eviction behaviour is verified; the
resulting 400 is inferred from documented API contracts, not observed.
**Evidence:** `Memory.Ring.add` (`lib/memory.ml:65-71`) appends and, on
exceeding the window, calls `drop_oldest` (`:59-63`), which removes the single
oldest non-system message. System messages are held separately in `system_msgs`
and correctly never dropped. Default window is 40 messages
(`lib/session.ml:19`).

**What happens:** the oldest message at the boundary is typically an assistant
message carrying `tool_calls`. Its corresponding `tool` result messages are
newer, so they survive — now referencing a `tool_call_id` that no longer appears
in the request. OpenAI, Groq, Mistral, and DeepSeek all reject this shape.

**Why it matters here specifically:** an agentic loop generates messages fast
(assistant + N tool results per turn), so a 40-message window is reached in a
handful of turns. The failure lands mid-run, after tokens have been spent, and
`Retry.classify` will not recover it — a 400 is not retriable at `Medium`
(`lib/provider.ml:56-61`), correctly so, since retrying an identical malformed
request is futile.

**Intent for the fix:** eviction must be **pair-aware and atomic** — an
assistant message bearing `tool_calls` and all of its result messages form one
indivisible unit that is dropped together or not at all. Note this interacts
with H2 (token-aware budgeting): if you are rewriting eviction anyway, do both
in one pass rather than twice.

Worth adding a defensive validation step before the wire call that drops
orphaned `tool` messages regardless of how they arose — cheap insurance, and it
protects the checkpoint-restore path (`Session.of_json`,
`lib/session.ml:~380`) which reconstructs a Ring from arbitrary saved JSON.

---

### C4 — Compaction resets the turn counter, defeating the budget

**Confidence:** Verified.
**Evidence:** `Session.summarise` returns a session with `turn_idx = 0`
(`lib/session.ml:202`). It is invoked from *inside* the turn loop, at
`lib/session.ml:250-256`, whenever `Compaction_policy.should_compact` fires.
Both budget guards then read the reset counter: the inner one at
`lib/session.ml:~262` (`sess_after_sum.turn_idx >= max_t`) and the outer one in
`Agent.run_generic` (`lib/agent.ml:67` and `:74`).

**What happens:** with `auto_summarize = true` and `memory_size = 40` (both
defaults, `lib/session.ml:18-20`), a long run compacts, and compaction hands
the agent a brand-new full turn budget. It can do so repeatedly. `max_turns`
(default 10, `lib/config.ml:620-621`) is not an enforceable ceiling on any run
long enough to trigger compaction.

**Why it matters here specifically:** this is the quota-drain bug. On a metered
free tier the turn budget is the only thing standing between a confused model
and the daily token cap — and a confused model is *exactly* the one that
generates enough messages to trigger compaction. The failure mode is
self-reinforcing: the worse the model performs, the longer it runs, the more it
compacts, the longer it is permitted to run. Each compaction also costs an
*additional* model call (see H1).

**Intent for the fix:** separate the two things `turn_idx` is currently
conflating — "messages since last compaction" (a memory concern) and "turns
consumed against budget" (an accounting concern). The budget counter must be
monotonic for the lifetime of the agent run and must never be reset by a
memory operation.

Be careful: `turn_idx` is serialised into checkpoints
(`Session.export_json`), so changing its meaning or adding a field touches the
save/restore format. `Session.of_json` already has a legacy-compatibility
branch for older checkpoints — extend that pattern rather than breaking it.

`Session.clear` (`lib/session.ml:85`) also resets `turn_idx`, which is correct
there — that is a genuine new conversation. Only the compaction reset is wrong.

---

## 4. High-severity findings

---

### H1 — Compaction is expensive, lossy, and unaffordable on a free tier

**Confidence:** Verified.
**Evidence:** `Session.summarise` (`lib/session.ml:177-203`) formats the entire
history into one prompt and issues a **full additional model call** via
`Provider.complete_packed`, using the same model as the agent itself. It then
replaces memory wholesale with a `SummaryMemory` containing only the summary
string — every tool result, file content, and command output is discarded.

**Why it matters here specifically:** three compounding problems on cheap models.
The summarisation call competes for the same rate limit as the actual work. The
model doing the summarising is the same weak model, so the summary is poor.
And total discard is catastrophic for an agent mid-task — it forgets what it
already read and re-reads it, spending more tokens.

**Intent for the fix:** move from "summarise everything, discard everything" to
a tiered strategy — evict or truncate old tool *outputs* first (they are the
bulk and the least re-usable), preserve the task statement and recent turns
verbatim, and only invoke a model call when structural eviction is
insufficient. Where a cheap model is configured alongside an expensive one,
routing summarisation to the cheap one is a natural extension of the subagent
machinery already present. Consider also making the summarisation model
independently configurable.

---

### H2 — Nothing is measured in tokens

**Confidence:** Verified.
**Evidence:** `memory_size` is a message count (`lib/session.ml:19`);
`max_tool_output_len` is a byte count (`:20`). A grep for
`token_estimate|estimate_tokens|context_window|num_ctx` across `lib/` and
`bin/` returns nothing. The registry records no context window per model.

**What happens:** 40 messages overflows an 8k-context local model and wastes
90% of a 128k one. There is no way to answer "will this request fit?" before
sending it, so context-overflow errors are discovered by the server.

**Intent for the fix:** a cheap token estimator (a chars/4 heuristic is
adequate — do not add a tokeniser dependency for this), a `context_window`
field per model in the capability table (D1), and compaction driven by
*fraction of window consumed* rather than message count. Keep `memory_size` as
a user-facing override for people who want the old behaviour.

---

### H3 — The request prefix is cache-hostile by construction

**Confidence:** Verified for the code path. The caching semantics below are
from the `claude-api` skill (see §8) and are Anthropic-specific; the
automatic-caching behaviour of other providers is Inferred.
**Evidence:** `Session.history_for_llm` (`lib/session.ml:116-127`) applies
`truncate_tool_output` to every message at `idx < hist_len - 2` — that is,
re-derived on every call, with the boundary moving as history grows. A grep for
`cache_control|prompt_cach|cached_tokens` returns nothing.

**What happens:** a tool result renders in full while it is within the last two
messages, then renders truncated once it slides out. Its serialised bytes
*change* between turns. Prompt caching is strict prefix matching — any byte
change invalidates everything after it — so the cache diverges at that message
on every single turn and effectively never hits.

**Why it matters here specifically:** the agent loop resends the entire history
every turn. This is precisely the workload prompt caching exists for, and the
savings are large — DeepSeek and OpenAI apply prefix caching automatically, so
**this fix pays off on those providers with no provider-specific code at all**.
That makes it unusually high value for the effort.

**Intent for the fix:** truncate **once, at insertion time**, so a message's
serialised form is immutable for its lifetime. This makes the prefix
byte-stable and is a precondition for everything else in this area. Note it
changes what is stored, so it interacts with checkpointing.

Anthropic requires *explicit* `cache_control` breakpoints (max 4, with a
minimum cacheable prefix of roughly 1–4k tokens depending on model) and renders
`tools` → `system` → `messages`, so the natural breakpoints are after the tools
block and after the stable history prefix. **However — see M2: that is not
reachable through the current Anthropic transport.** Sequence the byte-stable
fix first; it stands alone and benefits the providers you can reach today.

Instrument the win: `usage.cache_read_input_tokens` (Anthropic) and
`usage.prompt_tokens_details.cached_tokens` (OpenAI-shaped) tell you whether
caching is actually working. `parse_usage` (`lib/providers/openai_compatible.ml:97`)
already extracts usage and is the right place to extend.

---

### H4 — Retry ignores `Retry-After`, guaranteeing free-tier failure

**Confidence:** Verified for the code; Inferred for the specific header values
returned by named providers.
**Evidence:** `Retry.delay_seconds` (`lib/provider.ml:73-75`) is pure
exponential backoff from a 0.5s base, capped at 30s. `Medium` — the default —
allows 4 attempts (`:50`), so the waits are 0.5s, 1s, 2s. Nothing reads response
headers; `Retry.classify` (`:67-70`) is deliberately structural, keying only on
HTTP status and Eio exception type.

**What happens:** a 429 is correctly classified as retriable at `Medium`
(`:60`), but the harness exhausts all its retries in 3.5 seconds and fails.
Free tiers commonly return a `Retry-After` in the tens of seconds.

**Why it matters here specifically:** 429 is the *normal steady state* of a
free tier, not an exceptional condition. The harness currently treats it as a
transient blip.

**Intent for the fix:** honour `Retry-After` and the `x-ratelimit-reset-*`
family when present, falling back to exponential backoff when absent. Better
still, avoid the 429: a client-side token-bucket limiter per provider, seeded
from a `rate_limits` field in the capability table (D1), keeps you under the
ceiling instead of discovering it.

The existing structural-classification discipline is good and `AGENTS.md`
explicitly prohibits string-scraping exception text (§8, "Ignoring
reasoning/thinking fields", "String-scraping raw exception strings"). Reading a
*typed header* off a response is not string-scraping — but you will need to
thread header access through `Caravan_error.Provider_failure`, which currently
carries only `status` and `body`. That is a small type change with several call
sites; check `lib/caravan_error.ml` and its `.mli`.

---

### H5 — Plain-reply-means-finished is wrong in agent mode

**Confidence:** Verified.
**Evidence:** `Agent.is_finished` (`lib/agent.ml:25-33`) — with no
`finish_reason` and no pending tool calls, any non-empty content counts as
done.

**What happens:** a model that opens with "Sure, I'll read that file for you!"
before acting is treated as having completed the task.

**Why it matters here specifically:** conversational preamble before tool use
is characteristic of small instruct-tuned models. Combined with C2 this is the
dominant silent-failure path.

**Intent for the fix:** in agent mode, completion should require an explicit
`finish` call. On a bare-text turn, re-prompt once with a format reminder
before giving up — but bound the retries so a model that *never* emits tool
calls fails fast and loudly with a diagnostic ("model produced no tool calls in
N consecutive turns; it may not support function calling — try
`tool_call_mode = text`") rather than looping.

This must stay mode-aware: the interactive REPL (`Session.turn`) *should* treat
a plain reply as a complete answer. Only `Agent.run*` needs the stricter rule.

---

## 5. Medium and low findings

**M1 — Tool-schema bloat.** All 14 static tools, plus `delegate` and any MCP
tools, are serialised into every request
(`lib/providers/openai_compatible.ml:67-77`). On an 8k-context model that is a
large fixed per-turn tax, and small models choose badly when given many
options. *Intent:* tool profiles — a `core` set (`bash`, `read_file`,
`write_file`, `ls`, `grep`, `finish`) selected by default for
low-capability models, `full` for the rest, driven by D1. Confidence: Verified.

**M2 — The Anthropic backend is a compatibility shim.** The registry points
`anthropic` at `https://api.anthropic.com/v1` through the shared
OpenAI-compatible engine (`lib/providers/registry.ml`). Anthropic's
OpenAI-compatibility layer is explicitly limited-fidelity; Anthropic-native
levers — `cache_control` breakpoints, extended thinking, effort control,
structured stop/refusal reasons — are not reachable through it. *Intent:* if
Anthropic is meant to be a first-class backend rather than a convenience entry,
it needs a native provider module speaking `/v1/messages`. This is a
significant piece of work and I would **not** put it in the critical path —
but H3's Anthropic half is blocked on it, so decide consciously. Confidence:
Verified that the shim is what's wired; Inferred as to its exact feature
coverage — check current Anthropic documentation before committing.

**M3 — The Claude model ladder is a generation stale.** `registry.ml` lists
`claude-sonnet-4-5` as default with `claude-opus-4-5` in the hints and ladder.
Current models are Opus 5 (`claude-opus-5`), Sonnet 5 (`claude-sonnet-5`),
Fable 5 (`claude-fable-5`), with Haiku 4.5 (`claude-haiku-4-5`) still current
as the small model. *Intent:* refresh, and while you are there consider that a
hardcoded model ladder will always drift — a note in the docs about
`caravan models` being authoritative costs nothing. Confidence: Verified
against the `claude-api` skill.

**M4 — No free-tier awareness in docs or registry.** Neither `README.md` nor
`docs/src/providers.md` contains the word "free". OpenRouter's `:free` model
suffix is unmentioned. Absent from the registry entirely: Cerebras, GitHub
Models (free with a GitHub token), Nvidia NIM, Cloudflare Workers AI — all
plausible free-tier entries for the target user. *Intent:* a "getting started
for free" path in the docs is arguably the highest-leverage *non-code* change
in this audit, and it is the one a new user hits first. Confidence: Verified
for the absence; Inferred for which providers are worth adding.

**M5 — `Registry.entry` is capability-blind.** Covered as D1 below; recorded
here so the findings list is complete.

**L1 — `gen_tools.ml` uses substring matching to discover tool modules.** The
generator scans `.ml` files for the literal `module <Capname>`
(`lib/tools/gen_tools.ml`), with a hand-maintained exclusion for `delegate.ml`.
It works, but it will silently mis-generate on an unexpected file. Low priority;
worth knowing about if you add tools, which the tool-profile work (M1) implies
you might.

---

### D1 — Design intent: make the registry capability-aware

Not a bug — the spine of the refactor, and the thing I most want to survive
contact with implementation.

`Registry.entry` (`lib/providers/registry.ml`) records `base_url`, `key_env`,
`default_model`, `model_hints`, `notes` — *where* a backend lives, never *what
it can do*. Consequently every behavioural decision in the harness is a global
constant: one `memory_size`, one retry mode, one tool list, one parsing
strategy, for a 1B local llama and for Opus 5 alike.

**The intent:** introduce a capability record — carrying at minimum context
window, native tool-calling fidelity (native / flaky / none), streaming
tool-call support, cache semantics (none / automatic / explicit), and known
free-tier rate limits — and derive behaviour from it:

| Capability | Drives |
|---|---|
| `context_window` | compaction threshold (H2) |
| `tool_calling` | fallback parser activation (C2), tool profile (M1), system-prompt layer (C1) |
| `cache` | prefix strategy and breakpoints (H3) |
| `rate_limits` | client-side limiter and retry policy (H4) |

Note that every critical and high finding terminates in this table. That is why
it is the spine and not a nice-to-have: fix the bugs without it and the
harness is correct for today's models and brittle for tomorrow's.

Two cautions. **Capabilities are per-model, not per-provider** — OpenRouter
alone spans the whole range — so the lookup needs a sensible
model-pattern-matching story with a conservative default for unknown models
(assume small context, assume flaky tool calling; being wrong in that direction
degrades gracefully, the opposite direction fails). And **keep it declarative
and overridable from `config.toml`**: users will know things about their local
models that no shipped table can.

---

## 6. Open questions — for the maintainer, not the implementer

Please do not resolve these unilaterally.

1. **`strict_mode = 1`** (`lib/tools/bash.ml:6`) forbids `;` and newlines in
   bash commands, forcing one command per tool call. It buys verifiability at
   the cost of doubling or tripling round trips — and round trips are the
   scarce resource on a rate-limited free tier. Should this become
   capability-driven, or does the verification discipline take priority?
   *Judgment call.*

2. **`max_turns = 10`** (`lib/config.ml:620-621`) is tight for weak models,
   which need more steps to reach the same place. Once C4 makes the budget
   actually enforceable, 10 may prove too small — the current value has never
   been tested under a working ceiling. Suggest revisiting *after* C4, with
   measurements. *Judgment call.*

3. **System prompt: append or replace?** (C1) — whether a user-supplied
   `system` value supplements or overrides the shipped default. I lean append
   with an explicit escape hatch, but it is a UX decision with
   backwards-compatibility implications for existing configs.

4. **Native Anthropic provider (M2)** — real work, and it competes with
   breadth-of-free-providers work (M4) for the same time. Both are defensible;
   they serve different users.

5. **Is a text-mode tool protocol a first-class feature or a fallback?** (C2)
   If first-class — advertised, configurable via `tool_call_mode`, documented —
   it becomes a genuine differentiator for local-model users. If a silent
   fallback, it is a bug fix. This changes the scope materially.

---

## 7. Suggested sequencing

Dependency-ordered. Each tier should build, test, and be independently
reviewable — please do not collapse them into one change.

**Tier 0 — foundation.** D1's capability record, with conservative defaults and
no behaviour wired to it yet. Landing the shape first means Tiers 1–2 have
somewhere to put their decisions instead of adding more global constants.

**Tier 1 — stop the bleeding.** C4 (budget), C1 (system prompt + preamble), C2
(fallback tool-call parsing), H5 (finish discipline), C3 (pair-aware eviction).
C4 first — it is small, self-contained, and it is the one currently costing
real money. C2 and H5 are entangled and should land together with tests
covering their interaction.

**Tier 2 — economics.** H3 (byte-stable prefix; the non-Anthropic half pays off
immediately), H2 (token budgeting), H4 (`Retry-After` + limiter), H1 (cheaper
compaction), M1 (tool profiles).

**Tier 3 — reach.** M4 (free-tier docs and providers — consider pulling the
docs half forward, it is cheap and high-visibility), M3 (model refresh), M2
(native Anthropic, if chosen).

---

## 8. Notes for the implementing agent

- **Read `AGENTS.md` first, in full.** It is 29 KB of genuinely binding
  convention, not boilerplate. §8 ("Common Traps & Anti-Patterns") is 25+
  named anti-patterns, several of which sit directly on this work: no mutable
  fields in `Session.t`; no `printf` in library code (use `Trace.emit`); never
  swallow `Eio.Cancel.Cancelled`; construct `chat_message` only via the
  `Types.*_msg` smart constructors; `Yojson.Safe` only; wire JSON and export
  JSON are distinct. §12 is the PR checklist and is the definition of done.

- **Load the `claude-api` skill** before writing anything touching Anthropic
  model IDs, pricing, or caching semantics. §5 M2/M3 and §4 H3 all depend on
  facts that drift; do not take my summary as current, re-check it.

- **Build and test:** `dune build` (must be warning-free), `dune runtest`. CI
  runs the matrix on OCaml 5.2.0 and 5.3.0 and additionally asserts
  `Caravan.opam` stays in sync with `dune-project` — if you add a dependency,
  regenerate it.

- **Testing conventions** (`AGENTS.md` §7): mock providers, never live API
  calls; spinner disabled in test sessions; table-driven tests over enumerated
  ones. `test/test_Caravan.ml:7-21` has a `make_mock_provider` helper that
  returns plain assistant messages — **it cannot currently produce tool calls**,
  so C2/C3/H5 will need an extended mock that emits `tool_calls` and one that
  emits tool-call-shaped *text*. Build that helper early; most of Tier 1
  depends on it.

- **New public API needs an `.mli` entry with a doc-comment.** Several modules
  here (`session`, `memory`, `provider`) have `.mli` files that will need
  updating in step.

- **Suggested regression tests**, since these failures are all silent:
  C2, a mock returning a fenced JSON tool call must execute the tool; C3, a
  session driven past its window must produce no orphaned `tool` message;
  C4, a run that compacts must still terminate at `max_turns`; H3, two
  consecutive `history_for_llm` calls must produce byte-identical prefixes for
  the unchanged portion.

- **A request:** where you disagree with this audit, say so in your report
  rather than quietly diverging. Several findings rest on inference about
  external API behaviour (marked accordingly) and may not survive contact with
  a live endpoint. A correction is a good outcome.

---

*Read-only audit. No files were modified in producing this document.*
