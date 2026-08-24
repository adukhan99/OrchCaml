# AGENTS.md — Caravan Coding Guidelines

> **Purpose**: This file captures the style, architecture, idioms, and known
> pitfalls of the Caravan codebase so that any AI model (Claude, GPT, Gemini,
> or other) can produce OCaml that slots seamlessly into the project without
> regressing quality or violating the established patterns.

---

## 1  Project Overview

Caravan is a **typed LLM orchestration framework** written in OCaml 5.1+.  It
provides:

- **Sessions** (multi-turn, memory-managed conversations)
- **Agents** (autonomous ReAct loops with turn budgets)
- **Chains** (composable typed LLM pipelines via Kleisli composition)
- **A plugin runtime** (spatiotemporal composability — reactive services,
  revertible effects, isolation realms)
- **Tools** (first-class module based; bash, file I/O, web search, delegation)
- **Providers** (pluggable LLM backends: Ollama, OpenAI-compatible, llama.cpp)
- A full CLI REPL, web cockpit, and embedded Lisp interpreter (Slip)

The runtime is Eio-based (structured concurrency).  JSON is handled via
`Yojson.Safe`.  Configuration is TOML via `otoml`.  PPX preprocessors:
`ppx_let`, `ppx_yojson_conv`.

---

### 1.1  Operating Philosophy ("Move Like Claude")

When contributing to or refactoring Caravan, adopt a disciplined, high-latent-layer problem-solving approach:

1. **Investigate Ground Truth First**: Never make assumptions or apply band-aid fixes based solely on visual symptoms. When an agent or REPL stalls, run background diagnostics, capture raw SSE streams, or trace Eio fiber execution to observe real runtime behavior.
2. **Abstract Space Over Enumeration**: Address systemic issues through typed, domain-level abstractions and architectural patterns rather than enumerating special-case conditionals.
3. **Strict Separation of Concerns in Streaming**:
   - **Connection State**: Treat HTTP 2xx response headers as connection success rather than keying success on text-token emission.
   - **UI Streaming**: Stream interactive feedback (such as reasoning/thinking tokens inside `<thought>` blocks) on-token to keep the user interface responsive.
   - **Prompt History**: Maintain clean message buffers (`buf`) for turn history—never leak UI stream artifacts or ephemeral thinking tokens into stored LLM context.
4. **Surgical Respect for Idioms**: Strictly adhere to functional OCaml 5.1+ conventions: zero object-orientation, immutable state (`Session.t`), GADT packed existentials, algebraic effects for side-effects, structured error handling (`Caravan_error`), and 2-space column-aligned formatting.

---

## 2  Repository Layout

```
Caravan/
├── lib/                    # Core library (package: Caravan)
│   ├── providers/          # Sub-library: CaravanProviders
│   └── tools/              # Sub-library: CaravanTools
├── bin/                    # CLI binary entry point
│   ├── main.ml             # REPL, slash commands, CLI subcommands
│   ├── editor.ml           # Line-editing engine
│   ├── render.ml           # Trace renderer sink
│   ├── subagents.ml        # Subagent composition for the CLI
│   └── web.ml              # Web cockpit HTTP server
├── test/                   # ppx_expect + ppx_inline_test test suite
├── docs/                   # mdbook documentation
├── examples/               # Example configurations and scripts
└── dune-project            # Dune 3.21, OCaml >= 5.1
```

### Key module responsibilities

| Module            | Role                                                          |
|-------------------|---------------------------------------------------------------|
| `Types`           | `role`, `chat_message`, `tool_call`, `result_with_meta`, `gen_options`, wire-JSON serialisation |
| `Provider`        | `PROVIDER` module type, `packed_provider` existential, `provider_spec` |
| `Tool`            | `TOOL` module type, `packed_tool` existential, dispatch pipeline |
| `Session`         | Stateful multi-turn sessions, memory management, tool execution, streaming |
| `Agent`           | Autonomous agent loops with turn budgets and nudge injection   |
| `Memory`          | `MEMORY` module type, `Ring`, `Summary`, `Hierarchical`, `Noop` backends |
| `Plugin`          | Spatiotemporal composability runtime (typed keys, services, events, fibers, reconciliation) |
| `Plugin_host`     | Config-driven plugin composition, MCP mount, builtin-tool fibers |
| `Config`          | TOML reading/writing, env-var resolution, editable settings    |
| `Effects`         | OCaml 5 algebraic effects for tool execution, permissions, networking |
| `Trace`           | Structured event stream + JSONL audit sink                     |
| `Parser`          | Typed output parsers (monadic combinator library)              |
| `Value`           | Structured data + LISPy query pipeline                         |
| `Prompt`          | Writer monad for composing conversation message lists           |
| `Chain`           | Kleisli-style composable pipelines (`|>>`)                     |
| `Subagent`        | Cold-start worker delegation                                    |
| `Caravan_error`   | Unified error types + `humanize` for user-facing messages      |
| `Permission`      | `auto` / `ask` / `readonly` tool policies                      |

---

## 3  Coding Style

### 3.1  Formatting & Layout

- **Two-space indent** everywhere.  Never tabs.
- **Column-aligned** record fields and match arms when it improves scan-ability:
  ```ocaml
  type role =
    | System
    | User
    | Assistant
    | Tool of string
  ```
- **Padded arrows** in pattern matches for vertical alignment:
  ```ocaml
  | Ok r    -> r
  | Error e -> failwith e
  ```
- Short one-line functions are fine:
  ```ocaml
  let null = Null
  let string s = String s
  ```
- Avoid trailing whitespace.  Keep blank lines between logical sections.
- Use `(** ... *)` for doc-comments on public-facing values; use `(* ... *)` for
  implementation notes.

### 3.2  Naming Conventions

- **Module names**: `CamelCase` (`Plugin_host`, `Caravan_error`).
- **Type names**: `snake_case` — `chat_message`, `packed_provider`, `gen_options`.
- **Value/function names**: `snake_case` — `make_message`, `history_for_llm`.
- **Module type names**: `UPPER_CASE` — `PROVIDER`, `TOOL`, `MEMORY`.
- Existential wrappers are named `packed_*` (`packed_provider`, `packed_tool`, `packed_memory`).
- Accessor functions on packed existentials: `name_of_packed`, `schema_of_packed`,
  `complete_packed`, etc.
- Setter-style functions on session: `set_system`, `set_options`, `with_tools`,
  `with_model`, `with_provider`, `with_spinner_config`.  The `set_*` pattern
  takes `t -> arg -> t`; the `with_*` pattern often reverses the argument order
  or closes over a value.

### 3.3  Module Structure Patterns

- **Interface-first**: public modules have `.mli` files.  Write the `.mli` first
  or keep it updated.  The `.mli` is the contract; hide implementation details.
- **Doc-comments go in the `.mli`**, not the `.ml` (exception: `(** ... *)` on the
  first line of an `.ml` is fine as a module-level summary).
- Modules follow a consistent section layout:
  1. Module-level doc-comment
  2. `open` declarations
  3. Type definitions
  4. Smart constructors / constants
  5. Core functions (pure first, effectful later)
  6. JSON serialization / deserialization
  7. Sub-modules (nested `sig`/`struct` blocks)

### 3.4  OCaml Version & Features

- **OCaml 5.1+** is required.  Use OCaml 5 algebraic effects (`Effect.t`,
  `Effect.perform`, `Effect.Deep.try_with`).
- Use **`ppx_let`** (`let%bind`, `let%map`, `let*`) in monadic code:
  ```ocaml
  let%bind role = role_r in
  let%map tcs = tool_calls_r in
  { role; content; ... }
  ```
- Use **`ppx_yojson_conv`** (`[@@deriving yojson]`) only for simple
  round-trippable records (e.g. `usage`, `gen_options`).  Hand-write JSON
  serialisation for anything with custom wire formats or optional fields.
- **`ppx_expect`** and **`ppx_inline_test`** for the test suite.
- Use `Fun.protect ~finally:...` for resource cleanup, not manual try/finally.

---

## 4  Architecture Patterns

### 4.1  First-Class Module Existentials ("Packed" Pattern)

The codebase's central abstraction is the **packed existential** — a GADT that
pairs a first-class module with its configuration/state, erasing the config type:

```ocaml
type packed_provider =
  | Provider : (module PROVIDER with type config = 'c) * 'c -> packed_provider
```

This pattern appears for:
- `Provider.packed_provider` — LLM backends
- `Tool.packed_tool` — tools
- `Memory.packed_memory` — memory backends

**Rules**:
1. To unpack, pattern-match locally:
   `let Provider ((module P), cfg) = provider in P.complete net cfg ...`
2. Provide `*_packed` convenience functions that hide the unpack:
   `let complete_packed net ?model (Provider ((module P), cfg)) msgs = ...`
3. Never expose the config type through the API — that's the whole point.

### 4.2  Immutable Session Updates

`Session.t` is abstract and immutable.  All mutators return a new `t`:

```ocaml
let sess = Session.create ~tools model provider in
let sess = Session.set_system sess "You are helpful." in
let sess = Session.set_options sess (fun o -> { o with temperature = Some 0.7 }) in
```

**Never** add mutable fields to `Session.t`.  The REPL state (`repl_state` in
`bin/main.ml`) is the only mutable session holder — it re-binds `st.session`
after each turn.

### 4.3  Result Types for Fallibility

- Use `(value, string) result` for operations that can fail with a human-readable
  message (tool parsing, chain steps, configuration parsing).
- Use `_result` suffixed function names for the result-returning variant;
  provide a raising wrapper with the unsuffixed name:
  ```ocaml
  let tool_call_of_json_result json = ...   (* returns Result *)
  let tool_call_of_json json = ...          (* raises on Error *)
  ```
- For JSON parsing, always provide both `*_result` and raising variants.

### 4.4  Algebraic Effects

OCaml 5 effects are used for cross-cutting concerns that must not pollute
function signatures:

| Effect              | Purpose                         |
|---------------------|---------------------------------|
| `Exec_tool`         | Tool execution interception     |
| `Ask_permission`    | Permission checks               |
| `Log_event`         | Structured logging              |
| `Spawn_subagent`    | Subagent delegation             |
| `Parse_warning`     | Graceful parse-error reporting  |
| `Get_net`           | Ambient network capability      |

**Rules**:
1. Effects are defined once in `effects.ml` / `effects.mli`.
2. Each tool defines its own `Exec : input -> output Effect.t` in its `TOOL` module.
3. Effect handlers fall back gracefully via `Effect.Unhandled`:
   ```ocaml
   try Effect.perform (T.Exec input)
   with Effect.Unhandled _ -> T.execute input
   ```
4. Never swallow `Eio.Cancel.Cancelled` — always re-raise it.

### 4.5  Plugin Runtime

The plugin system implements the **spatiotemporal composability** paper
(Shi, Zhang, Cui — Peking University / DeepSeek-AI, 2026).

**Key invariants**:
- Every side-effect through a context is paired with an inverse (disposer).
- Disposers run in LIFO order on unload.
- `provide` is single-source per realm — `Duplicate_provider` if violated.
- Components declare their `inject` (dependencies) and `provide` (offerings).
- Fibers activate reactively when all `inject` keys are satisfied.
- The `settle` loop is synchronous and runs to completion.

### 4.6  Trace, Not Print

The library **never prints user-facing output directly**.  All output goes
through `Trace.emit`:

```ocaml
Trace.emit (Trace.Tool_call_start { name = tc.name; args = tc.args });
```

Front-ends install sinks (ANSI renderer, JSONL transcript, test harness).
Use `Trace.error` for user-facing failures and `Trace.log` for diagnostics.

### 4.7  The TOOL Module Type

Every tool is a first-class module satisfying:

```ocaml
module type TOOL = sig
  val name        : string
  val aliases     : string list
  val description : string
  type input
  type output
  val json_schema    : unit -> Yojson.Safe.t
  val parse_args     : Yojson.Safe.t -> (input, string) result
  val format_output  : output -> string
  val is_mutating    : bool
  val describe_action : input -> string
  type _ Effect.t += Exec : input -> output Effect.t
  val execute : input -> output
end
```

**When adding a new tool**:
1. Create `lib/tools/<name>.ml` with a `module <Name> : TOOL`.
2. Register it in `lib/tools/gen_tools.ml` and `lib/tools/dune`.
3. `json_schema` returns a JSON Schema object; `parse_args` validates against it.
4. `is_mutating` must honestly reflect whether the tool has side-effects.
5. `describe_action` produces a human-readable summary for permission prompts.
6. `aliases` lets LLMs use alternative names (e.g. `"sh"` for `"bash"`).

---

## 5  JSON Handling

### 5.1  Yojson.Safe Only

**Always** use `Yojson.Safe`, never `Yojson.Basic`.

### 5.2  Parse, Don't Validate

- Parse JSON at domain boundaries into OCaml types immediately.
- Use `Yojson.Safe.Util.member`, `to_string`, `to_int`, etc.
- Wrap in `try ... with Yojson.Safe.Util.Type_error` for structured error
  handling.

### 5.3  UTF-8 Sanitisation

All strings entering the system from external sources (LLM responses, tool
output) pass through `Types.parse_utf8`, which replaces malformed UTF-8 with
U+FFFD:

```ocaml
let make_message ?tool_calls role content = {
  role;
  content = parse_utf8 content;
  ...
}
```

**Never bypass `make_message` to construct a `chat_message` directly.**

### 5.4  Wire vs. Export JSON

Two serialisation paths exist:
- `chat_message_to_json` — **export/persistence**: includes `timestamp` and all
  internal fields.
- `chat_message_to_wire_json` — **API calls**: omits `timestamp` and other
  Caravan-internal fields that strict OpenAI endpoints reject.

**Never confuse the two.**  API calls must use `messages_to_wire_json`.

### 5.5  Tool Argument Sanitisation

LLMs emit raw control characters in JSON strings.  Always sanitise tool args
via `Types.sanitize_json_args` (round-trips through `Yojson.Safe`) — **never**
write manual control-character escaping.  That causes double-escaping bugs.

---

## 6  Error Handling

### 6.1  Error Categories

Use `Caravan_error.t` for structured errors:

```ocaml
type t =
  | Tool_error of string
  | Tool_not_found of string
  | Json_parse_error of string
  | Provider_error of string
  | Mcp_error of string
  | Subagent_error of string
  | Eio_error of string
  | Permission_denied of string
  | Exception of string
```

### 6.2  Humanization

`Caravan_error.humanize` converts raw exceptions into user-friendly messages
with hints.  Always prefer `humanize` in user-facing catch sites:

```ocaml
with exn ->
  Trace.error "repl" "%s" (Caravan_error.humanize exn);
  println_ansi (red (Caravan_error.humanize exn))
```

### 6.3  Safe Execution

`Caravan_error.safe_run` wraps a thunk, catching everything **except**
`Eio.Cancel.Cancelled` (which must always propagate).

### 6.4  Provider Error Handling & Structured Exceptions

Providers must parse raw HTTP error response bodies into structured domain errors at the provider boundary rather than formatting exceptions into raw unstructured strings (`failwith`).

- Define structured exception variants in `Caravan_error` (such as `Caravan_error.Provider_failure` containing `provider`, `status`, `body`, and `detail`).
- Raise structured exceptions using `Caravan_error.raise_provider_failure` directly from provider clients when receiving non-2xx HTTP responses.
- `Caravan_error.humanize` pattern-matches on `Provider_failure` directly, presenting clean messages with actionable user hints without scraping raw exception strings.

---

## 7  Testing

### 7.1  Test Framework

- Use `ppx_inline_test` (`let%test`, `let%test_unit`) for boolean/unit tests.
- Use `ppx_expect` (`let%expect_test`) for output-comparison tests.
- Tests live in `test/test_Caravan.ml`, `test/test_plugin.ml`,
  `test/test_plugin_host.ml`.

### 7.2  Shared Test Infrastructure

`test/test_Caravan.ml` defines three module-level helpers.  **All tests must
use these instead of re-declaring boilerplate inline.**

```ocaml
(** Stateless mock provider that always returns [reply_content]. *)
let make_mock_provider ?(pname = "mock") reply_content : Provider.packed_provider

(** Session pre-configured with spinner disabled. *)
let make_session ?(tools = []) model provider : Session.t

(** Bracket: write TOML to a temp file, set CARAVAN_CONFIG, run [f path],
    restore env via Fun.protect on exit. *)
let with_tmp_config ~name ~toml_content (f : string -> unit) : unit
```

Use `make_mock_provider` whenever the test only needs a plain assistant
response.  Only write a custom inline module when the provider needs state
(e.g. a `call_count` ref, multi-turn sequencing, or finish-tool injection).

Use `with_tmp_config` for every test that reads or writes `Config.*` —
it guarantees cleanup even when the test raises.

### 7.3  Table-Driven Tests Over Enumerated Tests

When multiple tests exercise the same code path with different inputs, write
one test unit with an inner `check` closure:

```ocaml
let%test_unit "my_feature" =
  let check ~input ~expected =
    assert (my_fn input = expected)
  in
  check ~input:"a" ~expected:1;
  check ~input:"b" ~expected:2;
  check ~input:"c" ~expected:3
```

This pattern catches regressions across **all** cases on every run, makes
adding new cases trivial, and keeps the file compact.  Examples in the
codebase:

| Test unit            | Cases covered                              |
|----------------------|--------------------------------------------|
| `usage_parsing`      | 3 providers (openai, llama_cpp, ollama)    |
| `doctor_run_checks`  | all-pass, missing key, unknown provider    |
| `tool_dispatch_table`| 8 tools (read, write, grep, sed, bash, …)  |
| `cli_resolve`        | 8 resolution scenarios                     |

### 7.4  Mock Providers

For stateless behavior use `make_mock_provider` (§7.2).  For stateful
behavior (multi-turn sequencing, tool-call injection) write a minimal inline
module.  Keep it as short as possible — stream can often delegate to complete:

```ocaml
let call_count = ref 0 in
let module P : Provider.PROVIDER with type config = unit = struct
  type config = unit
  let name = "seq"
  let complete _net _cfg ?model:_ ?options:_ ?tools:_ _msgs =
    incr call_count;
    let tc = Types.{ id = "c"; name = "finish";
                    args = {|{"summary":"done"}|}; extra_content = None } in
    Types.wrap_result ~raw_response:"" ~model:"m" ~provider:"p"
      (Types.assistant_tool_msg ~tool_calls:[tc] "")
  let stream _net _cfg ?model:_ ?options:_ ?tools:_ msgs ~on_token:_ =
    complete _net _cfg msgs
  let list_models _ _ = []
end in
let provider = Provider.Provider ((module P), ()) in
```

### 7.5  Spinner Config in Tests

Use `make_session` — it disables the spinner automatically.  The explicit form
for reference:

```ocaml
|> Session.with_spinner_config { enabled = false; get_verb = fun _ -> "mock" }
```

### 7.6  Eio in Tests

Tests that need networking or clocks must wrap in `Eio_main.run`:

```ocaml
let%test_unit "my_test" =
  Eio_main.run (fun env ->
    ...)
```

### 7.7  Config Tests

Use `with_tmp_config` — **never** open-code the setup/teardown sequence:

```ocaml
(* RIGHT *)
let%test_unit "my_config_test" =
  with_tmp_config ~name:"my_cfg" ~toml_content:"key = \"value\"\n" (fun _ ->
    assert (Config.get_string "key" = Some "value"))

(* WRONG — brittle, leaks state on failure *)
let%test_unit "my_config_test" =
  let tmp = "my_cfg.toml" in
  let oc = open_out tmp in output_string oc "..."; close_out oc;
  Unix.putenv "CARAVAN_CONFIG" tmp;
  Config.reload ();
  ...;
  Sys.remove tmp; Unix.putenv "CARAVAN_CONFIG" ""; Config.reload ()
```

---

## 8  Common Traps & Anti-Patterns

### ❌ Double-escaping JSON

**Wrong**: Manually escaping `\n`, `\t` in JSON strings before passing to Yojson.
**Right**: Let `Yojson.Safe.to_string` handle escaping.  Use
`Types.sanitize_json_args` for round-tripping dirty LLM output.

### ❌ Constructing `chat_message` records directly

**Wrong**: `{ role = User; content = raw_string; timestamp = 0.0; ... }`
**Right**: `Types.user_msg raw_string` — the smart constructors enforce UTF-8
sanitisation and set timestamps.

### ❌ Using `Yojson.Basic`

**Wrong**: `Yojson.Basic.from_string`.
**Right**: `Yojson.Safe.from_string`.  The entire codebase is `Yojson.Safe`.

### ❌ Printing directly from library code

**Wrong**: `Printf.printf "Tool result: %s\n" result`
**Right**: `Trace.emit (Trace.Tool_call_end { name; output; duration })`

### ❌ Catching `Eio.Cancel.Cancelled`

**Wrong**: `with _ -> Error "something"` (swallows cancellation)
**Right**: `with Eio.Cancel.Cancelled _ as exn -> raise exn | exn -> Error ...`

### ❌ Mutable state in `Session.t`

**Wrong**: Adding `mutable` fields to the session record.
**Right**: Return a new `t` from every mutation.

### ❌ Hardcoding provider details

**Wrong**: `if provider = "openai" then ...` scattered in business logic.
**Right**: All provider-specific logic lives in `lib/providers/`.  Core modules
operate on `packed_provider` and are backend-agnostic.

### ❌ Forgetting wire vs. export JSON

**Wrong**: Sending `chat_message_to_json` payloads to an LLM API (includes
`timestamp`, which strict endpoints reject).
**Right**: Use `chat_message_to_wire_json` / `messages_to_wire_json` for API
calls.

### ❌ `option` overuse for tool_calls

The `tool_calls` field is `tool_call list option`.  `None` means "not present"
(plain message); `Some []` means "present but empty" (rare but valid).  Do not
conflate them:

```ocaml
(* Correct check for "has tool calls" *)
match reply.tool_calls with
| Some tcs when tcs <> [] -> (* process *)
| _ -> (* plain reply *)
```

### ❌ Breaking the packed existential abstraction

**Wrong**: Trying to extract the config type from a `packed_provider` outside
the module that packed it.
**Right**: Use the `*_packed` accessor functions.

### ❌ Blocking in Eio fibers

**Wrong**: Using `Unix.sleep` or other blocking calls inside Eio fibers.
**Right**: Use `Eio.Time.sleep` or Eio-aware alternatives.

### ❌ Nesting `Eio_main.run`

**Wrong**: Calling `Eio_main.run` from within an Eio fiber.
**Right**: Pass `net` and `clock` down from the single top-level
`Eio_main.run`.  Tools use `Effects.get_net ()` to access the ambient network.

### ❌ Using `List.mem` on non-structural types

**Wrong**: `List.mem fiber fibers` where `fiber` contains closures.
**Right**: Use `List.exists (fun f -> f == target)` for physical identity, or
compare on `uid` / `name`.

### ❌ Creating global mutable service keys

Plugin `Key.create` is generative — each call produces a unique key.  Never
call `Key.create` in a let binding that runs multiple times; define keys as
module-level constants:

```ocaml
(* RIGHT — one key per module load *)
let provider : Provider.packed_provider Key.t = Key.create ~name:"provider" ()

(* WRONG — new key each time *)
let get_provider ctx =
  let k = Key.create () in  (* BUG: this is a fresh key every call! *)
  Plugin.get ctx k
```

### ❌ String-scraping raw exception strings for error recovery

**Wrong**: Formatting structured HTTP error JSON into string exceptions (`failwith "error 400: {...}"`) and using string indexing/slice searching (`String.index str '{'`) to extract the JSON payload in error handlers.
**Right**: Parse response payloads into structured domain types (`provider_error_detail`) at the provider boundary when the HTTP response arrives, and raise structured exception variants (`Caravan_error.Provider_failure`).

### ❌ Unsafe `Yojson.Safe.Util.member` chaining on nullable fields

**Wrong**: Chaining `Yojson.Safe.Util.member "field"` on values that may be `` `Null `` or non-associative objects (e.g. `json |> member "metadata" |> member "raw"`), which throws unhandled `Yojson.Safe.Util.Type_error` exceptions.
**Right**: Use safe member lookup helper functions or `Parser.permissive_json` combinators to parse and extract JSON fields safely.

### ❌ Keying a streaming fallback guard on text-token emission

**Wrong**: Tracking whether `on_token` was called (`tokens_emitted`) to decide if a stream "succeeded" before triggering a fallback. Tool-call-only responses from the LLM emit zero content tokens, so this flag stays `false` on every agentic tool-use turn — causing the fallback to fire silently on every tool call, doubling request volume and producing no streamed output.
**Right**: Set a `stream_succeeded` flag immediately after receiving an HTTP 2xx response header. The fallback should only fire when the *connection* failed (before any SSE data), not when the response body happened to contain no text.

### ❌ Hand-rolled JSON record parsing / destructuring

**Wrong**: Manually extracting fields from JSON records via verbose `member "field"` and custom `member_opt` helper chains instead of deriving type-safe JSON conversion.
**Right**: Annotate simple round-trippable records with `[@@deriving yojson]` (using `yojson_safe` for embedded `Yojson.Safe.t` AST fields) to generate automatic, type-safe serialization and deserialization.

### ❌ Ignoring reasoning/thinking fields in SSE streaming

**Wrong**: Only parsing `"content"` from delta chunks in streaming providers. Reasoning models (like DeepSeek-R1 or OpenRouter stealth models) stream their reasoning steps inside `"reasoning"` or `"reasoning_content"` keys while sending empty `"content"`. Ignoring these keys makes the CLI appear frozen/stuck for minutes before any text appears.
**Right**: Parse and stream reasoning tokens (e.g. wrapped in `<thought>` blocks) on-token to keep the interface active and responsive, but do NOT accumulate them into the final message history `buf` to avoid prompt bloating.

### ❌ Per-test inline MockProvider when a shared helper exists

**Wrong**: Defining a new 10-line `module MockProvider` inside every test that
only needs a plain assistant reply.
**Right**: Use `make_mock_provider reply` from the shared test infrastructure.
Only write a custom inline module when the provider must carry state
(call counters, multi-turn sequences, tool-call injection).

### ❌ One test unit per input case

**Wrong**: Writing `let%test_unit "feature_case_a"`, `let%test_unit
"feature_case_b"`, … for the same function under different inputs — one failure
leaves all subsequent cases unexercised and the file grows without bound.
**Right**: Write one `let%test_unit "feature"` with an inner `check` closure
called for every case (see §7.3).  Each call is independently labelled with a
comment; a failure reports the exact case without abandoning the rest.

---

## 9  Configuration Resolution Order

Settings resolve top-down:
1. **CLI flag** (`--provider`, `--model`, etc.)
2. **Environment variable** (`CARAVAN_PROVIDER`, `CARAVAN_MODEL`, etc.)
3. **`config.toml`** (under `[orchestrator]`, then top-level fallback)
4. **Default** (hard-coded in `Config` or `Registry`)

Functions like `Config.get_string_opt`, `Config.get_int_opt`,
`Config.get_bool_opt` codify this cascade.

---

## 10  Build & Run

```bash
# Build
dune build

# Run tests
dune runtest

# REPL
dune exec bin/main.exe

# One-shot agent
dune exec bin/main.exe -- run "Describe the project structure"

# Web UI
dune exec bin/main.exe -- web

# Diagnostics
dune exec bin/main.exe -- doctor
```

---

## 11  Dependencies

Core runtime dependencies (from `dune-project`):

| Library       | Purpose                           |
|---------------|-----------------------------------|
| `eio`         | Structured concurrency runtime    |
| `eio_main`    | Top-level Eio entry point         |
| `cohttp-eio`  | HTTP client for LLM API calls     |
| `ssl` / `eio-ssl` | TLS for HTTPS endpoints      |
| `yojson`      | JSON parsing and serialisation    |
| `cmdliner`    | CLI argument parsing              |
| `re`          | Regular expressions (PCRE-style)  |
| `otoml`       | TOML configuration parsing        |
| `fmt`         | Format-based pretty printing      |
| `uutf`        | UTF-8 validation and sanitisation |
| `redis-sync`  | Optional Redis session store      |

Dev/test:
| `ppx_expect`       | Expect-test framework         |
| `ppx_inline_test`  | Inline test framework         |
| `ppx_let`          | Monadic let bindings          |
| `ppx_yojson_conv`  | JSON deriving for records     |

---

## 12  Checklist for Pull Requests

Before submitting code, verify:

- [ ] `dune build` succeeds with no warnings
- [ ] `dune runtest` passes (all inline and expect tests)
- [ ] New public API has an `.mli` entry with a doc-comment
- [ ] JSON serialisation uses `Yojson.Safe` only
- [ ] No direct `printf`/`print` in library code — use `Trace.emit`
- [ ] `chat_message` values constructed via `Types.*_msg` smart constructors
- [ ] Tool argument strings sanitised via `sanitize_json_args` where needed
- [ ] `Eio.Cancel.Cancelled` is never swallowed
- [ ] New tools have `is_mutating` set honestly
- [ ] Mock providers used in tests (no real API calls)
- [ ] Spinner disabled in test sessions
- [ ] No mutable fields added to `Session.t`
