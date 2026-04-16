# OrchCaml — Typed LLM Orchestration Framework

A functionalised, type-safe LLM orchestration library and TUI for OCaml. Think LangChain meets MPI: typed message passing, composable pipelines, streaming-aware I/O, and a slick interactive terminal.

---

## User Review Required

> [!IMPORTANT]
> **No HTTP client is currently installed.** We need to install `cohttp-lwt-unix` (and its deps: `uri`, `tls`, etc.) and one TUI library. This requires running `opam install` — which I'll propose as a command for you to approve before starting.

> [!IMPORTANT]
> **API key design:** For OpenAI/Anthropic/etc., keys will be read from environment variables (e.g. `OPENAI_API_KEY`) or a `~/.orchcaml/config.toml` file. The config loading code will have clearly annotated `(* API_KEY_SOURCE *)` markers where you can swap the source later.

> [!NOTE]
> **Ollama is already running** at `localhost:11434` with models including a Qwen 3.5 derivative and `gpt-oss:20b`. The Ollama backend will work out of the box with zero config.

---

## Architecture Overview

```
OrchCaml
├── lib/                        ← core library (OrchCaml)
│   ├── types.ml                ← typed message + role definitions
│   ├── provider.ml             ← abstract backend interface (module signature)
│   ├── providers/
│   │   ├── ollama.ml           ← Ollama REST API backend
│   │   └── openai.ml          ← OpenAI-compatible backend (annotated API key)
│   ├── chain.ml                ← composable pipeline: prompt → llm → parser
│   ├── parser.ml               ← typed output parsers (string, json, list, etc.)
│   ├── memory.ml               ← conversation memory (sliding window, summary)
│   ├── template.ml             ← prompt templating with {{variable}} syntax
│   └── session.ml              ← interaction session manager
└── bin/
    └── main.ml                 ← TUI entry point (cmdliner + readline-style REPL)
```

---

## Core Design Principles

### Typed Messages (`types.ml`)

Messages carry a `role` (System | User | Assistant | Tool) and a typed `content`:

```ocaml
type role = System | User | Assistant | Tool of string
type 'a message = { role: role; content: 'a; timestamp: float }
type chat_message = string message

(* Chain I/O is typed end-to-end *)
type ('input, 'output) chain
```

### Provider Abstraction (`provider.ml`)

A first-class module signature, so you can swap backends without changing pipeline code:

```ocaml
module type PROVIDER = sig
  type config
  val name : string
  val complete : config -> chat_message list -> string Lwt.t
  val stream   : config -> chat_message list -> string Lwt_stream.t
end
```

### Composable Chains (`chain.ml`)

Inspired by functional composition — every step is a typed `'a -> 'b Lwt.t` function, composed with `|>`:

```ocaml
(* Conceptually: *)
let my_chain =
  Template.render ~vars:[("topic", topic)]
  |>> Ollama.complete ~model:"gpt-oss:20b"
  |>> Parser.json_field "answer"
  |>> Parser.string
```

The `|>>` operator is a typed Lwt-aware bind, giving compile-time guarantees that parser input matches LLM output type.

### Output Parsers (`parser.ml`)

Pre-built parsers mirroring LangChain's output parsers but type-safe:

| Parser | Output type | Notes |
|---|---|---|
| `string` | `string` | passthrough |
| `json` | `Yojson.Safe.t` | full JSON tree |
| `json_field k` | `Yojson.Safe.t` | extract a key |
| `list` | `string list` | newline/bullet split |
| `bool` | `bool` | yes/no/true/false |
| `extract_code` | `string` | strips markdown fences |
| `structured s` | `'a` | ppx_yojson_conv-driven typed records |

### Memory (`memory.ml`)

- **Buffer memory**: fixed sliding window of N messages
- **Summary memory**: uses LLM to compress history when over token budget
- Pluggable: `chain` accepts memory as a first-class argument

### Prompt Templates (`template.ml`)

Simple `{{var}}` interpolation with an optional schema for validation — no dependencies, pure OCaml.

### Session Manager (`session.ml`)

Manages multi-turn interactions: wraps a chain + memory into a stateful session, suitable for long conversations or agent loops.

---

## TUI Design (`bin/main.ml`)

A readline-style interactive REPL using `cmdliner` for subcommand dispatch and ANSI escape codes via `fmt` for styling (no heavy TUI dep required in V1):

```
╔══════════════════════════════════════════╗
║  OrchCaml  v0.1  —  LLM Orchestrator    ║
╚══════════════════════════════════════════╝

Commands:
  /model <name>       — switch model
  /system <text>      — set system prompt  
  /memory <n>         — set window size (0 = unlimited)
  /clear              — clear history
  /chain <file.ml>    — load a chain definition
  /export             — dump session as JSON
  /quit               — exit

[ollama/gpt-oss:20b] ›  █
```

- Uses `Lwt` for async streaming — tokens print as they arrive
- Uses ANSI colour codes for role differentiation (user = cyan, assistant = green, system = yellow)
- Arrow key history via `ln` (OCaml's `read_line` + a simple ring buffer)
- Subcommands via `cmdliner` for non-interactive usage: `orchcaml complete --model X --prompt "..."`

---

## Proposed Changes

### Phase 1: Foundations

#### [MODIFY] [dune-project](file:///home/adukhan/Documents/Code/OCaml/OrchCaml/dune-project)
Add dependencies: `cohttp-lwt-unix`, `uri`, `yojson`, `lwt`, `lwt_ppx`, `cmdliner`, `fmt`, `re`.

#### [NEW] lib/types.ml
Core message types, role variants, typed result wrappers.

#### [NEW] lib/provider.ml
`PROVIDER` module signature + `Config` types.

#### [NEW] lib/template.ml
`{{var}}` template engine with `render : vars:(string * string) list -> string -> string`.

#### [NEW] lib/parser.ml
All typed parsers listed above.

#### [NEW] lib/chain.ml
`|>>` composition operator, `run : ('a, 'b) chain -> 'a -> 'b Lwt.t`.

#### [NEW] lib/memory.ml
Buffer and summary memory implementations.

#### [NEW] lib/session.ml
Stateful session wrapping chain + memory.

#### [MODIFY] lib/dune
Expose all modules, add library dependencies.

---

### Phase 2: Providers

#### [NEW] lib/providers/ollama.ml
- `complete`: POST to `localhost:11434/api/chat`, parse response
- `stream`: POST with `"stream": true`, parse NDJSON chunks
- Config includes `host`, `port`, `model`, `options` (temperature, top_p etc.)

#### [NEW] lib/providers/openai.ml
- OpenAI-compatible API client (works with OpenAI, Groq, Together, Mistral etc.)
- `(* API_KEY_SOURCE: read from env OPENAI_API_KEY or ~/.orchcaml/config.toml *)`
- Config includes `base_url` for custom endpoints

#### [NEW] lib/providers/dune
Sub-library stanza.

---

### Phase 3: TUI / CLI

#### [MODIFY] bin/main.ml
Full REPL with streaming output, model switching, history, slash commands.

#### [MODIFY] bin/dune
Add all library deps.

---

## Packages to Install

```
opam install cohttp-lwt-unix uri tls-lwt
```

> [!NOTE]
> `lambda-term` or `notty` would give us a richer TUI later (proper cursor control, multiline input, scrollback). For V1 we use raw ANSI + `read_line` to keep the dep count minimal. We can add `lambda-term` in V2 for the fancy TUI.

---

## Verification Plan

### Automated Tests
```bash
# Build check
dune build

# Run REPL (connects to local Ollama)
./_build/default/bin/main.exe

# Non-interactive test
echo "What is 2+2?" | ./_build/default/bin/main.exe complete --model gpt-oss:20b
```

### Manual Verification
- Test streaming token output from Ollama
- Test chain composition: template → llm → json parser
- Test memory: verify context is preserved across turns
- Test `/model`, `/system`, `/clear` TUI commands
