# API Reference

Caravan provides three findlib libraries:
- `Caravan` — Core framework, ReAct agents, typed chains, session state, event tracing, memory compaction, micro-LISP, and MCP client.
- `CaravanProviders` — Pluggable backends (Ollama, OpenAI, llama.cpp, Groq, Anthropic, Gemini, DeepSeek, generic OpenAI-compatible endpoints).
- `CaravanTools` — Built-in executable tools (Bash shell execution, file read/write, web fetch/search, subagent delegation, LISP runner).

Browsing the complete, auto-generated OCaml odoc HTML documentation is available at:
**[Full HTML API Reference (odoc)](https://adukhan99.github.io/Caravan/api/)**

---

## Core Library: `Caravan`

### `Caravan.Agent`

Autonomous agentic loop (ReAct loop) execution engine.

```ocaml
type agent_config = {
  max_turns : int;
  continue_prompt : string;
  nudge : bool;
}

val default_config : unit -> agent_config

val run :
  ?config:agent_config ->
  ?on_turn:(Session.t -> chat_message result_with_meta -> unit) ->
  ?on_step:(Session.t -> unit) ->
  _ Eio.Net.t ->
  _ Eio.Time.clock ->
  Session.t ->
  string ->
  (Session.t * chat_message result_with_meta, string) result

val run_stream :
  ?config:agent_config ->
  ?on_turn:(Session.t -> chat_message result_with_meta -> unit) ->
  ?on_step:(Session.t -> unit) ->
  _ Eio.Net.t ->
  _ Eio.Time.clock ->
  Session.t ->
  string ->
  on_token:(string -> unit) ->
  (Session.t * chat_message result_with_meta, string) result
```

---

### `Caravan.Chain`

Composable typed LLM processing pipelines using Result-bind (`|>>`).

```ocaml
type ('a, 'b) t = 'a -> ('b, string) result

val (|>>) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val run : ('a, 'b) t -> 'a -> ('b, string) result
val run_exn : ('a, 'b) t -> 'a -> 'b

val prompt_template : string -> (string * string) list -> (string, string) result
val prompt_messages : ?system:string -> string -> (string * string) list -> (chat_message list, string) result
val llm : _ Eio.Net.t -> Provider.packed_provider -> chat_message list -> (string, string) result
val llm_stream : _ Eio.Net.t -> Provider.packed_provider -> on_token:(string -> unit) -> chat_message list -> (string, string) result
val parse : 'a Parser.t -> string -> ('a, string) result

val parallel : Eio.Switch.t -> ('a, 'b) t list -> ('a, 'b list) t
val retry : n:int -> ('a, 'b) t -> 'a -> ('b, string) result

module Kleisli : sig
  val compose : ('a -> ('b, 'e) result) -> ('b -> ('c, 'e) result) -> 'a -> ('c, 'e) result
  val ( >=> ) : ('a -> ('b, 'e) result) -> ('b -> ('c, 'e) result) -> 'a -> ('c, 'e) result
end
```

---

### `Caravan.Session`

Multi-turn session history, system prompt management, and state persistence.

```ocaml
type t

type config = {
  model               : string;
  system              : string option;
  options             : gen_options;
  memory_size         : int;
  max_tool_output_len : int option;
  auto_summarize      : bool;
} [@@deriving yojson]

val create : ?tools:Tool.packed_tool list -> string -> Provider.packed_provider -> t
val set_system : t -> string -> t
val add_user : t -> string -> t
val add_assistant : t -> string -> t
val turn_idx : t -> int
val history : t -> chat_message list

val export_json : t -> Yojson.Safe.t
val of_json : provider:Provider.packed_provider -> ?tools:Tool.packed_tool list -> Yojson.Safe.t -> (t, string) result
val save_checkpoint : ?path:string -> t -> (string, string) result
val load_checkpoint : provider:Provider.packed_provider -> ?tools:Tool.packed_tool list -> ?path:string -> unit -> (t, string) result
```

---

### `Caravan.Trace`

Auditable event stream for LLM completions, tool calls, nudges, and summarization.

```ocaml
type event =
  | Session_start of { provider : string; model : string }
  | Model_call_start of { prompt_len : int }
  | Model_call_end of { duration_s : float; usage : usage_stats option }
  | Tool_call_start of { name : string; args : string }
  | Tool_call_end of { name : string; output : string; duration_s : float }
  | Nudge of { content : string }
  | Task_finished of { summary : string }

type sink = event -> unit

val add_sink : sink -> unit
val with_sink : sink -> (unit -> 'a) -> 'a
val emit : event -> unit
val open_transcript : dir:string -> unit -> sink
```

---

### `Caravan.Types`

Core message types, roles, results, and token usage records.

```ocaml
type role = System | User | Assistant | Tool

type tool_call = {
  id : string;
  name : string;
  args : string;
}

type chat_message = {
  role : role;
  content : string;
  name : string option;
  tool_calls : tool_call list option;
  tool_call_id : string option;
}

type usage_stats = {
  prompt_tokens : int;
  completion_tokens : int;
  total_tokens : int;
}

type 'a result_with_meta = {
  value : 'a;
  usage : usage_stats option;
  finish_reason : string option;
}
```

---

### `Caravan.Provider`

Abstract provider interface and packed existential types.

```ocaml
module type PROVIDER = sig
  type config
  val name : string
  val complete : _ Eio.Net.t -> config -> chat_message list -> chat_message result_with_meta
  val stream : _ Eio.Net.t -> on_token:(string -> unit) -> config -> chat_message list -> chat_message result_with_meta
  val list_models : _ Eio.Net.t -> config -> (string list, string) result
end

type packed_provider = Provider : (module PROVIDER with type config = 'c) * 'c -> packed_provider

val name_of_packed : packed_provider -> string
val complete_packed : _ Eio.Net.t -> packed_provider -> chat_message list -> chat_message result_with_meta
val stream_packed : _ Eio.Net.t -> on_token:(string -> unit) -> packed_provider -> chat_message list -> chat_message result_with_meta
```

---

### `Caravan.Tool`

First-class module tool interface and effect-based tool execution handler.

```ocaml
module type TOOL = sig
  val name : string
  val aliases : string list
  val description : string
  type input
  type output
  val json_schema : unit -> Yojson.Safe.t
  val parse_args : Yojson.Safe.t -> (input, string) result
  val format_output : output -> string
  type _ Effect.t += Exec : input -> output Effect.t
  val execute : input -> output
end

type packed_tool = Tool : (module TOOL) -> packed_tool

val name_of_packed : packed_tool -> string
val description_of_packed : packed_tool -> string
val schema_of_packed : packed_tool -> Yojson.Safe.t
val find_tool : packed_tool list -> string -> packed_tool option
```

---

### `Caravan.Subagent`

Isolation and delegation of background sub-tasks to dedicated worker subagents.

```ocaml
type subagent_spec = {
  name : string;
  system_prompt : string;
  tools : Tool.packed_tool list;
  provider : Provider.packed_provider option;
  model : string option;
  max_turns : int option;
}

val delegate :
  _ Eio.Net.t ->
  _ Eio.Time.clock ->
  Session.t ->
  subagent_spec ->
  string ->
  (Session.t * chat_message result_with_meta, string) result
```

---

### `Caravan.Permission`

Security permission policies governing tool execution.

```ocaml
type mode = Auto | Ask | Readonly

type policy = {
  mode : mode;
  prompt_user : string -> string -> bool;
}

val policy_of_mode : ?prompt_user:(string -> string -> bool) -> string -> policy
val is_mutating : string -> bool
```

---

### `Caravan.Memory`

Context window management and history compaction.

```ocaml
module type MEMORY = sig
  type t
  val create : capacity:int -> t
  val add : t -> chat_message -> t
  val get : t -> chat_message list
  val clear : t -> t
end

type packed_memory = Memory : (module MEMORY with type t = 'm) * 'm -> packed_memory

module Ring : MEMORY
module Summary : MEMORY
module Hierarchical : MEMORY
```

---

### `Caravan.Lisp`

Slip — Caravan's embedded micro-LISP interpreter for programmatic tool composition and evaluation.

```ocaml
type expr =
  | Symbol of string
  | String of string
  | Number of float
  | List of expr list
  | NativeFun of (expr list -> (expr, string) result)

val parse : string -> (expr list, string) result
val eval : env:(string, expr) Hashtbl.t -> expr -> (expr, string) result
val eval_string : env:(string, expr) Hashtbl.t -> string -> (expr, string) result
```

---

### `Caravan.Mcp`

Model Context Protocol (MCP) tool integration and server connection registry.

```ocaml
type mcp_server_config = {
  name : string;
  transport : string;
  command : string;
  args : string list;
}

val load_mcp_tools : mcp_server_config list -> (Tool.packed_tool list, string) result
```

---

## Provider Library: `CaravanProviders`

- `CaravanProviders.Ollama`: Connects to local Ollama daemon (`http://localhost:11434`).
- `CaravanProviders.Openai`: OpenAI API connector (GPT-4o, GPT-4o-mini, O3-mini).
- `CaravanProviders.Llama_cpp`: Local `llama.cpp` HTTP server connector.
- `CaravanProviders.Openai_compatible`: Generic OpenAI-compatible backend connector for vLLM, DeepSeek, Groq, Together, Mistral, OpenRouter, LM Studio, XAI, etc.
- `CaravanProviders.Registry`: Global registry mapping provider names to instances and default models.

---

## Tools Library: `CaravanTools`

- `CaravanTools.Bash`: Executes shell commands with strict-mode safety options.
- `CaravanTools.Read_file` / `CaravanTools.Write_file`: File I/O tools.
- `CaravanTools.Web_search` / `CaravanTools.Read_browser_page`: Web research tools.
- `CaravanTools.Delegate`: Subagent task delegation tool.
- `CaravanTools.Finish`: Task completion tool.
- `CaravanTools.Lisp`: Programmatic Slip LISP script runner tool.
- `CaravanTools.All_tools`: Registry exporting `all_tools : Tool.packed_tool list`.
