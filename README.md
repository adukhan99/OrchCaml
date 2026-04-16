# OrchCaml

**OrchCaml** is a functional, type-safe LLM orchestration framework for OCaml. It provides a structured way to build, compose, and deploy LLM pipelines with strong compile-time guarantees.

Inspired by LangChain but designed with OCaml's type system in mind, OrchCaml models LLM interactions as typed asynchronous streams flowing through composable "chains."

## Key Features

- **Typed Chains**: Compose pipelines using the `|>>` operator (asynchronous bind). Every step is a typed function `'a -> 'b Lwt.t`.
- **Pluggable Providers**: Support for **Ollama** (local) and **OpenAI-compatible** APIs (Groq, Together, etc.). Swap backends with a single line of code.
- **Typed Parsers**: Transform raw LLM strings into structured OCaml data (JSON, lists, booleans, code blocks) with built-in validation.
- **Prompt Templates**: Logic-less mustache-style templates (`{{variable}}`) with variable extraction and validation.
- **Conversation Memory**: Built-in sliding window memory for stateful, multi-turn interactions.
- **Interactive TUI**: A feature-rich REPL for testing prompts, switching models, and exporting sessions.

## Installation

OrchCaml requires OCaml 5.0+ and Dune.

```bash
# Clone the repository
git clone https://github.com/adukhan99/OrchCaml.git
cd OrchCaml

# Install dependencies
opam install . --deps-only

# Build the project
dune build
```

## Quick Start: The Library

Building a typed pipeline that takes a topic and returns a list of facts:

```ocaml
open OrchCaml
open OrchCaml.Chain

let fact_chain provider =
  (* 1. Define the prompt template *)
  prompt_template "List 3 interesting facts about {{topic}}." 
  
  (* 2. Send to the LLM *)
  |>> llm provider 
  
  (* 3. Parse the output into a string list *)
  |>> parse Parser.numbered_list

let () = Lwt_main.run (
  let provider = OrchCamlProviders.Ollama.make_provider ~model:"llama3.2" () in
  let%lwt facts = run (fact_chain provider) [("topic", "OCaml")] in
  List.iter (Printf.printf "- %s\n") facts;
  Lwt.return_unit
)
```

## Quick Start: The TUI

OrchCaml comes with a powerful CLI tool for interactive use.

```bash
# Start the REPL (uses local Ollama by default)
dune exec orchcaml

# Use OpenAI-compatible provider
export OPENAI_API_KEY="sk-..."
dune exec orchcaml -- --provider openai --model gpt-4o

# Run a single completion
dune exec orchcaml complete "Why is functional programming useful?"
```

### REPL Slash Commands

Inside the REPL, use these commands to control the session:
- `/model <name>`: Switch the active model.
- `/system <text>`: Set a persistent system instruction.
- `/memory <n>`: Set the sliding window size (0 for unlimited).
- `/export [file.json]`: Export the full conversation history.
- `/models`: List models available on the current provider.

## Architecture

- **`OrchCaml.Types`**: Foundational types for messages, roles, and results.
- **`OrchCaml.Chain`**: The core DSL for pipeline composition.
- **`OrchCaml.Provider`**: Abstract interface for LLM backends.
- **`OrchCaml.Parser`**: Combinators for turning text into types.
- **`OrchCaml.Template`**: Simple interpolation engine.
- **`OrchCaml.Session`**: Higher-level manager for stateful chat.

## License

MIT
