(lang dune 3.21)

(name OrchCaml)

(generate_opam_files true)

(source
 (github adukhan99/OrchCaml))

(authors "OrchCaml Contributors")

(maintainers "OrchCaml Contributors")

(license GPL-3.0)

(package
 (name OrchCaml)
 (synopsis "A typed LLM orchestration framework for OCaml")
 (description
  "OrchCaml provides a functional, type-safe interface for composing LLM pipelines with typed message passing, pluggable providers (Ollama, OpenAI-compatible), output parsers, prompt templates, conversation memory, and an interactive TUI.")
 (depends
  ocaml
  (dune
   (>= 3.21))
  (lwt
   (>= 6.0))
  (lwt_ppx
   (>= 2.0))
  (cohttp-lwt-unix
   (>= 6.0))
  (uri
   (>= 4.0))
  (yojson
   (>= 3.0))
  (cmdliner
   (>= 1.3))
  (fmt
   (>= 0.9))
  (re
   (>= 1.10)))
 (tags
  ("llm" "orchestration" "ai" "langchain" "ollama" "openai")))
