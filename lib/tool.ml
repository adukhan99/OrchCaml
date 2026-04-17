(** OrchCaml.Tool — Formal module signature for tools.

    This module defines the architectural requirement for Tools
    as mandated by the OrchCaml architectural guidelines.
    It replaces implicit "duck typing" of tool calls with a strict signature
    ensuring type safety, composability, and discoverability.
*)

module type TOOL = sig
  (** The name of the tool, used by the LLM to invoke it. *)
  val name : string

  (** A description of what the tool does. *)
  val description : string

  (** The input type required by the tool. *)
  type input

  (** The output type produced by the tool. *)
  type output

  (** Generates the JSON Schema describing the inputs for LLM discovery. *)
  val json_schema : unit -> Yojson.Safe.t

  (** The effect that represents execution of this tool.
      By tracking this as an effect, handlers can manage logging, retries,
      or mocks around tool execution without polluting the business logic. *)
  type _ Effect.t += Exec : input -> output Effect.t

  (** An effect-based execution function that performs the tool logic. 
      Typically this will construct the `Exec` effect and perform it. *)
  val execute : input -> output
end

(** A packed (existential) tool — lets you store tools of different
    input/output types in the same data structure. *)
type packed_tool =
  | Tool : (module TOOL with type input = 'i and type output = 'o) -> packed_tool

let name_of_packed (Tool (module T)) = T.name
let description_of_packed (Tool (module T)) = T.description
