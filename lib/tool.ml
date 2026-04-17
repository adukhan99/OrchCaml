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

  (** Parses the JSON arguments into the tool's input type. *)
  val parse_args : Yojson.Safe.t -> (input, string) result

  (** Formats the output of the tool into a string for the LLM. *)
  val format_output : output -> string

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
let schema_of_packed (Tool (module T)) = T.json_schema ()

let dispatch (Tool (module T)) (args_json : string) : string Lwt.t =
  let json =
    try Yojson.Safe.from_string args_json
    with _ -> `Null
  in
  match T.parse_args json with
  | Error err -> Lwt.return (Printf.sprintf "Error parsing arguments: %s" err)
  | Ok input ->
      let output =
        Effect.Deep.try_with
          (fun () -> Effect.perform (T.Exec input))
          ()
          { effc = fun (type a) (eff : a Effect.t) ->
              match eff with
              | T.Exec i -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k (T.execute i))
              | _ -> None
          }
      in
      Lwt.return (T.format_output output)
