(** Fallback extraction of tool calls a model emitted as text.

    A large share of small local GGUFs and free-tier hosted models were
    tuned on text-based tool protocols: instead of populating the
    native [tool_calls] field they put the invocation in [content] —
    fenced JSON, a bare JSON object, or [<tool_call>]-style XML.
    Without this module the harness saw prose, declared the task
    complete, and returned a confident answer produced by an agent that
    executed zero tools (audit C2) — silently.

    False positives are the danger: a model legitimately {i discussing}
    a tool call must not have it executed.  Three guards enforce that:

    - the {b whole} content (modulo a single surrounding code fence or
      XML wrapper and whitespace) must be the invocation — JSON embedded
      in prose never matches;
    - the parsed name must resolve to a registered tool (aliases
      included);
    - every extraction is announced with a distinct
      [Trace.Tool_call_fallback] event, so the behaviour is auditable
      rather than magic — and under [ask] permissions the user still
      gets the prompt.

    ReAct-style [Action:]/[Action Input:] blocks are deliberately not
    recognised: they are the format most likely to false-positive on
    ordinary prose. *)

open Types

(* Synthesised tool-call ids.  A module-level counter keeps them unique
   within a run; the "textcall" prefix makes their origin obvious in
   transcripts and checkpoints. *)
let id_counter = ref 0

let next_id () =
  incr id_counter;
  Printf.sprintf "textcall_%d" !id_counter

(* ── Whole-content unwrapping ─────────────────────────────────────────── *)

(* Strict fence strip: matches ONLY when the fence spans the entire
   (trimmed) content.  [Parser.extract_code] grabs a fenced block out of
   surrounding prose, which is exactly what we must not do here. *)
let whole_fence_re =
  Re.compile
    Re.(whole_string
          (seq [
             rep space;
             str "```"; rep (compl [char '\n']); char '\n';
             group (rep any);
             str "```"; rep space;
           ]))

(* XML wrappers used by common text tool protocols (Hermes, Qwen,
   various fine-tunes).  Same whole-content discipline. *)
let xml_wrapper_re =
  Re.compile
    Re.(whole_string
          (seq [
             rep space;
             char '<'; group (alt [str "tool_call"; str "function_call"]); char '>';
             group (rep any);
             str "</"; alt [str "tool_call"; str "function_call"]; str ">";
             rep space;
           ]))

type wrapped = { inner : string; format : string }

let unwrap content =
  match Re.exec_opt xml_wrapper_re content with
  | Some m -> { inner = String.trim (Re.Group.get m 2); format = "xml" }
  | None ->
    match Re.exec_opt whole_fence_re content with
    | Some m -> { inner = String.trim (Re.Group.get m 1); format = "fenced_json" }
    | None -> { inner = String.trim content; format = "json" }

(* ── JSON shapes ──────────────────────────────────────────────────────── *)

let member_opt key = function
  | `Assoc kvs -> List.assoc_opt key kvs
  | _ -> None

let first_member keys json =
  List.find_map (fun k -> member_opt k json) keys

let name_keys = ["tool"; "tool_name"; "name"]
let args_keys = ["arguments"; "args"; "parameters"; "input"]

(* Arguments may arrive as a JSON object or as a pre-serialised string;
   both are normalised through [sanitize_json_args]. *)
let args_string = function
  | None -> "{}"
  | Some (`String s) -> sanitize_json_args s
  | Some j -> sanitize_json_args (Yojson.Safe.to_string j)

(** Parse one invocation object.  Accepted shapes:
    - [{"tool"|"tool_name"|"name": n, "arguments"|"args"|"parameters"|"input": o}]
    - [{"function": {"name": n, "arguments": o}}] (OpenAI-style, with or
      without an [id]) *)
let invocation_of_json json =
  let from_parts name_json args =
    match name_json with
    | Some (`String name) when String.trim name <> "" ->
      Some (String.trim name, args_string args)
    | _ -> None
  in
  match member_opt "function" json with
  | Some (`Assoc _ as fn) ->
    from_parts (member_opt "name" fn) (first_member args_keys fn)
  | _ ->
    from_parts (first_member name_keys json) (first_member args_keys json)

(** [extract ~tools content] returns the synthesised tool calls and a
    format label for tracing, or [None] when [content] is not a
    complete, well-formed invocation of a registered tool. *)
let extract ~(tools : Tool.packed_tool list) content =
  if tools = [] || String.trim content = "" then None
  else
    let { inner; format } = unwrap content in
    (* Cheap pre-filter before attempting a JSON parse on arbitrary prose. *)
    if String.length inner = 0 || inner.[0] <> '{' then None
    else
      match Yojson.Safe.from_string inner with
      | exception _ -> None
      | json ->
        let invocations =
          match member_opt "tool_calls" json with
          | Some (`List items) -> List.map invocation_of_json items
          | Some _ -> [None]
          | None -> [invocation_of_json json]
        in
        (* All-or-nothing: one unparseable or unregistered entry rejects
           the whole extraction — half-executing a batch would be worse
           than treating it as prose. *)
        let resolved =
          List.map
            (fun inv ->
               match inv with
               | Some (name, args) when Tool.find_tool tools name <> None ->
                 Some { id = next_id (); name; args; extra_content = None }
               | _ -> None)
            invocations
        in
        if resolved = [] || List.exists Option.is_none resolved then None
        else Some (List.filter_map Fun.id resolved, format)
