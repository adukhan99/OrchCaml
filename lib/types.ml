(** OrchCaml.Types — Core message and type definitions.

    All LLM interactions are modelled as typed messages flowing through
    a pipeline. This module defines the foundational types used throughout
    the framework. *)

(** The role of a participant in a conversation. *)
type role =
  | System      (** The system / instruction prompt *)
  | User        (** A human turn *)
  | Assistant   (** An LLM response turn *)
  | Tool of string  (** A tool/function call result, carrying the call-id *)

let role_to_string = function
  | System       -> "system"
  | User         -> "user"
  | Assistant    -> "assistant"
  | Tool name    -> "tool:" ^ name

(** Total parse — returns [Error] instead of raising. *)
let role_of_string_result = function
  | "system"    -> Ok System
  | "user"      -> Ok User
  | "assistant" -> Ok Assistant
  | "tool"      -> Ok (Tool "")
  | s when String.length s > 5 && String.sub s 0 5 = "tool:" ->
    Ok (Tool (String.sub s 5 (String.length s - 5)))
  | s           -> Error ("Unknown role: " ^ s)

(** Legacy alias — kept for convenience; prefer [role_of_string_result]. *)
let role_of_string s =
  match role_of_string_result s with
  | Ok r    -> r
  | Error e -> failwith e

(* ------------------------------------------------------------------ *)
(*  Refined message ADT                                                 *)
(* ------------------------------------------------------------------ *)

(** A tool call requested by an assistant turn. *)
type tool_call = {
  id   : string;
  name : string;
  args : string;
}

(** A single message — role-specific invariants are enforced by the
    smart constructors below.  The record is intentionally kept flat so
    that existing serialisation code does not change. *)
type chat_message = {
  role       : role;
  content    : string;
  timestamp  : float;
  (** [tool_calls] is [Some _] only on [Assistant] messages that request
      tool invocations.  Callers SHOULD use [assistant_tool_msg] rather
      than constructing this field directly. *)
  tool_calls : tool_call list option;
}

(* ------------------------------------------------------------------ *)
(*  Smart constructors (enforce role/field invariants)                  *)
(* ------------------------------------------------------------------ *)

let make_message ?tool_calls role content = {
  role;
  content;
  timestamp = Unix.gettimeofday ();
  tool_calls;
}

let system_msg    content          = make_message System    content
let user_msg      content          = make_message User      content
let assistant_msg content          = make_message Assistant content

(** [assistant_tool_msg ~tool_calls content] constructs an [Assistant]
    message that carries one or more tool call requests.  [tool_calls]
    MUST be non-empty; use [assistant_msg] for plain text replies. *)
let assistant_tool_msg ~tool_calls content =
  make_message ~tool_calls Assistant content

(** [tool_msg call_id content] constructs a [Tool] result message.
    [call_id] carries the id of the tool call being answered. *)
let tool_msg call_id content = make_message (Tool call_id) content

(* ------------------------------------------------------------------ *)
(*  JSON serialisation                                                   *)
(* ------------------------------------------------------------------ *)

let tool_call_to_json tc =
  `Assoc [
    ("id", `String tc.id);
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String tc.name);
      ("arguments", `String tc.args);
    ]);
  ]

let chat_message_to_json msg =
  let base = [
    ("role",      `String (match msg.role with Tool _ -> "tool" | r -> role_to_string r));
    ("content",   if msg.content = "" && msg.tool_calls <> None then `Null else `String msg.content);
    ("timestamp", `Float  msg.timestamp);
  ] in
  let base = match msg.tool_calls with
    | Some tcs -> ("tool_calls", `List (List.map tool_call_to_json tcs)) :: base
    | None -> base
  in
  let base = match msg.role with
    | Tool id -> ("tool_call_id", `String id) :: base
    | _ -> base
  in
  `Assoc base

(** Total — returns [Error] on malformed JSON instead of raising. *)
let tool_call_of_json_result json =
  try
    let open Yojson.Safe.Util in
    let func = json |> member "function" in
    Ok {
      id   = json |> member "id"   |> to_string;
      name = func |> member "name" |> to_string;
      args = func |> member "arguments" |> to_string;
    }
  with Yojson.Safe.Util.Type_error (msg, _) -> Error ("tool_call parse: " ^ msg)

let tool_call_of_json json =
  match tool_call_of_json_result json with
  | Ok tc   -> tc
  | Error e -> failwith e

(** Total — returns [Error] on malformed JSON. *)
let chat_message_of_json_result json =
  try
    let open Yojson.Safe.Util in
    let role_str = json |> member "role" |> to_string in
    let role_r =
      if role_str = "tool" then
        match json |> member "tool_call_id" with
        | `String id -> Ok (Tool id)
        | _          -> role_of_string_result role_str
      else
        role_of_string_result role_str
    in
    match role_r with
    | Error e -> Error e
    | Ok role ->
      let tool_calls =
        match json |> member "tool_calls" with
        | `Null  -> Ok None
        | `List l ->
          let results = List.map tool_call_of_json_result l in
          let errs = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
          if errs <> [] then Error (String.concat "; " errs)
          else Ok (Some (List.filter_map (function Ok tc -> Some tc | _ -> None) results))
        | _ -> Ok None
      in
      (match tool_calls with
       | Error e -> Error e
       | Ok tcs ->
         Ok {
           role;
           content    = (match json |> member "content" with `String s -> s | `Null -> "" | _ -> "");
           timestamp  = (match json |> member "timestamp" with `Float f -> f | _ -> 0.0);
           tool_calls = tcs;
         })
  with Yojson.Safe.Util.Type_error (msg, _) -> Error ("chat_message parse: " ^ msg)

let chat_message_of_json json =
  match chat_message_of_json_result json with
  | Ok m    -> m
  | Error e -> failwith e

let messages_to_json msgs =
  `List (List.map chat_message_to_json msgs)

(* ------------------------------------------------------------------ *)
(*  Metadata wrapper                                                    *)
(* ------------------------------------------------------------------ *)

(** Structured result type wrapping a value with metadata. *)
type 'a result_with_meta = {
  value        : 'a;
  raw_response : string;
  model        : string;
  provider     : string;
  finish_reason: string option;
}

let wrap_result ~raw_response ~model ~provider ?finish_reason value =
  { value; raw_response; model; provider; finish_reason }

(* ------------------------------------------------------------------ *)
(*  Generation options                                                  *)
(* ------------------------------------------------------------------ *)

type gen_options = {
  temperature  : float option;
  top_p        : float option;
  top_k        : int option;
  max_tokens   : int option;
  stop         : string list;
  seed         : int option;
}

let default_options = {
  temperature  = None;
  top_p        = None;
  top_k        = None;
  max_tokens   = None;
  stop         = [];
  seed         = None;
}

let options
    ?temperature ?top_p ?top_k ?max_tokens ?(stop=[]) ?seed () =
  { temperature; top_p; top_k; max_tokens; stop; seed }
