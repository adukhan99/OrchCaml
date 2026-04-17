(** OrchCaml.Types — Core message and type definitions.

    All LLM interactions are modelled as typed messages flowing through
    a pipeline. This module defines the foundational types used throughout
    the framework. *)

(** The role of a participant in a conversation. *)
type role =
  | System      (** The system / instruction prompt *)
  | User        (** A human turn *)
  | Assistant   (** An LLM response turn *)
  | Tool of string  (** A tool/function call result, named *)

let role_to_string = function
  | System       -> "system"
  | User         -> "user"
  | Assistant    -> "assistant"
  | Tool name    -> "tool:" ^ name

let role_of_string = function
  | "system"    -> System
  | "user"      -> User
  | "assistant" -> Assistant
  | "tool"      -> Tool ""
  | s when String.length s > 5 && String.sub s 0 5 = "tool:" ->
    Tool (String.sub s 5 (String.length s - 5))
  | s           -> failwith ("Unknown role: " ^ s)

type tool_call = {
  id   : string;
  name : string;
  args : string;
}

(** A single message in a conversation. *)
type chat_message = {
  role       : role;
  content    : string;
  timestamp  : float;
  tool_calls : tool_call list option;
}

let make_message ?tool_calls role content = {
  role;
  content;
  timestamp = Unix.gettimeofday ();
  tool_calls;
}

let system_msg = make_message System
let user_msg = make_message User
let assistant_msg = make_message Assistant
let tool_msg name content = make_message (Tool name) content

(** Serialise a chat message to a Yojson value. *)
let tool_call_to_json tc =
  `Assoc [
    ("id", `String tc.id);
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String tc.name);
      ("arguments", `String tc.args);
    ]);
  ]

(** Serialise a chat message to a Yojson value. *)
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

let tool_call_of_json json =
  let open Yojson.Safe.Util in
  let func = json |> member "function" in
  {
    id   = json |> member "id" |> to_string;
    name = func |> member "name" |> to_string;
    args = func |> member "arguments" |> to_string;
  }

(** Deserialise a chat message from a Yojson value. *)
let chat_message_of_json json =
  let open Yojson.Safe.Util in
  let role_str = json |> member "role" |> to_string in
  let role = 
    if role_str = "tool" then
      match json |> member "tool_call_id" with
      | `String id -> Tool id
      | _ -> role_of_string role_str
    else
      role_of_string role_str
  in
  {
    role;
    content    = (match json |> member "content" with `String s -> s | `Null -> "" | _ -> "");
    timestamp  = json |> member "timestamp" |> to_float;
    tool_calls = (match json |> member "tool_calls" with
                  | `Null -> None
                  | `List l -> Some (List.map tool_call_of_json l)
                  | _ -> None);
  }

(** Serialise a list of chat messages (e.g. for the Ollama/OpenAI API). *)
let messages_to_json msgs =
  `List (List.map chat_message_to_json msgs)

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

(** Generation options shared across providers. *)
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

(** Convenience builder for gen_options. *)
let options
    ?temperature ?top_p ?top_k ?max_tokens ?(stop=[]) ?seed () =
  { temperature; top_p; top_k; max_tokens; stop; seed }
