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
  | s when String.length s > 5 && String.sub s 0 5 = "tool:" ->
    Tool (String.sub s 5 (String.length s - 5))
  | s           -> failwith ("Unknown role: " ^ s)

(** A single message in a conversation. *)
type chat_message = {
  role      : role;
  content   : string;
  timestamp : float;
}

let make_message role content = {
  role;
  content;
  timestamp = Unix.gettimeofday ();
}

let system_msg content    = make_message System content
let user_msg content      = make_message User content
let assistant_msg content = make_message Assistant content

(** Serialise a chat message to a Yojson value. *)
let chat_message_to_json msg =
  `Assoc [
    ("role",      `String (role_to_string msg.role));
    ("content",   `String msg.content);
    ("timestamp", `Float  msg.timestamp);
  ]

(** Deserialise a chat message from a Yojson value. *)
let chat_message_of_json json =
  let open Yojson.Safe.Util in
  {
    role      = json |> member "role"      |> to_string |> role_of_string;
    content   = json |> member "content"   |> to_string;
    timestamp = json |> member "timestamp" |> to_float;
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
