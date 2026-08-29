(** Message and type definitions. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Identity converter for raw Yojson ASTs in PPX records. *)
type yojson_safe = Yojson.Safe.t
let yojson_of_yojson_safe (x : yojson_safe) : Yojson.Safe.t = x
let yojson_safe_of_yojson (x : Yojson.Safe.t) : yojson_safe = x


type role =
  | System
  | User
  | Assistant
  | Tool of string

let role_to_string = function
  | System       -> "system"
  | User         -> "user"
  | Assistant    -> "assistant"
  | Tool name    -> "tool:" ^ name

let role_of_string_result = function
  | "system"    -> Ok System
  | "user"      -> Ok User
  | "assistant" -> Ok Assistant
  | "tool"      -> Ok (Tool "")
  | s when String.length s > 5 && String.sub s 0 5 = "tool:" ->
    Ok (Tool (String.sub s 5 (String.length s - 5)))
  | s           -> Error ("Unknown role: " ^ s)

let role_of_string s =
  match role_of_string_result s with
  | Ok r    -> r
  | Error e -> failwith e

type tool_call = {
  id            : string;
  name          : string;
  args          : string;
  extra_content : Yojson.Safe.t option;
}

(** A single message in a conversation. *)
type chat_message = {
  role          : role;
  content       : string;
  timestamp     : float;
  tool_calls    : tool_call list option;
  extra_content : Yojson.Safe.t option;
}

(** Parse a raw string into a guaranteed valid UTF-8 string at the domain boundary using [uutf].
    Any malformed UTF-8 byte sequence is parsed into the Unicode Replacement
    Character U+FFFD ("\xEF\xBF\xBD"). Valid UTF-8 strings are returned unchanged. *)
let parse_utf8 (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let has_malformed = ref false in
  Uutf.String.fold_utf_8 (fun () _ -> function
    | `Malformed _ ->
        has_malformed := true;
        Buffer.add_string buf "\xEF\xBF\xBD"
    | `Uchar u ->
        Uutf.Buffer.add_utf_8 buf u
  ) () s;
  if !has_malformed then Buffer.contents buf else s


let make_message ?tool_calls ?extra_content role content = {
  role;
  content = parse_utf8 content;
  timestamp = Unix.gettimeofday ();
  tool_calls = Option.map (List.map (fun tc -> { tc with args = parse_utf8 tc.args })) tool_calls;
  extra_content;
}

let system_msg    content          = make_message System    content
let user_msg      content          = make_message User      content
let assistant_msg content          = make_message Assistant content

let assistant_tool_msg ~tool_calls content =
  make_message ~tool_calls Assistant content

let tool_msg call_id content = make_message (Tool call_id) content

(** Sanitize a JSON-as-string value by round-tripping it through Yojson.
    This normalises any raw control characters the LLM may have emitted
    into proper JSON escapes, without double-escaping already-escaped
    content.  Returns the raw string unmodified if it is invalid JSON,
    as Yojson's serializer will safely escape it when wrapped in `String. *)
let sanitize_json_args s =
  let clean_s = parse_utf8 s in
  try
    let json = Yojson.Safe.from_string clean_s in
    Yojson.Safe.to_string json
  with Yojson.Json_error _ -> clean_s

let tool_call_to_json tc =
  let fields = [
    ("id", `String tc.id);
    ("type", `String "function");
    ("function", `Assoc [
      ("name", `String tc.name);
      ("arguments", `String tc.args);
    ]);
  ] in
  match tc.extra_content with
  | Some ec -> `Assoc (("extra_content", ec) :: fields)
  | None -> `Assoc fields

let chat_message_to_json msg =
  let base = [
    ("role",      `String (match msg.role with Tool _ -> "tool" | r -> role_to_string r));
    ("content",   if msg.content = "" && msg.tool_calls <> None then `Null else `String msg.content);
    ("timestamp", `Float  msg.timestamp);
  ] in
  let base = match msg.extra_content with
    | Some ec -> ("extra_content", ec) :: base
    | None -> base
  in
  let base = match msg.tool_calls with
    | Some tcs -> ("tool_calls", `List (List.map tool_call_to_json tcs)) :: base
    | None -> base
  in
  let base = match msg.role with
    | Tool id -> ("tool_call_id", `String id) :: base
    | _ -> base
  in
  `Assoc base

let tool_call_of_json_result json =
  try
    let open Yojson.Safe.Util in
    let func = json |> member "function" in
    let extra_content =
      match json |> member "extra_content" with
      | `Null -> None
      | ec -> Some ec
    in
    Ok {
      id   = parse_utf8 (json |> member "id"   |> to_string);
      name = parse_utf8 (func |> member "name" |> to_string);
      args = parse_utf8 (func |> member "arguments" |> to_string);
      extra_content;
    }
  with Yojson.Safe.Util.Type_error (msg, _) -> Error ("tool_call parse: " ^ msg)

let tool_call_of_json json =
  match tool_call_of_json_result json with
  | Ok tc   -> tc
  | Error e -> failwith e

let chat_message_of_json_result json =
  try
    let open Yojson.Safe.Util in
    let role_str = json |> member "role" |> to_string in
    let extra_content =
      match json |> member "extra_content" with
      | `Null -> None
      | ec -> Some ec
    in
    let role_r =
      if role_str = "tool" then
        match json |> member "tool_call_id" with
        | `String id -> Ok (Tool id)
        | _          -> role_of_string_result role_str
      else
        role_of_string_result role_str
    in
    let tool_calls_r =
      match json |> member "tool_calls" with
      | `Null  -> Ok None
      | `List l ->
        let results = List.map tool_call_of_json_result l in
        let errs = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
        if errs <> [] then Error (String.concat "; " errs)
        else Ok (Some (List.filter_map (function Ok tc -> Some tc | _ -> None) results))
      | _ -> Ok None
    in
    let open struct
      module Let_syntax = struct
        let bind x ~f = Result.bind x f
        let map x ~f = Result.map f x
      end
    end in
    let%bind role = role_r in
    let%map tcs = tool_calls_r in
    {
      role;
      content       = parse_utf8 (match json |> member "content" with `String s -> s | `Null -> "" | _ -> "");
      timestamp     = (match json |> member "timestamp" with `Float f -> f | _ -> 0.0);
      tool_calls    = tcs;
      extra_content;
    }
  with Yojson.Safe.Util.Type_error (msg, _) -> Error ("chat_message parse: " ^ msg)


let chat_message_of_json json =
  match chat_message_of_json_result json with
  | Ok m    -> m
  | Error e -> failwith e

let messages_to_json msgs =
  `List (List.map chat_message_to_json msgs)

(** Wire format: what actually gets sent to a chat-completions API.
    Unlike [chat_message_to_json] (used for exports and persistence), this
    omits Caravan-internal fields such as [timestamp] — strict
    OpenAI-compatible endpoints reject unknown message fields. *)
let chat_message_to_wire_json msg =
  let base = [
    ("role",    `String (match msg.role with Tool _ -> "tool" | r -> role_to_string r));
    ("content", if msg.content = "" && msg.tool_calls <> None then `Null else `String msg.content);
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

let messages_to_wire_json msgs =
  `List (List.map chat_message_to_wire_json msgs)

type usage = {
  prompt_tokens     : int;
  completion_tokens : int;
  total_tokens      : int;
  total_duration    : float option;
  (** Prompt tokens served from the provider's cache, when reported
      ([usage.prompt_tokens_details.cached_tokens] on OpenAI-shaped
      APIs, [prompt_cache_hit_tokens] on DeepSeek).  This is how you
      verify byte-stable prefixes are actually paying off. *)
  cached_tokens     : int option; [@yojson.option]
} [@@deriving yojson]

type 'a result_with_meta = {
  value        : 'a;
  raw_response : string;
  model        : string;
  provider     : string;
  finish_reason: string option;
  usage        : usage option;
  turn_count   : int option;
}

let wrap_result ~raw_response ~model ~provider ?finish_reason ?usage ?turn_count value =
  { value; raw_response; model; provider; finish_reason; usage; turn_count }

type gen_options = {
  temperature  : float option;
  top_p        : float option;
  top_k        : int option;
  max_tokens   : int option;
  stop         : string list;
  seed         : int option;
} [@@deriving yojson]

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
