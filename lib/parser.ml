(** OrchCaml.Parser — Typed output parsers for LLM responses.

    Each parser is a function [string -> ('a, string) result].
    Parsers compose with [and_then] and [map]. *)

(** The result of a parser: either a value or an error message. *)
type 'a parse_result = ('a, string) result

(** A parser is simply a function from string to a typed result. *)
type 'a t = string -> 'a parse_result

(** --- Combinators --- *)

let map f p s = Result.map f (p s)

let and_then f p s = match p s with
  | Error e -> Error e
  | Ok v    -> f v s

let (>|=) p f = map f p
let (>>=) p f = and_then f p

let return v _s = Ok v
let fail msg _s = Error msg

(** Try the first parser; if it fails, try the second. *)
let or_else p1 p2 s = match p1 s with
  | Ok _ as ok -> ok
  | Error _    -> p2 s

(** --- Base parsers --- *)

(** [string] passes through the raw LLM output unchanged. *)
let string : string t = fun s -> Ok s

(** [trimmed] strips leading/trailing whitespace. *)
let trimmed : string t = fun s -> Ok (String.trim s)

(** [json] parses the output as a JSON value. *)
let json : Yojson.Safe.t t = fun s ->
  match Yojson.Safe.from_string (String.trim s) with
  | v               -> Ok v
  | exception Yojson.Json_error msg -> Error ("JSON parse error: " ^ msg)

(** [json_field key] extracts a single field from a JSON object response. *)
let json_field key : Yojson.Safe.t t = fun s ->
  match json s with
  | Error e -> Error e
  | Ok j ->
    (match Yojson.Safe.Util.member key j with
     | `Null  -> Error (Printf.sprintf "Field '%s' not found in JSON" key)
     | v      -> Ok v)

(** [json_string_field key] extracts a string field from a JSON object. *)
let json_string_field key : string t = fun s ->
  match json_field key s with
  | Error e -> Error e
  | Ok (`String v) -> Ok v
  | Ok other -> Error (Printf.sprintf "Field '%s' is not a string: %s"
                  key (Yojson.Safe.to_string other))

(** [list] splits the output into lines, filtering blank lines. *)
let list : string list t = fun s ->
  Ok (s
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.filter (fun l -> l <> ""))

(** [numbered_list] parses "1. item" or "1) item" formatted lists. *)
let numbered_list : string list t = fun s ->
  let re = Re.compile (Re.seq [
    Re.bos |> Re.opt;
    Re.rep Re.space;
    Re.rep1 Re.digit;
    Re.alt [Re.char '.'; Re.char ')'];
    Re.rep1 Re.space;
    Re.group (Re.rep1 Re.any);
  ]) in
  let lines = String.split_on_char '\n' s in
  let items = List.filter_map (fun line ->
    match Re.exec_opt re (String.trim line) with
    | Some m -> Some (String.trim (Re.Group.get m 1))
    | None   -> None
  ) lines in
  if items = [] then
    (* fall back to plain line splitting *)
    list s
  else
    Ok items

(** [bool] parses yes/no/true/false responses. *)
let bool : bool t = fun s ->
  match String.lowercase_ascii (String.trim s) with
  | "yes" | "true"  | "1" | "y" -> Ok true
  | "no"  | "false" | "0" | "n" -> Ok false
  | other -> Error ("Cannot parse bool from: " ^ other)

(** [int_val] parses an integer from the response. *)
let int_val : int t = fun s ->
  let s = String.trim s in
  (* grab first run of digits, possibly preceded by a sign *)
  let re = Re.compile (Re.seq [Re.opt (Re.char '-'); Re.rep1 Re.digit]) in
  match Re.exec_opt re s with
  | None   -> Error ("No integer found in: " ^ s)
  | Some m -> Ok (int_of_string (Re.Group.get m 0))

(** [float_val] parses a float from the response. *)
let float_val : float t = fun s ->
  let s = String.trim s in
  let re = Re.compile (Re.seq [
    Re.opt (Re.char '-');
    Re.rep1 Re.digit;
    Re.opt (Re.seq [Re.char '.'; Re.rep1 Re.digit]);
  ]) in
  match Re.exec_opt re s with
  | None   -> Error ("No float found in: " ^ s)
  | Some m -> Ok (float_of_string (Re.Group.get m 0))

(** [extract_code ?lang] strips markdown code fences from the response.
    If [lang] is given (e.g. "ocaml"), only strips fences of that language.
    Returns the code inside the first code block found. *)
let extract_code ?lang : string t = fun s ->
  let lang_pat = match lang with
    | None   -> Re.rep (Re.compl [Re.char '\n'])
    | Some l -> Re.str l
  in
  let re = Re.compile (Re.seq [
    Re.str "```";
    lang_pat;
    Re.char '\n';
    Re.group (Re.rep Re.any);
    Re.str "```";
  ]) in
  match Re.exec_opt re s with
  | Some m -> Ok (String.trim (Re.Group.get m 1))
  | None   ->
    (* no fence found — try to strip a single-backtick inline *)
    let re2 = Re.compile (Re.seq [
      Re.char '`';
      Re.group (Re.rep1 (Re.compl [Re.char '`']));
      Re.char '`';
    ]) in
    (match Re.exec_opt re2 s with
     | Some m -> Ok (Re.Group.get m 1)
     | None   -> Ok (String.trim s))   (* return as-is if no fence *)

(** [first_line] returns only the first non-empty line. *)
let first_line : string t = fun s ->
  match list s with
  | Error e -> Error e
  | Ok []   -> Ok ""
  | Ok (h::_) -> Ok h

(** [regex_capture ~pattern] extracts the first capture group matching [pattern]. *)
let regex_capture ~pattern : string t = fun s ->
  let re = Re.compile (Re.Pcre.re pattern) in
  match Re.exec_opt re s with
  | None   -> Error (Printf.sprintf "Pattern /%s/ did not match" pattern)
  | Some m ->
    (try Ok (Re.Group.get m 1)
     with Not_found ->
       try Ok (Re.Group.get m 0)
       with Not_found -> Error "No capture group in match")

(** [json_array_strings] parses a JSON array of strings from the response. *)
let json_array_strings : string list t = fun s ->
  match json s with
  | Error e -> Error e
  | Ok (`List items) ->
    let strings = List.filter_map (function
      | `String s -> Some s
      | _ -> None) items in
    Ok strings
  | Ok _ -> Error "Expected a JSON array"

(** --- Running parsers --- *)

(** [run p s] applies parser [p] to string [s]. *)
let run p s = p s

(** [run_exn p s] applies [p] and raises [Failure] on error. *)
let run_exn p s =
  match p s with
  | Ok v    -> v
  | Error e -> failwith e
