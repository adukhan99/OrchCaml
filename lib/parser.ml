(** OrchCaml.Parser — Typed output parsers for LLM responses.

    Each parser is a function [string -> ('a, string) result].
    Parsers compose with [and_then], [map], and the applicative [<*>]. *)

(** The result of a parser: either a value or an error message. *)
type 'a parse_result = ('a, string) result

(** A parser is simply a function from string to a typed result. *)
type 'a t = string -> 'a parse_result

(** --- Monad / Functor combinators --- *)

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

let (<|>) = or_else

(** --- Applicative --- *)

(** [ap pf pa] applies a parser of functions to a parser of values,
    both fed the *same* input string.  This is the standard Applicative
    [<*>] for the [(string ->) Result] functor.

    Example:
    {[
      (* Build a record from two JSON fields in one expression *)
      let make_point x y = {x; y} in
      let point_parser =
        Parser.return make_point
        <*> Parser.json_string_field "x" >|= float_of_string
        <*> Parser.json_string_field "y" >|= float_of_string
      in
    ]}

    Note: both parsers receive the full original string, so [<*>] is
    most useful when each sub-parser extracts an independent piece of
    information from the same response (e.g. different JSON fields).
*)
let ap pf pa =
  pf >>= fun f ->
  pa >|= fun a ->
  f a

let (<*>) = ap

(** [product pa pb] runs both parsers and collects their results as a pair. *)
let product pa pb =
  pa >>= fun a ->
  pb >|= fun b ->
  (a, b)


(** --- Base parsers --- *)

(** [string] passes through the raw LLM output unchanged. *)
let string : string t = fun s -> return s s

(** [trimmed] strips leading/trailing whitespace. *)
let trimmed : string t = fun s -> return (String.trim s) s

(** [json] parses the output as a JSON value. *)
let json : Yojson.Safe.t t = fun s ->
  try return (Yojson.Safe.from_string (String.trim s)) s
  with Yojson.Json_error msg -> fail ("JSON parse error: " ^ msg) s

(** [json_field key] extracts a single field from a JSON object response. *)
let json_field key : Yojson.Safe.t t =
  json >>= fun j s ->
  match Yojson.Safe.Util.member key j with
  | `Null  -> fail (Printf.sprintf "Field '%s' not found in JSON" key) s
  | v      -> return v s

(** [json_string_field key] extracts a string field from a JSON object. *)
let json_string_field key : string t =
  json_field key >>= function
  | `String v -> return v
  | other -> fun s -> fail (Printf.sprintf "Field '%s' is not a string: %s"
                  key (Yojson.Safe.to_string other)) s

(** [list] splits the output into lines, filtering blank lines. *)
let list : string list t = fun s ->
  return (s
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.filter (fun l -> l <> "")) s

(** [numbered_list] parses "1. item" or "1) item" formatted lists. *)
let numbered_list : string list t = fun s ->
  let parse_numbered s =
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
    if items = [] then Error "No numbered items found"
    else Ok items
  in
  (parse_numbered <|> list) s


(** [bool] parses yes/no/true/false responses. *)
let bool : bool t = fun s ->
  match String.lowercase_ascii (String.trim s) with
  | "yes" | "true"  | "1" | "y" -> return true s
  | "no"  | "false" | "0" | "n" -> return false s
  | other -> fail ("Cannot parse bool from: " ^ other) s

(** [int_val] parses an integer from the response. *)
let int_val : int t = fun s ->
  let s_trim = String.trim s in
  (* grab first run of digits, possibly preceded by a sign *)
  let re = Re.compile (Re.seq [Re.opt (Re.char '-'); Re.rep1 Re.digit]) in
  match Re.exec_opt re s_trim with
  | None   -> fail ("No integer found in: " ^ s_trim) s
  | Some m -> return (int_of_string (Re.Group.get m 0)) s

(** [float_val] parses a float from the response. *)
let float_val : float t = fun s ->
  let s_trim = String.trim s in
  let re = Re.compile (Re.seq [
    Re.opt (Re.char '-');
    Re.rep1 Re.digit;
    Re.opt (Re.seq [Re.char '.'; Re.rep1 Re.digit]);
  ]) in
  match Re.exec_opt re s_trim with
  | None   -> fail ("No float found in: " ^ s_trim) s
  | Some m -> return (float_of_string (Re.Group.get m 0)) s

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
  | Some m -> return (String.trim (Re.Group.get m 1)) s
  | None   ->
    (* no fence found — try to strip a single-backtick inline *)
    let re2 = Re.compile (Re.seq [
      Re.char '`';
      Re.group (Re.rep1 (Re.compl [Re.char '`']));
      Re.char '`';
    ]) in
    (match Re.exec_opt re2 s with
     | Some m -> return (Re.Group.get m 1) s
     | None   -> return (String.trim s) s)   (* return as-is if no fence *)

(** [first_line] returns only the first non-empty line. *)
let first_line : string t =
  list >>= function
  | [] -> return ""
  | h :: _ -> return h

(** [regex_capture ~pattern] extracts the first capture group matching [pattern]. *)
let regex_capture ~pattern : string t = fun s ->
  let re = Re.compile (Re.Pcre.re pattern) in
  match Re.exec_opt re s with
  | None   -> fail (Printf.sprintf "Pattern /%s/ did not match" pattern) s
  | Some m ->
    (try return (Re.Group.get m 1) s
     with Not_found ->
       try return (Re.Group.get m 0) s
       with Not_found -> fail "No capture group in match" s)

(** [json_array_strings] parses a JSON array of strings from the response. *)
let json_array_strings : string list t =
  json >>= function
  | `List items ->
    return (List.filter_map (function
      | `String s -> Some s
      | _ -> None) items)
  | _ -> fail "Expected a JSON array"



(** --- Running parsers --- *)

(** [run p s] applies parser [p] to string [s]. *)
let run p s = p s

(** [run_exn p s] applies [p] and raises [Failure] on error. *)
let run_exn p s =
  match p s with
  | Ok v    -> v
  | Error e -> failwith e
