(** OrchCaml.Template — Prompt template engine.

    Supports [{{variable}}] interpolation with optional variable schema
    validation. Pure OCaml, no external dependencies. *)

(** An AST node for prompt templates. *)
type ast_node =
  | Text of string
  | Var of string

(** A compiled template with an AST and extracted variable names. *)
type t = {
  source    : string;
  ast       : ast_node list;
  variables : string list;
}

(** Regular expression matching [{{varname}}] placeholders. *)
let var_re = Re.compile (Re.seq [
  Re.str "{{";
  Re.rep1 Re.space |> Re.opt;
  Re.group (Re.rep1 (Re.alt [Re.alnum; Re.char '_']));
  Re.rep1 Re.space |> Re.opt;
  Re.str "}}";
])

(** Parse a template string into an AST, extracting the names of all variables. *)
let of_string source =
  let ast =
    Re.split_full var_re source
    |> List.map (function
        | `Text t -> Text t
        | `Delim d ->
          let var_name = Re.Group.get d 1 in
          Var var_name
      )
  in
  let variables =
    ast |> List.filter_map (function Var v -> Some v | Text _ -> None)
    |> List.sort_uniq String.compare
  in
  { source; ast; variables }

(** [render ~vars tmpl] substitutes each [{{key}}] with its value from [vars].
    Raises [Invalid_argument] if a required variable is missing. *)
let render ~vars tmpl =
  let missing =
    List.filter (fun v -> not (List.mem_assoc v vars)) tmpl.variables
  in
  if missing <> [] then
    invalid_arg ("Template: missing variables: " ^ String.concat ", " missing);
  List.map (function
    | Text t -> t
    | Var v -> List.assoc v vars
  ) tmpl.ast
  |> String.concat ""

(** [render_string ~vars src] compiles and renders in one step. *)
let render_string ~vars src = render ~vars (of_string src)

(** [variables tmpl] returns the list of variable names in [tmpl]. *)
let variables tmpl = tmpl.variables

(** [source tmpl] returns the original template string. *)
let source tmpl = tmpl.source

(** Pretty-print a template for debugging. *)
let pp fmt tmpl =
  Format.fprintf fmt "@[<v>Template {@ source = %S;@ variables = [%s]@]}"
    tmpl.source
    (String.concat "; " (List.map (fun v -> "\"" ^ v ^ "\"") tmpl.variables))

(** A multi-message chat template — maps roles to template strings. *)
type chat_template = {
  system_tmpl : t option;
  human_tmpl  : t;
}

let chat_template ?system human = {
  system_tmpl = Option.map of_string system;
  human_tmpl  = of_string human;
}

(** [render_chat ~vars tmpl] produces a list of chat messages. *)
let render_chat ~vars tmpl =
  let open Types in
  let human_msg = user_msg (render ~vars tmpl.human_tmpl) in
  match tmpl.system_tmpl with
  | None     -> [ human_msg ]
  | Some sys ->
    let sys_vars = List.filter (fun (k, _) -> List.mem k sys.variables) vars in
    [ system_msg (render ~vars:sys_vars sys); human_msg ]
