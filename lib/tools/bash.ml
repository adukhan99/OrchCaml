open Caravan.Tool

(** Strict mode (read lazily so env/config changes are honoured):
    0 = permissive (any shell command), 1 = single-command discipline,
    2 = tool hidden entirely (filtered out by the front-end).
    Default 0: single-command discipline doubles or triples round
    trips, and round trips are the scarce resource on a rate-limited
    free tier — verification-heavy workflows can opt back in. *)
let strict_mode () =
  Caravan.Config.get_int_opt (Some "CARAVAN_STRICT_MODE") "strict_mode"
  |> Option.value ~default:0

module Bash : TOOL with type input = string and type output = string = struct
  let name = "bash"
  let aliases = ["sh"; "shell"; "terminal"; "cmd"; "exec"; "run_command"]
  let description =
    "The primary tool for running and orchestrating CLI applications and system utilities. \
     Use this to execute external programs, manage system tools, and process their output. \
     stdout and stderr are both captured. Commands may contain '&&' and '||' for control flow. \
     In strict mode, do NOT chain independent commands with ';' or newlines — issue each as \
     its own tool call so intermediate results can be verified."

  type input = string
  type output = string

  let json_schema () =
    `Assoc [
      "type", `String "object";
      "properties", `Assoc [
        "command", `Assoc [
          "type", `String "string";
          "description", `String "The bash command to execute."
        ]
      ];
      "required", `List [`String "command"]
    ]

  let has_delimiters s =
    String.contains s ';' || String.contains s '\n'

  let parse_args json =
    let open Yojson.Safe.Util in
    try
      let cmd = json |> member "command" |> to_string in
      if strict_mode () = 1 && has_delimiters cmd then
        Error
          "Multiple commands detected (';' or newline). \
           Please issue each command as a separate tool call."
      else Ok cmd
    with Type_error (s, _) -> Error s

  let format_output s = s

  let is_mutating = true
  let describe_action command = Printf.sprintf "Execute command: %s" command

  type _ Effect.t += Exec : input -> output Effect.t

  (** Run through the shell with stderr merged into stdout, so the model
      sees compiler errors, warnings, and diagnostics — not just stdout. *)
  let execute command =
    try
      let wrapped = Printf.sprintf "( %s ) 2>&1" command in
      let ic = Unix.open_process_in wrapped in
      let out = In_channel.input_all ic in
      match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> if out = "" then "(no output, exit 0)" else out
      | Unix.WEXITED n -> Printf.sprintf "%s\n[exit status %d]" out n
      | Unix.WSIGNALED n -> Printf.sprintf "%s\n[killed by signal %d]" out n
      | Unix.WSTOPPED n -> Printf.sprintf "%s\n[stopped by signal %d]" out n
    with e -> "Error executing command: " ^ Printexc.to_string e
end
