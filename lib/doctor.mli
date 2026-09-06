(** Structured system and configuration diagnostics. *)

type severity = Pass | Warn | Fail

(** What would put a failing check right.  A fix is data, not an action:
    [lib] never prompts or writes to a terminal, so it says what to do and
    each front-end decides how. *)
type fix =
  | Set_setting of string * string   (** write this exact value *)
  | Edit_setting of string           (** ask the user for a value *)
  | Remove_key of string             (** delete a key the schema rejects *)
  | Store_api_key of string          (** prompt for a provider's key *)
  | Fix_permissions of string * int  (** chmod a path *)
  | Edit_config                      (** open the file in $EDITOR *)
  | Run_init                         (** re-run the setup wizard *)

type check = {
  label    : string;
  severity : severity;
  message  : string;
  hint     : string option;
  fix      : fix option;
}

(** One-line imperative description of a fix, so every surface labels it
    the same way. *)
val describe_fix : fix -> string

(** Whether a fix can be applied without asking the user anything. *)
val is_automatic : fix -> bool

type provider_kind = Local | Cloud

type provider_info = {
  name          : string;
  kind          : provider_kind;
  base_url      : string;
  requires_key  : bool;
  key_env       : string option;
}

(** Run a suite of diagnostic checks. *)
val run_checks :
  find_provider:(string -> provider_info option) ->
  api_key_for:(provider_info -> string option) ->
  list_models:(provider_info -> string option -> string list) ->
  subagents_roster:(Config.subagent_config * string) list ->
  subagents_enabled:bool ->
  unit -> check list
