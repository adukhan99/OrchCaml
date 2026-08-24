(** Centralized TOML configuration reader and writer. *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let config_path () =
  match Sys.getenv_opt "CARAVAN_CONFIG" with
  | Some p when p <> "" -> p
  | _ ->
    let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
    Filename.concat home ".caravan/config.toml"

let current_loaded_path = ref None
let cached_ast = ref None

(** Invalidate cached AST in memory. *)
let reload () =
  cached_ast := None;
  current_loaded_path := None

(** Rewrite the config TOML file from an updated AST with strict 0600 permissions. *)
let write_ast ast =
  let path = config_path () in
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then (try Unix.mkdir dir 0o700 with _ -> ());
  let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 path in
  output_string oc (Otoml.Printer.to_string ast);
  close_out oc;
  (try Unix.chmod path 0o600 with _ -> ());
  reload ();
  Ok path

(* Helper: find a string value in AST at [keys]. *)
let find_string_in_ast ast keys =
  try Some (Otoml.find ast Otoml.get_string keys)
  with _ -> None

(** Ensure an [orchestrator] section exists in [ast].
    If [orchestrator] lacks provider or model:
    1. Try top-level provider/model from [ast].
    2. Fall back to [fallback_provider] and [fallback_model] (e.g. from the first subagent input). *)
let ensure_orchestrator_in_ast ?fallback_provider ?fallback_model ast =
  let has_orch_p =
    match find_string_in_ast ast ["orchestrator"; "provider"] with
    | Some s -> String.trim s <> ""
    | None -> false
  in
  let has_orch_m =
    match find_string_in_ast ast ["orchestrator"; "model"] with
    | Some s -> String.trim s <> ""
    | None -> false
  in
  if has_orch_p && has_orch_m then ast
  else
    let prov =
      if has_orch_p then find_string_in_ast ast ["orchestrator"; "provider"]
      else match find_string_in_ast ast ["provider"] with
        | Some p when String.trim p <> "" -> Some p
        | _ -> fallback_provider
    in
    let md =
      if has_orch_m then find_string_in_ast ast ["orchestrator"; "model"]
      else match find_string_in_ast ast ["model"] with
        | Some m when String.trim m <> "" -> Some m
        | _ -> fallback_model
    in
    match prov, md with
    | Some p, Some m ->
      let ast' = Otoml.update ast ["orchestrator"; "provider"] (Some (Otoml.string p)) in
      Otoml.update ast' ["orchestrator"; "model"] (Some (Otoml.string m))
    | _ -> ast

(** Automatically assign/ensure the [orchestrator] table in config.toml
    from main fields or fallback inputs. *)
let ensure_orchestrator ?fallback_provider ?fallback_model () =
  let path = config_path () in
  try
    let ast =
      if Sys.file_exists path then
        (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
      else Otoml.TomlTable []
    in
    let ast' = ensure_orchestrator_in_ast ?fallback_provider ?fallback_model ast in
    if ast' <> ast then
      ignore (write_ast ast')
  with _ -> ()

(** Create default config directory and file if missing, auto-populating [orchestrator]. *)
let ensure_config_exists () =
  let path = config_path () in
  let dir = Filename.dirname path in
  if not (Sys.file_exists dir) then
    (try Unix.mkdir dir 0o755 with _ -> ());
  if not (Sys.file_exists path) then
    (try
       let oc = open_out path in
       output_string oc "# Caravan Configuration\n\n";
       close_out oc
     with _ -> ());
  ensure_orchestrator ()

let is_first_run () =
  let path = config_path () in
  not (Sys.file_exists path) ||
  (try
     let ic = open_in path in
     let len = in_channel_length ic in
     close_in ic;
     len < 50
   with _ -> true)

let load_toml () =
  let path = config_path () in
  ensure_config_exists ();
  if Sys.file_exists path then
    try Some (Otoml.Parser.from_file path)
    with exn ->
      Printf.eprintf "[Caravan] Warning: Failed to parse %s: %s\n%!"
        path (Printexc.to_string exn);
      None
  else None

let get_ast () =
  let path = config_path () in
  match !cached_ast, !current_loaded_path with
  | Some ast, Some p when p = path -> ast
  | _ ->
    let ast = load_toml () in
    current_loaded_path := Some path;
    cached_ast := Some ast;
    ast

let get_int key =
  match get_ast () with
  | None -> None
  | Some ast ->
    try Some (Otoml.find ast Otoml.get_integer [key])
    with _ ->
      try Some (Otoml.find ast Otoml.get_integer ["orchestrator"; key])
      with _ -> None

let get_int_opt env_var toml_key =
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" -> (try Some (int_of_string v) with _ -> get_int toml_key)
               | _ -> get_int toml_key)
  | None -> get_int toml_key

let get_string key =
  match get_ast () with
  | None -> None
  | Some ast ->
    let find_str k =
      try Some (Otoml.find ast Otoml.get_string [k])
      with _ ->
        try Some (Otoml.find ast Otoml.get_string ["orchestrator"; k])
        with _ -> None
    in
    match find_str key with
    | Some _ as r -> r
    | None ->
      if key = "openai_api_key" then find_str "api_key"
      else if key = "api_key" then find_str "openai_api_key"
      else None

let get_string_opt env_var toml_key =
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" -> Some v
               | _ -> get_string toml_key)
  | None -> get_string toml_key

let get_bool ?(path=[]) key =
  match get_ast () with
  | None -> None
  | Some ast ->
    try Some (Otoml.find ast Otoml.get_boolean (path @ [key]))
    with _ ->
      if path = [] then
        try Some (Otoml.find ast Otoml.get_boolean ["orchestrator"; key])
        with _ -> None
      else None

let get_bool_opt ?path env_var toml_key =
  let of_env_str = function
    | "true" | "1" | "yes" -> Some true
    | "false" | "0" | "no" -> Some false
    | _ -> None
  in
  let lookup () = get_bool ?path toml_key in
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" ->
                 (match of_env_str (String.lowercase_ascii v) with
                  | Some _ as r -> r
                  | None -> lookup ())
               | _ -> lookup ())
  | None -> lookup ()

type mcp_server_config = {
  name      : string;
  transport : string;
  command   : string;
  args      : string list;
}

(** SLURM-GRES-style generic resource descriptor for a subagent.
    Each key maps to a boolean capability flag.  Unknown keys are
    preserved so future resource types (e.g. [gen_image]) compose
    without breaking older configs.  Default: all capabilities on. *)
type gres = {
  thinking   : bool;  (** Extended chain-of-thought / thinking tokens *)
  tools      : bool;  (** Tool-calling support *)
  vision     : bool;  (** Image / multi-modal input *)
  gen_image  : bool;  (** Image generation output *)
  extra      : (string * bool) list;  (** Forward-compatible catch-all *)
} [@@deriving yojson]

let default_gres = {
  thinking  = true;
  tools     = true;
  vision    = true;
  gen_image = false;
  extra     = [];
}

(** Config for a single subagent worker, read from a [[subagents]] table. *)
type subagent_config = {
  name          : string;
  worker_role   : string; [@key "role"]
  provider_ref  : string; [@key "provider"]
  model         : string;
  max_tokens    : int option;
  temperature   : float option;
  tool_names    : string list; [@key "tools"]
  system_prompt : string;
  realm         : string option;
  (** Optional plugin-toolset sandbox realm: plugins registered into the
      named realm add worker-only tools resolved at delegation time. *)
  gres          : gres;
} [@@deriving yojson]

(** Config for a named provider endpoint, read from [providers.<name>]. *)
type provider_config = {
  base_url    : string;
  api_key_env : string option;  (** env-var name that holds the key *)
  org_id_env  : string option;
}

let get_mcp_servers () =
  match get_ast () with
  | None -> []
  | Some ast ->
    try
      let servers_node = Otoml.find ast (fun x -> x) ["mcp"; "servers"] in
      let elements =
        match servers_node with
        | Otoml.TomlArray l
        | Otoml.TomlTableArray l -> l
        | _ -> []
      in
      List.filter_map (fun item ->
        match item with
        | Otoml.TomlTable fields
        | Otoml.TomlInlineTable fields ->
          let get_field k = List.assoc_opt k fields in
          let name = match get_field "name" with Some (Otoml.TomlString s) -> Some s | _ -> None in
          let transport = match get_field "transport" with Some (Otoml.TomlString s) -> Some s | _ -> None in
          let command = match get_field "command" with Some (Otoml.TomlString s) -> Some s | _ -> None in
          let args =
            match get_field "args" with
            | Some (Otoml.TomlArray arr)
            | Some (Otoml.TomlTableArray arr) ->
              List.filter_map (function Otoml.TomlString s -> Some s | _ -> None) arr
            | _ -> []
          in
          (match name, transport, command with
           | Some name, Some transport, Some command ->
             Some { name; transport; command; args }
           | _ -> None)
        | _ -> None
      ) elements
    with _ -> []

(** Read a TOML boolean field from an association list, defaulting to [d]. *)
let assoc_bool fields key d =
  match List.assoc_opt key fields with
  | Some (Otoml.TomlBoolean b) -> b
  | _ -> d

(** Read a TOML string field from an association list, returning None on miss. *)
let assoc_string_opt fields key =
  match List.assoc_opt key fields with
  | Some (Otoml.TomlString s) -> Some s
  | _ -> None

(** Read a TOML integer field from an association list, returning None on miss. *)
let assoc_int_opt fields key =
  match List.assoc_opt key fields with
  | Some (Otoml.TomlInteger n) -> Some n
  | _ -> None

(** Get a single MCP server config by name. *)
let get_mcp_server name =
  List.find_opt (fun (s : mcp_server_config) -> s.name = name) (get_mcp_servers ())

(** Append a new [[mcp.servers]] entry to the config file. *)
let add_mcp_server (cfg : mcp_server_config) : (string, string) result =
  if String.trim cfg.name = "" then Error "server name must not be empty"
  else if String.trim cfg.command = "" then Error "command must not be empty"
  else begin
    let path = config_path () in
    try
      let ast =
        if Sys.file_exists path then
          (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
        else Otoml.TomlTable []
      in
      let existing = get_mcp_servers () in
      if List.exists (fun (s : mcp_server_config) -> s.name = cfg.name) existing then
        Error (Printf.sprintf "mcp server '%s' already exists" cfg.name)
      else begin
        let entry = Otoml.TomlTable [
          ("name",      Otoml.string cfg.name);
          ("transport", Otoml.string cfg.transport);
          ("command",   Otoml.string cfg.command);
          ("args",      Otoml.TomlArray (List.map Otoml.string cfg.args));
        ] in
        let existing_nodes =
          try match Otoml.find ast (fun x -> x) ["mcp"; "servers"] with
            | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
            | _ -> []
          with _ -> []
        in
        let new_arr = Otoml.TomlTableArray (existing_nodes @ [entry]) in
        let ast' = Otoml.update ast ["mcp"; "servers"] (Some new_arr) in
        let ast'' = ensure_orchestrator_in_ast ast' in
        write_ast ast''
      end
    with exn -> Error (Printexc.to_string exn)
  end

(** Remove a [[mcp.servers]] entry by name. *)
let delete_mcp_server name : (string, string) result =
  let path = config_path () in
  try
    let ast =
      if Sys.file_exists path then
        (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
      else Otoml.TomlTable []
    in
    let existing_nodes =
      try match Otoml.find ast (fun x -> x) ["mcp"; "servers"] with
        | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
        | _ -> []
      with _ -> []
    in
    let filtered =
      List.filter (fun item ->
        match item with
        | Otoml.TomlTable fs | Otoml.TomlInlineTable fs ->
          assoc_string_opt fs "name" <> Some name
        | _ -> true
      ) existing_nodes
    in
    if List.length filtered = List.length existing_nodes then
      Error (Printf.sprintf "mcp server '%s' not found" name)
    else begin
      let value =
        if filtered = [] then None
        else Some (Otoml.TomlTableArray filtered)
      in
      let ast' = Otoml.update ast ["mcp"; "servers"] value in
      write_ast ast'
    end
  with exn -> Error (Printexc.to_string exn)

(** Read a TOML float field. Accepts both TomlFloat and TomlInteger. *)
let assoc_float_opt fields key =
  match List.assoc_opt key fields with
  | Some (Otoml.TomlFloat f) -> Some f
  | Some (Otoml.TomlInteger n) -> Some (float_of_int n)
  | _ -> None

(* ── [[plugins]] — declarative plugin composition ─────────────────────── *)

(** One [[plugins]] entry: a declarative request for a plugin
    instantiation, reconciled by the harness (see [Plugin_host]). *)
type plugin_config = {
  id      : string;          (** stable identity; defaults to [plugin] *)
  plugin  : string;          (** builder name in the host's registry *)
  enabled : bool;            (** default [true] *)
  config  : Yojson.Safe.t;   (** the whole entry table, as JSON *)
}

(** Best-effort TOML→JSON for plugin configs. Dates and other exotic
    values render as strings. *)
let rec toml_to_json (v : Otoml.t) : Yojson.Safe.t =
  match v with
  | Otoml.TomlString s -> `String s
  | Otoml.TomlInteger n -> `Int n
  | Otoml.TomlFloat f -> `Float f
  | Otoml.TomlBoolean b -> `Bool b
  | Otoml.TomlArray l | Otoml.TomlTableArray l -> `List (List.map toml_to_json l)
  | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
    `Assoc (List.map (fun (k, v) -> (k, toml_to_json v)) fields)
  | other -> (try `String (Otoml.Printer.to_string other) with _ -> `Null)

(** Read all [[plugins]] entries. Entries missing a [plugin] name are
    dropped. An absent table yields [] — the harness then synthesizes
    its default composition (built-in tools + [[mcp.servers]]). *)
let get_plugins () =
  match get_ast () with
  | None -> []
  | Some ast ->
    try
      let node = Otoml.find ast (fun x -> x) ["plugins"] in
      let elements =
        match node with
        | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
        | _ -> []
      in
      List.filter_map (fun item ->
        match item with
        | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
          (match assoc_string_opt fields "plugin" with
           | None -> None
           | Some plugin ->
             Some {
               id = Option.value ~default:plugin (assoc_string_opt fields "id");
               plugin;
               enabled = assoc_bool fields "enabled" true;
               config = toml_to_json (Otoml.TomlTable fields);
             })
        | _ -> None
      ) elements
    with _ -> []

(** Read a [gres.*] sub-table from a [[subagents]] entry. *)
let parse_gres fields =
  match List.assoc_opt "gres" fields with
  | Some (Otoml.TomlTable gfields | Otoml.TomlInlineTable gfields) ->
    let known = ["thinking"; "tools"; "vision"; "gen_image"] in
    let extra =
      List.filter_map (fun (k, v) ->
        if List.mem k known then None
        else match v with Otoml.TomlBoolean b -> Some (k, b) | _ -> None
      ) gfields
    in
    { thinking  = assoc_bool gfields "thinking"  default_gres.thinking;
      tools     = assoc_bool gfields "tools"     default_gres.tools;
      vision    = assoc_bool gfields "vision"    default_gres.vision;
      gen_image = assoc_bool gfields "gen_image" default_gres.gen_image;
      extra;
    }
  | _ -> default_gres

(** Read all [[subagents]] entries from the config file. *)
let get_subagents () =
  match get_ast () with
  | None -> []
  | Some ast ->
    try
      let node = Otoml.find ast (fun x -> x) ["subagents"] in
      let elements = match node with
        | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
        | _ -> []
      in
      List.filter_map (fun item ->
        match item with
        | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
          let get_str  k = assoc_string_opt fields k in
          let get_strl k =
            match List.assoc_opt k fields with
            | Some (Otoml.TomlArray arr | Otoml.TomlTableArray arr) ->
              List.filter_map (function Otoml.TomlString s -> Some s | _ -> None) arr
            | _ -> []
          in
          (match get_str "name", get_str "provider", get_str "model" with
           | Some name, Some provider_ref, Some model ->
             Some {
               name;
               worker_role    = Option.value ~default:"atomic" (get_str "role");
               provider_ref;
               model;
               max_tokens    = assoc_int_opt   fields "max_tokens";
               temperature   = assoc_float_opt fields "temperature";
               tool_names    = get_strl "tools";
               system_prompt = Option.value ~default:"" (get_str "system_prompt");
               realm         = get_str "realm";
               gres          = parse_gres fields;
             }
           | _ -> None)
        | _ -> None
      ) elements
    with _ -> []

(** Read a single [providers.<name>] table. *)
let get_provider_config name =
  match get_ast () with
  | None -> None
  | Some ast ->
    try
      let node = Otoml.find ast (fun x -> x) ["providers"; name] in
      (match node with
       | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
         (match assoc_string_opt fields "base_url" with
          | None -> None
          | Some base_url ->
            Some {
              base_url;
              api_key_env = assoc_string_opt fields "api_key_env";
              org_id_env  = assoc_string_opt fields "org_id_env";
            })
       | _ -> None)
    with _ -> None

(** Read the [orchestrator] table. Returns (provider_ref, model). *)
let get_orchestrator () =
  match get_ast () with
  | None -> None
  | Some ast ->
    try
      let node = Otoml.find ast (fun x -> x) ["orchestrator"] in
      (match node with
       | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
         (match assoc_string_opt fields "provider",
                assoc_string_opt fields "model" with
          | Some p, Some m -> Some (p, m)
          | _ -> None)
       | _ -> None)
    with _ -> None

let get_stream () =
  get_bool_opt (Some "CARAVAN_STREAM") "stream" |> Option.value ~default:true

let get_spinner_enabled () =
  get_bool_opt ~path:["spinner"] (Some "CARAVAN_SPINNER") "enabled" |> Option.value ~default:true

let get_spinner_verbose () =
  match get_bool_opt ~path:["spinner"] (Some "CARAVAN_SPINNER_VERBOSE") "verbose" with
  | Some b -> b
  | None ->
    get_bool_opt (Some "CARAVAN_VERBOSE") "verbose" |> Option.value ~default:false

(** Read the TOML [spinner.<tool>] key as a string or array of strings. *)
let get_spinner_verbs tool_name =
  match get_ast () with
  | None -> None
  | Some ast ->
    (* Try array first, then fall back to plain string. *)
    (try
       let arr = Otoml.find ast (Otoml.get_array Otoml.get_string) ["spinner"; tool_name] in
       if arr = [] then None else Some arr
     with _ ->
       try Some [Otoml.find ast Otoml.get_string ["spinner"; tool_name]]
       with _ -> None)

(** Return the list of verbs for [tool_name].
    TOML overrides take priority; built-in defaults are lists so every
    tool can have several synonyms picked randomly at call time. *)
let get_verbs tool_name =
  match get_spinner_verbs tool_name with
  | Some vs -> vs
  | None    ->
    match tool_name with
    | "thinking" -> ["Thinking"]
    | "summarizing" -> ["Summarizing"]
    | _ -> ["Running " ^ tool_name]

(** Pick a verb at random from a list. *)
let pick_verb = function
  | []  -> "Working"
  | [v] -> v
  | vs  -> List.nth vs (Random.int (List.length vs))

(* ── Overhaul additions ─────────────────────────────────────────────────── *)

(** Directory holding Caravan state (config, logs). *)
let caravan_dir () =
  match Sys.getenv_opt "CARAVAN_CONFIG" with
  | Some p when p <> "" -> Filename.dirname p
  | _ ->
    let home = match Sys.getenv_opt "HOME" with Some h -> h | None -> "." in
    Filename.concat home ".caravan"

(** Where session transcripts (JSONL event logs) are written. *)
let log_dir () = Filename.concat (caravan_dir ()) "logs"

(** Whether to write a JSONL transcript per session (default: true). *)
let get_transcript_enabled () =
  get_bool_opt (Some "CARAVAN_TRANSCRIPT") "transcript" |> Option.value ~default:true

(** Tool permission mode: "auto" (allow all), "ask" (prompt for mutating
    tools), "readonly" (deny mutating tools). Default "auto". *)
let get_permission_mode () =
  get_string_opt (Some "CARAVAN_PERMISSIONS") "permissions"
  |> Option.value ~default:"auto"
  |> String.lowercase_ascii

(** Agent turn budget (CLI flag > env > TOML > default 10). *)
let get_max_turns () =
  get_int_opt (Some "CARAVAN_MAX_TURNS") "max_turns" |> Option.value ~default:10

(** Whether the agent loop injects budget-awareness nudges (default: true). *)
let get_nudge_enabled () =
  get_bool_opt (Some "CARAVAN_NUDGE") "nudge" |> Option.value ~default:true

(** Look up an API key: [env_var] first, then [api_keys.<name>] in TOML,
    then a legacy top-level key if given. *)
let get_api_key ~env_var ~name ?legacy_key () =
  match Sys.getenv_opt env_var with
  | Some k when k <> "" -> Some k
  | _ ->
    let from_table =
      match get_ast () with
      | None -> None
      | Some ast ->
        (try Some (Otoml.find ast Otoml.get_string ["api_keys"; name])
         with _ -> None)
    in
    (match from_table with
     | Some _ as r -> r
     | None ->
       (match legacy_key with
        | Some k -> get_string k
        | None -> None))

(** Force the cached TOML AST to be re-read (used by tests and
    [caravan config set]). *)
let reload () =
  cached_ast := None;
  current_loaded_path := None

(* ── Writing configuration ──────────────────────────────────────────────
   One shared implementation behind `caravan config set`, the REPL's
   /config set, /key, and the web cockpit — so a no-code user can edit
   the config from any surface without touching a shell editor. *)

(** Parse a CLI-style value into a TOML value: int, float, bool, string. *)
let toml_value_of_string s =
  match int_of_string_opt s with
  | Some i -> Otoml.integer i
  | None ->
    match float_of_string_opt s with
    | Some f -> Otoml.float f
    | None ->
      match String.lowercase_ascii s with
      | "true" -> Otoml.boolean true
      | "false" -> Otoml.boolean false
      | _ -> Otoml.string s

(** Set a (dotted) key to a TOML value in the config file, creating the
    file/directories as needed. The file is (re)written 0600 and the
    in-memory cache is refreshed. Returns the config path. *)
let set_toml_value dotted_key (value : Otoml.t) : (string, string) result =
  let path = config_path () in
  try
    let ast =
      if Sys.file_exists path then
        (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
      else Otoml.TomlTable []
    in
    let keys = String.split_on_char '.' dotted_key |> List.filter (fun k -> k <> "") in
    if keys = [] then Error "empty key"
    else begin
      let ast' = Otoml.update ast keys (Some value) in
      let ast'' = ensure_orchestrator_in_ast ast' in
      write_ast ast''
    end
  with exn -> Error (Printexc.to_string exn)

(** [set_value "model" "llama3.2"] — string input variant with type
    sniffing (ints, floats, bools become typed TOML values). *)
let set_value dotted_key raw_value =
  set_toml_value dotted_key (toml_value_of_string raw_value)

(** Store an API key under [api_keys.<provider>]. Kept separate so all
    call sites treat keys as strings verbatim (a numeric-looking key must
    never be coerced to an integer). *)
let set_api_key provider key =
  set_toml_value ("api_keys." ^ provider) (Otoml.string key)

(** Keys a settings UI should offer, with short descriptions and the
    values they accept. Single source of truth for the REPL and web
    settings surfaces. *)
let editable_keys : (string * string * string) list = [
  ("provider",    "Backend to talk to",                 "see `caravan providers`");
  ("model",       "Model name",                         "provider-specific");
  ("base_url",    "Endpoint override",                  "URL");
  ("system",      "Default system prompt",              "text");
  ("stream",      "Stream tokens as they arrive",       "true | false");
  ("max_turns",   "Agent turn budget",                  "integer");
  ("nudge",       "Budget nudges in agent loops",       "true | false");
  ("permissions", "Mutating-tool policy",               "auto | ask | readonly");
  ("transcript",  "JSONL session logs",                 "true | false");
  ("strict_mode", "bash tool discipline",               "0 | 1 | 2");
  ("enable_subagents", "Offer the delegate tool when [[subagents]] exist", "true | false");
  ("verbose",     "Verbose tool call & trace output",   "true | false");
]

(** Field descriptors for the subagent creation UI — single source of truth
    shared by the REPL and web cockpit.
    [(toml_key, label, placeholder, required)]. *)
let editable_subagent_fields : (string * string * string * bool) list = [
  ("name",          "Name",           "e.g. coder",          true);
  ("provider",      "Provider",       "registry or [providers.*]", true);
  ("model",         "Model",          "e.g. qwen3:8b",       true);
  ("system_prompt", "System prompt",  "persona / instructions", false);
  ("tools",         "Tools",          "comma-separated names", false);
  ("role",          "Worker role",    "atomic | parallel",   false);
  ("max_tokens",    "Max tokens",     "integer",             false);
  ("temperature",   "Temperature",    "0.0 – 2.0",          false);
  ("realm",         "Sandbox realm",  "plugin toolset realm name", false);
]

(** Append a new [[subagents]] entry to the config file.
    [fields] is an assoc list of string key/value pairs coming from the
    web or REPL UI. Required keys: name, provider, model. *)
let add_subagent (fields : (string * string) list) : (string, string) result =
  let lookup k = List.assoc_opt k fields in
  match lookup "name", lookup "provider", lookup "model" with
  | None, _, _ -> Error "name is required"
  | _, None, _ -> Error "provider is required"
  | _, _, None -> Error "model is required"
  | Some name, Some provider, Some model ->
    if String.trim name = "" then Error "name must not be empty"
    else if String.trim provider = "" then Error "provider must not be empty"
    else if String.trim model = "" then Error "model must not be empty"
    else begin
      let path = config_path () in
      try
        let ast =
          if Sys.file_exists path then
            (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
          else Otoml.TomlTable []
        in
        (* Build the TOML inline-table for this subagent *)
        let pairs = ref [
          ("name",     Otoml.string name);
          ("provider", Otoml.string provider);
          ("model",    Otoml.string model);
        ] in
        (match lookup "system_prompt" with
         | Some sp when sp <> "" -> pairs := !pairs @ [("system_prompt", Otoml.string sp)]
         | _ -> ());
        (match lookup "tools" with
         | Some ts when ts <> "" ->
           let tool_list =
             String.split_on_char ',' ts
             |> List.map String.trim
             |> List.filter (fun s -> s <> "")
             |> List.map Otoml.string
           in
           if tool_list <> [] then
             pairs := !pairs @ [("tools", Otoml.TomlArray tool_list)]
         | _ -> ());
        (match lookup "role" with
         | Some r when r <> "" -> pairs := !pairs @ [("role", Otoml.string r)]
         | _ -> ());
        (match lookup "realm" with
         | Some r when r <> "" -> pairs := !pairs @ [("realm", Otoml.string r)]
         | _ -> ());
        (match lookup "max_tokens" with
         | Some mt when mt <> "" ->
           (match int_of_string_opt mt with
            | Some n -> pairs := !pairs @ [("max_tokens", Otoml.integer n)]
            | None -> ())
         | _ -> ());
        (match lookup "temperature" with
         | Some t when t <> "" ->
           (match float_of_string_opt t with
            | Some f -> pairs := !pairs @ [("temperature", Otoml.float f)]
            | None -> ())
         | _ -> ());
        let entry = Otoml.TomlTable !pairs in
        (* Append to the existing [[subagents]] array or create one *)
        let existing =
          try match Otoml.find ast (fun x -> x) ["subagents"] with
            | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
            | _ -> []
          with _ -> []
        in
        (* Check for duplicate name *)
        let dupe =
          List.exists (fun item ->
            match item with
            | Otoml.TomlTable fs | Otoml.TomlInlineTable fs ->
              assoc_string_opt fs "name" = Some name
            | _ -> false
          ) existing
        in
        if dupe then Error (Printf.sprintf "subagent '%s' already exists" name)
        else begin
          let new_arr = Otoml.TomlTableArray (existing @ [entry]) in
          let ast' = Otoml.update ast ["subagents"] (Some new_arr) in
          let ast'' = ensure_orchestrator_in_ast ~fallback_provider:provider ~fallback_model:model ast' in
          write_ast ast''
        end
      with exn -> Error (Printexc.to_string exn)
    end

(** Remove a [[subagents]] entry by name. *)
let delete_subagent name : (string, string) result =
  let path = config_path () in
  try
    let ast =
      if Sys.file_exists path then
        (try Otoml.Parser.from_file path with _ -> Otoml.TomlTable [])
      else Otoml.TomlTable []
    in
    let existing =
      try match Otoml.find ast (fun x -> x) ["subagents"] with
        | Otoml.TomlArray l | Otoml.TomlTableArray l -> l
        | _ -> []
      with _ -> []
    in
    let filtered =
      List.filter (fun item ->
        match item with
        | Otoml.TomlTable fs | Otoml.TomlInlineTable fs ->
          assoc_string_opt fs "name" <> Some name
        | _ -> true
      ) existing
    in
    if List.length filtered = List.length existing then
      Error (Printf.sprintf "subagent '%s' not found" name)
    else begin
      let value =
        if filtered = [] then None
        else Some (Otoml.TomlTableArray filtered)
      in
      let ast' = Otoml.update ast ["subagents"] value in
      write_ast ast'
    end
  with exn -> Error (Printexc.to_string exn)

(** Serialize a [subagent_config] to a JSON object for the web API. *)
let subagent_to_json (cfg : subagent_config) : Yojson.Safe.t =
  yojson_of_subagent_config cfg

