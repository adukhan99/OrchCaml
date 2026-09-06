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

(* ââ Safe file writes âââââââââââââââââââââââââââââââââ
   The config file holds API keys and is edited from four surfaces, so a
   write goes to a sibling temp file and is renamed into place: an
   interrupted write can never truncate it.  The previous contents are
   kept as [config.toml.bak] â a config is hand-edited often enough that
   one undo step is worth the inode. *)

(** Create [path] and any missing parents, 0700.  Caravan's directories
    hold transcripts and a config with API keys in it, so they are never
    group- or world-readable. *)
let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if parent <> path then mkdir_p parent;
    (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let read_file_opt path =
  try
    let ic = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr ic)
      (fun () -> Some (really_input_string ic (in_channel_length ic)))
  with _ -> None

(** Write [text] as the config file, atomically and 0600, keeping the
    previous contents alongside as [.bak].  Returns the path written. *)
let write_config_text text : (string, string) result =
  let path = config_path () in
  mkdir_p (Filename.dirname path);
  (match read_file_opt path with
   | Some prev when prev <> text ->
     (try
        let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 (path ^ ".bak") in
        output_string oc prev;
        close_out oc
      with _ -> ())
   | _ -> ());
  let tmp = path ^ ".tmp" in
  (try
     let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 tmp in
     Fun.protect ~finally:(fun () -> try close_out oc with _ -> ())
       (fun () -> output_string oc text);
     (try Unix.chmod tmp 0o600 with _ -> ());
     Sys.rename tmp path;
     reload ();
     Ok path
   with exn ->
     (try Sys.remove tmp with _ -> ());
     Error (Printexc.to_string exn))

(** Rewrite the config file from an AST.  Lossy â an otoml AST carries no
    comments and reprints tables in its own order â so scalar writes go
    through [set_toml_value]'s textual splice instead, and this remains the
    fallback for structural edits (subagents, MCP servers) and for files
    the splicer cannot reason about. *)
let write_ast ast = write_config_text (Otoml.Printer.to_string ast)

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

let is_first_run () =
  let path = config_path () in
  not (Sys.file_exists path) ||
  (try
     let ic = open_in path in
     let len = in_channel_length ic in
     close_in ic;
     len < 50
   with _ -> true)

(** Parse the config file, or [None] when there is none.  Reading is
    pure: it creates no directory and writes no file.  (It used to
    materialise a default config and an [orchestrator] table on every
    read, which meant `caravan config get` rewrote the user's file and
    stripped its comments.) *)
let load_toml () =
  let path = config_path () in
  if Sys.file_exists path then
    try Some (Otoml.Parser.from_file path)
    with exn ->
      Printf.eprintf "[Caravan] Warning: Failed to parse %s: %s\n%!"
        path (Printexc.to_string exn);
      None
  else None

(** Parse the config file, reporting the parse error rather than a
    warning on stderr.  Used by `/config edit` to reject an edit that
    would leave the file unreadable, and by the doctor. *)
let parse_check () : (unit, string) result =
  let path = config_path () in
  if not (Sys.file_exists path) then Ok ()
  else match Otoml.Parser.from_file_result path with
    | Ok _ -> Ok ()
    | Error e -> Error e

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

(** Read a top-level or [orchestrator] TOML float. Accepts integers too. *)
let get_float key =
  match get_ast () with
  | None -> None
  | Some ast ->
    try Some (Otoml.find ast Otoml.get_float [key])
    with _ ->
      try Some (float_of_int (Otoml.find ast Otoml.get_integer [key]))
      with _ ->
        try Some (float_of_int (Otoml.find ast Otoml.get_integer ["orchestrator"; key]))
        with _ -> None

let get_int_opt env_var toml_key =
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" -> (try Some (int_of_string v) with _ -> get_int toml_key)
               | _ -> get_int toml_key)
  | None -> get_int toml_key

let get_float_opt env_var toml_key =
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" -> (try Some (float_of_string v) with _ -> get_float toml_key)
               | _ -> get_float toml_key)
  | None -> get_float toml_key

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

(** Read a TOML boolean field from an association list, returning None on miss. *)
let assoc_bool_opt fields key =
  match List.assoc_opt key fields with
  | Some (Otoml.TomlBoolean b) -> Some b
  | _ -> None

(** Read the [capabilities] table: each sub-table keyed by a model-name
    pattern, e.g. [capabilities."my-model"] with capability fields.
    Returns [(pattern, fields)] in file order; see [Capability.lookup]. *)
let get_capability_overrides () =
  match get_ast () with
  | None -> []
  | Some ast ->
    try
      match Otoml.find ast (fun x -> x) ["capabilities"] with
      | Otoml.TomlTable entries | Otoml.TomlInlineTable entries ->
        List.filter_map (fun (pattern, v) ->
          match v with
          | Otoml.TomlTable fields | Otoml.TomlInlineTable fields ->
            Some (pattern, fields)
          | _ -> None
        ) entries
      | _ -> []
    with _ -> []

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

(** Read the [orchestrator] table. Returns (provider_ref, model).

    When the table is absent the pair is resolved from the top-level
    [provider]/[model] keys â in memory.  Caravan used to write the
    table out to disk to achieve the same thing, which is why a plain
    read could rewrite the user's file. *)
let get_orchestrator () =
  let from_table =
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
  in
  match from_table with
  | Some _ as r -> r
  | None ->
    match get_string "provider", get_string "model" with
    | Some p, Some m when String.trim p <> "" && String.trim m <> "" -> Some (p, m)
    | _ -> None

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

(** Agent turn budget (CLI flag > env > TOML > default 24).  Raised
    from 10 once C4 made the ceiling actually enforceable: weak models
    need more steps to reach the same place, and the budget nudges keep
    long runs pointed at the task. *)
let get_max_turns () =
  get_int_opt (Some "CARAVAN_MAX_TURNS") "max_turns" |> Option.value ~default:24

(** Whether the agent loop injects budget-awareness nudges (default: true). *)
let get_nudge_enabled () =
  get_bool_opt (Some "CARAVAN_NUDGE") "nudge" |> Option.value ~default:true

(** How tool calls are recognised in model replies:
    - "auto"   (default) — native tool_calls, plus the text fallback
      parser when a reply's whole content is a well-formed invocation;
    - "native" — trust only the API tool_calls field;
    - "text"   — same recognition as "auto"; the explicit setting exists
      so text-protocol use is a documented first-class mode, and so
      front-ends can add text-mode prompt scaffolding on top. *)
let get_tool_call_mode () =
  get_string_opt (Some "CARAVAN_TOOL_CALL_MODE") "tool_call_mode"
  |> Option.value ~default:"auto"
  |> String.lowercase_ascii

(** Which tools are exposed to the model: "auto" (capability-driven —
    low-capability models get the core set), "core" (force the reduced
    set), "full" (everything). Default "auto". *)
let get_tool_profile () =
  get_string_opt (Some "CARAVAN_TOOL_PROFILE") "tool_profile"
  |> Option.value ~default:"auto"
  |> String.lowercase_ascii

(** Model used for history summarisation calls, when set — routing
    compaction to a cheap model keeps it off the working model's rate
    limit and budget.  Unset (default) means use the session's model. *)
let get_summarize_model () =
  get_string_opt (Some "CARAVAN_SUMMARIZE_MODEL") "summarize_model"

(** Whether agent runs require an explicit [finish] tool call to count
    as complete (default: true). When false, a plain text reply ends
    the run — the pre-refactor behaviour. *)
let get_require_finish () =
  get_bool_opt (Some "CARAVAN_REQUIRE_FINISH") "require_finish"
  |> Option.value ~default:true

(** How aggressively provider calls retry transient failures:
    "off" | "low" | "medium" | "high" (default "medium"). Parsed into a
    [Provider.Retry.mode] at the call site; unknown values fall back to
    the default there. *)
let get_provider_retry_mode () =
  get_string_opt (Some "CARAVAN_PROVIDER_RETRY") "provider_retry"
  |> Option.value ~default:"medium"

(** Base delay (seconds) for exponential provider-retry backoff: attempt
    N sleeps base * 2^(N-1), capped at 30s. Default 0.5. *)
let get_provider_retry_base_delay () =
  get_float_opt (Some "CARAVAN_PROVIDER_RETRY_BASE_DELAY") "provider_retry_base_delay"
  |> Option.value ~default:0.5

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

(* ── Comment-preserving TOML writes ─────────────────────────────────────
   Reprinting the file from an otoml AST loses every comment and reorders
   the user's tables.  A scalar write is therefore applied as a textual
   splice: find the assignment line for the key inside its table and
   replace only the value span, leaving indentation, ordering, and
   trailing comments exactly as the user wrote them.

   The spliced text is re-parsed and the value read back before anything
   is written, so a splice that cannot be expressed — a multi-line value,
   a quoted table path, a key living inside an array-of-tables — degrades
   to the lossy AST printer rather than corrupting the file. *)

let toml_string_literal s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 0x20 ->
      Buffer.add_string buf (Printf.sprintf "\\u%04X" (Char.code c))
    | c -> Buffer.add_char buf c) s;
  Buffer.add_char buf '"';
  Buffer.contents buf

(** TOML distinguishes [1] (integer) from [1.0] (float), so a float
    literal must keep a fraction or an exponent. *)
let toml_float_literal f =
  let s = Printf.sprintf "%.12g" f in
  let marked =
    String.exists (fun c -> c = '.' || c = 'e' || c = 'E' || c = 'n' || c = 'i') s
  in
  if marked then s else s ^ ".0"

(** Render a scalar as TOML source.  [None] for composite values, which
    the splicer leaves to the AST printer. *)
let toml_scalar_literal (v : Otoml.t) =
  match v with
  | Otoml.TomlString s  -> Some (toml_string_literal s)
  | Otoml.TomlInteger i -> Some (string_of_int i)
  | Otoml.TomlFloat f   -> Some (toml_float_literal f)
  | Otoml.TomlBoolean b -> Some (string_of_bool b)
  | _ -> None

(** A bare TOML key needs no quoting, and is the only shape the splicer
    walks; anything else falls back to the AST. *)
let is_bare_key k =
  k <> "" &&
  String.for_all (fun c ->
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
    || ('0' <= c && c <= '9') || c = '_' || c = '-') k

type toml_header = {
  th_path  : string list option;  (** [None] when the path is not bare *)
  th_array : bool;                (** [[array of tables]] *)
}

(** [Some header] when the line opens a table.  A header we cannot read
    still closes the previous table, so the walker must see it. *)
let parse_table_header line =
  let s = String.trim line in
  let n = String.length s in
  if n < 3 || s.[0] <> '[' then None
  else begin
    let is_array = s.[1] = '[' in
    let open_len = if is_array then 2 else 1 in
    let close = if is_array then "]]" else "]" in
    let close_len = String.length close in
    let rec find i =
      if i + close_len > n then None
      else if String.sub s i close_len = close then Some i
      else find (i + 1)
    in
    match find open_len with
    | None -> None
    | Some i ->
      let inner = String.trim (String.sub s open_len (i - open_len)) in
      if inner = "" then None
      else
        let parts = String.split_on_char '.' inner |> List.map String.trim in
        let bare = parts <> [] && List.for_all is_bare_key parts in
        Some { th_path = (if bare then Some parts else None); th_array = is_array }
  end

(** Matches an assignment to [key] at the start of a line, bare or quoted. *)
let assignment_re key =
  let open Re in
  compile (seq [ bos; rep (set " \t");
                 alt [ str key;
                       seq [char '"';  str key; char '"'];
                       seq [char '\''; str key; char '\''] ];
                 rep (set " \t"); char '=' ])

(** Index of the line assigning [key] directly inside table [path].
    Array-of-tables sections are skipped: a key inside one is ambiguous
    (which element?), so those writes go to the AST. *)
let locate_assignment lines ~path ~key =
  let re = assignment_re key in
  let cur = ref (Some []) in
  let found = ref None in
  List.iteri (fun i line ->
    match parse_table_header line with
    | Some h -> cur := (if h.th_array then None else h.th_path)
    | None ->
      if !found = None && !cur = Some path && Re.execp re line then found := Some i
  ) lines;
  !found

(** Replace the value in an assignment line, keeping its indentation and
    any trailing comment.  [None] when the value runs past the end of the
    line (a multi-line string or array), which the splicer will not touch. *)
let replace_value_in_line line ~literal =
  match String.index_opt line '=' with
  | None -> None
  | Some eq ->
    let n = String.length line in
    (* Walk the value span tracking string and bracket state, so a '#'
       inside a string is not mistaken for the start of a comment. *)
    let rec scan i ~basic ~lit ~depth =
      if i >= n then (if basic || lit || depth > 0 then None else Some n)
      else
        let c = line.[i] in
        if basic then
          if c = '\\' then scan (i + 2) ~basic ~lit ~depth
          else scan (i + 1) ~basic:(c <> '"') ~lit ~depth
        else if lit then scan (i + 1) ~basic ~lit:(c <> '\'') ~depth
        else match c with
          | '"'       -> scan (i + 1) ~basic:true ~lit ~depth
          | '\''      -> scan (i + 1) ~basic ~lit:true ~depth
          | '[' | '{' -> scan (i + 1) ~basic ~lit ~depth:(depth + 1)
          | ']' | '}' -> scan (i + 1) ~basic ~lit ~depth:(depth - 1)
          | '#' when depth = 0 -> Some i
          | _         -> scan (i + 1) ~basic ~lit ~depth
    in
    match scan (eq + 1) ~basic:false ~lit:false ~depth:0 with
    | None -> None
    | Some stop ->
      let comment = String.sub line stop (n - stop) in
      let span = String.sub line (eq + 1) (stop - eq - 1) in
      let gap =
        if comment = "" then ""
        else begin
          let j = ref (String.length span) in
          while !j > 0 && (span.[!j - 1] = ' ' || span.[!j - 1] = '\t') do decr j done;
          let ws = String.sub span !j (String.length span - !j) in
          if ws = "" then "  " else ws
        end
      in
      Some (String.sub line 0 (eq + 1) ^ " " ^ literal ^ gap ^ comment)

(** Add [line] to table [path], after the table's last existing entry so
    it lands under the right header, creating the header if needed. *)
let insert_assignment lines ~path ~line =
  let arr = Array.of_list lines in
  let n = Array.length arr in
  let cur = ref (Some []) in
  let header_seen = ref (path = []) in
  let last = ref None in
  let first_header = ref None in
  for i = 0 to n - 1 do
    match parse_table_header arr.(i) with
    | Some h ->
      if !first_header = None then first_header := Some i;
      cur := (if h.th_array then None else h.th_path);
      if !cur = Some path then (header_seen := true; last := Some i)
    | None ->
      let t = String.trim arr.(i) in
      if !cur = Some path && t <> "" && t.[0] <> '#' then last := Some i
  done;
  let insert_before =
    match !last with
    | Some i -> Some (i + 1)
    | None ->
      if not !header_seen then None
      else match !first_header with
        | Some i when path = [] -> Some i   (* keep top-level keys above tables *)
        | _ -> Some n
  in
  match insert_before with
  | Some i ->
    Array.to_list (Array.sub arr 0 i) @ [line] @ Array.to_list (Array.sub arr i (n - i))
  | None ->
    let rec drop_trailing = function
      | l :: rest when String.trim l = "" -> drop_trailing rest
      | rest -> rest
    in
    let body = List.rev (drop_trailing (List.rev lines)) in
    let sep = if body = [] then [] else [""] in
    body @ sep @ ["[" ^ String.concat "." path ^ "]"; line; ""]

(** Apply a scalar write to the config file's text.  [None] when the edit
    is not expressible as a splice, or when the result does not read back
    as the value asked for. *)
let splice_scalar ~text ~keys ~key ~path (value : Otoml.t) =
  match toml_scalar_literal value with
  | None -> None
  | Some literal ->
    if not (List.for_all is_bare_key (key :: path)) then None
    else begin
      let lines = if String.trim text = "" then [] else String.split_on_char '\n' text in
      let lines' =
        match locate_assignment lines ~path ~key with
        | Some i ->
          (match replace_value_in_line (List.nth lines i) ~literal with
           | Some l -> Some (List.mapi (fun j x -> if j = i then l else x) lines)
           | None -> None)
        | None ->
          Some (insert_assignment lines ~path
                  ~line:(Printf.sprintf "%s = %s" key literal))
      in
      match lines' with
      | None -> None
      | Some ls ->
        let text' = String.concat "\n" ls in
        let text' = if text' <> "" && text'.[String.length text' - 1] <> '\n'
                    then text' ^ "\n" else text' in
        (* Never write a file we cannot read back as what was asked for. *)
        (match Otoml.Parser.from_string_result text' with
         | Error _ -> None
         | Ok ast ->
           (try if Otoml.find ast (fun x -> x) keys = value then Some text' else None
            with _ -> None))
    end

(** Remove the assignment line for [keys] from the config text. *)
let splice_removal ~text ~keys ~key ~path =
  if not (List.for_all is_bare_key (key :: path)) then None
  else begin
    let lines = String.split_on_char '\n' text in
    match locate_assignment lines ~path ~key with
    | None -> None
    | Some i ->
      let text' = String.concat "\n" (List.filteri (fun j _ -> j <> i) lines) in
      (match Otoml.Parser.from_string_result text' with
       | Error _ -> None
       | Ok ast ->
         (try ignore (Otoml.find ast (fun x -> x) keys); None   (* still there *)
          with _ -> Some text'))
  end

(** Read the config file as text, or [""] when there is none. *)
let config_text () = Option.value ~default:"" (read_file_opt (config_path ()))

(** The AST as last written, for the fallback paths. *)
let config_ast_for_write text =
  if String.trim text = "" then Otoml.TomlTable []
  else (try Otoml.Parser.from_string text with _ -> Otoml.TomlTable [])

let split_dotted dotted_key =
  String.split_on_char '.' dotted_key |> List.filter (fun k -> k <> "")

(** Render a value as TOML source for a freshly written block: scalars,
    plus arrays of scalars (a subagent's tool list). *)
let toml_inline_literal (v : Otoml.t) =
  match toml_scalar_literal v with
  | Some _ as s -> s
  | None ->
    match v with
    | Otoml.TomlArray items ->
      let parts = List.map toml_scalar_literal items in
      if List.for_all Option.is_some parts then
        Some ("[" ^ String.concat ", " (List.filter_map Fun.id parts) ^ "]")
      else None
    | _ -> None

(** Append a [[table]] block with [pairs] to the config, as text.

    Structural edits used to go through the AST printer, which meant
    declaring a subagent silently deleted every comment in the file.
    Appending is the one structural edit that is trivially expressible as
    text, and it is the common one. [None] if any value cannot be
    rendered, leaving the caller to fall back. *)
let append_table_array ~table (pairs : (string * Otoml.t) list) =
  let rendered = List.map (fun (k, v) -> (k, toml_inline_literal v)) pairs in
  if not (is_bare_key table)
  || not (List.for_all (fun (k, v) -> is_bare_key k && v <> None) rendered)
  then None
  else begin
    let text = config_text () in
    let lines = if String.trim text = "" then [] else String.split_on_char '\n' text in
    let rec drop_trailing = function
      | l :: rest when String.trim l = "" -> drop_trailing rest
      | rest -> rest
    in
    let body = List.rev (drop_trailing (List.rev lines)) in
    let block =
      Printf.sprintf "[[%s]]" table
      :: List.filter_map
           (fun (k, v) -> Option.map (Printf.sprintf "%s = %s" k) v) rendered
    in
    let sep = if body = [] then [] else [""] in
    let text' = String.concat "\n" (body @ sep @ block) ^ "\n" in
    match Otoml.Parser.from_string_result text' with
    | Error _ -> None
    | Ok _ -> Some text'
  end

(** Remove the [[table]] block whose [name] field matches, as text: from
    its header down to the line before the next table header. *)
let remove_table_array ~table ~name =
  let text = config_text () in
  if String.trim text = "" then None
  else begin
    let arr = Array.of_list (String.split_on_char '\n' text) in
    let n = Array.length arr in
    let name_re = assignment_re "name" in
    (* Walk the array-of-tables blocks, looking for the matching name. *)
    let target = ref None in
    let block_start = ref None in
    let matched = ref false in
    let close i =
      match !block_start with
      | Some s when !matched && !target = None -> target := Some (s, i)
      | _ -> ()
    in
    for i = 0 to n - 1 do
      match parse_table_header arr.(i) with
      | Some h ->
        close i;
        if h.th_array && h.th_path = Some [table] then begin
          block_start := Some i; matched := false
        end else block_start := None
      | None ->
        if !block_start <> None && Re.execp name_re arr.(i) then
          (* Compare the parsed value, so quoting style does not matter. *)
          (match Otoml.Parser.from_string_result (String.trim arr.(i)) with
           | Ok ast ->
             (try if Otoml.find ast Otoml.get_string ["name"] = name then matched := true
              with _ -> ())
           | Error _ -> ())
    done;
    close n;
    match !target with
    | None -> None
    | Some (s, e) ->
      let keep i = i < s || i >= e in
      let kept = List.filteri (fun i _ -> keep i) (Array.to_list arr) in
      (* Collapse the blank line the removed block left behind. *)
      let rec squeeze = function
        | a :: b :: rest when String.trim a = "" && String.trim b = "" -> squeeze (b :: rest)
        | x :: rest -> x :: squeeze rest
        | [] -> []
      in
      let text' = String.concat "\n" (squeeze kept) in
      (match Otoml.Parser.from_string_result text' with
       | Error _ -> None
       | Ok _ -> Some text')
  end

(** Set a (dotted) key to a TOML value in the config file, creating the
    file and its directory as needed.  The user's comments, ordering and
    indentation survive whenever the edit can be expressed as a splice;
    otherwise the AST printer takes over.  Returns the config path. *)
let set_toml_value dotted_key (value : Otoml.t) : (string, string) result =
  let keys = split_dotted dotted_key in
  match List.rev keys with
  | [] -> Error "empty key"
  | key :: rev_path ->
    let path = List.rev rev_path in
    let text = config_text () in
    (match splice_scalar ~text ~keys ~key ~path value with
     | Some text' -> write_config_text text'
     | None ->
       (try write_ast (Otoml.update (config_ast_for_write text) keys (Some value))
        with exn -> Error (Printexc.to_string exn)))

(** Remove a (dotted) key from the config file, so a setting falls back to
    its default instead of being pinned to whatever was typed once. *)
let unset_toml_value dotted_key : (string, string) result =
  let keys = split_dotted dotted_key in
  match List.rev keys with
  | [] -> Error "empty key"
  | key :: rev_path ->
    let path = List.rev rev_path in
    let text = config_text () in
    let present =
      match Otoml.Parser.from_string_result text with
      | Error _ -> false
      | Ok ast -> (try ignore (Otoml.find ast (fun x -> x) keys); true with _ -> false)
    in
    if not present then Error (Printf.sprintf "'%s' is not set" dotted_key)
    else
      (match splice_removal ~text ~keys ~key ~path with
       | Some text' -> write_config_text text'
       | None ->
         (try write_ast (Otoml.update (config_ast_for_write text) keys None)
          with exn -> Error (Printexc.to_string exn)))

(** [set_value "model" "llama3.2"] — string input variant with type
    sniffing (ints, floats, bools become typed TOML values). *)
let set_value dotted_key raw_value =
  set_toml_value dotted_key (toml_value_of_string raw_value)

(** Store an API key under [api_keys.<provider>]. Kept separate so all
    call sites treat keys as strings verbatim (a numeric-looking key must
    never be coerced to an integer). *)
let set_api_key provider key =
  set_toml_value ("api_keys." ^ provider) (Otoml.string key)

(* ── The setting schema ─────────────────────────────────────────────────
   One typed description of every user-editable setting.  It is the single
   source of truth for `/config keys`, write validation, the web cockpit
   form, and the doctor's config checks, so a new setting is declared once
   and every surface picks it up.

   The point of the types is that a setting knows what it accepts: writing
   [model 3] used to store the integer 3, after which every read of the
   model fell silently back to the provider default. *)

type setting_kind =
  | Bool
  | Int   of int * int        (** inclusive bounds *)
  | Float of float * float    (** inclusive bounds *)
  | Enum  of string list      (** matched case-insensitively, stored canonical *)
  | Str                       (** free text — never coerced to a number *)

(** When a change takes effect, so a UI can say so instead of leaving the
    user wondering why nothing happened. *)
type setting_scope =
  | Live         (** the running session picks it up on the next turn *)
  | New_session  (** applies when the next session is created *)
  | Restart      (** applies on the next process start *)

type setting = {
  key     : string;
  kind    : setting_kind;
  doc     : string;
  default : string;          (** rendered default, for display only *)
  env     : string option;   (** environment variable that shadows this key *)
  scope   : setting_scope;
}

let settings : setting list = [
  { key = "provider"; kind = Str; scope = New_session;
    doc = "Backend to talk to"; default = "ollama";
    env = Some "CARAVAN_PROVIDER" };
  { key = "model"; kind = Str; scope = New_session;
    doc = "Model name"; default = "provider default";
    env = Some "CARAVAN_MODEL" };
  { key = "base_url"; kind = Str; scope = New_session;
    doc = "Endpoint override"; default = "provider default";
    env = Some "CARAVAN_BASE_URL" };
  { key = "system"; kind = Str; scope = New_session;
    doc = "Extra system prompt (appended to the shipped default)"; default = "none";
    env = None };
  { key = "system_replace"; kind = Bool; scope = New_session;
    doc = "system replaces the shipped default instead of appending";
    default = "false"; env = Some "CARAVAN_SYSTEM_REPLACE" };
  { key = "stream"; kind = Bool; scope = Live;
    doc = "Stream tokens as they arrive"; default = "true";
    env = Some "CARAVAN_STREAM" };
  { key = "max_turns"; kind = Int (1, 1000); scope = Live;
    doc = "Agent turn budget"; default = "24";
    env = Some "CARAVAN_MAX_TURNS" };
  { key = "nudge"; kind = Bool; scope = Live;
    doc = "Budget nudges in agent loops"; default = "true";
    env = Some "CARAVAN_NUDGE" };
  { key = "tool_call_mode"; kind = Enum ["auto"; "native"; "text"]; scope = New_session;
    doc = "Tool-call recognition"; default = "auto";
    env = Some "CARAVAN_TOOL_CALL_MODE" };
  { key = "require_finish"; kind = Bool; scope = Live;
    doc = "Agent runs must call finish to complete"; default = "true";
    env = Some "CARAVAN_REQUIRE_FINISH" };
  { key = "summarize_model"; kind = Str; scope = Live;
    doc = "Model for compaction summaries"; default = "session model";
    env = Some "CARAVAN_SUMMARIZE_MODEL" };
  { key = "tool_profile"; kind = Enum ["auto"; "core"; "full"]; scope = New_session;
    doc = "Tool surface exposed to the model"; default = "auto";
    env = Some "CARAVAN_TOOL_PROFILE" };
  { key = "permissions"; kind = Enum ["auto"; "ask"; "readonly"]; scope = Live;
    doc = "Mutating-tool policy"; default = "auto";
    env = Some "CARAVAN_PERMISSIONS" };
  { key = "provider_retry"; kind = Enum ["off"; "low"; "medium"; "high"];
    scope = New_session; doc = "Provider error retry aggression"; default = "medium";
    env = Some "CARAVAN_PROVIDER_RETRY" };
  { key = "provider_retry_base_delay"; kind = Float (0.0, 30.0); scope = New_session;
    doc = "Base backoff seconds between provider retries"; default = "0.5";
    env = Some "CARAVAN_PROVIDER_RETRY_BASE_DELAY" };
  { key = "transcript"; kind = Bool; scope = Restart;
    doc = "JSONL session logs"; default = "true";
    env = Some "CARAVAN_TRANSCRIPT" };
  { key = "strict_mode"; kind = Int (0, 2); scope = Live;
    doc = "bash tool discipline"; default = "0";
    env = Some "CARAVAN_STRICT_MODE" };
  { key = "enable_subagents"; kind = Bool; scope = Restart;
    doc = "Offer the delegate tool when [[subagents]] exist"; default = "true";
    env = Some "CARAVAN_SUBAGENTS" };
  { key = "verbose"; kind = Bool; scope = Live;
    doc = "Verbose tool call & trace output"; default = "false";
    env = Some "CARAVAN_VERBOSE" };
]

let find_setting key = List.find_opt (fun s -> s.key = key) settings

(** The "accepts" column shown by `/config keys` and the web form. *)
let accepts_of_kind = function
  | Bool           -> "true | false"
  | Int (lo, hi)   -> Printf.sprintf "integer %d-%d" lo hi
  | Float (lo, hi) -> Printf.sprintf "number %g-%g" lo hi
  | Enum vs        -> String.concat " | " vs
  | Str            -> "text"

let scope_note = function
  | Live        -> "applies immediately"
  | New_session -> "applies to new sessions"
  | Restart     -> "applies on restart"

(** Display triple kept for the surfaces that only render a table.
    Derived from [settings] — never edited on its own. *)
let editable_keys : (string * string * string) list =
  List.map (fun s -> (s.key, s.doc, accepts_of_kind s.kind)) settings

(** Parse [raw] against a setting's kind.  Errors are addressed to whoever
    typed the value, not to a log. *)
let validate_value s raw : (Otoml.t, string) result =
  let raw = String.trim raw in
  match s.kind with
  | Bool ->
    (match String.lowercase_ascii raw with
     | "true"  | "1" | "yes" | "on"  -> Ok (Otoml.boolean true)
     | "false" | "0" | "no"  | "off" -> Ok (Otoml.boolean false)
     | _ -> Error (Printf.sprintf "%s expects true or false (got %s)" s.key raw))
  | Int (lo, hi) ->
    (match int_of_string_opt raw with
     | Some i when i >= lo && i <= hi -> Ok (Otoml.integer i)
     | Some i -> Error (Printf.sprintf "%s must be between %d and %d (got %d)" s.key lo hi i)
     | None   -> Error (Printf.sprintf "%s expects a whole number (got %s)" s.key raw))
  | Float (lo, hi) ->
    (match float_of_string_opt raw with
     | Some f when f >= lo && f <= hi -> Ok (Otoml.float f)
     | Some f -> Error (Printf.sprintf "%s must be between %g and %g (got %g)" s.key lo hi f)
     | None   -> Error (Printf.sprintf "%s expects a number (got %s)" s.key raw))
  | Enum vs ->
    let low = String.lowercase_ascii raw in
    (match List.find_opt (fun v -> String.lowercase_ascii v = low) vs with
     | Some v -> Ok (Otoml.string v)
     | None ->
       Error (Printf.sprintf "%s expects one of: %s (got %s)"
                s.key (String.concat ", " vs) raw))
  | Str ->
    if raw = "" then
      Error (Printf.sprintf "%s expects a value (use 'unset %s' to clear it)" s.key s.key)
    else Ok (Otoml.string raw)   (* verbatim: a model named "3" is a string *)

(** Levenshtein distance, used only to turn a typo into a suggestion. *)
let edit_distance a b =
  let la = String.length a and lb = String.length b in
  if la = 0 then lb else if lb = 0 then la
  else begin
    let prev = Array.init (lb + 1) (fun j -> j) in
    let cur = Array.make (lb + 1) 0 in
    for i = 1 to la do
      cur.(0) <- i;
      for j = 1 to lb do
        let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
        cur.(j) <- min (min (cur.(j - 1) + 1) (prev.(j) + 1)) (prev.(j - 1) + cost)
      done;
      Array.blit cur 0 prev 0 (lb + 1)
    done;
    prev.(lb)
  end

(** The closest known setting to a misspelling, if one is close enough. *)
let suggest_key key =
  let scored = List.map (fun s -> (edit_distance key s.key, s.key)) settings in
  match List.sort compare scored with
  | (d, k) :: _ when d <= 3 && d < String.length k -> Some k
  | _ -> None

(** Sections that hold user-chosen names rather than fixed settings, so a
    dotted write into them is legitimate even though the leaf is not in
    [settings]. *)
let open_sections = ["api_keys"; "providers"; "spinner"; "orchestrator"; "capabilities"]

(** Validate a write against the schema, then apply it.

    An unknown bare key is refused with a suggestion.  Previously the typo
    was written happily and every later read fell back to the default —
    the setting simply never took effect, with nothing to show why. *)
let set_checked dotted_key raw : (string, string) result =
  match split_dotted dotted_key with
  | [] -> Error "empty key"
  | [bare] ->
    (match find_setting bare with
     | Some s -> Result.bind (validate_value s raw) (set_toml_value bare)
     | None ->
       Error (Printf.sprintf "unknown setting '%s'%s" bare
                (match suggest_key bare with
                 | Some k -> Printf.sprintf " — did you mean '%s'?" k
                 | None   -> " (see /config keys for the list)")))
  | section :: _ as parts ->
    if not (List.mem section open_sections) then
      Error (Printf.sprintf "unknown section '%s' (see /config keys for the list)" section)
    else if section = "api_keys" then
      (* Keys are stored verbatim: a numeric-looking key is still a string. *)
      set_toml_value dotted_key (Otoml.string raw)
    else
      match find_setting (List.nth parts (List.length parts - 1)) with
      | Some s -> Result.bind (validate_value s raw) (set_toml_value dotted_key)
      | None   -> set_toml_value dotted_key (toml_value_of_string raw)

(** Clear a setting so it falls back to its default. *)
let unset_checked dotted_key : (string, string) result =
  match split_dotted dotted_key with
  | [] -> Error "empty key"
  | [bare] when find_setting bare = None ->
    Error (Printf.sprintf "unknown setting '%s'%s" bare
             (match suggest_key bare with
              | Some k -> Printf.sprintf " — did you mean '%s'?" k
              | None   -> " (see /config keys for the list)"))
  | _ -> unset_toml_value dotted_key

(** [Some (var, value)] when an environment variable currently overrides
    [key].  This is the usual answer to "I saved it, why is it not
    taking effect?". *)
let env_shadow key =
  match find_setting key with
  | Some { env = Some var; _ } ->
    (match Sys.getenv_opt var with
     | Some v when v <> "" -> Some (var, v)
     | _ -> None)
  | _ -> None

(** The TOML type of a stored value, for diagnostics. *)
let toml_type_name (v : Otoml.t) =
  match v with
  | Otoml.TomlString _ -> "string"       | Otoml.TomlInteger _ -> "integer"
  | Otoml.TomlFloat _ -> "float"         | Otoml.TomlBoolean _ -> "boolean"
  | Otoml.TomlArray _ -> "array"         | Otoml.TomlTable _ -> "table"
  | Otoml.TomlInlineTable _ -> "inline table"
  | Otoml.TomlTableArray _ -> "array of tables"
  | _ -> "value"

(** Top-level keys in the file that no setting describes — typos, or
    settings that have been renamed. *)
let unknown_keys () : string list =
  match get_ast () with
  | None -> []
  | Some (Otoml.TomlTable fields) ->
    List.filter_map (fun (k, v) ->
      match v with
      | Otoml.TomlTable _ | Otoml.TomlInlineTable _ | Otoml.TomlTableArray _ -> None
      | _ -> if find_setting k = None then Some k else None) fields
  | Some _ -> []

(** Stored values whose TOML type the schema cannot accept, with an
    explanation.  [model = 3] is the motivating case: legal TOML, read as
    nothing, silently replaced by the default. *)
let mistyped_keys () : (string * string) list =
  let check_scalar s (v : Otoml.t) =
    let bad expected =
      let got = toml_type_name v in
      let article = if String.contains "aeiou" got.[0] then "an" else "a" in
      Some (Printf.sprintf "is %s %s; %s expects %s" article got s.key expected)
    in
    match s.kind, v with
    | Bool, Otoml.TomlBoolean _ -> None
    | Bool, _ -> bad "true or false"
    | Int _, Otoml.TomlInteger _ -> None
    | Int _, _ -> bad "a whole number"
    | Float _, (Otoml.TomlFloat _ | Otoml.TomlInteger _) -> None
    | Float _, _ -> bad "a number"
    | (Str | Enum _), Otoml.TomlString _ -> None
    | (Str | Enum _), _ -> bad "text"
  in
  let of_table fields =
    List.filter_map (fun (k, v) ->
      match find_setting k with
      | None -> None
      | Some s ->
        match check_scalar s v with
        | Some why -> Some (k, why)
        | None ->
          (* An enum also has to name one of its values. *)
          match s.kind, v with
          | Enum vs, Otoml.TomlString got
            when not (List.exists (fun x ->
                        String.lowercase_ascii x = String.lowercase_ascii got) vs) ->
            Some (k, Printf.sprintf "is \"%s\"; %s expects one of: %s"
                       got s.key (String.concat ", " vs))
          | _ -> None) fields
  in
  match get_ast () with
  | None -> []
  | Some (Otoml.TomlTable fields) ->
    let top = of_table fields in
    let orch =
      match List.assoc_opt "orchestrator" fields with
      | Some (Otoml.TomlTable f) | Some (Otoml.TomlInlineTable f) ->
        List.map (fun (k, why) -> ("orchestrator." ^ k, why)) (of_table f)
      | _ -> []
    in
    top @ orch
  | Some _ -> []

(** Write an [orchestrator] table when there is not already one to
    resolve against.  Declaring a subagent is the only thing that needs
    it; reading the config resolves the same pair in memory. *)
let ensure_orchestrator_written ?fallback_provider ?fallback_model () =
  match get_orchestrator () with
  | Some _ -> ()
  | None ->
    let pick explicit fallback =
      match explicit with
      | Some v when String.trim v <> "" -> Some v
      | _ -> fallback
    in
    (match pick (get_string "provider") fallback_provider,
           pick (get_string "model") fallback_model with
     | Some p, Some m ->
       ignore (set_toml_value "orchestrator.provider" (Otoml.string p));
       ignore (set_toml_value "orchestrator.model" (Otoml.string m))
     | _ -> ())

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
        (* Each optional field is looked up, gated on non-empty, then
           converted to a TOML value; a conversion failure (e.g. an
           unparsable number) silently drops the field rather than erroring. *)
        let str_field s = if s = "" then None else Some (Otoml.string s) in
        let int_field s = if s = "" then None else Option.map Otoml.integer (int_of_string_opt s) in
        let float_field s = if s = "" then None else Option.map Otoml.float (float_of_string_opt s) in
        let tools_field s =
          if s = "" then None
          else
            let tool_list =
              String.split_on_char ',' s
              |> List.map String.trim
              |> List.filter (fun s -> s <> "")
              |> List.map Otoml.string
            in
            if tool_list = [] then None else Some (Otoml.TomlArray tool_list)
        in
        let add_field key convert =
          match lookup key with
          | Some v -> (match convert v with
              | Some t -> pairs := !pairs @ [(key, t)]
              | None -> ())
          | None -> ()
        in
        add_field "system_prompt" str_field;
        add_field "tools" tools_field;
        add_field "role" str_field;
        add_field "realm" str_field;
        add_field "max_tokens" int_field;
        add_field "temperature" float_field;
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
          let written =
            match append_table_array ~table:"subagents" !pairs with
            | Some text -> write_config_text text
            | None ->
              (* Fall back to the AST printer, which loses comments. *)
              let new_arr = Otoml.TomlTableArray (existing @ [entry]) in
              write_ast (Otoml.update ast ["subagents"] (Some new_arr))
          in
          (* A roster needs an orchestrator to resolve against. *)
          (match written with
           | Ok _ -> ensure_orchestrator_written ~fallback_provider:provider ~fallback_model:model ()
           | Error _ -> ());
          written
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
    else
      match remove_table_array ~table:"subagents" ~name with
      | Some text -> write_config_text text
      | None ->
        let value = if filtered = [] then None else Some (Otoml.TomlTableArray filtered) in
        write_ast (Otoml.update ast ["subagents"] value)
  with exn -> Error (Printexc.to_string exn)

(** Serialize a [subagent_config] to a JSON object for the web API. *)
let subagent_to_json (cfg : subagent_config) : Yojson.Safe.t =
  yojson_of_subagent_config cfg

