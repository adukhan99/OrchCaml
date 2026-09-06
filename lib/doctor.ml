type severity = Pass | Warn | Fail

type check = {
  label    : string;
  severity : severity;
  message  : string;
  hint     : string option;
}

type provider_kind = Local | Cloud

type provider_info = {
  name          : string;
  kind          : provider_kind;
  base_url      : string;
  requires_key  : bool;
  key_env       : string option;
}

let run_checks ~find_provider ~api_key_for ~list_models ~subagents_roster ~subagents_enabled () =
  let checks = ref [] in
  let add ~label ~severity ~message ?hint () =
    checks := { label; severity; message; hint } :: !checks
  in

  (* 1. Config file: parses, is private, and says what the schema means *)
  let path = Config.config_path () in
  if Sys.file_exists path then begin
    (match Config.parse_check () with
     | Ok () ->
       add ~label:"Config file" ~severity:Pass
         ~message:(Printf.sprintf "Config file valid TOML (%s)" path) ()
     | Error e ->
       add ~label:"Config file" ~severity:Fail
         ~message:(Printf.sprintf "Config file has TOML syntax errors (%s)" path)
         ~hint:e ());
    (try
       let st = Unix.stat path in
       if st.Unix.st_perm land 0o077 <> 0 then
         add ~label:"Config permissions" ~severity:Warn ~message:(Printf.sprintf "Config is group/world-readable") ~hint:(Printf.sprintf "consider: chmod 600 %s" path) ()
     with _ -> ());

    (* A key no setting describes is a typo that will never take effect. *)
    List.iter (fun key ->
      add ~label:"Config key" ~severity:Warn
        ~message:(Printf.sprintf "'%s' is not a Caravan setting — it is ignored" key)
        ~hint:(match Config.suggest_key key with
               | Some s -> Printf.sprintf "did you mean '%s'?  (caravan config keys)" s
               | None -> "caravan config keys lists every setting") ()
    ) (Config.unknown_keys ());

    (* A value of the wrong TOML type reads back as nothing, so the
       setting silently falls back to its default. *)
    List.iter (fun (key, why) ->
      add ~label:"Config value" ~severity:Fail
        ~message:(Printf.sprintf "%s %s" key why)
        ~hint:(Printf.sprintf "caravan config set %s <value>" key) ()
    ) (Config.mistyped_keys ());

    (* An environment variable beats the file, which is the usual reason a
       saved setting appears to do nothing. *)
    List.iter (fun (s : Config.setting) ->
      match Config.env_shadow s.Config.key with
      | Some (var, v) when Config.get_string s.Config.key <> None
                        || Config.get_int s.Config.key <> None
                        || Config.get_bool s.Config.key <> None ->
        add ~label:"Config override" ~severity:Warn
          ~message:(Printf.sprintf "%s=%s overrides '%s' from the config file" var v s.Config.key)
          ~hint:(Printf.sprintf "unset %s to use the saved value" var) ()
      | _ -> ()
    ) Config.settings
  end else
    add ~label:"Config file" ~severity:Warn ~message:(Printf.sprintf "No config file at %s" path) ~hint:"run 'caravan init'" ();

  (* 2. Provider *)
  let provider_name =
    Config.get_string_opt (Some "CARAVAN_PROVIDER") "provider"
    |> Option.value ~default:"ollama"
  in
  (match find_provider provider_name with
   | None ->
     add ~label:"Provider" ~severity:Fail ~message:(Printf.sprintf "Provider '%s' unknown." provider_name) ~hint:"Check spelling or registry" ()
   | Some e ->
     add ~label:"Provider" ~severity:Pass ~message:(Printf.sprintf "Provider '%s' supported" e.name) ();
     let base_url = Config.get_string_opt (Some "CARAVAN_BASE_URL") "base_url" in
     
     if e.requires_key then begin
       match api_key_for e with
       | Some _ -> add ~label:"API Key" ~severity:Pass ~message:(Printf.sprintf "API key for %s found" e.name) ()
       | None ->
         add ~label:"API Key" ~severity:Fail ~message:(Printf.sprintf "API key for %s missing" e.name)
             ~hint:(Printf.sprintf "set %s or [api_keys] %s in config" (Option.value ~default:"its env var" e.key_env) e.name) ()
     end;
     
     match e.kind with
     | Local ->
       let url = Option.value ~default:e.base_url base_url in
       (try
          let models = list_models e base_url in
          add ~label:"Endpoint" ~severity:Pass ~message:(Printf.sprintf "%s reachable at %s (%d models)" e.name url (List.length models)) ()
        with exn ->
          add ~label:"Endpoint" ~severity:Fail ~message:(Printf.sprintf "Could not reach %s at %s" e.name url)
              ~hint:(Caravan_error.humanize exn) ())
     | Cloud -> ());

  (* 3. Transcript dir *)
  if Config.get_transcript_enabled () then begin
    let dir = Config.log_dir () in
    (try
       Config.mkdir_p dir;
       add ~label:"Transcript" ~severity:Pass ~message:(Printf.sprintf "Transcript directory writable (%s)" dir) ()
     with _ -> add ~label:"Transcript" ~severity:Warn ~message:(Printf.sprintf "Cannot create transcript directory %s" dir) ())
  end;

  (* 4. Subagents *)
  if subagents_roster <> [] then begin
    if not subagents_enabled then
      add ~label:"Subagents" ~severity:Warn ~message:(Printf.sprintf "%d subagent(s) configured but enable_subagents = false" (List.length subagents_roster)) ();
    List.iter (fun ((cfg : Config.subagent_config), status) ->
      if String.length status >= 10 && String.sub status 0 10 = "UNRESOLVED" then
        add ~label:(Printf.sprintf "Subagent '%s'" cfg.name) ~severity:Fail
            ~message:(Printf.sprintf "provider '%s' unresolved" cfg.provider_ref)
            ~hint:(Printf.sprintf "no [providers.%s] table, not in registry" cfg.provider_ref) ()
      else if Re.execp (Re.compile (Re.str "unset")) status then
        add ~label:(Printf.sprintf "Subagent '%s'" cfg.name) ~severity:Warn ~message:status ()
      else
        add ~label:(Printf.sprintf "Subagent '%s'" cfg.name) ~severity:Pass
            ~message:(Printf.sprintf "mapped to %s via %s" cfg.model status) ()
    ) subagents_roster
  end;

  (* 5. MCP servers *)
  let mcp_servers = Config.get_mcp_servers () in
  List.iter (fun (srv : Config.mcp_server_config) ->
    let cmd_ok = Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" (Filename.quote srv.command)) = 0 in
    if cmd_ok then
      add ~label:(Printf.sprintf "MCP '%s'" srv.name) ~severity:Pass ~message:(Printf.sprintf "command '%s' found" srv.command) ()
    else
      add ~label:(Printf.sprintf "MCP '%s'" srv.name) ~severity:Fail ~message:(Printf.sprintf "command '%s' not in PATH" srv.command) ()
  ) mcp_servers;

  List.rev !checks
