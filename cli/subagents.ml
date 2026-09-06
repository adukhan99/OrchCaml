(** Wire `[[subagents]]` config tables into a live `delegate` tool.

    Until now subagents were library-only (see the swarm examples); this
    module makes them a first-class CLI feature:

    - declared in `~/.caravan/config.toml` as `[[subagents]]` tables,
      with providers resolved from `[providers.<name>]` sections or the
      built-in registry;
    - gated by `enable_subagents` (default: on when tables exist) — no
      tables, no tool, zero surface;
    - governed like everything else: `delegate` is classed as a mutating
      tool (so `ask`/`readonly` permission modes apply), and every
      delegation plus each worker's own tool calls flow through [Trace]
      into the session transcript;
    - misconfigured entries degrade to warnings, never startup failures. *)

open Caravan

let enabled () =
  Config.get_bool_opt (Some "CARAVAN_SUBAGENTS") "enable_subagents"
  |> Option.value ~default:true

(** Resolve one [[subagents]] entry to a runnable spec.
    Returns [Error reason] when the entry can't be honoured. *)
let build_spec ~(registered_tools : Tool.packed_tool list)
    (cfg : Config.subagent_config) : (Subagent.subagent_spec, string) result =
  (* Provider: a [providers.<ref>] table wins; else the registry. *)
  let provider_result =
    match Config.get_provider_config cfg.provider_ref with
    | Some pc ->
      let api_key =
        Option.bind pc.api_key_env (fun ev ->
          match Sys.getenv_opt ev with
          | Some v when v <> "" -> Some v
          | _ -> None)
      in
      let options =
        Types.options ?temperature:cfg.temperature ?max_tokens:cfg.max_tokens ()
      in
      Ok (CaravanProviders.Openai_compatible.make_provider
            ~provider_name:cfg.provider_ref
            ~base_url:pc.base_url
            ~options
            ?api_key
            ~model:cfg.model
            ())
    | None ->
      (try Ok (CaravanProviders.Registry.make_provider
                 ~model:cfg.model cfg.provider_ref)
       with CaravanProviders.Registry.Unknown_provider _ ->
         Error (Printf.sprintf
                  "subagent '%s': provider '%s' is neither a [providers.%s] \
                   table nor a registry name"
                  cfg.name cfg.provider_ref cfg.provider_ref))
  in
  match provider_result with
  | Error _ as e -> e
  | Ok provider ->
    (* Tools: resolve each requested name; unknown names are dropped with
       a warning rather than aborting startup. *)
    let tools, missing =
      List.fold_left (fun (acc, missing) name ->
        match Tool.find_tool registered_tools name with
        | Some t -> (t :: acc, missing)
        | None -> (acc, name :: missing)
      ) ([], []) cfg.tool_names
    in
    List.iter (fun name ->
      Trace.log "warn" "subagent '%s': tool '%s' not found — dropped" cfg.name name
    ) (List.rev missing);
    (* Workers always get finish so they can signal completion. *)
    let tools =
      if List.exists (fun t -> Tool.name_of_packed t = "finish") tools then tools
      else match Tool.find_tool registered_tools "finish" with
        | Some f -> f :: tools
        | None -> tools
    in
    Ok {
      Subagent.name  = cfg.name;
      role           = cfg.worker_role;
      system_prompt  = cfg.system_prompt;
      tools          = List.rev tools;
      provider       = Some provider;
      model          = Some cfg.model;
    }

(** Dispatch-time sandbox tools: a worker declared with [realm = "r"]
    resolves the plugin toolset of realm [r] at every delegation, so
    plugins registered into the realm add (or withdraw) worker-only
    tools without rebuilding the delegate. *)
let live_tools ~host (configs : Config.subagent_config list) =
  let realm_of =
    List.filter_map
      (fun (cfg : Config.subagent_config) ->
        Option.map (fun r -> (cfg.name, r)) cfg.realm)
      configs
  in
  fun worker ->
    match List.assoc_opt worker realm_of with
    | Some realm -> Plugin_host.realm_tools host ~realm
    | None -> []

(** Build the delegate tool from config, if subagents are declared and
    enabled. Returns [None] (and logs why) otherwise. *)
let delegate_tool ~net ~clock ~host ~(registered_tools : Tool.packed_tool list) () =
  let configs = Config.get_subagents () in
  if configs = [] then None
  else if not (enabled ()) then begin
    Trace.log "info"
      "subagents: %d configured but enable_subagents is false — delegate tool not offered"
      (List.length configs);
    None
  end else begin
    let specs =
      List.filter_map (fun cfg ->
        match build_spec ~registered_tools cfg with
        | Ok spec -> Some spec
        | Error reason -> Trace.log "warn" "subagents: %s — skipped" reason; None
      ) configs
    in
    if specs = [] then begin
      Trace.log "warn" "subagents: none of the %d configured entries were usable"
        (List.length configs);
      None
    end else begin
      Trace.log "info" "subagents: delegate tool enabled with %d worker(s): %s"
        (List.length specs)
        (String.concat ", " (List.map (fun (s : Subagent.subagent_spec) -> s.name) specs));
      Some (CaravanTools.Delegate.make ~net ~clock ~registered_tools
              ~live_tools:(live_tools ~host configs)
              ~subagent_specs:specs ())
    end
  end

(** Session tool set: static + MCP tools plus the delegate tool when
    configured. Call inside [Eio_main.run]. *)
let session_tools ~net ~clock ~host base_tools =
  match delegate_tool ~net ~clock ~host ~registered_tools:base_tools () with
  | Some d -> base_tools @ [d]
  | None -> base_tools

(** Human-readable roster for the /subagents command and doctor. *)
let describe () =
  let configs = Config.get_subagents () in
  List.map (fun (cfg : Config.subagent_config) ->
    let provider_status =
      match Config.get_provider_config cfg.provider_ref with
      | Some pc ->
        let key_ok = match pc.api_key_env with
          | None -> true
          | Some ev -> (match Sys.getenv_opt ev with Some v -> v <> "" | None -> false)
        in
        if key_ok then Printf.sprintf "[providers.%s]" cfg.provider_ref
        else Printf.sprintf "[providers.%s] (key env %s unset)"
               cfg.provider_ref (Option.value ~default:"?" pc.api_key_env)
      | None ->
        (match CaravanProviders.Registry.find cfg.provider_ref with
         | Some _ -> Printf.sprintf "registry:%s" cfg.provider_ref
         | None -> "UNRESOLVED provider")
    in
    (cfg, provider_status)
  ) configs
