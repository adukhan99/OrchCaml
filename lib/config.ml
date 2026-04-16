(** OrchCaml.Config — Centralized TOML configuration reader.

    Parses ~/.orchcaml/config.toml to fetch default values for CLI arguments
    (base_url, system prompt, model, provider, API keys).
*)

let config_path = Filename.concat (Sys.getenv "HOME") ".orchcaml/config.toml"

let load_toml () =
  if Sys.file_exists config_path then
    try Some (Otoml.Parser.from_file config_path)
    with exn ->
      Printf.eprintf "[OrchCaml] Warning: Failed to parse %s: %s\n%!"
        config_path (Printexc.to_string exn);
      None
  else None

let toml_ast = lazy (load_toml ())

let get_string key =
  match Lazy.force toml_ast with
  | None -> None
  | Some ast ->
    try Some (Otoml.find ast Otoml.get_string [key])
    with _ -> None

let get_string_opt env_var toml_key =
  match env_var with
  | Some e -> (match Sys.getenv_opt e with
               | Some v when v <> "" -> Some v
               | _ -> get_string toml_key)
  | None -> get_string toml_key
