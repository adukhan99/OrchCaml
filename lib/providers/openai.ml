(** OrchCaml.Providers.Openai — OpenAI-compatible LLM backend.

    Works with any OpenAI-compatible API (OpenAI, Groq, Together,
    Mistral, local OpenAI-proxy, etc.) by setting [base_url].

    API keys are read from the environment or config file.
    Look for [(* API_KEY_SOURCE *)] comments to change the source.
*)

open OrchCaml.Types
open OrchCaml.Provider
open OrchCaml.Config

(* ------------------------------------------------------------------
   API key loading
   (* API_KEY_SOURCE: modify load_api_key_from_env to change key sources *)
   ------------------------------------------------------------------ *)

let load_api_key_from_env () =
  match get_string_opt (Some "OPENAI_API_KEY") "openai_api_key" with
  | Some k -> k
  | None -> failwith "OPENAI_API_KEY not found in env or ~/.orchcaml/config.toml"

(** (* API_KEY_SOURCE: env OPENAI_ORG_ID *) *)
let load_org_id () = Sys.getenv_opt "OPENAI_ORG_ID"

(* ------------------------------------------------------------------
   Config type
   ------------------------------------------------------------------ *)

type config = {
  base_url : string;
  api_key  : string;   (** (* API_KEY_SOURCE *) *)
  model    : string;
  options  : gen_options;
  org_id   : string option;  (** (* API_KEY_SOURCE: env OPENAI_ORG_ID *) *)
}

let make_config
    ?(base_url = "https://api.openai.com/v1")
    ?(options  = default_options)
    ?api_key
    ~model
    () =
  let api_key = match api_key with
    | Some k -> k
    | None   -> load_api_key_from_env ()
  in
  { base_url; api_key; model; options; org_id = load_org_id () }

(* ------------------------------------------------------------------
   Helpers
   ------------------------------------------------------------------ *)

let options_to_json_fields (o : gen_options) =
  let opt key f = function None -> [] | Some v -> [(key, f v)] in
  List.concat [
    opt "temperature"  (fun v -> `Float v) o.temperature;
    opt "top_p"        (fun v -> `Float v) o.top_p;
    opt "max_tokens"   (fun v -> `Int v)   o.max_tokens;
    opt "seed"         (fun v -> `Int v)   o.seed;
    (if o.stop = [] then []
     else [("stop", `List (List.map (fun s -> `String s) o.stop))]);
  ]

let msg_to_openai_json msg =
  `Assoc [
    ("role",    `String (role_to_string msg.role));
    ("content", `String msg.content);
  ]

let make_body cfg msgs ~stream =
  `Assoc (List.concat [
    [
      ("model",    `String cfg.model);
      ("messages", `List (List.map msg_to_openai_json msgs));
      ("stream",   `Bool stream);
    ];
    options_to_json_fields cfg.options;
  ])

let auth_headers cfg =
  let h = [
    ("Content-Type",  "application/json");
    ("Authorization", "Bearer " ^ cfg.api_key);
  ] in
  match cfg.org_id with
  | None    -> h
  | Some id -> ("OpenAI-Organization", id) :: h

let parse_complete_response body_str model =
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let choice  = json |> member "choices" |> index 0 in
  let content = choice |> member "message" |> member "content" |> to_string in
  let finish  = choice |> member "finish_reason" |> to_string_option in
  wrap_result ~raw_response:body_str ~model ~provider:"openai" ?finish_reason:finish content

(* ------------------------------------------------------------------
   PROVIDER module
   ------------------------------------------------------------------ *)

module Openai = struct
  type nonrec config = config

  let name = "openai"

  let complete cfg msgs =
    let open Lwt.Syntax in
    let uri  = Uri.of_string (cfg.base_url ^ "/chat/completions") in
    let hdrs = Cohttp.Header.of_list (auth_headers cfg) in
    let body = Cohttp_lwt.Body.of_string
      (Yojson.Safe.to_string (make_body cfg msgs ~stream:false)) in
    let* (resp, body_lwt) = Cohttp_lwt_unix.Client.post ~headers:hdrs ~body uri in
    let status = Cohttp.Response.status resp in
    let* body_str = Cohttp_lwt.Body.to_string body_lwt in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Lwt.return (parse_complete_response body_str cfg.model)
    else
      Lwt.fail_with (Printf.sprintf "OpenAI error %s: %s"
        (Cohttp.Code.string_of_status status) body_str)

  let stream cfg msgs =
    let uri      = Uri.of_string (cfg.base_url ^ "/chat/completions") in
    let hdrs     = Cohttp.Header.of_list
      (("Accept", "text/event-stream") :: auth_headers cfg) in
    let body_str = Yojson.Safe.to_string (make_body cfg msgs ~stream:true) in
    let token_stream, token_push = Lwt_stream.create () in
    let safe_push v = try token_push v with Lwt_stream.Closed -> () in
    let meta_promise, meta_resolver = Lwt.wait () in
    let buf = Buffer.create 4096 in
    let run () =
      let open Lwt.Syntax in
      let* (resp, body_lwt) =
        Cohttp_lwt_unix.Client.post
          ~headers:hdrs ~body:(Cohttp_lwt.Body.of_string body_str) uri
      in
      let status = Cohttp.Response.status resp in
      if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then begin
        let* err = Cohttp_lwt.Body.to_string body_lwt in
        token_push None;
        Lwt.wakeup_exn meta_resolver
          (Failure (Printf.sprintf "OpenAI stream error %s: %s"
            (Cohttp.Code.string_of_status status) err));
        Lwt.return_unit
      end else begin
        let body_stream = Cohttp_lwt.Body.to_stream body_lwt in
        let* () = Lwt_stream.iter (fun chunk ->
          let lines = String.split_on_char '\n' chunk in
          List.iter (fun line ->
            let line = String.trim line in
            if String.length line > 6 && String.sub line 0 6 = "data: " then begin
              let data = String.sub line 6 (String.length line - 6) in
              if data = "[DONE]" then begin
                safe_push None;
                let full = Buffer.contents buf in
                Lwt.wakeup meta_resolver
                  (wrap_result ~raw_response:full ~model:cfg.model
                     ~provider:"openai" full)
              end else begin
                (try
                  let json = Yojson.Safe.from_string data in
                  let open Yojson.Safe.Util in
                  let delta =
                    json |> member "choices" |> index 0
                    |> member "delta" |> member "content"
                  in
                  (match delta with
                   | `String token ->
                     Buffer.add_string buf token;
                     safe_push (Some token)
                   | _ -> ())
                with exn -> 
                  Printf.eprintf "[OpenAI Stream Parse Error]: %s\nData: %s\n%!" (Printexc.to_string exn) data)
              end
            end
          ) lines
        ) body_stream in
        safe_push None;
        (if Lwt.is_sleeping meta_promise then
          Lwt.wakeup meta_resolver
            (wrap_result ~raw_response:(Buffer.contents buf)
               ~model:cfg.model ~provider:"openai"
               (Buffer.contents buf)));
        Lwt.return_unit
      end
    in
    Lwt.async run;
    (token_stream, meta_promise)

  let list_models cfg =
    let open Lwt.Syntax in
    let uri  = Uri.of_string (cfg.base_url ^ "/models") in
    let hdrs = Cohttp.Header.of_list (auth_headers cfg) in
    Lwt.catch
      (fun () ->
        let* (resp, body_lwt) = Cohttp_lwt_unix.Client.get ~headers:hdrs uri in
        let status = Cohttp.Response.status resp in
        let* body_str = Cohttp_lwt.Body.to_string body_lwt in
        if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
          try
            let json = Yojson.Safe.from_string body_str in
            let open Yojson.Safe.Util in
            let models =
              json |> member "data" |> to_list
              |> List.map (fun m -> m |> member "id" |> to_string)
            in
            Lwt.return models
          with _ ->
            Lwt.fail_with "cannot query remote provider"
        else
          Lwt.fail_with "cannot query remote provider")
      (fun _ -> Lwt.fail_with "cannot query remote provider")

end

(** Convenience constructor: pack an OpenAI provider.
    (* API_KEY_SOURCE: pass ~api_key to override env-var lookup *) *)
let make_provider
    ?(base_url = "https://api.openai.com/v1")
    ?(options  = OrchCaml.Types.default_options)
    ?api_key
    ~model
    () =
  let cfg = make_config ~base_url ~options ?api_key ~model () in
  Provider ((module Openai), cfg)

let provider : (module PROVIDER with type config = config) = (module Openai)
