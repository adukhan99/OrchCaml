(** OrchCaml.Providers.Ollama — Ollama local LLM backend.

    Talks to the Ollama REST API at [http://localhost:11434] (configurable).
    Supports both non-streaming [complete] and streaming [stream] calls.

    API reference: https://github.com/ollama/ollama/blob/main/docs/api.md
*)

open OrchCaml.Types
open OrchCaml.Provider

(* ------------------------------------------------------------------
   Config type — defined outside the functor so make_config is accessible
   ------------------------------------------------------------------ *)

type config = {
  host    : string;
  port    : int;
  model   : string;
  options : gen_options;
  timeout : float;
}

let make_config
    ?(host    = "localhost")
    ?(port    = 11434)
    ?(options = default_options)
    ?(timeout = 120.)
    ~model
    () =
  { host; port; model; options; timeout }

(* ------------------------------------------------------------------
   Helpers (shared between complete and stream)
   ------------------------------------------------------------------ *)

let base_url cfg =
  Printf.sprintf "http://%s:%d" cfg.host cfg.port

let options_to_json (o : gen_options) =
  let opt key f = function None -> [] | Some v -> [(key, f v)] in
  `Assoc (List.concat [
    opt "temperature"  (fun v -> `Float v) o.temperature;
    opt "top_p"        (fun v -> `Float v) o.top_p;
    opt "top_k"        (fun v -> `Int v)   o.top_k;
    opt "num_predict"  (fun v -> `Int v)   o.max_tokens;
    opt "seed"         (fun v -> `Int v)   o.seed;
    (if o.stop = [] then []
     else [("stop", `List (List.map (fun s -> `String s) o.stop))]);
  ])

let make_body cfg msgs ~stream =
  `Assoc [
    ("model",    `String cfg.model);
    ("messages", messages_to_json msgs);
    ("stream",   `Bool stream);
    ("options",  options_to_json cfg.options);
  ]

let post_json url body =
  let open Lwt.Syntax in
  let uri = Uri.of_string url in
  let body_str = Yojson.Safe.to_string body in
  let headers = Cohttp.Header.of_list [
    ("Content-Type", "application/json");
    ("Accept",       "application/json");
  ] in
  let* (resp, body_lwt) =
    Cohttp_lwt_unix.Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body_str) uri in
  let status = Cohttp.Response.status resp in
  let* body_str = Cohttp_lwt.Body.to_string body_lwt in
  Lwt.return (status, body_str)

let parse_complete_response body_str model =
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let content = json |> member "message" |> member "content" |> to_string in
  let finish   = json |> member "done_reason" |> to_string_option in
  wrap_result ~raw_response:body_str ~model ~provider:"ollama" ?finish_reason:finish content

(* ------------------------------------------------------------------
   PROVIDER module — constrained to the signature after definition
   ------------------------------------------------------------------ *)

module Ollama = struct
  type nonrec config = config

  let name = "ollama"

  let complete cfg msgs =
    let open Lwt.Syntax in
    let url  = base_url cfg ^ "/api/chat" in
    let body = make_body cfg msgs ~stream:false in
    let* (status, resp_body) = post_json url body in
    if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
      Lwt.return (parse_complete_response resp_body cfg.model)
    else
      Lwt.fail_with (Printf.sprintf "Ollama error %s: %s"
        (Cohttp.Code.string_of_status status) resp_body)

  let stream cfg msgs =
    let url      = base_url cfg ^ "/api/chat" in
    let body_str = Yojson.Safe.to_string (make_body cfg msgs ~stream:true) in
    let uri      = Uri.of_string url in
    let headers  = Cohttp.Header.of_list [("Content-Type", "application/json")] in
    let token_stream, token_push = Lwt_stream.create () in
    let safe_push v = try token_push v with Lwt_stream.Closed -> () in
    let meta_promise, meta_resolver = Lwt.wait () in
    let buf = Buffer.create 4096 in
    let run () =
      let open Lwt.Syntax in
      let* (resp, body_lwt) =
        Cohttp_lwt_unix.Client.post
          ~headers ~body:(Cohttp_lwt.Body.of_string body_str) uri
      in
      let status = Cohttp.Response.status resp in
      if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then begin
        let* err = Cohttp_lwt.Body.to_string body_lwt in
        token_push None;
        Lwt.wakeup_exn meta_resolver
          (Failure (Printf.sprintf "Ollama stream error %s: %s"
            (Cohttp.Code.string_of_status status) err));
        Lwt.return_unit
      end else begin
        let body_stream = Cohttp_lwt.Body.to_stream body_lwt in
        let* () = Lwt_stream.iter (fun chunk ->
          let lines = String.split_on_char '\n' chunk in
          List.iter (fun line ->
            let line = String.trim line in
            if line <> "" then begin
              (try
                let json = Yojson.Safe.from_string line in
                let open Yojson.Safe.Util in
                let token = json |> member "message" |> member "content" |> to_string in
                Buffer.add_string buf token;
                safe_push (Some token);
                let done_ = json |> member "done" |> to_bool in
                if done_ then begin
                  safe_push None;
                  let full   = Buffer.contents buf in
                  let finish = json |> member "done_reason" |> to_string_option in
                  Lwt.wakeup meta_resolver
                    (wrap_result ~raw_response:full ~model:cfg.model
                       ~provider:"ollama" ?finish_reason:finish full)
                end
              with _ -> ())
            end
          ) lines
        ) body_stream in
        safe_push None;
        (if Lwt.is_sleeping meta_promise then
          Lwt.wakeup meta_resolver
            (wrap_result ~raw_response:(Buffer.contents buf)
               ~model:cfg.model ~provider:"ollama"
               (Buffer.contents buf)));
        Lwt.return_unit
      end
    in
    Lwt.async run;
    (token_stream, meta_promise)

  let list_models cfg =
    let open Lwt.Syntax in
    let url = base_url cfg ^ "/api/tags" in
    let uri = Uri.of_string url in
    let* (_resp, body) = Cohttp_lwt_unix.Client.get uri in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Safe.from_string body_str in
    let open Yojson.Safe.Util in
    let models =
      json |> member "models" |> to_list
      |> List.map (fun m -> m |> member "name" |> to_string)
    in
    Lwt.return models

end

(** Convenience constructor: pack an Ollama provider. *)
let make_provider ?(host="localhost") ?(port=11434)
    ?(options=OrchCaml.Types.default_options) ?(timeout=120.) ~model () =
  let cfg = make_config ~host ~port ~options ~timeout ~model () in
  Provider ((module Ollama), cfg)

(** Expose as a PROVIDER-signed module for explicit use. *)
let provider : (module PROVIDER with type config = config) = (module Ollama)
