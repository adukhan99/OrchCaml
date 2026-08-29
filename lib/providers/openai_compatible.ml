(** Unified OpenAI-compatible LLM provider engine.
    Powers OpenAI, llama.cpp, Ollama (/v1), vLLM, and any OpenAI-compatible API. *)

open Caravan.Types
open Caravan.Provider
open Caravan.Tool

type config = {
  provider_name : string;
  base_url      : string;
  api_key       : string option;
  org_id        : string option;
  model         : string;
  options       : gen_options;
  extra_headers : (string * string) list;
  chat_path     : string;
  models_path   : string;
  timeout       : float;
}

let name = "openai_compatible"

let make_config
    ?(provider_name = "openai_compatible")
    ?(base_url = "https://api.openai.com/v1")
    ?(options = default_options)
    ?api_key
    ?org_id
    ?(extra_headers = [])
    ?(chat_path = "/chat/completions")
    ?(models_path = "/models")
    ?(timeout = 120.)
    ~model
    () =
  { provider_name; base_url; api_key; org_id; model; options; extra_headers; chat_path; models_path; timeout }

let options_to_json_fields (o : gen_options) =
  let opt key f = function None -> [] | Some v -> [(key, f v)] in
  List.concat [
    opt "temperature"  (fun v -> `Float v) o.temperature;
    opt "top_p"        (fun v -> `Float v) o.top_p;
    opt "top_k"        (fun v -> `Int v)   o.top_k;
    opt "max_tokens"   (fun v -> `Int v)   o.max_tokens;
    opt "seed"         (fun v -> `Int v)   o.seed;
    (if o.stop = [] then []
     else [("stop", `List (List.map (fun s -> `String s) o.stop))]);
  ]

let make_body cfg ?model ?options ?tools msgs ~stream =
  let effective_model = Option.value ~default:cfg.model model in
  let effective_options = Option.value ~default:cfg.options options in
  let base_fields = List.concat [
    [
      ("model",    `String effective_model);
      ("messages", messages_to_wire_json msgs);
      ("stream",   `Bool stream);
    ];
    options_to_json_fields effective_options;
  ] in
  let base_fields =
    if stream then ("stream_options", `Assoc [("include_usage", `Bool true)]) :: base_fields
    else base_fields
  in
  match tools with
  | None | Some [] -> `Assoc base_fields
  | Some ts ->
      let tools_json = `List (List.map (fun t ->
        `Assoc [
          ("type", `String "function");
          ("function", `Assoc [
            ("name", `String (name_of_packed t));
            ("description", `String (description_of_packed t));
            ("parameters", schema_of_packed t);
          ])
        ]) ts)
      in
      `Assoc (("tools", tools_json) :: base_fields)

let auth_headers cfg =
  let h = ("Content-Type", "application/json") :: cfg.extra_headers in
  let h = match cfg.api_key with
    | None -> h
    | Some k -> ("Authorization", "Bearer " ^ k) :: h
  in
  match cfg.org_id with
  | None -> h
  | Some id -> ("OpenAI-Organization", id) :: h

let make_client net uri = Caravan.Tls.make_client net uri

(** Retry-after hint from typed response headers — how long the server
    asked us to wait before the next attempt (free tiers return this on
    429; see [Provider.Retry]). *)
let retry_hint resp =
  let headers = Http.Response.headers resp in
  Caravan.Caravan_error.retry_hint_of_headers
    (fun name -> Http.Header.get headers name)

let read_body (body : Cohttp_eio.Body.t) =
  Eio.Buf_read.(of_flow body ~max_size:max_int |> take_all)

let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    let prompt_tokens     = u |> member "prompt_tokens"     |> to_int_option |> Option.value ~default:0 in
    let completion_tokens = u |> member "completion_tokens" |> to_int_option |> Option.value ~default:0 in
    let total_tokens      = u |> member "total_tokens"      |> to_int_option |> Option.value ~default:0 in
    let total_duration    =
      match u |> member "total_duration" with
      | `Int ns   -> Some (float_of_int ns /. 1e9)
      | `Float ns -> Some (ns /. 1e9)
      | _         -> None
    in
    (* Cache-hit accounting: OpenAI-shaped APIs report
       prompt_tokens_details.cached_tokens; DeepSeek uses
       prompt_cache_hit_tokens.  Recording it makes the byte-stable
       prefix work verifiable instead of hopeful. *)
    let cached_tokens =
      match u |> member "prompt_tokens_details" |> member "cached_tokens" with
      | `Int n -> Some n
      | _ ->
        (match u |> member "prompt_cache_hit_tokens" with
         | `Int n -> Some n
         | _ -> None)
    in
    Some { prompt_tokens; completion_tokens; total_tokens; total_duration; cached_tokens }
  | _ -> None

let parse_complete_response body_str provider_name model =
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let choice = json |> member "choices" |> index 0 in
  let msg_json = choice |> member "message" in
  let content =
    match msg_json |> member "content" with
    | `String s -> s
    | `Null -> ""
    | s -> to_string s
  in
  let extra_content =
    match msg_json |> member "extra_content" with
    | `Null -> None
    | ec -> Some ec
  in
  let finish = choice |> member "finish_reason" |> to_string_option in
  let tool_calls =
    match msg_json |> member "tool_calls" with
    | `Null -> None
    | `List l ->
      Some (List.map (fun tc ->
        let id = tc |> member "id" |> to_string_option |> Option.value ~default:"call_0" in
        let func = tc |> member "function" in
        let name = func |> member "name" |> to_string in
        let args = func |> member "arguments" |> to_string |> Caravan.Types.sanitize_json_args in
        let tc_ec =
          match tc |> member "extra_content" with
          | `Null -> None
          | ec -> Some ec
        in
        { id; name; args; extra_content = tc_ec }
      ) l)
    | _ -> None
  in
  let usage = parse_usage json in
  let reply_msg = make_message ?tool_calls ?extra_content Assistant content in
  wrap_result ~raw_response:body_str ~model ~provider:provider_name ?finish_reason:finish ?usage reply_msg

let log_structured_error provider_name status body_str =
  match Caravan.Caravan_error.parse_provider_error body_str with
  | Some detail ->
    let p_name = Option.value ~default:provider_name detail.provider_name in
    let code_str = Option.value ~default:(string_of_int status) detail.code in
    let raw_str = match detail.raw with Some r -> Printf.sprintf ", raw: %s" r | None -> "" in
    let user_str = match detail.user_id with Some u -> Printf.sprintf ", user_id: %s" u | None -> "" in
    Caravan.Trace.log "debug" "[%s] Upstream provider '%s' returned HTTP %s: %s%s%s"
      provider_name p_name code_str detail.message raw_str user_str
  | None ->
    Caravan.Trace.log "debug" "[%s] Provider returned HTTP %d: %s" provider_name status body_str

let complete net cfg ?model ?options ?tools msgs =
  let effective_model = Option.value ~default:cfg.model model in
  let uri      = Uri.of_string (cfg.base_url ^ cfg.chat_path) in
  let body_str = Yojson.Safe.to_string (make_body cfg ?model ?options ?tools msgs ~stream:false) in
  let headers  = Http.Header.of_list (auth_headers cfg) in
  let client   = make_client net uri in
  Eio.Switch.run @@ fun sw ->
  let (resp, body) =
    Cohttp_eio.Client.post client ~sw ~headers
      ~body:(Cohttp_eio.Body.of_string body_str) uri
  in
  let status = Http.Response.status resp |> Http.Status.to_int in
  let resp_body = read_body body in
  if status >= 200 && status < 300 then
    parse_complete_response resp_body cfg.provider_name effective_model
  else begin
    log_structured_error cfg.provider_name status resp_body;
    let retry_after = retry_hint resp in
    Caravan.Caravan_error.raise_provider_failure ?retry_after
      ~provider:cfg.provider_name ~status ~body:resp_body ()
  end

let stream net cfg ?model ?options ?tools msgs ~on_token =
  let effective_model = Option.value ~default:cfg.model model in
  let uri      = Uri.of_string (cfg.base_url ^ cfg.chat_path) in
  let headers  = Http.Header.of_list (("Accept", "text/event-stream") :: auth_headers cfg) in
  let body_str = Yojson.Safe.to_string (make_body cfg ?model ?options ?tools msgs ~stream:true) in
  let client   = make_client net uri in
  (* Track whether the SSE connection was established successfully (HTTP 2xx).
     This is what determines whether the fallback should fire — not whether
     text tokens were emitted, since tool-call-only turns have zero text tokens
     but are still perfectly valid streaming responses. *)
  let stream_succeeded = ref false in
  let wrapped_on_token token = on_token token in
  let finalize_tool_calls tool_acc =
    if Hashtbl.length tool_acc = 0 then None
    else begin
      let pairs = Hashtbl.fold (fun idx v acc -> (idx, v) :: acc) tool_acc [] in
      let sorted = List.sort (fun (a,_) (b,_) -> compare a b) pairs in
      Some (List.map (fun (_, (id, name, abuf, tc_ec)) ->
        { id; name; args = Caravan.Types.sanitize_json_args (Buffer.contents abuf); extra_content = tc_ec }
      ) sorted)
    end
  in
  let try_stream () =
    let buf      = Buffer.create 4096 in
    let tool_acc : (int, string * string * Buffer.t * Yojson.Safe.t option) Hashtbl.t = Hashtbl.create 4 in
    let extra_content_ref = ref None in
    let usage_ref = ref None in
    let result_ref = ref None in
    let in_reasoning = ref false in
    let close_reasoning () =
      if !in_reasoning then begin
        in_reasoning := false;
        wrapped_on_token "\n</thought>\n\n"
      end
    in
    Eio.Switch.run @@ fun sw ->
    let (resp, body) =
      Cohttp_eio.Client.post client ~sw ~headers
        ~body:(Cohttp_eio.Body.of_string body_str) uri
    in
    let status = Http.Response.status resp |> Http.Status.to_int in
    if status < 200 || status >= 300 then begin
      let err = read_body body in
      log_structured_error cfg.provider_name status err;
      let retry_after = retry_hint resp in
      Caravan.Caravan_error.raise_provider_failure ?retry_after
        ~provider:cfg.provider_name ~status ~body:err ()
    end;
    (* HTTP 2xx — the stream is live; disable the fallback from here on. *)
    stream_succeeded := true;
    let buf_r = Eio.Buf_read.of_flow body ~max_size:max_int in
    (try
      while true do
        let line = String.trim (Eio.Buf_read.line buf_r) in
        if String.length line > 6 && String.sub line 0 6 = "data: " then begin
          let data = String.sub line 6 (String.length line - 6) in
          if data = "[DONE]" then begin
            close_reasoning ();
            let full = Buffer.contents buf in
            let tool_calls = finalize_tool_calls tool_acc in
            let reply = make_message ?tool_calls ?extra_content:(!extra_content_ref) Assistant full in
            result_ref := Some (wrap_result ~raw_response:full ~model:effective_model
              ~provider:cfg.provider_name ?usage:(!usage_ref) reply);
            raise End_of_file
          end else begin
            (try
              let json = Yojson.Safe.from_string data in
              let open Yojson.Safe.Util in
              (match json |> member "usage" with
               | `Assoc _ -> usage_ref := parse_usage json
               | _ -> ());
              let choices = json |> member "choices" in
              if choices <> `Null && choices <> `List [] then begin
                let delta = choices |> index 0 |> member "delta" in
                (match delta |> member "extra_content" with
                 | `Null -> ()
                 | ec -> extra_content_ref := Some ec);
                let reasoning_opt =
                  match delta |> member "reasoning" with
                  | `String s when s <> "" -> Some s
                  | _ ->
                    match delta |> member "reasoning_content" with
                    | `String s when s <> "" -> Some s
                    | _ -> None
                in
                (match reasoning_opt with
                 | Some rtoken ->
                   if not !in_reasoning then begin
                     in_reasoning := true;
                     wrapped_on_token "<thought>\n"
                   end;
                   wrapped_on_token rtoken
                 | None ->
                   (match delta |> member "content" with
                    | `String token when token <> "" ->
                      close_reasoning ();
                      Buffer.add_string buf token;
                      wrapped_on_token token
                    | _ -> ()));
                (match delta |> member "tool_calls" with
                 | `List tcs ->
                   if tcs <> [] then close_reasoning ();
                   List.iter (fun tc ->
                     let idx = tc |> member "index" |> to_int in
                     let (id, name, abuf, tc_ec) =
                       match Hashtbl.find_opt tool_acc idx with
                       | Some existing -> existing
                       | None ->
                         let entry = ("", "", Buffer.create 64, None) in
                         Hashtbl.add tool_acc idx entry;
                         entry
                       in
                     let new_id =
                       match tc |> member "id" with
                       | `String s when s <> "" -> s
                       | _ -> id
                     in
                     let new_ec =
                       match tc |> member "extra_content" with
                       | `Null -> tc_ec
                       | ec -> Some ec
                     in
                     let fn = tc |> member "function" in
                     let new_name =
                       match fn |> member "name" with
                       | `String s when s <> "" -> s
                       | _ -> name
                     in
                     (match fn |> member "arguments" with
                      | `String s -> Buffer.add_string abuf s
                      | _ -> ());
                     Hashtbl.replace tool_acc idx (new_id, new_name, abuf, new_ec)
                   ) tcs
                 | _ -> ())
              end
            with exn ->
              Printf.eprintf "[%s Stream Parse Error]: %s\nData: %s\n%!"
                cfg.provider_name (Printexc.to_string exn) data)
          end
        end
      done
    with End_of_file -> ());
    close_reasoning ();
    match !result_ref with
    | Some r -> r
    | None ->
      let full = Buffer.contents buf in
      let tool_calls = finalize_tool_calls tool_acc in
      (* Guard against a stalled/truncated stream: if the server closed the
         connection before sending [DONE] and we have neither content nor tool
         calls, raise rather than returning an empty message.  An empty reply
         would make [is_finished] return false and spin the agent loop forever. *)
      if full = "" && tool_calls = None then
        Caravan.Caravan_error.raise_provider_failure
          ~provider:cfg.provider_name ~status:200
          ~body:"Stream closed without [DONE] and without content" ()
      else
        let reply = make_message ?tool_calls ?extra_content:(!extra_content_ref) Assistant full in
        wrap_result ~raw_response:full ~model:effective_model ~provider:cfg.provider_name
          ?usage:(!usage_ref) reply
  in
  try
    try_stream ()
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn when not !stream_succeeded ->
    let human_err = Caravan.Caravan_error.humanize exn in
    let single_line_err = String.concat " " (String.split_on_char '\n' human_err) in
    Caravan.Trace.log "warn" "[%s] Streaming failed before connection; falling back to non-streaming completion... (%s)"
      cfg.provider_name single_line_err;
    complete net cfg ?model ?options ?tools msgs

(** List models. Transport failures (connection refused, TLS, DNS)
    PROPAGATE so callers like [doctor] and the init wizard can report an
    unreachable endpoint honestly. The [cfg.model] fallback applies only
    when the server responds but has no parseable /models listing. *)
let list_models net cfg =
  let uri    = Uri.of_string (cfg.base_url ^ cfg.models_path) in
  let client = make_client net uri in
  Eio.Switch.run @@ fun sw ->
  let headers = Http.Header.of_list (auth_headers cfg) in
  let (resp, body) = Cohttp_eio.Client.get client ~sw ~headers uri in
  let status = Http.Response.status resp |> Http.Status.to_int in
  let body_str = read_body body in
  if status >= 200 && status < 300 then
    try
      let json = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      let items =
        match json |> member "data" with
        | `List l -> l
        | _ -> (match json |> member "models" with `List l -> l | _ -> [])
      in
      List.map (fun m ->
        match m |> member "id" with
        | `String id -> id
        | _ -> (match m |> member "name" with `String name -> name | _ -> "")
      ) items |> List.filter (fun s -> s <> "")
    with _ -> [cfg.model]
  else if status = 404 || status = 405 then
    (* Server up, no /models endpoint (some llama.cpp builds, gateways). *)
    [cfg.model]
  else
    failwith (Printf.sprintf "%s models error %d: %s"
                cfg.provider_name status body_str)

let make_provider
    ?(provider_name = "openai_compatible")
    ?(base_url = "https://api.openai.com/v1")
    ?(options = default_options)
    ?api_key
    ?org_id
    ?(extra_headers = [])
    ?(chat_path = "/chat/completions")
    ?(models_path = "/models")
    ?(timeout = 120.)
    ~model
    () =
  let cfg = make_config ~provider_name ~base_url ~options ?api_key ?org_id ~extra_headers ~chat_path ~models_path ~timeout ~model () in
  Provider ((module struct
    type nonrec config = config
    let name = provider_name
    let complete = complete
    let stream = stream
    let list_models = list_models
  end), cfg)
