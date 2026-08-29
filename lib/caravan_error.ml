type t =
  | Tool_error of string
  | Tool_not_found of string
  | Json_parse_error of string
  | Provider_error of string
  | Mcp_error of string
  | Subagent_error of string
  | Eio_error of string
  | Permission_denied of string
  | Exception of string

let to_string = function
  | Tool_error msg -> "Tool Error: " ^ msg
  | Tool_not_found msg -> "Tool Not Found: " ^ msg
  | Json_parse_error msg -> "JSON Parse Error: " ^ msg
  | Provider_error msg -> "Provider Error: " ^ msg
  | Mcp_error msg -> "MCP Error: " ^ msg
  | Subagent_error msg -> "Subagent Error: " ^ msg
  | Eio_error msg -> "Eio Error: " ^ msg
  | Permission_denied msg -> "Permission Denied: " ^ msg
  | Exception msg -> "Exception: " ^ msg

type provider_error_detail = {
  provider_name : string option;
  code          : string option;
  message       : string;
  raw           : string option;
  param         : string option;
  user_id       : string option;
}

exception Provider_failure of {
  provider : string;
  status   : int;
  body     : string;
  detail   : provider_error_detail option;
  retry_after : float option;
      (** Seconds the server asked us to wait, from typed response
          headers (Retry-After and the x-ratelimit-reset family). Free tiers
          commonly return tens of seconds on 429 — exponential backoff
          alone exhausts its retries long before the window reopens. *)
}

let get_member field = function
  | `Assoc kvs -> (match List.assoc_opt field kvs with Some v -> v | None -> `Null)
  | _ -> `Null

let get_string field json =
  match get_member field json with
  | `String s -> Some s
  | _ -> None

let get_string_or_int field json =
  match get_member field json with
  | `String s -> Some s
  | `Int i -> Some (string_of_int i)
  | _ -> None

let parse_provider_error str =
  match Parser.permissive_json str with
  | Error _ -> None
  | Ok json ->
    let err_node = get_member "error" json in
    if err_node = `Null then None
    else
      let message =
        match get_string "message" err_node with
        | Some m -> m
        | None ->
          match err_node with
          | `String m -> m
          | `Assoc _ -> Yojson.Safe.to_string err_node
          | _ ->
            match get_string "message" json with
            | Some m -> m
            | None -> "Provider error"
      in
      let code =
        match get_string_or_int "code" err_node with
        | Some c -> Some c
        | None -> get_string_or_int "code" json
      in
      let meta = get_member "metadata" err_node in
      let raw = get_string "raw" meta in
      let provider_name =
        match get_string "provider_name" meta with
        | Some p -> Some p
        | None -> get_string "provider" json
      in
      let param = get_string "param" err_node in
      let user_id = get_string "user_id" json in
      Some { provider_name; code; message; raw; param; user_id }

(* ── Retry hints from typed response headers ──────────────────────────── *)

(* Parse a rate-limit duration: bare seconds ("30", "7.66"), or the
   OpenAI/Groq compound style ("250ms", "1s", "6m0s", "1h2m3.5s"). *)
let parse_duration s =
  let s = String.trim (String.lowercase_ascii s) in
  if s = "" then None
  else
    match float_of_string_opt s with
    | Some f when f >= 0.0 -> Some f
    | Some _ -> None
    | None ->
      let re = Re.compile
          Re.(seq [group (rep1 (alt [digit; char '.']));
                   group (alt [str "ms"; str "h"; str "m"; str "s"])]) in
      let ms = Re.all re s in
      if ms = [] then None
      else
        (* Reject strings with leftovers beyond number+unit pairs so we
           don't misread arbitrary header text as a duration. *)
        let consumed =
          List.fold_left (fun acc g -> acc + String.length (Re.Group.get g 0)) 0 ms in
        if consumed <> String.length s then None
        else
          List.fold_left
            (fun acc g ->
               match acc, float_of_string_opt (Re.Group.get g 1) with
               | Some total, Some v ->
                 let mult = match Re.Group.get g 2 with
                   | "ms" -> 0.001 | "s" -> 1.0 | "m" -> 60.0 | "h" -> 3600.0
                   | _ -> 1.0
                 in
                 Some (total +. (v *. mult))
               | _ -> None)
            (Some 0.0) ms

(** Extract a retry-after hint from response headers via [get] (a
    case-insensitive header lookup supplied by the transport).  Checks
    [Retry-After] (delta-seconds form), then the
    [x-ratelimit-reset-requests] / [x-ratelimit-reset-tokens] family.
    Reading a typed header is not string-scraping an exception — the
    value never leaves the provider boundary unstructured. *)
let retry_hint_of_headers get =
  let first_some l = List.find_map (fun k -> get k) l in
  match first_some ["retry-after"; "Retry-After"] with
  | Some v when parse_duration v <> None -> parse_duration v
  | _ ->
    (match first_some ["x-ratelimit-reset-requests"; "x-ratelimit-reset-tokens"] with
     | Some v -> parse_duration v
     | None -> None)

let raise_provider_failure ?retry_after ~provider ~status ~body () =
  let detail = parse_provider_error body in
  raise (Provider_failure { provider; status; body; detail; retry_after })

let contains haystack needle =
  try let _ = Re.exec (Re.compile (Re.str needle)) haystack in true
  with Not_found -> false

let humanize exn =
  match exn with
  | Provider_failure { provider; status = _; body = _; detail } ->
    let p_info =
      match detail with
      | Some { provider_name = Some p; _ } -> " (" ^ p ^ ")"
      | _ -> " (" ^ provider ^ ")"
    in
    let msg =
      match detail with
      | Some d -> d.message
      | None -> "Provider returned error"
    in
    let raw_info =
      match detail with
      | Some { raw = Some r; _ } when r <> "" -> " [raw: " ^ r ^ "]"
      | _ -> ""
    in
    Printf.sprintf "Provider error%s: %s%s\n  Hint: Check model compatibility, API keys, or settings via /config."
      p_info msg raw_info
  | _ ->
    let raw = Printexc.to_string exn in
    match parse_provider_error raw with
    | Some detail ->
      let p_info = match detail.provider_name with Some p -> " (" ^ p ^ ")" | None -> "" in
      let raw_info = match detail.raw with Some r when r <> "" -> " [raw: " ^ r ^ "]" | _ -> "" in
      Printf.sprintf "Provider error%s: %s%s\n  Hint: Check model compatibility, API keys, or settings via /config."
        p_info detail.message raw_info
    | None ->
      if contains raw "ECONNREFUSED" || contains raw "Connection refused" then
        "Could not connect to the AI provider.\n" ^
        "  Hint: Is Ollama running? Try: ollama serve\n" ^
        "  Hint: Using OpenAI? Check your API key and internet connection."
      else if contains raw "404"
           || (contains raw "model" && (contains raw "not found" || contains raw "does not exist")) then
        "Model not found on this provider.\n" ^
        "  Hint: Run /models to see what's available, or /model <name> to switch."
      else if contains raw "401" || contains raw "Unauthorized" then
        "Authentication failed. Your API key may be missing or invalid.\n" ^
        "  Hint: Set it with: export OPENAI_API_KEY=\"sk-...\"\n" ^
        "  Hint: Or add it to ~/.caravan/config.toml"
      else if contains raw "429" || contains raw "rate" then
        "Rate limited by the provider. Wait a moment and try again."
      else
        Printf.sprintf "Something went wrong: %s\n  Hint: Try /config to check your settings." raw

let of_exn exn =
  Exception (Printexc.to_string exn)

let safe_run f =
  try Ok (f ())
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (of_exn exn)
