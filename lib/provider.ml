(** Abstract LLM backend interface. *)

open Types

module type PROVIDER = sig
  type config

  val name : string

  val complete
    :  _ Eio.Net.t
    -> config
    -> ?model:string
    -> ?options:gen_options
    -> ?tools:Tool.packed_tool list
    -> chat_message list
    -> chat_message result_with_meta

  val stream
    :  _ Eio.Net.t
    -> config
    -> ?model:string
    -> ?options:gen_options
    -> ?tools:Tool.packed_tool list
    -> chat_message list
    -> on_token:(string -> unit)
    -> chat_message result_with_meta

  val list_models : _ Eio.Net.t -> config -> string list
end

type packed_provider =
  | Provider : (module PROVIDER with type config = 'c) * 'c -> packed_provider

(* ── Transient-failure retry ──────────────────────────────────────────── *)

(** How aggressively [complete_packed]/[stream_packed] retry transient
    provider failures, configured by the [provider_retry] setting. *)
module Retry = struct
  type mode =
    | Off      (** never retry — first error propagates *)
    | Low      (** 1 retry, 5xx and network-level failures only *)
    | Medium   (** 3 retries, adding 429 (rate limit) to [Low]'s classes *)
    | High     (** unlimited retries on every [Provider_failure] status,
                   including deterministic 4xx client errors *)

  let default_mode = Medium
  let default_base_delay = 0.5

  let max_attempts = function
    | Off -> 1
    | Low -> 2
    | Medium -> 4
    | High -> max_int

  let status_retriable mode status =
    match mode with
    | Off -> false
    | Low -> status >= 500
    | Medium -> status >= 500 || status = 429
    | High -> true

  (* Classification is structural only: HTTP statuses from the typed
     [Provider_failure], and Eio's typed network exceptions (refused /
     unreachable / reset / timeout — inherently transient). Raw exception
     strings are deliberately not scraped. *)
  let classify ~mode = function
    | Caravan_error.Provider_failure { status; _ } -> status_retriable mode status
    | Eio.Io _ -> mode <> Off
    | _ -> false

  (* Exponential backoff before attempt [attempt] (1-based):
     [base_delay * 2^(attempt-1)], capped at 30s. *)
  let delay_seconds ~base_delay attempt =
    Float.min 30.0 (base_delay *. (2.0 ** float_of_int (attempt - 1)))

  (* Ceiling on honouring a server-provided wait: long enough for real
     free-tier windows (commonly tens of seconds), short enough that a
     pathological header cannot stall the harness for minutes. *)
  let max_server_delay = 120.0

  (** Seconds to wait before the retry that follows [exn].  A typed
      [Retry-After]/[x-ratelimit-reset-*] hint from the server wins over
      exponential backoff — a free tier telling us "come back in 30s"
      makes 0.5s/1s/2s backoff a guaranteed failure — clamped to
      [max_server_delay] and floored at the backoff so a zero header
      cannot busy-loop. *)
  let delay_for ~base_delay ~attempt exn =
    let backoff = delay_seconds ~base_delay attempt in
    match exn with
    | Caravan_error.Provider_failure { retry_after = Some ra; _ } ->
      Float.max backoff (Float.min ra max_server_delay)
    | _ -> backoff

  let of_string = function
    | "off" | "none" -> Some Off
    | "low" -> Some Low
    | "medium" | "med" -> Some Medium
    | "high" -> Some High
    | _ -> None

  let to_string = function
    | Off -> "off" | Low -> "low" | Medium -> "medium" | High -> "high"

  (** [run ~mode ~base_delay ~clock ~on_retry f] executes [f ()] until it
      returns or a non-retriable exception escapes; between attempts it
      emits [on_retry attempt] and sleeps on the clock. [~retriable]
      defaults to {!classify} and may tighten it per call site.
      Cancellation always propagates immediately. *)
  let run ~(mode : mode) ?(base_delay = default_base_delay)
      ?(clock : _ Eio.Time.clock option)
      ?(retriable : (exn -> bool) option) ~(on_retry : int -> unit) f =
    let max_attempts = max_attempts mode in
    let retriable = match retriable with Some r -> r | None -> classify ~mode in
    let rec loop attempt =
      match f () with
      | v -> v
      | exception exn ->
        (match exn with
         | Eio.Cancel.Cancelled _ -> raise exn
         | _ when attempt < max_attempts && retriable exn ->
           on_retry attempt;
           (match clock with
            | Some c -> Eio.Time.sleep c (delay_for ~base_delay ~attempt exn)
            | None -> ());
           loop (attempt + 1)
         | _ -> raise exn)
    in
    loop 1
end

let complete_packed ?(retry_mode = Retry.default_mode)
    ?(retry_base_delay = Retry.default_base_delay)
    ?(retry_clock : _ Eio.Time.clock option) net ?model ?options ?tools
    (Provider ((module P), cfg)) msgs =
  let max_attempts = Retry.max_attempts retry_mode in
  Retry.run ~mode:retry_mode ~base_delay:retry_base_delay ?clock:retry_clock
    ~on_retry:(fun attempt ->
      Trace.emit (Trace.Provider_retry { provider = P.name; attempt; max_attempts }))
    (fun () -> P.complete net cfg ?model ?options ?tools msgs)

let stream_packed ?(retry_mode = Retry.default_mode)
    ?(retry_base_delay = Retry.default_base_delay)
    ?(retry_clock : _ Eio.Time.clock option) net ?model ?options ?tools ~on_token
    (Provider ((module P), cfg)) msgs =
  (* Once any token reached the caller's UI a retry would duplicate visible
     output, so post-emission failures propagate instead of retrying. *)
  let emitted = ref false in
  let guarded_on_token tok = emitted := true; on_token tok in
  let max_attempts = Retry.max_attempts retry_mode in
  Retry.run ~mode:retry_mode ~base_delay:retry_base_delay ?clock:retry_clock
    ~retriable:(fun exn -> not !emitted && Retry.classify ~mode:retry_mode exn)
    ~on_retry:(fun attempt ->
      Trace.emit (Trace.Provider_retry { provider = P.name; attempt; max_attempts }))
    (fun () ->
       emitted := false;
       P.stream net cfg ?model ?options ?tools msgs ~on_token:guarded_on_token)

let list_models_packed net (Provider ((module P), cfg)) =
  P.list_models net cfg
let name_of_packed (Provider ((module P), _)) = P.name

(** Unified configuration spec describing any LLM provider endpoint. *)
type provider_spec = {
  name : string;
  base_url : string;
  model : string;
  api_key : string option;
}

(** Parse raw provider configuration arguments into a [provider_spec].
    [~default_base_url] is used when [base_url] is not explicitly specified. *)
let parse_spec ~provider_name ~model ~base_url ~default_base_url ~api_key =
  { name = String.lowercase_ascii (String.trim provider_name);
    base_url = Option.value ~default:default_base_url base_url;
    model;
    api_key }

