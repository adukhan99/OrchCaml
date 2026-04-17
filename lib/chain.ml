(** OrchCaml.Chain — Composable typed LLM pipelines.

    The chain type is ['a -> ('b, string) result Lwt.t].
    Error paths are first-class citizens — [|>>] threads the Result monad
    inside Lwt using [Lwt_result] patterns, so a failing step short-circuits
    the rest of the pipeline without exceptions.

    Example:
    {[
      let my_chain provider =
        Chain.prompt_template "Explain {{topic}} in one sentence."
        |>> Chain.llm provider
        |>> Chain.parse Parser.trimmed
      in
      (* result : (string, string) result *)
      let%lwt result = Chain.run (my_chain provider) [("topic", "monads")] in
      match result with
      | Ok answer -> ...
      | Error msg -> ...
    ]}
*)

open Types

(** A chain step: an async function from ['a] to [('b, string) result]. *)
type ('a, 'b) t = 'a -> ('b, string) result Lwt.t

(** Compose two chain steps left-to-right via [Lwt_result.bind]. *)
let (|>>) (f : ('a, 'b) t) (g : ('b, 'c) t) : ('a, 'c) t =
  fun x ->
    let open Lwt_result.Syntax in
    let* y = f x in
    g y

(** Run a chain with an input value. *)
let run chain input = chain input

(** Run and raise on [Error] (escape hatch for interop). *)
let run_exn chain input =
  let open Lwt.Syntax in
  let* r = chain input in
  match r with
  | Ok v    -> Lwt.return v
  | Error e -> Lwt.fail_with e

(** Lift a pure function into a chain (always succeeds). *)
let lift f x = Lwt.return (Ok (f x))

(** Lift a [Result]-returning pure function into a chain. *)
let lift_result f x = Lwt.return (f x)

(** Lift an [Lwt]-returning function (always succeeds) into a chain. *)
let lift_lwt f x =
  let open Lwt.Syntax in
  let+ v = f x in Ok v

(** [tap f] runs [f] for side effects on a successfully computed value
    and passes it through unchanged.  If the upstream is [Error], [f] is
    not called. *)
let tap (f : 'b -> unit Lwt.t) : ('b, string) result -> ('b, string) result Lwt.t =
  function
  | Error _ as e -> Lwt.return e
  | Ok v ->
    let open Lwt.Syntax in
    let* () = f v in
    Lwt.return (Ok v)

(** [tap_pure f] is [tap] for synchronous side-effecting functions. *)
let tap_pure (f : 'b -> unit) : ('b, string) result -> ('b, string) result Lwt.t =
  function
  | Error _ as e -> Lwt.return e
  | Ok v ->
    f v;
    Lwt.return (Ok v)

(** --- Prompt steps --- *)

(** [prompt_template tmpl_str] compiles and renders a template string.
    Input: a variable list [(name, value)].
    Output: the rendered string, or [Error] on missing variables. *)
let prompt_template tmpl_str : (string * string) list -> (string, string) result Lwt.t =
  let tmpl = Template.of_string tmpl_str in
  fun vars -> Lwt.return (Template.render ~vars tmpl)

(** [prompt_messages tmpl_str] like [prompt_template] but produces a
    [chat_message list] suitable for direct LLM submission.
    Returns [Error] if template rendering fails. *)
let prompt_messages ?system tmpl_str
  : (string * string) list -> (chat_message list, string) result Lwt.t =
  let ct = Template.chat_template ?system tmpl_str in
  fun vars -> Lwt.return (Template.render_chat ~vars ct)

(** [just_messages msgs] lifts a static message list into a chain. *)
let just_messages msgs : unit -> (chat_message list, string) result Lwt.t =
  fun () -> Lwt.return (Ok msgs)

(** --- LLM steps --- *)

(** [llm provider] calls the provider and returns the assistant's content. *)
let llm (provider : Provider.packed_provider) : chat_message list -> (string, string) result Lwt.t =
  fun msgs ->
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let* result = Provider.complete_packed provider msgs in
        Lwt.return (Ok result.value.content))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(** [llm_with_meta provider] like [llm] but returns the full result
    with metadata (model, provider, finish reason). *)
let llm_with_meta (provider : Provider.packed_provider)
  : chat_message list -> (chat_message result_with_meta, string) result Lwt.t =
  fun msgs ->
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let* result = Provider.complete_packed provider msgs in
        Lwt.return (Ok result))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(** [llm_stream provider ~on_token] streams tokens via [on_token] and
    returns the full accumulated response when done. *)
let llm_stream (provider : Provider.packed_provider)
    ~(on_token : string -> unit)
  : chat_message list -> (string, string) result Lwt.t =
  fun msgs ->
    let open Lwt.Syntax in
    Lwt.catch
      (fun () ->
        let stream, _meta_promise = Provider.stream_packed provider msgs in
        let buf = Buffer.create 4096 in
        let+ () = Lwt_stream.iter (fun token ->
          Buffer.add_string buf token;
          on_token token
        ) stream in
        Ok (Buffer.contents buf))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(** --- Parse steps --- *)

(** [parse p] applies parser [p] to the LLM's string output.
    Returns [Error] if the parser fails — no exceptions raised. *)
let parse (p : 'a Parser.t) : string -> ('a, string) result Lwt.t =
  fun s -> Lwt.return (p s)

(** --- Memory-aware chain helpers --- *)

(** [with_memory (module M) mem chain] wraps [chain] so that each call:
    1. Prepends memory history to the input messages.
    2. Adds the user's messages and the assistant response to memory.

    Input: [chat_message list] (the new user turn).
    Output: [(m * string)] (the new memory state and the assistant response).
*)
let with_memory
    (type m)
    (module Mem : Memory.MEMORY with type t = m)
    (mem : m)
    (provider : Provider.packed_provider)
  : chat_message list -> (m * string, string) result Lwt.t =
  fun new_msgs ->
    let open Lwt.Syntax in
    let mem_with_user = List.fold_left Mem.add mem new_msgs in
    let history = Mem.get mem_with_user in
    Lwt.catch
      (fun () ->
        let* result = Provider.complete_packed provider history in
        let final_mem = Mem.add mem_with_user result.value in
        Lwt.return (Ok (final_mem, result.value.content)))
      (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

(** --- Sequence combinators --- *)

(** [sequence chains] runs a list of chains in order, threading the
    value through each one. Short-circuits on the first [Error]. *)
let sequence : ('a, 'a) t list -> ('a, 'a) t = fun chains x ->
  List.fold_left (fun acc_lwt chain ->
    let open Lwt_result.Syntax in
    let* v = acc_lwt in
    chain v
  ) (Lwt.return (Ok x)) chains

(** [parallel chains] runs a list of chains in parallel with the same
    input.  Uses Applicative/Validation-style error collection:
    ALL branches run to completion; the result is [Ok (list of values)]
    only when every branch succeeds, otherwise [Error (all errors joined)].
*)
let parallel : ('a, 'b) t list -> ('a, 'b list) t = fun chains x ->
  let open Lwt.Syntax in
  let* results = Lwt.all (List.map (fun c -> c x) chains) in
  let errs = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
  if errs <> [] then
    Lwt.return (Error (String.concat "\n" errs))
  else
    let vals = List.filter_map (function Ok v -> Some v | _ -> None) results in
    Lwt.return (Ok vals)

(** [retry ~n chain] retries [chain] up to [n] times on [Error].
    Propagates the last error if all attempts fail. *)
let retry ~n chain x =
  let open Lwt.Syntax in
  let rec loop i =
    let* r = chain x in
    match r with
    | Ok _ as ok -> Lwt.return ok
    | Error e ->
      if i >= n then Lwt.return (Error e)
      else begin
        Printf.eprintf "[OrchCaml] Retry %d/%d: %s\n%!" (i+1) n e;
        loop (i + 1)
      end
  in
  loop 0
