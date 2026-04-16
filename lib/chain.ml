(** OrchCaml.Chain — Composable typed LLM pipelines.

    A [chain] is a typed asynchronous function ['a -> 'b Lwt.t].
    Chains compose with [|>>] (Lwt-aware left-to-right bind), giving
    compile-time guarantees that the output type of one step matches
    the input type of the next.

    Example:
    {[
      let my_chain provider =
        Chain.prompt_template "Explain {{topic}} in one sentence."
        |>> Chain.llm provider
        |>> Chain.parse Parser.trimmed
      in
      let%lwt result = Chain.run (my_chain provider) [("topic", "monads")] in
      ...
    ]}
*)

open Types

(** A chain step: an async function from ['a] to ['b]. *)
type ('a, 'b) t = 'a -> 'b Lwt.t

(** Compose two chain steps left-to-right. *)
let (|>>) (f : ('a, 'b) t) (g : ('b, 'c) t) : ('a, 'c) t =
  fun x ->
    let open Lwt.Syntax in
    let* y = f x in
    g y

(** Run a chain with an input value. *)
let run chain input = chain input

(** Lift a pure function into a chain. *)
let lift f x = Lwt.return (f x)

(** Lift a pure result-returning function, raising on error. *)
let lift_result f x =
  match f x with
  | Ok v    -> Lwt.return v
  | Error e -> Lwt.fail_with e

(** [tap f] runs [f] for its side effects (e.g. logging) and passes
    through the value unchanged. Useful for debugging pipelines. *)
let tap f x =
  let open Lwt.Syntax in
  let* () = f x in
  Lwt.return x

(** [tap_pure f] is [tap] for synchronous side-effecting functions. *)
let tap_pure f x =
  f x;
  Lwt.return x

(** --- Prompt steps --- *)

(** [prompt_template tmpl_str] compiles and renders a template string.
    Input: a variable list [(name, value)].
    Output: the rendered string. *)
let prompt_template tmpl_str : (string * string) list -> string Lwt.t =
  let tmpl = Template.of_string tmpl_str in
  fun vars -> Lwt.return (Template.render ~vars tmpl)

(** [prompt_messages tmpl_str] like [prompt_template] but produces a
    [chat_message list] suitable for direct LLM submission.
    Input: [(system_opt, vars)] where system_opt is an optional system string.
    Output: [chat_message list]. *)
let prompt_messages ?system tmpl_str
  : (string * string) list -> chat_message list Lwt.t =
  let ct = Template.chat_template ?system tmpl_str in
  fun vars -> Lwt.return (Template.render_chat ~vars ct)

(** [just_messages msgs] lifts a static message list into a chain. *)
let just_messages msgs : unit -> chat_message list Lwt.t =
  fun () -> Lwt.return msgs

(** --- LLM steps --- *)

(** [llm provider] calls the provider with a message list and returns
    the raw response string. No streaming. *)
let llm (provider : Provider.packed_provider) : chat_message list -> string Lwt.t =
  fun msgs ->
    let open Lwt.Syntax in
    let* result = Provider.complete_packed provider msgs in
    Lwt.return result.value

(** [llm_with_meta provider] like [llm] but returns the full result
    with metadata (model, provider, finish reason). *)
let llm_with_meta (provider : Provider.packed_provider)
  : chat_message list -> string result_with_meta Lwt.t =
  fun msgs -> Provider.complete_packed provider msgs

(** [llm_stream provider ~on_token] streams tokens via [on_token] and
    returns the full accumulated response when done. *)
let llm_stream (provider : Provider.packed_provider)
    ~(on_token : string -> unit)
  : chat_message list -> string Lwt.t =
  fun msgs ->
    let stream, _meta_promise = Provider.stream_packed provider msgs in
    let buf = Buffer.create 4096 in
    Lwt_stream.iter (fun token ->
      Buffer.add_string buf token;
      on_token token
    ) stream
    |> Lwt.map (fun () -> Buffer.contents buf)

(** --- Parse steps --- *)

type 'a parse_result = ('a, string) result

(** [parse p] applies parser [p] to the LLM's string output.
    Raises [Failure] if the parser returns an error. *)
let parse (p : 'a Parser.t) : string -> 'a Lwt.t =
  fun s ->
    match p s with
    | Ok v    -> Lwt.return v
    | Error e -> Lwt.fail_with ("Parse error: " ^ e)

(** [try_parse p] applies [p] but returns [parse_result] instead of
    raising on failure. *)
let try_parse (p : 'a Parser.t) : string -> 'a parse_result Lwt.t =
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
  : chat_message list -> (m * string) Lwt.t =
  fun new_msgs ->
    let open Lwt.Syntax in
    (* Add new user messages to memory *)
    let mem_with_user = List.fold_left Mem.add mem new_msgs in
    (* Build full history *)
    let history = Mem.get mem_with_user in
    (* Call the LLM *)
    let* result = Provider.complete_packed provider history in
    (* Store assistant response *)
    let final_mem = Mem.add mem_with_user (assistant_msg result.value) in
    Lwt.return (final_mem, result.value)

(** --- Sequence combinators --- *)

(** [sequence chains] runs a list of chains in order, threading the
    value through each one. *)
let sequence : ('a, 'a) t list -> ('a, 'a) t = fun chains x ->
  List.fold_left (fun acc_lwt chain ->
    Lwt.bind acc_lwt chain
  ) (Lwt.return x) chains

(** [parallel chains] runs a list of chains in parallel with the same
    input, returning all results. *)
let parallel : ('a, 'b) t list -> ('a, 'b list) t = fun chains x ->
  Lwt.all (List.map (fun c -> c x) chains)

(** [retry ~n chain] retries [chain] up to [n] times on failure. *)
let retry ~n chain x =
  let rec loop i =
    Lwt.catch
      (fun () -> chain x)
      (fun exn ->
         if i >= n then Lwt.fail exn
         else begin
           Printf.eprintf "[OrchCaml] Retry %d/%d: %s\n%!" (i+1) n
             (Printexc.to_string exn);
           loop (i + 1)
         end)
  in
  loop 0
