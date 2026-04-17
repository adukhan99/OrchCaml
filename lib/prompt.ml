(** OrchCaml.Prompt — Writer Monad for composable conversation building.

    A [Prompt.t] is a pure description of a conversation fragment: a
    computation that yields a value ['a] and a list of [chat_message]s to
    append to the history.  Think of it as the Writer monad where the
    "written" output is a message log.

    Example:
    {[
      let my_prompt =
        let open Prompt in
        let* () = system "You are a helpful OCaml assistant." in
        let* () = user "Explain the Writer monad in one sentence." in
        return ()

      (* Run it: *)
      let msgs = Prompt.exec my_prompt
      (* msgs : chat_message list *)
    ]}

    Prompts are pure — no side effects until you call [exec] or hand the
    accumulated [chat_message list] to a provider.  timestamps are assigned
    lazily at [exec] time, keeping the structure referentially transparent
    during construction.
*)

open Types

(* ------------------------------------------------------------------ *)
(*  Core type                                                           *)
(* ------------------------------------------------------------------ *)

(** A prompt computation that emits messages and returns ['a]. *)
type 'a t = unit -> 'a * chat_message list

(* ------------------------------------------------------------------ *)
(*  Monad primitives                                                    *)
(* ------------------------------------------------------------------ *)

(** Lift a pure value.  Emits no messages. *)
let return x () = (x, [])

(** [bind m f] sequences two prompts, appending their message logs. *)
let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
  fun () ->
    let (a, msgs_a) = m () in
    let (b, msgs_b) = (f a) () in
    (b, msgs_a @ msgs_b)

let (let*) = bind

(** [map f m] transforms the return value without affecting the log. *)
let map f m () =
  let (a, msgs) = m () in
  (f a, msgs)

let (>|=) m f = map f m

(* ------------------------------------------------------------------ *)
(*  Writer primitives                                                   *)
(* ------------------------------------------------------------------ *)

(** Emit a single message into the log. *)
let tell msg () = ((), [msg])

(** Emit multiple messages at once. *)
let tell_all msgs () = ((), msgs)

(* ------------------------------------------------------------------ *)
(*  Message constructors                                                *)
(* ------------------------------------------------------------------ *)

(** Append a [System] message. *)
let system content = tell (system_msg content)

(** Append a [User] message. *)
let user content = tell (user_msg content)

(** Append an [Assistant] message (useful for few-shot examples). *)
let assistant content = tell (assistant_msg content)

(** Append a [Tool] result message. *)
let tool_result id content = tell (tool_msg id content)

(* ------------------------------------------------------------------ *)
(*  Conditional / combinatorial helpers                                 *)
(* ------------------------------------------------------------------ *)

(** [when_ cond p] runs [p] only when [cond] is [true]. *)
let when_ cond p = if cond then p else return ()

(** [unless cond p] runs [p] only when [cond] is [false]. *)
let unless cond p = when_ (not cond) p

(** [optional opt_str f] emits [f s] when [opt_str = Some s], else nothing. *)
let optional opt_str f =
  match opt_str with
  | None   -> return ()
  | Some s -> f s

(** [many xs f] sequences [f x] for each [x] in [xs]. *)
let many xs f =
  List.fold_left (fun acc x ->
    let* () = acc in
    f x
  ) (return ()) xs

(* ------------------------------------------------------------------ *)
(*  Execution                                                            *)
(* ------------------------------------------------------------------ *)

(** [exec p] runs the prompt computation and returns the accumulated
    [chat_message list].  This is the boundary where the pure description
    becomes a concrete list suitable for a provider. *)
let exec p =
  let (_, msgs) = p () in
  msgs

(** [exec_with p] runs the prompt and returns both the value and the log. *)
let exec_with p = p ()

(** [exec_in_session p sess] prepends the emitted messages to [sess]'s
    memory, returning the updated session.  Useful for injecting a
    structured prompt into an ongoing conversation. *)
let exec_in_session p sess =
  let msgs = exec p in
  let memory = List.fold_left Memory.Buffer.add sess.Session.memory msgs in
  { sess with Session.memory }
