open Types

let with_summary summary msgs =
  match summary with
  | None   -> msgs
  | Some s -> system_msg ("[Conversation summary]: " ^ s) :: msgs

module type MEMORY = sig
  type t

  val create   : unit -> t

  val add      : t -> chat_message -> t
  val get      : t -> chat_message list
  val clear    : t -> t
  val length   : t -> int
  val set_window : t -> int -> t
  val map_recent : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json  : t -> Yojson.Safe.t
  val of_json  : Yojson.Safe.t -> t
end

type packed_memory = Mem : (module MEMORY with type t = 'a) * 'a -> packed_memory

module Ring : sig
  type t
  val make        : ?window:int -> unit -> t
  val create      : unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
end = struct
  type t = {
    system_msgs : chat_message list;
    front       : chat_message list;
    rear        : chat_message list;
    window      : int;
    len         : int;
  }

  (* A window of 0 (or below) means "unlimited" everywhere in Caravan;
     normalising here keeps every construction site honest. *)
  let normalize_window w = if w <= 0 then max_int else w

  let make ?(window = 20) () =
    { system_msgs = []; front = []; rear = []; window = normalize_window window; len = 0 }

  let create () = make ()


  let rebalance dq =
    match dq.front with
    | [] -> { dq with front = List.rev dq.rear; rear = [] }
    | _  -> dq

  let tool_call_ids msg =
    match msg.tool_calls with
    | Some tcs when tcs <> [] -> Some (List.map (fun tc -> tc.id) tcs)
    | _ -> None

  (* Eviction is pair-aware and atomic: an assistant message bearing
     tool_calls and its result messages form one indivisible unit.
     Dropping only the assistant half leaves [tool] messages whose
     tool_call_id no longer appears in the request — a shape OpenAI,
     Groq, Mistral and DeepSeek all reject with a non-retriable 400,
     mid-run, after the tokens are already spent. *)
  let drop_oldest dq =
    let dq = rebalance dq in
    match dq.front with
    | []          -> dq
    | oldest :: t ->
      let dq = rebalance { dq with front = t; len = dq.len - 1 } in
      (match tool_call_ids oldest with
       | None -> dq
       | Some ids ->
         let rec drop_results dq =
           match dq.front with
           | { role = Tool id; _ } :: t when List.mem id ids ->
             drop_results (rebalance { dq with front = t; len = dq.len - 1 })
           | _ -> dq
         in
         drop_results dq)

  let add mem msg =
    match msg.role with
    | System ->
      { mem with system_msgs = mem.system_msgs @ [msg] }
    | _ ->
      let dq = { mem with rear = msg :: mem.rear; len = mem.len + 1 } in
      if dq.len > dq.window then drop_oldest dq else dq

  let set_window mem new_window =
    let window = normalize_window new_window in
    let rec prune d =
      if d.len > d.window then prune (drop_oldest d) else d
    in
    prune { mem with window }

  let get mem =
    mem.system_msgs @ mem.front @ List.rev mem.rear

  (* Apply [f] to every non-system message except the newest [keep].
     Exists so Session can truncate tool outputs exactly once, when a
     message ages out of the recent window — after which its serialised
     bytes never change again (the prompt-cache prefix precondition). *)
  let map_recent mem ~keep f =
    let msgs = mem.front @ List.rev mem.rear in
    let n = List.length msgs in
    let mapped = List.mapi (fun i m -> if i < n - keep then f m else m) msgs in
    { mem with front = mapped; rear = [] }

  let clear mem =
    { mem with system_msgs = []; front = []; rear = []; len = 0 }

  let length mem = List.length mem.system_msgs + mem.len

  let to_json mem =
    `List (List.map chat_message_to_json (get mem))

  let of_json json =
    let msgs = Yojson.Safe.Util.to_list json |> List.map chat_message_of_json in
    List.fold_left add (make ()) msgs
end

module Noop : MEMORY = struct
  type t = unit

  let create       ()      = ()
  let add          () _msg = ()
  let get          ()      = []
  let clear        ()      = ()
  let length       ()      = 0
  let set_window   () _    = ()
  let map_recent   () ~keep:_ _ = ()
  let to_json      ()      = `List []
  let of_json      _       = ()
end

module Summary : sig
  type t
  val create      : ?max_messages:int -> unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
  val compress    : complete:(chat_message list -> string) -> t -> t
end = struct
  type t = {
    buf          : Ring.t;
    max_messages : int;
    summary      : string option;
  }

  let create ?(max_messages = 40) () =
    { buf = Ring.make ~window:max_messages (); max_messages; summary = None }

  let add mem msg = { mem with buf = Ring.add mem.buf msg }

  let get mem = with_summary mem.summary (Ring.get mem.buf)

  let clear mem = { mem with buf = Ring.clear mem.buf; summary = None }

  let length mem = Ring.length mem.buf

  let set_window mem w =
    { mem with buf = Ring.set_window mem.buf w; max_messages = w }

  let map_recent mem ~keep f =
    { mem with buf = Ring.map_recent mem.buf ~keep f }

  let compress ~complete mem =
    let msgs    = Ring.get mem.buf in
    let new_sum = complete msgs in
    { mem with summary = Some new_sum; buf = Ring.clear mem.buf }

  let to_json mem =
    `Assoc [
      ("messages", Ring.to_json mem.buf);
      ("summary",  match mem.summary with
                   | None   -> `Null
                   | Some s -> `String s);
    ]

  let of_json json =
    let open Yojson.Safe.Util in
    let msgs = json |> member "messages" |> to_list |> List.map chat_message_of_json in
    let mem  = create () in
    let mem  = List.fold_left add mem msgs in
    let summary =
      match json |> member "summary" with
      | `String s -> Some s
      | _         -> None
    in
    { mem with summary }
end

module Hierarchical : sig
  type t
  val no_op_summarizer : chat_message list -> string
  val create      : ?max_short:int -> summarizer:(chat_message list -> string) -> unit -> t
  val add         : t -> chat_message -> t
  val get         : t -> chat_message list
  val clear       : t -> t
  val length      : t -> int
  val set_window  : t -> int -> t
  val map_recent  : t -> keep:int -> (chat_message -> chat_message) -> t
  val to_json     : t -> Yojson.Safe.t
  val of_json     : Yojson.Safe.t -> t
end = struct
  type t = {
    short_term  : Ring.t;
    running_sum : string option;
    max_short   : int;
    summarizer  : chat_message list -> string;
  }

  let no_op_summarizer msgs =
    String.concat "\n\n"
      (List.map (fun m ->
         Printf.sprintf "[%s]: %s" (role_to_string m.role) m.content) msgs)

  let create ?(max_short = 20) ~summarizer () =
    { short_term = Ring.make ~window:max_short ();
      running_sum = None;
      max_short;
      summarizer }

  let compress_internal mem =
    let msgs    = Ring.get mem.short_term in
    let sum_text = mem.summarizer msgs in
    let new_sum = match mem.running_sum with
      | None   -> sum_text
      | Some s -> s ^ "\n\n" ^ sum_text
    in
    { mem with running_sum = Some new_sum; short_term = Ring.clear mem.short_term }

  let add mem msg =
    let mem =
      if Ring.length mem.short_term >= mem.max_short
      then compress_internal mem
      else mem
    in
    { mem with short_term = Ring.add mem.short_term msg }

  let get mem = with_summary mem.running_sum (Ring.get mem.short_term)

  let clear mem =
    { mem with short_term = Ring.clear mem.short_term; running_sum = None }

  let length mem = Ring.length mem.short_term

  let set_window mem w =
    { mem with short_term = Ring.set_window mem.short_term w; max_short = w }

  let map_recent mem ~keep f =
    { mem with short_term = Ring.map_recent mem.short_term ~keep f }

  let to_json mem =
    `Assoc [
      ("short_term",  Ring.to_json mem.short_term);
      ("running_sum", match mem.running_sum with
                      | None   -> `Null
                      | Some s -> `String s);
    ]

  let of_json json =
    let open Yojson.Safe.Util in
    let mem  = create ~summarizer:no_op_summarizer () in
    let msgs = json |> member "short_term" |> to_list |> List.map chat_message_of_json in
    let mem  = List.fold_left add mem msgs in
    let running_sum =
      match json |> member "running_sum" with
      | `String s -> Some s
      | _         -> None
    in
    { mem with running_sum }
end

module SummaryMemory : MEMORY with type t = Summary.t = struct
  type t = Summary.t
  let create () = Summary.create ()
  let add = Summary.add
  let get = Summary.get
  let clear = Summary.clear
  let length = Summary.length
  let set_window = Summary.set_window
  let map_recent = Summary.map_recent
  let to_json = Summary.to_json
  let of_json = Summary.of_json
end


