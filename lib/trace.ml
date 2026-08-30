(** Structured event stream for everything Caravan does.

    The library never prints user-facing output directly; it emits events
    here. Front-ends (the REPL, one-shot CLI runs, the web UI, tests)
    install sinks that render, record, or ignore them.

    Two sinks ship with Caravan:
    - a pretty ANSI renderer (installed by [bin/main.ml]);
    - [jsonl_sink], which appends every event to a JSONL transcript so a
      session is auditable after the fact. *)

type event =
  | Model_call_start of { model : string; provider : string }
  | Stream_start                              (** first token of a stream *)
  | Assistant_reply  of { content : string; tool_call_names : string list }
  | Tool_call_start  of { name : string; args : string }
  | Tool_call_end    of { name : string; output : string; duration : float }
  | Tool_not_found   of { name : string }
  | Tool_call_fallback of { name : string; format : string }
      (** a tool call was recovered from plain [content] by the text
          fallback parser (the model did not use native tool_calls);
          [format] is the recognised shape (json / fenced_json / xml) *)
  | Permission_denied of { name : string }
  | Task_finished    of { summary : string }
  | Summarize_start
  | Summarize_end    of { summary : string }
  | Agent_turn       of { current : int; max : int }
  | Nudge            of { content : string }
  | Log              of { level : string; message : string }
  | Subagent_start   of { name : string; task : string }
  | Subagent_end     of { name : string; summary : string }
  | Plugin_transition of { name : string; uid : int; state : string }
      (** a plugin fiber changed lifecycle state (see [Plugin]) *)
  | Provider_retry of { provider : string; attempt : int; max_attempts : int }
      (** a provider call failed transiently and is being retried
          ([attempt] = retry number just decided, 1-based) *)
  | Run_error        of { origin : string; message : string }
      (** a provider/tool/run failure surfaced to the user — recorded so
          failed sessions are auditable, not only successful ones *)

type sink = event -> unit

let sinks : sink list ref = ref []

let add_sink s = sinks := s :: !sinks

let clear_sinks () = sinks := []

(** Run [f] with [s] temporarily installed. *)
let with_sink s f =
  let saved = !sinks in
  sinks := s :: saved;
  Fun.protect ~finally:(fun () -> sinks := saved) f

let emit ev = List.iter (fun s -> try s ev with _ -> ()) !sinks

let log level fmt = Printf.ksprintf (fun m -> emit (Log { level; message = m })) fmt

(** Record a user-surfaced failure. Use at REPL/agent catch sites in
    place of printing directly: the renderer prints it (in red, even in
    quiet mode) and the JSONL sink makes the failure auditable. *)
let error origin fmt =
  Printf.ksprintf (fun m -> emit (Run_error { origin; message = m })) fmt

(* ── JSONL transcript sink ────────────────────────────────────────────── *)

let event_to_json ev : Yojson.Safe.t =
  let now = Unix.gettimeofday () in
  let base kind fields = `Assoc (("ts", `Float now) :: ("event", `String kind) :: fields) in
  match ev with
  | Model_call_start { model; provider } ->
    base "model_call_start" [("model", `String model); ("provider", `String provider)]
  | Stream_start -> base "stream_start" []
  | Assistant_reply { content; tool_call_names } ->
    base "assistant_reply"
      [("content", `String content);
       ("tool_calls", `List (List.map (fun n -> `String n) tool_call_names))]
  | Tool_call_start { name; args } ->
    base "tool_call_start" [("name", `String name); ("args", `String args)]
  | Tool_call_end { name; output; duration } ->
    base "tool_call_end"
      [("name", `String name); ("output", `String output); ("duration_s", `Float duration)]
  | Tool_not_found { name } -> base "tool_not_found" [("name", `String name)]
  | Tool_call_fallback { name; format } ->
    base "tool_call_fallback" [("name", `String name); ("format", `String format)]
  | Permission_denied { name } -> base "permission_denied" [("name", `String name)]
  | Task_finished { summary } -> base "task_finished" [("summary", `String summary)]
  | Summarize_start -> base "summarize_start" []
  | Summarize_end { summary } -> base "summarize_end" [("summary", `String summary)]
  | Agent_turn { current; max } ->
    base "agent_turn" [("current", `Int current); ("max", `Int max)]
  | Nudge { content } -> base "nudge" [("content", `String content)]
  | Log { level; message } ->
    base "log" [("level", `String level); ("message", `String message)]
  | Subagent_start { name; task } ->
    base "subagent_start" [("name", `String name); ("task", `String task)]
  | Subagent_end { name; summary } ->
    base "subagent_end" [("name", `String name); ("summary", `String summary)]
  | Plugin_transition { name; uid; state } ->
    base "plugin_transition" [("name", `String name); ("uid", `Int uid); ("state", `String state)]
  | Provider_retry { provider; attempt; max_attempts } ->
    base "provider_retry"
      [("provider", `String provider); ("attempt", `Int attempt);
       ("max_attempts", `Int max_attempts)]
  | Run_error { origin; message } ->
    base "error" [("origin", `String origin); ("message", `String message)]

(** A sink that appends one JSON object per event to [oc], flushing eagerly
    so transcripts survive crashes. *)
let jsonl_sink oc : sink =
  fun ev ->
    output_string oc (Yojson.Safe.to_string (event_to_json ev));
    output_char oc '\n';
    flush oc

(** Open (creating directories as needed) a timestamped transcript file
    under [dir], register a JSONL sink for it, and return its path.
    Registers an [at_exit] close. *)
let open_transcript ~dir =
  let rec mkdir_p path =
    if not (Sys.file_exists path) then begin
      mkdir_p (Filename.dirname path);
      (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    end
  in
  mkdir_p dir;
  let tm = Unix.localtime (Unix.gettimeofday ()) in
  let name =
    Printf.sprintf "session-%04d%02d%02d-%02d%02d%02d-%d.jsonl"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec (Unix.getpid ())
  in
  let path = Filename.concat dir name in
  let oc = open_out_gen [Open_creat; Open_append] 0o600 path in
  add_sink (jsonl_sink oc);
  at_exit (fun () -> try close_out oc with _ -> ());
  path
