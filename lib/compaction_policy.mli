(** Policy for when to compact conversation history. *)

(** Fraction of the context window the history may consume before the
    token trigger fires (headroom for system prompt, schemas, reply). *)
val window_fraction : float

(** [should_compact ?context_window ~auto_summarize ~memory_size
    ~history_length ~history_tokens ~tool_call_names ()] returns [true]
    when the session should be compacted after a turn step: on an
    explicit summarize tool call, on message-count overflow
    ([history_length > memory_size], the legacy user-facing override),
    or — when [context_window] is known from the capability table — on
    estimated token consumption exceeding {!window_fraction} of it. *)
val should_compact :
  ?context_window:int ->
  auto_summarize:bool ->
  memory_size:int ->
  history_length:int ->
  history_tokens:int ->
  tool_call_names:string list ->
  unit ->
  bool
