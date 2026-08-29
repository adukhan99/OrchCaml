(** Fraction of the model's context window the history may consume
    before compaction fires.  Leaving quarter-window headroom covers the
    system prompt, tool schemas, and the reply. *)
let window_fraction = 0.75

let should_compact ?context_window ~auto_summarize ~memory_size
    ~history_length ~history_tokens ~tool_call_names () =
  let explicit = List.exists
    (fun n -> n = "summarize" || n = "compress_history" || n = "summarise")
    tool_call_names
  in
  (* Message-count overflow: the pre-token-awareness trigger, kept as a
     user-facing override for people who want the old behaviour. *)
  let count_overflow =
    auto_summarize
    && memory_size > 0
    && history_length > memory_size
  in
  (* Token overflow: the primary trigger once the model's context
     window is known (from the capability table).  A fixed message
     count both overflows an 8k local model and wastes 90% of a 128k
     one; fraction-of-window does neither. *)
  let token_overflow =
    match context_window with
    | Some window when window > 0 ->
      auto_summarize
      && float_of_int history_tokens > window_fraction *. float_of_int window
    | _ -> false
  in
  explicit || count_overflow || token_overflow
