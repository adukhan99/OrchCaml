(** Fallback extraction of tool calls a model emitted as text.

    Recognises whole-content invocations only — a fenced or bare JSON
    object naming a registered tool, or the same JSON inside a
    [<tool_call>]/[<function_call>] XML wrapper.  JSON embedded in
    prose, unknown tool names, and ReAct-style [Action:] blocks are all
    rejected: false positives (executing a tool the model was merely
    discussing) are the failure mode this module is designed against. *)

(** [extract ~tools content] returns the synthesised {!Types.tool_call}
    list plus a format label (["json"], ["fenced_json"], ["xml"]) for
    tracing, or [None] when [content] is not a complete invocation of a
    registered tool.  Multi-call [{"tool_calls": [...]}] payloads are
    all-or-nothing: one bad entry rejects the extraction. *)
val extract :
  tools:Tool.packed_tool list ->
  string ->
  (Types.tool_call list * string) option
