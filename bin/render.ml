(** CLI renderer for the Caravan trace stream.

    Turns structured [Caravan.Trace] events into the pretty terminal
    narrative: tool-call lines, results with timing, nudges, log lines.
    Installed once at startup by [main]; the library itself never prints. *)

open Caravan

type opts = {
  streaming : bool;   (** tokens already echoed — don't reprint content *)
  quiet     : bool;   (** suppress everything except errors and finish *)
  verbose   : bool;   (** full tool call arguments, results, and debug logs *)
}

let println = Ui.println_ansi

let render opts ev =
  let open Trace in
  match ev with
  | Model_call_start { model; provider } ->
    if opts.verbose && not opts.quiet then
      println (Ui.dim (Printf.sprintf "  [model] calling %s on %s…" model provider))
  | Tool_call_start { name; args } ->
    if not opts.quiet then println (Ui.format_tool_call ~verbose:opts.verbose ~name ~args ())
  | Tool_call_end { name = _; output; duration } ->
    if not opts.quiet then println (Ui.format_tool_result ~verbose:opts.verbose ~output ~duration ())
  | Tool_not_found { name } ->
    println (Ui.red (Printf.sprintf "  ✗ tool '%s' not found" name))
  | Tool_call_fallback { name; format } ->
    if not opts.quiet then
      println (Ui.dim (Printf.sprintf "  ↯ recovered '%s' tool call from text (%s)" name format))
  | Permission_denied { name } ->
    println (Ui.yellow (Printf.sprintf "  ⛔ permission denied: %s" name))
  | Task_finished { summary } ->
    if not opts.quiet then begin
      println (Ui.green (Printf.sprintf "  ✔ %s" summary))
    end
  | Assistant_reply { content; tool_call_names } ->
    (* Intermediate "thinking out loud" while tools are pending. The final
       reply is printed by the REPL/one-shot runner itself; streamed tokens
       are echoed by on_token. *)
    if (not opts.streaming) && (not opts.quiet)
       && tool_call_names <> [] && String.trim content <> ""
       && not (List.mem "finish" tool_call_names) then
      println (Ui.dim (String.trim content))
  | Stream_start ->
    if not opts.quiet && Ui.is_tty then
      println (Printf.sprintf "\n%s" (Ui.bold (Ui.green "Assistant:")))
  | Summarize_start ->
    if not opts.quiet then println (Ui.dim "  ✦ compacting context…")
  | Summarize_end { summary = _ } ->
    if not opts.quiet then println (Ui.dim "  ✦ context compacted")
  | Agent_turn _ -> ()
  | Nudge { content = _ } ->
    if not opts.quiet then println (Ui.dim "  ≫ nudge: budget reminder injected")
  | Log { level; message } ->
    (match level with
     | "error" -> println (Ui.red ("  " ^ message))
     | "warn" | "warning" -> println (Ui.yellow ("  " ^ message))
     | "debug" -> if opts.verbose && not opts.quiet then println (Ui.dim ("  [debug] " ^ message))
     | _ -> if not opts.quiet then println (Ui.dim ("  " ^ message)))
  | Subagent_start { name; task } ->
    if not opts.quiet then
      let task_preview = if opts.verbose then String.trim task else Ui.truncate_visible (String.trim task) 60 in
      println (Printf.sprintf "  %s %s %s"
                 (Ui.cyan "↳ [subagent:") (Ui.bold name) (Ui.dim (Printf.sprintf "] task: %s" task_preview)))
  | Subagent_end { name; summary } ->
    if not opts.quiet then
      let sum_preview = if opts.verbose then String.trim summary else Ui.truncate_visible (String.trim summary) 60 in
      println (Printf.sprintf "  %s %s %s"
                 (Ui.dim "  ⎿ [subagent:") (Ui.dim name) (Ui.dim (Printf.sprintf "] complete: %s" sum_preview)))
  | Provider_retry { provider; attempt; max_attempts } ->
    if not opts.quiet then
      println (Ui.yellow (Printf.sprintf "  ↻ %s transient failure (attempt %d/%d), retrying…"
                 provider attempt max_attempts))
  | Plugin_transition { name; uid = _; state } ->
    (* Lifecycle chatter is verbose-only; the transcript records it always. *)
    if opts.verbose && not opts.quiet then
      println (Ui.dim (Printf.sprintf "  [plugin] %s → %s" name state))
  | Run_error { origin = _; message = _ } ->
    (* The catch sites that emit this already print the failure to the
       terminal in their own format; the event exists so the JSONL
       transcript records failed sessions too. Render nothing. *)
    ()

(** Install the renderer; returns unit. Options are read through a ref so
    the REPL can flip streaming/quiet at runtime without reinstalling. *)
let install (opts_ref : opts ref) =
  Trace.add_sink (fun ev -> render !opts_ref ev)
