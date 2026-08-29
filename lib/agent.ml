(** Autonomous agentic loops. *)

open Types

type agent_config = {
  max_turns       : int;
  continue_prompt : string;
  nudge           : bool;
      (** When true, budget-awareness nudges are appended to continue
          prompts at the halfway point and near exhaustion. *)
  require_finish  : bool;
      (** When true (the default), only an explicit [finish] tool call
          completes an agent run: a plain text reply gets a bounded
          number of format-reminder re-prompts instead of silently
          ending the task.  Conversational preamble before tool use
          ("Sure, I'll read that file!") is characteristic of small
          instruct-tuned models — treating it as completion was the
          dominant silent-failure path (audit H5).  The interactive
          REPL is unaffected: only [Agent.run*] applies this rule. *)
  max_plain_replies : int;
      (** Consecutive plain-text run endings tolerated under
          [require_finish] before failing fast with a diagnostic. *)
}

(** Read lazily so environment/config changes are honoured per run, not
    frozen at module-load time. *)
let default_config () = {
  max_turns       = Config.get_max_turns ();
  continue_prompt = "Please continue until you are finished. Use the 'finish' tool to signal completion.";
  nudge           = Config.get_nudge_enabled ();
  require_finish  = Config.get_require_finish ();
  max_plain_replies = 3;
}

(** Completion is judged from the outcome of the last conversation run
    only — never by scanning history. (Scanning history meant that once
    any task had finished, every subsequent task in the same session was
    instantly considered finished by its stale [finish] call.) *)
let is_finished ?(require_finish = false) (result : chat_message result_with_meta) =
  match result.finish_reason with
  | Some "finish_tool" -> true
  | Some "max_turns"   -> false
  | _ ->
    (* A plain reply with content and no pending tool calls counts as
       done only when finish discipline is off. *)
    (match result.value.tool_calls with
     | None | Some [] ->
       (not require_finish) && String.trim result.value.content <> ""
     | Some _ -> false)

(** Re-prompt after a plain text reply under [require_finish]. *)
let plain_reply_reminder =
  "Your last reply was plain text, but tasks are only completed by \
   calling the 'finish' tool. If the task is done, call 'finish' now \
   with a summary. Otherwise continue working via tool calls."

(** Diagnostic when the model never produces tool calls — fail fast and
    loudly instead of looping (audit H5). *)
let no_tool_calls_diagnostic n =
  Printf.sprintf
    "Agent stopped: the model returned plain text without calling \
     'finish' %d times in a row. It may not support tool calling \
     reliably — try a stronger model, check `caravan models`, or set \
     require_finish = false to accept plain replies as completion."
    n

(** Budget-awareness nudge appended to the continue prompt. *)
let nudge_text ~task ~used ~max_turns =
  Printf.sprintf
    "\n\n[Caravan nudge] You have used %d of %d turns. Stay focused on the \
     original task: %s%s"
    used max_turns task
    (if max_turns - used <= 2
     then " If you cannot fully complete it, call 'finish' now with your best summary."
     else "")

let continue_prompt_for config ~task ~used =
  let base = config.continue_prompt in
  if not config.nudge || config.max_turns <= 0 then base
  else begin
    let half = config.max_turns / 2 in
    let near_end = config.max_turns - used <= 2 in
    if (used = half && half > 0) || near_end then begin
      let n = nudge_text ~task ~used ~max_turns:config.max_turns in
      Trace.emit (Trace.Nudge { content = String.trim n });
      base ^ n
    end else base
  end

let run_generic ?config ?on_turn ?on_step run_fn sess task =
  let config = match config with Some c -> c | None -> default_config () in
  let max_turns = if config.max_turns <= 0 then None else Some config.max_turns in
  let notify_step s =
    match on_step with
    | Some f -> (try f s with _ -> ())
    | None -> ()
  in
  let is_plain (result : chat_message result_with_meta) =
    result.finish_reason = Some (Session.done_reason_string Session.Via_plain_reply)
  in
  let rec loop ~plain_streak sess =
    if config.max_turns > 0 && Session.turn_idx sess >= config.max_turns then
      Error "Maximum turns reached without completion."
    else begin
      let (sess', result) = run_fn ?max_turns ?on_turn ?on_step sess in
      notify_step sess';
      if is_finished ~require_finish:config.require_finish result then
        Ok (sess', result)
      else begin
        (* Under finish discipline, a run ending in a bare text reply is
           re-prompted with a format reminder — but only a bounded
           number of consecutive times, so a model that never emits
           tool calls fails fast with a diagnostic instead of burning
           the whole budget on reminders. *)
        let plain_streak =
          if config.require_finish && is_plain result then plain_streak + 1 else 0
        in
        if config.require_finish && plain_streak >= config.max_plain_replies then
          Error (no_tool_calls_diagnostic plain_streak)
        else if config.max_turns > 0 && Session.turn_idx sess' >= config.max_turns then
          Error "Maximum turns reached without completion."
        else begin
          let prompt =
            if config.require_finish && is_plain result then begin
              Trace.log "warn"
                "Model replied with plain text (%d/%d) — reminding it of the finish protocol"
                plain_streak config.max_plain_replies;
              plain_reply_reminder
            end else
              continue_prompt_for config ~task ~used:(Session.turn_idx sess')
          in
          let sess'' = Prompt.(exec_in_session (user prompt) sess') in
          notify_step sess'';
          loop ~plain_streak sess''
        end
      end
    end
  in
  let sess_with_task = Prompt.(exec_in_session (user task) sess) in
  notify_step sess_with_task;
  loop ~plain_streak:0 sess_with_task

let run ?config ?on_turn ?on_step net clock sess task =
  run_generic ?config ?on_turn ?on_step
    (fun ?max_turns ?on_turn ?on_step s -> Session.run_conversations ?max_turns ?on_turn ?on_step net clock s)
    sess task

let run_stream ?config ?on_turn ?on_step net clock sess task ~on_token =
  run_generic ?config ?on_turn ?on_step
    (fun ?max_turns ?on_turn ?on_step s -> Session.run_conversations_stream ?max_turns ?on_turn ?on_step net clock s ~on_token)
    sess task

