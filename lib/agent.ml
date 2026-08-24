(** Autonomous agentic loops. *)

open Types

type agent_config = {
  max_turns       : int;
  continue_prompt : string;
  nudge           : bool;
      (** When true, budget-awareness nudges are appended to continue
          prompts at the halfway point and near exhaustion. *)
}

(** Read lazily so environment/config changes are honoured per run, not
    frozen at module-load time. *)
let default_config () = {
  max_turns       = Config.get_max_turns ();
  continue_prompt = "Please continue until you are finished. Use the 'finish' tool to signal completion.";
  nudge           = Config.get_nudge_enabled ();
}

(** Completion is judged from the outcome of the last conversation run
    only — never by scanning history. (Scanning history meant that once
    any task had finished, every subsequent task in the same session was
    instantly considered finished by its stale [finish] call.) *)
let is_finished (result : chat_message result_with_meta) =
  match result.finish_reason with
  | Some "finish_tool" -> true
  | Some "max_turns"   -> false
  | _ ->
    (* A plain reply with content and no pending tool calls counts as done. *)
    (match result.value.tool_calls with
     | None | Some [] -> String.trim result.value.content <> ""
     | Some _ -> false)

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
  let rec loop sess =
    if config.max_turns > 0 && Session.turn_idx sess >= config.max_turns then
      Error "Maximum turns reached without completion."
    else begin
      let (sess', result) = run_fn ?max_turns ?on_turn sess in
      notify_step sess';
      if is_finished result then
        Ok (sess', result)
      else if config.max_turns > 0 && Session.turn_idx sess' >= config.max_turns then
        Error "Maximum turns reached without completion."
      else
        let prompt = continue_prompt_for config ~task ~used:(Session.turn_idx sess') in
        let sess'' = Prompt.(exec_in_session (user prompt) sess') in
        notify_step sess'';
        loop sess''
    end
  in
  let sess_with_task = Prompt.(exec_in_session (user task) sess) in
  notify_step sess_with_task;
  loop sess_with_task

let run ?config ?on_turn ?on_step net clock sess task =
  run_generic ?config ?on_turn ?on_step
    (fun ?max_turns ?on_turn s -> Session.run_conversations ?max_turns ?on_turn net clock s)
    sess task

let run_stream ?config ?on_turn ?on_step net clock sess task ~on_token =
  run_generic ?config ?on_turn ?on_step
    (fun ?max_turns ?on_turn s -> Session.run_conversations_stream ?max_turns ?on_turn net clock s ~on_token)
    sess task

