(** Zero-dependency raw-mode line editor for the REPL.

    Why not minttea? It is built on the Riot actor runtime, which would
    contend with Eio for the scheduler; both are pre-0.1. This module
    gives us the pieces Caravan actually needs — cursor editing,
    persistent history, and a live slash-command palette — in a few
    hundred dependency-free lines.

    Features:
    - arrow keys / Home / End / Delete / Backspace, Ctrl/Alt-Left/Right, Alt-B/F, Ctrl-A/E/K/U/W/L
    - Up/Down history, persisted to ~/.caravan/history (0600)
    - a fish-style palette: typing '/' filters commands live below the
      input, and once a command is typed it offers that command's own
      argument candidates; Tab completes (cycling on ambiguity); Esc hides it
    - single-row horizontal scrolling (robust under any terminal width)
    - graceful fallback to [input_line] when stdin is not a TTY. *)

open Caravan

(* ── Terminal plumbing ────────────────────────────────────────────────── *)

let term_cols () =
  (* stty gives the live size; COLUMNS is a fallback; 80 the last resort. *)
  let from_stty () =
    try
      let ic = Unix.open_process_in "stty size 2>/dev/null" in
      let line = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);
      match String.split_on_char ' ' (String.trim line) with
      | [_; cols] -> int_of_string_opt cols
      | _ -> None
    with _ -> None
  in
  match from_stty () with
  | Some c when c > 20 -> c
  | _ ->
    (match Sys.getenv_opt "COLUMNS" with
     | Some c -> (try max 40 (int_of_string c) with _ -> 80)
     | None -> 80)

let with_raw_mode f =
  let open Unix in
  let attr = tcgetattr stdin in
  let raw = { attr with c_icanon = false; c_echo = false;
                        c_vmin = 1; c_vtime = 0; c_isig = false } in
  tcsetattr stdin TCSANOW raw;
  Fun.protect ~finally:(fun () -> tcsetattr stdin TCSANOW attr) f

let read_byte () =
  let b = Bytes.create 1 in
  match Unix.read Unix.stdin b 0 1 with
  | 0 -> None
  | _ -> Some (Bytes.get b 0)
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> None

(** One logical keypress. *)
type key =
  | Char of string      (* a UTF-8 character *)
  | Enter | Backspace | Delete | Tab | Esc
  | Up | Down | Left | Right | Home | End
  | Word_left | Word_right
  | Ctrl of char        (* Ctrl-A … Ctrl-Z by letter *)
  | Eof

let read_key () =
  match read_byte () with
  | None -> Eof
  | Some c ->
    let code = Char.code c in
    if c = '\n' || c = '\r' then Enter
    else if code = 127 || code = 8 then Backspace
    else if c = '\t' then Tab
    else if code = 4 then Eof                    (* Ctrl-D *)
    else if code = 27 then begin                 (* ESC sequence *)
      match read_byte () with
      | None -> Esc
      | Some ('[' | 'O') ->
        (* Collect parameter bytes until the final byte (@ … ~). *)
        let buf = Buffer.create 4 in
        let rec collect () =
          match read_byte () with
          | None -> Esc
          | Some ch when ch >= '@' && ch <= '~' && ch <> '[' ->
            let seq = Buffer.contents buf in
            (match ch, seq with
             | 'A', _  -> Up
             | 'B', _  -> Down
             | 'C', "" -> Right
             | 'C', _  -> Word_right
             | 'D', "" -> Left
             | 'D', _  -> Word_left
             | 'H', _  -> Home
             | 'F', _  -> End
             | '~', ("1" | "7") -> Home
             | '~', ("4" | "8") -> End
             | '~', "3" -> Delete
             | _ -> Esc)  (* unknown sequence: swallow *)
          | Some ch -> Buffer.add_char buf ch; collect ()
        in
        collect ()
      | Some ('b' | 'B') -> Word_left
      | Some ('f' | 'F') -> Word_right
      | Some '\027' ->
        (match read_byte () with
         | Some ('[' | 'O') ->
           (match read_byte () with
            | Some 'D' -> Word_left
            | Some 'C' -> Word_right
            | _ -> Esc)
         | _ -> Esc)
      | Some _ -> Esc
    end
    else if code < 32 then
      Ctrl (Char.chr (code + 64))                (* 1 → 'A', 3 → 'C', … *)
    else begin
      (* UTF-8: pull continuation bytes so a multi-byte char is one key. *)
      let extra =
        if code < 0x80 then 0
        else if code < 0xE0 then 1
        else if code < 0xF0 then 2
        else 3
      in
      let buf = Buffer.create 4 in
      Buffer.add_char buf c;
      for _ = 1 to extra do
        match read_byte () with
        | Some b -> Buffer.add_char buf b
        | None -> ()
      done;
      Char (Buffer.contents buf)
    end

(* ── History ──────────────────────────────────────────────────────────── *)

let history_max = 500

let history : string list ref = ref []   (* newest first *)
let history_loaded = ref false

let history_file () = Filename.concat (Config.caravan_dir ()) "history"

let load_history () =
  if not !history_loaded then begin
    history_loaded := true;
    try
      let ic = open_in (history_file ()) in
      let lines = ref [] in
      (try while true do lines := input_line ic :: !lines done
       with End_of_file -> ());
      close_in ic;
      history := !lines   (* file is oldest-first; reversed = newest-first *)
    with _ -> ()
  end

let append_history line =
  if String.trim line <> "" then begin
    (match !history with
     | last :: _ when last = line -> ()   (* skip consecutive duplicates *)
     | _ ->
       history := line :: !history;
       if List.length !history > history_max then
         history := List.filteri (fun i _ -> i < history_max) !history;
       (try
          let dir = Config.caravan_dir () in
          if not (Sys.file_exists dir) then Unix.mkdir dir 0o700;
          let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600 (history_file ()) in
          List.iter (fun l -> output_string oc l; output_char oc '\n')
            (List.rev !history);
          close_out oc
        with _ -> ()))
  end

(* ── The editor ───────────────────────────────────────────────────────── *)

type state = {
  mutable chars    : string list;  (* buffer as UTF-8 characters *)
  mutable cursor   : int;          (* index into chars *)
  mutable hist_pos : int;          (* -1 = editing fresh line *)
  mutable stash    : string list;  (* fresh line saved during history nav *)
  mutable tab_idx  : int;          (* completion cycling *)
  mutable palette_rows : int;      (* rows drawn below the input last time *)
}

let string_of_chars cs = String.concat "" cs

let chars_of_string s =
  let len = String.length s in
  let rec go i acc =
    if i >= len then List.rev acc
    else
      let c = Char.code s.[i] in
      let step = if c < 0x80 then 1 else if c < 0xE0 then 2
                 else if c < 0xF0 then 3 else 4 in
      let step = min step (len - i) in
      go (i + step) (String.sub s i step :: acc)
  in
  go 0 []

let is_word_char s =
  if String.length s <> 1 then true
  else
    match s.[0] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
    | _ -> false

let word_left st =
  let arr = Array.of_list st.chars in
  let len = Array.length arr in
  let i = ref (min st.cursor len) in
  while !i > 0 && not (is_word_char arr.(!i - 1)) do
    decr i
  done;
  while !i > 0 && is_word_char arr.(!i - 1) do
    decr i
  done;
  !i

let word_right st =
  let arr = Array.of_list st.chars in
  let len = Array.length arr in
  let i = ref (max 0 st.cursor) in
  while !i < len && not (is_word_char arr.(!i)) do
    incr i
  done;
  while !i < len && is_word_char arr.(!i) do
    incr i
  done;
  !i

(* ── The completion palette ───────────────────────────────────────────── *)

(** One row under the input: a command while the command is being typed,
    a candidate value once its arguments are.  [pr_insert] is the whole
    line Tab would write, or [""] for a row that is only a reminder. *)
type palette_row = {
  pr_label  : string;
  pr_args   : string;
  pr_doc    : string;
  pr_insert : string;
}

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(** Rows for the line as typed so far.  Argument candidates come from the
    command's own completer, so the palette knows about settings,
    providers and MCP servers without knowing what any of them are. *)
let palette_rows commands line =
  if String.length line = 0 || line.[0] <> '/' then []
  else
    match String.split_on_char ' ' line with
    | [] -> []
    | [name] ->
      List.filter (fun (c : Commands.t) -> starts_with ~prefix:name c.Commands.name)
        commands
      |> List.map (fun (c : Commands.t) ->
        { pr_label = c.Commands.name; pr_args = c.Commands.args;
          pr_doc = c.Commands.doc; pr_insert = c.Commands.name ^ " " })
    | name :: args ->
      match List.find_opt (fun (c : Commands.t) ->
              c.Commands.name = name || List.mem name c.Commands.aliases) commands with
      | None -> []
      | Some c ->
        let n = List.length args in
        let prefix = List.nth args (n - 1) in
        let typed = List.filteri (fun i _ -> i < n - 1) args in
        (match List.filter (starts_with ~prefix) (c.Commands.complete args) with
         | [] ->
           (* Nothing to offer: leave the command's own row as a reminder
              of what it takes. *)
           [ { pr_label = c.Commands.name; pr_args = c.Commands.args;
               pr_doc = c.Commands.doc; pr_insert = "" } ]
         | cands ->
           List.map (fun v ->
             { pr_label = v; pr_args = ""; pr_doc = "";
               pr_insert = String.concat " " ((name :: typed) @ [v]) ^ " " })
             cands)

let max_palette = 8

(** Redraw prompt + input (single row, horizontally scrolled) and the
    palette below; leave the cursor inside the input line. *)
let render ~prompt st ~commands =
  let cols = term_cols () in
  let pw = Ui.visible_width prompt in
  (* Horizontal window so prompt + slice + margins fit one row. *)
  let avail = max 8 (cols - pw - 2) in
  let n = List.length st.chars in
  let start =
    if n <= avail then 0
    else if st.cursor >= avail then st.cursor - avail + 1
    else 0
  in
  let visible =
    List.filteri (fun i _ -> i >= start && i < start + avail) st.chars
  in
  let shown = string_of_chars visible in
  let left_more = start > 0 in
  let right_more = start + avail < n in
  (* Wipe our previous rendering: cursor sits on the input row, so clear
     from line start downward. *)
  Printf.printf "\r\027[J";
  Printf.printf "%s%s%s%s"
    prompt
    (if left_more then Ui.dim "…" else "")
    shown
    (if right_more then Ui.dim "…" else "");
  (* Palette below. *)
  let matches = palette_rows commands (string_of_chars st.chars) in
  let matches = List.filteri (fun i _ -> i < max_palette) matches in
  let rows = List.length matches in
  List.iteri (fun i r ->
    let selected = (st.tab_idx > 0) && ((st.tab_idx - 1) mod rows = i) in
    let marker = if selected then Ui.cyan "▸" else " " in
    Printf.printf "\n%s %s %s %s"
      marker
      (if selected then Ui.bold (Ui.cyan r.pr_label) else Ui.cyan r.pr_label)
      (Ui.yellow r.pr_args)
      (Ui.dim r.pr_doc)
  ) matches;
  st.palette_rows <- rows;
  (* Return the cursor to its spot in the input row. *)
  if rows > 0 then Printf.printf "\027[%dA" rows;
  let cursor_col = pw + (if left_more then 1 else 0) + (st.cursor - start) in
  Printf.printf "\r\027[%dC" cursor_col;
  flush stdout

(** Erase the palette (called before returning the line). *)
let cleanup st =
  if st.palette_rows > 0 then begin
    Printf.printf "\027[J";
    st.palette_rows <- 0
  end;
  print_newline ();
  flush stdout

(** Read one line. Returns [None] on EOF (Ctrl-D on empty line). *)
let read_line ~prompt ~(commands : Commands.t list) () : string option =
  if not (Unix.isatty Unix.stdin) then
    (try Some (input_line stdin) with End_of_file -> None)
  else begin
    load_history ();
    let st = { chars = []; cursor = 0; hist_pos = -1; stash = [];
               tab_idx = 0; palette_rows = 0 } in
    let insert s =
      let before = List.filteri (fun i _ -> i < st.cursor) st.chars in
      let after  = List.filteri (fun i _ -> i >= st.cursor) st.chars in
      st.chars <- before @ [s] @ after;
      st.cursor <- st.cursor + 1
    in
    let delete_at i =
      if i >= 0 && i < List.length st.chars then
        st.chars <- List.filteri (fun j _ -> j <> i) st.chars
    in
    let set_line s =
      st.chars <- chars_of_string s;
      st.cursor <- List.length st.chars
    in
    let result = ref None in
    with_raw_mode (fun () ->
      render ~prompt st ~commands;
      let finished = ref false in
      while not !finished do
        let key = read_key () in
        (match key with
         | Enter ->
           cleanup st;
           let line = string_of_chars st.chars in
           append_history line;
           result := Some line;
           finished := true
         | Eof ->
           if st.chars = [] then begin
             cleanup st; result := None; finished := true
           end else begin
             (* Ctrl-D mid-line: delete char under cursor (readline habit). *)
             delete_at st.cursor;
             st.tab_idx <- 0
           end
         | Char c -> insert c; st.tab_idx <- 0
         | Backspace ->
           if st.cursor > 0 then begin
             delete_at (st.cursor - 1);
             st.cursor <- st.cursor - 1
           end;
           st.tab_idx <- 0
         | Delete -> delete_at st.cursor; st.tab_idx <- 0
         | Left -> if st.cursor > 0 then st.cursor <- st.cursor - 1
         | Right -> if st.cursor < List.length st.chars then st.cursor <- st.cursor + 1
         | Word_left -> st.cursor <- word_left st; st.tab_idx <- 0
         | Word_right -> st.cursor <- word_right st; st.tab_idx <- 0
         | Home | Ctrl 'A' -> st.cursor <- 0
         | End  | Ctrl 'E' -> st.cursor <- List.length st.chars
         | Ctrl 'C' ->
           (* Cancel the current line, keep the session. *)
           set_line ""; st.tab_idx <- 0; st.hist_pos <- -1
         | Ctrl 'K' ->
           st.chars <- List.filteri (fun i _ -> i < st.cursor) st.chars;
           st.tab_idx <- 0
         | Ctrl 'U' -> set_line ""; st.cursor <- 0; st.tab_idx <- 0
         | Ctrl 'W' ->
           (* Delete the word before the cursor. *)
           let rec back i =
             if i <= 0 then 0
             else if List.nth st.chars (i - 1) = " " && i <> st.cursor then i
             else back (i - 1)
           in
           let from = back st.cursor in
           st.chars <- List.filteri (fun i _ -> i < from || i >= st.cursor) st.chars;
           st.cursor <- from;
           st.tab_idx <- 0
         | Ctrl 'L' ->
           Printf.printf "\027[2J\027[H"; flush stdout
         | Up ->
           let h = !history in
           if st.hist_pos + 1 < List.length h then begin
             if st.hist_pos = -1 then st.stash <- st.chars;
             st.hist_pos <- st.hist_pos + 1;
             set_line (List.nth h st.hist_pos)
           end
         | Down ->
           if st.hist_pos >= 0 then begin
             st.hist_pos <- st.hist_pos - 1;
             if st.hist_pos = -1 then begin
               st.chars <- st.stash;
               st.cursor <- List.length st.chars
             end else
               set_line (List.nth !history st.hist_pos)
           end
         | Tab ->
           let line = string_of_chars st.chars in
           let matches = palette_rows commands line in
           let matches = List.filteri (fun i _ -> i < max_palette) matches in
           (* Reminder rows carry nothing to insert. *)
           (match List.filter (fun r -> r.pr_insert <> "") matches with
            | [] -> ()
            | [only] ->
              set_line only.pr_insert;
              st.tab_idx <- 0
            | many ->
              (* Cycle without the trailing space, so the next Tab keeps
                 cycling this same set instead of starting a new argument. *)
              st.tab_idx <- st.tab_idx + 1;
              let pick = List.nth many ((st.tab_idx - 1) mod List.length many) in
              set_line (String.trim pick.pr_insert))
         | Esc -> st.tab_idx <- 0
         | Ctrl _ -> ());
        if not !finished then render ~prompt st ~commands
      done);
    !result
  end
