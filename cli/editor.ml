(** Zero-dependency raw-mode line editor for the REPL.

    Why not minttea? It is built on the Riot actor runtime, which would
    contend with Eio for the scheduler; both are pre-0.1. This module
    gives us the pieces Caravan actually needs — multi-line editing,
    persistent history, and a live slash-command palette — in a few
    hundred dependency-free lines, on the raw-mode primitives in [Tty].

    Features:
    - multi-line input: the buffer soft-wraps across rows, Alt-Enter (or
      Ctrl-O) inserts a line break, Enter submits
    - bracketed paste: a pasted block arrives as one buffer and one
      history entry, instead of N lines fired at the model as N turns
    - arrow keys / Home / End / Delete / Backspace, Ctrl/Alt-Left/Right,
      Alt-B/F, Ctrl-A/E/K/U/W/L, Ctrl-R reverse history search
    - Up/Down drive the palette when it is open, move between rows when
      the buffer has several, and walk history otherwise
    - history persisted to ~/.caravan/history (0600)
    - a fish-style palette: typing '/' filters commands live below the
      input, and once a command is typed it offers that command's own
      argument candidates; Tab completes (cycling on ambiguity)
    - graceful fallback to [input_line] when stdin is not a TTY. *)

open Caravan

(* Keypress decoding, raw mode, and the terminal size live in [Tty],
   shared with the picker. *)
open Tty

(* ── History ──────────────────────────────────────────────────────────── *)

let history_max = 500

let history : string list ref = ref []   (* newest first *)
let history_loaded = ref false

let history_file () = Filename.concat (Config.caravan_dir ()) "history"

(* Now that an entry can span lines, newlines are escaped on the way out
   so the file stays one-entry-per-line and older files still load. *)
let encode_entry s =
  let b = Buffer.create (String.length s + 8) in
  String.iter (function
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

let decode_entry s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i >= n then ()
    else if s.[i] = '\\' && i + 1 < n then begin
      (match s.[i + 1] with
       | 'n' -> Buffer.add_char b '\n'
       | '\\' -> Buffer.add_char b '\\'
       | c -> Buffer.add_char b '\\'; Buffer.add_char b c);
      go (i + 2)
    end else (Buffer.add_char b s.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

let load_history () =
  if not !history_loaded then begin
    history_loaded := true;
    try
      let ic = open_in (history_file ()) in
      let lines = ref [] in
      (try while true do lines := decode_entry (input_line ic) :: !lines done
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
          Config.mkdir_p (Config.caravan_dir ());
          let oc = open_out_gen [Open_creat; Open_trunc; Open_wronly] 0o600
                     (history_file ()) in
          List.iter (fun l -> output_string oc (encode_entry l); output_char oc '\n')
            (List.rev !history);
          close_out oc
        with _ -> ()))
  end

(** The most recent history entry containing [q], at index [from] or
    older. *)
let search_history q from =
  if q = "" then None
  else begin
    let re = Re.compile (Re.str (String.lowercase_ascii q)) in
    let rec go i = function
      | [] -> None
      | h :: rest ->
        if i >= from && Re.execp re (String.lowercase_ascii h) then Some (i, h)
        else go (i + 1) rest
    in
    go 0 !history
  end

(* ── Buffer ───────────────────────────────────────────────────────────── *)

(* A growable array of UTF-8 characters.  The buffer used to be a
   [string list] rebuilt by two [List.filteri] calls on every keystroke
   and indexed with [List.nth] inside loops, which made editing a long
   line quadratic — and a pasted one unusable. *)
module Buf = struct
  type t = {
    mutable chars : string array;
    mutable len   : int;
  }

  let create () = { chars = Array.make 64 ""; len = 0 }

  let reserve b extra =
    let need = b.len + extra in
    if need > Array.length b.chars then begin
      let cap = ref (max 64 (Array.length b.chars)) in
      while !cap < need do cap := !cap * 2 done;
      let bigger = Array.make !cap "" in
      Array.blit b.chars 0 bigger 0 b.len;
      b.chars <- bigger
    end

  let get b i = b.chars.(i)
  let length b = b.len
  let clear b = b.len <- 0

  let insert b pos items =
    let k = List.length items in
    if k > 0 then begin
      reserve b k;
      Array.blit b.chars pos b.chars (pos + k) (b.len - pos);
      List.iteri (fun i c -> b.chars.(pos + i) <- c) items;
      b.len <- b.len + k
    end

  (** Remove up to [n] characters starting at [pos]. *)
  let delete b pos n =
    let n = min n (b.len - pos) in
    if n > 0 && pos >= 0 then begin
      Array.blit b.chars (pos + n) b.chars pos (b.len - pos - n);
      b.len <- b.len - n
    end

  let contents b =
    let bf = Buffer.create b.len in
    for i = 0 to b.len - 1 do Buffer.add_string bf b.chars.(i) done;
    Buffer.contents bf

  let set b s = clear b; insert b 0 (chars_of_string s)
end

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
  if String.length line = 0 || line.[0] <> '/' || String.contains line '\n' then []
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

(* ── Editor state ─────────────────────────────────────────────────────── *)

type mode =
  | Editing
  | Searching of { query : string; from : int }   (** Ctrl-R *)

type state = {
  buf                  : Buf.t;
  mutable cursor       : int;          (* index into the buffer *)
  mutable hist_pos     : int;          (* -1 = editing a fresh line *)
  mutable stash        : string;       (* fresh line saved during history nav *)
  mutable sel          : int option;   (* highlighted palette row *)
  mutable mode         : mode;
  mutable last_cur_row : int;          (* where the cursor was left last draw *)
}

(* ── Layout ───────────────────────────────────────────────────────────── *)

(* The buffer is laid out into display rows: an explicit newline always
   breaks a row, and a row also breaks when the next character would not
   fit.  Every row after the first is indented to the prompt's width, so
   the text stays in one column.

   This replaces a single row scrolled horizontally, which is a poor fit
   for an agentic harness — the things people type at one are long, and
   the things they paste are longer. *)
type layout = {
  rows    : string list;   (** row contents, without the prefix *)
  nrows   : int;
  cur_row : int;
  cur_col : int;           (** display column within the row *)
}

let layout st ~width =
  let rows = ref [] in
  let row = Buffer.create 80 in
  let col = ref 0 in
  let cur_row = ref 0 and cur_col = ref 0 in
  let row_idx = ref 0 in
  let break () =
    rows := Buffer.contents row :: !rows;
    Buffer.clear row;
    col := 0;
    incr row_idx
  in
  let n = Buf.length st.buf in
  for i = 0 to n - 1 do
    if i = st.cursor then (cur_row := !row_idx; cur_col := !col);
    let c = Buf.get st.buf i in
    if c = "\n" then break ()
    else begin
      let w = char_width c in
      if !col + w > width && !col > 0 then break ();
      Buffer.add_string row c;
      col := !col + w
    end
  done;
  if st.cursor >= n then (cur_row := !row_idx; cur_col := !col);
  rows := Buffer.contents row :: !rows;
  let rows = List.rev !rows in
  { rows; nrows = List.length rows; cur_row = !cur_row; cur_col = !cur_col }

let text_width_for prompt = Ui.visible_width prompt

(* One spare column: a row that exactly fills the terminal makes some
   emulators wrap on their own and swallow a line. *)
let wrap_width prompt = max 8 (Tty.cols () - text_width_for prompt - 1)

(* ── Rendering ────────────────────────────────────────────────────────── *)

let render ~prompt st ~commands =
  let pw = text_width_for prompt in
  let l = layout st ~width:(wrap_width prompt) in
  let indent = String.make pw ' ' in
  let out = Buffer.create 512 in
  (* Return to the top of what was drawn last time and clear from there. *)
  if st.last_cur_row > 0 then
    Buffer.add_string out (Printf.sprintf "\027[%dA" st.last_cur_row);
  Buffer.add_string out "\r\027[J";
  List.iteri (fun i content ->
    if i > 0 then Buffer.add_char out '\n';
    Buffer.add_string out (if i = 0 then prompt else indent);
    Buffer.add_string out content) l.rows;
  (* Rows under the input: the search prompt, or the palette. *)
  let extra =
    match st.mode with
    | Searching { query; _ } ->
      [Printf.sprintf "  %s%s%s"
         (Ui.dim "(reverse-i-search) ") (Ui.cyan query)
         (if query <> "" && search_history query 0 = None
          then Ui.red "   no match" else "")]
    | Editing ->
      let matches =
        palette_rows commands (Buf.contents st.buf)
        |> List.filteri (fun i _ -> i < max_palette)
      in
      (match st.sel with
       | Some i when i >= List.length matches -> st.sel <- None
       | _ -> ());
      List.mapi (fun i r ->
        let selected = st.sel = Some i in
        Printf.sprintf "%s %s %s %s"
          (if selected then Ui.cyan " ▸" else "  ")
          (if selected then Ui.bold (Ui.cyan r.pr_label) else Ui.cyan r.pr_label)
          (Ui.yellow r.pr_args) (Ui.dim r.pr_doc)) matches
  in
  List.iter (fun line ->
    Buffer.add_char out '\n'; Buffer.add_string out line) extra;
  (* Put the cursor back where the text says it is. *)
  let below = (l.nrows + List.length extra - 1) - l.cur_row in
  if below > 0 then Buffer.add_string out (Printf.sprintf "\027[%dA" below);
  Buffer.add_string out "\r";
  let col = pw + l.cur_col in
  if col > 0 then Buffer.add_string out (Printf.sprintf "\027[%dC" col);
  st.last_cur_row <- l.cur_row;
  print_string (Buffer.contents out);
  flush stdout

(** Leave the submitted text on screen and clear everything under it. *)
let cleanup ~prompt st =
  let pw = text_width_for prompt in
  let l = layout st ~width:(wrap_width prompt) in
  let indent = String.make pw ' ' in
  let out = Buffer.create 256 in
  if st.last_cur_row > 0 then
    Buffer.add_string out (Printf.sprintf "\027[%dA" st.last_cur_row);
  Buffer.add_string out "\r\027[J";
  List.iteri (fun i content ->
    if i > 0 then Buffer.add_char out '\n';
    Buffer.add_string out (if i = 0 then prompt else indent);
    Buffer.add_string out content) l.rows;
  Buffer.add_char out '\n';
  st.last_cur_row <- 0;
  print_string (Buffer.contents out);
  flush stdout

(* ── Motion ───────────────────────────────────────────────────────────── *)

let is_word_char s =
  String.length s = 1 &&
  (match s.[0] with 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false)

let word_left st =
  let i = ref (min st.cursor (Buf.length st.buf)) in
  while !i > 0 && not (is_word_char (Buf.get st.buf (!i - 1))) do decr i done;
  while !i > 0 && is_word_char (Buf.get st.buf (!i - 1)) do decr i done;
  !i

let word_right st =
  let n = Buf.length st.buf in
  let i = ref (max 0 st.cursor) in
  while !i < n && not (is_word_char (Buf.get st.buf !i)) do incr i done;
  while !i < n && is_word_char (Buf.get st.buf !i) do incr i done;
  !i

(** Start of the logical line the cursor sits on. *)
let line_start st =
  let i = ref st.cursor in
  while !i > 0 && Buf.get st.buf (!i - 1) <> "\n" do decr i done;
  !i

(** End of the logical line the cursor sits on. *)
let line_end st =
  let n = Buf.length st.buf in
  let i = ref st.cursor in
  while !i < n && Buf.get st.buf !i <> "\n" do incr i done;
  !i

(** Buffer index at display row [target], column [col] or the end of that
    row — the position the cursor lands on when moving up or down a row. *)
let index_at st ~width ~target ~col =
  let row = ref 0 and c = ref 0 in
  let n = Buf.length st.buf in
  let found = ref None in
  let i = ref 0 in
  while !found = None && !i <= n do
    if !row = target && (!c >= col || !i = n || Buf.get st.buf !i = "\n")
    then found := Some !i
    else if !i < n then begin
      let ch = Buf.get st.buf !i in
      if ch = "\n" then (incr row; c := 0)
      else begin
        let w = char_width ch in
        if !c + w > width && !c > 0 then (incr row; c := w) else c := !c + w
      end
    end;
    incr i
  done;
  match !found with Some i -> min i n | None -> n

(* ── Reading a line ───────────────────────────────────────────────────── *)

(** Read one line. [initial] pre-fills the buffer, with the cursor at its
    end, so a caller can offer the current value for editing.  Returns
    [None] on EOF (Ctrl-D on an empty buffer). *)
let read_line ?(initial = "") ~prompt ~(commands : Commands.t list) () : string option =
  if not (Tty.is_tty ()) then
    (try Some (input_line stdin) with End_of_file -> None)
  else begin
    load_history ();
    Tty.invalidate_size ();
    let st = { buf = Buf.create (); cursor = 0; hist_pos = -1; stash = "";
               sel = None; mode = Editing; last_cur_row = 0 } in
    Buf.set st.buf initial;
    st.cursor <- Buf.length st.buf;
    let set_line s = Buf.set st.buf s; st.cursor <- Buf.length st.buf in
    let insert_text s =
      let cs = chars_of_string s in
      Buf.insert st.buf st.cursor cs;
      st.cursor <- st.cursor + List.length cs;
      st.sel <- None
    in
    let result = ref None in
    Tty.paste_on ();
    Fun.protect ~finally:Tty.paste_off (fun () ->
      Tty.with_raw_mode (fun () ->
        render ~prompt st ~commands;
        let finished = ref false in
        while not !finished do
          let width = wrap_width prompt in
          (* Only rows Tab or Enter could insert count as a selection. *)
          let matches =
            match st.mode with
            | Searching _ -> []
            | Editing ->
              palette_rows commands (Buf.contents st.buf)
              |> List.filteri (fun i _ -> i < max_palette)
              |> List.filter (fun r -> r.pr_insert <> "")
          in
          (* The palette takes Up/Down only while the user is composing.
             Once they have stepped into history, Up keeps walking back —
             otherwise recalling any slash command would trap them in its
             completion list. Tab still completes either way. *)
          let palette_takes_arrows = matches <> [] && st.hist_pos = -1 in
          let key = Tty.read_key () in
          (match st.mode, key with
           (* ── reverse history search ── *)
           | Searching { query; from }, Char c ->
             let query = query ^ c in
             st.mode <- Searching { query; from };
             (match search_history query from with
              | Some (_, h) -> set_line h
              | None -> ())
           | Searching { query; from }, Backspace ->
             let cs = chars_of_string query in
             let keep = max 0 (List.length cs - 1) in
             let query = string_of_chars (List.filteri (fun i _ -> i < keep) cs) in
             st.mode <- Searching { query; from };
             (match search_history query 0 with
              | Some (_, h) -> set_line h
              | None -> ())
           | Searching { query; from }, Ctrl 'R' ->
             (match search_history query (from + 1) with
              | Some (i, h) -> st.mode <- Searching { query; from = i }; set_line h
              | None -> ())
           | Searching _, (Esc | Ctrl 'C' | Ctrl 'G') ->
             (* Cancel: put back whatever was being typed. *)
             st.mode <- Editing;
             set_line st.stash
           | Searching _, Enter ->
             (* Accept and run, as readline does. *)
             st.mode <- Editing;
             let line = Buf.contents st.buf in
             cleanup ~prompt st;
             append_history line;
             result := Some line;
             finished := true
           | Searching _, _ ->
             (* Any other key accepts the match and resumes editing. *)
             st.mode <- Editing

           (* ── editing ── *)
           | Editing, Enter ->
             (match st.sel with
              | Some i when i < List.length matches ->
                (* Take the highlighted completion rather than submitting
                   a half-typed command. *)
                set_line (List.nth matches i).pr_insert;
                st.sel <- None
              | _ ->
                let line = Buf.contents st.buf in
                cleanup ~prompt st;
                append_history line;
                result := Some line;
                finished := true)
           | Editing, Newline -> insert_text "\n"
           | Editing, Paste s -> insert_text s
           | Editing, Char c -> insert_text c
           | Editing, Eof ->
             if Buf.length st.buf = 0 then begin
               cleanup ~prompt st; result := None; finished := true
             end else begin
               (* Ctrl-D mid-line: delete the character under the cursor. *)
               Buf.delete st.buf st.cursor 1; st.sel <- None
             end
           | Editing, Backspace ->
             if st.cursor > 0 then begin
               Buf.delete st.buf (st.cursor - 1) 1;
               st.cursor <- st.cursor - 1
             end;
             st.sel <- None
           | Editing, Delete -> Buf.delete st.buf st.cursor 1; st.sel <- None
           | Editing, Left -> if st.cursor > 0 then st.cursor <- st.cursor - 1
           | Editing, Right ->
             if st.cursor < Buf.length st.buf then st.cursor <- st.cursor + 1
           | Editing, Word_left -> st.cursor <- word_left st; st.sel <- None
           | Editing, Word_right -> st.cursor <- word_right st; st.sel <- None
           | Editing, (Home | Ctrl 'A') -> st.cursor <- line_start st
           | Editing, (End | Ctrl 'E') -> st.cursor <- line_end st
           | Editing, Ctrl 'C' ->
             (* Cancel the current line, keep the session. *)
             set_line ""; st.sel <- None; st.hist_pos <- -1
           | Editing, Ctrl 'K' ->
             Buf.delete st.buf st.cursor (line_end st - st.cursor);
             st.sel <- None
           | Editing, Ctrl 'U' ->
             let start = line_start st in
             Buf.delete st.buf start (st.cursor - start);
             st.cursor <- start;
             st.sel <- None
           | Editing, Ctrl 'W' ->
             let start = word_left st in
             Buf.delete st.buf start (st.cursor - start);
             st.cursor <- start;
             st.sel <- None
           | Editing, Ctrl 'L' ->
             Printf.printf "\027[2J\027[H";
             st.last_cur_row <- 0;
             flush stdout
           | Editing, Ctrl 'R' ->
             st.stash <- Buf.contents st.buf;
             st.mode <- Searching { query = ""; from = 0 }
           | Editing, Up ->
             let l = layout st ~width in
             if palette_takes_arrows then
               st.sel <- Some (match st.sel with
                 | None -> List.length matches - 1
                 | Some i -> (i + List.length matches - 1) mod List.length matches)
             else if l.cur_row > 0 then
               st.cursor <- index_at st ~width ~target:(l.cur_row - 1) ~col:l.cur_col
             else begin
               let h = !history in
               if st.hist_pos + 1 < List.length h then begin
                 if st.hist_pos = -1 then st.stash <- Buf.contents st.buf;
                 st.hist_pos <- st.hist_pos + 1;
                 set_line (List.nth h st.hist_pos)
               end
             end
           | Editing, Down ->
             let l = layout st ~width in
             if palette_takes_arrows then
               st.sel <- Some (match st.sel with
                 | None -> 0
                 | Some i -> (i + 1) mod List.length matches)
             else if l.cur_row < l.nrows - 1 then
               st.cursor <- index_at st ~width ~target:(l.cur_row + 1) ~col:l.cur_col
             else if st.hist_pos >= 0 then begin
               st.hist_pos <- st.hist_pos - 1;
               if st.hist_pos = -1 then set_line st.stash
               else set_line (List.nth !history st.hist_pos)
             end
           | Editing, Tab ->
             (match matches with
              | [] -> ()
              | [only] -> set_line only.pr_insert; st.sel <- None
              | many ->
                (* Cycle without the trailing space, so the next Tab keeps
                   cycling this set instead of starting a new argument. *)
                let i = match st.sel with
                  | None -> 0
                  | Some i -> (i + 1) mod List.length many
                in
                st.sel <- Some i;
                set_line (String.trim (List.nth many i).pr_insert))
           | Editing, Esc -> st.sel <- None
           | Editing, (Page_up | Page_down | Ctrl _) -> ());
          if not !finished then render ~prompt st ~commands
        done));
    !result
  end
