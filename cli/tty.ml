(** Raw-mode terminal primitives, shared by the line editor and the picker.

    Named [Tty] rather than [Term] because [bin/main.ml] opens Cmdliner,
    whose own [Term] would shadow it.

    Everything here is dependency-free and deliberately small: keypress
    decoding, raw mode, and the terminal size. Both interactive widgets sit
    on top of it, so a fix to escape-sequence handling lands in one place. *)

(* ── Size ─────────────────────────────────────────────────────────────── *)

(* `stty size` costs a fork and an exec, so it is cached. It used to be
   called from the editor's redraw, i.e. twice per keystroke, which is
   what made typing on a long line feel sluggish. The cache is dropped on
   SIGWINCH where the signal is available, and whenever a widget starts. *)
let cached_size : (int * int) option ref = ref None

let invalidate_size () = cached_size := None

let sigwinch = 28   (* Linux/BSD; guarded below because it is not portable *)

let () =
  try Sys.set_signal sigwinch (Sys.Signal_handle (fun _ -> invalidate_size ()))
  with _ -> ()

let measure () =
  let from_stty () =
    try
      let ic = Unix.open_process_in "stty size 2>/dev/null" in
      let line = try input_line ic with End_of_file -> "" in
      ignore (Unix.close_process_in ic);
      match String.split_on_char ' ' (String.trim line) with
      | [r; c] ->
        (match int_of_string_opt r, int_of_string_opt c with
         | Some r, Some c when c > 20 -> Some (r, c)
         | _ -> None)
      | _ -> None
    with _ -> None
  in
  match from_stty () with
  | Some size -> size
  | None ->
    let env name default =
      match Sys.getenv_opt name with
      | Some v -> (try max default (int_of_string v) with _ -> default)
      | None -> default
    in
    (env "LINES" 24, env "COLUMNS" 80)

let size () =
  match !cached_size with
  | Some s -> s
  | None -> let s = measure () in cached_size := Some s; s

let cols () = snd (size ())
let rows () = fst (size ())

let is_tty () = Unix.isatty Unix.stdin

(* ── Raw mode ─────────────────────────────────────────────────────────── *)

let with_raw_mode f =
  let open Unix in
  let attr = tcgetattr stdin in
  let raw = { attr with c_icanon = false; c_echo = false;
                        c_vmin = 1; c_vtime = 0; c_isig = false } in
  tcsetattr stdin TCSANOW raw;
  Fun.protect ~finally:(fun () -> tcsetattr stdin TCSANOW attr) f

(* ── Bracketed paste ──────────────────────────────────────────────────── *)

(* With bracketed paste on, the terminal wraps pasted text in
   ESC[200~ … ESC[201~, which is the only way to tell a paste from
   typing. Without it, pasting a stack trace into the REPL submitted the
   first line as a turn and fired the rest at the model as further
   turns. *)
let paste_on () = print_string "\027[?2004h"; flush stdout
let paste_off () = print_string "\027[?2004l"; flush stdout

(* ── Keys ─────────────────────────────────────────────────────────────── *)

let read_byte () =
  let b = Bytes.create 1 in
  match Unix.read Unix.stdin b 0 1 with
  | 0 -> None
  | _ -> Some (Bytes.get b 0)
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> None

(** One logical keypress. *)
type key =
  | Char of string      (* a UTF-8 character *)
  | Paste of string     (* a bracketed paste, arriving as one event *)
  | Enter | Newline     (* submit; insert a line break *)
  | Backspace | Delete | Tab | Esc
  | Up | Down | Left | Right | Home | End
  | Page_up | Page_down
  | Word_left | Word_right
  | Ctrl of char        (* Ctrl-A … Ctrl-Z by letter *)
  | Eof

(** Read a pasted block, up to the ESC[201~ that closes it.  Line endings
    are normalised so a paste is one buffer however it was copied. *)
let read_paste () =
  let buf = Buffer.create 256 in
  let ended = ref false in
  while not !ended do
    match read_byte () with
    | None -> ended := true
    | Some '\027' ->
      (* Either the closing marker or a stray escape inside the paste. *)
      let tail = Buffer.create 8 in
      let rec collect () =
        match read_byte () with
        | None -> ended := true
        | Some ch ->
          Buffer.add_char tail ch;
          if ch >= '@' && ch <= '~' && Buffer.length tail > 1 then begin
            if Buffer.contents tail = "[201~" then ended := true
            else (Buffer.add_char buf '\027'; Buffer.add_buffer buf tail)
          end else if Buffer.length tail > 16 then begin
            Buffer.add_char buf '\027'; Buffer.add_buffer buf tail
          end else collect ()
      in
      collect ()
    | Some '\r' ->
      (* CR, or the CR of a CRLF: normalise either to one newline. *)
      Buffer.add_char buf '\n'
    | Some '\n' ->
      if Buffer.length buf = 0 || Buffer.nth buf (Buffer.length buf - 1) <> '\n'
      then Buffer.add_char buf '\n'
    | Some c -> Buffer.add_char buf c
  done;
  Paste (Buffer.contents buf)

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
             | '~', "5" -> Page_up
             | '~', "6" -> Page_down
             | '~', "200" -> read_paste ()
             | _ -> Esc)  (* unknown sequence: swallow *)
          | Some ch -> Buffer.add_char buf ch; collect ()
        in
        collect ()
      | Some ('\r' | '\n') -> Newline        (* Alt-Enter *)
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
    else if code = 15 then Newline               (* Ctrl-O, for terminals
                                                    that swallow Alt-Enter *)
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

(* ── UTF-8 helpers ────────────────────────────────────────────────────── *)

(** Split a string into UTF-8 characters. *)
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

let string_of_chars cs = String.concat "" cs

(** Display width of one UTF-8 character, for cursor arithmetic.

    [Ui.visible_width] deliberately counts every 3-byte sequence as one
    column so box-drawing characters line up in tables; a cursor needs the
    real answer, so this decodes the codepoint and applies the East Asian
    Wide ranges. Getting it wrong desynchronises the cursor from the text
    for the rest of the line. *)
let char_width s =
  let n = String.length s in
  if n = 0 then 0
  else
    let c = Char.code s.[0] in
    let at i = Char.code s.[i] land 0x3F in
    let cp =
      if c < 0x80 then c
      else if c < 0xE0 && n >= 2 then ((c land 0x1F) lsl 6) lor at 1
      else if c < 0xF0 && n >= 3 then ((c land 0x0F) lsl 12) lor (at 1 lsl 6) lor at 2
      else if n >= 4 then
        ((c land 0x07) lsl 18) lor (at 1 lsl 12) lor (at 2 lsl 6) lor at 3
      else 0xFFFD
    in
    if cp < 0x20 || (cp >= 0x7F && cp < 0xA0) then 0            (* control *)
    else if (cp >= 0x0300 && cp <= 0x036F)                      (* combining *)
         || (cp >= 0x200B && cp <= 0x200F) || cp = 0xFEFF then 0
    else if (cp >= 0x1100 && cp <= 0x115F)                      (* Hangul jamo *)
         || (cp >= 0x2E80 && cp <= 0xA4CF && cp <> 0x303F)      (* CJK *)
         || (cp >= 0xAC00 && cp <= 0xD7A3)                      (* Hangul *)
         || (cp >= 0xF900 && cp <= 0xFAFF)
         || (cp >= 0xFE30 && cp <= 0xFE6F)
         || (cp >= 0xFF00 && cp <= 0xFF60)                      (* fullwidth *)
         || (cp >= 0xFFE0 && cp <= 0xFFE6)
         || (cp >= 0x1F300 && cp <= 0x1FAFF)                    (* emoji *)
         || (cp >= 0x20000 && cp <= 0x3FFFD) then 2
    else 1

(** Display width of a string of plain (escape-free) text. *)
let text_width s = List.fold_left (fun a c -> a + char_width c) 0 (chars_of_string s)
