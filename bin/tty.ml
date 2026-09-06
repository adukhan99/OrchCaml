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
  | Enter | Backspace | Delete | Tab | Esc
  | Up | Down | Left | Right | Home | End
  | Page_up | Page_down
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
             | '~', "5" -> Page_up
             | '~', "6" -> Page_down
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
