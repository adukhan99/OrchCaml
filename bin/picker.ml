(** Interactive selection widgets for the REPL and the setup wizard.

    Caravan used to answer every "which one?" by printing a numbered table
    and asking the user to retype a number or a whole command — twelve
    providers to pick from by typing [7], a settings table you had to read
    and then re-enter as [/config set …]. This module is the one primitive
    behind all of those: arrow to a row, type to filter, Enter to choose.

    Everything degrades honestly when stdin is not a terminal: [select]
    prints the numbered list and reads a number, [confirm] reads a line,
    so pipelines and CI keep working unchanged. *)

open Caravan
open Ui

(* ── Items ────────────────────────────────────────────────────────────── *)

type 'a item = {
  value  : 'a;
  label  : string;   (** the name, matched against the filter *)
  hint   : string;   (** right-hand column, dim — a current value or status *)
  detail : string;   (** one line under the highlighted row *)
}

let item ?(hint = "") ?(detail = "") label value = { value; label; hint; detail }

(* ── Shared drawing ───────────────────────────────────────────────────── *)

(** Erase [n] lines below the cursor and return to the start of the line. *)
let erase_below n =
  if n > 0 then Printf.printf "\027[%dA" n;
  Printf.printf "\r\027[J"

let contains_ci ~needle haystack =
  let needle = String.lowercase_ascii needle in
  if needle = "" then true
  else
    let hay = String.lowercase_ascii haystack in
    let nl = String.length needle and hl = String.length hay in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
    go 0

(* ── select ───────────────────────────────────────────────────────────── *)

let numbered_fallback ?title items =
  (match title with Some t -> println_ansi (bold (yellow ("  " ^ t))) | None -> ());
  List.iteri (fun i it ->
    println_ansi (Printf.sprintf "  %s %s %s"
      (cyan (Printf.sprintf "[%2d]" (i + 1)))
      (pad_visible 24 (white it.label))
      (dim it.hint))) items;
  print_ansi (cyan (Printf.sprintf "  Select [1-%d]: " (List.length items)));
  flush stdout;
  match int_of_string_opt (String.trim (try input_line stdin with End_of_file -> "")) with
  | Some n when n >= 1 && n <= List.length items -> Some (List.nth items (n - 1)).value
  | _ -> None

type nav = { mutable query : string; mutable sel : int; mutable drawn : int }

(** Choose one item. [None] if the user cancels with Esc or Ctrl-C.

    [filter] (default true) lets the user narrow the list by typing;
    turn it off for short lists where typing should do nothing. *)
let select ?title ?(footer = "") ?(filter = true) (items : 'a item list) : 'a option =
  match items with
  | [] -> None
  | _ when not (Tty.is_tty ()) -> numbered_fallback ?title items
  | _ ->
    Tty.invalidate_size ();
    let st = { query = ""; sel = 0; drawn = 0 } in
    let visible () =
      if st.query = "" then items
      else List.filter (fun it ->
        contains_ci ~needle:st.query it.label
        || contains_ci ~needle:st.query it.hint) items
    in
    let max_rows () = max 3 (min 12 (Tty.rows () - 6)) in
    let draw () =
      erase_below st.drawn;
      let shown = visible () in
      let n = List.length shown in
      if st.sel >= n then st.sel <- max 0 (n - 1);
      let rows = max_rows () in
      (* Scroll so the selection stays in view. *)
      let top = if st.sel < rows then 0 else st.sel - rows + 1 in
      let window = List.filteri (fun i _ -> i >= top && i < top + rows) shown in
      let lines = ref 0 in
      let put s = Printf.printf "%s\n" s; incr lines in
      (match title with
       | Some t ->
         put (Printf.sprintf "  %s %s" (bold (yellow t))
                (if st.query = "" then "" else cyan ("/" ^ st.query)))
       | None ->
         if st.query <> "" then put (Printf.sprintf "  %s" (cyan ("/" ^ st.query))));
      if n = 0 then put (Printf.sprintf "  %s" (dim "no matches"))
      else
        List.iteri (fun i it ->
          let idx = top + i in
          let selected = idx = st.sel in
          put (Printf.sprintf " %s %s %s"
                 (if selected then cyan "▸" else " ")
                 (pad_visible 26 (if selected then bold (cyan it.label) else white it.label))
                 (dim it.hint))) window;
      (* Detail and scroll position belong to the highlighted row. *)
      (match List.nth_opt shown st.sel with
       | Some it when it.detail <> "" -> put (Printf.sprintf "    %s" (dim it.detail))
       | _ -> ());
      if n > rows then
        put (Printf.sprintf "    %s" (dim (Printf.sprintf "%d/%d" (st.sel + 1) n)));
      put (Printf.sprintf "  %s"
             (dim (if footer <> "" then footer
                   else if filter then "↑↓ move · type to filter · ⏎ select · esc cancel"
                   else "↑↓ move · ⏎ select · esc cancel")));
      st.drawn <- !lines;
      flush stdout
    in
    let result = ref None in
    Tty.with_raw_mode (fun () ->
      draw ();
      let finished = ref false in
      while not !finished do
        let shown = visible () in
        let n = List.length shown in
        (match Tty.read_key () with
         | Tty.Up -> if n > 0 then st.sel <- (st.sel + n - 1) mod n
         | Tty.Down -> if n > 0 then st.sel <- (st.sel + 1) mod n
         | Tty.Page_up -> st.sel <- max 0 (st.sel - max_rows ())
         | Tty.Page_down -> st.sel <- min (max 0 (n - 1)) (st.sel + max_rows ())
         | Tty.Home -> st.sel <- 0
         | Tty.End -> st.sel <- max 0 (n - 1)
         | Tty.Enter ->
           (match List.nth_opt shown st.sel with
            | Some it -> result := Some it.value
            | None -> ());
           finished := true
         | Tty.Esc | Tty.Eof | Tty.Ctrl 'C' | Tty.Ctrl 'G' -> finished := true
         | Tty.Backspace when filter ->
           let cs = Tty.chars_of_string st.query in
           let keep = max 0 (List.length cs - 1) in
           st.query <- Tty.string_of_chars (List.filteri (fun i _ -> i < keep) cs);
           st.sel <- 0
         | Tty.Char c when filter -> st.query <- st.query ^ c; st.sel <- 0
         | _ -> ());
        if not !finished then draw ()
      done;
      erase_below st.drawn;
      flush stdout);
    !result

(* ── confirm ──────────────────────────────────────────────────────────── *)

(** A yes/no question. Enter takes [default]. *)
let confirm ?(default = true) question =
  let suffix = if default then "[Y/n]" else "[y/N]" in
  if not (Tty.is_tty ()) then begin
    print_ansi (cyan (Printf.sprintf "  %s %s " question suffix));
    flush stdout;
    match String.lowercase_ascii
            (String.trim (try input_line stdin with End_of_file -> "")) with
    | "y" | "yes" -> true
    | "n" | "no" -> false
    | _ -> default
  end else begin
    print_ansi (cyan (Printf.sprintf "  %s %s " question suffix));
    flush stdout;
    let answer =
      Tty.with_raw_mode (fun () ->
        let rec loop () =
          match Tty.read_key () with
          | Tty.Char ("y" | "Y") -> true
          | Tty.Char ("n" | "N") -> false
          | Tty.Enter -> default
          | Tty.Esc | Tty.Eof | Tty.Ctrl 'C' -> false
          | _ -> loop ()
        in loop ())
    in
    println_ansi (if answer then green "yes" else yellow "no");
    answer
  end

(* ── prompt ───────────────────────────────────────────────────────────── *)

(** Read one line, offering [initial] as an editable starting value.
    [None] if the user cancels. *)
let prompt ?(initial = "") label =
  let p = Printf.sprintf "  %s " (cyan (label ^ ":")) in
  match Editor.read_line ~initial ~prompt:p ~commands:[] () with
  | None -> None
  | Some s -> Some (String.trim s)

(* ── secret ───────────────────────────────────────────────────────────── *)

(** Read a line without echoing it.

    Deliberately raw rather than [input_line] with echo off: the pickers
    and the line editor read stdin byte by byte, and a buffered channel
    reading alongside them swallows whatever it read ahead — so a pasted
    key, or fast typing, would eat the keystrokes meant for the next
    prompt. *)
let secret label =
  print_ansi (cyan label);
  flush stdout;
  if not (Tty.is_tty ()) then
    (try String.trim (input_line stdin) with End_of_file -> "")
  else begin
    let buf = ref [] in
    Tty.with_raw_mode (fun () ->
      let rec loop () =
        match Tty.read_key () with
        | Tty.Enter -> ()
        | Tty.Eof | Tty.Ctrl 'C' -> buf := []
        | Tty.Backspace -> (match !buf with _ :: t -> buf := t | [] -> ()); loop ()
        | Tty.Char c -> buf := c :: !buf; loop ()
        | _ -> loop ()
      in loop ());
    print_newline ();
    String.trim (String.concat "" (List.rev !buf))
  end

(* ── form ─────────────────────────────────────────────────────────────── *)

(** Fill in a set of named fields, one prompt per field, as described by
    [(key, label, placeholder, required)] — the shape
    [Config.editable_subagent_fields] already uses, so the REPL and the
    web cockpit describe the same form.

    Returns the non-empty fields, or [None] if the user cancels or leaves
    a required field blank. *)
let form (fields : (string * string * string * bool) list) =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | (key, label, placeholder, required) :: rest ->
      let label =
        if placeholder = "" then label
        else Printf.sprintf "%s %s" label (dim ("(" ^ placeholder ^ ")"))
      in
      (match prompt (if required then label else label ^ dim " ·optional") with
       | None -> None
       | Some "" when required ->
         println_ansi (red (Printf.sprintf "  %s is required — cancelled." key));
         None
       | Some "" -> go acc rest
       | Some v -> go ((key, v) :: acc) rest)
  in
  go [] fields
