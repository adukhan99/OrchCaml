(** Shared terminal toy-kit for the Caravan demos.

    A tiny cell-buffer renderer: every demo paints into a [canvas] of
    styled cells and hands it to [present], which emits one flicker-free
    frame.  Nothing here depends on the Caravan library — these are
    self-contained eye-candy programs, so they can never break the real
    build.  Colours come from a named [palette] so the demo runner can
    re-skin every demo from the outside via [CARAVAN_DEMO_PALETTE]. *)

(* ── colour ───────────────────────────────────────────────────────────── *)

type rgb = { r : int; g : int; b : int }

let clamp v = if v < 0 then 0 else if v > 255 then 255 else v
let mk r g b = { r = clamp r; g = clamp g; b = clamp b }

let hex s =
  let s = if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1) else s in
  let v = int_of_string ("0x" ^ s) in
  mk ((v lsr 16) land 0xff) ((v lsr 8) land 0xff) (v land 0xff)

let lerp a b t =
  let t = if t < 0. then 0. else if t > 1. then 1. else t in
  let f x y = int_of_float (float_of_int x +. (float_of_int (y - x)) *. t) in
  mk (f a.r b.r) (f a.g b.g) (f a.b b.b)

let shade c k =
  mk (int_of_float (float_of_int c.r *. k))
     (int_of_float (float_of_int c.g *. k))
     (int_of_float (float_of_int c.b *. k))

let mix = lerp

(** Sample a multi-stop gradient at [t] in 0..1. *)
let ramp (stops : rgb array) t =
  let n = Array.length stops in
  if n = 0 then mk 0 0 0
  else if n = 1 then stops.(0)
  else begin
    let t = if t < 0. then 0. else if t > 1. then 1. else t in
    let x = t *. float_of_int (n - 1) in
    let i = int_of_float (floor x) in
    let i = if i >= n - 1 then n - 2 else if i < 0 then 0 else i in
    lerp stops.(i) stops.(i + 1) (x -. float_of_int i)
  end

(* ── capabilities ─────────────────────────────────────────────────────── *)

let getenv k = Sys.getenv_opt k

let truecolor =
  match getenv "CARAVAN_DEMO_TRUECOLOR" with
  | Some "0" -> false
  | Some _ -> true
  | None ->
    (match getenv "COLORTERM" with
     | Some ("truecolor" | "24bit") -> true
     | _ -> false)

(** Glyph safety: [1] swaps exotic codepoints for ASCII-ish stand-ins. *)
let ascii_only = match getenv "CARAVAN_DEMO_ASCII" with Some ("1" | "yes" | "true") -> true | _ -> false

let to256 c =
  if abs (c.r - c.g) < 8 && abs (c.g - c.b) < 8 then begin
    let l = (c.r + c.g + c.b) / 3 in
    if l < 8 then 16 else if l > 248 then 231 else 232 + ((l - 8) * 24 / 240)
  end else
    16 + (36 * (c.r * 5 / 255)) + (6 * (c.g * 5 / 255)) + (c.b * 5 / 255)

let sgr_fg c =
  if truecolor then Printf.sprintf "\027[38;2;%d;%d;%dm" c.r c.g c.b
  else Printf.sprintf "\027[38;5;%dm" (to256 c)

let sgr_bg c =
  if truecolor then Printf.sprintf "\027[48;2;%d;%d;%dm" c.r c.g c.b
  else Printf.sprintf "\027[48;5;%dm" (to256 c)

(* ── palettes ─────────────────────────────────────────────────────────── *)

type palette = {
  name    : string;
  desc    : string;
  bg      : rgb;
  sky     : rgb array;   (** zenith → horizon *)
  sand    : rgb array;   (** horizon → foreground *)
  ink     : rgb array;   (** logo / headline gradient *)
  title   : rgb;
  accent  : rgb;
  text    : rgb;
  dim     : rgb;
  border  : rgb;
  tool    : rgb;
  ok      : rgb;
  warn    : rgb;
  err     : rgb;
  glow    : rgb;         (** sun, fire, highlight *)
  shadow  : rgb;
}

let palettes = [
  { name = "hermes"; desc = "Nous gold on midnight — the Hermes Agent house colours";
    bg = hex "#0B0A0F";
    sky  = [| hex "#0B0A0F"; hex "#161225"; hex "#2E2039"; hex "#6B3F2E" |];
    sand = [| hex "#7A5232"; hex "#A9713F"; hex "#C99257"; hex "#E3B97A" |];
    ink  = [| hex "#FFF3B0"; hex "#FFD700"; hex "#FFBF00"; hex "#CD7F32"; hex "#B8860B" |];
    title = hex "#FFD700"; accent = hex "#FFBF00"; text = hex "#E8DCC0"; dim = hex "#6E6455";
    border = hex "#5A4A2A"; tool = hex "#FFBF00"; ok = hex "#9BD17A"; warn = hex "#FFC857";
    err = hex "#E5534B"; glow = hex "#FFE79A"; shadow = hex "#2A1B0E" };

  { name = "lapis"; desc = "Lapis lazuli & gold leaf — Ur, third dynasty";
    bg = hex "#070C1A";
    sky  = [| hex "#050914"; hex "#0E1D3D"; hex "#1B3A6B"; hex "#2F5E93" |];
    sand = [| hex "#8A6A3A"; hex "#B08348"; hex "#C79A5B"; hex "#DCB77A" |];
    ink  = [| hex "#FFF1C1"; hex "#E8B923"; hex "#C9922B"; hex "#8E6A2A"; hex "#3E5C8C" |];
    title = hex "#E8B923"; accent = hex "#5C8AC6"; text = hex "#D9E2F2"; dim = hex "#4A5B78";
    border = hex "#2A4472"; tool = hex "#E8B923"; ok = hex "#79C9A8"; warn = hex "#E8B923";
    err = hex "#D4574E"; glow = hex "#FFE9A8"; shadow = hex "#0A1226" };

  { name = "ember"; desc = "Sunset over the erg — high-contrast orange";
    bg = hex "#150A0C";
    sky  = [| hex "#1B0B1E"; hex "#4A1042"; hex "#A32E3C"; hex "#F7931E" |];
    sand = [| hex "#8C3A16"; hex "#C1440E"; hex "#E1651B"; hex "#FFC857" |];
    ink  = [| hex "#FFE9C4"; hex "#FFC857"; hex "#FF6B35"; hex "#C1440E"; hex "#6E1F0B" |];
    title = hex "#FF6B35"; accent = hex "#FFC857"; text = hex "#F6DCC4"; dim = hex "#7A5044";
    border = hex "#6E2A18"; tool = hex "#FF8C42"; ok = hex "#B7D46A"; warn = hex "#FFC857";
    err = hex "#FF4D4D"; glow = hex "#FFEFC0"; shadow = hex "#2A0E08" };

  { name = "oasis"; desc = "Jade water, palm shade, cool sand";
    bg = hex "#04120F";
    sky  = [| hex "#04120F"; hex "#0A2A2C"; hex "#0E4749"; hex "#2C7A73" |];
    sand = [| hex "#6E6141"; hex "#9C8A55"; hex "#C4A86A"; hex "#E9C46A" |];
    ink  = [| hex "#E8FFD9"; hex "#A3E635"; hex "#3FBF8F"; hex "#16A085"; hex "#0E4749" |];
    title = hex "#A3E635"; accent = hex "#16A085"; text = hex "#D6E8DA"; dim = hex "#4E6A5E";
    border = hex "#1E5A4E"; tool = hex "#3FBF8F"; ok = hex "#A3E635"; warn = hex "#E9C46A";
    err = hex "#E76F51"; glow = hex "#DFFFB0"; shadow = hex "#05201A" };

  { name = "nous"; desc = "Psychedelic duotone — magenta / cyan CRT bloom";
    bg = hex "#05020C";
    sky  = [| hex "#05020C"; hex "#210A45"; hex "#5B1382"; hex "#B31E86" |];
    sand = [| hex "#3A0B52"; hex "#7C1E8E"; hex "#C42C86"; hex "#FF2E97" |];
    ink  = [| hex "#C6FF00"; hex "#00E5FF"; hex "#7C4DFF"; hex "#FF2E97"; hex "#3B0A57" |];
    title = hex "#FF2E97"; accent = hex "#00E5FF"; text = hex "#E6DCFF"; dim = hex "#6A4A8A";
    border = hex "#5B1382"; tool = hex "#00E5FF"; ok = hex "#C6FF00"; warn = hex "#FFD54F";
    err = hex "#FF3D57"; glow = hex "#FFFFFF"; shadow = hex "#12042A" };

  { name = "phosphor"; desc = "Amber CRT monochrome — one hue, sixteen levels";
    bg = hex "#0A0600";
    sky  = [| hex "#0A0600"; hex "#1A1000"; hex "#2E1D00"; hex "#4A3000" |];
    sand = [| hex "#3A2600"; hex "#5E3E00"; hex "#8A5C00"; hex "#B87C00" |];
    ink  = [| hex "#FFE0A0"; hex "#FFB000"; hex "#C88600"; hex "#7A5200"; hex "#3A2600" |];
    title = hex "#FFB000"; accent = hex "#FFC94D"; text = hex "#D89A2E"; dim = hex "#6B4A0C";
    border = hex "#5E3E00"; tool = hex "#FFB000"; ok = hex "#FFD37A"; warn = hex "#FFC94D";
    err = hex "#FF7A18"; glow = hex "#FFF0C8"; shadow = hex "#160D00" };

  { name = "bone"; desc = "Sun-bleached ivory — greyscale-safe, printer friendly";
    bg = hex "#0E0D0B";
    sky  = [| hex "#0E0D0B"; hex "#232019"; hex "#3A342E"; hex "#5B534A" |];
    sand = [| hex "#4A443C"; hex "#6E655A"; hex "#8A7B6B"; hex "#B8AC9B" |];
    ink  = [| hex "#F2E9DE"; hex "#CFC3B3"; hex "#A2947F"; hex "#6E655A"; hex "#3A342E" |];
    title = hex "#F2E9DE"; accent = hex "#CFC3B3"; text = hex "#CFC3B3"; dim = hex "#6E655A";
    border = hex "#4A443C"; tool = hex "#F2E9DE"; ok = hex "#CFC3B3"; warn = hex "#D8C89E";
    err = hex "#D89A8A"; glow = hex "#FFFFFF"; shadow = hex "#1A1815" };
]

let palette_names = List.map (fun p -> p.name) palettes

let find_palette n =
  match List.find_opt (fun p -> p.name = n) palettes with
  | Some p -> Some p
  | None -> None

(** Resolve the palette: [CARAVAN_DEMO_PALETTE] wins, else the demo's default. *)
let active ?(default = "hermes") () =
  let want = match getenv "CARAVAN_DEMO_PALETTE" with Some s when s <> "" -> s | _ -> default in
  match find_palette want with
  | Some p -> p
  | None -> (match find_palette default with Some p -> p | None -> List.hd palettes)

(* ── terminal size ────────────────────────────────────────────────────── *)

let resized = ref false

let query_size () =
  let via_stty () =
    let ic = Unix.open_process_in "stty size 2>/dev/null < /dev/tty" in
    let line = try Some (input_line ic) with _ -> None in
    ignore (Unix.close_process_in ic);
    match line with
    | Some l -> (try Some (Scanf.sscanf l " %d %d" (fun r c -> (c, r))) with _ -> None)
    | None -> None
  in
  let via_env () =
    match getenv "COLUMNS", getenv "LINES" with
    | Some c, Some r -> (try Some (int_of_string c, int_of_string r) with _ -> None)
    | _ -> None
  in
  let c, r = match via_stty () with
    | Some s -> s
    | None -> (match via_env () with Some s -> s | None -> (80, 24))
  in
  (max 20 c, max 8 r)

let install_winch () =
  try Sys.set_signal 28 (Sys.Signal_handle (fun _ -> resized := true)) with _ -> ()

(* ── canvas ───────────────────────────────────────────────────────────── *)

type cell = {
  ch    : string;
  fg    : rgb option;
  bg    : rgb option;
  bold  : bool;
  (** [risky] marks a glyph whose advance width the terminal may disagree
      about (cuneiform, rare symbols).  Rows containing one are painted
      cell-by-cell with absolute cursor moves so the grid can't drift. *)
  risky : bool;
}

let blank = { ch = " "; fg = None; bg = None; bold = false; risky = false }

type canvas = { w : int; h : int; cells : cell array }

let create w h = { w; h; cells = Array.make (w * h) blank }
let clear cv = Array.fill cv.cells 0 (Array.length cv.cells) blank

let fill cv ?fg ?bg ?(bold = false) ch =
  Array.fill cv.cells 0 (Array.length cv.cells) { ch; fg; bg; bold; risky = false }

let inside cv x y = x >= 0 && y >= 0 && x < cv.w && y < cv.h

(* ── ascii fallback ───────────────────────────────────────────────────── *)

(* [CARAVAN_DEMO_ASCII=1] is for terminals whose font has no cuneiform or
   braille.  Block and box-drawing glyphs are near-universal and carry most
   of the picture, so they are deliberately left alone; only the codepoints
   that actually go missing are swapped. *)

let codepoint s =
  if String.length s = 0 then 0
  else
    let c = Char.code s.[0] in
    let at i = if i < String.length s then Char.code s.[i] land 0x3F else 0 in
    if c < 0x80 then c
    else if c land 0xE0 = 0xC0 then ((c land 0x1F) lsl 6) lor at 1
    else if c land 0xF0 = 0xE0 then ((c land 0x0F) lsl 12) lor (at 1 lsl 6) lor at 2
    else ((c land 0x07) lsl 18) lor (at 1 lsl 12) lor (at 2 lsl 6) lor at 3

let ascii_table = [
  "✦", "*"; "✔", "+"; "⊘", "x"; "⎿", "L"; "↳", ">"; "●", "o"; "◈", "#";
  "≈", "~"; "▎", "|"; "▍", "#"; "▁", "_"; "▔", "-"; "·", "."; "╷", "'";
  "╵", "'"; "╻", "!"; "⌒", "~"; "É", "E"; "í", "i"; "—", "-"; "→", "->";
]

let ascii_sub ch =
  if not ascii_only then ch
  else
    let cp = codepoint ch in
    if cp >= 0x12000 && cp <= 0x123FF then "*"          (* cuneiform *)
    else if cp >= 0x2800 && cp <= 0x28FF then "*"       (* braille *)
    else match List.assoc_opt ch ascii_table with Some s -> s | None -> ch

let put cv x y ?fg ?bg ?(bold = false) ?(risky = false) ch =
  if inside cv x y then begin
    let old = cv.cells.(y * cv.w + x) in
    let bg = match bg with Some _ -> bg | None -> old.bg in
    let ch = ascii_sub ch in
    cv.cells.(y * cv.w + x) <- { ch; fg; bg; bold; risky }
  end

(** Paint a background colour without disturbing the glyph already there. *)
let tint cv x y bg =
  if inside cv x y then begin
    let c = cv.cells.(y * cv.w + x) in
    cv.cells.(y * cv.w + x) <- { c with bg = Some bg }
  end

let get cv x y = if inside cv x y then cv.cells.(y * cv.w + x) else blank

(* ── utf-8 ────────────────────────────────────────────────────────────── *)

let utf8_chars s =
  let n = String.length s in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let c = Char.code s.[i] in
      let len =
        if c < 0x80 then 1
        else if c land 0xE0 = 0xC0 then 2
        else if c land 0xF0 = 0xE0 then 3
        else if c land 0xF8 = 0xF0 then 4
        else 1
      in
      let len = min len (n - i) in
      go (i + len) (String.sub s i len :: acc)
  in
  go 0 []

let utf8_len s = List.length (utf8_chars s)

let utf8_sub s start len =
  let cs = utf8_chars s in
  let b = Buffer.create (String.length s) in
  List.iteri (fun i c -> if i >= start && i < start + len then Buffer.add_string b c) cs;
  Buffer.contents b

let text cv x y ?fg ?bg ?bold ?risky s =
  List.iteri (fun i c -> put cv (x + i) y ?fg ?bg ?bold ?risky c) (utf8_chars s)

let center cv y ?fg ?bg ?bold ?risky s =
  text cv ((cv.w - utf8_len s) / 2) y ?fg ?bg ?bold ?risky s

(** Draw [s] with each glyph sampled from a gradient across its own length. *)
let gradient_text cv x y stops ?bg ?(bold = false) ?(risky = false) s =
  let cs = utf8_chars s in
  let n = max 1 (List.length cs - 1) in
  List.iteri
    (fun i c ->
       let t = float_of_int i /. float_of_int n in
       put cv (x + i) y ~fg:(ramp stops t) ?bg ~bold ~risky c)
    cs

let hline cv x y w ?fg ?bg ?(bold = false) ch =
  for i = 0 to w - 1 do put cv (x + i) y ?fg ?bg ~bold ch done

let vline cv x y h ?fg ?bg ?(bold = false) ch =
  for i = 0 to h - 1 do put cv x (y + i) ?fg ?bg ~bold ch done

type box_style = Round | Sharp | Double | Heavy

let box_chars = function
  | Round  -> ("╭", "╮", "╰", "╯", "─", "│")
  | Sharp  -> ("┌", "┐", "└", "┘", "─", "│")
  | Double -> ("╔", "╗", "╚", "╝", "═", "║")
  | Heavy  -> ("┏", "┓", "┗", "┛", "━", "┃")

let box cv x y w h ?(style = Round) ?fg ?bg ?(bold = false) ?title ?title_fg () =
  if w >= 2 && h >= 2 then begin
    let tl, tr, bl, br, hz, vt = box_chars style in
    put cv x y ?fg ?bg ~bold tl;
    put cv (x + w - 1) y ?fg ?bg ~bold tr;
    put cv x (y + h - 1) ?fg ?bg ~bold bl;
    put cv (x + w - 1) (y + h - 1) ?fg ?bg ~bold br;
    hline cv (x + 1) y (w - 2) ?fg ?bg ~bold hz;
    hline cv (x + 1) (y + h - 1) (w - 2) ?fg ?bg ~bold hz;
    vline cv x (y + 1) (h - 2) ?fg ?bg ~bold vt;
    vline cv (x + w - 1) (y + 1) (h - 2) ?fg ?bg ~bold vt;
    match title with
    | Some t when utf8_len t + 4 <= w ->
      let tf = match title_fg with Some c -> Some c | None -> fg in
      text cv (x + 2) y ?fg:tf ?bg ~bold:true (" " ^ t ^ " ")
    | _ -> ()
  end

(* ── rendering ────────────────────────────────────────────────────────── *)

let out = Buffer.create (1 lsl 17)

let row_is_risky cv y =
  let rec go x = x < cv.w && (cv.cells.(y * cv.w + x).risky || go (x + 1)) in
  go 0

let emit_row_absolute cv y =
  for x = 0 to cv.w - 1 do
    let c = cv.cells.(y * cv.w + x) in
    Buffer.add_string out (Printf.sprintf "\027[%d;%dH\027[0m" (y + 1) (x + 1));
    (match c.bg with Some b -> Buffer.add_string out (sgr_bg b) | None -> ());
    if c.bold then Buffer.add_string out "\027[1m";
    (match c.fg with Some f -> Buffer.add_string out (sgr_fg f) | None -> ());
    Buffer.add_string out c.ch
  done;
  Buffer.add_string out "\027[0m"

let emit_row_fast cv y =
  let cur_fg = ref None and cur_bg = ref None and cur_bold = ref false in
  Buffer.add_string out "\027[0m";
  for x = 0 to cv.w - 1 do
    let c = cv.cells.(y * cv.w + x) in
    if c.bold <> !cur_bold then begin
      if c.bold then Buffer.add_string out "\027[1m"
      else begin
        Buffer.add_string out "\027[0m";
        cur_fg := None;
        cur_bg := None
      end;
      cur_bold := c.bold
    end;
    if c.bg <> !cur_bg then begin
      (match c.bg with
       | Some b -> Buffer.add_string out (sgr_bg b)
       | None -> Buffer.add_string out "\027[49m");
      cur_bg := c.bg
    end;
    if c.fg <> !cur_fg then begin
      (match c.fg with
       | Some f -> Buffer.add_string out (sgr_fg f)
       | None -> Buffer.add_string out "\027[39m");
      cur_fg := c.fg
    end;
    Buffer.add_string out c.ch
  done;
  Buffer.add_string out "\027[0m"

let present cv =
  Buffer.clear out;
  Buffer.add_string out "\027[H";
  for y = 0 to cv.h - 1 do
    if row_is_risky cv y then emit_row_absolute cv y
    else begin
      Buffer.add_string out (Printf.sprintf "\027[%d;1H" (y + 1));
      emit_row_fast cv y
    end
  done;
  print_string (Buffer.contents out);
  flush stdout

(** Re-emit the canvas into the normal buffer as ordinary lines, so the
    final composition survives in the user's scrollback. *)
let print_static cv =
  let b = Buffer.create (1 lsl 16) in
  for y = 0 to cv.h - 1 do
    (* trim trailing cells that carry neither glyph nor background *)
    let last = ref (-1) in
    for x = 0 to cv.w - 1 do
      let c = cv.cells.(y * cv.w + x) in
      if c.ch <> " " || c.bg <> None then last := x
    done;
    let cur_fg = ref None and cur_bg = ref None and cur_bold = ref false in
    for x = 0 to !last do
      let c = cv.cells.(y * cv.w + x) in
      if c.bold <> !cur_bold then begin
        if c.bold then Buffer.add_string b "\027[1m"
        else begin Buffer.add_string b "\027[0m"; cur_fg := None; cur_bg := None end;
        cur_bold := c.bold
      end;
      if c.bg <> !cur_bg then begin
        (match c.bg with Some g -> Buffer.add_string b (sgr_bg g) | None -> Buffer.add_string b "\027[49m");
        cur_bg := c.bg
      end;
      if c.fg <> !cur_fg then begin
        (match c.fg with Some f -> Buffer.add_string b (sgr_fg f) | None -> Buffer.add_string b "\027[39m");
        cur_fg := c.fg
      end;
      Buffer.add_string b c.ch
    done;
    Buffer.add_string b "\027[0m\n"
  done;
  print_string (Buffer.contents b);
  flush stdout

(* ── screen / input ───────────────────────────────────────────────────── *)

let is_tty = Unix.isatty Unix.stdout
let saved_tio : Unix.terminal_io option ref = ref None
let entered = ref false

let leave () =
  if !entered then begin
    entered := false;
    (match !saved_tio with
     | Some t -> (try Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH t with _ -> ())
     | None -> ());
    print_string "\027[0m\027[?7h\027[?25h\027[?1049l";
    flush stdout
  end

let enter () =
  if not !entered then begin
    entered := true;
    (if is_tty then
       try
         let t = Unix.tcgetattr Unix.stdin in
         saved_tio := Some t;
         let raw = { t with Unix.c_icanon = false; c_echo = false; c_vmin = 0; c_vtime = 0 } in
         Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH raw
       with _ -> ());
    print_string "\027[?1049h\027[?25l\027[?7l\027[2J";
    flush stdout;
    at_exit leave
  end

(** Non-blocking single-byte read; [None] when nothing is waiting. *)
let key () =
  if not is_tty then None
  else
    match Unix.select [ Unix.stdin ] [] [] 0.0 with
    | [ _ ], _, _ ->
      let b = Bytes.create 1 in
      (try if Unix.read Unix.stdin b 0 1 = 1 then Some (Bytes.get b 0) else None with _ -> None)
    | _ -> None

let now = Unix.gettimeofday
let sleep = Unix.sleepf

(* ── the frame loop ───────────────────────────────────────────────────── *)

(** [run ~fps frame] paints until [frame] returns [false], the user presses
    q/Q/Esc/Ctrl-C, or [duration] elapses.  [frame cv t i] receives the
    canvas (already cleared), seconds since start, and the frame index.
    The final canvas is re-printed into the normal screen unless
    [~keep:false], so a "boot animation" settles into a lasting picture. *)
let run ?(fps = 30.) ?(keep = true) ?duration (frame : canvas -> float -> int -> bool) =
  install_winch ();
  enter ();
  let w, h = query_size () in
  let cv = ref (create w h) in
  let t0 = now () in
  let i = ref 0 in
  let stop = ref false in
  let dt = 1.0 /. fps in
  (try
     while not !stop do
       if !resized then begin
         resized := false;
         let w, h = query_size () in
         cv := create w h;
         print_string "\027[2J";
         flush stdout
       end;
       let start = now () in
       let t = start -. t0 in
       clear !cv;
       if not (frame !cv t !i) then stop := true;
       present !cv;
       incr i;
       (match duration with Some d when t >= d -> stop := true | _ -> ());
       (match key () with
        | Some ('q' | 'Q' | '\027' | '\003') -> stop := true
        | _ -> ());
       let spent = now () -. start in
       if spent < dt then sleep (dt -. spent)
     done
   with Sys.Break -> ());
  leave ();
  if keep then print_static !cv

(** A still frame: paint once, hold for [hold] seconds (or until a key),
    then leave it in the scrollback. *)
let still ?(hold = 3600.) (paint : canvas -> unit) =
  install_winch ();
  enter ();
  let w, h = query_size () in
  let cv = create w h in
  paint cv;
  present cv;
  let t0 = now () in
  let stop = ref false in
  while (not !stop) && now () -. t0 < hold do
    (match key () with Some _ -> stop := true | None -> ());
    sleep 0.03
  done;
  leave ();
  print_static cv

(* ── little maths helpers the demos keep reaching for ─────────────────── *)

let pi = 4.0 *. atan 1.0
let fclamp lo hi v = if v < lo then lo else if v > hi then hi else v
let smooth t = let t = fclamp 0. 1. t in t *. t *. (3. -. (2. *. t))
let ease_out t = let t = fclamp 0. 1. t in 1. -. ((1. -. t) ** 3.)
let ease_in t = let t = fclamp 0. 1. t in t *. t *. t

(* Deterministic value noise — no Random state, so frames are reproducible. *)
let hash2 x y =
  let n = (x * 374761393) + (y * 668265263) in
  let n = n lxor (n lsr 13) in
  let n = n * 1274126177 in
  float_of_int ((n lxor (n lsr 16)) land 0xffffff) /. 16777215.

let noise1 x seed =
  let i = int_of_float (floor x) in
  let f = x -. float_of_int i in
  let a = hash2 i seed and b = hash2 (i + 1) seed in
  a +. ((b -. a) *. smooth f)

(** Fractal ridge used for dune silhouettes. *)
let dune x seed octaves =
  let rec go k amp freq acc =
    if k >= octaves then acc
    else go (k + 1) (amp *. 0.5) (freq *. 2.05) (acc +. (amp *. noise1 (x *. freq) (seed + k)))
  in
  go 0 1.0 1.0 0.0

let shades = [| " "; "░"; "▒"; "▓"; "█" |]
let shade_at t = shades.(int_of_float (fclamp 0. 4.999 (t *. 5.0)))

let bayer4 = [|
  [|  0;  8;  2; 10 |];
  [| 12;  4; 14;  6 |];
  [|  3; 11;  1;  9 |];
  [| 15;  7; 13;  5 |];
|]

let dither x y t =
  let th = (float_of_int bayer4.(y land 3).(x land 3) +. 0.5) /. 16.0 in
  if t > th then 1 else 0

let spinner_frames =
  if ascii_only then [| "|"; "/"; "-"; "\\" |]
  else [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]
let spinner t = spinner_frames.(int_of_float (t *. 12.) mod Array.length spinner_frames)

(* ── block lettering ──────────────────────────────────────────────────── *)

(* ANSI-Shadow style caps, six rows tall — the same family the Hermes Agent
   banner uses, so the demos read as siblings of it. *)
let big_glyphs = [
  'C', [| " ██████╗ "; "██╔════╝ "; "██║      "; "██║      "; "╚██████╗ "; " ╚═════╝ " |];
  'A', [| " █████╗ "; "██╔══██╗"; "███████║"; "██╔══██║"; "██║  ██║"; "╚═╝  ╚═╝" |];
  'R', [| "██████╗ "; "██╔══██╗"; "██████╔╝"; "██╔══██╗"; "██║  ██║"; "╚═╝  ╚═╝" |];
  'V', [| "██╗   ██╗"; "██║   ██║"; "██║   ██║"; "╚██╗ ██╔╝"; " ╚████╔╝ "; "  ╚═══╝  " |];
  'N', [| "███╗   ██╗"; "████╗  ██║"; "██╔██╗ ██║"; "██║╚██╗██║"; "██║ ╚████║"; "╚═╝  ╚═══╝" |];
  'S', [| "███████╗"; "██╔════╝"; "███████╗"; "╚════██║"; "███████║"; "╚══════╝" |];
  'O', [| " ██████╗ "; "██╔═══██╗"; "██║   ██║"; "██║   ██║"; "╚██████╔╝"; " ╚═════╝ " |];
  'U', [| "██╗   ██╗"; "██║   ██║"; "██║   ██║"; "██║   ██║"; "╚██████╔╝"; " ╚═════╝ " |];
  'E', [| "███████╗"; "██╔════╝"; "█████╗  "; "██╔══╝  "; "███████╗"; "╚══════╝" |];
  'I', [| "██╗"; "██║"; "██║"; "██║"; "██║"; "╚═╝" |];
  'M', [| "███╗   ███╗"; "████╗ ████║"; "██╔████╔██║"; "██║╚██╔╝██║"; "██║ ╚═╝ ██║"; "╚═╝     ╚═╝" |];
  'H', [| "██╗  ██╗"; "██║  ██║"; "███████║"; "██╔══██║"; "██║  ██║"; "╚═╝  ╚═╝" |];
  'G', [| " ██████╗ "; "██╔════╝ "; "██║  ███╗"; "██║   ██║"; "╚██████╔╝"; " ╚═════╝ " |];
  'L', [| "██╗     "; "██║     "; "██║     "; "██║     "; "███████╗"; "╚══════╝" |];
  'T', [| "████████╗"; "╚══██╔══╝"; "   ██║   "; "   ██║   "; "   ██║   "; "   ╚═╝   " |];
  'P', [| "██████╗ "; "██╔══██╗"; "██████╔╝"; "██╔═══╝ "; "██║     "; "╚═╝     " |];
  'D', [| "██████╗ "; "██╔══██╗"; "██║  ██║"; "██║  ██║"; "██████╔╝"; "╚═════╝ " |];
  'Y', [| "██╗   ██╗"; "╚██╗ ██╔╝"; " ╚████╔╝ "; "  ╚██╔╝  "; "   ██║   "; "   ╚═╝   " |];
  ' ', [| "  "; "  "; "  "; "  "; "  "; "  " |];
]

(* A two-row fallback for terminals too narrow for the big font. *)
let small_glyphs = [
  'C', [| "▄▀▀"; "▀▄▄" |]; 'A', [| "▄▀▄"; "█▀█" |]; 'R', [| "█▀▄"; "█▀▄" |];
  'V', [| "█ █"; "▀▄▀" |]; 'N', [| "█▄█"; "█ █" |]; 'S', [| "▄▀▀"; "▄▄▀" |];
  'O', [| "▄▀▄"; "▀▄▀" |]; 'U', [| "█ █"; "▀▄▀" |]; 'E', [| "█▀▀"; "█▄▄" |];
  'I', [| "█"; "█" |]; 'M', [| "█▄█"; "█ █" |]; 'H', [| "█ █"; "█▀█" |];
  'G', [| "▄▀▀"; "▀▄█" |]; 'L', [| "█  "; "█▄▄" |]; 'T', [| "▀█▀"; " █ " |];
  'P', [| "█▀▄"; "█▀ " |]; 'D', [| "█▀▄"; "█▄▀" |]; 'Y', [| "█ █"; " █ " |];
  ' ', [| " "; " " |];
]

(** Lay a word out in a block font.  Returns the rows, top to bottom. *)
let big_word ?(small = false) w =
  let table = if small then small_glyphs else big_glyphs in
  let rows = if small then 2 else 6 in
  let acc = Array.make rows (Buffer.create 16) in
  for i = 0 to rows - 1 do acc.(i) <- Buffer.create 64 done;
  String.iter
    (fun c ->
       let c = Char.uppercase_ascii c in
       match List.assoc_opt c table with
       | None -> ()
       | Some g ->
         for i = 0 to rows - 1 do
           Buffer.add_string acc.(i) g.(i);
           (* the two-row font has no built-in sidebearing; without this the
              letters run together into one solid smear *)
           if small then Buffer.add_string acc.(i) " "
         done)
    w;
  Array.to_list (Array.map Buffer.contents acc)

let big_width ?(small = false) w =
  match big_word ~small w with [] -> 0 | r :: _ -> utf8_len r

(** How long a demo lingers on its finished picture.  The runner sets
    [CARAVAN_DEMO_HOLD] when it plays the whole set unattended; on its own a
    demo waits for a keypress. *)
let hold_secs ?(default = infinity) () =
  match getenv "CARAVAN_DEMO_HOLD" with
  | Some s -> (try float_of_string s with _ -> default)
  | None -> default

(** Speed knob so a slow boot sequence can be skipped past. *)
let speed () =
  match getenv "CARAVAN_DEMO_SPEED" with
  | Some s -> (try max 0.1 (float_of_string s) with _ -> 1.0)
  | None -> 1.0

(** Reveal [s] progressively: 0.0 shows nothing, 1.0 the whole string. *)
let typed s t =
  let n = utf8_len s in
  let k = int_of_float (fclamp 0. 1. t *. float_of_int n +. 0.0001) in
  utf8_sub s 0 k
