(** Terminal styling, spinners, and the visual toolkit for the CLI.

    Layered design:
    - [Document]-based renderers (categorical abstractions, kept stable for
      library users and tests);
    - plain ANSI helper functions ([bold], [cyan], ...) that degrade to
      plain text when stdout is not a TTY;
    - higher-level widgets: banner, panels, horizontal rules, status bar,
      tool-trace lines, and a small markdown renderer for model output. *)

let is_tty = Unix.isatty Unix.stdout

(** The spinner and progress feedback write to stderr; they must vanish
    when stderr is redirected (tests, pipes, batch logs). *)
let err_is_tty = Unix.isatty Unix.stderr

(** Truecolor / 256-color capability sniffing. *)
let color_depth =
  lazy (
    match Sys.getenv_opt "COLORTERM" with
    | Some ("truecolor" | "24bit") -> `True_color
    | _ ->
      (match Sys.getenv_opt "TERM" with
       | Some t when Filename.check_suffix t "256color" -> `Color_256
       | Some "dumb" | None -> `Mono
       | Some _ -> `Color_16))

(* ── Categorical & Modular Rendering Abstractions ─────────────────────── *)

module type RENDERER = sig
  type t
  val empty : t
  val append : t -> t -> t
  val render_styled : Document.style -> t -> t
  val render_text : string -> t
  val compile : t -> string
end

module AnsiRenderer = struct
  type t = string
  let empty = ""
  let append = ( ^ )

  let color_code = function
    | Document.Cyan -> "36"
    | Document.Green -> "32"
    | Document.Yellow -> "33"
    | Document.Magenta -> "35"
    | Document.Red -> "31"
    | Document.Blue -> "34"
    | Document.White -> "97"

  let style_code = function
    | Document.Bold -> "1"
    | Document.Dim -> "2"
    | Document.Underline -> "4"
    | Document.Foreground c -> "1;" ^ color_code c
    | Document.Background c ->
      (match c with
       | Document.Cyan -> "46"
       | Document.Green -> "42"
       | Document.Yellow -> "43"
       | Document.Magenta -> "45"
       | Document.Red -> "41"
       | Document.Blue -> "44"
       | Document.White -> "107")

  let render_styled s t =
    if t = "" then "" else
    let code = style_code s in
    Printf.sprintf "\027[%sm%s\027[0m" code t

  let render_text s = s
  let compile t = t
end

module PlainTextRenderer = struct
  type t = string
  let empty = ""
  let append = ( ^ )
  let render_styled _s t = t
  let render_text s = s
  let compile t = t
end

let compile_document (type r) (module R : RENDERER with type t = r) (fmt_elem : 'a -> r) doc =
  let rec loop = function
    | Document.Empty -> R.empty
    | Document.Text x -> fmt_elem x
    | Document.Styled (st, d) -> R.render_styled st (loop d)
    | Document.Concat docs ->
      List.fold_left (fun acc d ->
        R.append acc (loop d)
      ) R.empty docs
  in
  loop doc

module TermRenderer = struct
  type t = string
  let empty = ""
  let append = ( ^ )
  let render_styled s t =
    if is_tty then AnsiRenderer.render_styled s t
    else PlainTextRenderer.render_styled s t
  let render_text s = s
  let compile t = t
end

(* ── Type-Safe Style API Wrappers ─────────────────────────────────────── *)

let style_doc style s =
  compile_document (module TermRenderer) (fun x -> x) (Document.Styled (style, Document.Text s))

let bold s      = style_doc Document.Bold s
let dim s       = style_doc Document.Dim s
let underline s = style_doc Document.Underline s
let cyan s      = style_doc (Document.Foreground Document.Cyan) s
let green s     = style_doc (Document.Foreground Document.Green) s
let yellow s    = style_doc (Document.Foreground Document.Yellow) s
let magenta s   = style_doc (Document.Foreground Document.Magenta) s
let red s       = style_doc (Document.Foreground Document.Red) s
let white s     = style_doc (Document.Foreground Document.White) s
let blue s      = style_doc (Document.Foreground Document.Blue) s

(** Direct RGB foreground (falls back to the nearest simple style). *)
let rgb (r, g, b) s =
  if not is_tty then s
  else match Lazy.force color_depth with
    | `True_color -> Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b s
    | `Color_256 ->
      let to6 v = (v * 6) / 256 in
      let idx = 16 + 36 * to6 r + 6 * to6 g + to6 b in
      Printf.sprintf "\027[38;5;%dm%s\027[0m" idx s
    | _ -> s

let print_ansi s = print_string s
let println_ansi s = print_endline s

(* ── Text metrics ─────────────────────────────────────────────────────── *)

(** Visible width of a string: skips ANSI escapes and counts UTF-8
    sequences as single columns (wide CJK/emoji count as 2 — close enough
    for box alignment). *)
let visible_width s =
  let len = String.length s in
  let rec go i acc =
    if i >= len then acc
    else
      let c = Char.code s.[i] in
      if c = 0x1b then
        (* Skip CSI escape sequence: ESC [ ... final byte in @-~ *)
        let rec skip j =
          if j >= len then j
          else if s.[j] >= '@' && s.[j] <= '~' && j > i + 1 then j + 1
          else skip (j + 1)
        in
        go (skip (i + 1)) acc
      else if c < 0x80 then go (i + 1) (acc + 1)
      else if c < 0xC0 then go (i + 1) acc          (* continuation byte *)
      else if c < 0xE0 then go (i + 1) (acc + 1)    (* 2-byte seq *)
      else if c < 0xF0 then go (i + 1) (acc + 1)    (* 3-byte seq (CJK≈2, but keep 1-col heuristic for box chars) *)
      else go (i + 1) (acc + 2)                     (* 4-byte seq: emoji ≈ 2 cols *)
  in
  go 0 0

let term_width () =
  match Sys.getenv_opt "COLUMNS" with
  | Some c -> (try max 40 (int_of_string c) with _ -> 80)
  | None -> 80

(** Pad [s] on the right to [n] visible columns.  Unlike ["%-ns"] this
    counts columns rather than bytes, so ANSI escapes and multi-byte
    characters do not knock a table out of alignment. *)
let pad_visible n s =
  s ^ String.make (max 0 (n - visible_width s)) ' '

(** Truncate [s] to [n] visible columns, appending an ellipsis. *)
let truncate_visible s n =
  if visible_width s <= n then s
  else
    let buf = Buffer.create n in
    let len = String.length s in
    let rec go i acc =
      if i >= len || acc >= n - 1 then ()
      else
        let c = Char.code s.[i] in
        let step = if c < 0x80 then 1 else if c < 0xE0 then 2 else if c < 0xF0 then 3 else 4 in
        let step = min step (len - i) in
        Buffer.add_string buf (String.sub s i step);
        go (i + step) (acc + 1)
    in
    go 0 0;
    Buffer.contents buf ^ "…"

(* ── Widgets ──────────────────────────────────────────────────────────── *)

(** Horizontal rule with optional centred title: [── title ───────]. *)
let rule ?title () =
  let w = min (term_width ()) 100 in
  match title with
  | None -> dim (String.concat "" (List.init w (fun _ -> "─")))
  | Some t ->
    let tlen = visible_width t + 2 in
    let right = max 0 (w - 4 - tlen) in
    dim "── " ^ bold t ^ dim (" " ^ String.concat "" (List.init right (fun _ -> "─")))

(** Simple rounded-corner panel around pre-rendered lines. *)
let panel ?(min_width = 0) lines =
  let content_w =
    List.fold_left (fun acc l -> max acc (visible_width l)) min_width lines
  in
  let top    = dim ("╭" ^ String.concat "" (List.init (content_w + 2) (fun _ -> "─")) ^ "╮") in
  let bottom = dim ("╰" ^ String.concat "" (List.init (content_w + 2) (fun _ -> "─")) ^ "╯") in
  let body =
    List.map (fun l ->
      let pad = content_w - visible_width l in
      dim "│ " ^ l ^ String.make (max 0 pad) ' ' ^ dim " │"
    ) lines
  in
  String.concat "\n" (top :: body @ [bottom])

(** Two-column key/value block used by /config, doctor, providers. *)
let kv_line ?(key_width = 14) k v =
  Printf.sprintf "  %s %s" (cyan (Printf.sprintf "%-*s" key_width (k ^ ":"))) v

let print_banner () =
  if is_tty then begin
    (* Caravan wordmark: desert-dusk gradient when the terminal allows. *)
    let word = "☾ C A R A V A N" in
    let sub  = "typed agentic harness · OCaml" in
    (* Split into UTF-8 codepoints so multi-byte glyphs stay intact. *)
    let utf8_chunks s =
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
    in
    let colored_word =
      match Lazy.force color_depth with
      | `True_color | `Color_256 ->
        (* amber → rose gradient across the letters *)
        let chunks = utf8_chunks word in
        let n = List.length chunks in
        List.mapi (fun i ch ->
            let t = float_of_int i /. float_of_int (max 1 (n - 1)) in
            let r = 236 + int_of_float (t *. 8.) in
            let g = 150 - int_of_float (t *. 60.) in
            let b = 60  + int_of_float (t *. 90.) in
            rgb (r, g, b) ch)
          chunks
        |> String.concat ""
      | _ -> bold (yellow word)
    in
    println_ansi (panel [bold colored_word; dim sub]);
    print_newline ()
  end

let render_status_bar ~provider ~model ~turns ~tokens_in ~tokens_out =
  let seg_model = Printf.sprintf "%s%s%s" (magenta provider) (dim "/") (white model) in
  let seg_turns = dim (Printf.sprintf "turn %d" turns) in
  let seg_toks  = dim (Printf.sprintf "▲%d ▼%d tok" tokens_in tokens_out) in
  Printf.sprintf "%s %s %s %s %s" seg_model (dim "·") seg_turns (dim "·") seg_toks

let print_help cmds =
  println_ansi (bold (yellow " Slash Commands:"));
  List.iter (fun (cmd, desc) ->
    println_ansi (Printf.sprintf "  %s  %s"
      (cyan (Printf.sprintf "%-22s" cmd))
      (dim desc))
  ) cmds;
  print_newline ()

module MakeTheme (R : RENDERER) = struct
  let keyword s = R.render_styled Document.Bold (R.render_text s)
  let error s = R.render_styled (Document.Foreground Document.Red) (R.render_text s)
  let title s = R.render_styled (Document.Foreground Document.Cyan) (R.render_styled Document.Bold (R.render_text s))
  let success s = R.render_styled (Document.Foreground Document.Green) (R.render_text s)
end

(* ── Markdown-lite rendering ──────────────────────────────────────────── *)

(** Render a useful subset of markdown for terminal display:
    headers, bullet points, `inline code`, **bold**, fenced code blocks.
    Not a spec-complete parser — a readability upgrade over raw text. *)
let render_markdown (src : string) : string =
  if not is_tty then src
  else begin
    let bold_re = Re.compile (Re.Pcre.re {|\*\*([^*]+)\*\*|}) in
    let code_re = Re.compile (Re.Pcre.re {|`([^`]+)`|}) in
    let inline s =
      let s = Re.replace bold_re s ~f:(fun g -> bold (Re.Group.get g 1)) in
      Re.replace code_re s ~f:(fun g -> cyan (Re.Group.get g 1))
    in
    let lines = String.split_on_char '\n' src in
    let buf = Buffer.create (String.length src + 64) in
    let in_code = ref false in
    List.iter (fun line ->
      let trimmed = String.trim line in
      if String.length trimmed >= 3 && String.sub trimmed 0 3 = "```" then begin
        in_code := not !in_code;
        Buffer.add_string buf (dim (if !in_code then "  ┌─ code" else "  └─"));
        Buffer.add_char buf '\n'
      end else if !in_code then begin
        Buffer.add_string buf (dim "  │ ");
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end else if String.length trimmed > 0 && trimmed.[0] = '#' then begin
        let text = String.trim (String.concat "" (String.split_on_char '#' trimmed)) in
        Buffer.add_string buf (bold (underline text));
        Buffer.add_char buf '\n'
      end else if String.length trimmed >= 2 &&
                  (String.sub trimmed 0 2 = "- " || String.sub trimmed 0 2 = "* ") then begin
        Buffer.add_string buf ("  " ^ yellow "•" ^ " " ^
                               inline (String.sub trimmed 2 (String.length trimmed - 2)));
        Buffer.add_char buf '\n'
      end else begin
        Buffer.add_string buf (inline line);
        Buffer.add_char buf '\n'
      end
    ) lines;
    (* Drop the final newline we always append. *)
    let s = Buffer.contents buf in
    if String.length s > 0 && s.[String.length s - 1] = '\n'
    then String.sub s 0 (String.length s - 1) else s
  end

(* ── Tool trace lines (Trace renderer helpers) ────────────────────────── *)

let format_tool_call ?(verbose = false) ~name ~args () =
  let trimmed = String.trim args in
  if not verbose then
    let args_preview = truncate_visible trimmed 60 in
    Printf.sprintf "%s %s%s%s%s"
      (magenta "⏺") (bold name) (dim "(") (dim args_preview) (dim ")")
  else
    if String.contains trimmed '\n' || String.length trimmed > 80 then
      let formatted_args =
        String.split_on_char '\n' trimmed
        |> List.map (fun line -> "    " ^ dim line)
        |> String.concat "\n"
      in
      Printf.sprintf "%s %s\n%s" (magenta "⏺") (bold name) formatted_args
    else
      Printf.sprintf "%s %s%s%s%s"
        (magenta "⏺") (bold name) (dim "(") (dim trimmed) (dim ")")

let format_tool_result ?(verbose = false) ~output ~duration () =
  let trimmed = String.trim output in
  if not verbose then
    let first_line =
      match String.index_opt trimmed '\n' with
      | Some i -> String.sub trimmed 0 i
      | None -> trimmed
    in
    let preview = truncate_visible first_line 70 in
    let extra_lines =
      List.length (String.split_on_char '\n' trimmed) - 1
    in
    let more = if extra_lines > 0 then dim (Printf.sprintf " (+%d lines)" extra_lines) else "" in
    Printf.sprintf "  %s %s%s %s"
      (dim "⎿") (dim preview) more (dim (Printf.sprintf "[%.1fs]" duration))
  else
    let formatted_lines =
      String.split_on_char '\n' trimmed
      |> List.map (fun line -> "  " ^ dim "│ " ^ dim line)
      |> String.concat "\n"
    in
    Printf.sprintf "  %s %s\n%s"
      (dim "⎿") (dim (Printf.sprintf "[%.1fs]" duration)) formatted_lines

(* ── Spinner ──────────────────────────────────────────────────────────── *)

module Spinner = struct
  (** Braille arc — the classic ten-frame dot sweep. *)
  let braille = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

  type config = {
    frames   : string array;
    colors   : (string -> string) array;
    interval : float;  (** seconds per frame *)
  }

  (** OCaml amber + warm tones — used while the LLM is thinking. *)
  let thinking = {
    frames   = braille;
    colors   = [| yellow; (fun s -> style_doc (Document.Foreground Document.Red) s);
                  magenta; cyan; yellow |];
    interval = 0.08;
  }

  (** Caravan teal/cyan palette — used while a tool is executing. *)
  let tool = {
    frames   = braille;
    colors   = [| cyan; blue; green; cyan; blue |];
    interval = 0.07;
  }

  (** Purple / magenta — used during context summarisation. *)
  let summarize = {
    frames   = braille;
    colors   = [| magenta; blue; cyan; magenta; blue |];
    interval = 0.09;
  }

  (** Green / teal — used for network fetches. *)
  let fetch = {
    frames   = braille;
    colors   = [| green; cyan; blue; green; cyan |];
    interval = 0.08;
  }

  (** Yellow / white — used for web searches. *)
  let search = {
    frames   = braille;
    colors   = [| yellow; white; cyan; yellow; white |];
    interval = 0.08;
  }

  (** Neutral fallback for unknown tools. *)
  let default = {
    frames   = braille;
    colors   = [| white; yellow; cyan; magenta; blue |];
    interval = 0.08;
  }

  (** Select a preset by the tool/context name forwarded from Config. *)
  let of_verb verb =
    let lv = String.lowercase_ascii verb in
    if String.length lv >= 7 && String.sub lv 0 7 = "thinkin" then thinking
    else if String.length lv >= 5 && String.sub lv 0 5 = "summa"  then summarize
    else if String.length lv >= 5 && String.sub lv 0 5 = "fetch"  then fetch
    else if String.length lv >= 6 && String.sub lv 0 6 = "search" then search
    else if String.length lv >= 7 && String.sub lv 0 7 = "running" then tool
    else if String.length lv >= 7 && String.sub lv 0 7 = "executi" then tool
    else default
end

(** Whether the spinner may draw at all: requires an interactive stderr on
    top of the configured enable flag. This is what keeps expect-tests and
    batch logs free of animation frames. *)
let spinner_allowed enabled = enabled && err_is_tty

(** Infinite braille render loop — intended to be raced via [Fiber.first].
    Eio's cancellation will interrupt [sleep] and [Fun.protect] clears the line. *)
let spinner_loop clock cfg verb =
  Fun.protect
    ~finally:(fun () -> Printf.eprintf "\r\027[K%!")
    (fun () ->
       let rec loop idx =
         let frame    = cfg.Spinner.frames.(idx mod Array.length cfg.Spinner.frames) in
         let color_fn = cfg.Spinner.colors.(idx mod Array.length cfg.Spinner.colors) in
         Printf.eprintf "\r%s %s...%!" (color_fn frame) verb;
         Eio.Time.sleep clock cfg.Spinner.interval;
         loop (idx + 1)
       in
       loop 0)

(** Promise-watching loop — wakes IMMEDIATELY when [promise] resolves
    (racing the frame sleep against the promise), then erases its line.
    Fast wake matters: any delay here is a window in which the caller's
    first streamed tokens would be drawn and then wiped by our erase. *)
let spinner_poll_loop clock cfg verb promise =
  Fun.protect
    ~finally:(fun () -> Printf.eprintf "\r\027[K%!")
    (fun () ->
       let rec loop idx =
         if Eio.Promise.is_resolved promise then ()
         else begin
           let frame    = cfg.Spinner.frames.(idx mod Array.length cfg.Spinner.frames) in
           let color_fn = cfg.Spinner.colors.(idx mod Array.length cfg.Spinner.colors) in
           Printf.eprintf "\r%s %s...%!" (color_fn frame) verb;
           Eio.Fiber.first
             (fun () -> Eio.Time.sleep clock cfg.Spinner.interval)
             (fun () -> Eio.Promise.await promise);
           loop (idx + 1)
         end
       in
       loop 0)

(** Run [fn] while showing a spinner; the spinner is cancelled when [fn] returns. *)
let with_spinner clock verb enabled fn =
  if not (spinner_allowed enabled) then fn ()
  else
    let cfg = Spinner.of_verb verb in
    Eio.Fiber.first (fun () -> spinner_loop clock cfg verb) fn

(** Fork a spinner fiber that watches [promise] and stops when it
    resolves. Returns a handshake promise that resolves only AFTER the
    spinner has erased its line — callers must await it before printing
    the first streamed output, or the erase races the tokens and eats
    the beginning of the reply. Returns [None] when no spinner runs. *)
let run_spinner_until_promise sw clock verb enabled promise =
  if not (spinner_allowed enabled) then None
  else begin
    let stopped, resolver = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      let cfg = Spinner.of_verb verb in
      spinner_poll_loop clock cfg verb promise;
      Eio.Promise.resolve resolver ());
    Some stopped
  end
