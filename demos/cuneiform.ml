(** cuneiform — "the tablet of the caravan".

    A clay slab is pressed out of the dark, then a stylus impresses the
    session record into it one wedge at a time.  Animation is boot-only:
    when the last wedge lands the tablet is finished and simply sits there.

    The glyph column uses real Sumero-Akkadian cuneiform.  Those codepoints
    live outside the BMP and terminals disagree about how wide they are, so
    every one is flagged [risky] — [Tui] then repaints that row with
    absolute cursor moves and the grid cannot drift. *)

open Dtui
open Tui

let p = active ~default:"lapis" ()
let sp = speed ()

(* ── content ──────────────────────────────────────────────────────────── *)

(* Honest glosses: these are the standard readings, bent to the demo. *)
let glossary = [
  ("𒀭", "dingir", "the divine mark — a name that is more");
  ("𒆠", "ki",     "earth, place — ground a caravan crosses");
  ("𒈗", "lugal",  "great man, king — whose seal closes it");
  ("𒌓", "ud",     "sun, day — one turn of the agent loop");
  ("𒆳", "kur",    "mountain, foreign land — outside context");
  ("𒁾", "dub",    "tablet — the transcript, kept once written");
  ("𒃻", "níg",    "thing, matter — a tool call and its result");
]

let colophon = [
  "IMPRESSED AT NIPPUR · TABLET I OF I";
  "12 tools · 4 providers · 1 plugin realm";
  "written by the hand of caravan, servant of the loop";
]

(* Sumerian numerals: 𒁹 = 1, 𒌋 = 10 *)
let tally n =
  let tens = n / 10 and ones = n mod 10 in
  String.concat "" (List.init tens (fun _ -> "𒌋") @ List.init ones (fun _ -> "𒁹"))

let ziggurat = [|
  "         ▟█▙         ";
  "       ▟█████▙       ";
  "      ▟███║███▙      ";
  "    ▟█████║█████▙    ";
  "   ▟███████║███████▙ ";
  " ▟█████████║█████████▙";
  "▐▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▌";
|]

(* ── a struck wedge ───────────────────────────────────────────────────── *)

type mark = {
  mx : int; my : int; mch : string;
  mfg : rgb;          (** settled, engraved colour *)
  mrisky : bool;
  mbold : bool;
}

let clay_at x y =
  (* Mottling keeps the slab from looking like a flat rectangle of colour. *)
  let n = (hash2 (x * 7) (y * 13) -. 0.5) *. 0.10 in
  let g = (float_of_int y /. 40.) +. n in
  ramp [| shade p.sand.(3) 0.95; p.sand.(2); shade p.sand.(1) 0.9 |] (fclamp 0. 1. (0.25 +. g))

let incised_of c = mix (shade c 0.30) p.shadow 0.55

(* ── build the whole tablet as an ordered list of impressions ─────────── *)

let build cv =
  let marks = ref [] in
  let add ?(risky = false) ?(bold = false) x y fg ch =
    if ch <> " " then marks := { mx = x; my = y; mch = ch; mfg = fg; mrisky = risky; mbold = bold } :: !marks
  in
  let tw = min (cv.w - 6) 88 in
  let th = min (cv.h - 3) 30 in
  let tx = (cv.w - tw) / 2 and ty = max 1 ((cv.h - th) / 2) in
  let small = tw < big_width "CARAVAN" + 6 in
  let lw = big_width ~small "CARAVAN" in
  let lh = if small then 2 else 6 in
  let lx = tx + ((tw - lw) / 2) and ly = ty + 1 in
  List.iteri
    (fun r row ->
       List.iteri
         (fun c ch ->
            (* The lit top edge of each stroke is what makes a wedge look
               pressed in rather than drawn on. *)
            let lit = r = 0 || (r = 1 && ch = "█") in
            let fg = if lit then mix (incised_of (clay_at (lx + c) (ly + r))) p.glow 0.35
                     else incised_of (clay_at (lx + c) (ly + r)) in
            add ~bold:(r < 2) (lx + c) (ly + r) fg ch)
         (utf8_chars row))
    (big_word ~small "CARAVAN");

  let rule_y = ly + lh + 1 in
  for x = tx + 3 to tx + tw - 4 do
    add x rule_y (incised_of (clay_at x rule_y)) "─"
  done;
  let sub = "𒀭 KA-RA-VA-AN · a typed caravan of tool calls 𒀭" in
  let sx = tx + ((tw - utf8_len sub) / 2) in
  List.iteri
    (fun i ch ->
       let risky = String.length ch > 3 in
       add ~risky (sx + i) (rule_y + 1) (incised_of (clay_at (sx + i) (rule_y + 1))) ch)
    (utf8_chars sub);

  (* left: the ziggurat, cut in relief *)
  let zx = tx + 3 and zy = rule_y + 3 in
  let zh = Array.length ziggurat in
  Array.iteri
    (fun r row ->
       List.iteri
         (fun c ch ->
            let t = float_of_int r /. float_of_int zh in
            let base = incised_of (clay_at (zx + c) (zy + r)) in
            let fg = mix (mix base p.glow 0.30) base (ease_in t) in
            add (zx + c) (zy + r) fg ch)
         (utf8_chars row))
    ziggurat;
  let cap = "É-TEMEN-AN-KI" in
  List.iteri
    (fun i ch -> add (zx + 4 + i) (zy + zh + 1) (mix (incised_of (clay_at 0 0)) p.title 0.45) ch)
    (utf8_chars cap);

  (* right: the glossary column *)
  let gx = zx + 24 in
  let gy = rule_y + 3 in
  List.iteri
    (fun i (glyph, translit, gloss) ->
       let y = gy + (i * 2) in
       if y < ty + th - 5 then begin
         add ~risky:true ~bold:true gx y (mix (incised_of (clay_at gx y)) p.title 0.55) glyph;
         List.iteri
           (fun c ch -> add (gx + 4 + c) y (mix (incised_of (clay_at 0 0)) p.title 0.30) ch)
           (utf8_chars translit);
         let ox = gx + 4 + 8 in
         List.iteri
           (fun c ch ->
              if ox + c < tx + tw - 3 then
                add (ox + c) y (incised_of (clay_at (ox + c) y)) ch)
           (utf8_chars gloss)
       end)
    glossary;

  (* colophon along the foot of the slab *)
  let fy = ty + th - 4 in
  List.iteri
    (fun i line ->
       let x = tx + ((tw - utf8_len line) / 2) in
       List.iteri
         (fun c ch ->
            let fg = if i = 0 then mix (incised_of (clay_at 0 0)) p.title 0.35
                     else incised_of (clay_at (x + c) (fy + i)) in
            add ~bold:(i = 0) (x + c) (fy + i) fg ch)
         (utf8_chars line))
    colophon;
  let t = tally 12 in
  let tx' = tx + ((tw - utf8_len t) / 2) in
  List.iteri
    (fun i ch -> add ~risky:true (tx' + i) (fy - 1) (mix (incised_of (clay_at 0 0)) p.accent 0.5) ch)
    (utf8_chars t);

  (List.rev !marks, tx, ty, tw, th)

(* ── frame ────────────────────────────────────────────────────────────── *)

let t_slab = 0.0
let t_press = 0.9
let per_mark = 0.0022   (* seconds per wedge — fast, it's a lot of clay *)

let total_time cv =
  let marks, _, _, _, _ = build cv in
  t_press +. (float_of_int (List.length marks) *. per_mark) +. 1.0

let frame cv t _i =
  let t = t *. sp in
  let marks, tx, ty, tw, th = build cv in
  let marks = Array.of_list marks in
  let n = Array.length marks in

  (* the room *)
  for y = 0 to cv.h - 1 do
    for x = 0 to cv.w - 1 do
      let d = (abs_float (float_of_int x -. (float_of_int cv.w /. 2.)) /. float_of_int cv.w)
              +. (abs_float (float_of_int y -. (float_of_int cv.h /. 2.)) /. float_of_int cv.h) in
      tint cv x y (mix (shade p.sky.(1) 0.7) p.bg (fclamp 0. 1. (d *. 1.6)))
    done
  done;

  (* the slab: wipes down out of the dark, then holds *)
  let slab = ease_out ((t -. t_slab) /. 0.7) in
  if slab > 0.01 then begin
    let rows = int_of_float (slab *. float_of_int th +. 0.5) in
    (* drop shadow *)
    for y = ty + 1 to ty + min th rows do
      for x = tx + 2 to tx + tw + 1 do
        if x < cv.w then tint cv x y (shade p.bg 0.5)
      done
    done;
    for r = 0 to min (th - 1) (rows - 1) do
      let y = ty + r in
      for c = 0 to tw - 1 do
        let x = tx + c in
        let corner =
          (r = 0 || r = th - 1) && (c < 2 || c > tw - 3)
          || (r = 1 || r = th - 2) && (c < 1 || c > tw - 2)
        in
        if not corner then begin
          let base = clay_at x y in
          let lit =
            if r = 0 || c = 0 then mix base p.glow 0.22
            else if r = th - 1 || c = tw - 1 then shade base 0.62
            else base
          in
          tint cv x y lit
        end
      done
    done
  end;

  (* the wedges *)
  if t > t_press && n > 0 then begin
    let k = int_of_float ((t -. t_press) /. per_mark) in
    let k = min k n in
    for i = 0 to k - 1 do
      let m = marks.(i) in
      let age = float_of_int (k - i) in
      (* a fresh strike glints, then the clay closes around it *)
      let heat = exp (-.age /. 26.0) in
      let fg = mix m.mfg p.glow (heat *. 0.85) in
      put cv m.mx m.my ~fg ~bold:(m.mbold || heat > 0.5) ~risky:m.mrisky m.mch
    done;
    if k < n then begin
      let m = marks.(k) in
      (* the stylus, hovering over the next impression *)
      put cv m.mx (m.my - 1) ~fg:p.glow ~bold:true "╻";
      if m.my >= 2 then put cv m.mx (m.my - 2) ~fg:(shade p.glow 0.6) "│"
    end
  end;

  let f = "  q  quit   ·   palette " ^ p.name ^ "  " in
  text cv (cv.w - utf8_len f - 1) (cv.h - 1) ~fg:(shade p.dim 0.9) f;
  true

let () =
  let hold = hold_secs () in
  let w, h = query_size () in
  let anim = total_time (create w h) in
  let dur = if hold = infinity then infinity else (anim /. sp) +. hold in
  run ~fps:30. ~keep:true ~duration:dur frame
