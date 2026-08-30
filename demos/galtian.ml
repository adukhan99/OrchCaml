(** galtian — "duotone caravan".

    Terminal-brutalist and fully animated: a plasma field halftoned through
    a 4×4 Bayer matrix, the word CARAVAN punched through it as a window into
    an inverted colour space, scanlines, and periodic RGB-split tearing.
    A nod to the psychedelic terminal work John Galt does for Nous Research,
    whose Hermes Agent these demos otherwise take their manners from. *)

open Dtui
open Tui

let p = active ~default:"nous" ()
let sp = speed ()

(* ── the field ────────────────────────────────────────────────────────── *)

let plasma x y t =
  let fx = float_of_int x and fy = float_of_int y *. 2.0 in
  let v =
    sin ((fx /. 7.0) +. t)
    +. sin ((fy /. 5.0) -. (t *. 0.7))
    +. sin (((fx +. fy) /. 11.0) +. (t *. 1.3))
    +. sin ((sqrt ((fx *. fx) +. (fy *. fy)) /. 9.0) -. (t *. 1.1))
  in
  (v +. 4.0) /. 8.0

(* Halftone one cell: pick the pair of ramp stops it falls between and let a
   block glyph carry the fraction.  This is what makes a 16-colour-looking
   gradient out of five stops. *)
let halftone cv x y stops v ~bold =
  let n = Array.length stops - 1 in
  let q = fclamp 0. 0.9999 v *. float_of_int n in
  let i = int_of_float q in
  let f = q -. float_of_int i in
  let lo = stops.(i) and hi = stops.(min n (i + 1)) in
  put cv x y ~fg:hi ~bg:lo ~bold (shade_at f)

(* ── letter mask ──────────────────────────────────────────────────────── *)

let mask_of cv word =
  let small = cv.w < big_width word + 6 in
  let rows = big_word ~small word in
  let lw = big_width ~small word in
  let lh = List.length rows in
  let x0 = (cv.w - lw) / 2 and y0 = (cv.h - lh) / 2 - 1 in
  let m = Hashtbl.create 4096 in
  (* Only the ██ body counts.  The ANSI-Shadow font draws each letter's drop
     shadow with box-drawing characters, and those run straight through the
     counters of A and R — masking every non-space cell fills the holes in
     and the word loses its shape.  The two-row fallback font has no shadow
     to strip, so there we take everything. *)
  let solid ch = small || ch = "█" in
  List.iteri
    (fun r row ->
       List.iteri
         (fun c ch -> if ch <> " " && solid ch then Hashtbl.replace m (x0 + c, y0 + r) ch)
         (utf8_chars row))
    rows;
  (* tighten the bounds to what actually got masked, so the frame does not
     hang off the blank row the stripped shadow leaves behind *)
  let xa = ref max_int and xb = ref min_int and ya = ref max_int and yb = ref min_int in
  Hashtbl.iter
    (fun (x, y) _ ->
       if x < !xa then xa := x;
       if x > !xb then xb := x;
       if y < !ya then ya := y;
       if y > !yb then yb := y)
    m;
  if !xa = max_int then (m, x0, y0, lw, lh)
  else (m, !xa, !ya, !xb - !xa + 1, !yb - !ya + 1)

(* ── HUD ──────────────────────────────────────────────────────────────── *)

let bar v width =
  let full = int_of_float (fclamp 0. 1. v *. float_of_int width) in
  String.concat "" (List.init width (fun i -> if i < full then "▍" else "▁"))

let stats t = [
  ("ENTROPY",  0.50 +. (0.42 *. sin (t *. 0.53)),        Printf.sprintf "%.3f" (0.50 +. (0.42 *. sin (t *. 0.53))));
  ("TEMP",     0.70,                                     "0.700");
  ("CTX",      0.14 +. (0.10 *. sin (t *. 0.21)),        "17.9k/128k");
  ("REALMS",   0.25,                                     "1");
  ("TOOLS",    1.00,                                     "12");
]

(* ── frame ────────────────────────────────────────────────────────────── *)

let frame cv t _i =
  let t = t *. sp in
  let w = cv.w and h = cv.h in
  let mask, mx, my, mw, mh = mask_of cv "CARAVAN" in

  (* horizontal tear: a few rows slip sideways for a moment at a time *)
  let bucket = int_of_float (t *. 3.4) in
  let tear y =
    let r = hash2 y (bucket * 977) in
    if r > 0.955 then int_of_float ((hash2 y (bucket * 31) -. 0.5) *. 14.0) else 0
  in

  (* A moat around the letterforms.  Without it the word sits in the middle
     of a field of the same brightness and the eye cannot find its edges;
     two cells of near-black is what makes it snap into focus. *)
  let halo = Hashtbl.create 8192 in
  Hashtbl.iter
    (fun (cx, cy) _ ->
       for dy = -2 to 2 do
         for dx = -2 to 2 do
           let k = (cx + dx, cy + dy) in
           if not (Hashtbl.mem mask k) then begin
             let d = max (abs dx) (abs dy) in
             match Hashtbl.find_opt halo k with
             | Some p when p <= d -> ()
             | _ -> Hashtbl.replace halo k d
           end
         done
       done)
    mask;

  for y = 0 to h - 1 do
    let dx = tear y in
    for x = 0 to w - 1 do
      (* the mask is sampled unshifted, so a tear slides the field behind
         the word without ever dragging the word itself out of shape *)
      let inside = Hashtbl.mem mask (x, y) in
      let ring = Hashtbl.find_opt halo (x, y) in
      let torn = dx <> 0 && (not inside) && ring = None in
      let sxx = if torn then x + dx else x in
      let v = plasma sxx y t in
      (* scanlines: the odd rows sit back a step *)
      let v = if y land 1 = 1 then v *. 0.80 else v in
      (* a slow specular sweep across the whole field *)
      let sweep = mod_float ((t *. 26.) +. (float_of_int y *. 0.6)) (float_of_int w *. 1.6) in
      let d = abs_float (float_of_int x -. sweep) in
      let v = if d < 6.0 then fclamp 0. 1. (v +. ((1.0 -. (d /. 6.0)) *. 0.30)) else v in
      match inside, ring with
      | true, _ ->
        (* Solid strokes, never halftoned: glyph density was breaking the
           letterforms up into texture.  All the modulation is in the
           colour, and the top edge of every stroke is lit so the word
           reads as raised rather than as a hole. *)
        let lit_top = not (Hashtbl.mem mask (x, y - 1)) in
        let core = mix p.glow p.accent (0.15 +. (0.45 *. v)) in
        let fg = if lit_top then mix core p.glow 0.75 else core in
        put cv x y ~fg ~bg:(shade p.bg 1.0) ~bold:true "█"
      | false, Some r ->
        let keep = if r = 1 then 0.0 else 0.18 in
        let c = mix (shade p.bg 1.0) (ramp p.ink v) keep in
        put cv x y ~fg:(mix c p.accent (keep *. 0.5)) ~bg:(shade p.bg 1.0)
          (if r = 1 then " " else shade_at (v *. 0.35))
      | false, None ->
        halftone cv x y p.ink v ~bold:false;
        if torn then begin
          (* RGB split: the slipped band biases towards the two poles *)
          let c = get cv x y in
          let bias = if y land 1 = 0 then p.accent else p.title in
          put cv x y ?fg:(Option.map (fun f -> mix f bias 0.55) c.fg)
            ?bg:(Option.map (fun b -> mix b (shade bias 0.35) 0.45) c.bg) ~bold:true c.ch
        end
    done
  done;

  (* a hairline frame around the word, so the cutout reads as deliberate *)
  if mw + 2 < w && mh + 2 < h then begin
    let pad = 4 in
    for x = mx - pad to mx + mw + pad - 1 do
      List.iter
        (fun y ->
           if x >= 0 && x < w && y >= 0 && y < h then
             put cv x y ~fg:(mix p.accent p.glow (0.4 +. (0.3 *. sin (t *. 3.)))) "─")
        [ my - pad; my + mh + pad - 1 ]
    done;
    for y = my - pad to my + mh + pad - 1 do
      List.iter
        (fun x ->
           if x >= 0 && x < w && y >= 0 && y < h then
             put cv x y ~fg:(mix p.accent p.glow (0.4 +. (0.3 *. sin (t *. 3.)))) "│")
        [ mx - pad; mx + mw + pad - 1 ]
    done
  end;

  (* corner tags *)
  let plate x y s fg =
    let n = utf8_len s + 2 in
    for i = 0 to n - 1 do put cv (x + i) y ~bg:(shade p.bg 1.0) " " done;
    text cv (x + 1) y ~fg ~bg:(shade p.bg 1.0) ~bold:true s
  in
  plate 1 1 "◈ NOUS // CARAVAN" p.glow;
  plate 1 2 "duotone field · plasma / bayer 4×4" p.accent;
  let rt = Printf.sprintf "T+%07.2f  %s" t (spinner t) in
  plate (w - utf8_len rt - 3) 1 rt p.title;

  (* HUD strip *)
  let hy = h - 3 in
  if hy > 4 then begin
    for x = 0 to w - 1 do
      for y = hy - 1 to h - 1 do put cv x y ~fg:p.dim ~bg:(shade p.bg 1.0) " " done
    done;
    hline cv 0 (hy - 1) w ~fg:(shade p.accent 0.6) ~bg:(shade p.bg 1.0) "─";
    let items = stats t in
    let colw = w / List.length items in
    List.iteri
      (fun i (k, v, txt) ->
         let x = (i * colw) + 2 in
         let bw = max 4 (min 8 (colw - 13)) in
         if x + bw + 3 < w then begin
           text cv x hy ~fg:p.accent ~bold:true k;
           text cv x (hy + 1) ~fg:(mix p.title p.accent 0.4) (bar v bw);
           let room = max 0 (w - (x + bw + 2)) in
           text cv (x + bw + 1) (hy + 1) ~fg:p.text (utf8_sub txt 0 (min room (colw - bw - 2)))
         end)
      items;
    let credit = "after the terminal work of John Galt · Nous Research     q quit  " in
    if utf8_len credit + 2 < w then
      text cv (w - utf8_len credit - 1) (h - 1) ~fg:(shade p.dim 1.1) credit
  end;
  true

let () =
  let hold = hold_secs () in
  run ~fps:24. ~keep:true ?duration:(if hold = infinity then None else Some (5.0 +. hold)) frame
