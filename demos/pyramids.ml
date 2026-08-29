(** pyramids — "dawn over the context stack".

    Boot animation only: the screen starts on a cold night, the sun climbs
    out of the sand, and as it does the east faces of three pyramids catch
    fire while their shadows shorten and swing.  When the sun is up the
    picture settles and a legend names each pyramid after a layer of the
    agent's context. *)

open Dtui
open Tui

let p = active ~default:"hermes" ()
let sp = speed ()

(* ── the three of them ────────────────────────────────────────────────── *)

type pyr = { pname : string; layer : string; toks : string; hgt : float; xoff : float; depth : float }

let pyrs = [
  { pname = "KHUFU";    layer = "system prompt";   toks = "2,048 tok";  hgt = 1.00; xoff = -0.22; depth = 1.00 };
  { pname = "KHAFRE";   layer = "tool schemas";    toks = "1,190 tok";  hgt = 0.78; xoff = 0.05;  depth = 0.72 };
  { pname = "MENKAURE"; layer = "live transcript"; toks = "14,662 tok"; hgt = 0.48; xoff = 0.27;  depth = 0.45 };
]

let sphinx = [|
  "        ▄▄▄            ";
  "       ▟███▙           ";
  "    ▗▄▟█████▙▄▄▄▄▄▄▄▄▖ ";
  "   ▟███████████████████▙";
  "  ▐█████████████████████▌";
|]

(* ── frame ────────────────────────────────────────────────────────────── *)

let t_rise = 0.8
let rise_len = 4.2
let t_text = t_rise +. rise_len +. 0.2
let anim_end = t_text +. 1.8

let frame cv t _i =
  let t = t *. sp in
  let w = cv.w and h = cv.h in
  let horizon = (h * 62) / 100 in
  let alt = smooth ((t -. t_rise) /. rise_len) in     (* 0 = below sand, 1 = up *)

  let sun_x = float_of_int w *. 0.80 in
  let sun_y = float_of_int horizon +. 3.0 -. (alt *. (float_of_int horizon *. 0.72)) in

  (* sky: night palette warmed towards dawn, plus the sun's own bloom *)
  for y = 0 to h - 1 do
    let u = fclamp 0. 1. (float_of_int y /. float_of_int (max 1 horizon)) in
    let night = ramp [| p.bg; shade p.sky.(1) 0.8; shade p.sky.(2) 0.7; shade p.sky.(2) 0.9 |] (u ** 0.9) in
    let dawn = ramp p.sky (u ** 0.55) in
    let base = mix night dawn alt in
    for x = 0 to w - 1 do
      let dx = (float_of_int x -. sun_x) *. 0.45 in
      let dy = float_of_int y -. sun_y in
      let d = sqrt ((dx *. dx) +. (dy *. dy)) in
      let bloom = fclamp 0. 1. (1.0 -. (d /. (float_of_int w *. 0.42))) ** 2.2 in
      tint cv x y (mix base p.glow (bloom *. 0.55 *. (0.25 +. (0.75 *. alt))))
    done
  done;

  (* stars, extinguished by the dawn *)
  let starlight = 1.0 -. smooth (alt *. 1.4) in
  if starlight > 0.02 then
    for k = 0 to (w * h) / 70 do
      let sx = int_of_float (hash2 k 61 *. float_of_int w) in
      let sy = int_of_float (hash2 k 67 *. float_of_int horizon *. 0.8) in
      let tw = 0.4 +. (0.6 *. sin ((t *. 1.1) +. (hash2 k 71 *. 6.28))) in
      let a = starlight *. tw in
      if a > 0.30 then begin
        let base = match (get cv sx sy).bg with Some b -> b | None -> p.bg in
        put cv sx sy ~fg:(mix base p.text (a *. 0.85)) (if a > 0.8 then "✦" else "·")
      end
    done;

  (* the sun itself *)
  if sun_y < float_of_int horizon +. 4.0 then begin
    let rr = 3.6 in
    for y = 0 to horizon do
      for x = 0 to w - 1 do
        let dx = (float_of_int x -. sun_x) *. 0.5 in
        let dy = float_of_int y -. sun_y in
        let d = sqrt ((dx *. dx) +. (dy *. dy)) in
        if d < rr then put cv x y ~fg:(mix p.glow (ramp p.ink 0.30) (d /. rr))
            ~bg:(mix p.glow (ramp p.ink 0.30) (d /. rr)) " "
      done
    done
  end;

  (* sand *)
  for y = horizon to h - 1 do
    let u = float_of_int (y - horizon) /. float_of_int (max 1 (h - horizon)) in
    let lit = ramp p.sand (0.15 +. (0.6 *. u)) in
    let night = shade lit 0.30 in
    for x = 0 to w - 1 do
      let n = (hash2 (x * 3) (y * 5) -. 0.5) *. 0.07 in
      tint cv x y (shade (mix night lit alt) (1.0 +. n))
    done
  done;

  (* pyramid geometry, back to front *)
  let sun_ang = 0.12 +. (alt *. 1.25) in
  List.iteri
    (fun _ pr ->
       let base_y = horizon + int_of_float (pr.depth *. float_of_int (h - horizon) *. 0.55) in
       let ph = max 5 (int_of_float (pr.hgt *. float_of_int (base_y - (if h >= 26 then 9 else 4)))) in
       let ax = (w / 2) + int_of_float (pr.xoff *. float_of_int w) in
       let ay = base_y - ph in
       let slope = 0.95 in
       (* cast shadow first: a parallelogram sliding west as the sun climbs *)
       let shadow_len = int_of_float (float_of_int ph *. 1.4 /. (0.22 +. tan sun_ang)) in
       for r = 0 to ph - 1 do
         let halfw = int_of_float (float_of_int r *. slope) in
         let sy = base_y in
         let lean = shadow_len - int_of_float (float_of_int r *. float_of_int shadow_len /. float_of_int ph) in
         for k = 0 to lean do
           let sx = ax - halfw - k in
           if sx >= 0 && sx < w && sy < h then
             match (get cv sx sy).bg with
             | Some b -> tint cv sx sy (shade b (0.55 +. (0.3 *. (float_of_int k /. float_of_int (max 1 lean)))))
             | None -> ()
         done
       done;
       (* the mass *)
       for r = 0 to ph - 1 do
         let y = ay + r in
         let halfw = int_of_float (float_of_int r *. slope) in
         let v = float_of_int r /. float_of_int ph in
         (* the top fifth keeps its polished casing, so it reads brighter *)
         let casing = if v < 0.20 then 0.22 else 0.0 in
         for x = ax - halfw to ax + halfw do
           if x >= 0 && x < w && y >= 0 && y < h then begin
             let east = x > ax in
             let edge = x = ax - halfw || x = ax + halfw in
             let course = if (r / 2) mod 2 = 0 then 1.0 else 0.93 in
             let dark = shade (mix p.shadow (ramp p.sand 0.25) 0.30) (0.9 +. (0.2 *. (1. -. v))) in
             let lit = mix (ramp p.ink (0.15 +. (0.55 *. v))) p.glow casing in
             let c =
               if east then mix dark lit (0.15 +. (0.85 *. alt))
               else mix (shade dark 0.72) (shade dark 1.35) alt
             in
             let c = shade c course in
             let g = if edge && x < ax then "╱" else if edge && x > ax then "╲" else "█" in
             (* the arris catches the most light — brighten it, don't draw it *)
             let c = if x = ax then mix c p.glow (0.30 *. alt) else c in
             put cv x y ~fg:c ~bg:(shade c 0.9) g
           end
         done
       done)
    pyrs;

  (* the sphinx, keeping watch downstage left *)
  let sx0 = 4 and sy0 = h - Array.length sphinx - 2 in
  if h >= 26 then
  Array.iteri
    (fun r row ->
       List.iteri
         (fun c ch ->
            if ch <> " " then begin
              let lit = mix (shade p.shadow 1.1) (ramp p.sand 0.35) (0.15 +. (0.55 *. alt)) in
              let top = r = 0 || (r = 1 && c < 8) in
              put cv (sx0 + c) (sy0 + r) ~fg:(if top then mix lit p.glow (0.35 *. alt) else lit) ch
            end)
         (utf8_chars row))
    sphinx;

  (* title and legend, once the sun is clear of the sand *)
  if t > t_text then begin
    let u = ease_out ((t -. t_text) /. 0.8) in
    let small = w < big_width "CARAVAN" + 6 in
    let rows = big_word ~small "CARAVAN" in
    let lw = big_width ~small "CARAVAN" in
    let lx = (w - lw) / 2 in
    List.iteri
      (fun r row ->
         let ru = ease_out ((t -. t_text -. (float_of_int r *. 0.06)) /. 0.5) in
         if ru > 0. then
           List.iteri
             (fun c ch ->
                if ch <> " " && float_of_int c < ru *. float_of_int (utf8_len row) then begin
                  let g = ramp p.ink (0.05 +. (0.65 *. (float_of_int r /. 6.))) in
                  put cv (lx + c) (1 + r) ~fg:g ~bold:(r < 2) ch
                end)
             (utf8_chars row))
      rows;
    let ly = 1 + List.length rows in
    let sub = "dawn over the context stack" in
    text cv ((w - utf8_len sub) / 2) ly ~fg:(mix p.dim p.title (0.4 *. u)) (typed sub u);

    (* legend: each pyramid is a layer of what the agent is carrying *)
    let roomy = h >= 26 in
    let lgx = max 3 (w - 47) and lgy = if roomy then h - 7 else h - 4 in
    if lgy > 2 then begin
      (* opaque interior: the pyramids stand behind this panel *)
      let pw = min 44 (w - lgx - 1) in
      let ph' = if roomy then 6 else 3 in
      for yy = lgy to lgy + ph' - 1 do
        for xx = lgx to lgx + pw - 1 do
          let under = match (get cv xx yy).bg with Some b -> b | None -> p.bg in
          put cv xx yy ~bg:(mix (shade p.bg 1.0) under 0.18) " "
        done
      done;
      if roomy then
        box cv lgx lgy pw 6 ~style:Sharp ~fg:(mix p.border p.title (0.3 *. u))
          ~title:"context" ~title_fg:p.title ();
      List.iteri
        (fun i pr ->
           let ru = ease_out ((t -. t_text -. 0.3 -. (float_of_int i *. 0.15)) /. 0.4) in
           if ru > 0. then begin
             let y = (if roomy then lgy + 1 else lgy) + i in
             put cv (lgx + 2) y ~fg:(ramp p.ink (0.2 +. (0.3 *. float_of_int i))) "▲";
             text cv (lgx + 4) y ~fg:p.title (typed (Printf.sprintf "%-9s" pr.pname) ru);
             text cv (lgx + 14) y ~fg:p.text (typed (Printf.sprintf "%-17s" pr.layer) ru);
             text cv (lgx + 32) y ~fg:p.dim (typed pr.toks ru)
           end)
        pyrs;
      let tot = "                    17,900 / 128k" in
      if roomy && t > t_text +. 0.9 then text cv (lgx + 2) (lgy + 4) ~fg:p.accent tot
    end
  end;

  let f = "  q  quit   ·   palette " ^ p.name ^ "  " in
  text cv (w - utf8_len f - 1) (h - 1) ~fg:(shade p.dim 0.9) f;
  true

let () =
  let hold = hold_secs () in
  let dur = if hold = infinity then infinity else (anim_end /. sp) +. hold in
  run ~fps:30. ~keep:true ~duration:dur frame
