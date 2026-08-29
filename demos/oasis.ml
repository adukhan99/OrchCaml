(** oasis — "the agent makes camp".

    The most product-shaped of the five: a Hermes-Agent-style boot banner
    (block logo, gradient, info panel) with a palm-fringed pool drawn
    procedurally beside it, then a simulated Caravan trace stream in the
    exact shapes [bin/render.ml] prints.  Animation is boot-only: once the
    trace finishes, the picture is still. *)

open Dtui
open Tui

let p = active ~default:"hermes" ()
let sp = speed ()

(* ── the pool and its palms ───────────────────────────────────────────── *)

(* Trunk and fronds are parametric so the crown can sway; a hand-drawn
   palm would sit dead on the page, and swaying is most of the charm. *)
(* Hand-drawn sprites: at this cell count a procedural frond just reads as
   scribble, so the palms are authored and the wind is applied as a shear
   of the crown rows against the fixed trunk. *)
let palm_big = [|
  "    ╲╲╲  ╷  ╱╱╱    ";
  "  ╲══╲╲╲ │ ╱╱╱══╱  ";
  " ══╲╲══╲╲│╱╱══╱╱══ ";
  "   ╱╱ ╱  │  ╲ ╲╲   ";
  "         ║         ";
  "         ║         ";
  "        ╱║         ";
  "        ▙█▟        ";
|]

let palm_small = [|
  "   ╲╲ ╷ ╱╱   ";
  " ══╲╲╲│╱╱╱══ ";
  "   ╱ ╱│╲ ╲   ";
  "      ║      ";
  "      ║      ";
  "     ▙█▟     ";
|]

let is_trunk ch = ch = "║" || ch = "█" || ch = "▙" || ch = "▟"

let palm cv ~x ~ground ~sprite ~sway ~fade =
  let col k = mix p.bg k fade in
  let n = Array.length sprite in
  Array.iteri
    (fun r row ->
       let y = ground - (n - 1) + r in
       let lift = float_of_int (n - 1 - r) /. float_of_int n in
       List.iteri
         (fun c ch ->
            if ch <> " " then begin
              if is_trunk ch then begin
                let t = 1.0 -. lift in
                put cv (x + c) y
                  ~fg:(col (mix (shade p.sand.(2) 0.9) (shade p.sand.(0) 0.6) t)) ch
              end else begin
                let dx = int_of_float (Float.round (sway *. lift *. 1.6)) in
                let leaf = mix p.ok (shade p.ok 0.5) (0.2 +. (0.6 *. float_of_int r /. float_of_int n)) in
                put cv (x + c + dx) y ~fg:(col leaf) ch
              end
            end)
         (utf8_chars row))
    sprite;
  (* dates in the notch under the crown *)
  let tr = ref 0 in
  Array.iteri (fun r row -> if !tr = 0 && String.length row > 0 && List.exists is_trunk (utf8_chars row) then tr := r) sprite;
  let cy = ground - (n - 1) + !tr in
  let cx = x + (utf8_len sprite.(0) / 2) in
  List.iter
    (fun (dx, k) -> if (get cv (cx + dx) cy).ch = " " then put cv (cx + dx) cy ~fg:(col (shade p.warn k)) "●")
    [ (-2, 1.0); (-1, 0.75); (2, 0.9) ]

let pool cv ~x ~y ~w ~t ~fade =
  let col k = mix p.bg k fade in
  for i = 0 to w - 1 do
    let u = float_of_int i /. float_of_int (max 1 w) in
    let depth = sin (u *. pi) in
    if depth > 0.05 then begin
      let wob = sin ((float_of_int i *. 0.55) -. (t *. 2.1)) in
      let g = if wob > 0.45 then "≈" else if wob > -0.3 then "~" else "─" in
      let c = mix p.accent (shade p.accent 0.35) ((wob +. 1.) /. 2.) in
      put cv (x + i) y ~fg:(col c) g;
      if depth > 0.55 then
        put cv (x + i) (y + 1) ~fg:(col (shade p.accent 0.4)) (if wob > 0. then "~" else "─")
    end
  done

(* ── the banner block ─────────────────────────────────────────────────── *)

let logo_rows small = big_word ~small "CARAVAN"

let draw_logo cv ~x ~y ~t ~small =
  (* Each row wipes in left-to-right, one after another, and a specular
     highlight travels across the finished letters. *)
  let rows = logo_rows small in
  let n = List.length rows in
  List.iteri
    (fun r row ->
       let start = 0.10 +. (float_of_int r *. 0.09) in
       let u = ease_out ((t -. start) /. 0.45) in
       if u > 0. then begin
         let cs = utf8_chars row in
         let total = List.length cs in
         let shown = int_of_float (u *. float_of_int total) in
         let sweep = ((t -. 1.1) *. 46.) -. float_of_int (r * 2) in
         List.iteri
           (fun c ch ->
              if c < shown && ch <> " " then begin
                let g = float_of_int r /. float_of_int (max 1 (n - 1)) in
                let base = ramp p.ink (0.10 +. (g *. 0.8)) in
                let d = abs_float (float_of_int c -. sweep) in
                let hot = if d < 5.0 then (1.0 -. (d /. 5.0)) ** 2.0 else 0.0 in
                let edge = if c = shown - 1 && u < 1.0 then 0.85 else 0.0 in
                let fg = mix base p.glow (max hot edge) in
                put cv (x + c) (y + r) ~fg ~bold:(r < 2) ch
              end)
           cs
       end)
    rows

(* ── the simulated trace ──────────────────────────────────────────────── *)

type ev = { icon : string; icol : rgb; body : string; bcol : rgb; tail : string }

let events () = [
  { icon = "●"; icol = p.tool; body = "read_file"; bcol = p.text;
    tail = "(path=\"docs/COMPOSABILITY_NOTES.md\")" };
  { icon = "⎿"; icol = p.dim; body = "4.2 kB"; bcol = p.dim; tail = " · 12ms" };
  { icon = "●"; icol = p.tool; body = "grep"; bcol = p.text;
    tail = "(pattern=\"Reconcile\", path=\"lib/\")" };
  { icon = "⎿"; icol = p.dim; body = "7 matches"; bcol = p.dim; tail = " · 31ms" };
  { icon = "↳"; icol = p.accent; body = "[subagent: scribe]"; bcol = p.text;
    tail = " task: summarise the plugin lifecycle" };
  { icon = "⎿"; icol = p.dim; body = "complete"; bcol = p.dim; tail = " · 3 phases, 25 tests · 1.4s" };
  (* render.ml prints ⛔ here, but that codepoint is East-Asian-wide and
     would shift the rest of the row; ⊘ carries the same meaning in one cell *)
  { icon = "⊘"; icol = p.warn; body = "permission denied"; bcol = p.warn; tail = ": write_file" };
  { icon = "✦"; icol = p.dim; body = "compacting context"; bcol = p.dim; tail = " · 18k → 4k tokens" };
  { icon = "✔"; icol = p.ok; body = "caravan ready"; bcol = p.ok; tail = " — 12 tools · 4 providers · 1 plugin realm" };
]

let info_rows = [
  ("provider", "openai-compatible · gpt-oss-120b");
  ("context",  "128k · compaction at 80%");
  ("tools",    "read_file  write_file  grep  bash  delegate");
  ("plugins",  "plugin-runtime · 1 realm · 25 tests green");
  ("home",     "~/.caravan/config.toml");
]

(* ── frame ────────────────────────────────────────────────────────────── *)

let t_scene = 0.6
let t_box = 1.75
let t_trace = 2.65
let ev_step = 0.42

let anim_end = t_trace +. (float_of_int (List.length (events ())) *. ev_step) +. 0.6

let frame cv t _i =
  let t = t *. sp in
  (* sky: a shallow vertical wash so the panel has something to sit on *)
  for y = 0 to cv.h - 1 do
    let u = float_of_int y /. float_of_int (max 1 (cv.h - 1)) in
    let c = ramp [| p.bg; p.bg; shade p.sky.(1) 0.55; shade p.sky.(2) 0.35 |] u in
    for x = 0 to cv.w - 1 do tint cv x y c done
  done;
  (* stars, thinning towards the horizon *)
  for k = 0 to (cv.w * cv.h) / 90 do
    let x = int_of_float (hash2 k 3 *. float_of_int cv.w) in
    let y = int_of_float (hash2 k 5 *. float_of_int cv.h *. 0.62) in
    let tw = 0.45 +. (0.55 *. sin ((t *. 1.6) +. (hash2 k 9 *. 6.28))) in
    if tw > 0.35 then
      put cv x y ~fg:(mix p.bg p.text (tw *. 0.6)) (if tw > 0.9 then "✦" else "·")
  done;

  let small = cv.w < big_width "CARAVAN" + 8 in
  let logo_w = big_width ~small "CARAVAN" in
  let logo_h = if small then 2 else 6 in

  (* scene: the oasis keeps to the right margin so the trace column below
     the panel always has clear ground to print on *)
  let fade = ease_out ((t -. t_scene) /. 1.1) in
  let scene_w = min 40 (max 34 (cv.w / 3)) in
  let scene_x = cv.w - scene_w in
  if fade > 0.01 && cv.w >= 92 then begin
    let ground = cv.h - 3 in
    let sway = sin (t *. 0.9) +. (0.4 *. sin (t *. 1.7)) in
    (* dune line first, so the palms stand in front of it *)
    for x = 0 to cv.w - 1 do
      let hgt = 1.0 +. (dune ((float_of_int x /. 26.) +. 3.0) 17 3 *. 2.4) in
      let y = ground + 1 - int_of_float hgt in
      if y > 0 && y < cv.h then
        for yy = y to cv.h - 1 do
          let d = float_of_int (yy - y) /. float_of_int (max 1 (cv.h - y)) in
          tint cv x yy (mix p.bg (shade p.sand.(0) 0.5) (fade *. (0.30 +. (0.45 *. d))))
        done
    done;
    palm cv ~x:(scene_x + 1) ~ground ~sprite:palm_big ~sway ~fade;
    palm cv ~x:(scene_x + 20) ~ground:(ground - 1) ~sprite:palm_small ~sway:(sway *. 0.7) ~fade;
    pool cv ~x:(scene_x + 2) ~y:(ground + 1) ~w:(scene_w - 4) ~t ~fade
  end;

  (* banner *)
  let lx = max 2 ((cv.w - logo_w) / 2) in
  let ly = 1 in
  draw_logo cv ~x:lx ~y:ly ~t ~small;
  if t > 0.95 then begin
    let sub = "a typed LLM orchestration framework for OCaml" in
    let s = typed sub (ease_out ((t -. 0.95) /. 0.7)) in
    text cv ((cv.w - utf8_len sub) / 2) (ly + logo_h) ~fg:p.dim s
  end;

  (* info panel *)
  let bw = min (cv.w - 4) 62 in
  let bx = (cv.w - bw) / 2 in
  let by = ly + logo_h + 2 in
  let bh = List.length info_rows + 2 in
  if t > t_box && by + bh < cv.h then begin
    let u = ease_out ((t -. t_box) /. 0.5) in
    let shown_w = max 2 (int_of_float (u *. float_of_int bw)) in
    let sx = bx + ((bw - shown_w) / 2) in
    (* opaque interior, or a star from the sky shows through the panel *)
    for yy = by to by + bh - 1 do
      for xx = sx to sx + shown_w - 1 do
        let under = match (get cv xx yy).bg with Some b -> b | None -> p.bg in
        put cv xx yy ~bg:(mix (shade p.bg 1.0) under 0.22) " "
      done
    done;
    box cv sx by shown_w bh ~style:Round ~fg:p.border ~title:"caravan · session" ~title_fg:p.title ();
    if u >= 1.0 then
      List.iteri
        (fun i (k, v) ->
           let ru = ease_out ((t -. t_box -. 0.35 -. (float_of_int i *. 0.09)) /. 0.3) in
           if ru > 0. then begin
             text cv (bx + 2) (by + 1 + i) ~fg:p.dim (Printf.sprintf "%-9s" k);
             text cv (bx + 12) (by + 1 + i) ~fg:p.text (typed v ru)
           end)
        info_rows
  end;

  (* trace stream *)
  let ty = by + bh + 1 in
  if t > t_trace && ty < cv.h then begin
    let evs = events () in
    List.iteri
      (fun i e ->
         let start = t_trace +. (float_of_int i *. ev_step) in
         let u = (t -. start) /. 0.3 in
         if u > 0. && ty + i < cv.h - 1 then begin
           let indent = if e.icon = "⎿" then 4 else 2 in
           let line = e.body ^ e.tail in
           let shown = typed line (fclamp 0. 1. u) in
           let ic =
             if u < 1.0 && e.icon = "●" then spinner (t *. 1.4) else e.icon
           in
           put cv indent (ty + i) ~fg:e.icol ~bold:(e.icon = "✔") ic;
           let cs = utf8_chars shown in
           List.iteri
             (fun c ch ->
                let fg = if c < utf8_len e.body then e.bcol else p.dim in
                put cv (indent + 2 + c) (ty + i) ~fg ~bold:(e.icon = "✔" && c < utf8_len e.body) ch)
             cs;
           if u < 1.0 then put cv (indent + 2 + List.length cs) (ty + i) ~fg:p.accent "▎"
         end)
      evs
  end;

  (* footer *)
  if t > anim_end -. 0.3 then begin
    let f = "  q  quit      ·      CARAVAN_DEMO_PALETTE=" ^ p.name ^ "  " in
    text cv (cv.w - utf8_len f - 1) (cv.h - 1) ~fg:(shade p.dim 0.8) f
  end;
  true

let () =
  let hold = hold_secs () in
  let dur = if hold = infinity then infinity else (anim_end /. sp) +. hold in
  run ~fps:30. ~keep:true ~duration:dur frame
