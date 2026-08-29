(** silkroad — "the long road".

    Continuously animated: three parallax dune layers scroll under a sinking
    sun while a camel train walks the near ridge, and a goods ticker runs
    along the foot of the screen.  Nothing here settles; it is a screensaver
    for a terminal that is thinking. *)

open Dtui
open Tui

let p = active ~default:"ember" ()
let sp = speed ()

(* ── sprites ──────────────────────────────────────────────────────────── *)

(* Camel facing right: hump amidships, neck rising aft-to-fore.  Legs live
   in their own two frames so the gait can alternate. *)
let camel_body = [|
  "        ▄▄ ";
  "       ▟█▘ ";
  "   ▄▄  ▐▌  ";
  "  ▟██▙ ▐▌  ";
  " ▟███████▙ ";
  "▐█████████▌";
|]

let camel_legs = [|
  [| " ▐▌▐▌ ▐▌▐▌ "; " ▐▌ ▐▌▐▌ ▐▌" |];
  [| " ▐▌ ▐▌▐▌▐▌ "; "  ▐▌▐▌ ▐▌▐▌" |];
|]

let rider = [|
  "    ▄  ";
  "   ▟█▖ ";
|]

(* two cells wide, so the flap reads at this scale *)
let bird = [| "╲╱"; "──"; "╱╲" |]

(* ── silhouette painting ──────────────────────────────────────────────── *)

(* A solid black shape reads as a cut-out; the rim light along its top edge
   is what stops it reading as a hole in the picture. *)
let silhouette cv x y rows ~body ~rim =
  let h = Array.length rows in
  let top = Hashtbl.create 32 in
  Array.iteri
    (fun r row ->
       List.iteri
         (fun c ch -> if ch <> " " && not (Hashtbl.mem top c) then Hashtbl.add top c r)
         (utf8_chars row))
    rows;
  Array.iteri
    (fun r row ->
       List.iteri
         (fun c ch ->
            if ch <> " " then begin
              let is_top = (try Hashtbl.find top c = r with Not_found -> false) in
              put cv (x + c) (y + r) ~fg:(if is_top then rim else body) ch
            end)
         (utf8_chars row))
    rows;
  ignore h

(* ── frame ────────────────────────────────────────────────────────────── *)

let goods =
  "lapis · myrrh · saffron · silk · tin · carnelian · frankincense · indigo · jade · \
   salt · amber · nutmeg · cinnabar · turquoise · ivory · pepper · "

let tools = [| "read_file"; "grep"; "bash"; "delegate"; "write_file"; "finish" |]

let frame cv t _i =
  let t = t *. sp in
  let w = cv.w and h = cv.h in
  let horizon = (h * 62) / 100 in

  (* sky *)
  for y = 0 to h - 1 do
    let u = fclamp 0. 1. (float_of_int y /. float_of_int (max 1 horizon)) in
    let c = if y <= horizon then ramp p.sky (u ** 0.85) else p.sky.(Array.length p.sky - 1) in
    for x = 0 to w - 1 do tint cv x y c done
  done;

  (* stars: only up top, and only while the sun is still high enough to
     leave the zenith dark *)
  for k = 0 to (w * h) / 120 do
    let sx = int_of_float (hash2 k 31 *. float_of_int w) in
    let sy = int_of_float (hash2 k 37 *. float_of_int horizon *. 0.55) in
    let tw = 0.4 +. (0.6 *. sin ((t *. 1.3) +. (hash2 k 41 *. 6.28))) in
    if tw > 0.55 then begin
      let base = match (get cv sx sy).bg with Some b -> b | None -> p.bg in
      put cv sx sy ~fg:(mix base p.glow (tw *. 0.55)) "·"
    end
  done;

  (* the sun, sinking *)
  let sun_x = float_of_int w *. 0.72 in
  let sun_y = float_of_int horizon -. 5.0 +. (2.5 *. sin (t *. 0.08)) in
  let rr = 4.0 in
  for y = 0 to horizon do
    for x = 0 to w - 1 do
      let dx = (float_of_int x -. sun_x) *. 0.5 in
      let dy = float_of_int y -. sun_y in
      let d = sqrt ((dx *. dx) +. (dy *. dy)) in
      if d < rr then tint cv x y (mix p.glow (ramp p.ink 0.35) (d /. rr))
      else if d < rr *. 2.6 then begin
        let hz = 1.0 -. ((d -. rr) /. (rr *. 1.6)) in
        match (get cv x y).bg with
        | Some b -> tint cv x y (mix b p.glow (hz *. 0.32))
        | None -> ()
      end
    done
  done;

  (* birds, drifting across the disc *)
  for k = 0 to 4 do
    let bx = int_of_float (mod_float ((float_of_int w *. 1.4) -. (t *. (7.0 +. (hash2 k 3 *. 5.0)))
                                      +. (hash2 k 5 *. float_of_int w)) (float_of_int w +. 6.)) - 3 in
    let by = int_of_float ((hash2 k 7 *. float_of_int horizon *. 0.5) +. (1.5 *. sin ((t *. 1.7) +. float_of_int k))) in
    let f = bird.(int_of_float ((t *. 6.) +. float_of_int k) mod 3) in
    if bx >= 0 && bx < w - 1 then text cv bx by ~fg:(shade p.shadow 2.2) f
  done;

  (* three parallax dune layers *)
  (* atmospheric perspective: the far ridge is half dissolved in haze, the
     near one is nearly black, and each scrolls faster than the last *)
  let haze = mix (ramp p.sky 0.85) p.glow 0.12 in
  let deep = shade p.shadow 0.75 in
  let layers = [
    (0, 2.0, 2.5, 34.0, 0.30, 3, -1);
    (1, 6.5, 3.5, 22.0, 0.62, 2, 2);
    (2, 15.0, 5.0, 15.0, 1.00, 1, 6);
  ] in
  let ridge = Array.make w horizon in
  List.iter
    (fun (li, speed, amp, wavelen, dark, seed, drop) ->
       let base = horizon + drop in
       for x = 0 to w - 1 do
         let u = ((float_of_int x +. (t *. speed)) /. wavelen) +. (float_of_int li *. 11.) in
         let hgt = dune u seed 3 *. amp in
         let y0 = base - int_of_float hgt in
         if li = 2 then ridge.(x) <- y0;
         let c = mix haze deep dark in
         for y = max 0 y0 to h - 1 do put cv x y ~fg:c ~bg:c " " done;
         if y0 >= 0 && y0 < h then begin
           let rim = mix (ramp p.ink 0.20) haze (float_of_int li /. 2.5) in
           put cv x y0 ~fg:(mix rim c (dark *. 0.45)) ~bg:c "▔"
         end
       done)
    layers;

  (* the caravan, treading the near ridge *)
  let camel_w = 11 in
  let n_camels = max 2 (min 4 (w / 26)) in
  let gap = 6 in
  let lead_x = (w / 2) - ((n_camels * (camel_w + gap)) / 2) in
  for i = 0 to n_camels - 1 do
    let cx = lead_x + (i * (camel_w + gap)) + int_of_float (1.5 *. sin ((t *. 1.1) +. (float_of_int i *. 0.7))) in
    let phase = (t *. 3.2) +. (float_of_int i *. 1.3) in
    let bob = if sin phase > 0. then 0 else 1 in
    let leg = if sin (phase *. 1.0) > 0. then 0 else 1 in
    (* stand on the highest sand under the whole animal, so the ridge line
       never cuts through its legs *)
    let ground =
      let g = ref horizon in
      for k = 0 to camel_w - 1 do
        let x = cx + k in
        if x >= 0 && x < w then g := min !g ridge.(x)
      done;
      !g
    in
    let top = ground - Array.length camel_body - 2 + bob in
    let body = shade p.shadow 0.9 and rim = mix (ramp p.ink 0.30) p.glow 0.25 in
    silhouette cv cx top camel_body ~body ~rim;
    let ly = top + Array.length camel_body in
    silhouette cv cx ly [| camel_legs.(0).(leg) |] ~body ~rim:body;
    silhouette cv cx (ly + 1) [| camel_legs.(1).(leg) |] ~body ~rim:body;
    if i = 0 then silhouette cv cx top rider ~body ~rim;
    (* the pack each camel carries: a Caravan tool, riding along *)
    let label = tools.(i mod Array.length tools) in
    if top - 4 > 0 then
      text cv (cx + ((camel_w - utf8_len label) / 2)) (top - 4)
        ~fg:(mix p.dim p.accent (0.4 +. (0.3 *. sin ((t *. 2.) +. float_of_int i)))) label
  done;

  (* banner *)
  let small = w < 70 in
  let rows = big_word ~small "CARAVAN" in
  List.iteri
    (fun r row ->
       gradient_text cv 2 (1 + r) p.ink ~bold:(r < 2) row)
    (if small then rows else rows);
  let tag = "the long road · press q to make camp" in
  text cv 3 (1 + List.length rows) ~fg:p.dim tag;

  (* goods ticker *)
  let ty = h - 1 in
  let gl = utf8_len goods in
  let off = int_of_float (t *. 9.0) mod gl in
  let strip = utf8_sub (goods ^ goods) off w in
  for x = 0 to w - 1 do tint cv x ty (shade p.bg 1.0) done;
  List.iteri
    (fun i ch ->
       let edge = min i (w - 1 - i) in
       let fade = fclamp 0. 1. (float_of_int edge /. 8.) in
       put cv i ty ~fg:(mix p.bg p.accent (0.25 +. (0.6 *. fade))) ch)
    (utf8_chars strip);
  true

let () =
  let hold = hold_secs () in
  run ~fps:30. ~keep:true ?duration:(if hold = infinity then None else Some (6.0 +. hold)) frame
