(** swatch — print every palette as colour bars.

    The runner shells out to this instead of duplicating the colour tables
    in bash, so [tui.ml] stays the single source of truth for the palettes. *)

open Dtui
open Tui

let bar stops n =
  let b = Buffer.create 256 in
  for i = 0 to n - 1 do
    let c = ramp stops (float_of_int i /. float_of_int (max 1 (n - 1))) in
    Buffer.add_string b (sgr_bg c);
    Buffer.add_string b " "
  done;
  Buffer.add_string b "\027[0m";
  Buffer.contents b

let dot c = sgr_fg c ^ "●" ^ "\027[0m"

let () =
  let current = match Sys.getenv_opt "CARAVAN_DEMO_PALETTE" with Some s -> s | None -> "" in
  print_string "\n";
  Printf.printf "  %s%-10s %-16s %-10s %-10s %-11s %s%s\n"
    "\027[1m" "PALETTE" "INK" "SKY" "SAND" "SEMANTIC" "DESCRIPTION" "\027[0m";
  List.iter
    (fun p ->
       let mark = if p.name = current then sgr_fg p.title ^ "▸" ^ "\027[0m" else " " in
       Printf.printf "%s %s%-9s\027[0m %s %s %s  %s%s%s%s%s  %s%s\027[0m\n"
         mark (sgr_fg p.title) p.name
         (bar p.ink 16) (bar p.sky 10) (bar p.sand 10)
         (dot p.title) (dot p.accent) (dot p.ok) (dot p.warn) (dot p.err)
         (sgr_fg p.dim) p.desc)
    palettes;
  Printf.printf "\n  %struecolor: %s   glyphs: %s\027[0m\n\n"
    "\027[2m"
    (if truecolor then "24-bit" else "256-colour fallback")
    (if ascii_only then "ascii-safe" else "full unicode")
