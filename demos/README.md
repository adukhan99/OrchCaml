# Caravan terminal demos

Five self-contained terminal pieces for Caravan, plus a runner that lets you
re-skin every one of them from the outside.

## Running them

They are a standalone dune project — nothing to install, no changes to any
other repo, no opam dependencies beyond `unix`:

```
unzip caravan-demos.zip
cd caravan-demos
chmod +x caravan-demo      # if your unzip dropped the mode bit
./caravan-demo             # builds on first run, then shows the menu
```

`./caravan-demo` builds for you; `dune build` by hand works too. You need
OCaml >= 4.08 and dune >= 3.0 on `PATH` (`eval $(opam env)` if you use opam).
Verified from a cold extract with OCaml 5.4 / dune 3.21.

**All eleven files must be present.** `dune` names every module explicitly,
so a missing one stops the build with `Error: Module <Name> doesn't exist`
instead of quietly dropping a demo:

```
dune-project   dune          caravan-demo   README.md    tui.ml
swatch.ml      cuneiform.ml  silkroad.ml    pyramids.ml  galtian.ml
oasis.ml
```

`tui.ml` is the shared renderer every demo links against and `swatch.ml`
backs `--palettes`; neither is optional.

If you would rather have them inside a Caravan checkout, drop the directory
in as `demos/` and delete `dune-project` — `caravan-demo` detects both
layouts and builds the right target either way.

```
./caravan-demo                 # interactive menu
./caravan-demo oasis           # run one
./caravan-demo 3 -p nous       # by number, in another palette
./caravan-demo --tour          # all five, a different palette each
./caravan-demo --palettes      # print every palette as colour bars
```

`q` or `Esc` quits any demo. The final frame is left in your scrollback
instead of being wiped, so you can scroll back through a tour.

## The five

| | demo | animation | what it is |
|---|---|---|---|
| 1 | `cuneiform` | boot only | A clay slab presses out of the dark and a stylus impresses the session record into it, wedge by wedge. Real Sumero-Akkadian glyphs down the left column. Default palette **lapis**. |
| 2 | `silkroad` | continuous | Three parallax dune layers scroll under a sinking sun while a camel train walks the near ridge; a goods ticker runs along the foot. Default **ember**. |
| 3 | `pyramids` | boot only | Night to dawn: the sun climbs out of the sand, the east faces catch fire, shadows shorten and swing, and a legend names each pyramid after a layer of the agent's context. Default **hermes**. |
| 4 | `galtian` | continuous | A plasma field halftoned through a 4×4 Bayer matrix, CARAVAN cut clean out of it as a black window, scanlines and RGB-split tearing. After the terminal work John Galt does for Nous Research. Default **nous**. |
| 5 | `oasis` | boot only | The most product-shaped one: a Hermes-Agent-style block banner, an info panel, and a simulated trace stream in the exact shapes `bin/render.ml` prints — beside a palm-fringed pool. Default **hermes**. |

## Palettes

`hermes` `lapis` `ember` `oasis` `nous` `phosphor` `bone`

Every demo reads its colours from the same table in `tui.ml`, so any demo
runs in any palette. `--palettes` prints them as colour bars; in the menu,
`p` / `P` cycle and `c` shows the bars.

`bone` is greyscale-safe and `phosphor` is a single hue, so those two are
the ones to reach for if 24-bit colour is unavailable.

## Environment knobs

The runner only sets these; you can set them yourself and run a demo
directly.

| variable | effect |
|---|---|
| `CARAVAN_DEMO_PALETTE` | one of the names above; overrides the demo's default |
| `CARAVAN_DEMO_SPEED` | animation rate multiplier (`2` = twice as fast) |
| `CARAVAN_DEMO_HOLD` | seconds to linger on the last frame; unset means wait for a key |
| `CARAVAN_DEMO_ASCII` | `1` swaps exotic glyphs for safe stand-ins |
| `CARAVAN_DEMO_TRUECOLOR` | `0` forces the 256-colour fallback |

```
CARAVAN_DEMO_PALETTE=phosphor ./_build/default/galtian.exe
```

## How it is put together

`tui.ml` is a small cell-buffer renderer. A demo clears a `canvas`, paints
styled cells into it, and hands it to `present`, which emits one frame with
the cursor parked at home — no clear, so no flicker. Colours are truecolor
with a 256-colour fallback chosen from `COLORTERM`.

Two details worth knowing if you extend these:

- **`put` preserves an existing background** unless you pass one. The demos
  paint a coloured ground first (sky, sand, clay) and then stamp glyphs on
  top of it; without this every glyph would punch a hole in the scenery.
- **Cells can be marked `risky`.** Cuneiform lives outside the BMP and
  terminals disagree about how wide those glyphs are. Any row containing a
  risky cell is repainted cell-by-cell with absolute cursor moves, so a
  terminal that guesses the width differently cannot shift the rest of the
  grid.

Everything reflows: `query_size` reads `stty size`, `SIGWINCH` rebuilds the
canvas mid-run, and each demo falls back to a two-row block font when the
terminal is too narrow for the six-row one. Tested from 60×20 up to 200×50.

`swatch.ml` exists so the runner does not have to duplicate the colour
tables in bash.
