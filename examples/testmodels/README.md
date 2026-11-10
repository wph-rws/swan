# Small test models for numerical questions

Three small stationary decks that run in seconds to minutes. They are meant
for questions about the iteration (when it stops, how fast it converges)
that should not need a run on a large grid. A small grid shows the same
local behaviour of the iteration as a large one.

| deck | grid | bottom | physics |
|---|---|---|---|
| `sq` | 20 x 10 cells of 500 m | 20 m, flat | `GEN3 KOMEN`, depth-induced breaking, JONSWAP friction; wind 15 m/s and a JONSWAP boundary (Hs 2 m, Tp 7.5 s) from the west |
| `sphys` | 50 x 20 cells of 500 m | 25 m in the west to 3 m in the east | the physics of the operational so-rp deck (`CDCAP=0.00275`, `WCAP KOMEN DELTA=1`, `TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5`, 0.03-1 Hz); wind 20 m/s from 300°, boundary Hs 3.65 m, Tp 7.41 s |
| `isl` | 40 x 20 cells of 250 m | 15 m, an island and a 1 m shoal behind it | as `sq`, with the semi-implicit DIA (`QUAD IQUAD=1`) |

Each deck writes Hs on the computational grid to `hs.blk` and Tm01 to
`tm.blk`, and keeps iterating until `MXITST` (`NPNTS=101`), so that a run
is a reference unless its NUMERIC line is changed. `isl` has steep
gradients: behind the island the default propagation scheme gives negative
action density, which RESCALE removes every iteration, so the iteration
never settles there.

`scripts/stop_check.py` runs a deck with several NUMERIC lines and compares
each run with a reference that keeps iterating; it is the measurement behind
the convergence section of AGENTS.md. For example:

```sh
python3 scripts/stop_check.py examples/testmodels/sphys/INPUT \
    --exe build/bin/swan.exe --work /tmp/stop-sphys \
    --reference "ref=NUMERIC STOPC DREL=0.01 CURVAT=0.001 NPNTS=101 STAT MXITST=600 ALFA=0.01" \
    --variant "stopc=NUMERIC STOPC DREL=0.01 CURVAT=0.001 NPNTS=98 STAT MXITST=40 ALFA=0.01" \
    --variant "extrap=NUMERIC STOPC DREL=0.01 CURVAT=0.001 NPNTS=98 STAT MXITST=600 ALFA=0.01 EXTRAP 10"
```
