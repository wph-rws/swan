# so-rp condition matrix

The matrix runs the operational so-rp computation (Oosterschelde, Scaloost
and the boundary posts) for ten conditions and six targets. The comparison
between versions uses `../par/so-rp_osk_org.swn`: the operational deck of
Rijkswaterstaat with two corrections, a grid of 248 × 577 cells and the
boundary on the wet sea boundary. Why, and what the operational deck looks
like, is described in `../par/operational/PROVENANCE.md`. `deck.py` fills in
the template the way `Run_so-rp_swan.sh` does, and refuses a corrected
template that has the old grid or the old boundary.

Two targets use the production deck itself (`template: production`), with its
wrong grid and boundary, because that is what production computes today.
`deck.py` accepts that deck only if it is byte for byte the file in
`../par/operational/` (md5 check). These targets show what production gives,
and what it would give with the current code; they are not a basis for
comparing versions. A run without `--target` skips them (`default_run: false`),
so the full matrix stays at 40 runs; name them to run them.

## Contents

- `conditions.json`: ten conditions (wind, direction, water level, barrier
  open or closed) and six targets: TU Delft upstream 41.51 built with the
  same compiler, RWS 41.31A.1, the current build with the deck as it is
  (`operational`), and the current build with the 41.31 behaviour that the deck
  does not set itself, as far as a deck can set it (`legacy-4131`: `DRAG FIT`,
  and `DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2` on the `TRIAD` line), all with
  the corrected template; and RWS 41.31A.1 and the current build with the
  production deck (`rws_4131_production`, `current_4151_production`).
- `deck.py`: filling in and checking a deck, and `stage_inputs`, which prepares
  a run directory: `so-rp/so-rp.rgf`, `so-rp/scaloost_rp.bot.recht`,
  `par/uitvoerpunten.par`, an empty `dat/` and the `swaninit` with `#` as the
  comment character. Grid and bottom are not in git; they belong in
  `so-rp_swan/so-rp/`.
- `bg2.py`: the boundary waves from the BG2 regression, as in
  `Bereken_golven_bg2.sh`.
- `matrix_runner.py`: one run per directory, never overwritten.
- `convergence_check.py` and `stopping_study.py`: the convergence check.

There are no measurement results here.

## Running

Check the manifest, the programs and all decks without running SWAN:

```sh
python3 so-rp_swan/matrix/matrix_runner.py plan
```

The two current targets expect an OpenMP build in `build-so-rp-current/`;
`plan` reports `MISSING` until it exists. Build it with the compiler named in
the Compiler section of AGENTS.md.

The production deck at one condition, with the program in production and
with the current code:

```sh
python3 so-rp_swan/matrix/matrix_runner.py run --condition u20_d310_lp300_open \
  --target rws_4131_production --target current_4151_production --threads 8
```

`convergence_check.py` and `stopping_study.py` take `--template production`
as well.

One run in a new directory:

```sh
python3 so-rp_swan/matrix/matrix_runner.py run \
  --condition u20_d310_lp300_open --target current_4151 --threads 8
```

`--executable TARGET=/absolute/path/swan.exe` replaces the program of a
target, and `--repetitions N` makes separately recorded repetitions. Each
directory holds `INPUT`, a copy `so-rp_osk.swn`, the input files, the output in
`dat/`, the screen output, and `run.json` with hashes, condition, physics,
program, thread count, duration and exit code.

Tests: `python3 -m pytest -q so-rp_swan/matrix so-rp_swan/comparison`.

## Checking stationary convergence

For each condition, `convergence_check.py` runs the operational computation
and two references from the same initial state, which are forced to run N and
2N iterations (`NPNTS=101`, otherwise the same numerics as the deck). How much
Hs still shifts between those two counts, per wet output point, as the
uncertainty of the long reference. The operational run stopped too early if,
at some point, it differs by more than 10 mm (`--tolerance`) even after
subtracting that uncertainty, and it is fine if it stays within 10 mm
everywhere even including that uncertainty; in between there is no verdict. A
reference that drifts away from its equilibrium never gives a verdict; that is
only visible with `--residual`, which also measures the balance residual in
all runs. Period and direction are reported separately. `inconclusive` gives
exit code 1, as do `premature` and `not-converged`.

```sh
OMP_NUM_THREADS=4 OMP_PROC_BIND=close OMP_PLACES=cores OMP_WAIT_POLICY=PASSIVE \
  taskset -c 0,2,4,6 python3 so-rp_swan/matrix/convergence_check.py \
  --executable build-so-rp-current/bin/swan.exe --residual \
  --reference-mxitst 200 --conditions u20_d310_lp300_open \
  --output-directory .upstream/convergence-check \
  --json .upstream/convergence-check/result.json
```

With 200 and 400 iterations one condition takes well over an hour and a half;
start with a single condition. `--reference-alfa` tests the effect of
under-relaxation separately.

`stopping_study.py` puts the same references next to the existing input, the
default `STOPC` and stricter variants, all ordinary upstream input. A
candidate is not a new production default. Large run directories belong under
`.upstream/`.

A note on low wind: the deck computes up to 1 Hz. At 2 m/s the peak lies at
the highest frequency at most output points (only on the seaward side of the
barrier do the boundary waves set it), while the manual asks for an upper
limit of at least 2.5 to 3 times the peak frequency; such a condition then
mainly says something about the frequency grid.
