# AGENTS.md

Shared project instructions for AI coding agents (Codex, Claude Code and others) in this SWAN repository.

## Language

Write clear, plain English in documentation, code comments and commit messages,
as upstream TU Delft does. Use short sentences and ordinary words. Explain a
finding first in plain terms (what is going on, how you know, what it means)
and only then in terms of the code. Avoid stacked jargon, a marketing tone, and
lists of routine names or counts without explanation. Commit messages are full
prose: what changed, why, and how it was checked. Proper names stay as they
are: SWAN commands, routine names, file names, `ASLR`, `git bisect`. Replies to
the user stay in Dutch.

## Repository layout

- `origin` = `wph-rws/swan` (GitHub), `upstream` = TU Delft (`gitlab.tudelft.nl/citg/wavemodels/swan`).
- Current version: SWAN **41.51** (see `SWAN_VERSION_NUMBER` in `src/swanmain.f90`). The upstream remote in this repository only goes back to **41.41**; 41.31 is not in it.
- The source has been modernised from fixed-form (`.ftn`/`.f`) to free-form Fortran (`.f90`).
- Available locally only: `so-rp_swan/` with the operational so-rp/Voordelta case (grid, bottom, the reference binary **RWS 41.31A.1**). In git are the operational deck, the corrected template and the condition matrix in `so-rp_swan/matrix/`.

## Important physics fact: changed defaults (41.31 → 41.51)

**Symptom:** a deck that does not choose its source terms itself gives a
different Hsig in 41.51 than in RWS 41.31, with identical input.

**Cause:** in **release 41.45** TU Delft changed a number of defaults (SWAN
release notes; the only trace in the upstream code is the version tag `42.00`
at `INKEYW ('STA','WESTH')` in `src/swanpre1.ftn`, upstream commit `2980d1a`;
in the fork that line has no tag, in [src/swanpre1.f90](src/swanpre1.f90)):

1. Without a `GEN` command: **Van der Westhuysen et al. (2007)** instead of Komen et al. (1984): wind IWIND 3→5 (Yan), whitecapping IWCAP 1→7 (Alves-Banner, saturation-based).
2. Wind drag: **Wu (1982)** instead of the second-order polynomial fit (Zijlema et al., 2012), also when the deck chooses `GEN3 KOMEN`.
3. Triads: after a bare `TRIAD` command **DCTA** instead of LTA (ITRIAD 11→5); for every triad formulation the defaults `[urcrit]` 0.2 → 0.63 and `[urslim]` 0.01 → 0.1, unless the deck sets them; and the LTA of `ITRIAD=1` itself works differently. Since 41.45 it integrates the spectrum over a window of 60° around each direction instead of over all directions (`[dint]`); since 41.51 it also uses the coupling coefficient of Akrish et al. (2024) instead of that of Madsen and Sørensen (1993) (`IC`), adds an interaction with the third harmonic, and applies the group velocity once to the net transfer instead of to each term.

**Approaching 41.31 in 41.51:** choose the source terms explicitly and set what
the deck leaves open: `GEN3 KOMEN DRAG FIT`, and on the `TRIAD` line
`URCRIT=0.2 URSLIM=0.01` (with a bare `TRIAD` also `ITRIAD=11`; with
`ITRIAD=1` also `DINT=-1` and, after `URSLIM`, `IC=2`). `GEN3 KOMEN` alone is
not enough: the wind drag still changes. The third harmonic and the
group-velocity weighting of the `ITRIAD=1` LTA cannot be switched off from a
deck, so such a deck never matches 41.31 exactly; on the so-rp deck about 2 cm
remains (below).

**The operational so-rp deck** (template from the Rijkswaterstaat production environment in
`so-rp_swan/par/operational/`, provenance in `PROVENANCE.md` there) chooses
`GEN3 KOMEN`, `WCAP KOMEN DELTA=1`, `TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5`,
`STOPC` with `MXITST=40` and `CDCAP=0.00275` itself. Of the list above, the
wind drag and the triad changes remain. Measured on 28 October 2026 with the
corrected deck (see below), 20 m/s from 310°, NAP +3.00 m, barrier open, mean
Hsig over the 130 wet output points; every run stopped on its own stopping
criterion after 35–37 iterations:

| Run | mean Hsig |
|---|---|
| RWS 41.31A.1 | 1.602 m |
| 41.51 | 1.653 m (+3.2%) |
| 41.51 with `GEN3 KOMEN DRAG FIT` (without URCRIT/URSLIM) | 1.603 m (+0.0%) |
| 41.51 `legacy-4131`: `DRAG FIT`, `DINT=-1`, `URCRIT=0.2`, `URSLIM=0.01`, `IC=2` | 1.583 m (−1.2%) |

With `DRAG FIT` alone the mean agrees only because the triad changes happen to
cancel on average: point by point 41.51 still differs from 41.31 by up to 9 cm
(RMS 3.5 cm). The `legacy-4131` variant of `so-rp_swan/matrix/deck.py` sets
everything a deck can set. What remains, 2 cm on average and up to 13 cm at
single points (RMS 4 cm), is the third harmonic and the group-velocity
weighting of the `ITRIAD=1` LTA. A test build with those two parts written as
in 41.31 reproduces RWS 41.31A.1 to within 0.1 mm, so on this deck the whole
difference between the versions lies in the wind drag and the triads. Step by
step from `legacy-4131` without `DINT` and `IC` (1.542 m): integrating over all
directions lowers the mean to 1.515 m, the Madsen and Sørensen coefficient
raises it to 1.583 m, and the last two parts bring it to 1.602 m.

**Grid and boundary of the operational deck are wrong.** `CGRID CURVILINEAR
249 578` asks for one row and column more than `so-rp.rgf` has (249 × 578
points). SWAN reports this, but carries on because `MAXERR = 2`, and then 98%
of the wet grid points lie on average 49 m, and up to 248 m, away from their
position. The deck's boundary segments lie on that incorrectly read grid,
40–140 m inside the wet sea boundary of the correct one. The template this
repository uses, `so-rp_swan/par/so-rp_osk_org.swn`, is the operational deck
with exactly two corrections: `CGRID CURVILINEAR 248 577` and the boundary on
the wet sea boundary, `BOUND SEGMENT IJ 218 65 29 65` and `28 66 28 71`. SWAN
counts these grid indices from 0, and a segment must lie exactly on the edge:
one cell further inside, SWAN accepts it without a message but overwrites the
boundary spectrum in the first sweep, so that no waves enter at all
(`so-rp_swan/matrix/test_deck.py` checks the position against the bottom). At
the reference condition the two corrections change mean Hsig by less than 1 cm
(RWS 41.31: 1.608 m in production, 1.602 m corrected), but single points by up
to 9 cm. This has been put to the maintainer of the operational deck. Compare
versions with the corrected template. Because production still runs the deck
as it is, the matrix also has two targets with the production deck itself
(`rws_4131_production` and `current_4151_production`, only run when named
with `--target`); `deck.py` accepts that deck only byte for byte. With it, the
current code gives 1.659 m at the reference condition, against 1.608 m for
RWS 41.31A.1 (measured 29 October 2026): the same +5 cm as on the corrected
deck, but up to 13 cm at single points. The operational deck also only
runs with the RWS/DZL `swaninit`, which sets `#` as the comment character.

> Always average over the wet points: dry points have Hsig = −9 (SWAN
> exception value `EXCV`), and a domain average that includes the −9 gives
> artificially low values.

## Compiler

The compiler is **gfortran 16.2**, for everything: serial, OpenMP, MPI,
netCDF, the strict gate, the upstream build of the delta gate and CI. The
stored references and the strict-gate verdicts were made with it. Always choose
it explicitly: `-DCMAKE_Fortran_COMPILER=/usr/bin/gfortran-16` for CMake,
`FC=/usr/bin/gfortran-16` for `scripts/strict_diagnostics.py`, and
`OMPI_FC=gfortran-16` for MPI builds. The plain `gfortran` command may point to
another version, and another compiler version can shift output and warnings.
A build directory that continues incrementally after a compiler change mixes
versions; when in doubt, check `readelf -p .comment` on the binary. The
`mpi.mod` and `netcdf.mod` of the system packages can be read by gfortran 16.2.
Locally, OpenMPI needs
`OMPI_MCA_pml=ob1 OMPI_MCA_btl=self,sm OMPI_MCA_mtl=^ofi OMPI_MCA_osc=^ucx` to
avoid crashing at start-up.

## Analysis tools

These tools are installed locally (FORD, gcovr and fortitude in `~/.venv`, the
rest through apt). Use them before reading large parts of the source by hand.
Their output goes to `build-*` directories, which git ignores.

- **Who calls what: FORD.** `ford doc/ford.md` (about 30 s) writes a page per
  routine to `build-ford/`, with its arguments, the modules it uses, and
  graphs of what it calls and what calls it, for example
  `build-ford/proc/swcomp.html`. Several modules in `src/` exist in two
  versions of which only one is built; the project file follows the MPI build
  with netCDF and says which versions it takes. Look up the callers before
  changing the arguments or the side effects of a routine.
- **Where something is defined: ctags.**
  `ctags -R --languages=Fortran --fields=+nKS --extras=+q -f build-ford/tags src/`
  takes less than a second. `grep -P '^SWCOMP\t' build-ford/tags` then gives
  the file, line, module and argument list. Plain `rg` also finds every use.
- **Which code a deck runs: gcovr.** Build with coverage in a separate
  directory (`-DCMAKE_Fortran_FLAGS="--coverage -O0"
  -DCMAKE_EXE_LINKER_FLAGS=--coverage`, here `build-coverage/`), run the
  deck, then `gcovr --root . --filter src/ --gcov-executable gcov-16
  --object-directory build-coverage`. Use it to check that a test really
  reaches the code it claims to test.
- **Error-prone constructs: fortitude.** `fortitude check src/` uses
  `fortitude.toml`, which selects only rules that can point at a real error
  (about 140 findings, mostly known and harmless). The style rules are left
  out on purpose: the code follows upstream layout.
- **A second compiler: LLVM flang 22.** It is a check, not a supported route
  (see [doc/support-matrix.md](doc/support-matrix.md)); references and gates
  stay on gfortran. Build with
  `-DCMAKE_Fortran_COMPILER=/usr/bin/flang-22` in `build-flang/`. On
  29 October 2026 the whole source compiled without a Fortran warning, and
  `-fsyntax-only -pedantic` found only unused variables and four harmless
  TARGET arguments. Of the 60 CTest tests, 58 passed. `source_terms_st6` differs
  only in rounding (Hsig within 0.3 mm), and `BF-49-directory-as-file`
  fails because the flang runtime opens a directory without an error, where
  gfortran refuses.
- **Timing: hyperfine.** `hyperfine -w 1 -r 5 -N --prepare 'cp deck.swn INPUT'
  swan.exe` gives a mean and spread over repeated runs. The rules under
  "Performance measurements" still apply. Callgrind (`valgrind --tool=callgrind`)
  counts instructions and is reproducible, which a wall-clock time is not.

What does not work here: WSL2 gives no hardware performance counters, so
`perf` only has software events (`task-clock`, `cpu-clock`) and cache misses
or IPC cannot be measured; `rr`, likwid and most of VTune need those counters
as well. The processor has performance and efficiency cores, so pin threads
for multicore timings.

## Verification: targeted gate selection

Not every change needs the full breadth (serial + OpenMP + MPI + strict). For each change, use `python3 scripts/select_gates.py [--base HEAD]` to find which gates the change can break, and run only those, with the commands it prints:

- MPI only for paths that touch MPI (source with mpi/parall/metis/coh/esmf/adcirc in the name, MPI tests, toolchain/CMake). A deck or physics change without a code difference behaves under MPI exactly as in serial on the same code.
- strict only for source or toolchain changes, or a change to its verdicts. Docs or comments only: no gates.
- upstream_delta for anything that can shift the output of a deck (source, toolchain, decks, the inventory itself). That gate runs every deck (the four heavy ones only with `--include-skipped`) through its own build of the pinned TU Delft source (`43e9bbb`) and through the fork, and checks the result against `doc/upstream-delta.json`: every difference must have a BF number. The inventory itself is checked on every commit in the pytest gate; the measured half runs under `SWAN_UPSTREAM_DELTA=1` and needs a one-off upstream build in `.upstream/build_O3_gfortran<version>`, with the same compiler as the fork build and at `-O3`, because both the optimisation level and the compiler version change output (use a path without a hyphen, because upstream `switch.pl` loops forever on one).
- Unknown paths safely choose everything; never silently nothing.
- Skipped gates stay at their last green commit. Mention that commit with the change, so the release claim stays traceable.

## Performance measurements: always multicore as well

In practice SWAN almost always runs on several cores. A speed claim on one core
therefore says little about the gain a user sees, and can even predict it
wrongly: a change that wins serially can lose on several cores once it skews
the load balance, adds a synchronisation point, or uses up memory bandwidth
that was still plentiful with one thread.

So measure every performance change on **at least one serial and one multicore
configuration**, and report both. The serial measurement isolates the
computation; the multicore measurement shows what the user keeps. Give the
thread count, the pinning and the cores used; without them a time cannot be
reproduced.

Two things go with this:

- **Check byte-identical output across thread counts.** The output should not
  depend on the number of threads. Measure that explicitly at 1, 2, 4 and more
  threads; a speed-up that breaks determinism is not a gain.
- **Measure on a quiet machine.** If anything else runs on the same node (a
  second agent, a build, another run), the time is useless. Check this
  beforehand, and say so if it was not possible.

This also applies to an option that is off by default: the disabled path must
be shown to cost nothing, and on several cores that is not a given. One
unconditional atomic counter in the grid-point loop costs little serially and
creates a point of contention on several cores.

## Convergence: "converged" is not the same as "stopped changing"

SWAN's stationary stopping criteria judge how the iterations progress: `ACCUR`
measures changes in Hs and Tm; the default criterion `STOPC` also takes the
curvature into account. Both exist upstream. Under-relaxation (the `[alfa]` of
false time stepping) makes that change smaller, and that is exactly what it is
for. The test cannot tell those two causes apart. The extra curvature check of
`STOPC` does not prove that the balance closes either; the criterion is not
applied on the first iteration.

This is not an edge case here: most decks in this repository set `ALFA`.
Measured on `examples/nonlinear_interactions/quad/quad_dia1`: with `ALFA=0.10` the run stops after two iterations and with
`ALFA=0.05` after ten, with the message "accuracy OK in 100.00 %", while Hs at
25 km is then 1.278 m. `ALFA=0.01` also stops too early: after 40 iterations
it reports 100% at 1.403 m, while the same run carried on until the balance
residual reaches rounding level (about 1500 iterations) ends at 1.923 m, 27%
higher.

The fork has an optional balance residual
(`balance residual energy rms ...` in PRINT; see
[doc/balance-residual-design.md](doc/balance-residual-design.md)). The
computation is off by default. `NUMERIC ... RESIDUAL 0` only switches the
measurement on; a positive threshold also requires a small energy residual
before stopping. This is an extension of the fork, not upstream input. A small
mean residual does not guarantee convergence of all points or of the whole
spectrum. Operational decks get no general residual threshold.

Whether the operational so-rp deck (`STOPC` with `CURVAT=0.001`, `NPNTS=98`,
`MXITST=40`, `ALFA=0.01`) stops too early has not been measured yet. At 2 m/s
wind the deck does not reach its criterion within 40 iterations (87–88%), and
wherever the boundary waves do not arrive, which is at most output points, the
peak lies at the highest frequency: the upper limit of 1 Hz is too low there.
The manual (`swanuse.pdf` p. 23) asks for an
upper limit of at least 2.5 to 3 times the highest peak frequency.

The residual does not replace the comparison everywhere: its floor depends on
the deck, so only a second run tells how much Hs still shifts. Note that the
reference of the convergence check must then itself keep iterating (an
unreachable `NPNTS` or a `RESIDUAL` threshold), or it stops just as early.
`so-rp_swan/matrix/convergence_check.py` checks this per condition with two
references from the same initial state: forced N and 2N iterations through
`NPNTS=101`, by default with `ALFA=0.01`. How much Hs still shifts between
those references counts per point as uncertainty; a verdict follows only if
the deviation of the operational run lies clearly above or below the 10 mm
limit even with that uncertainty (a choice of this fork, not an upstream
value). The check covers the wet output points, not the whole field or
spectrum; periods and direction are reported separately. Run it after changes
to numerics, source terms or deck settings. `inconclusive`, like `premature`
and `not-converged`, gives a non-zero exit code.
