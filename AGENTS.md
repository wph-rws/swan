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

## Upstream code in the fork

A procedure with an upstream name stays, where it can, in the fork file with
the name of its upstream file (`SWCOMP` from `swancom1.ftn` in
`swancom1.f90`). Move upstream code to another file only to break a module
cycle or for a clear reason of content, such as the wave-induced set-up in
`swan_wave_setup.f90`; do not split a file for its size alone.
[doc/upstream-map.json](doc/upstream-map.json) records every upstream
procedure and shared variable that is not where its name says, with the
reason, and `scripts/upstream_map.py check` (pytest gate) fails on a move
without an entry. `scripts/upstream_map.py where NAME` finds upstream code in
the fork; for a new upstream release follow
[doc/upstream-import.md](doc/upstream-import.md).

Every Fortran source has one layout, set by `scripts/fortran_layout.py`:
three spaces per level, `&` at the end of a continued line, no code line
longer than 132 characters, TU Delft headers after the procedure statement
with the declarations in sections 4 to 6. The script also converted the
upstream fixed-form files, and it refuses to write a file whose statements
would change. Run `python3 scripts/fortran_layout.py format <file>` after an
edit; `tests/test_fortran_layout.py` (pytest gate) fails on a file that is
not in the layout. Because the conversion and the layout came in their own
commits, a line can be traced to upstream: `git blame -w -C -C
--ignore-revs-file .git-blame-ignore-revs <file>` skips them (set
`git config blame.ignoreRevsFile .git-blame-ignore-revs` once to make it the
default). How the history is built, and how to find the commit of a fix, is
in [doc/history.md](doc/history.md).

## Traceability

- Every bug fix has one commit, which holds the fix, its test and its row in
  the bug-fix table, and ends with the trailer `Bug-fix: BF-NN`
  (`git log --grep='Bug-fix: BF-62'` finds it). The table names the commit.
  A later correction of a fix that is already pushed gets its own commit with
  the same trailer. Sixteen older fixes have their test elsewhere or none;
  [doc/history.md](doc/history.md) lists them and says why.
- A measurement in this file, in the bug-fix table or in a commit message
  names the commit or tag it was made on, not only the date; a number
  without one cannot be repeated after the code moves on. Measurements dated
  before 10 November 2026 were made on the earlier history (tag
  `archive/before-rebuild-2026-11-10`); the commit named with them is the
  commit of the rebuilt history that holds the same change, as explained in
  doc/history.md, and the number was not measured again there.

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
deck, so such a deck never matches 41.31 exactly; on the so-rp deck about 3 cm
remains (below).

**The operational so-rp deck** (template from the Rijkswaterstaat production environment in
`so-rp_swan/par/operational/`, provenance in `PROVENANCE.md` there) chooses
`GEN3 KOMEN`, `WCAP KOMEN DELTA=1`, `TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5`,
`STOPC` with `MXITST=40` and `CDCAP=0.00275` itself. Of the list above, the
wind drag and the triad changes remain. Measured on 10 November 2026 with
the corrected deck, the fork at commit `a49f403` and upstream `43e9bbb`, both
built with gfortran 16.2 at `-O3` with OpenMP, 20 m/s from 310°, NAP +3.00 m,
barrier open; mean Hsig over the 130 output points that are wet in every run
(RWS 41.31 and upstream do not find five of the 135; see BF-56). Every run
stopped on its own stopping criterion after 36–38 iterations:

| Run | mean Hsig | against 41.31 per point |
|---|---|---|
| RWS 41.31A.1 | 1.602 m | |
| upstream 41.51 | 1.653 m (+3.2%) | up to 12.8 cm, rms 5.7 cm |
| fork | 1.651 m (+3.0%) | up to 12.5 cm, rms 5.6 cm |
| fork with `GEN3 KOMEN DRAG FIT` (without URCRIT/URSLIM) | 1.600 m (−0.1%) | up to 8.4 cm, rms 3.6 cm |
| fork `legacy-4131`: `DRAG FIT`, `DINT=-1`, `URCRIT=0.2`, `URSLIM=0.01`, `IC=2` | 1.575 m (−1.7%) | up to 9.7 cm, rms 4.2 cm |

The fork differs from upstream on this deck only through BF-63: it makes the
third-harmonic term of the `ITRIAD=1` LTA energy-conserving by default, and
with `TRIAD ... UPSTREAM` it gives exactly the values of upstream at all 130
points. BF-63 lowers mean Hsig by 0.2 cm, at single points by up to 2.5 cm
(rms 0.4 cm).

With `DRAG FIT` alone the mean agrees only because the triad changes happen to
cancel on average; point by point the fork still differs from 41.31 by up to
8 cm. The `legacy-4131` variant of `so-rp_swan/matrix/deck.py` sets
everything a deck can set. What remains is the third harmonic and the
group-velocity weighting of the `ITRIAD=1` LTA, which a deck cannot switch
off. On 28 October 2026, on the earlier history and before BF-63, a test
build with those two parts written as in 41.31 reproduced RWS 41.31A.1 to
within 0.1 mm, so on this deck the whole difference between the versions
lies in the wind drag and the triads. Step by step from `legacy-4131`
without `DINT` and `IC` (then 1.542 m): integrating over all directions
lowered the mean to 1.515 m, the Madsen and Sørensen coefficient raised it
to 1.583 m, and the last two parts brought it to 1.602 m.

**Grid and boundary of the operational deck are wrong.** `CGRID CURVILINEAR
249 578` asks for one row and column more than `so-rp.rgf` has (249 × 578
points). RWS 41.31 and upstream report this at level 2 only and carry on
because `MAXERR = 2`; since BF-67 the fork treats it as a severe error
(level 3), so that the run only computes with `MAXERR = 3`. In 41.31 98%
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
(RWS 41.31: 1.608 m in production, 1.602 m corrected, over the same 130
points), but single points by up to 9 cm. This has been put to the maintainer of the operational deck. Compare
versions with the corrected template. Because production still runs the deck
as it is, the matrix also has two targets with the production deck itself
(`rws_4131_production` and `current_4151_production`, only run when named
with `--target`); `deck.py` accepts that deck only byte for byte, and changes
only `MAXERR = 2` into `MAXERR = 3`, so that the fork computes on the
incomplete grid as production does (for RWS 41.31 that changes nothing). With
it, the fork (commit `a49f403`) gives 1.656 m at the reference condition over
the same 130 points, against 1.608 m for RWS 41.31A.1 (measured 10 November
2026): the same +5 cm as on the corrected deck, up to 13 cm at single
points. On the production deck RWS 41.31 also finds the five other points
(1.619 m against 1.666 m over all 135). The deck "works" because the grid is
fine and smooth, so points shifted by some 50 m still lie in a plausible
place; the same shortage in another file can leave points at (0,0). The operational deck also only
runs with the RWS/DZL `swaninit`, which sets `#` as the comment character.

The corrected point search of the fork (BF-56) also locates five points that
upstream and RWS 41.31 miss on the corrected deck (135 output points in
total). When comparing versions, use their common wet points and report that
count; do not compare a mean over 130 points with a mean over 135.

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
  out on purpose: the layout is set by `scripts/fortran_layout.py`, which
  keeps the upstream text recognisable line by line.
- **A second compiler: LLVM flang 22.** It is a check, not a supported route
  (see [doc/support-matrix.md](doc/support-matrix.md)); references and gates
  stay on gfortran. Build with
  `-DCMAKE_Fortran_COMPILER=/usr/bin/flang-22` in `build-flang/`. On
  10 November 2026 (commit `a49f403`, flang 22.1.8) the whole source compiled
  without a Fortran warning, and `-fsyntax-only -pedantic` found only two
  unused variables (`IJ` and `IK`, which only an OpenMP clause uses) and six
  TARGET arguments, none of which keeps a pointer after the call. Of the 66
  CTest tests, 63 passed. `setup` and `source_terms_st6` differ only in
  rounding (34 of 1722 set-up values by one in the last printed digit; Hsig of
  ST6 within 0.1 mm), and in `input_validation` the case
  `BF-49-directory-as-file` fails because the flang runtime opens a directory
  without an error, where gfortran refuses.
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

**While working, test what the change touches, and test it lightly.** The
most useful check is usually a small new test aimed at the changed behaviour:
the set-up error of November 2026 passed every existing gate, because no
deck used `SETUP`, and one small deck found it. Repeating every gate after
each step finds little and costs an hour. A rough guide, with the existing
build directories:

| Kind of change | Check while working | Time |
|---|---|---:|
| Docs, or comments only | read the diff | < 1 min |
| Code that should not change output | incremental build, the tests that reach the code, an exact comparison of a few decks that run it; OpenMP or MPI only if it touches shared state, threads or reductions | 1–3 min |
| Code that should change output | a small test that fails before and passes after, and a check that unrelated decks stay the same | 2–5 min |
| Numerics or physics | the above, plus a small convergence, conservation or theory test on a model that runs in seconds | 5–15 min |

The full breadth below runs **once per change set, before it is merged**
(in CI, or locally if CI is not available), not after every step. Note that
CTest compares with a relative tolerance of `1e-3`, so a green CTest does not
show byte-identical output; compare the files when that is the claim.

Runs on the operational so-rp grid take about 17 s per iteration on four
cores. Use them only for questions about that deck itself, never as a check
of a code change. Before starting one, work out the run time and say it, and
give each process its own cores (`taskset`); `OMP_PROC_BIND` alone puts the
threads of every process on the same first cores.

Before merging, not every change needs the full breadth (serial + OpenMP + MPI + strict). For each change set, use `python3 scripts/select_gates.py --base <commit the branch started from>` to find which gates the change can break, and run only those, with the commands it prints. Those commands expect the build directories `build`, `build-modernization-openmp` and `build-modernization-mpi`, configured with gfortran-16 (Release; the second with `-DOPENMP=ON`, the third with `-DMPI=ON`). The default base `HEAD` only sees uncommitted work, so on a clean branch it selects nothing:

- MPI only for paths that touch MPI: source with mpi/parall/metis/coh/esmf/adcirc in the name, source that calls the parallel layer (a reduction, gather, broadcast or exchange, or the subdomain edge flags), MPI tests, toolchain/CMake. A deck or physics change without a code difference behaves under MPI exactly as in serial on the same code.
- strict only for source or toolchain changes, or a change to its verdicts. Docs or comments only: no gates.
- upstream_delta for anything that can shift the output of a deck (source, toolchain, decks, the inventory itself). That gate runs every deck (the four heavy ones only with `--include-skipped`) through its own build of the pinned TU Delft source (`43e9bbb`) and through the fork, and checks the result against `doc/upstream-delta.json`: every difference must have a BF number. For a deck that already differs, the inventory also keeps a fingerprint (SHA-256) of each fork output file, so that a new change in such a deck fails the gate as well, and a fork that crashes or writes nothing is never an accepted difference. The inventory itself is checked on every commit in the pytest gate; the measured half runs under `SWAN_UPSTREAM_DELTA=1` and needs a one-off upstream build in `.upstream/build_O3_gfortran<version>`, with the same compiler as the fork build and at `-O3`, because both the optimisation level and the compiler version change output (use a path without a hyphen, because upstream `switch.pl` loops forever on one).
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
Measured on `examples/nonlinear_interactions/quad/quad_dia1` with
`scripts/stop_check.py` (10 November 2026, commit `a49f403`), with the
upstream semi-implicit DIA (`QUADRUPLET 1 UPSTREAM`): with `ALFA=0.10` the run
stops after two iterations and with `ALFA=0.05` after ten, with the message
"accuracy OK in 100.00 %", while Hs at 25 km is then 1.278 m. `ALFA=0.01`
also stops too early: after 40 iterations it reports 100% at 1.403 m, while
the same run carried on for 1500 iterations ends at 1.923 m, 27% higher (and
after 3000 at the same value). With the fork's DIA (BF-62, the default) the
deck stops after 40 iterations at 1.410 m with `ALFA=0.01`, while its
balance lies at 1.933 m (residual 1e-8 when it is made to run 100 iterations
without `ALFA`; its own criterion stops it there after 28 iterations at
1.919 m): the false stop comes from the under-relaxation, not from the DIA.

The fork has an optional balance residual
(`balance residual energy rms ...` in PRINT; see
[doc/balance-residual-design.md](doc/balance-residual-design.md)). The
computation is off by default. `NUMERIC ... RESIDUAL 0` only switches the
measurement on; a positive threshold also requires a small energy residual
before stopping. This is an extension of the fork, not upstream input. A small
mean residual does not guarantee convergence of all points or of the whole
spectrum. Operational decks get no general residual threshold.

The operational so-rp deck (`STOPC` with `CURVAT=0.001`, `NPNTS=98`,
`MXITST=40`, `ALFA=0.01`) stops too early. Measured on 7 November 2026 (commit
`591c4ce`) with `convergence_check.py` on the corrected deck, references of 80 and 160 forced
iterations: at low water (NAP −2 m, 20 m/s) and at 40 m/s the deck stops after
36 and 25 iterations, while the reference hardly changes between 80 and 160;
there the deck is 2.3 and 5 cm low on mean Hs, and up to 8 and 24 cm at single
points. At the reference condition the mean is within 0.5 cm, but 39 of the
130 points are more than 1 cm off (up to 17 cm), and a cluster of points 12–26
m deep (x ≈ 36,500–37,000, y ≈ 404,000–404,600) keeps drifting after 160
iterations while the energy residual stays flat from iteration 80 on; a flat
residual does not tell whether that drift settles later. At 2 m/s
wind the deck does not reach its criterion within 40 iterations (87–88%), and
wherever the boundary waves do not arrive, which is at most output points, the
peak lies at the highest frequency: the upper limit of 1 Hz is too low there.
The manual (`swanuse.pdf` p. 23) asks for an
upper limit of at least 2.5 to 3 times the highest peak frequency.

The fork option `NUMERIC ... EXTRAP [nstep]` attacks the false stop itself
(see `src/swan_stop_extrapolation.f90`). Every [nstep] iterations it stores Hs
per point; from three checkpoints it takes the ratio r of the last two block
changes and estimates the change still to come as |D| r/(1 − r). A steady or
growing change (r ≥ 1), or a swing that does not get smaller, counts as going
on for 100 more blocks, so a drift below 1/100 of the tolerance per block
still passes. The iteration
stops only when that estimate is within the deck's own [dabs]/[drel] in
[npnts] percent of the wet points as well (under `NUMERIC ACCUR`, which has
no [dabs], the default [dabs] of `STOPC`). One iteration is too short for the
ratio: near convergence it jumps between −1 and 5 from noise. Measured on
10 November 2026 (commit `a49f403`) with `scripts/stop_check.py` and the
stopping numerics of so-rp (`STOPC DREL=0.01 CURVAT=0.001 NPNTS=98
ALFA=0.01`, with the default [dabs]), against references of 600 iterations
on the test models of `examples/testmodels`: on `sphys` (so-rp physics)
STOPC stops after 18 iterations up to 18 cm off and `EXTRAP 10` after 100
iterations within 1 mm; on `sq` after 7 (16 cm) against 50 (1 mm); on
`quad_dia1`, with the fork's DIA, after 40 (0.52 m) against 340 (4 mm).
With `DABS=0.00`, as in the so-rp deck, `quad_dia1` does not stop within 600
iterations: on its 41 wet points 98% means all of them, and one keeps an
estimate above 1% of its small Hs. Where the iteration never settles (`isl`,
the RESCALE floor behind an island) it runs to `MXITST` and says how many
points still move (50 with a steady or growing change), instead of
reporting convergence. The estimate is not a bound: an iteration
that slows down by itself, or a slow mode under a fast one, can still stop it
too early. Output is the
same at 1, 2, 4 and 8 threads and under MPI. On so-rp itself (`EXTRAP 10`,
`MXITST=200`, three conditions) the test is not met within 200 iterations:
93–96% of the wet points pass, against 98% required, and 37–59 minutes on 4
cores instead of about 7. Against the 160-iteration reference the result then
differs by at most 3–6 cm (rms 0.5 cm), against 8–24 cm for the operational
stop. The so-rp runs were made before the change of 8 November. Since none of
them met the test, the price of an honest stop on this deck is not measured;
it is at least five to eight times the run time. Making the iteration itself
converge faster is the real remedy, not a stricter stop.

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
spectrum; periods and direction are reported separately. Run it when a
change to numerics or source terms is meant to change how this deck
converges, or when the deck's settings change; check any other code change on
small models first (see Verification). `inconclusive`, like `premature` and
`not-converged`, gives a non-zero exit code.
