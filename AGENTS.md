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
deck, so such a deck never matches 41.31 exactly.

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
Measured on `examples/nonlinear_interactions/quad/quad_dia1`: with `ALFA=0.10`
the run stops after two iterations and with `ALFA=0.05` after ten, with the
message "accuracy OK in 100.00 %", while Hs at 25 km is then 1.278 m.
`ALFA=0.01` also stops too early: after 40 iterations it reports 100% at 1.403
m, while the same run carried on for about 1500 iterations ends at 1.923 m,
27% higher.

