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
netCDF. Always choose it explicitly:
`-DCMAKE_Fortran_COMPILER=/usr/bin/gfortran-16` for CMake and
`OMPI_FC=gfortran-16` for MPI builds. The plain `gfortran` command may point
to another version, and another compiler version can shift output. A build
directory that continues incrementally after a compiler change mixes versions;
when in doubt, check `readelf -p .comment` on the binary. The `mpi.mod` and
`netcdf.mod` of the system packages can be read by gfortran 16.2. Locally,
OpenMPI needs `OMPI_MCA_pml=ob1 OMPI_MCA_btl=self,sm OMPI_MCA_mtl=^ofi
OMPI_MCA_osc=^ucx` to avoid crashing at start-up.

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
