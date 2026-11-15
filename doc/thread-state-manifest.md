# Thread-state manifest

This document lists what each OpenMP thread keeps for itself in the fork, how
that differs from upstream SWAN 41.51, and where the whitecapping state is
held.

The fork has no `THREADPRIVATE` module variable and no `COPYIN` clause.
Everything a thread keeps for itself is a variable of the solver that the
thread is running (`SWCOMP` with `SWOMPU`, or `SwanCompUnstruc`), an element
of an array with one element per thread, or an argument.
`scripts/check_thread_state.py` fails if a `THREADPRIVATE` or `COPYIN`
directive appears anywhere under `src/`, and if a name in the table below is
no longer private to the threads of a solver.

## Why no thread-private module variables

A `THREADPRIVATE` module variable is one that every routine can read without
it showing in a call, and of which every thread has its own copy. The
compiler has to reach it through thread-local storage each time. The
gfortran 16.2 of MSYS2 (ucrt64) on Windows does not always do that at `-O1`
and higher: in a few places it uses an ordinary global address, so that all
threads read or write one copy. Upstream's OpenMP build (`43e9bbb`) computes
Hsig 0 everywhere on Windows for that reason, and the same wrong address was
seen in the fork's object code for `RDFSIN` in `SWIND_DBYB` (both found on
14 November 2026, the fork at commit `8e293ac`, with GNU Fortran 16.2.0 of
MSYS2 ucrt64). A variable of a routine,
an array element and an argument do not depend on thread-local storage, so
the fork uses only those. It also makes the state visible: a routine that
needs to know whether its grid point is a test point now says so in its
argument list.

## Method

The analysis is **import-aware**: a symbol only counts in a file that actually
`USE`s the matching module. That is not a formality. A count based on names
alone attributes ten writers of `EKTOT` to
[swanout1.f90:4089](../src/swanout1.f90#L4089) onwards, but that is a local
`REAL` at [line 3194](../src/swanout1.f90#L3194) in a file that does not import
the whitecapping state: a name clash, not shared state.

Lifetime is the shortest unit over which the value must stay valid:
`point` (one grid point or vertex), `sweep`, `iteration`, `run`, `process`.

## What upstream keeps per thread, and where it is in the fork

Upstream 41.51 has eight `THREADPRIVATE` directives. None exists in the fork:

| Upstream | Symbols | In the fork |
|---|---|---|
| `SWCOMM3` | `IXCGRD, IYCGRD, KCGRD` | `st_ix, st_iy, st_kc` in `SWOMPU` and `SwanCompUnstruc`, passed as arguments |
| `SWCOMM3` | `COSLAT` | `st_co`: `SWGEOM` fills it, `ACTION` and `SwanTranspAc` pass it on |
| `SWCOMM3` | `ICMAX` | `st_nm` in the solvers, the argument `st_n` or `icmax` in the routines |
| `SWCOMM3` | `CSETUP` | not thread state: `SETUPP` returns it to `SETUP_CONVERGED` of `SWCOMP` |
| `SWCOMM3` | `RDFSIN` | a variable of the solver per thread, passed as the argument `RDFSIN`; `RDFSIN_MASTER` in `swan_stencil` between two computations |
| `SWCOMM4` | `IPTST, TESTFL` | variables of the solver per thread, passed as the arguments `TESTFL` and `IPTST`; `TESTFL_MASTER` in `swan_test_output` between two computations |
| `SWCOMM4` | `PROPSL` | a local variable of `SWOMPU`, passed to `ACTION` and `QCSOURCE` |
| `M_WCAP` | the eleven whitecapping scalars | `wcap_workspace_t`, one per solver thread (see below) |
| `SwanCompdata` | `vs` | `st_kc` in `SwanCompUnstruc` |
| `TIMCOMM` (switch `!TIMG`) | `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | `THREAD_TIMERS` in `swan_time`, one element per thread |

## Per-thread state in the solvers

Both solvers have one parallel region. The table says how each name is made
private to a thread there; the script checks the cells that read `PRIVATE` or
`FIRSTPRIVATE` against the directive in the source.

| Symbol | Lifetime | Structured (`swancom1.f90`) | Unstructured (`SwanCompUnstruc.f90`) |
|---|---|---|---|
| `TESTFL` | point, and the last point of the thread | `FIRSTPRIVATE` | `FIRSTPRIVATE` |
| `RDFSIN` | from point to point | `FIRSTPRIVATE` | `FIRSTPRIVATE` |
| `IPTST` | point | local variable of `SWOMPU` | `PRIVATE` |
| `PROPSL` | point | local variable of `SWOMPU` | not used |
| `st_ix, st_iy, st_kc, st_co` | point | local variables of `SWOMPU` | `PRIVATE` |
| `st_nm` | point | local variable of `SWOMPU` | `FIRSTPRIVATE` |
| `wcap_workspace_t` | run | `THREAD_WORKSPACES%STRUCTURED(thread)` | `thread_workspaces%unstructured(thread)` |

A local variable of `SWOMPU` is private because every thread calls `SWOMPU`
itself, once per grid point.

### `TESTFL` and `IPTST` — is this grid point a test point

The solver works out per grid point whether it is a test point (`TESTFL`) and
which one (`IPTST`): `SWOMPU` does so before it calls anything, and
`SwanCompUnstruc` at the top of its block per vertex. Every routine that
writes test output gets the two as arguments; a routine that only prints gets
`TESTFL` alone. `IPTST` is only read where `TESTFL` is true, so it has no
meaning outside the grid point.

`TESTFL` does have a life outside the grid point, and the fork keeps it
because upstream output depends on it:

- `SACCUR` runs once per iteration in every thread, after the sweeps, and
  prints its test output if `TESTFL` is true. That is the value of the last
  grid point the thread computed. `SWOMPU` therefore returns `TESTFL` to
  `SWCOMP`, which keeps it per thread and passes it to `SACCUR`.
- `AC2TST`, called by `SWOUTP`, prints the spectra of all test points if
  `ITEST` is at least 100 and `TESTFL` is true. Outside the solver that is
  the value the first thread ended with, or the value `SWINCO` left when it
  filled the last grid point. This value is `TESTFL_MASTER` in
  `swan_test_output`: every thread starts a computation with it
  (`FIRSTPRIVATE`), and the first thread writes its own value back at the end
  of the parallel region.

Both are questionable in upstream: whether these lines are printed depends on
which grid point a thread happened to compute last, and so on the number of
threads. The fork reproduces it and does not correct it.

`IPTST` is also the name of the loop counter over the test points in
`SNEXTI`, `SWINCO`, `BCWAMN`, `SWBCPT`, `RETSTP` and `PLTSRC`. In upstream
that is the same module variable; in the fork each of these routines has a
local counter.

### `RDFSIN` — reduction factor of the ST6 wind input

`SWIND_DBYB` computes the factor per frequency for a grid point (through
`CALC_LFACTOR`) and applies it there. `SURF_ROUGH_ECMWF`, which `WINDP1`
calls for the ECMWF drag formulation, reads it before `SOURCE` calls
`SWIND_DBYB` for the grid point, so it reads the factor that the thread
computed last, at another grid point. The value therefore lives from point
to point. Each thread
has its own array in the solver, which starts with `RDFSIN_MASTER` from
`swan_stencil` (`FIRSTPRIVATE`; `SWPREP` sets it to zero); the first thread
writes its array back at the end of the parallel region. The routines get
the array as the argument `RDFSIN`.

### `PROPSL` — propagation scheme of one grid point

`SWOMPU` starts every grid point with the chosen scheme `PROPSC` and falls
back to a lower order near a boundary, dry land or an obstacle. `ACTION` and
`SWQCWIG` (through `QCSOURCE`) read the result; nothing else does, and
nothing reads it before `SWOMPU` has set it. It is a local variable of
`SWOMPU`. The unstructured solver does not use it; it passes `PROPSC` to
`QCSOURCE`, which does not read it for an unstructured grid.

## Manifest of the whitecapping state

Categories: **4** proven write-before-read scratch, **5** conditionally
written or persisting.

### Whitecapping integrals (upstream `M_WCAP`) — integral parameters per grid point

In upstream, the eleven values are threadprivate module variables in
`M_WCAP`. They are written only in `SINTGRL`
([swancom1.f90:5390-5970](../src/swancom1.f90#L5390-L5970)); only five files
import the module, and no scalar is in COPYIN.

| Symbol | Default on entry | Write condition | Cat. | Invariant |
|---|---|---|---|---|
| `KM_WAM` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L177) | refined when `EDRKTOT > 0.` | 4 | proven |
| `KM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L178) | refined when `EKTOT > 0.` | 4 | proven |
| `SIGM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L179) | refined when `ETOT1 > 0.` | 4 | proven |
| `SIGM_10` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L180) | refined when `ACTOT > 0.` | 4 | proven |
| `ACTOT` | **none** | [`IF (ETOT > 0.)`](../src/swancom1.f90#L5736) | **5** | tested: value is kept |
| `ETOT1` | **none** | same | **5** | tested: value is kept |
| `ETOT2` | **none** | same | **5** | tested: value is kept |
| `ETOT4` | **none** | same | **5** | tested: value is kept |
| `EDRKTOT` | **none** | same | **5** | tested: value is kept |
| `EKTOT` | **none** | same | **5** | tested: value is kept |
| `SIGM_WAM` | **none** | `IF (EDRKTOT > 0.)` inside `IF (ETOT > 0.)` | **5** | tested: value is kept |

In the fork the owner of all eleven is
[`wcap_workspace_t`](../src/swan_source_workspaces.f90), included in
`source_workspace_t` and stored per thread in the structured or unstructured
workspace. `begin_point` sets only the four entry defaults to 10, as
upstream `SINTGRL` does; the seven category-5 values are deliberately left alone.

**The category-5 finding.** When `ETOT <= 0.`, `SINTGRL` does not write seven
of the eleven, while [line 5963](../src/swancom1.f90#L5963) executes
`AC2TOT = ACTOT` unconditionally. The thread then carries over the value from
the previous point it handled. Upstream behaves the same way.

**Effect on results.** For valid, non-negative spectra, initialising these
seven values does not change the numerical result of the solver:

- [`SWCAP`](../src/swancom2.f90#L2284) and
  [`SWCAP8`](../src/swancom2.f90#L2791) return when `ETOT <= 0.` before
  a workspace value can affect a source term;
- `SSURF` reads `SIGM_WAM` for `ISURF = 6`, but only uses the chosen
  frequency behind the [`BB > 0`](../src/swancom2.f90#L2203) condition;
  when `ETOT = 0.` the surf-breaking contribution stays zero;
- `AC2TOT` has no reader after the call in the structured and unstructured
  solver;
- as soon as `ETOT > 0.`, `SINTGRL` computes the integrals again. For a valid
  positive spectrum `EDRKTOT > 0.` as well, and `SIGM_WAM` is determined again.

A direct check that passed old and zeroed values through `SWCAP`, `SWCAP8`
and `SSURF` gave bit-identical source term outputs. The code paths above and
the compact state test are the lasting evidence.

Initialising can make a noticeable difference in three exceptional situations:

1. Without initialisation, the first zero-energy call of a thread has no
   previous value yet. `SSURF` copies `SIGM_WAM` before the `BB` guard; a
   strict runtime with signalling NaN or floating-point traps can therefore
   stop. This affects robustness, not the normal physical result.
2. Modified external code can use the public `SINTGRL` output argument
   `AC2TOT`, or future SWAN code can start using a workspace value before a
   guard. Then the old value from another point can have an effect.
3. An invalid spectrum with negative values or NaNs breaks the positivity
   relation between `ETOT` and the other moments. The result neutrality
   described above does not hold for that case.

So initialising would not change results for the normal solver, but it can
prevent a runtime error and protects external or future users. It is
therefore a separate correctness change.

**Reachability in the so-rp case.** A temporary counter around the
`ETOT == 0.` branch was run on a so-rp computation (corrected deck, 20 m/s
from 310°, NAP +3.00 m, barrier open) with eight OpenMP threads, on
11 November 2026 at commit `20bfacb` (patch:
`doc/measurements/sintgrl-cnt.patch`). Input, grid, physics and output were
unchanged; only the iteration limit was reduced for the short trial. After
one iteration there were 148 `SINTGRL` calls with exactly zero energy, at 147
different grid points. A separate trial with two iterations counted 150 in
total, so two more in the second iteration. The count after one iteration
(148) was the same on four threads, and that after two (150) on one thread.
These are calls over the four sweeps, not necessarily the same number of
unique grid points. (A trial before the history was rebuilt counted 188 and
192.) So in this case the path really is reachable, and not only an
artificial unit-test situation.

**Limited test footprint.** One compact fixture in
[`test_contexts.f90`](../tests/test_contexts.f90) checks the seven carry-over
fields and the four entry defaults bit for bit. There is deliberately no
separate executable or permanent so-rp regression run for this effect alone.
The fork keeps upstream's behaviour here and does not initialise the values;
a fix would be its own small change to this fixture.

### `SwanCompdata` — no unstructured stencil mirror

Upstream keeps the unstructured stencil in the thread-private array `vs`. The
fork has no such array. `SwanCompUnstruc` fills the local `st_kc` directly;
the kernels below it receive the grid addresses they need as explicit
arguments. `SwanCompdata` holds no separate stencil state.

The shared integral route is explicit as well: `SINTGRL` receives the
current point, the two upwind neighbour points and `(IX,IY)` from the solver.
`BRKPAR`, `PEREXC` and `SWBIDW` then get those neighbour points as scalars.
As a result, these routines cannot read the stencil of another thread or
solver context without anyone noticing.

### `swan_time` — the timers

| Symbol | Build variant | Lifetime | Owner |
|---|---|---|---|
| `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | always present; used with `TIMG=ON` | process | `THREAD_TIMERS(thread)` of type `thread_timers_t` |

Timing is instrumentation, not model state. Upstream hides it behind the
`!TIMG` switch. In the fork the code is ordinary Fortran that every build
compiles; the calls of `SWTSTA`, `SWTSTO` and `SWPRTI` sit behind the
compile-time constant `timing_enabled`, so a standard build never runs them
and never allocates the timers.

Every thread has its own set of timers: one element of the array
`THREAD_TIMERS`, indexed with the OpenMP thread number. `SWMAIN` and both
solvers call `prepare_thread_timers` in the serial part of the program, so
that the array has an element for every thread before a parallel region
starts. `SWTSTA` and `SWTSTO` work on the element of the thread that calls
them. `SWPRTI` reports the element of the thread that calls it, which is the
thread that runs the serial part of the program. What the other threads
measure in the sweeps is not in the report. That was so with the
thread-private counters as well: `SWPRTI` only ever read the copy of its own
thread.
The timers belong to the process and not to a model run, so `SWCLME` does
not release them.

## Checks

1. Invariant tests for all seven category-5 values are present, and they pin
   upstream's carry-over behaviour.
2. The stencil exists once per call, in variables of the calling thread. The
   whitecapping workspace does not add another storage layer.
3. `scripts/check_thread_state.py` (CTest `thread_state_manifest`, and a step
   of CI) finds no `THREADPRIVATE` or `COPYIN` directive under `src/`, and
   finds the names of the table "Per-thread state in the solvers" in the
   `PRIVATE` or `FIRSTPRIVATE` clause the table gives.
4. The OpenMP check must include `TESTFL`/`IPTST`: they determine the `TEST`
   output, so an error shows up in the PRINT diff and not in the Hsig
   statistics. Under OpenMP every CTest reference test runs on 2 threads,
   and the CI smoke test compares the unstructured case on 1 and 4 threads;
   running the structured references on 1 and 4 threads is not automated.
