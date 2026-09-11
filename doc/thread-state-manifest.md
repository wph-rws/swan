# Thread-state manifest

This document lists the OpenMP thread state that the fork keeps, how it
differs from upstream SWAN 41.51, and where the whitecapping state is held.

`scripts/check_thread_state.py` compares the tables below with the source and
fails if a `THREADPRIVATE` symbol is added or removed.

## Method

The analysis is **import-aware**: a symbol only counts in a file that actually
`USE`s the matching module. That is not a formality. A count based on names
alone attributes ten writers of `EKTOT` to
[swanout1.f90:4085](../src/swanout1.f90#L4085) onwards, but that is a local
`REAL` at [line 3190](../src/swanout1.f90#L3190) in a file that does not import
the whitecapping state: a name clash, not shared state.

Lifetime is the shortest unit over which the value must stay valid:
`point` (one grid point or vertex), `sweep`, `iteration`, `run`, `process`.

## The four remaining directives

| # | Location | Module | Symbols | Build variant |
|---|---|---|---|---|
| 1 | [swan_stencil.f90:38](../src/swan_stencil.f90#L38) | `swan_stencil` | `RDFSIN` | always |
| 2 | [swan_test_output.f90:34](../src/swan_test_output.f90#L34) | `swan_test_output` | `IPTST, TESTFL` | always |
| 3 | [swan_propagation_scheme.f90:27](../src/swan_propagation_scheme.f90#L27) | `swan_propagation_scheme` | `PROPSL` | always |
| 4 | [swan_time.f90:40](../src/swan_time.f90#L40) | `swan_time` | `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | always present; active with `TIMG=ON` |

Directive 4 is ordinary Fortran that is always visible. Upstream hides it
behind the `!TIMG` switch. In the fork only the calls that change the
counters sit behind the compile-time constant `timing_enabled`. As a result
the compiler sees the timing backend in every build, and it stays inactive in
a standard build.

Upstream 41.51 has eight `THREADPRIVATE` directives. Four of them do not
exist in the fork:

- the eleven whitecapping scalars in `M_WCAP`. The fork has no such module;
  the values live in an explicit `wcap_workspace_t` per solver thread;
- the stencil values `IXCGRD, IYCGRD, KCGRD` and `COSLAT` in `SWCOMM3`;
- `ICMAX` and `CSETUP` in `SWCOMM3`;
- the unstructured stencil mirror `vs` in `SwanCompdata`.

In the fork the stencil values are not module data at all. The solvers keep
track of the active point in thread-local scratch (`st_ix, st_iy, st_kc,
st_co, st_nm` in `SWOMPU`, and the same in `SwanCompUnstruc`, where `st_nm`
inherits the serial width through `FIRSTPRIVATE`) and pass each kernel its
context explicitly, as scalars or `MICMAX` arrays. Only `RDFSIN` stays
threadprivate, seeded with `COPYIN`.

## COPYIN: which threads are seeded

The two solvers seed different sets. That is design input, not a detail: only
these symbols must have the master value on entry to the parallel region.

| Symbol | [swancom1.f90:1303-1305](../src/swancom1.f90#L1303-L1305) (structured) | [SwanCompUnstruc.f90:499](../src/SwanCompUnstruc.f90#L499) (unstructured) |
|---|---|---|
| `IPTST` | ✅ | ✅ |
| `TESTFL` | ✅ | ✅ |
| `RDFSIN` | ✅ | ✅ |
| `PROPSL` | ✅ | — |
| `wcap_workspace_t` | explicit per thread | explicit per thread |

The structured solver in upstream also seeds `ICMAX`, `CSETUP` and `COSLAT`.
The fork does not: no kernel reads them implicitly. The thread-local width
(`st_nm`) inherits the serial value through `FIRSTPRIVATE` in the
unstructured region; the structured call has its own stack copies per point.

`PROPSL` is missing from the unstructured region because that solver does not
use the structured propagation scheme. Unlike upstream, `CSETUP` is not
thread state: `SETUP2D` returns its convergence status through `SETUPP` to
the local `SETUP_CONVERGED` of `SWCOMP`.

## Manifest per symbol

Categories: **1** run-shared read-only, **2** run-shared mutable, **3**
thread state seeded through COPYIN, **4** proven write-before-read scratch,
**5** conditionally written or persisting, **6** solver- or switch-specific.

### `swan_stencil` — stencil and propagation choices

`RDFSIN` is the only value in this module (threadprivate, see below). Upstream
keeps five more stencil values in `SWCOMM3`; in the fork they are not module
data:

| Symbol | In the fork | Owner in the fork |
|---|---|---|
| `IXCGRD` | not in `swan_stencil` | thread-local `st_ix` in `SWOMPU`/`SwanCompUnstruc`, passed explicitly as scalars/arrays (caller) |
| `IYCGRD` | same | same (`st_iy`) |
| `KCGRD` | same | same (`st_kc`) |
| `COSLAT` | same | `SWGEOM` writes the local `st_co` of `SWOMPU`; `ACTION`/`SwanTranspAc` pass it explicitly |
| `RDFSIN` | ✅ COPYIN, threadprivate (wind scale per thread) | `swan_stencil` |
| `ICMAX` | not in `swan_stencil` | thread-local `st_nm`/`st_n`/`icmax` dummy in every kernel; `FIRSTPRIVATE` in the unstructured region |

The fork also has no copy of the stencil in `SwanCompdata` (upstream's `vs`).
Each call therefore has one stencil owner: the calling thread's scratch, not
a module.

### `swan_test_output` and `swan_propagation_scheme` — test status and local propagation

Upstream keeps these three in `SWCOMM4`. The fork splits them into two
modules: one for the test status and one for the propagation scheme.

| Symbol | Solver | COPYIN | First definition | Lifetime | Cat. | Proposed owner |
|---|---|---|---|---|---|---|
| `IPTST` | both | ✅ | [swancom1.f90:3181](../src/swancom1.f90#L3181), [SwanCompUnstruc.f90:995](../src/SwanCompUnstruc.f90#L995) | point | 3 | `common_thread_seed_t` |
| `TESTFL` | both | ✅ | [swanmain.f90:1654](../src/swanmain.f90#L1654) | point | 3 | `common_thread_seed_t` |
| `PROPSL` | structured | ✅ | [swanmain.f90:1217](../src/swanmain.f90#L1217) | sweep | 3/6 | `structured_thread_workspace_t` |

The COPYIN of `IPTST` and `TESTFL` is probably unnecessary. Both solvers set
the pair to `0`/`.FALSE.` at the top of the per-point block
([swancom1.f90:3181](../src/swancom1.f90#L3181),
[SwanCompUnstruc.f90:995](../src/SwanCompUnstruc.f90#L995)) before they read
it, and neither appears inside the parallel region itself
([swancom1.f90:1291-2353](../src/swancom1.f90#L1291-L2353)): all uses are in
called routines. That makes them scratch rather than seed. This is not
proven: nobody has checked whether a routine called earlier in the region
already reads `TESTFL`. Check that before deleting the COPYIN; until then, the
COPYIN is unnecessary but not wrong.

`IPTST` is also used as a `DO` loop variable in
[swanmain.f90:6096](../src/swanmain.f90#L6096) onwards, outside the parallel
region. That is legal, but it means the symbol carries two meanings. If this
state moves into a context type, the loop variable should become a local
variable, not a field of the context.

### Whitecapping integrals (upstream `M_WCAP`) — integral parameters per grid point

In upstream, the eleven values are threadprivate module variables in
`M_WCAP`. They are written only in `SINTGRL`
([swancom1.f90:5174-5737](../src/swancom1.f90#L5174-L5737)); only four files
import the module, and no scalar is in COPYIN.

| Symbol | Default on entry | Write condition | Cat. | Invariant |
|---|---|---|---|---|
| `KM_WAM` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L179) | refined when `EDRKTOT > 0.` | 4 | proven |
| `KM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L180) | refined when `EKTOT > 0.` | 4 | proven |
| `SIGM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L181) | refined when `ETOT1 > 0.` | 4 | proven |
| `SIGM_10` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L182) | refined when `ACTOT > 0.` | 4 | proven |
| `ACTOT` | **none** | [`IF (ETOT > 0.)`](../src/swancom1.f90#L5505) | **5** | tested: value is kept |
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
of the eleven, while [line 5730](../src/swancom1.f90#L5730) executes
`AC2TOT = ACTOT` unconditionally. The thread then carries over the value from
the previous point it handled. Upstream behaves the same way.

**Effect on results.** For valid, non-negative spectra, initialising these
seven values does not change the numerical result of the solver:

- [`SWCAP`](../src/swancom2.f90#L2234) and
  [`SWCAP8`](../src/swancom2.f90#L2730) return when `ETOT <= 0.` before
  a workspace value can affect a source term;
- `SSURF` reads `SIGM_WAM` for `ISURF = 6`, but only uses the chosen
  frequency behind the [`BB > 0`](../src/swancom2.f90#L2154) condition;
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

**Reachability in the so-rp case.** A temporary atomic counter around the
`ETOT == 0.` branch was run on a so-rp computation (20 m/s from 310°,
NAP +3.00 m, barrier open) with eight OpenMP threads. Input, grid and physics
were unchanged; only the iteration limit and the amount of output were
reduced for the short trial. After one iteration there were 188 `SINTGRL`
calls with exactly zero energy. A separate trial with two iterations counted
192 in total, so four more in the second iteration. These are calls over the
four sweeps, not necessarily the same number of unique grid points. So in
this case the path really is reachable, and not only an artificial unit-test
situation.

**Limited test footprint.** One compact fixture in
[`test_contexts.f90`](../tests/test_contexts.f90) checks the seven carry-over
fields and the four entry defaults bit for bit. There is deliberately no
separate executable or permanent so-rp regression run for this effect alone.
The fork keeps upstream's behaviour here and does not initialise the values;
a fix would be its own small change to this fixture.

### `SwanCompdata` — no unstructured stencil mirror

Upstream keeps the unstructured stencil in the threadprivate array `vs`. The
fork has no such array. `SwanCompUnstruc` fills the local `st_kc` directly;
the kernels below it receive the grid addresses they need as explicit
arguments. `SwanCompdata` holds no separate stencil state.

The shared integral route is explicit as well: `SINTGRL` receives the
current point, the two upwind neighbour points and `(IX,IY)` from the solver.
`BRKPAR`, `PEREXC` and `SWBIDW` then get those neighbour points as scalars.
As a result, these routines cannot read the stencil of another thread or
solver context without anyone noticing.

### `swan_time` — TIMG counters

| Symbol | Build variant | Lifetime | Cat. | Proposed owner |
|---|---|---|---|---|
| `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | always present; active with `TIMG=ON` | process | 6 | `timing_context_t`, outside the context design |

Timing is instrumentation, not model state, so this group is not part of the
context design. The CMake option only decides whether ordinary, visible calls
are active; the counters therefore belong to the process and not to a model
run.

## Checks

1. Invariant tests for all seven category-5 values are present, and they pin
   upstream's carry-over behaviour.
2. The stencil exists once per call, in the calling thread's scratch. The
   whitecapping workspace does not add another storage layer.
3. The COPYIN symbols that both solvers share (`IPTST`, `TESTFL` and
   `RDFSIN`; `PROPSL` only structured) stay in their own modules. The source
   term workspace does not store this COPYIN state a second time.
4. The OpenMP check must include `TESTFL`/`IPTST`: they determine the `TEST`
   output, so an error shows up in the PRINT diff and not in the Hsig
   statistics. The structured and unstructured reference tests therefore run
   with 1, 2 and 4 threads.
