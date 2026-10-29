# Modern Fortran status

SWAN is compiled as standard-conforming Fortran 2018, but language mode alone
does not make the complete code base idiomatic modern Fortran. The fork keeps
the numerical results and the input/output formats of upstream SWAN 41.51,
while making unsafe interfaces visible and removable.

## How the fork differs from upstream

- `swan_kinds` defines the supported real and integer kinds in one place.
  Numerical declarations in migrated code do not take their precision from
  compiler defaults or nonstandard spellings.
- `m_constants` is private by default, exports only its supported constants and
  initialization routine, and uses kind-qualified declarations and literals.
- `swan_service_interfaces` gives the heavily used runtime services explicit
  interfaces and argument intents: diagnostics, tracing, stop/error checks,
  text-boundary handling and floating-point comparison.
- Coherent families of upstream external procedures are private-by-default
  modules. These cover input parsing; coordinate and field input; time
  conversion; number formatting; real/integer array copying; angle and line
  geometry; wave dispersion; spectral shaping, interpolation, integration and
  output; structured-grid interpolation; boundary-point validation; and triad
  coefficients. Callers import only the procedures they use.
- Simulation-clock state is collected in `time_context_t`. Date conversions
  can receive an explicit context, so independent runs do not share a hidden
  first reference day, as they do in upstream. Existing callers use
  `default_time_context`.
- Command-parser state is collected in `command_reader_t`. Every stateful
  parser entry point accepts either its upstream argument list or a reader
  context as the first argument. Calls made inside the parser pass that
  context on; two readers therefore do not consume or overwrite each other's
  keyword, line and value state. Upstream's `OCPCOMM1` module does not exist
  in the fork.
- Diffraction, triad and quadruplet lookup state have separate owners:
  `diffraction_state_t`, `triad_state_t` and `snl4_tables_t`. The driver
  passes those objects explicitly; upstream's `M_DIFFR`, `M_SNL3` and `M_SNL4`
  storage modules do not exist in the fork.
- The shared `SIGPOW` array is `spectral_powers_t`. Its two writers, ordinary
  spectral-grid setup and IEM reinitialization, use one `rebuild` contract,
  and cleanup uses `clear`.
- The eleven whitecapping scalars that upstream declares `THREADPRIVATE` in
  `M_WCAP` are held in a `wcap_workspace_t` per structured or unstructured
  solver thread. The solver boundary chooses the active workspace; source-term
  kernels do not query the OpenMP runtime. Upstream's carry-over behaviour of
  the seven conditionally written fields is pinned by bitwise invariant tests.
- The `KSCIP1` and `KSCIP2` dispersion APIs make secondary results optional.
  Callers that only need a wave number do not pass the same work array as
  several output arguments, as upstream does. This removes a Fortran aliasing
  violation and avoids unused array writes.
- `KSCIP1`, `GAMMAF` and `TCROSS` keep their traced upstream entry points but
  delegate their mathematics to directly tested pure kernels. `GAMMAF`'s
  scalar kernel is elemental.
- MPI reductions use typed `MPI_ALLREDUCE` calls with `MPI_IN_PLACE`. Explicit
  buffer counts are validated where the Fortran rank exposes the available
  size. Upstream's unused send/reduction implementations are not in the fork.
- The FFTW compatibility entry points have explicit interfaces and keep their
  external ABI.
- Unlike upstream, `INTSTR` does not modify the integer supplied by its caller
  while converting it to text. `COPYCH` likewise caps its local working length
  instead of overwriting a length argument that callers often pass as a
  literal.
- Configured source files are rewritten only when their content changes. A
  second unchanged Ninja build therefore performs no Fortran compilation.
- CTest covers the FFT layer and the quick, nonstationary and nonlinear example
  suites, plus isolated time and command-reader contexts. MPI configurations
  add a genuine two-process quick test. The Python runners keep generated
  output in build-tree work directories.
- The serial and netCDF builds share one source list.

## Concrete before/after examples

Upstream's wave-number-only calls use one disposable array for several
distinct outputs:

```fortran
call KSCIP1(MSC, spcsig, depth, kwave, work, work, work)
call KSCIP2(MSC, spcsig, depth, kwave, work, work, work, work, mud_depth)
```

In the fork the typed interface expresses what the caller needs:

```fortran
call KSCIP1(MSC, spcsig, depth, kwave)
call KSCIP2(MSC, spcsig, depth, kwave, mud_depth=mud_depth)
```

The first form can be misoptimized because several definable dummy arguments
alias the same actual argument. The second form is standard-conforming, permits
interface checking, and skips writes to unused output arrays.

Broad module imports such as:

```fortran
use m_constants
```

are narrowed in the fork:

```fortran
use m_constants, only: dera, pih, rade, sqrtg, trshdep
```

This documents dependencies and prevents an unrelated new module symbol from
silently changing name resolution in a consumer.

In upstream, an external function needs an unrelated local type declaration:

```fortran
real :: gammaf
ctot = gammaf(0.5 * ms + 1.) / gammaf(0.5 * ms + 0.5)
```

In the fork the spectrum family exposes a checked API:

```fortran
use swan_spectrum_transform, only: gammaf
ctot = gammaf(0.5 * ms + 1.) / gammaf(0.5 * ms + 0.5)
```

The compiler knows the complete function signature at every migrated call
site. The same applies to multi-argument procedures such as `FLFILE`,
`READXY`, `SVALQI`, `WRSPEC`, `TCOEF`, `SWIPOL` and the
interpolation/integration routines, where a mismatch is much harder to spot by
inspection.

In upstream, stateful time conversion selects its first reference day through
a module global. In the fork, independent conversions make ownership visible:

```fortran
type(time_context_t) :: forecast_clock, hindcast_clock

t_forecast = dttime(forecast_date, forecast_clock)
t_hindcast = dttime(hindcast_date, hindcast_clock)
```

The upstream form `dttime(forecast_date)` remains available and delegates to
`default_time_context`.

The command reader follows the same compatibility pattern:

```fortran
type(command_reader_t) :: reader

call rdinit(reader)
call inkeyw(reader, 'STA', 'XXXX')
if (keywis(reader, 'COMPUTE')) then
   ! ...
end if
```

This replaces upstream's direct changes to `ELTYPE`, `KEYWRD`, `LENCST`,
`KAART` and the other `OCPCOMM1` variables with state owned by a concrete
reader. Calls without `reader` use `default_command_reader`.

## Compiler diagnostics

With GNU Fortran's `-Wall -Wextra -Wimplicit-interface -Wsurprising
-Wconversion-extra -Wcharacter-truncation` diagnostics, the counts are:

| Diagnostic inventory | Upstream 41.51 | Fork | Reduction |
|---|---:|---:|---:|
| All warnings | 19,022 | 1,280 | 93% |
| Implicit-interface warnings | 3,573 | 0 | 100% |

Both counts are measured with gfortran 16.2 and the same flags; for upstream
that means replacing the `-w` its own CMake sets, which silences every
warning. See
[strict-warnings.md](strict-warnings.md) for the breakdown by kind and the
review of each warning that can hide a defect.

Explicit interfaces also make some warnings appear. `-Wconversion-extra` and
the argument diagnostics can only fire at a call site once the compiler knows
the dummy argument types, so making several thousand calls checkable exposes
conversions that were invisible before.

The only calls into an external C library, the `METIS_*` calls, go through an
explicit `ISO_C_BINDING`/`BIND(C)` binding in
`src/swan_metis_interface.f90`. A C binding is an explicit interface. CMake
derives the `idx_t`/`real_t` widths from the installed `metis.h` (32/32 here)
and stops with a clear error on a different installation; `SwanParallel`
checks how arguments are passed and the index range.
`scripts/strict_diagnostics.py` guards that the strict build has no
`implicit-interface` warnings. Measure with a *clean* build: an incremental
one only reports the files it recompiled.

### Where not to start on the remaining warnings

The review of each warning that can hide a defect, and the order for further
work, are in [strict-warnings.md](strict-warnings.md). Two lessons for
`-Wmaybe-uninitialized`: prefer reports about *named locals* over array
descriptor temporaries (`*.dim[0].ubound`, `*.offset`), which are artefacts of
optimised descriptors; and do not silence a report with an initialiser, which
hides the next real defect instead of surfacing it.

`-Wunused-variable` looks like the easier target and is not:

- Much of it sits in numerics taken from elsewhere, such as `mod_xnl4v5.f90`
  and `SdsBabanin.f90`. Editing those buys a lower count and pays for it in
  merge friction on the next import from Delft.
- **Switch variants hide real uses.** GNU Fortran only sees the variant it
  compiles. `swanparll.f90` and `swancom1.f90` still carry substantial
  switch-prefixed sections, so a declaration that looks unused in the serial
  build may well be used under `!MPI` or `!JAC`. Removing it breaks a
  configuration nothing builds by default.

The safe order is therefore: leave the files taken from elsewhere alone, and
check every other candidate against the switch-prefixed lines in the *raw*
source before removing it. The generated source has already dropped the
inactive variants.

The same blind spot cuts the other way when a procedure moves into a module.
Its `USE` line is then visible to *every* variant, while the calls to it stay
behind their own switch prefix, and a default build compiles clean either way.
So the rule is: **an import used only from a switch-prefixed call site carries
that same prefix**. `SWRECVAC` and `SWSENDAC` do not exist under `!JAC`,
`SWSYNC` is only called under `!JAC`, and JAC-only bodies need imports while
the default ones do not; each of those imports therefore sits behind the
prefix of the variant that uses it. The only way to know is to build the
variant, which is why every one of them has a registered test.

## Guarding the result

Two mechanisms keep this from eroding. Both have been checked in each
direction: a check that cannot fail is worse than no check.

- **The results themselves are pinned.** The quick test and both nonstationary
  cases compare their table and block output against stored references,
  produced with gfortran 16.2. A reference changes only deliberately and with
  the reason recorded; the unstructured case differs from upstream because of
  BF-15. Text has to match exactly apart from trailing blanks, which the MPI
  output path strips; numbers are compared to a relative tolerance of 1e-3.
  NaN and infinity parse as floats but fail the comparison outright: they carry
  nothing a tolerance can act on, and NaN would otherwise pass by comparing
  unequal to everything. See `examples/reference_check.py`.
- **No warning that can hide a defect goes unreviewed.**
  `scripts/strict_diagnostics.py` does a clean build with the strict flags.
  Every warning of the kinds that can point to a real defect (unset values,
  index bounds, skipped calls, truncated text, loss of precision, and warnings
  without a `[-W…]` flag) needs a verdict in the review register, identified
  by category, file, routine and message, without line number. A new one, or
  one that moves to another routine, fails the check until it is reviewed. The
  check also refuses a build whose compile commands lack the strict flags.
  See [strict-warnings.md](strict-warnings.md).

### Why the numeric comparison is not bit-exact

A tolerance of 1e-3 is enough to tell a changed result from a rounding
difference, and it makes the check independent of the compiler's exact
rounding. Bit-reproducibility is checked separately where it matters: under
OpenMP, the unstructured solver gives the same hash on four threads as on one,
which `scripts/unstruc_determinism_smoke.py` checks on a deck with wind. In
upstream that result depends on the number of threads, for two reasons: a
missing stencil neighbour in the front ordering (BF-15) and a scratch variable
that all threads share in the first iteration of a run with wind (BF-17).

## Behaviour preservation

The optimized LTO build does not need `-fno-strict-aliasing`. Its quick-test
center table and significant wave-height block are byte-identical to the
ordinary Release build. Every build option the project offers passes its
registered tests: serial, OpenMP, MPI (also on two processes), MPI+netCDF,
netCDF, LTO, TIMG, METIS, MATL4, JAC, FFRO, the debug-invariant build and
`SWAN_RUNTIME_CHECKS`.

Two of those options change the answer rather than the code path around it, and
they have their own stored references instead of being exempt from comparison:

| Option | What changes | Where it shows |
|---|---|---|
| `JAC` | Jacobi subdomain exchange instead of following the wavefront across them | only on more than one MPI process |
| `FFRO` | fixed-front (`!FXFRO`) vertex ordering instead of the graph-coloured one | the unstructured case |

The difference comes from the scheme itself, which upstream offers in the
same way.

These checks demonstrate compatibility for the included regression cases. They
are not a claim that compiler diagnostics are clean or that every production
scenario is covered.

### Runtime checks

`-DSWAN_RUNTIME_CHECKS=ON` adds the compiler's own bounds, allocation and
argument checks. They pass because of one fix to a defect that upstream has as
well (BF-08): 25 arrays that could be passed on while still unallocated.

Input fields are only allocated once a `READINP` command supplies them, and the
global grid arrays only when the grid is structured; both are passed on
unconditionally, because whether the data exists is decided by flags like `LEDS`,
never by `ALLOCATED`. In upstream, any deck that leaves one out therefore
passes an unallocated allocatable. That is undefined behaviour that happens to
work, because the callee guards on the flag and never dereferences.

In the fork `SWINIT` gives all of them the empty state. That alone would be
worse than the disease: the allocations that fill them are guarded by
`IF (.NOT.ALLOCATED(..))` in upstream, so an array pre-allocated at size zero
would *stay* at size zero and the fill would write past its end. Sizing is
what makes the empty state safe, so they go through `ENSURE_FIELD_SIZE` in
`M_GENARR`, which grows the array to the size asked for. That also closes a
latent bug in upstream's guard (BF-09): reading the same grid again at a
different size silently kept the first one.

One deliberate exception, marked in the source: the `TEST` branch that falls
back to an empty `XYTST` still only allocates when unallocated, because
`NPTST` keeps its count from an earlier `TEST POI` and a resize there would
leave the two disagreeing.

Use it on top of an optimizing build type. Integer powers go through
`src/swan_integer_powers.f90`, so `-O0` evaluates them in the same order as an
optimized build, and the Debug route passes all tests (see
[closing-account.md](closing-account.md)).

## Remaining migration boundary

The large collections of upstream external procedures are split into
subsystem modules: source terms (`swan_wind_source`, `swan_dissipation`,
`swan_nonlinear_interactions`, `swan_propagation`), computation
(`swan_computation`), input (`swan_command_reading`, `swan_input_processing`),
output (`swan_output_orchestration`, `swan_output_writers`), services
(`swan_services`), parallel synchronization (`swan_parallel`) and the driver
(`swan_driver`). Each exports only the entry points its callers use, so a large
number of symbols that are global in upstream are implementation detail in the
fork.

There is no `MSGERR` dependency cycle. `swan_parallel_state` is a
dependency-free module holding `MASTER`, `INODE`, `NPROC`, `IAMMASTER` and
`PARLL`; `M_PARALL` uses and re-exports it. Because the flags are reachable
without `M_PARALL`, the diagnostic services that upstream keeps in
`ocpmix.f90` live in `swan_service_interfaces` itself: `STRACE`, `MSGERR`,
`STPNOW`, `EQREAL`, `EQDBLE`, `TABHED` and `BUGFIX` are module procedures, so
their roughly 76 callers are checked against the real implementation without
a single caller having to change. The fork has no `ocpmix.f90`.

There is no parser cycle either. `UPCASE` is in the leaf module
`swan_text_utilities`, so `DTSTTI` does not depend on the parser. `DTSTTI` and
`DTTIST` are module procedures of `swan_time`, and `REPARM` and `LSPLIT` are in
`swan_input_helpers`, which sits above the parser rather than below it. That
file holds only `swan_input_helpers` and is named after it.

What deliberately stays external: `TXPBLA`, kept in the interface block next to
the timing backend routines it shares a file with, plus the byte converters
`SWI2B` and `SWR2B` in the CMake-selected Matlab-v4 compatibility source.

Timing is not switch-activated source text, as it is in upstream: a generated
logical parameter guards ordinary calls, while its backend is compiled in
every configuration. Matlab output goes through the fixed module entry point
`SWRMAT`; CMake selects one whole v4 or v5 backend source. Only v4 links the
two external byte converters from upstream.

The standalone upstream `Swan*.f90` files (`SwanFindPoint`, `SwanReadGrid`,
`SwanVertlist` and 34 others) are modules in the fork, named after the file in
snake_case. Their `CONTAINS`-ed helpers are internal, and callers import them
with an `ONLY` list at module level.

### How modules are named

Two styles sit side by side, and the difference carries meaning:

- A module holding **one** upstream procedure takes its name from the file, in
  snake_case: `SwanDispParm.f90` → `swan_disp_parm` → `SwanDispParm`. The
  procedure names come from TU Delft, so keeping the chain aligned means a
  reader who sees `use swan_disp_parm` knows which file to open, and an upstream
  import still lands where it should.
- A module that **groups** several upstream procedures has no such name to
  inherit and gets a descriptive one: `swan_dissipation` covers SBOT, SVEG,
  SSURF, SWCAP and seven others.

### What remains shared

Long-lived mutable data modules are narrower than in upstream. The contexts
cover the clock, command parser, I/O streams, diagnostic status, diffraction,
nonlinear interaction tables, spectral powers and the whitecapping
thread-workspace. A reader can own its input file, its log and its error
state, and the file opener draws from its unit range. Every run-state group
that upstream `SWCLME` releases piece by piece has an idempotent owner `clear`
(see [run-state-ownership.md](run-state-ownership.md)), so consecutive runs in
one process reproduce each other bit for bit (`test_two_cases`, A–B–A). Grid
dimensions, most physics settings and output request tables live in small
focused modules (upstream's `SWCOMM*`/`OCPCOMM*` families do not exist in the
fork). In `swan_stencil` only `RDFSIN` is `THREADPRIVATE`, seeded by `COPYIN`
(for the other directives see
[thread-state-manifest.md](thread-state-manifest.md)); all other stencil
values travel as explicit procedure arguments from thread-local solver
scratch.

The accurate description is therefore “standard Fortran 2018 with a modular,
compiler-checked interface layer and explicit ownership for the migrated
subsystems over a still partly shared case state,” not “everything is modern
Fortran.”
