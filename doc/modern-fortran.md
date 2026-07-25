# Modern Fortran status

SWAN is compiled as standard-conforming Fortran 2018, but language mode alone
does not make the complete code base idiomatic modern Fortran. The current
modernization deliberately preserves numerical results and the established
input/output formats while making unsafe interfaces visible and removable.

## Changes made

- `swan_kinds` centralizes the supported real and integer kinds. New and
  migrated numerical declarations no longer encode precision through compiler
  defaults or nonstandard spellings.
- `m_constants` is private by default, exports only its supported constants and
  initialization routine, and uses kind-qualified declarations and literals.
- `swan_service_interfaces` gives the remaining heavily used runtime services
  explicit interfaces and argument intents: diagnostics, tracing, stop/error
  checks, text-boundary handling and floating-point comparison.
- Coherent external procedure families are now private-by-default modules.
  These cover input parsing; coordinate and field input; time conversion;
  number formatting; real/integer array copying; angle and line geometry;
  wave dispersion; spectral shaping, interpolation, integration and output;
  structured-grid interpolation; boundary-point validation; and triad
  coefficients. Callers import only the procedures they use.
- Simulation-clock state is collected in `time_context_t`. Date conversions
  can now receive an explicit context, so independent runs no longer share the
  hidden first reference day. Existing callers use `default_time_context`.
- Command-parser state is collected in `command_reader_t`. Every stateful
  parser entry point accepts either its legacy argument list or a reader
  context as the first argument. Calls made inside the parser propagate that
  context; two readers therefore do not consume or overwrite each other's
  keyword, line and value state. `OCPCOMM1` has been removed.
- The `KSCIP1` and `KSCIP2` dispersion APIs make secondary results optional.
  Callers that only need a wave number no longer pass the same work array as
  several output arguments. This removes a Fortran aliasing violation and
  avoids unused array writes.
- MPI reductions use typed `MPI_ALLREDUCE` calls with `MPI_IN_PLACE`. Explicit
  buffer counts are validated where the Fortran rank exposes the available
  size. The unused legacy send/reduction implementations have been removed.
- The FFTW compatibility entry points have explicit interfaces while retaining
  their existing external ABI.
- `INTSTR` no longer modifies the integer supplied by its caller while
  converting it to text. `COPYCH` similarly caps its local working length
  instead of overwriting a length argument that callers often pass as a
  literal.
- Configured source files are rewritten only when their content changes. A
  second unchanged Ninja build therefore performs no Fortran compilation.
- CTest covers the FFT layer and the quick, nonstationary and nonlinear example
  suites, plus isolated time and command-reader contexts. MPI configurations
  add a genuine two-process quick test. The Python runners isolate generated
  output in build-tree work directories.
- The serial and netCDF source lists share one canonical source list.

## Concrete before/after examples

An old wave-number-only call used one disposable array for four distinct
outputs:

```fortran
call KSCIP1(MSC, spcsig, depth, kwave, work, work, work)
call KSCIP2(MSC, spcsig, depth, kwave, work, work, work, work, mud_depth)
```

The typed interface now expresses what the caller needs:

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

are now narrowed:

```fortran
use m_constants, only: dera, pih, rade, sqrtg, trshdep
```

This documents dependencies and prevents an unrelated new module symbol from
silently changing name resolution in a consumer.

An external function previously needed an unrelated local type declaration:

```fortran
real :: gammaf
ctot = gammaf(0.5 * ms + 1.) / gammaf(0.5 * ms + 0.5)
```

The spectrum family now exposes a checked API:

```fortran
use swan_spectrum_transform, only: gammaf
ctot = gammaf(0.5 * ms + 1.) / gammaf(0.5 * ms + 0.5)
```

The compiler now knows the complete function signature at every migrated call
site. The same treatment was applied to multi-argument procedures such as
`FLFILE`, `READXY`, `SVALQI`, `WRSPEC`, `TCOEF`, `SWIPOL` and the
interpolation/integration routines, where a mismatch is substantially harder
to spot by inspection.

Stateful time conversion previously selected its first reference day through a
module global. Independent conversions now make ownership visible:

```fortran
type(time_context_t) :: forecast_clock, hindcast_clock

t_forecast = dttime(forecast_date, forecast_clock)
t_hindcast = dttime(hindcast_date, hindcast_clock)
```

The legacy `dttime(forecast_date)` form remains available and delegates to
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

This replaces direct mutation of `ELTYPE`, `KEYWRD`, `LENCST`, `KAART` and the
other `OCPCOMM1` variables with state owned by a concrete reader. Existing
calls without `reader` use `default_command_reader`.

## Measured effect

With GNU Fortran's `-Wall -Wextra -Wimplicit-interface -Wsurprising
-Wconversion-extra -Wcharacter-truncation` diagnostics, the inventory changed
as follows:

| Diagnostic inventory | Before | Interface layer | Current | Reduction |
|---|---:|---:|---:|---:|
| All warnings | 4,443 | 1,508 | 1,554 | 65% |
| Implicit-interface warnings | 3,229 | 288 | 6 | 99.8% |

The "interface layer" column is the state after explicit interfaces were added
but before the monolithic source files became modules; "current" is after that
subsystem split, which also turned the standalone `Swan*.f90` files into
modules and moved the diagnostic services into `swan_service_interfaces`. The
total warning count rose slightly, which is expected rather than a regression:
`-Wconversion-extra` and the argument diagnostics can only fire at a call site
once the compiler knows the dummy argument types, so making several thousand
calls checkable exposes conversions that were previously invisible. The
categories behind the current total are dominated by `-Wconversion-extra`
(493), `-Wunused-variable` (256) and `-Wcompare-reals` (226).

Six implicit-interface call sites remain. Two are `METIS_*` calls into the
external C library and can never be Fortran interfaces; the other four are
`LSPLIT` and `DTSTTI`/`DTTIST`, which stay external because of the parser cycle
described below. Measure with a *clean* build — an incremental one only reports
the files it recompiled.

The optimized LTO build now passes without the former
`-fno-strict-aliasing` workaround. Its quick-test center table and significant
wave-height block are byte-identical to the baseline and ordinary Release
builds. Serial, OpenMP, MPI, LTO, netCDF and timing-instrumented builds pass
their registered tests; the MPI smoke test runs on two processes.

These checks demonstrate compatibility for the included regression cases. They
are not a claim that compiler diagnostics are already clean or that every
production scenario is covered.

## Remaining migration boundary

The large collections of external procedures have since been split into
subsystem modules: source terms (`swan_wind_source`, `swan_dissipation`,
`swan_nonlinear_interactions`, `swan_propagation`), computation
(`swan_computation`), input (`swan_command_reading`, `swan_input_processing`),
output (`swan_output_orchestration`, `swan_output_writers`), services
(`swan_services`), parallel synchronization (`swan_parallel`) and the driver
(`swan_driver`). Each exports only the entry points its callers use, so a large
number of previously global symbols are now implementation detail.

The `MSGERR` cycle has since been broken. `swan_parallel_state` is a new
dependency-free module holding `MASTER`, `INODE`, `NPROC`, `IAMMASTER` and
`PARLL`; `M_PARALL` uses and re-exports it. With the flags reachable without
`M_PARALL`, the diagnostic services could move out of `ocpmix.f90` into
`swan_service_interfaces` itself: `STRACE`, `MSGERR`, `STPNOW`, `EQREAL`,
`EQDBLE`, `TABHED` and `BUGFIX` are module procedures now, so their roughly 76
callers are checked against the real implementation without a single caller
having to change.

What deliberately stays external:

- `REPARM` and `LSPLIT` in `ocpmix.f90`. They need the command parser, which
  uses `swan_service_interfaces`, so moving them would recreate a cycle.
- `DTSTTI` and `DTTIST` in `ocpids.f90`. `swan_time` declares them, but they
  call `UPCASE` from `swan_input_parser`, which uses `swan_time`.
- `TXPBLA`, kept in the interface block next to the switch-activated timing
  routines it shares a file with.

The same applies to the switch-activated timing (`!TIMG`) and Matlab-binary
(`!MatL4`) routines, which are called as externals from many files.

The standalone `Swan*.f90` files (`SwanFindPoint`, `SwanReadGrid`,
`SwanVertlist` and 34 others) are now modules too, named after the file in
snake_case. Their `CONTAINS`-ed helpers became genuinely internal, and callers
import them with an `ONLY` list at module level.

Long-lived mutable data modules also remain extensive. The time and
command-reader contexts establish the migration pattern, but the default
singletons intentionally preserve compatibility for the current top-level
driver. The parser's input unit and diagnostic streams also still come from
the shared Ocean Pack runtime, so a reader is state-isolated but not yet a
self-contained I/O object. Further contexts should be introduced subsystem by
subsystem with dedicated numerical fixtures. Until those steps are complete,
the accurate description is “standard Fortran 2018 with a substantially
modernized and tested interface layer,” not “everything is modern Fortran.”
