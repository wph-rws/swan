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
- Diffraction, triad and quadruplet lookup state now have separate owners:
  `diffraction_state_t`, `triad_state_t` and `snl4_tables_t`. The driver passes
  those objects explicitly; the former `M_DIFFR`, `M_SNL3` and `M_SNL4`
  storage modules no longer exist.
- The shared `SIGPOW` array is `spectral_powers_t`. Its two former writers,
  ordinary spectral-grid setup and IEM reinitialization, use one `rebuild`
  contract and cleanup uses `clear`.
- Whitecapping's eleven formerly `THREADPRIVATE` scalars are held in a
  `wcap_workspace_t` per structured or unstructured solver thread. The solver
  boundary chooses the active workspace; source-term kernels do not query the
  OpenMP runtime. The historical carry-over semantics of the seven
  conditionally written fields are pinned by bitwise invariant tests.
- The `KSCIP1` and `KSCIP2` dispersion APIs make secondary results optional.
  Callers that only need a wave number no longer pass the same work array as
  several output arguments. This removes a Fortran aliasing violation and
  avoids unused array writes.
- `KSCIP1`, `GAMMAF` and `TCROSS` retain their traced legacy entry points but
  delegate their mathematics to directly tested pure kernels. `GAMMAF`'s
  scalar kernel is elemental.
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
| All warnings | 4,443 | 1,508 | 1,534 | 65% |
| Implicit-interface warnings | 3,229 | 288 | 2 | 99.9% |

The "interface layer" column is the state after explicit interfaces were added
but before the monolithic source files became modules; "current" is after that
subsystem split, which also turned the standalone `Swan*.f90` files into
modules and moved the diagnostic services into `swan_service_interfaces`. The
total warning count rose slightly, which is expected rather than a regression:
`-Wconversion-extra` and the argument diagnostics can only fire at a call site
once the compiler knows the dummy argument types, so making several thousand
calls checkable exposes conversions that were previously invisible. The
categories behind the current total are dominated by `-Wconversion-extra`
(493), `-Wunused-variable` (259) and `-Wcompare-reals` (226).

Two implicit-interface call sites remain, both `METIS_*` calls into the external
C library, which can never be Fortran interfaces. That is the floor.
`scripts/strict_diagnostics.py` enforces it. Measure with a *clean* build — an
incremental one only reports the files it recompiled.

### What the remaining warnings are, and where not to start

The 1,534 that remain are dominated by `-Wconversion-extra` (493),
`-Wunused-variable` (259), `-Wcompare-reals` (226), `-Wfunction-elimination`
(163) and `-Wmaybe-uninitialized` (143).

A sample of five `-Wmaybe-uninitialized` clusters was checked against the code
to judge whether the category is worth working through:

| Site | Verdict |
|---|---|
| `SwanGridCell` `vn`/`vp` | False positive — the cell is attached to the vertex by construction, so the search loop always hits. |
| `swancom4` `MI4S` and friends | **Real defect**, fixed: an empty `MDIA LAMBDA` list left the spectral range assigned from uninitialised locals. Guarded in the parser. |
| `swancom2` `C0`–`C5` | False positive — set and used under the same `IICE == 3` branch. |
| `swanser` `IXMIN`/`IYMIN` | Reachable only if the entire computational grid has no active point; degenerate rather than live. |
| descriptor temporaries (`*.dim[0].ubound`, `*.offset`) | Artefacts of optimised array descriptors, not source-level defects. |

The yield is low but not zero. Two lessons for anyone continuing: prefer reports
about *named locals* over descriptor temporaries, and resist silencing them with
an initialiser — that hides the next real one instead of surfacing it.

`-Wunused-variable` looks like the easier target and is not:

- More than half sits in vendored numerics, `mod_xnl4v5.f90` (95) and
  `SdsBabanin.f90` (43). Editing those buys a lower count and pays for it in
  merge friction on the next import from Delft.
- **Switch variants hide real uses.** GNU Fortran only sees the variant it
  compiles. `swanparll.f90` carries 1,671 switch-prefixed lines and
  `swancom1.f90` 378, so a declaration that looks unused in the serial build may
  well be used under `!MPI`, `!JAC`, `!TIMG` or `!MatL4`. Removing it breaks a
  configuration nothing builds by default.

The safe order is therefore: leave the vendored files alone, and check every
other candidate against the switch-prefixed lines in the *raw* source before
removing it — the generated source has already dropped the inactive variants.

The same blind spot cuts the other way when a procedure moves into a module.
Its `USE` line is then visible to *every* variant, while the calls to it stay
behind their own switch prefix — and a default build compiles clean either way.
So the rule is: **an import used only from a switch-prefixed call site carries
that same prefix**. `SWRECVAC` and `SWSENDAC` do not exist under `!JAC`,
`SWSYNC` is only called under `!JAC`, and the `!MatL4` and `!JAC` bodies need
`INTSTR` while the default ones do not; each of those imports therefore sits
behind the prefix of the variant that uses it. The only way to know is to build
the variant, which is why every one of them has a registered test.

## Guarding the result

Two mechanisms keep this from eroding, both of which were checked in each
direction before being relied on — a gate that cannot fail is worse than no
gate.

- **The results themselves are pinned.** The quick test and both nonstationary
  cases compare their table and block output against references generated from
  a build of 23 July 2026, before any of this work. The current build reproduces all
  six files. Text has to match exactly apart from trailing blanks, which the MPI
  output path strips; numbers are compared to a relative tolerance of 1e-3.
  NaN and infinity parse as floats but fail the comparison outright: they carry
  nothing a tolerance can act on, and NaN would otherwise pass by comparing
  unequal to everything. See `examples/reference_check.py`.
- **The diagnostic inventory cannot grow.** `scripts/strict_diagnostics.py`
  builds clean with the strict warning set and fails when any category exceeds
  its recorded budget. A category the budget has never seen has an implicit
  budget of zero, so an entirely new kind of warning fails too. Shrinking a
  category is a deliberate act ending in `--update-budget`.

### Why the numeric comparison is not bit-exact

The unstructured solver is **not bit-reproducible under OpenMP**. The thread
count determines the order of summation, so the same binary can print a
different last digit from one run to the next — a spread of about 2e-4
relative, in the unstructured case only; the regular grid is stable.

This predates the modernization: a build of 23 July 2026 behaves identically, so it
is a property of the solver rather than a consequence of the restructuring. It
does mean two runs cannot be compared bit for bit to prove a change was inert.
Do that serially or with MPI, which are reproducible.

## Behaviour preservation

The optimized LTO build now passes without the former
`-fno-strict-aliasing` workaround. Its quick-test center table and significant
wave-height block are byte-identical to the baseline and ordinary Release
builds. Every build option the project offers passes its registered tests:
serial, OpenMP, MPI (also on two processes), MPI+netCDF, netCDF, LTO, TIMG,
METIS, MATL4, JAC, FFRO, the debug-invariant build and `SWAN_RUNTIME_CHECKS`.

Two of those options change the answer rather than the code path around it, and
they have their own stored references instead of being exempt from comparison:

| Option | What changes | Where it shows |
|---|---|---|
| `JAC` | Jacobi subdomain exchange instead of following the wavefront across them | only on more than one MPI process |
| `FFRO` | fixed-front (`!FXFRO`) vertex ordering instead of the graph-coloured one | the unstructured case |

Both were checked against binaries built from the pre-modernization baseline in
the same configuration and are byte-identical to them, so the difference is the
scheme and not this work.

These checks demonstrate compatibility for the included regression cases. They
are not a claim that compiler diagnostics are already clean or that every
production scenario is covered.

### Runtime checks

`-DSWAN_RUNTIME_CHECKS=ON` adds the compiler's own bounds, allocation and
argument checks. Reaching a state where they pass took one fix, on a defect the
pre-modernization baseline has as well: 25 arrays that could be passed on while
still unallocated.

Input fields are only allocated once a `READINP` command supplies them, and the
global grid arrays only when the grid is structured; both are passed on
unconditionally, because whether the data exists is decided by flags like `LEDS`,
never by `ALLOCATED`. Any deck that left one out therefore passed an unallocated
allocatable — undefined behaviour that happens to work, because the callee guards
on the flag and never dereferences.

`SWINIT` now gives all of them the empty state. That by itself would be worse
than the disease: the allocations that fill them were guarded by
`IF (.NOT.ALLOCATED(..))`, so an array pre-allocated at size zero would *stay* at
size zero and the fill would write past its end. Sizing is what makes the empty
state safe, so they go through `ENSURE_FIELD_SIZE` in `M_GENARR`, which grows the
array to the size asked for. That also closes a latent bug in the old guard:
reading the same grid again at a different size silently kept the first one.

One deliberate exception, marked in the source: the `TEST` branch that falls back
to an empty `XYTST` still only allocates when unallocated, because `NPTST` keeps
its count from an earlier `TEST POI` and a resize there would leave the two
disagreeing.

Use it on top of an optimizing build type. At `-O0` the nonstationary regular
case drifts about 1.2e-3 in the mean period against a 1e-3 tolerance, which is
the optimization level's floating point rather than a defect — `-O2` reproduces
the references exactly. The tolerance was deliberately left alone: widening it
to accommodate a build nobody ships would weaken the guard for every build that
does.

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

The parser cycle has since been broken as well. `UPCASE` moved to the leaf
module `swan_text_utilities`, which removed `DTSTTI`'s dependency on the parser;
`DTSTTI` and `DTTIST` then became module procedures of `swan_time`, and `REPARM`
and `LSPLIT` moved into `swan_input_helpers`, which sits above the parser rather
than below it. That file holds only `swan_input_helpers` now, and is named
after it.

What deliberately stays external: `TXPBLA`, kept in the interface block next to
the switch-activated timing routines it shares a file with.

The same applies to the switch-activated timing (`!TIMG`) and Matlab-binary
(`!MatL4`) routines, which are called as externals from many files.

The standalone `Swan*.f90` files (`SwanFindPoint`, `SwanReadGrid`,
`SwanVertlist` and 34 others) are now modules too, named after the file in
snake_case. Their `CONTAINS`-ed helpers became genuinely internal, and callers
import them with an `ONLY` list at module level.

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

Long-lived mutable data modules are still extensive. The contexts cover the
clock, command parser, I/O streams, diagnostic status, diffraction, nonlinear
interaction tables, spectral powers and the whitecapping thread-workspace. A
reader can own its input file, its log and its error state, and the file opener
draws from its unit range. Grid dimensions, most physics settings and output
request tables remain in the broad `SWCOMM*`/`OCPCOMM*` families, so SWAN still
runs one case per process.

The accurate description is therefore “standard Fortran 2018 with a modular,
compiler-checked interface layer and explicit ownership for the migrated
subsystems over a still partly shared case state,” not “everything is modern
Fortran.”
