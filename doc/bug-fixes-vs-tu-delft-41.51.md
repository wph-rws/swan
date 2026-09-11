# Bug fixes relative to TU Delft SWAN 41.51

## Basis of comparison

This list compares the RWS fork with TU Delft's `upstream/main`:

- repository: [`citg/wavemodels/swan`](https://gitlab.tudelft.nl/citg/wavemodels/swan);
- upstream commit: [`43e9bbba393f2cf9eaff78bc92eaed118a3a5c8e`](https://gitlab.tudelft.nl/citg/wavemodels/swan/-/commit/43e9bbba393f2cf9eaff78bc92eaed118a3a5c8e), 13 July 2026;
- last source patch in it: `0dabfa4` (`updated src (patch B)`), also
  13 July 2026;
- version number on both sides: **41.51** (in this fork the parameter
  `SWAN_VERSION_NUMBER` in [src/swanmain.f90](../src/swanmain.f90)).

The merge base of the two lines is `43e9bbb`. The items below are therefore
local fixes on top of the same 41.51 source base, not differences between two
SWAN releases. File names refer to the modernised free-form source in this
fork; the TU Delft source is still entirely based on the `.ftn` form
(18 `.ftn` + 57 `.ftn90` under `src/`, not a single `.f90`) and is
preprocessed by `switch.pl` before compiling.

Which decks actually compute differently from that source base is not only
described here in prose, but also recorded in machine-readable form in
[upstream-delta.json](upstream-delta.json): per deck `identical`, `differs` with
the BF number, `fork only`, or `skipped` with a reason.
[scripts/upstream_delta.py](../scripts/upstream_delta.py) runs the decks
through the pinned upstream binary and through the fork and checks that claim;
[tests/test_upstream_delta.py](../tests/test_upstream_delta.py) guards the
inventory at every commit and runs the measured half under
`SWAN_UPSTREAM_DELTA=1`. A deck that starts to differ without a BF reference
therefore turns a gate red, instead of being discovered afterwards.

The qualifications mean:

- **proven**: the defect was reproduced before the fix and was gone after the
  fix, or was shown with a memory tool;
- **proven with a tool, consequence latent**: a memory or runtime check
  points to the defect, but on the decks tried the result did not change;
- **proven from the source**: the bug follows directly from the upstream data
  flow or a compiler diagnostic and the fix has targeted coverage, but there
  is no separate before/after measurement of a consequence a user would see;
- **latent**: the TU Delft code breaks a language, interface or internal
  contract, but the normal path that is covered did not dereference the wrong
  value;
- **diagnostics/build**: the computation has not changed, but a missing
  warning, error handling or build route has been fixed.

Combined qualifications occur where the defect itself is established but no
consequence appeared on the decks tried. The evidence column therefore always
separates what was **measured** from what was **derived** from the source;
where there is no before/after measurement of the numerical consequence, none
is claimed.

## Bug fixes delivered

The items are grouped by what a user notices. The numbers were assigned in
the order the bugs were found, so they are not in sequence.

### A. Valid input gave a wrong result, a crash or an unjustified message

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-01 | **proven** | **A second computation in the same process could crash or misread input.** `LOGCOM(6)` is set to `.TRUE.` as soon as `AC2` has been allocated (`swanpre1.ftn:4990`) and is never reset, while `AC2` is freed at shutdown (`swanmain.ftn:9243`); a following run therefore saw an array that no longer existed. The input file name remembered by `REPARM` also stayed in place between runs. The duplicate allocation status has been replaced by `ALLOCATED(AC2)` and the file registration is reset at every run. The owner `clear` paths have also been made idempotent. Reachable through a coupler or the library API, not in the standalone binary. | The A–B–A tests for equal cases, changing physics, grids and spectra are bit-identical. See `tests/test_two_cases.f90`, `tests/test_physics_reuse.f90`, `tests/test_grid_reuse.f90` and `tests/test_spectrum_reuse.f90`. |

### B. Wrong input gave a crash, a hang or a silent computation instead of a message

A deck with an error should stop with a message that names the error. In these cases SWAN crashed, hung, silently carried on computing with unset values, or ended normally without computing.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|

### C. Latent, diagnostics and build

Here the code broke a language or interface rule, or a warning was missing, without a wrong result being measured on the decks tried.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|

### D. Changed, but no bug shown

These items change the code relative to upstream, but no defect was reproduced. They keep their number so that references to it still work.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|


## Not counted as bug fixes

This list is only about bugs in the TU Delft version. Deliberately left out
are:

- the conversion to free-form Fortran, modules and explicit state ownership;
- the replacement of FFTPACK by FFTW3, the spatial index and other
  performance improvements;
- the extension of CMake, CI, tests, examples and verification tools, as far
  as no upstream bug was fixed in the process;
- removed dead code and compiler warnings without a demonstrated bug in
  behaviour;
- **the ESMF branch of `SWIND3`, `SWIND4` and `SWIND5`.** That branch refers to
  `KCGRD(1)`, and that looks like a reference to an argument that has been
  gone from these routines since SWAN 40.00. It is not: `KCGRD` is a module
  variable of `SWCOMM3` (`swmod1.ftn:2078`, `THREADPRIVATE`), all three
  routines do `USE SWCOMM3`, and the value is the current grid point. The
  branch was therefore valid in the TU Delft source and pointed to the right
  point. This fork replaced `KCGRD(1)` in these routines with an explicit
  `IGP` argument, so the dormant branch had to move along in the fork's own
  tree; in doing so it was made buildable as an ordinary CMake capability and
  covered by a backend test (`src/coupling/`,
  `tests/test_esmf_coupling_backend.f90`). That is modernisation maintenance,
  not a fix of upstream;
- the changed default physics since SWAN 41.45. That is a deliberate
  upstream change. `GEN3 KOMEN DRAG FIT` is a compatibility setting to
  approximate the 41.31 physics, not a bug fix in 41.51.
