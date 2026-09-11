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

Two fixes (BF-01 and BF-05) only bite when one process runs more than one
computation. The standalone 41.51 binary runs one case per process; the way
to get there is an external coupler — the upstream source refers, in the
comment at `SWMAIN`, to `couple2swan.F`, which is not included — or the
library API of this fork. That does not make them any less real, but it does
decide who is affected.

## Bug fixes delivered

The items are grouped by what a user notices. The numbers were assigned in
the order the bugs were found, so they are not in sequence.

### A. Valid input gave a wrong result, a crash or an unjustified message

These are the bugs a user with a correct deck could hit. BF-01 and BF-05 only apply when one process runs more than one computation.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-01 | **proven** | **A second computation in the same process could crash or misread input.** `LOGCOM(6)` is set to `.TRUE.` as soon as `AC2` has been allocated (`swanpre1.ftn:4990`) and is never reset, while `AC2` is freed at shutdown (`swanmain.ftn:9243`); a following run therefore saw an array that no longer existed. The input file name remembered by `REPARM` also stayed in place between runs. The duplicate allocation status has been replaced by `ALLOCATED(AC2)` and the file registration is reset at every run. The owner `clear` paths have also been made idempotent. Reachable through a coupler or the library API, not in the standalone binary. | The A–B–A tests for equal cases, changing physics, grids and spectra are bit-identical. See `tests/test_two_cases.f90`, `tests/test_physics_reuse.f90`, `tests/test_grid_reuse.f90` and `tests/test_spectrum_reuse.f90`. |
| BF-02 | **proven** | **Mixed netCDF output could end with an allocator error after correct output.** Two simultaneous `BLOCK` requests for the same grid, one text and one netCDF, passed with `CALL FOR (NREF, TRIM(FILENM)//'.dum', 'UF', IOSTAT)` a short string result to an interface that declares the argument as `CHARACTER DDNAME*(LENFNM)` (140 characters). Both netCDF calls now use a local buffer of the required length; the same has been done for the table route. | `netcdf_mixed_block_output` reproduces the former crash; `netcdf_table_output` covers the second call. Valgrind no longer reports an invalid read. See `src/swanout2.f90`, `examples/output_backends/run.py` and `tests/check_netcdf_output.f90`. |
| BF-05 | **proven** | **Linked parser, boundary, obstacle and output lists leaked memory.** Single head nodes were never freed (`OPSTMP` is allocated eight times in `swanpre2.ftn` and freed nowhere), and of chains only the last node was freed (`IF (ASSOCIATED(TMP)) DEALLOCATE(TMP)`). The fix covers `BFLTMP`/`BSTMP`/`BGPTMP`, `ORQTMP`/`OPSTMP`, `NEXTI`, several `XYPT` chains and the obstacle transmission and corner point chains. Five error paths after `BCWAMN`/`BCWW3N`/`BCFILE` now also clean up their node that is not yet linked and any point chain. Relevant where one process does more than one run; in a process that runs one case, the operating system cleans up at exit. | On the A–B–A case, definitely/indirectly lost fell cumulatively from **2040 B to 0 B** (216 + 792 + 936 + 96 B, measured at the four checkpoints of the test). The separate FILE error decks fell from 280 B and 328 B total loss respectively to 0 B. The shutdown protocol with 50 changing runs, an input error and recovery passes without lost blocks or growth in open units (`scripts/lifetime_protocol.py`). |
| BF-09 | **proven from the source** | **Reading the same input field again on a larger grid wrote past the end of the array.** Upstream allocates once (`IF (.NOT.ALLOCATED(DEPTH)) ALLOCATE(DEPTH(MXG*MYG))`, `swanpre1.ftn:4484 ff.`) and then copies with `CALL SWCOPR (TARR, DEPTH, MXG*MYG)`, in which the target is declared as `REAL ARR2(LENGTH)`. A second `READINP` for the same field on a larger grid size silently kept the first size and wrote the difference past the end. `ENSURE_FIELD_SIZE` now resizes the field before filling it. | The upstream data flow shows the too-small target array directly; a separate before/after deck measurement is missing. The targeted test enlarges `DEPTH` from two to five elements and guards the new extent. That test pins the resizing primitive itself, not the route to it: if `SREDEP` ever stopped calling `ENSURE_FIELD_SIZE`, this would go unnoticed — a coverage gap at path level that remains open. See `src/swmod2.f90` (`ENSURE_FIELD_SIZE`), the calls in `src/swanpre1.f90` and `tests/test_state_clear.f90`. |

### B. Wrong input gave a crash, a hang or a silent computation instead of a message

A deck with an error should stop with a message that names the error. In these cases SWAN crashed, hung, silently carried on computing with unset values, or ended normally without computing.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-10 | **proven** | **A surplus length in a variable boundary segment displaced the following boundary conditions.** Upstream links a duplicate grid-point node when the next length lies beyond the end of the segment. With one segment the duplicate is beyond the part copied by `SWPREP`; with another segment it replaces a genuine node. The fork consumes surplus parameters without adding that node. | On two segments, the last west point gave Hs 1.992 m instead of 1.872 m, and the last south point 0.851 m instead of 1.676 m. The fork gives the latter values with and without the surplus length. `test_surplus_boundary_lengths_do_not_change_the_next_segment` guards this; a single-segment comparison alone cannot detect it. See `src/swanpre2.f90` (`SWBOUN`), upstream `swanmain.ftn:4015-4035`. |

### C. Latent, diagnostics and build

Here the code broke a language or interface rule, or a warning was missing, without a wrong result being measured on the decks tried.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-03 | **proven** | **`INTSTR` changed the caller's integer argument while formatting.** The conversion now works on a local copy and the argument is `INTENT(IN)`. | A leaf-module fixture guards that the argument stays unchanged. See `src/swan_number_formatting.f90` and `tests/test_leaf_modules.f90`. |
| BF-04 | **latent** | **Several unstructured calls had invalid dummy argument ranks or shapes.** For `PLTSRC`, `SPREDT`, `SWTRCF`, `STRSSB`, `PEREXC`, `SWBIDW` and `BRKPAR`, scalars were passed as a rank-2 placeholder, or arrays with two elements to a dummy `RDX(MICMAX)`/`RDY(MICMAX)` — with `MICMAX = 13` as a parameter, while `SwanCompUnstruc` passes `real, dimension(2) :: rdx`. Unused placeholders have been made `OPTIONAL`, and the two-element windows that are actually used have a matching assumed-size interface. | The routines did not read beyond the second index, so no wrong value was dereferenced: **latent**. The explicit module interfaces make the old calls invalid at compile time. The regular and unstructured non-stationary references stayed bit-identical. See also `doc/modern-fortran.md`. |
| BF-08 | **proven with a tool, consequence latent** | **25 optional allocatable arrays were passed as arguments while unallocated.** Input fields are only allocated once a `READINP` command supplies them, and the global grid arrays only for a structured grid, but they are passed unconditionally: whether the data exists is decided by flags such as `LEDS`, never by `ALLOCATED`. That is undefined behaviour in Fortran and stops under `-fcheck=all`. `SWINIT` now gives the 20 fields, the four global grid arrays and `XYTST` a valid empty state. | The full runtime check suite is green and was tested negatively by temporarily removing one empty initialisation. The defect can therefore be shown with a tool; the consequence stayed latent because the path called always tested the flag first and never read the array. For anyone adopting this fix: a reader that only assigns a field if it does not exist yet will find an empty array after `SWINIT` and must then also check the size. See `src/swanmain.f90` and `tests/test_state_clear.f90`. |
| BF-11 | **proven, latent for current production decks** | **`ININTV` and `INITVD` could write an uninitialised helper value to the caller on the `UNC`/`RQI` no-value path.** Upstream calls `INREAL (NAME, RI, KONT, RSTA)` and then unconditionally computes `RVAR = FAC * RI`, while `RI` is never set on that path. `RI` is now seeded with the existing `RVAR`, exactly in line with the contract "do not change the value". | The new contract test was red before the fix because of real stack garbage and is green after it; the two `-Wuninitialized` warnings have disappeared. All current production callers use `STA` or `REQ` and stay bit-identical. See `src/ocpcre.f90`, `tests/test_contexts.f90`. |

### D. Changed, but no bug shown

These items change the code relative to upstream, but no defect was reproduced. They keep their number so that references to it still work.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-06 | **latent/interface** | **The METIS C calls had no explicit interoperable interface and no check on the declared ABI widths.** Upstream declares them as `integer(kind=kint), external :: METIS_PartGraphKway` and relies on `kint` matching `idx_t` from the library. They now go through `ISO_C_BINDING`/`BIND(C)`. When `metis.h` is available at the include location found, CMake checks whether that header declares 32-bit `idx_t` and 32-bit `real_t`; other declared widths stop the configuration. This does not by itself prove that a library found separately was built with exactly that header. | The 32/32 header/library combination actually found, argument passing, index ranges and real partitioning on two ranks have been tested. A 64-bit index library and a deliberately mismatched header/library combination were not available, and are therefore explicitly not qualified. See `src/swan_metis_interface.f90`, `src/SwanParallel.f90` and `tests/test_metis_backend.f90`. |
| BF-07 | **build** | **The Windows CMake route always passed the `CVIS` switch for GNU Fortran** (`if( WIN32 ) set( SWITCHES -dos -cvis )` in `src/CMakeLists.txt`, without a compiler check). That switch activates the non-standard `SHARED` specifier in the `OPEN` statements and makes a standard-conforming GNU build impossible, while the GNU runtime does not need it for Windows file sharing. The standard-conforming file backend is now the default; the preserved DEC `SHARED` backend is only chosen through the explicit capability option `SWAN_DEC_SHARED_IO`, and no longer automatically on the basis of Windows or “not GNU”. | Backend selection and switch compatibility have been tested statically. No DEC-capable Windows compiler was available; an actual build of the optional `SHARED` backend is therefore not qualified. See `CMakeLists.txt`, `src/platform/`, `tests/test_switch_compatibility.py` and `doc/switch-manifest.json`. |


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
