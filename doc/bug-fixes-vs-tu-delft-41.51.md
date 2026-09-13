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
| BF-15 | **proven** | **The fronts of the unstructured solver were ordered on only one of the two stencil neighbours; this affected OpenMP and the serial computation.** In the `!GRAPH` block of `SwanVertlist.ftn90`, both neighbours of the vertex are computed (`vu(1)` and `vu(2)`, upstream lines 266-267), but the level assignment uses only one of them, with `m = vu(1)` — comment "pick first neighbour vertex", line 274. The transport reads `ac2` from both neighbours and writes to the centre, so a vertex with an open fan (mesh edge, coastline, obstacle cut) could end up in the same front as a neighbour it reads while another thread still has to update that neighbour. The result therefore depended on the number of threads and on the scheduling. The missing edge is now included: the loop counts both neighbours. On decks with wind a second, unrelated race also made the result depend on the threads; see BF-17. | The mechanism was first shown on a synthetic mesh and then measured on the non-stationary unstructured case: 237 of the 1617 values in the block file shift, at most 11.4% relative to the fork (12.9% relative to upstream) at a single point, while the table of the centre point stays unchanged. The stored reference holds the result with the fix. `tests/test_front_dependency_completeness.f90` guards the completeness of the dependencies. The FFRO variant uses fixed fronts in the sort order and is not affected; its reference is unchanged. **The serial route also shifts**, because the front layout decides the order in which the sweep updates the nodes: with only `vu(1)`, upstream could update a node before a neighbour it reads, and then read that neighbour's value from the previous sweep. Measured with the same compiler on both sides, `-O3`): the fork with only `vu(1)` is byte-identical to upstream on `nonstationary_unstructured`, and the fork with both neighbours is byte-identical to upstream built with `FFRO`. The difference from upstream on that deck is therefore entirely this fix. See `src/propagation/swan_front_scheduling_backend_graph.f90`. |
| BF-16 | **proven** | **Without depth-induced breaking, the breaker fraction was arbitrary, and so was the limiter.** With `OFF BREAKING`, `SINTGRL` sets the maximum wave height to 100 m, so that `FRABRE` should give a breaker fraction of zero. However, the directional factor `KTETA` that `FRABRE` divides by was only set in `BRKPAR`, so only when breaking is on; otherwise it held a leftover from the stack. In practice that gave `Qb = 1` everywhere, and because `PHILIM` turns off the limiter where waves break, every run without breaking ran without a limiter. `KTETA` is now 1 when breaking is off, the value that `BRKPAR` itself uses as default. | Valgrind on upstream 41.51 (`-O3 -g`) with `quad_dia2`: 16,000 reports "Conditional jump depends on uninitialised value" in `FRABRE`, value created on the stack of `SWOMPU`; on the fixed fork zero reports. This was also the cause of the changing answers from upstream without a fixed memory layout on `quad_dia2`, `quad_mdia` and `src_nobreak`; the fork happened to have a fixed stack leftover. The smoke deck with `OFF BREAKING` reported `Qb = 1.000000`, now 0. `quad_dia2` without under-relaxation ran to the limit of 100 iterations and got stuck at 83% (Hs at 100 km 2.771 m); now it converges in 28 iterations to 3.000 m. The "limit cycle" of `QUADRUPL 1` without under-relaxation, long listed as a stiff option and a candidate for reporting, was also this defect: `quad_dia1` without `ALFA` now converges at 5, 10, 15 and 25 m/s in 9 to 32 iterations, while upstream stays stuck at 29 to 76% after 300 iterations. Some decks without breaking therefore differ from upstream; `quad_off` remains identical because its limiter is disabled with the quadruplets, with this number in [upstream-delta.json](upstream-delta.json). Test `tests/test_breaking_off.py` (ctest `breaking_off`). See `src/swancom1.f90`. |
| BF-17 | **proven** | **On an unstructured grid under OpenMP, a run with wind gave a different result on almost every run with more than one thread.** In a stationary third-generation run SWAN computes the first iteration with its second-generation model as a first guess (`IWIND = 2`). That model writes the energy of the wind sea, `ETOTW`, in `WINDP2` and reads it back in `WNDPAR`. In the OpenMP loop of `SwanCompUnstruc`, upstream passes the local variable `dummy` for that argument (and for `AC2TOT` of `SINTGRL`), and `dummy` is shared by all threads, so one thread could read the energy that another thread had just written for a different vertex. In the same loop the threads test `accur` and `stopcr` to leave the iteration loop right after the master thread has set them, without a barrier in between, so a thread could decide on the previous value. `dummy` is now private to each thread, and both tests come after a barrier. | Found on an unstructured Voordelta mesh of 4,686 vertices with the deck of the CI smoke test (wind, `GEN3 KOMEN`): two to four threads gave a different Hsig field on almost every run, up to 2.8 cm (4%) at interior points, while one thread was reproducible. Without wind, or with `GEN1`, the runs were reproducible, which pointed to the second-generation first guess; ThreadSanitizer then showed two threads writing the same address in `WINDP2`. After the fix, 32 runs per mesh on meshes of 783 and about 18,500 vertices, with 1, 2, 4 and 22 threads, idle and under load, are bit-identical to the serial run. The barrier is derived from the code; no run was seen to leave the loop at a different iteration. `scripts/unstruc_determinism_smoke.py` runs the case on the 1000 m mesh and fails without the fix. Upstream identical (`43e9bbb:src/SwanCompUnstruc.ftn90:1020` and the `SOURCE` call). See `src/SwanCompUnstruc.f90`. |
| BF-24 | **proven** | **`COMPUTE STATIONARY` gave an unjustified error message in a stationary run, depending on the compiler.** `SWREAD` tests `IF (NSTATM.LE.0 .OR. KEYWIS('STAT'))` (since 40.00, `43e9bbb:src/swanpre1.ftn:879`). In a stationary run the left-hand side is already true; Fortran then allows the compiler not to evaluate the right-hand side, and gfortran does skip it. When it finds a match, `KEYWIS` marks the keyword as used; without that call `STAT` is left over and the next command reports `Illegal keyword: STATIONA`. The run does carry on, with the same output. `KEYWIS` is now called separately beforehand. | Reproduced on `examples/quick_test` with `COMPUTE STAT` and `COMPUTE STATIONARY`: error message in the fork, in the upstream build (41.51AB, gfortran) and in a gfortran build of 41.31; no message in the Intel build RWS 41.31A.1. After the fix no message, table and block byte-identical. Found through the strict warning `function-elimination` (`Impure function ‘keywis_default’ might not be evaluated`). Of the other reviewed warnings of this kind, 40 are synonyms `KEYWIS('A') .OR. KEYWIS('B')`, where skipping makes no difference; the other ten (`VALIDBP` in `CGBOUN`, `INMESH` in `CVMESH`) are also harmless, for the reasons in the assessment register. Test `test_compute_stationary_is_valid` in `tests/test_input_validation.py` fails without the fix. See `src/swanpre1.f90`, `doc/strict-warnings.md`. |

### B. Wrong input gave a crash, a hang or a silent computation instead of a message

A deck with an error should stop with a message that names the error. In these cases SWAN crashed, hung, silently carried on computing with unset values, or ended normally without computing.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-10 | **proven** | **A surplus length in a variable boundary segment displaced the following boundary conditions.** Upstream links a duplicate grid-point node when the next length lies beyond the end of the segment. With one segment the duplicate is beyond the part copied by `SWPREP`; with another segment it replaces a genuine node. The fork consumes surplus parameters without adding that node. | On two segments, the last west point gave Hs 1.992 m instead of 1.872 m, and the last south point 0.851 m instead of 1.676 m. The fork gives the latter values with and without the surplus length. `test_surplus_boundary_lengths_do_not_change_the_next_segment` guards this; a single-segment comparison alone cannot detect it. See `src/swanpre2.f90` (`SWBOUN`), upstream `swanmain.ftn:4015-4035`. |
| BF-19 | **proven** | **An empty MDIA lambda list silently removed all quadruplet interactions; omitting `LAMBDA` could crash.** Upstream executes `DO IDIA=1,MDIA` zero times for an empty list (`swancom1.ftn:7674`). With no `LAMBDA` keyword it frees the lambda array but retains the old count. The parser now requires a non-empty lambda list and a `CNL4` or `CNL4_12` coefficient list before using the arrays. The scratch list is bounded as well. | Upstream gives Hs 0.87735 m with an empty list, the same as no quadruplet transfer, against 0.87899 m with default MDIA. `MDIA CNL4 1.0` after `QUADRUPLET 4` segfaults with `MAXERR=2`. The tests in `tests/test_input_validation.py` cover missing and empty lambda lists, missing coefficients and a valid MDIA command. `SWPRE4W` is a fork routine; it is not the mechanism of the upstream empty-list defect. |
| BF-23 | **proven** | **An unknown boundary-file header left the caller using unset file characteristics and a spectrum that did not exist.** `BCFILE` now closes the file and reports a terminating error naming it, before the caller can link a partial description. This follows the other unreadable-boundary-file paths. A level-3 message alone is insufficient when the deck accepts severe errors. | `test_boundary_file_with_unknown_header_stops_the_run` covers SIDE, SEGMENT and BOUNDNEST1, with default `MAXERR` and with `MAXERR=3`. Each requires exit code 1, no output table and no `norm_end`. A valid boundary file retains its output. The lifetime-fault test does not exercise this path. See `src/swanpre2.f90` (`BCFILE`). |
| BF-12 | **proven** | **An unstructured MPI run without METIS crashed with a segmentation fault after a message that was already fatal.** After `MSGERR(4)` a stop check was missing, after which the unpartitioned `ivertg` was dereferenced in that same branch (`swanpre1.ftn:1532-1533` for the `MSGERR(4)`, `1550` for `I = ivertg(J)`). An `IF (STPNOW()) RETURN` now ends the path as a normal SWAN error. | On two ranks the outcome changed from `SIGSEGV` to exit code 1 with the intended message and without `norm_end`; the same case with METIS stays green. See `src/swanpre1.f90` and `tests/test_mpi_unstructured_partition.py`. |
| BF-22 | **proven from the source** | **`SREDEP` indexed its parameter tables with an unassigned index after an unknown quantity in `READINP`.** The chain of field keywords ends with `ELSE / CALL WRNKEY`, after which `IGR1` stays undefined; the very next line is `CALL INREAL ('FAC', IFLFAC(IGR1), 'STA', 1.)`, followed by eleven more `IFLxxx(IGR1)` accesses. That is a read and, as soon as the deck also gives `[fac]`, a write outside the table bounds. `MSGERR` does not stop execution itself — only `STPNOW()` at level ≥ 4 does that, and `WRNKEY` gives level 2 — so the routine simply carried on. It now returns as soon as no branch matched. | The strict build confirms it independently: the warning `swanpre1.f90: 'igr1' may be used uninitialized` has disappeared from the baseline after the fix. It affects a deck with an unknown quantity. With the default `[maxerr]` = 1, such a deck is then not computed; with `[maxerr]` 2 or higher it is; in both cases the access outside the tables already happens while the input is read. The gain is a clean error instead of an access outside the bounds. Upstream identical (`43e9bbb:src/swanpre1.ftn:4425` ff.). See `src/swanpre1.f90`. |
| BF-25 | **proven** | **`CGRID` with too few frequencies silently carried on to a "normal" end with Hs = 0.** `[msc] = 0` (or a `FLOW`/`FHIGH` that rounds to 0 intervals) divides by zero in `SWREAD` for the resolution and leaves one frequency; a negative value allocates `SPCSIG` with a negative length. With quadruplets, `FAC4WW` reads `SPCSIG(MSC/2-1)`, so index 0 or lower with fewer than four frequencies; `ERRCHK` only tested `MSC .EQ. 3`. `SWREAD` now rejects `[msc] < 1` (and then sets a valid value, because the message does not stop the input processing straight away), and the quadruplet check also rejects `MSC < 3`. | With runtime checks: `Index '0' of dimension 1 of array 'spcsig' below lower bound` in `swancom4.f90` for `msc` 0 and 1. The upstream build and the normal fork build end normally with Hs = 0 and Tm01 = −9 at the centre point. After the fix, a terminating message for `msc` 0, −3, `FLOW 0.05 FHIGH 0.052` and (with quadruplets) 1; `msc = 1` without quadruplets keeps running. Found while checking the edge branches behind the `do-subscript` warnings. Tests `test_too_few_frequencies_are_rejected` and `test_two_frequencies_without_quadruplets_keep_running`. See `src/swanpre1.f90`, `src/swanmain.f90`. |

### C. Latent, diagnostics and build

Here the code broke a language or interface rule, or a warning was missing, without a wrong result being measured on the decks tried.

| ID | Qualification | Defect in the TU Delft base and fix | Evidence |
|---|---|---|---|
| BF-03 | **proven** | **`INTSTR` changed the caller's integer argument while formatting.** The conversion now works on a local copy and the argument is `INTENT(IN)`. | A leaf-module fixture guards that the argument stays unchanged. See `src/swan_number_formatting.f90` and `tests/test_leaf_modules.f90`. |
| BF-04 | **latent** | **Several unstructured calls had invalid dummy argument ranks or shapes.** For `PLTSRC`, `SPREDT`, `SWTRCF`, `STRSSB`, `PEREXC`, `SWBIDW` and `BRKPAR`, scalars were passed as a rank-2 placeholder, or arrays with two elements to a dummy `RDX(MICMAX)`/`RDY(MICMAX)` — with `MICMAX = 13` as a parameter, while `SwanCompUnstruc` passes `real, dimension(2) :: rdx`. Unused placeholders have been made `OPTIONAL`, and the two-element windows that are actually used have a matching assumed-size interface. | The routines did not read beyond the second index, so no wrong value was dereferenced: **latent**. The explicit module interfaces make the old calls invalid at compile time. The regular and unstructured non-stationary references stayed bit-identical. See also `doc/modern-fortran.md`. |
| BF-08 | **proven with a tool, consequence latent** | **25 optional allocatable arrays were passed as arguments while unallocated.** Input fields are only allocated once a `READINP` command supplies them, and the global grid arrays only for a structured grid, but they are passed unconditionally: whether the data exists is decided by flags such as `LEDS`, never by `ALLOCATED`. That is undefined behaviour in Fortran and stops under `-fcheck=all`. `SWINIT` now gives the 20 fields, the four global grid arrays and `XYTST` a valid empty state. | The full runtime check suite is green and was tested negatively by temporarily removing one empty initialisation. The defect can therefore be shown with a tool; the consequence stayed latent because the path called always tested the flag first and never read the array. For anyone adopting this fix: a reader that only assigns a field if it does not exist yet will find an empty array after `SWINIT` and must then also check the size. See `src/swanmain.f90` and `tests/test_state_clear.f90`. |
| BF-11 | **proven, latent for current production decks** | **`ININTV` and `INITVD` could write an uninitialised helper value to the caller on the `UNC`/`RQI` no-value path.** Upstream calls `INREAL (NAME, RI, KONT, RSTA)` and then unconditionally computes `RVAR = FAC * RI`, while `RI` is never set on that path. `RI` is now seeded with the existing `RVAR`, exactly in line with the contract "do not change the value". | The new contract test was red before the fix because of real stack garbage and is green after it; the two `-Wuninitialized` warnings have disappeared. All current production callers use `STA` or `REQ` and stay bit-identical. See `src/ocpcre.f90`, `tests/test_contexts.f90`. |
| BF-13 | **proven with a tool, consequence latent** | **The date/time conversion read undefined character positions.** `DATE_AND_TIME (TIMSTR(1:8), TIMSTR(10:20), ...)` does not fill all of `TIMSTR`, while `DTSTTI` also reads the separator at position 9. The buffer is now filled with spaces first. | On the nonstationary/unstructured example, Valgrind went from one "conditional jump depends on uninitialised value" to zero. See `src/ocpids.f90`. |
| BF-14 | **proven, diagnostics** | **A directional spread given in degrees was not checked against the directional resolution.** The `POWER` form checked whether the spectrum could be represented, but `DEGREES` did not. Both constant and variable parametric boundaries now check `spread < 0.5*DDIR`, with the same level-2 message and the same once-per-run `LSPNAR` latch as the upstream `POWER` check. | A spread of 5 degrees on 15-degree bins gives the diagnostic in both branches. With the default `MAXERR=1` this refuses the computation; with `MAXERR=2` the run computes. This is an input-resolution check, not a change to the boundary spectrum. Test `test_directional_spread_check_covers_both_parameter_branches` covers both levels and branches. Why the check matters: with a spread of 2 degrees on bins of 15 degrees the boundary gave Hs = 0.07 m instead of about 1.0 m, because the narrow beam is lost in the discretisation; at 10 degrees Hs = 0.999 m remained. See `src/swanpre2.f90`. |
| BF-18 | **latent** | **The unstructured solver also looked at cells around a vertex that do not exist.** To compute the Ursell number and the quadruplets once per vertex and iteration, `SwanCompUnstruc` tests whether no cell around the vertex has been updated yet: `all(vert(ivert)%updated(:)==0)`. The array has room for `MAXVERTCELL` = 10 cells, but only the first `noc` entries, one per actual cell, are ever set; the rest is uninitialised memory. In practice that memory is zero, so the test happens to work. If it is not, for instance when memory is reused by a second run in the same process, the vertex never gets `swpnr = 1` and its quadruplets and Ursell number are skipped. The test now looks at the first `noc` entries only. | Found with Valgrind (`Conditional jump or move depends on uninitialised value`, with its origin in the allocation in `SwanGridTopology`). The output of every deck stays byte-identical. Upstream identical (`43e9bbb:src/SwanCompUnstruc.ftn90:904`). See `src/SwanCompUnstruc.f90`. |
| BF-20 | **proven from the source, diagnostics** | **`SWPRTI` initialised only 30 of its 33 timing rows.** `TABLE` is declared as `(33,2)` but zeroed in a `DO K = 1, 30`, while rows 31 to 33 — sea ice, Bragg scattering and QC scattering — are summed and printed further on. All 33 rows are now initialised. This concerns the `!TIMG` lines, which are only active in a build with the timing switch; without that switch the whole routine is off. | The static data flow is unambiguous, six `-Wmaybe-uninitialized` warnings have disappeared and the timing configuration is green; a before/after measurement of wrong timing output is missing. See `src/swanser.f90`. |
| BF-21 | **latent** | **The asymmetry breaking index read uninitialised locals for an empty spectrum.** In the `ISURF = 7` branch of `BRKPAR`, the sentinel for "no peak found" is `ISIGM = -1` (`43e9bbb:src/swancom2.ftn:3256`), but the final choice tests `IF (ISIGM.EQ.0)` (line 3285). That branch therefore never fires; a point without energy falls through to `ELSEIF (E2.GT.0.35*E1)`, while `E1` and `E2` are only assigned inside `IF (ISIGM.GT.0)` and are ordinary locals without `SAVE` — undefined behaviour in Fortran. That this is a slip and not intended is clear from the Ruessink branch seventy lines higher up, which does catch the same sentinel correctly (`IF (ISIGM.GT.0) ... ELSE KP = 0.`). In the fork the sentinel is ruled out by the structure: the harmonics and the choice that depends on them are in the same `IF (ISIGM.GT.0)` block, so `E1` and `E2` never come into play without an assigned value. The matching `ELSE` gives the spilling constant `PSURF(4)` instead of zero: zero would, through `HM = 0`, switch on the saturated branch of `SSURF`. The defect is in the TU Delft source. | The consequence stays latent: the branch is only reachable where the spectrum is exactly zero, and there `SSURF` with `BB = 0` gives no source in any case, so both possible outcomes of the comparison on garbage values gave the same result. The strict build does see the defect: `maybe-uninitialized` for `e1` and `e2` in `swancom2.f90` disappears with the fix. See `src/swancom2.f90`. |

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

## Known open issues

These issues are in upstream and have been investigated, but not resolved:

- **A stop message does not prove convergence.** SWAN stops a stationary run
  when Hs and Tm hardly change any more between two iterations.
  Under-relaxation (`ALFA`) makes that change small, even when the solution
  has not been reached yet, and the criterion cannot tell those two apart. On
  `quad_dia1` the run stops at `ALFA=0.10` after two iterations and at
  `ALFA=0.05` after ten with "accuracy OK in 100.00 %" at Hs 1.278 m at 25 km,
  and at `ALFA=0.01` after 40 iterations at 1.403 m. Computed on for about
  1500 iterations, the same run arrives at 1.923 m. This is not a property of
  the fork: on `quad_dia2` upstream 41.51 and RWS 41.31A.1 behave the same
  way. At `ALFA=0.01` all three report 100%, while Hs at 25 km is a quarter
  below the run without under-relaxation (`doc/early-stop-by-version.json`).
  The mechanism has therefore been in SWAN since at least 41.31. Because a
  stationary growth problem builds up energy towards the balance, stopping too
  early ends too low; for design conditions that is the unsafe direction.
 
- **How complete this list is.** Some defects were found by reading code that
  looks wrong and checking the pattern against `43e9bbb`, the others through
  compiler warnings, runtime checks, Valgrind and the comparison with upstream
  per deck. All 25 example decks have been run against upstream with the same
  compiler and `-O3`, and each difference was assigned to BF-15 or BF-16, each
  time proven by reverting the fix and measuring byte-identity. That covers
  the paths these decks touch, not the branches outside them, and that is
  exactly where BF-15 was. So there are almost certainly still bugs in
  upstream that are not listed here.
