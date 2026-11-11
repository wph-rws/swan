# Support matrix — 10 November 2026

Per combination: passed / failed / not run / n/a, with version, OS,
architecture, flags, libraries and source revision. Not installed,
not licensed and missing hardware are different reasons for
"not run". Only measured combinations count as supported.

The compilers and the build configurations were measured on 10 November
2026, on commit `13f05a6` of the rebuilt history, with
`scripts/build_matrix.py --clean` (gfortran 16.2, WSL2; MPI runs with the
`OMPI_MCA_*` settings of AGENTS.md). Repeat for each release on the same
source revision.

## Compilers

| Compiler | Version/OS/arch | Status | Evidence |
|---|---|---|---|
| GNU 16.2 (reference) | 16.2.0 / Linux x86-64 (WSL2; the CI container installs the same release), OpenMPI 5.0.10 | **passed** (serial/OpenMP/runtime/strict/MPI and the build variants below) | 10-11: CTest 66/66 serial, OpenMP and runtime checks, 73/73 MPI; strict gate green (1030 warnings, the 233 checked ones all with a verdict); delta gate green with upstream on the same compiler and `-O3`. MPI here requires `OMPI_MCA_pml=ob1 OMPI_MCA_btl=self,sm OMPI_MCA_mtl=^ofi OMPI_MCA_osc=^ucx`. 16.2 wrongly rejects a dummy procedure with an interface under `implicit none(type, external)`, so `z_root2` uses `implicit none(type)` there. `gfortran`/`f95` on this machine do not point to 16.2, so choose `gfortran-16` explicitly |
| Intel `ifx` (IntelLLVM, second implementation) | not installed here | **not run** (toolchain missing) | candidate; first language tests + serial, then OpenMP/MPI/netCDF + operational regressions; the claim "second implementation supported" is ruled out until green |
| Intel `ifort` (classic) | not installed | **not run** (toolchain missing) | upstream's build settings are kept; not a target for new work |
| LLVM Flang 22 | 22.1.8 / Linux x86-64 (system package) | **checked, not supported** | 10-11: compiles without a Fortran warning; CTest 63/66: `setup` and `source_terms_st6` differ only in rounding, and in `input_validation` the case `BF-49-directory-as-file` fails because the flang runtime opens a directory without an error (see AGENTS.md); references and gates stay on gfortran |
| IBM Open XL Fortran (IBMFlang) | not installed | **not run** (candidate) | additional check after language tests + serial output evidence |
| NAG | not installed (licence required) | **not run** (not licensed) | useful additional language/runtime check after licence + tests |
| IBM XL / Open XL, Fujitsu, HPE Cray | no target hardware here | **not run** (partner runner required) | external qualification per toolchain/hardware/test logs |
| Lahey, DEC/SGI, PGI/NVFortran | out of scope | **n/a** (not supported) | upstream's settings for Lahey/DEC/SGI are kept but untested; PGI/NVFortran explicitly out of scope |
| METIS 32-bit (libmetis-dev 5.1.0, IDX32/REAL32) | /usr/include+libmetis.so | **passed** (METIS, and METIS+MPI with real partitioning) | `metis_partitioning` + `quick_test_mpi` green; header+lib from the same installation; widths 32/32 checked at configure time |
| METIS 64-bit index | not installed | **not run** (library missing) | requires a derived 64-bit interface + a test with both 32- and 64-bit libs; without METIS there is no dependency |
| netCDF-Fortran / MPI (OpenMPI, GNU wrapper) | libnetcdff-dev 4.6.4, OpenMPI 5.0.10 from the system packages | **passed** with GNU 16.2 (10 November: see "Build variants") | both `.mod` files come from the system packages and have module version 16, which 16.2 reads |
| Native Windows (MSYS2 UCRT64-GNU, ifx-Windows) | not measured here | **not run** | separate build/run tests, paths with spaces, I/O, Matlab/netCDF, DLL discovery, installed executable; a green WSL result does not certify a Windows binary |
| WSL2, Linux x86-64 | kernel 6.18.58.1-microsoft-standard-WSL2 | **passed** (Linux development + small MPI) | cluster performance on the intended production hardware not measured |

## Configurations

`scripts/build_matrix.py` builds 30 configurations, each in a clean build
directory, and runs the full CTest suite in each. On 10 November 2026 (commit
`13f05a6`):

| Configuration | CTest | Result |
|---|---:|---|
| default (Release), OpenMP, TIMG, TIMG+OpenMP, MATL4, LTO, native (`-march=native`), runtime checks, debug invariants, Debug (`-O0`), strict | 66 each | passed |
| MPI, TIMG+MPI, MATL4+MPI | 73 each | passed |
| netCDF, MATL4+netCDF | 70 each | passed |
| MPI+netCDF | 80 | passed |
| METIS | 66 | passed |
| METIS+MPI | 74 | passed |
| JAC | 72 | passed |
| FFRO, FFRO+OpenMP | 65 each | passed |
| FFRO+MPI | 72 | passed |
| COH | 66 | passed |
| COH+MPI | 67 | passed |
| ESMF (with runtime checks) | 67 | passed |
| ADCIRC | 66 | passed |
| legacy Cray I/O, legacy SGI I/O | 65 of 66 each | `hotstart_continuation` fails (below) |
| Intel `ifx` | – | not run: not installed |

The preserved Cray and SGI I/O branches (`SWAN_LEGACY_CRAY_IO`,
`SWAN_LEGACY_SGI_IO`) fail `hotstart_continuation` because these branches open
every sequential file with `RECL=1000`, as upstream does, and with gfortran that
is 1000 bytes, too short for one spectrum in an unformatted hotfile. CI builds
the default, OpenMP, runtime-check, strict and MPI configurations and the
variants in the next table; the others (TIMG, MATL4, FFRO+MPI, LTO, native,
debug invariants, Debug and the legacy I/O routes) run only locally.

The first run of the matrix that day also showed a real defect: the METIS
build crashed on every unstructured deck run on one process, because the NaN
depth check of BF-61 asked for the table of resident vertices, which only a
parallel run has. That was corrected in the commit of BF-61 before the
history was published.

No test in these suites ran the quasi-coherent model (`GEN4`) on more than
one process, and there it hung after the first iteration, in the fork only.
After each iteration QCM gathers its bulk dissipation on the master and
broadcasts it back. The fork's typed `SWGATHER` checked the receive buffer,
which only the master uses, on every process; on the other processes the
check failed, so they left the gather and waited in the broadcast, while the
master waited in the gather. The commit that fixed this added `mpi_qcm`,
which runs `examples/qcm/qcm_scatter.swn` on one, two and four processes in
every MPI configuration except COH; from that commit on, the CTest counts of
those configurations are one higher than in the tables here.

## Build variants (10 November 2026)

Each variant was built in its own build directory and run with the full CTest
suite, on WSL2. For each variant the table lists which test exercises its own
path: a green suite without such a test only proves that the normal paths
still work. No test was skipped (JUnit output checked, also for "skip" in the
test output). The libraries come from the system packages: libnetcdff-dev
4.6.4, libnetcdf-dev 4.10.1, libmetis-dev 5.1.0 (32-bit indices),
libopenmpi-dev 5.0.10.

| Variant | Compiler | CTest | Test of its own path | CI job |
|---|---|---|---|---|
| netCDF | GNU 16.2 | 70/70 | `netcdf_output`, `netcdf_mixed_block_output`, `netcdf_table_output`, `netcdf_spectrum_output` (contents read with our own netCDF check program), `netcdf_long_name` (file name longer than 80 characters, BF-32) | `variants` / netcdf |
| MPI + netCDF | GNU 16.2 | 80/80 | same, plus `netcdf_output_mpi`, `netcdf_long_name_mpi`, `netcdf_spectrum_output_mpi` (2 processes) | `variants` / mpi-netcdf |
| METIS (serial) | GNU 16.2 | 66/66 | none: without MPI there is no partitioning; only `metis_backend` (choice of backend), and the unstructured decks that go through the METIS branches on one process | `variants` / metis |
| METIS + MPI | GNU 16.2 | 74/74 | `metis_partitioning` (real partition of the unstructured grid over 2 processes, own reference) and `mpi_unstructured_partition` | `variants` / metis-mpi |
| JAC | GNU 16.2 | 72/72 | `quick_test_mpi` with reference `reference-jac` (Jacobi exchange over 2 processes gives a different final field than the wavefront), `hcat_hotstart` with halo 1, `mpi_edges` with four processes; `mpi_output_ownership` is not registered, because Jacobi values depend on how the grid is split | `variants` / jac |
| FFRO (serial) | GNU 16.2 | 65/65 | `nonstationary_examples` with reference `reference-ffro` (unstructured, fixed front order) | `variants` / ffro |
| FFRO + OpenMP | GNU 16.2 | 65/65 | same; see the warning below | `variants` / ffro-openmp |
| COH (serial) | GNU 16.2 | 66/66 | `mpi_lifecycle_backend` (the MPI life cycle does nothing) | `variants` / coh |
| COH + MPI | GNU 16.2 | 67/67 | `coherent_mpi_host` | `variants` / coh-mpi |
| ESMF (with runtime checks) | GNU 16.2 | 67/67 | `esmf_wind_input` (SWAN leaves the coupling storage in place after the run and fills it with wind growth); also `esmf_coupling_backend` | `variants` / esmf |
| ADCIRC | GNU 16.2 | 66/66 | `adcirc_grid` (runs in every configuration) | `variants` / adcirc |

What was established for each variant:

- **netCDF** and **MPI** use a `.mod` from the system packages
  (`netcdf.mod`, `mpi.mod`). Gfortran 16.2 reads these (same module
  version), so all variants build with the same compiler and match the
  normal references byte-identically.
- **METIS** only partitions under MPI. On one process the METIS build takes
  the METIS branches of the code only where they check for a parallel run;
  the unstructured decks run through them.
- **JAC** switches MPI on for the whole build, in the top-level
  `CMakeLists.txt`. That way the tests are registered for an MPI binary,
  including the two-process tests, which are the only ones that tell Jacobi
  apart from the wavefront.
- **FFRO with OpenMP is not deterministic**, even with a fixed
  number of threads. The fixed fronts are consecutive pieces of the
  sorted node list; nodes in the same front are updated at the same time,
  even when one reads the other. On the mesh of the CI determinism test
  (4,686 vertices, `scripts/unstruc_determinism_smoke.py`) two runs with 2
  threads gave different output, and so did two runs with 4 (10 November
  2026).
  Upstream has the same loop (`!FXFRO` in `SwanCompUnstruc.ftn90`); it is
  how that option is designed, not a fault of the fork. The default
  graph order does not have this problem. The CI test of ffro-openmp does not
  hit it: the unstructured example has 15 nodes, so one node per front.
  `front_dependency_completeness` does not apply to the fixed order and is
  not registered there.
- **The regular grid with OpenMP** gives the same output at every thread
  count on the example decks. `triad_biph_dewit` did not, in upstream too,
  until BF-70 (see
  [bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md)); the
  CTest `triad_dewit_determinism` of an OpenMP build checks that deck on more
  threads than CPUs.
- **COH** leaves starting and stopping MPI to a host program, and
  SWAN then does not split up the domain (as in upstream, where under COH all
  MPI calls are dropped). A standalone `swan.exe` from a COH+MPI build under
  `mpiexec -n 2` runs two full computations in the same directory and fails; the
  multi-process MPI tests are therefore not registered under COH.
  `coherent_mpi_host` starts MPI itself, runs the quick test through the library
  and requires that MPI is still open and usable afterwards and that the output equals
  the serial reference. Linked against the normal MPI build, that
  program aborts on the double `MPI_INIT`, as it should.
- **ESMF** needs no ESMF library: the option only adds up the exponential
  wind growth in `SINBAC`, for a coupler. That coupler (an ESMF cap)
  is not in this repo and the system packages provide no ESMF. `esmf_wind_input`
  plays the coupler: it switches on `SAVE_SINBAC`, runs a small KOMEN deck
  and requires a bit-identical result and then a filled `SINBAC`. With a `SINBAC`
  that is too small, the runtime check stops on the grid point index, so the path
  really is exercised. As in upstream, the fork leaves `SINBAC` and
  `SAVE_SINBAC` in place after `SWMAIN`: the coupler allocates and frees them,
  and can read out the wind growth afterwards. The only difference is that
  `SAVE_SINBAC` starts as `.FALSE.` here, where upstream leaves it unset.
- **ADCIRC** as a CMake option changes nothing: upstream 41.51 no longer has
  `!ADC` lines and the ADCIRC grid reader is always built. Upstream has no
  test for that reader. `adcirc_grid` reads the unstructured example as
  `fort.14` and requires byte-identical results with the Triangle grid.

In CI the variants run as one matrix job, `variants`, with gfortran 16.
Each matrix entry reports to the gate as `variant-<name>`;
`scripts/ci_gate.py --check-workflow` requires that the matrix entries and the required
list match each other. `scripts/build_matrix.py` chooses the compiler explicitly
(gfortran-16) and requires, for each variant, the test of its own path.

Not run: METIS with 64-bit indices (no library, see above), a
real ESMF or COHERENS coupling (no coupling code in the repo, no ESMF package
in the system packages) and the ADCIRC+SWAN coupling (`PADCSWAN`, not included).
CI with gfortran 16.2 ran once on GitHub, on 29 October 2026, and failed for
a reason of the CI environment (see closing-account.md §4); it has not run on
the rebuilt history yet. Locally, on the same source revision: default build
66/66, strict gate green (1030 warnings), delta gate against upstream
`43e9bbb` green (every difference has a BF number).

Required release combinations and available runners are set per release.
A second implementation is tested as early as possible. A missing candidate
does not block a qualified GNU route, but it does rule out the related
support claim.
