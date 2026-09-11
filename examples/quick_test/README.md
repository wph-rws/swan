# Quick stationary test

This is a small end-to-end smoke test for SWAN 41.51. It exercises a regular
two-dimensional grid, bathymetry input, a parametric wave boundary, constant
wind, the stationary solver, block output and tabular point output.

The case uses only 21 x 11 spatial points, 24 directional bins and 16 frequency
bins. It should normally finish in seconds and is intended to remain below three
minutes even on modest hardware. Runtime is not a hard guarantee because it
depends on the compiler, build options and machine.

## Build and run under WSL/Linux

From the repository root:

```sh
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran-16
cmake --build build
python3 examples/quick_test/run.py
```

If the executable is somewhere else:

```sh
python3 examples/quick_test/run.py --swan-executable /path/to/swan.exe
```

The script removes only outputs from an earlier quick-test run, measures the
runtime, checks that SWAN created `norm_end`, compares the table and block
output with `reference/` (relative tolerance 1e-3, see
`examples/reference_check.py`), and writes these useful files. Use
`--work-directory` to run elsewhere and `--smoke` to run without the
comparison (a smoke run does not count as a passed regression test):

- `quick_test_center.tbl`: depth and wave parameters at the domain centre;
- `quick_test_hs.blk`: significant wave height on the complete grid;
- `quick_test.prt`: SWAN's PRINT output with the iteration history;
- `quick_test.erf`: errors, only when SWAN creates an `Errfile`.

To run under MPI, add `--mpi-exec mpiexec --mpi-processes 2`
(and `--mpi-numproc-flag` if the launcher takes another option than `-n`).

## Alternative Unix runner

From this directory, with `swan.exe` available on `PATH`, use the
[`swanrun`](../../bin/swanrun) script that ships with the repository:

```sh
../../bin/swanrun -input quick_test
```

Alternatively copy `quick_test.swn` to `INPUT`, run `swan.exe`, and inspect
`PRINT` plus the two output files named in the command file.

On Windows, `run.ps1` runs the case from PowerShell and checks that
`norm_end` exists. It does not compare the output with `reference/`.

## Expected result

A successful run creates `norm_end`. The centre table should contain finite,
positive significant wave height and period values, and the runner's
comparison with `reference/` passes with a gfortran 16.2 build.

`reference-jac/` belongs to a build with the Jacobi sweep exchange
(`-DJAC=ON`) and is only needed under MPI on two or more processes; on one
process both schemes give the same answer. Select it with
`--mpi-exec mpiexec --mpi-processes 2 --reference reference-jac`.
