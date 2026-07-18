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
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
python3 examples/quick_test/run.py
```

If the executable is somewhere else:

```sh
python3 examples/quick_test/run.py --swan-executable /path/to/swan.exe
```

The script removes only outputs from an earlier quick-test run, measures the
runtime, checks that SWAN created `norm_end`, and writes these useful files:

- `quick_test_center.tbl`: depth and wave parameters at the domain centre;
- `quick_test_hs.blk`: significant wave height on the complete grid;
- `quick_test.prt`: SWAN diagnostics and convergence information;
- `quick_test.erf`: errors, only when SWAN creates an `Errfile`.

## Alternative Unix runner

From this directory, with `swan.exe` available on `PATH`:

```sh
swanrun -input quick_test
```

Alternatively copy `quick_test.swn` to `INPUT`, run `swan.exe`, and inspect
`PRINT` plus the two output files named in the command file.

Windows users can still run the equivalent `run.ps1` script from PowerShell.

## Expected result

A successful run creates `norm_end`. The centre table should contain finite,
positive significant wave height and period values. Exact values may vary
slightly with compiler and numerical build options; this is a smoke test rather
than a reference-value regression test.
