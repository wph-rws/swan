# Compact nonstationary examples

These two cases exercise SWAN's time-dependent solver on the same synthetic
2 x 1 km domain:

- `regular` uses a 21 x 11 regular computational grid;
- `unstructured` uses an included Triangle mesh with 15 vertices and 16
  triangles, and interpolates output to the regular comparison frame.

Both cases run from 2026-01-01 00:00 through 01:00. The regular case uses
one-minute computational time steps with the default S&L propagation scheme;
its requested output interval remains ten minutes. The unstructured case uses
ten-minute computational time steps and SWAN's built-in lowest-order upwind
scheme. The wind input contains fields every 30 minutes:

| Time | x component | y component | Description |
| --- | ---: | ---: | --- |
| 00:00 | 4 m/s | 0 m/s | weak eastward wind |
| 00:30 | 12 m/s | 0 m/s | stronger eastward wind |
| 01:00 | 6 m/s | 6 m/s | wind turns toward the northeast |

SWAN interpolates the wind forcing at intermediate computational times. Each
time level in `wind.wnd` starts with one header line, followed by the complete
x-component map and then the complete y-component map. That layout corresponds
to `READINP WIND ... 3 0 1 0 FREE`: layout 3, no file header, one time-level
header and no component headers.

The smaller time step keeps the regular case below the CFL threshold that SWAN
applies to its default higher-order scheme. The `PROP` command is omitted from
the unstructured case because SWAN always uses its robust lowest-order upwind
scheme on unstructured meshes.

## Build and run

From the repository root:

```sh
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
python3 examples/nonstationary/run.py
```

Run only one grid type with `--case regular` or `--case unstructured`. If the
executable is elsewhere, add `--swan-executable /path/to/swan.exe`.

The runner removes only outputs from an earlier run of the selected case,
checks for normal completion and verifies the requested block and table files.
Useful results in each case directory are:

- `nonstationary_*_center.tbl`: wind and wave parameters at ten-minute
  intervals at the domain centre;
- `nonstationary_*_hs.blk`: successive significant-wave-height maps;
- `nonstationary_*.prt`: SWAN diagnostics and convergence information;
- `nonstationary_*.erf`: errors, only when SWAN creates an `Errfile`.

To run a case without Python, change to its directory, copy the `.swn` file to
`INPUT`, and invoke `swan.exe`. The unstructured Triangle files are included,
so no mesh generator or third-party meshing tool is required.

These are functional examples with synthetic forcing, not calibrated physical
models. See the official SWAN documentation for
[time-dependent input fields](https://swanmodel.sourceforge.io/online_doc/swanuse/node26.html),
[time-dependent output](https://swanmodel.sourceforge.io/online_doc/swanuse/node32.html),
and the [`COMPUTE NONSTATIONARY` command](https://swanmodel.sourceforge.io/online_doc/swanuse/node34.html).
