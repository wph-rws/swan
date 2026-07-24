# Voordelta demonstration

This is a larger stationary SWAN example covering the Voordelta, from the
Westerschelde mouth to the Rotterdam coast. The Cartesian coordinates use
Amersfoort / RD New (EPSG:28992). The 65 x 70 km domain contains a 500 m
computational grid with 18,471 points and a 250 m bathymetry grid with 73,341
points.

## Bathymetry and limitations

The included `voordelta.dep` is derived entirely from two public
Rijkswaterstaat datasets:

- [Bathymetrie Nederland - kust en vaklodingen 20 m](https://data.overheid.nl/en/dataset/c84734f2-73db-4a5b-aa19-801a8faadc30),
  using the fixed 2024 WCS coverage;
- [Bathymetrie versie 2017](https://data.overheid.nl/dataset/1d107faa-bcee-4a64-833f-2874715ffca3),
  used as an offshore background where the coastal coverage has gaps.

Both datasets are published under CC0 1.0. Source elevations relative to NAP
were converted to positive water depths. Elevations above NAP and missing cells
are dry (`-999`). The optional `prepare_bathymetry.py` script records the exact
WCS requests and recreates the file; it requires NumPy, Rasterio and network
access. Running SWAN itself needs none of these.

The NCP source is an interpolated background product and Rijkswaterstaat warns
that it should only be used as background. The wind and boundary spectrum in
this example are synthetic. Consequently this is a realistic demonstration
grid, not a calibrated, validated or operational Voordelta model.

## Build and run under WSL/Linux

From the repository root:

```sh
cmake -S . -B build -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build
python3 examples/voordelta/run.py
```

The runner automatically creates `examples/voordelta/results/`, moves all
numerical output and diagnostics there, and creates two PNG maps. Plotting
requires NumPy and Matplotlib. If they are not installed, the SWAN run still
completes and the figures can be created later after installing them:

```sh
python3 examples/voordelta/plot_results.py
```

To use another executable:

```sh
python3 examples/voordelta/run.py --swan-executable /path/to/swan.exe
```

The useful outputs in `examples/voordelta/results/` are:

- `voordelta_hs.blk`: significant wave height over the computational grid;
- `voordelta_depth.blk`: interpolated model depth;
- `voordelta_sites.tbl`: results at three representative offshore locations;
- `voordelta.prt`: diagnostics and convergence information;
- `voordelta_hs.png`: map of significant wave height;
- `voordelta_depth.png`: map of model bathymetry.

Exact numerical results can vary slightly with compiler and build options.
At this 500 m resolution, narrow tidal channels can become one-cell-wide
connections. SWAN reports `Point removed from computational grid` warnings for
these cells and excludes them from its two-dimensional computational domain;
this is expected for this demonstration grid.

## Run on multiple processors with MPI

The Voordelta grid is also the repository's MPI example. It was selected as a
compact counterpart to the realistic Dutch coastal grids used in published
SWAN parallel-performance studies. In particular, the published Wadden Sea
benchmark has more than two million grid points and needs about 6 GB of memory;
that makes it useful for HPC scaling, but too large for a practical example in
this repository. The 18,471-point Voordelta grid still gives SWAN enough work
and grid lines to demonstrate its MPI strip decomposition on a workstation.

Configure a separate MPI-enabled build and run the case on four processes:

```sh
cmake -S . -B build-mpi -GNinja -DCMAKE_Fortran_COMPILER=gfortran -DMPI=ON
cmake --build build-mpi --parallel
python3 examples/voordelta/run_mpi.py --processes 4
```

The runner requires `mpiexec`, refuses a single-process run, works in an
isolated temporary directory, and verifies that SWAN wrote one `PRINT` file per
requested process. Numerical output, plots and the renamed per-process reports
are placed in `examples/voordelta/results_mpi/`; serial results in `results/`
are not changed. Use `--launcher mpirun` for an installation that exposes only
that launcher. Additional launcher options can be repeated, for example
`--launcher-argument=--oversubscribe` on a machine with fewer available slots.

This choice is based on the
[published SWAN MPI/OpenMP benchmark cases](https://pmc.ncbi.nlm.nih.gov/articles/PMC7304017/)
and the official [MPI run instructions](https://swanmodel.sourceforge.io/online_doc/swanimp/node20.html).
As with the serial example, this demonstrates the software workflow rather
than constituting a calibrated operational model. A small case can run more
slowly under MPI because process start-up and communication add overhead; use a
representative production model when measuring parallel speed-up.

## Performance benchmark

The benchmark runner executes each OpenMP configuration in an isolated
temporary directory and verifies that all numerical output is byte-identical:

```sh
python3 examples/voordelta/benchmark.py \
  --swan-executable build/bin/swan.exe --threads 1,2,4,8
```

For a cross-build comparison of both OpenMP and MPI scaling, use
[`benchmarks/benchmark_parallel.py`](../../benchmarks/benchmark_parallel.py).
The measured upstream comparison and complete build commands are documented
in
[`benchmarks/parallel_upstream.md`](../../benchmarks/parallel_upstream.md).
