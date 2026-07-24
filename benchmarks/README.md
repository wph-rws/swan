# Benchmarks

The benchmark reports compare this repository with Delft University of
Technology's upstream SWAN repository. Raw measurements are stored alongside
the reports as JSON.

- [`parallel_upstream.md`](parallel_upstream.md):
  OpenMP and MPI scaling with 1, 2, 4 and 8 workers. It reports both the
  repository defaults (upstream `-O1` versus current `-O3`) and an equal-`-O3`
  source control.
- [`cpu_upstream.md`](cpu_upstream.md): serial
  end-to-end, compiler/source matrix, FFT kernels and unstructured-grid
  hotspots.

`benchmark_parallel.py` is the reusable cross-build runner. It alternates the
upstream and current executable, reports median wall-clock time and scaling,
and validates the generated model fields and site-table numerical data. See
the parallel report for complete build and invocation commands.
