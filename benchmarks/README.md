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
- [`snl4_layout.md`](snl4_layout.md): a rejected
  optimization. Turning the quadruplet workspace direction-major is
  bit-identical but gains about 1%, because `SWSNL2` is bound by memory
  latency rather than by layout or SIMD width. Kept so the question is not
  re-opened without new evidence.

`benchmark_snl4.f90` is the harness behind that report. It drives the
production `SWSNL2` and a direction-major prototype over identical input,
asserts bit equality, and varies cache pressure so the isolated kernel gain can
be separated from the gain the full model would actually see.

`benchmark_parallel.py` is the reusable cross-build runner. It alternates the
upstream and current executable, reports median wall-clock time and scaling,
and validates the generated model fields and site-table numerical data. See
the parallel report for complete build and invocation commands.
