# Benchmarks

- `benchmark_parallel.py` runs the fork and upstream alternately, reports
  median wall-clock time and scaling, and checks that the generated fields
  and site tables agree.
- `residual_cost.py` measures the balance residual switched on against
  switched off.
- `benchmark_fft.f90` times the FFT kernels behind QCM and Bragg scattering.
  The FFTW plans are made with `FFTW_ESTIMATE`. With `FFTW_MEASURE`, FFTW
  chose a different plan in almost every run at 63 × 63 and 125 × 125
  points, the sizes of the QCM surf-breaking grid, and the last bits of the
  transform changed with it. It gave no gain in a whole QCM run: 29.1 s
  against 29.0 s on one core and 8.2 s on eight, although the bare 63 × 63
  transform was 18% faster. A transform looks up its plan without a lock;
  only the first use of a size takes one. On eight threads that made a 6 × 6
  transform 27 times faster. A whole QCM run on eight threads went from
  8.56 s to 8.28 s, because the FFT is only a small share of it.

Measure on a quiet machine, both on one core and on several, and report the
thread count, the binding and the cores used (see AGENTS.md). Build with
`-DCMAKE_Fortran_COMPILER=gfortran-16`.
