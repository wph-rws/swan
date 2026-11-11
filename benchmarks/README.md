# Benchmarks

- [`speed-vs-upstream.md`](speed-vs-upstream.md): the fork against TU Delft's
  upstream SWAN 41.51 on the Voordelta example, with OpenMP and with MPI on
  one to eight workers, both with each project's default build and with
  upstream at `-O3`, plus the cost of the balance residual. The raw data are
  in the JSON files next to it.
- `benchmark_parallel.py` runs the fork and upstream alternately, reports
  median wall-clock time and scaling, and checks that the generated fields
  and site tables agree.
- `residual_cost.py` measures the balance residual switched on against
  switched off.
- `benchmark_fft.f90` times the FFT kernels behind QCM and Bragg scattering.
  The FFTW plans are made with `FFTW_ESTIMATE`. With `FFTW_MEASURE`, FFTW
  chose a different plan from run to run at 63 × 63 and 125 × 125 points, the
  sizes of the QCM surf-breaking grid (9 different plans in 20 runs at each
  size), and the last bits of the transform changed with it (2 different
  results in 20 runs at each size). It gave no gain in a whole QCM run
  (`examples/qcm/qcm_scatter.swn`): 8.24 s against 8.26 s on one core, and
  3.29 s against 3.10 s on eight, although the bare 63 × 63 transform took
  13% less time (0.67 against 0.77 s). A transform looks up its plan without
  a lock; only the first use of a size takes one. On eight threads that made
  a 6 × 6 transform 14 times faster than with the lock on every lookup (0.41
  against 5.79 s). A whole QCM run on eight threads did not get measurably
  faster (3.10 s, against 3.17 s with the lock), because the FFT is only a
  small share of it. Measured on 11 November 2026 at commit `20bfacb`,
  against copies of it with `FFTW_MEASURE` or with the lock on every lookup,
  on a quiet machine: the median of five runs, one core on CPU 0 and eight
  threads on CPUs 0, 2, …, 14. The whole runs of a series spread by up to 8%
  and the bare transforms by up to 14%, so a difference of a few percent in a
  whole run means nothing here. The two copies are
  `doc/measurements/fftw_measure.patch` and `fftw_lock.patch`; the scripts
  are `doc/measurements/fft_timing.py` and `qcm_timing.py`.
- `benchmark_fft_omp.f90` lets every OpenMP thread transform its own array
  forward and back at the same time, to measure what a plan lookup costs
  when many threads ask at once (the 6 × 6 case above).
- `benchmark_fft_plan.f90` makes the plan for one size, prints it and a
  checksum of one transform; run many times, it shows whether the plan and
  the last bits of the result change from run to run.

The three FFT programs are not part of the CMake build. Compile them against
the module files and the library of a build directory, for example
`gfortran-16 -O3 -fopenmp -I build/mod benchmarks/benchmark_fft_omp.f90
build/lib/libswan41.51.a -lfftw3`; the build must be made with `-DOPENMP=ON`
for the OpenMP program. `benchmark_fft SIZE REPETITIONS` and
`benchmark_fft_omp SIZE REPETITIONS` print the time in seconds and a
checksum, `benchmark_fft_plan SIZE` the plan and a checksum.

Measure on a quiet machine, both on one core and on several, and report the
thread count, the binding and the cores used (see AGENTS.md). Build with
`-DCMAKE_Fortran_COMPILER=gfortran-16`.
