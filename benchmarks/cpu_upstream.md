# CPU performance versus upstream

This report compares the CPU performance of the build of 18 July 2026 with freshly
fetched upstream revision `43e9bbb`. The measurements were made on 18 July
2026. Individual measurements and machine-readable metadata are available in
[`cpu_upstream.json`](cpu_upstream.json).

## Conclusion

The current default CPU build is approximately 1.85--1.88 times as fast as the
upstream default build, reducing wall-clock time by about 46%.

| Serial case | Upstream default | Current default | Speed-up | Time reduction |
|---|---:|---:|---:|---:|
| Regular Voordelta | 43.46 s | 23.51 s | 1.85x | 45.9% |
| Unstructured Voordelta, 500 m | 66.78 s | 35.61 s | 1.88x | 46.7% |

All depth and significant-wave-height output files were byte-identical between
the builds. The site tables contained identical numerical data; current and
upstream only differed by one space in a header line.

## Method

- CPU: Intel Core Ultra 9 185H, 22 logical CPUs
- compiler: GNU Fortran 13.3.0
- execution: serial, pinned to logical CPU 0
- OpenMP, MPI, `SWAN_NATIVE` and LTO: disabled
- statistic: median of 3--8 runs per configuration
- regular grid: 18,471 computational points
- unstructured grid: 18,471 vertices and 36,400 triangles (500 m spacing)

Upstream configures GNU Fortran with `-O`, which is `-O1`. The current CMake
configuration defaults to `Release`, or `-O3`. To separate source and compiler
effects, both source revisions were built with both optimization levels.
Required source-dialect compatibility flags were retained for each revision.
A literal exchange of every flag is not possible because upstream and FFTPACK
contain deleted Fortran syntax and argument mismatches that are rejected by the
current strict Fortran 2018 flags.

## Compiler/source matrix

| Sources | Optimization | Regular | Unstructured 500 m |
|---|---:|---:|---:|
| Upstream | `-O1` | 43.46 s | 66.78 s |
| Upstream | `-O3` | 29.18 s | 42.27 s |
| Current | `-O1` | 41.58 s | 61.06 s |
| Current | `-O3` | 23.51 s | 35.61 s |

Compiler effect:

| Sources | Regular `-O3` speed-up | Unstructured `-O3` speed-up |
|---|---:|---:|
| Upstream | 1.49x | 1.58x |
| Current | 1.77x | 1.71x |

Source effect at equal optimization:

| Optimization | Regular current speed-up | Unstructured current speed-up |
|---|---:|---:|
| `-O1` | 1.05x | 1.09x |
| `-O3` | 1.24x | 1.19x |

The source and compiler gains are therefore not independent: the current
sources benefit more from `-O3` than upstream does.

## FFTW versus FFTPACK

An isolation build used all current sources and `-O3`, but restored upstream's
FFTPACK 5.1 implementation. Only the FFTPACK source was compiled with
`-std=legacy -fallow-argument-mismatch`; every other source retained the normal
current Release flags.

| Full case, current `-O3` | FFTW | FFTPACK | FFTW speed-up |
|---|---:|---:|---:|
| Regular | 23.51 s | 23.57 s | 1.00x (+0.2%) |
| Unstructured 500 m | 35.61 s | 35.10 s | 0.99x (-1.4%) |

These end-to-end cases use `GEN3 KOMEN` and do not execute the QCM or Bragg FFT
paths. Their FFT backend difference is not material and lies within roughly 2%
run-to-run and binary-layout variation.

A separate kernel benchmark initialized plans before timing and repeatedly
executed a forward/backward transform pair. The table reports
`FFTPACK time / FFTW time`; values above one mean FFTW is faster.

| Square transform | FFTW speed-up |
|---:|---:|
| 2 | 1.83x |
| 4 | 1.69x |
| 8 | 1.30x |
| 16 | 0.97x |
| 32 | 0.87x |
| 64 | 0.88x |
| 128 | 1.12x |
| 256 | 2.07x |

The backend effect is size-dependent under the current
`FFTW_ESTIMATE | FFTW_UNALIGNED` planning configuration. FFTW wins for very
small and larger transforms, while FFTPACK wins for the tested 16--64 sizes.
The end-to-end effect for a QCM or Bragg workload will also depend on the share
of total run time spent in these transforms.

The `fftw_compat` correctness test passed after the measurements.

## Unstructured-grid hotspot benchmarks

The larger 200 m mesh has 114,426 vertices. Focused benchmarks compare the old
algorithms directly with the current production implementations.

| Hotspot | Former implementation | Current implementation | Speed-up | Validation |
|---|---:|---:|---:|---|
| Four sweep-direction vertex sorts | 22.0897 s | 0.05015 s | 440x | identical order |
| 343,278 nearest-vertex queries | 39.0559 s | 0.01662 s | 2,350x | zero mismatches |

These are hotspot speed-ups, not full-model speed-ups: the physical model
calculation remains after initialization and lookup work is complete.

## Reproduction notes

The principal cross-build configurations were:

```sh
# Current default (-O3)
cmake -S . -B build-current -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran -DBUILD_TESTING=OFF

# Current sources with upstream optimization (-O1)
cmake -S . -B build-current-o1 -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_BUILD_TYPE=None -DCMAKE_Fortran_FLAGS=-O \
  -DBUILD_TESTING=OFF

# Upstream default (-O1)
cmake -S upstream-worktree -B build-upstream -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran

# Upstream sources with current optimization (-O3)
cmake -S upstream-worktree -B build-upstream-o3 -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_Fortran_FLAGS_NONE=-O3
```

Runs set `OMP_NUM_THREADS=1` and `OMP_DYNAMIC=FALSE`, were pinned with Linux
CPU affinity, and alternated configurations to reduce cache, temperature and
frequency drift. The existing `benchmark_sort.f90` and
`benchmark_findpoint.f90` programs were used for the hotspot measurements.
