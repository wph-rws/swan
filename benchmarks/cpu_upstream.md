# CPU performance versus upstream

> A follow-up report covers
> [OpenMP and MPI scaling versus upstream](parallel_upstream.md)
> for both the repository defaults and equal `-O3` optimization.

This report compares the CPU performance of the current working tree,
the build of 19 July 2026, with freshly fetched upstream revision `43e9bbb`. It
includes the SWSNL workspace optimizations in `swancom4.f90` and the FFTW
planning optimization in `fftw_compat.f90`. Production builds explicitly used
`SWAN_DEBUG_INVARIANTS=OFF`, so the optional diagnostic checks were not
compiled. The original upstream measurements were made on 18 July 2026 and
the optimized current build was remeasured on 19 July 2026. Individual
measurements and machine-readable metadata are available in
[`cpu_upstream.json`](cpu_upstream.json).

## Conclusion

The current default CPU build is approximately 1.94--2.01 times as fast as the
upstream default build, reducing wall-clock time by about 48--50%.

| Serial case | Upstream default | Current default | Speed-up | Time reduction |
|---|---:|---:|---:|---:|
| Regular Voordelta | 43.46 s | 22.46 s | 1.94x | 48.3% |
| Unstructured Voordelta, 500 m | 66.78 s | 33.19 s | 2.01x | 50.3% |

All depth and significant-wave-height output files were byte-identical between
the builds. The site tables contained identical numerical data; current and
upstream only differed by one space in a header line.

## Method

- CPU: Intel Core Ultra 9 185H, 22 logical CPUs
- compiler: GNU Fortran 13.3.0
- execution: serial, pinned to logical CPU 0
- OpenMP, MPI, `SWAN_NATIVE`, LTO and `SWAN_DEBUG_INVARIANTS`: disabled
- statistic: median of 3--5 current runs per configuration; retained upstream
  measurements are medians of 3 runs
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
| Current | `-O1` | 36.75 s | 56.47 s |
| Current | `-O3` | 22.46 s | 33.19 s |

Compiler effect:

| Sources | Regular `-O3` speed-up | Unstructured `-O3` speed-up |
|---|---:|---:|
| Upstream | 1.49x | 1.58x |
| Current | 1.64x | 1.70x |

Source effect at equal optimization:

| Optimization | Regular current speed-up | Unstructured current speed-up |
|---|---:|---:|
| `-O1` | 1.18x | 1.18x |
| `-O3` | 1.30x | 1.27x |

The source and compiler gains are therefore not independent. Compared with the
earlier measurements of 18 July 2026, the current default build reduces the regular
case median by 4.5% and the unstructured case median by 6.8%.

## FFTW versus FFTPACK

The end-to-end Voordelta cases use `GEN3 KOMEN` and do not execute the QCM or
Bragg FFT paths. The earlier isolation build of 18 July 2026 found less than 2%
difference between FFTW and FFTPACK in these cases, consistent with run-to-run
and binary-layout variation. That isolation result was not rerun because the
backend is not exercised.

A separate kernel benchmark initialized plans before timing and repeatedly
executed a forward/backward transform pair. Current FFTW uses measured aligned
plans when the caller has the cached alignment and retains an estimated,
unaligned fallback. The first speed-up column compares the previous FFTW
implementation with the current one. The last column reports
`FFTPACK time / current FFTW time`.

| Square transform | Previous FFTW | Current FFTW | Current/previous speed-up | Current FFTW vs FFTPACK |
|---:|---:|---:|---:|---:|
| 2 | 0.096 s | 0.116 s | 0.83x | 1.52x |
| 4 | 0.084 s | 0.076 s | 1.10x | 1.86x |
| 8 | 0.132 s | 0.070 s | 1.88x | 2.43x |
| 16 | 0.216 s | 0.085 s | 2.54x | 2.47x |
| 32 | 0.310 s | 0.111 s | 2.79x | 2.42x |
| 64 | 0.474 s | 0.157 s | 3.01x | 2.65x |
| 128 | 0.634 s | 0.198 s | 3.21x | 3.58x |
| 256 | 0.420 s | 0.258 s | 1.63x | 3.37x |

The measured aligned plans improve every tested size from 4 through 256, by up
to 3.21x over the previous FFTW configuration. For the tiny 2x2 transform the
additional alignment and plan-selection overhead outweighs the faster plan,
although current FFTW remains 1.52x faster than FFTPACK. The end-to-end effect
for a QCM or Bragg workload will also depend on the share of total run time
spent in these transforms.

The `fftw_compat` correctness test passed after the measurements.

## Unstructured-grid hotspot benchmarks

The larger 200 m mesh has 114,426 vertices. These unchanged 18 July
measurements compare the old algorithms directly with the current production
implementations.

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
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DSWAN_DEBUG_INVARIANTS=OFF -DBUILD_TESTING=OFF

# Current sources with upstream optimization (-O1)
cmake -S . -B build-current-o1 -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_BUILD_TYPE=None -DCMAKE_Fortran_FLAGS=-O \
  -DSWAN_DEBUG_INVARIANTS=OFF -DBUILD_TESTING=OFF

# Upstream default (-O1)
cmake -S upstream-worktree -B build-upstream -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran

# Upstream sources with current optimization (-O3)
cmake -S upstream-worktree -B build-upstream-o3 -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran \
  -DCMAKE_Fortran_FLAGS_NONE=-O3
```

Runs set `OMP_NUM_THREADS=1` and `OMP_DYNAMIC=FALSE`, were pinned with Linux
CPU affinity, and the original cross-build measurements alternated
configurations to reduce cache, temperature and frequency drift. The FFT
kernel can be reproduced after building the current library with:

```sh
gfortran -O3 -std=f2018 -fimplicit-none \
  benchmarks/benchmark_fft.f90 build/lib/libswan41.51.a \
  -lfftw3 -o /tmp/swan-fft-benchmark

taskset -c 0 /tmp/swan-fft-benchmark 64 10000
```

The existing `benchmark_sort.f90` and `benchmark_findpoint.f90` programs were
used for the hotspot measurements.
