# Parallel CPU performance versus upstream

This report compares OpenMP and MPI performance of the build of 23 July 2026
with freshly fetched upstream revision `43e9bbb`. It contains two separately
measured matrices:

1. the repository defaults: upstream `None/-O1` versus current `Release/-O3`;
2. an equal-optimization control with effective `-O3` on both revisions.

The first matrix represents what users get from the documented default builds.
The second separates source-level gains from the different compiler
optimization defaults. Individual measurements, hashes and machine-readable
metadata are available in
[`parallel_defaults_upstream.json`](parallel_defaults_upstream.json)
and
[`parallel_upstream.json`](parallel_upstream.json).

## Conclusion

Using each repository's standard compiler settings, the current build is
1.37--1.90 times as fast as upstream with OpenMP and 1.57--1.91 times as fast
with MPI. At one to four workers, wall-clock time is reduced by 44--48%.

The equal-`-O3` control still shows a 1.18--1.28x OpenMP and 1.15--1.30x MPI
advantage. The production gain therefore comes from both the optimized current
sources and the stronger current default compiler optimization. These effects
are not independent, so their individual speed-ups should not be multiplied.

For this 18,471-point case, four OpenMP threads give the shortest default-build
time: 7.22 seconds, versus 13.32 seconds for upstream at the same thread count.
Eight OpenMP threads are slower for the current source because this case is too
small to amortize the extra synchronization and memory-system overhead. MPI
continues to improve through eight processes, but only marginally beyond four
for the current build.

## Repository-default comparison

### OpenMP

| Threads | Upstream `-O1` | Upstream scaling | Current `-O3` | Current scaling | Current vs upstream |
|---:|---:|---:|---:|---:|---:|
| 1 | 44.06 s | 1.00x | 23.90 s | 1.00x | 1.84x |
| 2 | 23.16 s | 1.90x | 12.20 s | 1.96x | 1.90x |
| 4 | 13.32 s | 3.31x | 7.22 s | 3.31x | 1.85x |
| 8 | 12.88 s | 3.42x | 9.42 s | 2.54x | 1.37x |

The current build reduces wall-clock time by 45.7%, 47.3%, 45.8% and 26.8%
at one, two, four and eight threads respectively. Its parallel efficiency is
98% at two threads, 83% at four threads and 32% at eight threads.

### MPI

| Processes | Upstream `-O1` | Upstream scaling | Current `-O3` | Current scaling | Current vs upstream |
|---:|---:|---:|---:|---:|---:|
| 1 | 45.04 s | 1.00x | 25.13 s | 1.00x | 1.79x |
| 2 | 30.08 s | 1.50x | 15.75 s | 1.60x | 1.91x |
| 4 | 18.69 s | 2.41x | 10.45 s | 2.40x | 1.79x |
| 8 | 16.57 s | 2.72x | 10.57 s | 2.38x | 1.57x |

The current build reduces wall-clock time by 44.2%, 47.7%, 44.1% and 36.2%
at one, two, four and eight processes respectively. Its parallel efficiency is
80% at two processes, 60% at four processes and 30% at eight processes.

## Equal-`-O3` source control

This control gives both revisions effective `-O3` optimization. It quantifies
the end-to-end source advantage without upstream's weaker compiler default.
The matrix was run independently, so only within-matrix ratios should be used;
small absolute-time differences between the two matrices reflect normal
session-to-session variation.

### OpenMP

| Threads | Upstream `-O3` | Upstream scaling | Current `-O3` | Current scaling | Current vs upstream |
|---:|---:|---:|---:|---:|---:|
| 1 | 30.38 s | 1.00x | 23.79 s | 1.00x | 1.28x |
| 2 | 15.73 s | 1.93x | 12.42 s | 1.92x | 1.27x |
| 4 | 9.45 s | 3.21x | 7.75 s | 3.07x | 1.22x |
| 8 | 11.53 s | 2.63x | 9.79 s | 2.43x | 1.18x |

### MPI

| Processes | Upstream `-O3` | Upstream scaling | Current `-O3` | Current scaling | Current vs upstream |
|---:|---:|---:|---:|---:|---:|
| 1 | 32.77 s | 1.00x | 25.39 s | 1.00x | 1.29x |
| 2 | 20.46 s | 1.60x | 15.75 s | 1.61x | 1.30x |
| 4 | 13.85 s | 2.37x | 10.70 s | 2.37x | 1.29x |
| 8 | 12.12 s | 2.70x | 10.54 s | 2.41x | 1.15x |

## Method

- CPU: Intel Core Ultra 9 185H, 22 logical CPUs exposed by WSL2
- compiler: GNU Fortran 13.3.0
- MPI: Open MPI 4.1.6 through `mpifort` and `mpiexec`
- case: regular Voordelta, 18,471 computational points
- workers: 1, 2, 4 and 8
- statistic: median of three runs per build and worker count
- order: upstream/current alternated for successive repetitions
- OpenMP affinity: one hardware thread per physical core selected with
  `taskset`, `OMP_PLACES=cores`, `OMP_PROC_BIND=close` and
  `OMP_DYNAMIC=FALSE`; `OMP_WAIT_POLICY=PASSIVE` prevented idle workers from
  spinning
- MPI affinity: `--bind-to core --map-by core`; each process used one OpenMP
  thread
- `SWAN_NATIVE`, LTO and `SWAN_DEBUG_INVARIANTS`: disabled

The MPI wall-clock measurement includes launcher startup. All runs used fresh
temporary work directories. The current and upstream depth and
significant-wave-height fields were byte-identical in both matrices at every
worker count. The site-table numerical data were also identical. The table
files themselves have four distinct hashes because upstream and current format
one header line differently and MPI formats trailing whitespace differently by
process count.

This small example provides a reproducible comparison and exposes the point at
which parallel overhead dominates. It should not be used to predict scaling
for a production model. Larger calibrated cases can have a different optimal
thread/process count, memory footprint and communication-to-computation ratio.

## Reproduction

Fetch upstream and create separate source worktrees for its two build modes:

```sh
git fetch upstream
mkdir -p /tmp/swan_parallel_benchmark
git worktree add --detach \
  /tmp/swan_parallel_benchmark/upstream_openmp upstream/main
git worktree add --detach \
  /tmp/swan_parallel_benchmark/upstream_mpi upstream/main
```

The underscore-only temporary paths are intentional. Upstream `switch.pl`
mistakes hyphens anywhere in an absolute source path for command-line
switches. OpenMP and MPI also require separate upstream worktrees because that
script generates mode-specific source files in the source tree.

Configure the repository-default builds from the current repository root:

```sh
cmake -S . \
  -B /tmp/swan_parallel_benchmark/build_current_openmp -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran -DOPENMP=ON \
  -DSWAN_DEBUG_INVARIANTS=OFF -DBUILD_TESTING=OFF

cmake -S . \
  -B /tmp/swan_parallel_benchmark/build_current_mpi -G Ninja \
  -DCMAKE_Fortran_COMPILER=mpifort -DMPI=ON \
  -DSWAN_DEBUG_INVARIANTS=OFF -DBUILD_TESTING=OFF

cmake -S /tmp/swan_parallel_benchmark/upstream_openmp \
  -B /tmp/swan_parallel_benchmark/build_upstream_openmp -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran -DOPENMP=ON

cmake -S /tmp/swan_parallel_benchmark/upstream_mpi \
  -B /tmp/swan_parallel_benchmark/build_upstream_mpi -G Ninja \
  -DCMAKE_Fortran_COMPILER=mpifort -DMPI=ON

cmake --build /tmp/swan_parallel_benchmark/build_current_openmp --parallel
cmake --build /tmp/swan_parallel_benchmark/build_current_mpi --parallel
cmake --build /tmp/swan_parallel_benchmark/build_upstream_openmp --parallel
cmake --build /tmp/swan_parallel_benchmark/build_upstream_mpi --parallel
```

Run the default-build matrix:

```sh
python3 benchmarks/benchmark_parallel.py \
  --current-openmp \
    /tmp/swan_parallel_benchmark/build_current_openmp/bin/swan.exe \
  --upstream-openmp \
    /tmp/swan_parallel_benchmark/build_upstream_openmp/bin/swan.exe \
  --current-mpi \
    /tmp/swan_parallel_benchmark/build_current_mpi/bin/swan.exe \
  --upstream-mpi \
    /tmp/swan_parallel_benchmark/build_upstream_mpi/bin/swan.exe \
  --workers 1,2,4,8 --repetitions 3 \
  --current-revision HEAD --upstream-revision 43e9bbb \
  --current-build-description 'repository default Release; effective -O3' \
  --upstream-build-description 'upstream default None; effective -O1' \
  --json parallel-default-results.json \
  --markdown parallel-default-results.md
```

For the equal-`-O3` control, configure two additional upstream build
directories with `-DCMAKE_Fortran_FLAGS_NONE=-O3`, then run the same command
with those upstream executables. The current executables are unchanged.

On Open MPI, the runner automatically adds `--bind-to core --map-by core`.
Other launchers can be configured with repeated `--mpi-argument` options.
