# Speed against upstream

Measured on 28 October 2026 on the Voordelta example (`examples/voordelta`),
with gfortran 16.2 for every build, on an Intel Core Ultra 9 185H with 22
logical CPUs under WSL2, on an otherwise idle machine.

`benchmark_parallel.py` runs each configuration three times, alternating
upstream and the fork, and reports the median wall-clock time. OpenMP runs are
pinned with `taskset` to physical cores 0, 2, 4, … with `OMP_PLACES=cores` and
`OMP_PROC_BIND=close`; MPI runs use `--bind-to core --map-by core`. The script
also checks that both give the same answer: in both comparisons below the
field outputs are byte-identical and the site table holds the same numbers.

There are two comparisons. The first sets each project's own default build
against the other: upstream's CMake builds with build type None, which
compiles at `-O1`; the fork builds Release at `-O3`. The second builds the
upstream source at `-O3` as well, so that only the source differs.

## Default builds

OpenMP threads:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 42.21 s | 23.66 s | 1.78x |
| 2 | 21.59 s | 11.78 s | 1.83x |
| 4 | 13.52 s | 7.27 s | 1.86x |
| 8 | 13.62 s | 11.04 s | 1.23x |

MPI processes:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 44.53 s | 25.74 s | 1.73x |
| 2 | 27.60 s | 15.61 s | 1.77x |
| 4 | 18.71 s | 10.48 s | 1.78x |
| 8 | 17.18 s | 11.54 s | 1.49x |

## Upstream at -O3

OpenMP threads:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 28.55 s | 22.89 s | 1.25x |
| 2 | 15.51 s | 12.37 s | 1.25x |
| 4 | 9.79 s | 7.65 s | 1.28x |
| 8 | 12.24 s | 11.02 s | 1.11x |

MPI processes:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 31.35 s | 25.82 s | 1.21x |
| 2 | 19.81 s | 16.04 s | 1.23x |
| 4 | 18.77 s | 10.52 s | 1.78x |
| 8 | 13.38 s | 11.01 s | 1.21x |

## What this shows

With each project's own default, the fork is about 1.8 times as fast as
upstream on one to four workers, with OpenMP and with MPI. Most of that comes
from the optimisation level: with upstream also at `-O3`, the source changes
are worth about 1.25 times. The upstream MPI run on four processes at `-O3`
scaled poorly in this series (18.77 s, against 13.38 s on eight); that point
is an outlier, not a property of the source.

Eight workers are slower than four for both programs. The Voordelta grid is
small enough that eight workers mostly wait for each other and for memory;
four is the useful maximum for this case on this machine.

## The balance residual

`residual_cost.py` runs the same deck with `NUMERIC STOPC` and with
`NUMERIC STOPC RESIDUAL 0`, seven rounds each, alternating:

| Configuration | Residual off | Residual on | Cost |
|---|---:|---:|---:|
| one core (CPU 0) | 22.91 s | 23.49 s | 2.5% |
| four threads (CPU 0, 2, 4, 6) | 7.26 s | 7.35 s | 3.0% |

The outputs are byte-identical with and without the residual. Without the
keyword, the residual is not computed at all.

## Raw data and how to repeat

The raw measurements are in `speed-vs-upstream-default.json`,
`speed-vs-upstream-o3.json` and `residual-cost.json`.

Upstream's `switch.pl` breaks on a hyphen anywhere in the source path, and it
writes generated sources into the source tree, so the OpenMP and MPI builds of
upstream each need their own worktree on a path without hyphens:

```sh
export OMPI_FC=gfortran-16
git worktree add --detach /path/without_hyphens/upstream_openmp 43e9bbb
git worktree add --detach /path/without_hyphens/upstream_mpi 43e9bbb

cmake -S . -B b_cur_omp -G Ninja -DCMAKE_Fortran_COMPILER=gfortran-16 -DOPENMP=ON -DBUILD_TESTING=OFF
cmake -S . -B b_cur_mpi -G Ninja -DCMAKE_Fortran_COMPILER=mpifort -DMPI=ON -DBUILD_TESTING=OFF
cmake -S upstream_openmp -B b_up_omp -G Ninja -DCMAKE_Fortran_COMPILER=gfortran-16 -DOPENMP=ON
cmake -S upstream_mpi -B b_up_mpi -G Ninja -DCMAKE_Fortran_COMPILER=mpifort -DMPI=ON
# for the -O3 comparison add -DCMAKE_Fortran_FLAGS_NONE=-O3 to the two upstream builds
```

Then, after `cmake --build` for each:

```sh
python3 benchmarks/benchmark_parallel.py \
  --current-openmp b_cur_omp/bin/swan.exe --upstream-openmp b_up_omp/bin/swan.exe \
  --current-mpi b_cur_mpi/bin/swan.exe --upstream-mpi b_up_mpi/bin/swan.exe \
  --workers 1,2,4,8 --repetitions 3 --json result.json --markdown result.md
python3 benchmarks/residual_cost.py --swan-executable b_cur_omp/bin/swan.exe
```

On this WSL machine Open MPI needs `OMPI_MCA_pml=ob1 OMPI_MCA_btl=self,sm
OMPI_MCA_mtl=^ofi OMPI_MCA_osc=^ucx` to start.
