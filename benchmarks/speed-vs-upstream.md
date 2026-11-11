# Speed against upstream

Measured on 11 November 2026, on commit `13f05a6`, on the Voordelta example (`examples/voordelta`),
with gfortran 16.2 for every build, on an Intel Core Ultra 9 185H with 22
logical CPUs under WSL2, on an otherwise idle machine, apart from one
left-over process that kept one logical CPU busy (it moved between CPUs 17
and 19); the runs used
CPUs 0 to 14.

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
| 1 | 44.85 s | 24.16 s | 1.86x |
| 2 | 24.93 s | 13.41 s | 1.86x |
| 4 | 13.53 s | 7.79 s | 1.74x |
| 8 | 13.20 s | 10.91 s | 1.21x |

MPI processes:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 47.40 s | 27.16 s | 1.75x |
| 2 | 30.66 s | 17.18 s | 1.78x |
| 4 | 19.48 s | 11.57 s | 1.68x |
| 8 | 16.51 s | 11.10 s | 1.49x |

## Upstream at -O3

OpenMP threads:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 30.91 s | 24.07 s | 1.28x |
| 2 | 16.37 s | 13.20 s | 1.24x |
| 4 | 10.82 s | 8.71 s | 1.24x |
| 8 | 12.09 s | 10.90 s | 1.11x |

MPI processes:

| Workers | Upstream | Fork | Fork vs upstream |
|---:|---:|---:|---:|
| 1 | 33.29 s | 26.92 s | 1.24x |
| 2 | 21.63 s | 17.34 s | 1.25x |
| 4 | 14.76 s | 11.19 s | 1.32x |
| 8 | 13.36 s | 11.10 s | 1.20x |

## What this shows

With each project's own default, the fork is about 1.7 to 1.9 times as fast
as upstream on one to four workers, with OpenMP and with MPI. Most of that
comes from the optimisation level: with upstream also at `-O3`, the source
changes are worth about 1.25 times. The same fork binary ran in both series,
and on four OpenMP threads its median differs between them by 12% (7.79 and
8.71 s): differences of that size between the rows are noise of this
machine, not of the code.

With OpenMP, eight workers are slower than four for the fork and for
upstream at `-O3`; upstream's default build gains 2%. With MPI,
eight are about as fast as four for the fork and somewhat faster for
upstream. The Voordelta grid is small enough that eight workers mostly wait
for each other and for memory; four is the useful maximum for this case on
this machine.

## The balance residual

`residual_cost.py` runs the same deck with `NUMERIC STOPC` and with
`NUMERIC STOPC RESIDUAL 0`, seven rounds each, alternating. The cost is the
median of the ratio of the two runs of each round:

| Configuration | Residual off (median) | Residual on (median) | Cost |
|---|---:|---:|---:|
| one core (CPU 0) | 24.22 s | 24.84 s | 2.7% |
| four threads (CPU 0, 2, 4, 6) | 8.27 s | 8.32 s | 4.4% |

On four threads the single runs spread from 7.7 to 8.9 s, more than the cost
itself, so that number is uncertain by a few per cent. The outputs are
byte-identical with and without the residual. Without the keyword, the
residual is not computed at all.

## Raw data and how to repeat

The raw measurements are in `speed-vs-upstream-default.json`,
`speed-vs-upstream-o3.json` and `residual-cost.json`.

Upstream's CMake route does not activate the `!MPI` lines: it runs `switch.pl`
on a pattern that does not expand, so a build with `-DMPI=ON` silently gives
a serial program, and runs on several processes then compute the whole case
several times. Run `switch.pl` on the upstream sources by hand first, in a
copy on a path without hyphens (`switch.pl` loops forever on one), and
check with `nm` that the program calls `MPI_Init`:

```sh
export OMPI_FC=gfortran-16
git -C path/to/upstream archive 43e9bbb | tar -x -C /path/without_hyphens/upstream_mpi
(cd /path/without_hyphens/upstream_mpi/src && perl ../switch.pl -unix -mpi *.ftn *.ftn90)

cmake -S . -B b_cur_omp -G Ninja -DCMAKE_Fortran_COMPILER=gfortran-16 -DOPENMP=ON -DBUILD_TESTING=OFF
cmake -S . -B b_cur_mpi -G Ninja -DCMAKE_Fortran_COMPILER=gfortran-16 -DMPI=ON -DBUILD_TESTING=OFF
cmake -S path/to/upstream -B b_up_omp -DCMAKE_Fortran_COMPILER=gfortran-16 -DOPENMP=ON
cmake -S /path/without_hyphens/upstream_mpi -B b_up_mpi -DCMAKE_Fortran_COMPILER=/usr/bin/mpif90 -DMPI=ON
# for the -O3 comparison add -DCMAKE_Fortran_FLAGS_NONE=-O3 to the two upstream builds
nm b_up_mpi/bin/swan.exe | grep -ci mpi_init    # must not be 0
```

Then, after `cmake --build` for each:

```sh
python3 benchmarks/benchmark_parallel.py \
  --current-openmp b_cur_omp/bin/swan.exe --upstream-openmp b_up_omp/bin/swan.exe \
  --current-mpi b_cur_mpi/bin/swan.exe --upstream-mpi b_up_mpi/bin/swan.exe \
  --workers 1,2,4,8 --repetitions 3 --cpu-list 0,2,4,6,8,10,12,14 \
  --mpi-argument=--bind-to --mpi-argument=core --mpi-argument=--map-by --mpi-argument=core \
  --json result.json --markdown result.md
python3 benchmarks/residual_cost.py --swan-executable b_cur_omp/bin/swan.exe
```

On this WSL machine Open MPI needs `OMPI_MCA_pml=ob1 OMPI_MCA_btl=self,sm
OMPI_MCA_mtl=^ofi OMPI_MCA_osc=^ucx` to start.
