# Speed against upstream

Measured on 15 November 2026 under native Windows, on the fork at commit
`2466aa4`, against upstream TU Delft SWAN 41.51 (`43e9bbb`) and against the
fork before this speed work (`e68e9be`). Every program was built with GNU
Fortran 16.2.0 of MSYS2 (ucrt64), with CMake and Ninja. The machine is a
laptop with an Intel Core Ultra 9 185H: six performance cores, eight
efficiency cores and two low-power efficiency cores.

## How it was measured

- **Performance cores only.** A run on N workers gets the first N of the
  logical CPUs 12, 14, 16, 18, 10 and 0: one logical CPU on each of the six
  performance cores. Windows keeps a process on the CPUs it is given; WSL
  does not tell which CPU is which kind of core. There are never more than
  six workers.
- **In turn.** Within a series the two programs run right after each other
  on the same CPUs, and which goes first alternates. A table gives the
  median of three runs, or of five where the caption says so.
- **Mains power**, checked before and after every series.
- **Not a quiet machine.** The laptop was in use. Windows showed 0 to 16%
  load before and after the series, and all times are 5 to 10% higher than
  those of the same programs a day earlier. The factors hold, because the
  runs alternate; an absolute time is good to about a tenth.
- **Same answer.** In every series of the first two sections all runs of
  both programs gave the same block files and the same numbers in the site
  table.

The measurement script is [`windows/speed_vs_upstream.ps1`](windows/speed_vs_upstream.ps1);
[`windows/`](windows/) holds the build commands, the scripts of each series
and the timings ([`windows/results/series.json`](windows/results/series.json)).
The last section says how to repeat it.

Upstream comes in two builds. Its own CMake builds with build type None,
which compiles at `-O`; the fork builds Release at `-O3`. The second build
of upstream is at `-O3` as well, so that only the source differs.

## The fork against upstream, Voordelta

`examples/voordelta`: a regular grid of 131 x 141 points, 36 directions and
25 frequencies, stationary, 9050 wet points.

One core, both programs built without OpenMP or MPI:

| Upstream build | Upstream | Fork | Fork vs upstream |
|---|---:|---:|---:|
| default (`-O`) | 42.15 s | 10.84 s | 3.89x |
| `-O3` | 30.28 s | 11.02 s | 2.75x |

OpenMP threads:

| Workers | Upstream default | Fork | Factor | Upstream `-O3` | Fork | Factor |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 44.13 s | 10.92 s | 4.04x | 32.94 s | 11.28 s | 2.92x |
| 2 | 25.11 s | 6.70 s | 3.75x | 18.56 s | 6.80 s | 2.73x |
| 4 | 13.30 s | 3.92 s | 3.39x | 10.36 s | 4.14 s | 2.50x |
| 6 | 10.37 s | 3.18 s | 3.27x | 8.39 s | 3.44 s | 2.44x |

MPI processes (MS-MPI 10.1):

| Workers | Upstream default | Fork | Factor | Upstream `-O3` | Fork | Factor |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 43.44 s | 11.84 s | 3.67x | 31.30 s | 11.28 s | 2.78x |
| 2 | 29.29 s | 7.35 s | 3.99x | 20.04 s | 7.10 s | 2.82x |
| 4 | 16.84 s | 4.40 s | 3.83x | 11.76 s | 4.31 s | 2.73x |
| 6 | 13.57 s | 3.45 s | 3.94x | 9.89 s | 3.35 s | 2.95x |

Upstream's OpenMP build needs help to compute at all under Windows. This
compiler reads some `THREADPRIVATE` variables as ordinary global variables
as soon as it optimises, and upstream then gives Hsig 0 everywhere. With
`-fno-ivopts` (and at `-O3` also `-fno-ipa-cp-clone`) it computes right, and
those are the builds in the table; they are 6 to 8% slower on one thread
than upstream's serial build. The fork has no `THREADPRIVATE` variable and
needs no such option.

So with both sides at `-O3` the source of the fork is 2.4 to 3.0 times as
fast as that of upstream, and against upstream as it builds itself 3.3 to
4.0 times.

Where the difference comes from: about a third of the run time of upstream
goes into subnormal numbers. The DIA multiplies three energy densities, and
the product is often below the smallest normal number; the processor
computes many times slower with such numbers. The fork lets them become
zero. Upstream built with the Intel compiler does not have this slowness,
because that compiler does the same by default, so against such a build the
factor is smaller; that was not measured under Windows. The rest is work
taken out of the sweep that gave the same result every time; the commit
messages of 13 to 15 November 2026 describe each step.

## The fork now against the fork before this work

Voordelta, five runs per build in turn (`e68e9be` / `2466aa4`):

| Workers | Serial build | OpenMP | MPI |
|---:|---:|---:|---:|
| 1 | 23.04 / 11.07 s = 2.08x | 23.90 / 11.06 s = 2.16x | 25.61 / 11.50 s = 2.23x |
| 2 | | 13.68 / 6.70 s = 2.04x | 16.66 / 7.70 s = 2.16x |
| 4 | | 7.79 / 4.22 s = 1.84x | 9.79 / 4.30 s = 2.28x |
| 6 | | 6.54 / 3.26 s = 2.00x | 7.96 / 3.40 s = 2.34x |

Other routes, OpenMP builds on one and on four threads (medians of two to
five runs per build in turn):

| Deck | 1 thread | 4 threads |
|---|---:|---:|
| so-rp physics on a small grid (`windows/decks/sorp_physics`) | 4.59 / 1.76 s = 2.61x | 1.37 / 0.64 s = 2.13x |
| nonstationary, regular grid (`examples/nonstationary/regular`) | 2.20 / 0.77 s = 2.86x | 0.94 / 0.44 s = 2.17x |
| nonstationary, S&L scheme with a current (`windows/decks/nonstat_sandl`) | 3.73 / 1.43 s = 2.61x | 1.40 / 0.68 s = 2.05x |
| ST6, a 1D deck (`examples/source_terms_st6`) | 4.30 / 2.56 s = 1.68x | 4.41 / 2.54 s = 1.74x |
| strong current (`windows/decks/current_strong`) | 0.46 / 0.27 s = 1.68x | 0.27 / 0.16 s = 1.71x |
| obstacles (`windows/decks/obstacle`) | 0.36 / 0.20 s = 1.76x | too short to time |
| `examples/testmodels/sphys` | 10.68 / 4.37 s = 2.45x | 3.23 / 1.57 s = 2.06x |
| `examples/testmodels/slope` | 26.59 / 10.71 s = 2.48x | 8.16 / 4.19 s = 1.94x |
| unstructured Voordelta (`examples/voordelta_unstructured`, 114,426 vertices) | 198.1 / 99.5 s = 1.99x | 93.1 / 49.7 s = 1.87x |

The two commits give the same output on these decks, except where they are
meant to differ: the test-point source spectra of ST6 (abrupt underflow)
and the deck with a current (BF-71).

## Scaling with the number of threads

On six threads the fork takes 3.0 to 3.4 s for Voordelta where one thread
takes 11: a factor of 3.4, not six. Three things were measured to see
where the rest goes (commit `2466aa4`).

**The hardware itself.** N independent serial runs at the same time, each
on its own performance core, take longer per run than one run alone: 11.7 s
alone (11.6 to 12.1 s), a median of 13.0 s with two and with four at a
time, and 13.8 s with six (three repetitions; with six at a time the runs
spread from 12.4 to 16.6 s). The clock frequency drops and the cores share
memory. That is 12 to 18% that no schedule can win back.

**Waiting.** A build with timers around the two places where a thread
waits (`windows/p1_probe.patch`; the timers only run while a thread waits)
gives, for one of three runs:

| Threads | Time in the parallel region | Waiting for the row before, per thread | Computing, all threads together |
|---:|---:|---:|---:|
| 1 | 11.20 s | 0 | 10.98 s |
| 2 | 7.04 s | 0.26 to 0.39 s (5%) | 13.04 s (+19%) |
| 4 | 4.37 s | 0.44 to 0.57 s (12%) | 14.71 s (+34%) |
| 6 | 3.02 s | 0.29 to 0.40 s (11%) | 15.31 s (+39%) |

The sweep is a pipeline of grid rows: every row waits, point by point, for
the row before it. A thread waits a tenth of the time on four and six
threads. The part of the run that only one thread does is 0.003 s.

**The same work costs more.** The last column is the time the threads spend
computing. It grows by 19 to 39%, more than the 12 to 18% of the hardware
alone. Reading rows that another core has just written explains a part of
the difference, a quarter to a third (see the trial below); the rest was
not traced.

So of the factor of 1.8 that is missing on six threads, the hardware takes
about 1.2, the dearer computing about 1.2 and the waiting about 1.1; the
rest is the part of an iteration outside the sweep. MPI, which gives every
process a strip of the grid, scales the same (11.5 s on one process, 3.4 s
on six).

**A trial with another schedule: no gain.** In the pipeline neighbouring
rows belong to different threads, so every row reads a row that another
core wrote. In a trial each thread got one band of neighbouring rows
instead, with about the same number of wet points in every band, and went
through it column by column; a band may compute a column once the band
before it has finished that column (`windows/band_schedule.patch`, on
`89d4af9`, whose source is that of `2466aa4`). Output was byte for byte that
of the pipeline on seven decks at 1, 2, 4 and 6 threads, and in every timed
run. Medians of seven runs in turn, the band schedule against the
pipeline:

| Deck | 1 thread | 2 threads | 4 threads | 6 threads |
|---|---:|---:|---:|---:|
| Voordelta | 10.87 against 10.71 s | 6.71 against 6.62 s | 4.16 against 3.83 s (9% slower) | 3.35 against 3.09 s (8% slower) |
| so-rp physics, small grid | 1.65 against 1.66 s | 1.07 against 1.09 s | 0.60 against 0.59 s | 0.55 against 0.50 s (10% slower) |
| `slope` | 10.18 against 10.31 s | 6.38 against 6.46 s | 3.51 against 3.55 s | 2.68 against 2.64 s |

The timers show why. With bands the threads together compute 6% less on
four threads and 8% less on six (13.4 s against 14.6 s in these runs), so
the reads from other cores do cost something. But the waiting grows from
9 to 14% of the sweep on four threads and from 11 to 18% on six, and the
first bands are done early and wait 0.4 s per thread at the end of each
sweep. Half the Voordelta grid is dry, and the dry part lies elsewhere in
every band, so at one column a band waits for the band before it and at
another the next band waits for it. The pipeline deals the rows out one by
one and does not have that. On `slope`, where every point is wet, the two
schedules take the same time: taking away nearly all reads of a row from
another core gains nothing there. Bands of two rows dealt out in turn were
within 4% of the pipeline at every thread count, faster and slower (five
runs). The pipeline stays.

A thread on an efficiency core (logical CPU 4) takes 1.3 times as long as on
a performance core (medians of six runs in turn: 15.9 s against 12.1 s). In
the pipeline the slowest thread sets the pace, which is why a seventh
worker does not help.

## Build variants

All with the fork at `2466aa4`; the output is byte-identical in every pair.

**AVX2.** The portable Release build against `-DSWAN_NATIVE=ON`
(`-march=native -ffp-contract=off`) and against `-march=x86-64-v3
-ffp-contract=off`, OpenMP, five runs per build in turn (seven for the
small deck):

| Deck | Threads | Portable | `SWAN_NATIVE` | Portable | `x86-64-v3` |
|---|---:|---:|---:|---:|---:|
| Voordelta | 1 | 10.55 s | 10.64 s | 10.96 s | 10.74 s |
| Voordelta | 4 | 3.87 s | 3.76 s | 3.85 s | 3.82 s |
| so-rp physics | 1 | 1.62 s | 1.61 s | 1.66 s | 1.64 s |
| so-rp physics | 4 | 0.57 s | 0.57 s | 0.58 s | 0.61 s |

Nothing above the spread of the measurement. The portable build is the one
to use and to hand out.

**Profile-guided optimisation.** A serial build with `-fprofile-generate`
ran Voordelta and the so-rp physics deck once; a second build used that
profile (`-fprofile-use -fprofile-correction`). Against the plain serial
build, on one core:

| Deck | Plain | With the profile | |
|---|---:|---:|---:|
| Voordelta (trained on) | 10.92 s | 9.94 s | 10% faster |
| so-rp physics (trained on) | 1.65 s | 1.65 s | no difference |
| `examples/testmodels/slope` (not trained on) | 11.82 s | 11.04 s | 7% faster |

A gain of nothing to a tenth, for a build in two steps with training runs
in between. It is not part of the build; `windows/build.cmd` and
`windows/pgo_train.cmd` show how it was done.

**The balance residual** (`NUMERIC ... RESIDUAL 0`, off by default), seven
rounds on Voordelta: 11.66 s to 11.99 s on one thread (3.1%), 4.01 s to
4.25 s on four (8.3%; the rounds spread from one to twelve percent). The
answer is the same with and without it.

## Forms of the DIA that change the answer

Not adopted, only timed. `QUADRUPLET 2` is the default (the DIA at every
sweep, explicit); `QUADRUPLET 3` computes the DIA once per iteration;
`QUADRUPLET 1` is the semi-implicit form. Every run stops on the criterion
of its deck, after the same number of iterations or one fewer (Voordelta
24, 24 and 23; the so-rp physics deck 17). Medians of
five runs (seven for the small deck), the fork alone:

| Deck | Threads | `QUADRUPLET 2` | `QUADRUPLET 3` | `QUADRUPLET 1` |
|---|---:|---:|---:|---:|
| Voordelta | 1 | 11.94 s | 12.18 s | 16.12 s |
| Voordelta | 4 | 4.25 s | 4.24 s | 5.70 s |
| so-rp physics | 1 | 1.74 s | 1.80 s | 2.36 s |
| so-rp physics | 4 | 0.62 s | 0.63 s | 0.79 s |

`QUADRUPLET 3` saves no time, and it changes the answer. Against
`QUADRUPLET 2`, measured under WSL at commit `365afb3` on the fields Hsig,
Tm01 and Dir at the wet points (the builds give the same numbers on any
machine):

| Deck | Hsig, at the deck's own stop | Hsig, after 200 forced iterations |
|---|---|---|
| Voordelta | mean +2.0 mm, up to 11 cm; Tm01 up to 1.7 s | mean +3.2 mm, up to 22 cm; Tm01 up to 2.4 s |
| unstructured Voordelta | mean +3.0 mm, up to 3.2 cm | not run |
| so-rp physics on a small grid | up to 5 mm | up to 1 mm |
| the operational so-rp deck (135 output points) | mean +0.3 mm, up to 4.9 mm | not run |
| `examples/testmodels/slope` | up to 11 mm | up to 11 mm |
| strong current | up to 3 mm | up to 5 mm |
| nonstationary, regular grid | up to 0.5 mm | |

On Voordelta the two forms do not end in the same field: after 200
iterations they differ by more than 1 cm at 568 of the 9050 wet points and
by more than 5 cm at 263, up to 22 cm, while each of them moves by at most
1 to 4 cm between 100 and 200 iterations. With the under-relaxation of the so-rp deck (`ALFA=0.01`)
all three forms reach the same field within 1 mm.

## Windows against Linux

The Windows build does not give the output of the Linux build bit for bit.
On `examples/quick_test` and on the Hsig block of the so-rp physics deck
the files are identical. On Voordelta 989 of the 18,471 Hsig values differ,
by up to 2.3 mm; on ST6, Hsig differs by up to 0.4 mm. The cause is the
run-time library: the Windows library rounds `exp`, `tanh`, `cos`, `sin`,
`log`, a real power and `acos` differently from glibc in the last bit
(`windows/libm`: between 71 and 2414 of 100,000 arguments per function;
`sinh`, `sqrt` and `atan2` agree). Voordelta's iteration does not settle,
so one different bit grows to a millimetre. The stored references and the
gates are those of gfortran 16.2 under Linux.

## Repeating the measurement

Under Windows with MSYS2 (packages `mingw-w64-ucrt-x86_64-gcc-fortran`,
`-cmake`, `-ninja`, and `-msmpi` with Microsoft's MS-MPI runtime for the MPI
builds). The scripts in `windows/` use the directories of the machine they
ran on: `C:\swan-win-perf\final-round` for this round, with the source of
the fork in `src\final`, that of `e68e9be` in `src\e68e9be` and the decks in
`decks\`; `C:\swan-win-perf\upstream-vergelijking` for the measurement
script and the Voordelta deck; and `C:\swanuvgl` for upstream. The upstream
source must lie on a path without a hyphen, because its `switch.pl` reads
every argument with a hyphen as an option and never ends.

- `windows/build.cmd NAME` builds one program (`final_omp`, `final_serial`,
  `final_mpi`, `old_omp`, ..., `probe`, `pgo_gen`, `pgo_use`).
- `windows/run_A.sh` to `run_DEF.sh` run the series, called from WSL; each
  series is one call of `speed_vs_upstream.ps1`, which has a help text.
- `windows/concurrent.ps1` runs the independent serial runs,
  `windows/probe.cmd` the build with the timers, `windows/run_F.cmd` and
  `compare_F.py` the comparison with Linux output.
- `windows/collect_results.py` gathers the timings of all series into
  `results/series.json`.

`benchmark_parallel.py` and `residual_cost.py` in this directory do the same
kind of measurement under Linux, pinned to CPU numbers. Use them to compare
two builds on the same machine, not for an absolute time.
