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

Four later commits were measured on 17 and 18 November 2026; the section
"Work taken out on 17 and 18 November 2026" has those numbers, with the fork
against upstream again.

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
factor is smaller: about 2 on one core (next section). The rest is work
taken out of the sweep that gave the same result every time; the commit
messages of 13 to 15 November 2026 describe each step.

## With the Intel compiler

Measured on 15 November 2026 at commit `f0ab262`, whose source is that of
`2466aa4`, with Intel ifx 2026.2.0 and GNU Fortran 16.2.0. This one
measurement was made under WSL2, because ifx is installed for Linux only.
WSL does not tell which kind of core a run gets, so these times compare the
builds with each other and do not belong beside the Windows times of the
other sections. Voordelta, five runs per build in turn on the same CPU
numbers; the table gives the median and, in brackets, the fastest and the
slowest run. The laptop was in use (load 2 to 4).

| Source | Compiler and flags | One core, serial build | Four threads, OpenMP build |
|---|---|---:|---:|
| Fork | gfortran `-O3` | 9.80 s (9.61 to 10.01) | 3.84 s (3.40 to 4.33) |
| Fork | ifx `-O3 -fp-model precise` | 10.39 s (9.96 to 10.48) | 4.20 s (3.49 to 4.77) |
| Upstream | ifx, its own flags (`-O2`) | 20.11 s (19.66 to 20.66) | 7.21 s (6.67 to 7.97) |
| Upstream | ifx `-O3` | 20.83 s (19.96 to 21.28) | 6.53 s (6.02 to 6.81) |
| Upstream | gfortran `-O3` | 27.91 s (27.71 to 28.82) | 10.31 s (9.97 to 11.88) |

What the table says:

- **The Intel compiler does not make the fork faster.** The fork takes 6%
  longer with ifx on one core, and 9% longer on four threads, where the
  spread of the runs is larger than that difference.
- **It does make upstream faster,** 1.3 to 1.4 times on one core and 1.4 to
  1.6 times on four threads. A program built with ifx starts with abrupt
  underflow, so upstream does not pay for the subnormal numbers there. With
  gfortran the fork sets that mode itself, and has nothing left to gain.
- **With ifx on both sides the fork is 1.9 to 2.0 times as fast as
  upstream** on one core and 1.6 to 1.7 times on four threads; with
  gfortran on both sides, in the same series, 2.8 and 2.7 times. The
  difference between those factors is the subnormal numbers; what is left
  is the work taken out of the sweep.
- **The fastest build of each:** the fork with gfortran against upstream
  with ifx is 2.1 times as fast on one core and 1.7 times on four threads.

Every run took the same number of iterations. The two gfortran builds give
the same Hsig file byte for byte. The ifx builds do not: the fork with ifx
differs from the fork with gfortran at 872 of the 9072 wet points, by up to
1.7 mm (rms 0.08 mm), and upstream with ifx by up to 2.1 mm, with the same
mean Hsig to five decimals. That is the other mathematical library and the
order of products (AGENTS.md, "Why compilers give other last digits"). The
fork with ifx gives the same file on one core and on four threads.

[`intel/`](intel/) holds the two scripts and the times
([`intel/times.csv`](intel/times.csv)).

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

## Work taken out on 17 and 18 November 2026

Four commits after `4ea92ec` take more work out of every visit to a grid
point. The first three give the same output as before, byte for byte. The
fourth changes the last bits of a deck with `TRIAD DCTA NONC` on a full
circle of directions, and nothing else.

- **`dc27d92`, the sweep.** `SORDUP` forms the transport terms of several
  directions at once. `SPROSD` writes the refraction velocity as one
  coefficient and two gradients per frequency. `SINTGRL` takes the squared
  `sinh` and `tanh` of wave number times depth from the cache of the
  dispersion relation when the depth is fixed, and keeps its thirteen
  temporary arrays in the workspace of the thread. `SWPSEL` no longer
  scans every frequency for the range of a direction when there is no
  current. `RESCALE` looks for a negative bin before it sums.
- **`510d38a`, the budget.** At every visit SWAN cleared the coefficients
  from which it sums the dissipation, redistribution and transport for
  output: 31 spectra. It now clears only what the output of the deck
  reads. For a deck that asks for the dissipation alone, as the
  operational so-rp deck does, that is 9 spectra; for a deck without
  such output 7 fewer than before.
- **`4ffe8af`, the DCTA triads.** The collinear form takes each pair of
  frequencies once for all directions. The non-collinear form skips a
  pair when none of its terms can pass the energy threshold that the
  terms are tested against anyway. `FAC3WW` works out the weights of a
  pair once, and not at every grid point.
- **`0ec1db1`, the rotation in the non-collinear DCTA.** For every pair of
  frequencies the non-collinear form goes through all pairs of directions,
  and for each it worked out where the third direction of the triad lies
  and how strongly the three act on each other. On a full circle the
  directions are evenly spaced all the way round, so that depends only on
  the angle between the two directions. It is now worked out once for
  every such angle and looked up for every pair. On a sector the loop of
  upstream stays.

The commit messages say why the first three steps leave the numbers as
they were, and what the fourth changes.

### How it was measured

As in "How it was measured" above: under Windows, programs built with
GNU Fortran 16.2.0 of MSYS2 at `-O3`, one thread per performance core on
the logical CPUs 12, 14, 16, 18, 10 and 0, the two programs of a series
in turn, on mains power. A table gives the median of fifteen runs per
build for the OpenMP builds and of seven for the serial builds. The
series ran between 22:20 and 23:20 with nothing else started on the
laptop. Windows showed 0 to 29% load before and after the series; a
program that keeps the laptop awake and Explorer were the busiest
processes. The times on four threads spread most: in most series one or
two of the fifteen runs took one and a half times the median or more.
The median leaves such runs out. The series of the rotation commit ran on
18 November from 08:02, in the same way, with 0 to 19% load.

Voordelta and the small deck with the so-rp physics
(`windows/decks/sorp_physics`, which asks for the dissipation as the
operational deck does) were forced to run eight iterations with
`NPNTS=101` and `MXITST=8`. The two DCTA decks come from
[`dcta_decks.py`](../doc/measurements/dcta_decks.py), which makes them
from the submerged bar example: a non-collinear deck of 37 x 9 points
with 36 directions and 21 frequencies, six iterations, and a collinear
deck of 101 x 13 points with 72 directions and 31 frequencies, twelve
iterations.

In every series all runs of both programs gave the same block files and
the same numbers in the table, at every number of threads. That holds for
the series of the rotation commit as well: on this deck it changes no
printed digit.

### Each commit against the one before

Voordelta, eight iterations, before / after:

| Threads | `4ea92ec` / `dc27d92` | `dc27d92` / `510d38a` | `4ea92ec` / `4ffe8af` |
|---:|---:|---:|---:|
| 1 | 3.576 / 3.223 s | 3.242 / 3.200 s | 3.566 / 3.160 s = 11% less |
| 2 | 2.195 / 1.967 s | 1.969 / 1.950 s | 2.182 / 1.938 s = 11% less |
| 4 | 1.300 / 1.196 s | 1.317 / 1.339 s | 1.363 / 1.214 s = 11% less |
| 6 | 1.090 / 1.019 s | 1.024 / 1.003 s | 1.093 / 1.013 s = 7% less |
| serial build | 3.578 / 3.281 s | 3.260 / 3.180 s | 3.583 / 3.205 s = 11% less |

The small deck with the so-rp physics, eight iterations:

| Threads | `4ea92ec` / `dc27d92` | `dc27d92` / `510d38a` | `4ea92ec` / `4ffe8af` |
|---:|---:|---:|---:|
| 1 | 0.803 / 0.748 s | 0.746 / 0.707 s | 0.801 / 0.698 s = 13% less |
| 2 | 0.528 / 0.486 s | 0.489 / 0.465 s | 0.535 / 0.470 s = 12% less |
| 4 | 0.328 / 0.307 s | 0.285 / 0.296 s | 0.315 / 0.282 s = 10% less |
| 6 | 0.297 / 0.288 s | 0.290 / 0.272 s | 0.295 / 0.261 s = 12% less |
| serial build | 0.811 / 0.777 s | 0.744 / 0.697 s | 0.787 / 0.698 s = 11% less |

The sweep commit takes 6 to 10% off Voordelta and 3 to 8% off the so-rp
deck. The budget commit takes 5 to 6% off the so-rp deck, which asks for
the dissipation, and 1 to 2% off Voordelta, which does not. On four
threads its two medians lie within the spread of the runs; the fastest
runs there were 0.274 / 0.267 s and 1.156 / 1.130 s. The last column
measures the three commits together in one series (the DCTA commit does
not touch these decks).

The DCTA decks, `510d38a` / `4ffe8af`:

| Threads | Non-collinear (`TRIAD DCTA NONCOLL`) | Collinear (`TRIAD DCTA`) |
|---:|---:|---:|
| 1 | 2.216 / 1.719 s = 22% less | 1.514 / 1.366 s = 10% less |
| 2 | 1.338 / 1.040 s = 22% less | 0.951 / 0.854 s = 10% less |
| 4 | 0.839 / 0.669 s = 20% less | 0.550 / 0.503 s = 9% less |
| 6 | 0.778 / 0.617 s = 21% less | 0.461 / 0.431 s = 7% less |
| serial build | 2.269 / 1.716 s = 24% less | 1.558 / 1.442 s = 7% less |

The non-collinear deck again, `4ffe8af` / `0ec1db1` (18 November):

| Threads | Non-collinear (`TRIAD DCTA NONCOLL`) |
|---:|---:|
| 1 | 1.744 / 0.634 s = 2.8 times as fast |
| 2 | 1.016 / 0.408 s = 2.5 times |
| 4 | 0.653 / 0.277 s = 2.4 times |
| 6 | 0.616 / 0.259 s = 2.4 times |
| serial build | 1.738 / 0.638 s = 2.7 times |

Set beside the times of `510d38a` in the table before it, the two DCTA
commits together make this deck 3.0 to 3.6 times as fast. The collinear
deck, and every deck without `TRIAD DCTA NONC` on a full circle, runs the
same code as at `4ffe8af`.

### The fork against upstream again

The whole Voordelta run, each program to its own stop, upstream
(`43e9bbb`) at `-O3` against the fork at `4ffe8af`; five runs per build in
turn. Upstream's OpenMP build has `-fno-ivopts -fno-ipa-cp-clone`, as
above.

| Workers | Upstream `-O3` | Fork `4ffe8af` | Factor | Factor at `2466aa4` |
|---:|---:|---:|---:|---:|
| serial builds | 27.59 s | 9.31 s | 2.96x | 2.75x |
| 1 thread | 29.63 s | 9.18 s | 3.23x | 2.92x |
| 2 threads | 17.29 s | 5.58 s | 3.10x | 2.73x |
| 4 threads | 9.73 s | 3.94 s | 2.47x | 2.50x |
| 6 threads | 6.84 s | 2.67 s | 2.56x | 2.44x |

So with both sides at `-O3` the fork is now 2.5 to 3.2 times as fast as
upstream on this deck. All times are lower than on 15 November, upstream's
too, because the laptop was not in use; compare the factors. On four
threads the runs of the fork lay between 3.19 and 4.13 s, so that factor
is the least certain of the five. Upstream and the fork gave the same
block files and table numbers in every run.

The same whole run for the fork alone, `4ea92ec` / `4ffe8af`: 10.50 /
9.19 s, 6.41 / 5.75 s, 4.25 / 3.87 s and 3.03 / 2.79 s on 1, 2, 4 and 6
threads, and 10.34 / 9.31 s for the serial builds: 8 to 12% less.

### The checks of the output

Each of the four commits passed the serial, OpenMP and MPI test suites,
pytest, the strict gate and the measured upstream-delta gate under Linux
with gfortran 16.2. Beyond the gates, binary hotfiles and every output
file were compared with those of `4ea92ec` at 1, 2, 4 and 6 threads and
with six threads on two CPUs: on Voordelta, on the so-rp physics deck
with each kind of budget output and with turbulence, mud and sea ice
switched on, on an unstructured deck, on decks with a strong current, an
obstacle and the S&L scheme, and on ten DCTA decks (collinear and
non-collinear, on a full circle, a sector and with a current). For the
first three commits all were the same byte for byte.

The rotation commit was compared with `4ffe8af` in the same way. The
non-collinear decks on a sector, with and without a current, and the
collinear deck are the same byte for byte, and on a full circle the
output is the same at 1, 2, 4 and 6 threads and with six threads on two
CPUs. On the full circle it differs from `4ffe8af`, and so from upstream,
in the last bits: in the binary hotfile of the non-collinear deck above
about two thirds of the action densities differ, after 6 iterations and
after 40 alike. The largest difference is 9.3E-13 on values up to 5.5E-6,
the median relative difference is 1E-6, and single values six orders
below the largest differ by up to 10%. The block file of Hs, the table
and the spectra are equal; with a current added, one printed directional
spread differs by 0.1 degree. Two example decks now run the non-collinear
form in the test suite, `triad_dcta_nonc` on the full circle and
`triad_dcta_nonc_sector` on a sector: against upstream the first differs
in one printed digit of the test-point source terms, and the second is
equal in every file. The delta inventory records that as a deliberate
difference, beside abrupt underflow.

### What was tried and did not go in

- **Factoring the group velocity out of `SORDUP`.** With no current or
  diffraction the propagation velocities are `c_g*cos(theta)` and
  `c_g*sin(theta)`, so the direction can be projected once per point and
  reused at each frequency. That saves multiplications and loads and
  changes where the results are rounded. In a test build of an earlier
  state of the sweep commit (the
  [patch](../doc/measurements/sordup_group_projection.patch) applies to
  `dc27d92`), five runs on the eight-iteration Voordelta deck gave 3.373 /
  3.335 s on one thread and 1.223 / 1.278 s on four: a small gain on one
  core, and a higher median on four. The printed Hsig changed by up to 1 mm over
  9072 wet points on Voordelta (rms 0.015 mm) and over 1071 wet points
  on the small deck (rms 0.031 mm). Not taken.
- **Sharing the refraction coefficients between neighbouring directions
  in `STRSD`, and multiplying by a stored reciprocal in the Thomas
  solver.** Short series showed no consistent gain for the first; the
  second moved the printed Hsig by up to 1 mm. Both were timed while
  another job ran on the laptop, so they have no numbers here.

### Where the time goes now

A profile of the serial build of `4ffe8af` on the so-rp physics deck
(twelve iterations; `perf` under WSL, which samples the program counter
and tells nothing about the kind of core): the DIA 11%, clearing memory
10%, `STRSD` 10%, `ADDDIS` 8%, the transport loop of `SORDUP` 7%,
`SPROXY` 6%, `PHILIM` 4% and `SINTGRL` 4%. Before `510d38a` clearing
memory took 17%. `ADDDIS` is bound by its sums: it adds the terms of all
bins one after the other, and that order fixes the last bit.

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
- `windows/sweep_2026_11_17.ps1` runs the series of 17 and 18 November
  2026 and says which directories and builds it expects; their timings
  are in `results/series-2026-11-17.json` and, for the rotation commit,
  `results/series-2026-11-18.json`.

`benchmark_parallel.py` and `residual_cost.py` in this directory do the same
kind of measurement under Linux, pinned to CPU numbers. Use them to compare
two builds on the same machine, not for an absolute time.
