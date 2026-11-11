# Material behind repeated measurements

The documents quote numbers that were measured with test builds, small
programs and scripts that the build and the gates do not need. This folder
keeps that material, so that each number can be measured again from the
repository. A number that needs nothing more than the commit itself, a deck
the text describes in a sentence, or a script under `scripts/`, has nothing
here.

Nothing in this folder is built or run by CMake, CTest, pytest or CI. Most
measurements were made on 11 November 2026 at commit `20bfacb`; the
documents say which commit each number belongs to.

## How to use a patch

Each patch is a test build: an exported copy of a commit with one change.
All patches apply to `20bfacb` (checked with `git apply --check`); to a later
commit they apply as long as the lines around the change are the same.

```sh
git archive 20bfacb | tar -x -C /tmp/variant
cd /tmp/variant && git apply /path/to/swan/doc/measurements/NAME.patch
cmake -S . -B build -DCMAKE_Fortran_COMPILER=/usr/bin/gfortran-16 \
      -DCMAKE_BUILD_TYPE=Release [-DOPENMP=ON]
cmake --build build -j4
```

The scripts take the executable and a work directory as arguments, and find
the decks in the repository themselves. `--help` (or the comment at the top of
the shell script) gives the options. Pin every run to its own CPUs with
`taskset`, and time only on a quiet machine (see AGENTS.md).

## Files

### The LTA of 41.31 against RWS 41.31A.1

Used in AGENTS.md, "Approaching 41.31 in 41.51" (the test build that
reproduces RWS 41.31A.1 to within 0.06 mm, and the step-by-step chain from
`legacy-4131`), and in doc/closing-account.md.

- `lta4131.patch`: `SWLTA` written as in 41.31: the self-interaction term
  multiplied by the group velocity per term, no third harmonic, and no group
  velocity outside the bracket. Build it with `-DOPENMP=ON`.
- `sorp_lta_chain.py`: the so-rp runs of the chain (corrected template,
  condition u20_d310_lp300_open, deck of the target `current_4151_legacy`, only
  the TRIAD line changed per step), with and without `UPSTREAM`, and the run
  with the test build. Each run takes some minutes on four cores.
- `sorp_compare_points.py`: mean Hsig over the output points wet in every run,
  and the largest and rms difference per point against the reference.

```sh
python3 so-rp_swan/matrix/matrix_runner.py run --condition u20_d310_lp300_open \
    --target rws_4131 --threads 4                      # the reference, RWS 41.31A.1
python3 doc/measurements/sorp_lta_chain.py --fork BUILD/bin/swan.exe \
    --test-build BUILD_LTA4131/bin/swan.exe --work /tmp/lta-chain --cpu-groups 0-3,4-7
python3 doc/measurements/sorp_compare_points.py --reference RWS_RUN_DIR /tmp/lta-chain/L*
```

### BF-63: the net/absolute ratios of the LTA

Used in the BF-63 row of
[bug-fixes-vs-tu-delft-41.51.md](../bug-fixes-vs-tu-delft-41.51.md) (the
ratios on `examples/testmodels/sphys` and at x = 15 m on the submerged bar).

- `lta_diag.patch`: a test build that writes, for every call of `SWLTA`,
  energy-weighted sums of the second-harmonic part, the third-harmonic part
  and the bracket, before and after the group velocity, to `lta_diag.txt`.
  Its table and spectra are byte-identical to the build without it.
- `lta_ratios.py`: the ratios from `lta_diag.txt`.

Run each deck in its own directory, with the test build (serial is enough):
`triad_lta_upstream.swn`, `triad_lta.swn` and `triad_lta11.swn` of
`examples/nonlinear_interactions/triad` as `INPUT` with `bottom.bot`; and
`examples/testmodels/sphys/INPUT` with `box.bot`, as it is, with `UPSTREAM` at
the end of its TRIAD line, and with `ITRIAD=11`. Then:

```sh
python3 doc/measurements/lta_ratios.py --bar BAR_RUN_DIR ...
python3 doc/measurements/lta_ratios.py --sweeps 4 SPHYS_RUN_DIR ...
```

### BF-62: the semi-implicit DIA

Used in the BF-62 row of the bug-fix table. The test models are
`examples/testmodels/sq`, `chan` and `slope` (see the README there) and the
Voordelta example.

- `bf62_runs.py`: every run of the row: the three models and Voordelta, with
  `QUADRUPLET 2`, `QUADRUPLET 1` (the fix) and `QUADRUPLET 1 UPSTREAM`, at the
  iteration counts and `ALFA` values the row names, the runs without limiter,
  and the thread check (Voordelta, 20 iterations at 1, 2, 4 and 8 threads).
  The Voordelta references of 2000 iterations take hours.
- `bf62_compare.py`: the comparisons of the row (largest, 99th percentile and
  mean difference of Hs in mm), the residuals of the runs without limiter, and
  the byte check of the thread runs.
- `bf62-rhs.patch`: a test build in which the `UPSTREAM` branch of `SWSNL1`
  also puts the diagonal term times the action density on the right-hand
  side, for the runs `sq_ref_rhs_*` ("with only the right-hand side made
  consistent").
- `bf62-fd.patch` and `bf62-fd-interior.patch`: test builds that compute
  central finite differences of the DIA source term at the three bins with the
  largest `DSNL`, once, at the first grid point with energy in iteration 3,
  and write them to standard error. The first takes all bins (it found the
  highest frequency bin), the second only the bins below the highest
  frequency. Run on the smoke deck (`examples/quick_test/quick_test.swn`) with
  `QUAD IQUAD=1` added.
- `bf62_timing.py`: the cost of one iteration of each DIA form, on Voordelta,
  one and eight threads. Only on a quiet machine.

```sh
python3 doc/measurements/bf62_runs.py --exe BUILD_OMP/bin/swan.exe \
    --rhs-exe BUILD_RHS/bin/swan.exe --work /tmp/bf62 --cpus 16,17,18,19
python3 doc/measurements/bf62_compare.py /tmp/bf62
python3 doc/measurements/bf62_timing.py --exe BUILD_OMP/bin/swan.exe --work /tmp/bf62-timing
```

### FFT plan flag and lock-free plan lookup

Used in [benchmarks/README.md](../../benchmarks/README.md), under
`benchmark_fft.f90`. The two benchmark programs `benchmark_fft_omp.f90` and
`benchmark_fft_plan.f90` are in `benchmarks/`.

- `fftw_measure.patch`: the four plan calls with `FFTW_MEASURE` instead of
  `FFTW_ESTIMATE`.
- `fftw_lock.patch`: every plan lookup takes the planner lock (the early,
  lock-free return taken out).
- `fft_timing.py`: compiles the three benchmark programs against each build
  and measures the plan variation, the bare 63 × 63 transform and the 6 × 6
  transform on one and on eight threads.
- `qcm_timing.py`: whole runs of `examples/qcm/qcm_scatter.swn` with the
  three builds, five runs each, one and eight threads.

```sh
python3 doc/measurements/fft_timing.py --head BUILD --measure BUILD_MEASURE \
    --lock BUILD_LOCK --work /tmp/fft
python3 doc/measurements/qcm_timing.py --head BUILD/bin/swan.exe \
    --measure BUILD_MEASURE/bin/swan.exe --lock BUILD_LOCK/bin/swan.exe --work /tmp/qcm
```

All three builds with `-DOPENMP=ON`.

### Other rows of the bug-fix table

- `bf01-probe-full.patch`, `bf01-probe-K.patch`, `bf01-probe-N.patch`
  (BF-01): `tests/test_physics_reuse.f90` with a message instead of a stop at
  each check, so that it runs on to the end with a library that lacks the
  reset; `-K` runs only the `BREAKING BKD` deck twice, `-N` only the
  nonstationary deck and the decks after it. Apply the patch to a copy of
  `20bfacb`, build the commit before the reset (`225b99c`), and compile the
  patched test against its library:
  `gfortran-16 -O3 -I BUILD_225b99c/mod test_physics_reuse.f90
  BUILD_225b99c/lib/libswan41.51.a -lfftw3`. Run it in an empty directory;
  it writes its own decks and prints the values to standard error.
- `bf08-neg.patch` (BF-08): the empty `ALLOCATE(XYTST(0))` in `SWINIT` taken
  out. Build with `-DSWAN_RUNTIME_CHECKS=ON` and run the smoke deck.
- `bf29-wrap-count.patch` (BF-29): a counter of the directional wrap in
  `SwanIEMsrfbeat`, printed to standard error at the end. Run the two decks of
  `tests/test_surfbeat.py`.
- `bf61-nan.patch` (BF-61): puts a NaN in the depth of the middle grid point
  at the second call of `SWDEPCHK` from `SWCOMP`, after the second set-up
  update. Build with `-DOPENMP=ON` and run `examples/quick_test/quick_test.swn`
  with `SETUP` added, at 1 and at 4 threads.
- `bf66-hwang.patch`, `bf66-zero.patch` (BF-66): the first-guess `CDRAG` of
  `WINDP1` set to the Hwang formula or to 0. Run
  `python3 examples/source_terms_st6/run.py` with each build and with the
  fork as it is (Wu), and compare the outputs.
- `bf28_sector_sweep.py` (BF-28, and the open issue "QCM crashes after its own
  severe error"): the sweep of 120 sectors on `examples/qcm/qcm.swn`, with
  the fork and with upstream; with `--from-sector-centre` the wind and waves
  come from the middle of each sector.
- `sorp_random_points.py` and `sorp_random_points.pnt` (BF-56 and BF-57):
  4000 random output points on the so-rp grid, and an independent search for
  the cell that holds each point. The comment at the top of the script gives
  the deck of the runs.

```sh
python3 doc/measurements/bf28_sector_sweep.py --fork BUILD/bin/swan.exe \
    --upstream UPSTREAM_BUILD/bin/swan.exe --work /tmp/bf28 [--from-sector-centre]
python3 doc/measurements/sorp_random_points.py --depth-mat BLOCK_RUN/dat/blk.mat \
    --run upstream=R_UPSTREAM --run fork=R_FORK --run before57=R_DCFFBC1 \
    --test70 upstream=R70_UPSTREAM --test70 fork=R70_FORK --point 2844
```

### Thread state and the open issues

- `sintgrl-cnt.patch`: counts, in a named critical section, the calls of
  `SINTGRL` with exactly zero energy and writes one line per call to standard
  error. Used in [thread-state-manifest.md](../thread-state-manifest.md),
  "Reachability in the so-rp case". Build with `-DOPENMP=ON` and run the
  corrected so-rp deck at u20_d310_lp300_open with `MXITST=1` and `MXITST=2`.
- `fftpert.patch`: multiplies every FFTW transform output by
  `1 + SWAN_FFT_EPS` (an environment variable). Used in the note on
  `qcm/qcm_scatter.swn` in [upstream-delta.json](../upstream-delta.json). Run
  `examples/qcm/qcm_scatter.swn` serially with `SWAN_FFT_EPS` 0, 1e-12 and
  1e-9, and compare `qcm_scatter.mat` value by value with the fork as it is.
- `triad_dewit_repeat.sh`: runs
  `examples/nonlinear_interactions/triad/triad_biph_dewit.swn` N times at a
  given number of OpenMP threads and prints the md5 of each table. Used in
  the open issue "A deck that is not reproducible with OpenMP" of the bug-fix
  table.

```sh
doc/measurements/triad_dewit_repeat.sh BUILD_OMP/bin/swan.exe 8 10 /tmp/dewit 12-15
```
