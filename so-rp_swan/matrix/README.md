# so-rp verification matrix

`conditions.json` defines ten compact regression conditions and four separate
reference targets: the pre-modernization 41.51 build, BSS 41.31A.1, current
41.51 defaults and current 41.51 with explicit 41.31-compatible physics.

Validate the manifest, executables and every generated deck without running
SWAN:

```sh
python so-rp_swan/matrix/matrix_runner.py plan
```

Run one matrix entry in a new isolated directory:

```sh
python so-rp_swan/matrix/matrix_runner.py run \
  --condition u20_d310_lp300_open \
  --target current_4151_default \
  --threads 8
```

Use `--executable TARGET=/absolute/path/to/swan.exe` to override a manifest
executable. `--repetitions N` creates independently recorded repeats. Existing
run directories are never overwritten. Every successful directory contains
the exact `INPUT`, a copy named `so-rp_osk.swn`, all static inputs, SWAN
outputs, captured screen output and `run.json` with hashes, condition,
physics, executable, thread count, timing and exit status.

Run all Python verification tests with:

```sh
pytest -q so-rp_swan/matrix so-rp_swan/comparison/test_result_stats.py
```

The full manifest represents 40 production-size SWAN runs and is therefore not
part of the daily test suite. All 40 final sign-off runs, their exact
default-equivalence result, wet-point compatibility statistics, hashes and
serial/MPI companion gates are recorded in `VALIDATION.md`.

After a complete run, enforce exact current-default equivalence to the
pre-modernization 41.51 target and summarize the 41.31 compatibility residual
over the shared wet output points with:

```sh
python so-rp_swan/matrix/analyze_validation.py RUNS_ROOT
```
