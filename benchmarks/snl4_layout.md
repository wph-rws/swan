# Quadruplet workspace layout: measured, not adopted

This report records a layout experiment on `SWSNL2` that was measured and then
**rejected**. It is kept so the question does not get re-opened without new
evidence. No production source was changed.

## Conclusion

Turning the quadruplet workspace from frequency-major to direction-major is
bit-identical and slightly faster in isolation, but the gain disappears at the
cache pressure the production sweep actually imposes. The estimated end-to-end
effect is about **1%**, with a measurement spread that includes zero. That does
not justify converting four arrays across five routines, three of which have no
test coverage.

## Why the layout was a candidate

`SWSNL2` is the largest single consumer of CPU time in a regular Voordelta run:

| Routine | % self | Calls |
|---|---:|---:|
| `swsnl2_` | **43.7%** | 790,512 |
| `sordup_` | 8.1% | 701,550 |
| `solmat_` | 7.2% | 823,450 |
| `strsd_` | 7.0% | 823,450 |
| `swpsel_` | 5.6% | 823,450 |
| `sproxy_` | 4.1% | 823,450 |

Measured with `gfortran -O3 -g -pg`, serial, pinned to logical CPU 0.

Its workspace is the only spectral storage in SWAN that is frequency-major:

```
UE, SA1, SA2, SFNL  (MSC4MI:MSC4MA, MDC4MI:MDC4MA)
AC2, IMATRA, IMATDA, PLNL4S, MEMNL4  (MDC, MSC, ...)
```

That mismatch costs a transpose on entry and leaves two loops unvectorized.
With `-fopt-info-vec-all`, at `-O3`:

- `swancom4.f90:1645` — `UE <- AC2`, reported vectorized but reading `AC2` at
  stride `MDC` (36 floats, 144 bytes), so the loads are scalar with inserts.
- `swancom4.f90:1749` — SFNL assembly, `missed: not vectorized`, 18 streams all
  strided along the second index of `SA1`/`SA2`.

## Method

`benchmark_snl4.f90` drives the production `SWSNL2` and a direction-major
prototype of the same arithmetic over identical input, checks bit equality,
and reports the median of five rounds.

Parameters were read out of a live Voordelta run under gdb rather than guessed:

| Quantity | Value |
|---|---|
| `MSC`, `MDC` | 25, 36 |
| `MSC4MI:MSC4MA` | -2:30 |
| `MDC4MI:MDC4MA` | -39:76 |
| `ISCLW:ISCHG` | 1:28 |
| `IDLOW:IDHGH` | -3:13 |
| sweep sector `IDDLOW:IDDTOP` | 1:9 |
| `IQUAD` | 2 |

The prototype is bit-identical to the production routine on `IMATRA`, `IMATDA`,
`REDC0` and `REDC1` in every configuration measured.

## Result

In isolation the prototype wins. Under cache pressure it does not.

The harness touches a scratch buffer between calls to emulate the rest of
`SOURCE` evicting the workspace. The buffer is touched identically in both arms,
so it inflates both times and compresses the ratio; the absolute saving per call
is the quantity that survives and scales to the model run.

| Scratch touched | Production per call | Prototype per call | Saved per call |
|---:|---:|---:|---:|
| 0 KB | 4.01 us | 3.31 us | 0.70 us |
| 256 KB | 6.06 us | 5.51 us | 0.55 us |
| 512 KB | 8.22 us | 7.57 us | 0.65 us |
| 1024 KB | 12.67 us | 11.87 us | 0.80 us |
| 2048 KB | 24.15 us | 24.20 us | -0.06 us |

The production routine costs **11.1 us per call** in the real run
(8.80 s over 790,512 calls). The harness reaches that cost between 768 KB and
1024 KB of scratch. Six repeats at that calibrated operating point:

| Scratch | Production per call | Saved per call |
|---:|---:|---:|
| 768 KB | 10.03 us | 0.12 us |
| 768 KB | 9.94 us | 0.36 us |
| 768 KB | 9.58 us | 0.27 us |
| 1024 KB | 11.63 us | 0.41 us |
| 1024 KB | 11.60 us | 0.62 us |
| 1024 KB | 11.48 us | -0.13 us |

Median saving 0.32 us per call, range -0.13 to 0.62. Against 790,512 calls that
is roughly 0.25 s out of a 24 s run: **about 1%, not distinguishable from noise**.

## Interpretation

`SWSNL2` is bound by memory latency in the cache hierarchy, not by instruction
selection or by array layout. Two independent experiments point the same way:

- Doubling the SIMD width and enabling FMA (`-O3 -march=native`, which widens
  the main interaction loop from 16-byte to 32-byte vectors) changes the full
  run from 24.02 s to 24.27 s — no gain.
- Fixing the layout removes two strided loops and gains about 1%.

The stable 0.70 us saving at 0 KB scratch is what a microbenchmark reports when
it keeps the 61 KB workspace artificially resident. That regime does not exist
in the model, where every call is separated by the rest of the source-term
evaluation.

## Reproduction

```sh
cmake -S . -B build-bench -G Ninja \
  -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_BUILD_TYPE=None \
  -DCMAKE_Fortran_FLAGS=-O3 -DSWAN_DEBUG_INVARIANTS=OFF -DBUILD_TESTING=OFF
cmake --build build-bench

gfortran -O3 -std=f2018 -fimplicit-none -fno-second-underscore \
  -ffree-line-length-none \
  benchmarks/benchmark_snl4.f90 build-bench/lib/libswan41.51.a \
  -Ibuild-bench/mod -lfftw3 -o /tmp/bench-snl4

# REPETITIONS SECTOR GRIDPOINTS SCRATCH_KB
taskset -c 0 /tmp/bench-snl4 30000 9 9076 1024
```

- CPU: Intel Core Ultra 9 185H, 22 logical CPUs
- compiler: GNU Fortran 13.3.0
- serial, `OMP_NUM_THREADS=1`, pinned to logical CPU 0

## What would change the conclusion

- A grid or spectral resolution where the workspace no longer fits the cache
  budget the same way. All numbers above are for `MSC=25`, `MDC=36`.
- A sweep sector materially wider than 9 directions, which would lengthen the
  inner loop of the SFNL assembly and change what vectorization is worth.
- Making `SWSNL2` cache-resident by other means, which would restore the 0.70 us
  regime and make the layout worth revisiting.
