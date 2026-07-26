# Matrix validation

Final validation date: 2026-07-26. Final run root:
`runs-validation-full-final-2026-07-26`. The matrix contains ten conditions,
four targets and therefore 40 production-size runs. Every run used four OpenMP
threads, returned status zero, wrote `norm_end`, rank-appropriate `PRINT`,
block, table and both spectrum files, and passed the runner's explicit
normal-end and nonempty-output checks.

The four target runners were pinned to non-overlapping CPU sets. This matters
for throughput because each runner sets `OMP_PLACES=cores` and
`OMP_PROC_BIND=close`; it has no effect on the numerical comparison.

## Wet-point results

The gate first proves equal wet/dry masks within each comparison pair and only
then computes statistics. The reference condition has 132 wet and three dry
requested points. Low wind and low water legitimately have fewer wet points;
the table reports the actual shared mask and never includes `EXCV=-9`.

| Condition | Wet/dry points | Current default mean Hsig (m) | Default max abs vs premodern (m) | BSS mean Hsig (m) | Current legacy mean Hsig (m) | Legacy bias vs BSS (m) | Legacy RMS (m) | Legacy max abs (m) |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `u20_d310_lp300_open` | 132/3 | 1.645319 | 0.000000 | 1.290126 | 1.291079 | +0.000953 | 0.005644 | 0.017770 |
| `u02_d090_l0000_open` | 128/7 | 0.018129 | 0.000000 | 0.017858 | 0.017858 | +0.000000 | 0.000000 | 0.000000 |
| `u20_d300_lm200_open` | 119/16 | 1.241231 | 0.000000 | 0.903059 | 0.903668 | +0.000609 | 0.001149 | 0.005070 |
| `u20_d300_lp650_open` | 132/3 | 1.893785 | 0.000000 | 1.453601 | 1.454590 | +0.000989 | 0.005130 | 0.019380 |
| `u20_d310_lp300_closed` | 132/3 | 1.641627 | 0.000000 | 1.286907 | 1.287859 | +0.000952 | 0.005644 | 0.017770 |
| `u20_d120_lp300_open` | 132/3 | 0.859827 | 0.000000 | 0.585327 | 0.585327 | -0.000000 | 0.000005 | 0.000050 |
| `u40_d300_lp300_open` | 132/3 | 3.448175 | 0.000000 | 2.197290 | 2.196340 | -0.000951 | 0.001841 | 0.005990 |
| `u20_d315_lp300_open` | 132/3 | 1.625411 | 0.000000 | 1.266784 | 1.267937 | +0.001153 | 0.004940 | 0.015780 |
| `u20_d316_lp300_open` | 132/3 | 1.617083 | 0.000000 | 1.259290 | 1.259721 | +0.000431 | 0.003772 | 0.008940 |
| `u20_d346_lp300_open` | 132/3 | 1.448489 | 0.000000 | 1.069668 | 1.070214 | +0.000546 | 0.000986 | 0.001840 |

The current 41.51-default result is **exactly equal in all 10 conditions** to
the distinct pre-modernization 41.51 executable, both over every shared wet
field cell and every shared wet requested point. Bias, RMS and maximum
absolute difference are zero. The final current OpenMP executable SHA-256 is
`d0dfb643f72eddae2cc381eb8ea06da799996987ab4c00ea1841e9378e83b6a6`.

The legacy column compares BSS 41.31A.1 with current 41.51 using
`GEN3 KOMEN DRAG FIT` and the explicit old triad defaults. Its small,
condition-dependent residual is the documented version/implementation
difference, not a modernization difference. The largest requested-point RMS
in this matrix is 0.005644 m and the largest absolute difference is 0.019380 m.

The table is generated and gated by
[`analyze_validation.py`](analyze_validation.py). That program fails on a
missing run, failed metadata, wet/dry mismatch, mixed current executable hashes
or any nonzero default-versus-premodern difference.

## Serial and MPI gates

Both current physics decks were also run with a true serial executable
(`309c777402754a…`). They finished normally in 834.34 s (default) and
766.65 s (legacy) and are exactly equal to the current
outputs over all 71,685 wet reference-field cells and all 132 wet requested
points.

Both decks then ran normally with two MPI ranks using executable
`0270864cfef1b3…`: 999.24 s for default and 776.04 s for legacy. MPI is
bit-exact to serial on all 132 wet requested points for both physics routes.
The MPI `.mat` assembly marks 174 partition cells dry that are wet in the
serial field (71,511 versus 71,685 wet cells), identically for both decks.
Consequently the formal physics comparison follows the plan's requested-point
mask and does not claim whole-field mask identity across decomposition modes.

## Build, test, diagnostic and performance gates

- GNU Fortran 13 and 15 Release builds pass all nine registered serial tests.
  Intel and Flang are not installed in this environment. NVFortran 26.5 is
  present but is explicitly outside the compiler set accepted by CMake.
- Serial, OpenMP, netCDF, runtime checks, LTO, TIMG, MATL4, METIS, FFRO and
  debug-invariant configurations pass 9/9 tests. MPI, JAC+MPI and MPI+netCDF
  pass 10/10, including the genuine two-rank test.
- The structured and unstructured OpenMP references pass at 1, 2 and 4
  threads. Nonlinear interaction tests exercise active triad and quadruplet
  paths.
- The strict diagnostic ratchet remains unchanged at 1,534 warnings, including
  exactly two unavoidable external METIS interfaces; no warning category
  budget was raised.
- The complete Python matrix/comparison suite passes 785 tests.
