# Matrix validation

Latest validation date: 2026-09-07. Analysis root:
`runs-validation-b5`. Both current targets were rebuilt after the ownership
and stencil work
(explicit run-state owners, no implicit stencil readers) and rerun for all
ten conditions: 20 new production-size runs with executable SHA-256
`3fdd7b3d77d1b09b5122289b8b8099e014fade8a63a11432dfeffc6ab6e6a879`.
The immutable pre-modernization 41.51
and BSS 41.31 target results are linked from
`runs-validation-full-final-2026-07-26`, completing the same four-target,
40-entry comparison. Every new run used four OpenMP threads, returned status
zero, wrote `norm_end`, `PRINT`, block, table and both spectrum files, and
passed the runner's explicit normal-end and nonempty-output checks.
Previous validation (2026-08-25, `runs-validation-modernized-2026-08-25`,
SHA-256 `4d443450…affbfc36b`) is superseded for the current source but kept
for history.

The current target runs were sharded over CPU sets for throughput. CPU
placement has no effect on the numerical comparison.

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
`4d443450c2381cf1569e9702ea4c9861627ea0bd46ce41bb0415046affbfc36b`.

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

## Build, test, diagnostic and performance gates (WP1.6-meting 2026-09-07, tranche 6)

- CTest-registratie per configuratie (`ctest -N`, 2026-09-07, kandidaat
  tranche 6; geen schatting): **50** voor standaard, OpenMP, TIMG,
  TIMG+OpenMP, Matlab 4, METIS, FFRO, COH, ESMF, ADCIRC, LTO, native, runtime
  checks, debug invariants, beide legacy-I/O-routes, GNU 15, strict en debug;
  **53** voor netCDF en Matlab 4+netCDF; **54** voor MPI, JAC, TIMG+MPI,
  Matlab 4+MPI en FFRO+MPI; **55** voor METIS+MPI; **59** voor MPI+netCDF.
  Deze aantallen zijn inclusief de tien configuratie-onafhankelijke poorttests
  (`reference_check_negatives`, `strict_diagnostics_negatives`,
  `build_matrix_negatives`, `ci_gate_self_test`, `mpi_field_mask_self_test`,
  `lifetime_fault`, `swan_library_contract`, `physics_reuse`, `grid_reuse`,
  `spectrum_reuse`), de echte tweeranks-MPI-regressies waar van toepassing en
  de MPI-only poort `mpi_unstructured_partition` (partitiecontract:
  success-verwachting met METIS, clean-failure-verwachting zonder). Het oude
  "25 tests"-getal is hiermee vervallen; herhaal deze `ctest -N`-telling per
  configuratie na iedere bronwijziging die tests toevoegt.
- Intel and Flang are not installed in this environment. NVFortran 26.5 is
  present but is explicitly outside the compiler set accepted by CMake
  (buiten scope: geen NVIDIA/NVFortran/PGI-support).
- The structured and unstructured OpenMP references pass at 1, 2 and 4
  threads. Nonlinear interaction tests exercise active triad and quadruplet
  paths.
- Strict-poort (WP2.4 + WP3a/WP5b-tranches, schone bouw met `--require-baseline`,
  GNU 13.3.0): **1.373 waarschuwingen in 512 fingerprints**, `within budget`,
  `no new warnings` (`function-elimination` 163→85 door zuivere `pvalid`;
  `implicit-interface` 2→0 door `swan_metis_interface`/BIND(C); WP5b-tranches 1–3
  voegen geen nieuwe waarschuwingen toe). Configureerlog
  `strict-configure.log` en bouwlog `strict-build.log` bewaard in de bouwmap;
  geen incrementeel slotlog. Budget en fingerprints zijn meegecommit
.
- The complete Python matrix/comparison suite passes 785 tests.
