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

## Build, test, diagnostic and performance gates (meting 2026-09-07)

- CTest-registratie per configuratie (`ctest -N`, 2026-09-07, kandidaat
  de kandidaat; geen schatting): **58** voor standaard, OpenMP, TIMG,
  TIMG+OpenMP, Matlab 4, METIS, FFRO, COH, ESMF, ADCIRC, LTO, native, runtime
  checks, debug invariants, beide legacy-I/O-routes, GNU 15, strict en debug;
  **61** voor netCDF en Matlab 4+netCDF; **62** voor MPI, JAC, TIMG+MPI,
  Matlab 4+MPI en FFRO+MPI; **63** voor METIS+MPI; **67** voor MPI+netCDF.
  Deze aantallen zijn inclusief de achttien configuratie-onafhankelijke poorttests
  (`reference_check_negatives`, `strict_diagnostics_negatives`,
  `build_matrix_negatives`, `ci_gate_self_test`, `mpi_field_mask_self_test`,
  `lifetime_fault`, `swan_library_contract`, `physics_reuse`, `grid_reuse`,
  `spectrum_reuse`, `shoaling`, `shoaling_units`, `grid_convergence`,
  `grid_convergence_units`, `time_convergence`, `time_convergence_units`,
  `curve_output`, `curve_output_units`), de echte tweeranks-MPI-regressies
  waar van toepassing en de MPI-only poort `mpi_unstructured_partition`
  (partitiecontract: success-verwachting met METIS, clean-failure-verwachting
  zonder). Het oude "25 tests"-getal is hiermee vervallen; herhaal deze
  `ctest -N`-telling per configuratie na iedere bronwijziging die tests
  toevoegt. `shoaling`/`grid_convergence`/`time_convergence`/`curve_output`
  zijn bewezen op serieel/OpenMP/MPI/METIS+MPI; overige varianten staan via
  registratie (werking volgt uit onafhankelijkheid van hun selecties:
  gestructureerd, serieel, tekstuitvoer).
- Intel and Flang are not installed in this environment. NVFortran 26.5 is
  present but is explicitly outside the compiler set accepted by CMake
  (buiten scope: geen NVIDIA/NVFortran/PGI-support).
- The structured and unstructured OpenMP references pass at 1, 2 and 4
  threads. Nonlinear interaction tests exercise active triad and quadruplet
  paths. The unstructured OpenMP solver is outside the operational so-rp
  route (structured decks only): the front-scheduling race documented in
  `doc/moderniseringsplan.md` (missing `vu(2)` edge, fixed
  unconditionally) does not touch the structured operational suite, which
  is bit-exact before and after.
- Strict-poort (schone bouw met `--require-baseline`, GNU 13.3.0):
  **1.317 waarschuwingen** tegen een budget van **1.317**, `within budget`,
  `no new warnings` — gemeten 11 september 2026 (1.324/488 na
  de fysicabenoeming, 10 september; 1.358/502 na de `RI`-contractseed; 1.360/503 na
  de laatste geheugenvrijgave; 1.361/504 op de kandidaat van 7 september). De daling van
  zes zit volledig in `unused-dummy-argument` (67→61) door de
  argumentbundeling; de zevende waarschuwing die wegvalt is de
  `maybe-uninitialized` op `vu[0]` uit het determinismeherstel, waarvoor het
  budget bewust op 135 blijft staan. Attributie: `function-elimination` 163→85 door zuivere `pvalid`;
  `implicit-interface` 2→0 door `swan_metis_interface`/BIND(C);
  `unused-function` 4→0 plus één `compare-reals` in dood `SWSOR` door
  verwijderde dode procedures; −5 `unused-parameter` buiten gevenderde code;
  −3 `maybe-uninitialized` (138→135): 137 door de NEXTI/ORQTMP-keten-vrijgave,
  136 door de SPROUT-vrijgave (`TMP`-pad), 135 door de XYPT-vrijgave
  (XYPT-`SWBOUN`-keten; de oude kale `DEALLOCATE(TMP)`-regel verdwenen;
  geheugen cumulatief 2040 B → 0 B);   `uninitialized` 2→0 door de
  de `RI`-contractseed in `ININTV`/`INITVD`; `function-elimination` 85→51
  door zuivere `EQREAL`/`EQDBLE`/`EQCSTR` (+`UPCASE`)
  (10 september 2026);
  alles volledig geattribueerd zonder compensatie).
  Configureerlog `strict-configure.log` en bouwlog `strict-build.log` bewaard
  in de bouwmap; geen incrementeel slotlog. Budget en fingerprints zijn
  meegecommit.
- The complete Python matrix/comparison suite passes 797 tests.

## Extended quantities — Tm01, direction, convergence

Generated with the current `analyze_validation.py` on the same stored data
(`runs-validation-b5`, same four targets and run hashes as above; no new model
runs). Tm01 and direction use the header-named table columns (not positions);
direction is compared circularly excluding points below 0.05 m Hsig in both
runs; convergence is the per-iteration PRINT accuracy series. The
default-vs-premodern gate demands bit-equality here too (any nonzero raises
before the table completes); the legacy-vs-BSS columns only report
(`tolerances.json`: report_only — the maxima below are results, not
automatically acceptable migration tolerances). Spectra (`.sp1`/`.sp2`) and
source terms are explicit follow-up work, not claimed here.

| Condition | Default Tm01 max abs vs premodern (s) | Default Dir max abs vs premodern (deg) | Convergence iters premodern/current | Legacy Tm01 max abs vs BSS (s) | Legacy Dir max abs vs BSS (deg) |
|---|---:|---:|---:|---:|---:|
| `u20_d310_lp300_open` | 0.000000 | 0.000000 | 35/35 | 0.030800 | 0.741000 |
| `u02_d090_l0000_open` | 0.000000 | 0.000000 | 4/4 | 0.000100 | 0.000000 |
| `u20_d300_lm200_open` | 0.000000 | 0.000000 | 37/37 | 0.010200 | 0.062000 |
| `u20_d300_lp650_open` | 0.000000 | 0.000000 | 40/40 | 0.025900 | 0.668000 |
| `u20_d310_lp300_closed` | 0.000000 | 0.000000 | 35/35 | 0.030800 | 0.741000 |
| `u20_d120_lp300_open` | 0.000000 | 0.000000 | 37/37 | 0.000400 | 0.006000 |
| `u40_d300_lp300_open` | 0.000000 | 0.000000 | 34/34 | 0.011500 | 0.078000 |
| `u20_d315_lp300_open` | 0.000000 | 0.000000 | 36/36 | 0.027300 | 0.635000 |
| `u20_d316_lp300_open` | 0.000000 | 0.000000 | 36/36 | 0.022000 | 0.623000 |
| `u20_d346_lp300_open` | 0.000000 | 0.000000 | 35/35 | 0.006400 | 0.152000 |

Extended gate: default-vs-premodern Tm01, direction and convergence history are
bit-equal in all 10 conditions.

## Spectra — VaDens/NDIR/DSPRDEGR per frequentie per locatie

Generated with the current `analyze_validation.py` on the same stored data
(premodern/BSS via the symlinked full-final runs, same identities as above;
no new model runs). Parser is strikt structureel (aantallen, LOCATION/NODATA-
grammatica, exceptiewaarden per grootheid); droge punten dragen NODATA
(bewezen exact de droge uitvoerpunten). Default-vs-premodern eist
bitgelijkheid op `.sp1` én `.sp2`; legacy-vs-BSS alleen rapportage
(`tolerances.json`: report_only — maxima inclusief energiearme cellen en
maskerverschillen zijn resultaten, geen toleranties).

| Condition | Default spectral max abs sp1/sp2 | Legacy VaDens max abs vs BSS | Legacy NDIR max abs vs BSS (deg) | Legacy mask differences |
|---|---:|---:|---:|---:|
| `u20_d310_lp300_open` | 0.000000 | 0.064900 | 16.900000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u02_d090_l0000_open` | 0.000000 | 0.000000 | 0.500000 | {'DSPRDEGR': 8, 'NDIR': 8, 'VaDens': 8} |
| `u20_d300_lm200_open` | 0.000000 | 0.019700 | 3.900000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u20_d300_lp650_open` | 0.000000 | 0.074300 | 6.400000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u20_d310_lp300_closed` | 0.000000 | 0.064900 | 11.600000 | {'DSPRDEGR': 1, 'NDIR': 1, 'VaDens': 1} |
| `u20_d120_lp300_open` | 0.000000 | 0.000010 | 0.100000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u40_d300_lp300_open` | 0.000000 | 0.075000 | 3.900000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u20_d315_lp300_open` | 0.000000 | 0.066500 | 72.000000 | {'DSPRDEGR': 0, 'NDIR': 0, 'VaDens': 0} |
| `u20_d316_lp300_open` | 0.000000 | 0.034900 | 152.300000 | {'DSPRDEGR': 3, 'NDIR': 3, 'VaDens': 3} |
| `u20_d346_lp300_open` | 0.000000 | 0.010300 | 51.300000 | {'DSPRDEGR': 2, 'NDIR': 2, 'VaDens': 2} |

Spectral gate: default-vs-premodern spectra are bit-equal in .sp1 and .sp2 in
all 10 conditions.
