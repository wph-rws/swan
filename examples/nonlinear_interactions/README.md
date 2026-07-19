# Nonlinear wave-interaction examples

This suite makes the nonlinear spectral source terms visible and exercises the
alternative algorithms in SWAN 41.51. It contains three deliberately focused
one-dimensional cases:

- `quad` is a 100 km, 50 m deep fetch with a 15 m/s following wind. Runs with
  quadruplets disabled, DIA integrated per sweep (`IQUAD=2`) and DIA integrated
  per iteration (`IQUAD=3`) show how four-wave interactions reshape a developing
  directional wind-sea spectrum. An exact XNL run (`IQUAD=51`) is included as a
  slow reference.
- `triad` propagates a small incident spectrum over a synthetic submerged bar.
  Runs with triads disabled, DCTA and the new FTIM method isolate the transfer to
  superharmonics in shallow water. Wind input, whitecapping and quadruplets are
  disabled in all three variants.
- `combined` follows wind-wave growth over a 40 km deep section and subsequent
  transformation down a slope to 2 m water depth. DIA per iteration and FTIM
  are active together, along with breaking and bottom friction.

These are compact functional and regression examples, not calibrated physical
models. The submerged bar follows the type of flume configuration used to
verify spectral triad models, but it is not a reproduction of a specific
laboratory experiment. The rationale and algorithms are documented in the
[SWAN scientific and technical manual](https://swanmodel.sourceforge.io/download/zip/swantech.pdf),
and exact command options are in the
[SWAN physics command reference](https://swanmodel.sourceforge.io/online_doc/swanuse/node28.html).

## Build and run

From the repository root:

```sh
cmake -S . -B build -GNinja -DCMAKE_Fortran_COMPILER=gfortran
cmake --build build --parallel
python3 examples/nonlinear_interactions/run.py
```

The default `standard` selection runs seven fast variants and normally takes
only a few seconds. Run an individual group with `--case quad`, `--case triad`
or `--case combined`.

The exact XNL calculation evaluates the nonlinear Boltzmann integral and is
intentionally excluded from the default. On the development machine it took
about 90 seconds, compared with less than a second for DIA, and temporarily
created a 27 MB lookup file. Run it separately or include it in the whole suite:

```sh
python3 examples/nonlinear_interactions/run.py --case xnl
python3 examples/nonlinear_interactions/run.py --case all
```

Use `--swan-executable /path/to/swan.exe` for another build and `--no-plots` if
Matplotlib is unavailable or only numerical validation is wanted.

Every `.swn` file can also be run without Python: enter its directory, copy it
to `INPUT`, and start a serial `swan.exe`. The Python runner is recommended
because it isolates runs in temporary directories and checks all outputs.

## Validation and results

For each variant, the runner checks normal completion, the integral-parameter
table, all one-dimensional spectra and the iteration-by-iteration source-term
file. It then verifies that:

- disabled interaction terms report zero integrated transfer;
- DIA and XNL report nonzero quadruplet transfer;
- DCTA and FTIM report nonzero triad transfer;
- DIA measurably changes the spectrum after 100 km of fetch;
- DCTA and FTIM measurably change the spectrum behind the submerged bar;
- both nonlinear source terms are active in the combined case.

Generated files are stored below `results/<variant>/`. The most useful summary
products are:

- `quad_comparison.png`: down-fetch wave growth and spectra with and without DIA;
- `triad_comparison.png`: DCTA/FTIM harmonic transfer across the bar;
- `combined_evolution.png`: ocean-to-nearshore spectral evolution;
- `validation.txt`: quantitative comparisons from the latest selected run;
- `*.tbl`, `*.spc` and `*_sources.spc`: raw SWAN tables, spectra and source terms;
- `*.prt` and `console.log`: convergence diagnostics and screen output.

The QUAD cases use a full directional circle and approximately 10% logarithmic
frequency resolution. This is intentional: the SWAN manual warns that DIA is a
poor approximation for long-crested waves and spectral resolutions far from
10%. FTIM was introduced in SWAN 41.51; see the
[release notes](https://swanmodel.sourceforge.io/modifications/modifications.htm).
