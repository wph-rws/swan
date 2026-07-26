# Matrix validation

Validation date: 2026-07-25. Condition:
`u20_d310_lp300_open` (20 m/s from 310°, NAP +3.00 m, open barrier), eight
OpenMP threads, `MXITST=50`.

The manifest contains ten conditions and four targets (40 entries). Every
default- and legacy-physics deck was generated and structurally validated. The
reference condition was then run end-to-end for all four targets. All runs
returned status zero, wrote `norm_end`, `PRINT`, block, table and both spectrum
files, and passed the explicit normal-end and nonempty-output checks.

| Target | Executable SHA-256 (prefix) | Wet points | Mean point Hsig | Wall time |
|---|---|---:|---:|---:|
| pre-modernization 41.51 | `ed0a45d87aff` | 132 (3 dry) | 1.645319 m | 163.76 s |
| BSS 41.31A.1 | `c392f287bc3f` | 132 (3 dry) | 1.290126 m | 76.60 s |
| current 41.51 default | `ca52afaa7ecc` | 132 (3 dry) | 1.645319 m | 190.97 s |
| current 41.51 legacy defaults | `ca52afaa7ecc` | 132 (3 dry) | 1.291079 m | 162.54 s |

The current executable was rebuilt from the working tree before the final two
runs. Current 41.51 default versus the distinct pre-modernization executable is
exactly equal over all 71,685 wet field cells and all 132 wet output points:
bias, RMS and maximum absolute Hsig difference are all zero.

Current 41.51 with `GEN3 KOMEN DRAG FIT` and the explicit old triad defaults
versus BSS has field bias −0.000380 m, RMS 0.004080 m and maximum absolute
difference 0.104611 m. At the 132 wet requested points the bias is +0.000953 m,
RMS 0.005644 m and maximum absolute difference 0.017770 m. This is the already
documented version/implementation residual, not a modernization regression.

Run-to-run behavior is already pinned by the project regression evidence:
this structured so-rp reference is stable, while the unstructured OpenMP case
has an approximately 2e-4 relative last-digit spread. The runner exposes
`--repetitions` and records each repeat separately so that spread can be
remeasured for any new condition or executable. The remaining 36 manifest
entries are the formal migration/sign-off matrix; they were intentionally not
run as part of this implementation validation.
