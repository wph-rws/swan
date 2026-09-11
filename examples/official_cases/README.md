# Official TU Delft benchmark cases

End-to-end validation against the benchmark cases published at
<https://swanmodel.sourceforge.io/download/download.htm>.
Each analytic case compares a SWAN transect table with the analytical
solution shipped in the tarball (Ris 1997 for refraction and current).

## No vendored inputs

The download page grants testing use but not redistribution, so the
tarballs are **downloaded at test time**, verified against the SHA-256
hashes in `cases.json`, and never committed. A failed download or hash
mismatch is an error, never a silent green run. `run.py` keeps a cache
(the work directory); pass `--keep` to retain staged case directories.

## Gates

| case | deck | gate |
|---|---|---|
| `refrac` | `a11refr.swn` | Hs ≤ 5e-3 m on all wet points, direction ≤ 0.5° where depth > 1 m, plus **bit-exact** agreement with the Delft-shipped 41.51 table |
| `shoal01` | `a21sho01.swn` | same analytic tolerances (rectangular sloping beach); Delft table reported |
| `slancur` | `a33curs.swn` | same analytic tolerances (slanting current); Delft table reported |

Dry transect points (SWAN exception value, Hs ≤ −9) are masked, exactly
like the matrix rule; the shoreline direction turn is excluded below 1 m
depth because the analytic direction turns near-singularly there while any
gridded model — including Delft's own binary — takes the discrete turn.

## Open (not gated)

`ring` ships no analytical solution and `Haringvliet` needs buoy-observation
criteria; both stay open work. The unstructured shoal
variants (`a21sho02/04`) and the stepwise beach (`a21sho03`, no analytic
solution) are staged the same way when their gates are defined.

## Run

```sh
python3 examples/official_cases/run.py --swan-executable build/bin/swan.exe \
    --work-directory /tmp/official-runs
python3 -m pytest tests/test_official_cases.py -q
```
