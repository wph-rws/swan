#!/usr/bin/env python3
"""Compare the saved Hs fields of the BF-62 runs made by bf62_runs.py.

    python3 doc/measurements/bf62_compare.py WORK                 # the comparisons of the BF-62 row
    python3 doc/measurements/bf62_compare.py WORK RUN_A RUN_B     # one pair

For each pair it prints the number of points compared and the largest, 99th
percentile and mean absolute difference of Hs in mm. The box models compare
every point that is wet in both runs (their block files print four
significant digits, so 1 mm is the resolution); the Voordelta runs only the
points deeper than 1 m (`voordelta_depth.blk`). The thread check compares
`voordelta_hs.blk` and `voordelta_sites.tbl` byte for byte, and the runs
without limiter give their first and last balance residual.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

import numpy as np


def field(run: Path):
    for name in ("hs.blk", "voordelta_hs.blk"):
        if (run / name).exists():
            return np.array((run / name).read_text().split(), float)
    return None


def compare(runs: Path, a: str, b: str) -> str:
    A, B = field(runs / a), field(runs / b)
    if A is None or B is None:
        return f"{a} vs {b}: missing"
    if A.shape != B.shape:
        return f"{a} vs {b}: different grids"
    mask = (A > 0) & (B > 0)
    depth = runs / b / "voordelta_depth.blk"
    if depth.exists():
        mask &= np.array(depth.read_text().split(), float) > 1.0
    d = np.abs(A - B)[mask] * 1000
    return (f"{a} vs {b}: n={mask.sum()} max {d.max():.2f} mm, p99 {np.percentile(d, 99):.2f} mm, "
            f"mean {d.mean():.2f} mm, mean signed {((A - B)[mask] * 1000).mean():+.2f} mm, "
            f"equal as printed {np.array_equal(A, B)}")


def residual(runs: Path, name: str) -> str:
    summary = runs / name / "summary.json"
    if not summary.exists():
        return f"{name}: missing"
    values = json.loads(summary.read_text())["residual"]
    return f"{name}: {len(values)} iterations, residual first {values[0]}, last {values[-1]}"


def report(runs: Path) -> None:
    pairs = [("sq_ref_up_a01_2000", "sq_ref_q2a_2000"), ("sq_ref_up_2000", "sq_ref_q2a_2000"),
             ("sq_ref_rhs_a01_2000", "sq_ref_q2a_2000")]
    for model in ("chan", "slope"):
        pairs.append((f"{model}_fix_40", f"{model}_ref_fix_1000"))
        pairs += [(f"{model}_q2a_{n}", f"{model}_ref_q2a_1000") for n in (40, 80, 160, 320)]
        pairs += [(f"{model}_q2n_{n}", f"{model}_ref_q2a_1000") for n in (40, 320)]
        pairs.append((f"{model}_ref_fix_1000", f"{model}_ref_q2a_1000"))
    pairs += [("vd_fix_40", "vd_ref_fix_2000"), ("vd_q2a_80", "vd_ref_q2a_2000"),
              ("vd_q2a_160", "vd_ref_q2a_2000"), ("vd_q2a_80", "vd_ref_fix_2000"),
              ("vd_q2a_160", "vd_ref_fix_2000"), ("vd_ref_fix_2000", "vd_ref_q2a_2000")]
    for a, b in pairs:
        print(compare(runs, a, b))
    for name in ("sq_ref_up_a01_2000", "sq_ref_up_2000", "sq_ref_rhs_a01_2000",
                 "chan_fix_nolim_200", "slope_fix_nolim_200", "sq_fix_nolim_200"):
        print(residual(runs, name))
    first = runs / "vd_det_fix_t1"
    for threads in (2, 4, 8):
        other = runs / f"vd_det_fix_t{threads}"
        same = [(first / f).exists() and (other / f).exists()
                and (first / f).read_bytes() == (other / f).read_bytes()
                for f in ("voordelta_hs.blk", "voordelta_sites.tbl")]
        print(f"vd_det_fix_t{threads} against 1 thread: byte-identical {all(same)}")


def main(argv: list[str]) -> int:
    if len(argv) not in (1, 3) or argv[0] in ("-h", "--help"):
        print(__doc__)
        return 0 if argv and argv[0] in ("-h", "--help") else 2
    runs = Path(argv[0]) / "runs"
    if len(argv) == 3:
        print(compare(runs, argv[1], argv[2]))
    else:
        report(runs)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
