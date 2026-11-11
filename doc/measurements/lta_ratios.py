#!/usr/bin/env python3
"""Net/absolute ratios of the LTA triad term, from the test build of
lta_diag.patch (the BF-63 row of doc/bug-fixes-vs-tu-delft-41.51.md).

The test build writes one line per call of `SWLTA` to `lta_diag.txt` in the
run directory: the call number, the grid point, the test-point flag, and
energy-weighted sums (each bin weighted with sigma*FRINTF*DDIR) of the
second-harmonic part, the third-harmonic part and the bracket (their sum),
net and absolute, then the bracket after the multiplication with the group
velocity, net and absolute, and its gain and loss.

    python3 doc/measurements/lta_ratios.py --bar RUN_DIR ...
        submerged bar (examples/nonlinear_interactions/triad): the last call of
        each point, with the test point (x = 15 m) printed on its own
    python3 doc/measurements/lta_ratios.py --sweeps 4 RUN_DIR ...
        test model sphys: the sum of the last four calls of each point (the
        four sweeps of the last iteration)
"""
from __future__ import annotations

import argparse
from collections import defaultdict
from pathlib import Path


def calls(run: Path) -> dict[int, list[tuple[bool, list[float]]]]:
    per_point = defaultdict(list)
    for line in (run / "lta_diag.txt").read_text().split("\n"):
        fields = line.split()
        if fields:
            per_point[int(fields[1])].append((fields[2] == "T", [float(x) for x in fields[3:13]]))
    return per_point


def span(values: list[float], fmt: str = "+.4f") -> str:
    return f"{min(values):{fmt}} .. {max(values):{fmt}}" if values else "none"


def summarise(name: str, points: dict[int, tuple[bool, list[float]]]) -> None:
    #  v = second net, abs; third net, abs; bracket net, abs; after cg net, abs; gain, loss
    for igp, (test, v) in sorted(points.items()):
        if test:
            print(f"  test point IGP {igp}: bracket {v[4] / v[5]:+.4f}; third harmonic "
                  f"{v[2] / v[3] if v[3] else 0:+.4f}; second harmonic {v[0] / v[1]:+.4f}; "
                  f"after cg {v[6] / v[7]:+.4f}; loss/gain after cg {v[9] / v[8]:.3f}")
    values = [v for _, v in points.values()]
    third = [v[2] / v[3] for v in values if v[3] > 0]
    second = [v[0] / v[1] for v in values if v[1] > 0]
    bracket = [v[4] / v[5] for v in values if v[5] > 0]
    loss_gain = [v[9] / v[8] for v in values if v[8] > 0]
    total = [sum(v[i] for v in values) for i in range(10)]
    print(f"== {name}: {len(points)} points")
    print(f"  per point: third harmonic {span(third)}; second harmonic {span(second)}; "
          f"bracket {span(bracket)}; loss/gain after cg {span(loss_gain, '.3f')}")
    print(f"  summed over the points: bracket {total[4] / total[5]:+.4f}; "
          f"loss/gain after cg {total[9] / total[8]:.3f}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--bar", action="store_true", help="last call of each point")
    mode.add_argument("--sweeps", type=int, help="sum of the last N calls of each point")
    parser.add_argument("runs", nargs="+", type=Path, help="run directories with lta_diag.txt")
    args = parser.parse_args()
    for run in args.runs:
        per_point = calls(run)
        if args.bar:
            points = {igp: c[-1] for igp, c in per_point.items()}
        else:
            n = args.sweeps
            points = {igp: (any(t for t, _ in c[-n:]), [sum(v[i] for _, v in c[-n:]) for i in range(10)])
                      for igp, c in per_point.items() if len(c) >= n and len(c) % n == 0}
        summarise(str(run), points)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
