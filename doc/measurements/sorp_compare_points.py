#!/usr/bin/env python3
"""Compare so-rp runs point by point with a reference run (RWS 41.31A.1).

Reads the table `dat/uitvoerpunten.tab` (Hsig in column 4) of the reference
run and of every run given, keeps the output points that are wet (Hsig > 0)
in all of them, and prints per run the number of wet points, the mean Hsig
over the common wet points, and the mean, largest and rms difference against
the reference. Used for the LTA chain (sorp_lta_chain.py).

    python3 doc/measurements/sorp_compare_points.py --reference RWS_RUN_DIR RUN_DIR ...
"""
from __future__ import annotations

import argparse
import math
from pathlib import Path


def hsig(table: Path) -> list[tuple[str, str, float]]:
    rows = []
    for line in table.read_text().split("\n"):
        if line.startswith("%") or not line.strip():
            continue
        fields = line.split()
        rows.append((fields[0], fields[1], float(fields[3])))
    return rows


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--reference", type=Path, required=True, help="run directory of the reference")
    parser.add_argument("runs", nargs="+", type=Path, help="run directories to compare")
    args = parser.parse_args()
    names = ["reference"] + [run.name for run in args.runs]
    data = [hsig(d / "dat/uitvoerpunten.tab") for d in [args.reference, *args.runs]]
    n = len(data[0])
    assert all(len(rows) == n for rows in data), "the tables have a different number of points"
    for rows in data:
        assert [r[:2] for r in rows] == [r[:2] for r in data[0]], "the tables list other points"
    common = [i for i in range(n) if all(rows[i][2] > 0 for rows in data)]
    print(f"output points {n}; wet in every run listed: {len(common)}")
    reference = [data[0][i][2] for i in common]
    mean_reference = sum(reference) / len(reference)
    for name, rows in zip(names, data):
        h = [rows[i][2] for i in common]
        wet = sum(1 for r in rows if r[2] > 0)
        mean = sum(h) / len(h)
        d = [a - b for a, b in zip(h, reference)]
        largest = max(abs(x) for x in d)
        rms = math.sqrt(sum(x * x for x in d) / len(d))
        print(f"{name:22s} wet {wet:3d}  mean {mean:.4f} m  against the reference: "
              f"mean {100 * (mean - mean_reference):+.2f} cm, largest {1000 * largest:.3f} mm, "
              f"rms {1000 * rms:.3f} mm")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
