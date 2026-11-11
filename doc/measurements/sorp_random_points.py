#!/usr/bin/env python3
"""Random output points on the so-rp grid, checked against an independent
search for the containing cell (the BF-56 and BF-57 rows of
doc/bug-fixes-vs-tu-delft-41.51.md).

The 4000 points are in `sorp_random_points.pnt`, drawn uniformly over the
bounding box of the valid grid coordinates. Each run is the corrected so-rp
deck (condition u20_d310_lp300_open) with `MXITST=1`, one thread, and the
output points replaced by this file:

    POINTS 'locaties' FILE 'sorp_random_points.pnt'
    TABLE  'locaties' ... 'dat/uitvoerpunten.tab' with only XP YP DEP

and without the SPECOUT line; for the INMESH verdicts the same with `TEST 70 0`
instead of `TEST 1 0`. The active corners come from a block output of DEPTH
on the computational grid (`BLOCK 'COMPGRID' ... 'dat/blk.mat' ... DEPTH`,
for example the BF-40 run with `MXITST=3`).

    python3 doc/measurements/sorp_random_points.py --depth-mat RUN/dat/blk.mat \\
        --run upstream=R_UPSTREAM --run fork=R_FORK [--run before57=R_DCFFBC1 ...] \\
        [--test70 upstream=R70_UPSTREAM --test70 fork=R70_FORK] [--point 2844]

The independent search: a point lies in a cell when it is inside the
quadrilateral of the cell's four corners (ray casting), searched over the
cells with four valid corner coordinates (a KD tree on the cell centres); a
cell is fully active when its four corners have a depth (not NaN) in the
block output. Per run it prints how many points in a fully active cell got
−99 and how many points outside every cell got a value; per pair of runs
the points where both give a value and differ by more than 1 mm. With
`--test70`, it counts the `crossing` lines of `INMESH` per point in PRINT and
names the points INMESH judged inside although they lie in no cell. With
`--point`, it gives the bilinear depth at that point from the corner depths.
Needs numpy and scipy, and the so-rp grid (not in git).
"""
from __future__ import annotations

import argparse
import collections
import re
from pathlib import Path

import numpy as np
import scipy.io
from scipy.spatial import cKDTree

HERE = Path(__file__).resolve().parent
REPOSITORY = HERE.parents[1]


def read_grid(path: Path, nx: int = 249, ny: int = 578):
    lines = path.read_text().splitlines()[7:]
    values = []
    for line in lines:
        rest = line[10:]
        for k in range(0, len(rest), 12):
            field = rest[k:k + 12].strip()
            if field:
                values.append(float(field))
    values = np.array(values)
    n = nx * ny
    assert len(values) == 2 * n, len(values)
    return values[:n].reshape(ny, nx), values[n:].reshape(ny, nx)


def table(run: Path) -> np.ndarray:
    rows = [l.split() for l in (run / "dat/uitvoerpunten.tab").read_text().splitlines()
            if l.strip() and not l.startswith("%")]
    return np.array([[float(v) for v in r] for r in rows])


def inside(px: float, py: float, corners) -> bool:
    crossed = False
    for k in range(4):
        x1, y1 = corners[k]
        x2, y2 = corners[(k + 1) % 4]
        if (y1 > py) != (y2 > py) and px < x1 + (py - y1) * (x2 - x1) / (y2 - y1):
            crossed = not crossed
    return crossed


def labelled(items: list[str]) -> dict[str, Path]:
    return {item.split("=", 1)[0]: Path(item.split("=", 1)[1]) for item in items}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--grid", type=Path, default=REPOSITORY / "so-rp_swan/so-rp/so-rp.rgf")
    parser.add_argument("--points", type=Path, default=HERE / "sorp_random_points.pnt")
    parser.add_argument("--depth-mat", type=Path, required=True, help="block output with Depth")
    parser.add_argument("--run", action="append", default=[], help="LABEL=RUN_DIR")
    parser.add_argument("--test70", action="append", default=[], help="LABEL=RUN_DIR with TEST 70")
    parser.add_argument("--point", type=int, action="append", default=[], help="point number to explain")
    args = parser.parse_args()

    x, y = read_grid(args.grid)
    valid = (x != 0) | (y != 0)
    depth = scipy.io.loadmat(args.depth_mat)["Depth"].astype(float)
    active = ~np.isnan(depth)
    points = np.array([[float(v) for v in l.split()] for l in args.points.read_text().splitlines()])
    J, I = np.nonzero(valid[:-1, :-1] & valid[1:, :-1] & valid[:-1, 1:] & valid[1:, 1:])
    cx = (x[J, I] + x[J + 1, I] + x[J, I + 1] + x[J + 1, I + 1]) / 4
    cy = (y[J, I] + y[J + 1, I] + y[J, I + 1] + y[J + 1, I + 1]) / 4
    tree = cKDTree(np.c_[cx, cy])
    cells = []
    for px, py in points:
        found = None
        for k in tree.query([px, py], k=12)[1]:
            j, i = J[k], I[k]
            corners = [(x[j, i], y[j, i]), (x[j, i + 1], y[j, i + 1]),
                       (x[j + 1, i + 1], y[j + 1, i + 1]), (x[j + 1, i], y[j + 1, i])]
            if inside(px, py, corners):
                found = (j, i)
                break
        cells.append(found)
    in_grid = np.array([c is not None for c in cells])
    full = np.array([c is not None and active[c[0], c[1]] and active[c[0], c[1] + 1]
                     and active[c[0] + 1, c[1]] and active[c[0] + 1, c[1] + 1] for c in cells])
    print(f"points {len(points)}; inside a cell with valid coordinates {in_grid.sum()}; "
          f"in a fully active cell {full.sum()}")

    depths = {label: table(run)[:, 2] for label, run in labelled(args.run).items()}
    for label, d in depths.items():
        print(f"{label:10s} -99 in a fully active cell: {((d < -98) & full).sum():3d}   "
              f"a value outside every cell: {((d > -98) & ~in_grid).sum():3d}   values: {(d > -98).sum()}")
    labels = list(depths)
    for n, a in enumerate(labels):
        for b in labels[n + 1:]:
            differ = (depths[a] > -98) & (depths[b] > -98) & (np.abs(depths[a] - depths[b]) > 1e-3)
            print(f"{a} against {b}: both give a value and differ by more than 1 mm at "
                  f"{differ.sum()} points", [(int(k), depths[a][k], depths[b][k]) for k in np.nonzero(differ)[0]][:10])

    key = {(round(a, 1), round(b, 1)): k for k, (a, b) in enumerate(points)}
    for label, run in labelled(args.test70).items():
        crossings, calls = collections.Counter(), collections.Counter()
        for line in (run / "PRINT").read_text(errors="replace").splitlines():
            m = re.match(r"\s*crossing (\S+) point\s+(\S+)\s+(\S+)", line)
            if m:
                k = key.get((round(float(m.group(2)), 1), round(float(m.group(3)), 1)))
                if k is None:
                    continue
                crossings[k] += 1
                if m.group(1) == "1":
                    calls[k] += 1
        judged_inside = {k for k in crossings if (crossings[k] // calls[k]) % 2 == 1}
        outside = sorted(k for k in judged_inside if not in_grid[k])
        print(f"{label}: INMESH judged {len(judged_inside)} points inside; of these {len(outside)} "
              f"lie in no cell with valid coordinates: {outside}")

    for k in args.point:
        j, i = cells[k]
        X = np.array([x[j, i], x[j, i + 1], x[j + 1, i], x[j + 1, i + 1]])
        Y = np.array([y[j, i], y[j, i + 1], y[j + 1, i], y[j + 1, i + 1]])
        s, t = 0.5, 0.5
        for _ in range(50):     # invert the bilinear map by Newton iteration
            w = np.array([(1 - s) * (1 - t), s * (1 - t), (1 - s) * t, s * t])
            ds = np.array([-(1 - t), 1 - t, -t, t])
            dt = np.array([-(1 - s), -s, 1 - s, s])
            step = np.linalg.solve([[ds @ X, dt @ X], [ds @ Y, dt @ Y]],
                                   [w @ X - points[k, 0], w @ Y - points[k, 1]])
            s, t = s - step[0], t - step[1]
        corner = np.array([depth[j, i], depth[j, i + 1], depth[j + 1, i], depth[j + 1, i + 1]])
        w = np.array([(1 - s) * (1 - t), s * (1 - t), (1 - s) * t, s * t])
        print(f"point {k} {points[k]}: cell (iy, ix) {j + 1} {i + 1}, corner depths {corner}, "
              f"bilinear {w @ corner:.5f}", {label: d[k] for label, d in depths.items()})
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
