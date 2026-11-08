#!/usr/bin/env python3
"""Gate: every grid point is computed and written, however the grid is split.

The Jacobi build (JAC) splits the grid by Orthogonal Recursive Bisection, in
both directions, so with four processes several subdomains lie along the same
outer edge. Upstream decided from the process number which subdomain reaches
an edge, which only holds for strips. On the quick test with four processes,
upstream never computed the east edge of one subdomain and a point inside the
domain (Hs 0), and never collected the west edge of another subdomain for
output (Hs -9) (BF-68).

This gate runs the quick test on one process and on four, and checks that the
block output holds no exception value, that the four-process run agrees with
the one-process run, and that the check of the frequency range counts all 231
wet points in both. With the wavefront build the runs must be identical; the
Jacobi scheme converges to a slightly different state when split, so there a
difference of a few centimetres is allowed (the reference-jac of the quick
test, on two processes, already differs by 0.9 cm).

Use as a gate:
    python3 tests/test_mpi_edges.py --swan-executable EXE --mpi-exec mpiexec
        [--jacobi] --work-directory DIR
Without binaries, pytest runs the plain ``test_*`` functions.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CASE = REPO / "examples" / "quick_test"
SHAPE = (11, 21)          # MYC+1 rows of MXC+1 points
WET_POINTS = 231
JACOBI_TOLERANCE = 0.05   # m; the split runs differ by up to 2.2 cm with 8 processes


def stage(work: Path, low_fhigh: bool) -> Path:
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    shutil.copy2(CASE / "bottom.bot", work / "bottom.bot")
    deck = (CASE / "quick_test.swn").read_text()
    if low_fhigh:
        #  a highest frequency below 2.5 times the peak everywhere, so that the
        #  warning names the number of wet points counted
        deck = deck.replace("CIRCLE 24 0.05 1.0 16", "CIRCLE 24 0.05 0.2 16", 1)
    (work / "INPUT").write_text(deck)
    return work


def read_block(path: Path) -> list[list[float]]:
    values = [float(v) for v in path.read_text().split()]
    rows, columns = SHAPE
    if len(values) != rows * columns:
        raise ValueError(f"{path} holds {len(values)} values, expected {rows * columns}")
    return [values[i * columns:(i + 1) * columns] for i in range(rows)]


def exception_points(block: list[list[float]]) -> list[tuple[int, int]]:
    return [(ix, iy) for iy, row in enumerate(block) for ix, v in enumerate(row) if v < -8.]


def largest_difference(a: list[list[float]], b: list[list[float]]) -> float:
    return max(abs(x - y) for ra, rb in zip(a, b) for x, y in zip(ra, rb))


def counted_points(work: Path) -> int | None:
    text = "".join(p.read_text(errors="replace") for p in sorted(work.glob("PRINT*")))
    found = re.search(r"at \d+ of (\d+) wet grid points", text)
    return int(found.group(1)) if found else None


def run(swan: str, mpi: str, flag: str, processes: int, work: Path) -> None:
    command = [mpi, flag, str(processes), swan]
    result = subprocess.run(command, cwd=work, capture_output=True, text=True, timeout=300)
    if result.returncode != 0:
        raise RuntimeError(f"{' '.join(command)} failed in {work}:\n{result.stderr[-2000:]}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--processes", type=int, default=4)
    parser.add_argument("--jacobi", action="store_true",
                        help="the build uses the Jacobi exchange (-DJAC=ON)")
    parser.add_argument("--work-directory", default=None)
    args = parser.parse_args()
    base = Path(args.work_directory or tempfile.mkdtemp(prefix="swan-mpi-edges-"))

    blocks, failures = {}, []
    for processes in (1, args.processes):
        work = stage(base / f"np{processes}", low_fhigh=False)
        run(args.swan_executable, args.mpi_exec, args.mpi_numproc_flag, processes, work)
        blocks[processes] = read_block(work / "quick_test_hs.blk")
        bad = exception_points(blocks[processes])
        if bad:
            failures.append(f"{processes} processes: exception value at (ix, iy) {bad}")
        work = stage(base / f"np{processes}-count", low_fhigh=True)
        run(args.swan_executable, args.mpi_exec, args.mpi_numproc_flag, processes, work)
        counted = counted_points(work)
        if counted != WET_POINTS:
            failures.append(f"{processes} processes: {counted} wet points counted, expected {WET_POINTS}")

    difference = largest_difference(blocks[1], blocks[args.processes])
    allowed = JACOBI_TOLERANCE if args.jacobi else 0.
    if difference > allowed:
        failures.append(f"Hs on {args.processes} processes differs by {difference:.4f} m "
                        f"from one process (allowed {allowed} m)")
    for failure in failures:
        print("FAIL:", failure)
    if not failures:
        print(f"all {WET_POINTS} points computed and written on 1 and {args.processes} "
              f"processes; largest difference {difference:.4f} m")
    return 1 if failures else 0


def test_exception_points_are_found():
    block = [[0.5] * SHAPE[1] for _ in range(SHAPE[0])]
    block[3][20] = -9.
    assert exception_points(block) == [(20, 3)]


def test_read_block_rejects_a_wrong_size(tmp_path):
    path = tmp_path / "hs.blk"
    path.write_text("1.0 2.0\n")
    try:
        read_block(path)
    except ValueError:
        return
    raise AssertionError("a block of the wrong size was accepted")


def test_counted_points_reads_the_warning(tmp_path):
    (tmp_path / "PRINT-001").write_text(
        " ** Warning          : frequency grid too short: at 219 of 219 wet grid points\n")
    assert counted_points(tmp_path) == 219


if __name__ == "__main__":
    sys.exit(main())
