#!/usr/bin/env python3
"""Full MPI field check: compare the wet/dry masks first, then the values.

Usage:
    python3 scripts/check_mpi_field_mask.py --serial serial.mat --mpi mpi.mat
    python3 scripts/check_mpi_field_mask.py --self-test

Intersecting the masks must not hide the 174 cells: mask differences are
always reported explicitly and fail, even when the point results are equal.
"""
from __future__ import annotations

import argparse
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "so-rp_swan" / "comparison"))
from result_stats import compare, read_hsig_field  # noqa: E402


def check(serial: Path, mpi: Path) -> int:
    try:
        serial_field = read_hsig_field(serial)
        mpi_field = read_hsig_field(mpi)
    except (FileNotFoundError, ValueError) as exc:
        print(f"field check fails: {exc}", file=sys.stderr)
        return 1
    print(f"serial:  {serial_field.wet_count} wet / {serial_field.dry_count} dry")
    print(f"MPI:     {mpi_field.wet_count} wet / {mpi_field.dry_count} dry")
    try:
        stats = compare(serial_field, mpi_field)
    except ValueError as exc:
        print(f"FIELD MASK DIFFERS (values not compared): {exc}", file=sys.stderr)
        return 1
    print(f"masks equal; bias={stats.bias:.6f} rms={stats.rms:.6f} max={stats.maximum_absolute:.6f} n={stats.count}")
    return 0


def self_test() -> int:
    #  A private directory, so that parallel ctest runs or another user's
    #  leftovers cannot interfere.
    with tempfile.TemporaryDirectory(prefix="swan-field-mask-selftest-") as tmp:
        return _self_test(Path(tmp))


def _self_test(tmp: Path) -> int:
    import numpy as np
    from scipy.io import savemat

    serial = tmp / "serial.mat"
    mpi = tmp / "mpi.mat"
    savemat(serial, {"Hsig": np.array([[1.0, 2.0], [3.0, float("nan")]])})
    savemat(mpi, {"Hsig": np.array([[1.0, 2.0], [float("nan"), float("nan")]])})
    # Like the 174 cells: one cell dry in MPI while wet in serial -> must fail
    result = check(serial, mpi)
    if result == 0:
        print("self-test fails: a mask difference came out green", file=sys.stderr)
        return 1
    savemat(mpi, {"Hsig": np.array([[1.0, 2.0], [3.0, float("nan")]])})
    if check(serial, mpi) != 0:
        print("self-test fails: equal masks came out red", file=sys.stderr)
        return 1
    print("check_mpi_field_mask self-test green")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--serial")
    parser.add_argument("--mpi")
    parser.add_argument("--self-test", action="store_true")
    arguments = parser.parse_args()
    if arguments.self_test:
        return self_test()
    if not arguments.serial or not arguments.mpi:
        parser.error("--serial and --mpi are required (or --self-test)")
    return check(Path(arguments.serial), Path(arguments.mpi))


if __name__ == "__main__":
    raise SystemExit(main())
