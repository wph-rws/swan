#!/usr/bin/env python3
"""Volledige MPI-veldcontrole: vergelijk eerst nat/droogmaskers, dan waarden.

Gebruik:
    python3 scripts/check_mpi_field_mask.py --serial serial.mat --mpi mpi.mat
    python3 scripts/check_mpi_field_mask.py --self-test

De doorsnede van maskers mag de 174 cellen niet verbergen: maskerverschillen
worden altijd expliciet gemeld en falen, ook als puntresultaten gelijk zijn.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "so-rp_swan" / "comparison"))
from result_stats import compare, read_hsig_field  # noqa: E402


def check(serial: Path, mpi: Path) -> int:
    try:
        serial_field = read_hsig_field(serial)
        mpi_field = read_hsig_field(mpi)
    except (FileNotFoundError, ValueError) as exc:
        print(f"veldcontrole faalt: {exc}", file=sys.stderr)
        return 1
    print(f"serieel: {serial_field.wet_count} nat / {serial_field.dry_count} droog")
    print(f"MPI:     {mpi_field.wet_count} nat / {mpi_field.dry_count} droog")
    try:
        stats = compare(serial_field, mpi_field)
    except ValueError as exc:
        print(f"VELDMASKER VERSCHILT (geen waardenvergelijking): {exc}", file=sys.stderr)
        return 1
    print(f"maskers gelijk; bias={stats.bias:.6f} rms={stats.rms:.6f} max={stats.maximum_absolute:.6f} n={stats.count}")
    return 0


def self_test() -> int:
    import numpy as np
    from scipy.io import savemat

    tmp = Path("/tmp/swan-veldmasker-selftest")
    tmp.mkdir(exist_ok=True)
    serial = tmp / "serial.mat"
    mpi = tmp / "mpi.mat"
    savemat(serial, {"Hsig": np.array([[1.0, 2.0], [3.0, float("nan")]])})
    savemat(mpi, {"Hsig": np.array([[1.0, 2.0], [float("nan"), float("nan")]])})
    # 174-analoog: één cel droog in MPI terwijl serieel nat -> moet falen
    result = check(serial, mpi)
    if result == 0:
        print("self-test faalt: maskerverschil werd groen", file=sys.stderr)
        return 1
    savemat(mpi, {"Hsig": np.array([[1.0, 2.0], [3.0, float("nan")]])})
    if check(serial, mpi) != 0:
        print("self-test faalt: gelijke maskers werden rood", file=sys.stderr)
        return 1
    print("check_mpi_field_mask self-test groen")
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
        parser.error("--serial en --mpi zijn vereist (of --self-test)")
    return check(Path(arguments.serial), Path(arguments.mpi))


if __name__ == "__main__":
    raise SystemExit(main())
