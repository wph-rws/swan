#!/usr/bin/env python3
"""Exercise the retained historical switch.py command-line interface."""

from __future__ import annotations

import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SWITCH = ROOT / "switch.py"


def run(*arguments: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SWITCH), *arguments],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )


def main() -> int:
    translated = run(
        "--cmake-args", "-mpi", "-jac", "-fixfront", "-coh", "-esmf"
    )
    expected = "-DESMF=ON -DJAC=ON -DFFRO=ON -DMPI=ON -DCOH=ON"
    if translated.returncode or translated.stdout.strip() != expected:
        raise SystemExit(f"unexpected CMake translation: {translated.stdout!r}")

    incompatible = run("--cmake-args", "-esmf", "-adcirc")
    if incompatible.returncode == 0:
        raise SystemExit("historically incompatible switches were accepted")

    with tempfile.TemporaryDirectory() as directory:
        copied = run(
            "-mpi",
            "--output-dir",
            directory,
            "src/swan_kinds.f90",
        )
        destination = Path(directory) / "swan_kinds.f90"
        if copied.returncode or destination.read_bytes() != (
            ROOT / "src/swan_kinds.f90"
        ).read_bytes():
            raise SystemExit("legacy source-copy interface changed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
