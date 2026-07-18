#!/usr/bin/env python3
"""Build-independent runner for the SWAN quick test."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path


GENERATED_FILES = (
    "INPUT",
    "PRINT",
    "Errfile",
    "ERRPTS",
    "norm_end",
    "swaninit",
    "quick_test.prt",
    "quick_test.erf",
    "quick_test_hs.blk",
    "quick_test_center.tbl",
)


def find_executable(case_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")

    local_build = (case_directory / ".." / ".." / "build" / "bin" / "swan.exe").resolve()
    if local_build.is_file():
        return local_build

    installed = shutil.which("swan.exe")
    if installed:
        return Path(installed).resolve()
    raise FileNotFoundError(
        "SWAN executable not found. Build the repository first or pass "
        "--swan-executable /path/to/swan.exe."
    )


def run(executable: Path, case_directory: Path) -> None:
    for name in GENERATED_FILES:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / "quick_test.swn", input_file)
    started = time.monotonic()
    try:
        result = subprocess.run([executable], cwd=case_directory, check=False)
    finally:
        input_file.unlink(missing_ok=True)
    elapsed = time.monotonic() - started

    print_file = case_directory / "PRINT"
    if print_file.exists():
        print_file.replace(case_directory / "quick_test.prt")
    error_file = case_directory / "Errfile"
    if error_file.exists():
        error_file.replace(case_directory / "quick_test.erf")

    if result.returncode:
        raise RuntimeError(
            f"SWAN stopped with exit code {result.returncode}. "
            "Inspect quick_test.prt and quick_test.erf."
        )
    if not (case_directory / "norm_end").is_file():
        raise RuntimeError(
            "SWAN did not create norm_end. Inspect quick_test.prt and quick_test.erf."
        )

    print(f"Quick test completed normally in {elapsed:.2f} seconds.")
    print("Results: quick_test_center.tbl, quick_test_hs.blk, quick_test.prt")
    if elapsed > 180:
        print("Warning: the run took longer than the intended three-minute budget.")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--swan-executable",
        help="path to swan.exe (default: build/bin/swan.exe or PATH)",
    )
    arguments = parser.parse_args()
    case_directory = Path(__file__).resolve().parent

    try:
        executable = find_executable(case_directory, arguments.swan_executable)
        run(executable, case_directory)
    except (FileNotFoundError, OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
