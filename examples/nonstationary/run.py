#!/usr/bin/env python3
"""Run the compact regular and unstructured nonstationary examples."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path


CASES = {
    "regular": "nonstationary_regular",
    "unstructured": "nonstationary_unstructured",
}

SWAN_WORK_FILES = (
    "INPUT",
    "PRINT",
    "Errfile",
    "ERRPTS",
    "norm_end",
    "swaninit",
)


def find_executable(example_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")

    repository = example_directory.parent.parent
    local_build = repository / "build" / "bin" / "swan.exe"
    if local_build.is_file():
        return local_build.resolve()

    installed = shutil.which("swan.exe")
    if installed:
        return Path(installed).resolve()
    raise FileNotFoundError(
        "SWAN executable not found. Build the repository first or pass "
        "--swan-executable /path/to/swan.exe."
    )


def clean_case(case_directory: Path, basename: str) -> None:
    generated = (
        *SWAN_WORK_FILES,
        f"{basename}.prt",
        f"{basename}.erf",
        f"{basename}_hs.blk",
        f"{basename}_center.tbl",
    )
    for name in generated:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()


def run_case(executable: Path, example_directory: Path, case: str) -> float:
    basename = CASES[case]
    case_directory = example_directory / case
    clean_case(case_directory, basename)
    shutil.copyfile(case_directory / f"{basename}.swn", case_directory / "INPUT")

    started = time.monotonic()
    try:
        result = subprocess.run([executable], cwd=case_directory, check=False)
    finally:
        (case_directory / "INPUT").unlink(missing_ok=True)
    elapsed = time.monotonic() - started

    print_file = case_directory / "PRINT"
    if print_file.exists():
        print_file.replace(case_directory / f"{basename}.prt")
    error_file = case_directory / "Errfile"
    if error_file.exists():
        error_file.replace(case_directory / f"{basename}.erf")

    if result.returncode:
        raise RuntimeError(
            f"{case} case stopped with exit code {result.returncode}; inspect "
            f"{case}/{basename}.prt and .erf"
        )
    if not (case_directory / "norm_end").is_file():
        raise RuntimeError(
            f"{case} case did not create norm_end; inspect {case}/{basename}.prt"
        )
    for suffix in ("_hs.blk", "_center.tbl"):
        if not (case_directory / f"{basename}{suffix}").is_file():
            raise RuntimeError(f"{case} case did not create {basename}{suffix}")
    return elapsed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--case",
        choices=("all", *CASES),
        default="all",
        help="example to run (default: %(default)s)",
    )
    parser.add_argument(
        "--swan-executable",
        help="path to swan.exe (default: build/bin/swan.exe or PATH)",
    )
    arguments = parser.parse_args()
    example_directory = Path(__file__).resolve().parent

    try:
        executable = find_executable(example_directory, arguments.swan_executable)
        selected = CASES if arguments.case == "all" else (arguments.case,)
        for case in selected:
            elapsed = run_case(executable, example_directory, case)
            print(f"{case.capitalize()} case completed normally in {elapsed:.2f} seconds.")
            print(f"Results: {example_directory / case}")
    except (FileNotFoundError, OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
