#!/usr/bin/env python3
"""Build-independent runner for the SWAN quick test."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference  # noqa: E402


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


def run(
    executable: Path,
    case_directory: Path,
    mpi_executable: str | None = None,
    mpi_processes: int = 1,
    mpi_numproc_flag: str = "-n",
) -> None:
    for name in GENERATED_FILES:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / "quick_test.swn", input_file)
    command = [str(executable)]
    if mpi_processes > 1:
        launcher = mpi_executable or shutil.which("mpiexec")
        if not launcher:
            raise FileNotFoundError(
                "MPI launcher not found; pass --mpi-exec /path/to/mpiexec."
            )
        command = [launcher, mpi_numproc_flag, str(mpi_processes), str(executable)]
    started = time.monotonic()
    try:
        result = subprocess.run(command, cwd=case_directory, check=False)
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

    reference_directory = Path(__file__).resolve().parent / "reference"
    if compare_with_reference(case_directory, reference_directory,
                              ("quick_test_center.tbl", "quick_test_hs.blk")):
        print("Results match the stored reference.")

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
    parser.add_argument(
        "--work-directory",
        help="run in this directory instead of writing results beside the example",
    )
    parser.add_argument("--mpi-exec", help="MPI launcher, for example mpiexec")
    parser.add_argument(
        "--mpi-processes",
        type=int,
        default=1,
        help="number of MPI processes (default: 1)",
    )
    parser.add_argument(
        "--mpi-numproc-flag",
        default="-n",
        help="launcher option used before the process count (default: -n)",
    )
    arguments = parser.parse_args()
    source_directory = Path(__file__).resolve().parent
    case_directory = (
        Path(arguments.work_directory).expanduser().resolve()
        if arguments.work_directory
        else source_directory
    )

    try:
        executable = find_executable(source_directory, arguments.swan_executable)
        if case_directory != source_directory:
            case_directory.mkdir(parents=True, exist_ok=True)
            for name in ("quick_test.swn", "bottom.bot"):
                shutil.copy2(source_directory / name, case_directory / name)
        if arguments.mpi_processes < 1:
            raise ValueError("--mpi-processes must be positive")
        run(
            executable,
            case_directory,
            arguments.mpi_exec,
            arguments.mpi_processes,
            arguments.mpi_numproc_flag,
        )
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
