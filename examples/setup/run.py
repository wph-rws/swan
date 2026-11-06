#!/usr/bin/env python3
"""Build-independent runner for the SWAN wave-induced set-up test.

Two decks on the same sloping beach: setup2d.swn reaches SETUPP and the
Poisson solver SETUP2D, setup1d.swn the one-dimensional branch of SETUPP.
No other registered case uses SETUP.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference  # noqa: E402

#  Each deck with the input it reads and the output that is compared.
CASES = {
    "setup2d": ("slope.bot", ("setup2d.tbl", "setup2d.blk")),
    "setup1d": ("slope1d.bot", ("setup1d.tbl",)),
}
COMMON_FILES = ("INPUT", "PRINT", "Errfile", "ERRPTS", "norm_end", "swaninit")


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


def run_case(executable: Path, case_directory: Path, name: str,
             reference_name: str, smoke: bool) -> float:
    outputs = CASES[name][1]
    for generated in COMMON_FILES + outputs + (f"{name}.prt", f"{name}.erf"):
        path = case_directory / generated
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / f"{name}.swn", input_file)
    started = time.monotonic()
    try:
        result = subprocess.run([str(executable)], cwd=case_directory, check=False)
    finally:
        input_file.unlink(missing_ok=True)
    elapsed = time.monotonic() - started

    print_file = case_directory / "PRINT"
    if print_file.exists():
        print_file.replace(case_directory / f"{name}.prt")
    error_file = case_directory / "Errfile"
    if error_file.exists():
        error_file.replace(case_directory / f"{name}.erf")

    if result.returncode:
        raise RuntimeError(
            f"{name}: SWAN stopped with exit code {result.returncode}. "
            f"Inspect {name}.prt and {name}.erf."
        )
    if not (case_directory / "norm_end").is_file():
        raise RuntimeError(
            f"{name}: SWAN did not create norm_end. Inspect {name}.prt and {name}.erf."
        )

    reference_directory = Path(__file__).resolve().parent / reference_name
    compare_with_reference(case_directory, reference_directory, outputs, smoke=smoke)
    return elapsed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--swan-executable",
        help="path to swan.exe (default: build/bin/swan.exe or PATH)",
    )
    parser.add_argument(
        "--work-directory",
        help="run in this directory instead of writing results beside the example",
    )
    parser.add_argument(
        "--reference",
        default="reference",
        help="reference directory beside this script (default: reference)",
    )
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="run only, no regression comparison; does not count as a passed regression test",
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
            for name, (bottom, _) in CASES.items():
                for source in (f"{name}.swn", bottom):
                    shutil.copy2(source_directory / source, case_directory / source)
        total = 0.0
        for name in CASES:
            total += run_case(executable, case_directory, name,
                              arguments.reference, arguments.smoke)
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    if arguments.smoke:
        print("Smoke mode: SWAN ran normally; no regression comparison.")
    else:
        print("Results match the stored reference.")
    print(f"Set-up test completed normally in {total:.2f} seconds.")
    if total > 180:
        print("Warning: the run took longer than the intended three-minute budget.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
