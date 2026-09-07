#!/usr/bin/env python3
"""Run the compact regular and unstructured nonstationary examples."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference  # noqa: E402


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


def run_case(executable: Path, example_directory: Path, case: str,
             reference_name: str = "reference", smoke: bool = False) -> float:
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

    reference_directory = Path(__file__).resolve().parent / case / reference_name
    #  Only the unstructured case has a variant-specific reference; the
    #  structured cases traverse the grid the same way whatever the switch.
    if not reference_directory.is_dir():
        reference_directory = Path(__file__).resolve().parent / case / "reference"
    names = tuple(f"{basename}{suffix}" for suffix in ("_center.tbl", "_hs.blk"))
    if smoke:
        compare_with_reference(case_directory, reference_directory, names, smoke=True)
        print(f"{case} smoke: normaal gedraaid; geen regressievergelijking.")
    else:
        compare_with_reference(case_directory, reference_directory, names)
        print(f"{case} results match the stored reference.")
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
    parser.add_argument(
        "--work-directory",
        help="run in this directory instead of writing results beside the examples",
    )
    parser.add_argument(
        "--reference",
        default="reference",
        help="reference directory inside a case (default: reference). The "
        "fixed-front unstructured ordering visits the vertices in a different "
        "order and converges to a slightly different state, so it has its own "
        "reference rather than being exempt from the comparison.",
    )
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="alleen draaien zonder regressievergelijking; telt niet als geslaagde regressie",
    )
    arguments = parser.parse_args()
    source_directory = Path(__file__).resolve().parent
    example_directory = (
        Path(arguments.work_directory).expanduser().resolve()
        if arguments.work_directory
        else source_directory
    )

    try:
        executable = find_executable(source_directory, arguments.swan_executable)
        if example_directory != source_directory:
            for case in CASES:
                shutil.copytree(
                    source_directory / case,
                    example_directory / case,
                    dirs_exist_ok=True,
                )
        selected = CASES if arguments.case == "all" else (arguments.case,)
        for case in selected:
            elapsed = run_case(executable, example_directory, case,
                               arguments.reference, arguments.smoke)
            print(f"{case.capitalize()} case completed normally in {elapsed:.2f} seconds.")
            print(f"Results: {example_directory / case}")
    except (FileNotFoundError, OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
