#!/usr/bin/env python3
"""Run the larger SWAN Voordelta example."""

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
    "voordelta.prt",
    "voordelta.erf",
    "voordelta_depth.blk",
    "voordelta_hs.blk",
    "voordelta_sites.tbl",
)

RESULT_NAMES = {
    "PRINT": "voordelta.prt",
    "Errfile": "voordelta.erf",
    "ERRPTS": "ERRPTS",
    "norm_end": "norm_end",
    "swaninit": "swaninit",
    "voordelta_depth.blk": "voordelta_depth.blk",
    "voordelta_hs.blk": "voordelta_hs.blk",
    "voordelta_sites.tbl": "voordelta_sites.tbl",
}
FIGURE_NAMES = ("voordelta_depth.png", "voordelta_hs.png")


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
    results_directory = case_directory / "results"
    results_directory.mkdir(exist_ok=True)
    for name in GENERATED_FILES:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()
    for name in (*RESULT_NAMES.values(), *FIGURE_NAMES):
        path = results_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / "voordelta.swn", input_file)
    started = time.monotonic()
    try:
        result = subprocess.run([executable], cwd=case_directory, check=False)
    finally:
        input_file.unlink(missing_ok=True)
    elapsed = time.monotonic() - started

    for source_name, result_name in RESULT_NAMES.items():
        source = case_directory / source_name
        if source.exists():
            source.replace(results_directory / result_name)

    if result.returncode:
        raise RuntimeError(
            f"SWAN stopped with exit code {result.returncode}. "
            "Inspect results/voordelta.prt and results/voordelta.erf."
        )
    if not (results_directory / "norm_end").is_file():
        raise RuntimeError(
            "SWAN did not create norm_end. Inspect results/voordelta.prt and "
            "results/voordelta.erf."
        )

    try:
        from plot_results import create_plots

        figures = create_plots(results_directory)
    except ModuleNotFoundError as error:
        print(
            f"Warning: plots were not created because {error.name} is missing. "
            "Install NumPy and Matplotlib, then run plot_results.py."
        )
        figures = []

    print(f"Voordelta example completed normally in {elapsed:.2f} seconds.")
    print(f"Results: {results_directory}")
    for figure in figures:
        print(f"Created figure: {figure.name}")
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
        run(find_executable(case_directory, arguments.swan_executable), case_directory)
    except (FileNotFoundError, OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
