#!/usr/bin/env python3
"""Run the Voordelta example with multiple MPI processes."""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
import time
from pathlib import Path


MODEL_OUTPUTS = (
    "voordelta_depth.blk",
    "voordelta_hs.blk",
    "voordelta_sites.tbl",
)
RUN_OUTPUTS = (*MODEL_OUTPUTS, "norm_end", "swaninit")


def find_executable(case_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")

    repository = case_directory.parent.parent
    local_build = repository / "build-mpi" / "bin" / "swan.exe"
    if local_build.is_file():
        return local_build.resolve()

    installed = shutil.which("swan.exe")
    if installed:
        return Path(installed).resolve()
    raise FileNotFoundError(
        "MPI-enabled SWAN executable not found. Build with -DMPI=ON in "
        "build-mpi, or pass --swan-executable /path/to/swan.exe."
    )


def find_launcher(requested: str) -> Path:
    launcher = shutil.which(requested)
    if launcher:
        return Path(launcher).resolve()

    requested_path = Path(requested).expanduser()
    if requested_path.is_file():
        return requested_path.resolve()
    raise FileNotFoundError(f"MPI launcher not found: {requested}")


def clean_results(results_directory: Path) -> None:
    for path in results_directory.iterdir():
        if path.is_dir() and not path.is_symlink():
            shutil.rmtree(path)
        else:
            path.unlink()


def preserve_run(work_directory: Path, results_directory: Path) -> int:
    for name in RUN_OUTPUTS:
        source = work_directory / name
        if source.is_file():
            shutil.copy2(source, results_directory / name)

    reports = sorted(work_directory.glob("PRINT-[0-9][0-9][0-9]"))
    for report in reports:
        rank = report.name.removeprefix("PRINT-")
        shutil.copy2(report, results_directory / f"voordelta_mpi.prt-{rank}")

    main_report = work_directory / "PRINT"
    if main_report.is_file():
        shutil.copy2(main_report, results_directory / "voordelta_mpi.prt")

    for error_file in sorted(work_directory.glob("Errfile-[0-9][0-9][0-9]")):
        if error_file.stat().st_size:
            rank = error_file.name.removeprefix("Errfile-")
            shutil.copy2(error_file, results_directory / f"voordelta_mpi.erf-{rank}")

    main_error = work_directory / "Errfile"
    if main_error.is_file() and main_error.stat().st_size:
        shutil.copy2(main_error, results_directory / "voordelta_mpi.erf")
    return len(reports)


def run(
    executable: Path,
    launcher: Path,
    case_directory: Path,
    processes: int,
    launcher_arguments: list[str],
) -> None:
    if processes < 2:
        raise ValueError("--processes must be at least 2 for this MPI example")

    results_directory = case_directory / "results_mpi"
    results_directory.mkdir(exist_ok=True)
    clean_results(results_directory)

    started = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="swan-voordelta-mpi-") as temporary:
        work_directory = Path(temporary)
        shutil.copy2(case_directory / "voordelta.swn", work_directory / "INPUT")
        shutil.copy2(case_directory / "voordelta.dep", work_directory / "voordelta.dep")

        command = [
            str(launcher),
            "-n",
            str(processes),
            *launcher_arguments,
            str(executable),
        ]
        environment = os.environ.copy()
        environment["OMP_NUM_THREADS"] = "1"
        result = subprocess.run(
            command,
            cwd=work_directory,
            env=environment,
            check=False,
        )
        report_count = preserve_run(work_directory, results_directory)

    elapsed = time.monotonic() - started
    if result.returncode:
        raise RuntimeError(
            f"MPI run stopped with exit code {result.returncode}; inspect "
            f"the voordelta_mpi.prt and .erf files in {results_directory}"
        )
    if report_count != processes:
        raise RuntimeError(
            f"expected {processes} per-process PRINT files, found {report_count}"
        )
    if not (results_directory / "norm_end").is_file():
        raise RuntimeError("MPI run did not create norm_end")
    for name in MODEL_OUTPUTS:
        if not (results_directory / name).is_file():
            raise RuntimeError(f"MPI run did not create {name}")

    try:
        from plot_results import create_plots

        figures = create_plots(results_directory)
    except ModuleNotFoundError as error:
        print(
            f"Warning: plots were not created because {error.name} is missing. "
            "Install NumPy and Matplotlib to create them."
        )
        figures = []

    print(
        f"Voordelta MPI example completed normally with {processes} processes "
        f"in {elapsed:.2f} seconds."
    )
    print(f"Verified {report_count} per-process diagnostic files.")
    print(f"Results: {results_directory}")
    for figure in figures:
        print(f"Created figure: {figure.name}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--processes",
        type=int,
        default=4,
        help="number of MPI processes, at least 2 (default: %(default)s)",
    )
    parser.add_argument(
        "--swan-executable",
        help="MPI-enabled swan.exe (default: build-mpi/bin/swan.exe or PATH)",
    )
    parser.add_argument(
        "--launcher",
        default="mpiexec",
        help="MPI launcher name or path (default: %(default)s)",
    )
    parser.add_argument(
        "--launcher-argument",
        action="append",
        default=[],
        help="extra argument placed before swan.exe; repeat as needed",
    )
    arguments = parser.parse_args()
    case_directory = Path(__file__).resolve().parent

    try:
        run(
            find_executable(case_directory, arguments.swan_executable),
            find_launcher(arguments.launcher),
            case_directory,
            arguments.processes,
            arguments.launcher_argument,
        )
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
