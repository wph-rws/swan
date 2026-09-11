#!/usr/bin/env python3
"""Build-independent runner for the ST6 source-term test-point case.

Pins the test-point source-term output (S1D) of an ST6/Babanin run. That
path reaches SSWELL_ZIEGER, the only kernel family that needs TESTFL and
IPTST to place its diagnostics, and no other registered case exercises it:
the other test-point decks all run GEN3 WESTH, which never sets IWCAP to 8.
A stale or misplaced test-point flag shifts the swell-dissipation column by
percents, well above the comparison tolerance, so the stored reference
catches it.
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


REFERENCE_FILES = (
    "st6_testpoints_sources.spc",
    "st6_testpoints.tbl",
)

#  Both files of this case are raw source-term output, printed with four
#  significant digits: the spectrum spans 4.5e4 down to 1e-6 in the same
#  columns, and the table's Swind/Swcap columns sit around 1e-4. Those are the
#  smallest numbers any registered case compares, and a build that differs only
#  in code generation moves their last printed digits. Measured against the
#  stored reference with SWAN_RUNTIME_CHECKS (-fcheck=all on top of Release):
#  2.2e-3 relative in the spectrum and 1.4e-3 in the table, deterministic
#  across runs, while Hsig and Tm01 stay well inside 1e-3. The shared gate is
#  therefore too tight here, and widening it for this case is not a loss: the
#  failure this case exists to catch is a stale or misplaced test-point flag,
#  which shifts the swell-dissipation column by percents.
TOLERANCES = {
    "st6_testpoints_sources.spc": 5.0e-3,
    "st6_testpoints.tbl": 5.0e-3,
}

GENERATED_FILES = (
    "INPUT",
    "PRINT",
    "Errfile",
    "ERRPTS",
    "norm_end",
    "swaninit",
    "st6_testpoints.prt",
    "st6_testpoints.erf",
    "st6_testpoints.tbl",
    "st6_testpoints_sources.spc",
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
    reference_name: str = "reference",
    smoke: bool = False,
) -> None:
    for name in GENERATED_FILES:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / "st6_testpoints.swn", input_file)
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
        print_file.replace(case_directory / "st6_testpoints.prt")
    error_file = case_directory / "Errfile"
    if error_file.exists():
        error_file.replace(case_directory / "st6_testpoints.erf")

    if result.returncode:
        raise RuntimeError(
            f"SWAN stopped with exit code {result.returncode}. "
            "Inspect st6_testpoints.prt and st6_testpoints.erf."
        )
    if not (case_directory / "norm_end").is_file():
        raise RuntimeError(
            "SWAN did not create norm_end. Inspect st6_testpoints.prt and "
            "st6_testpoints.erf."
        )

    reference_directory = Path(__file__).resolve().parent / reference_name
    if smoke:
        compare_with_reference(case_directory, reference_directory,
                               REFERENCE_FILES,
                               smoke=True)
        print("Smoke-modus: SWAN draaide normaal; geen regressievergelijking.")
    else:
        compare_with_reference(case_directory, reference_directory,
                               REFERENCE_FILES, tolerances=TOLERANCES)
        print("Results match the stored reference.")

    print(f"ST6 source-term test completed normally in {elapsed:.2f} seconds.")
    print("Results: st6_testpoints_sources.spc, st6_testpoints.tbl, "
          "st6_testpoints.prt")
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
    parser.add_argument(
        "--reference",
        default="reference",
        help="reference directory beside this script (default: reference).",
    )
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="alleen draaien zonder regressievergelijking; telt niet als geslaagde regressie",
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
            for name in ("st6_testpoints.swn", "bottom.bot"):
                shutil.copy2(source_directory / name, case_directory / name)
        if arguments.mpi_processes < 1:
            raise ValueError("--mpi-processes must be positive")
        run(
            executable,
            case_directory,
            arguments.mpi_exec,
            arguments.mpi_processes,
            arguments.mpi_numproc_flag,
            arguments.reference,
            arguments.smoke,
        )
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
