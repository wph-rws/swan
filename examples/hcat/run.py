#!/usr/bin/env python3
"""Exercise the complete parallel-hotfile, hcat, and single-hotstart route."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference  # noqa: E402


def require_file(value: str, description: str) -> Path:
    path = Path(value).expanduser().resolve()
    if not path.is_file():
        raise FileNotFoundError(f"{description} not found: {path}")
    return path


def run_command(command: list[str], work_directory: Path, description: str) -> None:
    result = subprocess.run(command, cwd=work_directory, check=False)
    if result.returncode:
        raise RuntimeError(
            f"{description} stopped with exit code {result.returncode}; "
            f"inspect {work_directory}"
        )


def run_swan(
    command: list[str], deck: str, work_directory: Path, description: str
) -> None:
    work_directory.mkdir(parents=True, exist_ok=True)
    (work_directory / "INPUT").write_text(deck)
    run_command(command, work_directory, description)
    if not (work_directory / "norm_end").is_file():
        raise RuntimeError(
            f"{description} did not create norm_end; inspect {work_directory}"
        )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--hcat-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--mpi-processes", type=int, default=2)
    parser.add_argument(
        "--halo",
        type=int,
        default=3,
        help="number of overlap layers written by the selected parallel scheme",
    )
    parser.add_argument(
        "--reference",
        default="reference",
        help="reference variant for both the cold MPI run and the hotstart route",
    )
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="alleen draaien zonder regressievergelijking; telt niet als geslaagde regressie",
    )
    arguments = parser.parse_args()

    try:
        if arguments.mpi_processes < 2:
            raise ValueError("--mpi-processes must be at least 2")
        if arguments.halo < 0:
            raise ValueError("--halo must not be negative")
        swan = require_file(arguments.swan_executable, "SWAN executable")
        hcat = require_file(arguments.hcat_executable, "hcat executable")
        mpi = require_file(arguments.mpi_exec, "MPI launcher")

        source_directory = Path(__file__).resolve().parent.parent / "quick_test"
        base_deck = (source_directory / "quick_test.swn").read_text()
        writer_deck = base_deck.replace(
            "\nSTOP\n", "\nHOTFILE 'quick_hot' FREE\nSTOP\n"
        )
        reader_deck = base_deck.replace(
            "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE\n",
            "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE\n"
            "INITIAL HOTSTART SINGLE 'quick_hot' FREE\n",
        )
        if writer_deck == base_deck or reader_deck == base_deck:
            raise RuntimeError("could not add the hotfile commands to the quick-test deck")

        work_directory = Path(arguments.work_directory).expanduser().resolve()
        if work_directory.exists():
            shutil.rmtree(work_directory)
        writer_directory = work_directory / "writer"
        reader_directory = work_directory / "reader"
        for directory in (writer_directory, reader_directory):
            directory.mkdir(parents=True)
            shutil.copy2(source_directory / "bottom.bot", directory / "bottom.bot")

        mpi_command = [
            str(mpi),
            arguments.mpi_numproc_flag,
            str(arguments.mpi_processes),
            str(swan),
        ]
        run_swan(mpi_command, writer_deck, writer_directory, "parallel SWAN writer")
        if arguments.smoke:
            compare_with_reference(
                writer_directory,
                source_directory / arguments.reference,
                ("quick_test_center.tbl", "quick_test_hs.blk"),
                smoke=True,
            )
        else:
            compare_with_reference(
                writer_directory,
                source_directory / arguments.reference,
                ("quick_test_center.tbl", "quick_test_hs.blk"),
            )

        part_files = [
            writer_directory / f"quick_hot-{index:03d}"
            for index in range(1, arguments.mpi_processes + 1)
        ]
        missing = [path.name for path in part_files if not path.is_file()]
        if missing:
            raise RuntimeError(f"parallel SWAN produced no {', '.join(missing)}")

        run_command(
            [str(hcat), "-v", "-h", str(arguments.halo), "quick_hot"],
            writer_directory,
            "hcat",
        )
        combined = writer_directory / "quick_hot"
        if not combined.is_file() or combined.stat().st_size == 0:
            raise RuntimeError("hcat produced no non-empty quick_hot")

        shutil.copy2(combined, reader_directory / combined.name)
        run_swan([str(swan)], reader_deck, reader_directory, "serial hotstart reader")
        if arguments.smoke:
            compare_with_reference(
                reader_directory,
                Path(__file__).resolve().parent / arguments.reference,
                ("quick_test_center.tbl", "quick_test_hs.blk"),
                smoke=True,
            )
            print("Smoke-modus: hotstart-route draaide; geen regressievergelijking.")
        else:
            compare_with_reference(
                reader_directory,
                Path(__file__).resolve().parent / arguments.reference,
                ("quick_test_center.tbl", "quick_test_hs.blk"),
            )
            print("Parallel hotfiles were merged, read back, and matched the reference.")
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
