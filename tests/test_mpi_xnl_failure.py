#!/usr/bin/env python3
"""Gate: a failed XNL initialisation on one MPI process stops every process.

Under MPI each process of the exact quadruplet code (QUADRUPLET 51) reads its
own configuration file, xnl4v5.cfg-001, xnl4v5.cfg-002 and so on, and makes
its own interaction tables. One process can therefore fail where the others
succeed. Before, the failing process stopped (BF-65) while the others started
iterating and waited for it in the first exchange; OpenMPI then ended the job
because one process had exited, other launchers or a coupled host would wait
for ever. Now the processes decide together and all of them stop with the
message.

Use as a gate:
    python3 tests/test_mpi_xnl_failure.py --swan-executable EXE --mpi-exec mpiexec
        --work-directory DIR
Without binaries, pytest runs the plain ``test_*`` functions.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CASE = REPO / "examples" / "quick_test"
MESSAGE = "initialisation of the exact quadruplet computation (XNL) failed"


def stage(work: Path) -> Path:
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    shutil.copy2(CASE / "bottom.bot", work / "bottom.bot")
    deck = (CASE / "quick_test.swn").read_text()
    assert "\nGEN3 KOMEN\n" in deck, "quick test deck has changed shape"
    deck = deck.replace("\nGEN3 KOMEN\n", "\nGEN3 KOMEN\nQUADRUPLET 51\n", 1)
    (work / "INPUT").write_text(deck)
    #  only the second process gets a coupling coefficient that does not exist
    (work / "xnl4v5.cfg-002").write_text("COUPLING 3\n")
    return work


def processes_reporting(work: Path) -> list[str]:
    return [p.name for p in sorted(work.glob("PRINT-*"))
            if MESSAGE in p.read_text(errors="replace")]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--work-directory", default=None)
    args = parser.parse_args()
    work = stage(Path(args.work_directory or tempfile.mkdtemp(prefix="swan-mpi-xnl-")))
    result = subprocess.run([args.mpi_exec, args.mpi_numproc_flag, "2", args.swan_executable],
                            cwd=work, capture_output=True, text=True, timeout=120)
    reporting = processes_reporting(work)
    failures = []
    if result.returncode == 0:
        failures.append("the run ended with exit code 0")
    if reporting != ["PRINT-001", "PRINT-002"]:
        failures.append(f"the XNL message is in {reporting}, expected both PRINT files")
    for failure in failures:
        print("FAIL:", failure)
    if not failures:
        print("both processes stopped with the XNL message")
    return 1 if failures else 0


def test_processes_reporting_reads_each_print_file(tmp_path):
    (tmp_path / "PRINT-001").write_text("nothing\n")
    (tmp_path / "PRINT-002").write_text(f" ** Terminating error: {MESSAGE}; see xnl4v5.err\n")
    assert processes_reporting(tmp_path) == ["PRINT-002"]


if __name__ == "__main__":
    sys.exit(main())
