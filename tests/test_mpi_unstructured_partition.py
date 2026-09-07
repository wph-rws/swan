#!/usr/bin/env python3
"""Poort: ongestructureerd + MPI vereist een gepartitioneerde mesh.

Zonder METIS kan de mesh niet worden gepartitioneerd (LOGCOM(7) blijft onwaar)
en moet de run schoon falen met de bedoelde foutmelding
("to run in parallel the mesh must be partitioned first", exitcode 1, geen
norm_end) in plaats van te segfaulten op ongepartitioneerde ivertg (
reproduceerbaar vóór de bewaking na MSGERR(4) in swanpre1.f90). Met METIS moet
dezelfde case met 2 ranks gewoon slagen. De CTest-registratie kiest de
verwachting per configuratie (METIS aan/uit); dit is dus dekking van zowel het
succes- als het foutpad.

Gebruik als poort:
    python3 tests/test_mpi_unstructured_partition.py --swan-executable EXE
        --mpi-exec mpiexec --expect {success,clean-failure}
        --work-directory DIR
Zonder binaries draaien de pure ``test_*``-functies onder pytest.
"""

from __future__ import annotations

import argparse
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CASE = REPO / "examples" / "nonstationary" / "unstructured"
BASENAME = "nonstationary_unstructured"
CASE_FILES = ("bottom.bot", "wind.wnd", f"{BASENAME}.node", f"{BASENAME}.ele",
              f"{BASENAME}.swn")
PARTITION_MESSAGE = "to run in parallel the mesh must be partitioned first"


def stage(case_directory: Path, work_directory: Path) -> Path:
    if work_directory.exists():
        shutil.rmtree(work_directory)
    work_directory.mkdir(parents=True)
    for name in CASE_FILES:
        shutil.copy2(case_directory / name, work_directory / name)
    deck = work_directory / f"{BASENAME}.swn"
    (work_directory / "INPUT").write_text(deck.read_text())
    return work_directory


def run_mpi(swan: Path, mpi: str, numproc_flag: str, work: Path,
            processes: int = 2) -> subprocess.CompletedProcess:
    return subprocess.run(
        [mpi, numproc_flag, str(processes), str(swan)],
        cwd=work, capture_output=True, text=True)


def print_text(work: Path) -> str:
    chunks = []
    for path in sorted(work.glob("PRINT*")):
        try:
            chunks.append(path.read_text(errors="replace"))
        except OSError:
            continue
    return "\n".join(chunks)


def classify(returncode: int, prints: str, norm_end: bool) -> str:
    """Deel een MPI-run in: success, clean-failure of crash."""
    if returncode == 0 and norm_end:
        return "success"
    if returncode == -signal.SIGSEGV:
        return "crash"
    if returncode != 0 and PARTITION_MESSAGE in prints and not norm_end:
        return "clean-failure"
    return "unexpected"


def check(swan: str, mpi: str, numproc_flag: str, expect: str,
          work_directory: str | None) -> int:
    swan_path = Path(swan).expanduser().resolve()
    if not swan_path.is_file():
        print(f"error: SWAN executable not found: {swan_path}", file=sys.stderr)
        return 2
    work = (Path(work_directory).expanduser().resolve() if work_directory
            else Path(tempfile.mkdtemp(prefix="mpi-unstructured-")))
    stage(CASE, work)
    result = run_mpi(swan_path, mpi, numproc_flag, work)
    outcome = classify(result.returncode, print_text(work),
                       (work / "norm_end").is_file())
    if outcome == expect:
        print(f"mpi_unstructured_partition groen: {outcome} (verwacht {expect})")
        return 0
    print(f"error: uitkomst is {outcome}, verwacht {expect} "
          f"(exit={result.returncode})", file=sys.stderr)
    return 1


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--expect", choices=("success", "clean-failure"),
                        required=True)
    parser.add_argument("--work-directory", default=None)
    arguments = parser.parse_args()
    try:
        return check(arguments.swan_executable, arguments.mpi_exec,
                     arguments.mpi_numproc_flag, arguments.expect,
                     arguments.work_directory)
    except (FileNotFoundError, OSError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2


# Pure eenheden, zonder binaries te draaien (pytest-verzameling).

def test_classify_success():
    assert classify(0, "bla", True) == "success"


def test_classify_segfault_is_crash():
    assert classify(-signal.SIGSEGV, PARTITION_MESSAGE, False) == "crash"


def test_classify_clean_failure():
    assert classify(1, f"x {PARTITION_MESSAGE} y", False) == "clean-failure"


def test_classify_missing_message_is_unexpected():
    assert classify(1, "iets anders", False) == "unexpected"


def test_classify_norm_end_with_failure_is_unexpected():
    assert classify(1, PARTITION_MESSAGE, True) == "unexpected"


if __name__ == "__main__":
    raise SystemExit(main())
