#!/usr/bin/env python3
"""Gate: the quasi-coherent model (GEN4) runs to the end under MPI.

After every iteration, QCM collects its bulk dissipation from all processes
on the master (SWCOLLECT, an MPI gather) and sends the whole field back to
every process (SWBROADC). Only the master receives in a gather, so the other
processes pass an empty receive buffer. The fork's typed wrapper SWGATHER
checked that buffer against its length on every process; on the other
processes the check failed, they left the gather and went on to the
broadcast, while the master waited in the gather. The run hung after the
first iteration and never ended.

This test runs examples/qcm/qcm_scatter.swn, a QCM deck whose bottom really
scatters, on one, two and four processes with a time limit, and compares the
binary block output of the runs on several processes with the run on one.
With the wavefront exchange they agree byte for byte. The Jacobi exchange
(--jacobi) gives values that depend on how the grid is split, so there Hsig
may differ by a tolerance.

The 5 x 3 grid of examples/qcm/qcm.swn is not used: split into strips it is
narrower than the halo, and its block output then has gaps under MPI, also
without QCM and also in TU Delft's own MPI build.

Use as a gate:
    python3 tests/test_mpi_qcm.py --swan-executable EXE --mpi-exec mpiexec
        [--jacobi] --work-directory DIR
Without binaries, pytest runs the plain ``test_*`` functions.
"""

from __future__ import annotations

import argparse
import os
import shutil
import signal
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
CASE = REPO / "examples" / "qcm"
DECK = "qcm_scatter.swn"
OUTPUT = "qcm_scatter.mat"
PROCESSES = (1, 2, 4)
TIME_LIMIT = 90               # s per run; a run takes a few seconds
#  m of Hsig; the deck stops after five iterations, long before the split
#  Jacobi runs come together. On the commit that added this test they
#  differed from one process by up to 3.8 cm on two processes and 4.8 cm
#  on four.
JACOBI_TOLERANCE = 0.10


def stage(work: Path) -> Path:
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    shutil.copy2(CASE / "bottom_scatter.bot", work / "bottom_scatter.bot")
    shutil.copy2(CASE / DECK, work / "INPUT")
    return work


def run(command: list[str], work: Path) -> str | None:
    """Run SWAN; return a failure text, or None when it ended normally."""
    #  A hung MPI job leaves its processes behind unless the whole
    #  process group is killed.
    with open(work / "stdout.txt", "w") as log:
        process = subprocess.Popen(command, cwd=work, stdout=log,
                                   stderr=subprocess.STDOUT,
                                   start_new_session=True)
        try:
            code = process.wait(timeout=TIME_LIMIT)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
            return f"did not end within {TIME_LIMIT} s"
    if code != 0:
        return f"ended with exit code {code}"
    if not (work / "norm_end").exists() or not (work / OUTPUT).exists():
        return f"wrote no norm_end or no {OUTPUT}"
    return None


def largest_hsig_difference(first: Path, second: Path) -> float:
    import numpy as np
    import scipy.io
    a = scipy.io.loadmat(first)["Hsig"]
    b = scipy.io.loadmat(second)["Hsig"]
    if a.shape != b.shape or not np.array_equal(np.isnan(a), np.isnan(b)):
        return float("inf")
    wet = ~np.isnan(a)
    return float(np.max(np.abs(a[wet] - b[wet]))) if wet.any() else 0.0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--jacobi", action="store_true",
                        help="the build uses the Jacobi exchange (-DJAC=ON)")
    parser.add_argument("--work-directory", default=None)
    args = parser.parse_args()
    root = Path(args.work_directory or tempfile.mkdtemp(prefix="swan-mpi-qcm-"))
    executable = str(Path(args.swan_executable).resolve())

    failures = []
    baseline = None
    for count in PROCESSES:
        work = stage(root / f"np{count}")
        failure = run([args.mpi_exec, args.mpi_numproc_flag, str(count),
                       executable], work)
        if failure:
            failures.append(f"{count} processes: {failure} ({work})")
            continue
        output = work / OUTPUT
        if baseline is None:
            baseline = output
        elif args.jacobi:
            difference = largest_hsig_difference(baseline, output)
            if difference > JACOBI_TOLERANCE:
                failures.append(f"{count} processes: Hsig differs by {difference:.4f} m "
                                f"from one process ({work})")
        elif output.read_bytes() != baseline.read_bytes():
            failures.append(f"{count} processes: {OUTPUT} differs from one process ({work})")

    for failure in failures:
        print("FAIL:", failure)
    if not failures:
        print(f"QCM ran to the end on {', '.join(map(str, PROCESSES))} processes "
              "and agreed with the run on one process")
    return 1 if failures else 0


def test_stage_copies_deck_and_bottom(tmp_path):
    work = stage(tmp_path / "np1")
    assert "GEN4" in (work / "INPUT").read_text()
    assert (work / "bottom_scatter.bot").exists()


def test_run_reports_a_hang(tmp_path, monkeypatch):
    monkeypatch.setattr(sys.modules[__name__], "TIME_LIMIT", 1)
    failure = run([sys.executable, "-c", "import time; time.sleep(30)"], tmp_path)
    assert failure == "did not end within 1 s"


if __name__ == "__main__":
    sys.exit(main())
