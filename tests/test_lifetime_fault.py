"""Failure after partial initialisation, and recovery."""
from __future__ import annotations

import shutil
import subprocess
import sys
from pathlib import Path

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
RUNNER = REPO / "examples" / "quick_test" / "run.py"


def _swan() -> str:
    return swan_executable()


def test_failure_then_recovery(tmp_path):
    swan = _swan()
    work = tmp_path / "work"
    # 1. failing run: SWAN directly with an empty INPUT -> no norm_end and exit != 0
    #    (the runner overwrites quick_test.swn, so the failure goes straight to the executable).
    src = REPO / "examples" / "quick_test"
    (work).mkdir()
    shutil.copy2(src / "bottom.bot", work / "bottom.bot")
    (work / "INPUT").write_text("STOP\n")
    bad = subprocess.run([swan], cwd=work, capture_output=True, text=True)
    assert (bad.returncode != 0) or (not (work / "norm_end").is_file()), (
        "failing deck must be red (exit status or norm_end)"
    )
    # 2. recovery: the good deck must then be green (no leftover state in the runner)
    shutil.copy2(src / "quick_test.swn", work / "quick_test.swn")
    good = subprocess.run(
        [sys.executable, str(RUNNER), "--swan-executable", swan,
         "--work-directory", str(tmp_path / "work2")],
        capture_output=True, text=True,
    )
    assert good.returncode == 0, f"recovery after input error fails: {good.stderr[-2000:]}"
