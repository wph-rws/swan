"""Fout-na-gedeeltelijke-initialisatie en herstel."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
RUNNER = REPO / "examples" / "quick_test" / "run.py"


def _swan() -> str:
    #  De bouw geeft het pad door: CMake zet SWAN_EXECUTABLE op het gebouwde
    #  doel. Zonder dat moest deze proef het pad raden, en dan slaat hij stil
    #  over in elke bouwmap met een andere naam -- wat er in ctest uitziet als
    #  geslaagd.
    from_build = os.environ.get("SWAN_EXECUTABLE")
    if from_build and Path(from_build).is_file():
        return from_build
    installed = shutil.which("swan.exe")
    if installed:
        return installed
    pytest.skip(
        "geen swan.exe: draai via ctest of zet SWAN_EXECUTABLE"
    )

def test_fout_daarna_herstel(tmp_path):
    swan = _swan()
    work = tmp_path / "work"
    # 1. falende run: SWAN direct met lege INPUT -> geen norm_end en exit != 0
    #    (runner overschrijft quick_test.swn, dus fout direct via executable).
    src = REPO / "examples" / "quick_test"
    (work).mkdir()
    shutil.copy2(src / "bottom.bot", work / "bottom.bot")
    (work / "INPUT").write_text("STOP\n")
    bad = subprocess.run([swan], cwd=work, capture_output=True, text=True)
    assert (bad.returncode != 0) or (not (work / "norm_end").is_file()), (
        "falende deck moet rood zijn (exitstatus of norm_end)"
    )
    # 2. herstel: goede deck moet daarna groen zijn (geen resttoestand in runner)
    shutil.copy2(src / "quick_test.swn", work / "quick_test.swn")
    good = subprocess.run(
        [sys.executable, str(RUNNER), "--swan-executable", swan,
         "--work-directory", str(tmp_path / "work2")],
        capture_output=True, text=True,
    )
    assert good.returncode == 0, f"herstel na invoerfout faalt: {good.stderr[-2000:]}"
