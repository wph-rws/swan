"""Runner gates for the curve-output test."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
RUNNER = REPO / "examples" / "curve_output" / "run.py"


def _find_swan() -> str:
    return swan_executable()


def _run(*arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(RUNNER), *arguments],
        capture_output=True, text=True)


def test_missing_executable_is_red(tmp_path):
    result = _run("--swan-executable", "/does-not-exist/swan.exe",
                  "--work-directory", str(tmp_path / "work"))
    assert result.returncode != 0


def test_missing_reference_is_red(tmp_path):
    swan = _find_swan()
    result = _run("--swan-executable", str(swan),
                  "--work-directory", str(tmp_path / "work"),
                  "--reference", "does-not-exist")
    assert result.returncode != 0


def test_smoke_does_not_count_as_regression(tmp_path):
    swan = _find_swan()
    result = _run("--swan-executable", str(swan),
                  "--work-directory", str(tmp_path / "work"),
                  "--reference", "does-not-exist",
                  "--smoke")
    assert result.returncode == 0
    assert "Smoke mode" in result.stdout
