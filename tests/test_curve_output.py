"""Runner-poorten voor de curve-output-test."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
RUNNER = REPO / "examples" / "curve_output" / "run.py"


def _find_swan() -> Path:
    for candidate in (
        REPO / "build-modernization" / "bin" / "swan.exe",
        Path("/tmp/swan-final20-serial/bin/swan.exe"),
        Path("/tmp/swan-curve-output/bin/swan.exe"),
    ):
        if candidate.is_file():
            return candidate
    pytest.skip("geen gebouwde swan.exe gevonden voor curve-runnerproef")


def _run(*arguments: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        [sys.executable, str(RUNNER), *arguments],
        capture_output=True, text=True)


def test_ontbrekende_executable_is_rood(tmp_path):
    result = _run("--swan-executable", "/bestaat-niet/swan.exe",
                  "--work-directory", str(tmp_path / "work"))
    assert result.returncode != 0


def test_ontbrekende_referentie_is_rood(tmp_path):
    swan = _find_swan()
    result = _run("--swan-executable", str(swan),
                  "--work-directory", str(tmp_path / "work"),
                  "--reference", "bestaat-niet")
    assert result.returncode != 0


def test_smoke_telt_niet_als_regressie(tmp_path):
    swan = _find_swan()
    result = _run("--swan-executable", str(swan),
                  "--work-directory", str(tmp_path / "work"),
                  "--reference", "bestaat-niet",
                  "--smoke")
    assert result.returncode == 0
    assert "Smoke-modus" in result.stdout
