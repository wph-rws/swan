"""The path to swan.exe for tests that actually run SWAN.

CMake passes the built target through `cmake -E env` in the test command itself,
together with SWAN_EXECUTABLE_REQUIRED=1. A missing binary must then not lead
to a skip: a skipped test counts as passed in ctest, and that is how five
entries in the OpenMP configuration tested nothing for weeks. Outside ctest
(plain `pytest tests/`) the test is skipped when there is no binary.
"""
from __future__ import annotations

import os
import shutil
from pathlib import Path

import pytest


def swan_executable() -> str:
    exe = os.environ.get("SWAN_EXECUTABLE")
    if exe and Path(exe).is_file():
        return exe
    if os.environ.get("SWAN_EXECUTABLE_REQUIRED") == "1":
        pytest.fail(f"SWAN_EXECUTABLE is missing or does not exist: {exe!r}")
    installed = shutil.which("swan.exe")
    if installed:
        return installed
    pytest.skip("no swan.exe: run through ctest or set SWAN_EXECUTABLE")
