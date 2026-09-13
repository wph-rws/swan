"""SURFBEAT (IEM) must not read or write outside its arrays.

No example deck chose SURFBEAT, so the build with runtime checks never
reached SwanIEMsrfbeat. It had three faults:

- the upper neighbouring direction, used to distribute the bound energy, did
  not wrap around, so Ebig(MDC+1, ...) was written: in memory, the first
  direction of the next frequency;
- a row whose western point is dry computed with phase speeds,
  boundary values and wave numbers from a previous row, or with no value;
- without energetic components, iss(0) was read.

This test runs the smoke deck with a sloping bottom, following the recipe in
the manual (two COMPUTEs, only propagation, breaking and friction): once
with a wet western boundary everywhere and once with a dry western point in the
northern rows. In a build with runtime checks it fails on each of the
three faults; in every build it requires a normal end and finite output.
"""
from __future__ import annotations

import math
import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
QUICK_TEST = REPO / "examples" / "quick_test"


def _swan() -> str:
    return swan_executable()


def _deck() -> str:
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    for old, new in [
        ("WIND 8.0 270.0", ""),
        ("GEN3 KOMEN", "OFF WCAP\nOFF QUAD\nSET MAXERR 2"),
        ("\nCOMPUTE\n", "\nSURFBEAT\nCOMPUTE\nCOMPUTE\n"),
    ]:
        assert old in deck, "the smoke deck has changed shape"
        deck = deck.replace(old, new, 1)
    return deck


#  Bottom on a 2 x 2 grid: southern row, northern row. Depth is positive.
BOTTOMS = {
    "wet_west_edge": "20.0 2.0\n20.0 2.0\n",
    "dry_west_point": "20.0 2.0\n-3.0 10.0\n",
}


@pytest.mark.parametrize("bottom", sorted(BOTTOMS))
def test_surfbeat_stays_within_its_bounds(tmp_path, bottom):
    work = tmp_path / "work"
    work.mkdir()
    (work / "bottom.bot").write_text(BOTTOMS[bottom], encoding="utf-8")
    (work / "INPUT").write_text(_deck(), encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr[-2000:]
    values = [float(v) for v in (work / "quick_test_hs.blk").read_text().split()]
    assert values, "no output"
    assert all(math.isfinite(v) for v in values)
