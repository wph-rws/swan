"""Unusual or faulty input must give the documented result or a clear message.

Each test writes a small deck on the spot, mostly the smoke deck with one
command added or changed. These decks do not belong under examples/: those are
valid physics cases that also have reference output.
"""
from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
QUICK_TEST = REPO / "examples" / "quick_test"


def _swan() -> str:
    return swan_executable()


def _run_deck(tmp_path: Path, deck: str, files: dict[str, str] | None = None,
              timeout: float | None = None):
    work = tmp_path / "work"
    work.mkdir(parents=True)
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    for name, content in (files or {}).items():
        (work / name).write_text(content, encoding="utf-8")
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=timeout)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    return result, report


#  CVMESH looks for an output point in a curvilinear grid and then checks in
#  KGRPNT whether that point is active. For a point outside the grid
#  (XC = -99) that check must be skipped, or KGRPNT is read at index -97.
#  Fortran does not promise to skip the right-hand side of .AND., so the check
#  is a separate IF, as upstream has it with its GOTO. With runtime checks a
#  read outside KGRPNT would stop this run.
def test_point_outside_curvilinear_grid(tmp_path):
    xs = "\n".join(" ".join(str(500.0 * i) for i in range(5)) for _ in range(3))
    ys = "\n".join(" ".join(str(500.0 * j) for _ in range(5)) for j in range(3))
    deck = """PROJECT 'CURV' '1'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
CGRID CURVILINEAR 4 2 CIRCLE 24 0.05 1.0 16
READGRID COORDINATES 1.0 'coor.grd' 3 0 FREE
INPGRID BOTTOM CURVILINEAR 0 0 4 2
READINP BOTTOM 1.0 'curved.bot' 3 0 FREE
WIND 8.0 270.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SEGMENT IJ 0 0 0 2 CONSTANT PAR 1.0 6.0 270.0 20.0
GEN3 KOMEN
POINTS 'P' 1000 500 5000 5000
TABLE 'P' NOHEADER 'p.tbl' XP YP DEPTH
COMPUTE
STOP
"""
    files = {"coor.grd": xs + "\n" + ys + "\n", "curved.bot": "20 20 20 20 20\n" * 3}
    result, report = _run_deck(tmp_path, deck, files, timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 0, report[-2000:]
    rows = [r.split() for r in (tmp_path / "work" / "p.tbl").read_text().splitlines()]
    assert float(rows[0][2]) == pytest.approx(20.0)
    assert float(rows[1][2]) == pytest.approx(-99.0)
