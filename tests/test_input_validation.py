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


#  `COMPUTE STATionary` is the documented form for a stationary run.
#  SWREAD tested `NSTATM.LE.0 .OR. KEYWIS('STAT')`; in a stationary run
#  the left-hand side is already true, and gfortran then skips the right. STAT
#  was therefore left unused and the next command reported
#  'Illegal keyword'. An Intel build evaluates both sides and reported nothing.
@pytest.mark.parametrize("compute", ["COMPUTE STAT", "COMPUTE STATIONARY"])
def test_compute_stationary_is_valid(tmp_path, compute):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "\nCOMPUTE\n" in deck, "smoke deck has changed shape"
    (work / "INPUT").write_text(deck.replace("\nCOMPUTE\n", f"\n{compute}\n"),
                                encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 0
    assert "Illegal keyword" not in report, report[-2000:]


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


#  BF-23: a boundary file whose first line is not a header that BCFILE knows.
#  Nothing of the file is read then, so there is no boundary spectrum. As a
#  severe error (level 3) this still let a deck with SET MAXERR 3 compute:
#  the boundary points pointed to a spectrum that did not exist, SWAN read
#  whatever lay behind the empty table of boundary spectra, and the run ended
#  normally with exit code 0 and Hs = 0 at every point despite the wind
#  (upstream: 0.23 m, a segmentation fault or a failed allocation, depending
#  on the deck). It must stop the run whatever [maxerr] is, and write no
#  output.
@pytest.mark.parametrize("maxerr", [None, 3])
@pytest.mark.parametrize("boundary", [
    "BOUNDSPEC SIDE WEST CONSTANT FILE 'junk.bnd'",
    "BOUNDSPEC SEGMENT IJ 0 0 0 10 VARIABLE FILE 0. 'junk.bnd' 1",
    "BOUNDNEST1 NEST 'junk.bnd' CLOSED",
], ids=["side", "segment", "nest"])
def test_boundary_file_with_unknown_header_stops_the_run(tmp_path, boundary, maxerr):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    (work / "junk.bnd").write_text("JUNK header line\n1 2 3\n4 5 6\n", encoding="utf-8")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(old, boundary, 1)
    if maxerr is not None:
        deck = deck.replace("SET NAUTICAL", f"SET NAUTICAL\nSET MAXERR {maxerr}", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 1, (result.returncode, report[-2000:])
    assert "Terminating error: unsupported boundary data file junk.bnd" in report, report[-2000:]
    assert not (work / "quick_test_center.tbl").exists(), "SWAN computed without a boundary"
    assert not (work / "norm_end").exists()


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
