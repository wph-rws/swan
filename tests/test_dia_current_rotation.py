"""With a current, the DIA must give the same waves when the model is turned.

A box with wind, boundary waves and a uniform current is run twice: as it
is, and turned by a quarter turn (grid, wind, boundary and current all
turned together). With 36 directions a quarter turn is exactly nine bins,
so the two runs solve the same discrete problem and must end in the same
wave field.

The turn moves the seam of the direction counter, between bin MDC and bin
1, to other waves. That is what makes the test see BF-71. With a current,
the bins of a sweep differ per frequency. When their counters do not all
cross the seam the same way, SWPSEL widens the range of the sweep to the
full circle, and the DIA then fills its interactions for the directions 1
to MDC and a few on either side. The range of a single frequency can still
have counters below that, and SWSNL1 and SWSNL2 read the interactions at
those counters: columns that were not filled. Upstream reads zeros there
and so leaves out part of the quadruplet source term of those bins.

Measured before the fix (fork at e68e9be, gfortran 16.2, -O3), on the grid
of this test: with QUADRUPLET 1 the turned run differed from the straight
one by up to 1.1 cm in Hs and 0.197 s in Tm01, with QUADRUPLET 2 by up to
1.9 cm and 0.209 s. After the fix QUADRUPLET 1, whose iteration reaches a
balance, gives the same Hs to the last printed digit and Tm01 within
0.001 s. QUADRUPLET 2 keeps 0.7 cm and 0.030 s: its iteration does not
settle, and the two runs take their sweeps in a different order.
"""
from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

MX, MY = 10, 6            # cells of 500 m of the straight box
CELL = 500.0
U, V = 0.6, -0.3          # current of the straight box, m/s
ITERATIONS = 60


def _deck(turned: bool, quad: str) -> tuple[str, str]:
    if turned:
        xlen, ylen, mx, my = MY * CELL, MX * CELL, MY, MX
        direction, side = 180.0, "SOUTH"
        cu, cv = -V, U
    else:
        xlen, ylen, mx, my = MX * CELL, MY * CELL, MX, MY
        direction, side = 270.0, "WEST"
        cu, cv = U, V
    deck = f"""\
PROJECT 'ROTATE' '001'
SET LEVEL 0.0 NAUTICAL
MODE STATIONARY TWODIMENSIONAL
COORDINATES CARTESIAN
CGRID REGULAR 0. 0. 0. {xlen} {ylen} {mx} {my} CIRCLE 36 0.04 1.0 24
INPGRID BOTTOM REGULAR 0. 0. 0. 1 1 {xlen} {ylen}
READINP BOTTOM 1.0 'box.bot' 1 0 FREE
INPGRID CURRENT REGULAR 0. 0. 0. 1 1 {xlen} {ylen}
READINP CURRENT 1.0 'current.dat' 1 0 FREE
WIND 15.0 {direction}
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SIDE {side} CONSTANT PAR 2.0 7.5 {direction} 25.0
GEN3 KOMEN
{quad}
BREAKING CONSTANT 1.0 0.73
FRICTION JONSWAP CONSTANT 0.038
BLOCK 'COMPGRID' NOHEADER 'hs.blk' LAYOUT 3 HSIGN
BLOCK 'COMPGRID' NOHEADER 'tm01.blk' LAYOUT 3 TM01
NUMERIC STOPC NPNTS=101 STAT MXITST={ITERATIONS}
COMPUTE
STOP
"""
    current = f"{cu} {cu}\n{cu} {cu}\n{cv} {cv}\n{cv} {cv}\n"
    return deck, current


def _field(work: Path, name: str, turned: bool) -> list[list[float]]:
    """A block file as rows of y and columns of x of the straight box."""
    values = [float(v) for v in (work / name).read_text().split()]
    if not turned:
        assert len(values) == (MX + 1) * (MY + 1)
        return [values[j * (MX + 1):(j + 1) * (MX + 1)] for j in range(MY + 1)]
    #  The turned run has x' = ylen - y and y' = x: its row is a column
    #  of the straight box, written from the largest y to the smallest.
    assert len(values) == (MX + 1) * (MY + 1)
    rows = [values[i * (MY + 1):(i + 1) * (MY + 1)] for i in range(MX + 1)]
    return [[rows[i][MY - j] for i in range(MX + 1)] for j in range(MY + 1)]


def _run(tmp_path: Path, turned: bool, quad: str) -> dict[str, list[list[float]]]:
    work = tmp_path / ("turned" if turned else "straight")
    work.mkdir()
    deck, current = _deck(turned, quad)
    (work / "INPUT").write_text(deck)
    (work / "current.dat").write_text(current)
    (work / "box.bot").write_text("20. 20.\n20. 20.\n")
    result = subprocess.run([swan_executable()], cwd=work,
                            capture_output=True, text=True)
    report = (work / "PRINT").read_text(errors="replace")
    assert result.returncode == 0, report[-2000:]
    assert (work / "norm_end").exists(), report[-2000:]
    return {name: _field(work, name + ".blk", turned) for name in ("hs", "tm01")}


def _largest_difference(a: list[list[float]], b: list[list[float]]) -> float:
    return max(abs(x - y) for row_a, row_b in zip(a, b)
               for x, y in zip(row_a, row_b))


#  The limits lie between the values after the fix and before it (see the
#  text at the top): Hs 0 and 0.7 cm against 1.1 and 1.9 cm, Tm01 0.001 and
#  0.030 s against 0.197 and 0.209 s. Tm01 is the check that decides: it
#  drops by a factor of seven or more. For QUADRUPLET 2 the iteration does
#  not settle, so its Hs keeps 0.7 cm after the fix; that limit leaves room
#  for another compiler or another order of the sweeps.
@pytest.mark.parametrize("quad, hs_limit, tm01_limit", [
    ("QUADRUPLET 1", 0.002, 0.01),
    ("QUADRUPLET 2", 0.015, 0.08),
])
def test_a_quarter_turn_gives_the_same_waves(tmp_path, quad, hs_limit, tm01_limit):
    straight = _run(tmp_path, False, quad)
    turned = _run(tmp_path, True, quad)
    hs = _largest_difference(straight["hs"], turned["hs"])
    tm01 = _largest_difference(straight["tm01"], turned["tm01"])
    #  the waves must be there at all, or equal fields mean nothing
    assert max(max(row) for row in straight["hs"]) > 1.0
    assert hs < hs_limit, (quad, hs, tm01)
    assert tm01 < tm01_limit, (quad, hs, tm01)
