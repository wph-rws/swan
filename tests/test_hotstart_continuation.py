"""A stationary run that is continued through a hotfile computes on inside the
domain bit for bit as if there had been no restart.

An UNFORMATTED hotfile stores the spectrum in every grid point exactly, and
INIT HOTSTART reads it back exactly. What a restart does change is the
boundary: SNEXTI imposes the full boundary spectrum again at every COMPUTE
in the points with a boundary condition, also in the directions that leave
the domain and that the previous run had computed there itself. That is also
the case upstream. For those outgoing directions the inner domain lies upstream
of the boundary, so the inner field does not notice; only the boundary points
themselves (and a residual or accuracy taken over all points) jump up briefly
after a restart.

This test guards both sides: storing and reading back stay exact, and
the difference stays limited to the boundary points.
"""
from __future__ import annotations

import subprocess
from pathlib import Path

from swan_executable import swan_executable

NX, NY = 12, 8      # meshes; the grid has (NX+1) x (NY+1) points
N, M = 4, 2         # iterations before and after the restart

DECK = """PROJ 'hot' '1'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
CGRID REGULAR 0. 0. 0. {lx} {ly} {nx} {ny} CIRCLE 24 0.05 1.00 20
INP BOTTOM REGULAR 0. 0. 0. 1 1 {lx} {ly}
READ BOTTOM 1. 'bottom' IDLA=3 FREE
WIND 30.0 300.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUND SIDE WEST CON PAR 4.0 8.0 300.0 30.0
{init}
BREAKING
FRICTION JONSWAP
TRIAD
NUM ACCUR NPNTS=101 STAT MXITST={n} ALFA=0.01
COMPUTE
HOTFILE 'hot' UNF
STOP
"""


def _run(work: Path, iterations: int, init: str = "") -> None:
    work.mkdir(parents=True)
    (work / "bottom").write_text("30.0 5.0\n30.0 5.0\n")
    (work / "INPUT").write_text(DECK.format(
        lx=NX * 2000.0, ly=NY * 2000.0, nx=NX, ny=NY, n=iterations, init=init))
    result = subprocess.run([swan_executable()], cwd=work, capture_output=True,
                            text=True, timeout=120)
    assert result.returncode == 0, result.stdout[-2000:] + result.stderr[-2000:]
    assert (work / "hot").is_file()


def _records(path: Path) -> list[bytes]:
    """Records of a sequential UNFORMATTED file (4-byte markers)."""
    data = path.read_bytes()
    records, pos = [], 0
    while pos < len(data):
        length = int.from_bytes(data[pos:pos + 4], "little")
        records.append(data[pos + 4:pos + 4 + length])
        pos += length + 8
    return records


def _spectra(path: Path) -> dict[tuple[int, int], bytes]:
    """Spectrum per grid point (ix, iy) from a hotfile of a regular grid.

    Order as BACKUP writes it: after the header, per point a record with the
    point index and, if the point is active, a record with the spectrum. The
    points run with ix outer and iy inner.
    """
    records = _records(path)
    npoints = (NX + 1) * (NY + 1)
    rest = len(records)
    #  count from the end: all points are active here (two records each)
    body = records[rest - 2 * npoints:]
    spectra = {}
    for k in range(npoints):
        ix, iy = divmod(k, NY + 1)
        spectra[(ix, iy)] = body[2 * k + 1]
    return spectra


def test_restart_continues_exactly_inside_the_domain(tmp_path):
    _run(tmp_path / "straight", N + M)
    _run(tmp_path / "part1", N)
    _run(tmp_path / "part2", M, "INIT HOTSTART '../part1/hot' UNF")

    straight = _spectra(tmp_path / "straight" / "hot")
    restarted = _spectra(tmp_path / "part2" / "hot")
    differing = sorted(p for p in straight if straight[p] != restarted[p])

    interior = [p for p in differing if p[0] != 0]
    assert not interior, (
        f"after a restart the spectrum inside the domain differs in {interior[:5]}; "
        "storing or reading back the hotfile is no longer exact")
    #  the west boundary gets the boundary spectrum imposed again (see above)
    assert differing, (
        "the boundary points now also compute on seamlessly; update the explanation in this "
        "test and in doc/balance-residual-design.md")
