"""The fixed semi-implicit DIA lets the stationary iteration reach a balance.

The semi-implicit DIA of SWSNL1 puts DSNL/(2 pi)**3 on the main diagonal but
leaves the matching term out of the right-hand side. Its fixed point is
therefore not a balance of the source terms, and the derivative it uses is
(2 pi)**3 = 248 times too small to damp the quadruplets, which drive a
two-cycle in the spectral tail. The explicit DIA (QUADRUPLET 2) has that
two-cycle as well. The fork (BF-62) uses the consistent linearisation with
the full derivative for QUADRUPLET 1; QUADRUPLET 1 UPSTREAM keeps the upstream
form.

On a small channel with wind, all three forms run 80 iterations without
under-relaxation (ALFA = 0), so that only the DIA differs. The fixed form
brings the balance residual down to 4e-8; the upstream semi-implicit DIA
stalls at 8e-4 and the explicit DIA at 4e-3. Under-relaxation is left out on
purpose: with ALFA = 0.01 every form, the fixed one included, is still at
about 1e-2 after 80 iterations, so such a run says nothing about the DIA.

The test point of the same run prints the quadruplet source term. The DIA
only moves energy between frequencies, so that column must nearly cancel
when it is integrated over the frequencies.
"""
from __future__ import annotations

import re
import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

DECK = """\
PROJECT 'NEWTON' '001'
SET LEVEL 0.0 NAUTICAL
MODE STATIONARY TWODIMENSIONAL
COORDINATES CARTESIAN
CGRID REGULAR 0. 0. 0. 5000.0 1000.0 10 2 CIRCLE 36 0.04 1.0 24
INPGRID BOTTOM REGULAR 0. 0. 0. 1 1 5000.0 1000.0
READINP BOTTOM 1.0 'box.bot' 1 0 FREE
WIND 15.0 270.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SIDE WEST CONSTANT PAR 2.0 7.5 270.0 25.0
GEN3 KOMEN
{quad}
BREAKING CONSTANT 1.0 0.73
FRICTION JONSWAP CONSTANT 0.038
TEST 1 0 POINTS XY 2500.0 500.0 S1D 'sources.spc'
NUMERIC STOPC NPNTS=101 STAT MXITST=80 ALFA={alfa} RESIDUAL 0
COMPUTE
STOP
"""

RESIDUAL = re.compile(r"balance residual energy rms\s+([-+.0-9Ee]+)")


def _run(tmp_path: Path, quad: str, alfa: str) -> Path:
    work = tmp_path / "work"
    work.mkdir()
    (work / "box.bot").write_text("20. 20.\n20. 20.\n")
    (work / "INPUT").write_text(DECK.format(quad=quad, alfa=alfa))
    result = subprocess.run([swan_executable()], cwd=work,
                            capture_output=True, text=True)
    report = (work / "PRINT").read_text(errors="replace")
    assert result.returncode == 0, report[-2000:]
    return work


def _last_residual(tmp_path: Path, quad: str, alfa: str) -> float:
    work = _run(tmp_path, quad, alfa)
    values = RESIDUAL.findall((work / "PRINT").read_text(errors="replace"))
    assert len(values) == 80, f"expected 80 iterations, got {len(values)}"
    return float(values[-1])


def _source_integrals(path: Path, name: str) -> tuple[float, float]:
    """Integrate one column of a TEST ... S1D file over the frequencies.

    Returns the integral and the integral of the absolute value, both for the
    last iteration in the file.
    """
    lines = path.read_text().splitlines()
    start = next(i for i, line in enumerate(lines) if line.startswith("AFREQ"))
    count = int(lines[start + 1].split()[0])
    freqs = [float(lines[start + 2 + i]) for i in range(count)]
    quant = next(i for i, line in enumerate(lines) if line.startswith("QUANT"))
    names = [lines[quant + 2 + 3 * i].split()[0]
             for i in range(int(lines[quant + 1].split()[0]))]
    last = max(i for i, line in enumerate(lines)
               if line.startswith("Test point"))
    column = [float(lines[last + 1 + i].split()[names.index(name)])
              for i in range(count)]

    def integrate(values: list[float]) -> float:
        return sum(0.5 * (values[i] + values[i + 1]) * (freqs[i + 1] - freqs[i])
                   for i in range(count - 1))

    return integrate(column), integrate([abs(value) for value in column])


def test_the_fixed_dia_reaches_a_balance(tmp_path):
    residual = _last_residual(tmp_path, "QUADRUPLET 1", "0.")
    assert residual < 1e-6, residual


#  The counter-test: like for like, also without under-relaxation, the
#  upstream forms stall (measured 7.9e-4 and 3.9e-3 against 4.4e-8), so the
#  test above depends on the fix and not on an easy deck.
@pytest.mark.parametrize("quad", ["QUADRUPLET 1 UPSTREAM", "QUADRUPLET 2"])
def test_the_upstream_forms_stall(tmp_path, quad):
    residual = _last_residual(tmp_path, quad, "0.")
    assert residual > 1e-4, residual


#  The solver uses S(N0) + D (N0 - N) for the fixed form, and the test point
#  must print the same. It printed S - D N: a large loss over the whole tail
#  of the spectrum, and a net loss of five times the wind input (integral
#  -5.8e-4, absolute integral 5.9e-4, wind 1.1e-4). The right column has an
#  integral of -9.8e-6 against an absolute integral of 3.2e-5; what is left
#  is the transfer to frequencies above the grid. The explicit DIA and the
#  upstream form print -1.0e-5 against 3.2e-5 on this deck.
@pytest.mark.parametrize(
    "quad", ["QUADRUPLET 1", "QUADRUPLET 1 UPSTREAM", "QUADRUPLET 2"])
def test_the_test_point_prints_a_conserving_quadruplet_term(tmp_path, quad):
    work = _run(tmp_path, quad, "0.")
    net, absolute = _source_integrals(work / "sources.spc", "Snl4")
    wind, _ = _source_integrals(work / "sources.spc", "Swind")
    assert absolute > 0.
    assert abs(net) < 0.5 * absolute, (net, absolute)
    assert abs(net) < 0.2 * wind, (net, wind)
