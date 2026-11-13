"""SWAN computes with abrupt underflow: no subnormal numbers in its results.

A result below the smallest normal single-precision number (1.18E-38) is a
subnormal number unless the program asks for abrupt underflow, and a
processor computes with those many times slower. The DIA makes them all the
time: it multiplies three energy densities that are tiny in the directions
against the wind. SWMAIN therefore sets abrupt underflow, so that such a
result becomes zero.

The setting is one line, and nothing but the run time shows when it is
lost. This test shows it in the output. A small box with wind writes the
source terms of three test points to a file. Computed with gradual
underflow (the fork before the setting, gfortran 16.2 at -O3) that file
holds 19 values that are not zero and smaller than 1.18E-38; with abrupt
underflow it holds none.

What the test does not see: SWCOMP also sets the mode on each OpenMP thread,
for threads that the calling program made before SWAN ran. In this run the
threads are made after SWMAIN set the mode and inherit it, and the small
numbers in the file are made when the main thread writes it. The check that
every thread gets its own mode back is in tests/test_two_cases.f90.
"""
from __future__ import annotations

import os
import re
import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

SMALLEST_NORMAL = 1.17549435e-38

DECK = """\
PROJECT 'UNDERFLOW' '001'
SET LEVEL 0.0 NAUTICAL
MODE STATIONARY TWODIMENSIONAL
COORDINATES CARTESIAN
CGRID REGULAR 0. 0. 0. 5000.0 3000.0 10 6 CIRCLE 36 0.04 1.0 24
INPGRID BOTTOM REGULAR 0. 0. 0. 1 1 5000.0 3000.0
READINP BOTTOM 1.0 'box.bot' 1 0 FREE
WIND 15.0 270.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SIDE WEST CONSTANT PAR 2.0 7.5 270.0 25.0
GEN3 KOMEN
BREAKING CONSTANT 1.0 0.73
FRICTION JONSWAP CONSTANT 0.038
TEST 1 0 POINTS XY 2500. 1000. 2500. 1500. 2500. 2000. S1D 'sources.spc'
NUMERIC STOPC NPNTS=101 STAT MXITST=5
COMPUTE
STOP
"""


@pytest.mark.parametrize("threads", [1, 4])
def test_the_source_terms_hold_no_subnormal_number(tmp_path: Path, threads: int):
    (tmp_path / "INPUT").write_text(DECK)
    (tmp_path / "box.bot").write_text("20. 20.\n20. 20.\n")
    #  the thread count only matters in an OpenMP build
    environment = dict(os.environ, OMP_NUM_THREADS=str(threads))
    result = subprocess.run([swan_executable()], cwd=tmp_path, env=environment,
                            capture_output=True, text=True)
    report = (tmp_path / "PRINT").read_text(errors="replace")
    assert result.returncode == 0, report[-2000:]
    assert (tmp_path / "norm_end").exists(), report[-2000:]

    values = [abs(float(word))
              for line in (tmp_path / "sources.spc").read_text().splitlines()
              for word in line.split()
              if re.fullmatch(r"[-+]?\d\.\d+E[-+]\d+", word)]
    #  the file must hold source terms at all, small ones among them, or
    #  the absence of subnormal numbers means nothing
    assert len(values) > 4000
    assert any(0.0 < value < 1.0e-30 for value in values)
    subnormal = [value for value in values if 0.0 < value < SMALLEST_NORMAL]
    assert not subnormal, (len(subnormal), min(subnormal))
