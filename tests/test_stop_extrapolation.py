"""NUMERIC ... EXTRAP: the iteration may only stop once the change of Hs that
is still to come is small, not just the change of the last iteration.

quad_dia1 with ALFA=0.01 is the known case of a false stop (AGENTS.md,
"Convergence"): STOPC stops after about 40 iterations with Hs at 25 km at
1.40 m, while the converged value is 1.923 m. With EXTRAP the same deck must
run on until Hs is within a centimetre of that value, and without EXTRAP
nothing may change.
"""
from pathlib import Path
import os
import re
import shutil
import subprocess

import pytest

from swan_executable import swan_executable

ROOT = Path(__file__).resolve().parents[1]
QUAD = ROOT / "examples/nonlinear_interactions/quad"
QUICK = ROOT / "examples/quick_test"
CONVERGED_HS_25KM = 1.923   # forced 3200 iterations, ALFA=0.01


def _run(tmp_path, name, numeric):
    work = tmp_path / name
    work.mkdir()
    shutil.copy(QUAD / "bottom.bot", work)
    deck = (QUAD / "quad_dia1.swn").read_text()
    old = "NUMERIC STOPC STAT MXITST=100 ALFA=0.01"
    assert old in deck, "quad_dia1 has changed shape"
    deck = deck.replace(old, numeric, 1)
    deck = re.sub(r"(?m)^TEST .*\n", "", deck)
    (work / "INPUT").write_text(deck)
    proc = subprocess.run([swan_executable()], cwd=work, capture_output=True, text=True,
                          env=dict(os.environ, OMP_NUM_THREADS="2"), timeout=120)
    report = (work / "PRINT").read_text()
    rows = [line.split() for line in (work / "quad_dia1.tbl").read_text().splitlines()
            if line.strip() and not line.startswith("%")]
    hs = {float(r[0]): float(r[2]) for r in rows}
    return proc, report, hs


def _iterations(report):
    return max(map(int, re.findall(r"iteration\s+(\d+)", report)))


def test_extrapolation_prevents_the_false_stop(tmp_path):
    proc, report, hs = _run(tmp_path, "plain", "NUMERIC STOPC STAT MXITST=600 ALFA=0.01")
    assert proc.returncode == 0
    assert _iterations(report) < 60
    assert hs[25000.0] < CONVERGED_HS_25KM - 0.3, "the false stop this test relies on is gone"
    assert "extrapolated change" not in report

    proc, report, hs = _run(tmp_path, "extrap", "NUMERIC STOPC STAT MXITST=600 ALFA=0.01 EXTRAP 10")
    assert proc.returncode == 0, report[-2000:]
    assert "extrapolated change of Hs OK in 100.00 %" in report
    assert "points fail with a steady or growing change" in report
    assert _iterations(report) < 600, "EXTRAP never let the iteration stop"
    assert abs(hs[25000.0] - CONVERGED_HS_25KM) < 0.01, hs


@pytest.mark.parametrize("value", ["1", "2.5", "-3"])
def test_wrong_block_length_is_refused(tmp_path, value):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy(QUICK / "bottom.bot", work)
    deck = (QUICK / "quick_test.swn").read_text().replace(
        "\nCOMPUTE\n", f"\nNUMERIC STOPC STAT MXITST=30 EXTRAP {value}\nCOMPUTE\n", 1)
    (work / "INPUT").write_text(deck)
    proc = subprocess.run([swan_executable()], cwd=work, capture_output=True, text=True, timeout=60)
    report = (work / "PRINT").read_text()
    assert proc.returncode != 0
    assert "[nstep] of NUMERIC EXTRAP must be a whole number of at least 2" in report


def test_unstructured_grid_refuses_the_option(tmp_path):
    """Not built for unstructured grids yet; ignoring it would look like a
    satisfied criterion, so the run stops instead."""
    from test_balance_residual import run
    _, proc, report = run(tmp_path, "unstructured", "EXTRAP 10", unstructured=True)
    assert proc.returncode != 0
    assert "NUMERIC EXTRAP is not available on an unstructured grid" in report
