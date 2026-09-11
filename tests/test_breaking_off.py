"""Without depth-induced breaking the fraction of breakers should be zero.

With OFF BREAKING, SINTGRL sets the maximum wave height to 100 m, but the
directional factor KTETA was only set by BRKPAR, so only when breaking
is on. FRABRE then divided by an unset value: in practice that gave
Qb = 1 everywhere, so the limiter was off in the whole domain and the iteration
took a different path -- on quad_dia2 49 iterations without convergence against 27
with. In upstream 41.51 the result also depended on the memory layout.
"""
from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
QUICK_TEST = REPO / "examples" / "quick_test"


def _swan() -> str:
    return swan_executable()


def test_no_breakers_without_breaking(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    for old, new in [
        ("BREAKING CONSTANT 1.0 0.73", "OFF BREAKING"),
        ("XP YP DEPTH HSIGN TM01 DIR", "XP YP DEPTH HSIGN TM01 DIR QB"),
    ]:
        assert old in deck, "the smoke deck has changed shape"
        deck = deck.replace(old, new, 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr[-2000:]
    rows = [line.split() for line in
            (work / "quick_test_center.tbl").read_text().splitlines()
            if line.strip() and not line.startswith("%")]
    assert rows and all(float(row[-1]) == 0.0 for row in rows), rows
