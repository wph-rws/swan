"""The physics-coverage gate must hold for every commit.

Running it from pytest puts it inside the gate that scripts/select_gates.py
already selects for source and example changes, so a formulation added to
swan_physics_selection.f90 without a deck fails before it is merged rather
than years later.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parent.parent
SCRIPT = REPOSITORY / "scripts" / "physics_coverage.py"


def test_every_inventoried_formulation_is_reachable_from_a_deck():
    completed = subprocess.run(
        [sys.executable, str(SCRIPT), "--repository", str(REPOSITORY)],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, (
        "physics coverage gate failed:\n"
        f"{completed.stdout}\n{completed.stderr}"
    )
