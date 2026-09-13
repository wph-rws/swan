"""The recorded verdicts of the strict build must be complete and explained.

Whether every checked warning has a verdict can only be seen after a build;
`scripts/strict_diagnostics.py` checks that. This test checks what can be seen
without one: every verdict is about a checked kind of warning, says what it
is, and gives a reason; a defect names its BF number.
"""
from __future__ import annotations

import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
import strict_diagnostics as sd  # noqa: E402

VERDICTS = ("defect", "suspected-defect", "harmless", "deliberate")


def test_every_verdict_is_about_a_checked_warning():
    for key, entry in sd.read_triage().items():
        assert key[0] in sd.CHECKED, key
        assert entry["count"] >= 1, key
        assert key[2] != "?", key


def test_every_verdict_is_explained():
    for key, entry in sd.read_triage().items():
        assert entry["verdict"] in VERDICTS, key
        assert len(entry["reason"].strip()) > 20, key
        if entry["verdict"] == "defect":
            assert entry.get("reference", "").startswith("BF-"), key
