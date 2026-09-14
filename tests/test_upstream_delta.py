"""The delta inventory must be correct at every commit.

The inventory itself is cheap to check: is every deck in it, does every
difference with upstream have a BF number, and does that BF number really exist.
That runs here, in the fast gate, at every commit.

The measured side -- running both the fork and the pinned upstream binary over
the decks -- costs a one-off build of upstream and then a few minutes of
computing time. It sits behind ``SWAN_UPSTREAM_DELTA=1``, so that it is run
on purpose (for a release, or a change that affects the output) instead of
at every pytest call. Skipping is honest here: the static half always stays
strict, and the measured half reports itself as skipped instead of
silently passing.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

REPOSITORY = Path(__file__).resolve().parent.parent
SCRIPT = REPOSITORY / "scripts" / "upstream_delta.py"
MANIFEST = REPOSITORY / "doc" / "upstream-delta.json"
BUGFIX_TABLE = REPOSITORY / "doc" / "bug-fixes-vs-tu-delft-41.51.md"


def test_manifest_is_complete_and_every_difference_is_attributed():
    completed = subprocess.run(
        [sys.executable, str(SCRIPT), "--inventory-only"],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, (
        "upstream delta inventory is not correct:\n"
        f"{completed.stdout}\n{completed.stderr}"
    )


def test_pinned_upstream_matches_the_bugfix_table():
    """The inventory and the bug-fix table must refer to the same upstream.

    Otherwise the gate compares against another source than the document that
    lists the repairs, and "differs -- BF-34" no longer means anything.
    """
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    commit = manifest["upstream"]["commit"]
    assert commit in BUGFIX_TABLE.read_text(encoding="utf-8"), (
        f"the pinned upstream {commit[:12]} does not appear in "
        f"{BUGFIX_TABLE.name}"
    )


@pytest.mark.skipif(
    os.environ.get("SWAN_UPSTREAM_DELTA") != "1",
    reason="measured comparison with upstream; set SWAN_UPSTREAM_DELTA=1 to run it",
)
def test_measured_delta_matches_the_manifest():
    completed = subprocess.run(
        [sys.executable, str(SCRIPT)],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, (
        "the measured differences with upstream do not match the inventory:\n"
        f"{completed.stdout}\n{completed.stderr}"
    )
