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


def test_inventory_requires_a_record_of_the_last_measurement(tmp_path):
    """Without date, compiler and deck set, "identical" cannot be traced back."""
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    del manifest["last_measurement"]
    stripped = tmp_path / "upstream-delta.json"
    stripped.write_text(json.dumps(manifest), encoding="utf-8")
    completed = subprocess.run(
        [sys.executable, str(SCRIPT), "--inventory-only", "--manifest", str(stripped)],
        capture_output=True,
        text=True,
    )
    assert completed.returncode != 0
    assert "last_measurement" in completed.stdout



def _inventory_check(tmp_path, change):
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    change(manifest)
    changed = tmp_path / "upstream-delta.json"
    changed.write_text(json.dumps(manifest), encoding="utf-8")
    return subprocess.run(
        [sys.executable, str(SCRIPT), "--inventory-only", "--manifest", str(changed)],
        capture_output=True,
        text=True,
    )


def _first_differing(manifest):
    return next(entry for entry in manifest["decks"] if entry["status"] == "differs")


def test_a_differing_deck_needs_a_fingerprint_of_the_fork_output(tmp_path):
    """Without it, a new change in an already differing deck would stay unseen."""
    completed = _inventory_check(tmp_path, lambda m: _first_differing(m).pop("fork_output"))
    assert completed.returncode != 0
    assert "without a fingerprint of the fork output" in completed.stdout


def test_a_fork_that_does_not_complete_a_deck_is_never_accepted(tmp_path):
    def failed(manifest):
        _first_differing(manifest)["status"] = "fork failed"
    completed = _inventory_check(tmp_path, failed)
    assert completed.returncode != 0
    assert "the fork did not complete the deck" in completed.stdout


@pytest.mark.parametrize("upstream_code", [0, 1])
def test_no_fork_output_is_a_failure_even_when_upstream_fails(tmp_path, monkeypatch, upstream_code):
    sys.path.insert(0, str(REPOSITORY / "scripts"))
    import upstream_delta as delta
    outcomes = iter([delta.RunOutcome(0, {}, 0.), delta.RunOutcome(upstream_code, {}, 0.)])
    monkeypatch.setattr(delta, "run_deck", lambda *args: next(outcomes))
    status, _, _ = delta._assess_deck("deck", Path("fork"), Path("upstream"), tmp_path, 5, 0)
    assert status == delta.STATUS_FORK_FAILED


def test_fingerprint_changes_names_the_changed_files():
    sys.path.insert(0, str(REPOSITORY / "scripts"))
    import upstream_delta
    recorded = {"a.tbl": "1", "b.spc": "2"}
    assert upstream_delta.fingerprint_changes(recorded, dict(recorded)) == []
    assert upstream_delta.fingerprint_changes(recorded, {"a.tbl": "1", "b.spc": "3"}) == ["b.spc"]
    assert upstream_delta.fingerprint_changes(recorded, {"a.tbl": "1"}) == ["b.spc"]
    assert upstream_delta.fingerprint_changes(None, {"a.tbl": "1"}) == []

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
