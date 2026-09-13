"""De delta-inventaris moet elk commit kloppen.

De inventaris zelf is goedkoop te controleren: staat elk deck erin, heeft elk
verschil met upstream een BF-nummer, en bestaat dat BF-nummer werkelijk. Dat
draait hier, in de snelle poort, bij elk commit.

De gemeten kant -- de fork en de vastgezette upstream-binary allebei over de
decks halen -- kost een eenmalige bouw van upstream en daarna enkele minuten
rekentijd. Die staat achter ``SWAN_UPSTREAM_DELTA=1``, zodat hij bewust
gedraaid wordt (release, of een wijziging die de uitvoer raakt) in plaats van
bij elke pytest-aanroep. Overslaan is hier eerlijk: de statische helft blijft
altijd hard, en de gemeten helft meldt zichzelf als overgeslagen in plaats van
stilzwijgend groen te zijn.
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
BUGFIX_TABLE = REPOSITORY / "doc" / "bugfixes-tov-tu-delft-41.51.md"


def test_manifest_is_complete_and_every_difference_is_attributed():
    completed = subprocess.run(
        [sys.executable, str(SCRIPT), "--inventory-only"],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, (
        "upstream-delta-inventaris klopt niet:\n"
        f"{completed.stdout}\n{completed.stderr}"
    )


def test_pinned_upstream_matches_the_bugfix_table():
    """De inventaris en de bugfixtabel moeten dezelfde upstream bedoelen.

    Anders vergelijkt de poort tegen een andere bron dan het document waar de
    reparaties in staan, en betekent "wijkt af -- BF-17" niets meer.
    """
    manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
    commit = manifest["upstream"]["commit"]
    assert commit in BUGFIX_TABLE.read_text(encoding="utf-8"), (
        f"de vastgezette upstream {commit[:12]} komt niet voor in "
        f"{BUGFIX_TABLE.name}"
    )


@pytest.mark.skipif(
    os.environ.get("SWAN_UPSTREAM_DELTA") != "1",
    reason="gemeten vergelijking met upstream; zet SWAN_UPSTREAM_DELTA=1 om te draaien",
)
def test_measured_delta_matches_the_manifest():
    completed = subprocess.run(
        [sys.executable, str(SCRIPT)],
        capture_output=True,
        text=True,
    )
    assert completed.returncode == 0, (
        "de gemeten verschillen met upstream wijken af van de inventaris:\n"
        f"{completed.stdout}\n{completed.stderr}"
    )
