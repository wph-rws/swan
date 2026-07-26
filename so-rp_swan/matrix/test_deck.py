#!/usr/bin/env python3
"""Tests for reconstructing the missing operational so-rp deck template."""

from __future__ import annotations

from pathlib import Path

import pytest

import bg2
import deck

REFERENCE = bg2.ROOT / "runs" / "current_converged" / "so-rp_osk.swn"
KNOWN = deck.Condition("reference", 310, 20, 300, "open")


def test_known_condition_reproduces_the_reference_deck_byte_for_byte():
    assert generate_default() == REFERENCE.read_text()


def test_operational_level_format_is_awk_compatible():
    text = deck.generate_deck(KNOWN, level_format="operational")
    assert "SET LEVEL=3 MAXERR=2 NAUTICAL" in text
    assert "water level NAP +3.00 m" in text


def test_closed_barrier_changes_only_metadata_and_three_obstacles():
    closed = deck.generate_deck(
        deck.Condition("reference-closed", 310, 20, 300, "gesloten")
    )
    assert "gesloten barrier" in closed
    assert closed.count("OBST TRANS 0.000 LINE ") == 3
    assert "OBST TRANS 0.387" not in closed


def test_generated_deck_has_no_placeholder():
    text = deck.generate_deck(deck.Condition("edge", 346, 2, -200, "open"))
    for placeholder in deck.PLACEHOLDERS:
        assert placeholder not in text


def test_legacy_physics_is_fully_explicit():
    text = deck.generate_deck(KNOWN, physics="legacy-4131")
    assert text.count("GEN3 KOMEN DRAG FIT") == 1
    assert text.count("TRIAD ITRIAD=11 URCRIT=0.2 URSLIM=0.01") == 1
    assert "\nTRIAD URSLIM=0.01\n" not in text


def test_damaged_template_is_rejected(tmp_path: Path):
    damaged = tmp_path / "template.swn"
    damaged.write_text(deck.TEMPLATE.read_text().replace("GOLFTR_OSK", "0.387"))
    with pytest.raises(ValueError, match="missing placeholder GOLFTR_OSK"):
        deck.generate_deck(KNOWN, template_path=damaged)


def test_unknown_placeholder_is_rejected(tmp_path: Path):
    damaged = tmp_path / "template.swn"
    damaged.write_text(deck.TEMPLATE.read_text() + "$ UNKNOWN_ORG\n")
    with pytest.raises(ValueError, match="unknown placeholder"):
        deck.generate_deck(KNOWN, template_path=damaged)


@pytest.mark.parametrize(
    "condition",
    [
        deck.Condition("north-0", 0, 2, -200, "open"),
        deck.Condition("north-360", 360, 50, 650, "gesloten"),
        deck.Condition("sector-edge", 315, 20, 0, "open"),
    ],
)
def test_representative_conditions_validate(condition: deck.Condition):
    deck.generate_deck(condition)


def generate_default() -> str:
    return deck.generate_deck(KNOWN)
