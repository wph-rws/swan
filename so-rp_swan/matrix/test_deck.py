#!/usr/bin/env python3
"""Tests for the decks generated from the corrected operational template."""

from __future__ import annotations

import re
from pathlib import Path

import pytest

import bg2
import deck

# Pinned deck for the reference condition; any change to the template or to how
# it is filled in must show up here.
REFERENCE = Path(__file__).parent / "reference" / "so-rp_osk_u20_d310_lp300_open.swn"
OPERATIONAL = bg2.ROOT / "par" / "operational" / "so-rp_osk_org.swn"
KNOWN = deck.Condition("reference", 310, 20, 300, "open")


def test_known_condition_reproduces_the_reference_deck_byte_for_byte():
    assert deck.generate_deck(KNOWN) == REFERENCE.read_text()


def test_template_is_the_operational_deck_with_only_grid_and_boundary_corrected():
    """The template may differ from the operational deck only in the grid and the boundary."""
    operational = OPERATIONAL.read_text()
    template = deck.TEMPLATE.read_text()
    segments = re.compile(r"BOUND SEGMENT (?:XY|IJ) [^\n]*\n")
    uncorrected = template.replace("CGRID CURVILINEAR 248 577 ", "CGRID CURVILINEAR 249 578 ")
    assert segments.sub("", uncorrected) == segments.sub("", operational)
    assert len(segments.findall(operational)) == 18
    assert [line.split(" CON ")[0] for line in segments.findall(template)] == [
        segment.strip() for segment in deck.BOUNDARY
    ]


def test_operational_substitution_follows_the_run_script():
    text = deck.generate_deck(KNOWN)
    assert "SET LEVEL = 3 MAXERR = 2 CDCAP=0.00275 INRHOG=0 NAUTICAL" in text
    assert "\nWIND 20 310\n" in text
    assert "BOUND SEGMENT IJ 218 65 29 65 CON PAR 3.65 7.41 310 30.0\n" in text


def test_closed_barrier_changes_only_the_three_obstacles():
    open_deck = deck.generate_deck(KNOWN)
    closed = deck.generate_deck(deck.Condition("reference", 310, 20, 300, "gesloten"))
    assert closed.count("OBST TRANS 0.000 LINE ") == 3
    assert closed.replace("OBST TRANS 0.000", "OBST TRANS 0.387") == open_deck


def test_generated_deck_has_no_placeholder():
    text = deck.generate_deck(deck.Condition("edge", 346, 2, -200, "open"))
    for placeholder in deck.PLACEHOLDERS:
        assert placeholder not in text


def test_legacy_physics_sets_only_the_41_31_defaults_the_deck_leaves_open():
    legacy = deck.generate_deck(KNOWN, physics="legacy-4131")
    operational = deck.generate_deck(KNOWN)
    assert legacy.count("GEN3    KOMEN DRAG FIT\n") == 1
    assert legacy.count("TRIAD\tITRIAD=1 TRFAC=0.8 CUTFR=2.5 DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2\n") == 1
    restored = legacy.replace(" DRAG FIT", "").replace(" DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2", "")
    assert restored == operational


def test_damaged_template_is_rejected(tmp_path: Path):
    damaged = tmp_path / "template.swn"
    damaged.write_text(deck.TEMPLATE.read_text().replace("GOLFTR_OSK", "0.387"))
    with pytest.raises(ValueError, match="missing placeholder GOLFTR_OSK"):
        deck.generate_deck(KNOWN, template_path=damaged)


@pytest.mark.parametrize("old, new, message", [
    ("CGRID CURVILINEAR 248 577 ", "CGRID CURVILINEAR 249 578 ", "248 by 577 grid"),
    ("BOUND SEGMENT IJ 28 66 28 71 ", "BOUND SEGMENT IJ 29 67 29 72 ", "28 66 28 71"),
    ("BOUND SEGMENT IJ 218 65 29 65 ", "BOUND SEGMENT IJ 219 66 30 66 ", "218 65 29 65"),
])
def test_uncorrected_grid_or_boundary_is_rejected(tmp_path: Path, old, new, message):
    damaged = tmp_path / "template.swn"
    damaged.write_text(deck.TEMPLATE.read_text().replace(old, new))
    with pytest.raises(ValueError, match=message):
        deck.generate_deck(KNOWN, template_path=damaged)


def test_boundary_lies_on_the_wet_sea_edge():
    """Every boundary point is wet and its seaward neighbour is not.

    SWAN counts grid indices from 0 and silently overwrites a boundary spectrum
    on a point inside the grid, so a segment one cell too far in gives no waves
    at all. The bottom file is not in git; without it this test is skipped.
    """
    bottom = deck.INPUT_FILES["so-rp/scaloost_rp.bot.recht"]
    if not bottom.is_file():
        pytest.skip("so-rp bottom file not present")
    rows = [line.split() for line in bottom.read_text().splitlines()]
    wet = [[value.lower() != "nan" for value in row] for row in rows]  # wet[j][i]
    sea_row = [(i, 65) for i in range(29, 219)]
    sea_column = [(28, j) for j in range(66, 72)]
    for i, j in sea_row:
        assert wet[j][i] and not wet[j - 1][i], (i, j)
    for i, j in sea_column:
        assert wet[j][i] and not wet[j][i - 1], (i, j)
    assert deck.BOUNDARY == ("BOUND SEGMENT IJ 218 65 29 65 ", "BOUND SEGMENT IJ 28 66 28 71 ")


def test_operational_template_with_xy_boundary_is_rejected():
    with pytest.raises(ValueError):
        deck.generate_deck(KNOWN, template_path=OPERATIONAL)


def test_production_deck_is_the_operational_deck_filled_in_as_the_run_script_does():
    production = deck.generate_deck(KNOWN, template="production")
    corrected = deck.generate_deck(KNOWN)
    assert "CGRID CURVILINEAR 249 578 " in production
    assert production.count(" CON PAR 3.65 7.41 310 30.0\n") == 18
    segments = re.compile(r"BOUND SEGMENT (?:XY|IJ) [^\n]*\n")
    fixed = production.replace("CGRID CURVILINEAR 249 578 ", "CGRID CURVILINEAR 248 577 ")
    assert segments.sub("", fixed) == segments.sub("", corrected)


def test_production_legacy_physics_changes_the_same_two_lines():
    legacy = deck.generate_deck(KNOWN, physics="legacy-4131", template="production")
    restored = legacy.replace(" DRAG FIT", "").replace(" DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2", "")
    assert restored == deck.generate_deck(KNOWN, template="production")


@pytest.mark.parametrize("path", [deck.TEMPLATE, None])
def test_production_template_must_be_the_production_file(tmp_path: Path, path):
    if path is None:
        path = tmp_path / "template.swn"
        path.write_text(OPERATIONAL.read_text().replace("MXITST=40", "MXITST=50"))
    with pytest.raises(ValueError, match="production deck"):
        deck.generate_deck(KNOWN, template="production", template_path=path)


def test_production_md5_matches_the_provenance_record():
    assert deck.PRODUCTION_TEMPLATE == OPERATIONAL
    assert deck.PRODUCTION_MD5 in (OPERATIONAL.parent / "PROVENANCE.md").read_text()


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


def test_stage_inputs_lays_out_the_run_directory(tmp_path: Path):
    missing = [path for path in deck.INPUT_FILES.values() if not path.is_file()]
    if missing:
        pytest.skip(f"so-rp input is not in git: {missing[0]}")
    deck.stage_inputs(tmp_path)
    for relative in deck.INPUT_FILES:
        assert (tmp_path / relative).is_file()
    assert (tmp_path / "dat").is_dir()
    assert "#                                       comment identifier" in (
        tmp_path / "swaninit").read_text()
