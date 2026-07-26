#!/usr/bin/env python3
"""Tests for the BG2 boundary-condition regression.

The reference values come from the operational chain itself: the committed
standalone deck ``runs/current_converged/so-rp_osk.swn`` was generated for
310 deg / 20 m/s / NAP +3.00 m and carries the BG2 boundary it was run with.
"""

from __future__ import annotations

import itertools
import os
import shutil
import subprocess

import pytest

import bg2

SHELL_SCRIPT = bg2.ROOT / "bin" / "Bereken_golven_bg2.sh"

SECTORS = bg2.read_sectors()

# The single condition for which an operationally generated deck exists.
KNOWN_DIRECTION = 310.0
KNOWN_WIND_SPEED_MS = 20.0
KNOWN_WATER_LEVEL_CM = 300


def _awk_contains(sector: bg2.Sector, direction: float) -> bool:
    """The literal sector test from bin/Bereken_golven_bg2.sh."""
    if sector.begin <= direction <= sector.end:
        return True
    return sector.begin == 346 and (
        direction >= sector.begin or direction <= sector.end
    )


def test_table_has_both_quantities_for_twelve_sectors():
    for quantity in bg2.QUANTITIES:
        rows = [s for s in SECTORS if s.quantity == quantity]
        assert len(rows) == 12, f"{quantity} has {len(rows)} rows"


def test_fixed_columns_parse_the_documented_sector():
    sector = bg2.select_sector(SECTORS, "HS", 310)
    assert (sector.begin, sector.end) == (286, 315)
    assert (sector.a, sector.b, sector.c) == (1.7260, 0.0855, -6.22)


def test_known_condition_reproduces_the_committed_deck():
    bc = bg2.boundary_condition(
        KNOWN_DIRECTION, KNOWN_WIND_SPEED_MS, KNOWN_WATER_LEVEL_CM, SECTORS
    )
    # (1.7260*200 + 0.0855*300 - 6.22) / 100
    assert bc.hs_m == pytest.approx(3.6463, abs=1e-9)
    # (0.1208*200 + 0.0047*300 + 41.83) / 10
    assert bc.tm10_s == pytest.approx(6.740, abs=1e-9)
    assert bc.tp_s == pytest.approx(7.414, abs=1e-9)

    # The deck header records Hs=3.65 m, Tp=7.41 s.
    assert bc.hs_text == "3.65"
    assert bc.tp_text == "7.41"
    assert bc.tm10_text == "6.74"


def test_tp_follows_the_unrounded_tm10():
    """The shell prints 1.1*tm10 from the raw value, not from the printed one."""
    bc = bg2.boundary_condition(
        KNOWN_DIRECTION, KNOWN_WIND_SPEED_MS, KNOWN_WATER_LEVEL_CM, SECTORS
    )
    assert bc.tp_s == pytest.approx(bg2.TP_OVER_TM10 * bc.tm10_s, abs=1e-12)


def test_wind_speed_enters_the_regression_in_dms():
    """20 m/s must act as 200 dm/s (Run_so-rp_swan.sh line 94)."""
    sector = bg2.select_sector(SECTORS, "HS", KNOWN_DIRECTION)
    expected = sector.evaluate(KNOWN_WIND_SPEED_MS * 10.0, KNOWN_WATER_LEVEL_CM) / 100.0
    bc = bg2.boundary_condition(
        KNOWN_DIRECTION, KNOWN_WIND_SPEED_MS, KNOWN_WATER_LEVEL_CM, SECTORS
    )
    assert bc.hs_m == pytest.approx(expected, abs=1e-12)
    # Reading the wind speed as m/s would give a very different boundary.
    wrong = sector.evaluate(KNOWN_WIND_SPEED_MS, KNOWN_WATER_LEVEL_CM) / 100.0
    assert abs(bc.hs_m - wrong) > 3.0


@pytest.mark.parametrize("direction", range(0, 361))
def test_every_direction_selects_exactly_one_sector(direction):
    for quantity in bg2.QUANTITIES:
        bg2.select_sector(SECTORS, quantity, direction)


@pytest.mark.parametrize("direction", range(0, 361))
def test_wrap_rule_matches_the_shell_literal(direction):
    """Deriving the wrap from begin > end must not change any selection."""
    for sector in SECTORS:
        assert sector.contains(direction) == _awk_contains(sector, direction)


@pytest.mark.parametrize(
    "direction,expected",
    [
        (0, (346, 15)),
        (15, (346, 15)),
        (16, (16, 45)),
        (315, (286, 315)),
        (316, (316, 345)),
        (345, (316, 345)),
        (346, (346, 15)),
        (360, (346, 15)),
    ],
)
def test_sector_boundaries_are_inclusive(direction, expected):
    for quantity in bg2.QUANTITIES:
        sector = bg2.select_sector(SECTORS, quantity, direction)
        assert (sector.begin, sector.end) == expected


def test_north_wraps_to_the_same_sector_from_both_sides():
    for quantity in bg2.QUANTITIES:
        assert bg2.select_sector(SECTORS, quantity, 360) is bg2.select_sector(
            SECTORS, quantity, 0
        )


def test_hs_floor_applies_at_low_wind_and_low_water():
    """270 deg / 2 m/s / -200 cm gives 0.0734 m before the floor."""
    sector = bg2.select_sector(SECTORS, "HS", 270)
    assert sector.evaluate(20.0, -200) / 100.0 < bg2.HS_FLOOR_M
    bc = bg2.boundary_condition(270, 2.0, -200, SECTORS)
    assert bc.hs_m == bg2.HS_FLOOR_M
    assert bc.hs_text == "0.10"


def test_tm10_floor_applies_in_the_sheltered_sector_at_high_wind():
    """The 106-135 regression has a negative wind coefficient."""
    sector = bg2.select_sector(SECTORS, "TM-10", 120)
    assert sector.evaluate(500.0, 650) / 10.0 < bg2.TM10_FLOOR_S
    bc = bg2.boundary_condition(120, 50.0, 650, SECTORS)
    assert bc.tm10_s == bg2.TM10_FLOOR_S
    assert bc.tp_s == pytest.approx(1.1, abs=1e-12)


def test_operational_grid_is_fully_covered():
    """Every condition in Run_so-rp_swan.sh lines 23-25 must evaluate."""
    directions = [90, 180, 210, 240, 270, 300, 330, 360]
    speeds = range(2, 51, 2)
    levels = list(range(-200, 501, 50)) + [560, 580, 590, 650]
    for direction in directions:
        for speed in speeds:
            for level in levels:
                bc = bg2.boundary_condition(direction, speed, level, SECTORS)
                assert bc.hs_m >= bg2.HS_FLOOR_M
                assert bc.tm10_s >= bg2.TM10_FLOOR_S


def test_level_text_matches_awk_default_ofmt():
    assert bg2.level_text(300) == "3"
    assert bg2.level_text(-200) == "-2"
    assert bg2.level_text(160) == "1.6"
    assert bg2.level_text(-150) == "-1.5"
    assert bg2.level_text(0) == "0"


@pytest.mark.skipif(
    shutil.which("bash") is None or not SHELL_SCRIPT.exists(),
    reason="operational shell script or bash unavailable",
)
@pytest.mark.parametrize("direction", [0, 15, 16, 90, 240, 315, 316, 345, 346, 360])
def test_matches_the_operational_shell_script(direction, tmp_path):
    """Differential check against bin/Bereken_golven_bg2.sh itself.

    The printed strings must match, not just the floats: these values are
    substituted into the deck verbatim.
    """
    env = {**os.environ, "SWANBASE": str(bg2.ROOT)}
    out = tmp_path / "bg2.txt"
    for speed, level in itertools.product([2, 20, 50], [-200, 0, 300, 650]):
        subprocess.run(
            ["bash", str(SHELL_SCRIPT), str(direction), str(speed * 10), str(level),
             str(out)],
            check=True,
            env=env,
        )
        shell = dict(
            (key.strip(), value.strip())
            for key, _, value in (
                line.partition(":") for line in out.read_text().splitlines()
            )
        )
        bc = bg2.boundary_condition(direction, speed, level, SECTORS)
        assert shell["HS_BG2"] == bc.hs_text, (direction, speed, level)
        assert shell["TM-1-0_BG2"] == bc.tm10_text, (direction, speed, level)
        assert shell["TP"] == bc.tp_text, (direction, speed, level)


def test_osk_transmission_states():
    assert bg2.OSK_TRANSMISSION["open"] == 0.387
    assert bg2.OSK_TRANSMISSION["gesloten"] == 0.000


def test_select_sector_keeps_the_shell_behaviour_out_of_range():
    """The shell lets 400 deg fall through the wrap test into sector 346-15."""
    sector = bg2.select_sector(SECTORS, "HS", 400)
    assert (sector.begin, sector.end) == (346, 15)
    assert _awk_contains(sector, 400)


@pytest.mark.parametrize("direction", [-1, 400, 361])
def test_boundary_condition_rejects_out_of_range_direction(direction):
    with pytest.raises(ValueError, match="0-360"):
        bg2.boundary_condition(direction, 20.0, 300, SECTORS)


def test_boundary_condition_rejects_negative_wind_speed():
    with pytest.raises(ValueError, match="negative wind speed"):
        bg2.boundary_condition(310, -1.0, 300, SECTORS)


def test_unknown_quantity_is_rejected():
    with pytest.raises(ValueError):
        bg2.select_sector(SECTORS, "TP", 310)
