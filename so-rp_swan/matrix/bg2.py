#!/usr/bin/env python3
"""BG2 boundary conditions for the operational so-rp condition grid.

``bin/Run_so-rp_swan.sh`` builds one SWAN deck per condition by calling
``bin/Bereken_golven_bg2.sh``, which evaluates a linear regression on wind
speed and water level to obtain the BG2 boundary wave height and period, and
then substitutes the results into ``par/so-rp_osk_org.swn``.

This module reimplements that shell/awk chain so a matrix runner can generate
decks directly, without the hard-coded operational paths. The regression
coefficients are *not* duplicated here: they are read from the same file the
shell script reads, using the same fixed column positions.

Units follow the operational chain exactly:

- wind direction in nautical degrees, 0-360 inclusive;
- wind speed in m/s, converted to dm/s before entering the regression
  (``Run_so-rp_swan.sh`` line 94: ``let wsn_dms=10*${wsn}``);
- water level in cm relative to NAP, which is also the unit of the operational
  condition list.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
COEFFICIENT_FILE = ROOT / "par" / "bg2" / "BG2_calculated_regression_coefficients.par"

# Fixed column positions taken from the awk program in bin/Bereken_golven_bg2.sh.
# awk substr() is 1-based with a length; the slices below are the 0-based
# equivalents, so a reformatted coefficient file breaks here rather than
# silently yielding different boundary conditions.
_SECTOR_BEGIN = slice(27, 30)  # substr($0, 28, 3)
_SECTOR_END = slice(31, 34)  # substr($0, 32, 3)
_A = slice(54, 62)  # substr($0, 55, 8)
_B = slice(64, 72)  # substr($0, 65, 8)
_C = slice(74, 80)  # substr($0, 75, 6)

QUANTITIES = ("HS", "TM-10")

HS_FLOOR_M = 0.10
TM10_FLOOR_S = 1.0
TP_OVER_TM10 = 1.1

# Wave transmission of the Oosterscheldekering, from Run_so-rp_swan.sh lines
# 79-81. The barrier state is part of the condition, not of the regression.
OSK_TRANSMISSION = {"open": 0.387, "gesloten": 0.000}


@dataclass(frozen=True)
class Sector:
    """One regression row: a quantity over a closed wind-direction sector."""

    quantity: str
    begin: int
    end: int
    a: float
    b: float
    c: float

    @property
    def wraps(self) -> bool:
        """True for the sector that crosses north (346-15 in the current file)."""
        return self.begin > self.end

    def contains(self, direction: float) -> bool:
        """Sector membership, with inclusive bounds on both ends.

        The shell script spells the wrap-around case as a literal test on
        ``wrbg == 346``. Deriving it from ``begin > end`` selects exactly the
        same rows for the current coefficient file -- ``test_bg2.py`` pins that
        equivalence -- while surviving a renumbered sector.
        """
        if self.wraps:
            return direction >= self.begin or direction <= self.end
        return self.begin <= direction <= self.end

    def evaluate(self, wind_speed_dms: float, water_level_cm: float) -> float:
        """The raw regression value y = a*wsn + b*wst + c, before scaling."""
        return self.a * wind_speed_dms + self.b * water_level_cm + self.c


def read_sectors(path: Path | str = COEFFICIENT_FILE) -> tuple[Sector, ...]:
    """Read the BG2 regression table, mirroring the awk field selection."""
    sectors: list[Sector] = []
    for line in Path(path).read_text().splitlines():
        fields = line.split()
        if not fields or fields[0] not in QUANTITIES:
            continue
        sectors.append(
            Sector(
                quantity=fields[0],
                begin=int(line[_SECTOR_BEGIN]),
                end=int(line[_SECTOR_END]),
                a=float(line[_A]),
                b=float(line[_B]),
                c=float(line[_C]),
            )
        )
    if not sectors:
        raise ValueError(f"no BG2 regression rows found in {path}")
    return tuple(sectors)


def select_sector(
    sectors: tuple[Sector, ...], quantity: str, direction: float
) -> Sector:
    """Return the single sector covering ``direction`` for ``quantity``.

    The shell script writes a line per match and lets the last one win. Here an
    ambiguous or uncovered direction is an error, so a damaged coefficient file
    cannot quietly produce a boundary condition.
    """
    matches = [s for s in sectors if s.quantity == quantity and s.contains(direction)]
    if len(matches) != 1:
        raise ValueError(
            f"{len(matches)} {quantity} sectors match direction {direction}; expected 1"
        )
    return matches[0]


@dataclass(frozen=True)
class BoundaryCondition:
    """BG2 boundary values for one condition, raw and as written into a deck."""

    direction_deg: float
    wind_speed_ms: float
    water_level_cm: int
    hs_m: float
    tm10_s: float
    tp_s: float

    @property
    def hs_text(self) -> str:
        return deck_value(self.hs_m)

    @property
    def tm10_text(self) -> str:
        return deck_value(self.tm10_s)

    @property
    def tp_text(self) -> str:
        return deck_value(self.tp_s)

    @property
    def water_level_text(self) -> str:
        return level_text(self.water_level_cm)


def deck_value(value: float) -> str:
    """Format as the shell does: ``printf "%5.2f"`` followed by awk field split."""
    return f"{value:5.2f}".strip()


def level_text(water_level_cm: float) -> str:
    """Format the water level in metres as ``awk '{print $1/100}'`` does.

    awk's default OFMT is ``%.6g``, so 300 cm becomes ``3`` rather than
    ``3.00``. That is cosmetic for SWAN but kept identical so generated decks
    can be diffed against operationally generated ones.
    """
    return f"{water_level_cm / 100.0:.6g}"


def boundary_condition(
    direction_deg: float,
    wind_speed_ms: float,
    water_level_cm: int,
    sectors: tuple[Sector, ...] | None = None,
) -> BoundaryCondition:
    """Evaluate the BG2 regression for one condition.

    Floors are applied before Tp is derived, exactly as in the shell script:
    Tp follows the floored but unrounded Tm-1,0.

    The direction is range-checked here rather than in :func:`select_sector`,
    which stays faithful to the shell: there, a direction of 400 degrees
    silently lands in the wrap-around sector. No operational condition is out
    of range, so rejecting it cannot change a reproduced result.
    """
    if not 0.0 <= direction_deg <= 360.0:
        raise ValueError(
            f"wind direction {direction_deg} outside the nautical range 0-360"
        )
    if wind_speed_ms < 0.0:
        raise ValueError(f"negative wind speed {wind_speed_ms}")
    if sectors is None:
        sectors = read_sectors()
    wind_speed_dms = wind_speed_ms * 10.0

    hs_sector = select_sector(sectors, "HS", direction_deg)
    hs = max(HS_FLOOR_M, hs_sector.evaluate(wind_speed_dms, water_level_cm) / 100.0)

    tm10_sector = select_sector(sectors, "TM-10", direction_deg)
    tm10 = max(
        TM10_FLOOR_S, tm10_sector.evaluate(wind_speed_dms, water_level_cm) / 10.0
    )

    return BoundaryCondition(
        direction_deg=direction_deg,
        wind_speed_ms=wind_speed_ms,
        water_level_cm=water_level_cm,
        hs_m=hs,
        tm10_s=tm10,
        tp_s=TP_OVER_TM10 * tm10,
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("direction", type=float, help="wind direction (nautical deg)")
    parser.add_argument("wind_speed", type=float, help="wind speed (m/s)")
    parser.add_argument("water_level", type=int, help="water level (cm rel. NAP)")
    parser.add_argument(
        "--coefficients", type=Path, default=COEFFICIENT_FILE, help="regression table"
    )
    args = parser.parse_args()

    bc = boundary_condition(
        args.direction,
        args.wind_speed,
        args.water_level,
        sectors=read_sectors(args.coefficients),
    )
    print(f"HS_BG2: {bc.hs_text}")
    print(f"TM-1-0_BG2: {bc.tm10_text}")
    print(f"TP: {bc.tp_text}")
    print(f"WR: {bc.direction_deg:g}")
    print(f"WSN: {bc.wind_speed_ms * 10:g}")
    print(f"WST: {bc.water_level_cm}")


if __name__ == "__main__":
    main()
