#!/usr/bin/env python3
"""Generate validated so-rp decks from the operational template.

The template `par/so-rp_osk_org.swn` is the operational deck of Rijkswaterstaat
(`par/operational/`, see PROVENANCE.md there) with two corrections: a grid of
248 by 577 meshes, so that SWAN reads `so-rp.rgf` correctly, and the wave
boundary on the wet seaward edge of that grid, given as grid indices. The
placeholders are filled in the way `Run_so-rp_swan.sh` fills them. The deck
expects the directories `so-rp/`, `par/` and `dat/` and a swaninit with `#`
as the comment character; `stage_inputs` sets those up.

`template="production"` fills in the operational deck itself instead, with its
wrong grid and boundary, to measure what production computes. Compare
versions with the corrected template.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import shutil
from dataclasses import dataclass
from pathlib import Path

import bg2

TEMPLATE = bg2.ROOT / "par" / "so-rp_osk_org.swn"
# The deck as it runs in production, byte for byte (see PROVENANCE.md there):
# CGRID 249 578 for a grid of 248 by 577 meshes, and 18 XY boundary segments
# that lie on the grid as SWAN then wrongly reads it. SWAN reports the grid
# errors but carries on, because the deck sets MAXERR = 2.
PRODUCTION_TEMPLATE = bg2.ROOT / "par" / "operational" / "so-rp_osk_org.swn"
PRODUCTION_MD5 = "27eb876439c91f1496723cc65e2d695d"
PRODUCTION_SEGMENTS = 18
TEMPLATES = {"corrected": TEMPLATE, "production": PRODUCTION_TEMPLATE}
PLACEHOLDERS = frozenset(
    {"WR_ORG", "WSN_ORG", "WST_ORG", "HS_ORG_BG2", "TP_ORG_BG2", "GOLFTR_OSK"}
)
# operational: the deck as it is. legacy-4131: with the 41.31 behaviour that the
# deck does not set itself, as far as a deck can set it: the wind drag (DRAG FIT
# instead of Wu), an LTA that integrates over all directions (DINT=-1, 60
# degrees in 41.51) with the coupling coefficient of Madsen and Sorensen (IC=2,
# QuadWave in 41.51), and the triad parameters URCRIT 0.2 and URSLIM 0.01 (0.63
# and 0.1 in 41.51). SWAN reads IC only after URSLIM. The third harmonic and the
# group-velocity weighting of the 41.51 LTA have no deck setting.
PHYSICS = ("operational", "legacy-4131")
TRIAD_LINE = r"(?m)^(TRIAD\s+ITRIAD=1 TRFAC=0\.8 CUTFR=2\.5)$"
LEGACY_TRIAD = " DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2"
# SWAN counts grid indices from 0. The wet sea edge is row J=65 (I=29..218) and,
# on the north-east side, column I=28 (J=66..71). A segment one cell further in
# is not a boundary: SWAN accepts it without a message and then overwrites the
# boundary spectrum in the first sweep, so no waves enter the model.
BOUNDARY = ("BOUND SEGMENT IJ 218 65 29 65 ", "BOUND SEGMENT IJ 28 66 28 71 ")
GRID_LINE = "CGRID CURVILINEAR 248 577 "

# Input in the run directory -> source, and the output the deck writes.
INPUT_FILES = {
    "so-rp/so-rp.rgf": bg2.ROOT / "so-rp" / "so-rp.rgf",
    "so-rp/scaloost_rp.bot.recht": bg2.ROOT / "so-rp" / "scaloost_rp.bot.recht",
    "par/uitvoerpunten.par": bg2.ROOT / "par" / "uitvoerpunten.par",
}
TABLE = "dat/uitvoerpunten.tab"
SPECTRUM = "dat/uitvoerpunten.sp1"
# Columns of TABLE (from 0): XP YP DEP HSIG RTP TPS TMM10 TM01 TM02 DIR ... DHS
COLUMN = {"hs": 3, "rtp": 4, "tm01": 7, "tm02": 8, "dir": 9, "dhs": 13}


@dataclass(frozen=True)
class Condition:
    """The inputs that define one stationary so-rp run."""

    condition_id: str
    direction_deg: int
    wind_speed_ms: int
    water_level_cm: int
    osk_state: str

    def __post_init__(self) -> None:
        if not re.fullmatch(r"[a-z0-9][a-z0-9_-]*", self.condition_id):
            raise ValueError(f"invalid condition id {self.condition_id!r}")
        if self.direction_deg not in range(0, 361):
            raise ValueError("wind direction must be an integer from 0 through 360")
        if self.wind_speed_ms < 0:
            raise ValueError("wind speed cannot be negative")
        if self.osk_state not in bg2.OSK_TRANSMISSION:
            raise ValueError(
                f"unknown OSK state {self.osk_state!r}; "
                f"expected one of {sorted(bg2.OSK_TRANSMISSION)}"
            )


def validate_template(text: str, template: str = "corrected") -> None:
    """Reject a template that is not the corrected or the production deck."""
    if template not in TEMPLATES:
        raise ValueError(f"unknown template {template!r}; expected one of {sorted(TEMPLATES)}")
    for placeholder in PLACEHOLDERS:
        if placeholder not in text:
            raise ValueError(f"template is missing placeholder {placeholder}")
    production = template == "production"
    if production and hashlib.md5(text.encode()).hexdigest() != PRODUCTION_MD5:
        raise ValueError("template is not byte for byte the production deck")
    expected_counts = {
        r"(?m)^SET LEVEL = WST_ORG ": 1,
        r"(?m)^WIND WSN_ORG WR_ORG$": 1,
        r"(?m)^BOUND SEGMENT ": PRODUCTION_SEGMENTS if production else 2,
        r"(?m)^BOUND SEGMENT XY ": PRODUCTION_SEGMENTS if production else 0,
        r"(?m)^OBST TRANS GOLFTR_OSK ": 3,
        r"(?m)^GEN3 +KOMEN$": 1,
        TRIAD_LINE: 1,
        r"(?m)^NUMERIC STOPC ": 1,
        r"(?m)^COMPUTE$": 1,
        r"(?m)^STOP$": 1,
    }
    for pattern, expected in expected_counts.items():
        actual = len(re.findall(pattern, text))
        if actual != expected:
            raise ValueError(
                f"template has {actual} lines matching {pattern!r}; expected {expected}"
            )
    if production:
        return
    if text.count(GRID_LINE) != 1:
        raise ValueError("template does not use the 248 by 577 grid that so-rp.rgf holds")
    for segment in BOUNDARY:
        if text.count(segment) != 1:
            raise ValueError(f"template does not contain the boundary {segment.strip()!r}")


def validate_deck(
    text: str, condition: Condition, physics: str, template: str = "corrected"
) -> None:
    """Check the filled-in commands and that every placeholder was replaced."""
    leftovers = sorted(token for token in PLACEHOLDERS if token in text)
    if leftovers:
        raise ValueError(f"unsubstituted deck placeholders: {', '.join(leftovers)}")
    bc = bg2.boundary_condition(
        condition.direction_deg, condition.wind_speed_ms, condition.water_level_cm
    )
    waves = f"CON PAR {bc.hs_text} {bc.tp_text} {condition.direction_deg} 30.0\n"
    required = [f"WIND {condition.wind_speed_ms} {condition.direction_deg}\n"]
    if template == "production":
        segments = re.findall(r"(?m)^BOUND SEGMENT XY .*\n", text)
        if len(segments) != PRODUCTION_SEGMENTS or any(
            not segment.endswith(" " + waves) for segment in segments
        ):
            raise ValueError("production deck does not give all 18 segments the boundary waves")
    else:
        required += [f"{segment}{waves}" for segment in BOUNDARY]
    for line in required:
        if text.count(line) != 1:
            raise ValueError(f"generated deck does not contain exactly one {line!r}")
    transmission = bg2.OSK_TRANSMISSION[condition.osk_state]
    if text.count(f"OBST TRANS {transmission:.3f} LINE ") != 3:
        raise ValueError("generated deck does not contain three expected OSK obstacles")
    if physics not in PHYSICS:
        raise ValueError(f"unknown physics {physics!r}; expected one of {PHYSICS}")
    legacy = physics == "legacy-4131"
    drag = len(re.findall(r"(?m)^GEN3 +KOMEN DRAG FIT$", text))
    triad = len(re.findall(r"(?m)^TRIAD\s+ITRIAD=1 TRFAC=0\.8 CUTFR=2\.5"
                           + re.escape(LEGACY_TRIAD) + "$", text))
    if drag != legacy or triad != legacy:
        raise ValueError(f"{physics} deck has {drag} DRAG FIT and {triad} 41.31 triad lines")


def generate_deck(
    condition: Condition,
    *,
    physics: str = "operational",
    template: str = "corrected",
    template_path: Path | str | None = None,
) -> str:
    """Fill in the template for one condition and check the result."""
    if physics not in PHYSICS:
        raise ValueError(f"unknown physics {physics!r}; expected one of {PHYSICS}")
    if template not in TEMPLATES:
        raise ValueError(f"unknown template {template!r}; expected one of {sorted(TEMPLATES)}")
    source = Path(template_path or TEMPLATES[template]).read_text()
    validate_template(source, template)
    bc = bg2.boundary_condition(
        condition.direction_deg, condition.wind_speed_ms, condition.water_level_cm
    )
    replacements = {
        "WR_ORG": str(condition.direction_deg),
        "WSN_ORG": str(condition.wind_speed_ms),
        "WST_ORG": bg2.level_text(condition.water_level_cm),
        "HS_ORG_BG2": bc.hs_text,
        "TP_ORG_BG2": bc.tp_text,
        "GOLFTR_OSK": f"{bg2.OSK_TRANSMISSION[condition.osk_state]:.3f}",
    }
    text = source
    for placeholder, value in replacements.items():
        text = text.replace(placeholder, value)
    if physics == "legacy-4131":
        text = re.sub(r"(?m)^(GEN3 +KOMEN)$", r"\1 DRAG FIT", text, count=1)
        text = re.sub(TRIAD_LINE, r"\1" + LEGACY_TRIAD, text, count=1)
    validate_deck(text, condition, physics, template)
    return text


def swaninit_text(family: str) -> str:
    """The swaninit of the operational suite: `#` is the comment character."""
    highest = "   99" if family == "rws" else "99999"
    return (
        "    4                                   version of initialisation file\n"
        "RWS/DZL (Hydro Meteo Centrum Zeeland)   name of institute\n"
        "    3                                   command file ref. number\n"
        "INPUT                                   command file name\n"
        "    4                                   print file ref. number\n"
        "PRINT                                   print file name\n"
        "    4                                   test file ref. number\n"
        "                                        test file name\n"
        "    6                                   screen ref. number\n"
        f"{highest}                                   highest file ref. number\n"
        "#                                       comment identifier\n"
        "\t                                       TAB character\n"
        "\\                                       dir sep char in input file\n"
        "/                                       dir sep char replacing previous one\n"
        "    1                                   default time coding option\n"
    )


def stage_inputs(directory: Path, family: str = "modern") -> None:
    """Put the input files, the dat/ directory and the swaninit in a run directory."""
    for relative, source in INPUT_FILES.items():
        if not source.is_file():
            raise FileNotFoundError(f"missing so-rp input: {source}")
        destination = directory / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
    (directory / "dat").mkdir(exist_ok=True)
    (directory / "swaninit").write_text(swaninit_text(family))


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("condition_id")
    parser.add_argument("direction", type=int)
    parser.add_argument("wind_speed", type=int)
    parser.add_argument("water_level", type=int)
    parser.add_argument("osk_state", choices=sorted(bg2.OSK_TRANSMISSION))
    parser.add_argument("--physics", choices=PHYSICS, default="operational")
    parser.add_argument("--template", choices=sorted(TEMPLATES), default="corrected")
    parser.add_argument("--template-path", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    condition = Condition(
        args.condition_id, args.direction, args.wind_speed, args.water_level, args.osk_state
    )
    text = generate_deck(
        condition,
        physics=args.physics,
        template=args.template,
        template_path=args.template_path,
    )
    if args.output:
        args.output.write_text(text)
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
