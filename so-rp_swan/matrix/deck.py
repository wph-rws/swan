#!/usr/bin/env python3
"""Generate validated standalone SWAN decks for the so-rp condition matrix."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

import bg2

TEMPLATE = bg2.ROOT / "par" / "so-rp_osk_org.swn"
PLACEHOLDERS = frozenset(
    {
        "WR_ORG",
        "WSN_ORG",
        "WST_ORG",
        "HS_ORG_BG2",
        "TP_ORG_BG2",
        "GOLFTR_OSK",
        "__SIGNED_LEVEL__",
        "__OSK_LABEL__",
    }
)
PHYSICS = ("default", "legacy-4131")
LEVEL_FORMATS = ("standalone", "operational")


@dataclass(frozen=True)
class Condition:
    """The inputs that uniquely define one stationary so-rp run."""

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


def _signed_level_text(water_level_cm: int) -> str:
    return f"{water_level_cm / 100.0:+.2f}"


def _level_command_text(water_level_cm: int, level_format: str) -> str:
    if level_format == "standalone":
        return f"{water_level_cm / 100.0:.2f}"
    if level_format == "operational":
        return bg2.level_text(water_level_cm)
    raise ValueError(
        f"unknown level format {level_format!r}; expected one of {LEVEL_FORMATS}"
    )


def validate_template(text: str) -> None:
    """Reject incomplete or structurally altered reconstruction templates."""
    for placeholder in PLACEHOLDERS:
        if placeholder not in text:
            raise ValueError(f"template is missing placeholder {placeholder}")

    expected_counts = {
        r"(?m)^SET LEVEL=": 1,
        r"(?m)^WIND ": 1,
        r"(?m)^BOUND SEGMENT ": 1,
        r"(?m)^OBST TRANS ": 3,
        r"(?m)^TRIAD ": 1,
        r"(?m)^COMPUTE$": 1,
        r"(?m)^STOP$": 1,
    }
    for pattern, expected in expected_counts.items():
        actual = len(re.findall(pattern, text))
        if actual != expected:
            raise ValueError(
                f"template has {actual} lines matching {pattern!r}; expected {expected}"
            )


def validate_deck(text: str, condition: Condition, physics: str) -> None:
    """Validate the generated commands and ensure substitution is complete."""
    leftovers = sorted(token for token in PLACEHOLDERS if token in text)
    if leftovers:
        raise ValueError(f"unsubstituted deck placeholders: {', '.join(leftovers)}")
    if re.search(r"\b[A-Z][A-Z0-9_]*(?:_ORG|_OSK)\b|__[A-Z0-9_]+__", text):
        raise ValueError("generated deck contains an unknown placeholder")

    bc = bg2.boundary_condition(
        condition.direction_deg,
        condition.wind_speed_ms,
        condition.water_level_cm,
    )
    required = (
        f"WIND {condition.wind_speed_ms}.0 {condition.direction_deg}.0",
        "BOUND SEGMENT IJ 46 67 220 67 CON PAR "
        f"{bc.hs_text} {bc.tp_text} {condition.direction_deg}.0 30.0",
    )
    for line in required:
        if text.count(line) != 1:
            raise ValueError(f"generated deck does not contain exactly one {line!r}")

    transmission = bg2.OSK_TRANSMISSION[condition.osk_state]
    obstacle = f"OBST TRANS {transmission:.3f} LINE "
    if text.count(obstacle) != 3:
        raise ValueError("generated deck does not contain three expected OSK obstacles")

    if physics == "default":
        if re.search(r"(?m)^GEN", text):
            raise ValueError("default-physics deck unexpectedly contains GEN")
    elif physics == "legacy-4131":
        if text.count("GEN3 KOMEN DRAG FIT") != 1:
            raise ValueError("legacy deck is missing GEN3 KOMEN DRAG FIT")
        triad = "TRIAD ITRIAD=11 URCRIT=0.2 URSLIM=0.01"
        if text.count(triad) != 1:
            raise ValueError("legacy deck is missing explicit 41.31 triad defaults")
    else:
        raise ValueError(f"unknown physics {physics!r}; expected one of {PHYSICS}")


def generate_deck(
    condition: Condition,
    *,
    physics: str = "default",
    level_format: str = "standalone",
    template_path: Path | str = TEMPLATE,
) -> str:
    """Render one deck and validate every condition-dependent command."""
    if physics not in PHYSICS:
        raise ValueError(f"unknown physics {physics!r}; expected one of {PHYSICS}")
    template = Path(template_path).read_text()
    validate_template(template)

    bc = bg2.boundary_condition(
        condition.direction_deg,
        condition.wind_speed_ms,
        condition.water_level_cm,
    )
    replacements = {
        "WR_ORG": str(condition.direction_deg),
        "WSN_ORG": str(condition.wind_speed_ms),
        "WST_ORG": _level_command_text(condition.water_level_cm, level_format),
        "HS_ORG_BG2": bc.hs_text,
        "TP_ORG_BG2": bc.tp_text,
        "GOLFTR_OSK": f"{bg2.OSK_TRANSMISSION[condition.osk_state]:.3f}",
        "__SIGNED_LEVEL__": _signed_level_text(condition.water_level_cm),
        "__OSK_LABEL__": condition.osk_state,
    }
    text = template
    for placeholder, value in replacements.items():
        text = text.replace(placeholder, value)

    if physics == "legacy-4131":
        text = text.replace(
            "BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES\n",
            "BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES\n"
            "GEN3 KOMEN DRAG FIT\n",
        )
        text = text.replace(
            "TRIAD URSLIM=0.01",
            "TRIAD ITRIAD=11 URCRIT=0.2 URSLIM=0.01",
        )

    validate_deck(text, condition, physics)
    return text


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("condition_id")
    parser.add_argument("direction", type=int)
    parser.add_argument("wind_speed", type=int)
    parser.add_argument("water_level", type=int)
    parser.add_argument("osk_state", choices=sorted(bg2.OSK_TRANSMISSION))
    parser.add_argument("--physics", choices=PHYSICS, default="default")
    parser.add_argument("--level-format", choices=LEVEL_FORMATS, default="standalone")
    parser.add_argument("--template", type=Path, default=TEMPLATE)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    condition = Condition(
        args.condition_id,
        args.direction,
        args.wind_speed,
        args.water_level,
        args.osk_state,
    )
    text = generate_deck(
        condition,
        physics=args.physics,
        level_format=args.level_format,
        template_path=args.template,
    )
    if args.output:
        args.output.write_text(text)
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
