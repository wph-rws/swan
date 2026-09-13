#!/usr/bin/env python3
"""Guard that every physics formulation stays reachable from at least one deck.

QUADRUPLET 1 was unreachable from every deck in the repository for four
commits. The bounds-checked CI build, the gate selector and the example
harness were all in place and all passed: the only thing missing was an input
that selected the formulation. Compiler and runtime diagnostics can only report
on code that runs, so coverage of the *option space* needs its own check.

The script works in two directions, and both have to hold:

  1. Every formulation constant declared in swan_physics_selection.f90 for the
     families below appears in INVENTORY. A formulation added to the code
     therefore forces an explicit decision here rather than silently becoming
     a dark branch.
  2. Every formulation marked COVERED is actually selected by at least one
     deck under examples/. Coverage that rots -- a deck deleted or its command
     edited -- fails the gate instead of quietly lapsing.

Formulations that are deliberately not covered carry a reason and are listed
in the report, so the gap stays visible rather than forgotten.

Usage:
    scripts/physics_coverage.py [--repository DIR] [--quiet]

Exit status is 0 when the inventory is complete and every covered formulation
is reachable, and 1 otherwise.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

COVERED = "covered"
DEFAULT = "default"
EXEMPT = "exempt"


@dataclass(frozen=True)
class Formulation:
    """One selectable formulation and how a deck asks for it.

    `pattern` is matched case-insensitively against each command line of every
    deck. SWAN accepts abbreviated keywords, so the patterns match the
    documented short forms too.
    """

    constant: str
    status: str
    pattern: str | None = None
    reason: str | None = None
    absent: str | None = None


#     The families this gate covers. Each maps the Fortran constant prefix to
#     the SWAN command that selects it. Families not listed here -- wind drag,
#     the ST6 sub-options, mud, vegetation -- are not yet inventoried; adding
#     one means adding its constants below.
INVENTORY: dict[str, tuple[Formulation, ...]] = {
    "IQUAD": (
        Formulation("IQUAD_OFF", COVERED, r"^\s*OFF\s+QUAD"),
        Formulation("IQUAD_DIA", COVERED, r"^\s*QUAD\w*\s+1\b"),
        Formulation("IQUAD_DIA_WAM", COVERED, r"^\s*QUAD\w*\s+2\b"),
        Formulation("IQUAD_EXACT", COVERED, r"^\s*QUAD\w*\s+3\b"),
        Formulation("IQUAD_MDIA", COVERED, r"^\s*QUAD\w*\s+4\b"),
        Formulation("IQUAD_FULL", COVERED, r"^\s*QUAD\w*\s+8\b"),
        # 51, 52 and 53 are three different transfer routines inside the XNL
        # suite, not three spellings of one; SWINTFXNL subtracts 50 and
        # dispatches on the remainder.
        Formulation("IQUAD_XNL_DEEP", COVERED, r"^\s*QUAD\w*\s+51\b"),
        Formulation("IQUAD_XNL_DEEP_WAM", COVERED, r"^\s*QUAD\w*\s+52\b"),
        Formulation("IQUAD_XNL_FINITE", COVERED, r"^\s*QUAD\w*\s+53\b"),
    ),
    "ITRIAD": (
        Formulation("ITRIAD_OFF", DEFAULT, absent=r"^\s*TRIAD"),
        Formulation("ITRIAD_LTA", COVERED, r"^\s*TRIAD.*\bLTA\b"),
        # ITRIAD = 11 is the pre-41.01 LTA and reaches its own arms of SWLTA
        # (FT = 0, group velocity from the local spectrum). One pattern for
        # both spellings reported it as covered while no deck selected it.
        Formulation(
            "ITRIAD_LTA_ORIGINAL",
            COVERED,
            r"^\s*TRIAD.*\bITRIAD\s*=\s*11\b",
        ),
        Formulation("ITRIAD_FTIM", COVERED, r"^\s*TRIAD.*\bFTIM\b"),
        Formulation("ITRIAD_DCTA", COVERED, r"^\s*TRIAD.*\bDCTA\b"),
        Formulation("ITRIAD_SPB", COVERED, r"^\s*TRIAD.*\bSPB\b"),
    ),
    "IBOT": (
        Formulation("IBOT_OFF", DEFAULT, absent=r"^\s*FRIC"),
        Formulation("IBOT_JONSWAP", COVERED, r"^\s*FRIC\w*\s+JON\w*(\s+CON\w*)?\s+[\d.]"),
        Formulation("IBOT_COLLINS", COVERED, r"^\s*FRIC\w*\s+COLL"),
        Formulation("IBOT_MADSEN", COVERED, r"^\s*FRIC\w*\s+MAD"),
        Formulation("IBOT_JONSWAP_VAR", COVERED, r"^\s*FRIC\w*\s+JON\w*\s+VAR"),
        Formulation("IBOT_RIPPLES", COVERED, r"^\s*FRIC\w*\s+RIP"),
    ),
    "ISURF": (
        Formulation("ISURF_OFF", COVERED, r"^\s*OFF\s+BRE"),
        Formulation("ISURF_CON", COVERED, r"^\s*BRE\w*\s+CON"),
        Formulation("ISURF_VAR", COVERED, r"^\s*BRE\w*\s+(VAR|NEL)"),
        Formulation("ISURF_RUE", COVERED, r"^\s*BRE\w*\s+RUE"),
        Formulation("ISURF_TG", COVERED, r"^\s*BRE\w*\s+TG\b"),
        Formulation("ISURF_BKD", COVERED, r"^\s*BRE\w*\s+BKD\b"),
        Formulation("ISURF_ASYM", COVERED, r"^\s*BRE\w*\s+ASYM\b"),
    ),
    "IBIPH": (
        Formulation(
            "IBIPH_OFF",
            EXEMPT,
            reason="not selectable from a deck; it is the pre-TRIAD default",
        ),
        Formulation("IBIPH_ELDEBERKY", COVERED, r"^\s*TRIAD"),
        Formulation("IBIPH_SAPR", COVERED, r"^\s*TRIAD.*\bBIPH\w*\s+SAPR\b"),
        Formulation("IBIPH_DEWIT", COVERED, r"^\s*TRIAD.*\bBIPH\w*\s+(DEWIT|WIT)\b"),
    ),
}

CONSTANT_PATTERN = re.compile(
    r"^\s*integer,\s*parameter\s*::\s*(\w+)\s*=", re.IGNORECASE
)


def declared_constants(selection_source: Path, family: str) -> set[str]:
    names = set()
    for line in selection_source.read_text(encoding="utf-8").splitlines():
        match = CONSTANT_PATTERN.match(line)
        if match and match.group(1).upper().startswith(f"{family}_"):
            names.add(match.group(1).upper())
    return names


def command_lines(deck: Path) -> list[str]:
    """Deck lines with comments and continuations folded away."""
    lines = []
    pending = ""
    for raw in deck.read_text(encoding="utf-8", errors="replace").splitlines():
        stripped = raw.strip()
        if not stripped or stripped.startswith("$"):
            continue
        if stripped.endswith("&"):
            pending += stripped[:-1] + " "
            continue
        lines.append(pending + stripped)
        pending = ""
    if pending:
        lines.append(pending)
    return lines


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository", type=Path, default=Path(__file__).resolve().parent.parent)
    parser.add_argument("--quiet", action="store_true")
    arguments = parser.parse_args()

    repository = arguments.repository.resolve()
    selection_source = repository / "src" / "swan_physics_selection.f90"
    if not selection_source.is_file():
        print(f"not found: {selection_source}", file=sys.stderr)
        return 1

    decks = sorted(repository.glob("examples/**/*.swn"))
    if not decks:
        print("no decks found under examples/", file=sys.stderr)
        return 1
    lines = [(deck, line) for deck in decks for line in command_lines(deck)]

    failures: list[str] = []
    exemptions: list[str] = []
    covered_count = 0

    for family, formulations in INVENTORY.items():
        inventoried = {formulation.constant for formulation in formulations}
        declared = declared_constants(selection_source, family)
        for missing in sorted(declared - inventoried):
            failures.append(
                f"{family}: {missing} is declared in swan_physics_selection.f90 "
                f"but absent from INVENTORY; add it as covered or exempt"
            )
        for stale in sorted(inventoried - declared):
            failures.append(
                f"{family}: {stale} is in INVENTORY but no longer declared in "
                f"swan_physics_selection.f90; remove it"
            )

        for formulation in formulations:
            if formulation.status == EXEMPT:
                exemptions.append(f"{formulation.constant}: {formulation.reason}")
                continue
            if formulation.status == DEFAULT:
                # Selected by omitting a command rather than by issuing one, so
                # a deck covers it exactly when that command is absent.
                expression = re.compile(formulation.absent, re.IGNORECASE)
                hits = [
                    deck for deck in decks
                    if not any(expression.search(line) for line in command_lines(deck))
                ]
                if hits:
                    covered_count += 1
                    if not arguments.quiet:
                        example = hits[0].relative_to(repository)
                        print(f"  [x] {formulation.constant:<20} {example} (by omission)")
                else:
                    failures.append(
                        f"{family}: {formulation.constant} is the state when "
                        f"{formulation.absent!r} is absent, but every deck issues it"
                    )
                continue
            expression = re.compile(formulation.pattern, re.IGNORECASE)
            hits = [deck for deck, line in lines if expression.search(line)]
            if hits:
                covered_count += 1
                if not arguments.quiet:
                    example = hits[0].relative_to(repository)
                    print(f"  [x] {formulation.constant:<20} {example}")
            else:
                failures.append(
                    f"{family}: {formulation.constant} is marked covered but no "
                    f"deck under examples/ selects it "
                    f"(pattern {formulation.pattern!r})"
                )

    if not arguments.quiet:
        print()
        for exemption in exemptions:
            print(f"  [ ] {exemption}")
        print()
        print(
            f"{covered_count} formulations reachable, {len(exemptions)} exempt, "
            f"across {len(decks)} decks"
        )

    if failures:
        print()
        for failure in failures:
            print(f"FAIL {failure}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
