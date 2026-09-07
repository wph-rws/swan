#!/usr/bin/env python3
"""Verify that SWCLME and the run-state ownership manifest agree exactly."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = ROOT / "doc" / "run-state-ownership.json"


def names(pattern: str, body: str) -> set[str]:
    return {match.upper() for match in re.findall(pattern, body, re.I)}


def procedure_body(source: Path, procedure: str) -> str:
    text = source.read_text(errors="replace")
    match = re.search(
        rf"^\s*SUBROUTINE\s+{re.escape(procedure)}\b.*?"
        rf"^\s*end\s+subroutine\s+{re.escape(procedure)}\b",
        text,
        re.I | re.M | re.S,
    )
    if match is None:
        raise ValueError(f"{source}: procedure {procedure} not found")
    return "\n".join(line.split("!", 1)[0] for line in match.group().splitlines())


def check(manifest_path: Path) -> list[str]:
    data = json.loads(manifest_path.read_text())
    source = ROOT / data["source"]
    body = procedure_body(source, data["procedure"])
    problems: list[str] = []

    expected_deallocate: set[str] = set()
    expected_delete: set[str] = set()
    expected_latches: set[str] = set()
    claimed: dict[str, str] = {}

    for group in data["groups"]:
        owner = group["owner"]
        cleanup = group["legacy_cleanup"]
        symbols = {symbol.upper() for symbol in group["symbols"]}
        latches = {symbol.upper() for symbol in group.get("latches", [])}
        for symbol in symbols | latches:
            if symbol in claimed:
                problems.append(
                    f"{symbol} is claimed by both {claimed[symbol]} and {owner}"
                )
            claimed[symbol] = owner
        if cleanup == "deallocate":
            expected_deallocate |= symbols
        elif cleanup == "delete":
            expected_delete |= symbols
            expected_latches |= latches
        elif cleanup == "owner_call":
            call = group["target_cleanup"].upper()
            if not re.search(rf"\bCALL\s+{re.escape(call)}\s*\(", body, re.I):
                problems.append(f"{owner}: owner call {call} is absent from SWCLME")
        else:
            problems.append(f"{owner}: unknown legacy_cleanup {cleanup!r}")

        for allocation_source in group.get("allocation_sources", []):
            if not (ROOT / allocation_source).exists():
                problems.append(
                    f"{owner}: allocation source does not exist: {allocation_source}"
                )

    actual_deallocate = names(r"\bDEALLOCATE\s*\(\s*([A-Z][A-Z0-9_]*)", body)
    actual_delete = names(r"\bCALL\s+DELETE\s*\(\s*([A-Z][A-Z0-9_]*)", body)
    actual_latches = names(r"\b([A-Z][A-Z0-9_]*)\s*=\s*\.FALSE\.", body)

    for label, expected, actual in (
        ("DEALLOCATE", expected_deallocate, actual_deallocate),
        ("DELETE", expected_delete, actual_delete),
        ("latch reset", expected_latches, actual_latches),
    ):
        for symbol in sorted(actual - expected):
            problems.append(f"unowned {label} action for {symbol} in SWCLME")
        for symbol in sorted(expected - actual):
            problems.append(f"manifest expects {label} action for absent {symbol}")

    for call in data["owner_calls"]:
        normalized = call.upper()
        if not re.search(rf"\bCALL\s+{re.escape(normalized)}\s*\(", body, re.I):
            problems.append(f"registered owner call {call} is absent from SWCLME")

    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    args = parser.parse_args()

    try:
        problems = check(args.manifest)
    except (OSError, ValueError, KeyError, json.JSONDecodeError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    if problems:
        for problem in problems:
            print(f"error: {problem}", file=sys.stderr)
        return 1

    data = json.loads(args.manifest.read_text())
    deallocations = sum(
        len(group["symbols"])
        for group in data["groups"]
        if group["legacy_cleanup"] == "deallocate"
    )
    deletes = sum(
        len(group["symbols"])
        for group in data["groups"]
        if group["legacy_cleanup"] == "delete"
    )
    latches = sum(
        len(group.get("latches", []))
        for group in data["groups"]
        if group["legacy_cleanup"] == "delete"
    )
    print(
        "run-state ownership is current: "
        f"{deallocations} deallocations, {deletes} list deletes, "
        f"{latches} latch resets, {len(data['owner_calls'])} owner calls"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
