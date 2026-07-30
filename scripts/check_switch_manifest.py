#!/usr/bin/env python3
"""Ratchet SWAN's custom source-switch surface against its manifest.

The source templates can carry more than one switch marker on a line, for
example ``!MPI!/impi``.  Scan by repeatedly peeling only markers that
``switch.py`` actually transforms; generic ``!WORD`` matching would count
ordinary Fortran comments and miss nested markers.

The manifest is both an ownership inventory and a ratchet:

* every transform marker must have exactly one manifest entry;
* a marker may disappear, but its occurrence count may not grow;
* a marker may not spread to a new file or procedure.

Use ``--report-json`` without a manifest to inspect the current source.
"""

from __future__ import annotations

import argparse
import ast
import copy
import json
import re
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE_DIR = ROOT / "src"
SWITCH_SCRIPT = ROOT / "switch.py"
MANIFEST = ROOT / "doc" / "switch-manifest.json"

_PROCEDURE_START = re.compile(
    r"^\s*(?!end\b)(?:(?:recursive|pure|elemental|impure|module)\s+)*"
    r"(?:[a-z][a-z0-9_]*(?:\s*\([^)]*\))?\s+)?"
    r"(subroutine|function)\s+([a-z][a-z0-9_]*)\b",
    re.I,
)
_PROCEDURE_END = re.compile(
    r"^\s*end\s*(subroutine|function)\b", re.I
)
_MODULE_START = re.compile(
    r"^\s*module\s+(?!procedure\b|subroutine\b|function\b)"
    r"([a-z][a-z0-9_]*)\b",
    re.I,
)
_MODULE_END = re.compile(r"^\s*end\s*module\b", re.I)


def transform_markers(path: Path = SWITCH_SCRIPT) -> list[str]:
    """Read the marker literals from ``transform`` without importing it."""
    tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    for node in tree.body:
        if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            continue
        if node.name != "transform":
            continue
        for statement in node.body:
            if not isinstance(statement, ast.Assign):
                continue
            if not any(
                isinstance(target, ast.Name) and target.id == "replacements"
                for target in statement.targets
            ):
                continue
            if not isinstance(statement.value, (ast.List, ast.Tuple)):
                break
            markers: list[str] = []
            for element in statement.value.elts:
                if not isinstance(element, (ast.List, ast.Tuple)):
                    raise ValueError("transform replacements must be pairs")
                marker = ast.literal_eval(element.elts[0])
                if not isinstance(marker, str):
                    raise ValueError("transform marker must be a string")
                markers.append(marker)
            if len(markers) != len(set(markers)):
                raise ValueError("duplicate marker in transform replacements")
            return markers
    raise ValueError("cannot find transform replacements in switch.py")


def peel_markers(line: str, markers: list[str]) -> tuple[list[str], str]:
    """Return all leading markers and the remaining source line."""
    found: list[str] = []
    remainder = line
    # Longest first makes the scan independent of prefix relationships.
    ordered = sorted(markers, key=len, reverse=True)
    while True:
        marker = next((item for item in ordered if remainder.startswith(item)), None)
        if marker is None:
            return found, remainder
        found.append(marker)
        remainder = remainder[len(marker) :]


def scan_sources(
    source_dir: Path, markers: list[str]
) -> dict[str, dict[str, object]]:
    counts = Counter()
    line_counts = Counter()
    files: dict[str, set[str]] = {marker: set() for marker in markers}
    procedures: dict[str, set[str]] = {marker: set() for marker in markers}
    scope_counts: dict[str, Counter[str]] = {
        marker: Counter() for marker in markers
    }

    for path in sorted(source_dir.glob("*.f90")):
        module = "<file>"
        procedure_stack: list[str] = []
        relative = path.name
        for line in path.read_text(encoding="ascii", errors="replace").splitlines():
            found, code = peel_markers(line, markers)

            module_start = _MODULE_START.match(code)
            if module_start:
                module = module_start.group(1)
            procedure_start = _PROCEDURE_START.match(code)
            if procedure_start:
                procedure_stack.append(procedure_start.group(2))

            if found:
                line_counts.update(set(found))
                scope = procedure_stack[-1] if procedure_stack else module
                for marker in found:
                    counts[marker] += 1
                    files[marker].add(relative)
                    procedures[marker].add(scope)
                    scope_counts[marker][f"{relative}:{scope}"] += 1

            if _PROCEDURE_END.match(code) and procedure_stack:
                procedure_stack.pop()
            if _MODULE_END.match(code):
                module = "<file>"
                procedure_stack.clear()

    return {
        marker: {
            "occurrences": counts[marker],
            "lines": line_counts[marker],
            "files": sorted(files[marker]),
            "procedures": sorted(procedures[marker], key=str.lower),
            "scope_counts": dict(sorted(scope_counts[marker].items())),
        }
        for marker in markers
    }


def source_summary(source_dir: Path, markers: list[str]) -> dict[str, int]:
    """Count unique marked lines and files, in addition to prefix instances."""
    occurrences = 0
    lines = 0
    files = 0
    for path in sorted(source_dir.glob("*.f90")):
        marked_file = False
        for line in path.read_text(
            encoding="ascii", errors="replace"
        ).splitlines():
            found, _ = peel_markers(line, markers)
            if found:
                occurrences += len(found)
                lines += 1
                marked_file = True
        files += marked_file
    return {"occurrences": occurrences, "lines": lines, "files": files}


def validate(manifest: dict[str, object], current: dict[str, dict[str, object]],
             markers: list[str], source_dir: Path = SOURCE_DIR) -> list[str]:
    errors: list[str] = []
    entries = manifest.get("markers")
    if not isinstance(entries, dict):
        return ["manifest field 'markers' must be an object"]

    expected = set(entries)
    transformed = set(markers)
    for marker in sorted(transformed - expected):
        errors.append(f"new transform marker is not in manifest: {marker}")
    for marker in sorted(expected - transformed):
        errors.append(f"manifest marker is no longer transformed: {marker}")

    for marker, entry in entries.items():
        if not isinstance(entry, dict):
            continue
        replacement_files = entry.get("replacement_files", [])
        if not isinstance(replacement_files, list):
            errors.append(f"{marker}: replacement_files must be a list")
            continue
        for relative in replacement_files:
            replacement = source_dir / str(relative)
            if not replacement.is_file():
                errors.append(f"{marker}: replacement file is missing: {relative}")
        fragments = entry.get("replacement_fragments", {})
        if not isinstance(fragments, dict):
            errors.append(f"{marker}: replacement_fragments must be an object")
            continue
        for relative, required in fragments.items():
            replacement = source_dir / str(relative)
            if not replacement.is_file():
                continue
            contents = replacement.read_text(encoding="ascii", errors="replace")
            if not isinstance(required, list):
                errors.append(
                    f"{marker}: replacement fragments for {relative} must be a list"
                )
                continue
            for fragment in required:
                if str(fragment) not in contents:
                    errors.append(
                        f"{marker}: replacement {relative} lacks {fragment!r}"
                    )

    required = {
        "owner", "variant", "counterpart", "dependency", "cmake_option",
        "tests", "max_occurrences", "files", "procedures",
    }
    for marker in sorted(expected & transformed):
        entry = entries[marker]
        if not isinstance(entry, dict):
            errors.append(f"{marker}: manifest entry must be an object")
            continue
        missing = required - set(entry)
        if missing:
            errors.append(f"{marker}: missing fields: {', '.join(sorted(missing))}")
            continue

        actual = current[marker]
        maximum = entry["max_occurrences"]
        if not isinstance(maximum, int) or maximum < 0:
            errors.append(f"{marker}: max_occurrences must be a nonnegative integer")
        elif actual["occurrences"] > maximum:
            errors.append(
                f"{marker}: occurrences grew from {maximum} to "
                f"{actual['occurrences']}"
            )

        if not isinstance(entry["files"], list) or not all(
            isinstance(item, str) for item in entry["files"]
        ):
            errors.append(f"{marker}: files must be a list of strings")
            continue
        old_files = set(entry["files"])
        new_files = set(actual["files"]) - old_files
        if new_files:
            errors.append(f"{marker}: new files: {', '.join(sorted(new_files))}")

        if not isinstance(entry["procedures"], list) or not all(
            isinstance(item, str) for item in entry["procedures"]
        ):
            errors.append(f"{marker}: procedures must be a list of strings")
            continue
        old_procedures = set(entry["procedures"])
        new_procedures = set(actual["procedures"]) - old_procedures
        if new_procedures:
            errors.append(
                f"{marker}: new procedures: {', '.join(sorted(new_procedures))}"
            )

    return errors


def self_test(markers: list[str]) -> list[str]:
    """Exercise nested scanning and every marker's growth ratchet."""
    errors: list[str] = []
    nested = peel_markers("!MPI!/impiMODULE MPI", markers)
    if nested != (["!MPI", "!/impi"], "MODULE MPI"):
        errors.append(f"nested marker scan returned {nested!r}")

    current = {
        marker: {
            "occurrences": 1,
            "lines": 1,
            "files": ["source.f90"],
            "procedures": ["OWNER"],
            "scope_counts": {"source.f90:OWNER": 1},
        }
        for marker in markers
    }
    entry = {
        "owner": "test",
        "variant": "positive",
        "counterpart": "negative",
        "dependency": "none",
        "cmake_option": "test",
        "tests": ["self-test"],
        "max_occurrences": 1,
        "files": ["source.f90"],
        "procedures": ["OWNER"],
    }
    manifest = {"markers": {marker: copy.deepcopy(entry) for marker in markers}}

    if validate(manifest, current, markers):
        errors.append("unchanged synthetic inventory did not validate")
    for marker in markers:
        grown = copy.deepcopy(current)
        grown[marker]["occurrences"] = 2
        result = validate(manifest, grown, markers)
        if not any(message.startswith(f"{marker}: occurrences grew") for message in result):
            errors.append(f"{marker}: growth was not rejected")
    result = validate(manifest, current, [*markers, "!NEW"])
    if "new transform marker is not in manifest: !NEW" not in result:
        errors.append("new transform marker was not rejected")
    probe = markers[0]
    replacements = copy.deepcopy(manifest)
    replacements["markers"][probe]["replacement_files"] = ["missing-backend.f90"]
    result = validate(replacements, current, markers)
    if not any("replacement file is missing" in message for message in result):
        errors.append("missing replacement file was not rejected")
    replacements = copy.deepcopy(manifest)
    replacements["markers"][probe]["replacement_files"] = [
        "swan_build_config.f90.in"
    ]
    replacements["markers"][probe]["replacement_fragments"] = {
        "swan_build_config.f90.in": ["missing replacement sentinel"]
    }
    result = validate(replacements, current, markers)
    if not any("lacks 'missing replacement sentinel'" in message for message in result):
        errors.append("missing replacement fragment was not rejected")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-dir", type=Path, default=SOURCE_DIR)
    parser.add_argument("--switch", type=Path, default=SWITCH_SCRIPT)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    parser.add_argument("--report-json", action="store_true")
    parser.add_argument("--self-test", action="store_true")
    arguments = parser.parse_args()

    try:
        markers = transform_markers(arguments.switch)
        if arguments.self_test:
            errors = self_test(markers)
            if errors:
                for error in errors:
                    print(error, file=sys.stderr)
                return 1
            print(f"switch manifest self-test: {len(markers)} markers guarded")
            return 0
        current = scan_sources(arguments.source_dir, markers)
        summary = source_summary(arguments.source_dir, markers)
        if arguments.report_json:
            print(json.dumps({"summary": summary, "markers": current}, indent=2))
            return 0
        manifest = json.loads(arguments.manifest.read_text(encoding="utf-8"))
        errors = validate(manifest, current, markers, arguments.source_dir)
    except (OSError, ValueError, SyntaxError, json.JSONDecodeError) as error:
        print(f"switch manifest check failed: {error}", file=sys.stderr)
        return 2

    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        return 1

    active = sum(bool(item["occurrences"]) for item in current.values())
    print(
        f"switch manifest current: {summary['occurrences']} prefixes on "
        f"{summary['lines']} lines in {summary['files']} files; "
        f"{active}/{len(markers)} markers in use"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
