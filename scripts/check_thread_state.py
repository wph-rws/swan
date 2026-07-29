#!/usr/bin/env python3
"""Verify that doc/thread-state-manifest.md still matches the source.

The manifest records the remaining thread-private state after the completed
workspace migration. A THREADPRIVATE symbol that is added or removed without
updating it silently invalidates the ownership design. This script parses every
OpenMP THREADPRIVATE directive from the Fortran sources -- including any
remaining directives behind a custom build switch -- and compares the exact
symbol set against the manifest.

Exit status 0 means the manifest is current.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SOURCE_DIR = ROOT / "src"
MANIFEST = ROOT / "doc" / "thread-state-manifest.md"

# A directive line, optionally preceded by a custom switch marker, and
# optionally continued with `!$OMP&` on following lines.
_DIRECTIVE = re.compile(r"^\s*!(?P<switch>[A-Za-z0-9]*)!?\$omp\s*threadprivate\s*\(", re.I)
_CONTINUATION = re.compile(r"^\s*!(?:[A-Za-z0-9]*)!?\$omp&?\s*", re.I)
_SYMBOL = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def _switch_of(line: str) -> str:
    """Return the build switch guarding a directive, or "" when unconditional."""
    match = _DIRECTIVE.match(line)
    switch = match.group("switch") if match else ""
    # `!$OMP` itself yields an empty switch; `!JAC!$OMP` yields "JAC".
    return switch.upper()


def scan_sources(source_dir: Path = SOURCE_DIR) -> dict[str, tuple[str, int, str]]:
    """Map each THREADPRIVATE symbol to (file, line, switch)."""
    found: dict[str, tuple[str, int, str]] = {}
    for path in sorted(source_dir.glob("*.f90")):
        lines = path.read_text(errors="replace").splitlines()
        index = 0
        while index < len(lines):
            if not _DIRECTIVE.match(lines[index]):
                index += 1
                continue
            start = index
            switch = _switch_of(lines[index])
            # Collect the argument list across continuation lines.
            body = lines[index].split("(", 1)[1]
            while body.rstrip().endswith("&"):
                index += 1
                if index >= len(lines):
                    raise ValueError(f"{path.name}:{start + 1}: unterminated directive")
                body = body.rstrip().rstrip("&")
                body += _CONTINUATION.sub("", lines[index])
            for symbol in _SYMBOL.findall(body.split(")")[0]):
                key = symbol.upper()
                if key in found:
                    other = found[key]
                    raise ValueError(
                        f"{path.name}:{start + 1}: {symbol} is also threadprivate "
                        f"in {other[0]}:{other[1]}"
                    )
                found[key] = (path.name, start + 1, switch)
            index += 1
    return found


def manifest_rows(manifest: Path = MANIFEST) -> list[tuple[str, int, set[str]]]:
    """Return (file, line, symbols) for every row of the directive table."""
    text = manifest.read_text()
    # The heading counts the directives in words, so it changes whenever one is
    # added or removed -- exactly when this check matters most. Match the shape
    # of the heading, not the number in it.
    table = re.search(
        r"^## De \w+ resterende directives\s*$(.*?)^## ", text, re.M | re.S
    )
    if table is None:
        raise ValueError(f"{manifest}: directive table not found")

    rows: list[tuple[str, int, set[str]]] = []
    for row in table.group(1).splitlines():
        cells = [cell.strip() for cell in row.split("|")]
        if len(cells) < 6 or not cells[1].isdigit():
            continue
        symbols: set[str] = set()
        for quoted in re.findall(r"`([^`]+)`", cells[4]):
            symbols |= {s.upper() for s in _SYMBOL.findall(quoted)}
        location = re.search(r"\[([\w.]+):(\d+)\]", cells[2])
        if location is None:
            raise ValueError(f"{manifest}: row {cells[1]} has no file:line link")
        rows.append((location.group(1), int(location.group(2)), symbols))
    if not rows:
        raise ValueError(f"{manifest}: directive table lists no symbols")
    return rows


def manifest_symbols(manifest: Path = MANIFEST) -> set[str]:
    """Collect every symbol named in a backticked cell of the directive table."""
    return set().union(*(symbols for _, _, symbols in manifest_rows(manifest)))


def check(source_dir: Path = SOURCE_DIR, manifest: Path = MANIFEST) -> list[str]:
    """Return a list of problems; empty means the manifest is current."""
    found = scan_sources(source_dir)
    documented = manifest_symbols(manifest)
    problems = []

    for symbol in sorted(set(found) - documented):
        file, line, switch = found[symbol]
        guard = f" (switch {switch})" if switch else ""
        problems.append(
            f"undocumented threadprivate {symbol} at {file}:{line}{guard}"
        )
    for symbol in sorted(documented - set(found)):
        problems.append(f"manifest documents {symbol}, which is no longer threadprivate")

    # The symbol sets can agree while every link in the table points at the
    # wrong line, which is what happens when declarations move between modules.
    # A reader who follows a stale link lands on unrelated code, so check that
    # each row still points at the directive it claims.
    for file, line, symbols in manifest_rows(manifest):
        actual = {s for s, (f, ln, _) in found.items() if f == file and ln == line}
        if actual != symbols:
            problems.append(
                f"manifest points at {file}:{line} for "
                f"{', '.join(sorted(symbols))}, but that line declares "
                + (", ".join(sorted(actual)) if actual else "no threadprivate symbol")
            )
    return problems


def update_links(source_dir: Path = SOURCE_DIR, manifest: Path = MANIFEST) -> int:
    """Repoint each table row at the line its symbols are actually declared on.

    The symbol sets decide where a row belongs, so this only ever moves a link
    that already describes an existing directive. A row whose symbols no longer
    form one directive is left alone and still fails the check.
    """
    found = scan_sources(source_dir)
    text = manifest.read_text()
    fixed = 0
    for file, line, symbols in manifest_rows(manifest):
        places = {(f, ln) for s, (f, ln, _) in found.items() if s in symbols}
        if len(places) != 1:
            continue
        new_file, new_line = places.pop()
        actual = {s for s, (f, ln, _) in found.items()
                  if (f, ln) == (new_file, new_line)}
        if actual != symbols or (new_file, new_line) == (file, line):
            continue
        text = text.replace(
            f"[{file}:{line}](../src/{file}#L{line})",
            f"[{new_file}:{new_line}](../src/{new_file}#L{new_line})",
        )
        fixed += 1
    if fixed:
        manifest.write_text(text)
    return fixed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, default=SOURCE_DIR)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    parser.add_argument(
        "--update-links",
        action="store_true",
        help="repoint rows whose symbols moved to another line, then check",
    )
    args = parser.parse_args()

    if args.update_links:
        moved = update_links(args.source_dir, args.manifest)
        print(f"{moved} verwijzing(en) bijgewerkt")

    problems = check(args.source_dir, args.manifest)
    if problems:
        for problem in problems:
            print(f"error: {problem}", file=sys.stderr)
        print(
            f"\n{len(problems)} problem(s). Update {args.manifest.name} "
            "before changing thread-private state.",
            file=sys.stderr,
        )
        return 1

    found = scan_sources(args.source_dir)
    switched = sum(1 for _, _, switch in found.values() if switch)
    print(
        f"thread-state manifest is current: {len(found)} threadprivate symbols "
        f"({switched} behind a build switch)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
