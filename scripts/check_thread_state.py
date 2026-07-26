#!/usr/bin/env python3
"""Verify that doc/thread-state-manifest.md still matches the source.

The manifest records the remaining thread-private state after the completed
workspace migration. A THREADPRIVATE symbol that is added or removed without
updating it silently invalidates the ownership design. This script parses every
OpenMP THREADPRIVATE directive from the Fortran sources -- including the ones
behind a build switch such as `!TIMG` -- and compares the exact symbol set
against the manifest.

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

# A directive line, optionally preceded by a switch marker such as `!TIMG`, and
# optionally continued with `!$OMP&` on following lines.
_DIRECTIVE = re.compile(r"^\s*!(?P<switch>[A-Za-z0-9]*)!?\$omp\s*threadprivate\s*\(", re.I)
_CONTINUATION = re.compile(r"^\s*!(?:[A-Za-z0-9]*)!?\$omp&?\s*", re.I)
_SYMBOL = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def _switch_of(line: str) -> str:
    """Return the build switch guarding a directive, or "" when unconditional."""
    match = _DIRECTIVE.match(line)
    switch = match.group("switch") if match else ""
    # `!$OMP` itself yields an empty switch; `!TIMG!$OMP` yields "TIMG".
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


def manifest_symbols(manifest: Path = MANIFEST) -> set[str]:
    """Collect every symbol named in a backticked cell of the directive table."""
    text = manifest.read_text()
    table = re.search(
        r"^## De zeven resterende directives\s*$(.*?)^## ", text, re.M | re.S
    )
    if table is None:
        raise ValueError(f"{manifest}: directive table not found")

    symbols: set[str] = set()
    for row in table.group(1).splitlines():
        cells = [cell.strip() for cell in row.split("|")]
        if len(cells) < 6 or not cells[1].isdigit():
            continue
        for quoted in re.findall(r"`([^`]+)`", cells[4]):
            symbols |= {s.upper() for s in _SYMBOL.findall(quoted)}
    if not symbols:
        raise ValueError(f"{manifest}: directive table lists no symbols")
    return symbols


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
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, default=SOURCE_DIR)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    args = parser.parse_args()

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
