#!/usr/bin/env python3
"""Verify that doc/thread-state-manifest.md still matches the source.

The fork keeps no thread-private module variables: what a thread keeps for
itself is a variable of the solver it runs, an element of an array with one
element per thread, or an argument (the manifest says why). This script
checks two things:

* no OpenMP THREADPRIVATE directive and no COPYIN clause exists in any
  Fortran source under the source directory, also not behind a build switch;
* every name that the manifest table "Per-thread state in the solvers" lists
  as PRIVATE or FIRSTPRIVATE is in that clause of the parallel region of the
  solver file the column names.

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

# An OpenMP directive line, optionally preceded by a custom switch marker
# (`!JAC!$OMP`), and optionally a continuation line (`!$OMP&`).
_OMP = re.compile(r"^\s*!(?:[A-Za-z0-9]*!)?\$omp&?(?P<body>.*)$", re.I)
_FORBIDDEN = re.compile(r"\b(threadprivate|copyin)\b", re.I)
_PARALLEL = re.compile(r"^\s*parallel\b", re.I)
_CLAUSE = re.compile(r"\b(firstprivate|private)\s*\(([^)]*)\)", re.I)
_SYMBOL = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_SOURCES = ("*.f90", "*.F90", "*.f90.in")


def source_files(source_dir: Path) -> list[Path]:
    """Every Fortran source under the source directory, also in its subdirectories."""
    return sorted({path for pattern in _SOURCES for path in source_dir.rglob(pattern)})


def forbidden_directives(source_dir: Path = SOURCE_DIR) -> list[tuple[str, int, str]]:
    """Every THREADPRIVATE directive or COPYIN clause: (file, line, keyword)."""
    found = []
    for path in source_files(source_dir):
        lines = path.read_text(errors="replace").splitlines()
        for number, line in enumerate(lines, 1):
            directive = _OMP.match(line)
            if directive is None:
                continue
            for keyword in _FORBIDDEN.findall(directive.group("body")):
                found.append((str(path.relative_to(source_dir)), number, keyword.upper()))
    return found


def private_clauses(path: Path) -> dict[str, set[str]]:
    """The names in the PRIVATE and FIRSTPRIVATE clauses of the parallel regions."""
    clauses: dict[str, set[str]] = {"PRIVATE": set(), "FIRSTPRIVATE": set()}
    lines = path.read_text(errors="replace").splitlines()
    index = 0
    while index < len(lines):
        directive = _OMP.match(lines[index])
        if directive is None or not _PARALLEL.match(directive.group("body")):
            index += 1
            continue
        # Collect the directive across its continuation lines; a blank line
        # between two of them does not end it.
        text = directive.group("body")
        while text.rstrip().endswith("&"):
            index += 1
            if index >= len(lines):
                raise ValueError(f"{path.name}: unterminated PARALLEL directive")
            if not lines[index].strip():
                continue
            continuation = _OMP.match(lines[index])
            if continuation is None:
                raise ValueError(f"{path.name}:{index + 1}: PARALLEL directive is cut off")
            text = text.rstrip().rstrip("&") + " " + continuation.group("body")
        for clause, names in _CLAUSE.findall(text):
            clauses[clause.upper()] |= {name.upper() for name in _SYMBOL.findall(names)}
        index += 1
    return clauses


def manifest_rows(manifest: Path = MANIFEST) -> list[tuple[set[str], str, str]]:
    """Return (symbols, file, clause) for every checked cell of the table."""
    text = manifest.read_text()
    table = re.search(
        r"^## Per-thread state in the solvers\s*$(.*?)^##", text, re.M | re.S
    )
    if table is None:
        raise ValueError(f"{manifest}: table 'Per-thread state in the solvers' not found")

    files: list[str] = []
    rows: list[tuple[set[str], str, str]] = []
    for row in table.group(1).splitlines():
        cells = [cell.strip() for cell in row.split("|")]
        if len(cells) < 6 or set(cells[1]) <= set("-: "):
            continue
        if not files:
            # The header names the solver file of each column.
            for cell in cells[3:5]:
                name = re.search(r"`([\w.]+\.f90)`", cell)
                if name is None:
                    raise ValueError(f"{manifest}: column '{cell}' names no source file")
                files.append(name.group(1))
            continue
        symbols: set[str] = set()
        for quoted in re.findall(r"`([^`]+)`", cells[1]):
            symbols |= {s.upper() for s in _SYMBOL.findall(quoted)}
        for file, cell in zip(files, cells[3:5]):
            clause = cell.strip("`").upper()
            if clause in ("PRIVATE", "FIRSTPRIVATE"):
                rows.append((symbols, file, clause))
    if not rows:
        raise ValueError(f"{manifest}: the table lists no PRIVATE or FIRSTPRIVATE name")
    return rows


def check(source_dir: Path = SOURCE_DIR, manifest: Path = MANIFEST) -> list[str]:
    """Return a list of problems; empty means the manifest is current."""
    problems = [
        f"{file}:{line}: {keyword} is not allowed; keep the state in a variable of "
        "the solver, in an array per thread, or pass it as an argument"
        for file, line, keyword in forbidden_directives(source_dir)
    ]

    clauses: dict[str, dict[str, set[str]]] = {}
    for symbols, file, clause in manifest_rows(manifest):
        if file not in clauses:
            path = source_dir / file
            if not path.is_file():
                problems.append(f"manifest names {file}, which is not in {source_dir}")
                clauses[file] = {"PRIVATE": set(), "FIRSTPRIVATE": set()}
            else:
                clauses[file] = private_clauses(path)
        for symbol in sorted(symbols - clauses[file][clause]):
            problems.append(
                f"manifest says {symbol} is {clause} in the parallel region of {file}, "
                "but the directive does not list it there"
            )
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-dir", type=Path, default=SOURCE_DIR)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    args = parser.parse_args()

    try:
        problems = check(args.source_dir, args.manifest)
    except (OSError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    if problems:
        for problem in problems:
            print(f"error: {problem}", file=sys.stderr)
        print(
            f"\n{len(problems)} problem(s). Bring the source and "
            f"{args.manifest.name} in line with each other.",
            file=sys.stderr,
        )
        return 1

    names = sum(len(symbols) for symbols, _, _ in manifest_rows(args.manifest))
    print(
        "thread-state manifest is current: no THREADPRIVATE or COPYIN under "
        f"{args.source_dir.name}/, {names} PRIVATE or FIRSTPRIVATE entries checked"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
