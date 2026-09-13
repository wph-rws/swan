#!/usr/bin/env python3
"""Targeted gate selection: which test configurations a change must pass.

Full breadth (serial + OpenMP + MPI + strict) for every change mostly costs
wall time in (re)building, not in running the tests (ctest takes ~2 minutes).
This selector works out from the changed paths which gates a change
can break; only those need to go green again. Unknown paths safely choose
everything (fail-safe), never nothing.

Rules:
- MPI only for paths that touch MPI (source with mpi/parall/metis/coh/esmf/
  adcirc/decomp in the name, MPI tests) and for every change to CMakeLists.txt
  or cmake/. A deck or physics change without a code difference behaves under
  MPI exactly as in serial on the same code; the MPI claim stays at the last
  green commit.
- strict only for source, toolchain or baseline changes (only those can
  add or remove warnings).
- upstream_delta for anything that can shift the output of a deck: source,
  toolchain, decks and the delta inventory itself. That gate compares the fork
  deck by deck with the pinned TU Delft source, so that a new difference must
  get a BF reference instead of going unnoticed.
- pytest takes <1 second and always runs as soon as there is anything to test.
- Docs/comments only: no gate at all.

Usage:
    scripts/select_gates.py [--base HEAD] [--json] [path ...]
Without paths, the change (tracked diff + untracked, minus ignored) is
compared against --base. With paths, exactly that set is assessed
(useful for unit tests and dry runs).
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

COMMANDS = {
    "serial": 'cmake --build build -j"$(nproc)" && ctest --test-dir build --output-on-failure -j"$(nproc)"',
    "openmp": 'cmake --build build-modernization-openmp -j"$(nproc)" && ctest --test-dir build-modernization-openmp --output-on-failure -j"$(nproc)"',
    "mpi": 'cmake --build build-modernization-mpi -j"$(nproc)" && ctest --test-dir build-modernization-mpi --output-on-failure -j"$(nproc)"',
    "strict": "python3 scripts/strict_diagnostics.py",
    "pytest": "python3 -m pytest tests/ so-rp_swan/comparison -q",
    "upstream_delta": "SWAN_UPSTREAM_DELTA=1 python3 -m pytest tests/test_upstream_delta.py -q",
}

MPI_RE = re.compile(r"mpi|parall|metis|\bcoh\b|esmf|adcirc|decomp", re.IGNORECASE)


def _classify(path: str) -> set[str]:
    """Gates needed for one path. Unknown -> everything (fail-safe)."""
    p = Path(path)
    name = p.name
    suffix = p.suffix.lower()
    #  The delta inventory and the script around it are the gate itself: whoever
    #  touches them must run it, even though the manifest lives under doc/.
    if name.startswith("upstream_delta") or name == "upstream-delta.json":
        return {"pytest", "upstream_delta"}
    if p.parts[:1] == ("so-rp_swan",) and suffix == ".py":
        #  The matrix and comparison scripts have their own pytest tests, and
        #  scripts/check_mpi_field_mask.py (a ctest entry point) imports
        #  comparison/result_stats.py.
        if name == "result_stats.py":
            return {"serial", "pytest"}
        return {"pytest"}
    if p.parts and p.parts[0] in ("doc", "so-rp_swan"):
        # Other so-rp files are validation evidence and input; docs have
        # no tests. so-rp runs go through the validation matrix, not ctest.
        return set()
    if suffix == ".md" or name == "AGENTS.md":
        return set()
    if name.startswith("strict_diagnostics"):
        #  Script and triage register are also checked by pytest
        #  (tests/test_strict_diagnostics*.py).
        return {"strict", "pytest"}
    #  Build set-up and toolchain affect every variant, MPI included (AGENTS.md).
    if MPI_RE.search(str(p)) or name == "CMakeLists.txt" or p.parts[:1] == ("cmake",):
        return {"serial", "openmp", "mpi", "strict", "pytest", "upstream_delta"}
    if suffix in (".f90", ".f", ".c", ".h"):
        return {"serial", "openmp", "strict", "pytest", "upstream_delta"}
    if p.parts[:1] == ("scripts",) and suffix == ".py":
        return {"serial", "pytest"}
    if p.parts[:1] == ("tests",):
        return {"serial", "openmp", "pytest"}
    if p.parts[:1] == ("examples",):
        return {"serial", "openmp", "pytest", "upstream_delta"}
    return {"serial", "openmp", "mpi", "strict", "pytest", "upstream_delta"}


def select_gates(paths: list[str]) -> dict[str, list[str]]:
    """Assess a set of paths -> {gate: [paths that call for it]}."""
    reasons: dict[str, list[str]] = {gate: [] for gate in COMMANDS}
    for path in paths:
        for gate in _classify(path):
            reasons[gate].append(path)
    return {gate: sorted(set(r)) for gate, r in reasons.items() if r}


def _git(root: Path, args: list[str]) -> str:
    """Run git and return its output; a git error stops the selection.

    Without this, a mistyped --base gives empty output, and that reads as a
    clean tree for which no gate is needed.
    """
    result = subprocess.run(["git", *args], capture_output=True, text=True, cwd=root)
    if result.returncode != 0:
        raise SystemExit(f"git {' '.join(args)} failed: {result.stderr.strip()}")
    return result.stdout


def worktree_changes(base: str) -> list[str]:
    """Changed paths relative to base: tracked diff plus untracked (minus ignored)."""
    root = Path(__file__).resolve().parent.parent
    out = _git(root, ["diff", "--name-only", base, "--", "."])
    paths = [l for l in out.splitlines() if l.strip()]
    st = _git(root, ["status", "--porcelain", "--untracked-files=normal", "--", "."])
    for line in st.splitlines():
        if line.startswith("??"):
            paths.append(line[3:].strip().rstrip("/"))
    # Ignored paths (pycache, build artefacts) are never part of a change.
    seen: dict[str, None] = {}
    for p in paths:
        if "__pycache__" in p or p.endswith((".pyc", ".o", ".mod")):
            continue
        seen.setdefault(p)
    return sorted(seen)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--base", default="HEAD", help="base to compare against (default: HEAD)")
    ap.add_argument("--json", action="store_true", help="machine-readable output")
    ap.add_argument("paths", nargs="*", help="assess exactly these paths instead of the change")
    args = ap.parse_args(argv)
    paths = args.paths if args.paths else worktree_changes(args.base)
    selection = select_gates(paths)
    scope = "as given" if args.paths else f"relative to {args.base}"
    if args.json:
        print(json.dumps({"paths": paths, "gates": selection}, indent=2))
        return 0
    if not paths:
        print(f"Tree clean relative to {args.base}: no gates required.")
        return 0
    if not selection:
        print(f"{len(paths)} changed path(s), all documentation/validation evidence: no gates required.")
        return 0
    skipped = [g for g in COMMANDS if g not in selection]
    print(f"Changed: {len(paths)} path(s) {scope}. Required:")
    for gate in COMMANDS:
        if gate in selection:
            print(f"  [x] {gate}: {COMMANDS[gate]}")
    if skipped:
        print(f"Skipped (not affected by this change): {', '.join(skipped)}.")
        print("Skipped gates stay at their last green commit.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
