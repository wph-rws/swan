#!/usr/bin/env python3
"""Gerichte poortselectie: welke testconfiguraties een wijziging moet halen.

Volledige breedte (serieel + OpenMP + MPI + strict) per wijziging kost vooral
muurtijd in (her)bouwen, niet in testuitvoering zelf (ctest is ~2 minuten).
Deze selector leidt uit de gewijzigde paden af welke poorten een wijziging
kán breken; alleen die hoeven opnieuw groen. Onbekende paden kiezen veilig
alles (fail-safe), nooit niets.

Regels:
- MPI alleen bij MPI-rakende paden (bron met mpi/parall/metis/coh/esmf/adcirc
  in de naam, MPI-tests, toolchain/CMake). Een deck- of fysicawijziging zonder
  codeverschil gedraagt zich onder MPI identiek aan serieel op dezelfde code;
  de MPI-claim blijft staan op de laatste groene commit.
- strict alleen bij bron-, toolchain- of budgetwijzigingen (alleen die kunnen
  waarschuwingen toevoegen of wegnemen).
- pytest is <1 seconde en loopt altijd mee zodra er iets te toetsen valt.
- Alleen docs/commentaar: geen enkele poort.

Gebruik:
    scripts/select_gates.py [--base HEAD] [--json] [pad ...]
Zonder paden wordt de wijziging (tracked diff + untracked, minus genegeerd)
tegen --base vergeleken. Met paden wordt exact die verzameling beoordeeld
(handig voor unit-tests en droge aves).
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
    "strict": "python3 scripts/strict_diagnostics.py --require-baseline",
    "pytest": "python3 -m pytest tests/ -q",
}

MPI_RE = re.compile(r"mpi|parall|metis|\bcoh\b|esmf|adcirc|decomp", re.IGNORECASE)


def _classify(path: str) -> set[str]:
    """Benodigde poorten voor één pad. Onbekend -> alles (fail-safe)."""
    p = Path(path)
    name = p.name
    suffix = p.suffix.lower()
    if p.parts and p.parts[0] in ("doc", "so-rp_swan"):
        # so-rp_swan is validatiebewijs, geen poortcode; docs hebben geen tests.
        # so-rp-wijzigingen lopen via de validatiematrix, niet via ctest.
        return set()
    if suffix == ".md" or name == "AGENTS.md":
        return set()
    if name.startswith("strict_diagnostics") or name in (
        "strict_diagnostics_budget.json",
        "strict_diagnostics_fingerprints.json",
    ):
        return {"strict"}
    if MPI_RE.search(str(p)):
        return {"serial", "openmp", "mpi", "strict", "pytest"}
    if suffix in (".f90", ".f", ".c", ".h") or name in ("CMakeLists.txt",) or p.parts[:1] == ("cmake",):
        return {"serial", "openmp", "strict", "pytest"}
    if p.parts[:1] == ("scripts",) and suffix == ".py":
        return {"serial", "pytest"}
    if p.parts[:1] == ("tests",):
        return {"serial", "openmp", "pytest"}
    if p.parts[:1] == ("examples",):
        return {"serial", "openmp", "pytest"}
    return {"serial", "openmp", "mpi", "strict", "pytest"}


def select_gates(paths: list[str]) -> dict[str, list[str]]:
    """Beoordeel een verzameling paden -> {poort: [redengevende paden]}."""
    reasons: dict[str, list[str]] = {gate: [] for gate in COMMANDS}
    for path in paths:
        for gate in _classify(path):
            reasons[gate].append(path)
    return {gate: sorted(set(r)) for gate, r in reasons.items() if r}


def worktree_changes(base: str) -> list[str]:
    """Gewijzigde paden t.o.v. base: tracked diff plus untracked (minus genegeerd)."""
    root = Path(__file__).resolve().parent.parent
    out = subprocess.run(
        ["git", "diff", "--name-only", base, "--", "."],
        capture_output=True, text=True, cwd=root,
    )
    paths = [l for l in out.stdout.splitlines() if l.strip()]
    st = subprocess.run(
        ["git", "status", "--porcelain", "--untracked-files=normal", "--", "."],
        capture_output=True, text=True, cwd=root,
    )
    for line in st.stdout.splitlines():
        if line.startswith("??"):
            paths.append(line[3:].strip().rstrip("/"))
    # Genegeerde paden (pycache, build-artefacten) horen nooit bij een wijziging.
    seen: dict[str, None] = {}
    for p in paths:
        if "__pycache__" in p or p.endswith((".pyc", ".o", ".mod")):
            continue
        seen.setdefault(p)
    return sorted(seen)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--base", default="HEAD", help="vergelijkingsbasis (default: HEAD)")
    ap.add_argument("--json", action="store_true", help="machineleesbare uitvoer")
    ap.add_argument("paths", nargs="*", help="beoordeel exact deze paden i.p.v. de wijziging")
    args = ap.parse_args(argv)
    paths = args.paths if args.paths else worktree_changes(args.base)
    selection = select_gates(paths)
    scope = "Opgegeven" if args.paths else f"t.o.v. {args.base}"
    if args.json:
        print(json.dumps({"paths": paths, "gates": selection}, indent=2))
        return 0
    if not paths:
        print(f"Boom schoon t.o.v. {args.base}: geen poorten vereist.")
        return 0
    if not selection:
        print(f"{len(paths)} gewijzigd(e) pad(en), alle documentatie/validatiebewijs: geen poorten vereist.")
        return 0
    skipped = [g for g in COMMANDS if g not in selection]
    print(f"Gewijzigd: {len(paths)} pad(en) {scope}. Vereist:")
    for gate in COMMANDS:
        if gate in selection:
            print(f"  [x] {gate}: {COMMANDS[gate]}")
    if skipped:
        print(f"Overgeslagen (niet MPI-/bron-rakend): {', '.join(skipped)}.")
        print("Overgeslagen poorten blijven staan op hun laatste groene commit.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
