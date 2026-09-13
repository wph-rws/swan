#!/usr/bin/env python3
"""Check that CI installs what the test layer imports.

The gates run on a bare runner. A module that happens to be present here in a
virtualenv is missing there — and then not one test fails but the whole
job, with an error message that has nothing to do with SWAN. That is exactly
what happened: `pytest`, `numpy` and `scipy` were not installed anywhere.

This check reads which external modules the test layer imports (anywhere in
a file, not only at the top), and requires every `apt-get install` line in the
workflow to install the matching package. An import behind `try:` counts as
optional: it may be missing, provided the code handles that itself. Imports
are not followed into other files of the repository.

Usage: python3 scripts/check_ci_dependencies.py [--workflow PATH]
"""

from __future__ import annotations

import argparse
import ast
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

#  Files that a gate actually runs (see the pytest and script steps in ci.yml).
TEST_LAYER = ("tests/*.py", "examples/*/run.py", "examples/*.py", "scripts/check_*.py",
            "scripts/ci_gate.py", "scripts/strict_diagnostics.py", "scripts/select_gates.py",
            "scripts/unstruc_determinism_smoke.py",
            "so-rp_swan/comparison/*.py")

#  Module name -> Debian package. Only what the test layer really needs.
PACKAGES = {"pytest": "python3-pytest", "numpy": "python3-numpy", "scipy": "python3-scipy",
          "matplotlib": "python3-matplotlib"}


def local_modules() -> set[str]:
    """Names of the repository's own Python modules, which need no package."""
    return {p.stem for p in ROOT.rglob("*.py")}


def external_imports(path: Path, local: set[str]) -> set[str]:
    """Modules this file imports outside a `try:`, minus stdlib and local ones."""
    try:
        tree = ast.parse(path.read_text())
    except (OSError, SyntaxError):
        return set()
    optional: set[int] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Try):
            for child in ast.walk(node):
                optional.add(id(child))
    found: set[str] = set()
    for node in ast.walk(tree):
        if id(node) in optional:
            continue
        if isinstance(node, ast.Import):
            found.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            found.add(node.module.split(".")[0])
    return {m for m in found
            if m not in sys.stdlib_module_names and m not in local}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workflow", default=".github/workflows/ci.yml")
    arguments = parser.parse_args()

    local = local_modules()
    needed: dict[str, set[str]] = {}
    for pattern in TEST_LAYER:
        for path in sorted(ROOT.glob(pattern)):
            for module in external_imports(path, local):
                needed.setdefault(module, set()).add(str(path.relative_to(ROOT)))

    unknown = sorted(m for m in needed if m not in PACKAGES)
    if unknown:
        print("CI dependencies: FAIL", file=sys.stderr)
        for module in unknown:
            print(f"  - {module} is imported by {sorted(needed[module])[0]} "
                  f"but is not in this script's package table", file=sys.stderr)
        return 1

    text = (ROOT / arguments.workflow).read_text()
    installs = re.findall(r"apt-get install -y ([^\n]*)", text)
    if not installs:
        print("CI dependencies: FAIL — no install step found", file=sys.stderr)
        return 1

    shortfalls: list[str] = []
    for line in installs:
        for module in sorted(needed):
            package = PACKAGES[module]
            if package not in line:
                shortfalls.append(f"{package} missing from: {line.strip()[:70]}...")

    if shortfalls:
        print("CI dependencies: FAIL", file=sys.stderr)
        for shortfall in sorted(set(shortfalls)):
            print(f"  - {shortfall}", file=sys.stderr)
        return 1

    print(f"CI dependencies: PASS ({len(installs)} jobs, "
          f"{len(needed)} external modules)")
    for module in sorted(needed):
        print(f"  {PACKAGES[module]:<20} for {len(needed[module])} file(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
