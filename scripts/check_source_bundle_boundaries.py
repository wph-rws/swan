#!/usr/bin/env python3
"""Guard the no-broadening boundary for source-term hop bundles."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ALLOWED = {
    "point_integrals_t": {"SOURCE", "SINTGRL", "SWOMPU", "SWANCOMPUNSTRUC"},
    "test_output_t": {"SOURCE", "SWOMPU", "SWANCOMPUNSTRUC"},
    "source_budget_t": {"SOURCE", "QCSOURCE", "SWOMPU", "SWANCOMPUNSTRUC"},
    "iteration_cache_t": {"SWOMPU", "SWCOMP"},
    "system_matrix_t": {"ACTION", "SOLMAT", "SOLMT1", "SOLPRE", "SWSIP", "SWOMPU", "SWANCOMPUNSTRUC"},
}

START = re.compile(r"^\s*subroutine\s+(\w+)", re.IGNORECASE)
END = re.compile(r"^\s*end\s+subroutine\b", re.IGNORECASE)
DECL = re.compile(r"\btype\s*\(\s*(\w+_t)\s*\)\s*(?:,[^:]*)?::", re.IGNORECASE)


def main() -> int:
    violations: list[str] = []
    receivers = {name: set() for name in ALLOWED}
    for path in sorted((ROOT / "src").rglob("*.f90")):
        procedure: str | None = None
        for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
            start = START.match(line)
            if start:
                procedure = start.group(1).upper()
            declaration = DECL.search(line)
            if declaration and declaration.group(1).lower() in ALLOWED and procedure:
                bundle = declaration.group(1).lower()
                receivers[bundle].add(procedure)
                if procedure not in ALLOWED[bundle]:
                    violations.append(
                        f"{path.relative_to(ROOT)}:{number}: {bundle} verbreed naar bladkernel {procedure}"
                    )
            if END.match(line):
                procedure = None

    for bundle, expected in ALLOWED.items():
        if receivers[bundle] != expected:
            violations.append(
                f"{bundle}: houders {sorted(receivers[bundle])}, verwacht {sorted(expected)}"
            )
    if violations:
        print("Bronbundelcontrole: FAIL")
        print("\n".join(f"  - {item}" for item in violations))
        return 1
    print("Bronbundelcontrole: PASS")
    for bundle, procedures in ALLOWED.items():
        print(f"  {bundle}: {', '.join(sorted(procedures))}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
