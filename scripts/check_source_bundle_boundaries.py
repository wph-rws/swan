#!/usr/bin/env python3
"""Check that the source-term bundle types stay in the routines they belong to.

The derived types in ALLOWED bundle arguments that are passed through a few
levels of the source-term call chain. They must not spread further into the
leaf kernels: each type may be declared (as an argument or a local) only in
the routines listed for it, and in all of them. Exit status 0 means the
source still matches the list.
"""

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

#  Leaf kernels are often PURE or ELEMENTAL, and functions carry a result
#  type, so any prefix words (with an optional kind or length in parentheses)
#  are allowed; a declaration in such a routine would otherwise go unchecked.
START = re.compile(
    r"^\s*(?!end\b)(?:\w+(?:\s*\([^)]*\))?\s+)*(?:subroutine|function)\s+(\w+)",
    re.IGNORECASE,
)
END = re.compile(r"^\s*end\s+(?:subroutine|function)\b", re.IGNORECASE)
DECL = re.compile(r"\btype\s*\(\s*(\w+_t)\s*\)\s*(?:,[^:]*)?::", re.IGNORECASE)


def main() -> int:
    violations: list[str] = []
    receivers = {name: set() for name in ALLOWED}
    for path in sorted((ROOT / "src").rglob("*.f90")):
        #  A stack, so that the rest of a routine after a CONTAINS section or
        #  an interface body is still attributed to that routine.
        open_procedures: list[str] = []
        for number, line in enumerate(path.read_text(errors="replace").splitlines(), 1):
            start = START.match(line)
            if start:
                open_procedures.append(start.group(1).upper())
            procedure = open_procedures[-1] if open_procedures else None
            declaration = DECL.search(line)
            if declaration and declaration.group(1).lower() in ALLOWED and procedure:
                bundle = declaration.group(1).lower()
                receivers[bundle].add(procedure)
                if procedure not in ALLOWED[bundle]:
                    violations.append(
                        f"{path.relative_to(ROOT)}:{number}: {bundle} widened to leaf kernel {procedure}"
                    )
            if END.match(line) and open_procedures:
                open_procedures.pop()

    for bundle, expected in ALLOWED.items():
        if receivers[bundle] != expected:
            violations.append(
                f"{bundle}: holders {sorted(receivers[bundle])}, expected {sorted(expected)}"
            )
    if violations:
        print("Source bundle check: FAIL")
        print("\n".join(f"  - {item}" for item in violations))
        return 1
    print("Source bundle check: PASS")
    for bundle, procedures in ALLOWED.items():
        print(f"  {bundle}: {', '.join(sorted(procedures))}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
