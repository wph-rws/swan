#!/usr/bin/env python3
"""Measure the strict-build diagnostic inventory and guard it against backsliding.

Configures and builds SWAN with GNU Fortran's strict warning set, counts the
warnings per category, and fails when a budget is exceeded. The budget that
matters is `implicit-interface`: every remaining one is a call the compiler
cannot check, and the two that are left are calls into the METIS C library,
which can never have a Fortran interface. Any increase means a procedure went
back to being external, so the budget is deliberately exact rather than
generous.

Always measures a clean build. An incremental one only reports the files it
recompiled, which silently understates every count.

Usage:
    scripts/strict_diagnostics.py [--build-dir DIR] [--update-budget]
"""

from __future__ import annotations

import argparse
import collections
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path

STRICT_FLAGS = (
    "-Wall -Wextra -Wimplicit-interface -Wsurprising "
    "-Wconversion-extra -Wcharacter-truncation"
)
BUDGET_FILE = Path(__file__).resolve().parent / "strict_diagnostics_budget.json"
# Every measured category is held at its recorded count: the inventory may
# shrink but never grow. Cleaning a category is therefore a deliberate act that
# ends with --update-budget, and an accidental increase fails the build.
ENFORCED = None  # None means "every category present in the budget"


def build(source: Path, build_dir: Path) -> str:
    if build_dir.exists():
        shutil.rmtree(build_dir)
    subprocess.run(
        ["cmake", "-S", str(source), "-B", str(build_dir), "-G", "Unix Makefiles",
         "-DCMAKE_BUILD_TYPE=Release", f"-DCMAKE_Fortran_FLAGS={STRICT_FLAGS}"],
        check=True, stdout=subprocess.DEVNULL,
    )
    # The first parallel build can lose a module-ordering race; a second pass
    # settles it. Only the last attempt's failure is worth reporting.
    log = ""
    for attempt in range(3):
        finished = subprocess.run(
            ["make", "-C", str(build_dir), f"-j{len(os_sched_affinity())}"],
            capture_output=True, text=True,
        )
        log = finished.stdout + finished.stderr
        if finished.returncode == 0:
            return log
    raise SystemExit(f"strict build failed:\n{log[-4000:]}")


def os_sched_affinity() -> list[int]:
    import os
    try:
        return sorted(os.sched_getaffinity(0))
    except AttributeError:  # pragma: no cover - non-Linux
        return [0]


def tally(log: str) -> collections.Counter:
    counts = collections.Counter()
    for match in re.finditer(r"\[-W([a-z-]+)\]", log):
        counts[match.group(1)] += 1
    return counts


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", default="build-strict-diagnostics")
    parser.add_argument("--update-budget", action="store_true",
                        help="store the measured counts as the new budget")
    arguments = parser.parse_args()

    source = Path(__file__).resolve().parent.parent
    counts = tally(build(source, Path(arguments.build_dir).resolve()))

    total = sum(counts.values())
    print(f"strict-build warnings: {total}")
    for category, count in counts.most_common():
        print(f"  {count:5d}  {category}")

    budget = json.loads(BUDGET_FILE.read_text()) if BUDGET_FILE.is_file() else {}
    if arguments.update_budget:
        BUDGET_FILE.write_text(json.dumps(dict(counts.most_common()), indent=2) + "\n")
        print(f"budget updated in {BUDGET_FILE.name}")
        return 0

    enforced = budget.keys() if ENFORCED is None else ENFORCED
    failures = [
        f"{category}: {counts[category]} exceeds the budget of {budget[category]}"
        for category in enforced
        if category in budget and counts[category] > budget[category]
    ]
    #  A category the budget has never seen has an implicit budget of zero.
    #  Without this the ratchet only guards the warnings that already existed,
    #  so a change introducing an entirely new kind of warning would pass.
    if ENFORCED is None:
        failures += [
            f"{category}: {counts[category]} in a category the budget does not list"
            for category in sorted(counts)
            if category not in budget
        ]
    if failures:
        print("\n".join(["", "Diagnostic budget exceeded:"] + failures), file=sys.stderr)
        return 1
    print("within budget")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
