#!/usr/bin/env python3
"""Measure the strict-build diagnostic inventory and guard it against backsliding.

Configures and builds SWAN with GNU Fortran's strict warning set, counts the
warnings per category, and fails when a budget is exceeded. The budget that
matters is `implicit-interface`: every remaining one is a call the compiler
cannot check, and the two that are left are calls into the METIS C library,
which can never have a Fortran interface. Any increase means a procedure went
back to being external, so the budget is deliberately exact rather than
generous.

The per-category counts alone cannot see a swap: one warning fixed and one
introduced in the same category leaves the total untouched. A second, finer
baseline closes that at file granularity. Each warning is reduced to a
fingerprint of (category, file, message) -- deliberately without line number or
source text, so that moving or reformatting code is invisible and only a
genuinely new warning shows up. Two clean parallel builds of the same tree
produce an identical fingerprint multiset, and a 51-file module split moved
none of them, so the comparison is quiet enough to enforce.

Always measures a clean build. An incremental one only reports the files it
recompiled, which silently understates every count.

Usage:
    scripts/strict_diagnostics.py [--build-dir DIR]
                                  [--update-budget] [--update-fingerprints]
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
FINGERPRINT_FILE = Path(__file__).resolve().parent / "strict_diagnostics_fingerprints.json"
# Every measured category is held at its recorded count: the inventory may
# shrink but never grow. Cleaning a category is therefore a deliberate act that
# ends with --update-budget, and an accidental increase fails the build.
ENFORCED = None  # None means "every category present in the budget"

# gfortran reports a warning either as a block -- location line, echoed source,
# caret, message -- or, when it has no source line to show, as a single line.
LOCATION = re.compile(r"^(?:\S*/)?([^/\s]+\.[fF]90):\d+:\d+:$")
MESSAGE = re.compile(r"^Warning: (.*) \[-W([a-z-]+)\]$")
INLINE = re.compile(
    r"^(?:\S*/)?([^/\s]+\.[fF]90):\d+:\d+: Warning: (.*) \[-W([a-z-]+)\]$"
)


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


def fingerprint(log: str) -> collections.Counter:
    """Count the warnings per (category, file, message).

    The build compiles generated sources out of the build tree, so only the
    base name is kept; it is the same name as the file under src/.
    """
    counts = collections.Counter()
    source = None
    for line in log.splitlines():
        stripped = line.strip()
        inline = INLINE.match(stripped)
        if inline:
            counts[(inline.group(3), inline.group(1), inline.group(2))] += 1
            continue
        location = LOCATION.match(stripped)
        if location:
            source = location.group(1)
            continue
        message = MESSAGE.match(stripped)
        if message:
            counts[(message.group(2), source, message.group(1))] += 1
    return counts


def compiler_identity(build_dir: Path) -> str:
    """Name the compiler the fingerprints were measured with.

    The fingerprints are only comparable within one compiler version: an
    upgrade rewrites the whole inventory, which is a re-measurement rather than
    a regression.
    """
    cache = (build_dir / "CMakeCache.txt").read_text().splitlines()
    path = next(line.split("=", 1)[1] for line in cache
                if line.startswith("CMAKE_Fortran_COMPILER:"))
    version = subprocess.run([path, "--version"], capture_output=True, text=True)
    return version.stdout.splitlines()[0].strip()


def read_fingerprints(path: Path) -> tuple[str, collections.Counter]:
    stored = json.loads(path.read_text())
    return stored["compiler"], collections.Counter(
        {tuple(row[:3]): row[3] for row in stored["fingerprints"]}
    )


def write_fingerprints(path: Path, compiler: str, counts: collections.Counter) -> None:
    rows = sorted([list(key) + [count] for key, count in counts.items()], key=repr)
    path.write_text(json.dumps({"compiler": compiler, "fingerprints": rows},
                               indent=1, ensure_ascii=False) + "\n")


def compare_fingerprints(baseline: collections.Counter,
                         measured: collections.Counter) -> list[str]:
    """Report every change, and return only the additions as failures.

    Removals are the point of the exercise and are printed for the record;
    additions are what the gate is for.
    """
    failures = []
    for key in sorted(set(baseline) | set(measured)):
        before, now = baseline[key], measured[key]
        if now > before:
            failures.append(f"+{now - before}  {key[0]}  {key[1]}: {key[2]}")
        elif now < before:
            print(f"  -{before - now}  {key[0]}  {key[1]}: {key[2]}")
    return failures


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build-dir", default="build-strict-diagnostics")
    parser.add_argument("--update-budget", action="store_true",
                        help="store the measured counts as the new budget")
    parser.add_argument("--update-fingerprints", action="store_true",
                        help="store the measured warnings as the new fingerprint baseline")
    arguments = parser.parse_args()

    source = Path(__file__).resolve().parent.parent
    build_dir = Path(arguments.build_dir).resolve()
    log = build(source, build_dir)
    counts = tally(log)
    measured = fingerprint(log)

    total = sum(counts.values())
    print(f"strict-build warnings: {total}")
    for category, count in counts.most_common():
        print(f"  {count:5d}  {category}")
    #  Every warning must end up in exactly one fingerprint. A mismatch means
    #  the compiler grew a diagnostic format this parser does not know, and the
    #  comparison below would silently be about a subset.
    if sum(measured.values()) != total:
        print(f"\n{total - sum(measured.values())} warnings were not recognised "
              f"by the fingerprint parser", file=sys.stderr)
        return 1

    budget = json.loads(BUDGET_FILE.read_text()) if BUDGET_FILE.is_file() else {}
    if arguments.update_budget:
        BUDGET_FILE.write_text(json.dumps(dict(counts.most_common()), indent=2) + "\n")
        print(f"budget updated in {BUDGET_FILE.name}")
    if arguments.update_fingerprints:
        write_fingerprints(FINGERPRINT_FILE, compiler_identity(build_dir), measured)
        print(f"fingerprints updated in {FINGERPRINT_FILE.name} "
              f"({len(measured)} distinct)")
    if arguments.update_budget or arguments.update_fingerprints:
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

    if not FINGERPRINT_FILE.is_file():
        print("no fingerprint baseline; run with --update-fingerprints")
        return 0
    compiler, baseline = read_fingerprints(FINGERPRINT_FILE)
    current = compiler_identity(build_dir)
    if compiler != current:
        #  Not a failure: a different compiler measures a different inventory,
        #  and comparing the two says nothing about the source.
        print(f"fingerprints skipped: baseline is {compiler}, this build is {current}")
        return 0
    print(f"fingerprints ({len(baseline)} distinct, {sum(baseline.values())} warnings):")
    additions = compare_fingerprints(baseline, measured)
    if additions:
        print("\n".join(["", "New warnings:"] + additions), file=sys.stderr)
        return 1
    print("  no new warnings")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
