#!/usr/bin/env python3
"""Measure the strict-build diagnostic inventory and guard it against backsliding.

Configures and builds SWAN with GNU Fortran's strict warning set, counts the
warnings per category, and fails when a budget is exceeded. The budget that
matters is `implicit-interface`: every remaining one is a call the compiler
cannot check, and the two that are left are calls into the METIS C library,
die een expliciete C-koppeling via ISO_C_BINDING/BIND(C) kunnen krijgen
via de expliciete C-koppeling; "nooit een Fortran-interface" was onjuist. Any increase means a
procedure went back to being external, so the budget is deliberately exact
rather than generous.

The per-category counts alone cannot see a swap: one warning fixed and one
introduced in the same category leaves the total untouched. A second, finer
baseline closes that at file granularity. Each warning is reduced to a
fingerprint of (category, file, message) -- deliberately without line number or
source text, so that moving or reformatting code is invisible and only a
genuinely new warning shows up. Two clean parallel builds of the same tree
produce an identical fingerprint multiset, and a 51-file module split moved
none of them, so the comparison is quiet enough to enforce.

Telt uitsluitend één volledig geslaagde schone bouw. Configureer-
en bouwlogs van iedere poging worden bewaard; een mislukte schone bouw
blokkeert de poort. Er is geen incrementeel slotlog en geen concatenatie van
pogingen: hercompilatie kan dezelfde waarschuwing dubbel tellen. De zogeheten
module-afhankelijkheidsrace is een niet-gereproduceerde hypothese; dit script
claimt geen oorzaak of reparatie zonder bewaarde logs en reproductie.

Usage:
    scripts/strict_diagnostics.py [--build-dir DIR] [--require-baseline]
                                  [--update-budget] [--update-fingerprints]
    --require-baseline (regressiemodus, ook via CI): een ontbrekend budget,
    een ontbrekende fingerprintbaseline en een niet-passende compiler/variant
    zijn een fout. Zonder deze vlag blijft lokaal de oude soepele melding
    behouden, maar CI gebruikt altijd --require-baseline.
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
    """Eén volledig geslaagde schone bouw; bewaar configureer- en bouwlogs.

    Geen herhaling, geen incrementeel slotlog, geen concatenatie: een tweede
    poging in dezelfde bouwmap is incrementeel en verbergt waarschuwingen uit
    reeds gecompileerde bestanden; concatenatie telt hercompilatie dubbel.
    Bij falen blijven configureerlog en bouwlog bewaard en blokkeert de poort;
    een lokale herhaling dient alleen diagnose (nieuwe schone map).
    """
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir(parents=True)
    configure_log = build_dir / "strict-configure.log"
    build_log = build_dir / "strict-build.log"
    configured = subprocess.run(
        ["cmake", "-S", str(source), "-B", str(build_dir), "-G", "Unix Makefiles",
         "-DCMAKE_BUILD_TYPE=Release", f"-DCMAKE_Fortran_FLAGS={STRICT_FLAGS}"],
        capture_output=True, text=True,
    )
    configure_log.write_text(configured.stdout + configured.stderr)
    if configured.returncode != 0:
        raise SystemExit(
            f"strict configure failed (zie {configure_log}):\n"
            f"{(configured.stdout + configured.stderr)[-4000:]}"
        )
    finished = subprocess.run(
        ["make", "-C", str(build_dir), f"-j{len(os_sched_affinity())}"],
        capture_output=True, text=True,
    )
    build_log.write_text(finished.stdout + finished.stderr)
    if finished.returncode != 0:
        raise SystemExit(
            f"strict build failed (zie {configure_log} en {build_log}):\n"
            f"{(finished.stdout + finished.stderr)[-4000:]}"
        )
    return finished.stdout + finished.stderr


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

    Keep only the base name so fingerprints remain stable across absolute
    workspace paths and the generated build-configuration module.
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
    parser.add_argument("--require-baseline", action="store_true",
                        help="regressiemodus: ontbrekend budget, ontbrekende "
                        "fingerprintbaseline en niet-passende compiler/variant "
                        "zijn een fout (CI gebruikt dit altijd)")
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
    current_compiler = compiler_identity(build_dir)
    print(f"strict-build warnings: {total}")
    print(f"compiler: {current_compiler}")
    print(f"variant: Release + STRICT_FLAGS ({STRICT_FLAGS})")
    for category, count in counts.most_common():
        print(f"  {count:5d}  {category}")
    #  Every warning must end up in exactly one fingerprint. A mismatch means
    #  the compiler grew a diagnostic format this parser does not know, and the
    #  comparison below would silently be about a subset.
    if sum(measured.values()) != total:
        print(f"\n{total - sum(measured.values())} warnings were not recognised "
              f"by the fingerprint parser", file=sys.stderr)
        return 1

    if not BUDGET_FILE.is_file():
        message = (f"geen budgetbestand {BUDGET_FILE.name}; "
                   "run met --update-budget na beoordeelde inventaris")
        if arguments.require_baseline:
            print(message, file=sys.stderr)
            return 1
        print(message)
        return 0
    budget = json.loads(BUDGET_FILE.read_text())
    if arguments.update_budget:
        BUDGET_FILE.write_text(json.dumps(dict(counts.most_common()), indent=2) + "\n")
        print(f"budget updated in {BUDGET_FILE.name}")
    if arguments.update_fingerprints:
        write_fingerprints(FINGERPRINT_FILE, current_compiler, measured)
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
        message = ("geen fingerprintbaseline; run met --update-fingerprints "
                   "na beoordeelde inventaris")
        if arguments.require_baseline:
            print(message, file=sys.stderr)
            return 1
        print(message)
        return 0
    compiler, baseline = read_fingerprints(FINGERPRINT_FILE)
    if compiler != current_compiler:
        message = (f"fingerprints skipped: baseline is {compiler}, "
                   f"this build is {current_compiler}")
        if arguments.require_baseline:
            print(message + " (niet-passende compiler/variant in "
                  "regressiemodus is een fout)", file=sys.stderr)
            return 1
        print(message)
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
