#!/usr/bin/env python3
"""Show when a stationary run stops, and how far it is then from its balance.

The script runs one small deck several times, each time with another NUMERIC
line, and compares the Hs that each run writes with that of a reference run
that is forced to keep iterating (NPNTS=101, so that STOPC never stops it).
It reports, per variant, the number of iterations, the last "accuracy OK"
percentage that SWAN printed, and the largest and the mean difference in Hs
with the reference. It is the measurement behind the convergence section of
AGENTS.md and the comments of examples/nonlinear_interactions/quad/quad_dia1.swn.

The deck must write Hs: a BLOCK file with HSIGN alone, or a TABLE whose
header names HSIGN (only that column is compared). Each variant replaces the
NUMERIC line of the deck. A variant is NAME=NUMERIC-LINE; the reference is
given the same way with --reference. With --quad-upstream the word UPSTREAM
is put at the end of the QUADRUPLET line (the upstream semi-implicit DIA, see
BF-62). Run directories are made under --work and never reused.

Example (the so-rp numerics on the test model sphys):

    python3 scripts/stop_check.py examples/testmodels/sphys/INPUT \\
        --exe build/bin/swan.exe --work /tmp/stop-sphys \\
        --reference "ref=NUMERIC STOPC DABS=0.00 DREL=0.01 CURVAT=0.001 NPNTS=101 STAT MXITST=600 ALFA=0.01" \\
        --variant "stopc=NUMERIC STOPC DABS=0.00 DREL=0.01 CURVAT=0.001 NPNTS=98 STAT MXITST=40 ALFA=0.01" \\
        --variant "extrap=NUMERIC STOPC DABS=0.00 DREL=0.01 CURVAT=0.001 NPNTS=98 STAT MXITST=600 ALFA=0.01 EXTRAP 10"
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
from pathlib import Path


def edit_deck(text: str, numeric: str, quad_upstream: bool) -> str:
    lines = text.splitlines()
    found = 0
    for i, line in enumerate(lines):
        if re.match(r"^\s*NUM(ERIC)?\b", line, re.I):
            lines[i] = numeric
            found += 1
        elif quad_upstream and re.match(r"^\s*QUAD(RUPL(ET)?)?\b", line, re.I):
            lines[i] = line.rstrip() + " UPSTREAM"
    if found != 1:
        raise ValueError(f"the deck must have exactly one NUMERIC line, found {found}")
    return "\n".join(lines) + "\n"


def hs_values(directory: Path, output: str) -> list[float]:
    text = (directory / output).read_text().splitlines()
    if output.endswith(".tbl"):
        header = next(i for i, line in enumerate(text) if "Hsig" in line or "HSIGN" in line.upper())
        names = text[header].lstrip("%").split()
        column = next(i for i, name in enumerate(names) if name.lower() in ("hsig", "hsign"))
        rows = [line.split() for line in text if line.strip() and not line.lstrip().startswith("%")]
        return [float(row[column]) for row in rows]
    return [float(value) for line in text for value in line.split()]


def run(deck: Path, name: str, numeric: str, args) -> dict:
    directory = args.work / name
    if directory.exists():
        raise FileExistsError(f"refusing to reuse {directory}")
    shutil.copytree(deck.parent, directory)
    deck_name = "INPUT" if deck.name == "INPUT" else deck.name
    text = edit_deck((directory / deck_name).read_text(), numeric, args.quad_upstream)
    (directory / "INPUT").write_text(text)
    environment = dict(os.environ, OMP_NUM_THREADS=str(args.threads))
    with open(directory / "screen.log", "w") as screen:
        code = subprocess.run([str(args.exe)], cwd=directory, env=environment,
                              stdout=screen, stderr=subprocess.STDOUT).returncode
    printed = (directory / "PRINT").read_text(errors="replace")
    iterations = [int(n) for n in re.findall(r"iteration\s+(\d+);", printed)]
    accuracy = re.findall(r"accuracy OK in\s+([\d.]+)\s*%", printed)
    return {"name": name, "exit": code, "numeric": numeric,
            "iterations": max(iterations) if iterations else None,
            "accuracy_ok_percent": float(accuracy[-1]) if accuracy else None,
            "hs": hs_values(directory, args.output)}


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("deck", type=Path, help="deck file; its directory is copied whole")
    parser.add_argument("--exe", type=Path, required=True)
    parser.add_argument("--work", type=Path, required=True)
    parser.add_argument("--output", default="hs.blk", help="file with Hs (BLOCK or TABLE)")
    parser.add_argument("--reference", required=True, help="NAME=NUMERIC-LINE")
    parser.add_argument("--variant", action="append", default=[], help="NAME=NUMERIC-LINE")
    parser.add_argument("--quad-upstream", action="store_true")
    parser.add_argument("--threads", type=int, default=1)
    parser.add_argument("--json", type=Path)
    args = parser.parse_args()
    args.exe = args.exe.resolve()
    args.work.mkdir(parents=True, exist_ok=True)
    deck = args.deck.resolve()

    name, numeric = args.reference.split("=", 1)
    reference = run(deck, name, numeric, args)
    results = [reference]
    print(f"{'run':<12} {'exit':>4} {'iter':>5} {'acc.%':>7} {'max|dHs| m':>11} {'mean|dHs| m':>12}")
    for item in [args.reference] + args.variant:
        name, numeric = item.split("=", 1)
        result = reference if name == reference["name"] else run(deck, name, numeric, args)
        if result is not reference:
            results.append(result)
        pairs = [(a, b) for a, b in zip(result["hs"], reference["hs"]) if a > 0 and b > 0]
        if len(result["hs"]) != len(reference["hs"]):
            raise ValueError(f"{name}: {len(result['hs'])} values against {len(reference['hs'])}")
        result["max_abs_dhs"] = max((abs(a - b) for a, b in pairs), default=0.0)
        result["mean_abs_dhs"] = sum(abs(a - b) for a, b in pairs) / len(pairs) if pairs else 0.0
        result["compared_points"] = len(pairs)
        accuracy = "-" if result["accuracy_ok_percent"] is None else f"{result['accuracy_ok_percent']:.2f}"
        print(f"{name:<12} {result['exit']:>4} {str(result['iterations']):>5} {accuracy:>7} "
              f"{result['max_abs_dhs']:>11.4f} {result['mean_abs_dhs']:>12.4f}")
    if args.json:
        args.json.write_text(json.dumps(results, indent=1))


if __name__ == "__main__":
    main()
