#!/usr/bin/env python3
"""so-rp runs behind the LTA comparison with RWS 41.31A.1 (AGENTS.md, under
"Approaching 41.31 in 41.51", and doc/closing-account.md).

All runs use the corrected so-rp template at condition u20_d310_lp300_open,
with the deck that `so-rp_swan/matrix/deck.py` makes for the target
`current_4151_legacy` (`GEN3 KOMEN DRAG FIT`, `TRIAD ITRIAD=1 ... DINT=-1
URCRIT=0.2 URSLIM=0.01 IC=2`). Only the TRIAD line changes per step:

    L0_base       without DINT and IC
    L1_dint       with DINT=-1
    L2_legacy     with DINT=-1 and IC=2 (the legacy-4131 deck itself)
    L3_testbuild  the legacy-4131 deck with the test build of lta4131.patch
    and L0, L1, L2 again with `UPSTREAM` at the end of the TRIAD line.

    python3 doc/measurements/sorp_lta_chain.py --fork build-omp/bin/swan.exe \\
        --test-build build-lta4131/bin/swan.exe --work /tmp/lta-chain \\
        [--cpu-groups 0-3,4-7,8-11,12-15,16-19] [NAME ...]

Each run uses four OpenMP threads, pinned to one group of `--cpu-groups`, and
as many runs go at once as there are groups. A run takes several minutes; a
run whose done.json exists is skipped. The so-rp data must be present under
`so-rp_swan/` (they are not in git). Compare the runs with
sorp_compare_points.py.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPOSITORY / "so-rp_swan/matrix"))
import deck  # noqa: E402
import matrix_runner  # noqa: E402

CONDITION = "u20_d310_lp300_open"
TARGET = "current_4151_legacy"
LINE = "TRIAD\tITRIAD=1 TRFAC=0.8 CUTFR=2.5 DINT=-1 URCRIT=0.2 URSLIM=0.01 IC=2"
STEPS = {
    "L0_base": "TRIAD\tITRIAD=1 TRFAC=0.8 CUTFR=2.5 URCRIT=0.2 URSLIM=0.01",
    "L1_dint": "TRIAD\tITRIAD=1 TRFAC=0.8 CUTFR=2.5 DINT=-1 URCRIT=0.2 URSLIM=0.01",
    "L2_legacy": LINE,
}


def jobs() -> dict[str, tuple[str, str]]:
    """name -> (which executable, TRIAD line)."""
    result = {"L3_testbuild": ("test", LINE)}
    for name, line in STEPS.items():
        result[name] = ("fork", line)
        result[name + "_upstream"] = ("fork", line + " UPSTREAM")
    return result


def condition(manifest):
    for entry in manifest.conditions:
        c = entry.condition
        for attribute in ("id", "name", "condition_id", "identifier"):
            if getattr(c, attribute, None) == CONDITION:
                return c
    raise SystemExit(f"condition {CONDITION} not in the matrix manifest")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("names", nargs="*", help="runs to do (default: all)")
    parser.add_argument("--fork", type=Path, required=True, help="fork executable (OpenMP)")
    parser.add_argument("--test-build", type=Path, help="build with lta4131.patch (for L3_testbuild)")
    parser.add_argument("--work", type=Path, required=True, help="work directory")
    parser.add_argument("--cpu-groups", default="0-3", help="comma-separated groups of four CPUs")
    args = parser.parse_args()
    manifest = matrix_runner.load_manifest()
    target = {t.target_id: t for t in manifest.targets}[TARGET]
    cond = condition(manifest)
    todo = jobs()
    names = args.names or [n for n in todo if n != "L3_testbuild" or args.test_build]
    free = args.cpu_groups.split(",")
    lock = threading.Lock()

    def take() -> str:
        while True:
            with lock:
                if free:
                    return free.pop(0)
            time.sleep(5)

    def run(name: str) -> None:
        which, line = todo[name]
        exe = (args.test_build if which == "test" else args.fork).resolve()
        directory = args.work / name
        if (directory / "done.json").exists():
            return
        if directory.exists():
            shutil.rmtree(directory)
        directory.mkdir(parents=True)
        deck.stage_inputs(directory, target.swan_family)
        text = deck.generate_deck(cond, physics=target.physics, template=target.template)
        assert text.count(LINE) == 1, "legacy TRIAD line not found"
        (directory / "INPUT").write_text(text.replace(LINE, line))
        env = dict(os.environ, OMP_NUM_THREADS="4", OMP_PROC_BIND="close", OMP_PLACES="threads",
                   OMP_WAIT_POLICY="PASSIVE")
        cpus = take()
        started = time.time()
        with open(directory / "screen.log", "wb") as screen:
            rc = subprocess.run(["taskset", "-c", cpus, str(exe)], cwd=directory, env=env,
                                stdout=screen, stderr=subprocess.STDOUT).returncode
        with lock:
            free.append(cpus)
        (directory / "done.json").write_text(json.dumps(
            {"rc": rc, "seconds": round(time.time() - started), "cores": cpus, "exe": str(exe)}))
        print(name, "exit", rc, flush=True)

    args.work.mkdir(parents=True, exist_ok=True)
    with ThreadPoolExecutor(len(free)) as pool:
        list(pool.map(run, names))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
