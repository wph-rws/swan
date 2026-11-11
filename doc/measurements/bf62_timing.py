#!/usr/bin/env python3
"""Timings behind the BF-62 row: the cost of one iteration of each DIA form.

Run it on a quiet machine (nothing else running). It times the Voordelta
example (`examples/voordelta`) for 20 iterations (`NPNTS=101`, so that STOPC
never stops early, `ALFA=0`, no balance residual) in three forms:
`QUADRUPLET 2` (the default DIA), `QUADRUPLET 1` (the fixed DIA, BF-62) and
`QUADRUPLET 1 UPSTREAM`. The runs are interleaved (q2, fix, up, q2, ...), on
one thread (logical CPU 2 by default) and on eight threads (CPUs 0, 2, ..., 14,
one per physical core, `OMP_PLACES=cores OMP_PROC_BIND=close`). It also checks
that the runs of each form give a byte-identical `voordelta_hs.blk`.

    python3 doc/measurements/bf62_timing.py --exe build-omp/bin/swan.exe \\
        --work /tmp/bf62-timing [--runs 3] [--cpus1 2] [--cpus8 0,2,4,6,8,10,12,14]

Writes WORK/timing_result.json and prints the mean per form and the ratios.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import statistics
import subprocess
import time
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[2]
VOORDELTA = REPOSITORY / "examples/voordelta"
NUMERIC = "NUMERIC STOPC NPNTS=101 STAT MXITST=20 ALFA=0\n"
VARIANTS = {"q2": "", "fix": "QUADRUPLET 1\n", "up": "QUADRUPLET 1 UPSTREAM\n"}


def stage(directory: Path, quad: str) -> None:
    if directory.exists():
        shutil.rmtree(directory)
    directory.mkdir(parents=True)
    shutil.copy(VOORDELTA / "voordelta.dep", directory / "voordelta.dep")
    text = (VOORDELTA / "voordelta.swn").read_text()
    text = text.replace("GEN3 KOMEN\n", "GEN3 KOMEN\n" + quad, 1)
    text = text.replace("\nCOMPUTE\n", "\n" + NUMERIC + "COMPUTE\n", 1)
    assert NUMERIC in text and (quad == "" or quad in text)
    (directory / "INPUT").write_text(text)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--exe", type=Path, required=True, help="SWAN executable (OpenMP build)")
    parser.add_argument("--work", type=Path, required=True, help="work directory")
    parser.add_argument("--runs", type=int, default=3, help="runs per form and thread count")
    parser.add_argument("--cpus1", default="2", help="CPU for the one-thread runs")
    parser.add_argument("--cpus8", default="0,2,4,6,8,10,12,14", help="CPUs for the eight-thread runs")
    args = parser.parse_args()
    exe = args.exe.resolve()
    config = {1: args.cpus1, 8: args.cpus8}
    result = {"exe": str(exe), "load_before": os.getloadavg(), "times": {}, "hashes": {}}
    for threads, cpus in config.items():
        for repeat in range(args.runs):
            for variant, quad in VARIANTS.items():
                directory = args.work / "timing_runs" / f"{variant}_t{threads}_r{repeat}"
                stage(directory, quad)
                env = dict(os.environ, OMP_NUM_THREADS=str(threads), OMP_PLACES="cores",
                           OMP_PROC_BIND="close")
                started = time.perf_counter()
                done = subprocess.run(["taskset", "-c", cpus, str(exe)], cwd=directory, env=env,
                                      stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)
                seconds = time.perf_counter() - started
                assert done.returncode == 0, directory
                key = f"{variant}_t{threads}"
                result["times"].setdefault(key, []).append(round(seconds, 3))
                result["hashes"].setdefault(key, []).append(
                    hashlib.sha256((directory / "voordelta_hs.blk").read_bytes()).hexdigest())
    result["load_after"] = os.getloadavg()
    (args.work / "timing_result.json").write_text(json.dumps(result, indent=1))
    for key, times in result["times"].items():
        same = len(set(result["hashes"][key])) == 1
        print(f"{key}: mean {statistics.mean(times):.2f} s  runs {times}  identical output: {same}")
    for threads in config:
        q2 = statistics.mean(result["times"][f"q2_t{threads}"])
        fix = statistics.mean(result["times"][f"fix_t{threads}"])
        up = statistics.mean(result["times"][f"up_t{threads}"])
        print(f"{threads} thread(s): fix / QUADRUPLET 2 = {fix / q2:.2f}; "
              f"fix against UPSTREAM {100 * (fix / up - 1):+.1f}%")
    print("load average before/after:", result["load_before"], result["load_after"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
