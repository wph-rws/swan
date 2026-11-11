#!/usr/bin/env python3
"""Whole QCM runs behind benchmarks/README.md: `examples/qcm/qcm_scatter.swn`
with the fork against the copies with fftw_measure.patch and fftw_lock.patch.

Five runs of each, interleaved; one thread on CPU 0 (head and measure) and
eight threads on CPUs 0, 2, ..., 14 (head, measure and lock), with
`OMP_PLACES=cores OMP_PROC_BIND=close`. Run on a quiet machine. It also
checks that `qcm_scatter.mat` is the same in every run.

    python3 doc/measurements/qcm_timing.py --head EXE --measure EXE --lock EXE \\
        --work /tmp/qcm [--runs 5]

Writes WORK/qcm_timing.json and prints the median per series.
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
QCM = REPOSITORY / "examples/qcm"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    for name in ("head", "measure", "lock"):
        parser.add_argument(f"--{name}", type=Path, required=True, help=f"SWAN executable ({name})")
    parser.add_argument("--work", type=Path, required=True, help="work directory")
    parser.add_argument("--runs", type=int, default=5)
    parser.add_argument("--cpus1", default="0")
    parser.add_argument("--cpus8", default="0,2,4,6,8,10,12,14")
    args = parser.parse_args()
    exe = {"head": args.head.resolve(), "measure": args.measure.resolve(), "lock": args.lock.resolve()}
    result = {"load_before": os.getloadavg(), "t": {}, "h": {}, "load": []}
    series = ((1, args.cpus1, ("head", "measure")), (8, args.cpus8, ("head", "measure", "lock")))
    for threads, cpus, variants in series:
        for repeat in range(args.runs):
            for variant in variants:
                directory = args.work / "qcm_runs" / f"{variant}_t{threads}_r{repeat}"
                if directory.exists():
                    shutil.rmtree(directory)
                directory.mkdir(parents=True)
                for bottom in QCM.glob("*.bot"):
                    shutil.copy(bottom, directory / bottom.name)
                shutil.copy(QCM / "qcm_scatter.swn", directory / "INPUT")
                env = dict(os.environ, OMP_NUM_THREADS=str(threads), OMP_PLACES="cores",
                           OMP_PROC_BIND="close")
                started = time.perf_counter()
                done = subprocess.run(["taskset", "-c", cpus, str(exe[variant])], cwd=directory,
                                      env=env, capture_output=True)
                seconds = time.perf_counter() - started
                assert done.returncode == 0, directory
                key = f"{variant}_t{threads}"
                result["t"].setdefault(key, []).append(round(seconds, 3))
                result["h"].setdefault(key, []).append(
                    hashlib.sha256((directory / "qcm_scatter.mat").read_bytes()).hexdigest()[:12])
                result["load"].append(os.getloadavg()[0])
    result["load_after"] = os.getloadavg()
    (args.work / "qcm_timing.json").write_text(json.dumps(result, indent=1))
    for key, times in result["t"].items():
        print(f"{key}: median {statistics.median(times):.2f} s, runs {times}, "
              f"outputs {sorted(set(result['h'][key]))}")
    print("load average before/after", result["load_before"], result["load_after"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
