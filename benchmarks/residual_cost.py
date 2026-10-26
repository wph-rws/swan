#!/usr/bin/env python3
"""Measure what the balance residual costs on the Voordelta example.

Runs the same deck with `NUMERIC STOPC` and with `NUMERIC STOPC RESIDUAL 0`,
alternating the two, seven rounds each, on one core and on four OpenMP
threads. Reports the median wall-clock times, the median of the per-round
ratio, and whether the outputs are byte-identical (the residual is only
measured; it must not change the solution).

Usage:
    python3 benchmarks/residual_cost.py --swan-executable build-openmp/bin/swan.exe \
        [--json residual-cost.json]
"""
from __future__ import annotations

import argparse
import filecmp
import json
import os
import shutil
import statistics
import subprocess
import tempfile
import time
from pathlib import Path

CASE = Path(__file__).resolve().parent.parent / "examples" / "voordelta"
CONFIGURATIONS = (("serial", 1, "0"), ("four threads", 4, "0,2,4,6"))


def prepare(base: Path, name: str, numeric: str) -> Path:
    work = base / name
    work.mkdir()
    lines = (CASE / "voordelta.swn").read_text().splitlines()
    at = next(i for i, line in enumerate(lines) if line.upper().startswith("COMPUTE"))
    lines.insert(at, numeric)
    (work / "INPUT").write_text("\n".join(lines) + "\n")
    shutil.copy(CASE / "voordelta.dep", work / "voordelta.dep")
    return work


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--swan-executable", required=True, type=Path)
    parser.add_argument("--rounds", type=int, default=7)
    parser.add_argument("--json", type=Path)
    arguments = parser.parse_args()
    exe = arguments.swan_executable.resolve()
    results = {}
    for label, threads, cpus in CONFIGURATIONS:
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory)
            off = prepare(base, "off", "NUMERIC STOPC")
            on = prepare(base, "on", "NUMERIC STOPC RESIDUAL 0")
            env = dict(os.environ, OMP_NUM_THREADS=str(threads),
                       OMP_PROC_BIND="close", OMP_PLACES="cores")
            times = {"off": [], "on": []}
            for round_ in range(arguments.rounds):
                order = (("off", off), ("on", on)) if round_ % 2 == 0 else (("on", on), ("off", off))
                for name, work in order:
                    start = time.perf_counter()
                    subprocess.run(["taskset", "-c", cpus, str(exe)], cwd=work, env=env,
                                   check=True, capture_output=True)
                    times[name].append(time.perf_counter() - start)
            identical = all(filecmp.cmp(off / f, on / f, shallow=False)
                            for f in ("voordelta_hs.blk", "voordelta_sites.tbl"))
        ratio = statistics.median(b / a for a, b in zip(times["off"], times["on"]))
        results[label] = {"off_median_s": statistics.median(times["off"]),
                          "on_median_s": statistics.median(times["on"]),
                          "cost_percent": 100 * (ratio - 1), "output_identical": identical,
                          "cpus": cpus, "runs": times}
        print(f"{label}: off {results[label]['off_median_s']:.2f} s, on "
              f"{results[label]['on_median_s']:.2f} s, cost {results[label]['cost_percent']:.1f}%, "
              f"output identical: {identical}")
    if arguments.json:
        arguments.json.write_text(json.dumps(results, indent=1) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
