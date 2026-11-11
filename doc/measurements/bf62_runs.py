#!/usr/bin/env python3
"""Runs behind the BF-62 row of doc/bug-fixes-vs-tu-delft-41.51.md.

The decks are the test models `examples/testmodels/sq`, `chan` and `slope` and
the Voordelta example `examples/voordelta/voordelta.swn`, each in three forms:
as it is (`QUADRUPLET 2`, the default DIA), with `QUADRUPLET 1` (the fixed
semi-implicit DIA) and with `QUADRUPLET 1 UPSTREAM`, added after `GEN3 KOMEN`.
Every run gets the NUMERIC line

    NUMERIC STOPC NPNTS=101 STAT MXITST=<n> ALFA=<a> [LIMITER=<l>] RESIDUAL 0

so that it never stops on its own criterion and prints the balance residual.
Each run is written to WORK/runs/<name>/ with a summary.json (iterations,
residual per iteration, time). bf62_compare.py compares the saved Hs fields.

    python3 doc/measurements/bf62_runs.py --exe build-omp/bin/swan.exe \\
        --work /tmp/bf62 [--rhs-exe BUILD/bin/swan.exe] [--cpus 16,17,18,19] [NAME ...]

Without names it runs every run in the list (`--list` prints it); a run whose
summary.json exists is skipped. The runs `sq_ref_rhs_*` need `--rhs-exe`, a
build with bf62-rhs.patch. The Voordelta references take hours on one core;
the box runs take seconds to minutes. Each run uses one of the CPUs of
`--cpus` (taskset), and as many runs go at once as there are CPUs; the runs
`vd_det_fix_t<N>` use N threads.
"""
from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import threading
import time
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[2]
MODELS = {
    "sq": (REPOSITORY / "examples/testmodels/sq/INPUT", ["box.bot"]),
    "chan": (REPOSITORY / "examples/testmodels/chan/INPUT", ["box.bot"]),
    "slope": (REPOSITORY / "examples/testmodels/slope/INPUT", ["box.bot"]),
    "vd": (REPOSITORY / "examples/voordelta/voordelta.swn", ["voordelta.dep"]),
}
QUAD = {"q2": "", "fix": "QUADRUPLET 1\n", "up": "QUADRUPLET 1 UPSTREAM\n"}


def run_list() -> dict[str, dict]:
    """name -> model, variant, iterations, ALFA, limiter, threads, rhs build."""
    runs: dict[str, dict] = {}

    def add(name, model, variant, n, alfa, limiter=None, threads=1, rhs=False):
        runs[name] = dict(model=model, variant=variant, n=n, alfa=alfa, limiter=limiter,
                          threads=threads, rhs=rhs)

    for model, nref in (("sq", 2000), ("chan", 1000), ("slope", 1000)):
        add(f"{model}_ref_q2a_{nref}", model, "q2", nref, "0.01")
        add(f"{model}_ref_fix_{nref}", model, "fix", nref, "0")
        add(f"{model}_fix_40", model, "fix", 40, "0")
        for n in (40, 80, 160, 320):
            add(f"{model}_q2a_{n}", model, "q2", n, "0.01")
        for n in (40, 320):
            add(f"{model}_q2n_{n}", model, "q2", n, "0")
        add(f"{model}_fix_nolim_200", model, "fix", 200, "0", limiter="1000")
    add("sq_ref_up_2000", "sq", "up", 2000, "0")
    add("sq_ref_up_a01_2000", "sq", "up", 2000, "0.01")
    add("sq_ref_rhs_a01_2000", "sq", "up", 2000, "0.01", rhs=True)
    add("sq_ref_rhs_2000", "sq", "up", 2000, "0", rhs=True)
    add("vd_ref_fix_2000", "vd", "fix", 2000, "0")
    add("vd_fix_40", "vd", "fix", 40, "0")
    for n in (40, 80, 160, 320):
        add(f"vd_q2a_{n}", "vd", "q2", n, "0.01")
    add("vd_ref_q2a_2000", "vd", "q2", 2000, "0.01")
    for threads in (1, 2, 4, 8):
        add(f"vd_det_fix_t{threads}", "vd", "fix", 20, "0", threads=threads)
    return runs


def deck_text(spec: dict) -> str:
    """The deck of one run: the model with its QUADRUPLET line and NUMERIC line."""
    text = MODELS[spec["model"]][0].read_text()
    quad = QUAD[spec["variant"]]
    if quad:
        assert text.count("GEN3 KOMEN\n") == 1
        text = text.replace("GEN3 KOMEN\n", "GEN3 KOMEN\n" + quad)
    numeric = f"NUMERIC STOPC NPNTS=101 STAT MXITST={spec['n']} ALFA={spec['alfa']}"
    if spec["limiter"]:
        numeric += f" LIMITER={spec['limiter']}"
    numeric += " RESIDUAL 0"
    lines = text.splitlines()
    found = [i for i, line in enumerate(lines) if line.upper().startswith("NUM")]
    if found:
        assert len(found) == 1
        lines[found[0]] = numeric
    else:
        at = lines.index("COMPUTE")
        lines.insert(at, numeric)
    return "\n".join(lines) + "\n"


def run_one(name: str, spec: dict, exe: Path, work: Path, cpus: str) -> str:
    directory = work / "runs" / name
    if (directory / "summary.json").exists():
        return f"{name}: done before, skipped"
    if directory.exists():
        shutil.rmtree(directory)
    directory.mkdir(parents=True)
    source, files = MODELS[spec["model"]]
    for f in files:
        shutil.copy(source.parent / f, directory / f)
    (directory / "INPUT").write_text(deck_text(spec))
    env = dict(os.environ, OMP_NUM_THREADS=str(spec["threads"]), OMP_PLACES="cores",
               OMP_PROC_BIND="close")
    started = time.perf_counter()
    with open(directory / "screen.log", "w") as screen:
        result = subprocess.run(["taskset", "-c", cpus, str(exe)], cwd=directory, env=env,
                                stdout=screen, stderr=subprocess.STDOUT)
    seconds = time.perf_counter() - started
    printed = (directory / "PRINT").read_text(errors="replace")
    residual = [float(x) for x in re.findall(r"balance residual energy rms\s+([\d.Ee+-]+)", printed)]
    accuracy = [float(x) for x in re.findall(r"accuracy OK in\s+([\d.]+) %", printed)]
    summary = dict(name=name, exit=result.returncode, seconds=round(seconds, 2),
                   iterations=len(residual) or len(accuracy) + 1, residual=residual,
                   accuracy=accuracy, exe=str(exe), **spec)
    (directory / "summary.json").write_text(json.dumps(summary))
    last = residual[-1] if residual else "-"
    return f"{name}: exit {result.returncode}, {summary['iterations']} iterations, " \
           f"{seconds:.1f} s, last residual {last}"


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("names", nargs="*", help="runs to do (default: all)")
    parser.add_argument("--exe", type=Path, help="SWAN executable (an OpenMP build)")
    parser.add_argument("--rhs-exe", type=Path, help="build with bf62-rhs.patch, for sq_ref_rhs_*")
    parser.add_argument("--work", type=Path, help="work directory; runs go to WORK/runs/")
    parser.add_argument("--cpus", default="0", help="comma-separated CPUs, one lane each")
    parser.add_argument("--list", action="store_true", help="print the runs and their decks")
    args = parser.parse_args()
    runs = run_list()
    if args.list:
        for name, spec in runs.items():
            print(name, spec)
        return 0
    if args.exe is None or args.work is None:
        parser.error("--exe and --work are needed")
    names = args.names or list(runs)
    unknown = [n for n in names if n not in runs]
    if unknown:
        parser.error(f"unknown run(s): {', '.join(unknown)}")
    if args.rhs_exe is None:
        if args.names and any(runs[n]["rhs"] for n in names):
            parser.error("the sq_ref_rhs_* runs need --rhs-exe")
        names = [n for n in names if not runs[n]["rhs"]]
    lanes = args.cpus.split(",")
    queue = sorted(names, key=lambda n: -runs[n]["n"])
    lock = threading.Lock()

    def lane(cpu: str) -> None:
        while True:
            with lock:
                if not queue:
                    return
                name = queue.pop(0)
            spec = runs[name]
            #  a run with more than one thread gets all CPUs of --cpus
            cpus = args.cpus if spec["threads"] > 1 else cpu
            exe = args.rhs_exe if spec["rhs"] else args.exe
            print(run_one(name, spec, exe.resolve(), args.work.resolve(), cpus), flush=True)

    #  with a multi-thread run in the set, the runs go one at a time
    one_at_a_time = any(runs[n]["threads"] > 1 for n in names)
    threads = [threading.Thread(target=lane, args=(cpu,))
               for cpu in (lanes[:1] if one_at_a_time else lanes)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
