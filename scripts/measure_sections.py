#!/usr/bin/env python3
"""Time SWAN per physics section on a reduced Voordelta case.

A change that touches a hot routine has to measure its own effect: reasoning
from a rule is not enough. An extra dummy argument on SWSNL2 once cost 37% of
the quadruplet section, and no general principle predicted it. This script is
that measurement.

The case is the stationary Voordelta example capped at two iterations, which
runs in about 1.7 s with a spread of a few percent instead of 22 s with 59%.
Timings come from SWAN's own TIMG capability, so the build has to be
configured with -DTIMG=ON.

AGENTS.md asks for every speed claim on one serial and one multicore
configuration. Run the script once with --threads 1 and once with several
threads, on a quiet machine, and compare each against a baseline with the same
thread count; --compare warns when the two runs were set up differently. Set
the pinning (OMP_PLACES, OMP_PROC_BIND) in the environment; it is recorded in
the JSON with the thread count and the CPU affinity.

Usage:
    scripts/measure_sections.py --threads 1 --repeat 5 --out baseline-t1.json
    scripts/measure_sections.py --threads 1 --repeat 5 --compare baseline-t1.json
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
EXAMPLE = ROOT / "examples" / "voordelta"
EXAMPLE_UNSTRUCTURED = ROOT / "examples" / "voordelta_unstructured"
DEFAULT_BUILD = ROOT / "build-modernization-timg"

# `# <label>: <cpu> <wall>` in the timing table SWPRTI writes.
ROW = re.compile(r"^\s*\d+\s+#\s+([a-zA-Z][^:]*?):\s+(\S+)\s+(\S+)\s*$")


def _replace_once(text: str, old: str, new: str, deck: Path) -> str:
    """Replace old by new once; stop if the deck no longer contains old.

    A silent miss would time the uncapped run (or one without its depth file)
    and report it as the reduced case.
    """
    if old not in text:
        raise SystemExit(f"{deck}: expected {old!r}; update this script")
    return text.replace(old, new, 1)


def _cap_iterations(text: str, iterations: int, deck: Path) -> str:
    return _replace_once(text, "GEN3 KOMEN",
                         f"GEN3 KOMEN\nNUMERIC STOPC STAT {iterations}", deck)


def reduced_deck(iterations: int) -> str:
    """The example deck with the iteration count capped."""
    deck = EXAMPLE / "voordelta.swn"
    return _cap_iterations(deck.read_text(), iterations, deck)


def reduced_deck_unstructured(iterations: int) -> str:
    """The unstructured Voordelta deck with local file names and capped iterations."""
    deck = EXAMPLE_UNSTRUCTURED / "voordelta_unstructured.swn"
    text = _replace_once(deck.read_text(), "'../voordelta/voordelta.dep'",
                         "'voordelta.dep'", deck)
    return _cap_iterations(text, iterations, deck)


def run_once(workdir: Path, exe: Path, env: dict[str, str]) -> dict[str, float]:
    (workdir / "PRINT").unlink(missing_ok=True)
    subprocess.run([str(exe)], cwd=workdir, env=env, capture_output=True, check=True)
    timings: dict[str, float] = {}
    for line in (workdir / "PRINT").read_text(errors="replace").splitlines():
        match = ROW.match(line)
        if not match:
            continue
        label, _cpu, wall = match.groups()
        try:
            timings[label.strip()] = float(wall)
        except ValueError:
            continue          # an overflowing F11.2 field prints asterisks
    if "total time" not in timings:
        raise SystemExit("no timing table in PRINT -- is the build -DTIMG=ON?")
    return timings


def measure(exe: Path, repeat: int, iterations: int, env: dict[str, str],
            case: str = "structured") -> dict[str, list[float]]:
    with tempfile.TemporaryDirectory(prefix="swan-measure-") as tmp:
        work = Path(tmp)
        if case == "unstructured":
            (work / "INPUT").write_text(
                reduced_deck_unstructured(iterations))
            shutil.copy(EXAMPLE / "voordelta.dep", work)
            for suffix in (".node", ".ele"):
                shutil.copy(
                    EXAMPLE_UNSTRUCTURED / f"voordelta_unstructured{suffix}",
                    work)
        else:
            (work / "INPUT").write_text(reduced_deck(iterations))
            shutil.copy(EXAMPLE / "voordelta.dep", work)
        samples: dict[str, list[float]] = {}
        for _ in range(repeat):
            for label, value in run_once(work, exe, env).items():
                samples.setdefault(label, []).append(value)
    return samples


def summarise(samples: dict[str, list[float]]) -> dict[str, dict[str, float]]:
    out = {}
    for label, values in samples.items():
        out[label] = {
            "median": statistics.median(values),
            "min": min(values),
            "max": max(values),
            "n": len(values),
        }
    return out


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--build-dir", type=Path, default=DEFAULT_BUILD)
    p.add_argument("--threads", type=int,
                   help="OpenMP threads for SWAN (default: OMP_NUM_THREADS "
                   "from the environment)")
    p.add_argument("--repeat", type=int, default=5)
    p.add_argument("--iterations", type=int, default=2,
                   help="cap on the number of SWAN iterations")
    p.add_argument("--case", choices=("structured", "unstructured"),
                   default="structured",
                   help="which Voordelta case to time")
    p.add_argument("--out", type=Path, help="write the summary as JSON")
    p.add_argument("--compare", type=Path, help="report against an earlier run")
    a = p.parse_args()

    #  Absolute, because SWAN runs in a temporary directory.
    exe = (a.build_dir / "bin" / "swan.exe").resolve()
    if not exe.exists():
        raise SystemExit(f"{exe} does not exist; build with -DTIMG=ON")

    if a.case == "unstructured":
        deck_text = reduced_deck_unstructured(a.iterations)
    else:
        deck_text = reduced_deck(a.iterations)
    env = os.environ.copy()
    if a.threads is not None:
        env["OMP_NUM_THREADS"] = str(a.threads)
    try:
        affinity = sorted(os.sched_getaffinity(0))
    except AttributeError:
        affinity = []
    meta = {
        "case": a.case,
        "exe_sha256": hashlib.sha256(exe.read_bytes()).hexdigest(),
        "deck_sha256": hashlib.sha256(deck_text.encode()).hexdigest(),
        "affinity": affinity,
        "threads": env.get("OMP_NUM_THREADS", ""),
        "omp_places": env.get("OMP_PLACES", ""),
        "omp_proc_bind": env.get("OMP_PROC_BIND", ""),
        "repeat": a.repeat,
        "iterations": a.iterations,
    }

    samples = measure(exe, a.repeat, a.iterations, env, a.case)
    now = summarise(samples)
    if a.out:
        a.out.write_text(json.dumps({"_meta": meta, **now}, indent=2))

    if a.compare:
        was = json.loads(a.compare.read_text())
        #  A baseline at another thread count or on another case says nothing
        #  about this change.
        for key in ("case", "iterations", "threads", "omp_places",
                    "omp_proc_bind", "deck_sha256"):
            before = was.get("_meta", {}).get(key)
            if before is not None and before != meta[key]:
                print(f"warning: {key} was {before!r}, is {meta[key]!r}",
                      file=sys.stderr)
        print(f"{'section':24s} {'was':>8s} {'is':>8s} {'difference':>10s}  spread")
        for label in sorted(now, key=lambda k: -now[k]["median"]):
            if label.startswith("_") or label not in was:
                continue
            b, n = was[label]["median"], now[label]["median"]
            if b < 0.005 and n < 0.005:
                continue          # below the timer's resolution either way
            delta = (n - b) / b * 100 if b else float("nan")
            spread = (now[label]["max"] - now[label]["min"]) / n * 100 if n else 0
            flag = "  <-- " if abs(delta) > 5 and n > 0.05 else ""
            print(f"{label:24s} {b:8.3f} {n:8.3f} {delta:+9.1f}%  {spread:5.1f}%{flag}")
    else:
        for label in sorted(now, key=lambda k: -now[k]["median"]):
            if label.startswith("_"):
                continue
            s = now[label]
            if s["median"] < 0.005:
                continue
            spread = (s["max"] - s["min"]) / s["median"] * 100
            print(f"{label:24s} {s['median']:8.3f}  spread {spread:5.1f}%")
    return 0


if __name__ == "__main__":
    sys.exit(main())
