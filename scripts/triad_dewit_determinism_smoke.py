#!/usr/bin/env python3
"""Smoke test for OpenMP determinism on a regular grid (BF-70).

The deck examples/nonlinear_interactions/triad/triad_biph_dewit.swn (a 1D
grid of 300 cells with TRIAD DCTA BIPHASE DEWIT) is run once on one thread
and then several times on more threads than it gets CPUs, and the table and
spectrum files of every run must be equal to those of the serial run.

At the start of each iteration every thread clears its own part of the
biphase column of COMPDA, and the master thread then writes the filtered De
Wit biphase for the whole grid (SWBIPM). Without a barrier between the two, a
thread that is still clearing overwrites part of the filtered biphase with
zero. The race shows most often when threads have to wait for a CPU, so the
runs on several threads are pinned to at most two CPUs. Before the fix, eight
threads on two CPUs gave a table different from the serial one in about two
of three runs (11 November 2026), so ten runs practically always caught it.
One run takes well under a second.
"""
from __future__ import annotations
import argparse, hashlib, os, shutil, subprocess, sys, tempfile
from pathlib import Path

CASE = Path(__file__).resolve().parent.parent / "examples" / "nonlinear_interactions" / "triad"
DECK = "triad_biph_dewit.swn"
OUTPUT = ("triad_biph_dewit.tbl", "triad_biph_dewit.spc")


def digest(work: Path) -> str:
    """One SHA-256 over the output files of a run, in a fixed order."""
    h = hashlib.sha256()
    for name in OUTPUT:
        h.update((work / name).read_bytes())
    return h.hexdigest()


def run(exe: str, work: Path, threads: int, cpus: set[int] | None) -> str | None:
    """Run the deck in work; return the digest of its output, or None."""
    for name in OUTPUT:
        (work / name).unlink(missing_ok=True)
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(threads)
    env.pop("OMP_PLACES", None)
    env.pop("OMP_PROC_BIND", None)
    pin = (lambda: os.sched_setaffinity(0, cpus)) if cpus else None
    r = subprocess.run([exe], cwd=work, env=env, capture_output=True, text=True,
                       preexec_fn=pin)
    if r.returncode != 0 or not all((work / name).exists() for name in OUTPUT):
        print(f"run on {threads} threads failed (rc={r.returncode})", file=sys.stderr)
        return None
    return digest(work)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--exe", required=True, help="path to swan.exe (OpenMP build)")
    ap.add_argument("--threads", type=int, default=8)
    ap.add_argument("--runs", type=int, default=10,
                    help="runs on --threads threads, each compared with one serial run")
    ap.add_argument("--cpus", type=int, default=2,
                    help="number of CPUs the runs on several threads are pinned to")
    a = ap.parse_args()

    exe = str(Path(a.exe).resolve())
    allowed = sorted(os.sched_getaffinity(0))
    cpus = set(allowed[:a.cpus]) if a.cpus > 0 else None
    work = Path(tempfile.mkdtemp(prefix="triad_dewit_smoke_"))
    try:
        shutil.copy(CASE / DECK, work / "INPUT")
        shutil.copy(CASE / "bottom.bot", work / "bottom.bot")
        serial = run(exe, work, 1, None)
        if serial is None:
            return 2
        differ = 0
        for _ in range(a.runs):
            h = run(exe, work, a.threads, cpus)
            if h is None:
                return 2
            differ += h != serial
        where = f"{len(cpus)} CPUs" if cpus else "all CPUs"
        if differ:
            print(f"SMOKE FAIL: {differ} of {a.runs} runs on {a.threads} threads "
                  f"({where}) differ from the serial run")
            return 1
        print(f"SMOKE PASS: {a.runs} runs on {a.threads} threads ({where}) "
              f"bit-identical to the serial run {serial[:16]}...")
        return 0
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    raise SystemExit(main())
