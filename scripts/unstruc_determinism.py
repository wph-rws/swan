#!/usr/bin/env python3
"""Harness for unstructured OpenMP determinism (BF-15, BF-17).

Runs the same deck N times per configuration and hashes the vertex-resolved
COMPGRID output (BLOCK COMPGRID + OUTPUT OPTIONS BLOCK 9 200, one-to-one
vertex numbers). When a run differs from the first run of its configuration,
the differing vertices are listed with their VMARKER and the largest absolute
and relative difference.

The configurations are all combinations of thread count, load and pinning.
Load "busy" starts one CPU-burning process per logical CPU next to SWAN;
pinning "spread" sets OMP_PLACES=cores and OMP_PROC_BIND=spread.

The number of runs is fixed by --runs. Use a small N as a pilot first. If
the pilot sees flips, "n_for_95pct" gives the number of runs that sees at
least one flip with 95% probability at the observed flip fraction; if it
sees none, "rule_of_three_95_upper_bound_p" (3/N) is the 95% upper bound
on the flip fraction. Run again with a larger --runs where needed.

Without --deck, --node and --ele the script builds its own case in the work
directory: the unstructured Voordelta mesh at --spacing metres (500 m gives
about 18,500 vertices) and the deck of the CI smoke test, capped at two
iterations and writing Hsig per vertex to the --hs file.

Usage (pilot):
  python3 scripts/unstruc_determinism.py --exe build-modernization-openmp/bin/swan.exe \\
      --runs 6 --out /tmp/swan-unstruc_det/b1_pilot.json
"""
from __future__ import annotations
import argparse, contextlib, hashlib, itertools, json, math, multiprocessing, os, shutil, subprocess, time
from pathlib import Path

from unstruc_determinism_smoke import make_mesh, vertex_deck

def sha256(p: Path) -> str:
    h = hashlib.sha256()
    with p.open("rb") as f:
        for c in iter(lambda: f.read(1 << 20), b""):
            h.update(c)
    return h.hexdigest()

def read_vals(p: Path):
    return [float(x) for x in p.read_text().split()]

def read_markers(node: Path):
    mk = {}
    for line in node.read_text().splitlines()[1:]:
        p = line.split()
        if len(p) == 4 and p[0].isdigit():
            mk[int(p[0])] = min(1, int(p[3]))
    return mk

def _burn(_):
    x = 0.0
    while True:
        x += 1.0
        if x > 1e18:
            x = 0.0

class Load:
    def __init__(self, nproc: int):
        self.nproc = nproc
        self.pool = None
    def __enter__(self):
        self.pool = multiprocessing.Pool(self.nproc)
        self.pool.map_async(_burn, range(self.nproc))
        time.sleep(1.0)  # let the load get going
        return self
    def __exit__(self, *a):
        if self.pool:
            self.pool.terminate()
            self.pool.join()
            self.pool = None

def one_run(exe: Path, deck: Path, node: Path, ele: Path, workbase: Path,
            threads: int, pin: str, tag: str, hs: str) -> tuple[str, float, Path]:
    d = workbase / tag
    if d.exists():
        shutil.rmtree(d)
    d.mkdir(parents=True)
    shutil.copy(node, d / "voordelta_unstructured.node")
    shutil.copy(ele, d / "voordelta_unstructured.ele")
    shutil.copy(deck, d / "INPUT")
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(threads)
    if pin == "spread":
        env["OMP_PLACES"] = "cores"
        env["OMP_PROC_BIND"] = "spread"
    else:
        env.pop("OMP_PLACES", None)
        env.pop("OMP_PROC_BIND", None)
    t0 = time.monotonic()
    r = subprocess.run([str(exe)], cwd=d, env=env, capture_output=True, text=True)
    dt = time.monotonic() - t0
    out = d / hs
    if r.returncode != 0 or not out.exists():
        prt = d / "voordelta_unstructured.prt"
        tail = prt.read_text()[-2000:] if prt.exists() else r.stderr[-2000:]
        missing = "" if out.exists() else f", no {hs}"
        raise RuntimeError(f"run {tag} failed (rc={r.returncode}{missing}): {tail}")
    return sha256(out), dt, out

def n_for_95(p: float) -> int | None:
    if p <= 0 or p >= 1:
        return None
    return math.ceil(math.log(0.05) / math.log(1 - p))

def record_diffs(ref: list[float], vals: list[float], markers: dict, diffs: dict) -> None:
    """Keep, per differing vertex, the largest absolute difference from the reference run."""
    for v, (a, b) in enumerate(zip(ref, vals, strict=True), start=1):
        if a != b:
            d = abs(a - b)
            if v not in diffs or d > diffs[v][1]:
                diffs[v] = [markers.get(v), d, d / max(abs(a), abs(b))]

def run_cell(args, load: str, pin: str, threads: int, markers: dict) -> dict:
    """Run one configuration args.runs times and compare every run with the first."""
    # SWAN runs inside its work directory, so a relative --exe must be resolved first
    exe = Path(args.exe).resolve()
    deck, node, ele = Path(args.deck), Path(args.node), Path(args.ele)
    tag = f"t{threads}_{load}_{pin}"
    hashes, dts = [], []
    ref_vals = None
    diffs: dict[int, list] = {}
    ctx = Load(os.cpu_count() or 1) if load == "busy" else contextlib.nullcontext()
    with ctx:
        for i in range(args.runs):
            h, dt, out = one_run(exe, deck, node, ele, Path(args.workbase), threads, pin,
                                 f"{tag}_{i}", args.hs)
            hashes.append(h)
            dts.append(dt)
            if ref_vals is None:
                ref_vals = read_vals(out)
            elif h != hashes[0]:
                record_diffs(ref_vals, read_vals(out), markers, diffs)
    # flips counts the runs whose output differs from the first run
    flips = sum(1 for h in hashes[1:] if h != hashes[0])
    p = flips / (args.runs - 1) if args.runs > 1 else 0.0
    worst = sorted(diffs.items(), key=lambda kv: -kv[1][1])[:50]
    return {
        "runs": args.runs, "hashes": hashes,
        "unique": len(set(hashes)), "flips_vs_first": flips,
        "flip_fraction": round(p, 4),
        "n_for_95pct": n_for_95(p) if flips else None,
        "rule_of_three_95_upper_bound_p": round(3 / args.runs, 4) if not flips else None,
        "time_mean_s": round(sum(dts) / len(dts), 2),
        "time_min_s": round(min(dts), 2), "time_max_s": round(max(dts), 2),
        "n_vertices_differing": len(diffs),
        "vertices": [{"v": v, "vmarker": m, "abs": a, "rel": r} for v, (m, a, r) in worst],
    }

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--exe", required=True)
    ap.add_argument("--deck", help="deck to run (default: build the case, see above)")
    ap.add_argument("--node")
    ap.add_argument("--ele")
    ap.add_argument("--spacing", type=float, default=500.0,
                    help="mesh spacing in metres for the built case")
    ap.add_argument("--threads", nargs="+", type=int,
                    default=sorted({1, 2, 4, os.cpu_count() or 1}))
    ap.add_argument("--runs", type=int, default=6)
    ap.add_argument("--loads", nargs="+", choices=["idle", "busy"], default=["idle", "busy"])
    ap.add_argument("--pins", nargs="+", choices=["spread", "none"], default=["spread", "none"])
    ap.add_argument("--hs", default="det_hs.blk", help="block file written by the deck")
    ap.add_argument("--out", required=True)
    ap.add_argument("--workbase", default="/tmp/swan-unstruc_det/b1work")
    args = ap.parse_args()
    if args.runs < 1:
        ap.error("--runs must be at least 1")
    given = [x is not None for x in (args.deck, args.node, args.ele)]
    if any(given) and not all(given):
        ap.error("give --deck, --node and --ele together, or none of them")
    if not any(given):
        case = Path(args.workbase) / "case"
        case.mkdir(parents=True, exist_ok=True)
        nverts = make_mesh(case, args.spacing)
        (case / "det.swn").write_text(vertex_deck(args.hs))
        args.deck = str(case / "det.swn")
        args.node = str(case / "voordelta_unstructured.node")
        args.ele = str(case / "voordelta_unstructured.ele")
        print(f"case: {nverts} vertices at {args.spacing:g} m in {case}", flush=True)

    markers = read_markers(Path(args.node))
    t0all = time.monotonic()
    out: dict = {"exe": args.exe, "deck": args.deck, "cells": {}}
    for load, pin, threads in itertools.product(args.loads, args.pins, args.threads):
        cell = f"t{threads}/{load}/{pin}"
        res = run_cell(args, load, pin, threads, markers)
        out["cells"][cell] = res
        print(f"{cell}: {res['unique']}/{args.runs} unique, flips={res['flips_vs_first']}, "
              f"t={res['time_mean_s']}s, nvert={res['n_vertices_differing']}", flush=True)
    out["total_s"] = round(time.monotonic() - t0all, 1)
    Path(args.out).write_text(json.dumps(out, indent=1))
    print(f"written: {args.out} ({out['total_s']}s)")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
