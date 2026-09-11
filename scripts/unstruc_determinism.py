#!/usr/bin/env python3
"""Harness voor ongestructureerd OpenMP-determinisme (doc/moderniseringsplan.md).

N runs per configuratie, hash van de vertex-resolute COMPGRID-uitvoer
(BLOCK COMPGRID + OUTPUT OPTIONS BLOCK 9 200, 1-op-1 vertexnummer),
bij verschil een lijst van vertexnummers met VMARKER, absoluut en
relatief verschil.

Matrix: threads x belasting x pinning. Adaptief: eerst pilot-N, daarna
opschalen tot 95% detectiekans op basis van de waargenomen flipfractie.

Gebruik (pilot):
  python3 scripts/unstruc_determinism.py --exe build-modernization-openmp/bin/swan.exe \\
      --deck /tmp/swan-unstruc_det/det500.swn \\
      --node /tmp/swan-unstruc_det/mesh500.node \\
      --ele  /tmp/swan-unstruc_det/mesh500.ele \\
      --threads 1 4 22 --runs 6 --out /tmp/swan-unstruc_det/b1_pilot.json
"""
from __future__ import annotations
import argparse, hashlib, json, math, multiprocessing, os, shutil, subprocess, sys, time
from pathlib import Path

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
        return self
    def __exit__(self, *a):
        if self.pool:
            self.pool.terminate()
            self.pool.join()
            self.pool = None

def one_run(exe: Path, deck: Path, node: Path, ele: Path, workbase: Path,
            threads: int, pin: str, tag: str) -> tuple[str, float, Path]:
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
    if r.returncode != 0 or not (d / "det_hs.blk").exists():
        raise RuntimeError(f"run {tag} faalde (rc={r.returncode}): {(d/'voordelta_unstructured.prt').exists() and (d/'voordelta_unstructured.prt').read_text()[-2000:] or r.stderr[-2000:]}")
    return sha256(d / "det_hs.blk"), dt, d

def n_for_95(p: float) -> int | None:
    if p <= 0 or p >= 1:
        return None
    return math.ceil(math.log(0.05) / math.log(1 - p))

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--exe", required=True)
    ap.add_argument("--deck", required=True)
    ap.add_argument("--node", required=True)
    ap.add_argument("--ele", required=True)
    ap.add_argument("--threads", nargs="+", type=int, default=[1, 4, 22])
    ap.add_argument("--runs", type=int, default=6)
    ap.add_argument("--loads", nargs="+", default=["leeg", "vol"])
    ap.add_argument("--pins", nargs="+", default=["spread", "geen"])
    ap.add_argument("--hs", default="det_hs.blk")
    ap.add_argument("--out", required=True)
    ap.add_argument("--workbase", default="/tmp/swan-unstruc_det/b1work")
    args = ap.parse_args()

    exe, deck, node, ele = Path(args.exe), Path(args.deck), Path(args.node), Path(args.ele)
    workbase = Path(args.workbase)
    markers = read_markers(node)
    t0all = time.monotonic()
    out: dict = {"exe": str(exe), "deck": str(deck), "cells": {}}
    ncpu = os.cpu_count() or 1
    for load in args.loads:
        for pin in args.pins:
            for th in args.threads:
                cell = f"t{th}/{load}/{pin}"
                hashes, dts = [], []
                ref_vals = None
                diff_union: dict[int, list] = {}
                if load == "vol":
                    ctx = Load(ncpu)
                else:
                    import contextlib
                    ctx = contextlib.nullcontext()
                with ctx:
                    time.sleep(1.0)  # laat belasting op gang komen
                    for i in range(args.runs):
                        h, dt, d = one_run(exe, deck, node, ele, workbase, th, pin,
                                           f"{cell.replace('/','_')}_{i}")
                        hashes.append(h)
                        dts.append(dt)
                        vals = read_vals(d / args.hs)
                        if ref_vals is None:
                            ref_vals = vals
                        elif vals != ref_vals:
                            for j, (a, b) in enumerate(zip(ref_vals, vals)):
                                if a != b:
                                    v = j + 1
                                    av = abs(a - b)
                                    denom = max(abs(a), abs(b))
                                    rel = av / denom if denom else 0.0
                                    if v not in diff_union:
                                        diff_union[v] = [markers.get(v), av, rel]
                                    else:
                                        if av > diff_union[v][1]:
                                            diff_union[v] = [markers.get(v), av, rel]
                uniq = len(set(hashes))
                flips = sum(1 for h in hashes[1:] if h != hashes[0])
                p = flips / max(1, args.runs - 1) if args.runs > 1 else 0.0
                # flipfractie over alle paren t.o.v. referentie
                cellres = {
                    "runs": args.runs, "hashes": hashes,
                    "unique": uniq, "flips_vs_first": flips,
                    "flipfractie": round(p, 4),
                    "n_voor_95pct": n_for_95(p) if flips else None,
                    "regel_van_drie_95bovengrens_p": round(3 / args.runs, 4) if not flips else None,
                    "tijd_gem_s": round(sum(dts) / len(dts), 2),
                    "tijd_min_s": round(min(dts), 2), "tijd_max_s": round(max(dts), 2),
                    "n_vertices_verschillend": len(diff_union),
                    "vertices": [{"v": v, "vmarker": m, "abs": a, "rel": r}
                                 for v, (m, a, r) in sorted(diff_union.items(),
                                                             key=lambda kv: -kv[1][1])[:50]],
                }
                out["cells"][cell] = cellres
                print(f"{cell}: {uniq}/{args.runs} uniek, flips={flips}, "
                      f"t={cellres['tijd_gem_s']}s, nvert={len(diff_union)}", flush=True)
    out["totaal_s"] = round(time.monotonic() - t0all, 1)
    Path(args.out).write_text(json.dumps(out, indent=1))
    print(f"geschreven: {args.out} ({out['totaal_s']}s)")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
