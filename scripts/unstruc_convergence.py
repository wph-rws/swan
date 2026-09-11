#!/usr/bin/env python3
"""Convergentietest: spreiding als functie van het iteratienummer.

Voor MXITST in {2,5,10,20,50}: 1x serieel (referentie) + 6x 22-thread.
Maat: max|dHsig| over natte vertices t.o.v. de seriele referentie, plus
spreiding tussen de 22-thread-runs onderling. Het verschil is 'alleen pad' als de
spreiding monotoon daalt tot onder het stopcriterium (DREL=1%).
"""
from __future__ import annotations
import argparse, os, re, shutil, subprocess, sys, time
from pathlib import Path

def run_one(exe, decktxt, node, ele, d, threads):
    if d.exists():
        shutil.rmtree(d)
    d.mkdir(parents=True)
    shutil.copy(node, d / "voordelta_unstructured.node")
    shutil.copy(ele, d / "voordelta_unstructured.ele")
    (d / "INPUT").write_text(decktxt)
    env = os.environ.copy()
    env["OMP_NUM_THREADS"] = str(threads)
    env["OMP_PLACES"] = "cores"
    env["OMP_PROC_BIND"] = "spread"
    r = subprocess.run([str(exe)], cwd=d, env=env, capture_output=True, text=True)
    if r.returncode != 0 or not (d / "det_hs.blk").exists():
        raise RuntimeError(f"run {d} faalde rc={r.returncode}")
    vals = [float(x) for x in (d / "det_hs.blk").read_text().split()]
    prt = (d / "PRINT").read_text(errors="replace") if (d / "PRINT").exists() else ""
    iters = len(re.findall(r"(?m)^\s*iteration\s+\d+", prt))
    acc = re.findall(r"accuracy OK in\s+([0-9.]+)", prt)
    return vals, iters, (float(acc[-1]) if acc else None)

def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--exe", required=True)
    ap.add_argument("--deck", required=True)
    ap.add_argument("--node", required=True)
    ap.add_argument("--ele", required=True)
    ap.add_argument("--mxitsts", nargs="+", type=int, default=[2, 5, 10, 20, 50])
    ap.add_argument("--repeats", type=int, default=6)
    ap.add_argument("--workbase", default="/tmp/swan-unstruc-convergentie")
    ap.add_argument("--out", default="/tmp/swan-unstruc-convergentie.json")
    a = ap.parse_args()
    exe = Path(a.exe)
    base = Path(a.deck).read_text()
    wb = Path(a.workbase)
    import json
    res = {}
    for mx in a.mxitsts:
        decktxt = re.sub(r"NUMERIC STOPC STAT \d+", f"NUMERIC STOPC STAT {mx}", base)
        ref, refit, refacc = run_one(exe, decktxt, a.node, a.ele, wb / f"mx{mx}_ref", 1)
        wet = [i for i, v in enumerate(ref) if v > 0]
        diffs, spreads, iters, accs = [], [], [], []
        runs = []
        for i in range(a.repeats):
            v, it, acc = run_one(exe, decktxt, a.node, a.ele, wb / f"mx{mx}_p{i}", 22)
            runs.append(v)
            iters.append(it)
            accs.append(acc)
            diffs.append(max(abs(v[j] - ref[j]) for j in wet))
        for i in range(a.repeats):
            for k in range(i + 1, a.repeats):
                spreads.append(max(abs(runs[i][j] - runs[k][j]) for j in wet))
        res[mx] = {"ref_iters_gezien": refit, "ref_acc_pct": refacc,
                   "par_iters": iters, "par_acc_pct": accs,
                   "max_vs_serieel": max(diffs),
                   "max_onderling": max(spreads) if spreads else 0.0}
        print(f"MXITST={mx}: iters={refit} acc={refacc}% "
              f"max_vs_ser={res[mx]['max_vs_serieel']:.2e} max_par={res[mx]['max_onderling']:.2e}",
              flush=True)
    Path(a.out).write_text(json.dumps(res, indent=1))
    print("geschreven:", a.out)
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
