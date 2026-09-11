#!/usr/bin/env python3
"""CI-smoke ongestructureerd determinisme (doc/moderniseringsplan.md).

Kleine mesh (210 vertices), 2 runs x 4 threads, hashvergelijking van de
vertex-resolute COMPGRID-uitvoer. Faalt zolang de vu(2)-rand ontbreekt en
slaagt daarna. Secondenwerk, bedoeld voor de openmp-job in ci.yml.
"""
from __future__ import annotations
import argparse, hashlib, os, shutil, subprocess, sys, tempfile
from pathlib import Path

CASE = Path(__file__).resolve().parent.parent / "examples" / "voordelta_unstructured"

def sha256(p: Path) -> str:
    h = hashlib.sha256()
    with p.open("rb") as f:
        for c in iter(lambda: f.read(1 << 20), b""):
            h.update(c)
    return h.hexdigest()

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--exe", required=True, help="pad naar swan.exe (OpenMP-build)")
    ap.add_argument("--spacing", type=float, default=5000.0)
    ap.add_argument("--threads", type=int, default=4)
    a = ap.parse_args()

    sys.path.insert(0, str(CASE))
    from generate_mesh import generate_mesh  # noqa

    work = Path(tempfile.mkdtemp(prefix="unstruc_smoke_"))
    try:
        nverts, _ = generate_mesh(work, a.spacing, True)
        base = (CASE / "voordelta_unstructured.swn").read_text()
        deck = base.replace("GEN3 KOMEN", "GEN3 KOMEN DRAG FIT")
        deck = deck.replace("FRICTION JONSWAP CONSTANT 0.038",
                            "FRICTION JONSWAP CONSTANT 0.038\n"
                            "NUMERIC STOPC STAT 2\n"
                            "OUTPUT OPTIONS BLOCK 9 200")
        # Alleen vertex-resolute uitvoer; FRAME-interpolatie eruit.
        lines = [ln for ln in deck.splitlines()
                 if not ln.startswith("FRAME") and "'MAP'" not in ln]
        block = "BLOCK 'COMPGRID' NOHEADER 'smoke_hs.blk' LAYOUT 3 HSIGN"
        out = []
        for ln in lines:
            if ln == "COMPUTE":
                out.append(block)
            out.append(ln)
        lines = out
        # Dep-pad absoluut maken (werkdir is elders).
        dep = (CASE.parent / "voordelta" / "voordelta.dep").resolve()
        deck = "\n".join(lines).replace("../voordelta/voordelta.dep", str(dep)) + "\n"
        (work / "INPUT").write_text(deck)
        env = os.environ.copy()
        env["OMP_NUM_THREADS"] = str(a.threads)
        env["OMP_PLACES"] = "cores"
        env["OMP_PROC_BIND"] = "spread"
        hashes = []
        for i in range(2):
            r = subprocess.run([a.exe], cwd=work, env=env,
                               capture_output=True, text=True)
            blk = work / "smoke_hs.blk"
            if r.returncode != 0 or not blk.exists():
                print(f"run {i} faalde (rc={r.returncode})", file=sys.stderr)
                return 2
            hashes.append(sha256(blk))
        print(f"mesh {nverts} vertices, {a.threads} threads: {hashes[0][:16]}... vs {hashes[1][:16]}...")
        if hashes[0] != hashes[1]:
            print("SMOKE FAIL: runs verschillen (vu(2)-rand ontbreekt?)")
            return 1
        print("SMOKE PASS: beide runs bit-identiek")
        return 0
    finally:
        shutil.rmtree(work, ignore_errors=True)

if __name__ == "__main__":
    raise SystemExit(main())
