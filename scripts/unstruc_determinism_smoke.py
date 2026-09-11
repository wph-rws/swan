#!/usr/bin/env python3
"""CI smoke test for unstructured determinism (BF-15, BF-17).

The unstructured Voordelta deck with wind on a mesh of about 4,700 vertices
(1000 m), two iterations: one run on a single thread, then three runs on four
threads, and the hashes of the vertex-resolved COMPGRID output must all be
equal. The wind matters: in the first iteration SWAN uses its second-generation
first guess, and that is where a shared scratch variable made the result depend
on the threads (BF-17). On a much smaller mesh the threads rarely meet there,
so the test would pass with the defect present. Takes seconds; meant for the
openmp job in ci.yml.
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

#  The deck edits below need these lines; without them the run would not be
#  capped at two iterations or would write no vertex output.
NEEDED_LINES = ("GEN3 KOMEN", "FRICTION JONSWAP CONSTANT 0.038", "COMPUTE")

def make_mesh(work: Path, spacing: float) -> int:
    """Write voordelta_unstructured.node/.ele into work; return the vertex count."""
    sys.path.insert(0, str(CASE))
    from generate_mesh import generate_mesh  # noqa
    nverts, _ = generate_mesh(work, spacing, True)
    return nverts

def vertex_deck(block: str) -> str:
    """The unstructured Voordelta deck, capped at two iterations, writing Hsig
    per vertex to the file block (BLOCK COMPGRID, one value per vertex)."""
    base = (CASE / "voordelta_unstructured.swn").read_text()
    for needed in NEEDED_LINES:
        if needed not in base.splitlines():
            raise ValueError(f"deck lacks the line {needed!r}; update this script")
    deck = base.replace("GEN3 KOMEN", "GEN3 KOMEN DRAG FIT")
    deck = deck.replace("FRICTION JONSWAP CONSTANT 0.038",
                        "FRICTION JONSWAP CONSTANT 0.038\n"
                        "NUMERIC STOPC STAT 2\n"
                        "OUTPUT OPTIONS BLOCK 9 200")
    # Vertex-resolved output only; drop the FRAME interpolation.
    lines = []
    for ln in deck.splitlines():
        if ln.startswith("FRAME") or "'MAP'" in ln:
            continue
        if ln == "COMPUTE":
            lines.append(f"BLOCK 'COMPGRID' NOHEADER '{block}' LAYOUT 3 HSIGN")
        lines.append(ln)
    # Make the depth file path absolute (the work directory is elsewhere).
    dep = (CASE.parent / "voordelta" / "voordelta.dep").resolve()
    return "\n".join(lines).replace("../voordelta/voordelta.dep", str(dep)) + "\n"

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--exe", required=True, help="path to swan.exe (OpenMP build)")
    ap.add_argument("--spacing", type=float, default=1000.0)
    ap.add_argument("--threads", type=int, default=4)
    ap.add_argument("--runs", type=int, default=3,
                    help="runs on --threads threads, each compared with one serial run")
    a = ap.parse_args()

    #  Absolute, because SWAN runs in the temporary work directory.
    exe = str(Path(a.exe).resolve())
    work = Path(tempfile.mkdtemp(prefix="unstruc_smoke_"))
    try:
        nverts = make_mesh(work, a.spacing)
        try:
            (work / "INPUT").write_text(vertex_deck("smoke_hs.blk"))
        except ValueError as error:
            print(error, file=sys.stderr)
            return 2
        blk = work / "smoke_hs.blk"
        hashes = []
        for i, threads in enumerate([1] + [a.threads] * a.runs):
            env = os.environ.copy()
            env["OMP_NUM_THREADS"] = str(threads)
            env["OMP_PLACES"] = "cores"
            env["OMP_PROC_BIND"] = "spread"
            #  Remove the previous run's output, so that every run must
            #  write its own.
            blk.unlink(missing_ok=True)
            r = subprocess.run([exe], cwd=work, env=env,
                               capture_output=True, text=True)
            if r.returncode != 0 or not blk.exists():
                print(f"run {i} failed (rc={r.returncode})", file=sys.stderr)
                return 2
            hashes.append(sha256(blk))
        serial, parallel = hashes[0], hashes[1:]
        print(f"mesh {nverts} vertices: 1 thread {serial[:16]}..., "
              f"{a.threads} threads " + ", ".join(h[:16] + "..." for h in parallel))
        if any(h != serial for h in parallel):
            print("SMOKE FAIL: the result depends on the threads")
            return 1
        print(f"SMOKE PASS: {a.runs} runs on {a.threads} threads bit-identical to the serial run")
        return 0
    finally:
        shutil.rmtree(work, ignore_errors=True)

if __name__ == "__main__":
    raise SystemExit(main())
