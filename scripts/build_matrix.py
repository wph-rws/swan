#!/usr/bin/env python3
"""Configure, build and test SWAN across every supported build configuration.

Most variants SWAN ships with (`!MPI`, `!MatL4`, `!NCF`, `!JAC`) still select
different source text; TIMG is now a normal compile-time capability. A change
that compiles in the default configuration can therefore still break another
configuration. Nothing but building them all catches that, and doing it by hand
invites doing it partially.

Each configuration is a separate build directory so that repeated runs are
incremental. Pass --clean to force a fresh configure, which is what the
diagnostic ratchet needs and what a release check should do.

Deliberately absent: a Debug configuration. At -O0 the regular nonstationary
case drifts against the stored reference (2.1e-4 m in Hsig, 4.5e-3 s in Tm01,
0.01 degree in direction; measured 2026-07-29), so it can only be carried as a
known-red entry, and it exercises nothing that the `runtime` configuration --
-fcheck=all on top of an optimizing build -- does not already check.

Usage:
    scripts/build_matrix.py [--clean] [--jobs N] [--only NAME ...]
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

from strict_diagnostics import STRICT_FLAGS

ROOT = Path(__file__).resolve().parent.parent

# name -> cmake -D arguments. The name is also the build directory suffix.
#
# A combination earns a place here when it selects source text that no single
# variant does. METIS only reaches SwanParallel when MPI is on, the timer calls
# inside `!MPI` lines only exist when both are on, and FFRO changes the
# unstructured vertex order that the MPI partitioning then has to carry.
MATRIX: dict[str, list[str]] = {
    "": [],
    "openmp": ["-DOPENMP=ON"],
    "timg": ["-DTIMG=ON"],
    "timg-openmp": ["-DTIMG=ON", "-DOPENMP=ON"],
    "timg-mpi": ["-DTIMG=ON", "-DMPI=ON"],
    "matl4": ["-DMATL4=ON"],
    "matl4-mpi": ["-DMATL4=ON", "-DMPI=ON"],
    "netcdf": ["-DNETCDF=ON"],
    "mpi": ["-DMPI=ON"],
    "mpi-netcdf": ["-DMPI=ON", "-DNETCDF=ON"],
    "jac": ["-DMPI=ON", "-DJAC=ON"],
    "metis": ["-DMETIS=ON"],
    "metis-mpi": ["-DMETIS=ON", "-DMPI=ON"],
    "ffro": ["-DFFRO=ON"],
    "ffro-mpi": ["-DFFRO=ON", "-DMPI=ON"],
    "lto": ["-DSWAN_LTO=ON"],
    "native": ["-DSWAN_NATIVE=ON"],
    "runtime": ["-DSWAN_RUNTIME_CHECKS=ON"],
    "debug-invariants": ["-DSWAN_DEBUG_INVARIANTS=ON"],
    "legacy-cray-io": ["-DSWAN_LEGACY_CRAY_IO=ON"],
    "legacy-sgi-io": ["-DSWAN_LEGACY_SGI_IO=ON"],
    "gcc15": ["-DCMAKE_Fortran_COMPILER=gfortran-15"],
    # The warning ratchet owns the strict flags; this entry only proves that a
    # strict build still compiles and passes its tests. Without the flags it
    # was an exact copy of the default configuration and tested nothing.
    "strict": [f"-DCMAKE_Fortran_FLAGS={STRICT_FLAGS}"],
}

# Configurations that need a toolchain that may not be installed. They are
# skipped with a message rather than failing the run.
OPTIONAL = {
    "gcc15": "gfortran-15",
}


def build_dir(name: str) -> Path:
    return ROOT / ("build-modernization" + (f"-{name}" if name else ""))


def run(cmd: list[str], **kw) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, **kw)


def check_one(name: str, args: list[str], jobs: int, clean: bool) -> tuple[str, str]:
    tool = OPTIONAL.get(name)
    if tool and shutil.which(tool) is None:
        return "overgeslagen", f"{tool} niet gevonden"

    bdir = build_dir(name)
    if clean and bdir.exists():
        shutil.rmtree(bdir)

    cfg = run(["cmake", "-S", ".", "-B", str(bdir),
               "-DCMAKE_BUILD_TYPE=Release", *args])
    if cfg.returncode:
        return "configure faalt", cfg.stderr.strip().splitlines()[-1:][0] if cfg.stderr.strip() else ""

    bld = run(["cmake", "--build", str(bdir), "-j", str(jobs)])
    if bld.returncode:
        errs = [l for l in (bld.stdout + bld.stderr).splitlines()
                if "Error" in l or "error:" in l]
        return "bouw faalt", errs[0] if errs else ""

    tst = run(["ctest", "--test-dir", str(bdir), "--output-on-failure"])
    passed = [l for l in tst.stdout.splitlines() if "tests passed" in l]
    if tst.returncode:
        failed = [l for l in tst.stdout.splitlines() if "(Failed)" in l]
        return "test faalt", "; ".join(failed) or (passed[0] if passed else "")
    n = tst.stdout.count("Passed")
    return "groen", f"{n} tests" if n else "geen tests geregistreerd"


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--clean", action="store_true",
                   help="verwijder de bouwmap eerst; nodig voor een volledige meting")
    p.add_argument("--jobs", type=int, default=8)
    p.add_argument("--only", nargs="*", default=None,
                   help="alleen deze configuraties (lege naam = de standaard)")
    a = p.parse_args()

    names = a.only if a.only is not None else list(MATRIX)
    width = max(len(n or "standaard") for n in names)
    bad = 0
    for name in names:
        if name not in MATRIX:
            print(f"onbekende configuratie: {name}", file=sys.stderr)
            return 2
        t0 = time.time()
        status, detail = check_one(name, MATRIX[name], a.jobs, a.clean)
        if status not in ("groen", "overgeslagen"):
            bad += 1
        print(f"{(name or 'standaard'):<{width}}  {status:<14} "
              f"{detail:<50} {time.time() - t0:5.0f}s", flush=True)
    print(f"\n{len(names) - bad}/{len(names)} in orde")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
