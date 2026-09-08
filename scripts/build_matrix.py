#!/usr/bin/env python3
"""Configure, build and test SWAN across every supported build configuration.

SWAN variants now use ordinary compile-time capabilities and whole
CMake-selected backend sources. A change that compiles in the default
configuration can therefore still break another configuration. Nothing but
building them all catches that, and doing it by hand invites doing it
partially.

Each configuration is a separate build directory so that repeated runs are
incremental. Pass --clean to force a fresh configure, which is what the
diagnostic ratchet needs and what a release check should do.

Rapportage: geslaagd, gefaald en niet uitgevoerd worden afzonderlijk
gerapporteerd; een overgeslagen optionele compiler telt niet als "in orde".
Een vereiste releasecompiler mag niet ontbreken. Per configuratie liggen de
verwachte testnamen en mogelijkheden vast (EXPECTED_TESTS); machineleesbare
CTest-resultaten (JUnit) bewijzen dekking. Nul tests, ontbrekende vereiste
tests en overgeslagen vereiste runtimeproeven falen; `ctest` met exitstatus
nul bewijst op zichzelf geen dekking. Een configuratie zonder geregistreerde
tests is rood (negatief pad is getest in tests/test_build_matrix.py).

Onderzoeksport: `debug` (-O0) is zichtbaar maar kwalificeert haar
route niet zolang de nonstationary-afwijking open staat. Een rode debug
blokkeert geen reeds gekwalificeerde GNU-route, maar sluit de claim
"Debug ondersteund" wel uit.

Usage:
    scripts/build_matrix.py [--clean] [--jobs N] [--only NAME ...]
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from pathlib import Path

from strict_diagnostics import STRICT_FLAGS

ROOT = Path(__file__).resolve().parent.parent

# name -> cmake -D arguments. The name is also the build directory suffix.
#
# A combination earns a place here when it activates behavior that no single
# variant does. METIS only reaches SwanParallel when MPI is on, MPI timing
# services only run when TIMG and MPI are both on, and FFRO changes the
# unstructured vertex order that MPI partitioning then has to carry.
# Matlab v4 plus netCDF proves that both selected output-source lists compose.
MATRIX: dict[str, list[str]] = {
    "": [],
    "openmp": ["-DOPENMP=ON"],
    "timg": ["-DTIMG=ON"],
    "timg-openmp": ["-DTIMG=ON", "-DOPENMP=ON"],
    "timg-mpi": ["-DTIMG=ON", "-DMPI=ON"],
    "matl4": ["-DMATL4=ON"],
    "matl4-mpi": ["-DMATL4=ON", "-DMPI=ON"],
    "matl4-netcdf": ["-DMATL4=ON", "-DNETCDF=ON"],
    "netcdf": ["-DNETCDF=ON"],
    "mpi": ["-DMPI=ON"],
    "mpi-netcdf": ["-DMPI=ON", "-DNETCDF=ON"],
    "jac": ["-DMPI=ON", "-DJAC=ON"],
    "metis": ["-DMETIS=ON"],
    "metis-mpi": ["-DMETIS=ON", "-DMPI=ON"],
    "ffro": ["-DFFRO=ON"],
    "ffro-mpi": ["-DFFRO=ON", "-DMPI=ON"],
    # The external coherent runtime is unavailable here. This entry tests the
    # selected no-op lifecycle contract; COH+MPI remains a compile/link gate.
    "coh": ["-DCOH=ON"],
    "esmf": ["-DESMF=ON"],
    "adcirc": ["-DADCIRC=ON"],
    "lto": ["-DSWAN_LTO=ON"],
    "native": ["-DSWAN_NATIVE=ON"],
    "runtime": ["-DSWAN_RUNTIME_CHECKS=ON"],
    "debug-invariants": ["-DSWAN_DEBUG_INVARIANTS=ON"],
    "legacy-cray-io": ["-DSWAN_LEGACY_CRAY_IO=ON"],
    "legacy-sgi-io": ["-DSWAN_LEGACY_SGI_IO=ON"],
    "gcc15": ["-DCMAKE_Fortran_COMPILER=gfortran-15"],
    # Onderzoeksport: -O0 met runtimechecks; bekend rood op
    # nonstationary_regular (2,1e-4 m Hsig, gemeten 2026-07-29). Zichtbaar,
    # kwalificeert de route niet.
    "debug": ["-DCMAKE_BUILD_TYPE=Debug", "-DSWAN_RUNTIME_CHECKS=ON"],
    # The warning ratchet owns the strict flags; this entry only proves that a
    # strict build still compiles and passes its tests. Without the flags it
    # was an exact copy of the default configuration and tested nothing.
    "strict": [f"-DCMAKE_Fortran_FLAGS={STRICT_FLAGS}"],
}

# Configurations that need a toolchain that may not be installed. They are
# reported as "niet uitgevoerd", never as "in orde".
OPTIONAL = {
    "gcc15": "gfortran-15",
}

# Zichtbare onderzoeksport: rood blokkeert de releaseclaim voor die route,
# maar geen reeds gekwalificeerde GNU-route.
RESEARCH = {"debug"}

# Minimale vereiste dekking per configuratie. Volledige telling volgt
# uit een ctest-meting per configuratie; deze namen mogen nooit
# ontbreken. Uitbreiding met exacte aantallen gebeurt na die meting.
COMMON_REQUIRED = {
    "quick_test",
    "thread_state_manifest",
    "run_state_ownership",
    "switch_manifest",
    "reference_check_negatives",
    "swan_library_contract",
    "mpi_field_mask_self_test",
    "lifetime_fault",
    "physics_reuse",
    "grid_reuse",
    "spectrum_reuse",
    "shoaling",
    "shoaling_units",
    "grid_convergence",
    "grid_convergence_units",
    "time_convergence",
    "time_convergence_units",
}
EXPECTED_TESTS: dict[str, set[str]] = {}
for _name, _args in MATRIX.items():
    _need = set(COMMON_REQUIRED)
    if "MPI=ON" in " ".join(_args) or "JAC=ON" in " ".join(_args):
        _need.add("quick_test_mpi")
        _need.add("mpi_unstructured_partition")
    if "NETCDF=ON" in " ".join(_args):
        _need.add("netcdf_output")
    EXPECTED_TESTS[_name] = _need


def build_dir(name: str) -> Path:
    return ROOT / ("build-modernization" + (f"-{name}" if name else ""))


def run(cmd: list[str], **kw) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, **kw)


def parse_junit(path: Path) -> tuple[int, int, int, set[str], list[str]]:
    """Lees JUnit: (totaal, gefaald+fout, overgeslagen, namen, gefaalde_namen)."""
    if not path.is_file():
        return 0, 0, 0, set(), []
    tree = ET.parse(path)
    root = tree.getroot()
    total = failed = skipped = 0
    names: set[str] = set()
    failed_names: list[str] = []
    for case in root.iter("testcase"):
        total += 1
        name = case.get("name", "")
        names.add(name)
        has_fail = case.find("failure") is not None or case.find("error") is not None
        has_skip = case.find("skipped") is not None
        if has_fail:
            failed += 1
            failed_names.append(name)
        elif has_skip:
            skipped += 1
    return total, failed, skipped, names, failed_names


def check_one(name: str, args: list[str], jobs: int, clean: bool) -> tuple[str, str]:
    tool = OPTIONAL.get(name)
    if tool and shutil.which(tool) is None:
        return "niet uitgevoerd", f"{tool} niet gevonden (optioneel)"

    bdir = build_dir(name)
    if clean and bdir.exists():
        shutil.rmtree(bdir)

    cfg = run(["cmake", "-S", ".", "-B", str(bdir),
               "-DCMAKE_BUILD_TYPE=Release" if name != "debug" else "-DCMAKE_BUILD_TYPE=Debug",
               *([a for a in args if not a.startswith("-DCMAKE_BUILD_TYPE")])])
    # NB: debug-configuratie krijgt Debug-buildtype; overige Release.
    if cfg.returncode:
        lines = cfg.stderr.strip().splitlines()
        return "configure faalt", lines[-1] if lines else ""

    bld = run(["cmake", "--build", str(bdir), "-j", str(jobs)])
    if bld.returncode:
        errs = [line for line in (bld.stdout + bld.stderr).splitlines()
                if "Error" in line or "error:" in line]
        return "bouw faalt", errs[0] if errs else ""

    junit = bdir / "junit-matrix.xml"
    if junit.is_file():
        junit.unlink()
    tst = run(["ctest", "--test-dir", str(bdir), "--output-on-failure",
               "--output-junit", str(junit)])
    total, failed, skipped, names, failed_names = parse_junit(junit)
    if total == 0:
        return "test faalt", "nul tests geregistreerd (geen dekking)"
    missing = sorted(EXPECTED_TESTS.get(name, set()) - names)
    if missing:
        return "test faalt", f"ontbrekende vereiste tests: {', '.join(missing)}"
    if failed:
        return "test faalt", "; ".join(failed_names[:5])
    if tst.returncode:
        # ctest faalde maar JUnit toont geen falers: alsnog rood.
        failed_lines = [line for line in tst.stdout.splitlines() if "(Failed)" in line]
        return "test faalt", "; ".join(failed_lines) or "ctest faalde zonder JUnit-falers"
    if skipped and name not in RESEARCH:
        # Overgeslagen vereiste runtimeproeven falen.
        return "test faalt", f"{skipped} tests overgeslagen van {total}"
    return "groen", f"{total} tests ({skipped} overgeslagen)" if skipped else f"{total} tests"


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
    n_green = n_bad = n_skipped = n_research_red = 0
    for name in names:
        if name not in MATRIX:
            print(f"onbekende configuratie: {name}", file=sys.stderr)
            return 2
        t0 = time.time()
        status, detail = check_one(name, MATRIX[name], a.jobs, a.clean)
        if status == "groen":
            n_green += 1
        elif status == "niet uitgevoerd":
            n_skipped += 1
        elif name in RESEARCH:
            n_research_red += 1
            status = "onderzoek-rood"
        else:
            n_bad += 1
        print(f"{(name or 'standaard'):<{width}}  {status:<16} "
              f"{detail:<55} {time.time() - t0:5.0f}s", flush=True)
    print(f"\ngroen={n_green} gefaald={n_bad} niet-uitgevoerd={n_skipped} "
          f"onderzoek-rood={n_research_red} (totaal {len(names)})")
    return 1 if n_bad else 0


if __name__ == "__main__":
    sys.exit(main())
