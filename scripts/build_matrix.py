#!/usr/bin/env python3
"""Configure, build and test SWAN across every supported build configuration.

SWAN variants use ordinary compile-time capabilities and whole
CMake-selected backend sources. A change that compiles in the default
configuration can therefore still break another configuration. Nothing but
building them all catches that, and doing it by hand invites doing it
partially.

Each configuration is a separate build directory so that repeated runs are
incremental. Pass --clean to force a fresh configure, which is what a release
check should do.

Reporting: passed, failed and not run are reported separately; a skipped
optional compiler does not count as "OK". A required release compiler may
not be missing. For each configuration the test names it must register are
fixed (EXPECTED_TESTS); machine-readable CTest results (JUnit) prove
coverage. Zero tests, missing required tests and any skipped test fail;
`ctest` with exit status zero does not by itself prove coverage. A
configuration without registered tests is red (the negative path is tested
in tests/test_build_matrix.py).

Research route: `debug` (-O0) is visible but does not qualify its route.
A red debug does not block an already qualified GNU route, but it does rule
out the claim "Debug supported". Skipped tests are allowed there.

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
    # JAC alone must be enough: it switches MPI on (CMakeLists.txt).
    "jac": ["-DJAC=ON"],
    "metis": ["-DMETIS=ON"],
    "metis-mpi": ["-DMETIS=ON", "-DMPI=ON"],
    "ffro": ["-DFFRO=ON"],
    "ffro-mpi": ["-DFFRO=ON", "-DMPI=ON"],
    # OpenMP divides the work over the fixed fronts; see the support matrix
    # for why this route is not the same from run to run.
    "ffro-openmp": ["-DFFRO=ON", "-DOPENMP=ON"],
    # COH leaves MPI to a host program. coh tests the empty lifecycle
    # serially; coh-mpi runs a small host program that manages MPI around SWAN.
    "coh": ["-DCOH=ON"],
    "coh-mpi": ["-DCOH=ON", "-DMPI=ON"],
    "esmf": ["-DESMF=ON"],
    "adcirc": ["-DADCIRC=ON"],
    "lto": ["-DSWAN_LTO=ON"],
    "native": ["-DSWAN_NATIVE=ON"],
    "runtime": ["-DSWAN_RUNTIME_CHECKS=ON"],
    "debug-invariants": ["-DSWAN_DEBUG_INVARIANTS=ON"],
    "legacy-cray-io": ["-DSWAN_LEGACY_CRAY_IO=ON"],
    "legacy-sgi-io": ["-DSWAN_LEGACY_SGI_IO=ON"],
    "ifx": ["-DCMAKE_Fortran_COMPILER=ifx"],
    # Research route: -O0 with runtime checks. This combination does not
    # qualify the route.
    "debug": ["-DCMAKE_BUILD_TYPE=Debug", "-DSWAN_RUNTIME_CHECKS=ON"],
}

# Configurations that need a toolchain that may not be installed. They are
# reported as "not run", never as "OK".
OPTIONAL = {
    "ifx": "ifx",
}

# Visible research route: red blocks the release claim for that route,
# but not an already qualified GNU route.
RESEARCH = {"debug"}

# Minimum required coverage per configuration. The full count follows
# from a ctest measurement per configuration; these names may never
# be missing. Extending this with exact counts happens after that measurement.
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
    "curve_output",
    "curve_output_units",
    "official_cases",
    "official_cases_units",
    "adcirc_grid",
}


def expected_tests(args: list[str]) -> set[str]:
    """Return the tests a configuration with these CMake arguments must run.

    Besides the common set, each variant needs the test that reaches its own
    path. Without that test a green configuration only proves that the
    ordinary paths still work.
    """
    need = set(COMMON_REQUIRED)
    flags = " ".join(args)
    mpi = "MPI=ON" in flags or "JAC=ON" in flags
    if mpi and "COH=ON" in flags:
        need.add("coherent_mpi_host")
    elif mpi:
        need.add("quick_test_mpi")
        need.add("mpi_unstructured_partition")
    if "NETCDF=ON" in flags:
        need.add("netcdf_output")
        if mpi:
            need.add("netcdf_output_mpi")
    if "METIS=ON" in flags and mpi:
        need.add("metis_partitioning")
    if "FFRO=ON" in flags:
        need.add("nonstationary_examples")
    if "COH=ON" in flags:
        need.add("mpi_lifecycle_backend")
    if "ESMF=ON" in flags:
        need.add("esmf_wind_input")
    return need


EXPECTED_TESTS: dict[str, set[str]] = {
    name: expected_tests(args) for name, args in MATRIX.items()
}


def compiler_for(args: list[str]) -> list[str]:
    """Choose the compiler explicitly, as AGENTS.md asks.

    The plain `gfortran` command may point to another version; the
    references were made with gfortran 16.2, and all variants, MPI and netCDF
    included, build with it.
    """
    if any(a.startswith("-DCMAKE_Fortran_COMPILER=") for a in args):
        return []
    return ["-DCMAKE_Fortran_COMPILER=gfortran-16"]


def build_dir(name: str) -> Path:
    return ROOT / ("build-modernization" + (f"-{name}" if name else ""))


def run(cmd: list[str], **kw) -> subprocess.CompletedProcess:
    return subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, **kw)


def parse_junit(path: Path) -> tuple[int, int, int, set[str], list[str]]:
    """Read JUnit: (total, failed+error, skipped, names, failed_names)."""
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
        return "not run", f"{tool} not found (optional)"

    bdir = build_dir(name)
    if clean and bdir.exists():
        shutil.rmtree(bdir)

    # Release unless the configuration chooses its own build type (debug).
    build_type = next((a for a in args if a.startswith("-DCMAKE_BUILD_TYPE=")),
                      "-DCMAKE_BUILD_TYPE=Release")
    cfg = run(["cmake", "-S", ".", "-B", str(bdir), build_type,
               *compiler_for(args),
               *(a for a in args if not a.startswith("-DCMAKE_BUILD_TYPE="))])
    if cfg.returncode:
        lines = cfg.stderr.strip().splitlines()
        return "configure fails", lines[-1] if lines else ""

    bld = run(["cmake", "--build", str(bdir), "-j", str(jobs)])
    if bld.returncode:
        errs = [line for line in (bld.stdout + bld.stderr).splitlines()
                if "Error" in line or "error:" in line]
        return "build fails", errs[0] if errs else ""

    junit = bdir / "junit-matrix.xml"
    if junit.is_file():
        junit.unlink()
    tst = run(["ctest", "--test-dir", str(bdir), "--output-on-failure",
               "--output-junit", str(junit)])
    try:
        total, failed, skipped, names, failed_names = parse_junit(junit)
    except ET.ParseError as error:
        return "test fails", f"unreadable JUnit file: {error}"
    if total == 0:
        return "test fails", "zero tests registered (no coverage)"
    missing = sorted(EXPECTED_TESTS.get(name, set()) - names)
    if missing:
        return "test fails", f"missing required tests: {', '.join(missing)}"
    if failed:
        return "test fails", "; ".join(failed_names[:5])
    if tst.returncode:
        # ctest failed but JUnit shows no failures: still red.
        failed_lines = [line for line in tst.stdout.splitlines() if "(Failed)" in line]
        return "test fails", "; ".join(failed_lines) or "ctest failed without JUnit failures"
    if skipped and name not in RESEARCH:
        # Any skipped test fails; a skip is coverage that did not happen.
        return "test fails", f"{skipped} of {total} tests skipped"
    return "green", f"{total} tests ({skipped} skipped)" if skipped else f"{total} tests"


def main() -> int:
    p = argparse.ArgumentParser()
    p.add_argument("--clean", action="store_true",
                   help="remove the build directory first; needed for a full measurement")
    p.add_argument("--jobs", type=int, default=8)
    p.add_argument("--only", nargs="*", default=None,
                   help="only these configurations (empty name = the default)")
    a = p.parse_args()

    names = a.only if a.only is not None else list(MATRIX)
    if not names:
        p.error("--only needs at least one configuration name")
    # Check every name before the first build, not after an hour of them.
    for name in names:
        if name not in MATRIX:
            print(f"unknown configuration: {name}", file=sys.stderr)
            return 2
    width = max(len(n or "default") for n in names)
    n_green = n_bad = n_skipped = n_research_red = 0
    for name in names:
        t0 = time.time()
        status, detail = check_one(name, MATRIX[name], a.jobs, a.clean)
        if status == "green":
            n_green += 1
        elif status == "not run":
            n_skipped += 1
        elif name in RESEARCH:
            n_research_red += 1
            status = "research-red"
        else:
            n_bad += 1
        print(f"{(name or 'default'):<{width}}  {status:<16} "
              f"{detail:<55} {time.time() - t0:5.0f}s", flush=True)
    print(f"\ngreen={n_green} failed={n_bad} not-run={n_skipped} "
          f"research-red={n_research_red} (total {len(names)})")
    return 1 if n_bad else 0


if __name__ == "__main__":
    sys.exit(main())
