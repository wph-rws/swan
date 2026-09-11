#!/usr/bin/env python3
"""Run the official TU Delft benchmark cases with analytic gates.

Usage:
    run.py --swan-executable PATH --work-directory DIR [--keep] [--case NAME]

For every case in cases.json: download the tarball (unless cached with a
matching SHA-256), extract, stage the deck as INPUT, run SWAN, require
norm_end, and compare the transect table against the shipped analytical
solution. Dry transect points (SWAN EXCV, Hs <= -9) are masked and never
averaged, following the matrix rule. Any gate failure is an error; a failed
download is an error too (CI has network) — never a silent green run.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
import tarfile
import time
import urllib.request
from pathlib import Path

EXAMPLE_DIR = Path(__file__).resolve().parent
EXCV_HS = -9.0


def fail(message: str) -> int:
    print(f"error: {message}")
    return 1


def fetch(tarball: str, sha256: str, base_url: str, cache: Path) -> Path:
    target = cache / tarball
    if target.is_file():
        digest = hashlib.sha256(target.read_bytes()).hexdigest()
        if digest == sha256:
            print(f"  cached {tarball} (sha256 ok)")
            return target
        print(f"  cached {tarball} has wrong hash, re-downloading")
        target.unlink()
    url = f"{base_url}/{tarball}"
    print(f"  downloading {url}")
    try:
        with urllib.request.urlopen(url, timeout=120) as response:
            data = response.read()
    except Exception as exc:
        raise RuntimeError(f"cannot download {url}: {exc}") from exc
    digest = hashlib.sha256(data).hexdigest()
    if digest != sha256:
        raise RuntimeError(
            f"sha256 mismatch for {tarball}: got {digest}, want {sha256}")
    target.write_bytes(data)
    return target


def read_numbers(path: Path, minimum_columns: int) -> list[list[float]]:
    rows: list[list[float]] = []
    for line in path.read_text().splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        parts = stripped.split()
        try:
            values = [float(part) for part in parts]
        except ValueError:
            continue
        if len(values) >= minimum_columns:
            rows.append(values)
    return rows


def read_ana(path: Path) -> list[list[float]]:
    rows: list[list[float]] = []
    for line in path.read_text().splitlines():
        stripped = line.strip()
        if not stripped or stripped[0].isalpha():
            continue
        try:
            values = [float(part) for part in stripped.split()]
        except ValueError:
            continue
        if len(values) >= 3:
            rows.append(values)
    return rows


def shortest_arc(a: float, b: float) -> float:
    return abs((a - b + 180.0) % 360.0 - 180.0)


def vendor_gaps(work: Path, stage: Path, case: dict,
                wet: list[list[float]], tc: dict) -> tuple[float, float]:
    """Largest ours-vs-Delft-shipped-table gaps, read from the pristine tree.

    The SWAN run overwrites same-named tables inside the stage directory, so
    the vendor file must come from work/vendor/<name>, never from stage.
    """
    name = case["name"]
    vendor = case["vendor_table"]
    roots = [child for child in (work / "vendor" / name).iterdir()
             if child.is_dir()]
    pristine_stage = roots[0] if len(roots) == 1 else work / "vendor" / name
    shipped = pristine_stage / vendor
    if not shipped.is_file():
        raise RuntimeError(f"{name} tarball has no {vendor}")
    vc = case.get("vendor_columns", tc)
    other = read_numbers(shipped, max(vc.values()) + 1)
    gap_hs = max(abs(row[tc["hs"]] - min(
        other, key=lambda c: abs(c[vc["key"]] - row[tc["key"]]))[vc["hs"]])
        for row in wet)
    gap_dir = max(shortest_arc(row[tc["dir"]], min(
        other, key=lambda c: abs(c[vc["key"]] - row[tc["key"]]))[vc["dir"]])
        for row in wet)
    return gap_hs, gap_dir


def run_case(executable: Path, work: Path, case: dict) -> int:
    name = case["name"]
    print(f"[{name}] {case['description']}")
    pristine = work / "vendor" / name
    if not pristine.is_dir():
        return fail(f"{name} was not extracted (fetch step incomplete)")
    case_dir = work / name
    if case_dir.exists():
        shutil.rmtree(case_dir)
    # Stage a copy: the SWAN run overwrites same-named tables (e.g.
    # a11ref01.tab), so the vendor comparison must read the pristine tree.
    shutil.copytree(pristine, case_dir)
    roots = [child for child in case_dir.iterdir() if child.is_dir()]
    stage = roots[0] if len(roots) == 1 else case_dir
    input_file = stage / "INPUT"
    shutil.copyfile(stage / case["deck"], input_file)

    started = time.monotonic()
    try:
        result = subprocess.run([str(executable)], cwd=stage, check=False,
                                capture_output=True, text=True)
    finally:
        input_file.unlink(missing_ok=True)
    print(f"  ran in {time.monotonic() - started:.1f} s, exit {result.returncode}")
    if result.returncode:
        return fail(f"{name} stopped with exit {result.returncode}\n{result.stdout[-2000:]}")
    if not (stage / "norm_end").is_file():
        return fail(f"{name} created no norm_end")

    table = stage / case["table"]
    if not table.is_file():
        return fail(f"{name} created no {case['table']}")
    ana = stage / case["ana"]
    if not ana.is_file():
        return fail(f"{name} tarball has no {case['ana']}")

    tc = case["table_columns"]
    ac = case["ana_columns"]
    swan = read_numbers(table, max(tc.values()) + 1)
    ref = read_ana(ana)
    if not swan or not ref:
        return fail(f"{name} parsed no rows (table {len(swan)}, ana {len(ref)})")
    wet = [row for row in swan if row[tc["hs"]] > EXCV_HS]
    dry = len(swan) - len(wet)
    print(f"  {len(swan)} transect rows, {len(wet)} wet, {dry} dry (EXCV-masked)")
    if not wet:
        return fail(f"{name} has no wet transect points")
    # Direction is compared only in water deeper than dir_min_depth: at the
    # discrete shoreline the analytic direction turns near-singularly while
    # any gridded model (including Delft's own binary) takes the discrete
    # turn instead. Hs is gated on every wet point.
    depth_column = tc.get("depth")
    floor = case.get("dir_min_depth", 0.0)
    peak_hs = peak_dir = 0.0
    for row in wet:
        anchor = min(ref, key=lambda candidate: abs(candidate[ac["key"]] - row[tc["key"]]))
        peak_hs = max(peak_hs, abs(row[tc["hs"]] - anchor[ac["hs"]]))
        if depth_column is None or row[depth_column] > floor:
            peak_dir = max(peak_dir, shortest_arc(row[tc["dir"]], anchor[ac["dir"]]))
    print(f"  max|dHs| = {peak_hs:.3e} (tol {case['tolerances']['hs']})  "
          f"max|dDir| = {peak_dir:.3e} (tol {case['tolerances']['dir']})")
    if peak_hs > case["tolerances"]["hs"]:
        return fail(f"{name} Hs gate failed: {peak_hs:.3e}")
    if peak_dir > case["tolerances"]["dir"]:
        return fail(f"{name} direction gate failed: {peak_dir:.3e}")
    vendor = case.get("vendor_table")
    if vendor:
        try:
            gap_hs, gap_dir = vendor_gaps(work, stage, case, wet, tc)
        except RuntimeError as exc:
            return fail(str(exc))
        mode = case.get("vendor_check", "report")
        print(f"  vs Delft-shipped table: max dHs {gap_hs:.3e}, max dDir {gap_dir:.3e} ({mode})")
        if mode == "exact" and (gap_hs != 0.0 or gap_dir != 0.0):
            return fail(f"{name} vendor-table gate failed")
    print(f"  [{name}] PASS")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True,
                        help="scratch directory (outside the repo); tarballs are cached here")
    parser.add_argument("--case", default=None, help="run a single case only")
    parser.add_argument("--keep", action="store_true", help="keep staged case directories")
    arguments = parser.parse_args(argv)

    executable = Path(arguments.swan_executable)
    if not executable.is_file():
        return fail(f"SWAN executable not found: {executable}")
    work = Path(arguments.work_directory)
    work.mkdir(parents=True, exist_ok=True)
    manifest = json.loads((EXAMPLE_DIR / "cases.json").read_text())
    cases = manifest["cases"]
    if arguments.case:
        cases = [case for case in cases if case["name"] == arguments.case]
        if not cases:
            return fail(f"unknown case {arguments.case}")

    for case in cases:
        try:
            archive = fetch(case["tarball"], case["sha256"], manifest["base_url"], work)
        except RuntimeError as exc:
            return fail(str(exc))
        vendor_dir = work / "vendor" / case["name"]
        marker = vendor_dir / ".extracted"
        if not marker.is_file():
            if vendor_dir.exists():
                shutil.rmtree(vendor_dir)
            vendor_dir.mkdir(parents=True)
            with tarfile.open(archive) as tar:
                tar.extractall(vendor_dir)
            marker.touch()
            print(f"  extracted {case['tarball']} to {vendor_dir}")
    status = 0
    for case in cases:
        status = run_case(executable, work, case) or status
    if not arguments.keep:
        for case in cases:
            shutil.rmtree(work / case["name"], ignore_errors=True)
    print("official_cases: ALL PASS" if status == 0 else "official_cases: FAILURES")
    return status


if __name__ == "__main__":
    sys.exit(main())
