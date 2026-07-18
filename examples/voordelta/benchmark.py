#!/usr/bin/env python3
"""Benchmark SWAN on Voordelta without modifying the example directory."""

from __future__ import annotations

import argparse
import hashlib
import os
import shutil
import subprocess
import tempfile
import time
from pathlib import Path


CASE_DIRECTORY = Path(__file__).resolve().parent
OUTPUTS = ("voordelta_depth.blk", "voordelta_hs.blk", "voordelta_sites.tbl")


def digest(path: Path) -> str:
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(block)
    return value.hexdigest()


def run_once(executable: Path, threads: int, root: Path) -> tuple[float, dict[str, str]]:
    run_directory = root / f"threads-{threads}"
    run_directory.mkdir()
    shutil.copyfile(CASE_DIRECTORY / "voordelta.swn", run_directory / "INPUT")
    shutil.copyfile(CASE_DIRECTORY / "voordelta.dep", run_directory / "voordelta.dep")

    environment = os.environ.copy()
    environment.update(
        OMP_NUM_THREADS=str(threads),
        OMP_PLACES="cores",
        OMP_PROC_BIND="close",
        OMP_WAIT_POLICY="PASSIVE",
    )
    started = time.monotonic()
    result = subprocess.run(
        [executable],
        cwd=run_directory,
        env=environment,
        stdout=subprocess.DEVNULL,
        check=False,
    )
    elapsed = time.monotonic() - started
    if result.returncode or not (run_directory / "norm_end").is_file():
        raise RuntimeError(f"SWAN failed with {threads} thread(s)")
    return elapsed, {name: digest(run_directory / name) for name in OUTPUTS}


def parse_threads(value: str) -> list[int]:
    threads = [int(item) for item in value.split(",")]
    if not threads or any(item < 1 for item in threads):
        raise argparse.ArgumentTypeError("threads must be positive comma-separated integers")
    return threads


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True, type=Path)
    parser.add_argument("--threads", default=[1, 2, 4, 8], type=parse_threads)
    arguments = parser.parse_args()
    executable = arguments.swan_executable.expanduser().resolve()
    if not executable.is_file():
        parser.error(f"SWAN executable not found: {executable}")

    reference: dict[str, str] | None = None
    reference_time: float | None = None
    with tempfile.TemporaryDirectory(prefix="swan-voordelta-benchmark-") as temporary:
        root = Path(temporary)
        print("threads  seconds  speedup  identical")
        for threads in arguments.threads:
            elapsed, hashes = run_once(executable, threads, root)
            if reference is None:
                reference = hashes
                reference_time = elapsed
            identical = hashes == reference
            speedup = reference_time / elapsed
            print(f"{threads:7d}  {elapsed:7.2f}  {speedup:7.2f}x  {str(identical):>9}")
            if not identical:
                raise RuntimeError(f"outputs differ with {threads} thread(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
