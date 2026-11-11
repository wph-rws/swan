#!/usr/bin/env python3
"""FFT measurements behind benchmarks/README.md: the FFTW plan flag and the
lock-free plan lookup.

Three builds of the same commit are compared, each an OpenMP Release build:
the fork as it is ("head"), a copy with fftw_measure.patch ("measure",
`FFTW_MEASURE` instead of `FFTW_ESTIMATE`) and a copy with fftw_lock.patch
("lock", every plan lookup takes the planner lock). The script compiles
`benchmarks/benchmark_fft.f90`, `benchmark_fft_omp.f90` and
`benchmark_fft_plan.f90` against the library of each build and measures:

  plans  benchmark_fft_plan 63 and 125: how many different plans and
         checksums FFTW gives in 20 runs (MEASURE) or 5 runs (ESTIMATE);
  bare   benchmark_fft 63 20000 on one thread, head against measure, 5 runs;
  omp    benchmark_fft_omp 6 2000000 on 1 and on 8 threads, head against lock.

The whole QCM runs are in qcm_timing.py. Run on a quiet machine. One thread
runs on CPU 0, eight threads on CPUs 0, 2, ..., 14 with
`OMP_PLACES=cores OMP_PROC_BIND=close`.

    python3 doc/measurements/fft_timing.py --head BUILD --measure BUILD_MEASURE \\
        --lock BUILD_LOCK --work /tmp/fft [--fc /usr/bin/gfortran-16]

Each BUILD is a CMake build directory (with mod/ and lib/). Writes
WORK/fft_timing.json and prints the medians.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import statistics
import subprocess
import time
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[2]
PROGRAMS = ("benchmark_fft", "benchmark_fft_omp", "benchmark_fft_plan")


def compile_benchmarks(fc: str, builds: dict[str, Path], bench: Path) -> None:
    bench.mkdir(parents=True, exist_ok=True)
    for variant, build in builds.items():
        build = build.resolve()
        library = next((build / "lib").glob("libswan*.a"))
        for program in PROGRAMS:
            subprocess.run([fc, "-O3", "-fopenmp", "-I", str(build / "mod"),
                            str(REPOSITORY / "benchmarks" / f"{program}.f90"), str(library),
                            "-lfftw3", "-o", str(bench / f"{program}-{variant}")],
                           check=True, cwd=bench)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    for name in ("head", "measure", "lock"):
        parser.add_argument(f"--{name}", type=Path, required=True, help=f"build directory ({name})")
    parser.add_argument("--work", type=Path, required=True, help="work directory")
    parser.add_argument("--fc", default="/usr/bin/gfortran-16", help="Fortran compiler")
    parser.add_argument("--cpus1", default="0")
    parser.add_argument("--cpus8", default="0,2,4,6,8,10,12,14")
    args = parser.parse_args()
    bench = args.work / "bench"
    compile_benchmarks(args.fc, {"head": args.head, "measure": args.measure, "lock": args.lock}, bench)

    def run(command: list[str], cpus: str, threads: int) -> str:
        env = dict(os.environ, OMP_NUM_THREADS=str(threads), OMP_PLACES="cores", OMP_PROC_BIND="close")
        done = subprocess.run(["taskset", "-c", cpus] + command, env=env, capture_output=True, text=True)
        assert done.returncode == 0, (command, done.stderr[-500:])
        return done.stdout

    result = {"load_before": os.getloadavg(), "plans": {}, "bare63": {}, "omp6": {}}
    for variant in ("head", "measure"):
        for size in (63, 125):
            plans, sums = [], []
            for _ in range(20 if variant == "measure" else 5):
                out = run([str(bench / f"benchmark_fft_plan-{variant}"), str(size)], args.cpus1, 1)
                plan, _, checksum = out.rpartition("checksum")
                plans.append(hashlib.sha1(" ".join(plan.split()).encode()).hexdigest()[:10])
                sums.append(checksum.strip())
            result["plans"][f"{variant}_{size}"] = {
                "runs": len(plans), "distinct_plans": len(set(plans)),
                "distinct_checksums": len(set(sums)), "plans": plans, "checksums": sums}
            print(variant, size, "distinct plans", len(set(plans)), "distinct checksums",
                  len(set(sums)), "of", len(plans), flush=True)
    for _ in range(5):
        for variant in ("head", "measure"):
            out = run([str(bench / f"benchmark_fft-{variant}"), "63", "20000"], args.cpus1, 1)
            result["bare63"].setdefault(variant, []).append(float(out.split()[0]))
    print("bare 63 x 63, 20000 forward and back, median s:",
          {k: round(statistics.median(x), 3) for k, x in result["bare63"].items()}, flush=True)
    for threads, cpus in ((1, args.cpus1), (8, args.cpus8)):
        for _ in range(5):
            for variant in ("head", "lock"):
                out = run([str(bench / f"benchmark_fft_omp-{variant}"), "6", "2000000"], cpus, threads)
                result["omp6"].setdefault(f"{variant}_t{threads}", []).append(float(out.split()[1]))
    print("6 x 6, 2000000 forward and back per thread, median s:",
          {k: round(statistics.median(x), 3) for k, x in result["omp6"].items()}, flush=True)
    result["load_after"] = os.getloadavg()
    (args.work / "fft_timing.json").write_text(json.dumps(result, indent=1))
    print("load average before/after", result["load_before"], result["load_after"])
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
