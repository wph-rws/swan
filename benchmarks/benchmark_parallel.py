#!/usr/bin/env python3
"""Benchmark OpenMP and MPI SWAN executables against upstream."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Any


REPOSITORY = Path(__file__).resolve().parent.parent
DEFAULT_CASE = REPOSITORY / "examples" / "voordelta"
INPUT_FILES = {
    "voordelta.swn": "INPUT",
    "voordelta.dep": "voordelta.dep",
}
OUTPUTS = (
    "voordelta_depth.blk",
    "voordelta_hs.blk",
    "voordelta_sites.tbl",
)
FIELD_OUTPUTS = ("voordelta_depth.blk", "voordelta_hs.blk")
TABLE_OUTPUT = "voordelta_sites.tbl"


def parse_positive_list(value: str) -> list[int]:
    try:
        values = [int(item) for item in value.split(",")]
    except ValueError as error:
        raise argparse.ArgumentTypeError(
            "expected comma-separated integers"
        ) from error
    if not values or any(item < 1 for item in values) or len(set(values)) != len(values):
        raise argparse.ArgumentTypeError(
            "values must be unique positive comma-separated integers"
        )
    return values


def parse_cpu_list(value: str) -> list[int]:
    try:
        values = [int(item) for item in value.split(",")]
    except ValueError as error:
        raise argparse.ArgumentTypeError(
            "expected comma-separated integers"
        ) from error
    if not values or any(item < 0 for item in values) or len(set(values)) != len(values):
        raise argparse.ArgumentTypeError(
            "CPU IDs must be unique non-negative comma-separated integers"
        )
    return values


def executable_path(value: str) -> Path:
    path = Path(value).expanduser().resolve()
    if not path.is_file():
        raise argparse.ArgumentTypeError(f"executable not found: {path}")
    return path


def file_digest(path: Path) -> str:
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(block)
    return value.hexdigest()


def table_data_digest(path: Path) -> str:
    """Hash table data while ignoring whitespace-only header differences."""
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for line in stream:
            stripped = line.strip()
            if stripped and not stripped.startswith(b"%"):
                value.update(b" ".join(stripped.split()))
                value.update(b"\n")
    return value.hexdigest()


def physical_cpu_ids() -> list[int]:
    """Return one Linux logical CPU ID for every physical package/core pair."""
    topology: dict[tuple[int, int], int] = {}
    for cpu_directory in sorted(
        Path("/sys/devices/system/cpu").glob("cpu[0-9]*"),
        key=lambda path: int(path.name[3:]),
    ):
        cpu = int(cpu_directory.name[3:])
        online_file = cpu_directory / "online"
        if online_file.is_file() and online_file.read_text().strip() == "0":
            continue
        try:
            package = int(
                (cpu_directory / "topology" / "physical_package_id").read_text()
            )
            core = int((cpu_directory / "topology" / "core_id").read_text())
        except (FileNotFoundError, ValueError):
            continue
        topology.setdefault((package, core), cpu)
    if topology:
        return list(topology.values())
    return list(range(os.cpu_count() or 1))


def cpu_model() -> str:
    try:
        with Path("/proc/cpuinfo").open() as stream:
            for line in stream:
                if line.startswith("model name"):
                    return line.partition(":")[2].strip()
    except OSError:
        pass
    return platform.processor()


def run_checked(command: list[str]) -> str:
    result = subprocess.run(
        command,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    if result.returncode:
        detail = result.stdout.strip()
        raise RuntimeError(
            f"command failed with exit code {result.returncode}: "
            f"{shlex_join(command)}\n{detail}"
        )
    return result.stdout


def shlex_join(command: list[str]) -> str:
    # shlex.join was added in Python 3.8, which is older than this runner's
    # supported Python, but keeping this local makes error output easy to test.
    import shlex

    return shlex.join(command)


def is_open_mpi(launcher: Path) -> bool:
    try:
        output = run_checked([str(launcher), "--version"])
    except (OSError, RuntimeError):
        return False
    lowered = output.lower()
    return "open mpi" in lowered or "openrte" in lowered


def prepare_run(case_directory: Path, run_directory: Path) -> None:
    for source_name, target_name in INPUT_FILES.items():
        source = case_directory / source_name
        if not source.is_file():
            raise FileNotFoundError(f"case input not found: {source}")
        shutil.copy2(source, run_directory / target_name)


def run_once(
    *,
    mode: str,
    executable: Path,
    workers: int,
    case_directory: Path,
    run_directory: Path,
    cpu_ids: list[int],
    taskset: Path | None,
    mpi_launcher: Path,
    mpi_arguments: list[str],
) -> dict[str, Any]:
    prepare_run(case_directory, run_directory)
    environment = os.environ.copy()
    environment.update(
        OMP_DYNAMIC="FALSE",
        OMP_NUM_THREADS=str(workers if mode == "openmp" else 1),
        OMP_PLACES="cores",
        OMP_PROC_BIND="close",
        OMP_WAIT_POLICY="PASSIVE",
    )
    if mode == "openmp":
        command = [str(executable)]
        if taskset is not None:
            selected = ",".join(str(cpu) for cpu in cpu_ids[:workers])
            command = [str(taskset), "--cpu-list", selected, *command]
    else:
        command = [
            str(mpi_launcher),
            *mpi_arguments,
            "-n",
            str(workers),
            str(executable),
        ]

    started = time.monotonic()
    result = subprocess.run(
        command,
        cwd=run_directory,
        env=environment,
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    elapsed = time.monotonic() - started
    if result.returncode or not (run_directory / "norm_end").is_file():
        detail = result.stdout[-4000:].strip()
        raise RuntimeError(
            f"{mode} run failed with {workers} worker(s):\n{detail}"
        )
    missing = [name for name in OUTPUTS if not (run_directory / name).is_file()]
    if missing:
        raise RuntimeError(f"run did not produce: {', '.join(missing)}")
    return {
        "seconds": elapsed,
        "sha256": {
            name: file_digest(run_directory / name)
            for name in OUTPUTS
        },
        "numerical_data_sha256": {
            TABLE_OUTPUT: table_data_digest(run_directory / TABLE_OUTPUT)
        },
    }


def summarize(raw_runs: dict[str, Any], workers: list[int]) -> dict[str, Any]:
    summary: dict[str, Any] = {}
    for mode, builds in raw_runs.items():
        summary[mode] = {}
        for build, worker_runs in builds.items():
            one_worker = statistics.median(
                run["seconds"] for run in worker_runs[str(workers[0])]
            )
            build_summary: dict[str, Any] = {}
            for worker in workers:
                runs = worker_runs[str(worker)]
                seconds = [run["seconds"] for run in runs]
                median = statistics.median(seconds)
                hashes = {
                    tuple(sorted(run["sha256"].items()))
                    for run in runs
                }
                numerical_hashes = {
                    tuple(sorted(run["numerical_data_sha256"].items()))
                    for run in runs
                }
                build_summary[str(worker)] = {
                    "seconds": seconds,
                    "median_seconds": median,
                    "scaling_speedup": one_worker / median,
                    "parallel_efficiency": one_worker / median / worker,
                    "repeat_outputs_byte_identical": len(hashes) == 1,
                    "repeat_numerical_data_identical": len(numerical_hashes) == 1,
                    "sha256": runs[0]["sha256"],
                    "numerical_data_sha256": runs[0]["numerical_data_sha256"],
                }
            summary[mode][build] = build_summary

        for worker in workers:
            upstream = summary[mode]["upstream"][str(worker)]["median_seconds"]
            current = summary[mode]["current"][str(worker)]["median_seconds"]
            summary[mode]["current"][str(worker)][
                "speedup_vs_upstream"
            ] = upstream / current
    return summary


def validation_summary(summary: dict[str, Any]) -> dict[str, Any]:
    all_hashes: dict[str, set[str]] = {name: set() for name in OUTPUTS}
    all_numerical_hashes: dict[str, set[str]] = {TABLE_OUTPUT: set()}
    repeats_identical = True
    repeated_numerical_data_identical = True
    for builds in summary.values():
        for configurations in builds.values():
            for result in configurations.values():
                repeats_identical &= result["repeat_outputs_byte_identical"]
                repeated_numerical_data_identical &= result[
                    "repeat_numerical_data_identical"
                ]
                for name, digest in result["sha256"].items():
                    all_hashes[name].add(digest)
                for name, digest in result["numerical_data_sha256"].items():
                    all_numerical_hashes[name].add(digest)
    return {
        "repeats_byte_identical": repeats_identical,
        "repeated_numerical_data_identical": repeated_numerical_data_identical,
        "all_configurations_byte_identical": all(
            len(hashes) == 1 for hashes in all_hashes.values()
        ),
        "field_outputs_byte_identical": all(
            len(all_hashes[name]) == 1 for name in FIELD_OUTPUTS
        ),
        "site_table_numerical_data_identical": (
            len(all_numerical_hashes[TABLE_OUTPUT]) == 1
        ),
        "unique_sha256_per_output": {
            name: len(hashes) for name, hashes in all_hashes.items()
        },
    }


def markdown_tables(summary: dict[str, Any], workers: list[int]) -> str:
    sections: list[str] = []
    for mode, title in (("openmp", "OpenMP"), ("mpi", "MPI")):
        sections.extend(
            [
                f"## {title}",
                "",
                "| Workers | Upstream | Upstream scaling | Current | "
                "Current scaling | Current vs upstream |",
                "|---:|---:|---:|---:|---:|---:|",
            ]
        )
        for worker in workers:
            upstream = summary[mode]["upstream"][str(worker)]
            current = summary[mode]["current"][str(worker)]
            sections.append(
                f"| {worker} | {upstream['median_seconds']:.2f} s | "
                f"{upstream['scaling_speedup']:.2f}x | "
                f"{current['median_seconds']:.2f} s | "
                f"{current['scaling_speedup']:.2f}x | "
                f"{current['speedup_vs_upstream']:.2f}x |"
            )
        sections.append("")
    return "\n".join(sections)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--current-openmp", required=True, type=executable_path)
    parser.add_argument("--upstream-openmp", required=True, type=executable_path)
    parser.add_argument("--current-mpi", required=True, type=executable_path)
    parser.add_argument("--upstream-mpi", required=True, type=executable_path)
    parser.add_argument("--case-directory", type=Path, default=DEFAULT_CASE)
    parser.add_argument(
        "--workers",
        type=parse_positive_list,
        default=[1, 2, 4, 8],
        help="thread/process counts (default: %(default)s)",
    )
    parser.add_argument(
        "--repetitions",
        type=int,
        default=3,
        help="runs per build and worker count (default: %(default)s)",
    )
    parser.add_argument(
        "--cpu-list",
        type=parse_cpu_list,
        help="logical CPU IDs used for OpenMP affinity",
    )
    parser.add_argument("--mpi-launcher", default="mpiexec")
    parser.add_argument(
        "--mpi-argument",
        action="append",
        default=[],
        help="launcher argument placed before -n; repeat as needed",
    )
    parser.add_argument("--json", type=Path, help="write raw data and metadata")
    parser.add_argument("--markdown", type=Path, help="write Markdown tables")
    parser.add_argument("--current-revision")
    parser.add_argument("--upstream-revision")
    parser.add_argument(
        "--current-build-description",
        help="compiler/profile description stored in JSON metadata",
    )
    parser.add_argument(
        "--upstream-build-description",
        help="compiler/profile description stored in JSON metadata",
    )
    arguments = parser.parse_args()

    if arguments.repetitions < 1:
        parser.error("--repetitions must be at least 1")
    if arguments.workers[0] != 1 or arguments.workers != sorted(arguments.workers):
        parser.error("--workers must be an ascending list starting with 1")
    case_directory = arguments.case_directory.expanduser().resolve()
    if not case_directory.is_dir():
        parser.error(f"case directory not found: {case_directory}")
    launcher_name = shutil.which(arguments.mpi_launcher)
    if launcher_name is None:
        parser.error(f"MPI launcher not found: {arguments.mpi_launcher}")
    mpi_launcher = Path(launcher_name).resolve()
    cpu_ids = arguments.cpu_list or physical_cpu_ids()
    if max(arguments.workers) > len(cpu_ids):
        parser.error(
            f"{max(arguments.workers)} workers requested, but only "
            f"{len(cpu_ids)} physical cores are available"
        )
    taskset_name = shutil.which("taskset")
    taskset = Path(taskset_name).resolve() if taskset_name else None
    mpi_arguments = list(arguments.mpi_argument)
    if not mpi_arguments and is_open_mpi(mpi_launcher):
        mpi_arguments = ["--bind-to", "core", "--map-by", "core"]

    executables = {
        "openmp": {
            "current": arguments.current_openmp,
            "upstream": arguments.upstream_openmp,
        },
        "mpi": {
            "current": arguments.current_mpi,
            "upstream": arguments.upstream_mpi,
        },
    }
    raw_runs: dict[str, Any] = {
        mode: {build: {str(worker): [] for worker in arguments.workers}
               for build in builds}
        for mode, builds in executables.items()
    }

    with tempfile.TemporaryDirectory(prefix="swan-parallel-benchmark-") as temporary:
        root = Path(temporary)
        for mode, builds in executables.items():
            for worker in arguments.workers:
                for repetition in range(arguments.repetitions):
                    order = (
                        ("upstream", "current")
                        if repetition % 2 == 0
                        else ("current", "upstream")
                    )
                    for build in order:
                        print(
                            f"{mode:7s} {build:8s} workers={worker} "
                            f"run={repetition + 1}/{arguments.repetitions}",
                            flush=True,
                        )
                        run_directory = (
                            root
                            / mode
                            / build
                            / f"workers-{worker}"
                            / f"run-{repetition + 1}"
                        )
                        run_directory.mkdir(parents=True)
                        result = run_once(
                            mode=mode,
                            executable=builds[build],
                            workers=worker,
                            case_directory=case_directory,
                            run_directory=run_directory,
                            cpu_ids=cpu_ids,
                            taskset=taskset,
                            mpi_launcher=mpi_launcher,
                            mpi_arguments=mpi_arguments,
                        )
                        raw_runs[mode][build][str(worker)].append(result)

    summary = summarize(raw_runs, arguments.workers)
    validation = validation_summary(summary)
    tables = markdown_tables(summary, arguments.workers)
    print()
    print(tables)
    print(
        "Field outputs byte-identical: "
        f"{validation['field_outputs_byte_identical']}; "
        "site-table numerical data identical: "
        f"{validation['site_table_numerical_data_identical']}"
    )

    document = {
        "date": time.strftime("%Y-%m-%d"),
        "revisions": {
            "current": arguments.current_revision,
            "upstream": arguments.upstream_revision,
        },
        "host": {
            "platform": platform.platform(),
            "processor": platform.processor(),
            "cpu_model": cpu_model(),
            "logical_cpus": os.cpu_count(),
            "physical_cpu_ids": cpu_ids,
        },
        "configuration": {
            "case_directory": str(case_directory),
            "workers": arguments.workers,
            "repetitions": arguments.repetitions,
            "openmp_affinity": {
                "taskset": str(taskset) if taskset else None,
                "OMP_PLACES": "cores",
                "OMP_PROC_BIND": "close",
                "OMP_DYNAMIC": "FALSE",
            },
            "mpi_launcher": str(mpi_launcher),
            "mpi_arguments": mpi_arguments,
            "executables": {
                mode: {build: str(path) for build, path in builds.items()}
                for mode, builds in executables.items()
            },
            "build_descriptions": {
                "current": arguments.current_build_description,
                "upstream": arguments.upstream_build_description,
            },
        },
        "results": summary,
        "validation": validation,
    }
    if arguments.json:
        arguments.json.parent.mkdir(parents=True, exist_ok=True)
        arguments.json.write_text(json.dumps(document, indent=2) + "\n")
    if arguments.markdown:
        arguments.markdown.parent.mkdir(parents=True, exist_ok=True)
        arguments.markdown.write_text(tables + "\n")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (FileNotFoundError, OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
