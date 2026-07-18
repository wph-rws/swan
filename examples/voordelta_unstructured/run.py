#!/usr/bin/env python3
"""Generate and run the multicore unstructured Voordelta example."""

from __future__ import annotations

import argparse
import importlib.util
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

from generate_mesh import DEFAULT_SPACING, generate_mesh

GENERATED_FILES = (
    "INPUT",
    "PRINT",
    "Errfile",
    "ERRPTS",
    "norm_end",
    "swaninit",
    "voordelta_unstructured.prt",
    "voordelta_unstructured.erf",
    "voordelta_depth.blk",
    "voordelta_hs.blk",
    "voordelta_sites.tbl",
)

RESULT_NAMES = {
    "PRINT": "voordelta_unstructured.prt",
    "Errfile": "voordelta_unstructured.erf",
    "ERRPTS": "ERRPTS",
    "norm_end": "norm_end",
    "swaninit": "swaninit",
    "voordelta_depth.blk": "voordelta_depth.blk",
    "voordelta_hs.blk": "voordelta_hs.blk",
    "voordelta_sites.tbl": "voordelta_sites.tbl",
}
FIGURE_NAMES = ("voordelta_depth.png", "voordelta_hs.png")


def build_has_options(executable: Path, required: tuple[str, ...]) -> bool:
    cache = executable.parent.parent / "CMakeCache.txt"
    try:
        contents = cache.read_text(encoding="utf-8")
    except OSError:
        return False
    return all(f"{option}:BOOL=ON" in contents for option in required)


def find_executable(case_directory: Path, requested: str | None, parallel: str) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")

    repository = (case_directory / ".." / "..").resolve()
    build_candidates = {
        "openmp": (("build-openmp", ("OPENMP",)), ("build", ("OPENMP",))),
        "mpi": (("build-mpi", ("MPI", "METIS")), ("build", ("MPI", "METIS"))),
        "serial": (("build", ()), ("build-openmp", ()), ("build-mpi", ())),
    }
    for build_name, required in build_candidates[parallel]:
        executable = repository / build_name / "bin" / "swan.exe"
        if executable.is_file() and build_has_options(executable, required):
            return executable
    installed = shutil.which("swan.exe")
    if installed:
        return Path(installed).resolve()
    raise FileNotFoundError(
        "SWAN executable not found. Build the requested parallel variant first "
        "or pass --swan-executable /path/to/swan.exe."
    )


def load_plotter(case_directory: Path):
    plotter_path = case_directory.parent / "voordelta" / "plot_results.py"
    specification = importlib.util.spec_from_file_location(
        "voordelta_plot_results", plotter_path
    )
    if specification is None or specification.loader is None:
        raise ImportError(f"cannot load {plotter_path}")
    module = importlib.util.module_from_spec(specification)
    specification.loader.exec_module(module)
    return module.create_plots


def run_command(
    executable: Path,
    parallel: str,
    cores: int,
    mpi_launcher: str,
) -> tuple[list[str], dict[str, str]]:
    environment = os.environ.copy()
    if parallel == "openmp":
        environment["OMP_NUM_THREADS"] = str(cores)
        environment.setdefault("OMP_PLACES", "cores")
        environment.setdefault("OMP_PROC_BIND", "spread")
        command = [str(executable)]
    elif parallel == "mpi":
        launcher = shutil.which(mpi_launcher)
        if launcher is None:
            raise FileNotFoundError(f"MPI launcher not found: {mpi_launcher}")
        environment["OMP_NUM_THREADS"] = "1"
        command = [launcher, "-n", str(cores), str(executable)]
    else:
        environment["OMP_NUM_THREADS"] = "1"
        command = [str(executable)]
    return command, environment


def run(
    executable: Path,
    case_directory: Path,
    parallel: str,
    cores: int,
    mpi_launcher: str,
) -> None:
    results_directory = case_directory / "results"
    results_directory.mkdir(exist_ok=True)
    for name in GENERATED_FILES:
        path = case_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()
    for name in (*RESULT_NAMES.values(), *FIGURE_NAMES):
        path = results_directory / name
        if path.is_file() or path.is_symlink():
            path.unlink()

    input_file = case_directory / "INPUT"
    shutil.copyfile(case_directory / "voordelta_unstructured.swn", input_file)
    command, environment = run_command(executable, parallel, cores, mpi_launcher)
    active_cores = 1 if parallel == "serial" else cores
    print(
        f"Running ({parallel}, {active_cores} core{'s' if active_cores != 1 else ''}): "
        f"{' '.join(command)}"
    )
    started = time.monotonic()
    try:
        result = subprocess.run(
            command,
            cwd=case_directory,
            env=environment,
            check=False,
        )
    finally:
        input_file.unlink(missing_ok=True)
    elapsed = time.monotonic() - started

    for source_name, result_name in RESULT_NAMES.items():
        source = case_directory / source_name
        if source.exists():
            source.replace(results_directory / result_name)

    if result.returncode:
        raise RuntimeError(
            f"SWAN stopped with exit code {result.returncode}. Inspect "
            "results/voordelta_unstructured.prt and .erf."
        )
    if not (results_directory / "norm_end").is_file():
        raise RuntimeError(
            "SWAN did not create norm_end. Inspect "
            "results/voordelta_unstructured.prt and .erf."
        )

    try:
        figures = load_plotter(case_directory)(results_directory)
    except ModuleNotFoundError as error:
        print(
            f"Warning: plots were not created because {error.name} is missing. "
            "Install NumPy and Matplotlib, then run the regular Voordelta "
            "plot_results.py with this results directory."
        )
        figures = []

    print(f"Unstructured Voordelta completed normally in {elapsed:.2f} seconds.")
    print(f"Results: {results_directory}")
    for figure in figures:
        print(f"Created figure: {figure.name}")


def main() -> int:
    default_cores = min(4, os.cpu_count() or 1)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--parallel",
        choices=("openmp", "mpi", "serial"),
        default="openmp",
        help="execution mode (default: %(default)s)",
    )
    parser.add_argument(
        "--cores",
        type=int,
        default=default_cores,
        help="OpenMP threads or MPI processes (default: %(default)s)",
    )
    parser.add_argument(
        "--spacing",
        type=float,
        default=DEFAULT_SPACING,
        help="generated mesh spacing in metres (default: %(default)s)",
    )
    parser.add_argument(
        "--regenerate-mesh",
        action="store_true",
        help="replace existing Triangle mesh files",
    )
    parser.add_argument(
        "--mesh-only",
        action="store_true",
        help="generate the mesh without running SWAN",
    )
    parser.add_argument(
        "--mpi-launcher",
        default="mpiexec",
        help="MPI launcher command (default: %(default)s)",
    )
    parser.add_argument(
        "--swan-executable",
        help="path to swan.exe (default: matching build directory or PATH)",
    )
    arguments = parser.parse_args()
    if arguments.cores < 1:
        parser.error("--cores must be at least 1")

    case_directory = Path(__file__).resolve().parent
    try:
        vertices, elements = generate_mesh(
            case_directory,
            arguments.spacing,
            arguments.regenerate_mesh,
        )
        print(
            f"Mesh ready: {vertices:,} vertices, {elements:,} triangles, "
            f"spacing {arguments.spacing:g} m."
        )
        if not arguments.mesh_only:
            executable = find_executable(
                case_directory,
                arguments.swan_executable,
                arguments.parallel,
            )
            run(
                executable,
                case_directory,
                arguments.parallel,
                arguments.cores,
                arguments.mpi_launcher,
            )
    except (FileNotFoundError, ImportError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
