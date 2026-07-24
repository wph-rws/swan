#!/usr/bin/env python3
"""Run and validate the SWAN nonlinear wave-interaction examples."""

from __future__ import annotations

import argparse
import math
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Variant:
    directory: str
    basename: str
    description: str
    slow: bool = False


VARIANTS = {
    "quad_off": Variant("quad", "quad_off", "quadruplets disabled"),
    "quad_dia2": Variant("quad", "quad_dia2", "DIA per sweep"),
    "quad_dia3": Variant("quad", "quad_dia3", "DIA per iteration"),
    "quad_xnl": Variant(
        "quad", "quad_xnl", "exact XNL reference", slow=True
    ),
    "triad_off": Variant("triad", "triad_off", "triads disabled"),
    "triad_dcta": Variant("triad", "triad_dcta", "DCTA triads"),
    "triad_ftim": Variant("triad", "triad_ftim", "FTIM triads"),
    "combined": Variant(
        "combined", "combined", "DIA and FTIM from ocean to coast"
    ),
}

STANDARD_KEYS = (
    "quad_off",
    "quad_dia2",
    "quad_dia3",
    "triad_off",
    "triad_dcta",
    "triad_ftim",
    "combined",
)


@dataclass
class RunResult:
    variant: Variant
    table: list[list[float]]
    frequencies: list[float]
    locations: list[tuple[float, float]]
    spectra: list[list[float]]


def find_executable(example_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")

    repository = example_directory.parent.parent
    local_build = repository / "build" / "bin" / "swan.exe"
    if local_build.is_file():
        return local_build.resolve()

    installed = shutil.which("swan.exe")
    if installed:
        return Path(installed).resolve()
    raise FileNotFoundError(
        "SWAN executable not found. Build the repository first or pass "
        "--swan-executable /path/to/swan.exe."
    )


def select_variants(selection: str) -> tuple[str, ...]:
    if selection == "standard":
        return STANDARD_KEYS
    if selection == "all":
        return (*STANDARD_KEYS[:3], "quad_xnl", *STANDARD_KEYS[3:])
    if selection == "quad":
        return STANDARD_KEYS[:3]
    if selection == "triad":
        return STANDARD_KEYS[3:6]
    if selection == "combined":
        return ("combined",)
    if selection == "xnl":
        return ("quad_xnl",)
    raise ValueError(f"unknown selection: {selection}")


def clean_result_directory(directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    for path in directory.iterdir():
        if path.is_file() or path.is_symlink():
            path.unlink()
        else:
            raise RuntimeError(f"unexpected directory in generated results: {path}")


def parse_table(path: Path) -> list[list[float]]:
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        values = [float(value) for value in stripped.split()]
        if len(values) != 7:
            raise ValueError(f"{path} has a row with {len(values)} columns; expected 7")
        if not all(math.isfinite(value) for value in values):
            raise ValueError(f"{path} contains a non-finite table value")
        rows.append(values)
    if not rows:
        raise ValueError(f"{path} contains no table rows")
    return rows


def find_keyword(lines: list[str], keyword: str) -> int:
    for index, line in enumerate(lines):
        if line.strip().split(maxsplit=1)[0:1] == [keyword]:
            return index
    raise ValueError(f"spectrum does not contain {keyword}")


def parse_spectrum(
    path: Path,
) -> tuple[list[float], list[tuple[float, float]], list[list[float]]]:
    lines = path.read_text(encoding="utf-8").splitlines()

    location_index = find_keyword(lines, "LOCATIONS")
    location_count = int(lines[location_index + 1].split()[0])
    locations = []
    for line in lines[location_index + 2 : location_index + 2 + location_count]:
        x, y = (float(value) for value in line.split()[:2])
        locations.append((x, y))

    frequency_index = find_keyword(lines, "AFREQ")
    frequency_count = int(lines[frequency_index + 1].split()[0])
    frequencies = [
        float(line.split()[0])
        for line in lines[
            frequency_index + 2 : frequency_index + 2 + frequency_count
        ]
    ]

    spectra = []
    index = 0
    while index < len(lines):
        if lines[index].strip().split(maxsplit=1)[0:1] == ["LOCATION"]:
            values = []
            for line in lines[index + 1 : index + 1 + frequency_count]:
                value = float(line.split()[0])
                if value <= -90.0:
                    value = 0.0
                elif value < 0.0 or not math.isfinite(value):
                    raise ValueError(f"{path} contains invalid variance density {value}")
                values.append(value)
            spectra.append(values)
            index += frequency_count
        index += 1

    if len(spectra) != location_count:
        raise ValueError(
            f"{path} contains {len(spectra)} spectra; expected {location_count}"
        )
    return frequencies, locations, spectra


def preserve_outputs(work_directory: Path, result_directory: Path, basename: str) -> None:
    excluded = {"INPUT", "bottom.bot"}
    for source in work_directory.iterdir():
        if not source.is_file() or source.name in excluded:
            continue
        if source.suffix.lower() == ".bqf":
            continue
        destination_name = source.name
        if source.name == "PRINT":
            destination_name = f"{basename}.prt"
        elif source.name == "Errfile":
            destination_name = f"{basename}.erf"
        shutil.copy2(source, result_directory / destination_name)


def run_variant(
    executable: Path,
    example_directory: Path,
    results_root: Path,
    key: str,
) -> RunResult:
    variant = VARIANTS[key]
    source_directory = example_directory / variant.directory
    result_directory = results_root / key
    clean_result_directory(result_directory)

    with tempfile.TemporaryDirectory(prefix=f"swan-{key}-") as temporary:
        work_directory = Path(temporary)
        shutil.copy2(source_directory / f"{variant.basename}.swn", work_directory / "INPUT")
        shutil.copy2(source_directory / "bottom.bot", work_directory / "bottom.bot")

        started = time.monotonic()
        process = subprocess.run(
            [executable],
            cwd=work_directory,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
        elapsed = time.monotonic() - started
        (result_directory / "console.log").write_text(
            process.stdout, encoding="utf-8"
        )
        preserve_outputs(work_directory, result_directory, variant.basename)

    print_file = result_directory / f"{variant.basename}.prt"
    error_file = result_directory / f"{variant.basename}.erf"
    table_file = result_directory / f"{variant.basename}.tbl"
    spectrum_file = result_directory / f"{variant.basename}.spc"
    source_file = result_directory / f"{variant.basename}_sources.spc"

    if process.returncode:
        raise RuntimeError(
            f"{key} stopped with exit code {process.returncode}; inspect {print_file}"
        )
    if error_file.is_file() and error_file.stat().st_size:
        raise RuntimeError(f"{key} created a non-empty error file: {error_file}")
    for required in (print_file, table_file, spectrum_file, source_file):
        if not required.is_file() or required.stat().st_size == 0:
            raise RuntimeError(f"{key} did not create {required.name}")
    if not (result_directory / "norm_end").is_file():
        raise RuntimeError(f"{key} did not complete normally")

    table = parse_table(table_file)
    frequencies, locations, spectra = parse_spectrum(spectrum_file)
    if not any(value > 0.0 for spectrum in spectra for value in spectrum):
        raise RuntimeError(f"{key} produced only empty spectra")

    qualifier = " (slow reference)" if variant.slow else ""
    print(f"{key}: completed in {elapsed:.2f} s{qualifier}")
    return RunResult(variant, table, frequencies, locations, spectra)


def relative_spectral_difference(first: list[float], second: list[float]) -> float:
    scale = sum(abs(value) for value in first)
    if scale == 0.0:
        raise ValueError("cannot compare against an empty spectrum")
    return sum(abs(a - b) for a, b in zip(first, second, strict=True)) / scale


def validate_results(results: dict[str, RunResult]) -> list[str]:
    messages = []
    for key, result in results.items():
        triad_sources = [abs(row[5]) for row in result.table]
        quad_sources = [abs(row[6]) for row in result.table]

        if key in {"quad_off", "triad_off"}:
            source_values = quad_sources if key == "quad_off" else triad_sources
            if max(source_values) > 1.0e-12:
                raise RuntimeError(f"{key} reports a source term that should be disabled")
        if key in {"quad_dia2", "quad_dia3", "quad_xnl", "combined"}:
            if max(quad_sources) <= 1.0e-8:
                raise RuntimeError(f"{key} did not activate quadruplet transfer")
        if key in {"triad_dcta", "triad_ftim", "combined"}:
            if max(triad_sources) <= 1.0e-8:
                raise RuntimeError(f"{key} did not activate triad transfer")

    if {"quad_off", "quad_dia2", "quad_dia3"}.issubset(results):
        off = results["quad_off"].spectra[-1]
        dia2 = results["quad_dia2"].spectra[-1]
        dia3 = results["quad_dia3"].spectra[-1]
        off_difference = relative_spectral_difference(off, dia2)
        dia_difference = relative_spectral_difference(dia2, dia3)
        if off_difference <= 0.01:
            raise RuntimeError("DIA did not measurably change the down-fetch spectrum")
        messages.append(
            f"QUAD: DIA2 differs {off_difference:.1%} from OFF at 100 km; "
            f"DIA3 differs {dia_difference:.1%} from DIA2."
        )

    if {"triad_off", "triad_dcta", "triad_ftim"}.issubset(results):
        off = results["triad_off"].spectra[-2]
        dcta = results["triad_dcta"].spectra[-2]
        ftim = results["triad_ftim"].spectra[-2]
        dcta_difference = relative_spectral_difference(off, dcta)
        ftim_difference = relative_spectral_difference(off, ftim)
        if min(dcta_difference, ftim_difference) <= 0.01:
            raise RuntimeError("triads did not measurably change the lee-side spectrum")
        messages.append(
            f"TRIAD: DCTA differs {dcta_difference:.1%} and FTIM differs "
            f"{ftim_difference:.1%} from OFF at x=25 m."
        )

    messages.append(f"Validated {len(results)} runs and their spectra/source terms.")
    return messages


def create_plots(results: dict[str, RunResult], results_root: Path) -> list[Path]:
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ModuleNotFoundError as error:
        print(
            f"Warning: plots were not created because {error.name} is missing. "
            "Install Matplotlib and rerun."
        )
        return []

    figures = []
    quad_keys = [key for key in ("quad_off", "quad_dia2", "quad_dia3", "quad_xnl") if key in results]
    if quad_keys:
        figure, axes = plt.subplots(1, 2, figsize=(12, 4.8), constrained_layout=True)
        for key in quad_keys:
            result = results[key]
            axes[0].loglog(
                result.frequencies,
                [max(value, 1.0e-10) for value in result.spectra[-1]],
                label=result.variant.description,
            )
            axes[1].plot(
                [row[0] / 1000.0 for row in result.table],
                [row[2] for row in result.table],
                marker="o",
                label=result.variant.description,
            )
        axes[0].set(xlabel="Frequency [Hz]", ylabel="Variance density [m²/Hz]", title="Spectrum at 100 km")
        axes[1].set(xlabel="Fetch [km]", ylabel="Significant wave height [m]", title="Deep-water wind-wave growth")
        for axis in axes:
            axis.grid(True, which="both", alpha=0.3)
            axis.legend()
        destination = results_root / "quad_comparison.png"
        figure.savefig(destination, dpi=170)
        plt.close(figure)
        figures.append(destination)

    triad_keys = [key for key in ("triad_off", "triad_dcta", "triad_ftim") if key in results]
    if triad_keys:
        figure, axes = plt.subplots(1, 2, figsize=(12, 4.8), constrained_layout=True)
        for key in triad_keys:
            result = results[key]
            axes[0].semilogy(
                result.frequencies,
                [max(value, 1.0e-10) for value in result.spectra[-2]],
                label=result.variant.description,
            )
            axes[1].plot(
                [row[0] for row in result.table],
                [row[3] for row in result.table],
                marker="o",
                label=result.variant.description,
            )
        axes[0].set(xlabel="Frequency [Hz]", ylabel="Variance density [m²/Hz]", title="Lee-side spectrum at x=25 m")
        axes[1].set(xlabel="Distance [m]", ylabel="Mean period Tm01 [s]", title="Transfer across the submerged bar")
        for axis in axes:
            axis.grid(True, which="both", alpha=0.3)
            axis.legend()
        destination = results_root / "triad_comparison.png"
        figure.savefig(destination, dpi=170)
        plt.close(figure)
        figures.append(destination)

    if "combined" in results:
        result = results["combined"]
        figure, axes = plt.subplots(1, 2, figsize=(12, 4.8), constrained_layout=True)
        for location, spectrum in zip(result.locations, result.spectra, strict=True):
            axes[0].loglog(
                result.frequencies,
                [max(value, 1.0e-10) for value in spectrum],
                label=f"x={location[0] / 1000.0:g} km",
            )
        axes[0].set(xlabel="Frequency [Hz]", ylabel="Variance density [m²/Hz]", title="Ocean-to-nearshore spectral evolution")
        axes[0].grid(True, which="both", alpha=0.3)
        axes[0].legend(ncol=2)

        distances = [row[0] / 1000.0 for row in result.table]
        axes[1].plot(distances, [row[2] for row in result.table], "o-", color="tab:blue", label="Hs")
        axes[1].set(xlabel="Distance [km]", ylabel="Significant wave height [m]", title="Combined DIA + FTIM")
        depth_axis = axes[1].twinx()
        depth_axis.plot(distances, [row[1] for row in result.table], "s--", color="tab:brown", label="Depth")
        depth_axis.set_ylabel("Water depth [m]")
        axes[1].grid(True, alpha=0.3)
        axes[1].legend(loc="upper left")
        depth_axis.legend(loc="upper right")
        destination = results_root / "combined_evolution.png"
        figure.savefig(destination, dpi=170)
        plt.close(figure)
        figures.append(destination)

    return figures


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--case",
        choices=("standard", "all", "quad", "triad", "combined", "xnl"),
        default="standard",
        help=(
            "case group to run; standard excludes the roughly two-minute XNL "
            "reference (default: %(default)s)"
        ),
    )
    parser.add_argument(
        "--swan-executable",
        help="path to swan.exe (default: build/bin/swan.exe or PATH)",
    )
    parser.add_argument(
        "--results-directory",
        help="write generated results here instead of examples/nonlinear_interactions/results",
    )
    parser.add_argument("--no-plots", action="store_true", help="skip Matplotlib plots")
    arguments = parser.parse_args()

    example_directory = Path(__file__).resolve().parent
    results_root = (
        Path(arguments.results_directory).expanduser().resolve()
        if arguments.results_directory
        else example_directory / "results"
    )
    results_root.mkdir(parents=True, exist_ok=True)

    try:
        executable = find_executable(example_directory, arguments.swan_executable)
        selected = select_variants(arguments.case)
        results = {}
        for key in selected:
            result = run_variant(executable, example_directory, results_root, key)
            results[key] = result

        messages = validate_results(results)
        figures = [] if arguments.no_plots else create_plots(results, results_root)
        summary = "\n".join(messages) + "\n"
        (results_root / "validation.txt").write_text(summary, encoding="utf-8")
        for message in messages:
            print(message)
        for figure in figures:
            print(f"Created figure: {figure.name}")
        print(f"Results: {results_root}")
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
