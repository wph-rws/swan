#!/usr/bin/env python3
"""Run and validate the SWAN nonlinear wave-interaction examples."""

from __future__ import annotations

import argparse
import math
import re
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
    "quad_dia1": Variant("quad", "quad_dia1", "semi-implicit DIA per sweep"),
    "quad_dia2": Variant("quad", "quad_dia2", "DIA per sweep"),
    "quad_dia3": Variant("quad", "quad_dia3", "DIA per iteration"),
    "quad_mdia": Variant("quad", "quad_mdia", "multiple DIA (MDIA)"),
    "quad_full": Variant("quad", "quad_full", "fully explicit, full circle"),
    "quad_xnl": Variant(
        "quad", "quad_xnl", "exact XNL reference, deep water", slow=True
    ),
    "quad_xnl52": Variant(
        "quad", "quad_xnl52", "exact XNL, WAM depth scaling", slow=True
    ),
    "quad_xnl53": Variant(
        "quad", "quad_xnl53", "exact XNL, finite depth", slow=True
    ),
    "triad_off": Variant("triad", "triad_off", "triads disabled"),
    "triad_dcta": Variant("triad", "triad_dcta", "DCTA triads"),
    "triad_lta": Variant("triad", "triad_lta", "LTA triads"),
    "triad_lta11": Variant(
        "triad", "triad_lta11", "pre-41.01 LTA triads (the operational choice)"
    ),
    "triad_ftim": Variant("triad", "triad_ftim", "FTIM triads"),
    "combined": Variant(
        "combined", "combined", "DIA and FTIM from ocean to coast"
    ),
    "src_all": Variant("sources", "src_all", "wind/wcap/breaking/friction on"),
    "src_nowind": Variant("sources", "src_nowind", "wind input disabled"),
    "src_nowcap": Variant("sources", "src_nowcap", "whitecapping disabled"),
    "src_nobreak": Variant("sources", "src_nobreak", "breaking disabled"),
    "src_nofric": Variant("sources", "src_nofric", "bottom friction disabled"),
    "src_fric_collins": Variant("sources", "src_fric_collins", "Collins friction"),
    "src_fric_madsen": Variant("sources", "src_fric_madsen", "Madsen friction"),
    "src_fric_jonvar": Variant("sources", "src_fric_jonvar", "JONSWAP friction, depth-varying"),
    "src_fric_ripples": Variant("sources", "src_fric_ripples", "Smith ripple friction"),
    "src_break_var": Variant("sources", "src_break_var", "Nelder variable-gamma breaking"),
    "src_break_rue": Variant("sources", "src_break_rue", "Ruessink breaking"),
    "src_break_tg": Variant("sources", "src_break_tg", "Thornton-Guza breaking"),
    "src_break_bkd": Variant("sources", "src_break_bkd", "beta-kd breaking"),
    "src_break_asym": Variant("sources", "src_break_asym", "asymmetry breaking"),
}

# Bottom-friction and depth-induced-breaking formulations that must each stay
# reachable and active.
FORMULATION_KEYS = (
    "src_fric_collins",
    "src_fric_madsen",
    "src_fric_jonvar",
    "src_fric_ripples",
    "src_break_var",
    "src_break_rue",
    "src_break_tg",
    "src_break_bkd",
    "src_break_asym",
)

# No formulation is quarantined at the moment. BREAKING ASYM was, until the
# discontinuity at a zero biphase was repaired in SINTGRL; see
# doc/bugfixes-tov-tu-delft-41.51.md. Keep the mechanism: a formulation that
# is reachable but known broken belongs here rather than silently asserted.
BROKEN_KEYS: tuple[str, ...] = ()

# Named groups rather than positional slices: a slice silently selects the
# wrong variants as soon as a group grows, which is how quad_mdia went missing
# from its own group on the first attempt.
QUAD_KEYS = (
    "quad_off", "quad_dia1", "quad_dia2", "quad_dia3", "quad_mdia", "quad_full",
)
# The XNL suite takes about two minutes per deck, so it stays opt-in.
XNL_KEYS = ("quad_xnl", "quad_xnl52", "quad_xnl53")
TRIAD_KEYS = ("triad_off", "triad_dcta", "triad_ftim", "triad_lta", "triad_lta11")
COMBINED_KEYS = ("combined",)
SOURCE_TOGGLE_KEYS = (
    "src_all",
    "src_nowind",
    "src_nowcap",
    "src_nobreak",
    "src_nofric",
)

STANDARD_KEYS = (
    *QUAD_KEYS,
    *TRIAD_KEYS,
    *COMBINED_KEYS,
    *SOURCE_TOGGLE_KEYS,
    *FORMULATION_KEYS,
    *BROKEN_KEYS,
)

# TABLE columns of the sources group: XP DEPTH HSIGN TM01 GENW DISW DISSU DISB.
SOURCE_COLUMNS = {"wind": 4, "wcap": 5, "breaking": 6, "friction": 7}
SOURCE_VARIANTS = {
    "src_nowind": "wind",
    "src_nowcap": "wcap",
    "src_nobreak": "breaking",
    "src_nofric": "friction",
}


@dataclass
class RunResult:
    variant: Variant
    table: list[list[float]]
    frequencies: list[float]
    locations: list[tuple[float, float]]
    spectra: list[list[float]]
    accuracy: float | None
    required_accuracy: float | None

    @property
    def converged(self) -> bool:
        """Whether SWAN's own stopping criterion was met.

        A run that stops on the iteration cap still writes norm_end and exits
        zero, so nothing downstream notices unless this is read explicitly.
        """
        if self.accuracy is None or self.required_accuracy is None:
            return True
        return self.accuracy >= self.required_accuracy


ACCURACY_PATTERN = re.compile(
    r"accuracy OK in\s+([0-9.]+)\s*% of wet grid points\s*\(\s*([0-9.]+)\s*% required"
)


def parse_accuracy(print_file: Path) -> tuple[float | None, float | None]:
    """Last reported accuracy and the accuracy SWAN required, from PRINT."""
    matches = ACCURACY_PATTERN.findall(
        print_file.read_text(encoding="utf-8", errors="replace")
    )
    if not matches:
        return None, None
    reached, required = matches[-1]
    return float(reached), float(required)


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
    groups = {
        "standard": STANDARD_KEYS,
        "all": (*QUAD_KEYS, *XNL_KEYS, *STANDARD_KEYS[len(QUAD_KEYS):]),
        "quad": QUAD_KEYS,
        "triad": TRIAD_KEYS,
        "combined": COMBINED_KEYS,
        "sources": SOURCE_TOGGLE_KEYS,
        "formulations": FORMULATION_KEYS + BROKEN_KEYS,
        "xnl": XNL_KEYS,
    }
    if selection not in groups:
        raise ValueError(f"unknown selection: {selection}")
    return groups[selection]


def clean_result_directory(directory: Path) -> None:
    directory.mkdir(parents=True, exist_ok=True)
    for path in directory.iterdir():
        if path.is_file() or path.is_symlink():
            path.unlink()
        else:
            raise RuntimeError(f"unexpected directory in generated results: {path}")


def parse_table(path: Path, columns: int = 7) -> list[list[float]]:
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        values = [float(value) for value in stripped.split()]
        if len(values) != columns:
            raise ValueError(f"{path} has a row with {len(values)} columns; expected {columns}")
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

    table = parse_table(table_file, 8 if variant.directory == "sources" else 7)
    frequencies, locations, spectra = parse_spectrum(spectrum_file)
    if not any(value > 0.0 for spectrum in spectra for value in spectrum):
        raise RuntimeError(f"{key} produced only empty spectra")

    accuracy, required = parse_accuracy(print_file)

    qualifier = " (slow reference)" if variant.slow else ""
    if accuracy is not None and required is not None and accuracy < required:
        qualifier += (
            f" -- DID NOT CONVERGE: {accuracy:.2f}% of wet points,"
            f" {required:.2f}% required"
        )
    print(f"{key}: completed in {elapsed:.2f} s{qualifier}")
    return RunResult(
        variant, table, frequencies, locations, spectra, accuracy, required
    )


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
        if key in BROKEN_KEYS:
            # Reaching this point already proves the path ran to norm_end
            # without a runtime error, which is all this deck is asked to show.
            continue
        if key in {"quad_dia1", "quad_dia2", "quad_dia3", "quad_mdia",
                   "quad_full", *XNL_KEYS, "combined"}:
            if max(quad_sources) <= 1.0e-8:
                raise RuntimeError(f"{key} did not activate quadruplet transfer")
        if key in {"triad_dcta", "triad_ftim", "triad_lta", "triad_lta11", "combined"}:
            if max(triad_sources) <= 1.0e-8:
                raise RuntimeError(f"{key} did not activate triad transfer")

    if {"quad_off", "quad_dia1", "quad_dia2", "quad_dia3"}.issubset(results):
        off = results["quad_off"].spectra[-1]
        dia1 = results["quad_dia1"].spectra[-1]
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
        # DIA1 is kept out of that sentence on purpose. QUADRUPLET 1 does not
        # converge on this deck -- it settles into a limit cycle rather than a
        # solution -- so a spectral difference against the converged DIA2 would
        # mostly measure where DIA1 happened to stop, not how the formulation
        # differs. Report it only as what it is.
        if not results["quad_dia1"].converged:
            messages.append(
                f"QUAD: DIA1 did not converge "
                f"({results['quad_dia1'].accuracy:.2f}% of wet points, "
                f"{results['quad_dia1'].required_accuracy:.2f}% required); its "
                f"spectrum is not comparable with the converged runs."
            )
        else:
            dia1_difference = relative_spectral_difference(dia2, dia1)
            messages.append(
                f"QUAD: DIA1 differs {dia1_difference:.1%} from DIA2."
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

    if {"triad_lta", "triad_lta11"}.issubset(results):
        # The two LTA generations must not collapse onto each other. ITRIAD=11
        # drops the shoaling factor and reads the group velocity locally, so a
        # measurable difference here is what keeps the pair honest: if it ever
        # vanishes, one of the two spellings has stopped selecting its own
        # formulation and the coverage claim is empty again.
        lta = results["triad_lta"].spectra[-2]
        lta11 = results["triad_lta11"].spectra[-2]
        lta_difference = relative_spectral_difference(lta, lta11)
        if lta_difference <= 0.01:
            raise RuntimeError(
                "TRIAD LTA and TRIAD ITRIAD=11 produced the same spectrum; "
                "one of the two is no longer selecting its own formulation"
            )
        messages.append(
            f"TRIAD: ITRIAD=11 differs {lta_difference:.1%} from LTA at x=25 m."
        )

    if {"src_all", *SOURCE_VARIANTS}.issubset(results):
        # Wind, whitecapping, breaking and bottom friction each report their
        # own source column in the gauges table. Every OFF toggle must zero
        # exactly its column (input validation forbids bare IWIND=0 with
        # quadruplets, so src_nowind also disables QUAD — quad coverage lives
        # with the quad variants above) and move the spectrum measurably.
        for key, process in SOURCE_VARIANTS.items():
            column = SOURCE_COLUMNS[process]
            if max(abs(row[column]) for row in results[key].table) > 1.0e-12:
                raise RuntimeError(f"{key} reports a source term that should be disabled")
        active = [
            max(abs(row[SOURCE_COLUMNS[process]]) for row in results["src_all"].table)
            for process in ("wind", "wcap", "breaking", "friction")
        ]
        if min(active) <= 1.0e-6:
            raise RuntimeError("src_all did not activate every source term")
        full = results["src_all"].spectra[3]
        for key in SOURCE_VARIANTS:
            difference = relative_spectral_difference(
                full, results[key].spectra[3])
            if difference <= 0.01:
                raise RuntimeError(f"{key} did not measurably change the x=45 km spectrum")
            messages.append(f"SOURCE: {key} differs {difference:.1%} from ALL at x=45 km.")

    if {"src_all", *FORMULATION_KEYS}.issubset(results):
        # Each formulation must stay reachable, keep its own process active,
        # and differ from the JONSWAP/CONSTANT pair that src_all uses. A
        # formulation that silently degenerates to the default would otherwise
        # pass unnoticed, which is how QUADRUPLET 1 stayed broken.
        reference = results["src_all"]
        for key in FORMULATION_KEYS:
            process = "friction" if "_fric_" in key else "breaking"
            column = SOURCE_COLUMNS[process]
            rows = results[key].table
            if max(abs(row[column]) for row in rows) <= 1.0e-8:
                raise RuntimeError(f"{key} reports no {process} dissipation")
            if not all(row[2] > 0.0 for row in rows[1:]):
                raise RuntimeError(f"{key} produced a vanishing wave height")
            difference = relative_spectral_difference(
                reference.spectra[3], results[key].spectra[3])
            if difference <= 0.001:
                raise RuntimeError(
                    f"{key} is indistinguishable from the src_all formulation")
        messages.append(
            f"FORMULATIONS: {len(FORMULATION_KEYS)} friction/breaking "
            f"formulations active and distinct from src_all."
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
        choices=("standard", "all", "quad", "triad", "combined", "sources",
                 "formulations", "xnl"),
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
