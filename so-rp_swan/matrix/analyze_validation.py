#!/usr/bin/env python3
"""Summarize and gate the complete four-target so-rp validation matrix."""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path

import numpy as np

HERE = Path(__file__).resolve().parent
COMPARISON = HERE.parent / "comparison"
sys.path.insert(0, str(COMPARISON))

from result_stats import (
    DifferenceStats,
    compare,
    compare_circular,
    compare_spectra,
    read_convergence_history,
    read_dir_points,
    read_hsig_field,
    read_hsig_points,
    read_spectra,
    read_tm01_points,
    report_spectral_difference,
)


TARGETS = (
    "premodern_4151",
    "bss_4131",
    "current_4151_default",
    "current_4151_legacy",
)


@dataclass(frozen=True)
class RunResult:
    directory: Path
    metadata: dict[str, object]


def load_run(root: Path, target: str, condition: str) -> RunResult:
    directory = root / target / condition / "replicate-001"
    metadata_path = directory / "run.json"
    if not metadata_path.is_file():
        raise FileNotFoundError(f"missing matrix metadata: {metadata_path}")
    metadata = json.loads(metadata_path.read_text())
    if metadata["returncode"] != 0 or metadata["error"] is not None:
        raise ValueError(f"{metadata_path}: run did not finish successfully")
    if metadata["condition"]["condition_id"] != condition:
        raise ValueError(f"{metadata_path}: condition metadata mismatch")
    if metadata["target"]["id"] != target:
        raise ValueError(f"{metadata_path}: target metadata mismatch")
    return RunResult(directory, metadata)


def zero(stats: DifferenceStats) -> bool:
    return stats.bias == 0.0 and stats.rms == 0.0 and stats.maximum_absolute == 0.0


def same_convergence(first: np.ndarray, second: np.ndarray) -> bool:
    return len(first) == len(second) and bool(np.array_equal(first, second))


def analyze(root: Path, manifest_path: Path) -> str:
    manifest = json.loads(manifest_path.read_text())
    conditions = [item["id"] for item in manifest["conditions"]]
    lines = [
        "| Condition | Wet/dry points | Current default mean Hsig (m) | "
        "Default max abs vs premodern (m) | BSS mean Hsig (m) | "
        "Current legacy mean Hsig (m) | Legacy bias vs BSS (m) | "
        "Legacy RMS (m) | Legacy max abs (m) |",
        "|---|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    current_hashes: set[str] = set()
    exact_default_conditions = 0
    spectral_lines = [
        "",
        "## Spectra",
        "",
        "1D-spectra (VaDens/NDIR/DSPRDEGR per frequentie per locatie) uit "
        ".sp1 en .sp2; droge punten dragen NODATA. De default-vs-premodern "
        "poort eist bitgelijkheid op beide bestanden; legacy-vs-BSS alleen "
        "rapportage (NDIR circulair; maxima inclusief energiearme cellen, "
        "aantallen erbij).",
        "",
        "| Condition | Default spectral max abs sp1/sp2 | "
        "Legacy VaDens max abs vs BSS | Legacy NDIR max abs vs BSS (deg) | "
        "Legacy mask differences |",
        "|---|---:|---:|---:|---:|",
    ]
    extended_lines = [
        "",
        "## Extended quantities",
        "",
        "Tm01 and direction use the header-named table columns (not positions); "
        "direction is compared circularly excluding points below 0.05 m Hsig in "
        "both runs; convergence is the per-iteration PRINT accuracy series. "
        "The default-vs-premodern gate demands bit-equality here too; the "
        "legacy-vs-BSS columns only report (tolerances.json: report_only).",
        "",
        "| Condition | Default Tm01 max abs vs premodern (s) | "
        "Default Dir max abs vs premodern (deg) | Convergence iters "
        "premodern/current | Legacy Tm01 max abs vs BSS (s) | "
        "Legacy Dir max abs vs BSS (deg) |",
        "|---|---:|---:|---:|---:|---:|",
    ]

    for condition in conditions:
        runs = {target: load_run(root, target, condition) for target in TARGETS}
        current_hashes.update(
            str(runs[target].metadata["executable_sha256"])
            for target in ("current_4151_default", "current_4151_legacy")
        )

        pre_field = read_hsig_field(
            runs["premodern_4151"].directory / "scaloost_rp.mat"
        )
        current_field = read_hsig_field(
            runs["current_4151_default"].directory / "scaloost_rp.mat"
        )
        pre_points = read_hsig_points(
            runs["premodern_4151"].directory / "uitvoerpunten.tab"
        )
        current_points = read_hsig_points(
            runs["current_4151_default"].directory / "uitvoerpunten.tab"
        )
        default_field_delta = compare(pre_field, current_field)
        default_point_delta = compare(pre_points, current_points)
        if not zero(default_field_delta) or not zero(default_point_delta):
            raise ValueError(
                f"{condition}: current default differs from premodern 41.51"
            )
        # Dezelfde bitgelijkheidseis voor perioden, richtingen en
        # convergentiegeschiedenis; maskers eerst, EXCV nooit mee, richting
        # circulair en bij verwaarloosbare energie apart.
        pre_tm01 = read_tm01_points(
            runs["premodern_4151"].directory / "uitvoerpunten.tab"
        )
        current_tm01 = read_tm01_points(
            runs["current_4151_default"].directory / "uitvoerpunten.tab"
        )
        default_tm01_delta = compare(pre_tm01, current_tm01)
        pre_dir = read_dir_points(
            runs["premodern_4151"].directory / "uitvoerpunten.tab"
        )
        current_dir = read_dir_points(
            runs["current_4151_default"].directory / "uitvoerpunten.tab"
        )
        default_dir_delta = compare_circular(
            pre_dir, current_dir, pre_points, current_points
        )
        pre_conv = read_convergence_history(
            runs["premodern_4151"].directory / "PRINT"
        )
        current_conv = read_convergence_history(
            runs["current_4151_default"].directory / "PRINT"
        )
        if (
            not zero(default_tm01_delta)
            or not zero(default_dir_delta)
            or not same_convergence(pre_conv, current_conv)
        ):
            raise ValueError(
                f"{condition}: current default differs from premodern 41.51 "
                "in Tm01, direction or convergence history"
            )
        exact_default_conditions += 1

        bss_field = read_hsig_field(runs["bss_4131"].directory / "scaloost_rp.mat")
        legacy_field = read_hsig_field(
            runs["current_4151_legacy"].directory / "scaloost_rp.mat"
        )
        bss_points = read_hsig_points(
            runs["bss_4131"].directory / "uitvoerpunten.tab"
        )
        legacy_points = read_hsig_points(
            runs["current_4151_legacy"].directory / "uitvoerpunten.tab"
        )
        # Validate the field mask too; the formal summary is deliberately over
        # the requested wet points, not over the domain.
        compare(bss_field, legacy_field)
        legacy_delta = compare(bss_points, legacy_points)
        bss_tm01 = read_tm01_points(runs["bss_4131"].directory / "uitvoerpunten.tab")
        legacy_tm01 = read_tm01_points(
            runs["current_4151_legacy"].directory / "uitvoerpunten.tab"
        )
        legacy_tm01_delta = compare(bss_tm01, legacy_tm01)
        bss_dir = read_dir_points(runs["bss_4131"].directory / "uitvoerpunten.tab")
        legacy_dir = read_dir_points(
            runs["current_4151_legacy"].directory / "uitvoerpunten.tab"
        )
        legacy_dir_delta = compare_circular(
            bss_dir, legacy_dir, bss_points, legacy_points
        )
        # Spectra: de default-poort eist bitgelijkheid op .sp1 én .sp2;
        # legacy alleen rapportage (maskerverschillen geteld, niet gemiddeld).
        default_spectral_max = 0.0
        for spectrum_file in ("uitvoerpunten.sp1", "uitvoerpunten.sp2"):
            pre_spectra = read_spectra(
                runs["premodern_4151"].directory / spectrum_file
            )
            current_spectra = read_spectra(
                runs["current_4151_default"].directory / spectrum_file
            )
            for stats in compare_spectra(
                    pre_spectra, current_spectra).values():
                if stats.maximum_absolute != 0.0:
                    raise ValueError(
                        f"{condition}: current default spectra differ from "
                        f"premodern 41.51 in {spectrum_file}")
                default_spectral_max = max(
                    default_spectral_max, stats.maximum_absolute)
        legacy_spectra = read_spectra(
            runs["current_4151_legacy"].directory / "uitvoerpunten.sp1")
        bss_spectra = read_spectra(
            runs["bss_4131"].directory / "uitvoerpunten.sp1")
        legacy_spectral, spectral_masks = report_spectral_difference(
            bss_spectra, legacy_spectra)
        spectral_lines.append(
            f"| `{condition}` | {default_spectral_max:.6f} | "
            f"{legacy_spectral['VaDens'].maximum_absolute:.6f} | "
            f"{legacy_spectral['NDIR'].maximum_absolute:.6f} | "
            f"{spectral_masks} |"
        )
        extended_lines.append(
            f"| `{condition}` | {default_tm01_delta.maximum_absolute:.6f} | "
            f"{default_dir_delta.maximum_absolute:.6f} | "
            f"{len(pre_conv)}/{len(current_conv)} | "
            f"{legacy_tm01_delta.maximum_absolute:.6f} | "
            f"{legacy_dir_delta.maximum_absolute:.6f} |"
        )

        lines.append(
            f"| `{condition}` | {legacy_points.wet_count}/{legacy_points.dry_count} | "
            f"{current_points.mean:.6f} | "
            f"{default_point_delta.maximum_absolute:.6f} | "
            f"{bss_points.mean:.6f} | {legacy_points.mean:.6f} | "
            f"{legacy_delta.bias:+.6f} | {legacy_delta.rms:.6f} | "
            f"{legacy_delta.maximum_absolute:.6f} |"
        )

    if len(current_hashes) != 1:
        raise ValueError(
            "current default and legacy entries do not use one executable: "
            + ", ".join(sorted(current_hashes))
        )

    lines.extend(
        (
            "",
            f"Default-equivalence gate: **{exact_default_conditions}/"
            f"{len(conditions)} conditions exactly equal** in both the wet "
            "field and requested wet points.",
            "",
            f"Current executable SHA-256: `{next(iter(current_hashes))}`.",
        )
    )
    lines.extend(extended_lines)
    lines.append(
        "",
    )
    lines.append(
        "Extended gate: default-vs-premodern Tm01, direction and convergence "
        f"history are bit-equal in all {exact_default_conditions} conditions "
        "(failure raises before this line is reached)."
    )
    lines.extend(spectral_lines)
    lines.append(
        "",
    )
    lines.append(
        "Spectral gate: default-vs-premodern spectra are bit-equal in .sp1 "
        f"and .sp2 in all {exact_default_conditions} conditions (failure "
        "raises before this line is reached)."
    )
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("runs_root", type=Path)
    parser.add_argument(
        "--manifest", type=Path, default=HERE / "conditions.json"
    )
    args = parser.parse_args()
    try:
        print(analyze(args.runs_root, args.manifest))
    except (FileNotFoundError, KeyError, TypeError, ValueError) as error:
        parser.error(str(error))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
