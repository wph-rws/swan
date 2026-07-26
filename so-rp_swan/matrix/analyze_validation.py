#!/usr/bin/env python3
"""Summarize and gate the complete four-target so-rp validation matrix."""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path

HERE = Path(__file__).resolve().parent
COMPARISON = HERE.parent / "comparison"
sys.path.insert(0, str(COMPARISON))

from result_stats import DifferenceStats, compare, read_hsig_field, read_hsig_points


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
