#!/usr/bin/env python3
"""Compare controlled SWAN runs with explicit wet/dry masks and metadata."""

from __future__ import annotations

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

from result_stats import compare, read_hsig_field, read_hsig_points

ROOT = Path(__file__).resolve().parents[1]
RUNS = ROOT / "runs"


@dataclass(frozen=True)
class Run:
    name: str
    directory: str
    physics: str
    condition: str = "u20_d310_l+300_open"


CASES = [
    Run("BSS 41.31A.1", "bss4131_converged", "implicit 41.31 defaults"),
    Run("upstream 41.31", "upstream4131_converged", "implicit 41.31 defaults"),
    Run("41.51 default", "current_converged", "implicit 41.51 defaults"),
    Run("41.51 + Komen", "current_converged_gen3_komen", "GEN3 KOMEN"),
    Run(
        "41.51 + Komen + old triad",
        "current_converged_gen3_komen_triad11",
        "GEN3 KOMEN; ITRIAD=11",
    ),
    Run(
        "41.51 + Komen + FIT",
        "current_converged_gen3_komen_drag_fit",
        "GEN3 KOMEN DRAG FIT",
    ),
    Run(
        "41.51 explicit 41.31 defaults",
        "current_converged_legacy_defaults",
        "GEN3 KOMEN DRAG FIT; ITRIAD=11; URCRIT=0.2",
    ),
]


def convergence(run_directory: Path) -> tuple[int, float]:
    path = run_directory / "PRINT"
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN PRINT file: {path}")
    text = path.read_text(errors="replace")
    values = re.findall(r"accuracy OK in\s+([0-9.]+)", text)
    if not values:
        raise ValueError(f"{path}: no convergence records")
    return len(values), float(values[-1])


def analyze(runs_root: Path = RUNS) -> str:
    fields = {
        run.name: read_hsig_field(runs_root / run.directory / "scaloost_rp.mat")
        for run in CASES
    }
    points = {
        run.name: read_hsig_points(runs_root / run.directory / "uitvoerpunten.tab")
        for run in CASES
    }
    reference_field = fields[CASES[0].name]
    reference_points = points[CASES[0].name]

    lines = [
        f"Condition: `{CASES[0].condition}`",
        "",
        "| Run | Physics | Field wet/dry | Field mean Hsig (m) | "
        "Points wet/dry | Points mean Hsig (m) | Field bias vs BSS (m) | "
        "RMS (m) | Max abs. (m) | Convergence |",
        "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|",
    ]
    for run in CASES:
        field = fields[run.name]
        point = points[run.name]
        field_delta = compare(reference_field, field)
        # Validate the output-point mask as a separate invariant even though the
        # reported difference metrics are intentionally field based.
        compare(reference_points, point)
        iteration, accuracy = convergence(runs_root / run.directory)
        lines.append(
            f"| {run.name} | {run.physics} | "
            f"{field.wet_count}/{field.dry_count} | {field.mean:.6f} | "
            f"{point.wet_count}/{point.dry_count} | {point.mean:.6f} | "
            f"{field_delta.bias:+.6f} | {field_delta.rms:.6f} | "
            f"{field_delta.maximum_absolute:.6f} | "
            f"iter {iteration}, {accuracy:.2f}% |"
        )
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs-root", type=Path, default=RUNS)
    args = parser.parse_args()
    try:
        print(analyze(args.runs_root))
    except (FileNotFoundError, ValueError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
