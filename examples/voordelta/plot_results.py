#!/usr/bin/env python3
"""Create bathymetry and wave-height maps from Voordelta SWAN output."""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


GRID_SHAPE = (141, 131)
EXTENT = (20_000.0, 85_000.0, 380_000.0, 450_000.0)
SITES = (
    (30_000.0, 430_000.0, "Offshore"),
    (45_000.0, 420_000.0, "Voordelta"),
    (55_000.0, 440_000.0, "Maasvlakte"),
)


def read_grid(path: Path) -> np.ndarray:
    values = np.fromfile(path, sep=" ")
    expected = GRID_SHAPE[0] * GRID_SHAPE[1]
    if values.size != expected:
        raise ValueError(f"{path} contains {values.size} values; expected {expected}")
    return values.reshape(GRID_SHAPE)


def finish_map(axis: plt.Axes) -> None:
    for x, y, label in SITES:
        axis.plot(x, y, "o", color="white", markeredgecolor="black", markersize=5)
        axis.annotate(
            label,
            (x, y),
            xytext=(6, 5),
            textcoords="offset points",
            fontsize=8,
            color="black",
            bbox={"facecolor": "white", "edgecolor": "none", "alpha": 0.7, "pad": 1},
        )
    axis.set_xlabel("RD x [m]")
    axis.set_ylabel("RD y [m]")
    axis.set_aspect("equal")
    axis.grid(color="white", linewidth=0.4, alpha=0.35)


def save_map(
    values: np.ma.MaskedArray,
    destination: Path,
    title: str,
    colorbar_label: str,
    cmap: str,
) -> None:
    figure, axis = plt.subplots(figsize=(9, 8), constrained_layout=True)
    axis.set_facecolor("#e6e2dc")
    image = axis.imshow(
        values,
        extent=EXTENT,
        origin="lower",
        interpolation="nearest",
        cmap=cmap,
    )
    figure.colorbar(image, ax=axis, shrink=0.86, label=colorbar_label)
    axis.set_title(title, fontsize=14, pad=12)
    finish_map(axis)
    figure.savefig(destination, dpi=180)
    plt.close(figure)


def create_plots(results_directory: Path) -> list[Path]:
    results_directory.mkdir(parents=True, exist_ok=True)
    depth = read_grid(results_directory / "voordelta_depth.blk")
    wave_height = read_grid(results_directory / "voordelta_hs.blk")
    dry = (depth <= 0.0) | (depth > 100.0) | ~np.isfinite(depth)

    depth_map = np.ma.masked_where(dry, depth)
    wave_map = np.ma.masked_where(dry | ~np.isfinite(wave_height), wave_height)
    destinations = [
        results_directory / "voordelta_depth.png",
        results_directory / "voordelta_hs.png",
    ]
    save_map(
        depth_map,
        destinations[0],
        "Voordelta – waterdiepte t.o.v. NAP",
        "Waterdiepte [m]",
        "viridis_r",
    )
    save_map(
        wave_map,
        destinations[1],
        "Voordelta – significante golfhoogte",
        "Hs [m]",
        "viridis",
    )
    return destinations


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "results_directory",
        nargs="?",
        type=Path,
        default=Path(__file__).resolve().parent / "results",
        help="directory containing voordelta_depth.blk and voordelta_hs.blk",
    )
    arguments = parser.parse_args()
    try:
        for path in create_plots(arguments.results_directory.resolve()):
            print(f"Created {path}")
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
