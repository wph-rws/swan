#!/usr/bin/env python3
"""Generate a rectangular Triangle mesh for the unstructured Voordelta case."""

from __future__ import annotations

import argparse
import math
from pathlib import Path

X_MIN = 20_000.0
X_MAX = 85_000.0
Y_MIN = 380_000.0
Y_MAX = 450_000.0
DEFAULT_SPACING = 200.0
BASENAME = "voordelta_unstructured"


def mesh_dimensions(spacing: float) -> tuple[int, int]:
    if not math.isfinite(spacing) or spacing <= 0.0:
        raise ValueError("spacing must be a positive finite number")

    x_meshes = round((X_MAX - X_MIN) / spacing)
    y_meshes = round((Y_MAX - Y_MIN) / spacing)
    if x_meshes < 1 or y_meshes < 1:
        raise ValueError("spacing is larger than the Voordelta domain")
    if not math.isclose(x_meshes * spacing, X_MAX - X_MIN, abs_tol=1.0e-6):
        raise ValueError("spacing must divide the 65 km x extent exactly")
    if not math.isclose(y_meshes * spacing, Y_MAX - Y_MIN, abs_tol=1.0e-6):
        raise ValueError("spacing must divide the 70 km y extent exactly")
    return x_meshes, y_meshes


def expected_counts(spacing: float) -> tuple[int, int]:
    x_meshes, y_meshes = mesh_dimensions(spacing)
    return (x_meshes + 1) * (y_meshes + 1), 2 * x_meshes * y_meshes


def mesh_matches(directory: Path, spacing: float) -> bool:
    node_path = directory / f"{BASENAME}.node"
    element_path = directory / f"{BASENAME}.ele"
    expected_vertices, expected_elements = expected_counts(spacing)
    try:
        node_header = node_path.open(encoding="ascii").readline().split()
        element_header = element_path.open(encoding="ascii").readline().split()
        return node_header == [
            str(expected_vertices),
            "2",
            "0",
            "1",
        ] and element_header == [str(expected_elements), "3", "0"]
    except OSError:
        return False


def generate_mesh(
    directory: Path, spacing: float, force: bool = False
) -> tuple[int, int]:
    x_meshes, y_meshes = mesh_dimensions(spacing)
    vertices, elements = expected_counts(spacing)
    directory.mkdir(parents=True, exist_ok=True)
    if not force and mesh_matches(directory, spacing):
        return vertices, elements

    node_path = directory / f"{BASENAME}.node"
    element_path = directory / f"{BASENAME}.ele"
    node_temporary = node_path.with_suffix(".node.tmp")
    element_temporary = element_path.with_suffix(".ele.tmp")

    try:
        with node_temporary.open("w", encoding="ascii", newline="\n") as output:
            output.write(f"{vertices} 2 0 1\n")
            vertex = 1
            for iy in range(y_meshes + 1):
                y = Y_MIN + iy * spacing
                for ix in range(x_meshes + 1):
                    x = X_MIN + ix * spacing
                    # Marker 1 is the offshore (west) wave boundary. Marker 2
                    # identifies the other three sides; zero means interior.
                    if ix == 0:
                        marker = 1
                    elif ix == x_meshes or iy == 0 or iy == y_meshes:
                        marker = 2
                    else:
                        marker = 0
                    output.write(f"{vertex} {x:.1f} {y:.1f} {marker}\n")
                    vertex += 1

        row_length = x_meshes + 1
        with element_temporary.open("w", encoding="ascii", newline="\n") as output:
            output.write(f"{elements} 3 0\n")
            element = 1
            for iy in range(y_meshes):
                for ix in range(x_meshes):
                    lower_left = iy * row_length + ix + 1
                    lower_right = lower_left + 1
                    upper_left = lower_left + row_length
                    upper_right = upper_left + 1
                    # Alternate diagonals to avoid a systematic directional bias.
                    # Both connectivity orders are counterclockwise.
                    if (ix + iy) % 2 == 0:
                        triangles = (
                            (lower_left, lower_right, upper_right),
                            (lower_left, upper_right, upper_left),
                        )
                    else:
                        triangles = (
                            (lower_left, lower_right, upper_left),
                            (lower_right, upper_right, upper_left),
                        )
                    for first, second, third in triangles:
                        output.write(f"{element} {first} {second} {third}\n")
                        element += 1

        node_temporary.replace(node_path)
        element_temporary.replace(element_path)
    finally:
        node_temporary.unlink(missing_ok=True)
        element_temporary.unlink(missing_ok=True)

    return vertices, elements


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--spacing",
        type=float,
        default=DEFAULT_SPACING,
        help="uniform vertex spacing in metres (default: %(default)s)",
    )
    parser.add_argument("--force", action="store_true", help="replace an existing mesh")
    arguments = parser.parse_args()
    try:
        vertices, elements = generate_mesh(
            Path(__file__).resolve().parent,
            arguments.spacing,
            arguments.force,
        )
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")
    print(
        f"Mesh ready: {vertices:,} vertices, {elements:,} triangles, "
        f"spacing {arguments.spacing:g} m."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
