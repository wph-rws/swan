#!/usr/bin/env python3
"""Analytical shoaling and conservation test (linear wave theory).

Two channel cases (uniform in y), each at normal (270) and oblique (300) incidence:
- flat (10 m): conservation — Hsig constant along the centre line (exact, any
  deviation is numerical);
- slope (20 -> 3 m): shoaling — the Hsig ratio follows Green through conservation
  of energy flux per frequency; oblique incidence also turns by refraction (Snell).
Normal domain 5000x4000 m; oblique 5000x8000 m (transect outside the
shadow of the north-west corner, see below).

The reference is the theory itself (computed in this runner), not stored
SWAN output: this is validation, not verification. Assumptions and their grounds:
- no wind, `OFF SOURCES` (all source terms off), no current, no friction;
  uniform depth contours parallel to y;
- normal 4 points, oblique 3 points (shorter transect against lateral leakage)
  with narrow directional spreading (ms=10); oblique transect at y=2000 in an
  8000 m wide channel, outside the shadow of the north-west corner (measured:
  centre line 3.6 degrees off, y=2000 exact);
- the oblique reference is Snell/KsKr at the peak frequency (an approximation whose
  error is second order in the spreading width; empirical margin 3-8x);
  Snell integrated over the full spectrum is explicitly follow-up work;
- JONSWAP (Hs=1 m, Tp=6 s, gamma=3.3) normalised numerically; ratios relative to
  the first point largely cancel absolute scale and tail details.

Gates (fixed in advance): DEPTH as read 0.02 m (validates the .bot orientation),
flat conservation 1%, slope Green 5%, direction 2 degrees (oblique, against Snell),
convergence >= 99.5%.
"""

from __future__ import annotations

import argparse
import math
import shutil
import subprocess
import sys
import time
from pathlib import Path

# A-priori gates (see the module docstring for the reasoning).
DEPTH_TOLERANCE = 0.02
FLAT_TOLERANCE = 0.01
SHOALING_TOLERANCE = 0.05
DIRECTION_TOLERANCE = 2.0
CONVERGENCE_MINIMUM = 99.5

GRAVITY = 9.81
NORMAL_XS = (500.0, 1500.0, 2500.0, 3500.0)
OBLIQUE_XS = (500.0, 1500.0, 2500.0)
NORMAL_DIR = 270.0
OBLIQUE_DIR = 300.0
PEAK_PERIOD = 6.0
Y_TRANSECT = 2000.0
NX, DX = 100, 50.0
# Normal: 4000 m wide (transect on the centre line); oblique: 8000 m wide
# (transect on the downstream side, outside the shadow of the
# north-west corner — measured: the centre line gives 3.6 degrees off, y=2000
# gives 0.03 degrees).
NY_NORMAL, YLEN_NORMAL = 40, 4000.0
NY_OBLIQUE, YLEN_OBLIQUE = 80, 8000.0
DY_NORMAL = YLEN_NORMAL / NY_NORMAL
DY_OBLIQUE = YLEN_OBLIQUE / NY_OBLIQUE


def dispersion_k(frequency: float, depth: float) -> float:
    """Wave number from sigma^2 = g k tanh(kd), by bisection (unique root)."""
    sigma = 2.0 * math.pi * frequency
    low, high = 1.0e-9, max(sigma * sigma / GRAVITY,
                            sigma / math.sqrt(GRAVITY * depth)) * 4.0

    def residual(wavenumber: float) -> float:
        return GRAVITY * wavenumber * math.tanh(wavenumber * depth) \
            - sigma * sigma

    assert residual(low) < 0.0 < residual(high)
    for _ in range(100):
        mid = 0.5 * (low + high)
        if residual(mid) > 0.0:
            high = mid
        else:
            low = mid
    return 0.5 * (low + high)


def group_velocity(frequency: float, depth: float) -> float:
    """cg from the linear dispersion relation."""
    wavenumber = dispersion_k(frequency, depth)
    kd = wavenumber * depth
    celerity = 2.0 * math.pi * frequency / wavenumber
    return 0.5 * celerity * (1.0 + 2.0 * kd / math.sinh(2.0 * kd))


def jonswap_shape(frequency: float, peak: float, gamma: float = 3.3) -> float:
    """Unnormalised JONSWAP shape."""
    sigma = 0.07 if frequency <= peak else 0.09
    peak_factor = gamma ** math.exp(
        -0.5 * ((frequency - peak) / (sigma * peak)) ** 2)
    return ((peak / frequency) ** 5
            * math.exp(-1.25 * (peak / frequency) ** 4) * peak_factor)


def green_ratio(depth: float, reference_depth: float,
                peak_period: float = 6.0) -> float:
    """Hs(d)/Hs(d_ref) from flux conservation per frequency (trapezium, fine grid)."""
    peak = 1.0 / peak_period
    frequencies = [0.04 * (1.0 / 0.04) ** (i / 1999) for i in range(2000)]
    weights = [jonswap_shape(f, peak) for f in frequencies]
    total = sum(0.5 * (a + b) * (g - f)
                for f, g, a, b in zip(frequencies, frequencies[1:],
                                      weights, weights[1:]))
    shoaled = sum(
        0.5 * (a * group_velocity(f, reference_depth)
               / group_velocity(f, depth)
               + b * group_velocity(g, reference_depth)
               / group_velocity(g, depth)) * (g - f)
        for f, g, a, b in zip(frequencies, frequencies[1:],
                              weights, weights[1:]))
    return math.sqrt(shoaled / total)


def celerity(frequency: float, depth: float) -> float:
    """Phase speed from the linear dispersion relation."""
    return 2.0 * math.pi * frequency / dispersion_k(frequency, depth)


def snell_from_direction(from_direction: float, depth: float,
                         reference_depth: float,
                         peak_period: float = PEAK_PERIOD) -> float:
    """Snell at the peak: the angle to the normal keeps sin/c constant."""
    peak = 1.0 / peak_period
    beta0 = math.radians(from_direction - 270.0)
    ratio = celerity(peak, depth) / celerity(peak, reference_depth)
    return 270.0 + math.degrees(math.asin(math.sin(beta0) * ratio))


def refraction_factor(from_direction: float, depth: float,
                      reference_depth: float,
                      peak_period: float = PEAK_PERIOD) -> float:
    """Kr at the peak = sqrt(cos(beta0)/cos(beta))."""
    beta0 = math.radians(from_direction - 270.0)
    beta = math.radians(snell_from_direction(
        from_direction, depth, reference_depth, peak_period) - 270.0)
    return math.sqrt(math.cos(beta0) / math.cos(beta))


def intended_depth(x: float, flat: bool) -> float:
    return 10.0 if flat else 20.0 - 17.0 * x / 5000.0


def case_tag(flat: bool, oblique: bool) -> str:
    return ("flat_oblique" if flat and oblique else
            "slope_oblique" if oblique else
            "flat" if flat else "slope")


def case_points(oblique: bool) -> tuple[float, ...]:
    return OBLIQUE_XS if oblique else NORMAL_XS


def write_case(directory: Path, flat: bool, oblique: bool) -> None:
    tag = case_tag(flat, oblique)
    points = case_points(oblique)
    direction = OBLIQUE_DIR if oblique else NORMAL_DIR
    ny, ylen, dy = ((NY_OBLIQUE, YLEN_OBLIQUE, DY_OBLIQUE) if oblique
                    else (NY_NORMAL, YLEN_NORMAL, DY_NORMAL))
    bottom = "\n".join(
        " ".join(f"{intended_depth(ix * DX, flat):.4f}" for ix in range(NX + 1))
        for _ in range(ny + 1)) + "\n"
    (directory / f"bottom_{tag}.bot").write_text(bottom)
    points_text = " ".join(f"{x:.1f} {Y_TRANSECT:.1f}" for x in points)
    (directory / "INPUT").write_text(
        "PROJECT 'SHOAL' '001'\n"
        "SET NAUTICAL\n"
        "MODE STATIONARY TWODIMENSIONAL\n"
        "COORDINATES CARTESIAN\n"
        f"CGRID REGULAR 0.0 0.0 0.0 5000.0 {ylen:.1f} 100 {ny} "
        "CIRCLE 24 0.04 1.0 24\n"
        f"INPGRID BOTTOM REGULAR 0.0 0.0 0.0 100 {ny} 50.0 {dy:.1f}\n"
        f"READINP BOTTOM 1.0 'bottom_{tag}.bot' 3 0 FREE\n"
        "BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES\n"
        f"BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 {direction:.1f} 10.0\n"
        "OFF SOURCES\n"
        f"POINTS 'TRANSECT' {points_text}\n"
        f"TABLE 'TRANSECT' HEADER '{tag}.tbl' XP YP DEPTH HSIGN DIR\n"
        "COMPUTE\n"
        "STOP\n"
    )


def read_table(path: Path, count: int) -> list[tuple[float, float, float, float, float]]:
    rows = []
    for line in path.read_text(errors="replace").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        fields = [float(token) for token in stripped.split()]
        rows.append((fields[0], fields[1], fields[2], fields[3], fields[4]))
    if len(rows) != count:
        raise RuntimeError(f"{path}: {len(rows)} rows, {count} expected")
    return rows


def final_accuracy(print_path: Path) -> float:
    values = []
    for line in print_path.read_text(errors="replace").splitlines():
        if "accuracy OK in" in line:
            token = line.split("accuracy OK in")[1].split("%")[0]
            values.append(float(token))
    if not values:
        raise RuntimeError(f"{print_path}: no convergence history")
    return values[-1]


def circular_distance(first: float, second: float) -> float:
    return abs((first - second + 540.0) % 360.0 - 180.0)


def check_case(directory: Path, flat: bool, oblique: bool, smoke: bool) -> None:
    tag = case_tag(flat, oblique)
    points = case_points(oblique)
    direction = OBLIQUE_DIR if oblique else NORMAL_DIR
    rows = read_table(directory / f"{tag}.tbl", len(points))
    for x, (_, _, depth, _, _) in zip(points, rows):
        if abs(depth - intended_depth(x, flat)) > DEPTH_TOLERANCE:
            raise RuntimeError(
                f"{tag}: DEPTH {depth} at x={x}, {intended_depth(x, flat)} "
                "expected (.bot orientation?)")
    depths = [row[2] for row in rows]
    if not oblique:
        for _, (_, _, _, _, measured) in zip(points, rows):
            if circular_distance(measured, direction) > DIRECTION_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: direction {measured}, {direction} expected")
    else:
        for x, (_, _, depth, _, measured) in zip(points, rows):
            expected = snell_from_direction(direction, depth, depths[0])
            if circular_distance(measured, expected) > DIRECTION_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: direction {measured} at x={x}, Snell "
                    f"{expected:.3f} expected")
    accuracy = final_accuracy(directory / "PRINT")
    if accuracy < CONVERGENCE_MINIMUM:
        raise RuntimeError(f"{tag}: convergence {accuracy} < {CONVERGENCE_MINIMUM}")
    if smoke:
        print(f"{tag}: smoke — ran without the analytical gate.")
        return
    hsig = [row[3] for row in rows]
    if flat:
        for x, value in zip(points, hsig):
            if abs(value / hsig[0] - 1.0) > FLAT_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: Hs({x})={value:.5f}, conservation violated "
                    f"(ratio {value / hsig[0]:.4f})")
        print(f"{tag}: conservation OK (max deviation "
              f"{max(abs(v / hsig[0] - 1.0) for v in hsig):.4f}).")
    elif not oblique:
        for x, value, depth in zip(points, hsig, depths):
            expected = green_ratio(depth, depths[0]) * hsig[0]
            deviation = abs(value - expected) / expected
            if deviation > SHOALING_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: Hs({x})={value:.5f}, Green {expected:.5f} "
                    f"(deviation {deviation:.4f})")
        print(f"{tag}: Green OK "
              f"(ratios {[f'{v / hsig[0]:.4f}' for v in hsig]}).")
    else:
        for x, value, depth in zip(points, hsig, depths):
            expected = (green_ratio(depth, depths[0])
                        * refraction_factor(direction, depth, depths[0])
                        * hsig[0])
            deviation = abs(value - expected) / expected
            if deviation > SHOALING_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: Hs({x})={value:.5f}, Green+Snell "
                    f"{expected:.5f} (deviation {deviation:.4f})")
        print(f"{tag}: Green+Snell OK "
              f"(ratios {[f'{v / hsig[0]:.4f}' for v in hsig]}).")


def find_executable(case_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")
    raise FileNotFoundError(
        "pass --swan-executable /path/to/swan.exe (there is no default path).")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument(
        "--smoke", action="store_true",
        help="run only, without the analytical comparison; "
             "does not count as a passed validation")
    arguments = parser.parse_args()
    try:
        executable = find_executable(Path(__file__).resolve().parent,
                                     arguments.swan_executable)
        work = Path(arguments.work_directory).expanduser().resolve()
        if work.exists():
            shutil.rmtree(work)
        for flat in (True, False):
            for oblique in (False, True):
                case = work / case_tag(flat, oblique)
                case.mkdir(parents=True)
                write_case(case, flat, oblique)
                started = time.monotonic()
                result = subprocess.run([str(executable)], cwd=case, check=False)
                elapsed = time.monotonic() - started
                if result.returncode or not (case / "norm_end").is_file():
                    raise RuntimeError(
                        f"{case.name} failed (exit {result.returncode}); "
                        "see PRINT.")
                check_case(case, flat, oblique, arguments.smoke)
                print(f"{case.name} in {elapsed:.1f} s.")
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
