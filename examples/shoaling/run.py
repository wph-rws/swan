#!/usr/bin/env python3
"""Analytische shoaling- en behoudsproef (lineaire golftheorie).

Twee gootcases (uniform in y), elk normaal (270) en schuin (300) invallend:
- vlak (10 m): behoud — Hsig constant langs de centerlijn (exact, elke
  afwijking is numeriek);
- helling (20 -> 3 m): shoaling — Hsig-ratio volgt Green via behoud van
  energieflux per frequentie; schuin bovendien refractiedraaiing via Snellius.
Normaal domein 5000x4000 m; schuin 5000x8000 m (transect buiten de
noordwest-hoekschaduw, zie onder).

Referentie is de theorie zelf (in deze runner berekend), geen opgeslagen
SWAN-uitvoer: dit is validatie, geen verificatie. Aannamen met onderbouwing:
- geen wind, `OFF SOURCES` (alle brontermen uit), geen stroming, geen wrijving;
  uniforme dieptecontouren evenwijdig aan y;
- normaal 4 punten, schuin 3 punten (kortere transect tegen laterale wegl loop)
  met smalle richtingsverdeling (ms=10); schuine transect op y=2000 in een
  8000 m brede goot, buiten de hoekschaduw van de noordwesthoek (gemeten:
  centerlijn 3,6 graden afwijking, y=2000 exact);
- schuine referentie is Snellius/KsKr op de piekfrequentie (benadering waarvan
  de fout tweede orde is in de spreidingsbreedte; empirische marge 3-8x);
  spectraal volledig geïntegreerde Snellius is expliciet vervolgwerk;
- JONSWAP (Hs=1 m, Tp=6 s, gamma=3,3) numeriek genormeerd; verhoudingen t.o.v.
  het eerste punt heffen absolute schaal- en staartdetails grotendeels weg.

Poorten (vooraf vastgelegd): DEPTH-opname 0,02 m (valideert .bot-orientatie),
vlak behoud 1%, helling-Green 5%, richting 2 graden (schuin t.o.v. Snellius),
convergentie >= 99,5%.
"""

from __future__ import annotations

import argparse
import math
import shutil
import subprocess
import sys
import time
from pathlib import Path

# A-priori poorten (zie module-docstring voor onderbouwing).
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
# Normaal: 4000 m breed (transect op de centerlijn); schuin: 8000 m breed
# (transect aan de benedenstroomse kant, buiten de hoekschaduw van de
# noordwesthoek — gemeten: centerlijn geeft 3,6 graden afwijking, y=2000 geeft
# 0,03 graden).
NY_NORMAL, YLEN_NORMAL = 40, 4000.0
NY_OBLIQUE, YLEN_OBLIQUE = 80, 8000.0
DY_NORMAL = YLEN_NORMAL / NY_NORMAL
DY_OBLIQUE = YLEN_OBLIQUE / NY_OBLIQUE


def dispersion_k(frequency: float, depth: float) -> float:
    """Golfgetal uit sigma^2 = g k tanh(kd), via bisectie (unieke wortel)."""
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
    """cg uit de lineaire dispersierelatie."""
    wavenumber = dispersion_k(frequency, depth)
    kd = wavenumber * depth
    celerity = 2.0 * math.pi * frequency / wavenumber
    return 0.5 * celerity * (1.0 + 2.0 * kd / math.sinh(2.0 * kd))


def jonswap_shape(frequency: float, peak: float, gamma: float = 3.3) -> float:
    """Ongenormeerde JONSWAP-vorm."""
    sigma = 0.07 if frequency <= peak else 0.09
    peak_factor = gamma ** math.exp(
        -0.5 * ((frequency - peak) / (sigma * peak)) ** 2)
    return ((peak / frequency) ** 5
            * math.exp(-1.25 * (peak / frequency) ** 4) * peak_factor)


def green_ratio(depth: float, reference_depth: float,
                peak_period: float = 6.0) -> float:
    """Hs(d)/Hs(d_ref) uit fluxbehoud per frequentie (trapezium, fijn rooster)."""
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
    """Fasesnelheid uit de lineaire dispersierelatie."""
    return 2.0 * math.pi * frequency / dispersion_k(frequency, depth)


def snell_from_direction(from_direction: float, depth: float,
                         reference_depth: float,
                         peak_period: float = PEAK_PERIOD) -> float:
    """Piek-Snellius: invalshoek t.o.v. de normaal behoudt sin/c."""
    peak = 1.0 / peak_period
    beta0 = math.radians(from_direction - 270.0)
    ratio = celerity(peak, depth) / celerity(peak, reference_depth)
    return 270.0 + math.degrees(math.asin(math.sin(beta0) * ratio))


def refraction_factor(from_direction: float, depth: float,
                      reference_depth: float,
                      peak_period: float = PEAK_PERIOD) -> float:
    """Piek-Kr = sqrt(cos(beta0)/cos(beta))."""
    peak = 1.0 / peak_period
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
        raise RuntimeError(f"{path}: {len(rows)} rijen, {count} verwacht")
    return rows


def final_accuracy(print_path: Path) -> float:
    values = []
    for line in print_path.read_text(errors="replace").splitlines():
        if "accuracy OK in" in line:
            token = line.split("accuracy OK in")[1].split("%")[0]
            values.append(float(token))
    if not values:
        raise RuntimeError(f"{print_path}: geen convergentiegeschiedenis")
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
                f"{tag}: DEPTH {depth} bij x={x}, {intended_depth(x, flat)} "
                "verwacht (.bot-orientatie?)")
    depths = [row[2] for row in rows]
    if not oblique:
        for _, (_, _, _, _, measured) in zip(points, rows):
            if circular_distance(measured, direction) > DIRECTION_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: richting {measured}, {direction} verwacht")
    else:
        for x, (_, _, depth, _, measured) in zip(points, rows):
            expected = snell_from_direction(direction, depth, depths[0])
            if circular_distance(measured, expected) > DIRECTION_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: richting {measured} bij x={x}, Snellius "
                    f"{expected:.3f} verwacht")
    accuracy = final_accuracy(directory / "PRINT")
    if accuracy < CONVERGENCE_MINIMUM:
        raise RuntimeError(f"{tag}: convergentie {accuracy} < {CONVERGENCE_MINIMUM}")
    if smoke:
        print(f"{tag}: smoke — gedraaid zonder analytische poort.")
        return
    hsig = [row[3] for row in rows]
    if flat:
        for x, value in zip(points, hsig):
            if abs(value / hsig[0] - 1.0) > FLAT_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: Hs({x})={value:.5f}, behoud geschonden "
                    f"(ratio {value / hsig[0]:.4f})")
        print(f"{tag}: behoud OK (max afwijking "
              f"{max(abs(v / hsig[0] - 1.0) for v in hsig):.4f}).")
    elif not oblique:
        for x, value, depth in zip(points, hsig, depths):
            expected = green_ratio(depth, depths[0]) * hsig[0]
            deviation = abs(value - expected) / expected
            if deviation > SHOALING_TOLERANCE:
                raise RuntimeError(
                    f"{tag}: Hs({x})={value:.5f}, Green {expected:.5f} "
                    f"(afwijking {deviation:.4f})")
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
                    f"{tag}: Hs({x})={value:.5f}, Green+Snellius "
                    f"{expected:.5f} (afwijking {deviation:.4f})")
        print(f"{tag}: Green+Snellius OK "
              f"(ratios {[f'{v / hsig[0]:.4f}' for v in hsig]}).")


def find_executable(case_directory: Path, requested: str | None) -> Path:
    if requested:
        executable = Path(requested).expanduser().resolve()
        if executable.is_file():
            return executable
        raise FileNotFoundError(f"SWAN executable not found: {executable}")
    raise FileNotFoundError(
        "geef --swan-executable /pad/naar/swan.exe (geen standaardpad).")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument(
        "--smoke", action="store_true",
        help="alleen draaien zonder analytische vergelijking; "
             "telt niet als geslaagde validatie")
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
                        f"{case.name} faalde (exit {result.returncode}); "
                        "zie PRINT.")
                check_case(case, flat, oblique, arguments.smoke)
                print(f"{case.name} in {elapsed:.1f} s.")
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
