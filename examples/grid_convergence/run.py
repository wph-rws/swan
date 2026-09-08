#!/usr/bin/env python3
"""Roosterconvergentieproef (vlakke goot, brontermen uit).

Drie geneste resoluties (dx = 40/20/10 m) op hetzelfde domein (2000x1000 m,
vlakke bodem 10 m, westelijke JONSWAP-rand Hs=1/Tp=6/ms=10, `OFF SOURCES`,
loodrechte inval). Het voortplantingsschema is eerste-orde upwind: de
center-Hsig moet lineair convergeren (verfijningsratio ~2) naar een waarde
dicht bij 1. Referentie is de convergentie-eigenschap zelf, geen opgeslagen
uitvoer.

Poorten (vooraf vastgelegd, eerste-orde-theorie): successive-verschilratio
>= 1,5 (convergeert; geen divergentie/oscillatie), fijnste-tweetal
relatief <= 0,5%, DEPTH 0,02 m, richting 1 graad, convergentie >= 99,5%.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

DEPTH_TOLERANCE = 0.02
RATIO_MINIMUM = 1.5
FINE_PAIR_TOLERANCE = 0.005
DIRECTION_TOLERANCE = 1.0
CONVERGENCE_MINIMUM = 99.5

RESOLUTIONS = (50, 100, 200)
DOMAIN_X, DOMAIN_Y, NY, DY = 2000.0, 1000.0, 10, 100.0
CENTER = (1000.0, 500.0)


def estimated_order(coarse: float, middle: float, fine: float) -> float:
    """Geschatte convergentieorde uit drie geneste oplossingen."""
    import math

    numerator = abs(coarse - middle)
    denominator = abs(middle - fine)
    if denominator == 0.0:
        raise ValueError("middel en fijn zijn bitgelijk; geen orde te bepalen")
    if numerator == 0.0:
        raise ValueError("grof en middel zijn bitgelijk; geen orde te bepalen")
    return math.log(numerator / denominator) / math.log(2.0)


def write_case(directory: Path, nx: int) -> None:
    bottom = "\n".join(
        " ".join("10.0000" for _ in range(nx + 1)) for _ in range(NY + 1)
    ) + "\n"
    (directory / f"bottom_{nx}.bot").write_text(bottom)
    (directory / "INPUT").write_text(
        "PROJECT 'GCONV' '001'\n"
        "SET NAUTICAL\n"
        "MODE STATIONARY TWODIMENSIONAL\n"
        "COORDINATES CARTESIAN\n"
        f"CGRID REGULAR 0.0 0.0 0.0 {DOMAIN_X:.1f} {DOMAIN_Y:.1f} "
        f"{nx} {NY} CIRCLE 24 0.04 1.0 24\n"
        f"INPGRID BOTTOM REGULAR 0.0 0.0 0.0 {nx} 1 "
        f"{DOMAIN_X / nx:.1f} {DOMAIN_Y:.1f}\n"
        f"READINP BOTTOM 1.0 'bottom_{nx}.bot' 3 0 FREE\n"
        "BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES\n"
        "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 10.0\n"
        "OFF SOURCES\n"
        f"POINTS 'CENTER' {CENTER[0]:.1f} {CENTER[1]:.1f}\n"
        f"TABLE 'CENTER' HEADER 'c{nx}.tbl' XP YP DEPTH HSIGN DIR\n"
        "COMPUTE\n"
        "STOP\n"
    )


def read_center(path: Path) -> tuple[float, float, float]:
    rows = []
    for line in path.read_text(errors="replace").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        fields = [float(token) for token in stripped.split()]
        rows.append((fields[2], fields[3], fields[4]))
    if len(rows) != 1:
        raise RuntimeError(f"{path}: {len(rows)} rijen, 1 verwacht")
    return rows[0]


def final_accuracy(print_path: Path) -> float:
    values = []
    for line in print_path.read_text(errors="replace").splitlines():
        if "accuracy OK in" in line:
            values.append(float(line.split("accuracy OK in")[1].split("%")[0]))
    if not values:
        raise RuntimeError(f"{print_path}: geen convergentiegeschiedenis")
    return values[-1]


def circular_distance(first: float, second: float) -> float:
    return abs((first - second + 540.0) % 360.0 - 180.0)


def check_all(work: Path, smoke: bool) -> None:
    results = {}
    for nx in RESOLUTIONS:
        case = work / f"nx{nx}"
        depth, hsig, direction = read_center(case / f"c{nx}.tbl")
        if abs(depth - 10.0) > DEPTH_TOLERANCE:
            raise RuntimeError(f"nx={nx}: DEPTH {depth}, 10 verwacht")
        if circular_distance(direction, 270.0) > DIRECTION_TOLERANCE:
            raise RuntimeError(f"nx={nx}: richting {direction}, 270 verwacht")
        accuracy = final_accuracy(case / "PRINT")
        if accuracy < CONVERGENCE_MINIMUM:
            raise RuntimeError(f"nx={nx}: convergentie {accuracy}")
        results[nx] = hsig
    if smoke:
        print("smoke — gedraaid zonder convergentiepoort.")
        return
    coarse, middle, fine = (results[nx] for nx in RESOLUTIONS)
    if abs(middle - fine) / abs(fine) > FINE_PAIR_TOLERANCE:
        raise RuntimeError(
            f"fijnste tweetal te ver uit elkaar: {middle:.5f} vs {fine:.5f}")
    order = estimated_order(coarse, middle, fine)
    ratio = abs(coarse - middle) / abs(middle - fine)
    if ratio < RATIO_MINIMUM:
        raise RuntimeError(
            f"geen convergentie: ratio {ratio:.2f} (orde {order:.2f})")
    print(f"roosterconvergentie OK (ratio {ratio:.2f}, orde {order:.2f}; "
          f"Hs {coarse:.5f} {middle:.5f} {fine:.5f}).")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument(
        "--smoke", action="store_true",
        help="alleen draaien zonder convergentievergelijking; "
             "telt niet als geslaagde validatie")
    arguments = parser.parse_args()
    try:
        executable = Path(arguments.swan_executable).expanduser().resolve()
        if not executable.is_file():
            raise FileNotFoundError(f"SWAN executable not found: {executable}")
        work = Path(arguments.work_directory).expanduser().resolve()
        if work.exists():
            shutil.rmtree(work)
        for nx in RESOLUTIONS:
            case = work / f"nx{nx}"
            case.mkdir(parents=True)
            write_case(case, nx)
            started = time.monotonic()
            result = subprocess.run([str(executable)], cwd=case, check=False)
            elapsed = time.monotonic() - started
            if result.returncode or not (case / "norm_end").is_file():
                raise RuntimeError(
                    f"nx={nx} faalde (exit {result.returncode}); zie PRINT.")
            print(f"nx={nx} in {elapsed:.1f} s.")
        check_all(work, arguments.smoke)
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
