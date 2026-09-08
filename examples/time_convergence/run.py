#!/usr/bin/env python3
"""Tijdstapgevoeligheid (niet-stationair, rapporterend).

Drie tijdstappen (1/2/4 min) op de compacte niet-stationaire case met PROP
BSBT (het default SORDUP-schema weigert netjes bij CFL>10 voor dt>=2 met
MSGERR(2) zonder te rekenen — gedocumenteerd invoervalidatiegedrag, geen
defect). Uitvoer elke 20 min valt samen voor alle dt.

Dit is bewust rapporterend, geen equivalentiepoort: de respons op
dt-verkleining is niet monotoon rond de windregime-overgang (gemeten),
dus een vooraf gemotiveerde convergentiepoort is hier niet eerlijk. Harde
poorten: zelfde uitvoertijden, eindige waarden, norm_end, convergentie-
geschiedenis aanwezig. Geen opgeslagen referentie.
"""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

SOURCE = Path(__file__).resolve().parent.parent / "nonstationary" / "regular"
BASENAME = "nonstationary_regular"
STEPS = (1, 2, 4)


def stage(work: Path, step: int) -> Path:
    case = work / f"dt{step}"
    case.mkdir(parents=True)
    for name in ("bottom.bot", "wind.wnd", f"{BASENAME}.swn"):
        shutil.copy2(SOURCE / name, case / name)
    deck = (case / f"{BASENAME}.swn").read_text()
    deck = deck.replace("20260101.000000 1 MIN 20260101.010000",
                        f"20260101.000000 {step} MIN 20260101.010000")
    deck = deck.replace("GEN3 KOMEN", "GEN3 KOMEN\nPROP BSBT")
    deck = deck.replace("OUTPUT 20260101.000000 10 MIN",
                        "OUTPUT 20260101.000000 20 MIN")
    if "PROP BSBT" not in deck or "20 MIN" not in deck:
        raise RuntimeError("tijdstap-deckaanpassing mislukt")
    (case / "INPUT").write_text(deck)
    return case


def read_series(case: Path) -> list[tuple[str, float]]:
    rows = []
    for line in (case / f"{BASENAME}_center.tbl").read_text(
            errors="replace").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        fields = stripped.split()
        value = float(fields[6])
        if not (value == value and value not in (float("inf"), float("-inf"))):
            raise RuntimeError(f"{case.name}: geen eindige Hsig")
        rows.append((fields[0], value))
    if not rows:
        raise RuntimeError(f"{case.name}: lege tabel")
    return rows


def report(work: Path, smoke: bool) -> None:
    series = {step: read_series(work / f"dt{step}") for step in STEPS}
    times = [stamp for stamp, _ in series[1]]
    for step in STEPS:
        if [stamp for stamp, _ in series[step]] != times:
            raise RuntimeError(f"dt={step}: uitvoertijden wijken af")
        if not (work / f"dt{step}" / "norm_end").is_file():
            raise RuntimeError(f"dt={step}: geen norm_end")
    print(f"uitvoertijden gelijk ({len(times)} niveaus), alle runs normaal "
          "beeindigd (norm_end; niet-stationair kent geen accuracy-poort).")
    if smoke:
        print("smoke — gedraaid zonder rapportage.")
        return
    print(f"{'tijd':>14} {'dt1':>9} {'dt2':>9} {'dt4':>9} "
          f"{'ratio(1-2)/(2-4)':>18}")
    for index, stamp in enumerate(times):
        values = [series[step][index][1] for step in STEPS]
        numerator = abs(values[0] - values[1])
        denominator = abs(values[1] - values[2])
        ratio = numerator / denominator if denominator else float("nan")
        print(f"{stamp:>14} {values[0]:9.5f} {values[1]:9.5f} "
              f"{values[2]:9.5f} {ratio:18.2f}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument(
        "--smoke", action="store_true",
        help="alleen draaien zonder rapportage; telt niet als geslaagde validatie")
    arguments = parser.parse_args()
    try:
        executable = Path(arguments.swan_executable).expanduser().resolve()
        if not executable.is_file():
            raise FileNotFoundError(f"SWAN executable not found: {executable}")
        work = Path(arguments.work_directory).expanduser().resolve()
        if work.exists():
            shutil.rmtree(work)
        work.mkdir(parents=True)
        for step in STEPS:
            case = stage(work, step)
            started = time.monotonic()
            result = subprocess.run([str(executable)], cwd=case, check=False)
            elapsed = time.monotonic() - started
            if result.returncode or not (case / "norm_end").is_file():
                raise RuntimeError(
                    f"dt={step} faalde (exit {result.returncode}); zie PRINT.")
            print(f"dt={step} in {elapsed:.1f} s.")
        report(work, arguments.smoke)
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
