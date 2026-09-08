#!/usr/bin/env python3
"""Levensduur-afsluitprotocol (vooraf vastgelegd, 8 september 2026).

Protocol, vastgelegd vóór meting zoals doc/moderniseringsplan.md §4 eist:

- Lengte: 2 warming-up runs (niet beoordeeld) + 50 beoordeelde runs,
  alternerend tussen twee kleine decks (quick_test / two_cases).
- Elke beoordeelde run onder Valgrind memcheck:
  --leak-check=full --show-leak-kinds=all --error-exitcode=9 --track-fds=yes
- Passcriteria per run: swan-exit 0, norm_end aanwezig, ERROR SUMMARY 0,
  definitely lost 0, indirectly lost 0, open fds bij afsluiten <= 8.
- Passcriteria over de reeks: |still_reachable(laatste) - still_reachable(eerste)|
  <= 4096 B, (max - min) <= 32768 B, |RSS(laatste) - RSS(eerste)| <= 256 MB
  (RSS = valgrind-proces, inclusief tool; relatief groeisignaal).
- Invoerfout-herstel (1x): deck met niet-bestaand grensbestand moet schoon
  falen (exit != 0, geen norm_end, melding in PRINT), waarna één herstel-run
  met het goede deck groen moet zijn zonder achtergebleven INPUT.
- Uitvoer: CSV met per-run waarden + eindoordeel PASS/FAIL (exitcode).

Gebruik: python3 scripts/lifetime_protocol.py --swan <pad/naar/swan.exe>
            [--csv <pad>] [--runs 50]
"""
from __future__ import annotations

import argparse
import csv
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent

TWO_CASES_INPUT = """PROJECT 'TWICE' '001'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
COORDINATES CARTESIAN
CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 4 2 CIRCLE 12 0.05 1.0 8
INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0
READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE
WIND 8.0 270.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0
OBST TRANS 0.25 LINE 900.0 0.0 900.0 1000.0
GEN3 KOMEN
NUMERIC STOPC STAT 2
BREAKING CONSTANT 1.0 0.73
FRICTION JONSWAP CONSTANT 0.038
POINTS 'CENTER' 1000.0 500.0
TABLE 'CENTER' HEADER 'two_cases.tbl' XP YP DEPTH HSIGN TM01 DIR
COMPUTE
STOP
"""

FAULT_INPUT = TWO_CASES_INPUT.replace(
    "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0",
    "BOUNDSPEC SIDE WEST CONSTANT FILE 'missing_bnd.dat'",
)


def prepare_dir(work: Path, swaninit: Path, bottom: Path, input_text: str) -> None:
    work.mkdir(parents=True, exist_ok=True)
    shutil.copy2(swaninit, work / "swaninit")
    shutil.copy2(bottom, work / "bottom.bot")
    (work / "INPUT").write_text(input_text)


def parse_valgrind(log: Path) -> dict:
    text = log.read_text(errors="replace")
    # Afwezigheid van een lekrecord betekent bij --show-leak-kinds=all: nul.
    out = {
        "errors": -1,
        "def_lost_b": 0,
        "def_lost_n": 0,
        "ind_lost_b": 0,
        "still_reach_b": 0,
        "open_fds": -1,
    }
    m = re.search(r"ERROR SUMMARY: (\d+) errors", text)
    if m:
        out["errors"] = int(m.group(1))
    m = re.search(r"definitely lost: ([\d,]+) bytes in (\d+) blocks", text)
    if m:
        out["def_lost_b"] = int(m.group(1).replace(",", ""))
        out["def_lost_n"] = int(m.group(2))
    m = re.search(r"indirectly lost: ([\d,]+) bytes in (\d+) blocks", text)
    if m:
        out["ind_lost_b"] = int(m.group(1).replace(",", ""))
    m = re.search(r"still reachable: ([\d,]+) bytes in (\d+) blocks", text)
    if m:
        out["still_reach_b"] = int(m.group(1).replace(",", ""))
    m = re.search(r"FILE DESCRIPTORS: (\d+) open", text)
    if m:
        out["open_fds"] = int(m.group(1))
    return out


def parse_rss(time_log: Path) -> int:
    m = re.search(r"Maximum resident set size \(kbytes\): (\d+)",
                  time_log.read_text(errors="replace"))
    return int(m.group(1)) if m else -1


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--swan", required=True)
    ap.add_argument("--csv", default="levensduurprotocol.csv")
    ap.add_argument("--runs", type=int, default=50)
    ap.add_argument("--warmup", type=int, default=2)
    args = ap.parse_args()

    swan = Path(args.swan).resolve()
    if not swan.is_file():
        print(f"geen swan.exe: {swan}", file=sys.stderr)
        return 2

    qt = REPO / "examples" / "quick_test"
    swaninit_src = REPO / "build-modernization-strict" / "two_cases" / "swaninit"
    if not swaninit_src.is_file():
        swaninit_src = qt / "swaninit"
    bottom_qt = qt / "bottom.bot"
    bottom_tc = REPO / "build-modernization-strict" / "two_cases" / "bottom.bot"

    decks = {
        "quick_test": (qt / "quick_test.swn", bottom_qt),
        "two_cases": (None, bottom_tc),  # INPUT is de embedded tekst
    }

    rows: list[dict] = []
    failures: list[str] = []
    base = Path(tempfile.mkdtemp(prefix="levensduur-"))
    print(f"werkmap: {base}")

    def run_one(idx: int, deck: str, judged: bool) -> dict:
        work = base / f"run{idx:03d}_{deck}"
        input_text = (decks[deck][0].read_text() if decks[deck][0] is not None
                      else TWO_CASES_INPUT)
        prepare_dir(work, swaninit_src, decks[deck][1], input_text)
        vg_log = work / "run.vglog"
        time_log = work / "run.time"
        cmd = ["valgrind", "--leak-check=full", "--show-leak-kinds=all",
               "--error-exitcode=9", "--track-fds=yes",
               f"--log-file={vg_log}", str(swan)]
        with open(time_log, "w") as tf:
            proc = subprocess.run(["/usr/bin/time", "-v", *cmd], cwd=work,
                                  stdout=tf, stderr=subprocess.STDOUT,
                                  timeout=300)
        row = {
            "run": idx, "deck": deck, "judged": judged,
            "swan_exit": proc.returncode,
            "norm_end": (work / "norm_end").is_file(),
            "rss_kb": parse_rss(time_log),
            **parse_valgrind(vg_log),
        }
        rows.append(row)
        if judged:
            if row["swan_exit"] != 0 or not row["norm_end"]:
                failures.append(f"run {idx}: exit={row['swan_exit']} norm_end={row['norm_end']}")
            for key, limit, what in (("errors", 0, "valgrind errors"),
                                     ("def_lost_b", 0, "definitely lost"),
                                     ("ind_lost_b", 0, "indirectly lost"),
                                     ("open_fds", 8, "open fds")):
                if row[key] > limit:
                    failures.append(f"run {idx}: {what}={row[key]}")
        return row

    idx = 1
    for _ in range(args.warmup):
        run_one(idx, "quick_test" if idx % 2 else "two_cases", judged=False)
        idx += 1
    first = last = None
    for _ in range(args.runs):
        row = run_one(idx, "quick_test" if idx % 2 else "two_cases", judged=True)
        if first is None:
            first = row
        last = row
        idx += 1

    sr = [r["still_reach_b"] for r in rows if r["judged"] and r["still_reach_b"] >= 0]
    if sr:
        if abs(sr[-1] - sr[0]) > 4096:
            failures.append(f"still_reachable groei {abs(sr[-1]-sr[0])} B > 4096 B")
        if max(sr) - min(sr) > 32768:
            failures.append(f"still_reachable band {max(sr)-min(sr)} B > 32768 B")
    rss = [r["rss_kb"] for r in rows if r["judged"] and r["rss_kb"] > 0]
    if rss and abs(rss[-1] - rss[0]) > 256 * 1024:
        failures.append(f"RSS-groei {abs(rss[-1]-rss[0])} kB > 256 MB")

    # Invoerfout-herstel (1x)
    fault = base / "fault"
    prepare_dir(fault, swaninit_src, bottom_tc, FAULT_INPUT)
    fproc = subprocess.run([str(swan)], cwd=fault, capture_output=True, timeout=300)
    fault_ok = (fproc.returncode != 0
                and not (fault / "norm_end").is_file()
                and "missing_bnd.dat" in (fault / "PRINT").read_text(errors="replace"))
    if not fault_ok:
        failures.append("invoerfout: faalde niet schoon")
    rows.append({"run": idx, "deck": "INVOERFOUT", "judged": True,
                 "swan_exit": fproc.returncode,
                 "norm_end": (fault / "norm_end").is_file(),
                 "rss_kb": -1, "errors": -1, "def_lost_b": -1, "def_lost_n": -1,
                 "ind_lost_b": -1, "still_reach_b": -1, "open_fds": -1})
    idx += 1
    rec = run_one(idx, "two_cases", judged=True)

    csv_path = REPO / args.csv
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    with open(csv_path, "w", newline="") as cf:
        writer = csv.DictWriter(cf, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    print(f"\nbeoordeelde runs: {args.runs}, warmup: {args.warmup}")
    if sr:
        print(f"still_reachable eerste/laatste: {sr[0]}/{sr[-1]} B, band {max(sr)-min(sr)} B")
    else:
        print("still_reachable: geen records (alle blokken vrijgegeven)")
    if rss:
        print(f"RSS eerste/laatste: {rss[0]}/{rss[-1]} kB")
    print(f"invoerfout schoon gefaald: {fault_ok}, herstel groen: "
          f"{rec['swan_exit'] == 0 and rec['norm_end'] and rec['errors'] == 0}")
    if failures:
        print("FAIL:")
        for f in failures:
            print(f"  - {f}")
        print(f"CSV: {csv_path}")
        return 1
    print("PASS — alle criteria gehaald")
    print(f"CSV: {csv_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
