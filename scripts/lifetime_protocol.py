#!/usr/bin/env python3
"""Memory lifetime close-out protocol (fixed in advance).

This is the shutdown protocol cited as evidence for BF-05 in
doc/bug-fixes-vs-tu-delft-41.51.md; rerun it after changes that allocate or
free run state.

Protocol, fixed before measuring:

- Length: 2 warm-up runs (not judged) + 50 judged runs,
  alternating between two small decks (quick_test / two_cases).
- Every judged run under Valgrind memcheck:
  --leak-check=full --show-leak-kinds=all --error-exitcode=9 --track-fds=yes
- Pass criteria per run: swan exit 0, norm_end present, ERROR SUMMARY 0,
  definitely lost 0, indirectly lost 0, open fds at exit <= 8. A Valgrind
  log without an error summary or a file descriptor count fails the run.
- Pass criteria over the series: |still_reachable(last) - still_reachable(first)|
  <= 4096 B, (max - min) <= 32768 B, |RSS(last) - RSS(first)| <= 256 MB
  (RSS = valgrind process, including the tool; a relative growth signal).
- Input-error recovery (1x): a deck with a non-existent boundary file must fail
  cleanly (exit != 0, no norm_end, message in PRINT), after which one recovery
  run with the good deck, in a fresh directory, must meet the per-run criteria.
- Output: CSV with per-run values + final verdict PASS/FAIL (exit code).

The two_cases deck takes its bottom file (and swaninit, when there is one)
from <build>/two_cases, which the two_cases ctest leaves behind; run that test
first.

Usage: python3 scripts/lifetime_protocol.py --swan <build>/bin/swan.exe
            [--csv <path>] [--runs 50] [--warmup 2]
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
    # With --show-leak-kinds=all, a missing leak record means zero. Valgrind
    # writes large byte and block counts with thousands separators.
    out = {
        "errors": -1,
        "def_lost_b": 0,
        "def_lost_n": 0,
        "ind_lost_b": 0,
        "still_reach_b": 0,
        "open_fds": -1,
    }
    m = re.search(r"ERROR SUMMARY: ([\d,]+) errors", text)
    if m:
        out["errors"] = int(m.group(1).replace(",", ""))
    m = re.search(r"definitely lost: ([\d,]+) bytes in ([\d,]+) blocks", text)
    if m:
        out["def_lost_b"] = int(m.group(1).replace(",", ""))
        out["def_lost_n"] = int(m.group(2).replace(",", ""))
    m = re.search(r"indirectly lost: ([\d,]+) bytes in ([\d,]+) blocks", text)
    if m:
        out["ind_lost_b"] = int(m.group(1).replace(",", ""))
    m = re.search(r"still reachable: ([\d,]+) bytes in ([\d,]+) blocks", text)
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
    ap.add_argument("--csv", default="lifetime_protocol.csv",
                    help="CSV output; a relative path is taken from the repository root")
    ap.add_argument("--runs", type=int, default=50)
    ap.add_argument("--warmup", type=int, default=2)
    args = ap.parse_args()

    swan = Path(args.swan).resolve()
    if not swan.is_file():
        print(f"no swan.exe: {swan}", file=sys.stderr)
        return 2
    for tool in ("valgrind", "/usr/bin/time"):
        if shutil.which(tool) is None:
            print(f"not found: {tool}", file=sys.stderr)
            return 2

    qt = REPO / "examples" / "quick_test"
    # The two_cases ctest leaves its swaninit and bottom in <build>/two_cases.
    two_cases = swan.parent.parent / "two_cases"
    swaninit_src = two_cases / "swaninit"
    if not swaninit_src.is_file():
        swaninit_src = qt / "swaninit"
    bottom_qt = qt / "bottom.bot"
    bottom_tc = two_cases / "bottom.bot"
    if not bottom_tc.is_file():
        print(f"no two_cases bottom: {bottom_tc} (run the two_cases ctest first)",
              file=sys.stderr)
        return 2

    decks = {
        "quick_test": (qt / "quick_test.swn", bottom_qt),
        "two_cases": (None, bottom_tc),  # INPUT is the embedded text
    }

    rows: list[dict] = []
    failures: list[str] = []
    base = Path(tempfile.mkdtemp(prefix="lifetime-"))
    print(f"work directory: {base}")

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
            # -1 means the Valgrind log lacked the record: no evidence, no pass.
            if row["errors"] < 0:
                failures.append(f"run {idx}: no ERROR SUMMARY in the valgrind log")
            if row["open_fds"] < 0:
                failures.append(f"run {idx}: no FILE DESCRIPTORS count in the valgrind log")
        return row

    idx = 1
    for _ in range(args.warmup):
        run_one(idx, "quick_test" if idx % 2 else "two_cases", judged=False)
        idx += 1
    for _ in range(args.runs):
        run_one(idx, "quick_test" if idx % 2 else "two_cases", judged=True)
        idx += 1

    sr = [r["still_reach_b"] for r in rows if r["judged"] and r["still_reach_b"] >= 0]
    if sr:
        if abs(sr[-1] - sr[0]) > 4096:
            failures.append(f"still_reachable growth {abs(sr[-1]-sr[0])} B > 4096 B")
        if max(sr) - min(sr) > 32768:
            failures.append(f"still_reachable band {max(sr)-min(sr)} B > 32768 B")
    rss = [r["rss_kb"] for r in rows if r["judged"] and r["rss_kb"] > 0]
    if rss and abs(rss[-1] - rss[0]) > 256 * 1024:
        failures.append(f"RSS growth {abs(rss[-1]-rss[0])} kB > 256 MB")

    # Input-error recovery (1x)
    fault = base / "fault"
    prepare_dir(fault, swaninit_src, bottom_tc, FAULT_INPUT)
    fproc = subprocess.run([str(swan)], cwd=fault, capture_output=True, timeout=300)
    fault_print = fault / "PRINT"
    fault_ok = (fproc.returncode != 0
                and not (fault / "norm_end").is_file()
                and fault_print.is_file()
                and "missing_bnd.dat" in fault_print.read_text(errors="replace"))
    if not fault_ok:
        failures.append("input error: did not fail cleanly")
    rows.append({"run": idx, "deck": "INPUT_ERROR", "judged": True,
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

    print(f"\njudged runs: {args.runs}, warm-up: {args.warmup}")
    if sr:
        print(f"still_reachable first/last: {sr[0]}/{sr[-1]} B, band {max(sr)-min(sr)} B")
    else:
        print("still_reachable: no records (all blocks freed)")
    if rss:
        print(f"RSS first/last: {rss[0]}/{rss[-1]} kB")
    print(f"input error failed cleanly: {fault_ok}, recovery green: "
          f"{rec['swan_exit'] == 0 and rec['norm_end'] and rec['errors'] == 0}")
    if failures:
        print("FAIL:")
        for f in failures:
            print(f"  - {f}")
        print(f"CSV: {csv_path}")
        return 1
    print("PASS — all criteria met")
    print(f"CSV: {csv_path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
