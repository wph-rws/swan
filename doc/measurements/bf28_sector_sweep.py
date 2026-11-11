#!/usr/bin/env python3
"""The sweep of 120 directional sectors behind the rows BF-28 and BF-69 in
doc/bug-fixes-vs-tu-delft-41.51.md. Since BF-69, a case whose boundary waves
come from outside the sector stops with a terminating error and exit code 1;
it still counts as failed here.

The deck is `examples/qcm/qcm.swn` (`GEN4`, the quasi-coherent model) with its
`CIRCLE` replaced by `SECTOR start start+width mdc` (mdc = width/10, at least
2), for starts every 30 degrees, widths 30, 50, 90, 120 and 200 degrees and
grid rotations 0 and 10 degrees, and the bottom grid widened so that it covers
the rotated grid. Wind and boundary waves come from 270 degrees (nautical, as
in the deck), or with `--from-sector-centre` from the middle of each sector.
Every case is run with the fork and with upstream, one thread each.

    python3 doc/measurements/bf28_sector_sweep.py --fork FORK_EXE \\
        --upstream UPSTREAM_EXE --work /tmp/bf28 [--from-sector-centre] \\
        [--cpus 20-21] [--jobs 2] [--starts 0,30] [--widths 30] [--rotations 0]

A case fails when the exit code is not 0 or `norm_end` is missing. The script
prints the failures per program, whether every fork failure also fails
upstream, and whether Hs at the centre point is the same in every case that
upstream completes. The results go to WORK/sweep.json. One case takes a few
seconds.
"""
from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[2]
QCM = REPOSITORY / "examples/qcm"
CIRCLE = "CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 4 2 CIRCLE 12 0.05 1.0 8"
BOTTOM = "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0"


def deck(start: int, width: int, rotation: int, from_centre: bool) -> str:
    text = (QCM / "qcm.swn").read_text()
    assert CIRCLE in text and BOTTOM in text
    mdc = max(2, round(width / 10))
    end = (start + width) % 360
    text = text.replace(CIRCLE, f"CGRID REGULAR 0.0 0.0 {rotation:.1f} 2000.0 1000.0 4 2 "
                                f"SECTOR {start:.1f} {end:.1f} {mdc} 0.05 1.0 8")
    text = text.replace(BOTTOM, "INPGRID BOTTOM REGULAR -1000.0 -1000.0 0.0 1 1 4000.0 3000.0")
    if from_centre:
        centre = (start + width / 2) % 360
        assert "WIND 8.0 270.0" in text and "PAR 1.0 6.0 270.0 20.0" in text
        text = text.replace("WIND 8.0 270.0", f"WIND 8.0 {centre:.1f}")
        text = text.replace("PAR 1.0 6.0 270.0 20.0", f"PAR 1.0 6.0 {centre:.1f} 20.0")
    return text


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--fork", type=Path, required=True, help="fork executable")
    parser.add_argument("--upstream", type=Path, required=True, help="upstream executable")
    parser.add_argument("--work", type=Path, required=True, help="work directory")
    parser.add_argument("--from-sector-centre", action="store_true",
                        help="wind and waves from the middle of each sector instead of from 270")
    parser.add_argument("--cpus", default="20-21", help="CPUs for taskset")
    parser.add_argument("--jobs", type=int, default=2, help="cases at once")
    parser.add_argument("--starts", default=",".join(str(s) for s in range(0, 360, 30)))
    parser.add_argument("--widths", default="30,50,90,120,200")
    parser.add_argument("--rotations", default="0,10")
    args = parser.parse_args()
    exe = {"fork": args.fork.resolve(), "upstream": args.upstream.resolve()}
    numbers = lambda text: [int(x) for x in text.split(",")]  # noqa: E731
    cases = [(who, s, w, r) for who in ("upstream", "fork") for s in numbers(args.starts)
             for w in numbers(args.widths) for r in numbers(args.rotations)]

    def job(case):
        who, start, width, rotation = case
        directory = args.work / f"{who}-s{start}-w{width}-r{rotation}"
        shutil.rmtree(directory, ignore_errors=True)
        directory.mkdir(parents=True)
        shutil.copy2(QCM / "bottom.bot", directory)
        (directory / "INPUT").write_text(deck(start, width, rotation, args.from_sector_centre))
        try:
            done = subprocess.run(["taskset", "-c", args.cpus, str(exe[who])], cwd=directory,
                                  capture_output=True, text=True, timeout=600,
                                  env=dict(os.environ, OMP_NUM_THREADS="1"))
            rc = done.returncode
        except subprocess.TimeoutExpired:
            rc = "TIMEOUT"
        table = directory / "qcm_center.tbl"
        hs = None
        if table.exists():
            rows = [l.split() for l in table.read_text().splitlines() if l.strip() and not l.startswith("%")]
            hs = rows[-1][3] if rows else None
        ok = rc == 0 and (directory / "norm_end").exists()
        return {"who": who, "start": start, "width": width, "rot": rotation, "rc": rc, "ok": ok, "hs": hs}

    with ThreadPoolExecutor(args.jobs) as pool:
        results = list(pool.map(job, cases))
    (args.work / "sweep.json").write_text(json.dumps(results, indent=0))
    by = {who: {(r["start"], r["width"], r["rot"]): r for r in results if r["who"] == who}
          for who in ("upstream", "fork")}
    for who, rows in by.items():
        failed = sorted(k for k, r in rows.items() if not r["ok"])
        print(f"{who}: {len(rows)} cases, {len(failed)} failed")
    fork_only = [k for k, r in by["fork"].items() if not r["ok"] and by["upstream"][k]["ok"]]
    print("fork failures that upstream completes:", fork_only or "none")
    same = [k for k, u in by["upstream"].items() if u["ok"] and by["fork"][k]["hs"] == u["hs"]]
    differ = [k for k, u in by["upstream"].items() if u["ok"] and by["fork"][k]["hs"] != u["hs"]]
    print(f"cases upstream completes: Hs equal in the fork {len(same)}, different {len(differ)}", differ)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
