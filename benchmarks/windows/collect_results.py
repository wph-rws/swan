#!/usr/bin/env python3
"""Collect the timings of all series of a measurement round into one file.

  collect_results.py RESULTS_DIR OUTPUT.json

speed_vs_upstream.ps1 writes one summary-<series>.json per series, in RESULTS_DIR/<series>/. Besides
the timings such a file holds the paths of the programs and the names of the busiest processes of the
machine at that moment. This script keeps the timings, the logical CPUs, the power source and the load
before and after, and leaves the rest out, so that the result can be kept with the source.
"""
import json
import sys
from pathlib import Path

results, target = Path(sys.argv[1]), Path(sys.argv[2])
out = {}
for f in sorted(results.glob("*/summary-*.json")):
    j = json.loads(f.read_text(encoding="utf-8-sig"))
    entry = {
        "logical_cpus": [{"logical_cpu": c["logical_cpu"], "performance_core": c["performance_core"]}
                         for c in j.get("logical_cpus", [])],
        "machine": [{"when": m["when"], "time": m["time"], "on_mains": m["on_mains"],
                     "load_percent": m.get("load_percent")} for m in j.get("machine", [])],
        "speed": j.get("speed"),
        "all_runs_same_answer": j.get("speed_all_runs_same_answer"),
        "failed_runs": j.get("failed_runs"),
    }
    if j.get("residual"):
        entry["residual"] = j["residual"]
    out[f.parent.name] = entry
target.write_text(json.dumps(out, indent=1) + "\n")
print(len(out), "series written to", target)
