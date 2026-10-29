#!/usr/bin/env python3
"""Check stationary convergence against references that must run to a fixed length.

ACCUR and STOPC judge the change between iterations; their own "accuracy OK"
message is not an independent test. The references use the upstream input
NPNTS=101 so that they cannot stop early: two runs from the same initial
state, of N and 2N iterations. The longer one is the reference; whatever Hs
still moves between N and 2N counts, point by point, as its uncertainty. An
operational run stopped too early if at some point it is off by more than
the tolerance even after subtracting that uncertainty, and it is fine if it
stays within the tolerance everywhere even with the uncertainty added; in
between there is no verdict. A reference that drifts away from its own
equilibrium gives no verdict. The check covers Hs at the wet output points;
differences in period and direction are reported separately. A stable Hs is
no proof that the whole spectrum has converged.

Verdicts: converged, premature, not-converged, inconclusive. Inconclusive also
returns a non-zero exit code: an unusable reference is not a passed check.
Existing result directories are never overwritten.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import re
import subprocess
import time
from dataclasses import dataclass, asdict
from pathlib import Path

import deck as deck_module
import matrix_runner

HERE = Path(__file__).resolve().parent
# A number as SWAN reads it, including Fortran D exponents.
NUMBER = r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][-+]?\d+)?"
MXITST_PATTERN = re.compile(r"MXITST\s*=\s*(\d+)", re.IGNORECASE)
ACCURACY_PATTERN = re.compile(
    r"accuracy OK in\s+([0-9.]+)\s*% of wet grid points\s*\(\s*([0-9.]+)\s*% required"
)
ITERATION_PATTERN = re.compile(r"iteration\s+(\d+)", re.IGNORECASE)
HS_COLUMN = deck_module.COLUMN["hs"]


@dataclass
class RunOutcome:
    label: str
    seconds: float
    iterations: int
    accuracy: float | None
    required: float | None
    met_criterion: bool
    hit_cap: bool
    max_iterations: int | None
    # only with --residual: the energy residual at the last iteration and the
    # lowest one the run reached (the floor of the deck)
    residual_final: float | None = None
    residual_lowest: float | None = None


@dataclass
class ConditionOutcome:
    condition: str
    verdict: str
    explanation: str
    operational: RunOutcome
    reference: RunOutcome
    reference_check: RunOutcome
    reference_usable: bool
    # largest Hs shift between the references of N and 2N iterations
    reference_hs_max_abs_m: float | None
    hs_max_abs_m: float | None
    hs_rms_m: float | None
    wet_points: int
    other_differences: dict
    reference_other_differences: dict
    # largest Hs change in the last iteration of the longer reference
    reference_hs_last_change_max_m: float | None = None
    # per point, the deviation minus and plus the uncertainty of the reference;
    # the largest of each
    hs_deviation_minus_margin_max_m: float | None = None
    hs_deviation_plus_margin_max_m: float | None = None


def numeric_command(text: str) -> tuple[str, str]:
    # Take only the NUMERIC command, including continuation lines; a $ starts a comment.
    lines = text.splitlines(keepends=True)
    commands = []
    for i, line in enumerate(lines):
        if re.match(r"^[ \t]*NUM\w*\s+(ACCUR|STOPC)\b", line, re.I):
            original = line
            while lines[i].split("$", 1)[0].rstrip().endswith("&"):
                i += 1
                if i >= len(lines):
                    raise ValueError("incomplete line continuation in NUMERIC")
                original += lines[i]
            commands.append(original.rstrip("\n"))
    if len(commands) != 1:
        raise ValueError("exactly one NUM ACCUR or NUM STOPC command is required")
    original = commands[0]
    command = " ".join(line.split("$", 1)[0].replace("&", " ").strip()
                       for line in original.splitlines())
    return original, command


def stationary_tail(command: str) -> str:
    parts = re.split(r"\bSTAT(?:IONARY)?\b", command, maxsplit=1, flags=re.I)
    if len(parts) != 2:
        raise ValueError("an explicit STAT with named MXITST and ALFA is required")
    tail = parts[1].strip()
    if re.match(NUMBER, tail):
        raise ValueError("use MXITST= and ALFA= instead of positional STAT values")
    return tail


def set_stationary(tail: str, name: str, value: float | int) -> str:
    pattern = rf"\b{name}\s*=\s*{NUMBER}"
    if re.search(pattern, tail, re.I):
        return re.sub(pattern, f"{name}={value}", tail, flags=re.I)
    # Insert before LIMITER and later options: ALFA belongs to STAT.
    return f"{name}={value} " + tail


def reference_numerics(line: str, alfa: float, mxitst: int) -> str:
    """An unreachable percentage keeps the reference from stopping early."""
    if not math.isfinite(alfa) or alfa < 0 or mxitst < 1:
        raise ValueError("invalid reference settings")
    _, command = numeric_command(line)
    kind = re.match(r"\s*NUM\w*\s+(ACCUR|STOPC)", command, re.I).group(1).upper()
    tail = stationary_tail(command)
    # A residual threshold is specific to this fork and does not belong in an upstream reference deck.
    tail = re.sub(rf"\bRESID(?:UAL)?\s+(?:RTOL\s*=\s*)?{NUMBER}", "", tail, flags=re.I)
    tail = set_stationary(tail, "MXITST", mxitst)
    tail = set_stationary(tail, "ALFA", alfa)
    return f"NUM {kind} NPNTS=101 STAT {tail}"


def with_residual(deck_text: str) -> str:
    """Switch on only the measurement of the balance residual; the solution does not change."""
    original, _ = numeric_command(deck_text)
    return deck_text.replace(original, original.rstrip() + " RESIDUAL 0", 1)


def build_variants(deck_text: str, alfa: float, mxitst: int) -> tuple[str, str]:
    original, _ = numeric_command(deck_text)
    return deck_text, deck_text.replace(original, reference_numerics(original, alfa, mxitst), 1)


def candidate_deck(text: str, choice: str, cap: int = 200) -> str:
    if choice == "operational":
        return text
    original, command = numeric_command(text)
    criteria = {
        "stopc": "STOPC DABS=0.005 DREL=0.01 CURVAT=0.005 NPNTS=99.5",
        "accur-strict": "ACCUR DREL=0.001 DHOVAL=0.001 DTOVAL=0.001 NPNTS=99.5",
        "accur-tight": "ACCUR DREL=0.0001 DHOVAL=0.0001 DTOVAL=0.0001 NPNTS=99.5",
        "stopc-strict": "STOPC DABS=0.0005 DREL=0.001 CURVAT=0.0005 NPNTS=99.5",
    }
    tail = set_stationary(stationary_tail(command), "MXITST", cap)
    return text.replace(original, f"NUM {criteria[choice]} STAT {tail}", 1)


def parse_print(print_file: Path, log_file: Path) -> tuple[int, float | None, float | None]:
    report = print_file.read_text(encoding="utf-8", errors="replace")
    history = ACCURACY_PATTERN.findall(report)
    accuracy, required = (map(float, history[-1]) if history else (None, None))
    iterations = 0
    for source in (log_file, print_file):
        found = ITERATION_PATTERN.findall(source.read_text(encoding="utf-8", errors="replace"))
        if found:
            iterations = max(iterations, max(map(int, found)))
    return iterations, accuracy, required


def deck_max_iterations(deck_text: str) -> int | None:
    match = MXITST_PATTERN.search(deck_text)
    return int(match.group(1)) if match else None


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run_once(label: str, deck_text: str, executable: Path,
             work: Path) -> tuple[RunOutcome, list[list[float]]]:
    work.mkdir(parents=True, exist_ok=False)
    deck_module.stage_inputs(work)
    (work / "INPUT").write_text(deck_text)
    executable_hash = sha256(executable)
    started = time.monotonic()
    with (work / "screen.log").open("w") as screen:
        process = subprocess.run([str(executable)], cwd=work, stdout=screen,
                                 stderr=subprocess.STDOUT, check=False)
    elapsed = time.monotonic() - started
    if process.returncode or not (work / "norm_end").is_file():
        raise RuntimeError(f"{label} did not finish; see {work}")
    iterations, accuracy, required = parse_print(work / "PRINT", work / "screen.log")
    cap = deck_max_iterations(deck_text)
    met = accuracy is not None and required is not None and accuracy >= required
    # A requested residual threshold must also be met; the accuracy line alone is not enough then.
    threshold = re.search(rf"\bRESID(?:UAL)?\s+(?:RTOL\s*=\s*)?({NUMBER})", deck_text, re.I)
    if threshold and float(threshold.group(1)) > 0:
        residuals = re.findall(r"balance residual energy rms\s+([0-9.Ee+-]+)",
                              (work / "PRINT").read_text())
        met = met and bool(residuals) and float(residuals[-1]) <= float(threshold.group(1))
    measured = [float(v) for v in re.findall(r"balance residual energy rms\s+([0-9.Ee+-]+)",
                                             (work / "PRINT").read_text())]
    outcome = RunOutcome(label, elapsed, iterations, accuracy, required, met,
                         cap is not None and iterations >= cap, cap,
                         measured[-1] if measured else None,
                         min(measured) if measured else None)
    provenance = {
        "outcome": asdict(outcome), "executable": str(executable),
        "executable_sha256": executable_hash,
        "files": {n: sha256(work / n) for n in
                  ("INPUT", *deck_module.INPUT_FILES, deck_module.TABLE)},
        "affinity": sorted(os.sched_getaffinity(0)),
        "environment": {k: v for k, v in os.environ.items() if k.startswith("OMP_")},
    }
    (work / "run.json").write_text(json.dumps(provenance, indent=2) + "\n")
    return outcome, read_table(work / deck_module.TABLE)


def read_table(path: Path) -> list[list[float]]:
    rows = []
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        try:
            row = [float(value) for value in stripped.split()]
        except ValueError as exc:
            raise ValueError(f"invalid table line in {path}: {line}") from exc
        if len(row) < 4 or not all(math.isfinite(v) for v in row):
            raise ValueError(f"incomplete or non-finite table line in {path}")
        rows.append(row)
    return rows


def comparable(a, b):
    if not a or len(a) != len(b):
        return False
    return all(len(x) >= 4 and len(y) >= 4 and x[:2] == y[:2]
               and all(math.isfinite(v) for v in x + y)
               and (x[3] >= 0) == (y[3] >= 0)
               for x, y in zip(a, b, strict=True))


def compare_hs(a: list[list[float]], b: list[list[float]]) -> tuple[float | None, float | None, int]:
    """Largest and rms deviation; shifted points or a different wet mask make the pair invalid."""
    if not comparable(a, b):
        return None, None, 0
    differences = [x[3] - y[3] for x, y in zip(a, b, strict=True) if x[3] >= 0]
    if not differences:
        return None, None, 0
    return max(map(abs, differences)), math.sqrt(sum(d*d for d in differences) / len(differences)), len(differences)


def other_differences(a, b) -> dict:
    if not comparable(a, b):
        return {}
    result = {}
    for name, key in (("tm01_s", "tm01"), ("tm02_s", "tm02"), ("direction_deg", "dir")):
        column = deck_module.COLUMN[key]
        differences = []
        for x, y in zip(a, b, strict=True):
            if min(len(x), len(y)) <= column or min(x[3], y[3]) < 0.05:
                continue
            if x[column] in (-9, -999) or y[column] in (-9, -999):
                continue
            d = x[column] - y[column]
            if key == "dir":
                d = (d + 180) % 360 - 180
            differences.append(d)
        if differences:
            result[name] = {"max_abs": max(map(abs, differences)),
                            "rms": math.sqrt(sum(d*d for d in differences)/len(differences)),
                            "points": len(differences)}
    return result


def deviation_with_margin(operational, reference, longer) -> tuple[float | None, float | None]:
    """Per point |operational - long| minus and plus |short - long|; the largest of each.

    Whatever Hs still moves between the references of N and 2N iterations is
    the uncertainty of the longer one. As long as the run creeps towards its
    solution, what is still to come after 2N is not larger than that shift.
    """
    if not (comparable(operational, longer) and comparable(reference, longer)):
        return None, None
    rows = [(abs(x[3] - z[3]), abs(y[3] - z[3]))
            for x, y, z in zip(operational, reference, longer, strict=True) if z[3] >= 0]
    if not rows:
        return None, None
    return max(d - m for d, m in rows), max(d + m for d, m in rows)


# A reference that converges to an equilibrium ends close to the lowest residual
# of its own run (about 1.0 to 1.1 times it). If it ends well above that
# (1.5 times, say), the iteration has drifted away from its best state: it is
# then not a solution, and the difference between N and 2N
# iterations says nothing about its uncertainty.
WANDERING_RATIO = 1.25
# Column dHs: the change of Hs in the last iteration, per output point.
LAST_CHANGE_COLUMN = deck_module.COLUMN["dhs"]


def last_change(table: list[list[float]]) -> float | None:
    """Largest change of Hs in the last iteration over the wet points."""
    values = [row[LAST_CHANGE_COLUMN] for row in table
              if len(row) > LAST_CHANGE_COLUMN and row[HS_COLUMN] >= 0]
    return max(map(abs, values)) if values else None


def reference_problem(first: RunOutcome, second: RunOutcome) -> str | None:
    """Why the references cannot carry a verdict, or None if they can."""
    if not (first.hit_cap and second.hit_cap and first.required == second.required == 101
            and first.iterations == first.max_iterations
            and second.iterations == second.max_iterations
            and second.iterations > first.iterations):
        return "the references did not both run the required number of iterations"
    final, lowest = second.residual_final, second.residual_lowest
    if final is not None and lowest and final > WANDERING_RATIO * lowest:
        return (f"the residual of the longer reference ends at {final:.3g} after reaching "
                f"{lowest:.3g}: the iteration drifts away from its equilibrium, so the reference "
                "is not a solution, and running longer will not fix that")
    return None


def judge(operational: RunOutcome, problem: str | None, lower: float | None,
          upper: float | None, largest: float | None, margin: float | None,
          tolerance: float, longer_iterations: int | None = None) -> tuple[str, str]:
    if problem:
        return "inconclusive", problem
    if lower is None or upper is None or largest is None:
        return "inconclusive", "no comparable wet output points"
    if not operational.met_criterion:
        return "not-converged", f"the stopping criterion was not met after {operational.iterations} iterations"
    spread = f"reference uncertainty of up to {1000 * (margin or 0.):.1f} mm"
    if lower > tolerance:
        return "premature", (f"criterion met, but Hs is off by up to {largest:.4f} m; "
                             f"even after subtracting the {spread} that exceeds the tolerance of {tolerance:.4f} m")
    if upper <= tolerance:
        return "converged", (f"criterion met; largest Hs deviation {largest:.4f} m, within the "
                             f"tolerance of {tolerance:.4f} m even with the {spread}")
    advice = f" (--reference-mxitst {longer_iterations})" if longer_iterations else ""
    return "inconclusive", (f"the largest Hs deviation ({largest:.4f} m) lies within the {spread} "
                            f"around the tolerance of {tolerance:.4f} m; a longer reference{advice} "
                            "reduces that uncertainty")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executable", type=Path, required=True)
    parser.add_argument("--conditions", nargs="*", default=None)
    parser.add_argument("--physics", default="operational", choices=deck_module.PHYSICS)
    parser.add_argument("--template", choices=sorted(deck_module.TEMPLATES), default="corrected")
    parser.add_argument("--reference-alfa", type=float, default=0.01)
    parser.add_argument("--reference-mxitst", type=int, default=200,
                        help="the references run exactly N and 2N iterations")
    parser.add_argument("--tolerance", type=float, default=0.01)
    parser.add_argument("--candidate", choices=("operational", "stopc", "accur-strict", "stopc-strict", "accur-tight"),
                        default="operational")
    parser.add_argument("--candidate-mxitst", type=int, default=200)
    parser.add_argument("--output-directory", type=Path, required=True)
    parser.add_argument("--json", type=Path)
    parser.add_argument("--residual", action="store_true",
                        help="also measure the balance residual (RESIDUAL 0) in all runs; "
                        "this does not change the solution")
    args = parser.parse_args()
    if (args.reference_mxitst < 1 or args.candidate_mxitst < 1
            or not 0 < args.tolerance or not math.isfinite(args.tolerance)
            or not math.isfinite(args.reference_alfa) or args.reference_alfa < 0):
        parser.error("required: N>0, ALFA>=0 and a positive, finite tolerance")
    executable = args.executable.resolve()
    if not executable.is_file():
        parser.error(f"executable missing: {executable}")
    conditions = [item.condition for item in matrix_runner.load_manifest().conditions]
    if args.conditions:
        missing = set(args.conditions) - {c.condition_id for c in conditions}
        if missing:
            parser.error(f"unknown conditions: {', '.join(sorted(missing))}")
        conditions = [c for c in conditions if c.condition_id in args.conditions]
    results = []
    for condition in conditions:
        text = candidate_deck(deck_module.generate_deck(condition, physics=args.physics, template=args.template),
                              args.candidate, args.candidate_mxitst)
        root = args.output_directory / condition.condition_id
        runs = []
        for label, n in (("operational", None), ("reference", args.reference_mxitst),
                         ("reference_check", 2 * args.reference_mxitst)):
            variant = text if n is None else build_variants(text, args.reference_alfa, n)[1]
            if args.residual:
                variant = with_residual(variant)
            print(f"{condition.condition_id}: {label} ...", flush=True)
            runs.append(run_once(label, variant, executable, root / label))
        (op, a), (ref, b), (long, c) = runs
        ref_difference, _, _ = compare_hs(b, c)
        problem = reference_problem(ref, long)
        largest, rms, wet = compare_hs(a, c)
        lower, upper = deviation_with_margin(a, b, c)
        verdict, explanation = judge(op, problem, lower, upper, largest, ref_difference,
                                     args.tolerance, long.iterations)
        results.append(ConditionOutcome(condition.condition_id, verdict, explanation, op, ref, long,
                                        problem is None, ref_difference, largest, rms, wet,
                                        other_differences(a, c), other_differences(b, c),
                                        last_change(c), lower, upper))
        print(f"{condition.condition_id}: {verdict} — {explanation}", flush=True)
        if args.json:
            args.json.parent.mkdir(parents=True, exist_ok=True)
            args.json.write_text(json.dumps({
                "reference_alfa": args.reference_alfa, "reference_mxitst": args.reference_mxitst,
                "tolerance_m": args.tolerance,
                "scope": "Hs at the wet output points; other quantities reported only",
                "physics": args.physics, "template": args.template, "candidate": args.candidate,
                "residual": args.residual,
                "results": [asdict(item) for item in results],
            }, indent=2) + "\n")
    return 0 if all(item.verdict == "converged" for item in results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
