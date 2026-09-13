#!/usr/bin/env python3
"""Check whether a run really converged or merely stopped changing.

SWAN's stationary stopping test measures how much Hs and Tm change between
iterations. Under-relaxation (the [alfa] of false time stepping) makes that
change small on purpose, so the test cannot tell a converged solution from a
heavily damped one that is still far from the balance. There is no residual to
settle it, and SWAN's only safeguard is a single iteration: the criterion may
not fire on the first one.

That matters here because nearly every deck in this repository sets ALFA.
Measured on examples/nonlinear_interactions/quad/quad_dia1: at ALFA = 0.01 the
run converges in 40 iterations, while at ALFA = 0.05 and 0.10 it stops after
two iterations claiming 100% of the wet points -- with Hs at 25 km at 1.278 m
instead of the 1.405 m that the converged run gives.

Without a residual the only reliable check is a comparison. This script runs
each condition twice: once with the deck's own numerics, and once with a
reference that weakens the under-relaxation and allows far more iterations. If
the operational run claims convergence but lands somewhere else than the
reference, it stopped early.

The verdicts are deliberately conservative:

  converged        the run met its criterion and agrees with the reference;
  premature        the run met its criterion but disagrees with the reference;
  not-converged    the run never met its criterion, it ran out of iterations;
  inconclusive     the reference did not converge either, so there is nothing
                   trustworthy to compare against.

"inconclusive" is a real outcome, not a failure of the script. It means the
case needs a residual or a better reference, and saying so is more useful than
a verdict the evidence does not support.

Usage:
    convergence_check.py --executable /path/to/swan.exe
                         [--conditions ID ...] [--physics NAME]
                         [--reference-alfa X] [--reference-mxitst N]
                         [--output-directory DIR] [--json FILE]
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass, asdict
from pathlib import Path

import deck as deck_module
import matrix_runner

HERE = Path(__file__).resolve().parent
CASE = HERE.parent / "runs" / "current"

#     NUM ACCUR [drel] [dhoval] [dtoval] [npnts] STAT MXITST=.. ALFA=.. LIMITER=..
NUMERIC_PATTERN = re.compile(r"^\s*NUM\w*\s+ACCUR\b.*$", re.MULTILINE)
MXITST_PATTERN = re.compile(r"MXITST\s*=\s*\d+", re.IGNORECASE)
ALFA_PATTERN = re.compile(r"ALFA\s*=\s*[0-9.eE+-]+", re.IGNORECASE)
ACCURACY_PATTERN = re.compile(
    r"accuracy OK in\s+([0-9.]+)\s*% of wet grid points\s*\(\s*([0-9.]+)\s*% required"
)
ITERATION_PATTERN = re.compile(r"iteration\s+(\d+)", re.IGNORECASE)

#     uitvoerpunten.tab: XP YP DEP HS PER TM01 TM02 RTP DIR DSPR FSPR DHSIGN
HS_COLUMN = 3


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


@dataclass
class ConditionOutcome:
    condition: str
    verdict: str
    explanation: str
    operational: RunOutcome
    reference: RunOutcome
    hs_max_abs_m: float | None
    hs_rms_m: float | None
    wet_points: int | None


def reference_numerics(line: str, alfa: float, mxitst: int) -> str:
    """The deck's own NUMERIC line with the under-relaxation weakened."""
    text = line
    text = MXITST_PATTERN.sub(f"MXITST={mxitst}", text)
    if ALFA_PATTERN.search(text):
        text = ALFA_PATTERN.sub(f"ALFA={alfa}", text)
    else:
        text = text.rstrip() + f" ALFA={alfa}"
    if "MXITST" not in text.upper():
        text = text.rstrip() + f" MXITST={mxitst}"
    return text


def build_variants(deck_text: str, alfa: float, mxitst: int) -> tuple[str, str]:
    match = NUMERIC_PATTERN.search(deck_text)
    if match is None:
        raise RuntimeError(
            "deck has no NUM ACCUR command; this check only applies to decks "
            "whose stopping criterion and under-relaxation are set explicitly"
        )
    reference = deck_text.replace(
        match.group(0), reference_numerics(match.group(0), alfa, mxitst)
    )
    return deck_text, reference


def parse_print(print_file: Path, log_file: Path) -> tuple[int, float | None, float | None]:
    report = print_file.read_text(encoding="utf-8", errors="replace")
    history = ACCURACY_PATTERN.findall(report)
    accuracy, required = (float(history[-1][0]), float(history[-1][1])) if history else (None, None)
    iterations = 0
    for source in (log_file, print_file):
        found = ITERATION_PATTERN.findall(
            source.read_text(encoding="utf-8", errors="replace")
        )
        if found:
            iterations = max(iterations, max(int(value) for value in found))
    return iterations, accuracy, required


def deck_max_iterations(deck_text: str) -> int | None:
    match = MXITST_PATTERN.search(deck_text)
    return int(match.group(0).split("=")[1]) if match else None


def run_once(
    label: str, deck_text: str, executable: Path, work: Path, inputs: Path
) -> tuple[RunOutcome, list[list[float]]]:
    work.mkdir(parents=True, exist_ok=True)
    for name in ("GRID", "DEPTH", "uitvoerpunten.par"):
        shutil.copy2(inputs / name, work / name)
    (work / "INPUT").write_text(deck_text)

    started = time.monotonic()
    process = subprocess.run(
        [str(executable)], cwd=work, capture_output=True, text=True, check=False
    )
    elapsed = time.monotonic() - started
    (work / "screen.log").write_text(process.stdout + process.stderr)

    if process.returncode or not (work / "norm_end").is_file():
        raise RuntimeError(f"{label} did not complete; inspect {work}")

    iterations, accuracy, required = parse_print(work / "PRINT", work / "screen.log")
    cap = deck_max_iterations(deck_text)
    met = accuracy is not None and required is not None and accuracy >= required
    outcome = RunOutcome(
        label=label,
        seconds=elapsed,
        iterations=iterations,
        accuracy=accuracy,
        required=required,
        met_criterion=met,
        hit_cap=cap is not None and iterations >= cap,
        max_iterations=cap,
    )
    return outcome, read_table(work / "uitvoerpunten.tab")


def read_table(path: Path) -> list[list[float]]:
    rows = []
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%"):
            continue
        try:
            rows.append([float(value) for value in stripped.split()])
        except ValueError:
            continue
    return rows


def compare_hs(a: list[list[float]], b: list[list[float]]) -> tuple[float | None, float | None, int]:
    """Largest and RMS difference in Hs over the points both runs report as wet."""
    if not a or not b or len(a) != len(b):
        return None, None, 0
    differences = [
        row_a[HS_COLUMN] - row_b[HS_COLUMN]
        for row_a, row_b in zip(a, b, strict=True)
        if row_a[HS_COLUMN] >= 0.0 and row_b[HS_COLUMN] >= 0.0
    ]
    if not differences:
        return None, None, 0
    largest = max(abs(value) for value in differences)
    rms = (sum(value * value for value in differences) / len(differences)) ** 0.5
    return largest, rms, len(differences)


def judge(
    operational: RunOutcome, reference: RunOutcome, largest: float | None, tolerance: float
) -> tuple[str, str]:
    if not reference.met_criterion:
        return (
            "inconclusive",
            "the reference run did not converge either, so there is nothing "
            "trustworthy to compare against; this condition needs a residual "
            "or a slower reference",
        )
    if not operational.met_criterion:
        return (
            "not-converged",
            f"the run never met its criterion: {operational.accuracy:.2f}% of wet "
            f"points against {operational.required:.2f}% required, after "
            f"{operational.iterations} iterations",
        )
    if largest is None:
        return "inconclusive", "the two runs do not report a comparable set of points"
    if largest > tolerance:
        return (
            "premature",
            f"the run claimed convergence but differs from the reference by "
            f"{largest:.4f} m in Hs, above the {tolerance:.4f} m tolerance; the "
            f"stopping test was satisfied by small changes, not by a small residual",
        )
    return (
        "converged",
        f"the run met its criterion and agrees with the reference to "
        f"{largest:.4f} m in Hs",
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--executable", type=Path, required=True)
    parser.add_argument("--conditions", nargs="*", default=None, help="condition ids; default all")
    parser.add_argument("--physics", default="default")
    parser.add_argument("--reference-alfa", type=float, default=0.0,
                        help="under-relaxation for the reference run (default 0, i.e. none)")
    parser.add_argument("--reference-mxitst", type=int, default=200,
                        help="iteration cap for the reference run (default 200)")
    parser.add_argument("--tolerance", type=float, default=0.01,
                        help="Hs difference in metres above which a claimed convergence counts as premature")
    parser.add_argument("--inputs", type=Path, default=CASE,
                        help="directory holding GRID, DEPTH and uitvoerpunten.par")
    parser.add_argument("--output-directory", type=Path, required=True)
    parser.add_argument("--json", type=Path, default=None)
    arguments = parser.parse_args()

    executable = arguments.executable.resolve()
    if not executable.is_file():
        print(f"executable not found: {executable}", file=sys.stderr)
        return 2

    manifest = matrix_runner.load_manifest()
    conditions = [item.condition for item in manifest.conditions]
    if arguments.conditions:
        wanted = set(arguments.conditions)
        selected = [c for c in conditions if c.condition_id in wanted]
        missing = wanted - {c.condition_id for c in selected}
        if missing:
            print(f"unknown condition(s): {', '.join(sorted(missing))}", file=sys.stderr)
            return 2
    else:
        selected = conditions

    results: list[ConditionOutcome] = []
    for condition in selected:
        text = deck_module.generate_deck(condition, physics=arguments.physics)
        operational_deck, reference_deck = build_variants(
            text, arguments.reference_alfa, arguments.reference_mxitst
        )
        root = arguments.output_directory / condition.condition_id
        print(f"{condition.condition_id}: operational ...", flush=True)
        op_outcome, op_table = run_once(
            "operational", operational_deck, executable, root / "operational", arguments.inputs
        )
        print(f"{condition.condition_id}: reference ...", flush=True)
        ref_outcome, ref_table = run_once(
            "reference", reference_deck, executable, root / "reference", arguments.inputs
        )
        largest, rms, wet = compare_hs(op_table, ref_table)
        verdict, explanation = judge(op_outcome, ref_outcome, largest, arguments.tolerance)
        results.append(
            ConditionOutcome(
                condition=condition.condition_id,
                verdict=verdict,
                explanation=explanation,
                operational=op_outcome,
                reference=ref_outcome,
                hs_max_abs_m=largest,
                hs_rms_m=rms,
                wet_points=wet,
            )
        )
        print(f"{condition.condition_id}: {verdict} -- {explanation}", flush=True)

    print()
    print(f"{'condition':<26}{'verdict':<16}{'op iter':>8}{'op acc':>9}{'ref iter':>9}{'dHs max':>10}")
    for item in results:
        accuracy = f"{item.operational.accuracy:.2f}%" if item.operational.accuracy is not None else "-"
        largest = f"{item.hs_max_abs_m:.4f}" if item.hs_max_abs_m is not None else "-"
        print(
            f"{item.condition:<26}{item.verdict:<16}{item.operational.iterations:>8}"
            f"{accuracy:>9}{item.reference.iterations:>9}{largest:>10}"
        )

    if arguments.json:
        arguments.json.write_text(
            json.dumps(
                {
                    "reference_alfa": arguments.reference_alfa,
                    "reference_mxitst": arguments.reference_mxitst,
                    "tolerance_m": arguments.tolerance,
                    "physics": arguments.physics,
                    "results": [asdict(item) for item in results],
                },
                indent=2,
            )
            + "\n"
        )

    problems = [item for item in results if item.verdict in ("premature", "not-converged")]
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main())
