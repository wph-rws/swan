#!/usr/bin/env python3
"""Afsluitende CI-controle: slaagt alleen als ieder vereist resultaat
aanwezig én geslaagd is.

Iedere vereiste job schrijft een JSON-artefact ``{"job": ..., "status": ...}``
met status ``passed``. De gate leest een directory met die artefacten en faalt
bij een ontbrekend artefact, een niet-``passed`` status of een onbekend
artefact. Een workflowbestand alleen dwingt ontbrekende controles niet af;
deze gate wel: publicatie hangt van de gate af (zie .github/workflows/ci.yml).

Gebruik:
    python3 scripts/ci_gate.py --results-dir DIR --required a b c
    python3 scripts/ci_gate.py --self-test  (negatieve paden)
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path


def check(results_dir: Path, required: list[str]) -> list[str]:
    failures: list[str] = []
    seen: set[str] = set()
    for artefact in sorted(results_dir.glob("*.json")):
        try:
            payload = json.loads(artefact.read_text())
        except (OSError, json.JSONDecodeError) as exc:
            failures.append(f"{artefact.name}: onleesbaar ({exc})")
            continue
        job = payload.get("job", artefact.stem)
        seen.add(str(job))
        if payload.get("status") != "passed":
            failures.append(
                f"{job}: status is {payload.get('status')!r}, geen 'passed'"
            )
    for job in required:
        if job not in seen:
            failures.append(f"{job}: vereist resultaat ontbreekt")
    return failures



def check_workflow(workflow: Path) -> list[str]:
    """Schrijft de workflow de woordenschat die deze gate accepteert?

    De jobs melden hun uitkomst zelf. GitHub noemt een geslaagde job
    ``success``; deze gate eist ``passed``. Dat verschil blijft onzichtbaar tot
    de workflow echt draait, en laat dan alle zes de jobs slagen terwijl de
    gate ze afwijst. Controleer daarom statisch dat elke statusregel de vertaling
    bevat in plaats van de kale GitHub-status.
    """
    tekst = workflow.read_text()
    regels = [r for r in tekst.split("\n") if '"status":' in r and "ci-results/" in r]
    if not regels:
        return [f"{workflow}: geen enkele job schrijft een resultaat weg"]
    fouten = []
    for regel in regels:
        if "'passed'" not in regel:
            fouten.append(
                f"schrijft de kale GitHub-status in plaats van 'passed': {regel.strip()[:80]}"
            )
    return fouten


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results-dir", default="ci-results")
    parser.add_argument("--required", nargs="*", default=None)
    parser.add_argument("--self-test", action="store_true")
    parser.add_argument("--check-workflow", default=None)
    arguments = parser.parse_args()
    if arguments.self_test:
        return _self_test()
    if arguments.check_workflow:
        fouten = check_workflow(Path(arguments.check_workflow))
        if fouten:
            print("CI-gate workflowcontract faalt:", file=sys.stderr)
            for fout in fouten:
                print(f"  - {fout}", file=sys.stderr)
            return 1
        print("CI-gate workflowcontract: PASS (alle jobs melden 'passed')")
        return 0
    required = arguments.required or [
        "serial",
        "openmp",
        "runtime",
        "strict",
        "mpi-2rank",
        "python-manifest",
    ]
    failures = check(Path(arguments.results_dir), required)
    if failures:
        print("CI-gate faalt:", file=sys.stderr)
        for failure in failures:
            print(f"  - {failure}", file=sys.stderr)
        return 1
    print(f"CI-gate groen: {len(required)} vereiste resultaten aanwezig en geslaagd.")
    return 0


def _self_test() -> int:
    import tempfile

    failures = 0

    def scenario(files: dict[str, dict], required: list[str]) -> list[str]:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for name, payload in files.items():
                (root / f"{name}.json").write_text(json.dumps(payload))
            return check(root, required)

    # 1. alles aanwezig en geslaagd -> groen
    if scenario(
        {j: {"job": j, "status": "passed"} for j in ("a", "b")}, ["a", "b"]
    ):
        print("self-test faalt: groen scenario werd rood", file=sys.stderr)
        failures += 1
    # 2. weggelaten job -> rood
    if not scenario({"a": {"job": "a", "status": "passed"}}, ["a", "b"]):
        print("self-test faalt: weggelaten job werd groen", file=sys.stderr)
        failures += 1
    # 3. gefaalde job -> rood
    if not scenario(
        {"a": {"job": "a", "status": "failed"}, "b": {"job": "b", "status": "passed"}},
        ["a", "b"],
    ):
        print("self-test faalt: gefaalde job werd groen", file=sys.stderr)
        failures += 1
    # 4. corrupt artefact -> rood
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        (root / "a.json").write_text("{geen json")
        if not check(root, ["a"]):
            print("self-test faalt: corrupt artefact werd groen", file=sys.stderr)
            failures += 1
    print("ci_gate self-test groen" if not failures else "ci_gate self-test ROOD")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
