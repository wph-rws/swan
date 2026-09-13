#!/usr/bin/env python3
"""Final CI check: passes only if every required result is present
and has passed.

Every required job writes a JSON artefact ``{"job": ..., "status": ...}``
with status ``passed``. The gate reads a directory with those artefacts and
fails on a missing required artefact, an unreadable artefact, or any artefact
(required or not) whose status is not ``passed``. A workflow file on its own
does not enforce missing checks;
this gate does: publishing depends on the gate (see .github/workflows/ci.yml).

Usage:
    python3 scripts/ci_gate.py --results-dir DIR --required a b c
    python3 scripts/ci_gate.py --self-test  (negative paths)
    python3 scripts/ci_gate.py --check-workflow .github/workflows/ci.yml
"""
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path


#  The build variants run as matrix jobs in .github/workflows/ci.yml. Each
#  matrix entry reports as "variant-<name>"; an entry that disappears from the
#  workflow is then missing here and turns the gate red.
VARIANTS = [
    "variant-ffro",
    "variant-ffro-openmp",
    "variant-metis",
    "variant-coh",
    "variant-esmf",
    "variant-adcirc",
    "variant-netcdf",
    "variant-mpi-netcdf",
    "variant-metis-mpi",
    "variant-jac",
    "variant-coh-mpi",
]


def check(results_dir: Path, required: list[str]) -> list[str]:
    failures: list[str] = []
    seen: set[str] = set()
    for artefact in sorted(results_dir.glob("*.json")):
        try:
            payload = json.loads(artefact.read_text())
        except (OSError, json.JSONDecodeError) as exc:
            failures.append(f"{artefact.name}: unreadable ({exc})")
            continue
        if not isinstance(payload, dict):
            failures.append(f"{artefact.name}: not a JSON object")
            continue
        job = payload.get("job", artefact.stem)
        seen.add(str(job))
        if payload.get("status") != "passed":
            failures.append(
                f"{job}: status is {payload.get('status')!r}, not 'passed'"
            )
    for job in required:
        if job not in seen:
            failures.append(f"{job}: required result missing")
    return failures


def check_workflow(workflow: Path) -> list[str]:
    """Does the workflow write the vocabulary that this gate accepts?

    The jobs report their own outcome. GitHub calls a successful job
    ``success``; this gate requires ``passed``. That difference stays invisible
    until the workflow really runs, and then lets every job pass while the
    gate rejects them. So check statically that every status line contains the
    translation instead of the bare GitHub status, and that the build variants
    in the workflow matrix match VARIANTS.
    """
    text = workflow.read_text()
    lines = [r for r in text.split("\n") if '"status":' in r and "ci-results/" in r]
    if not lines:
        return [f"{workflow}: no job writes a result"]
    errors = []
    for line in lines:
        if "'passed'" not in line:
            errors.append(
                f"writes the bare GitHub status instead of 'passed': {line.strip()[:80]}"
            )
    #  The matrix entries and the required list must match: a variant that is
    #  only in the workflow is not required by the gate; one that is only
    #  here always makes the gate fail.
    in_workflow = {f"variant-{name}" for name in
                   re.findall(r"^\s*- name: ([\w-]+)\s*\n\s+flags:", text, re.MULTILINE)}
    for name in sorted(in_workflow - set(VARIANTS)):
        errors.append(f"matrix entry {name} is not in the gate's VARIANTS")
    for name in sorted(set(VARIANTS) - in_workflow):
        errors.append(f"required {name} has no matrix entry in the workflow")
    return errors


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
        errors = check_workflow(Path(arguments.check_workflow))
        if errors:
            print("CI gate workflow contract fails:", file=sys.stderr)
            for error in errors:
                print(f"  - {error}", file=sys.stderr)
            return 1
        print("CI gate workflow contract: PASS (all jobs report 'passed')")
        return 0
    required = arguments.required or [
        "serial",
        "openmp",
        "runtime",
        "strict",
        "mpi-2rank",
        *VARIANTS,
        "python-manifest",
    ]
    failures = check(Path(arguments.results_dir), required)
    if failures:
        print("CI gate fails:", file=sys.stderr)
        for failure in failures:
            print(f"  - {failure}", file=sys.stderr)
        return 1
    print(f"CI gate green: {len(required)} required results present and passed.")
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

    # 1. everything present and passed -> green
    if scenario(
        {j: {"job": j, "status": "passed"} for j in ("a", "b")}, ["a", "b"]
    ):
        print("self-test fails: green scenario came out red", file=sys.stderr)
        failures += 1
    # 2. omitted job -> red
    if not scenario({"a": {"job": "a", "status": "passed"}}, ["a", "b"]):
        print("self-test fails: omitted job came out green", file=sys.stderr)
        failures += 1
    # 3. failed job -> red
    if not scenario(
        {"a": {"job": "a", "status": "failed"}, "b": {"job": "b", "status": "passed"}},
        ["a", "b"],
    ):
        print("self-test fails: failed job came out green", file=sys.stderr)
        failures += 1
    # 4. corrupt artefact -> red
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        (root / "a.json").write_text("{not json")
        if not check(root, ["a"]):
            print("self-test fails: corrupt artefact came out green", file=sys.stderr)
            failures += 1
    # 5. matrix entry without a required result -> red
    with tempfile.TemporaryDirectory() as tmp:
        workflow = Path(tmp) / "ci.yml"
        lines = [f"          - name: {name.removeprefix('variant-')}\n"
                  "            flags: -DX=ON" for name in VARIANTS]
        lines.append("          - name: new\n            flags: -DY=ON")
        lines.append("          echo '{\"status\": \"${{ job.status == 'success'"
                      " && 'passed' || job.status }}\"}' > ci-results/x.json")
        workflow.write_text("\n".join(lines) + "\n")
        if not check_workflow(workflow):
            print("self-test fails: matrix entry outside the gate came out green",
                  file=sys.stderr)
            failures += 1
    print("ci_gate self-test green" if not failures else "ci_gate self-test RED")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
