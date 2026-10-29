#!/usr/bin/env python3
"""Compare upstream stopping criteria with independently checked long runs.

Every condition starts from the same initial state. All candidates share the
same references; NPNTS=101 keeps those from stopping early. The results hold
only for the physics and output points used. This is not a speed measurement:
with runs in parallel, the recorded run times are for the record only.
"""
from __future__ import annotations

import argparse
from dataclasses import asdict
import json
import os
from pathlib import Path

import convergence_check as check
import deck
import matrix_runner


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--executable', type=Path, required=True)
    parser.add_argument('--conditions', nargs='+')
    parser.add_argument('--output-directory', type=Path, required=True)
    parser.add_argument('--reference-iterations', type=int, default=100)
    parser.add_argument('--tolerance', type=float, default=0.01)
    parser.add_argument('--candidate-mxitst', type=int, default=200)
    parser.add_argument('--candidates', nargs='+', default=['operational', 'stopc', 'accur-strict', 'stopc-strict'],
                        choices=['operational', 'stopc', 'accur-strict', 'stopc-strict', 'accur-tight'])
    parser.add_argument('--physics', choices=deck.PHYSICS, default='operational')
    parser.add_argument('--template', choices=sorted(deck.TEMPLATES), default='corrected')
    args = parser.parse_args()
    if args.reference_iterations < 1 or not 0 < args.tolerance:
        parser.error('invalid reference or tolerance')
    conditions = [item.condition for item in matrix_runner.load_manifest().conditions]
    if args.conditions:
        unknown = set(args.conditions) - {c.condition_id for c in conditions}
        if unknown:
            parser.error(f'unknown conditions: {sorted(unknown)}')
        conditions = [c for c in conditions if c.condition_id in args.conditions]
    args.output_directory.mkdir(parents=True, exist_ok=True)
    results = []
    for condition in conditions:
        root = args.output_directory / condition.condition_id
        text = deck.generate_deck(condition, physics=args.physics, template=args.template)
        references = []
        for n in (args.reference_iterations, 2*args.reference_iterations):
            label = f'reference{n}'
            print(f'{condition.condition_id}: {label}', flush=True)
            variant = check.build_variants(text, 0.01, n)[1]
            references.append(check.run_once(label, variant, args.executable.resolve(), root/label))
        (ref, a), (long, b) = references
        delta, rms, wet = check.compare_hs(a, b)
        problem = check.reference_problem(ref, long)
        record = {'condition': condition.condition_id, 'reference': asdict(ref),
                  'reference_check': asdict(long), 'reference_problem': problem,
                  'reference_hs_max_abs_m': delta, 'reference_hs_rms_m': rms,
                  'reference_other_differences': check.other_differences(a, b), 'candidates': {}}
        (root/'result.json').write_text(json.dumps(record, indent=2)+'\n')
        for label in args.candidates:
            print(f'{condition.condition_id}: {label}', flush=True)
            variant = check.candidate_deck(text, label, args.candidate_mxitst)
            outcome, table = check.run_once(label, variant, args.executable.resolve(), root/label)
            largest, rms, wet = check.compare_hs(table, b)
            lower, upper = check.deviation_with_margin(table, a, b)
            verdict, explanation = check.judge(outcome, problem, lower, upper, largest, delta,
                                               args.tolerance, long.iterations)
            record['candidates'][label] = {
                'run': asdict(outcome), 'verdict': verdict, 'explanation': explanation,
                'hs_max_abs_m': largest, 'hs_rms_m': rms, 'wet_points': wet,
                'hs_mean_m': sum(r[3] for r in table if r[3] >= 0)/wet if wet else None,
                'other_differences': check.other_differences(table, b),
            }
            (root/'result.json').write_text(json.dumps(record, indent=2)+'\n')
            print(f'{condition.condition_id}: {label}: {verdict}, max dHs={largest}', flush=True)
        results.append(record)
    report = {k: str(v) if isinstance(v, Path) else v for k,v in vars(args).items()}
    report.update(executable_sha256=check.sha256(args.executable), results=results,
                  environment={k:v for k,v in os.environ.items() if k.startswith('OMP_')})
    (args.output_directory/'study.json').write_text(json.dumps(report, indent=2)+'\n')
    return 0 if all(c['verdict']=='converged' for r in results for c in r['candidates'].values()) else 1


if __name__ == '__main__':
    raise SystemExit(main())
