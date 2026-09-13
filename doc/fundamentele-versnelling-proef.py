"""Exploratory numerical experiments; all runs use fresh directories.

Usage: python3 experiment.py REPO EXECUTABLE OUTPUT_DIRECTORY
Requires NumPy. No source changes or model outputs in REPO are made.
"""
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import subprocess
import sys
import time

import numpy as np

repo, exe, root = (Path(a).resolve() for a in sys.argv[1:])
root.mkdir(parents=True, exist_ok=True)
base = (repo / 'examples/voordelta/voordelta.swn').read_text()
cpu = min(os.sched_getaffinity(0))
env = dict(os.environ, OMP_NUM_THREADS='1', OPENBLAS_NUM_THREADS='1')

def digest(p):
    return hashlib.sha256(p.read_bytes()).hexdigest()

data = {
    'date_utc': time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()),
    'head': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=repo, text=True).strip(),
    'initial_worktree_diff': subprocess.check_output(['git', 'diff'], cwd=repo, text=True),
    'executable': str(exe), 'exe_sha256': digest(exe),
    'cpu': cpu, 'platform': platform.platform(),
    'base_deck_sha256': digest(repo / 'examples/voordelta/voordelta.swn'),
    'depth_sha256': digest(repo / 'examples/voordelta/voordelta.dep'),
    'method': 'One serial process pinned to one CPU. All timings include process startup and output, including binary hotfile writing. Preparation and copying input hotfiles excluded. Wind direction, boundary, grid and physics package fixed. Exploratory measurements, not release qualification.',
    'runs': [], 'comparisons': [],
}
if (root / 'results.json').exists():
    previous_data = json.loads((root / 'results.json').read_text())
    for key in ['exe_sha256', 'base_deck_sha256', 'depth_sha256']:
        if data[key] != previous_data[key]:
            raise RuntimeError(f'Cannot resume with changed {key}; choose a new output directory')
    data = previous_data

def save():
    (root / 'results.json').write_text(json.dumps(data, indent=2) + '\n')

def run(label, wind=15, quad=2, hot=None, strict=False, hs_tight=False):
    for previous in data['runs']:
        if previous['label'] == label:
            return previous
    work = root / label
    work.mkdir()
    shutil.copy2(repo / 'examples/voordelta/voordelta.dep', work / 'voordelta.dep')
    deck = base.replace('WIND 15.0 270.0', f'WIND {wind:.1f} 270.0')
    numeric = 'NUMERIC STOPC STAT MXITST=100'
    if hs_tight:
        numeric = 'NUMERIC STOPC DABS=0.005 DREL=0.001 CURVAT=0.001 NPNTS=99.5 STAT MXITST=100'
    if strict:
        numeric = 'NUMERIC STOPC DABS=0.0001 DREL=0.0001 CURVAT=0.0001 NPNTS=99.5 DTABS=0.001 CURVT=0.0001 STAT MXITST=200'
    deck = deck.replace('GEN3 KOMEN', f'GEN3 KOMEN\nQUADRUPL {quad}\n{numeric}')
    if hot:
        shutil.copy2(root / hot / 'end.hot', work / 'start.hot')
        deck = deck.replace('\nCOMPUTE\n', "\nINITIAL HOTSTART SINGLE 'start.hot' UNFORMATTED\nCOMPUTE\n")
    deck = deck.replace('\nSTOP\n', "\nHOTFILE 'end.hot' UNFORMATTED\nSTOP\n")
    (work / 'INPUT').write_text(deck)
    print('start', label, flush=True)
    start = time.perf_counter()
    with (work / 'screen.log').open('w') as log:
        result = subprocess.run(['taskset', '-c', str(cpu), str(exe)], cwd=work, env=env, stdout=log, stderr=subprocess.STDOUT, timeout=600)
    elapsed = time.perf_counter() - start
    report = (work / 'PRINT').read_text(errors='replace')
    if result.returncode or not (work / 'norm_end').is_file():
        entry = {'label': label, 'wind': wind, 'quad': quad, 'hot_from': hot, 'strict': strict,
                 'success': False, 'seconds': elapsed, 'return_code': result.returncode,
                 'screen_tail': (work / 'screen.log').read_text(errors='replace')[-3500:], 'deck': deck}
        data['runs'].append(entry)
        save()
        print('failed', label, result.returncode, flush=True)
        return entry
    iterations = max(map(int, re.findall(r'iteration\s+(\d+)\s*;', report, re.I)))
    acc = re.findall(r'accuracy OK in\s+([0-9.]+)', report)
    cap = 200 if strict else 100
    entry = {'label': label, 'success': True, 'wind': wind, 'quad': quad, 'hot_from': hot, 'strict': strict, 'hs_tight': hs_tight,
             'seconds': elapsed, 'iterations': iterations, 'max_iterations': cap,
             'reached_stop_criterion': bool(acc and float(acc[-1]) >= 99.5),
             'accuracy_percent': list(map(float, acc)), 'deck': deck,
             'output_hashes': {name: digest(work / name) for name in ['voordelta_hs.blk','voordelta_depth.blk','voordelta_sites.tbl','end.hot']}}
    data['runs'].append(entry)
    save()
    print('done', label, round(elapsed, 3), 's;', iterations, 'iterations; accuracy', acc[-1] if acc else '?', flush=True)
    return entry

def read_hot(path):
    # SWAN serial UNFORMATTED hotfile written by this GNU Fortran build.
    records = []
    with path.open('rb') as f:
        while tag := f.read(4):
            n = int(np.frombuffer(tag, dtype='<i4')[0])
            value = f.read(n)
            if f.read(4) != tag:
                raise ValueError('unexpected Fortran record markers')
            records.append(value)
    def integer(record):
        return int(np.frombuffer(record, dtype='<i4')[0])
    assert integer(records[2]) == 0, 'stationary hotfile expected'
    assert integer(records[3]) != 5, 'structured grid expected'
    count = integer(records[4])
    idx = 5 + count
    nf = integer(records[idx]); idx += 1
    freq = np.array([np.frombuffer(r, '<f4')[0] for r in records[idx:idx+nf]])
    idx += nf
    nd = integer(records[idx]); idx += 1 + nd
    spectra, points = [], []
    for point in range(count):
        node = integer(records[idx]); idx += 1
        if node != 1:
            spec = np.frombuffer(records[idx], '<f4').reshape(nf, nd)
            idx += 1
            spectra.append(spec)
            points.append(point)
    assert idx == len(records)
    return freq, np.array(points), np.stack(spectra)

def compare(label, reference):
    if any(c['label'] == label and c['reference'] == reference for c in data['comparisons']):
        return
    a = np.fromstring((root / label / 'voordelta_hs.blk').read_text(), sep=' ')
    b = np.fromstring((root / reference / 'voordelta_hs.blk').read_text(), sep=' ')
    wet = b >= 0
    assert np.array_equal(a >= 0, wet)
    diff = a[wet] - b[wet]
    f, ia, sa = read_hot(root / label / 'end.hot')
    fb, ib, sb = read_hot(root / reference / 'end.hot')
    assert np.array_equal(f, fb) and np.array_equal(ia, ib)
    assert np.isfinite(sa).all() and np.isfinite(sb).all()
    # On SWAN's log frequency grid the energy quadrature weight for
    # action density is proportional to sigma**2. Common constants cancel.
    weights = f.astype(float)**2
    denom = np.einsum('pfd,f->p', np.abs(sb), weights)
    specdiff = np.einsum('pfd,f->p', np.abs(sa.astype(float)-sb), weights)
    active = denom > 1e-12
    relative = specdiff[active] / denom[active]
    entry = {'label': label, 'reference': reference, 'wet_output_points': int(wet.sum()),
             'hs_mean_reference_m': float(b[wet].mean()), 'hs_mean_bias_m': float(diff.mean()),
             'hs_rmse_m': float(np.sqrt(np.mean(diff**2))), 'hs_max_abs_m': float(np.abs(diff).max()),
             'hs_p99_abs_m': float(np.quantile(np.abs(diff), .99)),
             'spectrum_point_count': len(sa), 'nonzero_reference_spectra': int(active.sum()),
             'energy_weighted_spectrum_l1_relative_global': float(specdiff.sum()/denom.sum()),
             'energy_weighted_spectrum_l1_relative_p99': float(np.quantile(relative, .99)),
             'energy_weighted_spectrum_l1_relative_max': float(relative.max()),
             'minimum_action': float(sa.min())}
    data['comparisons'].append(entry)
    save()
    print('comparison', json.dumps(entry), flush=True)

run('q2')
if run('q1', quad=1).get('success', True):
    compare('q1', 'q2')
if run('q3', quad=3).get('success', True):
    compare('q3', 'q2')
run('u16_cold', wind=16)
run('u16_warm', wind=16, hot='q2')
compare('u16_warm', 'u16_cold')
run('u16_strict_cold', wind=16, strict=True)
run('u16_strict_warm', wind=16, hot='q2', strict=True)
compare('u16_strict_warm', 'u16_strict_cold')
compare('u16_cold', 'u16_strict_cold')
compare('u16_warm', 'u16_strict_cold')
run('u16_hs_cold', wind=16, hs_tight=True)
run('u16_hs_warm', wind=16, hot='q2', hs_tight=True)
compare('u16_hs_warm', 'u16_hs_cold')
