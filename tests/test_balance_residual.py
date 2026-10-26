"""Check that the optional balance measurement leaves the default path untouched."""
from pathlib import Path
import os
import re
import shutil
import subprocess

import pytest

from swan_executable import swan_executable

ROOT = Path(__file__).resolve().parents[1]
CASE = ROOT / 'examples/quick_test'


def run(tmp_path, name, option, command="NUM STOPC STAT MXITST=30", unstructured=False,
        current=False):
    exe = swan_executable()
    work = tmp_path / name
    work.mkdir()
    shutil.copy(CASE / 'bottom.bot', work)
    text = (CASE / 'quick_test.swn').read_text().replace(
        'COMPUTE', f'{command} {option}\nCOMPUTE')
    if unstructured:
        text = text.replace('CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10 CIRCLE',
                            "CGRID UNSTRUCTURED CIRCLE")
        text = text.replace('$ A 2 x 2 input grid', "READGRID UNSTRUCTURED TRIANGLE 'mesh'\n$ A 2 x 2 input grid")
        text = text.replace('BOUNDSPEC SIDE WEST', 'BOUNDSPEC SIDE 1')
        nodes = ['15 2 0 1']
        elements = []
        for iy in range(3):
            for ix in range(5):
                marker = 1 if ix == 0 else (2 if ix == 4 or iy in (0, 2) else 0)
                nodes.append(f'{iy*5+ix+1} {ix*500} {iy*500} {marker}')
        for iy in range(2):
            for ix in range(4):
                a = iy*5+ix+1
                for triangle in ((a,a+1,a+6),(a,a+6,a+5)):
                    elements.append(f"{len(elements)+1} " + ' '.join(map(str, triangle)))
        (work/'mesh.node').write_text('\n'.join(nodes)+'\n')
        (work/'mesh.ele').write_text(f'{len(elements)} 3 0\n'+'\n'.join(elements)+'\n')
    if current:
        # a current that speeds up eastwards shifts the frequencies, so the
        # solver takes the SIP or SOLMT1 path instead of SOLMAT
        (work / 'cur.dat').write_text('0.0 -2.0\n0.0 -2.0\n0.0 0.0\n0.0 0.0\n')
        text = text.replace(
            "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE",
            "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE\n"
            "INPGRID CURRENT REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0\n"
            "READINP CURRENT 1.0 'cur.dat' 3 0 FREE")
    (work / 'INPUT').write_text(text)
    proc = subprocess.run([exe], cwd=work, capture_output=True, text=True,
                          env=dict(os.environ, OMP_NUM_THREADS='2'), timeout=60)
    report = (work / 'PRINT').read_text()
    return work, proc, report


def iterations(report):
    return max(map(int, re.findall(r'iteration\s+(\d+)', report)))


@pytest.mark.parametrize('unstructured', [False, True])
def test_diagnostic_is_opt_in_and_does_not_change_solution(tmp_path, unstructured):
    off, p, a = run(tmp_path, 'off', '', unstructured=unstructured)
    on, q, b = run(tmp_path, 'diagnostic', 'RESIDUAL 0', unstructured=unstructured)
    assert p.returncode == q.returncode == 0
    assert 'balance residual' not in a
    assert 'balance residual energy rms' in b
    assert iterations(a) == iterations(b)
    for name in ('quick_test_hs.blk', 'quick_test_center.tbl'):
        assert (off / name).read_bytes() == (on / name).read_bytes()


def test_positive_threshold_can_prevent_early_stop(tmp_path):
    _, _, a = run(tmp_path, 'off', '')
    _, p, b = run(tmp_path, 'threshold', 'RESIDUAL 1.e-12')
    assert p.returncode == 0
    assert iterations(a) < 30
    assert iterations(b) == 30


def test_negative_threshold_is_rejected(tmp_path):
    work, _, report = run(tmp_path, 'negative', 'RESIDUAL -0.1')
    assert not (work / 'norm_end').exists()
    assert not re.search(r'iteration\s+\d+', report)
    assert 'must be non-negative' in report


@pytest.mark.parametrize('criterion', ['ACCUR', 'STOPC'])
def test_reference_cannot_stop_early(tmp_path, criterion):
    _, p, report = run(tmp_path, 'reference', '',
                       command=f'NUM {criterion} NPNTS=101 STAT MXITST=7 ALFA=0.01')
    assert p.returncode == 0
    assert iterations(report) == 7
    assert '101.00 % required' in report


#  With a current the solver couples the frequencies (SIP, implicit) or
#  leaves out the bins blocked by the current (SOLMT1, explicit). The residual
#  follows the system each of them solves, and it does not touch the solution.
@pytest.mark.parametrize('scheme', ['SIGIMPL', 'SIGEXPL'])
def test_residual_with_current_and_scheme_in_frequency_space(tmp_path, scheme):
    off, p, a = run(tmp_path, 'off', scheme, current=True)
    on, q, b = run(tmp_path, 'diagnostic', f'RESIDUAL 0 {scheme}', current=True)
    assert p.returncode == q.returncode == 0
    assert 'not available' not in b
    assert 'balance residual energy rms' in b
    assert iterations(a) == iterations(b)
    for name in ('quick_test_hs.blk', 'quick_test_center.tbl'):
        assert (off / name).read_bytes() == (on / name).read_bytes()
