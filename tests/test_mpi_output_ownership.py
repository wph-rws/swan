#!/usr/bin/env python3
"""Output interpolation must retain wet corners next to leading land (BF-40).

The lower-left corner of an output cell may lie outside every trimmed
subdomain while another corner is wet. Compare TABLE, FRAME/BLOCK and
SPECOUT with one and several MPI processes, including the first active row.

Two layouts are run. In 'narrow', three rows and columns of land precede the
wet area. That is no wider than the halo of three cells, so the first
subdomain still reaches the edge of the grid; fractional points between land
and the first wet row are checked there. In 'wide' and 'wide-y', five rows and
columns of land precede the wet area. `SWBLADM` then cuts the first subdomain
off two cells from the edge, so the node before the first wet column belongs
to no process. Before BF-40, a point exactly on the first wet column counted
as a point of the cell to its left and was written by nobody: TABLE, BLOCK and
SPECOUT showed the exception value there with two or more processes, while the
'narrow' layout passed. 'wide' cuts the grid into strips in x (more points in
x), 'wide-y' is the same layout transposed, cut into strips in y.

In the wide layouts the fractional points lie only along the strips. A
fractional point across the strips, between the leading land and the first
wet column, is still lost with two or more processes: its lower-left corner
lies in that unowned land. That is a known open issue (see the bug list,
"MPI loses output points next to wide leading land"), not checked here.

CTest runs this file as a script (`mpi_output_ownership`); pytest collects no
tests from it.
"""
from pathlib import Path
import argparse
import subprocess
import shutil

ROOT = Path(__file__).resolve().parents[1]

#  (land width in cells, transposed, POINTS coordinates, FRAME origin)
LAYOUTS = {
    'narrow': (3, False, '250 450 450 250 250 250 300 300 450 450', '250 250'),
    #  500 is the first wet column and row; 650 and 450 are fractional
    #  along the strips only.
    'wide': (5, False, '500 750 500 500 500 650 750 500 650 450 650 650', '500 450'),
    #  The same points with x and y swapped.
    'wide-y': (5, True, '750 500 500 500 650 500 500 750 450 650 650 650', '450 500'),
}


def stage(work, layout):
    land, transposed, points, origin = LAYOUTS[layout]
    work.mkdir(parents=True, exist_ok=True)
    deck = (ROOT / 'examples/quick_test/quick_test.swn').read_text()
    if transposed:
        deck = deck.replace('CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10',
                            'CGRID REGULAR 0.0 0.0 0.0 1000.0 2000.0 10 20')
        bottom = 'INPGRID BOTTOM REGULAR 0 0 0 10 20 100 100 EXCEPTION -99'
        nx, ny = 11, 21
    else:
        bottom = 'INPGRID BOTTOM REGULAR 0 0 0 20 10 100 100 EXCEPTION -99'
        nx, ny = 21, 11
    deck = deck.replace('INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0', bottom)
    deck = deck.replace('BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0', '')
    deck = deck.replace("POINTS 'CENTER' 1000.0 500.0", f"POINTS 'CENTER' {points}")
    deck = deck.replace("TABLE 'CENTER' HEADER", "TABLE 'CENTER' NOHEADER")
    deck = deck.replace('\nCOMPUTE\n', f"\nFRAME 'F' {origin} 0 200 200 4 4\n"
                         "BLOCK 'F' NOHEADER 'frame.blk' DEPTH HSIGN\n"
                         "SPECOUT 'CENTER' SPEC2D ABS 'points.spc'\nCOMPUTE\n")
    (work / 'INPUT').write_text(deck)
    (work / 'bottom.bot').write_text('\n'.join(' '.join('-99' if i < land or j < land else '20'
        for i in range(nx)) for j in range(ny))+'\n')


def outputs(work):
    # Only descriptive header lines differ between standalone and MPI output.
    return {name: '\n'.join(line.rstrip() for line in (work / name).read_text().splitlines()
                            if not line.startswith(('$', '%')))
            for name in ('quick_test_center.tbl', 'frame.blk', 'points.spc')}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--swan-executable', required=True)
    ap.add_argument('--mpi-exec', required=True)
    ap.add_argument('--mpi-numproc-flag', default='-n')
    ap.add_argument('--work-directory', required=True)
    args = ap.parse_args()
    failures = []
    for layout in LAYOUTS:
        baseline = None
        for np in (1, 2, 3, 4):
            work = Path(args.work_directory) / layout / f'np{np}'
            if work.exists(): shutil.rmtree(work)
            stage(work, layout)
            proc = subprocess.run([args.mpi_exec, args.mpi_numproc_flag, str(np), args.swan_executable],
                                  cwd=work, capture_output=True, text=True, timeout=120)
            assert proc.returncode == 0, proc.stderr[-2000:]
            found = outputs(work)
            if baseline is None:
                baseline = found
                #  Every point has a wet corner, so the run on one process
                #  interpolates a depth of 20 m and a wave height at each.
                rows = found['quick_test_center.tbl'].splitlines()
                assert all(float(r.split()[2]) == 20. and float(r.split()[3]) > 0.
                           for r in rows), rows
            else:
                for name in baseline:
                    if found[name] != baseline[name]:
                        failures.append(f'{layout}: {np} processes differ in {name}: {work}')
    assert not failures, '\n'.join(failures)
    print(f'TABLE, FRAME/BLOCK and SPECOUT agree on 1, 2, 3 and 4 processes '
          f'({", ".join(LAYOUTS)})')


if __name__ == '__main__':
    main()
