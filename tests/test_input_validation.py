"""Unusual or faulty input must give the documented result or a clear message.

Each test writes a small deck on the spot, mostly the smoke deck with one
command added or changed. These decks do not belong under examples/: those are
valid physics cases that also have reference output.
"""
from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

import pytest

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
QUICK_TEST = REPO / "examples" / "quick_test"


def _swan() -> str:
    return swan_executable()


def _run(tmp_path: Path, extra_command: str) -> tuple[int, str]:
    """Run the smoke deck with one extra command in it."""
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "GEN3 KOMEN" in deck, "smoke deck has changed shape"
    deck = deck.replace("GEN3 KOMEN", f"GEN3 KOMEN\n{extra_command}", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")

    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    printfile = work / "PRINT"
    report = printfile.read_text(encoding="utf-8", errors="replace") if printfile.is_file() else ""
    return result.returncode, report


#  Numbers that name no formulation. Upstream accepts them: the run prints the
#  chosen formulation in its header, switches on the matching bookkeeping
#  through the `ITRIAD .GT. 0` and `IQUAD .GE. 1` gates (Ursell, biphase,
#  MEMNL4, the limiter) and computes no source term. TRIAD reaches its numeric
#  branch only through the named parameter: `TRIAD 4` already fails in INKEYW,
#  `TRIAD ITRIAD=4` does not. For [lambda], FAC4WW divides by (1-lambda)**4 and
#  takes LOG(1-lambda), so lambda = 1 divides by zero and lambda > 1 takes the
#  logarithm of a negative number; both end in INT() of a NaN as an allocation
#  bound.
REJECTED = [
    ("TRIAD ITRIAD=4", "TRIAD [itriad]"),
    ("TRIAD ITRIAD=6", "TRIAD [itriad]"),
    ("QUADRUPLET 6", "QUADRUPLET [iquad]"),
    ("QUADRUPLET 9", "QUADRUPLET [iquad]"),
    ("QUADRUPLET 1 1.0", "[lambda]"),
    ("QUADRUPLET 1 1.5", "[lambda]"),
    ("QUADRUPLET 1 0.0", "[lambda]"),
    ("QUADRUPLET MDIA LAMBDA 1.0 CNL4 3.0E7", "[lambda]"),
    #  The filter scheme in frequency space was never built: nothing applies
    #  DIFFC and the frequency terms exist only for schemes 1 and 2,
    #  so the frequency shift silently dropped out.
    ("NUMERIC FIL", "FIL is not implemented"),
]


@pytest.mark.parametrize("command,expected", REJECTED)
def test_unknown_formulation_is_rejected(tmp_path, command, expected):
    returncode, report = _run(tmp_path, command)
    assert returncode != 0 or "No start of computation" in report, (
        f"'{command}' ran on to a normal end; a number that does not name a "
        f"formulation should stop the run.\n{report[-2000:]}"
    )
    assert expected in report, (
        f"'{command}' stopped, but without the message that says why.\n"
        f"{report[-2000:]}"
    )


#  The counter-test. Without it, a check that rejects everything would also pass.
ACCEPTED = [
    "TRIAD ITRIAD=11",
    "TRIAD ITRIAD=1",
    "TRIAD LTA",
    "TRIAD DCTA",
    "QUADRUPLET 2",
    "QUADRUPLET 8",
    "QUADRUPLET 1 0.25",
    "QUADRUPLET MDIA LAMBDA 0.25 CNL4 3.0E7",
    "NUMERIC SIGIMPL",
    "NUMERIC SIGEXPL",
]


@pytest.mark.parametrize("command", ACCEPTED)
def test_valid_formulation_keeps_running(tmp_path, command):
    returncode, report = _run(tmp_path, command)
    assert returncode == 0, (
        f"'{command}' is valid but was refused.\n{report[-2000:]}"
    )


#  `COMPUTE STATionary` is the documented form for a stationary run.
#  SWREAD tested `NSTATM.LE.0 .OR. KEYWIS('STAT')`; in a stationary run
#  the left-hand side is already true, and gfortran then skips the right. STAT
#  was therefore left unused and the next command reported
#  'Illegal keyword'. An Intel build evaluates both sides and reported nothing.
@pytest.mark.parametrize("compute", ["COMPUTE STAT", "COMPUTE STATIONARY"])
def test_compute_stationary_is_valid(tmp_path, compute):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "\nCOMPUTE\n" in deck, "smoke deck has changed shape"
    (work / "INPUT").write_text(deck.replace("\nCOMPUTE\n", f"\n{compute}\n"),
                                encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 0
    assert "Illegal keyword" not in report, report[-2000:]


def _run_cgrid(tmp_path: Path, spectral: str, extra_command: str = "") -> tuple[int, str]:
    """Run the smoke deck with a different spectral grid in CGRID."""
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "CIRCLE 24 0.05 1.0 16" in deck, "smoke deck has changed shape"
    deck = deck.replace("CIRCLE 24 0.05 1.0 16", f"CIRCLE 24 {spectral}", 1)
    deck = deck.replace("GEN3 KOMEN", f"GEN3 KOMEN\n{extra_command}", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    printfile = work / "PRINT"
    report = printfile.read_text(encoding="utf-8", errors="replace") if printfile.is_file() else ""
    return result.returncode, report


#  Too few frequencies. Without a frequency interval SWREAD divides by zero;
#  FAC4WW fixes the DIA bins with SPCSIG(MSC/2-1) and with fewer than four
#  frequencies reads outside the table. Upstream only tested MSC = 3 and let
#  the rest compute on silently to a 'normal' end with Hs = 0.
TOO_FEW_FREQUENCIES = [
    ("0.05 1.0 0", "[msc] in CGRID must be at least 1"),
    ("0.05 1.0 -3", "[msc] in CGRID must be at least 1"),
    ("0.05 0.052", "[msc] in CGRID must be at least 1"),
    ("0.05 1.0 1", "Quadruplets need at least four frequencies"),
    ("0.05 1.0 2", "Do not activate quadruplets"),
]


@pytest.mark.parametrize("spectral,expected", TOO_FEW_FREQUENCIES)
def test_too_few_frequencies_are_rejected(tmp_path, spectral, expected):
    returncode, report = _run_cgrid(tmp_path, spectral)
    assert returncode != 0, f"'{spectral}' ran on to a normal end.\n{report[-2000:]}"
    assert expected in report, report[-2000:]


#  Without quadruplets SWAN reports a level 2 error in third-generation mode
#  with wind ("Quadruplets should be activated ..."). The manual allows such
#  an error with SET MAXERR, so the test raises it and expects SWAN to compute.
def test_two_frequencies_without_quadruplets_keep_running(tmp_path):
    returncode, report = _run_cgrid(tmp_path, "0.05 1.0 1", "OFF QUAD\nSET MAXERR 2")
    assert returncode == 0, report[-2000:]
    assert "No start of computation" not in report, report[-2000:]


#  "If the variables are in one file, then the READINP commands should be
#  given in the same sequence as the sequence in which the variables appear
#  in the file" (manual, READINP). REPARM did recognise the file, but
#  did not pass the unit and format on to the next quantity: upstream silently
#  did not read the current, the fork stopped on 'Error while reading file stderr'.
#  Moreover, REPARM kept only 36 characters of the name, so that a longer
#  name never counted as the same file ('File is already opened').
def _run_fields(tmp_path: Path, files: dict[str, str], readinp: str) -> tuple[int, str, str]:
    work = tmp_path / "work"
    work.mkdir(parents=True)
    for name, content in files.items():
        (work / name).write_text(content, encoding="utf-8")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(old, readinp, 1)
    deck = deck.replace("XP YP DEPTH HSIGN TM01 DIR", "XP YP DEPTH HSIGN TM01 DIR VEL", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    table = work / "quick_test_center.tbl"
    return result.returncode, report, table.read_text() if table.is_file() else ""


BOTTOM = "20.0 20.0\n20.0 20.0\n"
CURRENT = "0.8 0.8\n0.8 0.8\n0.0 0.0\n0.0 0.0\n"
CURRENT_GRID = "INPGRID CURRENT REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0"


@pytest.mark.parametrize("name", ["fields.dat",
                                  "input_fields_bottom_and_current_voordelta.dat"])
def test_several_quantities_from_one_file(tmp_path, name):
    code, report, separate = _run_fields(
        tmp_path / "separate", {"bottom.dat": BOTTOM, "current.dat": CURRENT},
        f"READINP BOTTOM 1.0 'bottom.dat' 3 0 FREE\n{CURRENT_GRID}\n"
        "READINP CURRENT 1.0 'current.dat' 3 0 FREE")
    assert code == 0, report[-2000:]
    assert "0.800000" in separate, "counter-test does not read the current"

    code, report, combined = _run_fields(
        tmp_path / "combined", {name: BOTTOM + CURRENT},
        f"READINP BOTTOM 1.0 '{name}' 3 0 FREE\n{CURRENT_GRID}\n"
        f"READINP CURRENT 1.0 '{name}'")
    assert code == 0, report[-2000:]
    assert "** Error" not in report, report[-2000:]
    assert combined == separate


#  QCM (GEN4) determines the bounds of the wavenumber grid for a sector from
#  the first direction, normalised to [0, 2pi). The chain started at
#  'dir1 > 0.', so that a sector starting exactly at Cartesian 0 degrees
#  (nautical: ending at 270) got no bounds and crashed. On a rotated
#  grid SSFILL does not shift that direction, so it can be reached there.
QCM = REPO / "examples" / "qcm"


def _run_qcm_sector(tmp_path: Path, last_direction: str,
                    spectral: str | None = None) -> tuple[int, str]:
    work = tmp_path / (spectral or last_direction).replace(" ", "_")
    work.mkdir(parents=True)
    shutil.copy2(QCM / "bottom.bot", work / "bottom.bot")
    deck = (QCM / "qcm.swn").read_text(encoding="utf-8")
    spectral = spectral or f"SECTOR 180.0 {last_direction} 12"
    replacements = {
        "CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 4 2 CIRCLE 12 0.05 1.0 8":
            f"CGRID REGULAR 0.0 0.0 10.0 2000.0 1000.0 4 2 {spectral} 0.05 1.0 8",
        "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0":
            "INPGRID BOTTOM REGULAR -1000.0 -1000.0 0.0 1 1 4000.0 3000.0",
    }
    for old, new in replacements.items():
        assert old in deck, "QCM deck has changed shape"
        deck = deck.replace(old, new, 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    table = work / "qcm_center.tbl"
    return result.returncode, table.read_text() if table.is_file() else ""


def test_qcm_sector_from_cartesian_zero(tmp_path):
    code, exact = _run_qcm_sector(tmp_path, "270.0")
    assert code == 0, "QCM sector from 0 degrees did not run to the end"
    code, shifted = _run_qcm_sector(tmp_path, "269.999")
    assert code == 0
    hs = [float(t.strip().splitlines()[-1].split()[3]) for t in (exact, shifted)]
    assert abs(hs[0] - hs[1]) < 1e-3 * hs[1], hs


#  A sector that starts in the second or third quadrant and runs across
#  Cartesian 0 (nautical 240 to 70 is Cartesian 200 to 30). The old
#  quadrant analysis compared the reduced end direction with thresholds that
#  were not meant for it; the wavenumber grid then did not enclose the sector,
#  and both upstream and the fork ended with a segmentation fault. The
#  sector contains the incoming direction, so Hs must lie close to the full
#  circle (measured 0.721 against 0.742 m).
def test_qcm_sector_across_cartesian_zero(tmp_path):
    code, sector = _run_qcm_sector(tmp_path, "", "SECTOR 240.0 70.0 19")
    assert code == 0, "QCM sector across 0 degrees did not run to the end"
    code, circle = _run_qcm_sector(tmp_path, "", "CIRCLE 36")
    assert code == 0
    hs = [float(t.strip().splitlines()[-1].split()[3]) for t in (sector, circle)]
    assert abs(hs[0] - hs[1]) < 0.1 * hs[1], hs


#  GEN4 computes surf breaking through the QC framework, and SCAT 0 then
#  switches the scattering off. The grids for that surf breaking are made in
#  SWQCINIT, which only ran with scattering on. The fork then ended with a
#  segmentation fault, and upstream stopped without output and without
#  norm_end. The run must end normally.
def test_gen4_without_scattering(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QCM / "bottom.bot", work / "bottom.bot")
    deck = (QCM / "qcm.swn").read_text(encoding="utf-8")
    assert "\nGEN4\n" in deck, "QCM deck has changed shape"
    deck = deck.replace("\nGEN4\n", "\nGEN4\nSCAT 0\n", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr[-2000:]
    assert (work / "norm_end").is_file(), "run did not end normally"
    table = (work / "qcm_center.tbl").read_text()
    assert float(table.strip().splitlines()[-1].split()[3]) > 0


#  After an input error, input processing went on with variables that were
#  never set on that error path. A message should stop the run or let it
#  continue, not make it crash or print garbage.
def _run_deck(tmp_path: Path, deck: str, files: dict[str, str] | None = None,
              timeout: float | None = None):
    work = tmp_path / "work"
    work.mkdir(parents=True)
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    for name, content in (files or {}).items():
        (work / name).write_text(content, encoding="utf-8")
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=timeout)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    return result, report


def _quick_deck() -> str:
    return (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")


def test_side_without_open_boundary_does_not_crash(tmp_path):
    # Whole boundary as exception value: SWBOUN finds no open side and
    # then used the unset corner indices of that side.
    rows = [" ".join("-99.0" if i in (0, 20) or j in (0, 10) else "20.0"
                     for i in range(21)) for j in range(11)]
    deck = _quick_deck().replace(
        "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0",
        "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 20 10 100.0 100.0 EXC -99.0", 1)
    deck = deck.replace("'bottom.bot'", "'ring.bot'", 1)
    result, report = _run_deck(tmp_path, deck, {"ring.bot": "\n".join(rows) + "\n"})
    assert result.returncode in (0, 1), result.stderr[-2000:]
    assert "No open boundary found" in report


def test_init_hotstart_with_unknown_keyword(tmp_path):
    deck = _quick_deck().replace("GEN3 KOMEN", "GEN3 KOMEN\nINIT HOTSTART XYZ 'none.hot'", 1)
    result, report = _run_deck(tmp_path, deck)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert "Illegal keyword: XYZ" in report


def test_test_point_outside_grid_shows_the_indices(tmp_path):
    deck = _quick_deck().replace("\nCOMPUTE\n", "\nTEST 1 0 POINTS IJ 100 5\nCOMPUTE\n", 1)
    result, report = _run_deck(tmp_path, deck)
    lines = report.splitlines()
    at = next(i for i, line in enumerate(lines) if "test point is not active" in line)
    assert lines[at + 1].split() == ["100", "5"], lines[at + 1]


#  BF-26 for vector fields: wind and current with a heading line before each
#  component ([nhedvec] = 1) in one file. For a file that was already open,
#  REPARM read nothing of the command, so also not the number of heading lines
#  per component; repeated settings were left unread. Without a value, the
#  values last given for the file now apply; with a value, the new ones.
WIND_WITH_HEADERS = "wind x\n1.0 1.0\n1.0 1.0\nwind y\n0.0 0.0\n0.0 0.0\n"
CURRENT_WITH_HEADERS = "current x\n0.8 0.8\n0.8 0.8\ncurrent y\n0.0 0.0\n0.0 0.0\n"


def _run_vector_fields(tmp_path: Path, files: dict[str, str], wind: str, current: str):
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    for old in ("WIND 8.0 270.0\n", "XP YP DEPTH HSIGN TM01 DIR"):
        assert old in deck, "smoke deck has changed shape"
    deck = deck.replace("WIND 8.0 270.0\n", "", 1)
    deck = deck.replace("XP YP DEPTH HSIGN TM01 DIR", "XP YP DEPTH HSIGN TM01 DIR VEL", 1)
    deck = deck.replace(
        "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE",
        "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE\n"
        "INPGRID WIND REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0\n" + wind + "\n"
        + CURRENT_GRID + "\n" + current, 1)
    result, report = _run_deck(tmp_path, deck, files)
    table = tmp_path / "work" / "quick_test_center.tbl"
    return result.returncode, report, table.read_text() if table.is_file() else ""


@pytest.mark.parametrize("current", ["READINP CURRENT 1.0 'vec.dat'",
                                     "READINP CURRENT 1.0 'vec.dat' 3 0 1 FREE"])
def test_vector_fields_with_heading_lines_from_one_file(tmp_path, current):
    code, report, separate = _run_vector_fields(
        tmp_path / "separate",
        {"wind.dat": WIND_WITH_HEADERS, "current.dat": CURRENT_WITH_HEADERS},
        "READINP WIND 1.0 'wind.dat' 3 0 1 FREE",
        "READINP CURRENT 1.0 'current.dat' 3 0 1 FREE")
    assert code == 0 and "** Error" not in report, report[-2000:]
    assert "0.800000" in separate, "counter-test does not read the current"

    code, report, combined = _run_vector_fields(
        tmp_path / "combined", {"vec.dat": WIND_WITH_HEADERS + CURRENT_WITH_HEADERS},
        "READINP WIND 1.0 'vec.dat' 3 0 1 FREE", current)
    assert code == 0 and "** Error" not in report, report[-2000:]
    assert combined == separate


#  BOUNDSPEC SIDE with an unknown side: WRNKEY reports the error, but SWBOUN
#  then looked for a side in the unset direction DIRSI. With SET MAXERR 3
#  the run computes on; the boundary condition should then be dropped instead
#  of ending up on an arbitrary side.
def test_unknown_side_drops_the_boundary(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "BOUNDSPEC SIDE WEST" in deck, "smoke deck has changed shape"
    deck = deck.replace("BOUNDSPEC SIDE WEST", "BOUNDSPEC SIDE XYZ", 1)
    deck = deck.replace("SET NAUTICAL", "SET NAUTICAL\nSET MAXERR 3", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 0, result.stderr[-2000:]
    assert "Illegal keyword: XYZ" in report, report[-2000:]
    assert "No open boundary found" not in report, report[-2000:]


#  CHGBAS distributes a boundary spectrum over SWAN's bins with cells that end
#  halfway to the neighbouring value. With one direction (or frequency) in the
#  boundary file there is no neighbour: CHGBAS read X1(2) outside the array and
#  the run silently went on without boundary energy.
ONE_DIRECTION_SPECTRUM = """SWAN   1
$ boundary spectrum with a single direction
LOCATIONS
     1
      0.0      500.0
AFREQ
     3
    0.10
    0.15
    0.20
NDIR
     1
  270.0
QUANT
     1
VaDens
m2/Hz/degr
   -99.0
FACTOR
  1.0
  0.5
  1.0
  0.5
"""


def test_boundary_spectrum_with_one_direction_is_refused(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    (work / "boundary.sp2").write_text(ONE_DIRECTION_SPECTRUM, encoding="utf-8")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(old, "BOUNDSPEC SIDE WEST CONSTANT FILE 'boundary.sp2'", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode != 0, report[-2000:]
    assert "a spectral axis needs at least two values" in report, report[-2000:]


#  CVMESH looks for an output point in a curvilinear grid and then checks in
#  KGRPNT whether that point is active. For a point outside the grid
#  (XC = -99) that check must be skipped, or KGRPNT is read at index -97.
#  Fortran does not promise to skip the right-hand side of .AND., so the check
#  is a separate IF, as upstream has it with its GOTO. With runtime checks a
#  read outside KGRPNT would stop this run.
def test_point_outside_curvilinear_grid(tmp_path):
    xs = "\n".join(" ".join(str(500.0 * i) for i in range(5)) for _ in range(3))
    ys = "\n".join(" ".join(str(500.0 * j) for _ in range(5)) for j in range(3))
    deck = """PROJECT 'CURV' '1'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
CGRID CURVILINEAR 4 2 CIRCLE 24 0.05 1.0 16
READGRID COORDINATES 1.0 'coor.grd' 3 0 FREE
INPGRID BOTTOM CURVILINEAR 0 0 4 2
READINP BOTTOM 1.0 'curved.bot' 3 0 FREE
WIND 8.0 270.0
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SEGMENT IJ 0 0 0 2 CONSTANT PAR 1.0 6.0 270.0 20.0
GEN3 KOMEN
POINTS 'P' 1000 500 5000 5000
TABLE 'P' NOHEADER 'p.tbl' XP YP DEPTH
COMPUTE
STOP
"""
    files = {"coor.grd": xs + "\n" + ys + "\n", "curved.bot": "20 20 20 20 20\n" * 3}
    result, report = _run_deck(tmp_path, deck, files, timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 0, report[-2000:]
    rows = [r.split() for r in (tmp_path / "work" / "p.tbl").read_text().splitlines()]
    assert float(rows[0][2]) == pytest.approx(20.0)
    assert float(rows[1][2]) == pytest.approx(-99.0)


