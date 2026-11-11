"""Unusual or faulty input must give the documented result or a clear message.

Each test writes a small deck on the spot, mostly the smoke deck with one
command added or changed. These decks do not belong under examples/: those are
valid physics cases that also have reference output.
"""
from __future__ import annotations

import math
import random
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
    #  BF-19: the MDIA entries refuse a missing or empty LAMBDA list and missing
    #  coefficients.
    ("QUADRUPLET 4\nMDIA CNL4 1.0", "MDIA requires LAMBDA"),
    ("QUADRUPLET 1 1.0", "[lambda]"),
    ("QUADRUPLET 1 1.5", "[lambda]"),
    ("QUADRUPLET 3 1.0", "[lambda]"),
    ("QUADRUPLET 4\nMDIA LAMBDA CNL4", "requires at least one"),
    ("QUADRUPLET 4\nMDIA LAMBDA .25", "requires CNL4 or CNL4_12"),
    ("QUADRUPLET MDIA LAMBDA 1.0 CNL4 3.0E7", "[lambda]"),
    #  UPSTREAM (this fork) chooses the upstream semi-implicit DIA (BF-62),
    #  so it belongs to [iquad] = 1 only.
    ("QUADRUPLET 2 UPSTREAM", "UPSTREAM requires [iquad] = 1"),
    #  UPSTREAM (this fork) chooses the upstream third harmonic of the LTA
    #  (BF-63); ITRIAD=11 has no third harmonic and DCTA a different form.
    ("TRIAD ITRIAD=11 UPSTREAM", "UPSTREAM requires the LTA"),
    ("TRIAD DCTA UPSTREAM", "UPSTREAM requires the LTA"),
    #  The filter scheme in frequency space was never built: nothing applies
    #  DIFFC and the frequency terms exist only for schemes 1 and 2,
    #  so the frequency shift silently dropped out.
    ("NUMERIC FIL", "FIL is not implemented"),
]


@pytest.mark.parametrize("command,expected", REJECTED)
def test_unknown_formulation_is_rejected(tmp_path, command, expected):
    returncode, report = _run(tmp_path, command)
    assert returncode != 0, (
        f"'{command}' ran on to a normal end; a number that does not name a "
        f"formulation should stop the run.\n{report[-2000:]}"
    )
    assert expected in report, (
        f"'{command}' stopped, but without the message that says why.\n"
        f"{report[-2000:]}"
    )


#  The counter-test. Without it, a check that rejects everything would also pass.
ACCEPTED = [
    "BRAGG 0",
    "TRIAD ITRIAD=0",
    "SET MAXERR 2\nQUADRUPLET 0 0.0",
    "QUADRUPLET 1 0.0",
    "TRIAD ITRIAD=11",
    "TRIAD ITRIAD=1",
    "TRIAD LTA",
    "TRIAD DCTA",
    "TRIAD LTA UPSTREAM",
    "TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5 UPSTREAM",
    "TRIAD LTA TRANSFER MS UPSTREAM",
    "QUADRUPLET 2",
    "QUADRUPLET 8",
    "QUADRUPLET 1 0.25",
    "QUADRUPLET 1 UPSTREAM",
    "QUADRUPLET 1 0.25 UPSTREAM",
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


#  BRAGG needs a bottom grid much finer than the computational grid: SWBRBOT
#  takes the mean depth and the bed elevation spectrum over a square of
#  [nreg] x [nreg] bottom points around each computational point, and SWAN
#  refuses an [nreg] above half the number of bottom points. The smoke deck
#  has a 2 x 2 bottom, so it can only test BRAGG 0. This deck has a 2 m bottom
#  grid with a patch of sinusoidal bars of 14 m wavelength, half the length
#  of the 5 s waves in 4 m of water, so that they reflect part of the incoming
#  energy (Bragg resonance); [nreg] = 32 covers 64 m, about four bars.
#
#  Upstream also has a formulation 4 ("implicit integration according to
#  Ardhuin and Herbers (2002)"), but its branch in SOURCE is empty, and other
#  numbers are not checked at all. With BRAGG 4, -1 or 5 upstream computes to
#  a normal end with the smoothed depth of the Bragg option and no scattering
#  at all: on this deck no reflection, and Hs up to 4% above BRAGG 1.
def _bragg_deck(work: Path, command: str) -> None:
    dx = 2.0
    bottom = "\n".join(
        " ".join(f"{4.0 + (0.4 * math.sin(2 * math.pi * i * dx / 14.0) if 50 <= i * dx <= 350 else 0.0):.4f}"
                 for i in range(201))
        for _ in range(51))
    (work / "bottom.bot").write_text(bottom + "\n", encoding="utf-8")
    (work / "INPUT").write_text(f"""PROJECT 'BRAGG' '001'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
CGRID REGULAR 0. 0. 0. 400. 100. 40 10 CIRCLE 36 0.1 1.0 24
INPGRID BOTTOM REGULAR 0. 0. 0. 200 50 {dx} {dx}
READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE
BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES
BOUNDSPEC SIDE WEST CONSTANT PAR 0.5 5.0 270.0 10.0
OFF QUAD
{command}
NUMERIC STOPC 0.00 0.01 0.001 99.5 STAT 30 0.01
POINTS 'P' 25. 50. 375. 50.
TABLE 'P' NOHEADER 'p.tbl' XP YP DEPTH HSIGN
COMPUTE
STOP
""", encoding="utf-8")


def _run_bragg(work: Path, command: str):
    work.mkdir(parents=True)
    _bragg_deck(work, command)
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=120)
    printfile = work / "PRINT"
    report = printfile.read_text(encoding="utf-8", errors="replace") if printfile.is_file() else ""
    table = work / "p.tbl"
    hs = [float(line.split()[3]) for line in table.read_text().splitlines()] if table.is_file() else []
    return result.returncode, report, hs, (work / "norm_end").is_file()


@pytest.fixture(scope="module")
def bragg_off(tmp_path_factory):
    returncode, report, hs, norm_end = _run_bragg(tmp_path_factory.mktemp("bragg0") / "work",
                                                  "BRAGG 0")
    assert returncode == 0 and norm_end, report[-2000:]
    return hs


@pytest.mark.parametrize("command", ["BRAGG 1 32", "BRAGG 2 32", "BRAGG 3 32"])
def test_bragg_formulation_computes(tmp_path, bragg_off, command):
    returncode, report, hs, norm_end = _run_bragg(tmp_path / "work", command)
    assert returncode == 0 and norm_end, (
        f"'{command}' is valid but was refused.\n{report[-2000:]}")
    #  The bars reflect energy: Hs in front of them rises by about 1 cm.
    assert hs[0] > bragg_off[0] + 0.005, (command, hs, bragg_off)


@pytest.mark.parametrize("command", ["BRAGG 4 32", "BRAGG 4", "BRAGG -1 32",
                                     "BRAGG 5 32", "BRAGG 9"])
def test_bragg_formulation_without_source_is_rejected(tmp_path, command):
    returncode, report, hs, norm_end = _run_bragg(tmp_path / "work", command)
    assert returncode != 0 and not norm_end, (
        f"'{command}' ran on to a normal end.\n{report[-2000:]}")
    assert "BRAGG [ibrag] must be 0, 1, 2 or 3" in report, report[-2000:]


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


@pytest.mark.parametrize("sector", ["SECTOR 280 330 10", "SECTOR 315 45 18"])
def test_qcm_breaking_grid_covers_negative_wave_numbers(tmp_path, sector):
    code, table = _run_qcm_sector(tmp_path, "", sector)
    assert code == 0, sector
    values = [float(v) for v in table.strip().splitlines()[-1].split()]
    assert len(values) == 6 and all(math.isfinite(v) for v in values)


#  BF-69: boundary waves from 270 degrees (nautical) and a sector from 30 to
#  60 degrees: no boundary point brings waves in. SWQCINIT then cannot compute
#  the standard deviation of the incoming spectra, from which it takes the
#  step of the wave number grid. It reported a severe error and carried on with
#  the placeholder 9999, which gave an empty coherent zone and a segmentation
#  fault, in upstream as well. The run must stop with the message, exit code 1
#  and no table, also with SET MAXERR 3. The message names the remedy, a given
#  number of wave number bins (SCAT GRID K); that run goes on as before.
def _run_qcm_without_incoming_waves(tmp_path: Path, extra: str) -> tuple[int, str, Path]:
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QCM / "bottom.bot", work / "bottom.bot")
    deck = (QCM / "qcm.swn").read_text(encoding="utf-8")
    replacements = {
        "CIRCLE 12 0.05 1.0 8": "SECTOR 30.0 60.0 3 0.05 1.0 8",
        "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0":
            "INPGRID BOTTOM REGULAR -1000.0 -1000.0 0.0 1 1 4000.0 3000.0",
        "\nGEN4\n": f"\nGEN4\n{extra}",
    }
    for old, new in replacements.items():
        assert old in deck, "QCM deck has changed shape"
        deck = deck.replace(old, new, 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    return result.returncode, report, work


@pytest.mark.parametrize("maxerr", ["", "SET MAXERR 3\n"], ids=["default", "maxerr-3"])
def test_qcm_without_incoming_waves_stops_cleanly(tmp_path, maxerr):
    code, report, work = _run_qcm_without_incoming_waves(tmp_path, maxerr)
    assert code == 1, (code, report[-2000:])
    assert ("Terminating error: unable to compute standard deviation of incoming spectra"
            in report), report[-2000:]
    assert not (work / "qcm_center.tbl").exists(), "table written for a run that did not compute"
    assert not (work / "norm_end").exists()


def test_qcm_without_incoming_waves_runs_with_given_grid(tmp_path):
    code, report, work = _run_qcm_without_incoming_waves(
        tmp_path, "SET MAXERR 3\nSCAT 1 GRID K 0.0 0.0 0.2 0.2 32 32\n")
    assert code == 0, (code, report[-2000:])
    assert "Severe error     : unable to compute standard deviation" in report, report[-2000:]
    assert (work / "norm_end").is_file()
    values = [float(v) for v in
              (work / "qcm_center.tbl").read_text().strip().splitlines()[-1].split()]
    assert len(values) == 6 and values[3] == 0.0, values


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


#  Above [maxerr] SWMAIN does not start the computation ("No start of computation
#  because of error level"), but it simply returned and wrote norm_end:
#  exit code 0. A script that only looks at the exit code then did not see
#  the error. An unknown keyword gives error level 3.
def test_error_level_above_maxerr_gives_exit_code(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "\nCOMPUTE\n" in deck, "smoke deck has changed shape"
    (work / "INPUT").write_text(
        deck.replace("\nCOMPUTE\n", "\nNUMERIC STOPC UNKNOWNOPTION\nCOMPUTE\n"), encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert "No start of computation" in report, report[-2000:]
    assert result.returncode != 0
    assert "error level" in result.stderr, result.stderr[-2000:]


#  The counter-test: with [maxerr] above the error level SWAN computes on, as the
#  manual promises, and the exit code is 0.
def test_error_level_below_maxerr_keeps_running(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    (work / "INPUT").write_text(
        deck.replace("\nCOMPUTE\n", "\nSET MAXERR 3\nNUMERIC STOPC UNKNOWNOPTION\nCOMPUTE\n"),
        encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert "No start of computation" not in report, report[-2000:]
    assert result.returncode == 0, result.stderr[-2000:]


#  BF-67: a grid or field file that holds fewer values than asked for. INAR2D
#  reported this at level 2 only, so with SET MAXERR 2 SWAN computed on with
#  the missing values as 0 and gave exit code 0; the operational so-rp deck
#  ran that way on a grid read one row and column too many. SWAN cannot tell
#  from the file how bad the shortage is, so now every incomplete file is a
#  severe error (level 3): the run does not start, unless the deck accepts
#  severe errors with SET MAXERR 3.
WIND_FROM_FILE = ("INPGRID WIND REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0\n"
                  "READINP WIND 1.0 'wind.dat' 3 0 FREE")


def _short_file_deck(maxerr: int, wind: bool) -> str:
    deck = _quick_deck()
    assert "SET NAUTICAL" in deck and "WIND 8.0 270.0" in deck, "smoke deck has changed shape"
    deck = deck.replace("SET NAUTICAL", f"SET MAXERR={maxerr} NAUTICAL", 1)
    if wind:
        deck = deck.replace("WIND 8.0 270.0", WIND_FROM_FILE, 1)
    return deck


@pytest.mark.parametrize("maxerr", [2, 3])
def test_short_bottom_file_is_a_severe_error(tmp_path, maxerr):
    """SWAN cannot tell how bad a short file is, so by default the run does
    not start; a deck that accepts severe errors computes on, as upstream."""
    result, report = _run_deck(tmp_path / str(maxerr), _short_file_deck(maxerr, wind=False),
                               {"bottom.bot": "20.0 20.0\n20.0\n"})
    assert "Unexpected end of file while reading bottom.bot" in report, report[-2000:]
    assert ("Severe error     : READINP: the input grid asks for 2 x 2 values"
            in report), report[-2000:]
    assert "Terminating error" not in report
    assert ("No start of computation" in report) == (maxerr == 2)
    assert (result.returncode == 0) == (maxerr == 3), report[-2000:]


def test_short_wind_file_blocks_the_computation(tmp_path):
    result, report = _run_deck(tmp_path, _short_file_deck(2, wind=True),
                               {"wind.dat": "15. 15. 15.\n"})
    assert result.returncode != 0, report[-2000:]
    assert "Severe error     : READINP: the input grid asks for 2 x 2 values" in report, report[-2000:]
    assert "No start of computation" in report
    #  The second wind component is no longer read from the closed unit,
    #  which made the runtime create an empty fort.<unit>.
    assert not list((tmp_path / "work").glob("fort.*"))


def test_short_wind_file_with_maxerr_3_computes_on(tmp_path):
    result, report = _run_deck(tmp_path, _short_file_deck(3, wind=True),
                               {"wind.dat": "15. 15. 15.\n"})
    assert result.returncode == 0, report[-2000:]
    assert "Severe error" in report
    assert "No start of computation" not in report
    assert not list((tmp_path / "work").glob("fort.*"))


def test_unreadable_value_in_bottom_file_is_a_severe_error(tmp_path):
    """A value that cannot be read (IDLA -2) counts like a short file."""
    result, report = _run_deck(tmp_path, _short_file_deck(2, wind=False),
                               {"bottom.bot": "20.0 20.0\nxx 20.0\n"})
    assert "Error while reading file" in report, report[-2000:]
    assert "Severe error     : READINP: the input grid asks for 2 x 2 values" in report
    assert result.returncode != 0 and "No start of computation" in report


def test_short_second_wind_component_is_a_severe_error(tmp_path):
    result, report = _run_deck(tmp_path, _short_file_deck(2, wind=True),
                               {"wind.dat": "15. 15. 15. 15.\n0. 0.\n"})
    assert "Severe error     : READINP: the input grid asks for 2 x 2 values" in report, report[-2000:]
    assert result.returncode != 0 and "No start of computation" in report
    assert not list((tmp_path / "work").glob("fort.*"))


def test_ended_file_series_is_a_severe_error(tmp_path):
    """With SERIES, a data file that ends early and no further file in the
    list left the field incomplete without telling the caller."""
    deck = _short_file_deck(2, wind=True).replace(
        "READINP WIND 1.0 'wind.dat' 3 0 FREE", "READINP WIND 1.0 SERIES 'wind.lst' 3 0 FREE", 1)
    result, report = _run_deck(tmp_path, deck, {"wind.lst": "wind1.dat\n",
                                                "wind1.dat": "15. 15. 15.\n"})
    assert "Series of input files ended" in report, report[-2000:]
    assert "Severe error     : READINP: the input grid asks for 2 x 2 values" in report
    assert result.returncode != 0 and "No start of computation" in report


NONSTATIONARY = REPO / "examples" / "nonstationary" / "regular"


def _nonstationary_table(tmp_path, name, wind):
    deck = (NONSTATIONARY / "nonstationary_regular.swn").read_text()
    #  an end of file during the updates is an error of level 2; without
    #  MAXERR 2 SWAN does not continue after it at all
    assert "SET NAUTICAL" in deck, "nonstationary deck has changed shape"
    deck = deck.replace("SET NAUTICAL", "SET MAXERR=2 NAUTICAL", 1)
    result, report = _run_deck(tmp_path / name, deck, {"wind.wnd": wind}, timeout=120)
    assert result.returncode == 0, report[-2000:]
    return (tmp_path / name / "work" / "nonstationary_regular_center.tbl").read_text()


def test_short_second_component_ends_the_wind_updates(tmp_path):
    """A later time level whose second component is short must end the
    updates, as a short first component does, and not mix a new first
    component with a partial or old second one."""
    first = "20260101.000000\n4.0 4.0\n4.0 4.0\n0.0 0.0\n0.0 0.0\n"
    only_first_level = _nonstationary_table(tmp_path, "one", first)
    short_second = _nonstationary_table(
        tmp_path, "short", first + "20260101.003000\n12.0 12.0\n12.0 12.0\n0.0\n")
    assert short_second == only_first_level


CURVILINEAR_DECK = """PROJECT 'CURV' '001'
SET MAXERR=2 NAUTICAL
MODE STATIONARY TWODIMENSIONAL
COORDINATES CARTESIAN
CGRID CURVILINEAR 3 2 CIRCLE 24 0.05 1.0 16
READGRID COORDINATES 1. 'grid.dat' 4 0 0 FREE
INPGRID BOTTOM CURVILINEAR 0 0 3 2
READINP BOTTOM 1.0 'curv.bot' 4 0 FREE
WIND 8.0 270.0
GEN3 KOMEN
POINTS 'P' 500. 500.
TABLE 'P' HEADER 'p.tbl' XP YP DEPTH HSIGN
COMPUTE
STOP
"""


def _grid_file(rows: int) -> str:
    xs, ys = [0, 500, 1000, 1500], [0, 500, 1000][:rows]
    return (" ".join(str(x) for _ in ys for x in xs) + "\n"
            + " ".join(str(y) for y in ys for _ in xs) + "\n")


@pytest.mark.parametrize("rows, maxerr", [(3, 2), (2, 2), (2, 3)])
def test_curvilinear_grid_file_one_row_short(tmp_path, rows, maxerr):
    """CGRID asks for 4 x 3 points; a file of 4 x 2 must stop the run, as the
    so-rp production deck should have, unless the deck sets SET MAXERR 3."""
    deck = CURVILINEAR_DECK.replace("SET MAXERR=2", f"SET MAXERR={maxerr}", 1)
    result, report = _run_deck(tmp_path, deck,
                               {"grid.dat": _grid_file(rows), "curv.bot": "20. " * 12 + "\n"})
    if rows == 3:
        assert result.returncode == 0, report[-2000:]
        assert "READGRID: CGRID asks for" not in report
        return
    assert ("Severe error     : READGRID: CGRID asks for 4 x 3 grid points, "
            "and the file holds fewer") in report, report[-2000:]
    assert "Unexpected end of file while reading grid.dat" in report
    assert ("No start of computation" in report) == (maxerr == 2)
    assert (result.returncode == 0) == (maxerr == 3), report[-2000:]


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
    rows = [r.split() for r in (work / "quick_test_center.tbl").read_text().splitlines()
            if r.strip() and not r.startswith("%")]
    assert float(rows[0][3]) == pytest.approx(0.17013, abs=0.00002)


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


#  BF-23: a boundary file whose first line is not a header that BCFILE knows.
#  Nothing of the file is read then, so there is no boundary spectrum. As a
#  severe error (level 3) this still let a deck with SET MAXERR 3 compute:
#  the boundary points pointed to a spectrum that did not exist, SWAN read
#  whatever lay behind the empty table of boundary spectra, and the run ended
#  normally with exit code 0 and Hs = 0 at every point despite the wind
#  (upstream: 0.23 m, a segmentation fault or a failed allocation, depending
#  on the deck). It must stop the run whatever [maxerr] is, and write no
#  output.
@pytest.mark.parametrize("maxerr", [None, 3])
@pytest.mark.parametrize("boundary", [
    "BOUNDSPEC SIDE WEST CONSTANT FILE 'junk.bnd'",
    "BOUNDSPEC SEGMENT IJ 0 0 0 10 VARIABLE FILE 0. 'junk.bnd' 1",
    "BOUNDNEST1 NEST 'junk.bnd' CLOSED",
], ids=["side", "segment", "nest"])
def test_boundary_file_with_unknown_header_stops_the_run(tmp_path, boundary, maxerr):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    (work / "junk.bnd").write_text("JUNK header line\n1 2 3\n4 5 6\n", encoding="utf-8")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(old, boundary, 1)
    if maxerr is not None:
        deck = deck.replace("SET NAUTICAL", f"SET NAUTICAL\nSET MAXERR {maxerr}", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 1, (result.returncode, report[-2000:])
    assert "Terminating error: unsupported boundary data file junk.bnd" in report, report[-2000:]
    assert not (work / "quick_test_center.tbl").exists(), "SWAN computed without a boundary"
    assert not (work / "norm_end").exists()


#  A typing error in the number of intervals of a CURVE allocated node after
#  node until memory ran out; the process then broke off in a way that
#  depended on the heap. Now SWAN stops with a message, within a fraction of a
#  second.
def test_curve_with_absurdly_many_points_stops_cleanly(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "POINTS 'CENTER' 1000.0 500.0"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(old, old + "\nCURVE 'line' 0. 500. 2000000000 2000. 500.", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode != 0
    assert "more than 10 million output points on CURVE" in report, report[-2000:]


#  RAY has the same structure as CURVE, with a number of intervals between
#  consecutive rays, and the same typing error had the same effect.
def test_ray_with_absurdly_many_points_stops_cleanly(tmp_path):
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    old = "POINTS 'CENTER' 1000.0 500.0"
    assert old in deck, "smoke deck has changed shape"
    deck = deck.replace(
        old, old + "\nRAY 'rays' 0. 500. 0. 600. 2000000000 2000. 500. 2000. 600.", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode != 0
    assert "more than 10 million output points on RAY" in report, report[-2000:]


#  A deck without a bottom crashed with a segmentation fault, upstream too. The
#  table of grid points only exists after READINP BOTTOM, but was still passed
#  to SWPREP, and the error message about the missing bottom had level
#  3, so preparation went on until that table was read. Without a bottom
#  there is nothing to compute, so it is now a stop error, also with SET MAXERR 3.
@pytest.mark.parametrize("removed, message", [
    (("INPGRID BOTTOM", "READINP BOTTOM"), "Bottom grid not defined"),
    (("READINP BOTTOM",), "No bottom levels read"),
])
def test_deck_without_bottom_stops_cleanly(tmp_path, removed, message):
    work = tmp_path / "work"
    work.mkdir()
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    lines = [r for r in deck.splitlines() if not r.startswith(removed)]
    assert len(lines) == len(deck.splitlines()) - len(removed), "smoke deck has changed shape"
    deck = "\n".join(lines).replace("\nCOMPUTE", "\nSET MAXERR 3\nCOMPUTE", 1) + "\n"
    (work / "INPUT").write_text(deck, encoding="utf-8")
    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True,
                            timeout=60)
    report = (work / "PRINT").read_text(encoding="utf-8", errors="replace")
    assert result.returncode == 1, result.stderr[-2000:]
    assert f"Terminating error: {message}" in report, report[-2000:]


#  Faulty input from a systematic trial on the smoke deck:
#  omitted commands, nonsensical numbers, wrong order and broken
#  files. Each of these decks crashed, hung, or ended without computing
#  with exit code 0; most did so upstream too. The requirement is that SWAN
#  stops with a message in PRINT and exit code 1. Where the message is a
#  stop error, the run uses SET MAXERR 3: then it may not continue either.
BOUNDARY_SPECTRUM_WITHOUT_FREQUENCIES = "SWAN   1\ngarbage\n1 2 3\n"
BOUNDARY_SPECTRUM_TEXT_AS_FACTOR = ONE_DIRECTION_SPECTRUM.replace(
    "NDIR\n     1\n  270.0", "NDIR\n     2\n  180.0\n  270.0").replace(
    "FACTOR\n  1.0\n  0.5\n  1.0\n  0.5\n",
    "FACTOR\n  abc\n  0.5 0.5\n  1.0 1.0\n  0.5 0.5\n")

HOTSTART_30_DIRECTIONS = ("SWAN   1\nLOCATIONS\n     1\n   0.0  500.0\n"
                          "AFREQ\n     3\n  0.1\n  0.2\n  0.3\nNDIR\n     30\n"
                          + "".join(f"  {12.0 * i:.1f}\n" for i in range(30))
                          + "QUANT\n     1\nVaDens\nm2/Hz/degr\n   -99.0\n")

CGRID = "CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10 CIRCLE 24 0.05 1.0 16"
INPGRID = "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0"
READINP = "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
BOUNDARY = "BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0"
BLOCK = "BLOCK 'COMPGRID' NOHEADER 'quick_test_hs.blk' LAYOUT 3 HSIGN"
POINTS = "POINTS 'CENTER' 1000.0 500.0"
FRAME_BLOCK = "\nBLOCK 'F' NOHEADER 'f.blk' HSIGN"

FAULTY_INPUT = [
    # BF-46: a computational grid or computation period without points, directions,
    # frequencies or time steps
    pytest.param([(CGRID, CGRID.replace("20 10", "0 10"))], {},
                 "[mxc] in CGRID must be at least 1", True, id="BF-46-mxc"),
    pytest.param([(CGRID, CGRID.replace("20 10", "20 -1"))], {},
                 "[myc] in CGRID must be at least 1", True, id="BF-46-myc"),
    pytest.param([(CGRID, CGRID.replace("CIRCLE 24", "CIRCLE 0"))], {},
                 "[mdc] in CGRID must be at least 1", True, id="BF-46-mdc"),
    pytest.param([(CGRID, CGRID.replace("0.05 1.0", "0.0 1.0"))], {},
                 "[flow] and [fhigh] in CGRID", True, id="BF-46-flow"),
    pytest.param([(CGRID, CGRID.replace("0.05 1.0", "1.0 0.05"))], {},
                 "[flow] and [fhigh] in CGRID", True, id="BF-46-fhigh"),
    pytest.param([(CGRID, CGRID.replace("CIRCLE 24", "SECTOR 200 200 24"))], {},
                 "[dir1] and [dir2] of SECTOR in CGRID must differ", True, id="BF-46-sector"),
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nSURFBEAT -1")], {},
                 "[df] in SURFBEAT must be positive", True, id="BF-46-surfbeat-df"),
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nSURFBEAT 0.01 -1")], {},
                 "[nmax] in SURFBEAT must be at least 1", True, id="BF-46-surfbeat-nmax"),
    pytest.param([("MODE STATIONARY", "MODE NONSTATIONARY"),
                  ("\nCOMPUTE\n",
                   "\nCOMPUTE NONSTAT 20200101.000000 -10 MIN 20200101.010000\n")], {},
                 "time step [deltc] in COMPUTE must be positive", True, id="BF-46-deltc"),
    # BF-47: commands that are missing, duplicated or in the wrong order
    pytest.param([(CGRID + "\n", "")], {},
                 "define computational grid before", True, id="BF-47-no-cgrid"),
    pytest.param([(CGRID, CGRID + "\nCGRID REGULAR 0 0 0 1000 1000 5 5 CIRCLE 12 0.1 1.0 8")],
                 {}, "the computational grid can be defined only once", True,
                 id="BF-47-two-cgrids"),
    pytest.param([("CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10", "CGRID CURVILINEAR 20 10")],
                 {}, "read curvilinear coordinates", True, id="BF-47-curvilinear"),
    pytest.param([("CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10", "CGRID UNSTRUCTURED"),
                  (INPGRID + "\n", ""), (READINP + "\n", "")], {},
                 "read the unstructured grid (READGRID UNSTRUC) before boundary commands",
                 True, id="BF-47-unstructured"),
    # BF-48: an input grid without meshes or with zero mesh size
    pytest.param([(INPGRID, INPGRID.replace("1 1 2000.0", "0 1 2000.0"))], {},
                 "[mxinp] in INPGRID must be at least 1", True, id="BF-48-mxinp"),
    pytest.param([(INPGRID, INPGRID.replace("2000.0 1000.0", "0.0 1000.0"))], {},
                 "[dxinp] and [dyinp] in INPGRID must be positive", True, id="BF-48-dxinp"),
    # BF-49: input files that are too short or cannot be opened
    pytest.param([(READINP, READINP.replace("3 0 FREE", "3 5 FREE"))], {},
                 "has fewer lines than [nhedf] heading lines", True, id="BF-49-heading-lines"),
    pytest.param([(READINP, "READINP BOTTOM 1.0 SERIES 'list.txt' 3 0 FREE")],
                 {"list.txt": ""}, "no data file name in series file", True,
                 id="BF-49-empty-series"),
    pytest.param([(READINP, READINP.replace("'bottom.bot'", "'.'"))], {},
                 "File open failed, filename: .", True, id="BF-49-directory-as-file"),
    pytest.param([(BLOCK, BLOCK.replace("'quick_test_hs.blk'", "'notthere/hs.blk'"))], {},
                 "File open failed, filename: notthere/hs.blk", True,
                 id="BF-49-output-directory-missing"),
    # BF-50: boundary spectra and hotstart files that are empty, truncated or unreadable
    pytest.param([(BOUNDARY, "BOUNDSPEC SIDE WEST CONSTANT FILE 'boundary.sp2'")], {"boundary.sp2": ""},
                 "boundary file boundary.sp2 ends early", True, id="BF-50-empty-boundary-file"),
    pytest.param([(BOUNDARY, "BOUNDNEST1 NEST 'boundary.sp2' CLOSED")], {"boundary.sp2": ""},
                 "boundary file boundary.sp2 ends early", True, id="BF-50-empty-nest-file"),
    pytest.param([(BOUNDARY, "BOUNDSPEC SIDE WEST CONSTANT FILE 'boundary.sp2'")],
                 {"boundary.sp2": BOUNDARY_SPECTRUM_WITHOUT_FREQUENCIES},
                 "no frequencies in boundary file boundary.sp2", True, id="BF-50-no-frequencies"),
    pytest.param([(BOUNDARY, "BOUNDSPEC SIDE WEST CONSTANT FILE 'boundary.sp2'")],
                 {"boundary.sp2": BOUNDARY_SPECTRUM_TEXT_AS_FACTOR},
                 "error reading data from boundary file", True, id="BF-50-text-as-factor"),
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nINITIAL HOTSTART 'empty.hot'")], {"empty.hot": ""},
                 "hotstart file empty.hot ends early", True, id="BF-50-empty-hotstart"),
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nINITIAL HOTSTART 'wide.hot'")],
                 {"wide.hot": HOTSTART_30_DIRECTIONS},
                 "has more directions than the computational grid", True,
                 id="BF-50-hotstart-more-directions"),
    # BF-51: clearing a list that was never filled
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nVEGETATION 1 -1")], {},
                 "No vegetation parameters found", False, id="BF-51-vegetation"),
    pytest.param([("GEN3 KOMEN", "GEN3 KOMEN\nOBSTACLE FOO")], {},
                 "Illegal keyword: FOO", False, id="BF-51-obstacle"),
    # BF-52: output requests with impossible dimensions or format
    pytest.param([(POINTS, POINTS + "\nFRAME 'F' 0 0 0 2000 1000 -1 -1" + FRAME_BLOCK)], {},
                 "[mxfr] and [myfr] in FRAME must be non-negative", True, id="BF-52-frame-negative"),
    pytest.param([(POINTS, POINTS + "\nFRAME 'F' 0 0 0 2000 1000 100000 100000" + FRAME_BLOCK)],
                 {}, "more than 10 million output points on FRAME", True, id="BF-52-frame-too-large"),
    pytest.param([(POINTS, POINTS + "\nGROUP 'G' SUBGRID 0 1000000 0 1000000"
                   "\nBLOCK 'G' NOHEADER 'g.blk' HSIGN")],
                 {}, "Check corners of GROUP (SUBGRID) command", True, id="BF-52-group"),
    pytest.param([(BLOCK, "OUTPUT OPTIONS '%' TABLE 1000\n" + BLOCK), ("TABLE 'CENTER' HEADER", "TABLE 'CENTER' NOHEADER")], {},
                 "[field] is too large", False, id="BF-52-table-width"),
    pytest.param([(BLOCK, "OUTPUT OPTIONS '%' BLOCK 0 6\n" + BLOCK)], {},
                 "[ndec] is too small", False, id="BF-52-decimals"),
    pytest.param([(BLOCK, BLOCK.replace("HSIGN", "FOO HSIGN"))], {},
                 "Illegal keyword: FOO", False, id="BF-52-unknown-quantity"),
]


@pytest.mark.parametrize("pairs, files, message, stop_error", FAULTY_INPUT)
def test_faulty_input_stops_with_message(tmp_path, pairs, files, message, stop_error):
    deck = _quick_deck()
    for old, new in pairs:
        assert old in deck, "smoke deck has changed shape"
        deck = deck.replace(old, new, 1)
    if stop_error:
        deck = deck.replace("SET NAUTICAL", "SET NAUTICAL\nSET MAXERR 3", 1)
    result, report = _run_deck(tmp_path, deck, files, timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 1, (result.returncode, result.stderr[-2000:])
    assert message in report, report[-2000:]
    if stop_error:
        assert "Terminating error" in report, report[-2000:]


#  BF-47: a deck with only COMPUTE passed a non-existent grid on to
#  SWPREP.
def test_compute_without_grid_stops_cleanly(tmp_path):
    result, report = _run_deck(tmp_path, "PROJECT 'X' '1'\nCOMPUTE\nSTOP\n", timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 1
    assert "Terminating error: no computational grid; give command CGRID" in report, \
        report[-2000:]


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


def _curvilinear_depths(tmp_path: Path, nodes, bottom, points, exception="-999 -999"):
    """Run a curvilinear grid given as rows of (x, y) and return DEPTH per point.

    Every run stops after one iteration: only the location of the points matters.
    """
    nx, ny = len(nodes[0]), len(nodes)
    xs = "\n".join(" ".join(f"{x:.3f}" for x, _ in row) for row in nodes)
    ys = "\n".join(" ".join(f"{y:.3f}" for _, y in row) for row in nodes)
    bot = "\n".join(" ".join(f"{b:.3f}" for b in row) for row in bottom)
    deck = f"""PROJECT 'CURV' '1'
SET NAUTICAL
MODE STATIONARY TWODIMENSIONAL
CGRID CURVILINEAR {nx-1} {ny-1} EXCEPTION {exception} CIRCLE 12 0.05 1.0 8
READGRID COORDINATES 1.0 'coor.grd' 3 0 FREE
INPGRID BOTTOM CURVILINEAR 0 0 {nx-1} {ny-1}
READINP BOTTOM 1.0 'curv.bot' 3 0 FREE
WIND 8.0 270.0
GEN3 KOMEN
NUMERIC STOPC STAT MXITST=1
POINTS 'P' FILE 'points'
TABLE 'P' NOHEADER 'p.tbl' XP YP DEPTH
COMPUTE
STOP
"""
    files = {"coor.grd": xs + "\n" + ys + "\n", "curv.bot": bot + "\n",
             "points": "\n".join(f"{x:.3f} {y:.3f}" for x, y in points) + "\n"}
    result, report = _run_deck(tmp_path, deck, files, timeout=120)
    assert result.returncode == 0, report[-2000:]
    rows = (tmp_path / "work" / "p.tbl").read_text().splitlines()
    return [float(r.split()[2]) for r in rows], report


def _fan_grid(hole, excluded_corners):
    """A fan of 41 x 49 points (70 to 110 degrees, radius 50 to 6050 m) with
    an excluded area `hole(i, j)` and excluded corner blocks."""
    def node(i, j):
        if hole(i, j) or any(c(i, j) for c in excluded_corners):
            return -999.0, -999.0
        angle, radius = math.radians(70.0 + i), 50.0 + 125.0 * j
        return radius * math.cos(angle), radius * math.sin(angle)
    return [[node(i, j) for i in range(41)] for j in range(49)], [[20.0] * 41 for _ in range(49)]


def _fan_point(angle, radius):
    return radius * math.cos(math.radians(angle)), radius * math.sin(math.radians(angle))


#  BF-56: when the first search fails, INMESH decides whether an output point
#  lies in a curvilinear grid by counting how often a line straight north from
#  the point crosses the grid outline. Upstream skipped a crossing whose
#  distance, relative to the length of its side, was within 0.1 of that of the
#  previous crossing. In a fan-shaped grid that ratio is almost the same for
#  every side the line crosses, so the north side of the excluded area below
#  was skipped: two crossings, and a point well inside the grid got depth -99.
#  The excluded corner makes the first search fail, so that INMESH is reached;
#  the extra node at the south-east corner of the hole keeps this test
#  independent of BF-58.
def test_point_south_of_excluded_area_in_fan_grid(tmp_path):
    nodes, bottom = _fan_grid(
        lambda i, j: (15 <= i <= 25 and 33 <= j <= 34) or (i == 26 and j == 33),
        [lambda i, j: i < 2 and j < 2])
    depths, report = _curvilinear_depths(
        tmp_path, nodes, bottom, [(-1.0, 60.0), (-36.6, 4200.0), (-1.0, 7000.0)])
    #  south of the excluded area, inside the grid
    assert depths[0] == pytest.approx(20.0), report[-2000:]
    #  inside the excluded area and north of the grid: still no output
    assert depths[1] == pytest.approx(-99.0)
    assert depths[2] == pytest.approx(-99.0)


#  BF-58: CGBOUN traces the outline of an excluded area from a point with
#  three active neighbours and asks SEPARAREA which way to go. Upstream asked
#  about the last point of the previous curve instead of the start point. For
#  this rectangular hole the walk then went the wrong way: the outline became
#  a loop around one active mesh east of the hole, and the hole itself had no
#  outline. A point inside that mesh got two crossings in INMESH and no
#  output. Both far corners are excluded, so that the first search fails.
def test_outline_of_excluded_area_in_fan_grid(tmp_path):
    nodes, bottom = _fan_grid(
        lambda i, j: 15 <= i <= 25 and 33 <= j <= 34,
        [lambda i, j: i < 2 and j < 2, lambda i, j: i > 38 and j > 46])
    points = [_fan_point(96.5, 4110.0), _fan_point(96.8, 4130.0), (-36.6, 4200.0)]
    depths, report = _curvilinear_depths(tmp_path, nodes, bottom, points)
    #  inside the mesh that the upstream outline enclosed
    assert depths[0] == pytest.approx(20.0), report[-2000:]
    assert depths[1] == pytest.approx(20.0), report[-2000:]
    #  inside the excluded area
    assert depths[2] == pytest.approx(-99.0)


@pytest.mark.parametrize("nx,ny,count", [(5, 0, 6), (0, 5, 6), (0, 0, 1)])
def test_frame_with_zero_meshes_has_output_points(tmp_path, nx, ny, count):
    deck = _quick_deck().replace(POINTS, POINTS +
        f"\nFRAME 'F' 0 0 0 2000 1000 {nx} {ny}" + FRAME_BLOCK)
    proc, report = _run_deck(tmp_path, deck)
    assert proc.returncode == 0, report[-2000:]
    values = [float(v) for v in (tmp_path / "work" / "f.blk").read_text().split()]
    assert len(values) == count
    assert all(math.isfinite(v) and v >= 0 for v in values)


def test_frame_point_count_cannot_overflow(tmp_path):
    deck = _quick_deck().replace(POINTS, POINTS +
        "\nFRAME 'F' 0 0 0 2000 1000 2147483647 20" + FRAME_BLOCK)
    proc, report = _run_deck(tmp_path, deck)
    assert proc.returncode == 1
    assert "more than 10 million output points on FRAME" in report


def test_table_format_reset_can_compute_with_maxerr_2(tmp_path):
    deck = _quick_deck().replace("SET NAUTICAL", "SET MAXERR 2 NAUTICAL")
    deck = deck.replace("TABLE 'CENTER' HEADER", "OUTPUT OPTIONS '%' TABLE 1000\nTABLE 'CENTER' NOHEADER")
    proc, report = _run_deck(tmp_path, deck)
    assert proc.returncode == 0, proc.stderr[-2000:]
    assert "[field] is too large" in report
    values = (tmp_path / "work" / "quick_test_center.tbl").read_text().split()
    assert float(values[3]) == pytest.approx(.8787, abs=.0001)


@pytest.mark.parametrize("quantity", ["WIND", "CURRENT", "FRICTION"])
@pytest.mark.parametrize("exception", ["", " EXCEPTION -99"])
def test_nan_in_auxiliary_field_stops_before_physics(tmp_path, quantity, exception):
    deck = _quick_deck().replace("GEN3 KOMEN", f"GEN3 KOMEN\nINPGRID {quantity} REGULAR 0 0 0 1 1 2000 1000{exception}\n"
                              f"READINP {quantity} 1 'aux.dat' 3 0 FREE")
    data = "NaN 1\n1 1\n" + ("0 0\n0 0\n" if quantity != "FRICTION" else "")
    proc, report = _run_deck(tmp_path, deck, {"aux.dat": data})
    assert proc.returncode == 1
    assert "NaN in auxiliary input field has no valid physical value" in report
    assert not (tmp_path / "work" / "quick_test_center.tbl").exists()


def test_nan_curvilinear_wind_is_not_used_as_a_velocity(tmp_path):
    deck = _quick_deck().replace(
        "CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10 CIRCLE 24 0.05 1.0 16",
        "CGRID CURVILINEAR 20 10 CIRCLE 24 0.05 1.0 16\nREADGRID COORDINATES 1 'coor.grd' 3 0 FREE")
    deck = deck.replace("WIND 8.0 270.0", "INPGRID WIND CURVILINEAR 0 0 20 10\nREADINP WIND 1 'wind.dat' 3 0 FREE")
    coords = '\n'.join(' '.join(str(i*100) for i in range(21)) for _ in range(11))+'\n'
    coords += '\n'.join(' '.join(str(j*100) for _ in range(21)) for j in range(11))+'\n'
    wind = '\n'.join(' '.join('NaN' if i == j == 5 else '8' for i in range(21)) for j in range(11))+'\n'
    wind += (' '.join(['0']*21)+'\n')*11
    proc, report = _run_deck(tmp_path, deck, {'coor.grd': coords, 'wind.dat': wind})
    assert proc.returncode == 1
    assert "NaN in auxiliary input field has no valid physical value" in report
    assert not (tmp_path / "work" / "quick_test_center.tbl").exists()


@pytest.mark.parametrize("files_in_series", [1, 2])
def test_series_end_retains_the_input_scale(tmp_path, files_in_series):
    deck = _quick_deck().replace("SET NAUTICAL", "SET MAXERR 3 NAUTICAL")
    deck = deck.replace("READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE",
                        "READINP BOTTOM 2.0 SERIES 'bottom.lst' 3 0 FREE")
    deck = deck.replace("POINTS 'CENTER' 1000.0 500.0", "POINTS 'CENTER' 0 1000")
    names = [f"short{i}.bot" for i in range(files_in_series)]
    files = {"bottom.lst": "\n".join(names)+"\n", names[0]: "10 10\n10\n"}
    if files_in_series == 2:
        files[names[1]] = "10\n"
    proc, report = _run_deck(tmp_path, deck, files)
    assert proc.returncode == 0, report[-2000:]
    rows = [l.split() for l in (tmp_path / "work" / "quick_test_center.tbl").read_text().splitlines()
            if l.strip() and not l.startswith('%')]
    assert float(rows[0][2]) == 20.


#  BF-10: values given beyond the length of a short segment must not move on
#  to the next segment.
def test_surplus_boundary_lengths_do_not_change_the_next_segment(tmp_path):
    boundary = "BOUNDSPEC SEGMENT IJ 0 0 0 10 VARIABLE PAR 0 1 6 270 20 1200 2 6 270 20{extra}\n" + \
               "BOUNDSPEC SEGMENT IJ 1 0 20 0 CONSTANT PAR 1.5 6 200 20"
    found = []
    for label, extra in (("plain", ""), ("surplus", " 1500 .2 6 270 20")):
        deck = _quick_deck().replace(BOUNDARY, boundary.format(extra=extra))
        deck = deck.replace(POINTS, "POINTS 'CENTER' 0 1000 2000 0")
        proc, report = _run_deck(tmp_path / label, deck)
        assert proc.returncode == 0, report[-2000:]
        rows = [l.split() for l in (tmp_path / label / "work" / "quick_test_center.tbl").read_text().splitlines()
                if l.strip() and not l.startswith('%')]
        found.append([float(r[3]) for r in rows])
    assert found[0] == found[1]
    assert found[0] == pytest.approx([1.872, 1.676], abs=.001)


#  BF-22: after an unknown quantity SREDEP stops, instead of reading past its
#  tables (BF-43 added this test).
@pytest.mark.parametrize("maxerr", [1, 3])
def test_unknown_readinp_quantity_is_dropped(tmp_path, maxerr):
    deck = _quick_deck().replace("SET NAUTICAL", f"SET MAXERR {maxerr} NAUTICAL")
    deck = deck.replace("GEN3 KOMEN", "GEN3 KOMEN\nREADINP FOO 1 'unused' 3 0 FREE")
    proc, report = _run_deck(tmp_path, deck)
    assert "Illegal keyword: FOO" in report
    assert proc.returncode == (1 if maxerr == 1 else 0)
    if maxerr == 3:
        assert (tmp_path / "work" / "quick_test_center.tbl").exists()


#  BF-14: a spread in degrees narrower than half a directional bin is reported
#  for constant and for variable parametric boundaries.
@pytest.mark.parametrize("branch", ["CONSTANT", "VARIABLE"])
@pytest.mark.parametrize("maxerr", [1, 2])
def test_directional_spread_check_covers_both_parameter_branches(tmp_path, branch, maxerr):
    deck = _quick_deck().replace("SET NAUTICAL", f"SET MAXERR {maxerr} NAUTICAL")
    replacement = "BOUNDSPEC SIDE WEST " + ("CONSTANT PAR 1 6 270 5" if branch == "CONSTANT"
                    else "VARIABLE PAR 0 1 6 270 5 1000 1 6 270 5")
    deck = deck.replace(BOUNDARY, replacement)
    proc, report = _run_deck(tmp_path, deck)
    assert report.count("directional spreading too narrow to be represented properly") == 1
    assert proc.returncode == (1 if maxerr == 1 else 0)
    assert (tmp_path / "work" / "quick_test_center.tbl").exists() == (maxerr == 2)


def test_st6_returns_finite_roughness_at_boundary_points(tmp_path):
    deck = _quick_deck().replace("GEN3 KOMEN", "GEN3 ST6 4.7E-7 6.6E-6 4 4 UP HWANG VECTAU U10PROXY 28 AGROW")
    deck = deck.replace("LAYOUT 3 HSIGN", "LAYOUT 3 ZLEN")
    proc, report = _run_deck(tmp_path, deck)
    assert proc.returncode == 0, report[-2000:]
    values = [float(v) for v in (tmp_path / "work" / "quick_test_hs.blk").read_text().split()]
    assert len(values) == 231
    assert all(math.isfinite(v) and 0 < v < .01 for v in values)
    assert values[::21] == pytest.approx([.0001026]*11, rel=.001)


def test_first_stationary_st6_guess_has_a_defined_drag(tmp_path):
    deck = _quick_deck().replace("GEN3 KOMEN", "GEN3 ST6 4.7E-7 6.6E-6 4 4 UP HWANG VECTAU U10PROXY 28 AGROW")
    deck = deck.replace("DEPTH HSIGN TM01 DIR", "DEPTH HSIGN TM01 DIR UFRIC")
    deck = deck.replace("\nCOMPUTE\n", "\nNUMERIC STOPC STAT MXITST=1\nCOMPUTE\n")
    proc, report = _run_deck(tmp_path, deck)
    assert proc.returncode == 0, report[-2000:]
    rows = [l.split() for l in (tmp_path / "work" / "quick_test_center.tbl").read_text().splitlines()
            if l.strip() and not l.startswith('%')]
    # WINDP1 uses Wu drag for the initial second-generation guess.
    assert float(rows[0][-1]) == pytest.approx(8*math.sqrt((.8+.065*8)*.001), rel=1e-5)


#  BF-57: the second search in CVMESH starts at the nearest active grid point
#  and can step into a mesh with a corner point without coordinates. The shape
#  of such a mesh is undefined (the corner lies at the exception value, here
#  400 km away), so the search may converge there at a wrong place. Upstream
#  accepted that, and the point got the depth of another place; when the
#  search failed, it went on with an unconverged estimate. The depth here
#  rises 0.5 m per mesh in x, so it shows where SWAN put the point.
def test_point_next_to_mesh_without_coordinates(tmp_path):
    n, d, ox, oy = 30, 100.0, 50000.0, 400000.0
    excluded = {(0, 0), (1, 0), (0, 1), (1, 1), (28, 28), (29, 28), (28, 29), (29, 29),
                (10, 10), (15, 12), (20, 20), (12, 22), (22, 8)}
    nodes = [[(0.0, 0.0) if (i, j) in excluded else
              (ox + i * d + 3.0 * j, oy + j * d + 2.0 * i) for i in range(n)] for j in range(n)]
    bottom = [[10.0 + 0.5 * i for i in range(n)] for _ in range(n)]
    rng = random.Random(1)
    points = []
    for i, j in sorted(excluded):
        if 1 < i < 28:
            for _ in range(60):
                points.append((ox + i * d + 3.0 * j + rng.uniform(-150, 150),
                               oy + j * d + 2.0 * i + rng.uniform(-150, 150)))
    depths, report = _curvilinear_depths(tmp_path, nodes, bottom, points, exception="0 0")
    wrong = []
    for (x, y), depth in zip(points, depths):
        #  grid indices from the exact inverse of x = i d + 3 j, y = j d + 2 i
        det = d * d - 6.0
        i = ((x - ox) * d - 3.0 * (y - oy)) / det
        j = ((y - oy) * d - 2.0 * (x - ox)) / det
        i0, j0 = int(i), int(j)
        if {(i0, j0), (i0 + 1, j0), (i0, j0 + 1), (i0 + 1, j0 + 1)} & excluded:
            continue
        if abs(depth - (10.0 + 0.5 * i)) > 0.03:
            wrong.append((round(x, 1), round(y, 1), depth, round(10.0 + 0.5 * i, 3)))
    assert not wrong, wrong[:5]


@pytest.mark.parametrize("offset", [0., -0.01])
def test_grid_lines_next_to_missing_coordinates_keep_output(tmp_path, offset):
    n, d, ox, oy = 30, 100., 50000., 400000.
    excluded = {(10, 10), (15, 12), (20, 20), (12, 22), (22, 8)}
    nodes = [[(0., 0.) if (i, j) in excluded else
              (ox+i*d+3*j, oy+j*d+2*i) for i in range(n)] for j in range(n)]
    bottom = [[10.+0.5*i for i in range(n)] for _ in range(n)]
    ij = [(i-1+offset/d, j+v) for i, j in sorted(excluded)
          for v in (-1., -.75, -.5, 0., .5, .75, 1.)]
    points = [(ox+i*d+3*j, oy+j*d+2*i) for i, j in ij]
    depths, report = _curvilinear_depths(tmp_path, nodes, bottom, points, exception="0 0")
    assert depths == pytest.approx([10.+.5*i for i, _ in ij], abs=.03), report[-2000:]


def test_outline_closes_each_curve_when_there_is_a_hole(tmp_path):
    nodes, bottom = _fan_grid(
        lambda i, j: (15 <= i <= 25 and 33 <= j <= 34) or (i == 26 and j == 33),
        [lambda i, j: i < 2 and j < 2])
    depths, report = _curvilinear_depths(tmp_path, nodes, bottom,
                                        [(120., 250.), (130., 300.)])
    assert depths == [-99., -99.], report[-2000:]


#  BF-60: a point that is not in any mesh, but within 1% of a side length from
#  the grid outline, counts as a point on the boundary. CVMESH looks for that
#  side in a loop over the outline that, like INMESH before BF-56, never
#  looked at the side that closes each curve: here the first side of the west
#  edge, from (0, 100) back to the start point (0, 0). A point 0.3 m west of
#  that side got no output, the same point 400 m further north did.
def test_point_just_outside_closing_side_of_outline(tmp_path):
    nodes = [[(100.0 * i, 100.0 * j) for i in range(10)] for j in range(10)]
    bottom = [[20.0] * 10 for _ in range(10)]
    points = [(-0.3, 50.0), (-0.3, 450.0), (450.0, -0.3), (-5.0, 50.0)]
    depths, report = _curvilinear_depths(tmp_path, nodes, bottom, points)
    assert depths[:3] == [pytest.approx(20.0)] * 3, report[-2000:]
    #  5 m away is not on the boundary
    assert depths[3] == pytest.approx(-99.0)


#  BF-61: SETUPP adds the set-up to the water depth inside the iterations of
#  SWCOMP, so the check on NaN depths also runs there. The set-up examples
#  (examples/setup) run in CTest with their own decks; this run adds SETUP to
#  the quick test, so that the check is reached there and does not stop a
#  valid set-up computation.
def test_setup_run_passes_depth_check(tmp_path):
    returncode, report = _run(tmp_path, "SETUP")
    assert returncode == 0, report[-2000:]
    assert " SETUP" in report
    assert "water depth is NaN" not in report


#  BF-59: upstream treated a NaN in an input file as the exception value
#  without a message (EQREAL finds NaN equal to anything), and the bilinear
#  interpolation of SVALQI multiplied the exception corner, with weight 0,
#  by its value: 0 times NaN is NaN, so the neighbouring points west and south
#  of a NaN became exception points as well. A NaN must now act exactly as the
#  exception value, with a warning.
def test_nan_in_bottom_acts_as_exception_value(tmp_path):
    def run(sub, value):
        rows = [" ".join(value if i in (10, 11) and j in (4, 5) else "20.0" for i in range(21))
                for j in range(11)]
        deck = _quick_deck().replace(
            "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0",
            "INPGRID BOTTOM REGULAR 0.0 0.0 0.0 20 10 100.0 100.0 EXC -99999")
        assert "EXC -99999" in deck, "smoke deck has changed shape"
        result, report = _run_deck(tmp_path / sub, deck, {"bottom.bot": "\n".join(rows) + "\n"},
                                   timeout=60)
        assert result.returncode == 0, report[-2000:]
        return (tmp_path / sub / "work" / "quick_test_hs.blk").read_text(), report
    with_exception, _ = run("exc", "-99999")
    with_nan, report = run("nan", "NaN")
    assert with_nan == with_exception
    assert "4 NaN values in input field BOTTOM" in report


#  BF-54: after BOUNDSPEC SEGMENT on an unstructured grid SWAN checked whether the
#  boundary point found has a boundary marker, with `IX2.GT.0 .AND. vert(IX2)...`.
#  Fortran does not promise that the right-hand side of .AND. is skipped, so
#  `vert` could be read outside the table for a point outside the grid
#  (IX2 = -1), for vertex 0 and for a vertex number above the number of
#  vertices; with runtime checks that did happen, and in a release build
#  SWAN crashed before anything reached PRINT. Upstream has this too. With
#  the fix every build must give the message.
@pytest.mark.parametrize("segment, message", [
    ("SEGMENT XY -5000.0 500.0 0.0 1000.0", "not part of computational grid"),
    ("SEGMENT IJ 0 1", "0 is not a valid vertex index"),
    ("SEGMENT IJ 99 1", "99 is not a valid vertex index"),
], ids=["xy-outside-grid", "ij-vertex-0", "ij-vertex-too-large"])
def test_boundary_segment_outside_unstructured_grid(tmp_path, segment, message):
    deck = (UNSTRUCTURED / "nonstationary_unstructured.swn").read_text()
    old = "BOUNDSPEC SIDE 1 CONSTANT"
    assert old in deck, "example deck has changed shape"
    deck = deck.replace(old, f"BOUNDSPEC {segment} CONSTANT", 1)
    files = {f"nonstationary_unstructured.{ext}":
             (UNSTRUCTURED / f"nonstationary_unstructured.{ext}").read_text()
             for ext in ("node", "ele")}
    files["wind.wnd"] = (UNSTRUCTURED / "wind.wnd").read_text()
    result, report = _run_deck(tmp_path, deck, files, timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert message in report, report[-2000:]


#  BF-53: the readers of Triangle and ADCIRC grids used the number
#  on each line as an index into the node and element tables, and nobody
#  checked whether a triangle pointed to existing, distinct nodes.
#  A file that starts numbering at 0 (which Triangle allows), a
#  triangle with a non-existent node or a grid without boundary markers
#  gave a segmentation fault, a write outside the table or a hang.
UNSTRUCTURED = REPO / "examples" / "nonstationary" / "unstructured"


@pytest.mark.parametrize("change, message", [
    (("ele", "1 1 2 7", "1 0 2 7"), "element refers to a vertex that does not exist"),
    (("ele", "1 1 2 7", "1 1 1 7"), "element with a repeated vertex"),
    (("node", "1 0.0 0.0 1", "0 0.0 0.0 1"), "vertex number out of range in Triangle file"),
    (("node", None, None), "no boundary vertices in the unstructured grid"),
], ids=["vertex-0", "repeated-vertex", "numbering-from-0", "no-boundary-markers"])
def test_broken_unstructured_grid_stops_cleanly(tmp_path, change, message):
    grid_files = {
        "node": (UNSTRUCTURED / "nonstationary_unstructured.node").read_text(),
        "ele": (UNSTRUCTURED / "nonstationary_unstructured.ele").read_text(),
    }
    kind, old, new = change
    if old is None:
        # all boundary markers set to 0
        lines = grid_files["node"].splitlines()
        grid_files["node"] = "\n".join(
            [lines[0]] + [" ".join(r.split()[:3] + ["0"]) for r in lines[1:]]) + "\n"
    else:
        assert old in grid_files[kind], "example grid has changed shape"
        grid_files[kind] = grid_files[kind].replace(old, new, 1)
    deck = (UNSTRUCTURED / "nonstationary_unstructured.swn").read_text()
    deck = deck.replace("SET NAUTICAL", "SET NAUTICAL\nSET MAXERR 3", 1)
    files = {"grid.node": grid_files["node"], "grid.ele": grid_files["ele"],
             "wind.wnd": (UNSTRUCTURED / "wind.wnd").read_text()}
    deck = deck.replace("'nonstationary_unstructured'", "'grid'", 1)
    result, report = _run_deck(tmp_path, deck, files, timeout=60)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 1, (result.returncode, result.stderr[-2000:])
    assert f"Terminating error: {message}" in report, report[-2000:]


#  BF-64 and BF-65: the exact quadruplet code (XNL, QUADRUPLET 51-53) reads an
#  optional configuration file xnl4v5.cfg. Its check let through two options
#  that the code does not have: COUPLING 3 (Gorman), for which the coupling
#  coefficient was never set, so that SWAN computed on with garbage (upstream
#  odd wave heights, the fork NaN, both with exit code 0), and LOCUS 0, for
#  which the locus was never computed. And SWCOMP ignored the error that the
#  initialisation reports, so that a value the check does refuse, such as
#  COUPLING 5, ended in a segmentation fault. Now each of these stops the
#  run with a message.
@pytest.mark.parametrize("setting", ["COUPLING 3", "LOCUS 0", "COUPLING 5"],
                         ids=["coupling-gorman", "locus-0", "coupling-5"])
def test_wrong_xnl_configuration_stops_cleanly(tmp_path, setting):
    deck = _quick_deck()
    old = "GEN3 KOMEN"
    assert old in deck, "quick test deck has changed shape"
    deck = deck.replace(old, "GEN3 KOMEN\nQUADRUPLET 51", 1)
    result, report = _run_deck(tmp_path, deck, {"xnl4v5.cfg": setting + "\n"},
                               timeout=120)
    assert "Fortran runtime error" not in result.stderr, result.stderr[-2000:]
    assert result.returncode == 1, (result.returncode, result.stderr[-2000:])
    assert ("initialisation of the exact quadruplet computation (XNL) failed"
            in report), report[-2000:]


#  A frequency grid that ends too close to the spectral peak cuts off part of
#  the spectrum; the manual asks for [fhigh] of at least 2.5 to 3 times the
#  peak frequency. Upstream says nothing. The fork warns after a stationary
#  computation (swan_frequency_range) and changes nothing else.
@pytest.mark.parametrize("short_grid", [False, True], ids=["fine-grid", "short-grid"])
def test_short_frequency_grid_gives_a_warning(tmp_path, short_grid):
    deck = _quick_deck()
    if short_grid:
        for old, new in (("CIRCLE 24 0.05 1.0 16", "CIRCLE 24 0.05 0.5 16"),
                         ("WIND 8.0 270.0", "WIND 5.0 270.0"),
                         ("BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0", "")):
            assert old in deck, "quick test deck has changed shape"
            deck = deck.replace(old, new, 1)
    result, report = _run_deck(tmp_path, deck, timeout=60)
    assert result.returncode == 0, result.stderr[-2000:]
    warned = "frequency grid too short" in report
    assert warned == short_grid, report[-2000:]
