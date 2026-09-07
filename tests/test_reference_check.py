"""Negatieve tests: onvolledige controle mag nooit groen zijn."""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

EXAMPLES = Path(__file__).resolve().parent.parent / "examples"
sys.path.insert(0, str(EXAMPLES))
from reference_check import compare_with_reference, same_results  # noqa: E402


@pytest.fixture()
def pair(tmp_path):
    produced = tmp_path / "produced"
    reference = tmp_path / "reference"
    produced.mkdir()
    reference.mkdir()
    (reference / "a.tbl").write_text("1.0 2.0\n3.0 4.0\n")
    (produced / "a.tbl").write_text("1.0 2.0\n3.0 4.0\n")
    return produced, reference


def test_geldige_vergelijking_groen(pair):
    produced, reference = pair
    assert compare_with_reference(produced, reference, ("a.tbl",)) is True


def test_ontbrekende_uitvoer_rood(pair):
    produced, reference = pair
    (produced / "a.tbl").unlink()
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_een_ontbrekende_referentie_naast_geldige_rood(pair):
    produced, reference = pair
    (reference / "b.tbl").write_text("1.0\n")
    (produced / "b.tbl").write_text("1.0\n")
    # verwijder b uit referentie: gedeeltelijk ontbrekende set moet falen
    (reference / "b.tbl").unlink()
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl", "b.tbl"))


def test_lege_referentielijst_rood(pair):
    produced, reference = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ())


def test_verkeerde_referentielijst_rood(pair):
    produced, reference = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("bestaat-niet.tbl",))


def test_lege_bestanden_rood(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("")
    assert same_results(produced / "a.tbl", reference / "a.tbl") is False
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_verminkt_bestand_rood(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("1.0 2.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_nan_inf_rood(pair, tmp_path):
    produced, reference = pair
    (produced / "a.tbl").write_text("nan 2.0\n3.0 4.0\n")
    assert same_results(produced / "a.tbl", reference / "a.tbl") is False
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))
    (produced / "a.tbl").write_text("inf 2.0\n3.0 4.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))
    (reference / "a.tbl").write_text("nan 2.0\n3.0 4.0\n")
    (produced / "a.tbl").write_text("nan 2.0\n3.0 4.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_gewijzigde_uitkomst_rood(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("1.5 2.0\n3.0 4.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_ontbrekende_referentiemap_rood(pair, tmp_path):
    produced, _ = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, tmp_path / "ontbreekt", ("a.tbl",))


def test_smoke_telt_niet_als_regressie(pair):
    produced, reference = pair
    assert (
        compare_with_reference(produced, reference, ("a.tbl",), smoke=True) is False
    )


def test_runner_exitstatus_rood_bij_ontbrekende_referentie(tmp_path):
    # Controleer de exitstatus van de runner, niet alleen de helper.
    runner = EXAMPLES / "quick_test" / "run.py"
    result = subprocess.run(
        [
            sys.executable,
            str(runner),
            "--swan-executable",
            "/bestaat-niet/swan.exe",
            "--work-directory",
            str(tmp_path / "work"),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
