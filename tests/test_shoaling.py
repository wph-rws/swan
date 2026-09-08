"""Eenheid- en negatieve tests voor de analytische shoaling-runner."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest


def _load_runner() -> object:
    # Locatiegebonden import onder een unieke modulenaam: meerdere runners
    # heten `run.py`; via sys.path zouden ze elkaar overschaduwen in één
    # pytest-sessie (gevonden doordat een gecombineerde run faalde).
    path = (Path(__file__).resolve().parent.parent / "examples" / "shoaling"
            / "run.py")
    spec = importlib.util.spec_from_file_location("shoaling_run", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules["shoaling_run"] = module
    spec.loader.exec_module(module)
    return module


_run = _load_runner()
check_case = _run.check_case
circular_distance = _run.circular_distance
green_ratio = _run.green_ratio
intended_depth = _run.intended_depth
refraction_factor = _run.refraction_factor
snell_from_direction = _run.snell_from_direction


def test_green_ratio_is_one_at_reference_depth():
    assert green_ratio(18.3, 18.3) == pytest.approx(1.0)


def test_snell_identity_at_reference_depth():
    assert snell_from_direction(300.0, 8.1, 8.1) == pytest.approx(300.0)


def test_refraction_factor_identity_at_reference_depth():
    assert refraction_factor(300.0, 8.1, 8.1) == pytest.approx(1.0)


def test_circular_distance_wraps_around():
    assert circular_distance(359.0, 1.0) == pytest.approx(2.0)
    assert circular_distance(270.0, 270.0) == pytest.approx(0.0)


def _write_synthetic_case(directory: Path, hsig: list[float]) -> None:
    rows = "\n".join(
        f"{x:.1f} 2000.0 {10.0:.4f} {h:.5f} 270.000"
        for x, h in zip((500.0, 1500.0, 2500.0, 3500.0), hsig))
    (directory / "flat.tbl").write_text("%\n%\n" + rows + "\n")
    (directory / "PRINT").write_text(
        " accuracy OK in 100.00 % of wet grid points ( 99.50 % required)\n")


def test_check_case_red_on_tampered_table(tmp_path: Path):
    _write_synthetic_case(tmp_path, [1.0, 1.0, 1.0, 1.0])
    check_case(tmp_path, True, False, False)
    _write_synthetic_case(tmp_path, [1.0, 1.0, 1.0, 5.0])
    with pytest.raises(RuntimeError, match="behoud geschonden"):
        check_case(tmp_path, True, False, False)


def test_check_case_red_without_convergence(tmp_path: Path):
    _write_synthetic_case(tmp_path, [1.0, 1.0, 1.0, 1.0])
    (tmp_path / "PRINT").write_text("geen geschiedenis hier\n")
    with pytest.raises(RuntimeError, match="convergentiegeschiedenis"):
        check_case(tmp_path, True, False, False)


def test_intended_depth_contract():
    assert intended_depth(0.0, True) == pytest.approx(10.0)
    assert intended_depth(500.0, False) == pytest.approx(18.3)
