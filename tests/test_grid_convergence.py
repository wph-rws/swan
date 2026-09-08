"""Eenheid- en negatieve tests voor de roosterconvergentie-runner."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest


def _load_runner() -> object:
    # Locatiegebonden import onder een unieke modulenaam (zie test_shoaling).
    path = (Path(__file__).resolve().parent.parent / "examples"
            / "grid_convergence" / "run.py")
    spec = importlib.util.spec_from_file_location("grid_convergence_run", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules["grid_convergence_run"] = module
    spec.loader.exec_module(module)
    return module


_run = _load_runner()
check_all = _run.check_all
circular_distance = _run.circular_distance
estimated_order = _run.estimated_order


def test_estimated_order_first_order():
    assert estimated_order(0.0, 0.5, 0.75) == pytest.approx(1.0)


def test_estimated_order_bit_equal_pair_raises():
    with pytest.raises(ValueError, match="geen orde te bepalen"):
        estimated_order(0.0, 0.5, 0.5)


def test_circular_distance_wraps_around():
    assert circular_distance(359.0, 1.0) == pytest.approx(2.0)


def _write_case(work: Path, nx: int, hsig: float) -> None:
    case = work / f"nx{nx}"
    case.mkdir(parents=True, exist_ok=True)
    (case / f"c{nx}.tbl").write_text(
        "%\n%\n"
        f"1000.0 500.0 10.0000 {hsig:.5f} 270.000\n"
    )
    (case / "PRINT").write_text(
        " accuracy OK in 100.00 % of wet grid points ( 99.50 % required)\n"
    )


def test_check_all_green_and_red(tmp_path: Path):
    for nx, hsig in ((50, 0.99894), (100, 1.00042), (200, 1.00117)):
        _write_case(tmp_path, nx, hsig)
    check_all(tmp_path, False)
    _write_case(tmp_path, 100, 1.05)
    with pytest.raises(RuntimeError):
        check_all(tmp_path, False)


def test_check_all_red_without_convergence(tmp_path: Path):
    for nx, hsig in ((50, 0.99894), (100, 1.00042), (200, 1.00117)):
        _write_case(tmp_path, nx, hsig)
    (tmp_path / "nx50" / "PRINT").write_text("niets hier\n")
    with pytest.raises(RuntimeError, match="convergentiegeschiedenis"):
        check_all(tmp_path, False)
