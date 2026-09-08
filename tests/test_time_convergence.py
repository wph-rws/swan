"""Eenheid- en negatieve tests voor de tijdstap-runner (rapporterend)."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest


def _load_runner() -> object:
    # Locatiegebonden import onder een unieke modulenaam (zie test_shoaling).
    path = (Path(__file__).resolve().parent.parent / "examples"
            / "time_convergence" / "run.py")
    spec = importlib.util.spec_from_file_location("time_convergence_run", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules["time_convergence_run"] = module
    spec.loader.exec_module(module)
    return module


_run = _load_runner()
report = _run.report


def _write_run(work: Path, step: int, values: list[float],
               times: list[str] | None = None) -> None:
    case = work / f"dt{step}"
    case.mkdir(parents=True, exist_ok=True)
    stamps = times if times is not None else [
        "20260101.000000", "20260101.002000"]
    (case / "nonstationary_regular_center.tbl").write_text(
        "%\n%\n" + "".join(
            f"{stamp} 0.0 1000.0 500.0 20.0 4.0 {value:.5f} 4.0 270.0\n"
            for stamp, value in zip(stamps, values)) + "\n")
    (case / "norm_end").write_text("ok\n")


def test_report_green_and_smoke(tmp_path: Path, capsys):
    for step in (1, 2, 4):
        _write_run(tmp_path, step, [0.5, 1.0])
    report(tmp_path, False)
    assert "uitvoertijden gelijk" in capsys.readouterr().out
    report(tmp_path, True)
    assert "smoke" in capsys.readouterr().out


def test_report_red_on_shifted_times(tmp_path: Path):
    for step in (1, 2, 4):
        _write_run(tmp_path, step, [0.5, 1.0])
    _write_run(tmp_path, 4, [0.5, 1.0],
               ["20260101.000000", "20260101.003000"])
    with pytest.raises(RuntimeError, match="uitvoertijden wijken af"):
        report(tmp_path, False)


def test_report_red_on_non_finite(tmp_path: Path):
    for step in (1, 2, 4):
        _write_run(tmp_path, step, [0.5, 1.0])
    table = tmp_path / "dt2" / "nonstationary_regular_center.tbl"
    table.write_text(table.read_text().replace("1.00000", "nan"))
    with pytest.raises(RuntimeError, match="eindige Hsig"):
        report(tmp_path, False)


def test_report_red_without_norm_end(tmp_path: Path):
    for step in (1, 2, 4):
        _write_case = _write_run(tmp_path, step, [0.5, 1.0])
    (tmp_path / "dt4" / "norm_end").unlink()
    with pytest.raises(RuntimeError, match="geen norm_end"):
        report(tmp_path, False)
