"""Tests for the complete-matrix wet-point equivalence gate."""

from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pytest
from scipy.io import savemat

import analyze_validation


def _write_run(
    root: Path,
    target: str,
    *,
    field: np.ndarray,
    points: np.ndarray,
    tm01: np.ndarray | None = None,
    direction: np.ndarray | None = None,
    convergence: list[float] | None = None,
) -> None:
    directory = root / target / "case" / "replicate-001"
    directory.mkdir(parents=True)
    metadata = {
        "condition": {"condition_id": "case"},
        "target": {"id": target},
        "executable_sha256": "current-hash" if target.startswith("current_") else target,
        "returncode": 0,
        "error": None,
    }
    (directory / "run.json").write_text(json.dumps(metadata))
    savemat(directory / "scaloost_rp.mat", {"Hsig": field})
    table = np.zeros((points.size, 6))
    table[:, 3] = points
    table[:, 4] = points if tm01 is None else tm01
    table[:, 5] = points if direction is None else direction
    header = (
        "%\n%\n"
        "%       Xp            Yp            Depth         Hsig          Tm01          Dir\n"
    )
    (directory / "uitvoerpunten.tab").write_text(
        header
        + "".join(" ".join(f"{value:.4f}" for value in row) + "\n" for row in table)
    )
    series = [5.0, 98.0] if convergence is None else convergence
    (directory / "PRINT").write_text(
        "".join(
            f" accuracy OK in {value:.2f} % of wet grid points ( 98.00 % required)\n"
            for value in series
        )
    )
    spectrum = (
        "SWAN   1 test\n"
        "LOCATIONS x\n"
        "   3                                  number of locations\n"
        "    0.0 0.0\n"
        "    1.0 0.0\n"
        "    2.0 0.0\n"
        "RFREQ hz\n"
        "    2                                  number of frequencies\n"
        "    0.10\n"
        "    0.20\n"
        "QUANT\n"
        "     2                                  number of quantities in table\n"
        "VaDens m2/Hz\n"
        "m2/Hz unit\n"
        "   -0.9900E+02                          exception value\n"
        "NDIR degr\n"
        "degr unit\n"
        "   -0.9990E+03                          exception value\n"
        "LOCATION     1\n"
        "  0.5  10.0\n"
        "  1.5  20.0\n"
        "LOCATION     2\n"
        "  0.5  10.0\n"
        "  1.5  20.0\n"
        "NODATA\n"
    )
    (directory / "uitvoerpunten.sp1").write_text(spectrum)
    (directory / "uitvoerpunten.sp2").write_text(spectrum)


def _matrix(tmp_path: Path) -> tuple[Path, Path]:
    root = tmp_path / "runs"
    manifest = tmp_path / "conditions.json"
    manifest.write_text(json.dumps({"conditions": [{"id": "case"}]}))
    common = np.array([[1.0, 2.0], [-9.0, 4.0]])
    common_points = np.array([1.0, -9.0, 4.0])
    for target in analyze_validation.TARGETS:
        _write_run(root, target, field=common, points=common_points)
    return root, manifest


def test_complete_matrix_requires_exact_default_and_reports_wet_points(tmp_path: Path):
    root, manifest = _matrix(tmp_path)

    text = analyze_validation.analyze(root, manifest)

    assert "1/1 conditions exactly equal" in text
    assert "| `case` | 2/1 |" in text
    assert "Current executable SHA-256: `current-hash`" in text
    assert "## Extended quantities" in text
    assert "bit-equal in all 1 conditions" in text


def test_default_difference_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    savemat(changed / "scaloost_rp.mat", {"Hsig": [[1.0, 2.1], [-9.0, 4.0]]})

    with pytest.raises(ValueError, match="current default differs"):
        analyze_validation.analyze(root, manifest)


def test_default_tm01_difference_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    table = np.loadtxt(changed / "uitvoerpunten.tab", comments="%")
    table[:, 4] += 0.5
    header = (
        "%\n%\n"
        "%       Xp            Yp            Depth         Hsig          Tm01          Dir\n"
    )
    (changed / "uitvoerpunten.tab").write_text(
        header
        + "".join(" ".join(f"{value:.4f}" for value in row) + "\n" for row in table)
    )

    with pytest.raises(ValueError, match="Tm01, direction or convergence"):
        analyze_validation.analyze(root, manifest)


def test_default_convergence_difference_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    (changed / "PRINT").write_text(
        " accuracy OK in   5.66 % of wet grid points ( 98.00 % required)\n"
    )

    with pytest.raises(ValueError, match="Tm01, direction or convergence"):
        analyze_validation.analyze(root, manifest)


def test_missing_print_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    (changed / "PRINT").unlink()

    with pytest.raises((FileNotFoundError, ValueError), match="PRINT|convergence"):
        analyze_validation.analyze(root, manifest)


def test_default_spectra_difference_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    text = (changed / "uitvoerpunten.sp1").read_text().replace(
        "  1.5  20.0\n", "  2.5  20.0\n", 1)

    (changed / "uitvoerpunten.sp1").write_text(text)

    with pytest.raises(ValueError, match="spectra differ"):
        analyze_validation.analyze(root, manifest)


def test_missing_spectra_fail_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    (changed / "uitvoerpunten.sp2").unlink()

    with pytest.raises((FileNotFoundError, ValueError), match="spectrum"):
        analyze_validation.analyze(root, manifest)
