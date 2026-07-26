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
    table = np.zeros((points.size, 4))
    table[:, 3] = points
    np.savetxt(directory / "uitvoerpunten.tab", table)


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


def test_default_difference_fails_the_gate(tmp_path: Path):
    root, manifest = _matrix(tmp_path)
    changed = root / "current_4151_default" / "case" / "replicate-001"
    savemat(changed / "scaloost_rp.mat", {"Hsig": [[1.0, 2.1], [-9.0, 4.0]]})

    with pytest.raises(ValueError, match="current default differs"):
        analyze_validation.analyze(root, manifest)
