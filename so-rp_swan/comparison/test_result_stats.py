"""Tests for strict SWAN wet/dry statistics."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
from scipy.io import savemat

from result_stats import EXCV, WetValues, compare, read_hsig_field, read_hsig_points


def test_field_mean_excludes_nan_and_excv(tmp_path: Path):
    path = tmp_path / "field.mat"
    savemat(path, {"Hsig": np.array([[1.0, np.nan], [3.0, EXCV]])})
    values = read_hsig_field(path)
    assert (values.wet_count, values.dry_count) == (2, 2)
    assert values.mean == 2.0


def test_point_mean_excludes_three_land_points(tmp_path: Path):
    path = tmp_path / "points.tab"
    table = np.zeros((135, 12))
    table[:, 3] = 2.0
    table[-3:, 3] = EXCV
    np.savetxt(path, table)
    values = read_hsig_points(path)
    assert (values.wet_count, values.dry_count) == (132, 3)
    assert values.mean == 2.0


def test_compare_uses_the_proven_shared_wet_mask():
    reference = WetValues(
        np.array([1.0, 2.0, np.nan]), np.array([True, True, False]), "reference"
    )
    candidate = WetValues(
        np.array([2.0, 0.0, np.nan]), np.array([True, True, False]), "candidate"
    )
    stats = compare(reference, candidate)
    assert stats.bias == pytest.approx(-0.5)
    assert stats.rms == pytest.approx(np.sqrt(2.5))
    assert stats.maximum_absolute == 2.0
    assert stats.count == 2


def test_changed_wet_dry_mask_is_a_hard_error():
    reference = WetValues(
        np.array([1.0, np.nan]), np.array([True, False]), "reference"
    )
    candidate = WetValues(
        np.array([1.0, 2.0]), np.array([True, True]), "candidate"
    )
    with pytest.raises(ValueError, match=r"1 samples \(1 became wet, 0 became dry\)"):
        compare(reference, candidate)


@pytest.mark.parametrize("bad", [np.nan, np.inf, -np.inf])
def test_point_nan_or_infinity_is_a_hard_error(tmp_path: Path, bad: float):
    path = tmp_path / "points.tab"
    table = np.zeros((2, 12))
    table[1, 3] = bad
    np.savetxt(path, table)
    with pytest.raises(ValueError, match="NaN or infinity"):
        read_hsig_points(path)


def test_infinity_in_field_is_a_hard_error(tmp_path: Path):
    path = tmp_path / "field.mat"
    savemat(path, {"Hsig": np.array([[1.0, np.inf]])})
    with pytest.raises(ValueError, match="infinity"):
        read_hsig_field(path)


def test_missing_files_are_hard_errors(tmp_path: Path):
    with pytest.raises(FileNotFoundError, match="missing SWAN block output"):
        read_hsig_field(tmp_path / "missing.mat")
    with pytest.raises(FileNotFoundError, match="missing SWAN point output"):
        read_hsig_points(tmp_path / "missing.tab")
