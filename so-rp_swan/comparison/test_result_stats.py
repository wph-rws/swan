"""Tests for strict SWAN wet/dry statistics."""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
from scipy.io import savemat

from result_stats import (
    EXCV,
    WetValues,
    compare,
    compare_circular,
    read_convergence_history,
    read_dir_points,
    read_hsig_field,
    read_hsig_points,
    read_tm01_points,
)


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


QUICK_HEADER = (
    "%       Xp            Yp            Depth         Hsig          Tm01          Dir\n"
)
OPERATIONAL_HEADER = (
    "%       Xp            Yp            Depth         Hsig          Period        "
    "Tm01          Tm02          RTpeak        Dir           Dspr          FSpr          dHs\n"
)


def _write_table(path: Path, header: str, rows: list[list[float]]) -> None:
    path.write_text("%\n%\n" + header + "".join(
        " ".join(f"{value:.4f}" for value in row) + "\n" for row in rows
    ))


def test_tm01_and_dir_read_by_name_not_position(tmp_path: Path):
    # Operationele kolomvolgorde: Hsig=3, Tm01=5, Dir=8 (géén 3/4/5).
    path = tmp_path / "points.tab"
    _write_table(path, OPERATIONAL_HEADER, [
        [0.0, 0.0, 10.0, 2.0, 9.9, 3.0, 2.5, 4.0, 45.0, 20.0, 0.5, 0.01],
        [0.0, 0.0, 10.0, EXCV, 9.9, 3.0, 2.5, 4.0, 45.0, 20.0, 0.5, 0.01],
    ])
    tm = read_tm01_points(path)
    direction = read_dir_points(path)
    assert (tm.wet_count, tm.dry_count) == (1, 1)
    assert tm.mean == pytest.approx(3.0)
    assert direction.mean == pytest.approx(45.0)


def test_quick_style_table_still_reads(tmp_path: Path):
    path = tmp_path / "points.tab"
    _write_table(path, QUICK_HEADER, [
        [0.0, 0.0, 10.0, 2.0, 3.5, 90.0],
        [0.0, 0.0, 10.0, EXCV, 3.5, 90.0],
    ])
    assert read_tm01_points(path).mean == pytest.approx(3.5)
    assert read_dir_points(path).mean == pytest.approx(90.0)


def test_missing_quantity_column_is_a_hard_error(tmp_path: Path):
    path = tmp_path / "points.tab"
    _write_table(path, QUICK_HEADER, [[0.0, 0.0, 10.0, 2.0, 3.5, 90.0]])
    text = path.read_text().replace("Tm01", "Nope")
    path.write_text(text)
    with pytest.raises(ValueError, match="no Tm01 column"):
        read_tm01_points(path)


def test_circular_direction_wraps_around():
    reference = WetValues(np.array([359.0, 10.0]), np.array([True, True]), "r")
    candidate = WetValues(np.array([1.0, 12.0]), np.array([True, True]), "c")
    hs = WetValues(np.array([1.0, 1.0]), np.array([True, True]), "hs")
    stats = compare_circular(reference, candidate, hs, hs)
    assert stats.maximum_absolute == pytest.approx(2.0)
    assert stats.count == 2


def test_circular_direction_ignores_calm_points():
    reference = WetValues(np.array([0.0, 10.0]), np.array([True, True]), "r")
    candidate = WetValues(np.array([180.0, 10.0]), np.array([True, True]), "c")
    ref_hs = WetValues(np.array([0.01, 1.0]), np.array([True, True]), "hs")
    cand_hs = WetValues(np.array([0.01, 1.0]), np.array([True, True]), "hs")
    stats = compare_circular(reference, candidate, ref_hs, cand_hs)
    assert stats.maximum_absolute == pytest.approx(0.0)
    assert stats.count == 1


def test_circular_direction_without_energy_is_vacuous():
    calm = WetValues(np.array([0.0]), np.array([True]), "r")
    other = WetValues(np.array([180.0]), np.array([True]), "c")
    hs = WetValues(np.array([0.01]), np.array([True]), "hs")
    stats = compare_circular(calm, other, hs, hs)
    assert (stats.bias, stats.rms, stats.maximum_absolute, stats.count) == (
        0.0, 0.0, 0.0, 0)


def test_circular_direction_mask_mismatch_is_a_hard_error():
    reference = WetValues(np.array([1.0, 2.0]), np.array([True, True]), "r")
    candidate = WetValues(np.array([1.0, 2.0]), np.array([True, False]), "c")
    hs = WetValues(np.array([1.0, 1.0]), np.array([True, True]), "hs")
    with pytest.raises(ValueError, match="mask mismatch"):
        compare_circular(reference, candidate, hs, hs)


def test_convergence_history_reads_accuracy_series(tmp_path: Path):
    path = tmp_path / "PRINT"
    path.write_text(
        " Accuracy parameters : DREL   0.1000E-01 NPNTS   0.9800E+02\n"
        " accuracy OK in   5.66 % of wet grid points ( 98.00 % required)\n"
        " accuracy OK in  98.14 % of wet grid points ( 98.00 % required)\n"
    )
    history = read_convergence_history(path)
    assert list(history) == pytest.approx([5.66, 98.14])


def test_convergence_history_without_series_is_a_hard_error(tmp_path: Path):
    path = tmp_path / "PRINT"
    path.write_text(" Accuracy parameters : DREL   0.1000E-01\n")
    with pytest.raises(ValueError, match="no convergence history"):
        read_convergence_history(path)
    with pytest.raises(FileNotFoundError, match="missing PRINT"):
        read_convergence_history(tmp_path / "absent")
