"""Negative tests: an incomplete check may never be green."""
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


def test_valid_comparison_is_green(pair):
    produced, reference = pair
    assert compare_with_reference(produced, reference, ("a.tbl",)) is True


def test_missing_output_is_red(pair):
    produced, reference = pair
    (produced / "a.tbl").unlink()
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_one_missing_reference_beside_valid_ones_is_red(pair):
    produced, reference = pair
    (reference / "b.tbl").write_text("1.0\n")
    (produced / "b.tbl").write_text("1.0\n")
    # remove b from the reference: a partly missing set must fail
    (reference / "b.tbl").unlink()
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl", "b.tbl"))


def test_empty_reference_list_is_red(pair):
    produced, reference = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ())


def test_wrong_reference_list_is_red(pair):
    produced, reference = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("does-not-exist.tbl",))


def test_empty_files_are_red(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("")
    assert same_results(produced / "a.tbl", reference / "a.tbl") is False
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_damaged_file_is_red(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("1.0 2.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_nan_inf_is_red(pair, tmp_path):
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


def test_changed_result_is_red(pair):
    produced, reference = pair
    (produced / "a.tbl").write_text("1.5 2.0\n3.0 4.0\n")
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, reference, ("a.tbl",))


def test_missing_reference_directory_is_red(pair, tmp_path):
    produced, _ = pair
    with pytest.raises(RuntimeError):
        compare_with_reference(produced, tmp_path / "missing", ("a.tbl",))


def test_smoke_does_not_count_as_regression(pair):
    produced, reference = pair
    assert (
        compare_with_reference(produced, reference, ("a.tbl",), smoke=True) is False
    )


def test_runner_exit_status_is_red_on_missing_reference(tmp_path):
    # Check the exit status of the runner, not only the helper.
    runner = EXAMPLES / "quick_test" / "run.py"
    result = subprocess.run(
        [
            sys.executable,
            str(runner),
            "--swan-executable",
            "/does-not-exist/swan.exe",
            "--work-directory",
            str(tmp_path / "work"),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
