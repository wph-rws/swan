"""Tests for the manifest-driven isolated SWAN matrix runner."""

from __future__ import annotations

import json
import os
from pathlib import Path

import pytest

import deck
import matrix_runner


def test_repository_manifest_has_required_coverage_and_four_targets():
    manifest = matrix_runner.load_manifest()
    assert len(manifest.conditions) == 10
    assert len(manifest.targets) == 4
    assert {
        tag for item in manifest.conditions for tag in item.tags
    } >= matrix_runner.REQUIRED_COVERAGE
    assert {target.physics for target in manifest.targets} == {
        "default",
        "legacy-4131",
    }


def test_plan_validates_all_generated_decks():
    text = matrix_runner.describe_matrix(matrix_runner.load_manifest())
    assert "10 conditions x 4 targets = 40 runs" in text
    assert "u20_d310_lp300_open" in text


def _fake_swan(path: Path) -> None:
    path.write_text(
        "#!/bin/sh\n"
        "set -eu\n"
        "test -s INPUT\n"
        "test -s GRID\n"
        "test -s DEPTH\n"
        "test -s uitvoerpunten.par\n"
        "printf 'accuracy OK in  99.00\\n' > PRINT\n"
        "printf '  Normal end of run 0001\\n' > norm_end\n"
        "printf 'mat\\n' > scaloost_rp.mat\n"
        "printf 'table\\n' > uitvoerpunten.tab\n"
        "printf 'sp1\\n' > uitvoerpunten.sp1\n"
        "printf 'sp2\\n' > uitvoerpunten.sp2\n"
        "printf '%s|%s|%s\\n' \"$OMP_NUM_THREADS\" \"$OMP_PLACES\" "
        "\"$OMP_PROC_BIND\" > environment.txt\n"
    )
    path.chmod(0o755)


@pytest.mark.skipif(
    not (matrix_runner.ROOT / "GRID").is_file()
    or not (matrix_runner.ROOT / "DEPTH").is_file(),
    reason="operational GRID/DEPTH are too large for version control",
)
def test_run_one_is_isolated_verified_and_records_metadata(tmp_path: Path):
    executable = tmp_path / "fake-swan"
    _fake_swan(executable)
    condition = matrix_runner.ManifestCondition(
        deck.Condition("smoke", 310, 20, 300, "open"),
        "smoke test",
        ("reference",),
    )
    target = matrix_runner.Target(
        "fake",
        "fake executable",
        executable,
        "legacy-4131",
        "modern",
        (),
    )
    output = tmp_path / "runs" / "fake" / "smoke" / "replicate-001"

    result = matrix_runner.run_one(condition, target, output, threads=3)

    assert result == output
    assert (output / "environment.txt").read_text() == "3|cores|close\n"
    assert "GEN3 KOMEN DRAG FIT" in (output / "INPUT").read_text()
    assert (output / "INPUT").read_bytes() == (output / "so-rp_osk.swn").read_bytes()
    record = json.loads((output / "run.json").read_text())
    assert record["condition"]["condition_id"] == "smoke"
    assert record["target"]["physics"] == "legacy-4131"
    assert record["returncode"] == 0
    assert record["error"] is None
    assert record["omp_num_threads"] == 3
    assert len(record["executable_sha256"]) == 64
    assert len(record["input_sha256"]) == 64


def test_run_one_refuses_to_overwrite(tmp_path: Path):
    executable = tmp_path / "fake-swan"
    _fake_swan(executable)
    condition = matrix_runner.ManifestCondition(
        deck.Condition("smoke", 310, 20, 300, "open"), "smoke", ("reference",)
    )
    target = matrix_runner.Target(
        "fake", "fake", executable, "default", "modern", ()
    )
    output = tmp_path / "existing"
    output.mkdir()
    with pytest.raises(FileExistsError, match="refusing to overwrite"):
        matrix_runner.run_one(condition, target, output, threads=1)


def test_executable_override_must_be_absolute():
    with pytest.raises(ValueError, match="must be absolute"):
        matrix_runner.executable_overrides(["current=relative/swan.exe"])


def test_bss_environment_prepends_bundled_libraries(tmp_path: Path, monkeypatch):
    monkeypatch.setenv("LD_LIBRARY_PATH", "/system/libs")
    target = matrix_runner.Target(
        "bss", "bss", Path("/bin/true"), "default", "bss", (tmp_path,)
    )
    env = matrix_runner._environment(target, 8)
    assert env["LD_LIBRARY_PATH"] == f"{tmp_path}{os.pathsep}/system/libs"
    assert env["OMP_NUM_THREADS"] == "8"
