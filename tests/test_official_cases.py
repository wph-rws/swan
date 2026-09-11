"""Unit and negative tests for the official-test-case runner."""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

import pytest


def _load_runner() -> object:
    # Import by location under a unique module name: several runners
    # are called `run.py`; through sys.path they would shadow each other
    # in one pytest session.
    path = (Path(__file__).resolve().parent.parent / "examples"
            / "official_cases" / "run.py")
    spec = importlib.util.spec_from_file_location("official_cases_run", path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules["official_cases_run"] = module
    spec.loader.exec_module(module)
    return module


_run = _load_runner()


def test_shortest_arc_wraps_around_north():
    assert _run.shortest_arc(359.0, 1.0) == pytest.approx(2.0)
    assert _run.shortest_arc(1.0, 359.0) == pytest.approx(2.0)
    assert _run.shortest_arc(120.0, 120.0) == pytest.approx(0.0)


def test_read_numbers_skips_headers_and_percent():
    probe = Path(__file__).with_name("official_cases_probe.tab")
    probe.write_text("% comment\n% [m] [m]\n0.0 1.0 10.0\nnot numbers\n40.0 -9.0 -9.0\n")
    try:
        rows = _run.read_numbers(probe, 3)
    finally:
        probe.unlink()
    assert rows == [[0.0, 1.0, 10.0], [40.0, -9.0, -9.0]]


def test_cases_manifest_is_complete_and_pinned():
    manifest = json.loads((Path(__file__).resolve().parent.parent
                           / "examples" / "official_cases" / "cases.json").read_text())
    assert manifest["base_url"].startswith("https://")
    assert len(manifest["cases"]) >= 3
    for case in manifest["cases"]:
        assert len(case["sha256"]) == 64
        int(case["sha256"], 16)
        assert case["tolerances"]["hs"] > 0
        assert case["tolerances"]["dir"] > 0
        assert set(case["table_columns"]) >= {"key", "hs", "dir"}
        assert set(case["ana_columns"]) >= {"key", "hs", "dir"}


def test_vendor_comparison_reads_pristine_tree(tmp_path):
    # Regression test for the self-comparison trap: the run overwrites the
    # table that has the same name as the bundled Delft table. The vendor
    # comparison must read the pristine tree, even when the stage directory
    # by now holds our own output.
    work = tmp_path / "work"
    (work / "vendor" / "demo" / "inner").mkdir(parents=True)
    (work / "demo").mkdir(parents=True)
    (work / "vendor" / "demo" / "inner" / "out.tab").write_text(
        "0.0 1.000 90.0\n40.0 2.000 90.0\n")
    (work / "demo" / "inner").mkdir(parents=True)
    (work / "demo" / "inner" / "out.tab").write_text(
        "0.0 9.999 90.0\n40.0 9.999 90.0\n")
    case = {"name": "demo", "vendor_table": "out.tab"}
    wet = [[0.0, 9.999, 90.0], [40.0, 9.999, 90.0]]
    tc = {"key": 0, "hs": 1, "dir": 2}
    gap_hs, _ = _run.vendor_gaps(work, work / "demo", case, wet, tc)
    assert gap_hs == pytest.approx(8.999, abs=1e-12)


def test_missing_vendor_table_is_an_error(tmp_path):
    work = tmp_path / "work"
    (work / "vendor" / "demo").mkdir(parents=True)
    (work / "demo").mkdir(parents=True)
    case = {"name": "demo", "vendor_table": "absent.tab"}
    with pytest.raises(RuntimeError):
        _run.vendor_gaps(work, work / "demo", case, [[0.0, 1.0, 0.0]],
                         {"key": 0, "hs": 1, "dir": 2})
