"""Tests voor gerichte poortselectie: de juiste poorten, nooit stil niets."""
from __future__ import annotations

import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
from select_gates import select_gates  # noqa: E402


def test_fysicabron_slaat_mpi_over():
    sel = select_gates(["src/swan_physics_selection.f90"])
    assert set(sel) == {"serial", "openmp", "strict", "pytest"}


def test_mpi_bron_eist_mpi():
    sel = select_gates(["src/SwanParallel.f90"])
    assert "mpi" in sel and "strict" in sel


def test_metis_en_cmake_eisen_alles():
    sel = select_gates(["src/swan_metis_interface.f90", "CMakeLists.txt"])
    assert set(sel) == {"serial", "openmp", "mpi", "strict", "pytest"}


def test_deckwijziging_slaat_mpi_en_strict_over():
    sel = select_gates(["examples/nonlinear_interactions/sources/src_nobreak.swn"])
    assert set(sel) == {"serial", "openmp", "pytest"}


def test_docs_alleen_geeft_lege_selectie():
    assert select_gates(["doc/moderniseringsplan.md", "AGENTS.md"]) == {}


def test_onbekend_pad_is_fail_safe():
    sel = select_gates(["volslagen/nieuw/ding.xyz"])
    assert set(sel) == {"serial", "openmp", "mpi", "strict", "pytest"}


def test_strict_budget_alles_behalve_strict_niet_nodig():
    sel = select_gates(["scripts/strict_diagnostics_budget.json"])
    assert set(sel) == {"strict"}


def test_mpi_test_eist_mpi():
    sel = select_gates(["tests/test_mpi_unstructured_partition.py"])
    assert "mpi" in sel
