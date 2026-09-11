"""Tests for targeted gate selection: the right gates, never silently none."""
from __future__ import annotations

import sys
from pathlib import Path

import pytest

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
from select_gates import select_gates  # noqa: E402


def test_physics_source_skips_mpi():
    sel = select_gates(["src/swan_physics_selection.f90"])
    assert set(sel) == {"serial", "openmp", "pytest", "upstream_delta"}


def test_mpi_source_requires_mpi():
    sel = select_gates(["src/SwanParallel.f90"])
    assert "mpi" in sel and "serial" in sel


ALL_GATES = {"serial", "openmp", "mpi", "pytest", "upstream_delta"}


def test_metis_requires_everything():
    assert set(select_gates(["src/swan_metis_interface.f90"])) == ALL_GATES


@pytest.mark.parametrize("path", ["CMakeLists.txt", "src/CMakeLists.txt",
                                  "cmake/FindMETIS.cmake"])
def test_cmake_requires_everything(path):
    # Tested on its own: together with a Metis path it went unnoticed that
    # CMakeLists.txt itself did not select MPI.
    assert set(select_gates([path])) == ALL_GATES


def test_deck_change_skips_mpi():
    sel = select_gates(["examples/nonlinear_interactions/sources/src_nobreak.swn"])
    assert set(sel) == {"serial", "openmp", "pytest", "upstream_delta"}


def test_delta_manifest_requires_only_the_delta_gate():
    """The manifest lives under doc/, but it is data for a gate, not prose."""
    sel = select_gates(["doc/upstream-delta.json"])
    assert set(sel) == {"pytest", "upstream_delta"}


def test_delta_script_requires_the_delta_gate():
    sel = select_gates(["scripts/upstream_delta.py"])
    assert set(sel) == {"pytest", "upstream_delta"}


def test_docs_only_give_an_empty_selection():
    assert select_gates(["doc/modern-fortran.md", "AGENTS.md"]) == {}


def test_unknown_path_is_fail_safe():
    sel = select_gates(["entirely/new/thing.xyz"])
    assert set(sel) == {"serial", "openmp", "mpi", "pytest", "upstream_delta"}


def test_mpi_test_requires_mpi():
    sel = select_gates(["tests/test_mpi_unstructured_partition.py"])
    assert "mpi" in sel


def test_so_rp_python_has_a_gate():
    """The matrix scripts have their own tests, and result_stats.py runs under ctest."""
    assert set(select_gates(["so-rp_swan/matrix/deck.py"])) == {"pytest"}
    assert set(select_gates(["so-rp_swan/comparison/result_stats.py"])) == {"serial", "pytest"}
    assert select_gates(["so-rp_swan/matrix/README.md"]) == {}


def test_bad_base_is_an_error_not_a_clean_tree():
    """A mistyped --base must not read as a clean tree that needs no gate."""
    from select_gates import worktree_changes
    with pytest.raises(SystemExit):
        worktree_changes("no-such-revision-for-select-gates")
