"""The upstream map must say where every upstream routine and variable went.

`doc/upstream-map.json` records, for each upstream procedure and each piece
of upstream shared state that is not where its name says, where it went and
why. `scripts/upstream_map.py check` compares it with the pinned upstream
source (read from git) and with `src/`. These tests run that check, and show
that it fails for each kind of mistake it is meant to catch.
"""

from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

REPOSITORY = Path(__file__).resolve().parent.parent
SCRIPT = REPOSITORY / "scripts" / "upstream_map.py"
MAP = REPOSITORY / "doc" / "upstream-map.json"


def run(*arguments: str, map_file: Path | None = None) -> subprocess.CompletedProcess:
    command = [sys.executable, str(SCRIPT)]
    if map_file is not None:
        command += ["--map", str(map_file)]
    return subprocess.run(command + list(arguments), capture_output=True,
                          text=True, cwd=REPOSITORY)


def changed_map(tmp_path: Path, change) -> Path:
    data = json.loads(MAP.read_text(encoding="utf-8"))
    change(data)
    path = tmp_path / "upstream-map.json"
    path.write_text(json.dumps(data), encoding="utf-8")
    return path


def test_the_map_is_complete():
    completed = run("check")
    assert completed.returncode == 0, completed.stdout + completed.stderr
    assert "upstream map is complete" in completed.stdout


def test_a_moved_routine_without_an_entry_is_reported(tmp_path):
    map_file = changed_map(tmp_path, lambda d: d["procedures"].pop("MSGERR"))
    completed = run("check", map_file=map_file)
    assert completed.returncode == 1
    assert "procedure MSGERR" in completed.stdout
    assert "swan_service_interfaces.f90" in completed.stdout


def test_an_entry_for_a_routine_that_stayed_is_reported(tmp_path):
    def add(data):
        data["procedures"]["SWCOMP"] = {"status": "moved", "reason": "topic-module",
                                        "fork": ["src/swancom1.f90"]}
    completed = run("check", map_file=changed_map(tmp_path, add))
    assert completed.returncode == 1
    assert "SWCOMP: is in the fork file of the same name" in completed.stdout


def test_a_wrong_fork_file_is_reported(tmp_path):
    def wrong(data):
        data["procedures"]["MSGERR"]["fork"] = ["src/swanser.f90"]
    completed = run("check", map_file=changed_map(tmp_path, wrong))
    assert completed.returncode == 1
    assert "procedure MSGERR: map says ['src/swanser.f90']" in completed.stdout


def test_a_removed_routine_that_still_exists_is_reported(tmp_path):
    def removed(data):
        data["procedures"]["MSGERR"] = {"status": "removed", "reason": "dead-code"}
    completed = run("check", map_file=changed_map(tmp_path, removed))
    assert completed.returncode == 1
    assert "map says removed, but the fork defines it" in completed.stdout


def test_a_replacement_that_does_not_exist_is_reported(tmp_path):
    def replaced(data):
        data["procedures"]["NG"]["by"] = ["NO_SUCH_ROUTINE"]
    completed = run("check", map_file=changed_map(tmp_path, replaced))
    assert completed.returncode == 1
    assert "replacement 'NO_SUCH_ROUTINE' not found" in completed.stdout


def test_a_missing_reason_is_reported(tmp_path):
    def unexplained(data):
        data["procedures"]["MSGERR"]["reason"] = "moved"
    completed = run("check", map_file=changed_map(tmp_path, unexplained))
    assert completed.returncode == 1
    assert "neither a known reason nor a sentence" in completed.stdout


def test_a_wrong_component_is_reported(tmp_path):
    def wrong(data):
        data["state"]["M_WCAP::ACTOT"]["by"] = ["type wcap_workspace_t%no_such_field"]
    completed = run("check", map_file=changed_map(tmp_path, wrong))
    assert completed.returncode == 1
    assert "state M_WCAP::ACTOT" in completed.stdout


def test_moved_state_needs_a_note_for_its_upstream_module(tmp_path):
    map_file = changed_map(tmp_path, lambda d: d["modules"].pop("SWCOMM3"))
    completed = run("check", map_file=map_file)
    assert completed.returncode == 1
    assert "upstream module SWCOMM3 has no note" in completed.stdout


def test_the_map_must_describe_the_pinned_upstream(tmp_path):
    def other(data):
        data["upstream"] = "0dabfa4"
    completed = run("check", map_file=changed_map(tmp_path, other))
    assert completed.returncode == 1
    assert "but the pin is" in completed.stdout


@pytest.mark.parametrize("name, expected", [
    ("MSGERR", "src/swan_service_interfaces.f90"),
    ("INKEYW", "INKEYW_CTX"),
    ("SWCOMM3::KCGRD", "stencil"),
    ("ACTOT", "wcap_workspace_t%total_action"),
])
def test_where_names_the_fork_location(name, expected):
    completed = run("where", name)
    assert completed.returncode == 0, completed.stderr
    assert expected in completed.stdout


def test_an_upstream_change_is_mapped_to_the_fork():
    """Upstream "patch B" (f5d931d..0dabfa4) is already in the fork.

    Every changed routine and every changed file must be found, also the
    build files outside src/ that add the FFRO option. For the convergence
    checks the fork must have every added line and none of the removed ones.
    """
    completed = run("changes", "f5d931d", "0dabfa4", "--present")
    assert completed.returncode == 0, completed.stderr
    out = completed.stdout
    for target in ("SWANCOMPUNSTRUC", "SWANCONVACCUR", "SWANVERTLIST",
                   "file src/srclist.cmake", "file CMakeLists.txt",
                   "file switch.pl"):
        assert f"== {target}" in out
    accur = out.split("== SWANCONVACCUR", 1)[1].split("==", 1)[0]
    assert "fork src/SwanConvAccur.f90" in accur
    assert "added lines found in the fork 8/8" in accur
    assert "removed lines still in the fork 0/" in accur


sys.path.insert(0, str(REPOSITORY / "scripts"))
import upstream_map  # noqa: E402


def test_alternative_headers_make_one_procedure():
    """Upstream SWTABP has one header per switch variant and one body."""
    source = upstream_map.Source()
    text = "\n".join([
        "!NCF      SUBROUTINE SWTABP (A, B)",
        "!NNCF      SUBROUTINE SWTABP (A)",
        "      X = 1",
        "      END",
        "      SUBROUTINE SUHEAD",
        "      END",
    ])
    upstream_map.read_file(source, "src/x.ftn", text, True,
                           upstream_map.switch_markers())
    found = {p.name: (p.first, p.last, p.host) for p in source.procedures}
    assert found == {"SWTABP": (1, 4, ""), "SUHEAD": (5, 6, "")}


def test_a_line_that_moves_to_another_switch_counts_as_changed(tmp_path):
    hunk = upstream_map.Hunk("src/x.ftn90", "src/x.ftn90", "src/x.ftn90", 1, 1, 1, 1,
                             removed=["!GRAPH    call build_front(levels)"],
                             added=["!FXFRO    call build_front(levels)"])
    fork = tmp_path / "fork.f90"
    fork.write_text("call build_front(levels)\n", encoding="utf-8")
    result = upstream_map.presence([hunk], {str(fork)})
    assert "found in the fork 1/1" in result
    assert "still in the fork 1/1" in result


def test_a_hunk_over_two_procedures_is_split():
    """Each procedure gets only its own lines of a hunk that spans both."""
    new = upstream_map.Source()
    upstream_map.read_file(new, "src/x.f90", "\n".join([
        "subroutine a", "x = 1", "y = 2", "end subroutine a",
        "subroutine b", "z = 3", "end subroutine b",
    ]), False, ())
    hunk = upstream_map.Hunk("src/x.f90", "", "src/x.f90", 0, 0, 1, 7,
                             added=["subroutine a", "x = 1", "y = 2",
                                    "end subroutine a", "subroutine b", "z = 3",
                                    "end subroutine b"])
    parts = upstream_map.split_by_procedure(hunk, upstream_map.Source(), new)
    assert sorted(parts) == ["A", "B"]
    assert len(parts["A"].added) == 4 and len(parts["B"].added) == 3
