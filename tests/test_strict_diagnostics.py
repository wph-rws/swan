"""Behaviour tests: the strict gate must hide no checked warning."""
from __future__ import annotations

import collections
import json
import subprocess
import sys
import types
from pathlib import Path

import pytest

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
import strict_diagnostics as sd  # noqa: E402


def _key(category="maybe-uninitialized", file="a.f90", routine="r",
         message="‘x’ may be used uninitialized [-Wmaybe-uninitialized]"):
    return (category, file, routine, message)


def _verdict(category, file, routine, message, count=1, verdict="harmless"):
    return {"category": category, "file": file, "routine": routine,
            "message": message, "count": count, "verdict": verdict,
            "reason": "checked by hand in a test"}


# --- recognising -------------------------------------------------------------

def test_block_and_single_line_are_both_recognised():
    log = (
        "/src/swancom1.f90:10:3:\n\n   10 | x = 1\n      |   1\n"
        "Warning: Unused variable ‘x’ declared at (1) [-Wunused-variable]\n"
        "/src/swan_time.f90:339:16: Warning: Conversion from ‘REAL(4)’ to "
        "‘REAL(8)’ at (1) [-Wconversion-extra]\n"
    )
    found = sd.parse(log)
    assert [(o.category, o.file, o.line) for o in found] == [
        ("unused-variable", "swancom1.f90", 10),
        ("conversion-extra", "swan_time.f90", 339),
    ]


def test_warning_without_flag_counts():
    # gfortran gives some warnings unconditionally, without [-W...].
    log = (
        "/src/fftw_compat.f90:104:43:\n\n  104 | a, a\n      | 1\n"
        "Warning: Same actual argument associated with INTENT(OUT) argument "
        "‘in’ and INTENT(OUT) argument ‘out’ at (1)\n"
    )
    (found,) = sd.parse(log)
    assert found.category == sd.UNTAGGED
    assert found.file == "fftw_compat.f90"


def test_unrecognised_warning_fails_the_count():
    with pytest.raises(ValueError, match="not recognised"):
        sd.parse("something unknown Warning: new format\n")


def test_warning_without_location_fails_the_count():
    with pytest.raises(ValueError, match="without location"):
        sd.parse("Warning: detached from any location [-Wsurprising]\n")


def test_one_location_belongs_to_one_message():
    # A second message must not stay attached to the location of the first.
    log = ("/src/a.f90:1:1:\n"
           "Warning: first [-Wsurprising]\n"
           "Warning: second [-Wsurprising]\n")
    with pytest.raises(ValueError, match="without location"):
        sd.parse(log)


def test_routine_from_the_source():
    source = "\n".join([
        "module m",                                     # 1
        "  integer, parameter :: p = 1",                # 2
        "  interface",                                  # 3
        "    real function extern(x)",                  # 4
        "    end function extern",                      # 5
        "  end interface",                              # 6
        "contains",                                     # 7
        "  subroutine outer(a)",                        # 8
        "    a = 1",                                    # 9
        "  contains",                                   # 10
        "    pure integer function inner(b) result(c)",   # 11
        "      c = b",                                  # 12
        "    end function inner",                       # 13
        "  end subroutine outer",                       # 14
        "  RECURSIVE SUBROUTINE Big()",                 # 15
        "  ENDSUBROUTINE",                              # 16
        "end module m",                                 # 17
    ])
    table = sd.scopes(source)
    assert table[1] == "m"          # module level
    assert table[3] == "m"          # interface block does not count
    assert table[8] == "outer"
    assert table[11] == "inner"
    assert table[13] == "outer"     # the end still belongs to the routine
    assert table[14] == "big"
    assert table[16] == "m"


def test_move_to_another_routine_is_visible(tmp_path):
    source = tmp_path / "a.f90"
    source.write_text("subroutine one\nend subroutine\nsubroutine two\nend subroutine\n")
    resolve = sd.RoutineResolver(tmp_path)
    verdict = sd.fingerprint([sd.Occurrence("conversion", str(source), 1, "m")], resolve)
    moved = sd.fingerprint([sd.Occurrence("conversion", str(source), 3, "m")], resolve)
    triage = {key: _verdict(*key, count=1) for key in verdict}
    unreviewed, stale = sd.check(moved, triage)
    assert unreviewed and stale


# --- checking ------------------------------------------------------------------

def test_checked_warning_without_verdict_fails():
    measured = collections.Counter({_key(): 1})
    unreviewed, stale = sd.check(measured, {})
    assert unreviewed == {_key(): 1} and not stale


def test_warning_with_verdict_passes():
    measured = collections.Counter({_key(): 2})
    unreviewed, stale = sd.check(measured, {_key(): _verdict(*_key(), count=2)})
    assert not unreviewed and not stale


def test_more_messages_than_the_verdict_covers_fail():
    measured = collections.Counter({_key(): 3})
    unreviewed, _ = sd.check(measured, {_key(): _verdict(*_key(), count=2)})
    assert unreviewed == {_key(): 1}


def test_other_categories_are_only_counted():
    measured = collections.Counter({_key(category="unused-variable"): 40,
                                    _key(category="conversion-extra"): 7})
    unreviewed, stale = sd.check(measured, {})
    assert not unreviewed and not stale


def test_every_checked_category_needs_a_verdict():
    for category in sd.CHECKED:
        unreviewed, _ = sd.check(collections.Counter({_key(category=category): 1}), {})
        assert unreviewed, category


def test_gone_warning_leaves_a_stale_verdict_that_prune_removes():
    triage = {_key(): _verdict(*_key(), count=2),
              _key(message="y"): _verdict(*_key(message="y"))}
    measured = collections.Counter({_key(): 1})
    unreviewed, stale = sd.check(measured, triage)
    assert not unreviewed and set(stale) == {_key(), _key(message="y")}
    kept = sd.prune(triage, measured)
    assert list(kept) == [_key()] and kept[_key()]["count"] == 1


def test_prune_writes_only_the_verdicts(tmp_path):
    path = tmp_path / "triage.json"
    path.write_text(json.dumps({"note": "n", "verdicts": [
        _verdict(*_key()), _verdict(*_key(message="y"))]}))
    table = sd.prune(sd.read_triage(path), collections.Counter({_key(): 1}))
    sd.write_triage(path, table)
    stored = json.loads(path.read_text())
    assert stored["note"] == "n" and len(stored["verdicts"]) == 1


def test_two_verdicts_for_one_fingerprint_are_refused(tmp_path):
    path = tmp_path / "triage.json"
    path.write_text(json.dumps({"verdicts": [_verdict(*_key()), _verdict(*_key())]}))
    with pytest.raises(ValueError, match="two verdicts"):
        sd.read_triage(path)


# --- the build must really be strict --------------------------------------------

def test_flags_without_paths_and_files():
    command = ("/usr/bin/gfortran  -O0 -w  -I/usr/include -Wall -O3 -Jmod "
               "-DMPI -c /x/src/a.f90 -o CMakeFiles/a.dir/a.f90.o")
    assert sd.compile_flags(command) == "-O0 -w -Wall -O3 -DMPI"


def _compile_commands(tmp_path, extra):
    command = f"/usr/bin/gfortran {extra} -O3 -c /x/a.f90 -o a.o"
    (tmp_path / "compile_commands.json").write_text(
        json.dumps([{"command": command, "file": "/x/a.f90"}]))
    return tmp_path


def test_strict_build_passes_the_flag_check(tmp_path):
    assert sd.check_flags(_compile_commands(tmp_path, sd.STRICT_FLAGS)) == []


def test_silenced_build_fails_the_flag_check(tmp_path):
    # FC='gfortran -w' ends up outside CMAKE_Fortran_FLAGS. Without this check
    # a build without any warnings would look clean.
    problems = sd.check_flags(_compile_commands(tmp_path, sd.STRICT_FLAGS + " -w"))
    assert problems and "-w" in problems[0]


def test_build_without_the_strict_flags_fails_the_flag_check(tmp_path):
    assert sd.check_flags(_compile_commands(tmp_path, "-Wall"))


def test_main_refuses_a_silenced_build(tmp_path, capsys):
    _compile_commands(tmp_path, "-w")
    log = tmp_path / "strict-build.log"
    log.write_text("")
    assert sd.main(["--log", str(log), "--build-dir", str(tmp_path)]) == 1
    assert "strict flags" in capsys.readouterr().err


# --- building ----------------------------------------------------------------------

def _fake_run(returncodes):
    calls = []

    def run(command, **kwargs):
        calls.append((command, kwargs))
        return types.SimpleNamespace(returncode=returncodes[len(calls) - 1],
                                     stdout="out\n", stderr="")
    return run, calls


def test_build_is_one_clean_attempt_with_grouped_output(tmp_path, monkeypatch):
    run, calls = _fake_run([0, 0])
    monkeypatch.setattr(sd.subprocess, "run", run)
    (tmp_path / "old").mkdir(parents=True)
    (tmp_path / "old" / "leftover").write_text("previous build")
    sd.build(tmp_path, tmp_path / "old")
    assert not (tmp_path / "old" / "leftover").exists()
    configure, make = calls
    assert "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON" in configure[0]
    assert "--output-sync=target" in make[0]
    assert make[1]["env"]["LC_ALL"] == "C.UTF-8"
    assert (tmp_path / "old" / "strict-build.log").is_file()


def test_failed_build_is_not_repeated_and_blocks(tmp_path, monkeypatch):
    run, calls = _fake_run([0, 2])
    monkeypatch.setattr(sd.subprocess, "run", run)
    with pytest.raises(SystemExit):
        sd.build(tmp_path, tmp_path / "b")
    assert len(calls) == 2
    assert (tmp_path / "b" / "strict-configure.log").is_file()
    assert (tmp_path / "b" / "strict-build.log").is_file()


def test_help_names_the_actions():
    result = subprocess.run([sys.executable, str(SCRIPTS / "strict_diagnostics.py"),
                             "--help"], capture_output=True, text=True)
    for option in ("--log", "--list", "--prune"):
        assert option in result.stdout
