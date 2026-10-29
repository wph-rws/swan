"""Keep the check from passing on a reference that itself stopped early."""
import math

import pytest

import convergence_check as check
import deck


def outcome(**overrides):
    base = dict(label="operational", seconds=1.0, iterations=35, accuracy=98.14,
                required=98.0, met_criterion=True, hit_cap=False, max_iterations=50)
    base.update(overrides)
    return check.RunOutcome(**base)


def fixed(n):
    return outcome(label="reference", iterations=n, required=101, accuracy=99.9,
                   met_criterion=False, hit_cap=True, max_iterations=n)


@pytest.mark.parametrize("line", [
    "NUM ACCUR 0.01 0.01 0.01 98 STAT MXITST=20 ALFA=0.01 LIMITER=0.1",
    "NUM STOPC DABS=0.005 DREL=0.01 NPNTS=99.5 STAT MXITST=50 LIMITER=0.1",
    "numeric stopc npnts=99.5 &\n stat mxitst=50 alfa=0.01 limiter=0.1",
])
def test_reference_forces_full_iterations_and_keeps_limiter(line):
    result = check.reference_numerics(line, 0.0, 200)
    assert "NPNTS=101" in result
    assert "MXITST=200" in result
    assert "ALFA=0.0" in result
    assert "limiter=0.1" in result.lower()
    assert "98 " not in result
    assert "99.5" not in result


def test_multiline_replacement_preserves_other_commands_and_schemes():
    text = "PROJ 'x' '1'\nNUM STOPC NPNTS=99 & $ remark\n STAT MXITST=50 ALFA=0.01 &\n DIRIMPL 0.5\nCOMPUTE\nSTOP\n"
    op, ref = check.build_variants(text, 0.01, 400)
    assert op == text
    assert "DIRIMPL 0.5\nCOMPUTE\nSTOP\n" in ref
    assert "NPNTS=101" in ref
    assert "MXITST=400" in ref
    assert "NPNTS=99" not in ref


def test_reference_removes_fork_specific_residual():
    line = "NUM ACCUR NPNTS=98 STAT MXITST=50 ALFA=0.01 RESIDUAL 0.0095"
    assert "RESIDUAL" not in check.reference_numerics(line, 0.01, 200)


@pytest.mark.parametrize("text", ["COMPUTE\nSTOP", "NUM ACCUR STAT 50 0.01"])
def test_unsupported_input_fails_explicitly(text):
    with pytest.raises(ValueError):
        check.build_variants(text, 0.01, 200)


def test_reference_must_run_the_forced_iterations():
    assert check.reference_problem(outcome(label="reference"), fixed(400))
    assert check.reference_problem(fixed(200), fixed(200))
    assert check.reference_problem(fixed(200), fixed(400)) is None


def measured(n, final, lowest):
    return outcome(label="reference", iterations=n, required=101, accuracy=99.9,
                   met_criterion=False, hit_cap=True, max_iterations=n,
                   residual_final=final, residual_lowest=lowest)


def test_wandering_reference_gives_no_verdict():
    # a reference whose residual is lowest around iteration 55 (0.0556) and has
    # risen to 0.0835 at iteration 400
    problem = check.reference_problem(measured(200, 0.0766, 0.0556),
                                      measured(400, 0.0835, 0.0556))
    assert "drifts away" in problem
    verdict, explanation = check.judge(outcome(), problem, 0.05, 0.2, 0.1, 0.15, 0.01, 400)
    assert (verdict, explanation) == ("inconclusive", problem)
    assert check.reference_problem(measured(200, 0.0129, 0.0129),
                                   measured(400, 0.0128, 0.0128)) is None


def table(*hs):
    return [[i, 0, 10, h] for i, h in enumerate(hs)]


def test_margin_is_the_shift_between_the_references_per_point():
    # the references move 2.6 mm at one point and the operational run is half a
    # metre lower; that is certainly premature
    lower, upper = check.deviation_with_margin(table(3.0, 2.0), table(3.4974, 2.4), table(3.5, 2.4))
    assert lower == pytest.approx(0.5 - 0.0026)
    assert upper == pytest.approx(0.5 + 0.0026)
    assert check.deviation_with_margin(table(1.0), table(1.0, 2.0), table(1.0, 2.0)) == (None, None)
    dry = [[0, 0, 10, -9.0]]
    assert check.deviation_with_margin(dry, dry, dry) == (None, None)


@pytest.mark.parametrize("deviation,margin,expected", [
    (0.006, 0.003, "converged"),
    (0.009, 0.003, "inconclusive"),
    (0.012, 0.001, "premature"),
    (0.012, 0.003, "inconclusive"),
    (0.127, 0.0026, "premature"),
])
def test_verdict_takes_the_reference_margin_into_account(deviation, margin, expected):
    verdict, explanation = check.judge(outcome(), None, deviation - margin, deviation + margin,
                                       deviation, margin, 0.01, 400)
    assert verdict == expected
    if expected == "inconclusive":
        assert "--reference-mxitst 400" in explanation


@pytest.mark.parametrize("problem,met,lower,expected", [
    (None, False, 0.0, "not-converged"),
    ("reference unusable", True, 0.0, "inconclusive"),
    (None, True, None, "inconclusive"),
])
def test_verdict_requires_a_usable_reference_and_comparable_points(problem, met, lower, expected):
    upper = None if lower is None else lower + 0.001
    verdict, _ = check.judge(outcome(met_criterion=met), problem, lower, upper, 0.001, 0.001, 0.01)
    assert verdict == expected


def row(index, hs, **values):
    """A line of the operational output table (20 columns)."""
    result = [float(index), 0., 10., hs] + [0.] * 16
    for key, value in values.items():
        result[deck.COLUMN[key]] = value
    return result


def test_last_change_over_wet_points_only():
    rows = [row(0, 1.0, dhs=0.002), row(1, 1.0, dhs=0.0005), row(2, -9, dhs=-9)]
    assert check.last_change(rows) == pytest.approx(0.002)
    assert check.last_change([[0, 0, 10, 1.0]]) is None


def test_largest_and_rms_over_wet_points():
    a = [[0, 0, 10, 1.00], [1, 0, 10, 2.00], [2, 0, -9, -9]]
    b = [[0, 0, 10, 1.02], [1, 0, 10, 2.01], [2, 0, -9, -9]]
    largest, rms, wet = check.compare_hs(a, b)
    assert wet == 2
    assert largest == pytest.approx(0.02)
    assert rms == pytest.approx(math.sqrt((0.02**2 + 0.01**2)/2))


@pytest.mark.parametrize("b", [[], [[0, 0, 10, -9]], [[1, 0, 10, 1]], [[0, 0, 10, math.nan]]])
def test_different_coordinates_or_wet_masks_or_invalid_tables_are_refused(b):
    assert check.compare_hs([[0, 0, 10, 1]], b) == (None, None, 0)


def test_all_dry_is_not_evidence_of_convergence():
    assert check.compare_hs([[0, 0, -9, -9]], [[0, 0, -9, -9]]) == (None, None, 0)


def test_direction_wrap_and_low_waves():
    a = [row(0, 1, tm01=3, dir=359), row(1, 0.01, tm01=3, dir=90), row(2, 1, tm01=3, dir=-999)]
    b = [row(0, 1, tm01=3.1, dir=1), row(1, 0.01, tm01=3, dir=270), row(2, 1, tm01=3, dir=10)]
    result = check.other_differences(a, b)
    assert result['direction_deg']['max_abs'] == 2
    assert result['direction_deg']['points'] == 1
    assert result['tm01_s']['max_abs'] == pytest.approx(0.1)


def test_existing_run_directory_is_never_reused(tmp_path):
    with pytest.raises(FileExistsError):
        check.run_once('test', '', tmp_path/'missing.exe', tmp_path)


def test_non_finite_table_is_rejected(tmp_path):
    table = tmp_path/'table'
    table.write_text('% header\n0 0 10 nan\n')
    with pytest.raises(ValueError, match='non-finite'):
        check.read_table(table)


def test_cli_inconclusive_is_not_success(monkeypatch, tmp_path):
    from types import SimpleNamespace
    condition = SimpleNamespace(condition_id='test')
    monkeypatch.setattr(check.matrix_runner, 'load_manifest',
                        lambda: SimpleNamespace(conditions=[SimpleNamespace(condition=condition)]))
    text = 'NUM ACCUR 0.01 0.01 0.01 98 STAT MXITST=50 ALFA=0.01\nCOMPUTE\n'
    monkeypatch.setattr(check.deck_module, 'generate_deck', lambda *a, **k: text)
    # deviation 12 mm, uncertainty 5 mm: around the 10 mm tolerance, so no verdict
    records = iter([(outcome(), [[0, 0, 10, 1.188]]),
                    (fixed(200), [[0, 0, 10, 1.195]]),
                    (fixed(400), [[0, 0, 10, 1.2]])])
    monkeypatch.setattr(check, 'run_once', lambda *a: next(records))
    exe = tmp_path/'swan.exe'
    exe.touch()
    monkeypatch.setattr('sys.argv', ['check', '--executable', str(exe),
                                    '--output-directory', str(tmp_path/'runs')])
    assert check.main() == 1
