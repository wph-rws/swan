#!/usr/bin/env python3
"""Tests for the premature-convergence check.

These exercise the judgement, not SWAN: the point of the script is the verdict
it draws from two runs, and that has to be right before hours of model time are
spent producing the runs.
"""

from __future__ import annotations

import pytest

import convergence_check as check


def outcome(**overrides):
    base = dict(
        label="operational",
        seconds=1.0,
        iterations=35,
        accuracy=98.14,
        required=98.0,
        met_criterion=True,
        hit_cap=False,
        max_iterations=50,
    )
    base.update(overrides)
    return check.RunOutcome(**base)


class TestReferenceNumerics:
    def test_replaces_both_alfa_and_the_iteration_cap(self):
        line = "NUM ACCUR 0.01 0.01 0.01 98 STAT MXITST=20 ALFA=0.01 LIMITER=0.1"
        result = check.reference_numerics(line, 0.0, 200)
        assert "MXITST=200" in result
        assert "ALFA=0.0" in result
        # everything else in the line has to survive, or the reference would be
        # comparing a different stopping criterion as well
        assert "ACCUR 0.01 0.01 0.01 98" in result
        assert "LIMITER=0.1" in result

    def test_adds_alfa_when_the_deck_does_not_set_it(self):
        line = "NUM ACCUR 0.02 0.02 0.02 98 STAT MXITST=50"
        result = check.reference_numerics(line, 0.0, 200)
        assert "ALFA=0.0" in result
        assert "MXITST=200" in result

    def test_deck_without_numeric_command_is_refused(self):
        with pytest.raises(RuntimeError, match="no NUM ACCUR"):
            check.build_variants("PROJ 'x' '1'\nCOMPUTE\nSTOP\n", 0.0, 200)

    def test_operational_variant_is_returned_unchanged(self):
        text = "NUM ACCUR 0.01 0.01 0.01 98 STAT MXITST=20 ALFA=0.01\nCOMPUTE\n"
        operational, reference = check.build_variants(text, 0.0, 200)
        assert operational == text
        assert reference != text


class TestVerdicts:
    def test_agreement_with_a_converged_reference_is_converged(self):
        verdict, _ = check.judge(outcome(), outcome(label="reference"), 0.002, 0.01)
        assert verdict == "converged"

    def test_claimed_convergence_that_disagrees_is_premature(self):
        verdict, explanation = check.judge(
            outcome(iterations=2, accuracy=100.0),
            outcome(label="reference", iterations=120),
            0.127,
            0.01,
        )
        assert verdict == "premature"
        assert "0.1270" in explanation

    def test_running_out_of_iterations_is_not_converged(self):
        verdict, explanation = check.judge(
            outcome(iterations=20, accuracy=67.64, met_criterion=False, hit_cap=True,
                    max_iterations=20),
            outcome(label="reference", iterations=35),
            0.05,
            0.01,
        )
        assert verdict == "not-converged"
        assert "67.64" in explanation

    def test_an_unconverged_reference_yields_no_verdict_at_all(self):
        # The reference is the only yardstick there is. Without it the script
        # must say so rather than pass judgement on the operational run.
        verdict, explanation = check.judge(
            outcome(), outcome(label="reference", accuracy=71.0, met_criterion=False), 0.5, 0.01
        )
        assert verdict == "inconclusive"
        assert "reference" in explanation

    def test_unconverged_reference_wins_over_a_tiny_difference(self):
        verdict, _ = check.judge(
            outcome(), outcome(label="reference", met_criterion=False), 0.0, 0.01
        )
        assert verdict == "inconclusive"


class TestHsComparison:
    def test_reports_largest_and_rms_over_wet_points(self):
        a = [[0, 0, 10, 1.00], [0, 0, 10, 2.00]]
        b = [[0, 0, 10, 1.02], [0, 0, 10, 2.01]]
        largest, rms, wet = check.compare_hs(a, b)
        assert wet == 2
        assert largest == pytest.approx(0.02)
        assert rms == pytest.approx(((0.02**2 + 0.01**2) / 2) ** 0.5)

    def test_skips_points_either_run_reports_dry(self):
        a = [[0, 0, 10, 1.00], [0, 0, -9, -9.0]]
        b = [[0, 0, 10, 1.01], [0, 0, -9, -9.0]]
        largest, _, wet = check.compare_hs(a, b)
        assert wet == 1
        assert largest == pytest.approx(0.01)

    def test_mismatched_tables_give_nothing_rather_than_a_wrong_number(self):
        assert check.compare_hs([[0, 0, 10, 1.0]], []) == (None, None, 0)
        assert check.compare_hs([[0, 0, 10, 1.0]], [[0, 0, 10, 1.0], [0, 0, 10, 2.0]]) == (
            None,
            None,
            0,
        )

    def test_all_dry_gives_nothing(self):
        a = [[0, 0, -9, -9.0]]
        assert check.compare_hs(a, a) == (None, None, 0)
