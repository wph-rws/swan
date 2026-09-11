"""Shared reference comparison for the example runners.

Completing a run only shows SWAN did not crash. The table and block files are
the actual model output, so they are what pins behaviour: refactoring work is
expected to leave them unchanged. Regenerate a reference deliberately, and only
when a physics or numerics change is intended.

Regression mode (default) is strict: the required reference list per case
must be present in full and must match in full. A missing reference directory,
or one missing reference file next to a valid other file, is an error.
An empty or missing name list is an error too. Skipping is allowed only
in explicit ``--smoke`` mode; smoke never counts as a passed regression and
returns ``False`` without comparing. References are never regenerated
automatically to get a gate green.
"""

from __future__ import annotations

import math
from pathlib import Path


#  Relative tolerance for numeric fields. It absorbs rounding in the last
#  printed digit, and is tight enough that any change with physical meaning
#  still fails: those move results by percents, not by a hundredth of one.
#  It is not there to hide a thread-dependent spread: the unstructured solver
#  under OpenMP gives the same bits on any number of threads (BF-15, BF-17).
NUMERIC_TOLERANCE = 1.0e-3
#  Below this magnitude a figure is treated as zero, so tiny values near the
#  edge of the grid do not fail on their own round-off.
NEGLIGIBLE = 1.0e-6


def _as_float(token: str) -> float | None:
    try:
        return float(token)
    except ValueError:
        return None


def same_results(
    produced: Path,
    reference: Path,
    *,
    tolerance: float = NUMERIC_TOLERANCE,
) -> bool:
    """Compare two SWAN output files field by field.

    Text (headers, units) has to match exactly apart from trailing blanks: the
    MPI output path strips blanks the serial path writes, which is formatting
    rather than result. Numbers are compared with a relative tolerance, because
    the unstructured solver is not bit-reproducible under OpenMP: thread count
    changes the summation order, so the same build can print a different last
    digit from one run to the next.
    """
    try:
        produced_text = produced.read_text()
    except OSError:
        return False
    try:
        reference_text = reference.read_text()
    except OSError:
        return False
    if not produced_text.strip() or not reference_text.strip():
        #  Empty or damaged files (empty, whitespace only) are never
        #  a passed comparison.
        return False
    produced_lines = produced_text.splitlines()
    reference_lines = reference_text.splitlines()
    if len(produced_lines) != len(reference_lines):
        return False

    for produced_line, reference_line in zip(produced_lines, reference_lines):
        produced_fields = produced_line.split()
        reference_fields = reference_line.split()
        if len(produced_fields) != len(reference_fields):
            return False
        for produced_field, reference_field in zip(produced_fields, reference_fields):
            produced_value = _as_float(produced_field)
            reference_value = _as_float(reference_field)
            if produced_value is None or reference_value is None:
                if produced_field != reference_field:
                    return False
                continue
            #  NaN and infinity parse as floats but carry no information a
            #  tolerance can act on: NaN compares unequal to everything, so the
            #  difference test below would silently accept it. A run that
            #  produces either has gone wrong, and so has a reference that
            #  stores one.
            if not (math.isfinite(produced_value) and math.isfinite(reference_value)):
                return False
            scale = max(abs(produced_value), abs(reference_value), NEGLIGIBLE)
            if abs(produced_value - reference_value) > tolerance * scale:
                return False
    return True


def compare_with_reference(
    produced_directory: Path,
    reference_directory: Path,
    names: tuple[str, ...],
    *,
    smoke: bool = False,
    tolerances: dict[str, float] | None = None,
) -> bool:
    """Check the named output files against stored references.

    Regression mode (``smoke=False``, default): ``names`` is the required
    reference list. Returns ``True`` if every named comparison
    ran and matched; raises ``RuntimeError`` for a missing reference directory,
    a missing reference file, missing output, an empty/wrong
    name list, empty or damaged files, NaN/Inf or a changed
    result. A partly missing reference set next to a valid
    other file is therefore red as well.

    Smoke mode (``smoke=True``): compares nothing and returns ``False``.
    The caller may report this only as "smoke, not a regression".
    """
    if not names:
        raise RuntimeError(
            "empty reference list: a regression without required names is no coverage."
        )
    if smoke:
        return False
    if not reference_directory.is_dir():
        raise RuntimeError(
            f"reference directory is missing: {reference_directory}. "
            "Without a reference there is no regression coverage."
        )

    for name in names:
        reference = reference_directory / name
        if not reference.is_file():
            raise RuntimeError(
                f"reference file is missing: {reference}. One missing "
                "reference next to a valid other one is an error, not a skip."
            )
        if reference.stat().st_size == 0:
            raise RuntimeError(f"reference file is empty: {reference}.")
        produced = produced_directory / name
        if not produced.is_file():
            raise RuntimeError(f"SWAN produced no {name} to compare.")
        if produced.stat().st_size == 0:
            raise RuntimeError(f"SWAN produced an empty {name}; comparison fails.")
        tolerance = (tolerances or {}).get(name, NUMERIC_TOLERANCE)
        if not same_results(produced, reference, tolerance=tolerance):
            raise RuntimeError(
                f"{name} differs from the stored reference (or is empty/damaged/"
                "contains NaN/Inf). Either a change altered the results, or the "
                "reference needs updating on purpose."
            )
    return True
