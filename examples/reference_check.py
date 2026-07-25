"""Shared reference comparison for the example runners.

Completing a run only shows SWAN did not crash. The table and block files are
the actual model output, so they are what pins behaviour: refactoring work is
expected to leave them unchanged. Regenerate a reference deliberately, and only
when a physics or numerics change is intended.
"""

from __future__ import annotations

import math
from pathlib import Path


#  Relative tolerance for numeric fields. Wide enough to absorb the last-digit
#  spread that the unstructured solver shows under OpenMP (measured at about
#  2e-4, and present in the code long before this test existed), tight enough
#  that any change with physical meaning still fails: those move results by
#  percents, not by a hundredth of one.
NUMERIC_TOLERANCE = 1.0e-3
#  Below this magnitude a figure is treated as zero, so tiny values near the
#  edge of the grid do not fail on their own round-off.
NEGLIGIBLE = 1.0e-6


def _as_float(token: str) -> float | None:
    try:
        return float(token)
    except ValueError:
        return None


def same_results(produced: Path, reference: Path) -> bool:
    """Compare two SWAN output files field by field.

    Text (headers, units) has to match exactly apart from trailing blanks: the
    MPI output path strips blanks the serial path writes, which is formatting
    rather than result. Numbers are compared with a relative tolerance, because
    the unstructured solver is not bit-reproducible under OpenMP: thread count
    changes the summation order, so the same build can print a different last
    digit from one run to the next.
    """
    produced_lines = produced.read_text().splitlines()
    reference_lines = reference.read_text().splitlines()
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
            if abs(produced_value - reference_value) > NUMERIC_TOLERANCE * scale:
                return False
    return True


def compare_with_reference(
    produced_directory: Path, reference_directory: Path, names: tuple[str, ...]
) -> bool:
    """Check the named output files against stored references.

    Returns True when a comparison actually ran, False when no reference is
    present, so the caller can report which of the two happened. Raises when a
    file differs or is missing.
    """
    if not reference_directory.is_dir():
        return False

    compared = False
    for name in names:
        reference = reference_directory / name
        if not reference.is_file():
            continue
        produced = produced_directory / name
        if not produced.is_file():
            raise RuntimeError(f"SWAN produced no {name} to compare.")
        if not same_results(produced, reference):
            raise RuntimeError(
                f"{name} differs from the stored reference. Either a change "
                "altered the results, or the reference needs updating on purpose."
            )
        compared = True
    return compared
