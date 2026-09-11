"""Shared reference comparison for the example runners.

Completing a run only shows SWAN did not crash. The table and block files are
the actual model output, so they are what pins behaviour: refactoring work is
expected to leave them unchanged. Regenerate a reference deliberately, and only
when a physics or numerics change is intended.

Regressiemodus (default) is streng: de verplichte referentielijst per case
moet volledig aanwezig zijn en volledig kloppen. Een ontbrekende referentiemap
of één ontbrekend referentiebestand naast een geldig ander bestand is een fout.
Een lege of ontbrekende namenlijst is eveneens een fout. Overslaan mag alleen
in expliciete ``--smoke``-modus; smoke telt nooit als geslaagde regressie en
geeft ``False`` terug zonder te vergelijken. Referenties worden nooit
automatisch opnieuw gegenereerd om een poort groen te krijgen.
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
        #  Lege of verminkte bestanden (leeg, alleen witruimte) zijn nooit
        #  een geslaagde vergelijking.
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

    Regressiemodus (``smoke=False``, default): ``names`` is de verplichte
    referentielijst. Geeft ``True`` terug als iedere genoemde vergelijking
    liep en klopte; gooit ``RuntimeError`` bij een ontbrekende referentiemap,
    een ontbrekend referentiebestand, ontbrekende uitvoer, een lege/verkeerde
    namenlijst, lege of verminkte bestanden, NaN/Inf of een gewijzigde
    uitkomst. Een gedeeltelijk ontbrekende referentieset naast een geldig
    ander bestand is dus eveneens rood.

    Smoke-modus (``smoke=True``): vergelijkt niets en geeft ``False`` terug.
    De aanroeper mag dit alleen als "smoke, geen regressie" rapporteren.
    """
    if not names:
        raise RuntimeError(
            "lege referentielijst: regressie zonder verplichte namen is geen dekking."
        )
    if smoke:
        return False
    if not reference_directory.is_dir():
        raise RuntimeError(
            f"referentiemap ontbreekt: {reference_directory}. "
            "Zonder referentie is er geen regressiedekking."
        )

    for name in names:
        reference = reference_directory / name
        if not reference.is_file():
            raise RuntimeError(
                f"referentiebestand ontbreekt: {reference}. Eén ontbrekende "
                "referentie naast een geldige andere is een fout, geen skip."
            )
        if reference.stat().st_size == 0:
            raise RuntimeError(f"referentiebestand is leeg: {reference}.")
        produced = produced_directory / name
        if not produced.is_file():
            raise RuntimeError(f"SWAN produced no {name} to compare.")
        if produced.stat().st_size == 0:
            raise RuntimeError(f"SWAN produced an empty {name}; vergelijking faalt.")
        tolerance = (tolerances or {}).get(name, NUMERIC_TOLERANCE)
        if not same_results(produced, reference, tolerance=tolerance):
            raise RuntimeError(
                f"{name} differs from the stored reference (of is leeg/vervormd/"
                "bevat NaN/Inf). Either a change altered the results, or the "
                "reference needs updating on purpose."
            )
    return True
