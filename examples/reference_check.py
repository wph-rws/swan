"""Shared reference comparison for the example runners.

Completing a run only shows SWAN did not crash. The table and block files are
the actual model output, so they are what pins behaviour: refactoring work is
expected to leave them unchanged. Regenerate a reference deliberately, and only
when a physics or numerics change is intended.
"""

from __future__ import annotations

from pathlib import Path


def same_results(produced: Path, reference: Path) -> bool:
    """Compare two SWAN output files, ignoring trailing blanks.

    Every figure has to match. Line ends are the one exception: the MPI output
    path strips trailing blanks that the serial path writes, which is a
    formatting difference rather than a difference in results.
    """
    produced_lines = produced.read_text().splitlines()
    reference_lines = reference.read_text().splitlines()
    if len(produced_lines) != len(reference_lines):
        return False
    return all(
        a.rstrip() == b.rstrip() for a, b in zip(produced_lines, reference_lines)
    )


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
