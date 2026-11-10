"""The Fortran sources keep one layout, and the layout script keeps every statement.

`scripts/fortran_layout.py` converted the upstream fixed-form sources and lays
out every free-form source (see the docstring of the script). These tests check
that every source in the repository is in that layout, so that a diff against
upstream or `git blame` is not spoiled by stray reformatting, and they show on
small examples that the script refuses to change a statement and handles the
fixed-form cases that are easy to get wrong.
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest

REPOSITORY = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPOSITORY / "scripts"))
import fortran_layout as layout  # noqa: E402


def sources() -> list[Path]:
    """Every Fortran source of the repository: the tracked .f90 files, or, outside
    a git checkout, those in the directories that hold them."""
    listed = subprocess.run(["git", "ls-files", "*.f90"], capture_output=True, text=True,
                            cwd=REPOSITORY)
    if listed.returncode == 0 and listed.stdout.strip():
        return [REPOSITORY / name for name in sorted(listed.stdout.split())]
    found = []
    for pattern in ("src/*.f90", "src/*/*.f90", "tools/*.f90", "tests/*.f90",
                    "benchmarks/*.f90", "examples/*/*.f90"):
        found.extend(sorted(REPOSITORY.glob(pattern)))
    return found


def test_every_source_is_in_the_layout():
    result = subprocess.run(
        [sys.executable, str(REPOSITORY / "scripts" / "fortran_layout.py"), "format",
         "--check", *map(str, sources())],
        capture_output=True, text=True, cwd=REPOSITORY)
    assert result.returncode == 0, (
        "run `python3 scripts/fortran_layout.py format <file>` on:\n" + result.stdout + result.stderr)


def test_no_code_line_is_longer_than_the_standard_allows():
    long_lines = []
    for path in sources():
        for number, line in enumerate(path.read_text(encoding="latin-1").splitlines(), 1):
            if len(line) > layout.LIMIT and not line.lstrip().startswith("!"):
                long_lines.append(f"{path.relative_to(REPOSITORY)}:{number}")
    assert not long_lines, long_lines


def test_a_changed_statement_is_detected():
    before = "subroutine a(x)\n   x = 1.0\nend subroutine a\n"
    assert layout.equivalent(before, "subroutine a(x)\n      x =   1.0\nend subroutine a\n")
    assert not layout.equivalent(before, "subroutine a(x)\n   x = 1.5\nend subroutine a\n")
    assert not layout.equivalent(before, "subroutine a(x)\n   y = 1.0\nend subroutine a\n")


def test_layout_is_idempotent_on_nesting_labels_and_continuations():
    text = (
        "      SUBROUTINE A (X, N)\n"
        "      DO 10 I = 1, N\n"
        "      IF (X .GT. 0.) THEN\n"
        "      CALL B (X,\n"
        "     &        N)\n"
        "      END IF\n"
        "   10 CONTINUE\n"
        "      END\n")
    once = layout.format_text(layout.convert_fixed(text))
    assert layout.format_text(once) == once
    assert "   DO 10 I = 1, N" in once
    assert "10    CONTINUE" in once or "10 CONTINUE" in once
    assert "CALL B (X, &" in once


def test_conversion_keeps_a_tab_inside_a_string():
    converted = layout.convert_fixed("      ITAB = ICHAR('\t')\n")
    assert "ICHAR('\t')" in converted


def test_conversion_counts_columns_after_a_switch_prefix():
    # switch.pl removes '!WFR' before the compiler reads columns 1-72, so the
    # 72 columns start after the prefix; THEN must not be cut to THE.
    line = "!WFR         IF ( IPNB.EQ.IPR(1,SWPDIR) .OR. IPNB.EQ.IPR(2,SWPDIR) ) THEN\n"
    assert "THEN" in layout.convert_fixed(line)


def test_conversion_drops_version_tags_of_code_but_not_of_comments():
    text = ("!     SWCOMP  (main subroutine for the computational module)              31.02\n"
            "      X = 1                                                             40.41\n")
    converted = layout.convert_fixed(text)
    assert "31.02" in converted
    assert "40.41" not in converted


def test_conversion_strips_tags_from_openmp_lines():
    converted = layout.convert_fixed("!$OMP PARALLEL                                                          40.31\n")
    assert "40.31" not in converted


def test_a_string_is_continued_correctly_across_a_fixed_form_line():
    text = ("      WRITE (6, '(''a long text that runs on to the next line in fixed form, with blanks'' ,\n"
            "     &''here'')')\n")
    converted = layout.convert_fixed(text)
    assert layout.statements(layout.format_text(converted)) == layout.statements(converted)


def test_a_long_format_string_is_wrapped_inside_the_string():
    long = "      WRITE (6, \"(' " + "x" * 150 + "', I4)\") N\n"
    formatted = layout.format_text(long)
    assert all(len(line) <= layout.LIMIT for line in formatted.splitlines())
    assert layout.equivalent(long, formatted)


@pytest.mark.parametrize("case", ["declaration", "use"])
def test_fork_code_inside_a_tu_delft_header_moves_to_the_upstream_place(case):
    banner = ("!   --|-----------------------------------------------------------|--\n"
              "!     | Delft University of Technology                            |\n"
              "!   --|-----------------------------------------------------------|--\n")
    extra = ("   INTEGER, INTENT(IN) :: IGP\n" if case == "declaration"
             else "   USE swan_service_interfaces, ONLY: STRACE\n")
    text = ("SUBROUTINE A (X, IGP)\n" + (extra if case == "use" else "") +
            "!****************************************************************\n"
            "   IMPLICIT NONE\n" + (extra if case == "declaration" else "") + banner +
            "!  4. Argument variables\n!\n!     X   value\n!\n   REAL :: X\n"
            "!  5. Parameter variables\n!\n   X = 1.\nEND SUBROUTINE A\n")
    formatted = layout.format_text(text)
    assert layout.equivalent(text, formatted)
    head, tail = formatted.split("Delft University", 1)
    if case == "declaration":
        assert "INTENT(IN) :: IGP" not in head
        assert tail.index("INTEGER, INTENT(IN) :: IGP") < tail.index("5. Parameter variables")
    else:
        assert head.index("!*****") < head.index("USE swan_service_interfaces")


def test_no_version_tag_follows_an_openmp_sentinel():
    # With -fopenmp everything after '!$' is code, so an upstream version tag
    # in columns 73-80 of such a line breaks the OpenMP build; a serial build
    # does not notice it.
    tagged = []
    for path in sources():
        for number, line in enumerate(path.read_text(encoding="latin-1").splitlines(), 1):
            if line.lstrip().lower().startswith("!$") and layout.RE_TAG.search(line):
                tagged.append(f"{path.relative_to(REPOSITORY)}:{number}")
    assert not tagged, tagged


def test_alternative_switch_lines_each_get_their_continuation():
    # Under !NCF and !NNCF upstream gives two versions of the first line of
    # one statement, continued on a common line; each needs its '&'. Under
    # !MatL4 and !MatL5 it gives two versions of the last line, of which
    # neither may get one. Before this was fixed, the netCDF build of the
    # conversion commit did not compile.
    text = ("      SUBROUTINE A\n"
            "!NCF         CALL B ( X, Y,\n"
            "!NNCF        CALL B ( X,\n"
            "     &            Z )\n"
            "      OPEN(UNIT=N, FILE=F,\n"
            "     &     STATUS='REPLACE',\n"
            "!MatL4     &     ACCESS='DIRECT', RECL=1)\n"
            "!MatL5     &     ACCESS='DIRECT', RECL=4)\n"
            "      END\n")
    lines = layout.format_text(layout.convert_fixed(text)).splitlines()
    assert lines[1].startswith("!NCF") and lines[1].endswith("&")
    assert lines[2].startswith("!NNCF") and lines[2].endswith("&")
    assert not lines[3].endswith("&")
    matl = [line for line in lines if line.startswith("!MatL")]
    assert len(matl) == 2 and not any(line.endswith("&") for line in matl)
