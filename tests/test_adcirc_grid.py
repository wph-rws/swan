"""An unstructured grid from an ADCIRC file (fort.14) computes like the same grid from Triangle.

SWAN reads an unstructured grid from Triangle, Easymesh or
ADCIRC files. Only the Triangle route had a test. The ADCIRC route
also reads the bottom depth from fort.14, and, since the fork allocates every
input field empty up front (DEPTH(0)), wrote it outside that empty array: the run
gave the correct output and then aborted on a corrupted heap.

The test converts the grid of the unstructured example to fort.14,
with the same node numbering, boundary markers and depth, and requires both
routes to end cleanly with the same binary and give byte-identical output. The
CMake option ADCIRC changes nothing for this route: the reader is always
built.
"""
from __future__ import annotations

import shutil
import subprocess
from pathlib import Path

from swan_executable import swan_executable

REPO = Path(__file__).resolve().parent.parent
CASE = REPO / "examples" / "nonstationary" / "unstructured"
BASENAME = "nonstationary_unstructured"
OUTPUTS = (f"{BASENAME}_center.tbl", f"{BASENAME}_hs.blk")
DEPTH = "20.0"  # bottom.bot of the example is 20 m everywhere


def fort14(node_text: str, element_text: str) -> str:
    """Convert a Triangle grid to ADCIRC, with the same boundary markers.

    The ADCIRC reader gives the nodes of open boundary j marker j, and those of
    land boundary j marker (number of open boundaries + j). Marker 1 in .node
    therefore becomes the only open boundary, marker 2 the only land boundary.
    """
    nodes = [line.split() for line in node_text.splitlines()[1:] if line.strip()]
    elements = [line.split() for line in element_text.splitlines()[1:] if line.strip()]
    lines = ["ADCIRC copy of the Triangle example", f"{len(elements)} {len(nodes)}"]
    lines += [f"{n[0]} {n[1]} {n[2]} {DEPTH}" for n in nodes]
    lines += [f"{e[0]} 3 {e[1]} {e[2]} {e[3]}" for e in elements]
    open_boundary = [n[0] for n in nodes if n[3] == "1"]
    land_boundary = [n[0] for n in nodes if n[3] == "2"]
    lines += ["1", str(len(open_boundary)), str(len(open_boundary)), *open_boundary]
    lines += ["1", str(len(land_boundary)), f"{len(land_boundary)} 0", *land_boundary]
    return "\n".join(lines) + "\n"


def adcirc_deck(deck: str) -> str:
    """Read the grid from fort.14; the depth then comes from there too."""
    triangle = f"READGRID UNSTRUCTURED TRIANGLE '{BASENAME}'"
    assert triangle in deck, "the example deck has changed shape"
    deck = deck.replace(triangle, "READGRID UNSTRUCTURED ADCIRC")
    bottom = [line for line in deck.splitlines() if "BOTTOM" in line]
    assert len(bottom) == 2, bottom
    return "\n".join(line for line in deck.splitlines() if "BOTTOM" not in line) + "\n"


def run(work: Path, deck: str) -> None:
    for name in ("bottom.bot", "wind.wnd", f"{BASENAME}.node", f"{BASENAME}.ele"):
        shutil.copy2(CASE / name, work / name)
    (work / "INPUT").write_text(deck)
    result = subprocess.run([swan_executable()], cwd=work, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr[-2000:]
    assert (work / "norm_end").is_file()


def test_fort14_takes_over_grid_and_boundaries():
    text = fort14((CASE / f"{BASENAME}.node").read_text(),
                  (CASE / f"{BASENAME}.ele").read_text())
    lines = text.splitlines()
    assert lines[1] == "16 15"
    assert lines[2] == f"1 0.0 0.0 {DEPTH}"
    assert lines[17] == "1 3 1 2 7"
    assert lines[33:36] == ["1", "3", "3"]


def test_adcirc_grid_computes_like_triangle_grid(tmp_path):
    deck = (CASE / f"{BASENAME}.swn").read_text()
    triangle = tmp_path / "triangle"
    adcirc = tmp_path / "adcirc"
    triangle.mkdir()
    adcirc.mkdir()
    run(triangle, deck)
    (adcirc / "fort.14").write_text(fort14((CASE / f"{BASENAME}.node").read_text(),
                                           (CASE / f"{BASENAME}.ele").read_text()))
    run(adcirc, adcirc_deck(deck))
    for name in OUTPUTS:
        assert (adcirc / name).read_bytes() == (triangle / name).read_bytes(), name
