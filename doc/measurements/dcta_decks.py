"""Make the short two-dimensional DCTA timing decks from the submerged bar example."""

import argparse
from pathlib import Path


def make_decks(destination: Path) -> None:
    example = Path(__file__).resolve().parents[2] / "examples/nonlinear_interactions/triad"
    template = (example / "triad_dcta.swn").read_text()
    template = template.replace("ONEDIMENSIONAL", "TWODIMENSIONAL")
    template = template.replace("30 0 1.0 1.0", "30 1 1.0 3.0")
    template = template.replace("PAR 0.06 2.0 0.0 2.0", "PAR 0.06 2.0 0.0 10.0")
    template = "\n".join(line for line in template.splitlines() if not line.startswith("TEST ")) + "\n"
    bottom_row = (example / "bottom.bot").read_text()
    for name, grid, spectrum, model, iterations in (
        ("collinear", "100 12", "72 0.10 2.0 31", "DCTA", 12),
        ("noncollinear", "36 8", "36 0.10 2.0 21", "DCTA NONCOLL", 6),
    ):
        deck = template.replace("30.0 0.0 300 0", f"30.0 3.0 {grid}")
        deck = deck.replace("SECTOR -10.0 10.0 20 0.10 2.0 31", f"CIRCLE {spectrum}")
        deck = deck.replace("\nTRIAD DCTA\n", f"\nTRIAD {model}\n")
        deck = deck.replace("\nCOMPUTE", "\nBLOCK 'COMPGRID' NOHEADER 'height.blk' LAYOUT 3 HSIGN"
                            f"\nNUMERIC STOPC NPNTS=101 STAT MXITST={iterations}\nCOMPUTE")
        folder = destination / name
        folder.mkdir(parents=True, exist_ok=True)
        (folder / "INPUT").write_text(deck)
        (folder / "bottom.bot").write_text(bottom_row + bottom_row)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    make_decks(parser.parse_args().directory)
