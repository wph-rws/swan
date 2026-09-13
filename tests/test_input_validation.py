"""Invoer die geen formulering aanduidt moet worden afgewezen, niet genegeerd.

De QUAD- en TRIAD-commando's nemen een kaal getal. Zonder controle is elk
getal dat de dispatcher in SWCOMP niet kent een stille no-op: de run parseert,
drukt de gekozen formulering af in de kop, zet via de `ITRIAD .GT. 0`- en
`IQUAD .GE. 1`-poorten de bijbehorende boekhouding aan (Ursell, biphase,
MEMNL4, de limiter) en rekent vervolgens geen brontermen uit. Het resultaat
ziet eruit als een geslaagde som met de gevraagde fysica erin.

Dezelfde klasse: [lambda] van de quadruplets. FAC4WW deelt door (1-lambda)**4
en neemt LOG(1-lambda) om de verschoven spectrale bins te lokaliseren, dus
lambda = 1 geeft een deling door nul gevolgd door INT() van een NaN als
allocatiegrens, en lambda > 1 de logaritme van een negatief getal.

Deze decks horen niet onder examples/ thuis: dat zijn geldige fysicacases die
ook een referentie-uitvoer hebben. Fout-invoer wordt hier ter plekke
geschreven, zoals test_lifetime_fault.py het ook doet.
"""
from __future__ import annotations

import os
import shutil
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
QUICK_TEST = REPO / "examples" / "quick_test"


def _swan() -> str:
    from_build = os.environ.get("SWAN_EXECUTABLE")
    if from_build and Path(from_build).is_file():
        return from_build
    installed = shutil.which("swan.exe")
    if installed:
        return installed
    pytest.skip("geen swan.exe: draai via ctest of zet SWAN_EXECUTABLE")


def _run(tmp_path: Path, extra_command: str) -> tuple[int, str]:
    """Draai het smoke-deck met één extra commando erin."""
    work = tmp_path / "work"
    work.mkdir()
    shutil.copy2(QUICK_TEST / "bottom.bot", work / "bottom.bot")
    deck = (QUICK_TEST / "quick_test.swn").read_text(encoding="utf-8")
    assert "GEN3 KOMEN" in deck, "smoke-deck is van vorm veranderd"
    deck = deck.replace("GEN3 KOMEN", f"GEN3 KOMEN\n{extra_command}", 1)
    (work / "INPUT").write_text(deck, encoding="utf-8")

    result = subprocess.run([_swan()], cwd=work, capture_output=True, text=True)
    printfile = work / "PRINT"
    report = printfile.read_text(encoding="utf-8", errors="replace") if printfile.is_file() else ""
    return result.returncode, report


#  De bare-integer vormen die de dispatcher niet kent. TRIAD bereikt zijn
#  numerieke tak alleen via de benoemde parameter: `TRIAD 4` strandt al op
#  INKEYW, `TRIAD ITRIAD=4` kwam er tot nu toe doorheen.
REJECTED = [
    ("TRIAD ITRIAD=4", "TRIAD [itriad]"),
    ("TRIAD ITRIAD=6", "TRIAD [itriad]"),
    ("QUADRUPLET 6", "QUADRUPLET [iquad]"),
    ("QUADRUPLET 9", "QUADRUPLET [iquad]"),
    ("QUADRUPLET 1 1.0", "[lambda]"),
    ("QUADRUPLET 1 1.5", "[lambda]"),
    ("QUADRUPLET 1 0.0", "[lambda]"),
    ("QUADRUPLET MDIA LAMBDA 1.0 CNL4 3.0E7", "[lambda]"),
]


@pytest.mark.parametrize("command,expected", REJECTED)
def test_onbekende_formulering_wordt_afgewezen(tmp_path, command, expected):
    returncode, report = _run(tmp_path, command)
    assert returncode != 0, (
        f"'{command}' liep door tot een normaal einde; een getal dat geen "
        f"formulering aanduidt hoort de run te stoppen.\n{report[-2000:]}"
    )
    assert expected in report, (
        f"'{command}' stopte, maar zonder de melding die zegt waarom.\n"
        f"{report[-2000:]}"
    )


#  De tegenproef. Zonder deze zou een controle die alles afwijst ook slagen.
ACCEPTED = [
    "TRIAD ITRIAD=11",
    "TRIAD ITRIAD=1",
    "TRIAD LTA",
    "TRIAD DCTA",
    "QUADRUPLET 2",
    "QUADRUPLET 8",
    "QUADRUPLET 1 0.25",
    "QUADRUPLET MDIA LAMBDA 0.25 CNL4 3.0E7",
]


@pytest.mark.parametrize("command", ACCEPTED)
def test_geldige_formulering_blijft_lopen(tmp_path, command):
    returncode, report = _run(tmp_path, command)
    assert returncode == 0, (
        f"'{command}' is geldig maar werd geweigerd.\n{report[-2000:]}"
    )
