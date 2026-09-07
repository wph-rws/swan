#!/usr/bin/env python3
"""Bewaak dat CI installeert wat de testlaag importeert.

De poorten draaien op een kale runner. Een module die hier in een virtualenv
toevallig aanwezig is, ontbreekt daar — en dan faalt niet één test maar de hele
job, met een foutmelding die niets met SWAN te maken heeft. Dat is precies wat
er gebeurde: `pytest`, `numpy` en `scipy` stonden nergens in de installatie.

Deze controle leest welke externe modules de testlaag op moduleniveau
importeert, en eist dat iedere workflow-job die tests draait het bijbehorende
pakket installeert. Een import die achter `try:` staat telt als optioneel: die
mag ontbreken, mits de code dat zelf opvangt.

Gebruik: python3 scripts/check_ci_dependencies.py [--workflow PAD]
"""

from __future__ import annotations

import argparse
import ast
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

#  Bestanden die een poort daadwerkelijk uitvoert.
TESTLAAG = ("tests/*.py", "examples/*/run.py", "examples/*.py", "scripts/check_*.py",
            "scripts/ci_gate.py", "scripts/strict_diagnostics.py", "scripts/select_gates.py")

#  Modulenaam -> Debian-pakket. Alleen wat de testlaag werkelijk nodig heeft.
PAKKET = {"pytest": "python3-pytest", "numpy": "python3-numpy", "scipy": "python3-scipy",
          "matplotlib": "python3-matplotlib"}


def externe_imports(pad: Path) -> set[str]:
    """Modules die dit bestand onvoorwaardelijk importeert, buiten de stdlib."""
    try:
        boom = ast.parse(pad.read_text())
    except (OSError, SyntaxError):
        return set()
    optioneel: set[int] = set()
    for knoop in ast.walk(boom):
        if isinstance(knoop, ast.Try):
            for kind in ast.walk(knoop):
                optioneel.add(id(kind))
    gevonden: set[str] = set()
    for knoop in ast.walk(boom):
        if id(knoop) in optioneel:
            continue
        if isinstance(knoop, ast.Import):
            gevonden.update(alias.name.split(".")[0] for alias in knoop.names)
        elif isinstance(knoop, ast.ImportFrom) and knoop.level == 0 and knoop.module:
            gevonden.add(knoop.module.split(".")[0])
    lokaal = {p.stem for p in ROOT.rglob("*.py")}
    return {m for m in gevonden
            if m not in sys.stdlib_module_names and m not in lokaal}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workflow", default=".github/workflows/ci.yml")
    argumenten = parser.parse_args()

    nodig: dict[str, set[str]] = {}
    for patroon in TESTLAAG:
        for pad in sorted(ROOT.glob(patroon)):
            for module in externe_imports(pad):
                nodig.setdefault(module, set()).add(str(pad.relative_to(ROOT)))

    onbekend = sorted(m for m in nodig if m not in PAKKET)
    if onbekend:
        print("CI-afhankelijkheden: FAIL", file=sys.stderr)
        for module in onbekend:
            print(f"  - {module} wordt geimporteerd door {sorted(nodig[module])[0]} "
                  f"maar staat niet in de pakkettabel van dit script", file=sys.stderr)
        return 1

    tekst = (ROOT / argumenten.workflow).read_text()
    installaties = re.findall(r"apt-get install -y ([^\n]*)", tekst)
    if not installaties:
        print("CI-afhankelijkheden: FAIL — geen installatiestap gevonden", file=sys.stderr)
        return 1

    tekorten: list[str] = []
    for regel in installaties:
        for module in sorted(nodig):
            pakket = PAKKET[module]
            if pakket not in regel:
                tekorten.append(f"{pakket} ontbreekt in: {regel.strip()[:70]}...")

    if tekorten:
        print("CI-afhankelijkheden: FAIL", file=sys.stderr)
        for tekort in sorted(set(tekorten)):
            print(f"  - {tekort}", file=sys.stderr)
        return 1

    print(f"CI-afhankelijkheden: PASS ({len(installaties)} jobs, "
          f"{len(nodig)} externe modules)")
    for module in sorted(nodig):
        print(f"  {PAKKET[module]:<20} voor {len(nodig[module])} bestand(en)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
