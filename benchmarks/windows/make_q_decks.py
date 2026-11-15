#!/usr/bin/env python3
"""Section E: copies of the Voordelta deck and the so-rp physics deck with QUADRUPLET 2, 3 and 1.

The QUADRUPLET line goes before the first output or numerics command; a QUAD line of the deck itself
is taken out. Run under WSL: python3 make_q_decks.py
"""
import shutil
from pathlib import Path

R = Path("/mnt/c/swan-win-perf/final-round/decks")
SRC = {
    "voordelta": (Path("/mnt/c/swan-win-perf/upstream-vergelijking/deck"), "voordelta.swn"),
    "sorp": (R / "sorp_physics", "INPUT"),
}
FIRST = ("BLOCK", "TABLE", "POINTS", "NUM", "COMPUTE", "TEST", "CURVE", "SPEC", "GROUP", "FRAME")
for name, (directory, deck) in SRC.items():
    text = (directory / deck).read_text().replace("\r\n", "\n")
    for q in (2, 3, 1):
        out = R / f"{name}_q{q}"
        if out.exists():
            shutil.rmtree(out)
        out.mkdir()
        for f in directory.iterdir():
            if f.is_file() and f.name not in (deck, "INPUT") and f.suffix not in (".py", ".md", ".swn"):
                shutil.copy(f, out / f.name)
        lines, done = [], False
        for line in text.split("\n"):
            head = line.strip().upper()
            if head.startswith("QUAD"):
                continue
            if not done and head.startswith(FIRST):
                lines.append(f"QUADRUPLET {q}")
                done = True
            lines.append(line)
        (out / "INPUT").write_text("\n".join(lines))
        print(out.name, sorted(p.name for p in out.iterdir()))
