#!/usr/bin/env python3
"""Section F: the output of the Windows build against two Linux builds of the same source.

windows-out/<deck>   serial build with MSYS2 ucrt64 gfortran 16.2 (run_F.cmd)
linux-ref/<deck>     gfortran 16.2 under WSL, the default Release build
linux-ref-nomvec/    the same without glibc's vector maths library (-nostdinc)

Files are compared after taking out carriage returns; for files that differ, the numbers are compared
and the largest absolute and relative difference is printed. Run under WSL: python3 compare_F.py
"""
import re
from pathlib import Path

R = Path(__file__).resolve().parent
NUMBER = re.compile(r"[-+]?(?:\d+\.\d*|\.\d+|\d+)(?:[EeDd][-+]?\d+)?")


def numbers(text):
    return [float(x.replace("D", "E").replace("d", "e")) for x in NUMBER.findall(text)]


for deck in sorted(p.name for p in (R / "linux-ref").iterdir()):
    for name in sorted(p.name for p in (R / "linux-ref" / deck).iterdir()):
        if name == "INPUT.deck":
            continue
        win = R / "windows-out" / deck / name
        if not win.exists():
            print(f"{deck}/{name}: missing in the Windows output")
            continue
        w = win.read_bytes().replace(b"\r", b"").decode("latin-1")
        line = f"{deck}/{name}:"
        for ref in ("linux-ref", "linux-ref-nomvec"):
            l = (R / ref / deck / name).read_bytes().replace(b"\r", b"").decode("latin-1")
            if w == l:
                line += f"  {ref}: identical;"
                continue
            #  the header of a file names the version and the run; compare the numbers
            a, b = numbers(w), numbers(l)
            if len(a) != len(b):
                line += f"  {ref}: {len(a)} against {len(b)} numbers;"
                continue
            diff = [(abs(x - y), abs(x - y) / max(abs(x), abs(y), 1e-30)) for x, y in zip(a, b) if x != y]
            lines_differ = sum(1 for x, y in zip(w.split("\n"), l.split("\n")) if x != y)
            if not diff:
                line += f"  {ref}: numbers equal, {lines_differ} text line(s) differ;"
            else:
                line += (f"  {ref}: {len(diff)} of {len(a)} numbers differ, largest {max(d[0] for d in diff):.3g}"
                         f" (relative {max(d[1] for d in diff):.2g});")
        print(line)
