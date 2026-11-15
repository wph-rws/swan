#!/usr/bin/env python3
"""Compare libm_linux.txt and libm_windows.txt (output of libm.f90) function by function."""
from collections import Counter
from pathlib import Path

D = Path(__file__).resolve().parent
a = (D / "libm_linux.txt").read_text().split("\n")
b = (D / "libm_windows.txt").read_text().replace("\r", "").split("\n")
total, differ, largest = Counter(), Counter(), Counter()
for x, y in zip(a, b):
    if not x:
        continue
    f = x.split()[0]
    total[f] += 1
    if x != y:
        differ[f] += 1
        largest[f] = max(largest[f], abs(int(x.split()[1], 16) - int(y.split()[1], 16)))
for f in total:
    print(f"{f:6s} {differ[f]:6d} of {total[f]} differ, at most {largest[f]} unit(s) in the last place")
