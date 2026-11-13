# A box with a current

`current_box.swn` is a box of 10 x 6 cells of 500 m, 20 m deep, with wind
(15 m/s) and boundary waves (Hs 2 m, Tp 7.5 s) from the west and a uniform
current of (0.6, -0.3) m/s. It runs 20 iterations in about a second and
writes Hs and Tm01 on the computational grid and the 2D spectrum in the
middle of the box.

The deck is here for the comparison with upstream
(`scripts/upstream_delta.py`). With a current, the directions that belong
to a sweep differ per frequency, and that is the case in which the DIA of
upstream leaves out part of the quadruplet source term (BF-71 in
[doc/bug-fixes-vs-tu-delft-41.51.md](../../doc/bug-fixes-vs-tu-delft-41.51.md)).
None of the other example decks has a current. On this deck the fork
differs from upstream `43e9bbb` by up to 8 mm in Hs and 0.06 s in Tm01,
and the fork before the fix gave the output of upstream byte for byte
(gfortran 16.2 at `-O3`, 15 November 2026).

`tests/test_dia_current_rotation.py` tests the fix itself: it runs this box
as it is and turned by a quarter turn, and the two runs must agree.
