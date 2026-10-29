# Provenance of the operational deck

`so-rp_osk_org.swn` is the template from which the operational so-rp suite of
Rijkswaterstaat builds a SWAN deck for each condition (via
`bin/Run_so-rp_swan.sh`). It was fetched on 28 October 2026 and is byte for
byte identical to the original:

- date in the production environment: 2 September 2022;
- md5 `27eb876439c91f1496723cc65e2d695d`, 2833 bytes;
- probably written by Krijn Saman ("KS", with "PB"): the run script names
  Krijn Saman as its author (first version 20 August 2022), and the commits in
  the `hydro` repository from that time are by "KS" and "KS/PB". The template
  itself is not in that repository, because its `.gitignore` excludes all
  `*.swn` files.

The run script replaces `WST_ORG` (water level in m), `WSN_ORG` and `WR_ORG`
(wind speed and direction), `HS_ORG_BG2` and `TP_ORG_BG2` (boundary waves from
the BG2 regression) and `GOLFTR_OSK` (transmission of the Oosterschelde storm
surge barrier).

The deck only runs with the suite's own `swaninit` (RWS/DZL), which sets `#` as
the comment character. With SWAN's default (`$`) the run does not start, and
RWS 41.31 still ends with exit code 0.

## Two errors, and the corrected template

**The grid is read incorrectly.** `so-rp.rgf` holds 249 × 578 points, so
248 × 577 cells, but the deck says `CGRID CURVILINEAR 249 578`. SWAN reports
"Unexpected end of file" and 1558 times "incorrect grid coordinates", but
carries on because `MAXERR = 2`. As a result 70,745 of the 71,957 wet grid
points (98%) lie on average 49 m, and up to 248 m, away from their position in
`so-rp.rgf`, while the bottom stays tied to the grid index. With `248 577`
every coordinate is right to within 4 cm. The bottom has one row and column
more (250 × 579); there `INP BOTTOM … 249 578` is correct.

**The boundary lies on that wrong grid.** The wet sea boundary of the model is
the first row with a known bottom. In SWAN's grid indices, which count from 0,
that is J=65 from I=29 to I=218, and on the north-east side I=28 from J=66
onwards (seaward of it the bottom is `NaN`). The deck's 18 boundary segments
lie 40–140 m inside that line, closest to the sea boundary of the incorrectly
read grid. With the correct grid SWAN therefore reports "invalid boundary
point". A segment given in grid indices must lie exactly on that edge: SWAN
accepts wet points further inside without a message, but overwrites the
boundary spectrum there in the first sweep, so that no waves enter at all.

At the reference condition (20 m/s from 310°, NAP +3.00 m, barrier open), grid
and boundary together change the mean Hsig over the 130 wet output points by
less than 1 cm (RWS 41.31: 1.608 m as in production, 1.602 m corrected), but
single points by up to 9 cm.
With the current code (41.51) the effect is the same: 1.659 m with this
deck, 1.653 m corrected, up to 9 cm at single points. The current code also
reports the 1558 grid errors and carries on. Measured on 29 October 2026; on
this deck the current code is 5.2 cm higher on average than RWS 41.31A.1, and
up to 13 cm at single points.

The matrix runs this deck unchanged through the targets `rws_4131_production`
and `current_4151_production` (`template: production`), because production
still uses it. `matrix/deck.py` accepts it only if its md5 is the one above.

`../so-rp_osk_org.swn` is this deck with exactly two corrections:
`CGRID CURVILINEAR 248 577`, and the boundary
`BOUND SEGMENT IJ 218 65 29 65` and `BOUND SEGMENT IJ 28 66 28 71` with the
same boundary waves. Everything else is byte for byte the same;
`matrix/test_deck.py` checks that. Which boundary the maintainer intended, and
whether the BG2 boundary waves were tuned to the current location, has been
put to the maintainer.

## What the deck chooses itself

`GEN3 KOMEN`, `BREAK CONST 1.0 0.73`, `FRIC JONSWAP CFJON=0.038`,
`TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5`, `SET CDCAP=0.00275` and
`NUMERIC STOPC DABS=0 DREL=0.01 CURVAT=0.001 NPNTS=98 STAT MXITST=40 ALFA=0.01`.
`WCAP KOMEN DELTA=1` and `QUAD IQUAD=2` are also the defaults in 41.31 and
41.51. What the deck leaves open, and what differs between 41.31 and 41.51: the
wind drag (Wu instead of the fit) and the triad parameters URCRIT (0.2 → 0.63)
and URSLIM (0.01 → 0.1).

## The SWANIMP deck

The same directory in the production environment also holds `SWANIMP`, a deck from 2014. It is not
the operational computation and this repository does not use it: it chooses
no source terms (so Westhuysen in 41.51), uses `ACCUR` with `MXITST=20`, has
one boundary segment (`IJ 46 67 220 67`) and reads separate `GRID` and
`DEPTH` files. It has the same grid error, `CGRID … 249 578`.
