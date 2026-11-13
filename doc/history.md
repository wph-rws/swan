# How the history of the fork is built

The fork starts from TU Delft SWAN 41.51, upstream commit `43e9bbb` (tag
`upstream-41.51`). Its history is built so that each line can be traced back
to TU Delft, and each change of the fork to the commit that made it and the
reason given there.

## The order of the commits

1. **Rename** (`d174279`). Every upstream source gets the name it has in free
   form (`swancom1.ftn` becomes `swancom1.f90`); not a byte changes. This
   commit does not build; `git bisect skip` it.
2. **Conversion and layout** (`2bc4d0c`). The fixed-form sources are
   converted to free form, and all sources get the layout of
   `scripts/fortran_layout.py`. Every statement stays as it was, and the
   regression decks give byte-identical output to upstream.
3. **FFTW** (`5809f3d`). FFTW3 replaces FFTPACK behind the same three calls.
4. **Modules** (`0f9877e`). Every routine moves into a module, the shared
   state into modules that own it, and the build into CMake with backend
   files instead of switch.pl. Still byte-identical to upstream. This commit
   is large (175 files), because a routine cannot move into a module without
   its callers. It also holds the fix of BF-04, whose old calls do not
   compile with module interfaces. It also brings the fork's first
   speed-ups, which keep the output bit for bit: a cache of wave number and
   group velocity per grid point for stationary runs on a fixed depth (not
   with a nonstationary run, a changing depth or the wave-induced set-up),
   a bucket index for the nearest-vertex search and a cache of boundary
   faces on unstructured grids, an O(N log N) sort of the vertex list, and
   the precomputed MDIA coefficients (`SWPRE4W`).
5. **Faster kernels** (`a6a5b2a` to `4457162`), one commit per kernel; same
   output at `-O3`. The tag `fork-41.51-identical` marks the last commit that computes
   exactly what upstream computes.
6. **Tests, CI and the comparison with upstream**, then **one commit per bug
   fix**, BF-01 to BF-68, with the fork's own additions (balance residual,
   so-rp verification, upstream map, NUMERIC EXTRAP, the gates) between them
   where they were made. Each bug-fix commit holds the fix and its row in
   [bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md), and
   ends with the trailer `Bug-fix: BF-NN`. Most also hold their test; the
   exceptions are listed below.
7. **Layout check, this record, and the corrections after a review**
   (`5e60340` and the commits after it).
8. **Two more fixes and a fault of the fork** (`2db9a5d` to `6013ccb`, 11
   November 2026), found when the earlier measurements were repeated: BF-69
   and BF-70, each in one commit as above, and the commit that lets the
   quasi-coherent model run under MPI again, which has no BF number because
   upstream does not have that fault.

The bug numbers were given in the order the bugs were found. BF-56 to BF-63
come after BF-64 to BF-68 in the history, because BF-59 and BF-67 change the
same reading code in `SREDEP` and were made in that order. BF-58 also comes
before BF-57, whose test needs it.

## Fixes whose parts are in more than one commit

- **BF-01** has two commits: the fix, and a later one (`413681e`) that resets
  the rest of the state a second run inherited.
- **BF-04**: the fix is part of the module commit (`0f9877e`), because the
  old calls do not compile with module interfaces; its row came in the next
  commit that records it. Both carry the trailer.
- **BF-09**: the repair, `ENSURE_FIELD_SIZE`, came with the fix of BF-08
  (`02e03f6`), which needed it for the same arrays; the commit of BF-09 adds
  the test and the row.

## Fixes without a test in their own commit

For 53 of the 70 fixes the commit also adds or extends a test under `tests/`
or `examples/`. For the other 17 it does not:

- **The test is in the same commit, outside `tests/`.** BF-02 registers its
  CTest case `netcdf_mixed_block_output` in `CMakeLists.txt`; BF-17 adds
  `scripts/unstruc_determinism_smoke.py`, which CI runs; BF-70 adds
  `scripts/triad_dewit_determinism_smoke.py`, a CTest case of the OpenMP
  builds; BF-05 adds
  `scripts/lifetime_protocol.py`, a Valgrind protocol that is run by hand.
- **The test came with a later fix**, which needed the same test deck or
  helper: BF-10 in BF-52 (`3ae5395`), BF-14 in BF-50 (`d1353a4`), BF-19 in
  BF-36 (`a7014ca`), BF-22 in BF-43 (`238bb84`) and BF-64 in BF-65
  (`67a62d1`). Those tests name the fix they guard.
- **There is no regression test**, and the row in the bug-fix table says how
  the defect was shown instead. BF-04 is caught by the compiler; BF-13 and
  BF-18 were found with Valgrind; BF-20 and BF-33 with compiler warnings
  about uninitialised values; BF-08 by the runtime checks, shown by removing
  the fix by hand; BF-37 needs an exact floating-point value that a deck
  practically never gives; BF-38 changes a comment only.

## How the history was checked

On 10 November 2026, after a review of the published text by six
independent reviewers had led to corrections in the trees of seven commits
(the conversion, the registration of four tests, two benchmark files, the
layout test, and the METIS condition of the NaN depth check of BF-61) and in
seventeen commit messages, the history was put together once more, and every
commit from the conversion to `c9bf3e0` (109 commits) was checked out into a
fresh worktree and built from scratch at `-O0` with gfortran 16.2, once
serially and once with OpenMP, and its Fortran sources were checked against
the layout. All 109 built both ways, and all were in the layout. The rename
commit does not build (see above). The conversion and FFTW commits were also
built with netCDF, which before the correction failed there, as a MATL4
build would have, because four continuation lines under alternative switches
(`!NCF`/`!NNCF` and `!MatL4`/`!MatL5`) were converted wrongly. A
static check confirmed that no CMake file of any commit names a file that
the commit lacks.

The measurements of 10 and 11 November 2026 that the documents attribute to
a commit of this history were made on builds of the commits just before the
last of these corrections (the METIS condition of BF-61, and for the so-rp
runs, flang and the test models also the JAC condition of one test
registration). Their sources differ from the named commits only in those two
conditions, which no measured build reaches; the full build matrix was
repeated on `13f05a6`.

At `c9bf3e0`, built from scratch with gfortran 16.2 at `-O3`, CTest passed 66
of 66 serially, with OpenMP and with runtime checks, and 73 of 73 under MPI;
the strict gate was green (1030 warnings, the 233 in the checked kinds all
with a verdict); pytest passed 462 tests and skipped one (the measured part
of the delta gate, which runs on its own); and the delta gate was green, the
four heavy decks included. The configurations of `scripts/build_matrix.py`
and their results are in [support-matrix.md](support-matrix.md).
`fork-41.51-identical` was compared with an upstream build at `-O3` on the
41 light decks, serially; OpenMP and MPI runs were not compared with
upstream at that commit. Its sources are the same as before the last
assembly. The scripts that built and checked the history are not part of
the repository and were not kept.

Until 10 November 2026 the delta gate compared output with white space
collapsed. It therefore missed that the first conversion had taken blanks out
of the quoted text of 318 format specifications (table headers and PRINT
lines). The rebuilt history keeps those blanks as upstream has them from the
conversion on, and the gate compares bytes. An earlier claim that output was
"byte-identical" to upstream holds only with white space collapsed.

## Dates

The author dates of the commits up to the record commits are those of the
original work, from July to November 2026, to the day; the time of day is
often a placeholder. The checks that the messages
name (builds with gfortran 16.2, byte-identical output, tests that pass)
were repeated for every commit when the history was rebuilt on 10 November
2026, as described above; gfortran 16.2 itself has been used since
28 October 2026. A correction folded into a commit can therefore mention a
date after the commit's own date.

## Tracing a line to upstream

    git blame -w -C -C --ignore-revs-file .git-blame-ignore-revs src/swancom1.f90

`-w` ignores changes of indentation, `-C -C` follows code that moved to
another file (the set-up code to `swan_wave_setup.f90`, say), and the file
`.git-blame-ignore-revs` lists the mechanical commits (the conversion), so
that a line that only changed its form is attributed to the commit before.
For a line that is still upstream text, blame then names an upstream commit;
otherwise it names the fork commit that changed it. `git config
blame.ignoreRevsFile .git-blame-ignore-revs` makes the file the default.
`git log --follow src/swancom1.f90` follows a file through the rename.

For a whole routine, `scripts/upstream_map.py where NAME` names its upstream
and fork location, and [upstream-import.md](upstream-import.md) shows how to
put an upstream release in the fork's layout and compare it as text.

## Finding the commit of a fix

    git log --grep='Bug-fix: BF-62'
    python3 scripts/bf_commits.py list

The section "Commit of each fix" at the end of the bug-fix table names the
commit of every fix; `scripts/bf_commits.py check` (pytest gate) fails when
it and the history disagree.

A measurement in a row of the bug-fix table belongs to the commit of that
fix, unless the row names another commit. A fix with more than one commit
(above) has the trailer on each.

## Tags

| Tag | Commit | What it marks |
|---|---|---|
| `upstream-41.51` | `43e9bbb` | the TU Delft source the fork starts from |
| `fork-41.51-identical` | `4457162` | the last commit that computes exactly what upstream computes (41 light decks, serial, `-O3`) |
| `fork-41.51-2026-11-10` | the last commit of 10 November 2026 | the state of 10 November 2026, after the history was rebuilt |

CI runs on branches and pull requests, not on tags: a tag only marks a point
in the history, and a run at an old tag would use the workflow of that day.
A run at `fork-41.51-2026-11-10`
itself, with the workflow of that commit, fails in the four MPI jobs. The
cause is the runner, not the code: two tests, `mpi_edges` and
`mpi_output_ownership`, start three or four processes, the runner has fewer
cores, and Open MPI 5 refuses with "not enough slots". Every other job
passes there. Commit `442f8f5` (13 November 2026) lets the CI jobs start
more processes than cores; from that commit on CI is green.

## Measurements made before 10 November 2026

The history was rebuilt on 10 November 2026, with the knowledge of that day:
corrections of a fix were folded into the commit of that fix, and every
commit was given the layout above. The earlier history is not kept. The
measurements that the documents quoted from it were repeated on the rebuilt
history on 10 and 11 November 2026, at commits `aea2500` and `20bfacb`,
whose sources differ only in comments, and for the state before a fix on
the commit before it in this history. The test builds, small programs and
scripts that a repeated number needs are in
[measurements/](measurements/README.md). Where the repeat gave
a different number, the documents give the new one; an old number that is
still mentioned beside it ("an earlier set gave ...") is only an account of
the original work.

A few statements were not repeated, mostly because what they need was not
kept. They stand only as an account of the original work:

- **Counts inside one commit.** BF-05: the four steps of the lost memory
  (216 + 792 + 936 + 96 B), whose intermediate states existed only while the
  fix was made. BF-29: the change from 6.778e-05 to 6.779e-05 of an earlier
  form of the fix that is not in the history.
- **Decks, point files and trial lists that were not kept.** BF-05: the two
  `FILE` error decks (280 B and 328 B lost). BF-46: the trial of more than
  1900 variants. BF-53: the 29 variants run with the fork before the fix.
  BF-56: the comparison on 13 extra points of the so-rp deck without triads.
  The larger QCM deck (40 × 20 meshes) in the note on `qcm/qcm_scatter.swn`
  in [upstream-delta.json](upstream-delta.json).
- **Parts of a check that were not run on their own.** BF-01: the `fort.17`
  case and the refused whitecapping and boundary output. BF-17: that no run
  left the iteration loop at a different iteration (not checked separately;
  all runs after the fix were bit-identical). BF-62: the double-precision
  finite-difference check by Codex (3·10⁻¹⁰ at three bins, factor 248.05);
  the single-precision test build of 11 November 2026 replaces it. The
  comparison of the XNL interaction table between the fork and upstream.
  In [thread-state-manifest.md](thread-state-manifest.md), the direct check
  that passed old and zeroed values through `SWCAP`, `SWCAP8` and `SSURF`.
- **Counts of compiler warnings and of source patterns at the commit of a
  fix**: the six warnings of BF-20, the 40 synonyms and 9 `VALIDBP` warnings
  of BF-24, the 22 warnings of BF-27, and the 1505 compound conditions (307
  of them checked by hand) of BF-54. These were not counted again; the
  commits are in this history, so they can be.
- **A reading that the repeat could not settle.** BF-58: the outline of the
  excluded area of the fan grid "of 4 points" before the fix. `TEST 180`
  prints 5 points there (4 of them the corners of one mesh) and 30 after the
  fix, but the print does not mark where a curve ends, so the text was left
  as it was.
