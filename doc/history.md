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
   compile with module interfaces, and the fork's own guard that keeps the
   wave-induced set-up from reusing a cached depth.
5. **Faster kernels** (`a6a5b2a` to `4457162`), one commit per kernel; same output at
   `-O3`. The tag `fork-41.51-identical` marks the last commit that computes
   exactly what upstream computes.
6. **Tests, CI and the comparison with upstream**, then **one commit per bug
   fix**, BF-01 to BF-68, with the fork's own additions (balance residual,
   so-rp verification, upstream map, NUMERIC EXTRAP, the gates) between them
   where they were made. Each bug-fix commit holds the fix and its row in
   [bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md), and
   ends with the trailer `Bug-fix: BF-NN`. Most also hold their test; the
   exceptions are listed below.
7. **Layout check and this record** (`5e60340` and the two commits after it).

The bug numbers were given in the order the bugs were found. BF-56 to BF-63
come after BF-64 to BF-68 in the history, because BF-59 and BF-67 change the
same reading code in `SREDEP` and were made in that order.

## Fixes without a test in their own commit

For 52 of the 68 fixes the commit also adds or extends a test under `tests/`
or `examples/`. For the other 16 it does not:

- **The test is in the same commit, outside `tests/`.** BF-02 registers its
  CTest case `netcdf_mixed_block_output` in `CMakeLists.txt`; BF-17 adds
  `scripts/unstruc_determinism_smoke.py`, which CI runs; BF-05 adds
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

On 10 November 2026 every commit from the conversion on (103 commits) was
checked out into a fresh worktree and built from scratch at `-O0` with
gfortran 16.2, once serially and once with OpenMP, and its Fortran sources
were checked against the layout. All 103 built both ways, and all were in
the layout. The rename commit does not build (see above). The tests were run
at the tip: CTest serially, with OpenMP and with runtime checks (66 of 66
each), under MPI (73 of 73), the strict gate, pytest and the delta gate,
including the four heavy decks. `fork-41.51-identical` was compared with an
upstream build at `-O3` on the 41 light decks, serially; OpenMP and MPI runs
were not compared with upstream at that commit. The scripts that built and checked the history are kept outside the
repository, with the earlier histories.

Until 10 November 2026 the delta gate compared output with white space
collapsed. It therefore missed that the first conversion had taken blanks out
of the quoted text of 318 format specifications (table headers and PRINT
lines). The rebuilt history keeps those blanks as upstream has them from the
conversion on, and the gate compares bytes. An earlier claim that output was
"byte-identical" to upstream holds only with white space collapsed.

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
fix, unless the row names another commit. BF-01 has two commits: the fix,
and a later one (`413681e`) that resets the rest of the state a second run
inherited; both carry the trailer.

## Tags

| Tag | Commit | What it marks |
|---|---|---|
| `upstream-41.51` | `43e9bbb` | the TU Delft source the fork starts from |
| `fork-41.51-identical` | `4457162` | the last commit that computes exactly what upstream computes (41 light decks, serial, `-O3`) |
| `fork-41.51-2026-11-10` | the last commit of 10 November 2026 | the state of 10 November 2026, after the history was rebuilt |

## Measurements made before 10 November 2026

The history was rebuilt on 10 November 2026, with the knowledge of that day:
corrections of a fix were folded into the commit of that fix, and every
commit was given the layout above. A measurement that was made earlier names
the commit of this history that holds the same change. That commit also holds
corrections that came later (each is described in the bug-fix table, with
its fix), so the measured number belongs to that commit only as far as those
corrections leave the measured deck unchanged, and none of these numbers
was measured again on the rebuilt history. The earlier history, on which they
were measured, ends in the tag `archive/before-rebuild-2026-11-10`; older
states are kept, outside the repository, in git bundles.
