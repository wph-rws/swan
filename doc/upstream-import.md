# Bringing in a new upstream release

TU Delft delivers SWAN as fixed-form `.ftn` files, with switch-prefixed lines
for the build variants. The fork has rewritten every file in free form,
turned the switches into build options, and moved part of the code to other
files and modules. A new upstream release therefore cannot be merged with git.
It is brought in by hand: find out what changed upstream, carry each change
over to the place where that code now lives in the fork, and check that the
fork computes what the new upstream computes, apart from the known fixes.

This document describes that route. The overview of the fork is in
[closing-account.md](closing-account.md) §7, and the architecture in
[modern-fortran.md](modern-fortran.md).

## Where upstream code lives

Most of the code is still where its name says. Of the procedures in the
pinned upstream source (`43e9bbb`, 41.51), 380 sit in a fork file with the
same name as their upstream file: `SWCOMP` from `swancom1.ftn` is in
`swancom1.f90`, `SwanConvAccur` from `SwanConvAccur.ftn90` in
`SwanConvAccur.f90`. The same holds for 507 module variables, parameters and
types, which are still declared in a fork module with the same name as their
upstream module.

The rest is recorded in [upstream-map.json](upstream-map.json), each with its
reason:

- 56 procedures moved to another file. Most of them moved to break a module
  cycle (the error routines, the date routines, part of the output), or out
  of a collection file such as `swanser.ftn` into a small module for one
  topic. `SETUPP` and `SETUP2D` moved to `swan_wave_setup.f90`.
- 27 procedures were replaced by something with another name. The command
  reader routines (`INKEYW` and the rest) are generic interfaces now, with one
  specific procedure for the default reader and one that takes a reader
  object. The MPI wrappers (`SWREDUCE` and the rest) have one specific
  procedure per data type. A routine with switch variants became one named
  routine per variant.
- 112 procedures were removed: FFTPACK, which FFTW3 replaced, and four
  routines that upstream never calls.
- Of the shared variables, 469 moved to a single fork module of another
  name; the script finds those by itself. The other 125 have an entry: they
  became a component of an object (the command reader, the clock, a
  workspace), a local variable or argument of their only user, a renamed
  variable, or were removed because nothing read them; `PI` has an entry
  because two fork modules declare one.

The map stores only what the source cannot tell. Locations are not stored:
the script reads them from the source each time. `scripts/upstream_map.py
check`, run by the pytest gate (`tests/test_upstream_map.py`), fails when a
procedure or variable is not where the map says, when something moved without
an entry, or when an entry has no reason. A move without a reason in the map
therefore cannot get into the fork unnoticed.

To look something up:

    python3 scripts/upstream_map.py where SWREDUCE KCGRD ACTOT

It prints the upstream location, the fork location or replacement, and the
reason.

## Which routines a release changes

    python3 scripts/upstream_map.py changes OLD NEW --present

`OLD` is the pinned upstream commit and `NEW` the new release, both fetched
from the `upstream` remote. For each hunk of the upstream diff the script
names the upstream procedure that contains it, and where that procedure lives
in the fork. Hunks in module declarations, and changes to every other file of
the upstream tree (build files such as `CMakeLists.txt`, `srclist.cmake` and
`switch.pl`, scripts, documentation), are listed separately, because those
have to be compared with the fork by hand. With `--present` it also counts,
per procedure, how many of the added upstream lines already occur in the fork
file, and how many of the removed lines are still there. Comments, upstream
version tags, layout and case are ignored, and switch-prefixed lines are
looked up in the files that took over that switch
([switch-manifest.json](switch-manifest.json)). For a release that has not
been brought in yet, the added lines will mostly be missing: the count shows
what is still to do, and after the work it shows what was missed. A count is
a pointer, not a verdict: a short common line can match by accident, and a
line the fork rewrote does not match.

## The route

1. Fetch the release (`git fetch upstream`) and list the changes with
   `changes OLD NEW`.
2. Carry each change over to the fork location the script names. Read the
   upstream hunk in context: the fork code around it may have changed (see
   the BF numbers in [bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md)),
   and a hunk that touches a fixed defect has to be merged with that fix,
   not pasted over it. New upstream shared variables go where a variable of
   that topic already lives in the fork (`where` on a neighbour shows it).
3. Change the build files by hand: a new upstream source file needs a
   fork file and an entry in `src/srclist.cmake`; a new switch needs a CMake
   option and an entry in the switch manifest.
4. Pin the new release: set `upstream.commit` in
   [upstream-delta.json](upstream-delta.json) and `upstream` in the map, and
   make `upstream_map.py check` pass again. Every new procedure that did not
   go to the file of its upstream name needs an entry with a reason.
5. Build the new upstream as in AGENTS.md (same gfortran, `-O3`, a path
   without a hyphen) and measure the delta gate:
   `SWAN_UPSTREAM_DELTA=1 python3 -m pytest tests/test_upstream_delta.py`.
   Every deck must give the same result as the new upstream, apart from the
   differences that have a BF number. Update the bug-fix table where the new
   release changes a fixed routine.
6. Run the other gates that `scripts/select_gates.py` selects; for a release
   that is all of them. A gate or deck that turns red is a finding to explain,
   not a reason to adjust the gate or the reference.

The import is done when step 5 and step 6 pass at the new pin. Until then the
fork claims 41.51 compatibility only for the old pin.

## Trial on upstream "patch B"

The previous upstream update, "patch B" (`f5d931d..0dabfa4`, July 2026, 11
files, 728 lines added and 697 removed), is already in the fork, because the
fork started from `43e9bbb`, which includes it. It was used as a trial of the
route above: does the script lead every part of it to the right place, and is
each part really there?

`changes f5d931d 0dabfa4 --present` found these parts:

- `SwanConvAccur` and `SwanConvStopc`. Before patch B every OpenMP thread
  checked the convergence for its own range of vertices and the totals were
  added up atomically; since patch B one thread checks all vertices
  (`!$omp single` around the call). All added lines are in the fork (8 of 8
  and 5 of 5), and none of the removed ones.
- `SWCOMP`, `SWINIT` and `SWCLME`, one hunk each, all in the fork.
- `SwanCompUnstruc`, `SwanVertlist`, the declarations of `SwanCompdata`, and
  the removal of `SwanThreadBounds`. Patch B replaced the thread bounds by a
  scheduling of the unstructured sweep in wave fronts, with a graph variant
  (`!GRAPH`) and a fixed variant (`!FXFRO`). Most of the 880 changed lines of
  `SwanCompUnstruc` are only re-indented. In the fork the scheduling sits
  behind a backend contract, in
  `src/propagation/swan_front_scheduling_backend_graph.f90` and `..._fixed.f90`;
  the script finds those through the switch manifest. 62 of the 87 added
  lines of `SwanVertlist` occur literally. The rest, and the front loops of
  `SwanCompUnstruc`, were read by hand: they are there in rewritten form,
  as fields of the backend instead of shared arrays. The one removed line
  that is still there, `deallocate(dist)`, moved upstream into both switch
  variants; the fork does it once, for both. Four `!$omp barrier`
  lines that patch B removed seem to be still in the fork; the fork has two
  barriers of its own after the stopping tests (BF-17), and the level
  assignment of the graph variant uses both stencil neighbours (BF-15).
  `SwanThreadBounds` is not in the fork.
- The build files: the top-level `CMakeLists.txt`, `src/CMakeLists.txt` and
  `switch.pl` add the new `FFRO` option, which turns `!FXFRO` lines on and
  `!GRAPH` lines off; `srclist.cmake` and `srclistnc.cmake` drop
  `SwanThreadBounds`. The fork has the option, as a choice between two
  backend files, and not the file.

So the script led every part of patch B to the place that holds it, and every
part is in the fork. What it could not decide on its own was the code that the
fork rewrote rather than converted, and the build files; those took reading.
That is the expected split. The script takes away the search; deciding whether
a rewritten piece matches stays work for a person or an assistant, and the
delta gate checks the outcome.

## Keeping the map useful

- Keep a procedure with an upstream name in the file of that name. Move it
  only to break a module cycle or for a clear reason of content, and record
  the reason in the map in the same commit. The check enforces the second
  half.
- Give a new fork procedure that takes over part of an upstream one a name
  that says so, or mention it in the `note` of the map entry.
- The reasons in the map are shared texts; reuse one where it fits, and add a
  new one where it does not. A reason that no entry uses is reported.
