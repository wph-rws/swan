# Closing account

This document says what this fork delivers compared with TU Delft SWAN 41.51,
where the evidence for it is, and what remains open. It is meant for someone
who has to decide whether to use this fork, and who for that decision needs to
know what is not claimed.

## 1. What is in place

Upstream SWAN 41.51 has 76 source files, 19 of them in fixed-form Fortran,
four `COMMON` blocks, sixteen shared state containers, a Perl-driven build and
no tests.

The fork compared with that:

| | Upstream 41.51 | Fork |
|---|---|---|
| Source files | 76 | 161 (`git ls-files src \| grep -c '\.f90$'`) |
| Fixed-form | 19 | 0 |
| `COMMON` blocks | 4 | 0 |
| Modules | — | 164 |
| Shared state containers | 16 | 0; the run state in `M_GENARR`, `M_PARALL` and `OUTP_DATA` is still at module level, with an owner that clears it ([run-state-ownership.md](run-state-ownership.md)) |
| Registered tests | 0 | 60 serial, 64 with MPI (28 October 2026) |
| Build | CMake with 6 options, preprocessed by `switch.pl` (Perl) | CMake with 19 options, without Perl |
| Compiler warnings (strict set) | not measurable | 1280 with gfortran 16.2; the 281 that can hide a defect all have a verdict |

There is no `COMMON` block and no unnamed shared state left. Three modules
hold run state, each with an explicit owner. There is no general run object
(see [run-state-ownership.md](run-state-ownership.md)), and `IPTST`, `TESTFL`
and `PROPSL` are still per thread. But run state and thread state are kept
apart, computational kernels get the smallest sub-context they need, and no
kernel asks for the thread number itself. The architecture is described in
[modern-fortran.md](modern-fortran.md).

## 2. What has been proven

**Output is the same as upstream, within the print precision.** Each example
case compares its numerical output field by field with a reference under
version control, with a relative tolerance of 1e-3 for the rounding of the
printed numbers. Where a fix deliberately changes the result (BF-15, BF-16,
BF-34), this is recorded per deck in [upstream-delta.json](upstream-delta.json).
Where a build variant really changes the result — `JAC` and `FFRO` — that
variant has its own reference instead of a wider tolerance.

**The compiler checks what it can check.** `IMPLICIT NONE (TYPE,
EXTERNAL)` is used everywhere. Upstream 41.51 gives 3573 implicit interfaces
in the strict build; the fork gives none. The calls to the external METIS
library go through a checked `BIND(C)` interface.

**Warnings that can hide a defect are reviewed.** The strict build gives
1280 warnings. The 281 of the kinds that can point to a real defect (unset
values, index bounds, skipped calls, truncated text, loss of precision) all
have a recorded verdict, and the strict gate fails on any new one without a
verdict. The other kinds are counted but not checked; see
[strict-warnings.md](strict-warnings.md).

**SWAN can be reused within one process.** The same case runs twice
bit-identically, also with changing physics, a changing computational grid or
changing spectrum dimensions in between, and the run in between demonstrably
differs.

**The physics is not only pinned but also tested.** Shoaling against Green,
refraction against Snell, grid convergence against first-order theory — with
the pass criterion fixed in advance.

**Memory is freed.** In upstream 41.51 the linked lists of the parser leak
2040 bytes cumulatively over a series of three decks (A–B–A); in the fork
this is zero, measured with Valgrind on the same decks (BF-05).

**The difference from operational practice has been explained.** With the
operational so-rp deck, which chooses the source terms itself
(`GEN3 KOMEN`), 41.51 gives a 3.2 per cent higher wave height on the
reference condition than RWS 41.31A.1, the Rijkswaterstaat production build
of 41.31 (1.653 against 1.602 m). That comes from the default for the wind
drag, which the deck does not set itself; with `DRAG FIT` the mean difference
disappears. It does not come from the fork. The rest lies in the triads, whose
LTA was reformulated: a deck that sets everything it can comes to within 2 cm
of 41.31, and a test build with the old form of the last two parts reproduces
RWS 41.31A.1 to within 0.1 mm. The operational deck itself reads
the grid in wrongly and places the boundary on that grid; the repo computes
with a corrected template (see `so-rp_swan/par/operational/PROVENANCE.md`).

**The stop message does not prove convergence.** On the quad examples a run
reports "100%" while Hs still shifts by 27%. The check requires two long
references and takes their mutual Hs shift per point into account before an
operational run is judged; for the operational so-rp deck this measurement
has not been done yet. The balance residual is an addition of the fork and is
off by default: `RESIDUAL 0` only measures, a positive value adds a stop
condition. Switched off, it is not computed at all; switched on, it costs
2.5% on one core and 3.0% on four threads. No general operational residual threshold has been set
([balance-residual-design.md](balance-residual-design.md)).

**Speed against upstream.** On one core the fork is 1.78 times as fast as
upstream with upstream's own default build, and 1.25 times as fast when both
are built at `-O3`. On four OpenMP threads these factors are 1.86 and 1.28.
Thread counts, pinning and cases are in
[benchmarks/speed-vs-upstream.md](../benchmarks/speed-vs-upstream.md).

## 3. Fixes on top of the upstream source base

This fork fixes a number of real defects that are also in upstream
41.51. They are listed, each with its strength of evidence, in
[bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md) — proven,
latent, or diagnostics — so that they can be handed back to TU Delft.

The largest, by consequence for the output: `BREAKING ASYM` destroys the
wave field (BF-34); without breaking, the limiter is off everywhere, so runs
without breaking take a different path and `QUADRUPL 1` never converges
(BF-16); an MPI run leaves a row of active points out of the field (BF-40); a
missing stencil edge in the unstructured front decomposition makes the result
depend on the number of threads and makes the serial sweep order inconsistent
(BF-15); a scratch variable that all threads share makes an unstructured run
with wind differ from run to run under OpenMP (BF-17); and `NUMERIC ... FIL`
silently leaves out the frequency shift (BF-41).
In addition, upstream SWAN crashes or hangs on many kinds of wrong input: a
missing bottom, a grid without meshes, broken boundary, hotstart and grid
files and the like. With BF-45 to BF-53, the fork stops in those cases with a
message and an exit code that is not 0.

## 4. What is claimed

GNU Fortran 16.2 on Linux x86-64, Release, with the six required gates green
on the same commit: serial, OpenMP, runtime checks, strict, MPI on two ranks,
and the manifest and Python checks. This includes an installation test with a
case that runs from the installation. This was shown locally on
28 October 2026; CI with gfortran 16.2 has not run yet. MPI and netCDF also
build with 16.2: the `mpi.mod` and `netcdf.mod` from the system packages were
made with a different gfortran release, but have the same module version.

## 5. What is not claimed

This is the most important section of this document.

- **Zero warnings.** There are 1280 (gfortran 16.2, 28 October 2026).
  The categories with possibly serious consequences (uninitialised values,
  index bounds, skipped functions, truncated text, loss of value in
  conversions, and warnings without a flag; 281 together) have been assessed
  one by one, which produced BF-19 to BF-33; no suspected defect remains
  open. The other 999, mainly conversions to single precision, comparisons of
  real numbers and unused declarations, are counted but not assessed; cleaning them
  up touches inherited code without demonstrable gain.
- **The Debug route as a gate.** It runs only locally, not in CI. With
  gfortran 16.2 a Debug build (`-O0`) passes all 60 tests. Two things make `-O0` and `-O3` round
  differently. First, for a constant integer exponent above four, such as
  `SF**5` in the JONSWAP shape `SSHAPE`, GCC at `-O1` and higher multiplies in
  its own order, (x·x²)·x², while `-O0` calls libgcc's `__powisf2`, which
  computes x·(x²·x²); the same holds for `x**2.` with a real exponent. The
  fork writes these products out in the optimiser's order
  (`src/swan_integer_powers.f90`, `x*x`), so they are the same at every
  level. Second, `-O3` vectorises loops with a more generous cost model
  (`-fvect-cost-model=dynamic`) than `-O2` (`very-cheap`), and the vectorised
  maths functions from libmvec round one ulp differently in a few per cent of
  cases. That shifts a few output files of the friction decks and ST6 in the
  last digits. The references stay at `-O3`, the default of both the fork and
  the comparison with upstream.
- **A second compiler.** Intel, Flang and NAG have not been tried; their
  absence in this environment has been verified, their behaviour has not.
- **Native Windows, IBM, Fujitsu, Cray.** The branches have been kept and
  compile where that is possible; they have not been tried.
- **The full clean build matrix of all 28 configurations on one candidate.**
  The registration counts were measured per configuration; the combined
  clean build was not.
- **The MPI field contract on land.** With BF-40, the wet field of the MPI
  assembly is bit-identical to serial, on 2 and 4 ranks. Quantities that SWAN
  also fills in at inactive points (such as `WIND`) are at the exception
  value in MPI in land rows outside every subdomain.
- **That the abort on a deliberately malformed deck has been explained.** The
  most likely mechanism, a `CURVE` that allocates memory until it runs out,
  is bounded (BF-44), and that deck does not abort in 80 runs with varying
  heap settings. The abort could not be reproduced on demand, so there is no
  conclusive proof that this was the cause.
- **Simultaneous or overlapping use within one process.** Serial reuse has
  been proven; running several runs at the same time in a thread-safe way
  has not been investigated.
- **One bundle for the two test point flags.** The flags are per point and
  change inside the loop, while the bundle is bound per loop, so they stay
  separate. That is a property of the design, not an open issue.

A research trial that is known to fail does not qualify its route. Where
something has not been tried, the text says "not tried", not "supported".

## 6. How to check it yourself

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=ON \
      -DCMAKE_Fortran_COMPILER=gfortran-16
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

With gfortran 16.2, the reference compiler and the one CI uses, this should
give 60 out of 60 green from a fresh `git clone`, without preparation and
without local data. Choose the compiler explicitly, as above: another
gfortran version can shift the stored references by a fraction of a
millimetre. A build directory that, after a compiler change, only recompiles
the changed files mixes versions; `readelf -p .comment` on the binary shows
this. The other gates are in `.github/workflows/ci.yml`; the strict gate runs
with `FC=gfortran-16 python3 scripts/strict_diagnostics.py`.

Which gates a change can break is decided by
`python3 scripts/select_gates.py`. In CI all gates and all build variants
always run; the upstream delta gate and the Debug route only run locally.

## 7. Maintenance and upstream

`upstream` remains set to TU Delft, but a git merge does not work: the fork
has converted every source file to free form and split them over modules,
while TU Delft delivers new versions as patches on the `.ftn` files. A new
upstream release is therefore brought in by hand: work out the difference
between the old and the new upstream version, carry each change over into the
corresponding routine of the fork, and then pin the upstream delta gate to the
new release. That gate shows that every deck gives the same result as
upstream, apart from the known BF differences; the regression suite shows that
nothing else shifted. If either turns red, that is a finding, not a reason to
adjust the gate or the suite.

The supported combinations are in [support-matrix.md](support-matrix.md), the
architecture in [modern-fortran.md](modern-fortran.md), and the working
agreements in [AGENTS.md](../AGENTS.md).
