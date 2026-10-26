# Design: a balance residual for the stationary SWAN solution

## In short

The balance residual is an optional research tool of this fork. It is not an
upstream setting. Without `RESIDUAL` on the `NUMERIC` command, nothing is
computed: no accumulators are created, no bins are looped over and no
residual sums are combined across MPI processes. `RESIDUAL 0` computes and
reports the measure without changing the decision to stop; a positive value
adds a stopping threshold. A negative value is rejected. This holds for both
the regular and the unstructured route.

Operational decks get no `RESIDUAL` and no new numerical default.
`ACCUR` and `STOPC` are both supported upstream. Existing upstream settings
are first tested against long references. `MXITST` is only an upper limit:
raising it does not prevent stopping early.

A low average energy measure can hide spectral and local deviations. The
integrated measure can also be zero while single bins are not, because
contributions with opposite signs cancel out.

## Why

SWAN's stationary stopping criterion measures the *change* in Hs and Tm
between iterations. That measures "is it still moving", not "is it right".
The two part ways as soon as something makes the change small without
reaching the solution. Under-relaxation does that on purpose, the action
limiter does it as a side effect, and a limit cycle does it by itself.

This matters in three places:

- A speed-up that is not byte-identical cannot be shown to give the same
  solution if the only measure is the change between iterations.
- A run that keeps oscillating can be a limit cycle or slow convergence. With
  only the change, that can be seen from hundreds of iterations, not from one
  number.
- Two runs that both meet the 98% criterion can still differ clearly in Hs
  across the whole field.

Without a second, independent measure, these cases cannot be told apart.
This is not a shortcoming of this fork; upstream 41.51 is the same.

## What it should be

The residual of the discrete action balance, evaluated on the current field,
without updating that field:

```
r(x, sigma, theta) = d(c_x N)/dx + d(c_y N)/dy + d(c_sigma N)/dsigma
                   + d(c_theta N)/dtheta  -  S_tot / sigma
```

What matters is that it is *discrete*, not continuous: the residual must be
formed with the same discretisation that the solver uses. Otherwise it
measures the discretisation error instead of the convergence error. For the
stationary regular route that is the SORDUP/S&L scheme; for the unstructured
route it is that route's own stencil.

That makes the implementation simpler than it looks. For each grid point in
each sweep, SWAN already sets up a local system in `IMATDA` and `IMATRA`. The
residual is what is left of that system when the current `AC2` is put in,
instead of solving for it.

## Approach

1. **Use the local system that SWAN already builds.** Build `IMATDA`/`IMATRA`
   as `SOURCE` and the propagation routines do, but put in the current `AC2`
   instead of solving. The difference is the residual per bin.
2. **Scale per point.** A raw residual cannot be compared between deep and
   shallow water. Divide by a local scale. The sum of the absolute values of
   the separate terms is the usual choice, because it makes the residual
   dimensionless and bounded by 1.
3. **Report the result per iteration**, next to the existing accuracy
   percentage, in the PRINT file, in the same form as the existing
   `accuracy OK` line, so that existing parsers do not trip over it.
4. **Add a stopping criterion; do not replace one.** A new keyword on
   `NUMERIC` sets a residual threshold. If that keyword is left out, the
   behaviour does not change at all, so the verification matrix stays valid.

## The pitfalls

**The limiter and the energy cap do not belong in it.** `PHILIM` and the cap in
`SINTGRL` limit the *update*. The residual must measure the raw balance.
Otherwise it once again measures how hard the iteration is being held back,
instead of how far it is from the solution.

**The sweep sector.** A sweep works on part of the directions. The residual
must cover the full circle, so it is summed over the four sweeps.

**Dry points and the exception value.** Points with `DEP2 <= DEPMIN` do not
count, as with the existing criterion. The `-9` exception value in the output
fields must never end up in an average. This is the same pitfall as with
averaging over wet points in `AGENTS.md`.

**Cost.** A separate diagnostic pass through the grid would cost roughly one
extra iteration each time the residual is computed, and more if it computed
the source terms again. Reusing the system of the sweep avoids both.

**Verification.** The residual should go to zero on a run that demonstrably
converges (`quad_dia2` reaches 100%), and should *not* go to zero on a run that
stops too early (`quad_dia1` with a large `ALFA`). A residual that behaves the
same on both is wrong.

## What was built

The residual is in [`src/swan_balance_residual.f90`](../src/swan_balance_residual.f90).
It is computed only with `NUMERIC ... RESIDUAL`, on both the regular and the
unstructured grid. Each iteration, PRINT shows the following line next to the
`accuracy OK` line, and a stationary run also shows it on screen:

```
 balance residual energy rms  1.74E-02, max  3.62E-02, spectral rms  6.49E-02 in        41 wet grid points
```

### No separate pass

For each point in each sweep, SWAN already sets up the local system; the
residual is that system with the current `AC2` put in, just before `SOLPRE`.
Upstream points then already carry their update from the current sweep, so
this is the residual of the non-linear Gauss-Seidel iteration that SWAN
carries out. Per bin, it is zero exactly when an iteration leaves the field
unchanged without help from relaxation or the limiter. Two pitfalls drop out
by themselves: `SOLPRE` adds `ALFA` to both sides, which cancels out for the
current `AC2`, and `PHILIM` and `RESCALE` only come after the solve. The
source terms are not computed again.

### Two measures

Per bin, the residual does not go to zero on `quad_dia2`. At the test point
of that deck, the individual bins in the energetic part of the spectrum move
by 5 to 15% from one iteration to the next, even after the "100%" message and
even after 300 iterations. Meanwhile Hs stands still, because these movements
cancel out in the integral. A per-bin measure therefore says something true
(this spectrum never settles at the level of single bins), but it is useless
as a stopping criterion. So there are two:

- *spectral*: the sum of |d| per bin, divided by the sum of the absolute
  terms. The strict measure.
- *energy*: the sum of σ·d **with its sign** over the spectrum, divided by the
  sum of σ times the absolute terms. That is the residual of the energy
  balance of the whole spectrum. Noise between bins drops out, just as it
  cancels in Hs; a spectrum that is still gaining or losing energy does not.
  This measure measures what Hs is made of, and the threshold acts on it.

Both are summed per point over the four sweeps, and both are dimensionless and
bounded by 1. The line gives the rms over the wet points that the process
owns, and for *energy* also the maximum. Under MPI the sums are combined in
double precision (`SWREDUCE` for `real(8)`), as in the serial route.

### What it shows

`examples/nonlinear_interactions/quad`, Hs at 25 km:

| run | iterations | accuracy | energy rms | Hs |
|---|---:|---:|---:|---:|
| `quad_dia2`, reports converged | 28 | 100% | 0.0058 | 1.9072 m |
| `quad_dia1` `ALFA=0.01`, reports converged | 40 | 100% | **0.145** | **1.4032 m** |
| `quad_dia1` `ALFA=0.01`, 2000 iterations | 2000 | – | 0.0002 | 1.9232 m |
| `quad_dia1` `ALFA=0.05`, false stop | 10 | 100% | 0.160 | 1.2783 m |
| `quad_dia1` `ALFA=0` | 28 | 100% | 0.010 | 1.9069 m |

Two things stand out. The false stop can be recognised without a second run:
0.16 against 0.006 to 0.010 for a run that really converges. And the
`ALFA=0.01` run was still at 0.145 when it reported convergence, and was
52 cm below the value it reaches after about 1500 iterations. With
under-relaxation, the noise between bins dies away and the residual drops to
machine precision; without it, a floor remains that differs from deck to deck.

Because of that floor, the residual does not replace a second run
everywhere. It shows a false stop like the one above directly, but only a
second, longer run tells how much Hs still shifts.

### Stopping criterion

```
NUMERIC STOPC ... RESIDUAL [rtol]
```

`RESIDUAL` follows the `STOPC`/`ACCUR` part. The iteration then stops only
when the existing criterion is met **and** the *energy* rms is below `[rtol]`.
Without the keyword nothing changes: the numerical output of all 215 files
from the test suite is bit-identical to a build without the residual, serial
and with OpenMP, and across 1, 2, 4 and 8 threads.

A threshold must lie above the floor of the deck, otherwise the run always
iterates up to `MXITST`. As a safety net against false convergence, 0.03 is
enough on the decks above; anyone who wants the residual really to go to zero
needs under-relaxation and a generous `MXITST`.

### Schemes in frequency space

On the SIP and SOLMT1 paths (a current or a time-dependent depth, with a
scheme in frequency space) the frequencies are coupled. The residual follows
the system that the solver solves there too. For SIP (`SIGIMPL`, implicit in
frequency), each bin gets the extra terms `IMAT5L·N(σ−1)` and
`IMAT6U·N(σ+1)`, up to and including `ISSTOP`, in the same way that `SWSIP`
builds the system. For SOLMT1 (`SIGEXPL`, explicit in frequency), the
frequency terms are already in the right-hand side; there, the bins that an
opposing current blocks stay out of the defect, because `SOLMT1` replaces
their row with an identity row. On the smoke deck with a current from 0 to
2 m/s, the residual on both paths drops within a few iterations from 0.2 to
0.01–0.05, a floor about ten times higher than without a current (0.002).
This also fits the picture that the floor differs from deck to deck. The
solution stays byte-identical with and without the measurement
(`tests/test_balance_residual.py`).

The third scheme, `FIL`, is not implemented upstream and is rejected (BF-41);
for it, the residual gives "not available".

### The inner loop

`accumulate_balance_defect` has its own loop over the directions of one
frequency for each kind of system. The explicit system computes only the
diagonal. The direction-coupled system (the case for Voordelta and for every
deck without a current that has refraction or frequency shifting) handles the
first and last bin, which have only one neighbour, outside the loop, so the
loop itself does not have to check for each bin whether there is a
neighbour. SIP and SOLMT1 have their own loop with the frequency coupling and
the blocked bins. Each variant adds up in the same order and only leaves out
terms that are zero, so all variants give the same numbers as one general
loop would.

False sharing on the accumulators does not occur: a thread owns whole rows,
and the point numbering runs on within a row.

### Cost

With the residual switched off, the call sits behind
`balance_residual%enabled` and nothing else runs. The switched-off path
costs nothing measurable, serial or on several cores.

With the residual switched on (`NUMERIC STOPC RESIDUAL 0`), measured on
Voordelta regular, seven alternating rounds per configuration; the output is
byte-identical with the residual on and off:

| configuration | residual off | residual on | cost of the residual |
|---|---:|---:|---:|
| serial, one thread on CPU 0 | 22.91 s | 23.49 s | 2.5% |
| OpenMP, 4 threads on CPU 0,2,4,6 (`OMP_PROC_BIND=close`, `OMP_PLACES=cores`) | 7.26 s | 7.35 s | 3.0% |

Measured with gfortran 16.2 on an Intel Core Ultra 9 185H;
`benchmarks/residual_cost.py` repeats the measurement.

The residual is off by default. Computing it on every operational run costs
the time above, against being able to see without a second run that a run
stops too early; for research runs that trade is worth it, for production
runs it is not.

### A hotstart does not pick up exactly where the run left off

When a run is continued from a `HOTFILE`, the residual jumps up in the first
iterations after the restart, but that jump sits only in the boundary points.
The hotfile (`UNF`) stores and reads the spectrum exactly. What happens at
every `COMPUTE`, upstream too, is that `SNEXTI` imposes the full boundary
spectrum again in the points with a boundary condition, including the
directions that leave the domain. The previous run had computed those
itself. For those directions the inner domain lies upstream of the boundary,
so the inner field does not notice. On a small deck with the same physics and
numerics, the inner field after a restart is bit-for-bit equal to that of a
run that went straight through. Only the boundary points differ, and with the
`AC2` from before the restart put back into them, the residual is equal too.
A comparison of inner points across a restart is therefore exact; only the
residual and the accuracy, which cover all points, feel the jolt. This does
not hold for the rounded `FREE` format: that one also changes the inner
field. Test `hotstart_continuation`.
