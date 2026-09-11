# Run-state ownership

This document describes who clears the run state at the end of a run. The
machine-readable contract is in
[`run-state-ownership.json`](run-state-ownership.json), and
[`check_run_state_ownership.py`](../scripts/check_run_state_ownership.py)
checks it against `SWCLME`.

Upstream `SWCLME` releases the run state piece by piece. The contract covers
60 separate `DEALLOCATE` actions, five list `DELETE` actions and five matching
latch resets. In the fork each of them belongs to an idempotent owner `clear`:
obstacles, boundary lists, output lists, the general arrays from `M_GENARR`,
the input fields, the vegetation layers and the global grid, the parallel
storage, the unstructured grid and computational data, and the
IEM/Bragg/QCM state. The arrays sit with the module that holds them:
`fb/fbdxy` in `SwanBraggScat`, `kx…disbk1` in `SwanQCM` and
`iss/iwt/itt/freq/E0/Ebig` in `SwanIEM`.

Temporary state outside `SWCLME` (`kvertc/kvertf`, `IWEIG`,
`botspc/dpmean`) keeps its own allocate/deallocate pair and is left out of
this contract on purpose. `SINBAC`/`SAVE_SINBAC` are also left out: an ESMF
coupler allocates them and owns them. SWAN leaves them in place after a run,
as upstream does. `SWCLME` contains no separate `DEALLOCATE`, list `DELETE`
or latch actions; it contains only owner-level `clear` calls (20 of them).

In the manifest, each symbol is marked with how it is cleared: `deallocate`,
`delete` or `owner_call`. The check fails when a separate action disappears
without a registered owner call, when a new symbol appears in `SWCLME`, or
when two owners claim the same symbol.

The table is deliberately not a general `swan_run_t`: each module stays the
owner of its own lifetime. Only the point at which state is cleared is
explicit and idempotent. Moving an array that a hot loop uses into a type
component or behind an alias can make the loop slower, so such a change needs
a speed measurement, serial and multicore, before it goes in.
