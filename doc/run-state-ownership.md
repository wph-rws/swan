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

## Command state between runs

`SWMAIN` resets the command and boundary parsers before `SWINIT`. The reset
clears the command-presence flags, include stack, remembered wind choice,
whitecapping and swell latches, and the flag that the boundary output list has
been built. These values must persist across successive COMPUTE commands in
one deck, so resetting them at every entry of SWREAD would be wrong. The
command reader owns its repetition count and resets it with the rest of its
parse state.

Three more decisions are taken once per run and reset there in the same way.
`SANDL` gives its error for a CFL number above 10 with the higher-order
scheme only once; the flag now lives in `swan_propagation` and
`RESET_PROPAGATION_STATE` clears it. The error has level 2, so with the
default `[maxerr]` it stops the run; before the reset, a second run in the
same process skipped it and computed on. `MSGERR` opens its error message file
`Errfile` once; `SWMAIN` closes that unit at the end of a run, so
`RESET_ERROR_FILE_STATE` lets the next run open it again, where before its
errors went to a file `fort.17`. `BREAKING BKD` varies its breaker index until
the iteration is accurate enough and then switches that off (`MODGAM`);
`SWINIT` switches it on again, where before a second run kept the constant
index. Within one run all three keep upstream behaviour across COMPUTE
commands.

The reuse test compares both B and A in B–A–B–A. It also repeats decks with
explicit whitecapping, boundary output and wind input before bottom. A–B–A
alone cannot show that B inherited A's source-term choice. It then repeats a
nonstationary deck whose time step is too large for the higher-order scheme
three times, and a `BREAKING BKD` deck twice; each must give the same result
every time.

The other saved locals were reviewed. Output format flags are assigned from
the filename on every call; MATLAB record counters start again when each file
opens. Boundary interpolation remembers its first point only within a file
and replaces it on the first spectrum. The WW3 record counter resets when a
new boundary file starts. HSRR is assigned at entry. FAC4WW's cache checks all
of its inputs and the presence of the run-owned cached arrays, so a new run
rebuilds it. Fixed tables, format strings and trace counters carry no deck
physics. The VTK writer keeps a flag that gives its warning about an empty
subdomain of an output frame only once in a nonstationary run. It is a level-1
warning: it does not stop the run or change any field, but once a
nonstationary run has given it, a later run in the same process does not.
