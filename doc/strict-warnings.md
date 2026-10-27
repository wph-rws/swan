# Warnings from the strict build

The strict build compiles SWAN with every extra warning gfortran offers:
`-Wall -Wextra -Wimplicit-interface -Wsurprising -Wconversion-extra
-Wcharacter-truncation`, Release, the default serial variant, with the
reference compiler (gfortran 16.2, see `AGENTS.md`). It gives 1280 warnings.
Most of them say nothing about whether SWAN computes correctly. A few kinds
do, and those are the ones the strict gate checks.

## Which warnings are checked

Some kinds of warning can point to a real defect: a value that may be used
before it is set, an index that may fall outside an array, a function call
the compiler may skip, text that is cut off, a conversion that loses
precision, and the few warnings gfortran gives without naming a flag. Every
defect the strict build has found came from these kinds. Each warning of
these kinds has been looked at, and the verdict is recorded in
`scripts/strict_diagnostics_triage.json`.

The other kinds are only counted. They are mostly conversions to single
precision, comparisons of real numbers and unused declarations, and nearly
all of them sit in code inherited from TU Delft. None has ever led to a
defect, and cleaning them up would touch a lot of that code for no gain.

| Kind of warning | Count | Checked | What the review found |
|---|---:|---|---|
| `maybe-uninitialized` | 108 | yes | defects BF-19, BF-20, BF-21, BF-22, BF-23, BF-27, BF-28, BF-29, BF-30, BF-31, BF-33; the rest harmless |
| `conversion` | 72 | yes | harmless: intended rounding to single precision, or integer expressions that convert exactly |
| `function-elimination` | 49 | yes | defect BF-24; one in `CVMESH`, where the call is made only for a point not found yet; the rest harmless |
| `character-truncation` | 24 | yes | defects BF-26 and BF-32; the rest harmless or deliberate |
| `do-subscript` | 24 | yes | harmless; the edge case behind them was defect BF-25 |
| `untagged` | 4 | yes | deliberate: FFTW transforms in place |
| `conversion-extra` | 495 | counted only | |
| `compare-reals` | 225 | counted only | |
| `unused-variable` | 216 | counted only | |
| `unused-dummy-argument` | 61 | counted only | |
| `unused-parameter` | 2 | counted only | |

The defects themselves are described in
[bug-fixes-vs-tu-delft-41.51.md](bug-fixes-vs-tu-delft-41.51.md). The
pattern behind most of them: a chain of choices without a final `ELSE`,
followed by an error message that does not stop the input processing, after
which the code carries on with a value that was never set.

## How the gate works

`scripts/strict_diagnostics.py` builds SWAN from scratch with the strict
flags and reads every warning from the build log. A warning is identified by
its kind, file, routine and message, not by its line number: moving code
within a routine changes nothing, while a warning that moves to another
routine counts as new. A verdict covers a stated number of messages; if the
build gives more, the extra ones count as unreviewed.

The gate fails when

- a checked warning has no verdict;
- the build did not use the strict flags (for example `-w` added through
  `FC`, which would make every warning disappear and look like a clean
  result);
- the log contains a warning the script cannot read, or one it cannot trace
  to a routine;
- the build itself fails.

It does not fail when warnings disappear. The script then says which
verdicts no longer match anything; `--prune` removes them.

```
python3 scripts/strict_diagnostics.py               # build, then check
python3 scripts/strict_diagnostics.py --list        # also show every checked warning
python3 scripts/strict_diagnostics.py --log FILE    # check an existing build log
python3 scripts/strict_diagnostics.py --prune       # drop verdicts that no longer match
```

Choose the compiler with `FC=/usr/bin/gfortran-16`. A different compiler
version reports somewhat different warnings, even in unchanged code: the
same case can show up under another routine because the compiler inlines
differently. After a compiler change, the new checked warnings have to be
reviewed before the gate is green again; that is a few dozen at most, not a
recount of all 1280.

`tests/test_strict_diagnostics.py` tests the gate itself, including that a
missing verdict and a silenced build both fail.
`tests/test_strict_diagnostics_triage.py` checks that every recorded verdict
is about a checked kind of warning and gives a reason.

**What the gate does not see:** only the default serial variant is built.
Code behind `!MPI`, OpenMP or other build options is not compiled, so its
warnings are not checked.

## Reviewing a new warning

1. Read the path in the source, not just the reported line. Look for the
   branch in which the value is not set, the index that falls outside the
   array, the call that is skipped.
2. Ask whether a user can reach that path with input they can give. If so,
   make a small deck (usually a variant of `examples/quick_test`) and run it
   with the fork, with `-DSWAN_RUNTIME_CHECKS=ON`, and with the pinned
   upstream build in `.upstream/`.
3. If it is a defect, fix the cause. Do not set a value to zero just to make
   the warning go away: that hides the next real one. A default is only
   allowed if it is the documented default of the command.
4. Add a test that fails before the fix and passes after it, usually in
   `tests/test_input_validation.py`, and describe the defect in the bug-fix
   list.
5. Record the verdict: `defect` with the BF number, `harmless` with the
   reason the reported path is safe, `deliberate` with the reason and the
   conditions, or `suspected-defect` while it is still open. Where the
   compiler cannot see through a fix, the warning stays, with the verdict
   `defect`.

"It only affects test output" is not a reason to call a read of an unset
value harmless. The question is whether an unset value is read, not whether
the model results change.
