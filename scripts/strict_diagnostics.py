#!/usr/bin/env python3
"""Build SWAN with the strict warning set and check the warnings that can hide a defect.

Only a few kinds of compiler warning have ever pointed to a real defect in
SWAN: a value that may be used before it is set, an index that may fall
outside an array, a function call the compiler may skip, text that is cut
off, and a conversion that loses precision, plus the few warnings gfortran
gives without naming a flag. Every warning of those kinds needs a verdict in
`strict_diagnostics_triage.json`: someone looked at it and wrote down whether
it is a defect, harmless, or deliberate, and why. A warning of those kinds
without a verdict fails the gate.

All other warnings, mostly conversions to single precision, comparisons of
real numbers and unused declarations, are counted and printed. They do not
fail anything: none of them has ever led to a defect, and reviewing them
would mean rewriting a lot of inherited code for no gain.

A warning is identified by its category, file, routine and message, not by
its line number. Moving code within a routine therefore changes nothing,
while a warning that moves to another routine counts as new. A verdict
covers a stated number of messages; if the build gives more, the extra ones
count as unreviewed.

The gate also checks that the build really used the strict flags. A build
with `-w` added through FC, for example, gives no warnings at all, and that
must not look like a clean result.

Usage:
    scripts/strict_diagnostics.py                build from scratch, then check
    scripts/strict_diagnostics.py --log FILE     check an existing build log
    scripts/strict_diagnostics.py --list         also list every checked warning
                                                 with its location and verdict
    scripts/strict_diagnostics.py --prune        drop verdicts, or lower their
                                                 count, for warnings the build
                                                 no longer gives

Choose the compiler with FC (see AGENTS.md); `--build-dir` sets the build
directory, which `--log` also uses to find the compile commands.
"""

from __future__ import annotations

import argparse
import collections
import dataclasses
import json
import os
import re
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

STRICT_FLAGS = (
    "-Wall -Wextra -Wimplicit-interface -Wsurprising "
    "-Wconversion-extra -Wcharacter-truncation"
)
BUILD_TYPE = "Release"
GENERATOR = "Unix Makefiles"
#  Build options besides flags and build type. Empty means the default serial build;
#  so the strict gate says nothing about code behind MPI, OpenMP or other
#  switch variants.
BUILD_OPTIONS: tuple[str, ...] = ()
ROOT = Path(__file__).resolve().parent.parent
TRIAGE_FILE = Path(__file__).resolve().parent / "strict_diagnostics_triage.json"
#  gfortran gives a few warnings without naming a flag; they get this category.
UNTAGGED = "untagged"
#  The kinds of warning that can hide a real defect. Each one needs a verdict.
CHECKED = frozenset({
    "maybe-uninitialized", "uninitialized", "do-subscript", "array-bounds",
    "function-elimination", "character-truncation", "conversion", UNTAGGED,
})

# gfortran reports a warning as a block -- location line, source line, caret,
# message -- or, when it cannot show a source line, on a single line.
LOCATION = re.compile(r"^(\S*?\.[fF]90):(\d+):\d+:$")
MESSAGE = re.compile(r"^Warning: (.*?)(?: \[-W([a-z0-9-]+)\])?$")
INLINE = re.compile(r"^(\S+?):(?:(\d+):\d+:)? Warning: (.*?)(?: \[-W([a-z0-9-]+)\])?$")
ANY_WARNING = re.compile(r"\bWarning: ")


@dataclasses.dataclass(frozen=True)
class Occurrence:
    category: str
    path: str
    line: int
    message: str

    @property
    def file(self) -> str:
        return Path(self.path).name


def build(source: Path, build_dir: Path) -> str:
    """One fully successful clean build; keep the configure and build logs.

    No retry and no merged logs from several attempts: a second
    attempt in the same directory is incremental and hides warnings from
    files that were already compiled. On failure the gate blocks.
    """
    if build_dir.exists():
        shutil.rmtree(build_dir)
    build_dir.mkdir(parents=True)
    configure_log = build_dir / "strict-configure.log"
    build_log = build_dir / "strict-build.log"
    #  Fixed locale: the parser recognises English messages.
    env = dict(os.environ, LC_ALL="C.UTF-8", LANG="C.UTF-8")
    configured = subprocess.run(
        ["cmake", "-S", str(source), "-B", str(build_dir), "-G", GENERATOR,
         f"-DCMAKE_BUILD_TYPE={BUILD_TYPE}", f"-DCMAKE_Fortran_FLAGS={STRICT_FLAGS}",
         "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON", *BUILD_OPTIONS],
        capture_output=True, text=True, env=env,
    )
    configure_log.write_text(configured.stdout + configured.stderr)
    if configured.returncode != 0:
        raise SystemExit(
            f"strict configure failed (see {configure_log}):\n"
            f"{(configured.stdout + configured.stderr)[-4000:]}"
        )
    finished = subprocess.run(
        ["make", "-C", str(build_dir), f"-j{len(os.sched_getaffinity(0))}",
         "--output-sync=target"],
        capture_output=True, text=True, env=env,
    )
    build_log.write_text(finished.stdout + finished.stderr)
    if finished.returncode != 0:
        raise SystemExit(
            f"strict build failed (see {configure_log} and {build_log}):\n"
            f"{(finished.stdout + finished.stderr)[-4000:]}"
        )
    return finished.stdout + finished.stderr


def parse(log: str) -> list[Occurrence]:
    """Every warning in the log, with category, location and message.

    Fails if the number of `Warning: ` lines does not equal the number of
    recognised warnings: then the compiler uses a form this parser does
    not know, and the comparison would silently cover only a subset.
    """
    found = []
    path, line = None, 0
    for raw in log.splitlines():
        stripped = raw.strip()
        location = LOCATION.match(stripped)
        if location:
            path, line = location.group(1), int(location.group(2))
            continue
        message = MESSAGE.match(stripped)
        if message:
            if path is None:
                raise ValueError(f"warning without location: {stripped}")
            found.append(Occurrence(message.group(2) or UNTAGGED, path, line,
                                    message.group(1)))
            path = None
            continue
        inline = INLINE.match(stripped)
        if inline:
            found.append(Occurrence(inline.group(4) or UNTAGGED, inline.group(1),
                                    int(inline.group(2) or 0), inline.group(3)))
    expected = len(ANY_WARNING.findall(log))
    if expected != len(found):
        raise ValueError(f"{expected - len(found)} warnings not recognised "
                         f"by the parser ({len(found)} of {expected})")
    return found


# Start and end of program units and procedures in free-form Fortran.
_PREFIX = r"(?:(?:pure|impure|elemental|recursive|non_recursive|module)\s+)*"
_TYPE = (r"(?:(?:integer|real|logical|complex|character|double\s+precision|"
         r"type|class)\b[^!'\"]*?\s+)?")
_PROC = re.compile(rf"^\s*{_PREFIX}{_TYPE}{_PREFIX}(subroutine|function)\s+(\w+)", re.I)
_UNIT = re.compile(r"^\s*(module|submodule\s*\([^)]*\)|program)\s+(\w+)\s*(?:!.*)?$", re.I)
_END = re.compile(r"^\s*end\s*(subroutine|function|module|submodule|program)\b", re.I)
_IFACE = re.compile(r"^\s*(?:abstract\s+)?interface\b", re.I)
_END_IFACE = re.compile(r"^\s*end\s*interface\b", re.I)


def scopes(source: str) -> list[str]:
    """The enclosing routine for each line (index 0 = line 1).

    Outside every procedure this is the name of the module or program.
    Interface blocks do not count: their procedure headings are declarations,
    not a place where code lives.
    """
    result: list[str] = []
    stack: list[str] = []
    interface_depth = 0
    for text in source.splitlines():
        if interface_depth:
            if _END_IFACE.match(text):
                interface_depth -= 1
            elif _IFACE.match(text):
                interface_depth += 1
        elif _IFACE.match(text):
            interface_depth += 1
        elif _END.match(text):
            result.append(stack[-1] if stack else "-")
            if stack:
                stack.pop()
            continue
        else:
            match = _PROC.match(text) or _UNIT.match(text)
            if match:
                stack.append(match.group(2).lower())
        result.append(stack[-1] if stack else "-")
    return result


class RoutineResolver:
    """Looks up the enclosing routine for a location in the source."""

    def __init__(self, root: Path = ROOT):
        self.root = root
        self.cache: dict[str, list[str] | None] = {}

    def __call__(self, occurrence: Occurrence) -> str:
        if occurrence.path not in self.cache:
            path = Path(occurrence.path)
            if not path.is_absolute():
                path = self.root / path
            self.cache[occurrence.path] = (
                scopes(path.read_text(errors="replace")) if path.is_file() else None)
        table = self.cache[occurrence.path]
        if table is None or not 1 <= occurrence.line <= len(table):
            return "?"
        return table[occurrence.line - 1]


def fingerprint(occurrences: list[Occurrence], resolve=None) -> collections.Counter:
    """Count the warnings per (category, file, routine, message).

    Only the file name counts, so that the fingerprint does not depend on the
    absolute working path.
    """
    resolve = resolve or RoutineResolver()
    return collections.Counter(
        (o.category, o.file, resolve(o), o.message) for o in occurrences)


def categories(fingerprints: collections.Counter) -> dict[str, int]:
    totals = collections.Counter()
    for key, count in fingerprints.items():
        totals[key[0]] += count
    return dict(sorted(totals.items(), key=lambda item: (-item[1], item[0])))


def compile_flags(command: str) -> str:
    """The flags of one compile command, without compiler, paths and files.

    What remains decides which warnings the compiler gives: warning and
    optimisation flags, definitions of the switch variant, language standard.
    """
    tokens = shlex.split(command)[1:]
    kept, skip = [], False
    for token in tokens:
        if skip:
            skip = False
        elif token in ("-c", "-o", "-I", "-J"):
            skip = True
        elif not token.startswith(("-I", "-J")):
            kept.append(token)
    return " ".join(kept)


def check_flags(build_dir: Path) -> list[str]:
    """Compile commands that would hide warnings, as readable lines.

    The flags come from the compile commands the build really issued, so
    extra arguments through FC (`gfortran -w`) and flags from CMakeLists.txt
    are included. A command must carry every strict flag and must not
    switch all warnings off with `-w`. A single `-Wno-...` flag is not
    looked for.
    """
    commands = json.loads((build_dir / "compile_commands.json").read_text())
    required = STRICT_FLAGS.split()
    problems = []
    for entry in commands:
        flags = compile_flags(entry["command"]).split()
        missing = [flag for flag in required if flag not in flags]
        if missing or "-w" in flags:
            what = "warnings switched off (-w)" if "-w" in flags else \
                "missing " + " ".join(missing)
            problems.append(f"{Path(entry['file']).name}: {what}")
    return problems


def compiler_version(build_dir: Path) -> str:
    cache = (build_dir / "CMakeCache.txt").read_text().splitlines()
    path = next(line.split("=", 1)[1] for line in cache
                if line.startswith("CMAKE_Fortran_COMPILER:"))
    version = subprocess.run([path, "--version"], capture_output=True, text=True)
    return version.stdout.splitlines()[0].strip()


def read_triage(path: Path = TRIAGE_FILE) -> dict[tuple, dict]:
    """The recorded verdicts, keyed by fingerprint."""
    entries = json.loads(path.read_text())["verdicts"]
    table = {}
    for entry in entries:
        key = (entry["category"], entry["file"], entry["routine"], entry["message"])
        if key in table:
            raise ValueError(f"two verdicts for {key}")
        table[key] = entry
    return table


def write_triage(path: Path, table: dict[tuple, dict]) -> None:
    stored = json.loads(path.read_text())
    stored["verdicts"] = [table[key] for key in sorted(table)]
    path.write_text(json.dumps(stored, indent=1, ensure_ascii=False) + "\n")


def check(measured: collections.Counter, triage: dict[tuple, dict]):
    """Split the checked warnings into unreviewed and stale.

    Returns (unreviewed, stale): fingerprints the build gives more often than
    a verdict covers, and verdicts that cover more messages than the build
    still gives.
    """
    unreviewed, stale = {}, {}
    for key, count in measured.items():
        if key[0] in CHECKED:
            covered = triage[key]["count"] if key in triage else 0
            if count > covered:
                unreviewed[key] = count - covered
    for key, entry in triage.items():
        if entry["count"] > measured[key]:
            stale[key] = entry["count"] - measured[key]
    return unreviewed, stale


def prune(triage: dict[tuple, dict], measured: collections.Counter) -> dict[tuple, dict]:
    """Verdicts reduced to what the build still gives."""
    kept = {}
    for key, entry in triage.items():
        if measured[key]:
            kept[key] = dict(entry, count=min(entry["count"], measured[key]))
    return kept


def report(occurrences, measured, triage, unreviewed, stale, listing) -> None:
    checked = sum(count for key, count in measured.items() if key[0] in CHECKED)
    print(f"strict build: {sum(measured.values())} warnings, "
          f"{checked} of them in the checked categories")
    for category, count in categories(measured).items():
        mark = "checked" if category in CHECKED else "counted only"
        print(f"  {count:5d}  {category:24s} {mark}")
    suspected = [key for key, entry in triage.items()
                 if entry["verdict"] == "suspected-defect" and measured[key]]
    for key in suspected:
        print(f"suspected defect, still open: {key[0]}  {key[1]}:{key[2]}: {key[3]}")
    if listing:
        resolve = RoutineResolver()
        print()
        for o in sorted(occurrences, key=lambda o: (o.category, o.file, o.line)):
            if o.category in CHECKED:
                key = (o.category, o.file, resolve(o), o.message)
                verdict = triage[key]["verdict"] if key in triage else "UNREVIEWED"
                print(f"{o.category:22s} {o.file}:{o.line} [{key[2]}] {verdict}: {o.message}")
    if stale:
        print(f"\n{len(stale)} verdicts cover warnings the build no longer gives; "
              "remove them with --prune:")
        for key, extra in sorted(stale.items()):
            print(f"  -{extra}  {key[0]}  {key[1]}:{key[2]}: {key[3]}")
    if unreviewed:
        lines = [f"  +{extra}  {key[0]}  {key[1]}:{key[2]}: {key[3]}"
                 for key, extra in sorted(unreviewed.items())]
        print("\n".join(["", "Warnings without a verdict. Look at each one and record "
                         f"what it is in {TRIAGE_FILE.name}:", *lines]), file=sys.stderr)


def parse_arguments(argv=None):
    parser = argparse.ArgumentParser(
        description=__doc__.splitlines()[0],
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--build-dir", default="build-strict-diagnostics",
                        help="build directory; with --log, where the compile "
                        "commands are read (default: %(default)s)")
    parser.add_argument("--log", type=Path,
                        help="check this build log instead of building")
    parser.add_argument("--list", action="store_true",
                        help="list every checked warning with location and verdict")
    parser.add_argument("--prune", action="store_true",
                        help="drop verdicts, or lower their count, for warnings "
                        "the build no longer gives")
    parser.add_argument("--triage", type=Path, default=TRIAGE_FILE,
                        help=argparse.SUPPRESS)
    return parser.parse_args(argv)


def main(argv=None) -> int:
    arguments = parse_arguments(argv)
    build_dir = Path(arguments.build_dir).resolve()
    log = arguments.log.read_text() if arguments.log else build(ROOT, build_dir)
    if not (build_dir / "compile_commands.json").is_file():
        print(f"no compile_commands.json in {build_dir}; with --log, point "
              "--build-dir at the build that wrote the log", file=sys.stderr)
        return 1
    problems = check_flags(build_dir)
    if problems:
        print("the build did not use the strict flags, so its warnings say "
              "nothing:\n  " + "\n  ".join(problems[:10]), file=sys.stderr)
        return 1
    try:
        occurrences = parse(log)
    except ValueError as error:
        print(error, file=sys.stderr)
        return 1
    measured = fingerprint(occurrences)
    unresolved = sum(count for key, count in measured.items() if key[2] == "?")
    if unresolved:
        print(f"{unresolved} warnings could not be traced to a routine",
              file=sys.stderr)
        return 1

    print(f"compiler: {compiler_version(build_dir)}")
    triage = read_triage(arguments.triage)
    unreviewed, stale = check(measured, triage)
    if arguments.prune and stale:
        triage = prune(triage, measured)
        write_triage(arguments.triage, triage)
        print(f"{len(stale)} stale verdicts pruned in {arguments.triage.name}")
        stale = {}
    report(occurrences, measured, triage, unreviewed, stale, arguments.list)
    return 1 if unreviewed else 0


if __name__ == "__main__":
    raise SystemExit(main())
