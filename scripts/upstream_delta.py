#!/usr/bin/env python3
"""Compare the fork deck by deck with the pinned TU Delft source.

The bug-fix table says in prose which repairs shift the output. That is
exactly as reliable as the discipline of whoever keeps that table up to date. This
script turns it into a checked list: `doc/upstream-delta.json` states
per deck whether it should be identical to upstream or deliberately differs, with
the BF number, and this gate checks that claim against reality.

A deck that starts to differ without a BF reference thus turns the gate red
instead of being discovered after the fact.

Every run starts with a fixed memory layout (`setarch -R`), because upstream
otherwise gives two different answers from run to run; see `launcher`.

The comparison is strict: fields must be exactly equal. Upstream and the fork
run here with the same compiler, the same flags and on the same machine, so
everything that differs is the code -- there is no margin within which "roughly
equal" still means anything. Where the fork deliberately differs, the manifest says so.

Statuses in the manifest:

  identical     both binaries run the deck and give equal output;
  differs       both run, the output differs; `bugfix` names the repair
                that makes the difference, or `investigation` says what has been
                measured while the attribution is not yet settled;
  fork only     upstream refuses the deck (a command it does not know),
                `bugfix` names the repair that made the deck reachable;
  skipped       deliberately not compared, `reason` says why -- never silently;
  upstream unstable
                upstream gives a different answer on this deck from run to run,
                so a comparison says nothing about it;
  fork unstable
                the fork does that -- that is always a fault on this side.

Usage:

  python3 scripts/upstream_delta.py                 # gate: claim vs. reality
  python3 scripts/upstream_delta.py --update        # take over the measured reality
  python3 scripts/upstream_delta.py --deck quick_test/quick_test.swn

`--update` never turns the gate green on its own: a new difference gets
`"bugfix": null`, and the check fails on that until someone explains the difference.
"""

from __future__ import annotations

import argparse
import datetime
import filecmp
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parent.parent
MANIFEST = REPOSITORY / "doc" / "upstream-delta.json"
BUGFIX_TABLE = REPOSITORY / "doc" / "bug-fixes-vs-tu-delft-41.51.md"
EXAMPLES = REPOSITORY / "examples"

sys.path.insert(0, str(EXAMPLES))
from reference_check import same_results  # noqa: E402

#  Output that says nothing about the model: logs, error files and the
#  print file with timestamps and path names in it. What remains are the tables,
#  blocks and spectra -- the model itself.
IGNORED_NAMES = {
    "INPUT",
    "norm_end",
    "swaninit",
    "ERRPTS",
    "console.log",
}
#  .bqf is the binary interaction table that the XNL route writes alongside:
#  a work file with padding bytes, not model output.
IGNORED_SUFFIXES = {".prt", ".erf", ".log", ".egs", ".bqf"}
IGNORED_PREFIXES = ("PRINT", "Errfile", "ERRFILE", "screen")
#  Directories next to the deck that are neither input nor output.
SKIPPED_DIRECTORIES = {"reference", "reference-jac", "references", "results", "__pycache__"}

#  Set to False by --no-pin; see launcher().
PIN_MEMORY_LAYOUT = True

STATUS_IDENTICAL = "identical"
STATUS_DIFFERS = "differs"
STATUS_UPSTREAM_UNSTABLE = "upstream unstable"
STATUS_FORK_UNSTABLE = "fork unstable"
STATUS_FORK_ONLY = "fork only"
STATUS_SKIPPED = "skipped"


@dataclass
class RunOutcome:
    """What one binary made of one deck."""

    returncode: int
    outputs: dict[str, Path]
    seconds: float
    timed_out: bool = False


@dataclass
class DeckVerdict:
    deck: str
    status: str
    detail: str


def load_manifest(path: Path) -> dict:
    if not path.is_file():
        raise SystemExit(
            f"manifest missing: {path}. Pass the right path with --manifest; "
            "--update only updates an existing manifest."
        )
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)


def write_manifest(path: Path, manifest: dict) -> None:
    with path.open("w", encoding="utf-8") as handle:
        json.dump(manifest, handle, ensure_ascii=False, indent=2)
        handle.write("\n")


def known_bugfix_identifiers() -> set[str]:
    """The BF numbers that the bug-fix table actually contains."""
    if not BUGFIX_TABLE.is_file():
        return set()
    text = BUGFIX_TABLE.read_text(encoding="utf-8")
    return set(re.findall(r"^\| (BF-\d+) \|", text, flags=re.MULTILINE))


def discover_decks() -> list[str]:
    """Every deck in the example suite, as a path under `examples/`."""
    return sorted(
        str(deck.relative_to(EXAMPLES)) for deck in EXAMPLES.rglob("*.swn")
    )


def ensure_upstream_source(worktree: Path, commit: str) -> None:
    """Make sure the pinned upstream source is in place.

    The worktree belongs in `.upstream/`: that path is in `.gitignore`,
    so the reference source does not clutter the fork, and it survives -- unlike
    a checkout in /tmp -- a restart of the machine.

    The path must not contain a hyphen. The upstream build runs `switch.pl` to
    preprocess the fixed-form source, and that script parses its arguments with
    `while ($ARGV[0]=~/-.*/)` without `shift` in the catch-all branch: a source
    path with a hyphen in it matches that unanchored regex, is recognised by no
    option at all, and the script runs forever at 100% CPU. Better a
    clear error than a build that seems to hang.
    """
    if "-" in str(worktree):
        raise SystemExit(
            f"upstream path {worktree} contains a hyphen. The upstream switch.pl "
            "gets stuck on it in an endless loop; choose a path without a hyphen."
        )
    if (worktree / "CMakeLists.txt").is_file():
        head = subprocess.run(
            ["git", "-C", str(worktree), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            check=False,
        )
        if head.returncode == 0 and head.stdout.strip().startswith(commit[:12]):
            return
        raise SystemExit(
            f"upstream worktree {worktree} is not at {commit[:12]}. "
            "Remove it (git worktree remove) and run again."
        )
    worktree.parent.mkdir(parents=True, exist_ok=True)
    print(f"checking out the upstream source at {commit[:12]} in {worktree} ...")
    completed = subprocess.run(
        ["git", "-C", str(REPOSITORY), "worktree", "add", "--detach", str(worktree), commit],
        capture_output=True,
        text=True,
        check=False,
    )
    if completed.returncode != 0:
        raise SystemExit(
            "could not create the upstream worktree:\n"
            f"{completed.stdout}\n{completed.stderr}"
        )


def launcher() -> list[str]:
    """Start every run with a fixed memory layout.

    On several decks, upstream 41.51 gives two different answers from run to
    run for the same input. If you switch off Linux's randomised memory
    layout (`setarch -R`), it gives only
    one: so the result depends on where the process lands in memory,
    the signature of a variable that is read without ever having been
    written. Without that pin this gate flickers between "identical" and
    "differs" on the same code, and then it guards nothing.

    If `setarch` is missing, the gate carries on and warns. With
    `--no-pin` you deliberately keep the randomness, to see that
    instability for yourself: run a deck a few times and compare the outcomes.
    """
    if not PIN_MEMORY_LAYOUT:
        return []
    if shutil.which("setarch"):
        return ["setarch", "-R"]
    print("  note: setarch is missing, the comparison may change from run to run")
    return []


def find_binary(build_directory: Path) -> Path | None:
    candidate = build_directory / "bin" / "swan.exe"
    if candidate.is_file():
        return candidate
    for found in build_directory.rglob("swan.exe"):
        if found.is_file():
            return found
    return None


#  Upstream's CMake forces the build type to None and the flags to
#  "-O ..."; a requested Release build therefore silently becomes -O1. The fork
#  builds Release at -O3, and that difference alone changes the output of the
#  friction decks (swancom1) and of ST6 (SdsBabanin). So upstream gets the same
#  level here through the flags of build type None, which CMake puts after the
#  fixed flags, so that -O3 wins.
UPSTREAM_OPTIMISATION = "-O3"


def fork_compiler(fork_binary: Path) -> Path:
    """The Fortran compiler that the fork build was configured with.

    The compiler version changes the output too: two gfortran versions can
    compute the quadruplet decks differently at -O3. So upstream is built with the same
    compiler as the fork, looked up in the CMake cache next to the binary.
    """
    for directory in fork_binary.parents:
        cache = directory / "CMakeCache.txt"
        if cache.is_file():
            match = re.search(
                r"^CMAKE_Fortran_COMPILER:(?:FILEPATH|STRING)=(.+)$", cache.read_text(), re.MULTILINE
            )
            if match:
                return Path(match.group(1).strip()).resolve()
    raise SystemExit(
        f"no CMakeCache.txt with a Fortran compiler found above {fork_binary}"
    )


def upstream_build_directory(upstream_root: Path, compiler: Path) -> Path:
    """One build directory per compiler version, so that an old build never counts silently."""
    version = subprocess.run(
        [str(compiler), "-dumpfullversion"], capture_output=True, text=True, check=True
    ).stdout.strip()
    return upstream_root / f"build_O3_gfortran{version}"


def compiler_versions(binary: Path) -> set[str] | None:
    """The GCC versions listed in the `.comment` section of a binary."""
    if not shutil.which("readelf"):
        return None
    completed = subprocess.run(
        ["readelf", "-p", ".comment", str(binary)], capture_output=True, text=True, check=False
    )
    if completed.returncode != 0:
        return None
    return set(re.findall(r"GCC: \([^)]*\) (\S+)", completed.stdout))


def require_same_compiler(fork_binary: Path, upstream_binary: Path) -> None:
    """Refuse a comparison between binaries from different compilers.

    A build directory that only recompiles the changed files after a
    compiler change yields a binary with two versions in it; that is
    refused too, because such a binary belongs to no compiler at all.
    """
    fork = compiler_versions(fork_binary)
    upstream = compiler_versions(upstream_binary)
    if fork is None or upstream is None:
        print("  note: cannot determine the compiler versions (readelf missing?)")
        return
    if len(fork) > 1:
        raise SystemExit(
            f"the fork binary was built with more than one compiler ({', '.join(sorted(fork))}); "
            "rebuild it in a clean directory"
        )
    if fork != upstream:
        raise SystemExit(
            f"fork ({', '.join(sorted(fork))}) and upstream ({', '.join(sorted(upstream))}) "
            "were built with different compilers; the comparison then says nothing about the code"
        )


def build_upstream(worktree: Path, build_directory: Path, jobs: int, compiler: Path) -> Path:
    existing = find_binary(build_directory)
    if existing:
        return existing
    print(f"building upstream in {build_directory} (once, takes a few minutes) ...")
    configure = subprocess.run(
        [
            "cmake",
            "-S",
            str(worktree),
            "-B",
            str(build_directory),
            f"-DCMAKE_Fortran_COMPILER={compiler}",
            f"-DCMAKE_Fortran_FLAGS_NONE={UPSTREAM_OPTIMISATION}",
        ],
        capture_output=True,
        text=True,
        check=False,
    )
    if configure.returncode != 0:
        raise SystemExit(f"cmake configure failed:\n{configure.stdout}\n{configure.stderr}")
    build = subprocess.run(
        ["cmake", "--build", str(build_directory), "-j", str(jobs)],
        capture_output=True,
        text=True,
        check=False,
    )
    if build.returncode != 0:
        raise SystemExit(f"upstream build failed:\n{build.stdout[-4000:]}\n{build.stderr[-4000:]}")
    binary = find_binary(build_directory)
    if not binary:
        raise SystemExit(f"upstream build produced no swan.exe in {build_directory}")
    return binary


def is_output(name: str) -> bool:
    if name in IGNORED_NAMES:
        return False
    if Path(name).suffix.lower() in IGNORED_SUFFIXES:
        return False
    return not name.startswith(IGNORED_PREFIXES)


def copy_case_files(source: Path, destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=True)
    for item in source.iterdir():
        if item.name in SKIPPED_DIRECTORIES:
            continue
        if item.is_dir():
            shutil.copytree(item, destination / item.name, dirs_exist_ok=True)
        else:
            shutil.copy2(item, destination / item.name)


def stage_case(deck: str, work_root: Path) -> Path:
    """Stage the input of one case, with the deck as INPUT.

    The case gets its own directory with the original name, because a deck
    may refer to a neighbouring case: `voordelta_unstructured` reads the
    bathymetry as `../voordelta/voordelta.dep`. Those neighbours are copied
    along to exactly that relative place; otherwise the deck fails on a missing
    file and the gate would count that as "differs".
    """
    deck_path = EXAMPLES / deck
    case_directory = deck_path.parent
    run_directory = work_root / case_directory.name
    copy_case_files(case_directory, run_directory)
    for sibling in sorted(set(re.findall(r"\.\./([A-Za-z0-9_.-]+)/", deck_path.read_text(errors="replace")))):
        neighbour = case_directory.parent / sibling
        if neighbour.is_dir():
            copy_case_files(neighbour, work_root / sibling)
    shutil.copy2(deck_path, run_directory / "INPUT")
    return run_directory


def written_since(directory: Path, moment: float) -> dict[str, bytes]:
    """The files that the run itself wrote.

    By timestamp, not by content. Several cases keep their own
    output next to the deck; if the run writes exactly the same thing back
    there, that is still output that must be compared. A
    content comparison would silently skip it -- and precisely on the side
    that gets it right, which turns the comparison on its head.

    `shutil.copy2` keeps the original timestamp of the copied
    input, so everything touched after the start of the run comes from the run.
    """
    written: dict[str, bytes] = {}
    for path in directory.rglob("*"):
        if not path.is_file():
            continue
        if path.stat().st_mtime + 1.0 < moment:
            continue
        written[str(path.relative_to(directory))] = path.read_bytes()
    return written


def run_deck(binary: Path, deck: str, destination: Path, timeout: int) -> RunOutcome:
    """Run one deck and keep what it produced."""
    destination.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="swan-delta-") as temporary:
        work_directory = stage_case(deck, Path(temporary))
        timed_out = False
        moment = time.time()
        started = time.monotonic()
        try:
            completed = subprocess.run(
                [*launcher(), str(binary)],
                cwd=work_directory,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False,
            )
            returncode = completed.returncode
        except subprocess.TimeoutExpired:
            returncode = -1
            timed_out = True
        seconds = time.monotonic() - started
        outputs: dict[str, Path] = {}
        for name, content in written_since(work_directory, moment).items():
            if not is_output(name):
                continue
            target = destination / name
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(content)
            outputs[name] = target
    return RunOutcome(returncode=returncode, outputs=outputs, seconds=seconds, timed_out=timed_out)


def files_equal(first: Path, second: Path) -> bool:
    """Equal if the text fields match exactly, or if the bytes are equal.

    Zero tolerance: the same compiler, the same flags, the same machine, so
    every difference is code. `same_results` ignores only white space, which is
    the difference between the serial and the MPI write route, not a result.
    """
    if filecmp.cmp(first, second, shallow=False):
        return True
    try:
        first.read_text()
        second.read_text()
    except (OSError, UnicodeDecodeError):
        return False
    return same_results(first, second, tolerance=0.0)


def compare_outputs(first: RunOutcome, second: RunOutcome) -> tuple[bool, str]:
    """Compare what two runs produced."""
    names = sorted(set(first.outputs) | set(second.outputs))
    missing = [name for name in names if name not in first.outputs or name not in second.outputs]
    if missing:
        return False, "present on one side only: " + ", ".join(missing[:3])
    different = [name for name in names if not files_equal(first.outputs[name], second.outputs[name])]
    if different:
        return False, "differs in: " + ", ".join(different[:3])
    return True, f"{len(names)} output file(s) equal"


def assess_deck(
    deck: str,
    fork_binary: Path,
    upstream_binary: Path,
    root: Path,
    timeout: int,
    repeats: int,
) -> tuple[str, str, float]:
    """Determine the status of one deck, with a check on self-consistency.

    A single comparison assumes that both binaries always give the same answer
    on the same deck. That cannot be taken for granted: upstream 41.51
    turns out to give two different answers from run to run on several
    decks. One comparison would then say "identical" one time and
    "differs" the next, and the gate would flicker at random.

    So a difference that is found is first checked against the side itself: if
    a binary gives a different answer from itself when repeated, that is the
    result -- instability, not a difference between the two lines. Repeating
    happens only on a difference, so that the equal cases stay cheap.
    """
    fork = run_deck(fork_binary, deck, root / "fork", timeout)
    upstream = run_deck(upstream_binary, deck, root / "upstream", timeout)
    seconds = fork.seconds + upstream.seconds

    #  A time limit proves nothing about the difference; it only says that the
    #  gate allowed too little time. That should not end as "fork only",
    #  because then an overrun reads as a property of the code.
    if fork.timed_out or upstream.timed_out:
        side = "the fork" if fork.timed_out else "upstream"
        return STATUS_DIFFERS, time_limit_detail(side), seconds
    if fork.returncode != 0:
        return STATUS_DIFFERS, f"the fork stopped with code {fork.returncode}", seconds
    if upstream.returncode != 0:
        return STATUS_FORK_ONLY, f"upstream stopped with code {upstream.returncode}", seconds
    if not fork.outputs:
        return STATUS_DIFFERS, "the fork produced no output to compare", seconds

    same, detail = compare_outputs(fork, upstream)
    if same:
        return STATUS_IDENTICAL, detail, seconds

    for side, binary, first, status in (
        ("the fork", fork_binary, fork, STATUS_FORK_UNSTABLE),
        ("upstream", upstream_binary, upstream, STATUS_UPSTREAM_UNSTABLE),
    ):
        #  The distinct answers seen so far; a repeat that matches any of them
        #  is not a new answer.
        answers = [first]
        for attempt in range(repeats):
            again = run_deck(binary, deck, root / f"{status}_{attempt}", timeout)
            seconds += again.seconds
            #  An overrun is not an answer, so it must not count as instability.
            if again.timed_out:
                return STATUS_DIFFERS, time_limit_detail(side), seconds
            if not any(compare_outputs(seen, again)[0] for seen in answers):
                answers.append(again)
        if len(answers) > 1:
            return (
                status,
                f"{side} gives itself {len(answers)} different answers in {repeats + 1} runs; "
                "a comparison says nothing here",
                seconds,
            )
    return STATUS_DIFFERS, detail, seconds


def time_limit_detail(side: str) -> str:
    return f"{side} exceeded the time limit -- raise --timeout or skip the deck deliberately"


def entries_by_deck(manifest: dict) -> dict[str, dict]:
    return {entry["deck"]: entry for entry in manifest.get("decks", [])}


def check_inventory(manifest: dict, decks: list[str]) -> list[str]:
    """Static checks: is the inventory itself complete and explained?"""
    problems: list[str] = []
    unattributed: list[str] = []
    entries = entries_by_deck(manifest)
    known = known_bugfix_identifiers()

    for deck in decks:
        if deck not in entries:
            problems.append(f"{deck}: not in the manifest")
    for deck in entries:
        if deck not in decks:
            problems.append(f"{deck}: in the manifest but no longer exists")

    for deck, entry in sorted(entries.items()):
        status = entry.get("status")
        if status not in (
            STATUS_IDENTICAL,
            STATUS_DIFFERS,
            STATUS_FORK_ONLY,
            STATUS_SKIPPED,
            STATUS_UPSTREAM_UNSTABLE,
            STATUS_FORK_UNSTABLE,
        ):
            problems.append(f"{deck}: unknown status {status!r}")
            continue
        #  An unstable fork is never an accepted state: reproducible results
        #  are the promise of this side, so this is always red.
        if status == STATUS_FORK_UNSTABLE:
            problems.append(f"{deck}: the fork gives itself different answers")
            continue
        if status in (STATUS_DIFFERS, STATUS_FORK_ONLY):
            bugfix = entry.get("bugfix")
            if bugfix:
                #  An empty set means the table is missing or its format has
                #  changed; then no BF number can be confirmed.
                if bugfix not in known:
                    problems.append(f"{deck}: refers to {bugfix}, which is not in the bug-fix table")
            elif entry.get("investigation"):
                unattributed.append(deck)
            else:
                problems.append(
                    f"{deck}: {status} without a BF reference and without an investigation note -- "
                    "explain the difference or fix it"
                )
        if status == STATUS_SKIPPED and not entry.get("reason"):
            problems.append(f"{deck}: skipped without a reason")

    #  A ratchet, not a promise. The budget counts the decks that may differ
    #  from upstream without an explanation: it may shrink, never grow.
    #  So a new unexplained difference is red, while any existing
    #  backlog stays visible instead of making the gate unusable.
    #  Without a record of when and with what the decks were last measured,
    #  "identical" cannot be traced back to a state of the code.
    measurement = manifest.get("last_measurement")
    if not isinstance(measurement, dict) or not all(
        measurement.get(key) for key in ("date", "compiler", "decks")
    ):
        problems.append("the manifest has no last_measurement with date, compiler and decks")

    budget = manifest.get("unexplained_budget")
    if budget is None:
        problems.append("the manifest has no unexplained_budget")
    elif len(unattributed) > budget:
        problems.append(
            f"{len(unattributed)} decks differ without explanation, budget is {budget}: "
            + ", ".join(sorted(unattributed))
        )
    elif len(unattributed) < budget:
        problems.append(
            f"only {len(unattributed)} unexplained decks against a budget of "
            f"{budget}: lower the budget so that the ratchet holds"
        )
    return problems


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--manifest", default=str(MANIFEST), help="path to the delta inventory")
    parser.add_argument("--fork-binary", help="swan.exe of the fork (default: build/bin/swan.exe)")
    parser.add_argument("--upstream-binary", help="swan.exe of upstream (default: build it here)")
    parser.add_argument(
        "--upstream-build",
        default=str(REPOSITORY / ".upstream"),
        help="directory for the upstream worktree and build (default: %(default)s)",
    )
    parser.add_argument("--deck", action="append", help="limit to this deck (repeatable)")
    parser.add_argument("--include-skipped", action="store_true", help="also run the skipped decks")
    parser.add_argument("--update", action="store_true", help="write the measured reality into the manifest")
    parser.add_argument("--inventory-only", action="store_true", help="only the static checks, run nothing")
    parser.add_argument("--timeout", type=int, default=600, help="time limit in seconds per deck run (default: %(default)s)")
    parser.add_argument(
        "--repeats",
        type=int,
        default=2,
        help="extra runs per side when a difference is found, to tell instability of a "
        "binary apart from a real difference (default: %(default)s)",
    )
    parser.add_argument("--jobs", type=int, default=0, help="parallel build jobs (default: all cores)")
    parser.add_argument("--keep-results", help="keep the output of both binaries here")
    parser.add_argument(
        "--no-pin",
        action="store_true",
        help="run without a fixed memory layout; shows the instability of upstream on purpose",
    )
    arguments = parser.parse_args()

    global PIN_MEMORY_LAYOUT
    PIN_MEMORY_LAYOUT = not arguments.no_pin

    manifest_path = Path(arguments.manifest).resolve()
    manifest = load_manifest(manifest_path)
    decks = discover_decks()

    problems = check_inventory(manifest, decks)
    if arguments.inventory_only:
        for problem in problems:
            print(f"  [!] {problem}")
        print(f"\n{len(decks)} deck(s) in the suite, {len(entries_by_deck(manifest))} in the manifest.")
        return 1 if problems else 0

    selected = arguments.deck or decks
    unknown = [deck for deck in selected if deck not in decks]
    if unknown:
        raise SystemExit(
            f"unknown deck(s): {', '.join(unknown)}. Give the path under examples/, "
            "for example quick_test/quick_test.swn."
        )

    if arguments.fork_binary:
        fork_binary = Path(arguments.fork_binary).resolve()
    else:
        fork_binary = REPOSITORY / "build" / "bin" / "swan.exe"
    if not fork_binary.is_file():
        raise SystemExit(f"fork binary not found: {fork_binary}. Build the fork first.")

    jobs = arguments.jobs or len(os.sched_getaffinity(0))
    upstream_root = Path(arguments.upstream_build).resolve()
    if arguments.upstream_binary:
        upstream_binary = Path(arguments.upstream_binary).resolve()
        if not upstream_binary.is_file():
            raise SystemExit(f"upstream binary not found: {upstream_binary}")
    else:
        commit = manifest["upstream"]["commit"]
        ensure_upstream_source(upstream_root / "src", commit)
        compiler = fork_compiler(fork_binary)
        upstream_binary = build_upstream(
            upstream_root / "src", upstream_build_directory(upstream_root, compiler), jobs, compiler
        )
    require_same_compiler(fork_binary, upstream_binary)

    entries = entries_by_deck(manifest)

    results_root = Path(arguments.keep_results).resolve() if arguments.keep_results else None

    verdicts: list[DeckVerdict] = []
    with tempfile.TemporaryDirectory(prefix="swan-delta-runs-") as scratch:
        for deck in selected:
            entry = entries.get(deck, {})
            if entry.get("status") == STATUS_SKIPPED and not arguments.include_skipped:
                verdicts.append(DeckVerdict(deck, STATUS_SKIPPED, entry.get("reason", "")))
                continue
            root = Path(results_root or scratch) / deck.replace("/", "_")
            status, detail, seconds = assess_deck(
                deck, fork_binary, upstream_binary, root, arguments.timeout, arguments.repeats
            )
            verdicts.append(DeckVerdict(deck, status, detail))
            print(f"  {status:<18} {deck}  ({seconds:.1f} s)")

    width = max(len(verdict.deck) for verdict in verdicts) if verdicts else 10
    print()
    for verdict in verdicts:
        entry = entries.get(verdict.deck, {})
        claimed = entry.get("status", "-- not in manifest --")
        #  For a skipped deck the reason is already in the detail column; putting
        #  it on the left as well would print the same sentence twice.
        attribution = entry.get("bugfix") or ("" if verdict.status == STATUS_SKIPPED else entry.get("reason", ""))
        marker = "[x]" if claimed == verdict.status else "[!]"
        print(f"  {marker} {verdict.deck:<{width}}  {verdict.status:<18} {attribution:<8} {verdict.detail}")

    #  The claimed status is taken now, because --update below changes the
    #  entries in place.
    mismatched = [
        (verdict, entries.get(verdict.deck, {}).get("status", "-- not in manifest --"))
        for verdict in verdicts
        if entries.get(verdict.deck, {}).get("status") != verdict.status
    ]

    if arguments.update:
        for verdict in verdicts:
            #  A skipped deck was not run: keep its entry, including the
            #  "measured" note of an earlier run with --include-skipped.
            if verdict.status == STATUS_SKIPPED:
                continue
            entry = entries.get(verdict.deck)
            if entry is None:
                entry = {"deck": verdict.deck}
                manifest.setdefault("decks", []).append(entry)
            if entry.get("status") != verdict.status:
                entry["status"] = verdict.status
                if verdict.status in (STATUS_DIFFERS, STATUS_FORK_ONLY):
                    #  A new difference is not approved silently:
                    #  without a BF number the check still fails on it.
                    entry.setdefault("bugfix", None)
            entry["measured"] = verdict.detail
        manifest["decks"] = sorted(manifest["decks"], key=lambda item: item["deck"])
        #  Only a run over every deck says something about the whole inventory;
        #  a run limited with --deck leaves the record of the last full one.
        if not arguments.deck:
            versions = compiler_versions(fork_binary) or set()
            skipped = any(verdict.status == STATUS_SKIPPED for verdict in verdicts)
            record = {
                "date": datetime.date.today().isoformat(),
                "compiler": ", ".join(f"GNU Fortran {version}" for version in sorted(versions)) or "unknown",
                "decks": "all except the skipped ones" if skipped else "all, including the heavy ones",
            }
            decks_list = manifest.pop("decks")
            manifest["last_measurement"] = record
            manifest["decks"] = decks_list
        write_manifest(manifest_path, manifest)
        print(f"\nmanifest updated: {manifest_path}")
        problems = check_inventory(manifest, decks)

    for problem in problems:
        print(f"  [!] {problem}")
    for verdict, claimed in mismatched:
        print(f"  [!] {verdict.deck}: manifest says {claimed!r}, measured {verdict.status!r}")

    if problems or (mismatched and not arguments.update):
        print("\nThe delta inventory no longer matches reality.")
        return 1
    print("\nEvery difference with upstream is in the manifest and has an explanation.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
