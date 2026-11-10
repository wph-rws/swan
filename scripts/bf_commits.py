#!/usr/bin/env python3
"""Tie every bug fix in the table to the commit that made it.

Every bug-fix commit ends with the trailer `Bug-fix: BF-NN`. The section
"Commit of each fix" at the end of doc/bug-fixes-vs-tu-delft-41.51.md lists,
per BF number, the commit that made the fix. This script

  list    prints BF number, commit and subject from the trailers in the history;
  table   prints that list as the Markdown table of the section;
  check   fails when the section and the history disagree: a BF number of the
          bug-fix table without a commit, a listed commit that does not exist
          or does not carry the trailer, or a trailer for a number that the
          table does not have.

A later commit that corrects a fix carries the same trailer; the section names
the first commit, which made the fix, and `git log --grep='Bug-fix: BF-NN'`
shows them all. `check` needs the full history; in a shallow clone it says so
and passes.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parent.parent
TABLE = REPOSITORY / "doc" / "bug-fixes-vs-tu-delft-41.51.md"
HEADING = "## Commit of each fix"
TRAILER = re.compile(r"^Bug-fix: (BF-\d+)\s*$", re.MULTILINE)


def git(*args: str) -> str:
    return subprocess.run(["git", *args], cwd=REPOSITORY, capture_output=True,
                          text=True, check=True).stdout


def history() -> dict[str, list[tuple[str, str]]]:
    """BF number -> [(commit, subject)] in the order of the history."""
    out: dict[str, list[tuple[str, str]]] = {}
    log = git("log", "--reverse", "--format=%H%x1f%s%x1f%B%x1e", "HEAD")
    for record in log.split("\x1e"):
        if not record.strip():
            continue
        commit, subject, body = record.strip("\n").split("\x1f", 2)
        for number in TRAILER.findall(body):
            out.setdefault(number, []).append((commit, subject))
    return out


def table_numbers() -> set[str]:
    text = TABLE.read_text(encoding="utf-8")
    return set(re.findall(r"^\| (BF-\d+) \|", text, flags=re.MULTILINE))


def listed() -> dict[str, str]:
    """BF number -> commit, from the section at the end of the table."""
    text = TABLE.read_text(encoding="utf-8")
    if HEADING not in text:
        return {}
    section = text.split(HEADING, 1)[1]
    return dict(re.findall(r"^\| (BF-\d+) \| `([0-9a-f]{7,40})` \|", section, flags=re.MULTILINE))


def key(number: str) -> int:
    return int(number.split("-")[1])


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("command", choices=("list", "table", "check"))
    command = parser.parse_args().command
    found = history()
    if command == "list":
        for number in sorted(found, key=key):
            for commit, subject in found[number]:
                print(f"{number}  {commit[:10]}  {subject}")
        return 0
    if command == "table":
        print("| BF | Commit | Subject |")
        print("|---|---|---|")
        for number in sorted(found, key=key):
            commit, subject = found[number][0]
            print(f"| {number} | `{commit[:10]}` | {subject} |")
        return 0
    if git("rev-parse", "--is-shallow-repository").strip() == "true":
        print("shallow clone: the history is not complete, nothing checked")
        return 0
    problems = []
    numbers = table_numbers()
    commits = listed()
    for number in sorted(numbers, key=key):
        if number not in commits:
            problems.append(f"{number} has no commit in the section '{HEADING[3:]}'")
            continue
        made = [c for c, _ in found.get(number, [])]
        if not any(c.startswith(commits[number]) for c in made[:1]):
            problems.append(f"{number}: {commits[number]} is not the first commit with 'Bug-fix: {number}'")
    for number in found:
        if number not in numbers:
            problems.append(f"a commit has the trailer 'Bug-fix: {number}', which the table does not have")
    for line in problems:
        print(line)
    if not problems:
        print(f"{len(numbers)} fixes, each tied to its commit")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
