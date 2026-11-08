#!/usr/bin/env python3
"""Where upstream SWAN code lives in the fork.

TU Delft delivers SWAN as fixed-form `.ftn` files with switch-prefixed lines.
The fork has turned those into free-form modules, moved some routines to other
files, and spread the shared variables of `SWCOMM*` and `OCPCOMM*` over many
small modules. To carry an upstream change over, you first have to find where
the changed routine or variable lives now. This script answers that question,
and checks that the answer is written down for every case that is not obvious.

Two kinds of things are mapped:

- procedures (subroutines, functions, programs) of the pinned upstream source;
- shared state: module-level variables, parameters and derived types of the
  upstream modules, and the members of the upstream COMMON blocks.

The locations themselves are found by reading the source; they are not stored.
What is stored, in `doc/upstream-map.json`, is only what the source cannot
tell: why something moved, what it was renamed to or replaced by, and why it
was removed. A procedure that sits in a fork file with the same name as its
upstream file needs no entry. Neither does a variable that is declared in a
fork module with the same name as its upstream module, or in exactly one fork
module at all (the general reason for that is recorded per upstream module).

The upstream side is read from git at the commit pinned in
`doc/upstream-delta.json`, so the check does not depend on a local checkout.

Usage:
    scripts/upstream_map.py check            check the map (pytest gate)
    scripts/upstream_map.py where NAME ...   fork location of upstream names
    scripts/upstream_map.py changes OLD NEW  map an upstream change to the fork
    scripts/upstream_map.py summary          counts per status
"""

from __future__ import annotations

import argparse
import functools
import io
import json
import re
import subprocess
import sys
import tarfile
from dataclasses import dataclass, field
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parent.parent
SOURCE = REPOSITORY / "src"
MAP_FILE = REPOSITORY / "doc" / "upstream-map.json"
DELTA_FILE = REPOSITORY / "doc" / "upstream-delta.json"
SWITCH_MANIFEST = REPOSITORY / "doc" / "switch-manifest.json"

#  "variant": the routine stays in the file of its upstream name, and a second
#  version of it lives in a file that another build variant compiles instead
PROCEDURE_STATUSES = {"moved", "renamed", "replaced", "removed", "variant"}
STATE_STATUSES = {"moved", "renamed", "component", "local", "replaced", "removed"}

# ---------------------------------------------------------------------------
# Reading Fortran
# ---------------------------------------------------------------------------

TYPE_SPEC = re.compile(
    r"^(integer|real|logical|complex|double\s*precision|double\s*complex|character)"
    r"(\s*\*\s*(\d+|\(\s*\*\s*\)))?"
    r"(\s*\((?:[^()]|\([^()]*\))*\))?",
    re.IGNORECASE,
)
DECLARATION_START = re.compile(
    r"^(integer|real|logical|complex|double\s*precision|double\s*complex|character"
    r"|type\s*\(|class\s*\()",
    re.IGNORECASE,
)
PROCEDURE_START = re.compile(
    r"^(?:(?:pure|impure|elemental|recursive|module)\s+)*"
    r"(?:(?:integer|real|logical|complex|double\s*precision|character)"
    r"(?:\s*\*\s*\d+|\s*\((?:[^()]|\([^()]*\))*\))?\s+)?"
    r"(subroutine|function|program)\s+(\w+)",
    re.IGNORECASE,
)
PROCEDURE_END = re.compile(
    r"^end(\s*(subroutine|function|program)(\s+\w+)?)?$", re.IGNORECASE
)
MODULE_START = re.compile(r"^module\s+(\w+)$", re.IGNORECASE)
MODULE_END = re.compile(r"^end\s*module\b", re.IGNORECASE)
INTERFACE_START = re.compile(r"^(abstract\s+)?interface\b", re.IGNORECASE)
INTERFACE_END = re.compile(r"^end\s*interface\b", re.IGNORECASE)
TYPE_DEFINITION = re.compile(
    r"^type\b(?!\s*\()(?:\s*,[^:]*)?(?:\s*::)?\s*(\w+)\s*$", re.IGNORECASE
)
TYPE_END = re.compile(r"^end\s*type\b", re.IGNORECASE)
COMMON_BLOCK = re.compile(r"^common\s*/\s*(\w+)\s*/(.*)$", re.IGNORECASE)
CONTAINS = re.compile(r"^contains$", re.IGNORECASE)


@dataclass
class Statement:
    text: str
    line: int  # first source line, 1-based


@functools.lru_cache(maxsize=None)
def switch_markers() -> tuple[str, ...]:
    """The upstream switch prefixes, longest first (`!!ESMF` before `!ESMF`)."""
    manifest = json.loads(SWITCH_MANIFEST.read_text(encoding="utf-8"))
    return tuple(sorted(manifest["markers"], key=len, reverse=True))


def strip_comment(text: str) -> str:
    """Remove a trailing `!` comment that is not inside a string."""
    quote = ""
    for index, char in enumerate(text):
        if quote:
            if char == quote:
                quote = ""
        elif char in "'\"":
            quote = char
        elif char == "!":
            return text[:index]
    return text


def statements(text: str, fixed_form: bool, markers: tuple[str, ...]) -> list[Statement]:
    """Join continuation lines into statements, without comments.

    Switch-prefixed lines are read as code: every upstream variant counts,
    whichever build would compile it. The prefix is replaced by blanks, as
    switch.pl does, so the fixed-form columns stay where they were.
    """
    result: list[Statement] = []
    for number, raw in enumerate(text.splitlines(), start=1):
        line = raw.rstrip("\n")
        for marker in markers:
            if line.startswith(marker):
                line = " " * len(marker) + line[len(marker):]
                break
        if fixed_form:
            line = line[:72]
            if not line or line[0] in "cC*!":
                continue
            if len(line) > 5 and line[5] not in " 0" and result and line[:5].strip() == "":
                result[-1].text += " " + strip_comment(line[6:]).strip()
                continue
            body = strip_comment(line[6:] if len(line) > 6 else "").strip()
            label_and_body = strip_comment(line).strip()
            if not label_and_body:
                continue
            result.append(Statement(body or label_and_body, number))
        else:
            body = strip_comment(line).strip()
            if not body:
                continue
            if result and result[-1].text.endswith("&"):
                result[-1].text = result[-1].text[:-1].rstrip() + " " + body.lstrip("&").strip()
                continue
            result.append(Statement(body, number))
    # A trailing `&` on the last statement, or labels in free form, are
    # harmless for what is read here.
    for statement in result:
        statement.text = re.sub(r"^\d+\s+", "", statement.text)
    return result


def split_entities(text: str) -> list[str]:
    """Names in a declaration entity list, ignoring dimensions and values."""
    names, depth, quote, current = [], 0, "", ""
    for char in text:
        if quote:
            current += char
            if char == quote:
                quote = ""
            continue
        if char in "'\"":
            quote = char
        elif char in "([":
            depth += 1
        elif char in ")]":
            depth -= 1
        elif char == "," and depth == 0:
            names.append(current)
            current = ""
            continue
        current += char
    names.append(current)
    found = []
    for item in names:
        match = re.match(r"\s*(\w+)", item)
        if match:
            found.append(match.group(1).upper())
    return found


def declared_names(text: str) -> list[str]:
    if "::" in text:
        return split_entities(text.split("::", 1)[1])
    if re.match(r"^(type|class)\s*\(", text, re.IGNORECASE):
        depth, index = 0, 0
        for index, char in enumerate(text):
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    break
        return split_entities(text[index + 1:])
    match = TYPE_SPEC.match(text)
    if not match:
        return []
    rest = text[match.end():]
    if re.match(r"^\s*(function|subroutine)\b", rest, re.IGNORECASE):
        return []
    return split_entities(rest)


@dataclass
class Procedure:
    name: str
    file: str
    module: str  # enclosing module, "" for an external procedure
    host: str  # enclosing procedure for an internal one, else ""
    first: int
    last: int = 0


@dataclass
class State:
    name: str  # variable, parameter or type name
    kind: str  # "variable", "type", "common"
    file: str
    module: str  # module, or "/BLOCK/" for a COMMON member
    line: int


@dataclass
class Source:
    procedures: list[Procedure] = field(default_factory=list)
    state: list[State] = field(default_factory=list)
    modules: dict[str, str] = field(default_factory=dict)  # module -> file
    components: dict[str, set[str]] = field(default_factory=dict)  # type -> names


def read_file(source: Source, name: str, text: str, fixed_form: bool,
              markers: tuple[str, ...]) -> None:
    """Add the procedures, module state and derived types of one file."""
    module = ""
    in_module_spec = False
    procedures: list[Procedure] = []
    interface_depth = 0
    type_name = ""
    for statement in statements(text, fixed_form, markers):
        text_ = statement.text
        lower = text_.lower()
        if interface_depth:
            if INTERFACE_END.match(lower):
                interface_depth -= 1
            elif INTERFACE_START.match(lower):
                interface_depth += 1
            continue
        if INTERFACE_START.match(lower):
            interface_depth += 1
            continue
        if type_name:
            if TYPE_END.match(lower):
                type_name = ""
            elif DECLARATION_START.match(lower):
                source.components.setdefault(type_name, set()).update(
                    declared_names(text_)
                )
            continue
        match = MODULE_START.match(text_)
        if match and not lower.startswith("module procedure"):
            module = match.group(1).upper()
            source.modules[module] = name
            in_module_spec = True
            continue
        if MODULE_END.match(lower):
            module, in_module_spec = "", False
            continue
        if CONTAINS.match(lower):
            in_module_spec = False
            continue
        match = PROCEDURE_START.match(text_)
        if match and not lower.startswith(("end", "module procedure")):
            if procedures and procedures[-1].name == match.group(2).upper() \
                    and procedures[-1].first < statement.line:
                #  A second header of the same procedure: upstream gives
                #  SWTABP one per switch variant (!NCF and !NNCF), followed
                #  by a single body.
                continue
            host = procedures[-1].name if procedures else ""
            procedure = Procedure(match.group(2).upper(), name, module, host,
                                  statement.line)
            procedures.append(procedure)
            source.procedures.append(procedure)
            in_module_spec = False
            continue
        if PROCEDURE_END.match(lower):
            if procedures:
                procedures.pop().last = statement.line
            continue
        match = COMMON_BLOCK.match(text_)
        if match:
            block = "/" + match.group(1).upper() + "/"
            for member in split_entities(match.group(2)):
                source.state.append(State(member, "common", name, block,
                                          statement.line))
            continue
        if not (module and in_module_spec):
            continue
        match = TYPE_DEFINITION.match(text_)
        if match:
            type_name = match.group(1).upper()
            source.components.setdefault(type_name, set())
            source.state.append(State(type_name, "type", name, module,
                                      statement.line))
            continue
        if DECLARATION_START.match(lower):
            for variable in declared_names(text_):
                source.state.append(State(variable, "variable", name, module,
                                          statement.line))
    for procedure in procedures:
        procedure.last = procedure.last or len(text.splitlines())


def stem(path: str) -> str:
    """`src/swancom1.ftn` and `src/swancom1.f90` both give `swancom1`."""
    base = path.rsplit("/", 1)[-1]
    for suffix in (".f90.in", ".ftn90", ".ftn", ".f90", ".f"):
        if base.endswith(suffix):
            return base[: -len(suffix)]
    return base


def upstream_files(commit: str) -> dict[str, str]:
    """The raw upstream Fortran sources at `commit`, by path.

    Upstream ships each source twice: the raw `.ftn`/`.ftn90` and a generated
    `.f`/`.f90` copy. Only the raw one counts; a `.f90` without a raw
    counterpart is a source of its own.
    """
    archive = subprocess.run(
        ["git", "-C", str(REPOSITORY), "archive", "--format=tar", commit, "src"],
        capture_output=True,
        check=False,
    )
    if archive.returncode != 0:
        raise SystemExit(
            f"cannot read the upstream source at {commit[:12]} from git: "
            f"{archive.stderr.decode(errors='replace').strip()}\n"
            "A shallow clone lacks it; fetch the full history (fetch-depth: 0)."
        )
    files: dict[str, str] = {}
    with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as tar:
        for member in tar.getmembers():
            if member.isfile() and re.search(r"\.(ftn90|ftn|f90|f)$", member.name):
                data = tar.extractfile(member)
                assert data is not None
                files[member.name] = data.read().decode("latin-1")
    raw = {stem(path) for path in files if path.endswith((".ftn", ".ftn90"))}
    return {
        path: text
        for path, text in files.items()
        if path.endswith((".ftn", ".ftn90")) or stem(path) not in raw
    }


def read_upstream(commit: str) -> Source:
    markers = switch_markers()
    source = Source()
    for path, text in sorted(upstream_files(commit).items()):
        read_file(source, path, text, path.endswith((".ftn", ".f")), markers)
    return source


def read_fork() -> Source:
    source = Source()
    for path in sorted(SOURCE.rglob("*")):
        if not path.is_file() or not path.name.endswith((".f90", ".f90.in")):
            continue
        relative = path.relative_to(REPOSITORY).as_posix()
        read_file(source, relative, path.read_text(encoding="latin-1"), False, ())
    return source


def pinned_commit() -> str:
    return json.loads(DELTA_FILE.read_text(encoding="utf-8"))["upstream"]["commit"]


# ---------------------------------------------------------------------------
# The map
# ---------------------------------------------------------------------------


def by_name(items):
    result: dict[str, list] = {}
    for item in items:
        result.setdefault(item.name, []).append(item)
    return result


def fork_procedure_locations(fork: Source, name: str) -> list[Procedure]:
    return by_name(fork.procedures).get(name.upper(), [])


@dataclass
class Finding:
    kind: str
    name: str
    message: str

    def __str__(self) -> str:
        return f"{self.kind} {self.name}: {self.message}"


def classify_procedures(upstream: Source, fork: Source, entries: dict,
                        reasons: dict) -> tuple[dict[str, str], list[Finding]]:
    """Status per upstream procedure name, and every problem with the map."""
    problems: list[Finding] = []
    status: dict[str, str] = {}
    fork_names = by_name(fork.procedures)
    for name, defined in sorted(by_name(upstream.procedures).items()):
        upstream_stems = {stem(p.file) for p in defined}
        found = fork_names.get(name, [])
        #  every definition counts: a second one elsewhere, for instance in a
        #  build variant, needs an entry of its own
        kept = bool(found) and all(stem(p.file) in upstream_stems for p in found)
        entry = entries.get(name)
        if kept:
            status[name] = "kept"
            if entry is not None:
                problems.append(Finding("procedure", name,
                    "is in the fork file of the same name; remove its map entry"))
            continue
        if entry is None:
            where = ", ".join(sorted({p.file for p in found})) or "nowhere"
            problems.append(Finding("procedure", name,
                f"upstream {', '.join(sorted({p.file for p in defined}))}, "
                f"fork {where}: needs a map entry"))
            continue
        status[name] = entry.get("status", "?")
        problems.extend(check_entry("procedure", name, entry, reasons,
                                    PROCEDURE_STATUSES))
        files = set(entry.get("fork", []))
        found_files = {p.file for p in found}
        if entry.get("status") == "moved":
            if not found:
                problems.append(Finding("procedure", name,
                    "map says moved, but the fork does not define it"))
            elif files != found_files:
                problems.append(Finding("procedure", name,
                    f"map says {sorted(files)}, fork defines it in {sorted(found_files)}"))
        elif entry.get("status") == "removed":
            if found:
                problems.append(Finding("procedure", name,
                    f"map says removed, but the fork defines it in {sorted(found_files)}"))
        elif entry.get("status") == "variant":
            if files != found_files:
                problems.append(Finding("procedure", name,
                    f"map says {sorted(files)}, fork defines it in {sorted(found_files)}"))
            elif not any(stem(f) in upstream_stems for f in found_files):
                problems.append(Finding("procedure", name,
                    "map says variant, but no version is in the file of its upstream name"))
        elif entry.get("status") in ("renamed", "replaced"):
            for target in entry.get("by", []):
                if not resolve_target(fork, target):
                    problems.append(Finding("procedure", name,
                        f"replacement {target!r} not found in the fork"))
    for name in sorted(set(entries) - set(by_name(upstream.procedures))):
        problems.append(Finding("procedure", name,
            "has a map entry but is not an upstream procedure"))
    return status, problems


def resolve_target(fork: Source, target: str) -> bool:
    """A replacement is `NAME` (procedure), `module MOD`, `type T%c` or `file F`."""
    kind, _, value = target.partition(" ")
    if kind == "module":
        return value.upper() in fork.modules
    if kind == "file":
        return (REPOSITORY / value).is_file()
    if kind == "type":
        type_name, _, component = value.partition("%")
        components = fork.components.get(type_name.upper())
        return components is not None and (not component or component.upper() in components)
    if kind == "variable":
        module, _, variable = value.partition("::")
        return any(s.module == module.upper() and s.name == variable.upper()
                   for s in fork.state)
    return bool(by_name(fork.procedures).get(target.upper()))


def check_entry(kind: str, name: str, entry: dict, reasons: dict,
                allowed: set[str]) -> list[Finding]:
    problems = []
    if entry.get("status") not in allowed:
        problems.append(Finding(kind, name,
            f"status {entry.get('status')!r} is not one of {sorted(allowed)}"))
    reason = entry.get("reason")
    if not reason:
        problems.append(Finding(kind, name, "has no reason"))
    elif reason not in reasons and len(reason.split()) < 4:
        problems.append(Finding(kind, name,
            f"reason {reason!r} is neither a known reason nor a sentence"))
    if entry.get("status") in ("renamed", "replaced", "component") and not entry.get("by"):
        problems.append(Finding(kind, name, "says what replaced it, but not where ('by')"))
    return problems


def state_key(item: State) -> str:
    return f"{item.module}::{item.name}"


def classify_state(upstream: Source, fork: Source, entries: dict, modules: dict,
                   reasons: dict) -> tuple[dict[str, str], list[Finding]]:
    problems: list[Finding] = []
    status: dict[str, str] = {}
    fork_state = [s for s in fork.state if s.kind != "common"]
    declared: dict[str, set[str]] = {}
    for item in fork_state:
        declared.setdefault(item.name, set()).add(item.module)
    seen = set()
    for item in upstream.state:
        key = state_key(item)
        if key in seen:
            continue
        seen.add(key)
        entry = entries.get(key)
        homes = declared.get(item.name, set())
        if item.kind != "common" and item.module in homes:
            status[key] = "kept"
            if entry is not None:
                problems.append(Finding("state", key,
                    "is still declared in the module of the same name; remove its map entry"))
            continue
        if entry is None and len(homes) == 1 and item.kind != "common":
            module_note = modules.get(item.module)
            if module_note is None:
                problems.append(Finding("state", key,
                    f"moved to {next(iter(homes))}, but upstream module "
                    f"{item.module} has no note in 'modules'"))
            status[key] = "moved"
            continue
        if entry is None:
            where = ", ".join(sorted(homes)) or "no fork module"
            problems.append(Finding("state", key,
                f"declared in {where}: needs a map entry"))
            continue
        status[key] = entry.get("status", "?")
        problems.extend(check_entry("state", key, entry, reasons, STATE_STATUSES))
        if entry.get("status") == "moved":
            target = entry.get("module", "").upper()
            if target not in homes:
                problems.append(Finding("state", key,
                    f"map says module {target}, fork declares it in {sorted(homes) or 'none'}"))
        elif entry.get("status") == "removed" and homes:
            problems.append(Finding("state", key,
                f"map says removed, but {sorted(homes)} declare a variable of that name"))
        elif entry.get("status") in ("renamed", "replaced", "component"):
            for target in entry.get("by", []):
                if not resolve_target(fork, target):
                    problems.append(Finding("state", key,
                        f"replacement {target!r} not found in the fork"))
    upstream_modules = {s.module for s in upstream.state}
    for key in sorted(set(entries) - seen):
        problems.append(Finding("state", key,
            "has a map entry but is not upstream shared state"))
    for module in sorted(set(modules) - upstream_modules):
        problems.append(Finding("module", module,
            "has a note in 'modules' but is not an upstream module with state"))
    for module, note in sorted(modules.items()):
        if not note.get("reason"):
            problems.append(Finding("module", module, "has no reason"))
    return status, problems


def check(upstream: Source, fork: Source, data: dict) -> list[Finding]:
    reasons = data.get("reasons", {})
    problems: list[Finding] = []
    for key, text in reasons.items():
        if len(text.split()) < 8:
            problems.append(Finding("reason", key, "is too short to explain anything"))
    _, found = classify_procedures(upstream, fork, data.get("procedures", {}), reasons)
    problems += found
    _, found = classify_state(upstream, fork, data.get("state", {}),
                              data.get("modules", {}), reasons)
    problems += found
    used = {e.get("reason") for e in data.get("procedures", {}).values()}
    used |= {e.get("reason") for e in data.get("state", {}).values()}
    used |= {e.get("reason") for e in data.get("modules", {}).values()}
    for key in sorted(set(reasons) - used):
        problems.append(Finding("reason", key, "is not used by any entry"))
    return problems


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------


def describe_procedure(name: str, upstream: Source, fork: Source, data: dict) -> str:
    name = name.upper()
    defined = by_name(upstream.procedures).get(name, [])
    found = fork_procedure_locations(fork, name)
    lines = []
    up = ", ".join(sorted({f"{p.file}:{p.first}" for p in defined})) or "not upstream"
    lines.append(f"{name} (upstream {up})")
    for p in found:
        scope = p.module.lower() or "external"
        if p.host:
            scope += f", inside {p.host}"
        lines.append(f"    fork {p.file}:{p.first} ({scope})")
    entry = data.get("procedures", {}).get(name)
    if entry:
        lines.append(f"    {entry['status']}: {reason_text(entry, data)}")
        for target in entry.get("by", []):
            lines.append(f"    by {target}")
    elif not found:
        lines.append("    not in the fork, and no map entry")
    return "\n".join(lines)


def reason_text(entry: dict, data: dict) -> str:
    reason = entry.get("reason", "")
    text = data.get("reasons", {}).get(reason, reason)
    note = entry.get("note")
    return f"{text} {note}" if note else text


def describe_state(key: str, upstream: Source, fork: Source, data: dict) -> list[str]:
    module, _, name = key.partition("::")
    lines = []
    homes = [s for s in fork.state if s.name == name and s.kind != "common"]
    entry = data.get("state", {}).get(key)
    lines.append(f"{key}")
    for s in homes:
        lines.append(f"    fork {s.file}:{s.line} ({s.module.lower()})")
    if entry:
        lines.append(f"    {entry['status']}: {reason_text(entry, data)}")
        for target in entry.get("by", []):
            lines.append(f"    by {target}")
    elif module in data.get("modules", {}) and homes and module not in {s.module for s in homes}:
        lines.append(f"    moved: {reason_text(data['modules'][module], data)}")
    return lines


def command_where(names: list[str], upstream: Source, fork: Source, data: dict) -> None:
    upstream_state = by_name(upstream.state)
    for name in names:
        upper = name.upper()
        printed = False
        if "::" in upper:
            print("\n".join(describe_state(upper, upstream, fork, data)))
            continue
        if by_name(upstream.procedures).get(upper) or fork_procedure_locations(fork, upper):
            print(describe_procedure(upper, upstream, fork, data))
            printed = True
        for item in upstream_state.get(upper, []):
            print("\n".join(describe_state(state_key(item), upstream, fork, data)))
            printed = True
        if not printed:
            print(f"{upper}: neither an upstream procedure nor upstream shared state")


HUNK = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@")


def enclosing(procedures: list[Procedure], file: str, line: int) -> Procedure | None:
    """The innermost procedure of `file` that contains `line`."""
    best = None
    for p in procedures:
        if p.file == file and p.first <= line <= p.last:
            if best is None or p.first >= best.first:
                best = p
    return best


def switch_marker(line: str) -> str:
    """The switch prefix a raw upstream line starts with, or ""."""
    for marker in switch_markers():
        if line.startswith(marker):
            return marker.lower()
    return ""


def normalise(line: str, fixed_form: bool) -> str:
    """A source line reduced to what survives the move to free form.

    Comments, fixed-form columns 73 and up (upstream version tags),
    continuation marks, case and all blanks are dropped, so that a statement
    is recognised however the fork has laid it out.
    """
    for marker in switch_markers():
        if line.startswith(marker):
            line = " " * len(marker) + line[len(marker):]
            break
    if fixed_form:
        if line[:1] in ("c", "C", "*"):
            return ""
        line = line[:72]
        if len(line) > 5 and line[5] not in " 0" and line[:5].strip() == "":
            line = line[6:]
    stripped = line.lstrip()
    if stripped[:5].lower() == "!$omp":
        #  An OpenMP directive is code, not a comment.
        line = "!$omp" + strip_comment(stripped[5:])
    elif stripped[:2] == "!$":
        #  A line that is compiled only with OpenMP.
        line = strip_comment(stripped[2:])
    else:
        line = strip_comment(line)
    line = line.replace("&", "")
    return re.sub(r"\s+", "", line).lower()


@dataclass
class Hunk:
    file: str  # the new path, or the old one for a removed file
    old_file: str  # "" for an added file
    new_file: str  # "" for a removed file
    old_start: int
    old_count: int
    new_start: int
    new_count: int
    removed: list[str] = field(default_factory=list)
    added: list[str] = field(default_factory=list)


def read_hunks(old: str, new: str) -> list[Hunk]:
    """The hunks of the whole upstream diff OLD..NEW, without context.

    The whole tree, not only src/: a release can change the build in the
    top-level CMakeLists.txt or in switch.pl, as patch B did.
    """
    diff = subprocess.run(
        ["git", "-C", str(REPOSITORY), "diff", "-U0", "--no-renames", old, new],
        capture_output=True, text=True, check=True, errors="replace",
    ).stdout
    hunks: list[Hunk] = []
    file_old = file_new = ""
    for line in diff.splitlines():
        if line.startswith("--- "):
            file_old = line[6:] if line.startswith("--- a/") else ""
        elif line.startswith("+++ "):
            file_new = line[6:] if line.startswith("+++ b/") else ""
        elif (match := HUNK.match(line)):
            hunks.append(Hunk(file_new or file_old, file_old, file_new,
                              int(match.group(1)), int(match.group(2) or 1),
                              int(match.group(3)), int(match.group(4) or 1)))
        elif hunks and line.startswith("-"):
            hunks[-1].removed.append(line[1:])
        elif hunks and line.startswith("+"):
            hunks[-1].added.append(line[1:])
    return hunks


def fork_files_for(target: str, fork: Source, data: dict) -> set[str]:
    """Fork files that hold an upstream procedure or its replacements."""
    files = {p.file for p in fork_procedure_locations(fork, target)}
    entry = data.get("procedures", {}).get(target, {})
    for replacement in entry.get("by", []):
        files |= {p.file for p in fork_procedure_locations(fork, replacement)}
    return files


def switch_files(hunks: list[Hunk]) -> set[str]:
    """Fork files that took over the switch-prefixed lines of these hunks.

    The fork has no switch-prefixed source: each upstream switch became a
    build option, and doc/switch-manifest.json lists the files that now
    hold the code of each one.
    """
    manifest = json.loads(SWITCH_MANIFEST.read_text(encoding="utf-8"))["markers"]
    files: set[str] = set()
    for hunk in hunks:
        for line in hunk.added + hunk.removed:
            for marker in switch_markers():
                if line.startswith(marker):
                    for name in manifest[marker].get("replacement_files", []):
                        if name.endswith(".f90"):
                            files.add(f"src/{name}")
                    break
    return {name for name in files if (REPOSITORY / name).is_file()}


def presence(hunks: list[Hunk], files: set[str]) -> str:
    """How much of the change is already in the fork files.

    Counts the added upstream lines that occur in the fork, and the removed
    ones that still do. Short lines (END DO, ELSE) say nothing and are left
    out. A line can match by accident, and a line the fork rewrote will not
    match, so this is a pointer for the reader, not a verdict.
    """
    text: set[str] = set()
    for name in files:
        for line in (REPOSITORY / name).read_text(encoding="latin-1").splitlines():
            text.add(normalise(line, False))
    added_keys: list[tuple[str, str]] = []
    removed_keys: list[tuple[str, str]] = []
    for hunk in hunks:
        fixed = hunk.file.endswith(".ftn")
        added_keys += [(switch_marker(line), normalise(line, fixed)) for line in hunk.added]
        removed_keys += [(switch_marker(line), normalise(line, fixed)) for line in hunk.removed]
    #  A line that is both removed and added, under the same switch, was only
    #  moved or realigned. A line that moved to another switch did change:
    #  another build variant runs it now.
    unchanged = set(added_keys) & set(removed_keys)
    added_keys = [k for k in added_keys if len(k[1]) >= 12 and k not in unchanged]
    removed_keys = [k for k in removed_keys if len(k[1]) >= 12 and k not in unchanged]
    added, removed = len(added_keys), len(removed_keys)
    added_found = sum(key in text for _, key in added_keys)
    removed_found = sum(key in text for _, key in removed_keys)
    return (f"added lines found in the fork {added_found}/{added}, "
            f"removed lines still in the fork {removed_found}/{removed}")


def split_by_procedure(hunk: Hunk, old_source: Source,
                       new_source: Source) -> dict[str, Hunk]:
    """The lines of one hunk, per procedure that contains them.

    A hunk can span the end of one procedure and the start of the next; each
    procedure gets only its own lines. Removed lines are placed in the old
    version, added lines in the new one, and lines outside any procedure
    under "declarations of FILE".
    """
    parts: dict[str, Hunk] = {}

    def part(target: str) -> Hunk:
        if target not in parts:
            parts[target] = Hunk(hunk.file, hunk.old_file, hunk.new_file,
                                 hunk.old_start, hunk.old_count,
                                 hunk.new_start, hunk.new_count)
        return parts[target]

    for offset, line in enumerate(hunk.removed):
        p = enclosing(old_source.procedures, hunk.old_file, hunk.old_start + offset)
        part(p.name if p else f"declarations of {hunk.old_file}").removed.append(line)
    for offset, line in enumerate(hunk.added):
        p = enclosing(new_source.procedures, hunk.new_file, hunk.new_start + offset)
        part(p.name if p else f"declarations of {hunk.new_file}").added.append(line)
    return parts


def command_changes(old: str, new: str, fork: Source, data: dict,
                    show_presence: bool) -> None:
    """List every hunk of an upstream change with its place in the fork.

    A hunk is placed in the innermost procedure that contains it, in the old
    version for changed and removed lines and in the new version for added
    lines. Hunks outside any procedure are module declarations or file-level
    text. Every other changed file (build files, scripts, documentation, and
    Fortran outside src/) is listed for a check by hand.
    """
    old_source = read_upstream(old)
    new_source = read_upstream(new)
    raw_old = set(upstream_files(old))
    raw_new = set(upstream_files(new))
    groups: dict[str, list[Hunk]] = {}
    for hunk in read_hunks(old, new):
        if (not hunk.file.startswith("src/")
                or not re.search(r"\.(ftn90|ftn|f90|f)$", hunk.file)):
            groups.setdefault(f"file {hunk.file}", []).append(hunk)
            continue
        if hunk.file not in raw_old | raw_new:
            continue  # generated copy of a raw source
        parts = split_by_procedure(hunk, old_source, new_source)
        for target, piece in parts.items():
            groups.setdefault(target, []).append(piece)
    for target, hunks in groups.items():
        count = f"{len(hunks)} hunk{'s' if len(hunks) != 1 else ''}"
        print(f"== {target}  ({count})")
        if target.startswith("file "):
            print("    not Fortran source in src/: compare with the fork by hand")
            continue
        if target.startswith("declarations of "):
            print("    module declarations or file-level text; look up changed "
                  "names with 'where'")
            if show_presence:
                upstream_file = target.split(" of ", 1)[1]
                modules = {s.module for s in old_source.state + new_source.state
                           if s.file == upstream_file}
                files = {s.file for s in fork.state if s.module in modules}
                files |= {f for m, f in fork.modules.items() if m in modules}
                files |= switch_files(hunks)
                if files:
                    print(f"    {presence(hunks, files)} ({', '.join(sorted(files))})")
            continue
        in_new = bool(by_name(new_source.procedures).get(target))
        in_old = bool(by_name(old_source.procedures).get(target))
        description = describe_procedure(target, new_source if in_new else old_source,
                                         fork, data)
        print("    " + description.replace("\n", "\n    "))
        if not in_new:
            print("    removed upstream in this change")
        elif not in_old:
            print("    added upstream in this change")
        if show_presence:
            files = fork_files_for(target, fork, data) | switch_files(hunks)
            if files:
                print(f"    {presence(hunks, files)}")


def command_summary(upstream: Source, fork: Source, data: dict) -> None:
    reasons = data.get("reasons", {})
    status, _ = classify_procedures(upstream, fork, data.get("procedures", {}), reasons)
    counts: dict[str, int] = {}
    for value in status.values():
        counts[value] = counts.get(value, 0) + 1
    print("procedures:", ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))
    status, _ = classify_state(upstream, fork, data.get("state", {}),
                               data.get("modules", {}), reasons)
    counts = {}
    for value in status.values():
        counts[value] = counts.get(value, 0) + 1
    print("shared state:", ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("check", help="check the map against both sources")
    where = sub.add_parser("where", help="fork location of upstream names")
    where.add_argument("names", nargs="+")
    changes = sub.add_parser("changes", help="map an upstream change to the fork")
    changes.add_argument("old")
    changes.add_argument("new")
    changes.add_argument("--present", action="store_true",
                         help="count how much of each change the fork already has")
    sub.add_parser("summary", help="counts per status")
    parser.add_argument("--map", type=Path, default=MAP_FILE)
    parser.add_argument("--upstream", default=None,
                        help="upstream commit (default: the pin in upstream-delta.json)")
    arguments = parser.parse_args()

    data = json.loads(arguments.map.read_text(encoding="utf-8"))
    fork = read_fork()
    if arguments.command == "changes":
        command_changes(arguments.old, arguments.new, fork, data, arguments.present)
        return 0
    commit = arguments.upstream or pinned_commit()
    recorded = str(data.get("upstream", ""))
    if len(recorded) < 7 or not commit.startswith(recorded):
        print(f"the map describes upstream {data.get('upstream')!r}, "
              f"but the pin is {commit[:12]}")
        if arguments.command == "check":
            return 1
    upstream = read_upstream(commit)
    if arguments.command == "check":
        problems = check(upstream, fork, data)
        for problem in problems:
            print(problem)
        if problems:
            print(f"{len(problems)} problem(s) in {arguments.map.name}")
            return 1
        print("upstream map is complete")
        return 0
    if arguments.command == "where":
        command_where(arguments.names, upstream, fork, data)
    elif arguments.command == "summary":
        command_summary(upstream, fork, data)
    return 0


if __name__ == "__main__":
    sys.exit(main())
