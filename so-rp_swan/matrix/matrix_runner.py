#!/usr/bin/env python3
"""Prepare and run the manifest-driven so-rp SWAN regression matrix."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile
import time
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Iterable

import bg2
import deck

ROOT = bg2.ROOT
MANIFEST = Path(__file__).with_name("conditions.json")
DEFAULT_OUTPUT_ROOT = Path(__file__).with_name("runs")
EXPECTED_OUTPUTS = (
    "PRINT",
    "norm_end",
    "scaloost_rp.mat",
    "uitvoerpunten.tab",
    "uitvoerpunten.sp1",
    "uitvoerpunten.sp2",
)
REQUIRED_COVERAGE = frozenset(
    {
        "reference",
        "low_wind",
        "low_water",
        "high_water",
        "osk_open",
        "osk_closed",
        "sheltered",
        "exposed",
        "sector_boundary",
    }
)


@dataclass(frozen=True)
class ManifestCondition:
    condition: deck.Condition
    description: str
    tags: tuple[str, ...]


@dataclass(frozen=True)
class Target:
    target_id: str
    description: str
    executable: Path
    physics: str
    swan_family: str
    library_directories: tuple[Path, ...]


@dataclass(frozen=True)
class Manifest:
    description: str
    conditions: tuple[ManifestCondition, ...]
    targets: tuple[Target, ...]


def _strict_keys(data: dict, required: set[str], optional: set[str], context: str) -> None:
    missing = sorted(required - data.keys())
    unknown = sorted(data.keys() - required - optional)
    if missing or unknown:
        details = []
        if missing:
            details.append(f"missing {missing}")
        if unknown:
            details.append(f"unknown {unknown}")
        raise ValueError(f"{context}: {', '.join(details)}")


def _resolve_from_root(value: str) -> Path:
    path = Path(value)
    return path if path.is_absolute() else (ROOT / path).resolve()


def load_manifest(path: Path | str = MANIFEST) -> Manifest:
    path = Path(path)
    try:
        raw = json.loads(path.read_text())
    except FileNotFoundError:
        raise FileNotFoundError(f"missing condition manifest: {path}") from None
    except json.JSONDecodeError as error:
        raise ValueError(f"{path}: invalid JSON: {error}") from error

    _strict_keys(
        raw,
        {"schema_version", "description", "conditions", "targets"},
        set(),
        str(path),
    )
    if raw["schema_version"] != 1:
        raise ValueError(f"{path}: unsupported schema_version {raw['schema_version']!r}")
    if not 8 <= len(raw["conditions"]) <= 12:
        raise ValueError(f"{path}: expected 8-12 conditions, got {len(raw['conditions'])}")

    conditions: list[ManifestCondition] = []
    for index, item in enumerate(raw["conditions"]):
        context = f"{path}: conditions[{index}]"
        _strict_keys(
            item,
            {
                "id",
                "description",
                "direction_deg",
                "wind_speed_ms",
                "water_level_cm",
                "osk_state",
                "tags",
            },
            set(),
            context,
        )
        condition = deck.Condition(
            item["id"],
            item["direction_deg"],
            item["wind_speed_ms"],
            item["water_level_cm"],
            item["osk_state"],
        )
        tags = tuple(item["tags"])
        if not tags or any(not isinstance(tag, str) for tag in tags):
            raise ValueError(f"{context}: tags must be a nonempty string list")
        conditions.append(ManifestCondition(condition, item["description"], tags))

    ids = [item.condition.condition_id for item in conditions]
    if len(ids) != len(set(ids)):
        raise ValueError(f"{path}: condition ids are not unique")
    coverage = {tag for item in conditions for tag in item.tags}
    missing_coverage = sorted(REQUIRED_COVERAGE - coverage)
    if missing_coverage:
        raise ValueError(f"{path}: matrix is missing coverage tags {missing_coverage}")

    targets: list[Target] = []
    for index, item in enumerate(raw["targets"]):
        context = f"{path}: targets[{index}]"
        _strict_keys(
            item,
            {"id", "description", "executable", "physics", "swan_family"},
            {"library_directories"},
            context,
        )
        if item["physics"] not in deck.PHYSICS:
            raise ValueError(f"{context}: unknown physics {item['physics']!r}")
        if item["swan_family"] not in ("modern", "bss"):
            raise ValueError(f"{context}: swan_family must be 'modern' or 'bss'")
        target_id = item["id"]
        if not re.fullmatch(r"[a-z0-9][a-z0-9_-]*", target_id):
            raise ValueError(f"{context}: invalid target id {target_id!r}")
        targets.append(
            Target(
                target_id,
                item["description"],
                _resolve_from_root(item["executable"]),
                item["physics"],
                item["swan_family"],
                tuple(
                    _resolve_from_root(directory)
                    for directory in item.get("library_directories", [])
                ),
            )
        )
    target_ids = [target.target_id for target in targets]
    if len(target_ids) != len(set(target_ids)):
        raise ValueError(f"{path}: target ids are not unique")
    if len(targets) != 4:
        raise ValueError(f"{path}: expected the four reference targets, got {len(targets)}")

    return Manifest(raw["description"], tuple(conditions), tuple(targets))


def select_named(items: Iterable, names: list[str] | None, attribute: str):
    items = tuple(items)
    if not names:
        return items
    by_name = {getattr(item, attribute): item for item in items}
    unknown = sorted(set(names) - by_name.keys())
    if unknown:
        raise ValueError(f"unknown selections for {attribute}: {unknown}")
    return tuple(by_name[name] for name in names)


def executable_overrides(values: list[str]) -> dict[str, Path]:
    overrides: dict[str, Path] = {}
    for value in values:
        target_id, separator, executable = value.partition("=")
        if not separator or not target_id or not executable:
            raise ValueError(
                f"invalid --executable {value!r}; expected TARGET=/absolute/path"
            )
        path = Path(executable)
        if not path.is_absolute():
            raise ValueError(f"--executable path must be absolute: {path}")
        overrides[target_id] = path
    return overrides


def _swaninit_text(family: str) -> str:
    highest = "   99" if family == "bss" else "99999"
    return (
        "    4                                   version of initialisation file\n"
        "Delft University of Technology          name of institute\n"
        "    3                                   command file ref. number\n"
        "INPUT                                   command file name\n"
        "    4                                   print file ref. number\n"
        "PRINT                                   print file name\n"
        "    4                                   test file ref. number\n"
        "                                        test file name\n"
        "    6                                   screen ref. number\n"
        f"{highest}                                   highest file ref. number\n"
        "$                                       comment identifier\n"
        "\t                                       TAB character\n"
        "\\                                       dir sep char in input file\n"
        "/                                       dir sep char replacing previous one\n"
        "    1                                   default time coding option\n"
    )


def prepare_run_directory(
    directory: Path,
    condition: ManifestCondition,
    target: Target,
) -> str:
    """Populate a new isolated work directory and return the generated deck."""
    directory.mkdir(parents=True, exist_ok=False)
    for source, destination in (
        (ROOT / "GRID", directory / "GRID"),
        (ROOT / "DEPTH", directory / "DEPTH"),
        (ROOT / "par" / "uitvoerpunten.par", directory / "uitvoerpunten.par"),
    ):
        if not source.is_file():
            raise FileNotFoundError(f"missing matrix input: {source}")
        shutil.copy2(source, destination)

    text = deck.generate_deck(condition.condition, physics=target.physics)
    (directory / "INPUT").write_text(text)
    (directory / "so-rp_osk.swn").write_text(text)
    (directory / "swaninit").write_text(_swaninit_text(target.swan_family))
    return text


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _environment(target: Target, threads: int) -> dict[str, str]:
    env = dict(os.environ)
    env["OMP_NUM_THREADS"] = str(threads)
    env["OMP_PLACES"] = "cores"
    env["OMP_PROC_BIND"] = "close"
    library_paths = [str(path) for path in target.library_directories]
    current = env.get("LD_LIBRARY_PATH")
    if current:
        library_paths.append(current)
    if library_paths:
        env["LD_LIBRARY_PATH"] = os.pathsep.join(library_paths)
    return env


def verify_run(directory: Path, returncode: int) -> None:
    if returncode != 0:
        raise RuntimeError(f"SWAN exited with status {returncode}")
    missing = [
        name
        for name in EXPECTED_OUTPUTS
        if not (directory / name).is_file() or (directory / name).stat().st_size == 0
    ]
    if missing:
        raise RuntimeError(f"SWAN run is missing nonempty outputs: {missing}")
    if "Normal end of run" not in (directory / "norm_end").read_text(errors="replace"):
        raise RuntimeError("norm_end does not report a normal end")
    if "STOP" not in (directory / "INPUT").read_text():
        raise RuntimeError("prepared INPUT is incomplete")


def run_one(
    condition: ManifestCondition,
    target: Target,
    output_directory: Path,
    *,
    threads: int,
) -> Path:
    """Run one target/condition pair, publishing the directory only on success."""
    executable = target.executable.resolve()
    if not executable.is_file() or not os.access(executable, os.X_OK):
        raise FileNotFoundError(f"target executable is missing or not executable: {executable}")
    if output_directory.exists():
        raise FileExistsError(f"refusing to overwrite existing run: {output_directory}")
    output_directory.parent.mkdir(parents=True, exist_ok=True)

    temporary = Path(
        tempfile.mkdtemp(
            prefix=f".{output_directory.name}.running-",
            dir=output_directory.parent,
        )
    )
    started = datetime.now(timezone.utc)
    monotonic_start = time.monotonic()
    returncode = -1
    error: str | None = None
    try:
        # mkdtemp created the directory; the preparation contract intentionally
        # requires a new path.
        temporary.rmdir()
        text = prepare_run_directory(temporary, condition, target)
        with (temporary / "screen.log").open("wb") as screen:
            process = subprocess.run(
                [str(executable)],
                cwd=temporary,
                env=_environment(target, threads),
                stdout=screen,
                stderr=subprocess.STDOUT,
                check=False,
            )
        returncode = process.returncode
        verify_run(temporary, returncode)
    except Exception as exception:
        error = str(exception)
        raise
    finally:
        ended = datetime.now(timezone.utc)
        record = {
            "schema_version": 1,
            "condition": {
                **asdict(condition.condition),
                "description": condition.description,
                "tags": condition.tags,
            },
            "target": {
                "id": target.target_id,
                "description": target.description,
                "physics": target.physics,
                "swan_family": target.swan_family,
            },
            "executable": str(executable),
            "executable_sha256": _sha256(executable),
            "input_sha256": (
                hashlib.sha256(text.encode()).hexdigest()
                if "text" in locals()
                else None
            ),
            "omp_num_threads": threads,
            "started_utc": started.isoformat(),
            "ended_utc": ended.isoformat(),
            "duration_seconds": time.monotonic() - monotonic_start,
            "returncode": returncode,
            "error": error,
        }
        if temporary.exists():
            (temporary / "run.json").write_text(json.dumps(record, indent=2) + "\n")

    temporary.rename(output_directory)
    return output_directory


def run_matrix(
    manifest: Manifest,
    *,
    output_root: Path,
    condition_names: list[str] | None,
    target_names: list[str] | None,
    overrides: dict[str, Path],
    threads: int,
    repetitions: int,
) -> list[Path]:
    if threads < 1:
        raise ValueError("threads must be at least 1")
    if repetitions < 1:
        raise ValueError("repetitions must be at least 1")
    conditions = manifest.conditions
    if condition_names:
        by_id = {item.condition.condition_id: item for item in manifest.conditions}
        unknown = sorted(set(condition_names) - by_id.keys())
        if unknown:
            raise ValueError(f"unknown condition ids: {unknown}")
        conditions = tuple(by_id[name] for name in condition_names)
    targets = select_named(manifest.targets, target_names, "target_id")
    unknown_overrides = sorted(set(overrides) - {target.target_id for target in targets})
    if unknown_overrides:
        raise ValueError(f"executable overrides do not select a target: {unknown_overrides}")

    completed: list[Path] = []
    for target in targets:
        if target.target_id in overrides:
            target = Target(
                target.target_id,
                target.description,
                overrides[target.target_id],
                target.physics,
                target.swan_family,
                target.library_directories,
            )
        for condition in conditions:
            for repetition in range(1, repetitions + 1):
                destination = (
                    output_root
                    / target.target_id
                    / condition.condition.condition_id
                    / f"replicate-{repetition:03d}"
                )
                print(
                    f"[{target.target_id}] {condition.condition.condition_id} "
                    f"replicate {repetition}/{repetitions}",
                    flush=True,
                )
                completed.append(
                    run_one(condition, target, destination, threads=threads)
                )
    return completed


def describe_matrix(manifest: Manifest) -> str:
    lines = [
        f"{len(manifest.conditions)} conditions x {len(manifest.targets)} targets "
        f"= {len(manifest.conditions) * len(manifest.targets)} runs",
        "",
        "Targets:",
    ]
    for target in manifest.targets:
        state = "ok" if target.executable.is_file() else "MISSING"
        lines.append(
            f"- {target.target_id}: physics={target.physics}, "
            f"executable={target.executable} [{state}]"
        )
    lines.extend(("", "Conditions:"))
    for item in manifest.conditions:
        condition = item.condition
        lines.append(
            f"- {condition.condition_id}: U={condition.wind_speed_ms} m/s, "
            f"D={condition.direction_deg} deg, L={condition.water_level_cm} cm, "
            f"OSK={condition.osk_state}; tags={','.join(item.tags)}"
        )
        # Planning also validates every generated physics variant.
        for physics in deck.PHYSICS:
            deck.generate_deck(condition, physics=physics)
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    subparsers = parser.add_subparsers(dest="command", required=True)
    subparsers.add_parser("plan", help="validate and describe the complete matrix")
    run_parser = subparsers.add_parser("run", help="execute selected matrix entries")
    run_parser.add_argument("--output-root", type=Path, default=DEFAULT_OUTPUT_ROOT)
    run_parser.add_argument("--condition", action="append", dest="conditions")
    run_parser.add_argument("--target", action="append", dest="targets")
    run_parser.add_argument(
        "--executable",
        action="append",
        default=[],
        metavar="TARGET=/ABSOLUTE/PATH",
    )
    run_parser.add_argument("--threads", type=int, default=8)
    run_parser.add_argument("--repetitions", type=int, default=1)
    args = parser.parse_args()

    try:
        manifest = load_manifest(args.manifest)
        if args.command == "plan":
            print(describe_matrix(manifest))
        else:
            completed = run_matrix(
                manifest,
                output_root=args.output_root,
                condition_names=args.conditions,
                target_names=args.targets,
                overrides=executable_overrides(args.executable),
                threads=args.threads,
                repetitions=args.repetitions,
            )
            print(f"Completed {len(completed)} runs under {args.output_root}")
    except (FileExistsError, FileNotFoundError, RuntimeError, ValueError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
