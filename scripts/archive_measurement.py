#!/usr/bin/env python3
"""Archiveer één meting herleidbaar.

Verzamelt commit, werkboomhash, compiler, opties, bibliotheken, invoer,
uitvoer en meetresultaat gezamenlijk, inclusief executablehashes, exacte
commando's, ruwe logs en toegang tot externe datasets. Een nieuwe snapshot
maakt een oude meting niet achteraf herleidbaar: behoud haar oorspronkelijke
identiteit of label haar historisch en meet opnieuw.

Gebruik:
    python3 scripts/archive_measurement.py --executable build/bin/swan.exe \\
        --command "ctest --test-dir build" --input INPUT --output-dir out \\
        --archive-dir archives/2026-09-07-serial [--extra-log build.log] \\
        [--dataset "RWS bathymetrie GRID/DEPTH, lokaal so-rp_swan/"]
"""
from __future__ import annotations

import argparse
import hashlib
import json
import platform
import shutil
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def git_info(root: Path) -> dict:
    def run(args: list[str]) -> str:
        result = subprocess.run(
            ["git", *args], cwd=root, capture_output=True, text=True
        )
        return result.stdout.strip() if result.returncode == 0 else f"<git-fout: {' '.join(args)}>"

    return {
        "commit": run(["rev-parse", "HEAD"]),
        "branch": run(["branch", "--show-current"]),
        "worktree_status": run(["status", "--porcelain", "--untracked-files=all"]),
        "upstream_commit": run(["rev-parse", "upstream/main"]),
        "origin_commit": run(["rev-parse", "origin/main"]),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executable", required=True)
    parser.add_argument("--command", required=True, help="exacte meetopdracht")
    parser.add_argument("--input", default=None)
    parser.add_argument("--output-dir", default=None)
    parser.add_argument("--archive-dir", required=True)
    parser.add_argument("--extra-log", action="append", default=[])
    parser.add_argument("--dataset", default="")
    parser.add_argument("--compiler", default="")
    parser.add_argument("--options", default="")
    arguments = parser.parse_args()

    root = Path(__file__).resolve().parent.parent
    archive = Path(arguments.archive_dir)
    archive.mkdir(parents=True, exist_ok=False)
    executable = Path(arguments.executable)
    manifest: dict = {
        "timestamp_utc": datetime.now(timezone.utc).isoformat(),
        "git": git_info(root),
        "platform": platform.platform(),
        "command": arguments.command,
        "compiler": arguments.compiler
        or subprocess.run(
            ["gfortran", "--version"], capture_output=True, text=True
        ).stdout.splitlines()[0]
        if shutil.which("gfortran")
        else arguments.compiler,
        "options": arguments.options,
        "dataset_access": arguments.dataset,
    }
    if executable.is_file():
        manifest["executable"] = str(executable)
        manifest["executable_sha256"] = sha256(executable)
    else:
        print(f"executable ontbreekt: {executable}", file=sys.stderr)
        return 1
    if arguments.input and Path(arguments.input).exists():
        target = archive / ("input" + Path(arguments.input).suffix)
        shutil.copy2(arguments.input, target)
        manifest["input_sha256"] = sha256(target)
    if arguments.output_dir and Path(arguments.output_dir).is_dir():
        shutil.copytree(
            arguments.output_dir, archive / "output", ignore_dangling_symlinks=True
        )
    for log in arguments.extra_log:
        if Path(log).is_file():
            shutil.copy2(log, archive / Path(log).name)
    (archive / "manifest.json").write_text(
        json.dumps(manifest, indent=2, ensure_ascii=False) + "\n"
    )
    print(f"meting gearchiveerd in {archive} ({manifest['executable_sha256'][:12]}…) ")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
