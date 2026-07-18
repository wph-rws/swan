#!/usr/bin/env python3
"""Apply SWAN compile-time switches to the Fortran source templates."""

from __future__ import annotations

import glob
import os
import sys
from pathlib import Path


SWITCHES = {
    "-esmf": "esmf",
    "-timg": "tim",
    "-jac": "jac",
    "-fixfront": "ffro",
    "-mpi": "mpi",
    "-f95": "f95",
    "-dos": "dos",
    "-unix": "unx",
    "-cray": "cry",
    "-sgi": "sgi",
    "-impi": "imp",
    "-cvis": "cvi",
    "-adcirc": "adc",
    "-coh": "coh",
    "-metis": "met",
    "-netcdf": "ncf",
    "-matl4": "mv4",
}


def parse_arguments(
    arguments: list[str],
) -> tuple[set[str], Path | None, list[str]]:
    enabled: set[str] = set()
    output_directory: Path | None = None
    index = 0
    while index < len(arguments) and arguments[index].startswith("-"):
        option = arguments[index]
        if option == "--output-dir":
            index += 1
            if index >= len(arguments):
                raise SystemExit(
                    f"{Path(sys.argv[0]).name}: --output-dir requires a path"
                )
            output_directory = Path(arguments[index])
            index += 1
            continue
        try:
            enabled.add(SWITCHES[option])
        except KeyError:
            raise SystemExit(f"{Path(sys.argv[0]).name}: unsupported switch {option}")
        index += 1

    if "esmf" in enabled and "adc" in enabled:
        raise SystemExit(f"{Path(sys.argv[0]).name}: -esmf and -adcirc is not supported.")
    if "esmf" in enabled and "met" in enabled:
        raise SystemExit(f"{Path(sys.argv[0]).name}: -esmf and -metis is not supported.")

    return enabled, output_directory, arguments[index:]


def expand_files(patterns: list[str]) -> list[Path]:
    files: list[Path] = []
    for pattern in patterns:
        matches = glob.glob(pattern)
        files.extend(Path(match) for match in matches)
    return files


def output_path(source: Path, output_directory: Path | None) -> Path:
    name = source.name
    if not name.endswith(".f90"):
        raise ValueError(f"unsupported source extension: {source}")
    if output_directory is None:
        raise ValueError(".f90 input requires --output-dir to protect the source")
    return output_directory / name


def transform(line: str, enabled: set[str]) -> str:
    replacements = [
        ("!ESMF", "esmf" in enabled),
        ("!!ESMF", "esmf" not in enabled),
        ("!TIMG", "tim" in enabled),
        ("!JAC", "jac" in enabled),
        ("!WFR", "jac" not in enabled),
        ("!FXFRO", "ffro" in enabled),
        ("!GRAPH", "ffro" not in enabled),
        ("!MPI", "mpi" in enabled),
        ("!F95", "f95" in enabled),
        ("!DOS", "dos" in enabled),
        ("!UNIX", "unx" in enabled),
        ("!/Cray", "cry" in enabled),
        ("!/SGI", "sgi" in enabled),
        ("!/impi", "imp" in enabled),
        ("!CVIS", "cvi" in enabled),
        ("!ADC", "adc" in enabled),
        ("!NADC", "adc" not in enabled),
        ("!COH", "coh" in enabled),
        ("!NCOH", "coh" not in enabled),
        ("!METIS", "met" in enabled),
        ("!NCF", "ncf" in enabled),
        ("!NNCF", "ncf" not in enabled),
        ("!MatL4", "mv4" in enabled),
        ("!MatL5", "mv4" not in enabled),
    ]
    for marker, active in replacements:
        if active and line.startswith(marker):
            line = line[len(marker) :]
    return line


def process(
    source: Path, enabled: set[str], output_directory: Path | None
) -> None:
    destination = output_path(source, output_directory)
    destination.parent.mkdir(parents=True, exist_ok=True)
    with source.open("r", encoding="ascii", newline="") as input_file:
        contents = input_file.readlines()
    with destination.open("w", encoding="ascii", newline="") as output_file:
        output_file.writelines(transform(line, enabled) for line in contents)


def main(arguments: list[str]) -> int:
    enabled, output_directory, patterns = parse_arguments(arguments)
    try:
        for source in expand_files(patterns):
            process(source, enabled, output_directory)
    except (OSError, ValueError) as error:
        print(f"{Path(sys.argv[0]).name}: {error}", file=sys.stderr)
        return os.EX_IOERR
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
