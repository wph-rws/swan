#!/usr/bin/env python3
"""Run gfortran linting on a configured SWAN source variant.

The editable files in ``src`` are templates containing SWAN's historical
``!NCF``/``!NNCF``-style switches.  Some statements are therefore incomplete
until switch.py has selected a build variant.  The VS Code Fortran extension
invokes this wrapper as if it were gfortran; source templates are configured in
a temporary directory first, while all other compiler invocations and
arguments are passed through unchanged.  Without it the editor lints the raw
template and reports syntax errors that no build ever sees.

Which switches it selects is read from the CMake cache in ``build``, so lint
diagnostics follow the configuration you actually build.  Point
``SWAN_LINT_BUILD_DIR`` elsewhere to lint a different one.

``.vscode`` is deliberately not tracked, so wire this up yourself in
``.vscode/settings.json``::

    {
      "fortran.linter.compiler": "gfortran",
      "fortran.linter.compilerPath": "${workspaceFolder}/scripts/gfortran_swan_lint.py",
      "fortran.linter.includePaths": [
        "${workspaceFolder}/build/lint-mod",
        "${workspaceFolder}/build/mod",
        "/usr/include"
      ],
      "fortran.linter.modOutput": "${workspaceFolder}/build/lint-mod",
      "fortran.linter.extraArgs": [
        "-std=f2018",
        "-fimplicit-none",
        "-fno-second-underscore",
        "-ffree-line-length-none"
      ]
    }

It needs a configured ``build`` directory to read the cache and to find the
modules the sources import.

gfortran searches the current directory for modules before any ``-I``, and the
editor runs the linter from the workspace root.  A stray ``.mod`` there would
therefore shadow the build's own module and produce errors that correspond to
nothing in the source.  The compiler is run from the scratch directory instead,
so only the module paths that were asked for can be found, and a run that is not
given ``-J`` leaves its own ``.mod`` files in that scratch directory rather than
in the repository.  Path arguments are made absolute first, since they are
written relative to the editor's working directory.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SOURCE_DIR = ROOT / "src"
BUILD_DIR = Path(os.environ.get("SWAN_LINT_BUILD_DIR", ROOT / "build"))
CACHE = BUILD_DIR / "CMakeCache.txt"
LINT_MODULE_DIR = BUILD_DIR / "lint-mod"
NETCDF_MODULE_DIR = ROOT / "build-netcdf" / "mod"
NETCDF_ONLY_SOURCES = {"agioncmd.f90", "nctablemd.f90", "swn_outnc.f90"}

#  Options whose path follows as the next argument, and those that carry it
#  attached.  Both forms have to be resolved before the compiler is moved to a
#  different working directory.
SEPARATE_PATH_OPTIONS = ("-I", "-J", "-o", "-isystem", "-include",
                         "-fintrinsic-modules-path")
ATTACHED_PATH_OPTIONS = ("-I", "-J")


def enabled_cmake_options() -> set[str]:
    """Return enabled BOOL options from the lint build's CMake cache."""
    if not CACHE.is_file():
        return set()

    enabled: set[str] = set()
    for line in CACHE.read_text(encoding="utf-8", errors="replace").splitlines():
        match = re.fullmatch(r"([^:#]+):BOOL=(ON|OFF)", line)
        if match and match.group(2) == "ON":
            enabled.add(match.group(1))
    return enabled


def switch_arguments() -> list[str]:
    """Mirror the switch selection in src/CMakeLists.txt."""
    enabled = enabled_cmake_options()
    arguments: list[str] = []

    if "MPI" in enabled or "JAC" in enabled:
        arguments.append("-mpi")
    option_switches = {
        "JAC": "-jac",
        "METIS": "-metis",
        "FFRO": "-fixfront",
        "NETCDF": "-netcdf",
        "MATL4": "-matl4",
    }
    arguments.extend(
        switch
        for option, switch in option_switches.items()
        if option in enabled
    )
    return arguments


def source_templates(arguments: list[str]) -> list[tuple[int, Path]]:
    """Find compiler arguments that name editable SWAN source templates."""
    templates: list[tuple[int, Path]] = []
    for index, argument in enumerate(arguments):
        if not argument.lower().endswith(".f90"):
            continue
        candidate = Path(argument)
        if not candidate.is_absolute():
            candidate = Path.cwd() / candidate
        try:
            candidate = candidate.resolve()
            candidate.relative_to(SOURCE_DIR)
        except (OSError, ValueError):
            continue
        if candidate.is_file():
            templates.append((index, candidate))
    return templates


def absolute_path(value: str, base: Path) -> str:
    path = Path(value)
    return value if path.is_absolute() else str(base / path)


def absolute_path_arguments(arguments: list[str], base: Path) -> list[str]:
    """Resolve path arguments against ``base`` so the compiler can move cwd.

    Anything that is not recognizably a path is passed through untouched: a bare
    argument is only rewritten when it names something that exists, so option
    values such as ``-std=f2018`` are left alone.
    """
    resolved: list[str] = []
    expecting_path = False
    for argument in arguments:
        if expecting_path:
            resolved.append(absolute_path(argument, base))
            expecting_path = False
        elif argument in SEPARATE_PATH_OPTIONS:
            expecting_path = True
            resolved.append(argument)
        elif any(argument.startswith(option) and len(argument) > len(option)
                 for option in ATTACHED_PATH_OPTIONS):
            option = argument[:2]
            resolved.append(option + absolute_path(argument[2:], base))
        elif not argument.startswith("-") and (base / argument).exists():
            resolved.append(absolute_path(argument, base))
        else:
            resolved.append(argument)
    return resolved


def main() -> int:
    # The editor passes this directory through both -I and -J.  Create it here
    # as well as during local setup so deleting the build tree cannot bring the
    # missing-include warning back on the next lint invocation.
    LINT_MODULE_DIR.mkdir(parents=True, exist_ok=True)

    compiler = os.environ.get("SWAN_GFORTRAN") or shutil.which("gfortran")
    if not compiler:
        print("gfortran_swan_lint.py: gfortran not found", file=sys.stderr)
        return 127

    compiler_arguments = sys.argv[1:]
    templates = source_templates(compiler_arguments)
    if not templates:
        return subprocess.run([compiler, *compiler_arguments], check=False).returncode

    with tempfile.TemporaryDirectory(prefix="swan-lint-") as temporary:
        temporary_dir = Path(temporary)
        sources = [source for _, source in templates]
        configured = subprocess.run(
            [
                sys.executable,
                str(ROOT / "switch.py"),
                *switch_arguments(),
                "--output-dir",
                str(temporary_dir),
                *(str(source) for source in sources),
            ],
            capture_output=True,
            text=True,
            check=False,
        )
        if configured.returncode:
            sys.stdout.write(configured.stdout)
            sys.stderr.write(configured.stderr)
            return configured.returncode

        compiler_arguments = absolute_path_arguments(compiler_arguments, Path.cwd())

        path_mapping: dict[str, str] = {}
        for index, source in templates:
            lint_source = temporary_dir / source.name
            compiler_arguments[index] = str(lint_source)
            path_mapping[str(lint_source)] = str(source)

        needs_netcdf_modules = any(
            source.name in NETCDF_ONLY_SOURCES for source in sources
        )
        optional_modules = (
            ["-I", str(NETCDF_MODULE_DIR)]
            if needs_netcdf_modules and NETCDF_MODULE_DIR.is_dir()
            else []
        )
        #  Running from the scratch directory keeps a stray .mod in the
        #  repository root out of the module search path, and keeps any .mod
        #  this run writes without a -J out of the repository.
        result = subprocess.run(
            [compiler, *optional_modules, *compiler_arguments],
            cwd=temporary_dir,
            capture_output=True,
            text=True,
            check=False,
        )
        output = result.stdout
        errors = result.stderr
        for temporary_path, source_path in path_mapping.items():
            output = output.replace(temporary_path, source_path)
            errors = errors.replace(temporary_path, source_path)
        sys.stdout.write(output)
        sys.stderr.write(errors)
        return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
