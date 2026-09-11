#!/usr/bin/env python3
"""Run gfortran linting on SWAN sources with the project's own compiler.

The VS Code Fortran extension invokes this wrapper as if it were gfortran.  The
wrapper runs the compiler the ``build`` directory was configured with, from a
scratch directory (see below); all arguments are passed through unchanged
apart from making their paths absolute.  Point ``SWAN_LINT_BUILD_DIR``
elsewhere to follow a different build, or set ``SWAN_GFORTRAN`` to choose the
compiler yourself.

Upstream SWAN keeps ``!NCF``/``!NNCF``-style switch markers in its sources and
resolves them with switch.pl before compiling. In this fork CMake selects whole
backend files, and the ``switch_manifest`` test holds those markers at zero in
``src``, so the files are linted as they are.

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


def cache_lines() -> list[str]:
    """Return the lines of the lint build's CMake cache, or none without one."""
    if not CACHE.is_file():
        return []
    return CACHE.read_text(encoding="utf-8", errors="replace").splitlines()


def cache_compiler() -> str | None:
    """Return the gfortran the lint build was configured with, if any.

    AGENTS.md asks for an explicit gfortran-16, and the plain ``gfortran``
    command may be another version whose warnings differ.  A build configured
    with another compiler family (ifx) is not used: the editor passes
    gfortran options.
    """
    for line in cache_lines():
        match = re.fullmatch(r"CMAKE_Fortran_COMPILER:\w+=(.+)", line)
        if match:
            compiler = match.group(1).strip()
            if "gfortran" in Path(compiler).name and shutil.which(compiler):
                return compiler
            return None
    return None


def swan_sources(arguments: list[str]) -> list[Path]:
    """Find compiler arguments that name SWAN source files in ``src``."""
    sources: list[Path] = []
    for argument in arguments:
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
            sources.append(candidate)
    return sources


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

    compiler = (os.environ.get("SWAN_GFORTRAN") or cache_compiler()
                or shutil.which("gfortran"))
    if not compiler:
        print("gfortran_swan_lint.py: gfortran not found", file=sys.stderr)
        return 127

    compiler_arguments = sys.argv[1:]
    sources = swan_sources(compiler_arguments)
    if not sources:
        return subprocess.run([compiler, *compiler_arguments], check=False).returncode

    compiler_arguments = absolute_path_arguments(compiler_arguments, Path.cwd())
    needs_netcdf_modules = any(
        source.name in NETCDF_ONLY_SOURCES for source in sources
    )
    optional_modules = (
        ["-I", str(NETCDF_MODULE_DIR)]
        if needs_netcdf_modules and NETCDF_MODULE_DIR.is_dir()
        else []
    )
    with tempfile.TemporaryDirectory(prefix="swan-lint-") as temporary:
        #  Running from the scratch directory keeps a stray .mod in the
        #  repository root out of the module search path, and keeps any .mod
        #  this run writes without a -J out of the repository.
        return subprocess.run(
            [compiler, *optional_modules, *compiler_arguments],
            cwd=temporary,
            check=False,
        ).returncode


if __name__ == "__main__":
    raise SystemExit(main())
