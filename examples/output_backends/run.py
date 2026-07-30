#!/usr/bin/env python3
"""Run the quick case and validate one of SWAN's binary output backends."""

from __future__ import annotations

import argparse
import shutil
import struct
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference, same_results  # noqa: E402


EXPECTED_ROWS = 11
EXPECTED_COLUMNS = 21
EXPECTED_CENTER_HSIG = 0.87872


def require_file(value: str, description: str) -> Path:
    path = Path(value).expanduser().resolve()
    if not path.is_file():
        raise FileNotFoundError(f"{description} not found: {path}")
    return path


def matlab_center(values: tuple[float, ...], rows: int, columns: int) -> float:
    if len(values) != rows * columns:
        raise RuntimeError("MATLAB Hsig matrix has an inconsistent data length")
    # MAT-files store matrices column-major.
    return values[(columns // 2) * rows + rows // 2]


def validate_matlab_v4(path: Path) -> float:
    data = path.read_bytes()
    if len(data) < 20:
        raise RuntimeError("MATLAB-v4 output is shorter than its header")
    byte_order = None
    header = None
    for candidate in (">", "<"):
        values = struct.unpack_from(f"{candidate}5i", data)
        if values[0] == 1010:
            byte_order = candidate
            header = values
            break
    if byte_order is None or header is None:
        raise RuntimeError("MATLAB-v4 output has no SWAN numeric-matrix header")

    _, rows, columns, imaginary, name_length = header
    name_start = 20
    name_end = name_start + name_length
    name = data[name_start:name_end].rstrip(b"\0").decode("ascii")
    if (rows, columns, imaginary, name) != (
        EXPECTED_ROWS,
        EXPECTED_COLUMNS,
        0,
        "Hsig",
    ):
        raise RuntimeError(
            "unexpected MATLAB-v4 matrix metadata: "
            f"{name} {rows}x{columns}, imaginary={imaginary}"
        )
    count = rows * columns
    values = struct.unpack_from(f"{byte_order}{count}f", data, name_end)
    return matlab_center(values, rows, columns)


def mat5_element(
    data: bytes, offset: int, byte_order: str
) -> tuple[int, bytes, int]:
    if offset + 8 > len(data):
        raise RuntimeError("truncated MATLAB-v5 element tag")
    data_type, length = struct.unpack_from(f"{byte_order}2I", data, offset)
    start = offset + 8
    end = start + length
    if end > len(data):
        raise RuntimeError("truncated MATLAB-v5 element payload")
    next_offset = start + ((length + 7) // 8) * 8
    return data_type, data[start:end], next_offset


def validate_matlab_v5(path: Path) -> float:
    data = path.read_bytes()
    if len(data) < 136 or not data.startswith(b"Data produced by SWAN"):
        raise RuntimeError("MATLAB-v5 output has no SWAN file header")
    indicator = data[126:128]
    if indicator == b"IM":
        byte_order = "<"
    elif indicator == b"MI":
        byte_order = ">"
    else:
        raise RuntimeError("MATLAB-v5 output has an invalid endian indicator")

    matrix_type, matrix, _ = mat5_element(data, 128, byte_order)
    if matrix_type != 14:
        raise RuntimeError("MATLAB-v5 output does not start with an miMATRIX")
    offset = 0
    flags_type, _, offset = mat5_element(matrix, offset, byte_order)
    dimensions_type, dimensions_data, offset = mat5_element(
        matrix, offset, byte_order
    )
    name_type, name_data, offset = mat5_element(matrix, offset, byte_order)
    values_type, values_data, _ = mat5_element(matrix, offset, byte_order)
    if flags_type not in (5, 6) or dimensions_type != 5 or name_type != 1:
        raise RuntimeError("MATLAB-v5 output has unexpected matrix subelements")
    rows, columns = struct.unpack(f"{byte_order}2i", dimensions_data)
    name = name_data.decode("ascii").rstrip(" \0")
    if (rows, columns, name, values_type) != (
        EXPECTED_ROWS,
        EXPECTED_COLUMNS,
        "Hsig",
        7,
    ):
        raise RuntimeError(
            "unexpected MATLAB-v5 matrix metadata: "
            f"{name} {rows}x{columns}, data type={values_type}"
        )
    count = rows * columns
    values = struct.unpack(f"{byte_order}{count}f", values_data)
    return matlab_center(values, rows, columns)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument(
        "--backend",
        choices=("netcdf", "plain-nc", "matlab4", "matlab5"),
        required=True,
    )
    parser.add_argument("--netcdf-checker")
    parser.add_argument("--mpi-exec")
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--mpi-processes", type=int, default=1)
    parser.add_argument("--reference", default="reference")
    parser.add_argument("--expected-center", type=float, default=EXPECTED_CENTER_HSIG)
    parser.add_argument(
        "--mixed-block",
        action="store_true",
        help="retain the text block alongside the binary backend block",
    )
    parser.add_argument("--netcdf-table", action="store_true")
    parser.add_argument("--netcdf-spectrum", action="store_true")
    arguments = parser.parse_args()

    try:
        swan = require_file(arguments.swan_executable, "SWAN executable")
        if arguments.mpi_processes < 1:
            raise ValueError("--mpi-processes must be positive")
        source_directory = Path(__file__).resolve().parent.parent / "quick_test"
        work_directory = Path(arguments.work_directory).expanduser().resolve()
        if work_directory.exists():
            shutil.rmtree(work_directory)
        work_directory.mkdir(parents=True)
        shutil.copy2(source_directory / "bottom.bot", work_directory / "bottom.bot")

        if arguments.netcdf_table and arguments.backend != "netcdf":
            raise ValueError("--netcdf-table requires --backend netcdf")
        if arguments.netcdf_spectrum and arguments.backend != "netcdf":
            raise ValueError("--netcdf-spectrum requires --backend netcdf")
        if arguments.netcdf_table and arguments.netcdf_spectrum:
            raise ValueError("--netcdf-table and --netcdf-spectrum are exclusive")
        extension = "nc" if arguments.backend in ("netcdf", "plain-nc") else "mat"
        if arguments.netcdf_table:
            output_name = "quick_test_table.nc"
        elif arguments.netcdf_spectrum:
            output_name = "quick_test_spectrum.nc"
        else:
            output_name = f"quick_test.{extension}"
        block = (
            "BLOCK 'COMPGRID' NOHEADER 'quick_test_hs.blk' LAYOUT 3 HSIGN"
        )
        backend_block = (
            f"BLOCK 'COMPGRID' NOHEADER '{output_name}' LAYOUT 3 HSIGN"
        )
        replacement = f"{block}\n{backend_block}" if arguments.mixed_block else backend_block
        deck = (source_directory / "quick_test.swn").read_text()
        if arguments.netcdf_spectrum:
            request = f"SPECOUT 'CENTER' SPEC2D ABS '{output_name}'"
            deck = deck.replace("COMPUTE\n", f"{request}\n\nCOMPUTE\n")
        elif arguments.netcdf_table:
            deck = deck.replace("'quick_test_center.tbl'", f"'{output_name}'")
        else:
            deck = deck.replace(block, replacement)
        if deck.count(output_name) != 1:
            raise RuntimeError("could not add the backend output to the quick-test deck")
        (work_directory / "INPUT").write_text(deck)

        command = [str(swan)]
        if arguments.mpi_processes > 1:
            if not arguments.mpi_exec:
                raise ValueError("--mpi-exec is required for a parallel backend run")
            mpi = require_file(arguments.mpi_exec, "MPI launcher")
            command = [
                str(mpi),
                arguments.mpi_numproc_flag,
                str(arguments.mpi_processes),
                str(swan),
            ]
        result = subprocess.run(command, cwd=work_directory, check=False)
        if result.returncode or not (work_directory / "norm_end").is_file():
            raise RuntimeError(
                f"SWAN backend run failed with exit code {result.returncode}; "
                f"inspect {work_directory}"
            )
        reference_names = (
            ["quick_test_hs.blk"]
            if arguments.netcdf_table
            else ["quick_test_center.tbl"]
        )
        if arguments.mixed_block and not arguments.netcdf_table:
            reference_names.append("quick_test_hs.blk")
        if not compare_with_reference(
            work_directory,
            source_directory / arguments.reference,
            tuple(reference_names),
        ):
            raise RuntimeError(f"quick-test reference {arguments.reference} is missing")

        output = work_directory / output_name
        if not output.is_file() or output.stat().st_size == 0:
            raise RuntimeError(f"SWAN produced no non-empty {output_name}")
        if arguments.backend == "netcdf":
            if not arguments.netcdf_checker:
                raise ValueError("--netcdf-checker is required for the netcdf backend")
            checker = require_file(arguments.netcdf_checker, "netCDF checker")
            check = subprocess.run(
                [
                    str(checker),
                    str(output),
                    str(arguments.expected_center),
                    (
                        "spectrum"
                        if arguments.netcdf_spectrum
                        else "point" if arguments.netcdf_table else "map"
                    ),
                ],
                check=False,
            )
            if check.returncode:
                raise RuntimeError(
                    f"netCDF checker stopped with exit code {check.returncode}"
                )
        elif arguments.backend == "plain-nc":
            reference = source_directory / arguments.reference / "quick_test_hs.blk"
            if not same_results(output, reference):
                raise RuntimeError(
                    "a build without netCDF no longer writes .nc as ordinary "
                    "SWAN block output"
                )
            print("NETCDF=OFF preserved ordinary text output for a .nc filename.")
        else:
            center = (
                validate_matlab_v4(output)
                if arguments.backend == "matlab4"
                else validate_matlab_v5(output)
            )
            if abs(center - arguments.expected_center) > 1.0e-5:
                raise RuntimeError(
                    f"MATLAB Hsig at the grid centre is {center}, "
                    f"expected {arguments.expected_center}"
                )
            print(
                f"{arguments.backend} Hsig matrix is "
                f"{EXPECTED_ROWS}x{EXPECTED_COLUMNS}; centre={center:.5f} m."
            )
    except (FileNotFoundError, OSError, RuntimeError, ValueError, struct.error) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
