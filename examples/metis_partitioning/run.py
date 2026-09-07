#!/usr/bin/env python3
"""Verify that METIS partitions and runs the compact unstructured MPI case."""

from __future__ import annotations

import argparse
import math
import shutil
import subprocess
import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from reference_check import compare_with_reference  # noqa: E402


RESULTS = (
    "nonstationary_unstructured_center.tbl",
)


def require_file(value: str, description: str) -> Path:
    path = Path(value).expanduser().resolve()
    if not path.is_file():
        raise FileNotFoundError(f"{description} not found: {path}")
    return path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--swan-executable", required=True)
    parser.add_argument("--work-directory", required=True)
    parser.add_argument("--mpi-exec", required=True)
    parser.add_argument("--mpi-numproc-flag", default="-n")
    parser.add_argument("--mpi-processes", type=int, default=2)
    parser.add_argument("--reference", default="reference-metis-mpi")
    parser.add_argument(
        "--smoke",
        action="store_true",
        help="alleen draaien zonder regressievergelijking; telt niet als geslaagde regressie",
    )
    arguments = parser.parse_args()

    try:
        if arguments.mpi_processes < 2:
            raise ValueError("--mpi-processes must be at least 2")
        swan = require_file(arguments.swan_executable, "SWAN executable")
        mpi = require_file(arguments.mpi_exec, "MPI launcher")
        source_directory = (
            Path(__file__).resolve().parent.parent
            / "nonstationary"
            / "unstructured"
        )
        work_directory = Path(arguments.work_directory).expanduser().resolve()
        if work_directory.exists():
            shutil.rmtree(work_directory)
        work_directory.mkdir(parents=True)
        basename = "nonstationary_unstructured"
        for name in (
            "bottom.bot",
            "wind.wnd",
            f"{basename}.node",
            f"{basename}.ele",
        ):
            shutil.copy2(source_directory / name, work_directory / name)

        deck = (source_directory / f"{basename}.swn").read_text().replace(
            "SET NAUTICAL\n", "SET NAUTICAL\nTEST 50\n"
        )
        if deck.count("TEST 50") != 1:
            raise RuntimeError("could not enable the METIS partition report")
        (work_directory / "INPUT").write_text(deck)
        command = [
            str(mpi),
            arguments.mpi_numproc_flag,
            str(arguments.mpi_processes),
            str(swan),
        ]
        result = subprocess.run(command, cwd=work_directory, check=False)
        if result.returncode or not (work_directory / "norm_end").is_file():
            raise RuntimeError(
                f"parallel SWAN stopped with exit code {result.returncode}; "
                f"inspect {work_directory}"
            )

        partition_file = work_directory / "partit.mesh"
        if not partition_file.is_file():
            raise RuntimeError(
                "METIS produced no partit.mesh; SwanMeshPartition was not exercised"
            )
        owners = [int(line) for line in partition_file.read_text().splitlines()]
        expected_vertices = 15
        expected_owners = set(range(1, arguments.mpi_processes + 1))
        if len(owners) != expected_vertices or set(owners) != expected_owners:
            raise RuntimeError(
                "invalid METIS partition: "
                f"{len(owners)} vertices owned by ranks {sorted(set(owners))}"
            )
        master_print = work_directory / "PRINT-001"
        partition_message = (
            f"15 vertices is partitioned into    {arguments.mpi_processes} subdomains"
        )
        if not master_print.is_file() or partition_message not in master_print.read_text():
            raise RuntimeError("master log does not confirm SwanMeshPartition")

        block = work_directory / "nonstationary_unstructured_hs.blk"
        block_values = [float(value) for value in block.read_text().split()]
        if len(block_values) != 1617 or not all(map(math.isfinite, block_values)):
            raise RuntimeError(
                "METIS run produced an incomplete or non-finite map series"
            )

        reference_directory = source_directory / arguments.reference
        if arguments.smoke:
            compare_with_reference(
                work_directory, reference_directory, RESULTS, smoke=True
            )
            print("Smoke-modus: METIS-partitie draaide; geen regressievergelijking.")
        else:
            compare_with_reference(
                work_directory, reference_directory, RESULTS
            )
        distribution = Counter(owners)
        print(
            "METIS partitioned 15 vertices: "
            + ", ".join(
                f"rank {rank} owns {distribution[rank]}"
                for rank in sorted(distribution)
            )
        )
    except (FileNotFoundError, OSError, RuntimeError, ValueError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
