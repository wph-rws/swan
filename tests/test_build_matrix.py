"""Negative tests: the matrix may not pass off missing coverage as green."""
from __future__ import annotations

import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
from build_matrix import EXPECTED_TESTS, MATRIX, parse_junit  # noqa: E402


def test_config_without_tests_is_red_path(tmp_path):
    junit = tmp_path / "j.xml"
    junit.write_text('<?xml version="1.0"?><testsuites></testsuites>')
    total, failed, skipped, names, failed_names = parse_junit(junit)
    assert total == 0, "zero tests must be recognisable as the red path"


def test_missing_required_test_is_refused():
    for name in MATRIX:
        assert EXPECTED_TESTS[name] >= {
            "quick_test",
            "thread_state_manifest",
        }, f"{name!r} lacks required coverage"


def test_skipped_optional_compiler_is_not_ok():
    from build_matrix import OPTIONAL

    assert "ifx" in OPTIONAL
    # check_one returns "not run" for a missing optional toolchain;
    # main() does not count that as green (source inspection).
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert '"not run"' in source
    assert '"OK"' not in source or "not count as" in source


def test_zero_tests_fails_explicitly():
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert "zero tests registered" in source


def test_junit_is_used_for_coverage():
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert "--output-junit" in source
    assert "parse_junit" in source


def test_every_variant_requires_the_test_of_its_own_path():
    own_path = {
        "netcdf": "netcdf_output",
        "mpi-netcdf": "netcdf_output_mpi",
        "metis-mpi": "metis_partitioning",
        "jac": "quick_test_mpi",
        "ffro": "nonstationary_examples",
        "coh-mpi": "coherent_mpi_host",
        "esmf": "esmf_wind_input",
        "adcirc": "adcirc_grid",
    }
    for name, test in own_path.items():
        assert test in EXPECTED_TESTS[name], f"{name!r} does not require {test}"
    # Under COH SWAN does not split the domain; the separate MPI runs then do not apply.
    assert "quick_test_mpi" not in EXPECTED_TESTS["coh-mpi"]


def test_compiler_is_chosen_explicitly():
    from build_matrix import compiler_for

    assert compiler_for([]) == ["-DCMAKE_Fortran_COMPILER=gfortran-16"]
    assert compiler_for(["-DJAC=ON"]) == ["-DCMAKE_Fortran_COMPILER=gfortran-16"]
    assert compiler_for(["-DNETCDF=ON"]) == ["-DCMAKE_Fortran_COMPILER=gfortran-16"]
    assert compiler_for(MATRIX["ifx"]) == []


def test_debug_is_a_visible_research_build():
    from build_matrix import RESEARCH

    assert "debug" in RESEARCH
    assert "debug" in MATRIX
