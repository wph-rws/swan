"""Negatieve tests: de matrix mag dekking niet als groen verkopen."""
from __future__ import annotations

import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
from build_matrix import EXPECTED_TESTS, MATRIX, parse_junit  # noqa: E402


def test_config_zonder_tests_is_rood_pad(tmp_path):
    junit = tmp_path / "j.xml"
    junit.write_text('<?xml version="1.0"?><testsuites></testsuites>')
    total, failed, skipped, names, failed_names = parse_junit(junit)
    assert total == 0, "nul tests moet als rood-pad herkenbaar zijn"


def test_ontbrekende_vereiste_test_wordt_geweigerd():
    for name in MATRIX:
        assert EXPECTED_TESTS[name] >= {
            "quick_test",
            "thread_state_manifest",
        }, f"{name!r} mist verplichte dekking"


def test_overgeslagen_optionele_compiler_is_niet_in_orde():
    from build_matrix import OPTIONAL

    assert "gcc15" in OPTIONAL
    # check_one retourneert "niet uitgevoerd" voor ontbrekende optionele
    # toolchain; main() telt dat niet als groen (broninspectie).
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert '"niet uitgevoerd"' in source
    assert "in orde" not in source or "niet als" in source


def test_nul_tests_faalt_expliciet():
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert "nul tests geregistreerd" in source


def test_junit_wordt_gebruikt_voor_dekking():
    source = (SCRIPTS / "build_matrix.py").read_text()
    assert "--output-junit" in source
    assert "parse_junit" in source


def test_debug_is_zichtbare_onderzoeksport():
    from build_matrix import RESEARCH

    assert "debug" in RESEARCH
    assert "debug" in MATRIX
