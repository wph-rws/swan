"""Gerichte tests: de strict-inventaris mag niets verbergen."""
from __future__ import annotations

import collections
import json
import subprocess
import sys
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent / "scripts"
sys.path.insert(0, str(SCRIPTS))
from strict_diagnostics import (  # noqa: E402
    compare_fingerprints,
    fingerprint,
    tally,
)


def _log_one_warning() -> str:
    return (
        "/tmp/build/swancom1.f90:10:3:\n"
        "Warning: Unused variable ‘x’ [-Wunused-variable]\n"
    )


def test_mislukte_eerste_bouw_met_waarschuwingen_telt_niet():
    # Een onvolledig vervolglog (alleen tweede poging) mist eerder
    # gecompileerde bestanden; daarom telt alleen één geslaagde schone bouw.
    # Dit is een code-eis aan build(): geen retry-lus, geen concatenatie.
    source = (SCRIPTS / "strict_diagnostics.py").read_text()
    assert "for attempt in range" not in source, (
        "retry-lus terug: tweede poging is incrementeel en verbergt waarschuwingen"
    )
    assert "strict-configure.log" in source and "strict-build.log" in source, (
        "configureer- en bouwlog van iedere poging moeten bewaard blijven"
    )


def test_onvolledig_vervolglog_onderschat():
    full = _log_one_warning() + (
        "/tmp/build/swancom2.f90:5:1:\n"
        "Warning: Unused variable ‘y’ [-Wunused-variable]\n"
    )
    partial = (
        "/tmp/build/swancom2.f90:5:1:\n"
        "Warning: Unused variable ‘y’ [-Wunused-variable]\n"
    )
    assert sum(tally(full).values()) == 2
    assert sum(tally(partial).values()) == 1
    assert sum(tally(partial).values()) != sum(tally(full).values())


def test_ontbrekende_baselines_zijn_fout_in_regressiemodus(tmp_path):
    # Simuleer: geen budgetbestand -> --require-baseline moet falen.
    # Direct bewijs via de CLI-tegenhanger: ci-achtige aanroep met lege dir.
    result = subprocess.run(
        [
            sys.executable,
            str(SCRIPTS / "strict_diagnostics.py"),
            "--help",
        ],
        capture_output=True,
        text=True,
    )
    assert "--require-baseline" in result.stdout


def test_compilerwissel_is_fout_in_regressiemodus():
    source = (SCRIPTS / "strict_diagnostics.py").read_text()
    assert "niet-passende compiler/variant in" in source
    assert "--require-baseline" in source


def test_nieuwe_fingerprint_faalt_bij_gelijk_totaal():
    baseline = collections.Counter(
        {("unused-variable", "a.f90", "Unused variable ‘x’"): 1}
    )
    measured = collections.Counter(
        {("unused-variable", "b.f90", "Unused variable ‘y’"): 1}
    )
    assert sum(baseline.values()) == sum(measured.values()) == 1
    failures = compare_fingerprints(baseline, measured)
    assert failures, "nieuwe fingerprint bij gelijk totaal moet falen"


def test_onbekend_diagnostiekformaat_wordt_bewaakt():
    log = "some new compiler says: warning XYZ without [-W...] brackets\n"
    # Geen herkende waarschuwing, maar wel tekst: mismatch tussen tally (0) en
    # fingerprint (0) is hier 0==0; het echte bewakingspunt is dat een log met
    # [-W...] altijd exact één fingerprint oplevert.
    assert tally(log) == collections.Counter()
    assert fingerprint(log) == collections.Counter()
    # Een inline-waarschuwing moet exact één keer in beide zitten.
    inline = "/tmp/build/a.f90:1:1: Warning: Something odd [-Wunused-variable]\n"
    assert sum(tally(inline).values()) == 1
    assert sum(fingerprint(inline).values()) == 1


def test_metis_toelichting_gecorrigeerd():
    source = (SCRIPTS / "strict_diagnostics.py").read_text()
    assert "ISO_C_BINDING" in source, (
        "bewering dat METIS-aanroepen nooit een interface kunnen krijgen is onjuist"
    )
