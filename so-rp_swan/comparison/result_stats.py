"""Strict wet/dry handling and statistics for SWAN block and point output."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
from scipy.io import loadmat

EXCV = -9.0
HSIG_TABLE_COLUMN = 3
# Richting bij verwaarloosbare golfenergie wordt afzonderlijk behandeld: onder
# deze Hsig is de richting numeriek onbepaald en geen regressiesignaal.
DIR_ENERGY_THRESHOLD = 0.05


@dataclass(frozen=True)
class WetValues:
    """Values plus an explicit mask separating wet samples from dry samples."""

    values: np.ndarray
    wet: np.ndarray
    source: str

    def __post_init__(self) -> None:
        if self.values.shape != self.wet.shape:
            raise ValueError(f"{self.source}: value and wet-mask shapes differ")
        if self.values.size == 0:
            raise ValueError(f"{self.source}: no values")
        if not np.any(self.wet):
            raise ValueError(f"{self.source}: no wet values")
        selected = self.values[self.wet]
        if not np.all(np.isfinite(selected)):
            raise ValueError(f"{self.source}: wet values contain NaN or infinity")
        if np.any(selected == EXCV):
            raise ValueError(f"{self.source}: wet values contain EXCV={EXCV:g}")

    @property
    def wet_count(self) -> int:
        return int(np.count_nonzero(self.wet))

    @property
    def dry_count(self) -> int:
        return int(self.wet.size - self.wet_count)

    @property
    def mean(self) -> float:
        return float(np.mean(self.values[self.wet]))


@dataclass(frozen=True)
class DifferenceStats:
    bias: float
    rms: float
    maximum_absolute: float
    count: int


def read_hsig_field(path: Path | str) -> WetValues:
    """Read a MATLAB Hsig field; SWAN's NaNs and EXCV values denote dry cells."""
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN block output: {path}")
    data = loadmat(path)
    if "Hsig" not in data:
        raise ValueError(f"{path}: MATLAB file has no Hsig array")
    # Some stored MATLAB files contain signaling NaNs for dry cells. Converting
    # float32 to the host float type may raise NumPy's "invalid" warning while
    # correctly preserving those NaNs as the dry mask.
    with np.errstate(invalid="ignore"):
        values = np.asarray(data["Hsig"], dtype=float)
    if values.ndim != 2:
        raise ValueError(f"{path}: Hsig has rank {values.ndim}, expected 2")
    if np.any(np.isinf(values)):
        raise ValueError(f"{path}: Hsig contains infinity")
    wet = np.isfinite(values) & (values != EXCV)
    return WetValues(values, wet, str(path))


def read_hsig_points(path: Path | str) -> WetValues:
    """Read point Hsig; finite EXCV=-9 entries denote dry requested locations."""
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN point output: {path}")
    values = np.loadtxt(path, comments="%", ndmin=2)
    if values.ndim != 2 or values.shape[1] <= HSIG_TABLE_COLUMN:
        raise ValueError(
            f"{path}: table shape {values.shape} has no Hsig column "
            f"{HSIG_TABLE_COLUMN}"
        )
    hs = np.asarray(values[:, HSIG_TABLE_COLUMN], dtype=float)
    if not np.all(np.isfinite(hs)):
        raise ValueError(f"{path}: point Hsig contains NaN or infinity")
    wet = hs != EXCV
    return WetValues(hs, wet, str(path))


def compare(reference: WetValues, candidate: WetValues) -> DifferenceStats:
    """Compare values only after proving that their wet/dry masks are identical."""
    if reference.values.shape != candidate.values.shape:
        raise ValueError(
            f"shape mismatch: {reference.source} has {reference.values.shape}, "
            f"{candidate.source} has {candidate.values.shape}"
        )
    mismatch = reference.wet != candidate.wet
    mismatch_count = int(np.count_nonzero(mismatch))
    if mismatch_count:
        became_wet = int(np.count_nonzero(~reference.wet & candidate.wet))
        became_dry = int(np.count_nonzero(reference.wet & ~candidate.wet))
        raise ValueError(
            f"wet/dry mask mismatch between {reference.source} and "
            f"{candidate.source}: {mismatch_count} samples "
            f"({became_wet} became wet, {became_dry} became dry)"
        )

    delta = candidate.values[reference.wet] - reference.values[reference.wet]
    if not np.all(np.isfinite(delta)):
        raise ValueError("comparison produced NaN or infinity over the shared wet mask")
    return DifferenceStats(
        bias=float(np.mean(delta)),
        rms=float(np.sqrt(np.mean(delta * delta))),
        maximum_absolute=float(np.max(np.abs(delta))),
        count=int(delta.size),
    )


def read_table_named_columns(path: Path | str) -> tuple[dict[str, int], np.ndarray]:
    """Lees een SWAN-punttabel met kolomnamen uit de %-header.

    Verschillende decks vragen verschillende kolommen (quick-test vraagt
    Xp/Yp/Depth/Hsig/Tm01/Dir; de operationele tabel vraagt bovendien
    Period/Tm02/RTpeak/Dspr/FSpr/dHs), dus posities zijn geen contract: namen
    wel. Geeft (naam → kolomindex, datamatrix) terug. Ontbrekende namen of een
    lege tabel zijn een fout.
    """
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN point output: {path}")
    names: list[str] = []
    for line in path.read_text(errors="replace").splitlines():
        stripped = line.strip()
        if stripped.startswith("%") and "Xp" in stripped:
            names = stripped.lstrip("%").split()
            break
    if not names:
        raise ValueError(f"{path}: point table has no %-header with column names")
    values = np.loadtxt(path, comments="%", ndmin=2)
    if values.ndim != 2 or values.shape[1] != len(names):
        raise ValueError(
            f"{path}: table shape {values.shape} does not match "
            f"{len(names)} header names"
        )
    return {name: index for index, name in enumerate(names)}, np.asarray(
        values, dtype=float
    )


def _wet_point_values(path: Path | str, quantity: str) -> WetValues:
    """Grootheid per nat opgevraagd punt; EXCV nooit meenemen, maskers eerst."""
    columns, table = read_table_named_columns(path)
    for required in ("Hsig", quantity):
        if required not in columns:
            raise ValueError(f"{path}: point table has no {required} column")
    hs = table[:, columns["Hsig"]]
    series = table[:, columns[quantity]]
    if not np.all(np.isfinite(hs)):
        raise ValueError(f"{path}: point Hsig contains NaN or infinity")
    wet = hs != EXCV
    if not np.any(wet):
        raise ValueError(f"{path}: no wet values")
    selected = series[wet]
    if not np.all(np.isfinite(selected)):
        raise ValueError(f"{path}: wet {quantity} contains NaN or infinity")
    if np.any(selected == EXCV):
        raise ValueError(f"{path}: wet {quantity} contains EXCV")
    return WetValues(series, wet, str(path))


def read_tm01_points(path: Path | str) -> WetValues:
    """Tm01 per nat opgevraagd punt (kolom op naam, niet op positie)."""
    return _wet_point_values(path, "Tm01")


def read_dir_points(path: Path | str) -> WetValues:
    """Richting per nat opgevraagd punt (kolom op naam, niet op positie)."""
    return _wet_point_values(path, "Dir")


def compare_circular(reference: WetValues, candidate: WetValues,
                     reference_hs: WetValues, candidate_hs: WetValues) -> DifferenceStats:
    """Circulair vergelijken richtingen via de kortste boog.

    Maskers eerst (nat/droog én vorm). Punten waar beide runs onder
    DIR_ENERGY_THRESHOLD blijven tellen niet mee (richting daar onbepaald);
    count is het aantal meegenomen punten. Zijn alle natte punten uitgesloten,
    dan faalt de vergelijking (geen stilzwijgend succes zonder dekking).
    """
    for pair in ((reference, candidate), (reference_hs, candidate_hs)):
        if pair[0].values.shape != pair[1].values.shape:
            raise ValueError("shape mismatch in circular comparison")
        if int(np.count_nonzero(pair[0].wet != pair[1].wet)):
            raise ValueError("wet/dry mask mismatch in circular comparison")
    wet = reference.wet
    energetic = (reference_hs.values[wet] >= DIR_ENERGY_THRESHOLD) | (
        candidate_hs.values[wet] >= DIR_ENERGY_THRESHOLD
    )
    if not np.any(energetic):
        # Nergens energie: richting is nergens bepaald. Vacu waar (geen
        # verschil aantoonbaar) met count 0, zodat de aanroeper ziet dat er
        # geen dekking was. Hsig dekt deze punten al af.
        return DifferenceStats(bias=0.0, rms=0.0, maximum_absolute=0.0, count=0)
    delta = (
        (candidate.values[wet][energetic] - reference.values[wet][energetic] + 540.0)
        % 360.0
        - 180.0
    )
    if not np.all(np.isfinite(delta)):
        raise ValueError("circular comparison produced NaN or infinity")
    absolute = np.abs(delta)
    return DifferenceStats(
        bias=float(np.mean(delta)),
        rms=float(np.sqrt(np.mean(delta * delta))),
        maximum_absolute=float(np.max(absolute)),
        count=int(delta.size),
    )


def read_convergence_history(print_path: Path | str) -> np.ndarray:
    """Convergentiegeschiedenis uit PRINT.

    Geeft de per-iteratie reeks "accuracy OK in X % of wet grid points" terug
    als array (procenten, in iteratievolgorde). Alleen exact die regel telt;
    parameterregels ("Accuracy parameters") en andere percentages doen niet
    mee. Een lege/ontbrekende geschiedenis faalt (geen stilzwijgende
    acceptatie).
    """
    import re

    path = Path(print_path)
    if not path.is_file():
        raise FileNotFoundError(f"missing PRINT for convergence: {path}")
    pattern = re.compile(r"accuracy OK in\s+([0-9]+(?:\.[0-9]+)?)\s*%")
    residues = [float(match.group(1)) for line in
                path.read_text(errors="replace").splitlines()
                for match in [pattern.search(line)] if match is not None]
    if not residues:
        raise ValueError(f"{path}: no convergence history found")
    return np.asarray(residues, dtype=float)


@dataclass(frozen=True)
class SpectralSet:
    """Geparseerd SWAN-spectrum bestand (.sp1/.sp2): per locatie per frequentie.

    values: grootheid-naam -> (nlocaties, nfreq) array; exceptions: idem de
    exceptiewaarde; present: per opgevraagde locatie of er een LOCATION-blok
    staat (droge punten krijgen een NODATA-regel, bewezen op de matrixdata:
    exact de droge uitvoerpunten). Geldig = aanwezig én eindig én ongelijk
    aan de exceptiewaarde; NaN/Inf faalt altijd. Afwezige cellen dragen de
    exceptiewaarde.
    """

    values: dict[str, np.ndarray]
    exceptions: dict[str, float]
    present: np.ndarray
    source: str


def read_spectra(path: Path | str) -> SpectralSet:
    """Lees een SWAN-standaard-spectrumbestand strikt structureel.

    Verwacht: LOCATIONS + aantal, coördinaten, frequentielijst + aantal,
    QUANT + aantal grootheden met elk naam/unit/exceptiewaarde, daarna per
    locatie een LOCATION-blok met precies nfreq rijen van nquant kolommen.
    Elke afwijking is een fout (geen giswerk naar kolommen).
    """
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN spectrum output: {path}")
    raw = path.read_text(errors="replace").splitlines()
    cursor = [0]

    def next_line() -> str:
        if cursor[0] >= len(raw):
            raise ValueError(f"{path}: onverwacht einde spectrumbestand")
        line = raw[cursor[0]]
        cursor[0] += 1
        return line

    def expect_count(keyword: str) -> int:
        while True:
            line = next_line()
            if keyword in line:
                break
        # Het aantal staat soms op dezelfde regel ("3  number of ...") en
        # soms op de volgende regel; probeer lui (nooit een regel te veel).
        try:
            return int(float(line.split()[0]))
        except (ValueError, IndexError):
            pass
        try:
            return int(float(next_line().split()[0]))
        except (ValueError, IndexError):
            raise ValueError(f"{path}: geen aantal bij {keyword!r}") from None

    locations = expect_count("LOCATIONS")
    for _ in range(locations):
        parts = next_line().split()
        if len(parts) != 2:
            raise ValueError(f"{path}: locatieregel heeft geen x y-paar")
        for token in parts:
            float(token)
    frequencies = expect_count("number of frequencies")
    for _ in range(frequencies):
        float(next_line().split()[0])
    quantities = expect_count("number of quantities")
    names: list[str] = []
    exceptions: dict[str, float] = {}
    for _ in range(quantities):
        name = next_line().split()[0]
        next_line()
        exc_line = next_line()
        if "exception value" not in exc_line:
            raise ValueError(f"{path}: geen exceptiewaarde bij {name!r}")
        try:
            exceptions[name] = float(exc_line.split()[0])
        except (ValueError, IndexError):
            raise ValueError(
                f"{path}: onleesbare exceptiewaarde bij {name!r}") from None
        names.append(name)
    values: dict[str, np.ndarray] = {}
    present = np.zeros(locations, dtype=bool)
    for location in range(1, locations + 1):
        header = next_line()
        if header.strip() == "NODATA":
            continue
        parts = header.split()
        if len(parts) != 2 or parts[0] != "LOCATION":
            raise ValueError(f"{path}: geen LOCATION/NODATA bij punt {location}")
        try:
            number = int(parts[1])
        except ValueError:
            raise ValueError(
                f"{path}: onleesbaar locatienummer bij punt {location}") from None
        if number != location:
            raise ValueError(
                f"{path}: LOCATION {number} waar punt {location} hoort")
        present[location - 1] = True
        for frequency in range(frequencies):
            parts = next_line().split()
            if len(parts) != quantities:
                raise ValueError(
                    f"{path}: locatie {location} frequentie "
                    f"{frequency + 1}: {len(parts)} kolommen, "
                    f"{quantities} verwacht")
            try:
                row = [float(token) for token in parts]
            except ValueError:
                raise ValueError(
                    f"{path}: niet-numerieke spectraalwaarde bij locatie "
                    f"{location}") from None
            for name, value in zip(names, row):
                values.setdefault(name, np.full(
                    (locations, frequencies), np.nan))[location - 1,
                                                       frequency] = value
    for name in names:
        values.setdefault(name, np.full((locations, frequencies), np.nan))
    rest = [line for line in raw[cursor[0]:] if line.strip()]
    if rest:
        raise ValueError(f"{path}: {len(rest)} overbodige regels na laatste blok")
    for name, array in values.items():
        array[~np.repeat(present, frequencies).reshape(locations, frequencies)] = (
            exceptions[name])
        selected = array[array != exceptions[name]]
        if not np.all(np.isfinite(selected)):
            raise ValueError(f"{path}: {name} bevat NaN of oneindig")
    return SpectralSet(values, exceptions, present, str(path))


def compare_spectra(reference: SpectralSet, candidate: SpectralSet,
                    ) -> dict[str, DifferenceStats]:
    """Vergelijk spectra per grootheid; maskers eerst (strikt).

    Geldigmaskers (aanwezigheid én ongelijk exceptiewaarde) moeten exact
    overeenkomen; NDIR wordt circulair vergeleken, de overige lineair. Geeft
    per grootheid statistieken over gedeelde geldige cellen. Voor
    rapportage over verschillende fysica, zie report_spectral_difference.
    """
    return _compare_spectra_inner(reference, candidate, strict=True)[0]


def report_spectral_difference(reference: SpectralSet, candidate: SpectralSet,
                               ) -> tuple[dict[str, DifferenceStats], dict[str, int]]:
    """Rapporteer spectrale verschillen inclusief maskerverschillen.

    Structurele verschillen (grootheden, vorm, aanwezige locatiesets) blijven
    fouten; geldig/ongeldig-verschillen per grootheid worden geteld en de
    statistieken lopen over gedeelde geldige cellen. NDIR circulair.
    Geeft (statistieken, maskerverschillen) terug.
    """
    return _compare_spectra_inner(reference, candidate, strict=False)


def _compare_spectra_inner(reference: SpectralSet, candidate: SpectralSet,
                           *, strict: bool,
                           ) -> tuple[dict[str, DifferenceStats], dict[str, int]]:
    """Gedeelde implementatie; strict eist gelijke maskers, anders tellen."""
    if set(reference.values) != set(candidate.values):
        raise ValueError(
            f"grootheden verschillen: {sorted(reference.values)} vs "
            f"{sorted(candidate.values)}")
    if reference.present.shape != candidate.present.shape or not bool(
            np.array_equal(reference.present, candidate.present)):
        raise ValueError("aanwezige locatiesets verschillen")
    outcome: dict[str, DifferenceStats] = {}
    mismatches: dict[str, int] = {}
    for name in sorted(reference.values):
        left, right = reference.values[name], candidate.values[name]
        if left.shape != right.shape:
            raise ValueError(f"{name}: vorm {left.shape} vs {right.shape}")
        valid_left = left != reference.exceptions[name]
        valid_right = right != candidate.exceptions[name]
        mask = valid_left & valid_right
        mismatch = int(np.count_nonzero(valid_left != valid_right))
        if mismatch and strict:
            raise ValueError(f"{name}: {mismatch} geldig/ongeldig-verschillen")
        mismatches[name] = mismatch
        if name == "NDIR":
            delta = (right[mask] - left[mask] + 540.0) % 360.0 - 180.0
        else:
            delta = right[mask] - left[mask]
        if not np.all(np.isfinite(delta)):
            raise ValueError(f"{name}: vergelijking geeft NaN of oneindig")
        absolute = np.abs(delta)
        outcome[name] = DifferenceStats(
            bias=float(np.mean(delta)) if delta.size else 0.0,
            rms=float(np.sqrt(np.mean(delta * delta))) if delta.size else 0.0,
            maximum_absolute=float(np.max(absolute)) if delta.size else 0.0,
            count=int(delta.size),
        )
    return outcome, mismatches
