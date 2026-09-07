"""Strict wet/dry handling and statistics for SWAN block and point output."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
from scipy.io import loadmat

EXCV = -9.0
HSIG_TABLE_COLUMN = 3
TM01_TABLE_COLUMN = 4
DIR_TABLE_COLUMN = 5
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


def _read_table_column(path: Path | str, column: int, name: str) -> np.ndarray:
    path = Path(path)
    if not path.is_file():
        raise FileNotFoundError(f"missing SWAN point output: {path}")
    values = np.loadtxt(path, comments="%", ndmin=2)
    if values.ndim != 2 or values.shape[1] <= max(column, HSIG_TABLE_COLUMN):
        raise ValueError(f"{path}: table shape {values.shape} has no {name} column")
    return np.asarray(values, dtype=float)


def read_tm01_points(path: Path | str) -> WetValues:
    """Periodevergelijking; EXCV nooit meenemen, maskers eerst."""
    table = _read_table_column(path, TM01_TABLE_COLUMN, "Tm01")
    hs = table[:, HSIG_TABLE_COLUMN]
    tm = table[:, TM01_TABLE_COLUMN]
    wet = hs != EXCV
    if not np.any(wet):
        raise ValueError(f"{path}: no wet values")
    selected = tm[wet]
    if not np.all(np.isfinite(selected)):
        raise ValueError(f"{path}: wet Tm01 contains NaN or infinity")
    if np.any(selected == EXCV):
        raise ValueError(f"{path}: wet Tm01 contains EXCV")
    return WetValues(tm, wet, str(path))


def read_dir_points(path: Path | str) -> WetValues:
    """Richtingvergelijking; richting bij verwaarloosbare energie apart."""
    table = _read_table_column(path, DIR_TABLE_COLUMN, "Dir")
    hs = table[:, HSIG_TABLE_COLUMN]
    direction = table[:, DIR_TABLE_COLUMN]
    wet = hs != EXCV
    if not np.any(wet):
        raise ValueError(f"{path}: no wet values")
    selected = direction[wet]
    if not np.all(np.isfinite(selected)):
        raise ValueError(f"{path}: wet Dir contains NaN or infinity")
    return WetValues(direction, wet, str(path))


def compare_circular(reference: WetValues, candidate: WetValues) -> DifferenceStats:
    """Circulair vergelijken richtingen; maskers eerst, dan kortste boog.

    Punten met Hsig onder DIR_ENERGY_THRESHOLD in beide runs worden
    uitgesloten van de richtingsstatistiek (onbepaald bij geen energie) en
    apart gerapporteerd via de count.
    """
    if reference.values.shape != candidate.values.shape:
        raise ValueError("shape mismatch in circular comparison")
    mismatch = reference.wet != candidate.wet
    if int(np.count_nonzero(mismatch)):
        raise ValueError("wet/dry mask mismatch in circular comparison")
    ref = reference.values[reference.wet]
    cand = candidate.values[reference.wet]
    delta = (cand - ref + 540.0) % 360.0 - 180.0
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
    """Convergentiegeschiedenis uit PRINT (iteratie-residuen).

    Geeft de per-iteratie nauwkeurigheid terug als array; lege/ontbrekende
    geschiedenis faalt (geen stilzwijgende acceptatie).
    """
    path = Path(print_path)
    if not path.is_file():
        raise FileNotFoundError(f"missing PRINT for convergence: {path}")
    residues: list[float] = []
    for line in path.read_text(errors="replace").splitlines():
        stripped = line.strip()
        if stripped.lower().startswith("iteration") and "accuracy" in stripped.lower():
            continue
        if "accuracy OK" in line or "accuracy" in line.lower():
            tokens = [token for token in line.replace(",", " ").split()]
            for token in tokens:
                try:
                    value = float(token.rstrip("%"))
                except ValueError:
                    continue
                if 0.0 < value <= 100.0:
                    residues.append(value)
                    break
    if not residues:
        raise ValueError(f"{path}: no convergence history found")
    return np.asarray(residues, dtype=float)
