"""Strict wet/dry handling and statistics for SWAN block and point output."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
from scipy.io import loadmat

EXCV = -9.0
HSIG_TABLE_COLUMN = 3


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
