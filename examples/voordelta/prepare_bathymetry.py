#!/usr/bin/env python3
"""Recreate voordelta.dep from public Rijkswaterstaat WCS coverages.

This optional preparation step requires NumPy and Rasterio. The generated
SWAN depth file is committed with the example, so these packages and network
access are not needed to run the model.
"""

from __future__ import annotations

import hashlib
import tempfile
import urllib.parse
import urllib.request
from pathlib import Path

import numpy as np
import rasterio
from rasterio.transform import from_origin
from rasterio.warp import Resampling, reproject


CASE_DIRECTORY = Path(__file__).resolve().parent
OUTPUT = CASE_DIRECTORY / "voordelta.dep"
SHAPE = (281, 261)
TRANSFORM = from_origin(19_875.0, 450_125.0, 250.0, 250.0)
TARGET_CRS = "EPSG:28992"
DRY_VALUE = -999.0


def wcs_url(service: str, parameters: list[tuple[str, str]]) -> str:
    endpoint = f"https://geo.rijkswaterstaat.nl/services/ogc/gdr/{service}/ows"
    common = [
        ("service", "WCS"),
        ("version", "2.0.1"),
        ("request", "GetCoverage"),
        ("format", "image/tiff"),
    ]
    return endpoint + "?" + urllib.parse.urlencode(common + parameters)


COASTAL_URL = wcs_url(
    "bodemhoogte_20mtr",
    [
        ("coverageId", "bodemhoogte_20mtr__bodemhoogte_20mtr_2024"),
        ("subset", "X(20000,85000)"),
        ("subset", "Y(380000,450000)"),
        ("scaleSize", "i(261),j(281)"),
    ],
)

NCP_URL = wcs_url(
    "bathymetrie_ncp",
    [
        ("coverageId", "bathymetrie_ncp__bathymetrie_ncp_juni_2017"),
        ("subset", "E(528000,597000)"),
        ("subset", "N(5693000,5766000)"),
        ("scaleSize", "i(300),j(310)"),
    ],
)


def download(url: str, destination: Path) -> None:
    print(f"Downloading {url}")
    urllib.request.urlretrieve(url, destination)


def read_on_target_grid(source_path: Path) -> np.ndarray:
    result = np.full(SHAPE, np.nan, dtype=np.float32)
    with rasterio.open(source_path) as source:
        reproject(
            source=rasterio.band(source, 1),
            destination=result,
            src_transform=source.transform,
            src_crs=source.crs,
            src_nodata=source.nodata,
            dst_transform=TRANSFORM,
            dst_crs=TARGET_CRS,
            dst_nodata=np.nan,
            resampling=Resampling.bilinear,
        )
    return result


def write_swan_depths(depth: np.ndarray) -> None:
    with OUTPUT.open("w", encoding="ascii", newline="\n") as output_file:
        # SWAN IDLA=3 reads rows from the southern edge northward.
        for row in depth[::-1]:
            output_file.write(" ".join(f"{value:.2f}" for value in row))
            output_file.write("\n")


def main() -> int:
    with tempfile.TemporaryDirectory(prefix="swan-voordelta-") as directory:
        temporary = Path(directory)
        coastal_path = temporary / "coastal-2024.tif"
        ncp_path = temporary / "ncp-2017.tif"
        download(COASTAL_URL, coastal_path)
        download(NCP_URL, ncp_path)
        coastal_elevation = read_on_target_grid(coastal_path)
        ncp_elevation = read_on_target_grid(ncp_path)

    # Prefer the more detailed coastal survey and fill its gaps with the NCP
    # background grid. Both inputs are elevations relative to NAP.
    elevation = np.where(np.isfinite(coastal_elevation), coastal_elevation, ncp_elevation)
    depth = np.where(np.isfinite(elevation) & (elevation < -0.05), -elevation, DRY_VALUE)
    write_swan_depths(depth)

    wet = depth != DRY_VALUE
    digest = hashlib.sha256(OUTPUT.read_bytes()).hexdigest()
    print(
        f"Wrote {OUTPUT.name}: {depth.shape[1]} x {depth.shape[0]} points, "
        f"{wet.sum()} wet, depth {depth[wet].min():.2f}-{depth[wet].max():.2f} m"
    )
    print(f"SHA256 {digest}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
