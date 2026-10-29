# Upstream SWAN 41.31 OpenMP build

Built on 2026-10-28 from the historical TU Delft source archive
`swan4131.tar.gz`.

The archive (`comparison/source/`) and the executable (`comparison/bin/`) are
kept locally only; both directories are excluded by `.gitignore`. This
repository stores their hashes, the build settings and the build log, so a
rebuilt copy can be checked against them.

## Source

- Archive: `comparison/source/swan4131.tar.gz`
- SHA-256: `cd3ba1f0d79123f1b7d42a43169f07575b59b01e604c5e66fbc09769e227432e`
- Preserved mirror: `https://github.com/apchoi79/SWAN-Support`
- Mirror commit adding the archive:
  `29658bf84e77535b8e0085643d8a2d7e0638c489`
- Independent mirror: `https://gitlab.com/xavirg/swan-support`
- The archives from both mirrors were byte-for-byte identical.
- The source timestamps are 2019-05-29 and `swanmain.ftn` sets
  `VERNUM = 41.31`.

The official SWAN GitLab history starts at 41.41 and SourceForge no longer
serves the old 41.31 archive. The maintainer of the two mirrors states in the
official SWAN mailing-list archive that the mirrored archives came from the
original source distribution and retain their original timestamps.

## Build

- Compiler: GNU Fortran 16.2.0 (`gfortran-16`)
- Target: 64-bit Linux, OpenMP
- Upstream command: `make config`, which writes `macros.inc`, followed by
  `make omp`
- Effective fixed-form flags:
  `-O -w -fno-second-underscore -fallow-argument-mismatch -fopenmp`
- Effective free-form flags:
  `-O -w -fno-second-underscore -fallow-argument-mismatch
  -ffree-line-length-none -fopenmp`
- Link option: `-static-libgcc`

The 41.31 `platform.pl` only recognises the plain `gfortran` command, so
`macros.inc` names `gfortran-16` explicitly. `-fallow-argument-mismatch` is
the only addition to the supplied gfortran configuration and is needed to
compile this legacy Fortran source with modern gfortran. The legacy Makefile
has no Fortran module dependency graph, so the build runs sequentially.
`build.log` holds the complete build.

## Result

- Executable:
  `comparison/bin/swan-upstream-41.31-gfortran16-openmp.exe`
- SHA-256: `2d5c13147baa353319ee783523c18412c299d8cce0afd2228e35436abd00b4ec`
- Size: 1,548,592 bytes
- Smoke-test banner (`smoke-PRINT`): `VERSION NUMBER 41.31`
- OpenMP runtime: `libgomp.so.1`

The RWS executable reports `41.31A.1`. That suffix identifies a Deltares
version/special, not an official upstream release tag. This build is therefore
the closest verifiable upstream source version, 41.31; changing only the
printed version string to `41.31A.1` would not make it equivalent to the RWS
source.

On the operational so-rp computation with the corrected template
(`../../par/so-rp_osk_org.swn`), condition `u20_d310_lp300_open`, eight
threads, this build and the RWS 41.31A.1 executable both stop on their
criterion at 98.02% after 35 iterations; the accuracy per iteration differs by
at most 0.01 percentage point. Over the 130 wet output points Hsig differs by
at most 0.00017 m and by 0.000016 m RMS, and the mean Hsig is 1.602 m in both,
consistent with compiler rounding.
