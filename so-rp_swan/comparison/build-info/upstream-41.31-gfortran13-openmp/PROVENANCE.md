# Upstream SWAN 41.31 OpenMP build

Built on 2026-07-24 from the historical TU Delft source archive
`swan4131.tar.gz`.

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

- Compiler: GNU Fortran 13.3.0
- Target: 64-bit Linux, OpenMP
- Upstream command: `FC=gfortran make config`, followed by `make omp`
- Effective fixed-form flags:
  `-O -w -fno-second-underscore -fallow-argument-mismatch -fopenmp`
- Effective free-form flags:
  `-O -w -fno-second-underscore -fallow-argument-mismatch
  -ffree-line-length-none -fopenmp`
- Link option: `-static-libgcc`

`-fallow-argument-mismatch` is the only addition to the supplied gfortran
configuration and is needed to compile this legacy Fortran source with modern
gfortran. The legacy Makefile has no Fortran module dependency graph, so the
successful build was run sequentially.

## Result

- Executable:
  `comparison/bin/swan-upstream-41.31-gfortran13-openmp.exe`
- SHA-256: `62f600d647300560e3cbc0c1319361ba5abf18206ca4a439dc456fba4ca38af5`
- Size: 1,487,056 bytes
- Smoke-test banner: `VERSION NUMBER 41.31`
- OpenMP runtime: `libgomp.so.1`

The BSS executable reports `41.31A.1`. That suffix identifies a Deltares
version/special, not an official upstream release tag. This build is therefore
the closest verifiable upstream source version, 41.31; changing only the
printed version string to `41.31A.1` would not make it equivalent to the BSS
source.

For the converged Scaloost/Roompot case, this upstream build and the BSS
41.31A.1 executable have the exact same convergence history and both reach
98.15% at iteration 30. Their Hsig fields differ by only 0.00000225 m RMS and
0.000169 m maximum, consistent with compiler rounding. The complete comparison
is recorded in `so-rp_swan/DEFAULT_CHANGE_ANALYSIS.md`.
