# Welcome to the SWAN git repository

[![release](https://img.shields.io/badge/release%20-%20v41.51%20-%20brightgreen?color=success)]()
[![site](https://img.shields.io/badge/sourceforge%20-%20site%20-%20blue?logo=sourceforge&color=informational)](https://swanmodel.sourceforge.io)
[![image](https://img.shields.io/badge/delftwaves%2Fswan%20-%20image%20-%20blue?logo=docker&color=informational)](https://hub.docker.com/r/delftwaves/swan)
[![docs](https://img.shields.io/badge/docs%20-%20GitHub%20pages%20-%20blue?logo=github&color=informational)](https://delftwaves.github.io/swan-docs/)
[![doi](https://img.shields.io/badge/DOI%20-%2010.1029%2F98JC02622%20-%20blue?color=informational)](https://doi.org/10.1029/98JC02622)
[![license](https://img.shields.io/badge/license%20-%20GPL_v3%20-%20orange?color=important)](/LICENSE)

### table of contents

- [introduction](#introduction)
- [installation](#installation)
  - [prerequisites](#prerequisites)
  - [instructions](#instructions)
  - [configuring the build](#configuring-the-build)
  - [clean up the build files](#clean-up-the-build-files)
- [getting started](#getting-started)
  - [run modes](#run-modes)
  - [how to run](#how-to-run)
- [documentation](#documentation)
- [bugs and questions](#bugs-and-questions)

## introduction

SWAN is a third-generation wave model that computes random, short-crested wind-generated waves in coastal regions and inland waters.
For more in-depth background and scientific documentation, the reader is referred to the [SWAN website](https://swanmodel.sourceforge.io).
Please also check the [release notes](https://swanmodel.sourceforge.io/modifications/modifications.htm) for any additional information on the current version **41.51**.

This Readme provides a brief overview of software installation and configuration instructions for users and developers.
Please see the [Implementation Manual](https://swanmodel.sourceforge.io/online_doc/swanimp/swanimp.html) for additional documentation.

In addition to the installation, a brief outline on how to run the model is given below.

The SWAN software can be used freely under the terms of the [GNU General Public License](https://gitlab.tudelft.nl/citg/wavemodels/swan/-/blob/main/LICENSE).
It is permitted to copy, reuse, adapt and distribute the SWAN source code provided that proper reference is made to the original work.

## installation

#### prerequisites

To install SWAN on your local system, CMake and Ninja (or GNU make) need to be installed first.
We recommend to use [CMake 3.20+](https://cmake.org/) for building SWAN (check the version by typing `cmake --version`).
CMake is a build system and makes use of scripts (or configuration files) that control the build process.
There are installers available for Windows, Linux and macOS. See the
[download](https://cmake.org/download/) page for CMake installation instructions.

[Ninja](https://ninja-build.org/) is one of the many build tools to create executable files and libraries from source code.
The way it works is very similar to GNU make (or NMAKE for Windows); for example, it does not rebuild things that are already up to date.
Ninja can be downloaded from its [git repository](https://github.com/ninja-build/ninja/releases).

In addition to the build tools, Python 3 and the double-precision FFTW3
development library must be available on your local computer. On Ubuntu and
Debian, FFTW can be installed with:

```bash
sudo apt install libfftw3-dev
```

Check it by typing `python3 --version` on Linux or macOS, or `python --version`
on Windows. CMake uses Python to set the compile-time switches in the Fortran
source templates.

Finally, SWAN also requires a Fortran compiler with Fortran 2018 support to be present in your environment.
Popular Fortran compilers are [gfortran](https://gcc.gnu.org/fortran/) and
[Intel<sup>&reg;</sup> Fortran Compiler Classic](https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#fortran)
(as part of the Intel<sup>&reg;</sup> oneAPI HPC Toolkit) and both support the OpenMP standard.
Please check this [page](https://fortran-lang.org/learn/os_setup/install_gfortran) for the installation of gfortran on your platform.

Because the source code uses standard Fortran 2018, SWAN can be ported to
various architectures (e.g., Windows, Linux, macOS and Unix-like systems).
Currently, the build scripts support the following Fortran compilers:

1. GNU
1. Intel<sup>&reg;</sup>
1. Portland Group
1. Lahey
1. IBM XL Fortran

#### instructions

##### 1. clone the repo and navigate to the top level source directory

```bash
git clone https://gitlab.tudelft.nl/citg/wavemodels/swan.git && cd swan
```

##### 2. create the build directory

At the top of SWAN source directory execute the following commands

```bash
mkdir build && cd build
```

This step is required to perform an out-of-source build with CMake, that is, build files will not be created in the `/swan/src` directory.

##### 3. build the software

Two CMake configuration files are provided as required for the build. They are placed in the following source directories: `./swan/CMakeLists.txt` and `./swan/src/CMakeLists.txt`.

The following two CMake commands should suffice to build SWAN

```bash
cmake .. -G Ninja
cmake --build .
```

The first command refers to the source directory where the main configuration file is invoked. The second command carries out the building in the build directory.

The package is actually built by invoking Ninja. An alternative would be to use GNU make, as follows

```bash
cmake .. -G "Unix Makefiles"
make
```

or just (in case your OS is Unix-like)

```bash
cmake ..
make
```

However, we recommend Ninja because it is faster than GNU make.

##### 4. install the package

To install SWAN, run either

```bash
cmake --install .
```

or with the GNU make

```bash
make install
```

The default install directory is `$HOME/wavemodels/swan` (Unix-like operating systems, including macOS) or `%LocalAppData%\Programs\wavemodels\swan` (Windows).
(These directories allow app installation without requiring administrator rights.) Instead, you may install SWAN in any other user-defined directory, as follows

```bash
cmake --install . --prefix /somewhere/else/other/than/default/directory
```

or

```bash
cmake .. -DCMAKE_INSTALL_PREFIX=/somewhere/else/other/than/default/directory
make install
```

After installation a number of subdirectories are created.
The executables end up in the `/bin` directory, the archive/library files in `/lib`, and the module files in `/mod`.
Additionally, the `/doc` folder contains the pdf documents, the folder `/tools` consists of some useful scripts and the `/misc` directory
contains all of the files that do not fit in other folders (e.g., a machinefile and an edit file).

Please note that the installation can be skipped (though not recommended). Executables and libraries are then located in subdirectories of the build directory.

#### configuring the build

The build can be (re)configured by passing one or more options to the CMake command with prefix `-D`. A typical command line looks like

```bash
cmake .. -D<option>=<value>
```

where `<value>` is a string or a boolean, depending on the specified option. The table below provides an overview of the non-required options that can be used.

|  option                  | value type |               description                 | default value           |
|:------------------------:|:-----------|:------------------------------------------|:-----------------------:|
| `CMAKE_INSTALL_PREFIX`   | string     | user-defined installation path            | `../wavemodels/swan`    |
| `CMAKE_PREFIX_PATH`      | string     | semicolon-separated list of library paths | empty                   |
| `CMAKE_Fortran_COMPILER` | string     | full path to the Fortran compiler         | determined by CMake     |
| `CMAKE_BUILD_TYPE`       | string     | build configuration                       | `Release`               |
| `MPI`                    | boolean    | enable build with MPI                     | `OFF`                   |
| `OPENMP`                 | boolean    | enable build with OpenMP                  | `OFF`                   |
| `METIS`                  | boolean    | enable build with Metis                   | `OFF`                   |
| `NETCDF`                 | boolean    | enable build with netCDF                  | `OFF`                   |
| `TIMG`                   | boolean    | enable internal timing instrumentation    | `OFF`                   |
| `MATL4`                  | boolean    | enable MATLAB version 4 output            | `OFF`                   |
| `SWAN_NATIVE`            | boolean    | optimize for the CPU performing the build | `OFF`                   |
| `SWAN_LTO`               | boolean    | enable link-time optimization             | `OFF`                   |
| `SWAN_DEBUG_INVARIANTS`  | boolean    | enable diagnostic runtime invariant checks | `OFF`                   |
| `CMAKE_VERBOSE_MAKEFILE` | boolean    | provide verbose output of the build       | `OFF`                   |

For an optimized, portable OpenMP build, use:

```bash
cmake .. -GNinja -DOPENMP=ON
cmake --build . --parallel
```

`SWAN_NATIVE` and `SWAN_LTO` are optional, toolchain-dependent optimizations
that should be benchmarked on representative cases before use. `SWAN_NATIVE`
may produce an executable that does not run on older or different CPU models.
Leave it disabled when distributing binaries. Neither option enables unsafe
floating-point transformations such as `-ffast-math`.

`SWAN_DEBUG_INVARIANTS` enables additional internal consistency checks for
development and validation builds. Leave it disabled for production runs,
because the checks can alter compiler optimization and reduce performance.

The source is compiled in standard Fortran 2018 mode, while the migration from
legacy external procedures and global state is still ongoing. See
[Modern Fortran status](doc/modern-fortran.md) for concrete before/after
examples, measured diagnostic improvements and the remaining boundary.

For example, the following commands

```bash
cmake .. -GNinja -DNETCDF=ON -DMPI=ON
cmake --build .
```

will configure SWAN to be built created by Ninja that supports netCDF output and parallel computing using the MPI paradigm.
Note that CMake will check the availability of MPI and netCDF libraries within your environment.
Also note that netCDF libraries might be installed in a custom directory (e.g., `/home/your/name/netcdf`), which must then be a priori specified on the command line as follows:

```bash
export NetCDF_ROOT=/path/to/netcdf/root/directory
```

or

```bash
cmake .. [options] -DCMAKE_PREFIX_PATH=/path/to/netcdf/directory
```

so that CMake can find them. The same holds for Metis libraries, as follows:

```bash
export Metis_ROOT=/path/to/metis/root/directory
```

or

```bash
cmake .. [options] -DCMAKE_PREFIX_PATH=/path/to/metis/directory
```

Note: to define a path list with more than one prefixes use a semicolon as a separator.

The system default Fortran compiler (e.g., f77, g95) can be overwritten as follows

```bash
cmake .. [options] -DCMAKE_Fortran_COMPILER=/path/to/the/desired/compiler/including/the/name/of/compiler
```

Finally, if CMake fails to configure your project, then execute

```bash
cmake .. [options] -DCMAKE_VERBOSE_MAKEFILE=ON
```

which will generate detailed information that may provide some indications to debug the build process.

#### clean up the build files

To remove the build directory and all files that have been created after running `cmake --build .`, run at the top level of your project the following command:

```bash
cmake -P clobber.cmake
```

(The `-P` argument passed to CMake will execute a script *\<filename\>.cmake*.)

## getting started

*Note: before start using the SWAN package, it is suggested to first read
Chapters [2](https://swanmodel.sourceforge.io/online_doc/swanuse/node2.html#ch:defin) and
[3](https://swanmodel.sourceforge.io/online_doc/swanuse/node15.html#ch:inout)
of the [SWAN's User Guide](https://swanmodel.sourceforge.io/online_doc/swanuse/swanuse.html).*

#### run modes

There are three different modes in which you can run SWAN:

1. **serial**: for computers using one processor (recommended for small tests)
1. **parallel, shared (OpenMP)**: for shared memory systems (laptop/desktop using multiple processors)
1. **parallel, distributed (MPI)**:  for distributed memory systems (e.g., clusters, HPC)

See the above installation instructions for getting the proper executable.

#### how to run

The general run procedure is as follows:

1. complete or modify your command file `INPUT`
1. run the SWAN model:
   ```bash
   ./swan.exe
   ```
1. check the created `PRINT` file for warning and error messages
1. repeat if needed

For faster simulation on a cluster, replace the run command by

```bash
mpirun -np <n> swan.exe
```

with `<n>` the number of desired MPI processes.

The above procedure can be done automatically using the script `/bin/swanrun` (or `\bin\swanrun.bat` in case of Windows), provided that the
environment variable `PATH` has been adapted by including the path of the `/bin` directory.

For more details, consult the [Implementation manual](https://swanmodel.sourceforge.io/online_doc/swanimp/node12.html).

#### quick smoke test

A small stationary example is available in [`examples/quick_test`](examples/quick_test).
It exercises the regular grid, bathymetry input, boundary waves, wind and output
pipeline, and is sized to finish well within three minutes on typical hardware.
See the example README for Windows and Unix run instructions.

Two compact nonstationary examples are available in
[`examples/nonstationary`](examples/nonstationary). They apply a time-varying
wind to comparable regular and unstructured grids, with time-dependent block
and point output.

A substantially larger, geographically realistic example is available in
[`examples/voordelta`](examples/voordelta). It covers a 65 x 70 km domain on
public Rijkswaterstaat bathymetry and documents the source data and model
limitations. Its `run_mpi.py` runner builds on the same case to demonstrate and
verify execution with multiple MPI processes.

Focused nonlinear-interaction examples are available in
[`examples/nonlinear_interactions`](examples/nonlinear_interactions). They
compare DIA and exact XNL quadruplets, DCTA and FTIM triads over a submerged
bar, and a combined ocean-to-nearshore run in which both source terms are
active. The runner validates spectra and source terms and creates comparison
figures.

## documentation

See
1. the [SWAN website](https://swanmodel.sourceforge.io/) for general information
1. the [SWAN documentation](https://delftwaves.github.io/swan-docs/) that provides the user manual, scientific/technical documentation and many more
1. the [SWAN settings](https://swanmodel.sourceforge.io/settings/settings.htm) page for an overview of the source term packages

## bugs and questions

For bug reports please send to the [SourceForge mailing list](http://sourceforge.net/mail/?group_id=384349).


<small>&copy; Copyright 2026  Marcel Zijlema</small>
