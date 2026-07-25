# a CMake script to build SWAN
#
# Copyright (C) 2026  Delft University of Technology

# use of CMake version 3.20 or greater
cmake_minimum_required( VERSION 3.20 )

# by default, the SWAN package will be installed in user's local or home folder
if( WIN32 )
  cmake_path( SET LOCALSWAN $ENV{LOCALAPPDATA}/Programs/wavemodels/swan )
  set( CMAKE_INSTALL_PREFIX "${LOCALSWAN}" CACHE STRING "" )
else()
  set( CMAKE_INSTALL_PREFIX "$ENV{HOME}/wavemodels/swan" CACHE STRING "" )
endif()

# Fortran project for SWAN — wph-rws/swan fork.
# Reference line: TU Delft gitlab.tudelft.nl/citg/wavemodels/swan (upstream).
project( SWAN LANGUAGES Fortran DESCRIPTION "wph-rws/swan fork of SWAN 41.51" )

# Require portable, standard-conforming free-form Fortran.  CMake 3.20 and
# GNU Fortran support Fortran 2018 as the newest enforceable language mode;
# disabling extensions rejects legacy declarations such as REAL*8.
set( CMAKE_Fortran_STANDARD 2018 )
set( CMAKE_Fortran_STANDARD_REQUIRED ON )
set( CMAKE_Fortran_EXTENSIONS OFF )

# set the SWAN version
set( VERSION 41.51 )

# Use an optimized build by default while respecting an explicitly selected
# configuration. Multi-config generators select their configuration at build
# time and therefore do not use CMAKE_BUILD_TYPE.
if( NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES )
  set( CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE )
endif()
set_property( CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo MinSizeRel )

# set the executable names
if( WIN32 )
   set( EXE swan )
   set( HEXE hcat )
else()
   set( EXE swan.exe )
   set( HEXE hcat.exe )
endif()

# extra build options for the user
option( OPENMP "" OFF )
option( MPI "" OFF )
option( JAC "" OFF )
option( METIS "" OFF )
option( FFRO "" OFF )
option( COH "Preserve externally owned coherent-coupling MPI lifecycle" OFF )
option( ESMF "Enable ESMF exponential-wind-input coupling state" OFF )
option( ADCIRC "Compatibility selector; ADCIRC grid support is always built" OFF )
option( NETCDF "" OFF )
option( TIMG "Enable internal timing instrumentation" OFF )
option( MATL4 "Enable MATLAB version 4 output" OFF )
option( SWAN_NATIVE "Optimize for the CPU that performs the build" OFF )
option( SWAN_LTO "Enable interprocedural/link-time optimization" OFF )
option( SWAN_DEBUG_INVARIANTS "Enable diagnostic runtime invariant checks" OFF )
option( SWAN_RUNTIME_CHECKS "Enable compiler runtime checks (bounds, allocation, arguments)" OFF )
option( SWAN_LEGACY_CRAY_IO "Exercise the preserved Cray sequential-I/O branch" OFF )
option( SWAN_LEGACY_SGI_IO "Exercise the preserved SGI I/O and unit-limit branch" OFF )
option( SWAN_DEC_SHARED_IO "Use the preserved DEC SHARED file-opening backend" OFF )
option( SWAN_MPIF_COMPAT "Provide the preserved mpif.h-backed MPI module" OFF )

if( ESMF AND METIS )
  message( FATAL_ERROR "ESMF and METIS are historically incompatible" )
endif()
if( ESMF AND ADCIRC )
  message( FATAL_ERROR "ESMF and ADCIRC are historically incompatible" )
endif()
if( SWAN_MPIF_COMPAT AND NOT (MPI OR JAC) )
  message( FATAL_ERROR "SWAN_MPIF_COMPAT requires MPI or JAC" )
endif()
# JAC is a variant of the MPI build. MPI is switched on here, at the top
# level, so that the tests below are registered for an MPI binary as well:
# mpi_backend expects MPI, and the runs with two processes are the only
# test of the Jacobi exchange.
if( JAC AND NOT MPI )
  message( STATUS "JAC requires MPI; MPI is now on" )
  set( MPI ON )
endif()

# Compiler compatibility options are target-specific so CMake's build-type
# flags and user-provided CMAKE_Fortran_FLAGS remain effective.
# Tell IntelLLVM (ifx) explicitly apart from Intel classic (ifort);
# LLVMFlang/Flang/IBMFlang only with language probes and output evidence
# (candidate, no claimed support without qualification). Recognition alone
# is not support. Each toolchain has its own flags; warnings stay visible
# (no -W0/-w suppression), with a parser/baseline per compiler.
add_library( swan_build_options INTERFACE )
if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  target_compile_options( swan_build_options INTERFACE
    -std=f2018 -fimplicit-none -fno-second-underscore
    -ffree-line-length-none
  )
elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" )
  # Current Intel route (ifx). Visible strict set; no -W0.
  if( UNIX )
    target_compile_options( swan_build_options INTERFACE
      -warn all -assume byterecl -traceback -stand:f2018
    )
  elseif( WIN32 )
    target_compile_options( swan_build_options INTERFACE
      /warn:all /assume:byterecl /traceback /stand:f2018 /nologo
    )
  endif()
  message( STATUS "IntelLLVM (ifx) recognised: second implementation, qualification required (language probes + serial, then OpenMP/MPI/netCDF)" )
elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
  # Historical Intel classic (ifort); not a new strategic base.
  if( UNIX )
    target_compile_options( swan_build_options INTERFACE
      -warn all -assume byterecl -traceback -stand:f2018
    )
  elseif( WIN32 )
    target_compile_options( swan_build_options INTERFACE
      /warn:all /assume:byterecl /traceback /stand:f2018 /nologo
    )
  endif()
  message( STATUS "Intel classic (ifort) recognised: historical compatibility, not a new base; README names ifx as the current route" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(LLVMFlang|Flang|IBMFlang)$" )
  # Extra check on the candidate: recognition + visible flags, but
  # support only after language probes and output evidence for the target.
  target_compile_options( swan_build_options INTERFACE -std=f2018 )
  message( STATUS "${CMAKE_Fortran_COMPILER_ID} recognised as a candidate: requires language probes (implicit none(type,external), allocatables, C interop, OpenMP thread state) and serial output evidence" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  target_compile_options( swan_build_options INTERFACE "$<$<CONFIG:Release>:-fast>" )
  message( STATUS "PGI branch is historical; out of scope: no NVIDIA/NVFortran/PGI support" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Fujitsu" )
  target_compile_options( swan_build_options INTERFACE -O1 -staticlink -nwo )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(XL|IBMFlang)$" )
  # No -w suppression: without a visible set, an empty log is no proof of zero.
  target_compile_options( swan_build_options INTERFACE
    -O3 -qstrict -qarch=auto -qtune=auto -qcache=auto -qalign=4k
  )
  message( STATUS "XL/IBMFlang branch: requires a strict set and a parser per compiler, with its own baseline" )
else()
  message( FATAL_ERROR "-- Current Fortran compiler ${CMAKE_Fortran_COMPILER} not supported (ID=${CMAKE_Fortran_COMPILER_ID})" )
endif()

if( SWAN_RUNTIME_CHECKS )
  # Keep this on top of an optimizing build type. At -O0 GCC evaluates some
  # arithmetic differently from -O1 and up: X**N for a constant N above four
  # goes through libgcc's __powisf2 instead of the optimiser's product chain,
  # and X**2. through powf instead of X*X. Both are written out
  # (src/swan_integer_powers.f90, X**2), so those products are the same at
  # every level. What remains is -O3 being the odd one out: swancom1 and
  # SdsBabanin compute the same at -O0, -O1 and -O2 but differently at -O3,
  # and no single -O3-only flag is responsible.
  if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    target_compile_options( swan_build_options INTERFACE -fcheck=all )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$" )
    if( UNIX )
      target_compile_options( swan_build_options INTERFACE -check all )
    else()
      # Set up the native Windows form separately; do not reuse UNIX flags.
      target_compile_options( swan_build_options INTERFACE /check:all )
    endif()
  else()
    message( WARNING "SWAN_RUNTIME_CHECKS has no effect for ${CMAKE_Fortran_COMPILER_ID}" )
  endif()
endif()

if( SWAN_NATIVE )
  if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    # Native vector instructions may otherwise contract multiply-add pairs
    # and produce different rounding from portable Release builds.
    target_compile_options( swan_build_options INTERFACE -march=native -ffp-contract=off )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
    if( WIN32 )
      target_compile_options( swan_build_options INTERFACE /QxHost )
    else()
      target_compile_options( swan_build_options INTERFACE -xHost )
    endif()
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
    target_compile_options( swan_build_options INTERFACE -tp=native )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "XL" )
    # The standard XL options above already select and tune for this host.
  else()
    message( FATAL_ERROR
      "-- SWAN_NATIVE is not supported for ${CMAKE_Fortran_COMPILER_ID} Fortran"
    )
  endif()
endif()

if( SWAN_LTO )
  include( CheckIPOSupported )
  check_ipo_supported( RESULT SWAN_IPO_SUPPORTED OUTPUT SWAN_IPO_ERROR LANGUAGES Fortran )
  if( NOT SWAN_IPO_SUPPORTED )
    message( FATAL_ERROR "-- SWAN_LTO is not supported: ${SWAN_IPO_ERROR}" )
  endif()
  set( CMAKE_INTERPROCEDURAL_OPTIMIZATION ON )
endif()

# define bin/lib//mod/src directories
set( BIN ${CMAKE_BINARY_DIR}/bin )
set( LIB ${CMAKE_BINARY_DIR}/lib )
set( MOD ${CMAKE_BINARY_DIR}/mod )
set( SRC ${CMAKE_SOURCE_DIR}/src )

# have the bin and lib folders placed in the build folder
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${BIN} )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${LIB} )
set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${LIB} )

# have the modules placed in the mod folder
set( CMAKE_Fortran_MODULE_DIRECTORY ${MOD} )

# add find modules to the module path
list( APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" )

# The complex two-dimensional transforms use FFTW through a compatibility
# layer that preserves the former FFTPACK normalization and public routines.
find_package( FFTW3 REQUIRED )

# find Metis
if( METIS )
  find_package( Metis REQUIRED )
endif()

# find netCDF
if( NETCDF )
  # netCDF Fortran library is required
  find_package( NetCDF REQUIRED COMPONENTS Fortran )
endif()

# add the source to the library and place it in the lib folder
add_subdirectory( ${SRC} ${LIB} )

# create the SWAN executable and place it in the bin folder
add_subdirectory( ${SRC} ${BIN} )

# if desired, create the HCAT executable
add_subdirectory( ${SRC}/hcat )

