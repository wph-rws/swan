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

# Fortran project for SWAN
project( SWAN Fortran )

# Require portable, standard-conforming free-form Fortran.  CMake 3.20 and
# GNU Fortran 13 support Fortran 2018 as the newest enforceable language mode;
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
option( NETCDF "" OFF )
option( TIMG "Enable internal timing instrumentation" OFF )
option( MATL4 "Enable MATLAB version 4 output" OFF )
option( SWAN_NATIVE "Optimize for the CPU that performs the build" OFF )
option( SWAN_LTO "Enable interprocedural/link-time optimization" OFF )
option( SWAN_DEBUG_INVARIANTS "Enable diagnostic runtime invariant checks" OFF )
option( SWAN_RUNTIME_CHECKS "Enable compiler runtime checks (bounds, allocation, arguments)" OFF )

# Compiler compatibility options are target-specific so CMake's build-type
# flags and user-provided CMAKE_Fortran_FLAGS remain effective.
add_library( swan_build_options INTERFACE )
if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  target_compile_options( swan_build_options INTERFACE
    -std=f2018 -fimplicit-none -fno-second-underscore
    -ffree-line-length-none
  )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  if( UNIX )
    target_compile_options( swan_build_options INTERFACE
      -W0 -assume byterecl -traceback
      -diag-disable 8290 -diag-disable 8291 -diag-disable 8293
    )
  elseif( WIN32 )
    target_compile_options( swan_build_options INTERFACE
      /assume:byterecl /traceback /nowarn /nologo
      /Qdiag-disable:8290 /Qdiag-disable:8291 /Qdiag-disable:8293
    )
  endif()
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  target_compile_options( swan_build_options INTERFACE "$<$<CONFIG:Release>:-fast>" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Fujitsu" )
  target_compile_options( swan_build_options INTERFACE -O1 -staticlink -nwo )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "XL" )
  target_compile_options( swan_build_options INTERFACE
    -O3 -qstrict -qarch=auto -qtune=auto -qcache=auto -qalign=4k -w
  )
else()
  message( FATAL_ERROR "-- Current Fortran compiler ${CMAKE_Fortran_COMPILER} not supported" )
endif()

if( SWAN_RUNTIME_CHECKS )
  # Keep this on top of an optimizing build type. At -O0 the nonstationary
  # regular case drifts about 1.2e-3 in the mean period against the stored
  # reference, which is the optimization level's floating point and not a
  # defect; -O2 reproduces the reference exactly.
  if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    target_compile_options( swan_build_options INTERFACE -fcheck=all )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
    target_compile_options( swan_build_options INTERFACE -check all )
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

# Python is required for setting switches in the source code
find_package( Python3 REQUIRED COMPONENTS Interpreter )

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

include( CTest )
if( BUILD_TESTING )
  add_executable( test_fftw_compat tests/test_fftw_compat.f90 )
  target_link_libraries( test_fftw_compat PRIVATE swan${VERSION} )
  add_test( NAME fftw_compat COMMAND test_fftw_compat )

  add_executable( test_contexts tests/test_contexts.f90 )
  target_link_libraries( test_contexts PRIVATE swan${VERSION} )
  add_test( NAME contexts COMMAND test_contexts )

  add_executable( test_leaf_modules tests/test_leaf_modules.f90 )
  target_link_libraries( test_leaf_modules PRIVATE swan${VERSION} )
  add_test( NAME leaf_modules COMMAND test_leaf_modules )

  add_executable( test_physics_kernels tests/test_physics_kernels.f90 )
  target_link_libraries( test_physics_kernels PRIVATE swan${VERSION} )
  add_test( NAME physics_kernels COMMAND test_physics_kernels )

  # The context migration is designed around the thread-private state inventory,
  # so an undocumented THREADPRIVATE symbol has to fail a build rather than
  # silently invalidate the ownership design.
  add_test(
    NAME thread_state_manifest
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_thread_state.py
            --source-dir ${CMAKE_SOURCE_DIR}/src
            --manifest ${CMAKE_SOURCE_DIR}/doc/thread-state-manifest.md
  )

  add_test(
    NAME quick_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/quick_test/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/quick
  )
  if( MPI )
    # The Jacobi sweep exchanges action density between subdomains once per
    # iteration instead of following the wavefront across them, so a run split
    # over two processes converges to a slightly different state. That is the
    # scheme, not a defect, and it has its own reference; on one process the two
    # schemes agree, which is why only this test distinguishes them.
    if( JAC )
      set( QUICK_TEST_MPI_REFERENCE reference-jac )
    else()
      set( QUICK_TEST_MPI_REFERENCE reference )
    endif()
    add_test(
      NAME quick_test_mpi
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/quick_test/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/quick-mpi
              --mpi-exec ${MPIEXEC_EXECUTABLE}
              --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
              --mpi-processes 2
              --reference ${QUICK_TEST_MPI_REFERENCE}
    )
    set_tests_properties( quick_test_mpi PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )
  endif()
  # FFRO selects the fixed-front vertex ordering (!FXFRO) instead of the
  # graph-coloured one, which changes the order the unstructured solver sweeps
  # the mesh and so its converged state. That case has its own reference; the
  # structured cases are unaffected and fall back to the shared one.
  if( FFRO )
    set( NONSTATIONARY_REFERENCE reference-ffro )
  else()
    set( NONSTATIONARY_REFERENCE reference )
  endif()
  add_test(
    NAME nonstationary_examples
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/nonstationary/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/nonstationary
            --reference ${NONSTATIONARY_REFERENCE}
  )
  add_test(
    NAME nonlinear_interactions
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/nonlinear_interactions/run.py
            --case standard
            --no-plots
            --swan-executable $<TARGET_FILE:${EXE}>
            --results-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/nonlinear
  )
  set_tests_properties(
    quick_test nonstationary_examples nonlinear_interactions
    PROPERTIES RUN_SERIAL TRUE TIMEOUT 180
  )
endif()
