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
  # and no single -O3-only flag is responsible. With gfortran 16.2 that stays
  # within the tolerance of the stored references, and a Debug build passes
  # every test.
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

# Python runs the regression tests and the consistency checks below
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

  add_executable( test_build_config tests/test_build_config.f90 )
  target_link_libraries( test_build_config PRIVATE swan${VERSION} )
  add_test(
    NAME build_config
    COMMAND test_build_config
            ${SWAN_SEQUENTIAL_RECORD_LENGTH}
            ${SWAN_PRINT_RECORD_LENGTH}
            ${SWAN_DEFAULT_MAXIMUM_UNIT}
            ${SWAN_DEFAULT_FREE_UNIT_START}
  )

  add_executable( test_swan_library tests/test_swan_library.f90 )
  target_link_libraries( test_swan_library PRIVATE swan${VERSION} )
  add_test( NAME swan_library_contract COMMAND test_swan_library )

  if( METIS )
    set( SWAN_EXPECTED_METIS_BACKEND 1 )
  else()
    set( SWAN_EXPECTED_METIS_BACKEND 0 )
  endif()
  add_executable( test_metis_backend tests/test_metis_backend.f90 )
  target_link_libraries( test_metis_backend PRIVATE swan${VERSION} )
  add_test(
    NAME metis_backend
    COMMAND test_metis_backend ${SWAN_EXPECTED_METIS_BACKEND}
  )

  if( MPI )
    set( SWAN_EXPECTED_MPI_BACKEND 1 )
  else()
    set( SWAN_EXPECTED_MPI_BACKEND 0 )
  endif()
  add_executable( test_mpi_backend tests/test_mpi_backend.f90 )
  target_link_libraries( test_mpi_backend PRIVATE swan${VERSION} )
  add_test(
    NAME mpi_backend
    COMMAND test_mpi_backend ${SWAN_EXPECTED_MPI_BACKEND}
  )

  if( JAC )
    set( SWAN_EXPECTED_JACOBI_SWEEP 1 )
  else()
    set( SWAN_EXPECTED_JACOBI_SWEEP 0 )
  endif()
  add_executable( test_block_coloring_backend
    tests/test_block_coloring_backend.f90 )
  target_link_libraries( test_block_coloring_backend PRIVATE swan${VERSION} )
  add_test(
    NAME block_coloring_backend
    COMMAND test_block_coloring_backend ${SWAN_EXPECTED_JACOBI_SWEEP}
  )

  add_executable( test_sweep_scheduling_backend
    tests/test_sweep_scheduling_backend.f90 )
  target_link_libraries( test_sweep_scheduling_backend PRIVATE swan${VERSION} )
  add_test(
    NAME sweep_scheduling_backend
    COMMAND test_sweep_scheduling_backend ${SWAN_EXPECTED_JACOBI_SWEEP}
  )

  add_executable( test_partitioning_strategies
    tests/test_partitioning_strategies.f90 )
  target_link_libraries( test_partitioning_strategies PRIVATE swan${VERSION} )
  add_test(
    NAME partitioning_strategy
    COMMAND test_partitioning_strategies ${SWAN_EXPECTED_JACOBI_SWEEP}
  )

  if( FFRO )
    set( SWAN_EXPECTED_FIXED_FRONT 1 )
  else()
    set( SWAN_EXPECTED_FIXED_FRONT 0 )
  endif()
  add_executable( test_front_scheduling_backend
    tests/test_front_scheduling_backend.f90 )
  target_link_libraries( test_front_scheduling_backend PRIVATE swan${VERSION} )
  add_test(
    NAME front_scheduling_backend
    COMMAND test_front_scheduling_backend ${SWAN_EXPECTED_FIXED_FRONT}
  )
  # The fixed fronts do not keep neighbours apart; this property belongs only
  # to the graph ordering. Under FFRO the test could only print "skip" and
  # pass, which would count as coverage that is not there, so it is left out.
  if( NOT FFRO )
    add_executable( test_front_dependency_completeness
      tests/test_front_dependency_completeness.f90 )
    target_link_libraries( test_front_dependency_completeness PRIVATE swan${VERSION} )
    add_test(
      NAME front_dependency_completeness
      COMMAND test_front_dependency_completeness
    )
  endif()

  if( COH )
    set( SWAN_EXPECTED_COHERENT_COUPLING 1 )
  else()
    set( SWAN_EXPECTED_COHERENT_COUPLING 0 )
  endif()
  add_executable( test_mpi_lifecycle_backend
    tests/test_mpi_lifecycle_backend.f90 )
  target_link_libraries( test_mpi_lifecycle_backend PRIVATE swan${VERSION} )
  add_test(
    NAME mpi_lifecycle_backend
    COMMAND test_mpi_lifecycle_backend ${SWAN_EXPECTED_COHERENT_COUPLING}
  )

  if( ESMF )
    set( SWAN_EXPECTED_ESMF_COUPLING 1 )
  else()
    set( SWAN_EXPECTED_ESMF_COUPLING 0 )
  endif()
  add_executable( test_esmf_coupling_backend
    tests/test_esmf_coupling_backend.f90 )
  target_link_libraries( test_esmf_coupling_backend PRIVATE swan${VERSION} )
  add_test(
    NAME esmf_coupling_backend
    COMMAND test_esmf_coupling_backend ${SWAN_EXPECTED_ESMF_COUPLING}
  )
  # The ESMF build only adds the exponential wind growth when a coupler
  # switches on SAVE_SINBAC, and swan.exe never does. This test plays the
  # coupler for a small run. Own directory: it writes INPUT and PRINT.
  if( ESMF )
    add_executable( test_esmf_wind_input tests/test_esmf_wind_input.f90 )
    target_link_libraries( test_esmf_wind_input PRIVATE swan${VERSION} )
    add_test( NAME esmf_wind_input COMMAND test_esmf_wind_input )
    set_tests_properties( esmf_wind_input PROPERTIES
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/esmf_wind_input )
    file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/esmf_wind_input )
  endif()

  add_executable( test_state_clear tests/test_state_clear.f90 )
  target_link_libraries( test_state_clear PRIVATE swan${VERSION} )
  add_test( NAME state_clear COMMAND test_state_clear )

  add_executable( test_sweep_sector_budget tests/test_sweep_sector_budget.f90 )
  target_link_libraries( test_sweep_sector_budget PRIVATE swan${VERSION} )
  add_test( NAME sweep_sector_budget COMMAND test_sweep_sector_budget )

  # Runs SWMAIN twice in one process. It needs a directory of its own because
  # both runs write INPUT, PRINT and the result table in the working directory.
  add_executable( test_two_cases tests/test_two_cases.f90 )
  target_link_libraries( test_two_cases PRIVATE swan${VERSION} )
  add_test( NAME two_cases COMMAND ${CMAKE_COMMAND} -E env
            ${CMAKE_CURRENT_BINARY_DIR}/bin/test_two_cases )
  set_tests_properties( two_cases PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/two_cases )
  file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/two_cases )

  # Runs SWMAIN three times in one process with alternating physics (GEN3
  # KOMEN, 41.51 default, KOMEN again). Needs a directory of its own because
  # the runs write INPUT, PRINT and the result table in the working directory.
  add_executable( test_physics_reuse tests/test_physics_reuse.f90 )
  target_link_libraries( test_physics_reuse PRIVATE swan${VERSION} )
  add_test( NAME physics_reuse COMMAND ${CMAKE_COMMAND} -E env
            ${CMAKE_CURRENT_BINARY_DIR}/bin/test_physics_reuse )
  set_tests_properties( physics_reuse PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/physics_reuse )
  file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/physics_reuse )

  # Runs SWMAIN three times in one process with alternating grids (CGRID 4x2,
  # 6x3, 4x2 over the same domain). Needs a directory of its own because
  # the runs write INPUT, PRINT and the result table in the working directory.
  add_executable( test_grid_reuse tests/test_grid_reuse.f90 )
  target_link_libraries( test_grid_reuse PRIVATE swan${VERSION} )
  add_test( NAME grid_reuse COMMAND ${CMAKE_COMMAND} -E env
            ${CMAKE_CURRENT_BINARY_DIR}/bin/test_grid_reuse )
  set_tests_properties( grid_reuse PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/grid_reuse )
  file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/grid_reuse )

  # Runs SWMAIN three times in one process with alternating spectral
  # discretisations (CIRCLE 12/8, 16/10, 12/8 on the same grid). Needs a
  # directory of its own because the runs write INPUT, PRINT and the result
  # table in the working directory.
  add_executable( test_spectrum_reuse tests/test_spectrum_reuse.f90 )
  target_link_libraries( test_spectrum_reuse PRIVATE swan${VERSION} )
  add_test( NAME spectrum_reuse COMMAND ${CMAKE_COMMAND} -E env
            ${CMAKE_CURRENT_BINARY_DIR}/bin/test_spectrum_reuse )
  set_tests_properties( spectrum_reuse PROPERTIES
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/spectrum_reuse )
  file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/spectrum_reuse )

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
    NAME run_state_ownership
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_run_state_ownership.py
            --manifest ${CMAKE_SOURCE_DIR}/doc/run-state-ownership.json
  )

  # The bundled source-term argument groups only stay honest while the set of
  # procedures that may receive them is fixed: a bundle handed one level deeper
  # is shared state again under a new name. Guard it like the other invariants
  # the compiler cannot see.
  add_test(
    NAME source_bundle_boundaries
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_source_bundle_boundaries.py
  )

  # Keep the historical switch.py vocabulary ratcheted at zero use in product
  # sources; the script remains only as a downstream compatibility interface.
  add_test(
    NAME switch_manifest
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_switch_manifest.py
            --source-dir ${CMAKE_SOURCE_DIR}/src
            --switch ${CMAKE_SOURCE_DIR}/switch.py
            --manifest ${CMAKE_SOURCE_DIR}/doc/switch-manifest.json
  )
  # The gates run on a bare runner: a module that is present in a local
  # virtualenv but not installed there brings down the whole job on an
  # error that has nothing to do with SWAN. Check that the workflow
  # installs what the test layer imports.
  # The gate demands 'passed'; GitHub calls a successful job 'success'. That
  # difference lets every job pass and the gate still fail, and only shows
  # once the workflow really runs. Check the contract statically.
  add_test(
    NAME ci_gate_workflow_contract
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/ci_gate.py
            --check-workflow ${CMAKE_SOURCE_DIR}/.github/workflows/ci.yml
  )

  add_test(
    NAME ci_dependencies
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_ci_dependencies.py
            --workflow ${CMAKE_SOURCE_DIR}/.github/workflows/ci.yml
  )

  add_test(
    NAME switch_manifest_self_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_switch_manifest.py
            --switch ${CMAKE_SOURCE_DIR}/switch.py
            --self-test
  )
  add_test(
    NAME switch_compatibility
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/tests/test_switch_compatibility.py
  )
  add_test(
    NAME reference_check_negatives
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_reference_check.py -q
  )
  add_test(
    NAME strict_diagnostics_negatives
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_strict_diagnostics.py
            ${CMAKE_SOURCE_DIR}/tests/test_strict_diagnostics_triage.py -q
  )
  add_test(
    NAME build_matrix_negatives
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_build_matrix.py -q
  )
  add_test(
    NAME ci_gate_self_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/ci_gate.py --self-test
  )
  add_test(
    NAME mpi_field_mask_self_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/scripts/check_mpi_field_mask.py --self-test
  )
  # Tests that run SWAN get the built target in the command itself, not as a
  # test property: the OpenMP block below once replaced that property, and a
  # test without an executable skips -- which ctest counts as passed.
  set( SWAN_PYTEST_ENVIRONMENT
    SWAN_EXECUTABLE=$<TARGET_FILE:${EXE}> SWAN_EXECUTABLE_REQUIRED=1 )
  add_test(
    NAME lifetime_fault
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_lifetime_fault.py -q
  )

  add_test(
    NAME input_validation
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_input_validation.py -q
  )

  add_test(
    NAME breaking_off
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_breaking_off.py -q
  )

  # The ADCIRC grid reader (fort.14) is always built; the ADCIRC option does
  # not change that. No other case reads such a grid.
  add_test(
    NAME adcirc_grid
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_adcirc_grid.py -q
  )

  add_test(
    NAME surfbeat
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_surfbeat.py -q
  )

  add_test(
    NAME quick_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/quick_test/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/quick
  )

  # The curve output case covers the SPROUT XYPT builders (POINTS-list,
  # CURVE, RAY), which no other case reaches.
  add_test(
    NAME curve_output
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/curve_output/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/curve
  )
  set_tests_properties( curve_output PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )
  add_test(
    NAME curve_output_units
    COMMAND ${CMAKE_COMMAND} -E env ${SWAN_PYTEST_ENVIRONMENT}
            ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_curve_output.py -q
  )

  # The quick test leaves DIFFRAC off, so no other registered case reaches
  # DIFPAR or the diffraction terms in the propagation velocities. This case
  # is the quick test plus DIFFRAC.
  add_test(
    NAME diffraction
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/diffraction/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/diffraction
  )

  # GEN4 activates the quasi-coherent path (SWQCDFT/SWQCUFT/
  # SWQCWIG via QCSOURCE). No other registered case reaches it.
  add_test(
    NAME qcm_reference
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/qcm/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/qcm
  )

  # ST6/Babanin source terms with test-point output. This is the only
  # registered case that reaches the SSWELL_* routines: they sit behind
  # IWCAP == 8, which only ST6 whitecapping sets, and they are the only
  # kernels that need TESTFL/IPTST to place their diagnostics. Every other
  # test-point deck runs GEN3 WESTH, so without this case the
  # swell-dissipation column of the test output has no coverage at all, and
  # a defect there stays invisible to all other tests and to the
  # bit-identity gate.
  add_test(
    NAME source_terms_st6
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/source_terms_st6/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/source-terms-st6
  )

  if( MATL4 )
    set( MATLAB_TEST_BACKEND matlab4 )
  else()
    set( MATLAB_TEST_BACKEND matlab5 )
  endif()
  add_test(
    NAME matlab_output
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/matlab
            --backend ${MATLAB_TEST_BACKEND}
  )
  set_tests_properties( matlab_output PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )

  if( NOT NETCDF )
    add_test(
      NAME netcdf_disabled_plain_output
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory
              ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-disabled
              --backend plain-nc
    )
    set_tests_properties(
      netcdf_disabled_plain_output PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )
  endif()

  if( NETCDF )
    add_executable( check_netcdf_output tests/check_netcdf_output.f90 )
    target_link_libraries( check_netcdf_output PRIVATE NetCDF::NetCDF_Fortran )
    add_test(
      NAME netcdf_output
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf
              --backend netcdf
              --netcdf-checker $<TARGET_FILE:check_netcdf_output>
              --expected-center 0.87872
    )
    # Upstream cuts off a file name of more than 80 characters on its way
    # to the netCDF writer, which then creates a file with the wrong name
    # (BF-32).
    add_test(
      NAME netcdf_long_name
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-long-name
              --backend netcdf
              --netcdf-checker $<TARGET_FILE:check_netcdf_output>
              --expected-center 0.87872
              --long-name
    )
    set_tests_properties( netcdf_output PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )
    add_test(
      NAME netcdf_mixed_block_output
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-mixed
              --backend netcdf
              --netcdf-checker $<TARGET_FILE:check_netcdf_output>
              --expected-center 0.87872
              --mixed-block
    )
    set_tests_properties( netcdf_mixed_block_output PROPERTIES
      RUN_SERIAL TRUE TIMEOUT 180 )
    add_test(
      NAME netcdf_table_output
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-table
              --backend netcdf
              --netcdf-checker $<TARGET_FILE:check_netcdf_output>
              --expected-center 0.87872
              --netcdf-table
    )
    set_tests_properties( netcdf_table_output PROPERTIES
      RUN_SERIAL TRUE TIMEOUT 180 )
    add_test(
      NAME netcdf_spectrum_output
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory
              ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-spectrum
              --backend netcdf
              --netcdf-checker $<TARGET_FILE:check_netcdf_output>
              --expected-center 0.87893
              --netcdf-spectrum
    )
    set_tests_properties( netcdf_spectrum_output PROPERTIES
      RUN_SERIAL TRUE TIMEOUT 180 )
  endif()

  # With COH the host program manages MPI and SWAN does not split the domain:
  # every process computes the whole run. The runs below of a standalone
  # swan.exe over several processes then do not apply; the COH block after
  # them tests what that build does do.
  if( MPI AND NOT COH )
    # The Jacobi sweep exchanges action density between subdomains once per
    # iteration instead of following the wavefront across them, so a run split
    # over two processes converges to a slightly different state. That is the
    # scheme, not a defect, and it has its own reference; on one process the two
    # schemes agree, which is why only this test distinguishes them.
    if( JAC )
      set( QUICK_TEST_MPI_REFERENCE reference-jac )
      set( QUICK_TEST_MPI_CENTER 0.88086 )
      set( HCAT_HALO 1 )
    else()
      set( QUICK_TEST_MPI_REFERENCE reference )
      set( QUICK_TEST_MPI_CENTER 0.87872 )
      set( HCAT_HALO 3 )
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

    add_test(
      NAME matlab_output_mpi
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/matlab-mpi
              --backend ${MATLAB_TEST_BACKEND}
              --mpi-exec ${MPIEXEC_EXECUTABLE}
              --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
              --mpi-processes 2
              --reference ${QUICK_TEST_MPI_REFERENCE}
              --expected-center ${QUICK_TEST_MPI_CENTER}
    )
    set_tests_properties( matlab_output_mpi PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )

    if( NETCDF )
      add_test(
        NAME netcdf_output_mpi
        COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
                --swan-executable $<TARGET_FILE:${EXE}>
                --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-mpi
                --backend netcdf
                --netcdf-checker $<TARGET_FILE:check_netcdf_output>
                --mpi-exec ${MPIEXEC_EXECUTABLE}
                --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
                --mpi-processes 2
                --reference ${QUICK_TEST_MPI_REFERENCE}
                --expected-center ${QUICK_TEST_MPI_CENTER}
      )
      set_tests_properties( netcdf_output_mpi PROPERTIES
        RUN_SERIAL TRUE TIMEOUT 180 )
      add_test(
        NAME netcdf_long_name_mpi
        COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
                --swan-executable $<TARGET_FILE:${EXE}>
                --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-long-name-mpi
                --backend netcdf
                --netcdf-checker $<TARGET_FILE:check_netcdf_output>
                --mpi-exec ${MPIEXEC_EXECUTABLE}
                --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
                --mpi-processes 2
                --reference ${QUICK_TEST_MPI_REFERENCE}
                --expected-center ${QUICK_TEST_MPI_CENTER}
                --long-name
      )
      set_tests_properties( netcdf_long_name_mpi PROPERTIES
        RUN_SERIAL TRUE TIMEOUT 180 )
      add_test(
        NAME netcdf_spectrum_output_mpi
        COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_SOURCE_DIR}/examples/output_backends/run.py
                --swan-executable $<TARGET_FILE:${EXE}>
                --work-directory
                ${CMAKE_CURRENT_BINARY_DIR}/test-runs/netcdf-spectrum-mpi
                --backend netcdf
                --netcdf-checker $<TARGET_FILE:check_netcdf_output>
                --mpi-exec ${MPIEXEC_EXECUTABLE}
                --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
                --mpi-processes 2
                --reference ${QUICK_TEST_MPI_REFERENCE}
                --expected-center 0.87893
                --netcdf-spectrum
      )
      set_tests_properties( netcdf_spectrum_output_mpi PROPERTIES
        RUN_SERIAL TRUE TIMEOUT 180 )
    endif()

    # Preserve the complete hotfile compatibility route: SWAN writes one free
    # hotfile per MPI subdomain, hcat merges them, and a serial SWAN run reads
    # the combined file through INITIAL HOTSTART SINGLE.
    add_test(
      NAME hcat_hotstart
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/hcat/run.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --hcat-executable $<TARGET_FILE:${HEXE}>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/hcat
              --mpi-exec ${MPIEXEC_EXECUTABLE}
              --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
              --mpi-processes 2
              --halo ${HCAT_HALO}
              --reference ${QUICK_TEST_MPI_REFERENCE}
    )
    set_tests_properties( hcat_hotstart PROPERTIES RUN_SERIAL TRUE TIMEOUT 180 )

    if( METIS )
      add_test(
        NAME metis_partitioning
        COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_SOURCE_DIR}/examples/metis_partitioning/run.py
                --swan-executable $<TARGET_FILE:${EXE}>
                --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/metis
                --mpi-exec ${MPIEXEC_EXECUTABLE}
                --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
                --mpi-processes 2
      )
      set_tests_properties( metis_partitioning PROPERTIES
        RUN_SERIAL TRUE TIMEOUT 180 )
    endif()
    # Unstructured + MPI requires a partition. With METIS this case
    # passes with 2 ranks; without METIS it fails cleanly with the intended
    # message (no segfault). The expectation is fixed per configuration.
    if( METIS )
      set( MPI_UNSTRUCTURED_EXPECT success )
    else()
      set( MPI_UNSTRUCTURED_EXPECT clean-failure )
    endif()
    add_test(
      NAME mpi_unstructured_partition
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/tests/test_mpi_unstructured_partition.py
              --swan-executable $<TARGET_FILE:${EXE}>
              --mpi-exec ${MPIEXEC_EXECUTABLE}
              --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
              --expect ${MPI_UNSTRUCTURED_EXPECT}
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/mpi-unstructured
    )
    set_tests_properties( mpi_unstructured_partition PROPERTIES
      RUN_SERIAL TRUE TIMEOUT 180 )
  endif()
  # COH leaves starting and stopping MPI to the host program. A small
  # host program starts MPI, runs the quick test through the library and
  # checks that MPI is still open afterwards; the result must equal the
  # serial reference, because a COH run is never split up.
  if( MPI AND COH )
    add_executable( coherent_mpi_host tests/coherent_mpi_host.f90 )
    target_link_libraries( coherent_mpi_host PRIVATE swan${VERSION} )
    add_test(
      NAME coherent_mpi_host
      COMMAND ${Python3_EXECUTABLE}
              ${CMAKE_SOURCE_DIR}/examples/quick_test/run.py
              --swan-executable $<TARGET_FILE:coherent_mpi_host>
              --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/coherent
              --mpi-exec ${MPIEXEC_EXECUTABLE}
              --mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}
              --mpi-processes 1
    )
    set_tests_properties( coherent_mpi_host PROPERTIES
      RUN_SERIAL TRUE TIMEOUT 180 )
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
    diffraction qcm_reference source_terms_st6
    PROPERTIES RUN_SERIAL TRUE TIMEOUT 180
  )

  # Analytical shoaling/conservation test; the reference is linear theory
  # in the runner, not stored SWAN output.
  add_test(
    NAME shoaling
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/shoaling/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/shoaling
  )
  set_tests_properties( shoaling PROPERTIES RUN_SERIAL TRUE TIMEOUT 300 )
  add_test(
    NAME shoaling_units
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_shoaling.py -q
  )

  # Grid convergence test (first-order theory as reference).
  add_test(
    NAME grid_convergence
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/grid_convergence/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/grid-convergence
  )
  set_tests_properties( grid_convergence PROPERTIES RUN_SERIAL TRUE TIMEOUT 300 )
  add_test(
    NAME grid_convergence_units
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_grid_convergence.py -q
  )

  # Official Delft benchmark cases with an analytical gate (Ris 1997).
  # Tarballs are downloaded during the test and checked against SHA-256
  # (no redistribution); a failed download is an error, not a skip.
  add_test(
    NAME official_cases
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/official_cases/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/official
  )
  set_tests_properties( official_cases PROPERTIES RUN_SERIAL TRUE TIMEOUT 600 )
  add_test(
    NAME official_cases_units
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_official_cases.py -q
  )

  # Time-step sensitivity (reports only; the hard gates are structural).
  add_test(
    NAME time_convergence
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/time_convergence/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/time-convergence
  )
  set_tests_properties( time_convergence PROPERTIES RUN_SERIAL TRUE TIMEOUT 300 )
  add_test(
    NAME time_convergence_units
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_time_convergence.py -q
  )

  # Keep the small OpenMP reference runs deterministic while still exercising
  # real parallel work; otherwise the runtime inherits the host CPU count.
  if( OPENMP )
    get_property( SWAN_REGISTERED_TESTS DIRECTORY PROPERTY TESTS )
    set_property( TEST ${SWAN_REGISTERED_TESTS} APPEND PROPERTY
      ENVIRONMENT "OMP_NUM_THREADS=2" )
  endif()
endif()
