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
# Referentielijn: TU Delft gitlab.tudelft.nl/citg/wavemodels/swan (upstream).
project( SWAN LANGUAGES Fortran DESCRIPTION "wph-rws/swan fork of SWAN 41.51" )

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

# Compiler compatibility options are target-specific so CMake's build-type
# flags and user-provided CMAKE_Fortran_FLAGS remain effective.
# IntelLLVM (ifx) expliciet onderscheiden van Intel-classic (ifort);
# LLVMFlang/Flang/IBMFlang alleen met taalproeven en uitvoerbewijs (kandidaat,
# geen geclaimde ondersteuning zonder kwalificatie). Herkenning alleen is geen
# ondersteuning. Per toolchain eigen vlaggen; waarschuwingen blijven zichtbaar
# (geen -W0/-w-suppressie) met een parser/baseline per compiler.
add_library( swan_build_options INTERFACE )
if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  target_compile_options( swan_build_options INTERFACE
    -std=f2018 -fimplicit-none -fno-second-underscore
    -ffree-line-length-none
  )
elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM" )
  # Actuele Intel-route (ifx). Zichtbare strikte set; geen -W0.
  if( UNIX )
    target_compile_options( swan_build_options INTERFACE
      -warn all -assume byterecl -traceback -stand:f2018
    )
  elseif( WIN32 )
    target_compile_options( swan_build_options INTERFACE
      /warn:all /assume:byterecl /traceback /stand:f2018 /nologo
    )
  endif()
  message( STATUS "IntelLLVM (ifx) herkend: tweede implementatie, kwalificatie vereist (taalproeven + serieel, dan OpenMP/MPI/netCDF); zie doc/support-matrix.md" )
elseif( CMAKE_Fortran_COMPILER_ID STREQUAL "Intel" )
  # Historische Intel-classic (ifort); geen nieuwe strategische basis.
  if( UNIX )
    target_compile_options( swan_build_options INTERFACE
      -warn all -assume byterecl -traceback -stand:f2018
    )
  elseif( WIN32 )
    target_compile_options( swan_build_options INTERFACE
      /warn:all /assume:byterecl /traceback /stand:f2018 /nologo
    )
  endif()
  message( STATUS "Intel-classic (ifort) herkend: historische compatibiliteit, geen nieuwe basis; README noemt ifx als actuele route" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(LLVMFlang|Flang|IBMFlang)$" )
  # Aanvullende controle op de kandidaat: herkenning + zichtbare vlaggen,
  # maar ondersteuning pas na taalproeven en uitvoerbewijs voor de doelomgeving.
  target_compile_options( swan_build_options INTERFACE -std=f2018 )
  message( STATUS "${CMAKE_Fortran_COMPILER_ID} herkend als kandidaat: vereist taalproeven (implicit none(type,external), allocatables, C-interop, OpenMP-threadtoestand) en serieel uitvoerbewijs; zie doc/support-matrix.md" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  target_compile_options( swan_build_options INTERFACE "$<$<CONFIG:Release>:-fast>" )
  message( STATUS "PGI-tak historisch; buiten scope: geen NVIDIA/NVFortran/PGI-ondersteuning" )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Fujitsu" )
  target_compile_options( swan_build_options INTERFACE -O1 -staticlink -nwo )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(XL|IBMFlang)$" )
  # Geen -w-suppressie: leeg log is zonder zichtbare set geen bewijs voor nul.
  target_compile_options( swan_build_options INTERFACE
    -O3 -qstrict -qarch=auto -qtune=auto -qcache=auto -qalign=4k
  )
  message( STATUS "XL/IBMFlang-tak: strikte set en parser per compiler vereist met eigen baseline; zie doc/support-matrix.md" )
else()
  message( FATAL_ERROR "-- Current Fortran compiler ${CMAKE_Fortran_COMPILER} not supported (ID=${CMAKE_Fortran_COMPILER_ID}); zie doc/support-matrix.md" )
endif()

if( SWAN_RUNTIME_CHECKS )
  # Keep this on top of an optimizing build type. At -O0 the nonstationary
  # regular case drifts about 1.2e-3 in the mean period against the stored
  # reference, which is the optimization level's floating point and not a
  # defect; -O2 reproduces the reference exactly.
  if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    target_compile_options( swan_build_options INTERFACE -fcheck=all )
  elseif( CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$" )
    if( UNIX )
      target_compile_options( swan_build_options INTERFACE -check all )
    else()
      # Native Windows-vorm apart inrichten; geen UNIX-vlaggen hergebruiken.
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

  add_executable( test_output_interfaces tests/test_output_interfaces.f90 )
  target_link_libraries( test_output_interfaces PRIVATE swan${VERSION} )
  add_test( NAME output_interfaces COMMAND test_output_interfaces )

  add_executable( test_obstacle_state tests/test_obstacle_state.f90 )
  target_link_libraries( test_obstacle_state PRIVATE swan${VERSION} )
  add_test( NAME obstacle_state COMMAND test_obstacle_state )

  add_executable( test_boundary_state tests/test_boundary_state.f90 )
  target_link_libraries( test_boundary_state PRIVATE swan${VERSION} )
  add_test( NAME boundary_state COMMAND test_boundary_state )

  add_executable( test_output_state tests/test_output_state.f90 )
  target_link_libraries( test_output_state PRIVATE swan${VERSION} )
  add_test( NAME output_state COMMAND test_output_state )

  add_executable( test_general_array_state tests/test_general_array_state.f90 )
  target_link_libraries( test_general_array_state PRIVATE swan${VERSION} )
  add_test( NAME general_array_state COMMAND test_general_array_state )

  add_executable( test_input_fields tests/test_input_fields.f90 )
  target_link_libraries( test_input_fields PRIVATE swan${VERSION} )
  add_test( NAME input_fields COMMAND test_input_fields )

  add_executable( test_vegetation_layers tests/test_vegetation_layers.f90 )
  target_link_libraries( test_vegetation_layers PRIVATE swan${VERSION} )
  add_test( NAME vegetation_layers COMMAND test_vegetation_layers )

  add_executable( test_global_grid tests/test_global_grid.f90 )
  target_link_libraries( test_global_grid PRIVATE swan${VERSION} )
  add_test( NAME global_grid COMMAND test_global_grid )

  add_executable( test_parallel_storage tests/test_parallel_storage.f90 )
  target_link_libraries( test_parallel_storage PRIVATE swan${VERSION} )
  add_test( NAME parallel_storage COMMAND test_parallel_storage )

  add_executable( test_grid_data tests/test_grid_data.f90 )
  target_link_libraries( test_grid_data PRIVATE swan${VERSION} )
  add_test( NAME grid_data COMMAND test_grid_data )

  add_executable( test_computation_data tests/test_computation_data.f90 )
  target_link_libraries( test_computation_data PRIVATE swan${VERSION} )
  add_test( NAME computation_data COMMAND test_computation_data )

  add_executable( test_iem_state tests/test_iem_state.f90 )
  target_link_libraries( test_iem_state PRIVATE swan${VERSION} )
  add_test( NAME iem_state COMMAND test_iem_state )

  add_executable( test_bragg_state tests/test_bragg_state.f90 )
  target_link_libraries( test_bragg_state PRIVATE swan${VERSION} )
  add_test( NAME bragg_state COMMAND test_bragg_state )

  add_executable( test_qcm_state tests/test_qcm_state.f90 )
  target_link_libraries( test_qcm_state PRIVATE swan${VERSION} )
  add_test( NAME qcm_state COMMAND test_qcm_state )

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
  # De poorten draaien op een kale runner: een module die hier in een
  # virtualenv aanwezig is maar daar niet geinstalleerd wordt, laat de hele
  # job vallen op een fout die niets met SWAN te maken heeft. Bewaak dat de
  # workflow installeert wat de testlaag importeert.
  # De gate eist 'passed'; GitHub noemt een geslaagde job 'success'. Dat
  # verschil laat alle jobs slagen en de gate toch falen, en blijkt pas als
  # de workflow echt draait. Controleer het contract statisch.
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
            ${CMAKE_SOURCE_DIR}/tests/test_strict_diagnostics.py -q
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
  add_test(
    NAME lifetime_fault
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_lifetime_fault.py -q
  )
  set_tests_properties( lifetime_fault PROPERTIES
    ENVIRONMENT "SWAN_EXECUTABLE=$<TARGET_FILE:${EXE}>" )

  add_test(
    NAME quick_test
    COMMAND ${Python3_EXECUTABLE}
            ${CMAKE_SOURCE_DIR}/examples/quick_test/run.py
            --swan-executable $<TARGET_FILE:${EXE}>
            --work-directory ${CMAKE_CURRENT_BINARY_DIR}/test-runs/quick
  )

  # The curve output case covers the SPROUT XYPT builders (POINTS-list,
  # CURVE, RAY) that previously had no coverage anywhere; its reference was
  # produced with the pre-repair binary.
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
    COMMAND ${Python3_EXECUTABLE} -m pytest
            ${CMAKE_SOURCE_DIR}/tests/test_curve_output.py -q
  )

  # The quick test leaves DIFFRAC off, so no other registered case reaches
  # DIFPAR or the diffraction terms in the propagation velocities. This case
  # is the quick test plus DIFFRAC; its reference was produced before the
  # diffraction state was moved out of M_DIFFR.
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

  if( MPI )
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
    # Ongestructureerd + MPI vereist partitie. Met METIS slaagt deze
    # case met 2 ranks; zonder METIS faalt ze schoon met de bedoelde melding
    # (geen segfault). De verwachting ligt per configuratie vast.
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
    diffraction qcm_reference
    PROPERTIES RUN_SERIAL TRUE TIMEOUT 180
  )

  # Analytische shoaling-/behoudsproef; referentie is de lineaire theorie
  # in de runner, geen opgeslagen SWAN-uitvoer.
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

  # Roosterconvergentieproef (eerste-orde-theorie als referentie).
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

  # Tijdstapgevoeligheid (rapporterend; harde poorten structureel).
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
    set_tests_properties( ${SWAN_REGISTERED_TESTS} PROPERTIES
      ENVIRONMENT "OMP_NUM_THREADS=2" )
  endif()
endif()
