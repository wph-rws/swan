# define switches
set( SWITCHES )
if( WIN32 )
  set( SWAN_NATIVE_PATH_SEPARATOR_CODE 92 )
  set( SWAN_ALTERNATE_PATH_SEPARATOR_CODE 47 )
elseif( UNIX )
  set( SWAN_NATIVE_PATH_SEPARATOR_CODE 47 )
  set( SWAN_ALTERNATE_PATH_SEPARATOR_CODE 92 )
endif()

# include user-dependent switches
if( MPI OR JAC )
  list( APPEND SWITCHES -mpi )
endif()
#
if( JAC )
  set( MPI ON )
  list( APPEND SWITCHES -jac )
endif()
#
if( METIS )
  list( APPEND SWITCHES -metis )
endif()
#
if( FFRO )
  list( APPEND SWITCHES -fixfront )
endif()
#
if( NETCDF )
  list( APPEND SWITCHES -netcdf )
endif()
#
if( MATL4 )
  list( APPEND SWITCHES -matl4 )
endif()
#
if( SWAN_LEGACY_CRAY_IO AND SWAN_LEGACY_SGI_IO )
  message( FATAL_ERROR
    "-- SWAN_LEGACY_CRAY_IO and SWAN_LEGACY_SGI_IO are mutually exclusive" )
endif()
#
# Generate only the value-based build configuration module. All other Fortran
# sources are compiled directly from the source tree; variant implementations
# are selected as whole files below.
set( SWAN_GENERATED_SRC ${CMAKE_CURRENT_BINARY_DIR}/generated )
file( MAKE_DIRECTORY ${SWAN_GENERATED_SRC} )
if( TIMG )
  set( SWAN_TIMING_ENABLED ".true." )
else()
  set( SWAN_TIMING_ENABLED ".false." )
endif()
if( SWAN_DEBUG_INVARIANTS )
  set( SWAN_INVARIANT_CHECKS_ENABLED ".true." )
else()
  set( SWAN_INVARIANT_CHECKS_ENABLED ".false." )
endif()
if( JAC )
  set( SWAN_JACOBI_SWEEP_ENABLED ".true." )
else()
  set( SWAN_JACOBI_SWEEP_ENABLED ".false." )
endif()
if( FFRO )
  set( SWAN_FIXED_FRONT_ENABLED ".true." )
else()
  set( SWAN_FIXED_FRONT_ENABLED ".false." )
endif()
# Fork-identiteit: herkenbare bouwstempel, TU Delft als referentielijn.
set( SWAN_FORK_IDENTITY "wph-rws/swan fork of SWAN 41.51" CACHE STRING
  "Fork identity embedded in the build" )
set( SWAN_UPSTREAM_REFERENCE "gitlab.tudelft.nl/citg/wavemodels/swan upstream main"
  CACHE STRING "Upstream reference line for this fork" )
set( SWAN_SEQUENTIAL_RECORD_LENGTH 0 )
set( SWAN_PRINT_RECORD_LENGTH 0 )
set( SWAN_DEFAULT_MAXIMUM_UNIT 99999 )
set( SWAN_DEFAULT_FREE_UNIT_START 21 )
if( SWAN_LEGACY_CRAY_IO )
  set( SWAN_SEQUENTIAL_RECORD_LENGTH 1000 )
  set( SWAN_PRINT_RECORD_LENGTH 2000 )
elseif( SWAN_LEGACY_SGI_IO )
  set( SWAN_SEQUENTIAL_RECORD_LENGTH 1000 )
  set( SWAN_PRINT_RECORD_LENGTH 2000 )
  set( SWAN_DEFAULT_MAXIMUM_UNIT 199 )
  set( SWAN_DEFAULT_FREE_UNIT_START 103 )
endif()
set( SWAN_SEQUENTIAL_RECORD_LENGTH ${SWAN_SEQUENTIAL_RECORD_LENGTH}
  CACHE INTERNAL "Configured sequential record length" FORCE )
set( SWAN_PRINT_RECORD_LENGTH ${SWAN_PRINT_RECORD_LENGTH}
  CACHE INTERNAL "Configured print-file record length" FORCE )
set( SWAN_DEFAULT_MAXIMUM_UNIT ${SWAN_DEFAULT_MAXIMUM_UNIT}
  CACHE INTERNAL "Configured default maximum Fortran unit" FORCE )
set( SWAN_DEFAULT_FREE_UNIT_START ${SWAN_DEFAULT_FREE_UNIT_START}
  CACHE INTERNAL "Configured first free Fortran unit" FORCE )
configure_file(
  ${SRC}/swan_build_config.f90.in
  ${SWAN_GENERATED_SRC}/swan_build_config.f90
  @ONLY
)
set_property( DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${SRC}/swan_build_config.f90.in"
)

# choose parallelisation paradigm: OpenMP or MPI
if( OPENMP AND MPI )
  message( FATAL_ERROR "-- Cannot use both OpenMP and MPI" )
elseif( OPENMP )
  find_package( OpenMP REQUIRED COMPONENTS Fortran )
  if( NOT OpenMP_Fortran_FOUND )
    message( FATAL_ERROR "-- Fortran compiler does not support OpenMP" )
  endif()
  # turn off MPI
  unset( MPI_Fortran_FOUND CACHE )
  unset( MPI_Fortran_COMPILER CACHE )
  unset( MPI_Fortran_LIBRARIES CACHE )
elseif( MPI )
  find_package( MPI REQUIRED COMPONENTS Fortran )
  # turn off OpenMP
  unset( OpenMP_Fortran_FLAGS CACHE )
else()
  # turn off both OpenMP and MPI
  unset( OpenMP_Fortran_FLAGS CACHE )
  unset( MPI_Fortran_FOUND CACHE )
  unset( MPI_Fortran_COMPILER CACHE )
  unset( MPI_Fortran_LIBRARIES CACHE )
endif()

# build library
if( MATL4 )
  set( SWAN_MATLAB_SOURCES
    ${SRC}/output/swan_matlab_v4_backend.f90
    ${SRC}/output/swan_matlab_v4_bytes.f90
  )
else()
  set( SWAN_MATLAB_SOURCES
    ${SRC}/output/swan_matlab_v5_backend.f90
  )
endif()
if( NETCDF )
  set( SWAN_NETCDF_BACKEND_SOURCE
    ${SRC}/output/swan_netcdf_backend_enabled.f90
  )
else()
  set( SWAN_NETCDF_BACKEND_SOURCE
    ${SRC}/output/swan_netcdf_backend_disabled.f90
  )
endif()
if( METIS )
  set( SWAN_METIS_BACKEND_SOURCE
    ${SRC}/parallel/swan_metis_backend_enabled.f90
  )
else()
  set( SWAN_METIS_BACKEND_SOURCE
    ${SRC}/parallel/swan_metis_backend_disabled.f90
  )
endif()
if( MPI )
  set( SWAN_MPI_BACKEND_SOURCE
    ${SRC}/parallel/swan_mpi_backend_enabled.f90
  )
else()
  set( SWAN_MPI_BACKEND_SOURCE
    ${SRC}/parallel/swan_mpi_backend_disabled.f90
  )
endif()
if( JAC )
  set( SWAN_BLOCK_COLORING_BACKEND_SOURCE
    ${SRC}/parallel/swan_block_coloring_backend_enabled.f90
  )
else()
  set( SWAN_BLOCK_COLORING_BACKEND_SOURCE
    ${SRC}/parallel/swan_block_coloring_backend_disabled.f90
  )
endif()
if( JAC )
  set( SWAN_SWEEP_EXCHANGE_BACKEND_SOURCE
    ${SRC}/parallel/swan_sweep_exchange_backend_jacobi.f90
  )
else()
  set( SWAN_SWEEP_EXCHANGE_BACKEND_SOURCE
    ${SRC}/parallel/swan_sweep_exchange_backend_wavefront.f90
  )
endif()
if( FFRO )
  set( SWAN_FRONT_SCHEDULING_BACKEND_SOURCE
    ${SRC}/propagation/swan_front_scheduling_backend_fixed.f90
  )
else()
  set( SWAN_FRONT_SCHEDULING_BACKEND_SOURCE
    ${SRC}/propagation/swan_front_scheduling_backend_graph.f90
  )
endif()
if( COH )
  set( SWAN_MPI_LIFECYCLE_BACKEND_SOURCE
    ${SRC}/parallel/swan_mpi_lifecycle_backend_coherent.f90
  )
else()
  set( SWAN_MPI_LIFECYCLE_BACKEND_SOURCE
    ${SRC}/parallel/swan_mpi_lifecycle_backend_standard.f90
  )
endif()
if( ESMF )
  set( SWAN_ESMF_COUPLING_BACKEND_SOURCE
    ${SRC}/coupling/swan_esmf_coupling_backend_enabled.f90
  )
else()
  set( SWAN_ESMF_COUPLING_BACKEND_SOURCE
    ${SRC}/coupling/swan_esmf_coupling_backend_disabled.f90
  )
endif()
if( NOT TARGET swan${VERSION} )
  if( NOT NETCDF )
    include( ${SRC}/srclist.cmake )
    list( APPEND swan_src
      ${SWAN_MATLAB_SOURCES}
      ${SWAN_NETCDF_BACKEND_SOURCE}
      ${SWAN_METIS_BACKEND_SOURCE}
      ${SWAN_MPI_BACKEND_SOURCE}
      ${SWAN_BLOCK_COLORING_BACKEND_SOURCE}
      ${SWAN_SWEEP_EXCHANGE_BACKEND_SOURCE}
      ${SWAN_FRONT_SCHEDULING_BACKEND_SOURCE}
      ${SWAN_MPI_LIFECYCLE_BACKEND_SOURCE}
      ${SWAN_ESMF_COUPLING_BACKEND_SOURCE}
    )
    add_library( swan${VERSION} STATIC ${swan_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
  else()
    include( ${SRC}/srclistnc.cmake )
    list( APPEND swan_nc_src
      ${SWAN_MATLAB_SOURCES}
      ${SWAN_NETCDF_BACKEND_SOURCE}
      ${SWAN_METIS_BACKEND_SOURCE}
      ${SWAN_MPI_BACKEND_SOURCE}
      ${SWAN_BLOCK_COLORING_BACKEND_SOURCE}
      ${SWAN_SWEEP_EXCHANGE_BACKEND_SOURCE}
      ${SWAN_FRONT_SCHEDULING_BACKEND_SOURCE}
      ${SWAN_MPI_LIFECYCLE_BACKEND_SOURCE}
      ${SWAN_ESMF_COUPLING_BACKEND_SOURCE}
    )
    add_library( swan${VERSION} STATIC ${swan_nc_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
    target_include_directories( swan${VERSION} PUBLIC ${NetCDF_INCLUDE_DIRS} )
    target_link_libraries( swan${VERSION} PUBLIC NetCDF::NetCDF_Fortran )
  endif()
  target_link_libraries( swan${VERSION} PUBLIC swan_build_options FFTW3::fftw3 )
  # Geen automatische niet-GNU-selectie. DEC-SHARED en mpif.h-wrapper
  # alleen bij bewezen capability (expliciete optie), niet bij "niet-GNU".
  if( SWAN_DEC_SHARED_IO )
    target_sources( swan${VERSION} PRIVATE
      ${SRC}/platform/swan_file_open_backend_dec.f90 )
  else()
    target_sources( swan${VERSION} PRIVATE
      ${SRC}/platform/swan_file_open_backend_standard.f90 )
  endif()
  if( SWAN_MPIF_COMPAT )
    if( NOT (MPI OR JAC) )
      message( FATAL_ERROR "SWAN_MPIF_COMPAT vereist MPI of JAC (capability, geen auto-selectie)" )
    endif()
    target_sources( swan${VERSION} PRIVATE
      ${SRC}/platform/swan_mpi_mpif_wrapper.f90 )
  endif()
  if( OPENMP )
    target_link_libraries( swan${VERSION} PUBLIC OpenMP::OpenMP_Fortran )
  endif()
  if( MPI )
    # The library sources USE MPI directly.  Linking the imported target here
    # supplies mpi.mod while compiling and propagates the MPI libraries to all
    # executables that consume SWAN.
    target_link_libraries( swan${VERSION} PUBLIC MPI::MPI_Fortran )
  endif()
  if( METIS_FOUND )
    target_include_directories( swan${VERSION} PUBLIC ${Metis_INCLUDE_DIR} )
    target_link_libraries( swan${VERSION} PUBLIC ${Metis_LIBRARIES} )
    # Controleer header én bibliotheek uit dezelfde installatie en leid
    # de ABI-breedtes af van metis.h, niet van het Fortran-kindnummer. Alleen
    # linken bewijst geen passende ABI; argumentdoorgifte en indexbereik worden
    # in swan_metis_interface.f90 en SwanParallel.f90 gecontroleerd.
    if( EXISTS "${Metis_INCLUDE_DIR}/metis.h" )
      file( STRINGS "${Metis_INCLUDE_DIR}/metis.h" _metis_idx
        REGEX "^#define IDXTYPEWIDTH" )
      file( STRINGS "${Metis_INCLUDE_DIR}/metis.h" _metis_real
        REGEX "^#define REALTYPEWIDTH" )
      message( STATUS "METIS header ${Metis_INCLUDE_DIR}/metis.h: ${_metis_idx}; ${_metis_real}" )
      message( STATUS "METIS library: ${Metis_LIBRARIES}" )
      if( NOT "${_metis_idx}" MATCHES "32" OR NOT "${_metis_real}" MATCHES "32" )
        message( FATAL_ERROR
          "Alleen 32/32-METIS is momenteel afgeleid in swan_metis_interface.f90 "
          "(gevonden: ${_metis_idx}; ${_metis_real}). Herleid de interface voor "
          "64-bit-indexen en test met 32- én 64-bit-bibliotheken; "
          "zie doc/support-matrix.md." )
      endif()
    else()
      message( WARNING "metis.h niet gevonden onder ${Metis_INCLUDE_DIR}; ABI-controle overgeslagen" )
    endif()
  endif()
endif()

# build executable
if( NOT TARGET ${EXE} )
  set( swanmain ${SRC}/swan_program.f90 )
  add_executable( ${EXE} ${swanmain} )
  target_link_libraries( ${EXE} PRIVATE swan${VERSION} )
endif()

# install the package
install( TARGETS ${EXE} swan${VERSION} RUNTIME DESTINATION bin LIBRARY DESTINATION lib ARCHIVE DESTINATION lib )
install( DIRECTORY ${CMAKE_BINARY_DIR}/mod/ DESTINATION mod )
install( DIRECTORY ${CMAKE_SOURCE_DIR}/doc/ DESTINATION doc )
install( DIRECTORY ${CMAKE_SOURCE_DIR}/misc/ DESTINATION misc )
install( DIRECTORY ${CMAKE_SOURCE_DIR}/tools/ DESTINATION tools )
if( WIN32 )
  install( PROGRAMS ${CMAKE_SOURCE_DIR}/bin/swanrun.bat DESTINATION bin )
elseif( UNIX )
  install( PROGRAMS ${CMAKE_SOURCE_DIR}/bin/swanrun DESTINATION bin )
endif()
