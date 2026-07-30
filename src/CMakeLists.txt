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
# Generate configured Fortran sources in the build tree.  The .f90 files in
# the source tree remain the only editable sources; configured variants never
# obscure their Git history.
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
file( GLOB SWAN_SOURCE_INPUTS CONFIGURE_DEPENDS "${SRC}/*.f90" )
set_property( DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  ${SWAN_SOURCE_INPUTS}
  "${SRC}/swan_build_config.f90.in"
  "${CMAKE_SOURCE_DIR}/switch.py"
)
execute_process(
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/switch.py ${SWITCHES}
          --output-dir ${SWAN_GENERATED_SRC} ${SRC}/*.f90
  COMMAND_ERROR_IS_FATAL ANY
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
if( NOT TARGET swan${VERSION} )
  if( NOT NETCDF )
    include( ${SRC}/srclist.cmake )
    list( APPEND swan_src
      ${SWAN_MATLAB_SOURCES}
      ${SWAN_NETCDF_BACKEND_SOURCE}
      ${SWAN_METIS_BACKEND_SOURCE}
    )
    add_library( swan${VERSION} STATIC ${swan_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
  else()
    include( ${SRC}/srclistnc.cmake )
    list( APPEND swan_nc_src
      ${SWAN_MATLAB_SOURCES}
      ${SWAN_NETCDF_BACKEND_SOURCE}
      ${SWAN_METIS_BACKEND_SOURCE}
    )
    add_library( swan${VERSION} STATIC ${swan_nc_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
    target_include_directories( swan${VERSION} PUBLIC ${NetCDF_INCLUDE_DIRS} )
    target_link_libraries( swan${VERSION} PUBLIC NetCDF::NetCDF_Fortran )
  endif()
  target_link_libraries( swan${VERSION} PUBLIC swan_build_options FFTW3::fftw3 )
  if( WIN32 AND NOT CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    target_sources( swan${VERSION} PRIVATE
      ${SRC}/platform/swan_file_open_backend_dec.f90 )
  else()
    target_sources( swan${VERSION} PRIVATE
      ${SRC}/platform/swan_file_open_backend_standard.f90 )
  endif()
  if( MPI AND NOT CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
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
  endif()
endif()

# build executable
if( NOT TARGET ${EXE} )
  set( swanmain ${SWAN_GENERATED_SRC}/swan_program.f90 )
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
