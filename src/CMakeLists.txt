# define switches
if( WIN32 )
  set( SWITCHES -dos )
  # The CVIS switch enables the nonstandard SHARED specifier in OPEN
  # statements. GNU Fortran rejects that DEC extension in the required
  # standard-conforming mode, and Windows file sharing already works without
  # it for the GNU runtime.
  if( NOT CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    list( APPEND SWITCHES -cvis )
  endif()
elseif( UNIX )
  set( SWITCHES -unix )
endif()

# include user-dependent switches
if( MPI OR JAC )
  if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
    list( APPEND SWITCHES -mpi )
  else()
    list( APPEND SWITCHES -impi -mpi )
  endif()
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
if( TIMG )
  list( APPEND SWITCHES -timg )
endif()
#
if( MATL4 )
  list( APPEND SWITCHES -matl4 )
endif()
#
if( SWAN_DEBUG_INVARIANTS )
  list( APPEND SWITCHES -debug-invariants )
endif()

# Generate configured Fortran sources in the build tree.  The .f90 files in
# the source tree remain the only editable sources; configured variants never
# obscure their Git history.
set( SWAN_GENERATED_SRC ${CMAKE_CURRENT_BINARY_DIR}/generated )
file( MAKE_DIRECTORY ${SWAN_GENERATED_SRC} )
file( GLOB SWAN_SOURCE_INPUTS CONFIGURE_DEPENDS "${SRC}/*.f90" )
set_property( DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  ${SWAN_SOURCE_INPUTS}
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
if( NOT TARGET swan${VERSION} )
  if( NOT NETCDF )
    include( ${SRC}/srclist.cmake )
    add_library( swan${VERSION} STATIC ${swan_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
  else()
    include( ${SRC}/srclistnc.cmake )
    add_library( swan${VERSION} STATIC ${swan_nc_src} )
    set_target_properties( swan${VERSION} PROPERTIES LINKER_LANGUAGE Fortran )
    target_include_directories( swan${VERSION} PUBLIC ${NetCDF_INCLUDE_DIRS} )
    target_link_libraries( swan${VERSION} PUBLIC NetCDF::NetCDF_Fortran )
  endif()
  target_link_libraries( swan${VERSION} PUBLIC swan_build_options FFTW3::fftw3 )
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
  set( swanmain ${SWAN_GENERATED_SRC}/swanmain.f90 )
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
