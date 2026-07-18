# check MPI status
if( NOT MPI )
  return()
endif()

# Find MPI in this directory scope as well.  CMake imported targets created in
# src/ are not visible to this sibling subdirectory.
find_package( MPI REQUIRED COMPONENTS Fortran )

# define switches
if( WIN32 )
  set( HSWITCH -dos -cvis )
elseif( UNIX )
  set( HSWITCH -unix )
endif()

# set switches
set( SWAN_HCAT_GENERATED_SRC ${CMAKE_CURRENT_BINARY_DIR}/generated )
file( MAKE_DIRECTORY ${SWAN_HCAT_GENERATED_SRC} )
set_property( DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS
  "${SRC}/hcat/swanhcat.f90"
  "${CMAKE_SOURCE_DIR}/switch.py"
)
execute_process(
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/switch.py ${HSWITCH}
          --output-dir ${SWAN_HCAT_GENERATED_SRC} ${SRC}/hcat/swanhcat.f90
  COMMAND_ERROR_IS_FATAL ANY
)

# build and install the executable
set( swanhcat ${SWAN_HCAT_GENERATED_SRC}/swanhcat.f90 )
add_executable( ${HEXE} ${swanhcat} )
target_link_libraries( ${HEXE} PRIVATE swan_build_options MPI::MPI_Fortran )
install( TARGETS ${HEXE} RUNTIME DESTINATION bin )
