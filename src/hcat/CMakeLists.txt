# check MPI status
if( NOT MPI )
  return()
endif()

# Find MPI in this directory scope as well.  CMake imported targets created in
# src/ are not visible to this sibling subdirectory.
find_package( MPI REQUIRED COMPONENTS Fortran )

# build and install the executable
set( swanhcat ${SRC}/hcat/swanhcat.f90 )
add_executable( ${HEXE} ${swanhcat} )
target_link_libraries( ${HEXE} PRIVATE swan_build_options MPI::MPI_Fortran )
install( TARGETS ${HEXE} RUNTIME DESTINATION bin )
