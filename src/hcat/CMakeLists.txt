# check MPI status
if( NOT MPI )
  return()
endif()

# define switches
if( WIN32 )
  set( HSWITCH -dos -cvis )
elseif( UNIX )
  set( HSWITCH -unix )
endif()

# set switches
if( PERL_FOUND )
  execute_process( COMMAND ${PERL_EXECUTABLE} ${CMAKE_SOURCE_DIR}/switch.pl -outdir ${SWITCHED}/hcat ${HSWITCH} ${SRC}/hcat/swanhcat.f90 )
else()
  message( FATAL_ERROR "-- Cannot build hcat without Perl" )
endif()

# build and install the executable
set( swanhcat ${SWITCHED}/hcat/swanhcat.f90 )
add_executable( ${HEXE} ${swanhcat} )
install( TARGETS ${HEXE} RUNTIME DESTINATION bin )
