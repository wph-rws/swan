# delete build files and directories

# Source files are authoritative and must never be removed by clobber.
# Configured variants live exclusively below the build directory.
file( GLOB BUILD "${CMAKE_SOURCE_DIR}/build" )
set( DEL ${BUILD} )

# loop over list items and delete each one
foreach( D ${DEL} )
  if( EXISTS ${D} )
    file( REMOVE_RECURSE ${D} )
  endif()
endforeach()
