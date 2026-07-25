include( ${SRC}/srclist.cmake )

set( swan_nc_src
  ${swan_src}
  ${SRC}/nctablemd.f90
  ${SRC}/agioncmd.f90
  ${SRC}/swn_outnc.f90
)
