include( ${SRC}/srclist.cmake )

set( swan_nc_src
  ${swan_src}
  ${SWAN_GENERATED_SRC}/nctablemd.f90
  ${SWAN_GENERATED_SRC}/agioncmd.f90
  ${SWAN_GENERATED_SRC}/swn_outnc.f90
)
