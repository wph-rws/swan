find_path(FFTW3_INCLUDE_DIR
  NAMES fftw3.f03
  DOC "Directory containing the FFTW modern Fortran interface"
)

find_library(FFTW3_LIBRARY
  NAMES fftw3 libfftw3-3
  DOC "FFTW double-precision library"
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FFTW3
  REQUIRED_VARS FFTW3_LIBRARY FFTW3_INCLUDE_DIR
)

if(FFTW3_FOUND AND NOT TARGET FFTW3::fftw3)
  add_library(FFTW3::fftw3 UNKNOWN IMPORTED)
  set_target_properties(FFTW3::fftw3 PROPERTIES
    IMPORTED_LOCATION "${FFTW3_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${FFTW3_INCLUDE_DIR}"
  )
endif()

mark_as_advanced(FFTW3_INCLUDE_DIR FFTW3_LIBRARY)
