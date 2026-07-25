!------------------------------------------------------------------------------
module m_constants
use swan_kinds, only: swan_real
implicit none(type, external)
private

public :: dera, init_constants, pih, rade, sqrtg, trshdep
!------------------------------------------------------------------------------
!
! physical constants

real(swan_real) :: sqrtg   ! square root of grav
real(swan_real) :: trshdep ! threshold depth (=DEPMIN as given by SWAN)

! mathematical constants

real(swan_real) :: pih  ! pi/2
real(swan_real) :: dera ! conversion from degrees to radians
real(swan_real) :: rade ! conversion from radians to degrees

contains

!------------------------------------------------------------------------------
subroutine init_constants
!------------------------------------------------------------------------------

use swan_physical_settings
use swan_math_constants

pih  = 0.5_swan_real*PI
dera = PI/180.0_swan_real
rade = 180.0_swan_real/PI

!  physical constants

sqrtg   = sqrt(GRAV)
trshdep = DEPMIN

end subroutine

end module
