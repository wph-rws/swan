module swan_output_settings
!
!     Settings that steer how results are written, as opposed to the table that
!     describes what can be written (swan_output_variables).
!
!     They are the last inhabitants of SWCOMM1, which mixed them with the output
!     variable table, the output frame geometry and the unit vocabulary. Each of
!     those has its own module now.
!
   implicit none(type, external)
   private

   public :: MOUTPA, OUTPAR, SNAME, INRHOG, IUBOTR, ERRPTS

!     MOUTPA : size of OUTPAR
!     OUTPAR : parameters steering derived output quantities
   integer, parameter :: MOUTPA = 51
   real :: OUTPAR(MOUTPA)

!     SNAME  : name of the point set a request is being written for
   character(len=8) :: SNAME

!     INRHOG : 1 when energy output is scaled by rho*g
!     IUBOTR : 1 when an output request needs the bottom orbital velocity
   integer :: INRHOG, IUBOTR

!     ERRPTS : unit of the file listing points where the solver had trouble
   integer :: ERRPTS
end module swan_output_settings
