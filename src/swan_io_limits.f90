module swan_io_limits
!
!     Compile-time limits for SWAN's file handling.
!
!     LENFNM lived in OCPCOMM2 next to the project metadata, so fourteen files
!     imported a mutable state module to obtain one constant. It is a limit, not
!     state, and this module has no dependencies of its own.
!
   implicit none(type, external)
   private

   public :: LENFNM

!     LENFNM : maximum length of a file name
   integer, parameter :: LENFNM = 140
end module swan_io_limits
