module swan_io_units
!
!     The unit numbers SWAN reads from and writes to, and the range it hands
!     out for files it opens itself.
!
!     Established by SWANINIT from the environment and the SET command, and
!     read wherever something is written. They travelled with the diagnostics
!     dials in OCPCOMM4, so a routine that only wanted PRINTF got the error
!     severity with it.
!
!     io_context_t in swan_io_context holds the same four output units and is
!     already threaded through MSGERR and STRACE. This module is where the
!     process-wide default lives until every caller passes a context.
!
   implicit none(type, external)
   private

   public :: PRINTF, PRTEST, SCREEN, INPUTF
   public :: FUNLO, FUNHI, IUNMIN, IUNMAX, HIOPEN

!     PRINTF : the PRINT file
!     PRTEST : the file test output goes to; equals PRINTF unless redirected
!     SCREEN : the screen, equal to PRINTF on batch-oriented systems
!     INPUTF : the command input file
   integer :: PRINTF, PRTEST, SCREEN, INPUTF

!     FUNLO, FUNHI : lowest and highest unit number free for SWAN to use
!     IUNMIN, IUNMAX : the range a unit number must lie in to be legal
!     HIOPEN : highest unit number currently open
   integer :: FUNLO, FUNHI
   integer :: IUNMIN, IUNMAX
   integer :: HIOPEN
end module swan_io_units
