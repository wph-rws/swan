module swan_diagnostics_level
!
!     How much SWAN says about what it is doing, and how bad an error has to be
!     before it stops.
!
!     Five dials set by the TEST and SET commands. They were in OCPCOMM4 next
!     to the unit numbers, which is why thirty-four files -- nearly half the
!     importers -- pulled in a sixteen-symbol state module to read `LTRACE`
!     alone, in the `IF (LTRACE) CALL STRACE (...)` that opens almost every
!     routine.
!
!     MSGERR and STRACE already accept an explicit diagnostics_context_t and
!     fall back on these globals when none is passed. Finishing that route is
!     what eventually removes this module; naming the concern is the step
!     before it.
!
   implicit none(type, external)
   private

   public :: ITEST, ITRACE, LTRACE, LEVERR, MAXERR

!     ITEST  : how much test output is wanted; set by TEST [itest]
   integer :: ITEST

!     ITRACE : a trace message is printed up to ITRACE times per routine
!     LTRACE : whether to call STRACE at all; true exactly when ITRACE > 0
   integer :: ITRACE
   logical :: LTRACE

!     LEVERR : severity of the worst error encountered so far
!     MAXERR : severity from which on the computation is abandoned
!              1=warnings, 2=errors, 3=severe, 4=terminating
   integer :: LEVERR, MAXERR
end module swan_diagnostics_level
