MODULE swan_io_context
!
!     Explicit contexts for the process-wide I/O and diagnostic state that
!     currently lives as module globals in OCPCOMM4. A context bundles the
!     standard stream units (io_context_t) or the error/trace status
!     (diagnostics_context_t) into an object that can be owned, saved and
!     restored, so that independent runs no longer have to share one hidden
!     set of globals.
!
!     This is a transitional layer: the OCPCOMM4 globals remain the values the
!     established service routines (MSGERR, STRACE, STPNOW) act on. The
!     capture/apply bridges copy those globals in and out of a context, which
!     lets a caller redirect and restore the streams or error state without a
!     flag-day rewrite of every call site. Subsystems are migrated onto the
!     contexts one at a time, matching the pattern of time_context_t and
!     command_reader_t.
!
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: io_context_t, diagnostics_context_t
   PUBLIC :: default_io_context, default_diagnostics_context
   PUBLIC :: capture_io_context, apply_io_context
   PUBLIC :: capture_diagnostics_context, apply_diagnostics_context

!     Fortran unit numbers of the standard SWAN streams (OCPCOMM4 defaults;
!     the authoritative runtime values are set by SWANINIT).
   TYPE :: io_context_t
      INTEGER :: INPUTF = 3     ! command input file ('INPUT')
      INTEGER :: PRINTF = 4     ! standard output file ('PRINT')
      INTEGER :: PRTEST = 4     ! test output file (defaults to PRINTF)
      INTEGER :: SCREEN = 6     ! screen
   CONTAINS
      PROCEDURE :: reset => reset_io_context
   END TYPE io_context_t

!     Error- and trace-reporting status shared by the diagnostic services.
   TYPE :: diagnostics_context_t
      INTEGER :: LEVERR = 0          ! highest error severity encountered
      INTEGER :: MAXERR = 1          ! highest severity allowed before stopping
      INTEGER :: ITRACE = 0          ! a trace message is printed up to ITRACE times
      INTEGER :: ITEST  = 0          ! amount of test output requested
      LOGICAL :: LTRACE = .FALSE.    ! whether STRACE is called
   CONTAINS
      PROCEDURE :: reset => reset_diagnostics_context
   END TYPE diagnostics_context_t

   TYPE(io_context_t),          SAVE, TARGET :: default_io_context
   TYPE(diagnostics_context_t), SAVE, TARGET :: default_diagnostics_context

CONTAINS

SUBROUTINE reset_io_context (context)
   CLASS(io_context_t), INTENT(INOUT) :: context

   context%INPUTF = 3
   context%PRINTF = 4
   context%PRTEST = 4
   context%SCREEN = 6
END SUBROUTINE reset_io_context

SUBROUTINE reset_diagnostics_context (context)
   CLASS(diagnostics_context_t), INTENT(INOUT) :: context

   context%LEVERR = 0
   context%MAXERR = 1
   context%ITRACE = 0
   context%ITEST  = 0
   context%LTRACE = .FALSE.
END SUBROUTINE reset_diagnostics_context

!     Copy the live OCPCOMM4 stream units into a context.
SUBROUTINE capture_io_context (context)
   USE OCPCOMM4, ONLY: INPUTF, PRINTF, PRTEST, SCREEN
   TYPE(io_context_t), INTENT(OUT) :: context

   context%INPUTF = INPUTF
   context%PRINTF = PRINTF
   context%PRTEST = PRTEST
   context%SCREEN = SCREEN
END SUBROUTINE capture_io_context

!     Publish a context's stream units to the OCPCOMM4 globals.
SUBROUTINE apply_io_context (context)
   USE OCPCOMM4, ONLY: INPUTF, PRINTF, PRTEST, SCREEN
   TYPE(io_context_t), INTENT(IN) :: context

   INPUTF = context%INPUTF
   PRINTF = context%PRINTF
   PRTEST = context%PRTEST
   SCREEN = context%SCREEN
END SUBROUTINE apply_io_context

!     Copy the live OCPCOMM4 error/trace status into a context.
SUBROUTINE capture_diagnostics_context (context)
   USE OCPCOMM4, ONLY: LEVERR, MAXERR, ITRACE, ITEST, LTRACE
   TYPE(diagnostics_context_t), INTENT(OUT) :: context

   context%LEVERR = LEVERR
   context%MAXERR = MAXERR
   context%ITRACE = ITRACE
   context%ITEST  = ITEST
   context%LTRACE = LTRACE
END SUBROUTINE capture_diagnostics_context

!     Publish a context's error/trace status to the OCPCOMM4 globals.
SUBROUTINE apply_diagnostics_context (context)
   USE OCPCOMM4, ONLY: LEVERR, MAXERR, ITRACE, ITEST, LTRACE
   TYPE(diagnostics_context_t), INTENT(IN) :: context

   LEVERR = context%LEVERR
   MAXERR = context%MAXERR
   ITRACE = context%ITRACE
   ITEST  = context%ITEST
   LTRACE = context%LTRACE
END SUBROUTINE apply_diagnostics_context

END MODULE swan_io_context
