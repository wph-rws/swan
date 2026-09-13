module swan_service_interfaces
!
!     Runtime services shared across SWAN: diagnostics, tracing, the stop
!     check and the floating-point comparisons.
!
!     These were external procedures in ocpmix.f90, declared here through an
!     interface block. They are module procedures now, so every call site is
!     checked against the real implementation. That became possible once the
!     parallel identity flags moved to swan_parallel_state: MSGERR and STRACE
!     need them while M_PARALL calls MSGERR, which used to be a cycle.
!
!     REPARM and LSPLIT stay in ocpmix.f90 on purpose: they need the command
!     parser, which uses this module.
!
   use swan_parallel_state, only: MASTER, INODE, IAMMASTER, PARLL
   use swan_build_config, only: timing_enabled
   use swan_io_limits, only: LENFNM
   use swan_project_metadata, only: INST, PROJID, PROJNR, PROJT1, PROJT2, PROJT3, VERTXT
   implicit none(type, external)
   private

   public :: eqreal, eqdble
   public :: msgerr, stpnow, strace, txpbla
   public :: swtsta, swtsto, swprti
   public :: timing_enabled
   public :: swi2b, swr2b
   public :: tabhed, bugfix

   interface
!     TXPBLA stays external: it lives in swanser.f90 next to the switch
!     activated timing routines that are compiled as externals too.
      subroutine txpbla(text, first, last)
         character(len=*), intent(inout) :: text
         integer, intent(out)             :: first, last
      end subroutine txpbla

!     SWTSTA, SWTSTO and SWPRTI are the timing backend in swanser.f90.
!     Callers guard them with the compile-time timing_enabled capability.
      subroutine swtsta(itimer)
         integer, intent(in) :: itimer
      end subroutine swtsta

      subroutine swtsto(itimer)
         integer, intent(in) :: itimer
      end subroutine swtsto

      subroutine swprti
      end subroutine swprti

!     SWI2B and SWR2B are the preserved Matlab-v4 byte converters selected
!     from output/swan_matlab_v4_bytes.f90.
      subroutine swi2b(ival, bval)
         integer, intent(in)  :: ival
         integer, intent(out) :: bval(4)
      end subroutine swi2b

      subroutine swr2b(rval, bval)
         real,    intent(in)  :: rval
         integer, intent(out) :: bval(4)
      end subroutine swr2b
   end interface

contains

!                                                                *
!*****************************************************************
!                                                                *
SUBROUTINE STRACE (IENT, SUBNAM, DIAG, IO)
!                                                                *
!*****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_parallel_state, ONLY: INODE, IAMMASTER, PARLL
   USE swan_io_context, ONLY: diagnostics_context_t, io_context_t

   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. AUTHORS
!
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     This subroutine produces depending on the value of 'ITRACE'
!     a message containing the name 'SUBNAM'. the purpose of this
!     action is to detect the entry of a subroutine.
!
!  3. METHOD
!
!     the first executable statement of subroutine 'AAA' has to
!     be : CALL STRACE(IENT,'AAA')
!     further is necessary : DATA IENT/0/
!     IF ITRACE=0, no message
!     IF ITRACE>0, a message is printed up to ITRACE times
!
!  4. ARGUMENT VARIABLES
!
!     IENT   :  i/o    Number of entries into the calling subroutine

   INTEGER, INTENT(INOUT) :: IENT

!     SUBNAM :  inp    name of the calling subroutine.

   CHARACTER(LEN=*), INTENT(IN) :: SUBNAM

!     DIAG   :  inp    optional trace context; supplies ITRACE instead of the
!                      OCPCOMM4 global when present.
!     IO     :  inp    optional stream context; supplies the trace output units
!                      instead of the OCPCOMM4 globals when present.

   TYPE(diagnostics_context_t), OPTIONAL, INTENT(IN) :: DIAG
   TYPE(io_context_t), OPTIONAL, INTENT(IN) :: IO

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!$ LOGICAL,EXTERNAL :: OMP_IN_PARALLEL

   INTEGER :: CUR_ITRACE, CUR_PRTEST, CUR_SCREEN, CUR_PRINTF
!
!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CUR_ITRACE = ITRACE
   CUR_PRTEST = PRTEST
   CUR_SCREEN = SCREEN
   CUR_PRINTF = PRINTF
   IF (PRESENT(DIAG)) CUR_ITRACE = DIAG%ITRACE
   IF (PRESENT(IO)) THEN
      CUR_PRTEST = IO%PRTEST
      CUR_SCREEN = IO%SCREEN
      CUR_PRINTF = IO%PRINTF
   END IF

   IF (CUR_ITRACE.EQ.0) RETURN
   IF (IENT.GT.CUR_ITRACE) RETURN
!$ IF (OMP_IN_PARALLEL()) THEN
!$OMP MASTER
!$    IENT=IENT+1
!$    WRITE (CUR_PRTEST, "(' ++ trace subr: ',A)") SUBNAM
!$    IF (CUR_SCREEN.NE.CUR_PRINTF) WRITE (CUR_SCREEN, "(' ++ trace subr: ',A)") SUBNAM
!$OMP END MASTER
!$ ELSE
      IENT=IENT+1
      WRITE (CUR_PRTEST, "(' ++ trace subr: ',A)") SUBNAM
      IF ( CUR_SCREEN.NE.CUR_PRINTF .AND. IAMMASTER )&
      &WRITE (CUR_SCREEN, "(' ++ trace subr: ',A)") SUBNAM
!$ ENDIF
   RETURN
!  *  END OF SUBR. STRACE  *
end subroutine STRACE
!*****************************************************************
!                                                                *
SUBROUTINE MSGERR (LEV,STRING,DIAG,IO)
!                                                                *
!*****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_parallel_state, ONLY: INODE, IAMMASTER, PARLL
   USE swan_io_context, ONLY: diagnostics_context_t, io_context_t

   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. AUTHORS
!
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.03, Aug. 00: variable ERRFNM introduced in order to get correct
!                     message on UNIX system
!     40.02, Sep. 00: Removed STOP statement
!     40.13, Nov. 01: OPEN statement instead of CALL FOR
!                     to prevent recursive subroutines calling
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Error messages are produced by subroutine MSGERR. if necessary
!     the value of LEVERR is increased.
!     In case of a high error level an error message file is opened
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     LEV    : indicates how severe the present error is
!     STRING : contents of the present error message

   INTEGER, INTENT(IN) :: LEV

   CHARACTER(LEN=*), INTENT(IN) :: STRING

!     DIAG   : optional error context; when present its LEVERR is raised and its
!              MAXERR sets the terminating-error threshold, instead of the
!              OCPCOMM4 globals.
!     IO     : optional stream context; when present the message echo goes to
!              its PRINTF instead of the global.

   TYPE(diagnostics_context_t), OPTIONAL, INTENT(INOUT) :: DIAG
   TYPE(io_context_t), OPTIONAL, INTENT(IN) :: IO

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IERR   : if non-zero error message file was already opened unsuccessfully
!     IERRF  : unit reference number of the error message file
!     ILPOS  : actual length of error message filename

   INTEGER, SAVE :: IERR=0, IERRF=0
   INTEGER ILPOS
   INTEGER CUR_MAXERR, CUR_PRINTF

!     ERRM   : error message prefix

   CHARACTER (LEN=17) :: ERRM

!     ERRFNM : name of error message file

   CHARACTER (LEN=LENFNM), SAVE :: ERRFNM = 'Errfile'

!  8. SUBROUTINE USED
!
!     ---
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT


   IF (PRESENT(DIAG)) THEN
      IF (LEV.GT.DIAG%LEVERR) DIAG%LEVERR=LEV
      CUR_MAXERR = DIAG%MAXERR
   ELSE
      IF (LEV.GT.LEVERR) LEVERR=LEV
      CUR_MAXERR = MAXERR
   END IF
   CUR_PRINTF = PRINTF
   IF (PRESENT(IO)) CUR_PRINTF = IO%PRINTF
   IF (LEV.EQ.0) THEN
      ERRM = 'Message          '
   ELSE IF (LEV.EQ.1) THEN
      ERRM = 'Warning          '
   ELSE IF (LEV.EQ.2) THEN
      ERRM = 'Error            '
   ELSE IF (LEV.EQ.3) THEN
      ERRM = 'Severe error     '
   ELSE
      ERRM = 'Terminating error'
   ENDIF
   WRITE (CUR_PRINTF,"(' ** ', A, ': ',A)") ERRM, STRING
   IF (LEV.GT.CUR_MAXERR) THEN
      IF (IERRF.EQ.0) THEN
         IF (IERR.NE.0) RETURN

!         append node number to ERRFNM in case of
!         parallel computing

         IF (PARLL) THEN
            ILPOS = INDEX ( ERRFNM, ' ' )-1
            WRITE(ERRFNM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
         END IF

         IERRF = 17
         OPEN (UNIT=IERRF, FILE=ERRFNM, FORM='FORMATTED')
      ENDIF
      WRITE (IERRF,"(A, ': ',A)") ERRM, STRING
   ENDIF

   RETURN

end subroutine MSGERR

!*****************************************************************
!                                                                *
LOGICAL FUNCTION STPNOW(DIAG)
   USE swan_io_context, ONLY: diagnostics_context_t
!                                                                *
!*****************************************************************

   USE swan_diagnostics_level

   IMPLICIT NONE(TYPE, EXTERNAL)

!     DIAG : optional error/trace context. When present its error status is
!            used instead of the OCPCOMM4 globals, so an isolated run can be
!            asked whether it must stop without consulting the shared state.
   TYPE(diagnostics_context_t), OPTIONAL, INTENT(IN) :: DIAG


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. Authors
!
!     30.82, Feb. 99: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.82: New function
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Function determines wheter the SWAN program should be stopped
!     due to a terminating error
!
!  3. Method
!
!     Compares two common variables (the maximum allowable error-level,
!     MAXERR and the actual error-level: LEVERR).
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT  : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0
   INTEGER :: CUR_LEVERR, CUR_MAXERR

!  8. SUBROUTINE USED
!
!$ LOGICAL,EXTERNAL :: OMP_IN_PARALLEL
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL  STRACE (IENT,'STPNOW')

   IF (PRESENT(DIAG)) THEN
      CUR_LEVERR = DIAG%LEVERR
      CUR_MAXERR = DIAG%MAXERR
   ELSE
      CUR_LEVERR = LEVERR
      CUR_MAXERR = MAXERR
   END IF

   IF (CUR_LEVERR .GE. 4) THEN
      STPNOW = .TRUE.
   ELSE
      STPNOW = .FALSE.
   END IF
   IF (CUR_MAXERR.EQ.-1) STPNOW = .FALSE.
!$ IF (OMP_IN_PARALLEL()) STPNOW = .FALSE.

   RETURN
end function STPNOW
!*****************************************************************
!                                                                *
SUBROUTINE TABHED (PROGNM, LPR)
!                                                                *
!*****************************************************************


   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. AUTHORS
!
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.13, Jan. 01: VERTXT replaces VERNUM
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     prints the table heading, containing:
!     run description, 3 lines
!     name of institute, program name,
!     project name, run id.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     LPR    : input    unit ref. nr. for output

   INTEGER LPR

!     PROGNM : input    program name

   CHARACTER(LEN=*) :: PROGNM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   WRITE (LPR, "('1', A72, ' | ', A40)") PROJT1, INST
   WRITE (LPR, "(1X, A72, ' | ', A, ' version: ', A)") PROJT2, PROGNM, VERTXT
   WRITE (LPR, "(1X, A72, ' | ', A16, 1X, A4)") PROJT3, PROJID, PROJNR
   WRITE (LPR, "(' --------------------------------------------------', '---------------------------------------------------------')")
   RETURN
end subroutine TABHED
!*****************************************************************
!                                                                *

!************************************************************************
!                                                                      *
PURE LOGICAL FUNCTION EQREAL (REAL1, REAL2 )
!     Zuiver: de enige bijwerking was
!     STRACE-diagnostiek; regressies draaien met ITRACE=0 en geen enkele
!     referentie bevat EQREAL-regels. Zie triage-doc.
!                                                                      *
!************************************************************************


   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. Authors
!
!     30.72 IJsbrand Haagsma
!     30.60 Nico Booij
!     40.04 Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Oct. 97: Changed from EXCYES to make floating point point comparisons
!     30.60, July 97: new subroutine (EXCYES)
!     40.04, Aug. 00: introduced EPSILON and TINY
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     to determine whether a value (usually a value read from file)
!     is an exception value or not
!     Later (30.72) used to make comparisons of floating points within reasonable bounds
!
!  3. Method (updated...)
!
!     Returns true when ABS(REAL1-REAL2) .LE. TINY(REAL1), that is, on bit
!     equality and on differences down in the denormal range.
!
!     Read the second test literally before changing it: EPS is
!     EPSILON(REAL1)*ABS(REAL1-REAL2), so "ABS(REAL1-REAL2) .LT. EPS" asks
!     whether a number is smaller than itself times 1.2E-7. For every
!     non-zero difference it is false, so the test contributes nothing and
!     this function is an exact comparison, not a tolerant one.
!
!     That is not a defect to be repaired by substituting a relative
!     tolerance such as EPSILON*MAX(ABS(REAL1),ABS(REAL2)). Of the call
!     sites, most compare a field value against an exception value
!     (OVEXCV, excval, excfld) and must match it exactly -- a tolerance
!     there would classify a legitimate depth next to the sentinel as
!     missing data. A further group compares against a literal zero, where
!     a relative tolerance is degenerate in the same way this expression is.
!     A site that genuinely needs a tolerance -- SwanCrossObstacle divides
!     by the determinant it tests here -- needs an absolute one chosen for
!     that geometry, stated at that site.
!
!     So the comment is corrected rather than the arithmetic. See
!     doc/bugfixes-tov-tu-delft-41.51.md.
!
!  4. Argument variables
!
!     REAL1  : input    value that is to be tested
!     REAL2  : input    given exception value

   REAL, INTENT(IN) :: REAL1, REAL2

!  5. Parameter variables
!
!  6. Local variables
!
!     EPS    : Small number (related to REAL1 and its difference with REAL2)

   REAL      EPS

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWREAD
!     SWDIM
!     SIRAY
!     SWBOUN
!     SWODDC
!     SWOEXD
!     SWOEXA
!     SWOEXF
!     SWPLOT
!     SWSPEC
!     ISOLIN
!     SNYPT2
!     INCTIM
!     INDBLE
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
! 13. Source text

   EQREAL = .FALSE.

   EPS = EPSILON(REAL1)*ABS(REAL1-REAL2)
   IF (EPS ==0) EPS = TINY(REAL1)
   IF (ABS(REAL1-REAL2) .GT. TINY(REAL1)) THEN
      IF (ABS(REAL1-REAL2) .LT. EPS) EQREAL = .TRUE.
   ELSE
      EQREAL = .TRUE.
   ENDIF
   RETURN
!     end of subroutine EQREAL
end function EQREAL
!************************************************************************
!                                                                      *
PURE LOGICAL FUNCTION EQDBLE (DBLE1, DBLE2)
!     Zuiver: de enige bijwerking was
!     STRACE-diagnostiek; zie EQREAL hierboven.
!                                                                      *
!************************************************************************


   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. Authors
!
!     Marcel Zijlema
!
!  1. Updates
!
!     July 2015: copied from EQREAL, adapted to REAL(KIND=KIND(0.0D0))
!
!  2. Purpose
!
!     to determine whether a value is an exception value or not
!
!  4. Argument variables
!
!     DBLE1  : input    value that is to be tested
!     DBLE2  : input    given exception value

    REAL(KIND=KIND(0.0D0)), INTENT(IN) :: DBLE1, DBLE2

!  5. Parameter variables
!
!  6. Local variables
!
!  8. Subroutines used
!
!  9. Subroutines calling
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
! 13. Source text

   EQDBLE = .FALSE.

   IF ( .NOT. DBLE1 /= DBLE2 ) EQDBLE = .TRUE.
   RETURN
!     end of subroutine EQDBLE
end function EQDBLE
!************************************************************************
!                                                                      *
SUBROUTINE BUGFIX (FIXABC)
!                                                                      *
!************************************************************************



!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!
!  0. Authors
!
!     40.03  Nico Booij
!
!  1. UPDATE
!
!     40.03, May  00: new subroutine
!
!  2. PURPOSE
!
!     Adding one character to the version character string
!
!  3. METHOD
!
!
!  4. Argument variables
!
!       FIXABC  char   input    character indicating a bugfix

   CHARACTER (LEN=1), INTENT(IN) :: FIXABC

!  5. Parameter variables
!
!  6. Local variables
!
!       IC      counter of characters

   INTEGER   IC

!  8. Subroutines used
!
!  9. Subroutines calling
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       for characters in VERTXT starting at end, do
!           if character is not blank
!           then replace previous character by FIXABC
!       ----------------------------------------------------------------
!
! 13. Source text

   DO IC = LEN(VERTXT), 1, -1
      IF (VERTXT(IC:IC) .NE. ' ') THEN
         VERTXT(IC+1:IC+1) = FIXABC
         EXIT
      ENDIF
   ENDDO
   RETURN
!     end of subroutine BUGFIX
end subroutine BUGFIX

end module swan_service_interfaces
