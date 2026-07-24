!     Ocean Pack miscellaneous routines
!
!     real function DTTIME
!     subroutine    DTINTI
!     subroutine    DTRETI
!     char function DTTIWR
!     REPARM
!     INAR2D
!     STRACE
!     MSGERR
!     TABHED
!     FOR
!     logical function EQREAL  ( Checks whether REAL1 is appr.
!                                equal to REAL2                 )
!     logical function EQDBLE
!     LSPLIT             ( splits an input line into data items )
!     BUGFIX
!     COPYCH (copied from file OCPDPN)
!
!*******************************************************************
!                                                                  *
!*******************************************************************
!                                                                  *
!*****************************************************************
!                                                                *
!*****************************************************************
!                                                                *
!*****************************************************************
!                                                                *
SUBROUTINE REPARM (NDSL, NDSD, IDLA, IDFM, RFORM,&
&NHEDF, IDYN, NHEDT, LOGC, NHEDC)
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, ININTG, INKEYW, KEYWIS, WRNKEY
!                                                                *
!*****************************************************************

   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


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
!     30.74: IJsbrand Haagsma (Include version)
!     30.80: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.00: Nico Booij (modifications)
!     40.02: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.10, Dec. 95: [fac] is read before 'fname' in view of later
!                     reading of tables; argument VFAC removed
!                     arguments LOGT, NHEDT, LOGC, NHEDC added
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.00, Jan. 98: SWAN specific statements modified; argument list
!                     changed
!     30.82, Sep. 98: Added type specification for HEDLIN
!     30.80, Dec. 98: Initialisation of NHEDT and NHEDC
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Sep. 00: Replaced computed GOTO by CASE construct
!     40.03, Jul. 00: TRIM used to improve readability of message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     reads parameters for reading an array from users input
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IDFM   : output   format index
!     IDLA   : output   lay-out indicator
!     IDYN   : input    indicate whether grid is dynamic or not
!     NDSD   : ??       unit number of the file from which to read the dataset
!     NDSL   : ??       unit number of the file containing the list of filenames
!     NHEDF  : output   number of heading lines in the file (once in each file)
!     NHEDT  : output   number of heading lines in the file before reading
!                       each time level
!     NHEDC  : output   number of heading lines in the file before each
!                       or vector component

   INTEGER   IDFM, IDLA,  NDSL, NDSD, NHEDF, NHEDT, NHEDC, IDYN

!     LOGC   : input    if True more than one component is read from file

   LOGICAL   LOGC

!     RFORM  : output   reading format

   CHARACTER(LEN=*) :: RFORM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!
!     IENT   : Number of entries into this subroutine
!     IH     : ??
!     IOSTAT : input   0 : Full messages printed
!                      -1: Only error messages printed
!                      -2: No messages printed
!              output  error indicator

   INTEGER, SAVE :: IENT = 0
   INTEGER   IH, IOSTAT

!     HEDLIN : Content of a header line
!     KEYWIS : ??

      LOGICAL :: BNEW

!     OLDFIL : ??

   CHARACTER(LEN=80) :: HEDLIN
   CHARACTER(LEN=36), SAVE :: OLDFIL = ' '

!  8. SUBROUTINE USED


!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'REPARM')

   CALL INKEYW ('STA', '   ')
   IF (KEYWIS('SERI')) THEN
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
!       open namelist file and read first datafile name
      CALL FOR (NDSL, FILENM, 'OF', IOSTAT)
      IF (STPNOW()) RETURN
      READ(NDSL, '(A36)') FILENM
   ELSE
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
   ENDIF

   IF (FILENM.NE.OLDFIL) THEN
      BNEW = .TRUE.
      NDSD = 0
      IDLA = 1
      IDFM = 0
      RFORM = ' '
      NHEDF = 0
      OLDFIL = FILENM
   ELSE
      BNEW = .FALSE.
   ENDIF

   CALL INKEYW ('STA', ' ')
   IF (BNEW) THEN
!       read lay-out indicator
      CALL ININTG ('IDLA', IDLA, 'UNC', 1)
!       names changed and order changed, ver 30.20 (Swan)
      CALL ININTG ('NHEDF', NHEDF, 'UNC', 0)
      NHEDT = 0
      IF (IDYN.GT.0) THEN
         CALL ININTG ('NHEDT', NHEDT, 'UNC', 0)
      ENDIF
      NHEDC = 0
      IF (LOGC) THEN
         CALL ININTG ('NHEDVEC', NHEDC, 'UNC', 0)
      ENDIF
      CALL INKEYW ('STA', 'FREE')
      IDFM = 2
      IF (KEYWIS('FRE')) THEN
         IDFM = 0
      ELSE IF (KEYWIS('UNF')) THEN
         IDFM = -1
      ELSE IF (KEYWIS('FOR')) THEN
!         formatted read
         CALL ININTG ('IDFM', IDFM, 'NSKP', 2)
         SELECT CASE(IDFM)
          CASE(1)
            RFORM = '(10X,12F5.0)'
          CASE(2)
            CALL INCSTR ('FORM', RFORM, 'REQ', ' ')
          CASE(5)
            RFORM = '(16F5.0)'
          CASE(6)
            RFORM = '(12F6.0)'
          CASE(8)
            RFORM = '(10F8.0)'
          CASE DEFAULT
            CALL MSGERR (2, 'illegal format number')
            WRITE (PRINTF, "(' -> ', I6)") IDFM
         END SELECT
      ELSE
         CALL WRNKEY
         IDFM = 0
      ENDIF
!       --------------------------------------------------------
!                          open the file
!       --------------------------------------------------------
      IF (IDFM.NE.-1) THEN
         IOSTAT = 0
         CALL FOR (NDSD, FILENM, 'OF', IOSTAT)
         IF (STPNOW()) RETURN
         IF (NHEDF.GT.0) THEN
            WRITE (PRINTF, '(A,A,A)') ' **  Heading lines file ',&
            &TRIM(FILENM), ' **'
            DO IH=1, NHEDF
               READ (NDSD, '(A80)') HEDLIN
               WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
            ENDDO
         ENDIF
      ELSE
         IOSTAT = 0
         CALL FOR (NDSD, FILENM, 'OU', IOSTAT)
         IF (STPNOW()) RETURN
         DO IH=1, NHEDF
            READ (NDSD)
         ENDDO
      ENDIF
   ENDIF
   RETURN
end subroutine REPARM
!*****************************************************************
!                                                                *
!*****************************************************************
!                                                                *
SUBROUTINE STRACE (IENT, SUBNAM, DIAG, IO)
!                                                                *
!*****************************************************************

   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4
   USE M_PARALL
   USE swan_io_context, ONLY: diagnostics_context_t, io_context_t

   IMPLICIT NONE


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

   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4
   USE M_PARALL
   USE swan_io_context, ONLY: diagnostics_context_t, io_context_t

   IMPLICIT NONE


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
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_io_context, ONLY: diagnostics_context_t
!                                                                *
!*****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE

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

   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


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
LOGICAL FUNCTION EQREAL (REAL1, REAL2 )
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


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
!     Checks whether ABS(REAL1-REAL2) .LE. TINY(REAL1) or whether this
!     difference is .LE. then EPS (= EPSILON(REAL1)*ABS(REAL1-REAL2) )
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
!     IENT   : Number of entries into this subroutine

   REAL      EPS
   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE(IENT,'EQREAL')
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
LOGICAL FUNCTION EQDBLE (DBLE1, DBLE2)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


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

   REAL(KIND=KIND(0.0D0))    DBLE1, DBLE2

!  5. Parameter variables
!
!  6. Local variables
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE(IENT,'EQDBLE')
   EQDBLE = .FALSE.

   IF ( .NOT. DBLE1 /= DBLE2 ) EQDBLE = .TRUE.
   RETURN
!     end of subroutine EQDBLE
end function EQDBLE
!*******************************************************************
!                                                                  *
SUBROUTINE LSPLIT(RELINE, DATITM, NUMITM)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                  *
!*******************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


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
!     40.00, 40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.00, Jan. 98: New subroutine for SWAN
!     40.03, Jun. 00: declaration updated, TRIM added for readability
!                     test output added
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     a line read from a file is separated into single data items
!     each data item is found in a string DATITM
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     NUMITM : input    max number of data items in array

   INTEGER, INTENT(IN) :: NUMITM

!     DATITM : output   array of data items
!     RELINE : input    string (read from an input file)

   CHARACTER (LEN=*), INTENT(OUT) :: DATITM(NUMITM)
   CHARACTER (LEN=*), INTENT(IN) ::  RELINE

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     CRL    : a character of the input line RELINE
!     QUOTE  : ' i.e. string delimiter

   CHARACTER(LEN=1), PARAMETER :: QUOTE = "'"
   CHARACTER(LEN=1) :: CRL

!     ICR1   : ??
!     IENT   : Number of entries into this subroutine
!     ILL    : sequence number of character being processed
!     IITM   : counter of data items
!     LENLIN : lenght of an input line
!     RITM   : type of data, 0: empty string, 2: string enclosed
!              in quotes, 1: other

   INTEGER, SAVE :: IENT = 0
   INTEGER   ICR1, ILL, IITM, LENLIN, RITM

!     LCHSTR : if True, program is reading a string (enclosed in quotes)

   LOGICAL   LCHSTR

!  8. SUBROUTINE USED
!
!     ------
!
!  9. SUBROUTINES CALLING
!
!     SWBOUN
!
! 10. ERROR MESSAGES
!
!     Too many data items on input line
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'LSPLIT')

   LENLIN = LEN(RELINE)
   LCHSTR = .FALSE.
   RITM   = 1
   ICR1   = 1
   DO IITM = 1, NUMITM
      DATITM(IITM) = '    '
   ENDDO
   IF (ITEST.GE.150) WRITE (PRTEST,*) ' test LSPLIT ', RELINE

!     free format: separate the line into data items
!     blanks and commas serve as separation between data items
!     DATITM is string containing one data item

   IITM = 1
   do ILL = 1, LENLIN
      CRL = RELINE(ILL:ILL)
      IF (LCHSTR) THEN
!           reading a character string enclosed in quotes
         IF (CRL.EQ.QUOTE) THEN
!              closing quote
            LCHSTR = .FALSE.
            RITM   = 2
            IF (IITM.GT.NUMITM) THEN
               CALL MSGERR (2, 'too many items on input line')
               WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
            ENDIF
            DATITM(IITM) = RELINE (ICR1:ILL-1)
         ENDIF
      ELSE
         IF (CRL.EQ.',') THEN
            IF (RITM.EQ.0) THEN
!                 empty item
               IITM = IITM + 1
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               DATITM(IITM) = '    '
            ELSE
               IF (RITM.EQ.1) DATITM(IITM) = RELINE(ICR1:ILL)
               RITM = 0
            ENDIF
         ELSE IF (CRL.EQ.' ' .OR. CRL.EQ.default_command_reader%TABC) THEN
            IF (RITM.EQ.1) THEN
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               DATITM(IITM) = RELINE(ICR1:ILL)
               RITM = 2
            ENDIF
         ELSE
            IF (RITM.NE.1) THEN
               IITM = IITM + 1
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               IF (CRL.EQ.QUOTE) THEN
                  ICR1 = ILL+1
                  LCHSTR = .TRUE.
               ELSE
                  ICR1 = ILL
                  RITM = 1
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF (ITEST.GE.250) WRITE (PRTEST, "(' test LSPLIT ', A1, 3I3, 2X, A20)") CRL, RITM,&
      &IITM, ICR1
   end do
   IF (ITEST.GE.130) THEN
      DO IITM = 1, NUMITM
         WRITE (PRTEST, "(' LSPLIT data item ', I2, ' is: ', A)") IITM, DATITM(IITM)
      ENDDO
   ENDIF
   RETURN
end subroutine LSPLIT
!************************************************************************
!                                                                      *
SUBROUTINE BUGFIX (FIXABC)
!                                                                      *
!************************************************************************

   USE OCPCOMM2


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
!************************************************************************
!                                                                      *
