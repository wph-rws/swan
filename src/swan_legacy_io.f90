MODULE swan_legacy_io
   use swan_io_limits, only: LENFNM
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: INAR2D, COPYCH

CONTAINS

SUBROUTINE INAR2D (ARR, MXA, MYA, NDSL, NDSD, IDFM, RFORM,&
&IDLA, VFAC, NHED, NHEDF)
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
!                                                                *
!*****************************************************************

   USE OCPCOMM2
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine


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
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.08: Erick Rogers
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     01.05, Feb. 90: Before reading values in the array are divided by
!                     in order to retain correct values for points where no
!                     value was given
!     01.06, Apr. 91: i/o status is printed if read error occurs
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.72, Sept 97: Corrected reading of heading lines for SERIES of files
!                     in dynamic mode
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.00, July 98: SWAN specific statements modified
!                     unformatted read: heading lines also read unformatted
!                     distinction between NDSD (data file) and NDSL (file list)
!     30.82, Sep. 98: Added INQUIRE statement to produce correct file name in
!                     case of a read error
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Sep. 00: Replaced computed GOTO with CASE construct
!     40.02, Sep. 00: Replaced reserved words IOSTAT with IOERR and STATUS with IERR
!     40.03, Jul. 00: END= added to READ statement for correct reading of series
!                     of files
!     40.03, Jul. 00: TRIM used to improve readability of message
!     40.13, Apr. 01: END=930 added in READ statement; corresponding error message added
!     40.08, Mar. 03: Changed an INQUIRE statement so that it does not produce
!                     misleading results.
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Reads a 2d array from dataset
!     is used to read e.g. bathymetry, one component of wind velocity
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IDFM   : input    format index
!     IDLAM  : input    lay-out indicator
!     MXA    : input    number of points along x-side of grid
!     MYA    : input    number of points along y-side of grid
!     NDSD   : input    unit number of the file from which to read the dataset
!     NDSL   : input    unit number of the file containing the list of filenames
!     NHEDF  : input    number of heading lines in the file (first lines).
!     NHEDL  : input    number of heading lines in the file
!                       before each array

   INTEGER   IDFM, IDLA, MXA, MYA, NDSD, NDSL, NHED, NHEDF

!     ARR    : input    results appear in this array
!     RFORM  : input    format used in reading data (char. string)
!     VFAC   : input    factor by which data must be multiplied.

   REAL      ARR(MXA,MYA), VFAC

   CHARACTER(LEN=*) :: RFORM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IERR   : ??
!     IENT   : number of entries into this subroutine
!     IOERR  : input   0 : Full messages printed
!                      -1: Only error messages printed
!                      -2: No messages printed
!              output  error indicator
!     IH     : ??
!     IX     : ??
!     IY     : ??
!     NUMFIL : ??

   INTEGER, SAVE :: IENT = 0
   INTEGER   IERR, IOERR, IH, IX, IY, NUMFIL

!     HEDLIN : Content of a header line

   CHARACTER(LEN=80) :: HEDLIN

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

   CALL STRACE (IENT, 'INAR2D')

IF (NDSD.LT.0) RETURN
!     no reading from file due to open error
!
!     *** NUMFIL is the number of files that is open in one time step  **
   NUMFIL = 0
   IF (ITEST.GE.100) THEN
      WRITE (PRINTF, "(' * TEST INAR2D *', 4I4, 1X, A16, I3, 1X, E12.4, I3)") MXA, MYA, NDSD, IDFM, RFORM,&
      &IDLA, VFAC, NHED
   ENDIF

!     Read heading lines, and print the same:

   input_files: DO
   IERR = 0
   IF (NHED.GT.0) THEN
      IF (IDFM.LT.0) THEN
         IF (ITEST.GE.30)&
         &WRITE (PRINTF, '(I3,A)') NHED, ' Heading lines'
         do IH=1, NHED
            READ (NDSD, IOSTAT=IERR)
            IF (IERR /= 0) EXIT
         end do
      ELSE
         do IH=1, NHED
            READ (NDSD, '(A80)', IOSTAT=IERR) HEDLIN
            IF (IERR /= 0) EXIT
            IF (IH.EQ.1) WRITE (PRINTF, '(A)') ' **  Heading lines  **'
            WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
         end do
      ENDIF
   ENDIF

!     divide existing values in the array by VFAC

   IF (IERR == 0) THEN
      do IY = 1, MYA
         do IX = 1, MXA
            ARR(IX,IY) = ARR(IX,IY) / VFAC
         end do
      end do

!     start reading of 2D-array

      IF (IDFM.EQ.0) THEN
!       free format read
         SELECT CASE(IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ELSE IF (IDFM.GT.0) THEN
!       read with fixed format
         SELECT CASE (IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ELSE
!       unformatted read
         SELECT CASE(IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ENDIF
   ENDIF

   IF (IERR == 0) EXIT input_files

!     *** End of data file, in case SERIES next file is opened
!     *** unit = NDSD is closed before the next one is opened

   IF (IS_IOSTAT_END(IERR)) THEN
      CLOSE(NDSD)
      NUMFIL = NUMFIL + 1
      IF (NUMFIL < 2 .AND. NDSL > 0) THEN
         READ (NDSL, '(A)', IOSTAT=IOERR) FILENM
         IF (IOERR /= 0) THEN
            FILENM='UNKNOWN_FILE'
            INQUIRE (UNIT=NDSL, NAME=FILENM)
            CALL MSGERR (2, 'Series of input files ended in '//TRIM(FILENM))
            RETURN
         END IF
      IF (IDFM.NE.-1) THEN
         IOERR = 0
         CALL FOR (NDSD, FILENM, 'OF', IOERR)
         IF (STPNOW()) RETURN
      ELSE
         IOERR = 0
         CALL FOR (NDSD, FILENM, 'OU', IOERR)
         IF (STPNOW()) RETURN
      ENDIF

!       Read heading lines, and print these:

      IF (NHEDF.GT.0) THEN
         IF (IDFM.LT.0) THEN
            IF (ITEST.GE.30) WRITE (PRINTF, '(I3,A,A)') NHEDF,&
            &' Heading lines at begin of file ', TRIM(FILENM)
            do IH=1, NHEDF
               READ (NDSD, IOSTAT=IERR)
               IF (IERR /= 0) EXIT
            end do
         ELSE
            WRITE (PRINTF, '(A,A,A)') ' **  Heading lines file ',&
            &TRIM(FILENM), ' **'
            do IH=1, NHEDF
               READ (NDSD, '(A80)', IOSTAT=IERR) HEDLIN
               IF (IERR /= 0) EXIT
               WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
            end do
         ENDIF
      ENDIF
         IF (IERR == 0) CYCLE input_files
      ENDIF

!     error message when end of file is encountered
!
!     --- initialize FILENM so that previous value is not used
!         in case unit NDSD does not exist
      FILENM='UNKNOWN_FILE'
!     ------------------------------------------------------------------
!     THIS INQUIRE STATEMENT IS PROBLEMATIC, SINCE (AT LEAST
!     SOMETIMES) NDSD HAS ALREADY BEEN CLOSED, SO THE INQUIRE
!     STATEMENT SHOULD NOT WORK.
!     ------------------------------------------------------------------
   INQUIRE (UNIT=NDSD, NAME=FILENM)
   CALL MSGERR (2, 'Unexpected end of file while reading '//&
   &TRIM(FILENM))
   NDSD = 0
      IDLA = -1
!     Value of IDLA=-1 signals end of file to calling program

   ELSE

!     --- initialize FILENM
      FILENM='UNKNOWN_FILE'
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      CALL MSGERR (2, 'Error while reading file '//TRIM(FILENM))
      WRITE (PRINTF, "(' i/o status ', I6)") IERR
      IDLA = -2
!     Value of IDLA=-2 signals read error to calling program
   END IF
   EXIT input_files
   END DO input_files

!     Multiply all values in the array by VFAC

do IY = 1, MYA
      do IX = 1, MXA
         ARR(IX,IY) = ARR(IX,IY) * VFAC
      end do
end do

IF (ITEST.GE.100 .OR. IDLA.LT.0) THEN
      do IY=MYA, 1, -1
         WRITE (PRINTF, "((1X, 10E12.4))") (ARR(IX,IY), IX=1,MXA)
      end do
   ENDIF
   RETURN

end subroutine INAR2D

SUBROUTINE COPYCH (STRING, MOVE, IARRAY, LENARR, IERR)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4

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
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     ver 30.01
!     30.81, Nov. 98: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     30.81, Jan. 99: Replaced variable FROM by FROM_ and TO by TO_ (because
!                     FROM and TO are reserved words)
!     40.03, Nov. 99: LENS2 removed from WRITE statement (value not yet
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     copy a string into an integer array or vice-versa
!     MOVE (TO_ or FROM_) indicates copying direction
!
!  3. METHOD
!
!     ---
!
!  4. ARGUMENT VARIABLES
!
!     IARRAY : output   an integer array
!     LENARR : input    length of array IARRAY
!     IERR   : output   error status: 0=no error, 9=end-of-file

   INTEGER, SAVE :: IENT = 0
   INTEGER, INTENT(INOUT) :: IARRAY(*)
   INTEGER, INTENT(IN) :: LENARR
   INTEGER, INTENT(OUT) :: IERR

!     STRING : i/o      a character string
!     MOVE   : input    if MOVE=TO_, STRING is copied to IARRAY
!                       if MOVE=FROM_, STRING is copied from IARRAY

   CHARACTER(LEN=1), INTENT(IN) :: MOVE
   CHARACTER(LEN=*), INTENT(INOUT) :: STRING

!  5. PARAMETER VARIABLES
!
!     OPMLFC : largest allowed integer character (ASCII) code + 1
!     OPMNLI : number of characters that can be stored in one integer number

   INTEGER, PARAMETER :: OPMNLI=4, OPMLFC=128

!  6. LOCAL VARIABLES
!
!     IC     : counter
!     IENT   : number of entries into this subroutine
!     II     : counter
!     LENS1  : length of a string
!     LENS2  : length of a string
!     LL     : integer representation of a character
!     MC1    : integer converted to/from character
!     MCHAR  : integer converted to/from character
!     MM     : aux. number
!     NSL    : position of character in string

   INTEGER   IC, II, LENS1, LENS2, LL, MC1, MCHAR, MM, NSL, NWORDS

!     CC     : a single character
!     CHAR   : intrinsic character function, translates integer to character
!     FROM_  : 'F'
!     TO_    : 'T'

   CHARACTER(LEN=1) :: CC
   CHARACTER(LEN=1), PARAMETER :: TO_ = 'T', FROM_ = 'F'

!  8. SUBROUTINE USED
!
!     CHAR, ICHAR (intrinsic functions)
!
!  9. SUBROUTINES CALLING
!
!     ---
!
! 10. ERROR MESSAGES
!
!     If PINDEX or PPLACE is out of range an error message is printed
!
! 11. REMARKS
!
!     ---
!
! 12. STRUCTURE
!
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
! 13. SOURCE TEXT

   CALL STRACE (IENT,'COPYCH')

   LENS1 = LEN(STRING)
   NWORDS = LENARR
   IF (NWORDS.GT.360) THEN
      CALL MSGERR (2, 'extremely long string in COPYCH')
      WRITE (PRTEST, *) ' test COPYCH  ',&
      &MOVE, LENARR, LENS1, ' ', STRING(1:80)
      NWORDS = 360
   ENDIF
   LENS2 = NWORDS*OPMNLI

   IF (MOVE .EQ. TO_) THEN
      NSL = 0
      do II = 1, NWORDS
         MCHAR = 0
         do IC = 1, OPMNLI
            NSL = NSL + 1
            IF (NSL .LE. LENS1) THEN
               CC = STRING(NSL:NSL)
            ELSE
               CC = ' '
            ENDIF
            LL = ICHAR(CC)
            IF (LL.GE.OPMLFC) THEN
               IERR = 803
               WRITE (PRTEST, "(' character cannot be copied: ', A1)") CC
               LL = ICHAR ('?')
            ENDIF
            MCHAR = OPMLFC*MCHAR + LL
!            IF (ITEST.GE.250) WRITE (PRTEST, *) NSL, CC, LL
         end do
         IARRAY(II) = MCHAR
      end do
      IF (LENS1.GT.LENS2) THEN
         check_capacity: do II = LENS1+1, LENS2
            IF (STRING(II:II) .NE. ' ') THEN
               IERR = 801
               CALL MSGERR(1, 'string longer than capacity of array')
               IF (ITEST.GE.50) WRITE (PRTEST, *) ' test COPYCH  ',&
               &MOVE, NWORDS, LENS1, LENS2, ' ', STRING(1:80)
               EXIT check_capacity
            ENDIF
         end do check_capacity
      ENDIF
   ELSE IF (MOVE .EQ. FROM_) THEN

!       character string copied from an array
!
!       first the string is filled with blanks
      STRING = '    '
      NSL = 0
      unpack_words: do II = 1, NWORDS
         MC1 = IARRAY(II)
         do IC = 1, OPMNLI
            MM  = OPMLFC ** (OPMNLI-IC)
            LL  = MC1 / MM
            NSL = NSL + 1
            IF (NSL .LE. LENS1) THEN
               STRING(NSL:NSL) = CHAR(LL)
            ELSE
               IF (CHAR(LL) .NE. ' ') THEN
                  IF (IERR.NE.802)&
                  &CALL MSGERR(1, 'string shorter than capacity of array')
                  IF (ITEST.GE.50) WRITE (PRTEST, *) ' test COPYCH  ',&
                  &MOVE, NWORDS, LENS1, LENS2, ' ', STRING
                  IERR = 802
                  EXIT unpack_words
               ENDIF
            ENDIF
            MC1 = MC1 - LL * MM
!           IF (ITEST.GE.250) WRITE (PRTEST, *) NSL, LL, STRING(NSL:NSL)
         end do
         IF (MC1.NE.0) WRITE (PRINTF, *) ' Error COPYCH'
      end do unpack_words
   ELSE
      CALL MSGERR (2, 'error COPYCH, argument MOVE')
   ENDIF
   IF (ITEST.GE.230) WRITE (PRTEST, "(' exit COPYCH ', I3, 1X, A20, 1X, A1, 4(1X,I12))") LENS1, STRING, MOVE,&
   &(IARRAY(II), II=1,NWORDS)
   RETURN
!*    end of subroutine COPYCH   **
end subroutine COPYCH

END MODULE swan_legacy_io
