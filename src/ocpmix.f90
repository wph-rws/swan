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

module swan_input_helpers
!
!     Input helpers that sit above the command parser: REPARM reads the
!     parameters for an input array, LSPLIT splits a line into data items.
!     They live here rather than in swan_service_interfaces because they use
!     swan_input_parser, which in turn uses that module.
!
   implicit none
   private
   public :: REPARM, LSPLIT
contains

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

end module swan_input_helpers
