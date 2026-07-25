module swan_file_opening
   implicit none
   private
   public :: FOR

contains

SUBROUTINE FOR (IUNIT, DDNAME, SF, IOSTAT, IO)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_io_context, ONLY: io_context_t
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
!     30.13: Nico Booij
!     30.70: Nico Booij
!     30.82: IJsbrand Haagsma
!     34.01: IJsbrand Haagsma
!     40.00, 40.03: Nico Booij
!     40.41: Marcel Zijlema
!     41.20: Casey Dietrich
!
!  1. Updates
!
!     30.13, Jan. 96: new structure
!     30.70, Feb. 98: terminating error if input file does not exist
!     30.82, Nov. 98: Introduced recordlength of 1000 for new files to
!                     avoid errors on the Cray-J90
!     34.01, Feb. 99: STOP statement removed
!     40.00, Feb. 99: DIRCH2 replaces DIRCH1 in filenames
!     40.03, May  00: modification for Linux: local copy of filename
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!
!  1. PURPOSE
!
!     General open file routine.
!
!  2. METHOD
!
!     FORTRAN 77 OPEN option.
!                INQUIRE
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!       IUNIT   int     input   =0 : get free unit number
!                               >0 : fixed unit number
!                       output  allocated unit number
!       DDNAME  char    input   ddname/filename string (empty if IUNIT>0)
!       SF      char*2  input   file qualifiers
!                               1st char: O(ld),N(ew),S(cratch),U(nknown)
!                               2nd char: F(ormatted),U(nformatted)
!       IOSTAT  int     input   0 : Full messages printed
!                               -1: Only error messages printed
!                               -2: No messages printed
!                       output  error indicator

   INTEGER   IUNIT, IOSTAT
   CHARACTER(LEN=LENFNM) :: DDNAME
   CHARACTER(LEN=2) :: SF
   TYPE(io_context_t), OPTIONAL, INTENT(INOUT) :: IO

!  5. PARAMETER VAR. (CONSTANTS)
!
!     Error codes:
!
!       IOSTAT = IESUCC No errors
!       IOSTAT > 0      I/O error
!       IOSTAT = IENUNF No free unit number found
!       IOSTAT = IEUNBD Specified unit number out of bounds
!       IOSTAT = IENODD No filename supplied with IUNIT=0
!       IOSTAT = IEDDNM Incorrect filename supplied with IUNIT>0
!       IOSTAT = IEEXST Specified unit number does not exist
!       IOSTAT = IEOPEN Specified unit number already opened
!       IOSTAT = IESTAT Error in file qualifiers
!       IOSTAT = IENSCR Named scratch file
!       IOSTAT = IENSIO No specified I/O error

   INTEGER, PARAMETER :: IESUCC=0, IENUNF=-1, IEUNBD=-2, IENODD=-3
   INTEGER, PARAMETER :: IEDDNM=-4, IEEXST=-5, IEOPEN=-6, IESTAT=-7
   INTEGER, PARAMETER :: IENSCR=-12

!     EMPTY    blank string

   CHARACTER(LEN=*), PARAMETER :: EMPTY='        '

!  6. LOCAL VARIABLES
!
!     IENT      number of entries into this subroutine
!     IFO       format index
!     IFUN      free unit number
!     II        counter
!     IOSTTM    aux. error index
!     IS        file status index
!     IUTTM     aux. unit number

   INTEGER, SAVE :: IENT = 0
   INTEGER :: CUR_IUNMIN, CUR_IUNMAX, CUR_FUNLO, CUR_FUNHI
   INTEGER, SAVE :: IFUN = 0
   INTEGER   IFO, II, IOSTTM, IS, IUTTM

!     EXIST     if true, file exists
!     OPENED    if true, file is opened

   LOGICAL   EXIST, OPENED

!     S
!     F
!     FILTTM   auxiliary
!     FISTAT   file status, values: OLD, NEW, UNKNOWN
!     FORM     formatting, values: FORMATTED, UNFORMATTED
!     DDNAME_L local copy of DDNAME

   CHARACTER :: S, F
   CHARACTER(LEN=LENFNM) :: FILTTM, DDNAME_L
   CHARACTER(LEN=11), PARAMETER :: FISTAT(4) = &
      [CHARACTER(LEN=11) :: 'OLD', 'NEW', 'SCRATCH', 'UNKNOWN']
   CHARACTER(LEN=11), PARAMETER :: FORM(2) = &
      [CHARACTER(LEN=11) :: 'FORMATTED', 'UNFORMATTED']

!  4. SUBROUTINES USED
!
!
!  5. ERROR MESSAGES
!
!       and error messages added using MSGERR
!
!
!  6. REMARKS
!
!       Free unit number search interval: FUNLO<=IUNIT<=FUNHI
!       FUNLO, FUNHI, IUNMIN and IUNMAX were initialized by OCPINI,
!       they are transmitted via module OCPCOMM4
!
!  7. STRUCTURE
!
!       ----------------------------------------------------------------
!       Check file qualifiers
!       ----------------------------------------------------------------
!       If IUNIT = 0
!       Then If DDNAME = ' '
!            Then error message
!            Else Inquire to find if file exists and is opened,
!                 and if so, to find correct unit number
!                 If file is not opened
!                 Then get a free unit number, assign value to IUNIT
!                      open the file
!                 Else assign correct unit number to IUNIT
!       Else Inquire to find if file exists and is opened,
!                   and if so, to find correct filename
!            If file with unit nr IUNIT is already open
!            Then If filename does not correspond to DDNAME
!                 Then Close file with old filename and unit IUNIT
!                      Open file with new filename DDNAME and unit IUNIT
!            Else If DDNAME is not empty
!                 Then Open file with new filename DDNAME and unit IUNIT
!                 Else Open file with unit IUNIT
!       ----------------------------------------------------------------
!
!  8. SOURCE TEXT

   CALL STRACE (IENT, 'FOR')

!     IO : optional stream context. When present its free-unit bookkeeping is
!          used instead of the OCPCOMM4 globals, and the highest opened unit is
!          recorded back into it, so an isolated run can own its unit range.
!          Declared INTENT(INOUT) because HIOPEN is updated on a successful open.

   IF (PRESENT(IO)) THEN
      CUR_IUNMIN = IO%IUNMIN
      CUR_IUNMAX = IO%IUNMAX
      CUR_FUNLO  = IO%FUNLO
      CUR_FUNHI  = IO%FUNHI
   ELSE
      CUR_IUNMIN = IUNMIN
      CUR_IUNMAX = IUNMAX
      CUR_FUNLO  = FUNLO
      CUR_FUNHI  = FUNHI
   END IF

   IF (ITEST.GE.80) WRITE (PRTEST, "(' Entry FOR: ', I3, 1X, A36, A2, I7)") IUNIT, DDNAME, SF, IOSTAT
   DDNAME_L = DDNAME

!     check file qualifiers

   IF ((IUNIT.NE.0) .AND.&
   &((IUNIT .LT. CUR_IUNMIN) .OR. (IUNIT .GT. CUR_IUNMAX))) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3, 'Unit number out of range')
      IOSTAT= IEUNBD
      RETURN
   END IF

   S   = SF(1:1)
   F   = SF(2:2)
   IS  = INDEX('ONSU',S)
   IFO = INDEX('FU',F)
   IF ((IS .EQ. 0) .OR. (IFO .EQ. 0)) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3,'Error in file qualifiers')
      IOSTAT= IESTAT
      RETURN
   END IF

   IF ((S.EQ.'S').AND.(DDNAME.NE.EMPTY)) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3, 'Named scratch file')
      IOSTAT= IENSCR
      RETURN
   END IF

   IF (DDNAME.NE.EMPTY) THEN
!       directory separation character is replaced in filenames
      DO II = 1, LEN(DDNAME)
         IF (DDNAME(II:II).EQ.DIRCH1) DDNAME(II:II) = DIRCH2
      ENDDO
   ENDIF

   IF (IUNIT .EQ. 0) THEN
      IF (DDNAME.EQ.EMPTY) THEN
         IF (IOSTAT.GT.-1) CALL MSGERR (3, 'No filename given')
         IOSTAT= IENODD
         RETURN
      ELSE
!           Was the file opened already ?
         INQUIRE (FILE=DDNAME, IOSTAT=IOSTTM, EXIST=EXIST,&
         &OPENED=OPENED, NUMBER=IUTTM)
         IF (IOSTTM .NE. IESUCC) THEN
            IF (IOSTAT.GT.-1) CALL MSGERR (2,&
            &'Inquire failed, filename: '//DDNAME_L)
            IOSTAT = IOSTTM
            RETURN
         ENDIF
!           If file does not exist, print term. error
         IF (IS.EQ.1 .AND. .NOT. EXIST) THEN
            CALL MSGERR (4,&
            &'File cannot be opened/does not exist: '//DDNAME_L)
            IOSTAT = IEEXST
            RETURN
         END IF
         IF (OPENED) THEN
            IF (IOSTAT.GT.-1)&
            &CALL MSGERR (2, 'File is already opened: '//DDNAME_L)
            IOSTAT = IEOPEN
            IUNIT = IUTTM
            RETURN
         ENDIF
!           Assign free unit number
         DO
            IF (IFUN.EQ.0) THEN
               IFUN = CUR_FUNLO
            ELSE
               IFUN = IFUN + 1
            ENDIF
!Casey 160728: Merging the changes from Jason in an earlier version of SWAN.
            IF (IFUN.LT.411 .OR. IFUN.GT.417) EXIT
         END DO
         IUNIT = IFUN
         IF (IUNIT .GT. CUR_FUNHI) THEN
            IF (IOSTAT.GT.-2) CALL MSGERR (3, 'All free units used')
            IOSTAT= IENUNF
         ENDIF
      END IF
      OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,FILE=DDNAME,&
!/Cray      &RECL=1000,&
!/SGI      &RECL=1000,&
!CVIS      &SHARED,&
      &STATUS=FISTAT(IS),ACCESS='SEQUENTIAL',FORM=FORM(IFO))
      IF (open_failed()) RETURN
   ELSE
      INQUIRE (UNIT=IUNIT, NAME=FILTTM, IOSTAT=IOSTTM,&
      &EXIST=EXIST, OPENED=OPENED)
      IF (IOSTTM .NE. IESUCC) THEN
         IF (IOSTAT.GT.-1) CALL MSGERR (2,&
         &'Inquire failed, filename: '//FILTTM)
         IOSTAT = IOSTTM
         RETURN
      ENDIF
      IF (OPENED) THEN
         IF (IOSTAT.GT.-1) THEN
            CALL MSGERR (1,&
            &'File is already opened, filename: '//FILTTM)
         ENDIF
         IF (FILTTM.NE.DDNAME .AND. FILTTM.NE.EMPTY) THEN
            IF (IOSTAT.GT.-2) THEN
               WRITE (PRINTF, '(A, I4, 6A)') ' unit', IUNIT,&
               &' filenames: ', FILTTM, ' and: ', DDNAME
               CALL MSGERR (2, 'filename and unit number inconsistent')
            ENDIF
            IOSTAT = IEDDNM
!             close old file and open new one with given filename
            CLOSE (IUNIT)
            OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray            &RECL=1000,&
!/SGI            &RECL=1000,&
!CVIS            &SHARED,&
            &FILE=DDNAME,ACCESS='SEQUENTIAL',FORM=FORM(IFO))
            IF (open_failed()) RETURN
            IF (IOSTTM.NE.IESUCC) IOSTAT = IOSTTM
            HIOPEN = IFUN
            IF (PRESENT(IO)) IO%HIOPEN = IFUN
            IF (ITEST.GE.30) WRITE (PRINTF, "(' File opened: ', I6, 2X, A36, 2X, A2)") IUNIT, DDNAME, SF
            RETURN
         ENDIF
         IOSTAT = IEOPEN
         RETURN
      END IF
      IF (DDNAME.NE.EMPTY) THEN
         OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray         &RECL=1000,&
!/SGI         &RECL=1000,&
!CVIS         &SHARED,&
         &FILE=DDNAME,ACCESS='SEQUENTIAL',FORM=FORM(IFO))
         IF (open_failed()) RETURN
      ELSE
         OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray         &RECL=1000,&
!/SGI         &RECL=1000,&
!CVIS         &SHARED,&
         &ACCESS='SEQUENTIAL',FORM=FORM(IFO))
         IF (open_failed()) RETURN
      END IF
   END IF
   HIOPEN = IFUN
   IF (PRESENT(IO)) IO%HIOPEN = IFUN
IF (ITEST.GE.30) WRITE (PRINTF, "(' File opened: ', I6, 2X, A36, 2X, A2)") IUNIT, DDNAME, SF
   RETURN

CONTAINS

   LOGICAL FUNCTION open_failed()
      open_failed = IOSTTM.NE.IESUCC
      IF (.NOT.open_failed) RETURN
      IF (IOSTAT.GT.-2) THEN
         CALL MSGERR (3, 'File open failed, filename: '//DDNAME_L)
         WRITE (PRINTF,"(' File -> ', A36, 2X, ' IOSTAT=', I6, 4X, A2)") DDNAME, IOSTTM, SF
      ENDIF
      IUNIT = -1
      IOSTAT = IOSTTM
   END FUNCTION open_failed

!  *  end of subroutine  FOR  *
end subroutine FOR

end module swan_file_opening
