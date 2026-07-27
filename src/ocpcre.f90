module swan_input_parser
   use swan_kinds, only: swan_double
   use swan_io_context, only: io_context_t, diagnostics_context_t
   use swan_text_utilities, only: upcase
   use swan_io_limits, only: LENFNM
   implicit none(type, external)
   private

   integer, parameter, public :: LINELN = 180

   type, public :: command_reader_t
      character(len=4) :: BLANK = '    '
      character :: COMID = '$'
      character(len=LINELN) :: ELTEXT = ''
      character(len=4) :: ELTYPE = 'USED'
      character(len=LINELN) :: KAART = ''
      character :: KAR = ';'
      character(len=8) :: KEYWRD = ''
      character :: TABC = achar(9)
      integer :: ELINT = 0
      integer :: KARNR = LINELN + 1
      integer :: LENCST = 0
      real(swan_double) :: ELREAL = 0.0_swan_double
      logical :: CHGVAL = .false.
      ! Optional owned input stream. When io_bound is .true. the reader reads
      ! command lines from io%INPUTF instead of the shared OCPCOMM4 INPUTF, so
      ! two readers can consume two different input files. This is
      ! configuration, not parse state, so reset() leaves it untouched.
      type(io_context_t) :: io
      logical :: io_bound = .false.
      ! Optional owned diagnostics. When diag_bound is .true. the parser reports
      ! errors into diag (and to io's streams) instead of the shared globals, so
      ! two readers can keep separate logs. Configuration, not parse state.
      type(diagnostics_context_t) :: diag
      logical :: diag_bound = .false.
   contains
      procedure :: reset => reset_command_reader
   end type command_reader_t

   type(command_reader_t), public, save, target :: default_command_reader

   public :: eqcstr, getkar, ignore, incstr, inctim, indble
   public :: inintg, inintv, inkeyw, initvd, inreal, keywis
   public :: leesel, nwline, putkar, rdinit, upcase, wrnkey

   interface rdinit
      module procedure rdinit_default, rdinit_ctx
   end interface
   interface nwline
      module procedure nwline_default, nwline_ctx
   end interface
   interface inkeyw
      module procedure inkeyw_default, inkeyw_ctx
   end interface
   interface inreal
      module procedure inreal_default, inreal_ctx
   end interface
   interface indble
      module procedure indble_default, indble_ctx
   end interface
   interface inintg
      module procedure inintg_default, inintg_ctx
   end interface
   interface incstr
      module procedure incstr_default, incstr_ctx
   end interface
   interface inctim
      module procedure inctim_default, inctim_ctx
   end interface
   interface inintv
      module procedure inintv_default, inintv_ctx
   end interface
   interface initvd
      module procedure initvd_default, initvd_ctx
   end interface
   interface leesel
      module procedure leesel_default, leesel_ctx
   end interface
   interface getkar
      module procedure getkar_default, getkar_ctx
   end interface
   interface putkar
      module procedure putkar_default, putkar_ctx
   end interface
   interface keywis
      module procedure keywis_default, keywis_ctx
   end interface
   interface wrnkey
      module procedure wrnkey_default, wrnkey_ctx
   end interface
   interface ignore
      module procedure ignore_default, ignore_ctx
   end interface

contains

!  Report through the reader's own diagnostics/streams when it owns them,
!  otherwise through the shared globals. Keeps the 32 call sites in the parser
!  free of PRESENT/bound branching.
subroutine reader_msgerr (STATE, LEV, STRING)
   use swan_service_interfaces, only: MSGERR
   type(command_reader_t), intent(inout) :: STATE
   integer, intent(in) :: LEV
   character(len=*), intent(in) :: STRING

   if (STATE%diag_bound) then
      call MSGERR (LEV, STRING, STATE%diag, STATE%io)
   else if (STATE%io_bound) then
      call MSGERR (LEV, STRING, IO=STATE%io)
   else
      call MSGERR (LEV, STRING)
   end if
end subroutine reader_msgerr

!  Unit the reader echoes its input and messages to.
integer function reader_print_unit (STATE) result(UNIT)
   use OCPCOMM4, only: PRINTF
   type(command_reader_t), intent(in) :: STATE

   UNIT = PRINTF
   if (STATE%io_bound) UNIT = STATE%io%PRINTF
end function reader_print_unit

subroutine reset_command_reader(self)
   class(command_reader_t), intent(inout) :: self

   self%BLANK = '    '
   self%COMID = '$'
   self%ELTEXT = ''
   self%ELTYPE = 'USED'
   self%KAART = ''
   self%KAR = ';'
   self%KEYWRD = ''
   self%TABC = achar(9)
   self%ELINT = 0
   self%KARNR = LINELN + 1
   self%LENCST = 0
   self%ELREAL = 0.0_swan_double
   self%CHGVAL = .false.
end subroutine reset_command_reader


!               OCEAN PACK  command reading routines
!
!  Contents of this file:
!     RDINIT
!     NWLINE
!     INKEYW
!     INREAL
!     INDBLE
!     ININTG
!     INCSTR
!     INCTIM
!     ININTV
!     INITVD
!     LEESEL
!     GETKAR
!     PUTKAR
!     UPCASE
!     KEYWIS
!     WRNKEY
!     IGNORE
!     RDHMS
!
!****************************************************************
!                                                               *
SUBROUTINE RDINIT_CTX (STATE)
   USE swan_service_interfaces, ONLY: STRACE, EQREAL, MSGERR
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     Initialises the command reading system
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE (IENT,'RDINIT')
   STATE%KAR = ';'
   STATE%KARNR = LINELN + 1
   STATE%ELTYPE = 'USED'
   STATE%BLANK = '    '
   RETURN
end subroutine RDINIT_CTX
!****************************************************************
!                                                               *
SUBROUTINE NWLINE_CTX (STATE)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     34.01: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     34.01, Feb. 99: Changed STOP statement in a MSGERR(4,'message')
!     40.03, Apr. 99: length of command lines changed from 80 to LINELN
!                     name of input file included in error message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Jumps to reading of the next input line,
!     if the end of the previous one is reached.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE (IENT,'NWLINE')
   DO
      IF ((STATE%ELTYPE.EQ.'USED').OR.(STATE%ELTYPE.EQ.'EOR')) CALL LEESEL (STATE)
      IF (STATE%ELTYPE.EQ.'EOF') EXIT
      IF (STATE%ELTYPE.EQ.'KEY' .AND. STATE%KEYWRD.NE.'        ') EXIT
      IF (STATE%ELTYPE.EQ.'INT' .OR. STATE%ELTYPE.EQ.'REAL' .OR. &
          STATE%ELTYPE.EQ.'CHAR' .OR. STATE%KARNR.LE.LINELN) EXIT
!     The end of the previous line is reached, there are no more
!     unprocessed data items on that line.
!     Jump to new line can take place.
   WRITE (reader_print_unit(STATE),"(A4)") '    '
   STATE%KARNR=0
   STATE%KAR=' '
   STATE%ELTYPE='USED'
   END DO
   IF (STATE%ELTYPE.EQ.'EOF' .AND. ITEST.GE.10) THEN
      INQUIRE (UNIT=MERGE(STATE%io%INPUTF, INPUTF, STATE%io_bound), NAME=FILENM)
      WRITE (reader_print_unit(STATE), *) ' end of input file '//FILENM
   ENDIF
end subroutine NWLINE_CTX
!****************************************************************
!                                                               *
SUBROUTINE INKEYW_CTX (STATE, KONT, CSTA)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     ver 30.70, Jan. 1998: data type 'OTHR' is condidered
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     this subroutine reads a keyword.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     KONT   : action to be taken if no keyword is found in input:
!              'REQ' (required) error message
!              'STA' (standard) the value of csta is assigned to keywrd.
!
!     CSTA   : see above.

   CHARACTER(LEN=*), INTENT(IN) :: CSTA, KONT

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     LENS   : length of default string (CSTA)

   INTEGER, SAVE :: IENT = 0
   INTEGER   LENS

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

   CALL  STRACE ( IENT, 'INKEYW')

!     if necessary, a new data item is read.

   keyword_search: DO
   IF (STATE%ELTYPE.EQ.'KEY' .AND. STATE%KEYWRD.NE.'        ') EXIT keyword_search
   IF (STATE%ELTYPE.EQ.'KEY' .OR. STATE%ELTYPE.EQ.'EOR' .OR. &
       STATE%ELTYPE.EQ.'USED') CALL LEESEL (STATE)
   IF (STATE%ELTYPE.EQ.'KEY') EXIT keyword_search
!     KEYWORD IS READ
   IF ((KONT.EQ.'STA').OR.(KONT.EQ.'NSKP')) THEN
      LENS = LEN(CSTA)
      IF (LENS.GE.8) THEN
         STATE%KEYWRD = CSTA(1:8)
      ELSE
         STATE%KEYWRD = '        '
         STATE%KEYWRD(1:LENS) = CSTA
      ENDIF
      EXIT keyword_search
   ENDIF
!     at the end of the input 'STOP' is generated.
   IF (STATE%ELTYPE.EQ.'EOF') THEN
      STATE%KEYWRD='STOP'
      CALL reader_msgerr (STATE, 2, 'STOP statement is missing')
      EXIT keyword_search
   ENDIF
!     ----------------------------------------------------------
!     Data appear where a keyword is expected.
!     The user must be informed.
!     ----------------------------------------------------------
   IF (STATE%ELTYPE.EQ.'EOR') THEN
      STATE%KEYWRD = '        '
      EXIT keyword_search
   ENDIF
   IF (STATE%ELTYPE.EQ.'INT') THEN
      CALL reader_msgerr (STATE, 2, 'Data field skipped:'//STATE%ELTEXT)
      CALL LEESEL (STATE)
      CYCLE keyword_search
   ENDIF
   IF (STATE%ELTYPE.EQ.'REAL') THEN
      CALL reader_msgerr (STATE, 2, 'Data field skipped:'//STATE%ELTEXT)
      CALL LEESEL (STATE)
      CYCLE keyword_search
   ENDIF
   IF (STATE%ELTYPE.EQ.'CHAR' .OR. STATE%ELTYPE.EQ.'OTHR') THEN
      CALL reader_msgerr (STATE, 2, 'Data field skipped:'//STATE%ELTEXT)
      CALL LEESEL (STATE)
      CYCLE keyword_search
   ENDIF
   IF (STATE%ELTYPE.EQ.'EMPT') THEN
      CALL reader_msgerr (STATE, 2, 'Empty data field skipped')
      CALL LEESEL (STATE)
      CYCLE keyword_search
   ENDIF
   CALL reader_msgerr (STATE, 3, 'Error subr. INKEYW')
   EXIT keyword_search
!     ----------------------------------------------------------
   END DO keyword_search
   IF (ITEST.GE.10) WRITE (reader_print_unit(STATE),"(' KEYWORD: ',A8)") STATE%KEYWRD
   RETURN
end subroutine INKEYW_CTX
!****************************************************************
!                                                               *
SUBROUTINE INREAL_CTX (STATE, NAAM, R, KONT, RSTA)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     30.82: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     20.04, Aug. 93: logical CHGVAL is introduced it  is made True if
!                     user changes value of an input parameter via INREAL
!     30.82, Sep. 98: To avoid errors using the Cray-cf90 compiler
!                     introduced a dummy
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Reads a REAL number in free format.
!
!  3. METHOD
!
!     Uses the function INDBLE to read the number
!
!  4. ARGUMENT VARIABLES
!
!     R      : The value of the variable that is to be read.
!     RSTA   : Reference value needed for KONT='STA'or 'RQI'

   REAL, INTENT(INOUT) :: R
   REAL, INTENT(IN)    :: RSTA

!     KONT   : What to do with the variable?
!              ='REQ' : variable is required
!              ='UNC' : if no variable, then variable will not be changed
!              ='STA' : if no variable, then variable will get value of
!              ='RQI' : variable may not have the value of RSTA
!              ='REP' : (REPEAT)
!              ='NSKP': (NO SKIP) if data item is of different type,
!                       value is left unchanged
!     NAAM   : Name of the variable according to the user manual.

   CHARACTER(LEN=*), INTENT(IN) :: NAAM, KONT

!  5. PARAMETER VARIABLES
!
!     DRSTA  : REAL(KIND=KIND(0.0D0)) variant of RSTA
!     RDBL   : REAL(KIND=KIND(0.0D0)) variant of R

   REAL(KIND=KIND(0.0D0)) RDBL, DRSTA

!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE ( IENT, 'INREAL')

   RDBL = DBLE(R)
   DRSTA  = DBLE(RSTA)
   CALL INDBLE (STATE, NAAM, RDBL, KONT, DRSTA)

!     RDBL may have changed due to the value of KONT

   R = REAL(RDBL)
   RETURN

!     End of subroutine INREAL

end subroutine INREAL_CTX
!****************************************************************
!                                                               *
SUBROUTINE INDBLE_CTX (STATE, NAAM, R, KONT, RSTA)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, EQREAL
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     30.72, Oct. 97: Introduced logical function EQREAL for floating point
!                     comparisons
!     20.05, Aug. 93: NEW subroutine for double prec. data
!     40.03, Feb. 00: local copy of NAAM used in error message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Reads a REAL(KIND=KIND(0.0D0)) number, in free format.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     R      : THE VARIABLE THAT IS TO BE READ.
!     RSTA   : SEE ABOVE

   REAL(KIND=KIND(0.0D0)) R, RSTA

!     KONT   : What to do with the variable?
!              ='REQ'; Value in input file is required
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get value of RSTA
!              ='RQI'; Variable may not have the value of RSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.
!     NAAM   : name of the variable according to the user's manual.

   CHARACTER(LEN=*) :: KONT, NAAM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     LENNM  : length of string NAAM

   INTEGER, SAVE :: IENT = 0
   INTEGER   LENNM

!     NAAM_L : local copy of NAAM

   CHARACTER (LEN=40) :: NAAM_L

!     EQREAL :

      LOGICAL :: HAVE_CANDIDATE, KEEP_VALUE

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

   CALL STRACE ( IENT, 'INDBLE')

!     if necessary, a new data item is read

   STATE%CHGVAL = .FALSE.
   NAAM_L = NAAM
   IF (STATE%ELTYPE.EQ.'USED') CALL LEESEL (STATE)
   HAVE_CANDIDATE = .TRUE.
   KEEP_VALUE = .FALSE.
!     consider type of data item
   IF (STATE%ELTYPE.EQ.'KEY') THEN
!       find out whether in input is written: NAAM=...
      LENNM = LEN(NAAM)
      IF (NAAM.NE.STATE%ELTEXT(1:LENNM)) THEN
         HAVE_CANDIDATE = .FALSE.
      ELSE
         STATE%ELTYPE = 'USED'
         CALL LEESEL (STATE)
      END IF
   ENDIF

   IF (HAVE_CANDIDATE) THEN
      SELECT CASE (STATE%ELTYPE)
      CASE ('REAL')
         R = STATE%ELREAL
         STATE%ELTYPE = 'USED'
         KEEP_VALUE = .TRUE.
         IF (.NOT.EQREAL(REAL(R),REAL(RSTA))) STATE%CHGVAL = .TRUE.
      CASE ('INT')
         R = DBLE(STATE%ELINT)
         STATE%ELTYPE = 'USED'
         KEEP_VALUE = .TRUE.
         IF (.NOT.EQREAL(REAL(R),REAL(RSTA))) STATE%CHGVAL = .TRUE.
      CASE ('EOR')
         IF (KONT.EQ.'REP') STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('EOF')
         HAVE_CANDIDATE = .FALSE.
      CASE ('EMPT')
         STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('ERR')
         CALL reader_msgerr (STATE, 3, 'Read error with variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A, ' item=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         STATE%ELTYPE = 'USED'
      CASE ('CHAR', 'OTHR')
         IF (KONT.NE.'NSKP') THEN
            CALL reader_msgerr (STATE, 3, 'Wrong type of data for variable '//NAAM_L)
            WRITE (reader_print_unit(STATE),"(' -> ',A, ' item=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
            STATE%ELTYPE = 'USED'
         END IF
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INREAL')
         WRITE (reader_print_unit(STATE), '(1X,A,A)') STATE%ELTYPE, KONT
      END SELECT
   END IF


   IF (.NOT. KEEP_VALUE .AND. .NOT. HAVE_CANDIDATE) THEN
      SELECT CASE (KONT)
      CASE ('UNC')
         KEEP_VALUE = .TRUE.
      CASE ('RQI')
         IF (.NOT.EQREAL(REAL(R),REAL(RSTA))) THEN
            KEEP_VALUE = .TRUE.
         ELSE
            CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
            WRITE (reader_print_unit(STATE),"(' -> ',A, ' item=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         END IF
      CASE ('REQ')
         CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A, ' item=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
      CASE ('REP', 'STA', 'NSKP')
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INREAL')
         WRITE (reader_print_unit(STATE), '(1X,A,A)') STATE%ELTYPE, KONT
      END SELECT
   END IF

   IF (.NOT. KEEP_VALUE) R = RSTA
   IF (ITEST.GE.10) WRITE (reader_print_unit(STATE), "(1X,A8,'=',D12.4)") NAAM, R
   RETURN
end subroutine INDBLE_CTX
!****************************************************************
!                                                               *
SUBROUTINE ININTG_CTX (STATE, NAAM, IV, KONT, ISTA)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     20.04, August 1993:  logical CHGVAL is introduced
!                          it is made True if user changes value
!                          of an input parameter via ININTG
!     40.03, Feb. 00: local copy of NAAM used in error message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Reads an integer number, in free format
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IV     :  integer variable which is to be assigned a value
!     ISTA   :  default value

   INTEGER, INTENT(INOUT) :: IV
   INTEGER, INTENT(IN)    :: ISTA

!     NAAM   :  name of the variable according to the user manual
!     KONT   : What to do with the variable?
!              ='REQ'; error message if no value is found in the input file
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get default value
!              ='RQI'; Variable may not have the value of RSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.

   CHARACTER(LEN=*), INTENT(IN) :: NAAM, KONT

!  5. PARAMETER VARIABLES
!
!     PARAMETERS: SEE SUBR. INREAL
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     LENNM  : length of the string NAAM

   INTEGER, SAVE :: IENT = 0
   INTEGER   LENNM

!     NAAM_L : local copy of NAAM

   CHARACTER (LEN=40) :: NAAM_L
   LOGICAL :: HAVE_CANDIDATE, KEEP_VALUE

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

   CALL  STRACE ( IENT, 'ININTG')

   STATE%CHGVAL = .FALSE.
   NAAM_L = NAAM
!     IF NECESSARY, A NEW DATA ITEM IS READ
   IF (STATE%ELTYPE.EQ.'USED') CALL LEESEL (STATE)
   HAVE_CANDIDATE = .TRUE.
   KEEP_VALUE = .FALSE.

   IF (STATE%ELTYPE.EQ.'KEY') THEN
      LENNM = LEN(NAAM)
      IF (NAAM.NE.STATE%ELTEXT(1:LENNM)) THEN
         HAVE_CANDIDATE = .FALSE.
      ELSE
         CALL LEESEL (STATE)
      END IF
   END IF

   IF (HAVE_CANDIDATE) THEN
      SELECT CASE (STATE%ELTYPE)
      CASE ('INT')
         IV = STATE%ELINT
         IF (IV.NE.ISTA) STATE%CHGVAL = .TRUE.
         STATE%ELTYPE = 'USED'
         KEEP_VALUE = .TRUE.
      CASE ('EOR')
         IF (KONT.EQ.'REP') STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('EOF')
         HAVE_CANDIDATE = .FALSE.
      CASE ('EMPT')
         STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('CHAR', 'OTHR', 'REAL')
         IF (KONT.NE.'NSKP') THEN
            CALL reader_msgerr (STATE, 2, 'Wrong type of data for variable '//NAAM_L)
            WRITE (reader_print_unit(STATE),"(' -> ',A8, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
            STATE%ELTYPE = 'USED'
         END IF
      CASE ('ERR')
         CALL reader_msgerr (STATE, 2, 'Read error with variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A8, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         STATE%ELTYPE = 'USED'
      CASE DEFAULT
         CALL reader_msgerr (STATE, 2, 'Read error with variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A8, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         STATE%ELTYPE = 'USED'
      END SELECT
   END IF


   IF (.NOT. KEEP_VALUE .AND. .NOT. HAVE_CANDIDATE) THEN
      SELECT CASE (KONT)
      CASE ('UNC')
         KEEP_VALUE = .TRUE.
      CASE ('RQI')
         IF (IV.NE.ISTA) THEN
            KEEP_VALUE = .TRUE.
         ELSE
            CALL reader_msgerr (STATE, 2, 'No value for variable '//NAAM_L)
         END IF
      CASE ('REQ')
         CALL reader_msgerr (STATE, 2, 'No value for variable '//NAAM_L)
      CASE ('REP', 'STA', 'NSKP')
      CASE DEFAULT
         KEEP_VALUE = .TRUE.
      END SELECT
   END IF

   IF (.NOT. KEEP_VALUE) IV = ISTA
   IF (ITEST.GE.10) WRITE (reader_print_unit(STATE), "(1X,A8,'=',I6)") NAAM, IV
   RETURN
end subroutine ININTG_CTX
!****************************************************************
!                                                               *
SUBROUTINE INCSTR_CTX (STATE, NAAM, C, KONT, CSTA)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     20.04, August 1993:  logical CHGVAL is introduced
!                          it is made True if user changes value
!                          of an input parameter via INCSTR
!     40.03, Feb. 00: local copy of NAAM used in error message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Reads a string in free format
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     NAAM   : name of the variable according to the user manual
!     KONT   : What to do with the variable?
!              ='REQ'; error message if no value is found in the input file
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get default value
!              ='RQI'; Variable may not have the value of CSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.
!     C      : string that is to be read from input file
!     CSTA   : default value of the string

   CHARACTER(LEN=*), INTENT(IN)    :: NAAM, KONT, CSTA
   CHARACTER(LEN=*), INTENT(INOUT) :: C

!  5. PARAMETER VARIABLES
!
!     Parameters: see program documentation.
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     LENNM  : length of the string NAAM
!     LENW   : length of the string C
!     NS     : length of the string CSTA

   INTEGER, SAVE :: IENT = 0
   INTEGER   LENNM, LENW, NS

!     NAAM_L : local copy of NAAM

   CHARACTER (LEN=40) :: NAAM_L
   LOGICAL :: HAVE_CANDIDATE, KEEP_VALUE

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

   CALL  STRACE ( IENT, 'INCSTR')

   STATE%CHGVAL = .FALSE.
   NAAM_L = NAAM
   LENW = LEN(C)
!     IF NECESSARY, A NEW DATA ITEM IS READ.
   IF (STATE%ELTYPE.EQ.'USED') CALL LEESEL (STATE)
   HAVE_CANDIDATE = .TRUE.
   KEEP_VALUE = .FALSE.

   IF (STATE%ELTYPE.EQ.'KEY') THEN
!       FIND OUT WHETHER IN INPUT IS WRITTEN:  NAAM=....
      LENNM = LEN(NAAM)
      IF (NAAM.NE.STATE%ELTEXT(1:LENNM)) THEN
         HAVE_CANDIDATE = .FALSE.
      ELSE
         STATE%ELTYPE = 'USED'
         CALL LEESEL (STATE)
      END IF
   ENDIF

   IF (HAVE_CANDIDATE) THEN
      SELECT CASE (STATE%ELTYPE)
      CASE ('CHAR')
         IF (STATE%LENCST.GT.LENW) THEN
            CALL reader_msgerr (STATE, 2, 'too long string given for: '//NAAM_L)
            WRITE (reader_print_unit(STATE), "(' name=', A, ' string=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         ENDIF
         C = STATE%ELTEXT(1:LENW)
         IF (C.NE.CSTA) STATE%CHGVAL = .TRUE.
         STATE%ELTYPE = 'USED'
         KEEP_VALUE = .TRUE.
      CASE ('EOR')
         IF (KONT.EQ.'REP') STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('EOF')
         HAVE_CANDIDATE = .FALSE.
      CASE ('EMPT')
         STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('INT', 'REAL', 'OTHR')
         IF (KONT.NE.'NSKP') THEN
            CALL reader_msgerr (STATE, 3, 'Wrong type of data for variable '//NAAM_L)
            WRITE (reader_print_unit(STATE),"(' -> ',A8)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
            STATE%ELTYPE = 'USED'
         END IF
      CASE ('ERR')
         CALL reader_msgerr (STATE, 3, 'Read error with variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A8)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         STATE%ELTYPE = 'USED'
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INCSTR')
         WRITE (reader_print_unit(STATE), '(1X,A,1X,A)') STATE%ELTYPE, KONT
         KEEP_VALUE = .TRUE.
      END SELECT
   END IF


   IF (.NOT. KEEP_VALUE .AND. .NOT. HAVE_CANDIDATE) THEN
      SELECT CASE (KONT)
      CASE ('UNC')
         KEEP_VALUE = .TRUE.
      CASE ('RQI')
         IF (C(1:LENW).NE.CSTA(1:LENW)) THEN
            KEEP_VALUE = .TRUE.
         ELSE
            CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
         END IF
      CASE ('REQ')
         CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
      CASE ('REP', 'STA', 'NSKP')
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INCSTR')
         WRITE (reader_print_unit(STATE), '(1X,A,1X,A)') STATE%ELTYPE, KONT
      END SELECT
   END IF

   IF (.NOT. KEEP_VALUE) THEN
      NS = LEN(CSTA)
      IF (NS.LT.LENW) THEN
         C(1:NS) = CSTA(1:NS)
         C(NS+1:LENW) = ' '
         STATE%LENCST = NS
      ELSE
         C(1:LENW) = CSTA(1:LENW)
         STATE%LENCST = LENW
      ENDIF
   END IF
   IF (ITEST.GE.10) WRITE (reader_print_unit(STATE), "(1X, A, ' = ', A, 4X, 'length:', I3)") TRIM(NAAM), C, STATE%LENCST
   RETURN
end subroutine INCSTR_CTX
!****************************************************************
!                                                               *
SUBROUTINE INCTIM_CTX (STATE, IOPTIM, NAAM, RV, KONT, RSTA)
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, EQDBLE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     30.72, Oct. 97: Introduced logical function EQREAL for floating point
!                     comparisons
!     30.04, Mar. 95: New subroutine
!     40.03, Feb. 00: local copy of NAAM used in error message
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Reads and interprets a time string
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IOPTIM   int   inp   time reading option (see subr DTSTTI)


   INTEGER   IOPTIM

!     RV     : variable that is to be assigned a value
!     RSTA   : default value

   REAL(KIND=KIND(0.0D0))    RV, RSTA

!     NAAM   : name of the variable according to the user manual
!     KONT   : What to do with the variable?
!              ='REQ'; error message if no value is found in the input file
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get default value
!              ='RQI'; Variable may not have the value of RSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.

   CHARACTER(LEN=*) :: NAAM, KONT

!  5. PARAMETER VARIABLES
!
!     PARAMETERS: SEE PROGRAM DOCUMENTATION.
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     LENMN  : length of the string NAAM

   INTEGER, SAVE :: IENT = 0
   INTEGER    LENNM

!     NAAM_L : local copy of NAAM

   CHARACTER (LEN=40) :: NAAM_L

!     EQDBLE : logical function, True if arguments are equal

   LOGICAL    HAVE_CANDIDATE, KEEP_VALUE

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

   CALL STRACE ( IENT, 'INCTIM')

   STATE%CHGVAL = .FALSE.
   NAAM_L = NAAM
!     If necessary, a new data item is read.
   IF (STATE%ELTYPE.EQ.'USED') CALL LEESEL (STATE)
   HAVE_CANDIDATE = .TRUE.
   KEEP_VALUE = .FALSE.
!     Consider type of data item.
   IF (STATE%ELTYPE.EQ.'KEY') THEN
!       find out whether in input is written:  NAAM=....
      LENNM = LEN(NAAM)
      IF (NAAM.NE.STATE%ELTEXT(1:LENNM)) THEN
         HAVE_CANDIDATE = .FALSE.
      ELSE
         STATE%ELTYPE = 'USED'
         CALL LEESEL (STATE)
      END IF
   ENDIF

   IF (HAVE_CANDIDATE) THEN
      SELECT CASE (STATE%ELTYPE)
      CASE ('CHAR', 'OTHR', 'REAL', 'INT')
         CALL DTRETI (STATE%ELTEXT(1:STATE%LENCST), IOPTIM, RV)
         IF (.NOT.EQDBLE(RV,RSTA)) STATE%CHGVAL = .TRUE.
         STATE%ELTYPE = 'USED'
         KEEP_VALUE = .TRUE.
      CASE ('EOR')
         IF (KONT.EQ.'REP') STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('EOF')
         HAVE_CANDIDATE = .FALSE.
      CASE ('EMPT')
         STATE%ELTYPE = 'USED'
         HAVE_CANDIDATE = .FALSE.
      CASE ('ERR')
         CALL reader_msgerr (STATE, 3, 'Read error with variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         STATE%ELTYPE = 'USED'
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INCTIM')
         WRITE (reader_print_unit(STATE), '(1X,A,1X,A)') STATE%ELTYPE, KONT
         KEEP_VALUE = .TRUE.
      END SELECT
   END IF


   IF (.NOT. KEEP_VALUE .AND. .NOT. HAVE_CANDIDATE) THEN
      SELECT CASE (KONT)
      CASE ('UNC')
         KEEP_VALUE = .TRUE.
      CASE ('RQI')
         IF (.NOT.EQDBLE(RV,RSTA)) THEN
            KEEP_VALUE = .TRUE.
         ELSE
            CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
            WRITE (reader_print_unit(STATE),"(' -> ',A, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
         END IF
      CASE ('REQ')
         CALL reader_msgerr (STATE, 3, 'No value for variable '//NAAM_L)
         WRITE (reader_print_unit(STATE),"(' -> ',A, ' item read=', A)") NAAM, STATE%ELTEXT(1:STATE%LENCST)
      CASE ('REP', 'STA', 'NSKP')
      CASE DEFAULT
         CALL reader_msgerr (STATE, 3, 'Error subr. INCTIM')
         WRITE (reader_print_unit(STATE), '(1X,A,1X,A)') STATE%ELTYPE, KONT
      END SELECT
   END IF

   IF (.NOT. KEEP_VALUE) RV = RSTA
   IF (ITEST.GE.10) WRITE (reader_print_unit(STATE), "(1X, A, ' = ', A, 4X, 't in sec:', F10.0)") NAAM, STATE%ELTEXT(1:STATE%LENCST), RV
   RETURN
end subroutine INCTIM_CTX
!*******************************************************************
!                                                                  *
SUBROUTINE ININTV_CTX (STATE, NAME, RVAR, KONT, RSTA)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                  *
!*******************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     Dec 1995, ver 30.09 : new subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Read a time interval in the form: number  DAY/HR/MIN/SEC
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     NAAM   : name of the variable according to the user manual
!     KONT   : What to do with the variable?
!              ='REQ'; error message if no value is found in the input file
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get default value
!              ='RQI'; Variable may not have the value of RSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.

   CHARACTER(LEN=*) :: NAME, KONT

!     RSTA   : default value
!     RVAR   : variable that is to be assigned a value

   REAL      RSTA, RVAR

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

!     FAC    : a factor, value depends on unit of time used
!     RI     : auxiliary variable

   REAL      FAC, RI

!     KEYWIS : logical function, True if keyword encountered is equal to
!              keyword in user manual


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
!     -------------------------------------------------------------
!     Call INREAL to read number of time units
!     If a value was read
!     Then Read time unit
!          Case time unit is
!          DAY: Fac = 24*3600
!          HR:  Fac = 3600
!          MI:  Fac = 60
!          SEC: Fac = 1
!     Else Fac = 1
!     -------------------------------------------------------------
!     Interval in seconds = Fac * number of time units
!     -------------------------------------------------------------
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'ININTV')

   CALL INREAL (STATE, NAME, RI, KONT, RSTA)
   IF (STATE%CHGVAL) THEN
      CALL INKEYW (STATE, 'STA', 'S')
      IF (KEYWIS (STATE, 'DA')) THEN
         FAC = 24.*3600.
      ELSE IF (KEYWIS (STATE, 'HR')) THEN
         FAC = 3600.
      ELSE IF (KEYWIS (STATE, 'MI')) THEN
         FAC = 60.
      ELSE
         CALL IGNORE (STATE, 'S')
         FAC = 1.
      ENDIF
   ELSE
      FAC = 1.
   ENDIF
   RVAR = FAC * RI
   RETURN
!     end of subroutine ININTV
end subroutine ININTV_CTX
!*******************************************************************
!                                                                  *
SUBROUTINE INITVD_CTX (STATE, NAME, RVAR, KONT, RSTA)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                  *
!*******************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     July 2015: copied from ININTV, adapted to REAL(KIND=KIND(0.0D0))
!
!  2. PURPOSE
!
!     Read a time interval in the form: number  DAY/HR/MIN/SEC
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     NAAM   : name of the variable according to the user manual
!     KONT   : What to do with the variable?
!              ='REQ'; error message if no value is found in the input file
!              ='UNC'; If no value, then variable will not be changed
!              ='STA'; If no value, then variable will get default value
!              ='RQI'; Variable may not have the value of RSTA
!              ='REP'  (repeat)
!              ='NSKP' (no skip) if data item is of different type,
!                      value is left unchanged.

   CHARACTER(LEN=*) :: NAME, KONT

!     RSTA   : default value
!     RVAR   : variable that is to be assigned a value

   REAL(KIND=KIND(0.0D0))    RSTA, RVAR

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

!     FAC    : a factor, value depends on unit of time used
!     RI     : auxiliary variable

   REAL(KIND=KIND(0.0D0))    FAC, RI

!     KEYWIS : logical function, True if keyword encountered is equal to
!              keyword in user manual


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
!     -------------------------------------------------------------
!     Call INREAL to read number of time units
!     If a value was read
!     Then Read time unit
!          Case time unit is
!          DAY: Fac = 24*3600
!          HR:  Fac = 3600
!          MI:  Fac = 60
!          SEC: Fac = 1
!     Else Fac = 1
!     -------------------------------------------------------------
!     Interval in seconds = Fac * number of time units
!     -------------------------------------------------------------
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'INITVD')

   CALL INDBLE (STATE, NAME, RI, KONT, RSTA)
   IF (STATE%CHGVAL) THEN
      CALL INKEYW (STATE, 'STA', 'S')
      IF (KEYWIS (STATE, 'DA')) THEN
         FAC = 24.*3600.
      ELSE IF (KEYWIS (STATE, 'HR')) THEN
         FAC = 3600.
      ELSE IF (KEYWIS (STATE, 'MI')) THEN
         FAC = 60.
      ELSE
         CALL IGNORE (STATE, 'S')
         FAC = 1.
      ENDIF
   ELSE
      FAC = 1.
   ENDIF
   RVAR = FAC * RI
   RETURN
!     end of subroutine INITVD
end subroutine INITVD_CTX
!****************************************************************
!                                                               *
SUBROUTINE LEESEL_CTX (STATE)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     Jan. 1994, mod. 20.05: ELREAL is made REAL(KIND=KIND(0.0D0))
!     40.13, Jan. 01: ! is now added as comment sign
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     reads a new data item from the string 'KAART'.
!     type of the item is determined, and the contents appears
!     in ELTEXT, ELINT, or ELREAL, as the case may be.
!     the following types are distinguished:
!     'KEY'   keyword
!     'INT'   integer or real number
!     'REAL'  real number
!     'CHAR'  character string enclosed in quotes
!     'EMPT'  empty data field
!     'OTHR'  non-empty data item not recognized as real, int or char,
!             possibly a time string
!     'EOF'   end of input file
!
!     'EOR'   end of repeat, or end of record
!     'ERR'   error
!     'USED'  used, item last read is processed already.
!
!  3. METHOD
!
!     difference between comment signs $ and !:
!     everything on an input line behind a ! is ignored
!     text between two $-signs (on one line) is intepreted as comment
!     text behind two $-signs is intepreted as valid input
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     IRK    : auxiliary value used to detect errors
!     ISIGN1 : sign of mantissa part
!     ISIGN2 : sign of exponent part
!     ISTATE : state of the number reading process
!     J      : counter
!     JJ     : counter
!     JKAR   : counts the number of characters in the data field
!     NREP   : repetition number
!     NUM1   : value of integer part of mantissa
!     NUM2   : exponent value

   INTEGER, SAVE :: IENT = 0
   INTEGER, SAVE :: NREP = 1
   INTEGER   IRK, ISIGN1, ISIGN2, ISTATE, J, JJ, JKAR, NUM1, NUM2
   LOGICAL :: PARSE_AS_OTHER

!     RMANT  : real mantissa value

   REAL(KIND=KIND(0.0D0)) RMANT

!     QUOTE  : the quote character

   CHARACTER(LEN=1), PARAMETER :: QUOTE = "'"

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

   CALL  STRACE ( IENT, 'LEESEL')

   parse_item: BLOCK
   IF (NREP.GT.1) THEN
      NREP = NREP - 1
      EXIT parse_item
   ENDIF

   NREP = 1
   do J=1,LINELN,4
      STATE%ELTEXT(J:J+3) = '    '
   end do
   JKAR = 1
   STATE%ELINT = 0
   STATE%ELREAL = 0.

   item_start: DO
      IF (STATE%KARNR.EQ.0) CALL GETKAR (STATE)

      DO WHILE ((STATE%KAR.EQ.' ' .OR. STATE%KAR.EQ.STATE%TABC) .AND. STATE%KARNR.LE.LINELN)
         CALL GETKAR (STATE)
         IF (STATE%ELTYPE.EQ.'EOF') EXIT
      END DO

      IF (STATE%ELTYPE.EQ.'EOF') THEN
         STATE%ELTEXT = 'STOP'
         EXIT parse_item
      ENDIF

      IF (STATE%KAR.EQ.'!' .OR. STATE%KARNR.GT.LINELN) THEN
         IF (NREP.GT.1) THEN
            STATE%ELTYPE = 'EMPT'
         ELSE
            STATE%ELTYPE = 'EOR'
            IF (STATE%KAR.EQ.'!') STATE%KARNR = LINELN+1
         ENDIF
         EXIT parse_item
      ENDIF

      IF (STATE%KAR.EQ.',') THEN
         CALL GETKAR (STATE)
         STATE%ELTYPE = 'EMPT'
         EXIT parse_item
      ENDIF

      IF (INDEX(';/',STATE%KAR).GT.0) THEN
         IF (NREP.GT.1) THEN
            STATE%ELTYPE = 'EMPT'
         ELSE
            STATE%ELTYPE = 'EOR'
            CALL GETKAR (STATE)
         ENDIF
         EXIT parse_item
      ENDIF

      IF (STATE%KAR.EQ.'(') THEN
         CALL GETKAR (STATE)
         CYCLE item_start
      ENDIF

      IF (STATE%KAR.EQ.STATE%COMID) THEN
         IF (NREP.GT.1) THEN
            STATE%ELTYPE = 'EMPT'
            EXIT parse_item
         ENDIF
         DO
            CALL GETKAR (STATE)
            IF (STATE%KARNR.GT.LINELN .OR. STATE%KAR.EQ.STATE%COMID) EXIT
         END DO
         IF (STATE%KARNR.LE.LINELN) CALL GETKAR (STATE)
         CYCLE item_start
      ENDIF

      PARSE_AS_OTHER = .FALSE.
      IF (INDEX('+-.0123456789',STATE%KAR).GT.0) THEN
         NUM1 = 0
         NUM2 = 0
         ISIGN1 = 1
         ISIGN2 = 1
         ISTATE = 10
         IRK = 0
         RMANT = 0.
         STATE%ELTYPE = 'INT'

         IF (INDEX('+-',STATE%KAR).GT.0) THEN
            ISTATE = 9
            IF (STATE%KAR.EQ.'-') ISIGN1 = -1
            CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
            CALL GETKAR (STATE)
         ENDIF

         DO WHILE (INDEX('0123456789',STATE%KAR).GT.0)
            IRK = 1
            ISTATE = 8
            NUM1 = 10*NUM1 + INDEX('123456789',STATE%KAR)
            CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
            CALL GETKAR (STATE)
         END DO

         IF (STATE%KAR.EQ.'.') THEN
            ISTATE = 7
            STATE%ELTYPE = 'REAL'
            CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
            CALL GETKAR (STATE)
         ENDIF

         JJ = -1
         DO WHILE (INDEX('0123456789',STATE%KAR).GT.0)
            IRK = 1
            ISTATE = 6
            RMANT = RMANT + DBLE(INDEX('123456789',STATE%KAR))*1.D1**JJ
            JJ = JJ-1
            CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
            CALL GETKAR (STATE)
         END DO

         IF (ISTATE.GE.9 .OR. IRK.EQ.0) PARSE_AS_OTHER = .TRUE.

         IF (.NOT.PARSE_AS_OTHER .AND. INDEX('DdEe^',STATE%KAR).GT.0) THEN
            ISTATE = 5
            IRK = 0
            IF (STATE%ELTYPE.EQ.'INT') STATE%ELTYPE = 'REAL'
            CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
            CALL GETKAR (STATE)
            IF (INDEX('+-',STATE%KAR).GT.0) THEN
               IF (STATE%KAR.EQ.'-') ISIGN2 = -1
               ISTATE = 4
               CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
               CALL GETKAR (STATE)
            ENDIF
            DO WHILE (INDEX('0123456789',STATE%KAR).GT.0)
               IRK = 1
               ISTATE = 3
               NUM2 = 10*NUM2 + INDEX('123456789',STATE%KAR)
               CALL PUTKAR (STATE, STATE%ELTEXT, STATE%KAR, JKAR)
               CALL GETKAR (STATE)
            END DO
            IF (IRK.EQ.0) PARSE_AS_OTHER = .TRUE.
         ENDIF

         IF (INDEX('+-.',STATE%KAR).GE.1) PARSE_AS_OTHER = .TRUE.

         IF (.NOT.PARSE_AS_OTHER) THEN
            ISTATE = 2
            IF (ITEST.GE.330) WRITE (reader_print_unit(STATE),"(1X, A4, 2I6, F12.9, 2I6)") STATE%ELTYPE, ISIGN1, NUM1,&
            &RMANT, ISIGN2, NUM2
            IF (STATE%ELTYPE.EQ.'REAL') STATE%ELREAL = &
            &ISIGN1*(DBLE(NUM1)+RMANT) * 1.D1**(ISIGN2*NUM2)
            IF (STATE%ELTYPE.EQ.'INT') STATE%ELINT = ISIGN1*NUM1
            STATE%LENCST = JKAR - 1
            DO WHILE (STATE%KAR.EQ.' ' .OR. STATE%KAR.EQ.STATE%TABC)
               ISTATE = 1
               CALL GETKAR (STATE)
            END DO

            IF (STATE%KAR.EQ.'*') THEN
               IF (STATE%ELTYPE.EQ.'INT' .AND. STATE%ELINT.GT.0) THEN
                  NREP = STATE%ELINT
                  STATE%ELINT = 0
                  CALL GETKAR (STATE)
                  CYCLE item_start
               ELSE
                  CALL reader_msgerr (STATE, 2, 'Wrong repetition factor')
                  CALL GETKAR (STATE)
                  EXIT parse_item
               ENDIF
            ENDIF
            IF (STATE%KAR.EQ.',') THEN
               CALL GETKAR (STATE)
               EXIT parse_item
            ENDIF
            IF (ISTATE.EQ.1 .OR. INDEX(' ;',STATE%KAR).NE.0 .OR. &
                STATE%KAR.EQ.STATE%TABC) EXIT parse_item
            PARSE_AS_OTHER = .TRUE.
         ENDIF

      ELSE IF (STATE%KAR.EQ.QUOTE) THEN
         STATE%ELTYPE = 'CHAR'
         STATE%LENCST = 0
         JJ = 1
         DO
            CALL GETKAR (STATE)
            IF (STATE%KARNR.GT.LINELN) EXIT parse_item
            IF (STATE%KAR.EQ.QUOTE) THEN
               CALL GETKAR (STATE)
               IF (STATE%KAR.NE.QUOTE) EXIT
            ENDIF
            STATE%ELTEXT(JJ:JJ) = STATE%KAR
            STATE%LENCST = JJ
            JJ = JJ+1
         END DO
         DO WHILE (STATE%KAR.EQ.' ' .OR. STATE%KAR.EQ.STATE%TABC)
            CALL GETKAR (STATE)
         END DO
         IF (STATE%KAR.EQ.',') CALL GETKAR (STATE)
         EXIT parse_item

      ELSE
         CALL UPCASE (STATE%KAR)
         IF (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STATE%KAR).GT.0) THEN
            IF (NREP.GT.1) THEN
               STATE%ELTYPE = 'EMPT'
               EXIT parse_item
            ENDIF
            STATE%ELTYPE = 'KEY'
            ISTATE = 2
            JJ = 1
            DO
               STATE%ELTEXT(JJ:JJ) = STATE%KAR
               STATE%LENCST = JJ
               CALL GETKAR (STATE)
               CALL UPCASE (STATE%KAR)
               JJ = JJ+1
               IF (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.',STATE%KAR)&
                   .EQ.0) EXIT
            END DO
            STATE%KEYWRD = STATE%ELTEXT(1:8)
            DO WHILE (STATE%KAR.EQ.' ' .OR. STATE%KAR.EQ.STATE%TABC)
               CALL GETKAR (STATE)
            END DO
            IF (INDEX('=:',STATE%KAR).GT.0) CALL GETKAR (STATE)
            EXIT parse_item
         ENDIF

         IF (INDEX('_&',STATE%KAR).GT.0) THEN
            IF (NREP.GT.1) THEN
               STATE%ELTYPE = 'EMPT'
               EXIT parse_item
            ENDIF
            STATE%KARNR = 0
            CYCLE item_start
         ENDIF
         PARSE_AS_OTHER = .TRUE.
      ENDIF

      IF (PARSE_AS_OTHER) THEN
         STATE%ELTYPE = 'OTHR'
         DO
            STATE%ELTEXT(JKAR:JKAR) = STATE%KAR
            STATE%LENCST = JKAR
            JKAR = JKAR+1
            CALL GETKAR (STATE)
            IF (INDEX(' ,;', STATE%KAR).GE.1 .OR. STATE%KAR.EQ.STATE%TABC) EXIT
         END DO
         CALL GETKAR (STATE)
         EXIT parse_item
      ENDIF
   END DO item_start
   END BLOCK parse_item

   IF (ITEST.GE.120) WRITE (PRTEST, "(' test LEESEL: ', A1, 1X, I4, 1X, A4, D12.4, 2I6, 2X, A)") STATE%KAR, STATE%KARNR, STATE%ELTYPE, STATE%ELREAL,&
   &STATE%ELINT, NREP, STATE%ELTEXT(1:STATE%LENCST)
end subroutine LEESEL_CTX
!****************************************************************
!                                                               *
SUBROUTINE GETKAR_CTX (STATE)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.13, Jan. 2001: TRIM used to limit output
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     This procedure reads a next character (KAR) from the string KAART.
!     The position of this character in KAART is indicated by KARNR.
!     If needed, a new input line is read.
!     At the end of the input file ELTYPE is made 'EOF'.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0
   INTEGER   IO_STATUS
   INTEGER   IUNIT

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

   CALL STRACE (IENT, 'GETKAR')
   IF (STATE%KARNR.EQ.0) THEN
      IUNIT = INPUTF
      IF (STATE%io_bound) IUNIT = STATE%io%INPUTF
      READ (IUNIT, "(A)", IOSTAT=IO_STATUS) STATE%KAART
      IF (IO_STATUS /= 0) THEN
         STATE%ELTYPE = 'EOF'
         STATE%KAR = '@'
         IF (ITEST.GE.320) WRITE (reader_print_unit(STATE), "(' Test GETKAR', 2X, A4, 2X, A1, I4)") STATE%ELTYPE, STATE%KAR, STATE%KARNR
         RETURN
      END IF
      IF (ITEST.GE.-10) WRITE (reader_print_unit(STATE), "(1X,A)") TRIM(STATE%KAART)
      STATE%KARNR=1
   ENDIF
   IF (STATE%KARNR.GT.LINELN) THEN
      STATE%KAR=';'
   ELSE
      STATE%KAR = STATE%KAART(STATE%KARNR:STATE%KARNR)
      STATE%KARNR=STATE%KARNR+1
   ENDIF
   IF (ITEST.GE.320) WRITE (reader_print_unit(STATE), "(' Test GETKAR', 2X, A4, 2X, A1, I4)") STATE%ELTYPE, STATE%KAR, STATE%KARNR
   RETURN
!     end of subroutine GETKAR
end subroutine GETKAR_CTX
!****************************************************************
!                                                               *
SUBROUTINE PUTKAR_CTX (STATE, LTEXT, KARR, JKAR)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(command_reader_t), INTENT(INOUT) :: STATE

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
!     this procedure inserts a character (KARR) usually read by GETKAR
!     into the string LTEXT, usually equal to ELTEXT, in the place
!     JKAR. After this JKAR is increased by 1.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     JKAR   : counts the number of characters in a data field

   INTEGER  JKAR

!     LTEXT  : a character string; after a number of calls it should
!              contain the character representation of a data field
!     KARR   : character to be inserted into LTEXT

   CHARACTER(LEN=*) :: LTEXT
   CHARACTER(LEN=1) :: KARR

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE (IENT, 'PUTKAR')
   IF (JKAR.GT.LEN(LTEXT)) CALL reader_msgerr (STATE, 2, 'PUTKAR, string too long')
   LTEXT(JKAR:JKAR) = KARR
   STATE%LENCST = JKAR
   JKAR = JKAR + 1
   RETURN
!     end of subroutine PUTKAR
end subroutine PUTKAR_CTX
!****************************************************************
!                                                               *
!****************************************************************
!                                                               *
LOGICAL FUNCTION EQCSTR (STR1, STR2)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
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
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     EQCSTR is assigned the value true if the two strings are the same
!     (case-insensitive)
!
!  3. METHOD
!
!     both strings are converted to upper case and then compared
!     the length of string 2 gives the number of significant characters
!
!  4. ARGUMENT VARIABLES

   CHARACTER (LEN=*) :: STR1, STR2
!     two character strings to be compared
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES

   INTEGER, SAVE  :: IENT = 0
!     IENT   : Number of entries into this subroutine

   INTEGER :: IC, LLCC
!     IC     : sequence number of a character in the string
!     LLCC   : length of the given character string

   CHARACTER (LEN=1) :: CC1, CC2
!     a character, one from STR1 the other from STR2
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

   CALL STRACE (IENT, 'UPCASE')

   EQCSTR = .TRUE.
   LLCC = LEN (STR2)
   IF (LEN(STR1).LT.LLCC) THEN
      EQCSTR = .FALSE.
      RETURN
   ENDIF
   DO IC = 1, LLCC
      CC1 = STR1(IC:IC)
      CALL UPCASE (CC1)
      CC2 = STR2(IC:IC)
      CALL UPCASE (CC2)
      IF (CC1.NE.CC2) THEN
         EQCSTR = .FALSE.
         RETURN
      ENDIF
   ENDDO
end function EQCSTR
!****************************************************************
!                                                               *
LOGICAL FUNCTION KEYWIS_CTX (STATE, STRING)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     40.00, July
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     This procedure tests whether a keyword given by the user
!     coincides with a keyword known in the program (i.e. string).
!     if so, keywis is made .True., otherwise it is .False.
!     also ELTYPE is made 'USED', so that next element can be read.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     STRING : a keyword which is compared with a keyword found in the input file

   CHARACTER(LEN=*), INTENT(IN) :: STRING

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine
!     J      : counter
!     LENSS  : length of the keyword STRING

   INTEGER, SAVE :: IENT = 0
   INTEGER   J, LENSS

!     KAR1   : a character of the keyword appearing in the input file
!     KAR2   : corresponding character in the STRING

   CHARACTER(LEN=1) :: KAR1, KAR2

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

   CALL STRACE (IENT, 'KEYWIS')

   KEYWIS_CTX = .FALSE.
   IF (STATE%ELTYPE.EQ.'USED') RETURN

   KEYWIS_CTX=.TRUE.
   LENSS = LEN (STRING)
   do J=1, LENSS
      KAR1 = STATE%KEYWRD(J:J)
      KAR2 = STRING(J:J)
      IF (KAR1.NE.KAR2 .AND. KAR2.NE.' ') THEN
         KEYWIS_CTX=.FALSE.
         RETURN
      ENDIF
   end do
   IF (STATE%ELTYPE.EQ.'KEY') STATE%ELTYPE = 'USED'
end function KEYWIS_CTX
!****************************************************************
!                                                               *
SUBROUTINE WRNKEY_CTX (STATE)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     THIS PROCEDURE PRODUCES AN ERROR MESSAGE
!     IT IS CALLED IF AN ILLEGAL KEYWORD IS FOUND IN THE
!     USER'S INPUT. IT MAKES ELTYPE = 'USED'
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

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

   CALL STRACE (IENT, 'WRNKEY')

   CALL reader_msgerr (STATE, 2, 'Illegal keyword: '//STATE%KEYWRD)
   STATE%ELTYPE = 'USED'
   RETURN
end subroutine WRNKEY_CTX
!****************************************************************
!                                                               *
SUBROUTINE IGNORE_CTX (STATE, STRING)
   USE swan_service_interfaces, ONLY: STRACE
!                                                               *
!****************************************************************
   USE OCPCOMM4

   IMPLICIT NONE(TYPE, EXTERNAL)



   TYPE(command_reader_t), INTENT(INOUT) :: STATE
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
!     This procedure calls subroutine INKEYW to read a keyword.
!     if this keyword is equal to string, eltype is made 'USED'.
!     it is used if a keyword can occur in the input which
!     does not lead to any action.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     STRING : keyword (if appearing in input file) that can be ignored

   CHARACTER(LEN=*), INTENT(IN) :: STRING

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

!     KEYWIS : logical function


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

   CALL STRACE (IENT, 'IGNORE')

   CALL INKEYW (STATE, 'STA', 'XXXX')
   IF (KEYWIS (STATE, STRING)) RETURN
   IF (KEYWIS (STATE, 'XXXX')) RETURN
   IF (ITEST.GE.60) WRITE (reader_print_unit(STATE), "(' NOT IGNORED: ', A, 2X, A)") STATE%KEYWRD, STATE%ELTYPE
   RETURN
end subroutine IGNORE_CTX
! Legacy entry points retain the original signatures and use the singleton
! parser state. Passing a command_reader_t as first argument selects the
! context-aware implementation through the generic interfaces above.
subroutine rdinit_default
   call rdinit_ctx(default_command_reader)
end subroutine rdinit_default

subroutine nwline_default
   call nwline_ctx(default_command_reader)
end subroutine nwline_default

subroutine inkeyw_default(kont, csta)
   character(len=*), intent(in) :: kont, csta
   call inkeyw_ctx(default_command_reader, kont, csta)
end subroutine inkeyw_default

subroutine inreal_default(naam, r, kont, rsta)
   character(len=*), intent(in) :: naam, kont
   real, intent(inout) :: r
   real, intent(in) :: rsta
   call inreal_ctx(default_command_reader, naam, r, kont, rsta)
end subroutine inreal_default

subroutine indble_default(naam, r, kont, rsta)
   character(len=*), intent(in) :: naam, kont
   real(swan_double), intent(inout) :: r
   real(swan_double), intent(in) :: rsta
   call indble_ctx(default_command_reader, naam, r, kont, rsta)
end subroutine indble_default

subroutine inintg_default(naam, iv, kont, ista)
   character(len=*), intent(in) :: naam, kont
   integer, intent(inout) :: iv
   integer, intent(in) :: ista
   call inintg_ctx(default_command_reader, naam, iv, kont, ista)
end subroutine inintg_default

subroutine incstr_default(naam, c, kont, csta)
   character(len=*), intent(in) :: naam, kont, csta
   character(len=*), intent(inout) :: c
   call incstr_ctx(default_command_reader, naam, c, kont, csta)
end subroutine incstr_default

subroutine inctim_default(ioptim, naam, rv, kont, rsta)
   integer, intent(in) :: ioptim
   character(len=*), intent(in) :: naam, kont
   real(swan_double), intent(inout) :: rv
   real(swan_double), intent(in) :: rsta
   call inctim_ctx(default_command_reader, ioptim, naam, rv, kont, rsta)
end subroutine inctim_default

subroutine inintv_default(name, rvar, kont, rsta)
   character(len=*), intent(in) :: name, kont
   real, intent(inout) :: rvar
   real, intent(in) :: rsta
   call inintv_ctx(default_command_reader, name, rvar, kont, rsta)
end subroutine inintv_default

subroutine initvd_default(name, rvar, kont, rsta)
   character(len=*), intent(in) :: name, kont
   real(swan_double), intent(inout) :: rvar
   real(swan_double), intent(in) :: rsta
   call initvd_ctx(default_command_reader, name, rvar, kont, rsta)
end subroutine initvd_default

subroutine leesel_default
   call leesel_ctx(default_command_reader)
end subroutine leesel_default

subroutine getkar_default
   call getkar_ctx(default_command_reader)
end subroutine getkar_default

subroutine putkar_default(ltext, karr, jkar)
   character(len=*), intent(inout) :: ltext
   character(len=1), intent(in) :: karr
   integer, intent(inout) :: jkar
   call putkar_ctx(default_command_reader, ltext, karr, jkar)
end subroutine putkar_default

logical function keywis_default(string)
   character(len=*), intent(in) :: string
   keywis_default = keywis_ctx(default_command_reader, string)
end function keywis_default

subroutine wrnkey_default
   call wrnkey_ctx(default_command_reader)
end subroutine wrnkey_default

subroutine ignore_default(string)
   character(len=*), intent(in) :: string
   call ignore_ctx(default_command_reader, string)
end subroutine ignore_default

end module swan_input_parser
