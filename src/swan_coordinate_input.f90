MODULE swan_coordinate_input
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: READXY, REFIXY

CONTAINS

SUBROUTINE READXY (NAMX, NAMY, XX, YY, KONT, XSTA, YSTA)
   USE swan_input_parser, ONLY: INDBLE
   USE swan_service_interfaces, ONLY: STRACE, EQREAL
!                                                                      *
!************************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE swan_coordinate_offset
   USE swan_computational_grid_kind


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
!     40.22: John Cazes and Tim Campbell
!     40.13: Nico Booij
!     40.51: Marcel Zijlema
!
!  1. UPDATE
!
!       Nov. 1996               offset values are added to standard values
!                               because they will be subtracted later
!     40.13, Nov. 01: a valid value for YY is required if a valid value
!                     for XX has been given; parser state reactivated
!     40.51, Feb. 05: correction to location points equal to offset values
!
!  2. PURPOSE
!
!       Read x and y, initialize offset values XOFFS and YOFFS
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       NAMX, NAMY   inp char    names of the two coordinates as given in
!                                the user manual
!       XX, YY       out real    values of x and y taking into account offset
!       KONT         inp char    what to be done if values are missing
!                                see doc. of INDBLE (Ocean Pack doc.)
!       XSTA, YSTA   inp real    standard values of x and y
!
!  5. SUBROUTINES CALLING
!
!
!
!  6. SUBROUTINES USED
!
!       INDBLE (Ocean Pack)

!  7. ERROR MESSAGES
!
!       ---
!
!  8. REMARKS
!
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       Read x and y in double prec.
!       If this is first couple of values
!       Then assign values to XOFFS and YOFFS
!            make LXOFFS True
!       ---------------------------------------------------------------
!       make XX and YY equal to x and y taking into account offset
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   REAL(KIND=KIND(0.0D0)) XTMP, YTMP
   INTEGER, SAVE :: IENT = 0
   REAL, INTENT(OUT) :: XX, YY
   REAL, INTENT(IN) :: XSTA, YSTA
   CHARACTER(LEN=*), INTENT(IN) :: NAMX, NAMY, KONT
   CALL  STRACE (IENT,'READXY')

   CALL INDBLE (NAMX, XTMP, KONT, DBLE(XSTA)+DBLE(XOFFS))
   IF (default_command_reader%CHGVAL) THEN
!       a valid value was given for XX
      CALL INDBLE (NAMY, YTMP, 'REQ', DBLE(YSTA)+DBLE(YOFFS))
   ELSE
      CALL INDBLE (NAMY, YTMP, KONT, DBLE(YSTA)+DBLE(YOFFS))
   ENDIF
   IF (.NOT.LXOFFS) THEN
      XOFFS = REAL(XTMP)
      YOFFS = REAL(YTMP)
      LXOFFS = .TRUE.
   ENDIF
   IF (.NOT.EQREAL(XOFFS,REAL(XTMP))) THEN
      XX = REAL(XTMP-DBLE(XOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      XX = 1.E-5
   ELSE
      XX = 0.
   END IF
   IF (.NOT.EQREAL(YOFFS,REAL(YTMP))) THEN
      YY = REAL(YTMP-DBLE(YOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      YY = 1.E-5
   ELSE
      YY = 0.
   END IF

   RETURN
! * end of subroutine READXY  *
end subroutine READXY

SUBROUTINE REFIXY (NDS, XX, YY, IERR)
   USE swan_service_interfaces, ONLY: STRACE, EQREAL
!                                                                      *
!************************************************************************

   USE swan_coordinate_offset
   USE swan_computational_grid_kind


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
!     40.22: John Cazes and Tim Campbell
!     40.51: M. Zijlema
!
!  1. UPDATE
!
!       first version: 10.18 (Sept 1994)
!
!  2. PURPOSE
!
!       initialize offset values XOFFS and YOFFS, and shift XX and YY
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       NDS          in  int     file reference number
!       XX, YY       out real    values of x and y taking into account offset
!       IERR         out int     error indicator: IERR=0: no error, =-1: end-
!                                of-file, =-2: read error
!
!  5. SUBROUTINES CALLING
!
!
!
!  6. SUBROUTINES USED


!  7. ERROR MESSAGES
!
!       ---
!
!  8. REMARKS
!
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       If this is first couple of values
!       Then assign values to XOFFS and YOFFS
!            make LXOFFS True
!       ---------------------------------------------------------------
!       make XX and YY equal to x and y taking into account offset
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   REAL(KIND=KIND(0.0D0)) XTMP, YTMP
   REAL, INTENT(OUT) :: XX, YY
   INTEGER, SAVE :: IENT = 0
   INTEGER, INTENT(OUT) :: IERR
   INTEGER, INTENT(IN) :: NDS
   CALL  STRACE (IENT,'REFIXY')

   READ (NDS, *, IOSTAT=IERR) XTMP, YTMP
   IF (IERR.NE.0) THEN
      IF (IS_IOSTAT_END(IERR)) THEN
         IERR = -1
      ELSE
         IERR = -2
      ENDIF
      RETURN
   ENDIF
   IF (.NOT.LXOFFS) THEN
      XOFFS = REAL(XTMP)
      YOFFS = REAL(YTMP)
      LXOFFS = .TRUE.
   ENDIF
   IF (.NOT.EQREAL(XOFFS,REAL(XTMP))) THEN
      XX = REAL(XTMP-DBLE(XOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      XX = 1.E-5
   ELSE
      XX = 0.
   END IF
   IF (.NOT.EQREAL(YOFFS,REAL(YTMP))) THEN
      YY = REAL(YTMP-DBLE(YOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      YY = 1.E-5
   ELSE
      YY = 0.
   END IF

   IERR = 0
   RETURN
! * end of subroutine REFIXY  *
end subroutine REFIXY

END MODULE swan_coordinate_input
