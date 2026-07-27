MODULE swan_number_formatting
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: INTSTR, NUMSTR
   PUBLIC :: INAN, RNAN

!     The sentinels a caller passes to NUMSTR to say "this one is not the value
!     I mean". They were in OCPCOMM4, so a caller had to import the unit
!     numbers and the error severity to name an argument of the function it was
!     already calling.
!
!     INAN : integer standing for "not a number"
!     RNAN : real standing for "not a number"
   INTEGER, PARAMETER :: INAN = -1073750760
   REAL, PARAMETER    :: RNAN = -1.07374515E+09

CONTAINS

CHARACTER(LEN=20) FUNCTION INTSTR ( IVAL )

!****************************************************************

   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
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
!     40.23: Marcel Zijlema
!
!  1. Updates
!
!     40.23, Feb. 03: New subroutine
!
!  2. Purpose
!
!     Convert integer to string
!
!  4. Argument variables
!
!     IVAL        integer to be converted

   INTEGER, INTENT(IN) :: IVAL

!  6. Local variables
!
!     CVAL  :     character represented an integer of mantisse
!     I     :     counter
!     IPOS  :     position in mantisse
!     IQUO  :     whole quotient

   INTEGER I, IPOS, IQUO, IVALUE
   CHARACTER(LEN=1), ALLOCATABLE :: CVAL(:)

! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IVALUE = IVAL
   IPOS = 1
   DO WHILE (IVALUE/10**IPOS.GE.1.)
      IPOS = IPOS + 1
   END DO
   ALLOCATE(CVAL(IPOS))

   DO I=IPOS,1,-1
      IQUO=IVALUE/10**(I-1)
      CVAL(IPOS-I+1)=CHAR(INT(IQUO)+48)
      IVALUE=IVALUE-IQUO*10**(I-1)
   END DO

   WRITE (INTSTR,*) (CVAL(I), I=1,IPOS)

   RETURN
end function INTSTR

CHARACTER(LEN=20) FUNCTION NUMSTR ( IVAL, RVAL, FORM )

!****************************************************************

   IMPLICIT NONE(TYPE, EXTERNAL)


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
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
!     40.23: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.23, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Convert integer or real to string with given format
!
!  4. Argument variables
!
!     IVAL        integer to be converted
!     FORM        given format
!     RVAL        real to be converted

   INTEGER, INTENT(IN) :: IVAL
   REAL, INTENT(IN) :: RVAL
   CHARACTER(LEN=*), INTENT(IN) :: FORM

!  6. Local variables
!
! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IF ( IVAL.NE.INAN ) THEN
      WRITE (NUMSTR,FORM) IVAL
   ELSE IF ( RVAL.NE.RNAN ) THEN
      WRITE (NUMSTR,FORM) RVAL
   ELSE
      NUMSTR = ''
   END IF

   RETURN
end function NUMSTR

END MODULE swan_number_formatting
