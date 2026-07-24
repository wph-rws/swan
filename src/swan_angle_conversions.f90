MODULE swan_angle_conversions
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: DEGCNV, ANGRAD, ANGDEG

CONTAINS

REAL FUNCTION DEGCNV (DEGREE)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE SWCOMM3


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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform degrees from nautical to cartesian or vice versa.
!
!  3. METHOD
!
!       DEGCNV = 180 + dnorth - degree
!
!  4. PARAMETERLIST
!
!       DEGCNV      direction in cartesian or nautical degrees.
!       DEGREE      direction in nautical or cartesian degrees.
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
!
!  7. ERROR MESSAGES
!
!       NONE
!
!  8. REMARKS
!
!           Nautical convention           Cartesian convention
!
!                    0                             90
!                    |                              |
!                    |                              |
!                    |                              |
!                    |                              |
!        270 --------+-------- 90       180 --------+-------- 0
!                    |                              |
!                    |                              |
!                    |                              |
!                    |                              |
!                   180                            270
!
!  9. STRUCTURE
!
!     ---------------------------------
!     IF (NAUTICAL DEGREES) THEN
!       CONVERT DEGREES
!     IF (DEGREES > 360 OR < 0) THEN
!       CORRECT DEGREES WITHIN 0 - 360
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL, INTENT(IN) :: DEGREE
   CALL STRACE(IENT,'DEGCNV')

   IF ( BNAUT ) THEN
      DEGCNV = 180. + DNORTH - DEGREE
   ELSE
      DEGCNV = DEGREE
   ENDIF

   IF (DEGCNV .GE. 360.) THEN
      DEGCNV = MOD (DEGCNV, 360.)
   ELSE IF (DEGCNV .LT. 0.) THEN
      DEGCNV = MOD (DEGCNV, 360.) + 360.
   ELSE
!       DEGCNV between 0 and 360; do nothing
   ENDIF


!     *** end of subroutine DEGCNV ***

   RETURN
end function DEGCNV

REAL FUNCTION ANGRAD (DEGREE)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE SWCOMM3


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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform degrees to radians
!
!  3. METHOD
!
!       ANGRAD = DEGREE * PI / 180
!
!  4. PARAMETERLIST
!
!       ANGRAD      radians
!       DEGREE      degrees
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
!
!  7. ERROR MESSAGES
!
!       NONE
!
!  8. REMARKS
!
!       NONE
!
!  9. STRUCTURE
!
!     ---------------------------------
!     ANGLE[radian] = ANGLE[degrees} * PI / 180
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL, INTENT(IN) :: DEGREE
   CALL STRACE(IENT,'ANGRAD')

   ANGRAD = DEGREE * PI / 180.


!     *** end of subroutine ANGRAD ***

   RETURN
end function ANGRAD

REAL FUNCTION ANGDEG (RADIAN)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE SWCOMM3


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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform radians to degrees
!
!  3. METHOD
!
!       ANGDEG = RADIAN * 180 / PI
!
!  4. PARAMETERLIST
!
!       RADIAN      radians
!       ANGDEG      degrees
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
!
!  7. ERROR MESSAGES
!
!       NONE
!
!  8. REMARKS
!
!       NONE
!
!  9. STRUCTURE
!
!     ---------------------------------
!     ANGLE[degrees] = ANGLE[radians} * 180 / PI
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL, INTENT(IN) :: RADIAN
   CALL STRACE(IENT,'ANGDEG')

   ANGDEG = RADIAN * 180. / PI


!     *** end of subroutine ANGDEG ***

   RETURN
end function ANGDEG

END MODULE swan_angle_conversions
