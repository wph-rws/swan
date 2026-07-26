module swan_array_copy
   implicit none(type, external)
   private
   public :: SWCOPR, SWCOPI

contains

SUBROUTINE SWCOPR ( ARR1, ARR2, LENGTH )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE OCPCOMM4

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
!     Copies real array ARR1 to ARR2
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     ARR1        source array
!     ARR2        target array
!     LENGTH      array length

   INTEGER, INTENT(IN) :: LENGTH
   REAL, INTENT(IN) :: ARR1(LENGTH)
   REAL, INTENT(OUT) :: ARR2(LENGTH)

!  6. Local variables
!
!     I     :     loop counter
!     IENT  :     number of entries

   INTEGER, SAVE :: IENT = 0
   INTEGER I

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOPR')

!     --- check array length

   IF ( LENGTH.LE.0 ) THEN
      CALL MSGERR( 3, 'Array length should be positive' )
   END IF

!     --- copy elements of array ARR1 to ARR2

   do I = 1, LENGTH
      ARR2(I) = ARR1(I)
   end do

   RETURN
end subroutine SWCOPR

SUBROUTINE SWCOPI ( IARR1, IARR2, LENGTH )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE OCPCOMM4

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
!     Copies integer array IARR1 to IARR2
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     IARR1       source array
!     IARR2       target array
!     LENGTH      array length

   INTEGER, INTENT(IN) :: LENGTH
   INTEGER, INTENT(IN) :: IARR1(LENGTH)
   INTEGER, INTENT(OUT) :: IARR2(LENGTH)

!  6. Local variables
!
!     I     :     loop counter
!     IENT  :     number of entries

   INTEGER, SAVE :: IENT = 0
   INTEGER I

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOPI')

!     --- check array length

   IF ( LENGTH.LE.0 ) THEN
      CALL MSGERR( 3, 'Array length should be positive' )
   END IF

!     --- copy elements of array IARR1 to IARR2

   do I = 1, LENGTH
      IARR2(I) = IARR1(I)
   end do

   RETURN
end subroutine SWCOPI

end module swan_array_copy
