MODULE swan_geometry
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: TCROSS, TCROSS_KERNEL

CONTAINS

LOGICAL FUNCTION TCROSS (X1, X2, X3, X4, Y1, Y2, Y3, Y4, X1ONOBST)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units

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
!     40.00  Gerbrant van Vledder
!     40.04  Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!       30.70, Feb 98: argument list simplified
!                      subroutine changed into logical function
!       40.00, Aug 98: division by zero prevented
!       40.04, Aug 99: method corrected, IMPLICIT NONE added, XCONOBST added,
!                      introduced TINY and EPSILON (instead of comparing to 0)
!                      replaced 0 < LMBD,MIU by  0 <= LMBD,MIU
!                      XCONOBST added to argument list
!       40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!       Find if there is an obstacle crossing the stencil in used
!
!  3. Method
!
!     For the next situation (A, B and C are the points in the stencil,
!     D and E  are corners of the obstacle
!
!
!      obstacle --> D(X3,Y3)
!                    *
!                     *
!                      *
!        (X2,Y2)        * (XC,YC)
!            B-----------@--------------------------A (X1,Y1)
!                        ^*                         /
!                   _____| *                       /
!                  |        *                     /
!                  |         *                   /
!         crossing point      *                 /
!                              *               /
!                               E             /
!                              (X4,Y4)       /
!                                           C
!
!
!       The crossing point (@) should be found solving the next eqs.
!       for LMBD and MIU.
!
!       | XC |    | X1 |           | X2 - X1 |
!       |    | =  |    | +  LMBD * |         |
!       | YC |    | Y1 |           | Y2 - Y1 |
!
!
!       | XC |    | X3 |           | X4 - X3 |
!       |    | =  |    | +  MIU  * |         |
!       | YC |    | Y3 |           | Y4 - Y3 |
!
!
!     If solution exist and (0 <= LMBD <= 1 and 0 <= MIU <= 1)
!     there is an obstacle crossing the stencil
!
!  4. Argument variables
!
!     X1, Y1  inp    user coordinates of one end of grid link
!     X2, Y2  inp    user coordinates of other end of grid link
!     X3, Y3  inp    user coordinates of one end of obstacle side
!     X4, Y4  inp    user coordinates of other end of obstacle side
!     X1ONOBST outp   boolean which tells whether (X1,Y1) is on obstacle

   REAL, INTENT(IN) :: X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL, INTENT(OUT) :: X1ONOBST

!  5. Parameter variables
!
!  6. Local variables
!
!     A,B,C,D    dummy variables
!     DIV1       denominator of value of LMBD (or MIU)
!     E,F        dummy variables
!     IENT       number of entries
!     LMBD       coefficient in vector equation for stencil points (or obstacle)
!     MIU        coefficient in vector equation for obstacle (or stencil points)

   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWOBST
!     SWTRCF
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!     Calculate MIU and LMBD
!     If 0 <= MIU, LMBD <= 1
!     Then TCROSS is .True.
!     Else TCROSS is .False.
!
! 13. Source text
! ======================================================================
   IF (LTRACE) CALL STRACE (IENT,'TCROSS')

   CALL TCROSS_KERNEL(X1, X2, X3, X4, Y1, Y2, Y3, Y4, TCROSS, X1ONOBST)

   IF (TCROSS .AND. ITEST .GE. 120) THEN
      WRITE(PRINTF,"(' Obstacle crossing :',/, ' Coordinates of comp grid points and corners of obstacle:',/, ' P1(',E10.4,',',E10.4,')',' P2(',E10.4,',',E10.4,')',/, ' P3(',E10.4,',',E10.4,')',' P4(',E10.4,',',E10.4,')')")X1,Y1,X2,Y2,X3,Y3,X4,Y4
   ENDIF

!     End of subroutine TCROSS
   RETURN
end function TCROSS

PURE SUBROUTINE TCROSS_KERNEL (X1, X2, X3, X4, Y1, Y2, Y3, Y4, CROSSING, X1ONOBST)
   IMPLICIT NONE(TYPE, EXTERNAL)

   REAL, INTENT(IN) :: X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL, INTENT(OUT) :: CROSSING
   LOGICAL, INTENT(OUT) :: X1ONOBST
   REAL :: EPS
   REAL :: A, B, C, D, DIV1, E, F, LMBD, MIU

   EPS = EPSILON(X1)*SQRT((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1))
   IF (EPS ==0.) EPS = TINY(X1)
   A    = X2 - X1
!     A not equal to zero
   IF (ABS(A) .GT. TINY(X1)) THEN
      B    = X4 - X3
      C    = X3 - X1
      D    = Y2 - Y1
      E    = Y4 - Y3
      F    = Y3 - Y1
   ELSE
!       exchange MIU and LMBD
      A    = X4 - X3
      B    = X2 - X1
      C    = X1 - X3
      D    = Y4 - Y3
      E    = Y2 - Y1
      F    = Y1 - Y3
   ENDIF
   DIV1 = ((A*E) - (D*B))

!     DIV1 = 0 means that obstacle is parallel to line through
!     stencil points, or (X3,Y3) = (X4,Y4);
!     A = 0 means trivial set of equations X4= X3 and X2 =X1

   IF ((ABS(DIV1).LE.TINY(X1)) .OR.&
   &(ABS(A).LE.TINY(X1))) THEN
      MIU = -1.
      LMBD = -1.
   ELSE
      MIU  = ((D*C) - (A*F)) / DIV1
      LMBD = (C + (B*MIU)) / A
   END IF

   IF (MIU  .GE. 0. .AND. MIU  .LE. 1. .AND.&
   &LMBD .GE. 0. .AND. LMBD .LE. 1.) THEN

!       Only (X1,Y1) is checked, because of otherwise possible double
!       counting
      IF ((LMBD.LE.EPS .AND. ABS(X2-X1).GT.EPS).OR.&
      &(MIU .LE.EPS .AND. ABS(X2-X1).LE.EPS))THEN
         X1ONOBST = .TRUE.
      ELSE
         X1ONOBST = .FALSE.
      ENDIF

!       *** test output ***
      CROSSING = .TRUE.
   ELSE
      CROSSING = .FALSE.
   ENDIF

   RETURN
end subroutine TCROSS_KERNEL

END MODULE swan_geometry
