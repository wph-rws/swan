module swan_structured_output_interpolation
   implicit none
   private
   public :: SWIPOL

contains

SUBROUTINE SWIPOL (FINP, EXCVAL, XC, YC, MIP, CROSS, FOUTP,&
&KGRPNT, DEP2)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE SWCOMM4


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
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!     40.86: Nico Booij
!
!  1. UPDATE
!
!     40.00, July 98: no interpolation if one or more corners are dry
!                     argument DEP2 added
!                     margin around comp. grid introduced
!     40.13, Aug. 01: provision for repeating grid
!                     swcomm4.inc reactivated
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.86, Feb. 08: interpolation over an obstacle prevented
!
!  2. PURPOSE
!
!       Interpolate the function FINP to the point given by computational
!       grid coordinates XC and YC; result appears in array FOUTP
!
!  3. METHOD
!
!       This subroutine computes the contributions from surrounding
!       points to the function value in an output point. The points used
!       are indicated in the sketch below.
!
!                                                            Y
!           +-------------------------------------------------->
!           |
!           |       .       .       .       .       .       .
!           |
!           |
!           |
!           |       .       .       *       *       .       .
!           |
!           |
!           |                         o
!           |       .       .       *       *       .       .
!           |
!           |
!           |
!           |       .       .       .       .       .       .
!           |
!           |
!           |
!         X |       .       .       .       .       .       .
!           |
!           V
!
!                 *   point of the computational grid contributing
!                       to output point (o)
!                 .   other grid points
!
!  4. PARAMETERLIST
!
!       FINP    real a input    array of function values defined on the
!                               computational grid
!       EXCVAL  real   input    exception value (assigned if point is outside
!                               computational grid)
!       XC, YC  real a input    array containing computational grid coordinates
!                               of output points
!       MIP     INT    input    number of output points
!       FOUTP   real a output   array of interpolated values for the output
!                               points
!
!  5. SUBROUTINES CALLING
!
!       SWOEXD
!
!  6. SUBROUTINES USED
!
!       ---
!
!  7. ERROR MESSAGES
!
!       ---
!
!  8. REMARKS
!
!       IINTPC=1: bilinear interpolation
!       IINTPC=2: higher order interpolation using functions G1 and G2
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       For every output point do
!           If the output point is near line XCL then
!               Determine points contributing to the output point
!               Compute contribution to the projection of the output
!                 point on line XCL
!               Compute multiplication factor for interpolation in X-
!                 direction
!               Compute contribution for the output point
!               Add result to value of variable for the output point in
!                 array IFOP
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER, INTENT(IN) :: MIP
   REAL, INTENT(IN) :: FINP(MCGRD), XC(MIP), YC(MIP), DEP2(MCGRD)
   REAL, INTENT(IN) :: EXCVAL
   REAL, INTENT(OUT) :: FOUTP(MIP)
   LOGICAL, INTENT(IN) :: CROSS(4,MIP) ! true if obstacle is between output point
   ! and computational grid point
   LOGICAL OUTSID
   INTEGER, INTENT(IN) :: KGRPNT(MXC,MYC)

   REAL(KIND=KIND(0.0D0)) :: WW(1:4)  ! Interpolation weights for the 4 corners
   REAL(KIND=KIND(0.0D0)) :: SUMWW    ! sum of the weights
   INTEGER :: JX(1:4), JY(1:4) ! grid counters for the 4 corners
   INTEGER :: INDX(1:4)     ! grid counters for the 4 corners
   INTEGER :: JC            ! corner counter
   INTEGER, SAVE :: IENT = 0
   INTEGER :: IP, JX1, JX2, JY1, JY2
   REAL :: SX1, SX2, SY1, SY2

   IF (LTRACE) CALL  STRACE (IENT, 'SWIPOL')

   IF (ITEST.GE.150) WRITE (PRTEST, "(' XC , YC ,', ' JX1, JY1, JX2, JY2 SX1, SY1, FOUTP(IP),', ' INDX1 INDX2 INDX3 INDX4')")

   do IP=1,MIP
      point_interpolation: BLOCK
      IF (XC(IP) .LE. -0.5 .OR. YC(IP) .LE. -0.5) THEN
         FOUTP(IP) = EXCVAL
         JX1   = 0
         JY1   = 0
         JX2   = 0
         JY2   = 0
         SX1   = 0.
         SX2   = 0.
         INDX(1:4) = 0
         EXIT point_interpolation
      ENDIF
      OUTSID = .FALSE.
      FOUTP(IP) = 0.
      JX1 = INT(XC(IP)+3.001) - 2
      JX2 = JX1 + 1
      SX2 = XC(IP) + 1. - FLOAT(JX1)
      SX1 = 1. - SX2
      IF (JX1.LT.0)   OUTSID = .TRUE.
      IF (KREPTX .EQ. 0) THEN
         IF (JX1.GT.MXC) OUTSID = .TRUE.
         IF (JX1.EQ.MXC) JX2 = MXC
         IF (JX1.EQ.0)   JX1 = 1
      ELSE
         JX1 = 1 + MODULO (JX1-1,MXC)
         JX2 = 1 + MODULO (JX2-1,MXC)
      ENDIF
      IF (ONED) THEN
         JY1 = 1
         JY2 = 1
         SY1 = 0.5
         SY2 = 0.5
      ELSE
         JY1 = INT(YC(IP)+3.001) - 2
         JY2 = JY1 + 1
         SY2 = YC(IP) + 1. - FLOAT(JY1)
         SY1 = 1. - SY2
         IF (JY1.LT.0)   OUTSID = .TRUE.
         IF (JY1.GT.MYC) OUTSID = .TRUE.
         IF (JY1.EQ.MYC) JY2 = MYC
         IF (JY1.EQ.0)   JY1 = 1
      ENDIF
      IF (OUTSID) THEN
         FOUTP(IP) = EXCVAL
      ELSE
         JX(1) = JX1
         JY(1) = JY1
         WW(1) = SX1*SY1
         JX(2) = JX2
         JY(2) = JY1
         WW(2) = SX2*SY1
         JX(3) = JX1
         JY(3) = JY2
         WW(3) = SX1*SY2
         JX(4) = JX2
         JY(4) = JY2
         WW(4) = SX2*SY2
         DO JC = 1, 4
            INDX(JC) = KGRPNT(JX(JC),JY(JC))
            IF (WW(JC).LT.0.01) THEN
               WW(JC) = 0.
            ELSE
               IF (INDX(JC).LE.1) THEN
                  WW(JC) = 0.
               ELSE IF (DEP2(INDX(JC)).LE.DEPMIN) THEN
                  OUTSID = .TRUE.
               ELSE IF (CROSS(JC,IP) .AND. WW(JC).LT.0.999) THEN
                  WW(JC) = 0.
               ENDIF
            ENDIF
         ENDDO
         SUMWW = SUM(WW(1:4))
         IF (OUTSID) THEN
            FOUTP(IP) = EXCVAL
         ELSE
            IF (SUMWW.GT.0.1) THEN
               FOUTP(IP) = SUM(WW*FINP(INDX)) / SUMWW
            ELSE
               FOUTP(IP) = EXCVAL
            ENDIF
         ENDIF
      ENDIF
      END BLOCK point_interpolation
      IF (ITEST.GE.150) WRITE (PRTEST, "(2(F7.1,1X),4I5, 4(1X,F5.2), 3X,4(2X,I5), 3X, 4(1X,E9.3))")&
      &XC(IP) , YC(IP) ,JX1, JY1, JX2,JY2, (WW(JC),JC=1,4),&
      &(INDX(JC), JC=1,4), (FINP(INDX(JC)), JC=1,4)
   end do

   RETURN
! * end of subroutine SWIPOL *
end subroutine SWIPOL

end module swan_structured_output_interpolation
