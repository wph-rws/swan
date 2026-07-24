module swan_input_interpolation
   implicit none
   private
   public :: SVALQI

contains

REAL FUNCTION SVALQI (XP, YP, IGRID, ARRINP, ZERO ,IXC ,IYC)
   USE swan_point_interpolation, ONLY: SwanInterpolatePoint
   USE swan_service_interfaces, ONLY: EQREAL, STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
   USE M_PARALL

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
!     30.60: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.82: IJsbrand Haagsma
!     32.03: Nico Booij
!     40.04: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.60, Aug. 97: inequalities changed in view of bug reported by
!                     Ralf Kaiser (GT -> GE and LT -> LE)
!     32.03, Feb. 98: option for 1-D computation introduced
!                     real equality changed into inequality
!     30.82, Apr. 98: Replace statement with division through DYG to avoid division
!                     through zero in case of 1D.
!     30.82, Nov. 98: Now takes care of interpolation near points that
!                     contain exception values
!     40.04, Aug. 00: Interpolation near points that contain exception values
!                     modified
!                   : Removed include files that are not used
!     40.30, Mar. 03: correcting indices IXC, IYC with offsets MXF, MYF
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Dec. 07: extension to unstructured grids
!
!  2. Purpose
!
!     Determining the value of a quantity from an input grid
!     such as depth and the current velocity components
!     for point given in problem coordinates
!
!  3. Method (updated...)
!
!     The required values are computed by bilinear interpolation. The
!     coordinates are given in the bottom grid as the number of meshes
!     in X- and Y-direction, IB and JB respectively (both real).
!
!           YB|
!             |
!             |--------------- *                *
!             | A
!             | |SYB1
!             | V
!         IYB-|-------------------- o
!             |
!             |                     |
!         JB1-|--------------- *    |           *
!             |                     |
!             |                |    |    SXB1   |
!             |                |    |<--------->|
!             +----------------------------------------------->
!                              |    |                       XB
!                             IB1  IXB
!
!                   *  bottom grid points
!                   o  point for interpolation
!
!  4. Argument variables
!
!     IGRID    Grid indicator
!     IXC      Counter for X-coordinate in computational grid (used
!              in curvilinear case)
!     IYC      Counter for Y-coordinate in computational grid (used
!              in curvilinear case)
!     ZERO     If ZERO=0, then value outside the grid is zero, otherwise
!              the value is extrapolated

   INTEGER  IGRID, IXC, IYC, ZERO

!     ARRINP   Array holding the values at the input grid locations
!     SVALQI   Value of quantity in (XP,YP)
!     XP       X-coordinate in computational gridpoint
!     YP       Y-coordinate in computational gridpoint

   REAL     ARRINP(*), XP, YP

!  5. Parameter variables
!
!  6. Local variables
!
!     EQREAL   Boolean function which compares two REAL values
!     IB1      Grid counter in x-direction
!     IENT     Number of entries into this subroutine
!     II       Pointer number in ARRINP
!     INGRD    Boolean variable to determine whether point is in grid
!     IXB      Distance to origin in x-direction devided by meshsize
!              in y-direction
!     IXCGL    X-index with respect to global grid
!     IYB      Distance to origin in y-direction devided by meshsize
!              in y-direction
!     IYCGL    Y-index with respect to global grid
!     JB1      Grid counter in y-direction
!     SUMWEXC  Sum of weight factors of points with exception value
!     SUMWREG  Sum of weight factors of points with regular   value
!     SXB1     First weight factor for distance in x-direction
!     SXB2     Second weight factor for distance in x-direction
!     SYB1     First weight factor for distance in y-direction
!     SYB2     Second weight factor for distance in y-direction
!     WF1      Weight factor of point ARRINP(II)
!     WF2      Weight factor of point ARRINP(II+MXG(IGRID))
!     WF3      Weight factor of point ARRINP(II+1)
!     WF4      Weight factor of point ARRINP(II+1+MXG(IGRID))

   INTEGER, SAVE :: IENT = 0
   INTEGER  IB1, II, JB1, IXCGL, IYCGL
   REAL     IXB, IYB, SXB1, SXB2, SYB1, SYB2
   REAL     SUMWEXC, SUMWREG, WF1, WF2, WF3, WF4
   LOGICAL  INGRD

!  8. Subroutines used
!
!     LOGICAL FUNCTION EQREAL: Checks whether two reals are equal within certain margins
!     STRACE: Traces the entry into subroutines (test purposes)


!  9. Subroutines calling
!
!     SWDIM
!     INTEGER FUNCTION SIRAY
!     SWRBC
!     SNEXTI
!     FLFILE
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       If the point is out of bottom grid in X-direction, then
!           Compute lines for interpolation and interpolation factors
!             such that the value at the side of the grid is taken
!       Else
!           Compute nearest line IX in the bottom grid and the interpo-
!             lation factor in X-direction
!       ----------------------------------------------------------------
!       If the point is out of bottom grid in Y-direction, then
!           Compute lines for interpolation and interpolation factors
!             such that the value at the side of the grid is taken
!       Else
!           Compute nearest line IY in the bottom grid and the interpo-
!             lation factor in Y-direction
!       ----------------------------------------------------------------
!       Compute pointer in arrays and interpolation factors in both
!       directions
!       Compute the depth to the reference level for the point
!       Add the water level to the depth
!       If depth > 0 and current is on, then
!           Interpolate X- and Y-component of current velocity
!       Else
!           Current components are zero
!       ----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE (IENT, 'SVALQI')

!     --- take global indices instead of local ones

   IXCGL = IXC + MXF - 1
   IYCGL = IYC + MYF - 1

!     ***    Two different procedures in funcion of       ***
!     ***    grid type: regular or curvilinear (staggered)*** ver 30.21

   IF (IGTYPE(IGRID) .EQ. 1) THEN

!     Regular grid:

      IXB = ( (XP-XPG(IGRID))*COSPG(IGRID) +&
      &(YP-YPG(IGRID))*SINPG(IGRID) ) / DXG(IGRID)

      INGRD = .TRUE.
      IF (IXB .LE. 0.) THEN
         IB1   = 1
         SXB2  = 0.
         IF (IXB.LT.-0.1) INGRD = .FALSE.
      ELSE IF (IXB .GE. FLOAT(MXG(IGRID)-1)) THEN
         IB1   = MXG(IGRID)-1
         SXB2  = 1.
         IF (IXB.GT.FLOAT(MXG(IGRID))-0.9) INGRD = .FALSE.
      ELSE
         IB1   = INT(IXB)
         SXB2  = IXB-REAL(IB1)
         IB1   = IB1+1
      ENDIF
      IF (MYG(IGRID).GT.1) THEN
         IYB = (-(XP-XPG(IGRID))*SINPG(IGRID) +&
         &(YP-YPG(IGRID))*COSPG(IGRID) ) / DYG(IGRID)
         IF (IYB .LE. 0.) THEN
            JB1   = 1
            SYB2  = 0.
            IF (IYB.LT.-0.1) INGRD = .FALSE.
         ELSE IF (IYB .GE. FLOAT(MYG(IGRID)-1)) THEN
            JB1   = MYG(IGRID)-1
            SYB2  = 1.
            IF (IYB.GT.FLOAT(MYG(IGRID))-0.9) INGRD = .FALSE.
         ELSE
            JB1   = INT(IYB)
            SYB2  = IYB-REAL(JB1)
            JB1   = JB1+1
         ENDIF
      ENDIF

!       evaluate SVALQI (2D-mode):

      IF (.NOT.INGRD .AND. ZERO.EQ.0) THEN
         SVALQI = 0.
      ELSE IF (MYG(IGRID).GT.1) THEN
         SXB1   = 1.- SXB2
         SYB1   = 1.- SYB2
         II     = IB1 + (JB1-1) * MXG(IGRID)
         WF1 = SXB1*SYB1
         WF2 = SXB1*SYB2
         WF3 = SXB2*SYB1
         WF4 = SXB2*SYB2
         SUMWEXC = 0.
         IF  (EQREAL(ARRINP(II             ),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF1
            WF1 =0.
         ENDIF
         IF (EQREAL(ARRINP(II+  MXG(IGRID)),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF2
            WF2=0.
         ENDIF
         IF (EQREAL(ARRINP(II+1           ),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF3
            WF3=0.
         ENDIF
         IF (EQREAL(ARRINP(II+1+MXG(IGRID)),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF4
            WF4=0.
         ENDIF
         SUMWREG = 1. -SUMWEXC

         IF (SUMWEXC.GE.SUMWREG)   THEN
            SVALQI = EXCFLD(IGRID)
         ELSE
            SVALQI = ( WF1*ARRINP(II)   + WF2*ARRINP(II+MXG(IGRID))&
            &+ WF3*ARRINP(II+1) + WF4*ARRINP(II+1+MXG(IGRID)) )&
            &/ SUMWREG
         END IF
      ELSE

!       evaluate SVALQI (1D-mode):

         SXB1 = 1. - SXB2
         IF (EQREAL(ARRINP(IB1  ),EXCFLD(IGRID)).OR.&
         &EQREAL(ARRINP(IB1+1),EXCFLD(IGRID))    ) THEN

!           One of the cornerpoints contains an exception value thus:

            SVALQI = EXCFLD(IGRID)
         ELSE
            SVALQI = SXB1*ARRINP(IB1)&
            &+ SXB2*ARRINP(IB1+1)
         ENDIF
      ENDIF
   ELSEIF ( IGTYPE(IGRID).EQ.3 ) THEN

!     unstructured grid

      CALL SwanInterpolatePoint(SVALQI, XP, YP, ARRINP, EXCFLD(IGRID))

   ELSE IF (ABS(STAGX(IGRID)) .LT. 0.01 .AND.&
   &ABS(STAGY(IGRID)) .LT. 0.01) THEN

!     Curvilinear and non-staggered input grid:

      IB1   = IXCGL
      JB1   = IYCGL
      II     = IB1 + (JB1-1) * MXG(IGRID)
      SVALQI = ARRINP(II)
   ELSE

!     Curvilinear and staggered input grid:

      INGRD = .TRUE.
      IF (IXCGL .EQ. 1) THEN
         IB1   = 1
         SXB2  = 0.
         IF (STAGY(IGRID) .GT. 0.) INGRD = .FALSE.
      ELSE IF (IXCGL .GT. MXG(IGRID)-1) THEN
         IB1   = MXG(IGRID)-1
         SXB2  = 1.
         IF (STAGY(IGRID) .GT. 0.) INGRD = .FALSE.
      ELSE
         IB1   = IXCGL + 1
         SXB2  = 1. - STAGX(IGRID)
      ENDIF
      IF (IYCGL .EQ. 1) THEN
         JB1   = 1
         SYB2  = 0.
         IF (STAGX(IGRID) .GT. 0.) INGRD = .FALSE.
      ELSE IF (IYCGL .GT. MYG(IGRID)-1) THEN
         JB1   = MYG(IGRID)-1
         SYB2  = 1.
         IF (STAGY(IGRID) .GT. 0.) INGRD = .FALSE.
      ELSE
         JB1   = IYCGL + 1
         SYB2  =1. - STAGY(IGRID)
      ENDIF

!       evaluate SVALQI (2D-mode):

      IF (.NOT.INGRD .AND. ZERO.EQ.0) THEN
         SVALQI = 0.
      ELSE
         SXB1   = STAGX(IGRID)
         SYB1   = STAGY(IGRID)
         II     = IB1 + (JB1-1) * MXG(IGRID)
         WF1 = SXB1*SYB1
         WF2 = SXB1*SYB2
         WF3 = SXB2*SYB1
         WF4 = SXB2*SYB2
         SUMWEXC = 0.
         IF (EQREAL(ARRINP(II             ),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF1
            WF1 =0.
         ENDIF
         IF (EQREAL(ARRINP(II+  MXG(IGRID)),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF2
            WF2=0.
         ENDIF
         IF (EQREAL(ARRINP(II+1           ),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF3
            WF3=0.
         ENDIF
         IF (EQREAL(ARRINP(II+1+MXG(IGRID)),EXCFLD(IGRID))) THEN
            SUMWEXC = SUMWEXC + WF4
            WF4=0.
         ENDIF
         SUMWREG = 1. -SUMWEXC

         IF (SUMWEXC.GE.SUMWREG)   THEN
            SVALQI = EXCFLD(IGRID)
         ELSE
            SVALQI = ( WF1*ARRINP(II)   + WF2*ARRINP(II+MXG(IGRID))&
            &+ WF3*ARRINP(II+1) + WF4*ARRINP(II+1+MXG(IGRID)) )&
            &/ SUMWREG
         END IF
      ENDIF
   ENDIF

!     ***** test *****
   IF (ITEST .GE. 280)&
!     &   WRITE(PRINTF, "(' Test SVALQI:',5F10.3)") SVALQI,IGRID,XP,YP,IXB,IYB,II,ARRINP(II)
! 6010 FORMAT(' SVALQI  IGRID       XP      YP        IXB',
!     &       '       IYB  II  ARRINP(II)', /
!     &      ,E10.3,I3,1X,4E10.3,I4,E10.3)
   &WRITE(PRINTF, "(' Test SVALQI:',5F10.3)") XP, YP, IXB, IYB, SVALQI

   RETURN
!     end of function SVALQI
end function SVALQI

end module swan_input_interpolation
