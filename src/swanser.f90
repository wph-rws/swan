
!     SWAN - SERVICE ROUTINES
!
!  Contents of this file
!
!     READXY
!     REFIXY
!     INFRAM
!     AC2TST
!     CVCHEK
!     CVMESH
!     NEWTON
!     EVALF
!     SWOBST
!     TCROSS
!     SWTRCF
!     REFLECT
!     SSHAPE
!     SINTRP
!     HSOBND
!     CHGBAS
!     GAMMAF
!     WRSPEC
!     SWTSTA
!     SWTSTO
!     SWPRTI
!     TXPBLA
!     INTSTR
!     NUMSTR
!     SWCOPI
!     SWCOPR
!     SWI2B
!     SWR2B
!     MKPATH
!
!  functions:
!  ----------
!  DEGCNV  (converts from cartesian convention to nautical and
!           vice versa)
!  ANGRAD  (converts radians to degrees)
!  ANGDEG  (converts degrees to radians)
!
!  subroutines:
!  ------------
!  HSOBND  (Hs is calculated after a SWAN computation at all sides.
!           The calculated wave height from SWAN is then compared with
!           the wave heigth as provided by the user
!
!************************************************************************
!                                                                      *
!************************************************************************
!                                                                      *
!************************************************************************
!                                                                      *
module swan_services
   use swan_cross_obstacle, only: SwanCrossObstacle
   use swan_output_quadrature, only: XQLEN, YQLEN
   implicit none(type, external)
   private
   public :: AC2TST, CVCHEK, CVMESH, EVALF, SWOBST, SWTRCF, HSOBND, SWACC, MKPATH
contains

LOGICAL FUNCTION  INFRAM (XQQ, YQQ)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level


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
!     40.41: Marcel Zijlema
!
!  1. UPDATE
!
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!       Checking whether a point given in frame coordinates is located
!       in the plotting frame (INFRAM = .TRUE.) or not (INFRAM = .FALSE.)
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       XQQ     REAL   input    X-coordinate (output grid) of the point
!       YQQ     REAL   input    Y-coordinate (output grid) of the point
!
!  5. SUBROUTINES CALLING
!
!       SPLSIT, PLNAME
!
!  6. SUBROUTINES USED
!
!       none
!
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
!       Give INFRAM initial value true
!       IF XQQ < 0, XQQ > XQLEN, YQQ < 0 OR YQQ > YQLEN, THEN
!           INFRAM = false
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER, SAVE :: IENT = 0
   REAL XQQ, YQQ
   IF (LTRACE) CALL STRACE (IENT,'INFRAM')

   INFRAM = .TRUE.
   IF (XQQ .LT.    0.) INFRAM = .FALSE.
   IF (XQQ .GT. XQLEN) INFRAM = .FALSE.
   IF (YQQ .LT.    0.) INFRAM = .FALSE.
   IF (YQQ .GT. YQLEN) INFRAM = .FALSE.

   RETURN
! * end of function INFRAM *
end function INFRAM

!************************************************************************
!                                                                      *
SUBROUTINE AC2TST (XYTST, AC2,KGRPNT)
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE M_PARALL


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
!     0. Authors





   INTEGER   XYTST(*) ,KGRPNT(MXC,MYC)
   INTEGER   ID, II, INDEX, IS, IX, IY
   REAL      AC2(MDC,MSC,MCGRD)
!.................................................................
   IF ( ITEST .GE. 100 .AND. TESTFL) THEN
      DO II = 1, NPTST
         IF (OPTG.NE.5) THEN
            IX = XYTST(2*II-1)
            IY = XYTST(2*II)
            INDEX = KGRPNT(IX,IY)
            WRITE (PRINTF, "(/,'Spectrum for test point(index):', 2I5,2X,'(',I5,')')") IX+MXF-2, IY+MYF-2, KGRPNT(IX,IY)
         ELSE
            INDEX = XYTST(II)
            WRITE (PRINTF, "(/,'Spectrum for test point: (',I5,')')") INDEX
         ENDIF
         DO ID = 1, MDC
            WRITE (PRINTF, "(10(1X,E12.4))") (AC2(ID,IS,INDEX), IS=1,MIN(10,MSC))
         ENDDO
      ENDDO
   ENDIF
   RETURN
end subroutine AC2TST
!****************************************************************

SUBROUTINE CVCHEK (KGRPNT, XCGRID, YCGRID)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_computational_grid
   USE swan_test_output


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
!  0. Authors
!
!     30.72: IJsbrand Haagsma
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!            May  96: New subroutine
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.13, Mar. 01: messages corrected and extended
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Checks whether the given curvilinear grid is correct
!     also set the value of CVLEFT.
!
!  3. Method
!
!     Going around a mesh in the same direction the interior
!     of the mesh must be always in the same side if the
!     coordinates are correct
!
!  4. Argument variables
!
!     KGRPNT: input  Array of indirect addressing

   INTEGER KGRPNT(MXC,MYC)

!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)


!     5. SUBROUTINES CALLING
!
!        SWRBC
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. ERROR MESSAGES
!
!        ---
!
!     8. REMARKS
!
!
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!     FIRST = True
!     For ix=1 to MXC-1 do
!         For iy=1 to MYC-1 do
!             For iside=1 to 4 do
!                 Case iside=
!                 1: K1 = KGRPNT(ix,iy), K2 = KGRPNT(ix+1,iy),
!                    K3 = KGRPNT(ix+1,iy+1)
!                 2: K1 = KGRPNT(ix+1,iy), K2 = KGRPNT(ix+1,iy+1),
!                    K3 = KGRPNT(ix,iy+1)
!                 3: K1 = KGRPNT(ix+1,iy+1), K2 = KGRPNT(ix,iy+1),
!                    K3 = KGRPNT(ix,iy)
!                 4: K1 = KGRPNT(ix,iy+1), K2 = KGRPNT(ix,iy),
!                    K3 = KGRPNT(ix+1,iy)
!                 ---------------------------------------------------
!                 If K1>1 and K2>1 and K3>1
!                 Then Det = (xpg(K3)-xpg(K1))*(ypg(K2)-ypg(K1)) -
!                            (ypg(K3)-ypg(K1))*(xpg(K2)-xpg(K1))
!                      If FIRST
!                      Then Make FIRST = False
!                           If Det>0
!                           Then Make CVleft = False
!                           Else Make CVleft = True
!                      ----------------------------------------------
!                      If ((CVleft and Det<0) or (not CVleft and Det>0))
!                      Then Write error message with IX, IY, ISIDE
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!****************************************************************


   LOGICAL  FIRST
   INTEGER, SAVE :: IENT = 0
   INTEGER ICON, IIX, IIY, ISIDE, IX, IX1, IX2, IX3
   INTEGER IY, IY1, IY2, IY3, K1, K2, K3
   REAL DET, XC1, XC2, XC3, YC1, YC2, YC3

   IF (LTRACE) CALL STRACE (IENT,'CVCHEK')

!     test output

   IF (ITEST .GE. 150 .OR. INTES .GE. 30) THEN
      WRITE(PRINTF,"(/,' ... Subroutine CVCHEK...', /,2X,'POINT( IX, IY), INDEX, COORDX, COORDY')")
      ICON = 0
      do IIY = 1, MYC
         do IIX = 1, MXC
            ICON = ICON + 1
            WRITE(PRINTF,"(4X,I5,1X,I5,3X,I4,5X,F10.2,4X,F10.2)")IIX-1,IIY-1,KGRPNT(IIX,IIY),&
            &XCGRID(IIX,IIY)+XOFFS, YCGRID(IIX,IIY)+YOFFS
         end do
      end do
   ENDIF

   FIRST = .TRUE.

   do IX = 1,MXC-1
      do IY = 1,MYC-1
         do ISIDE = 1,4
            IF (ISIDE .EQ. 1) THEN
               IX1 = IX
               IY1 = IY
               IX2 = IX+1
               IY2 = IY
               IX3 = IX+1
               IY3 = IY+1
            ELSE IF (ISIDE .EQ. 2) THEN
               IX1 = IX+1
               IY1 = IY
               IX2 = IX+1
               IY2 = IY+1
               IX3 = IX
               IY3 = IY+1
            ELSE IF (ISIDE .EQ. 3) THEN
               IX1 = IX+1
               IY1 = IY+1
               IX2 = IX
               IY2 = IY+1
               IX3 = IX
               IY3 = IY
            ELSE IF (ISIDE .EQ. 4) THEN
               IX1 = IX
               IY1 = IY+1
               IX2 = IX
               IY2 = IY
               IX3 = IX+1
               IY3 = IY
            ENDIF
            K1  = KGRPNT(IX1,IY1)
            XC1 = XCGRID(IX1,IY1)
            YC1 = YCGRID(IX1,IY1)
            K2  = KGRPNT(IX2,IY2)
            XC2 = XCGRID(IX2,IY2)
            YC2 = YCGRID(IX2,IY2)
            K3  = KGRPNT(IX3,IY3)
            XC3 = XCGRID(IX3,IY3)
            YC3 = YCGRID(IX3,IY3)
            DET   = 0.
            IF (K1 .GE. 2 .AND. K2 .GE. 2 .AND. K3 .GE. 2) THEN
               DET = ((XC3 - XC1) * (YC2 - YC1)) -&
               &((YC3 - YC1) * (XC2 - XC1))
               IF (DET .EQ. 0.) THEN
!               three grid points on one line
                  CALL MSGERR (2,'3 comp. grid points on one line')
                  WRITE (PRINTF, "(3(1X, 2I3, 2(1X, F14.4)))")&
                  &IX1-1, IY1-1, XC1+XOFFS, YC1+YOFFS,&
                  &IX2-1, IY2-1, XC2+XOFFS, YC2+YOFFS,&
                  &IX3-1, IY3-1, XC3+XOFFS, YC3+YOFFS
               ENDIF

               IF (FIRST) THEN
                  FIRST = .FALSE.
                  IF (DET .GT. 0.) THEN
                     CVLEFT = .FALSE.
                  ELSE
                     CVLEFT = .TRUE.
                  ENDIF
               ENDIF
               IF (     (      CVLEFT .AND. DET .GT. 0.)&
               &.OR. (.NOT. CVLEFT .AND. DET .LT. 0.)) THEN
!               crossing grid lines in a mesh
                  CALL MSGERR (2,'Grid angle <0 or >180 degrees')
                  WRITE (PRINTF, "(3(1X, 2I3, 2(1X, F14.4)))")&
                  &IX1-1, IY1-1, XC1+XOFFS, YC1+YOFFS,&
                  &IX2-1, IY2-1, XC2+XOFFS, YC2+YOFFS,&
                  &IX3-1, IY3-1, XC3+XOFFS, YC3+YOFFS
               ENDIF
            ENDIF
         end do
      end do
   end do
   RETURN
!     *** end of subroutine CVCHEK ***
end subroutine CVCHEK
!************************************************************************
!                                                                      *
SUBROUTINE CVMESH (XP, YP, XC, YC, KGRPNT, XCGRID ,YCGRID, KGRBND)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid
   USE swan_test_output


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
!     30.72: IJsbrand Haagsma
!     40.00, 40.13: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.21, Jun. 96: New for curvilinear version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, May  98: procedure for points outside grid accelerated
!     40.00, Feb  99: procedure extended for 1D case
!                     XOFFS and YOFFS added in write statements
!     40.02, Mar. 00: Fixed bug that placed dry testpoints outside computational grid
!     40.13, Mar. 01: message "CVMESH 2nd attempt .." suppressed
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Nov. 04: search for boundary points improved
!
!  2. Purpose
!
!     procedure to find location in curvilinear grid for a point
!     given in problem coordinates
!
!  3. Method
!
!     First attempt: use Newton-Raphson method to find XC and YC
!     (Note: in the program XC and YC indicate the mesh and position in
!     the mesh) in a few steps; this may be most efficient if a series of
!     points is processed, because the previous point provides a good
!     first estimate.
!     This procedure may fail if the number of iterations is larger than
!     a previously set limit (default=5).
!
!     If the first attempt fails then determine whether the points (XP,YP)
!     is inside the mesh. If so, then the Newton-Raphson procedure is used
!     again with the pivoting point like first guess. Otherwise, scan the
!     boundaries whether the point is on the boundaries. If this fails,
!     may be concluded that the point (XP,YP) is outside the grid.
!
!  4. Argument variables
!
!     XCGRID  input  Coordinates of computational grid in x-direction
!     YCGRID  input  Coordinates of computational grid in y-direction
!     XP, YP  input  a point given in problem coordinates
!     XC, YC  outp   same point in computational grid coordinates

   REAL     XCGRID(MXC,MYC),    YCGRID(MXC,MYC)
   REAL     XP, YP, XC, YC

!     KGRPNT   input   array(MXC,MYC)  grid numbers
!                      if KGRPNT <= 1, point is not in comp. grid.
!     KGRBND   input   lists all boundary grid points consecutively

   INTEGER  KGRPNT(MXC,MYC), KGRBND(*)

!     Local variables
!
!     MXITNR   number of iterations in Newton-Raphson procedure
!     IX, IY   counter of computational grid point
!     K1       address of grid point
!     IXMIN    counter of grid point closest to (XP,YP)
!     IYMIN    counter of grid point closest to (XP,YP)
!     IBND     counter of boundary grid points

   INTEGER       :: IX, IY, K1, IXMIN, IYMIN, IBND
   INTEGER       :: ITER, IX1, IX1M, IX2, IX2M, IY1, IY1M, IY2, IY2M
   INTEGER       :: KORNER
   INTEGER, SAVE :: MXITNR = 0
   INTEGER, SAVE :: IENT = 0

!     INMESH   if True, point (XP,YP) is inside the computational grid
!     FINDXY   if True, Newton-Raphson procedure succeeded
!     ONBND    if True, given point is on boundary

   LOGICAL  FINDXY, ONBND

!     DISMIN   minimal distance found
!     XPC1     user coordinate of a computational grid point
!     YPC1     user coordinate of a computational grid point
!     XC0      grid coordinate of grid point closest to (XP,YP)
!     YC0      grid coordinate of grid point closest to (XP,YP)

   REAL       :: DISMIN
   REAL       :: XPC1, YPC1, XC0, YC0
   REAL       :: DISXY, RELDIS, RELLCM, RELLOC, SLEN2, XP1, XP2, YP1, YP2

!  5. SUBROUTINES CALLING
!
!     SINCMP
!
!  6. SUBROUTINES USED
!
!       NEWTON
!
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
!     --------------------------------------------------------------
!     Determine XC and YC from XP and YP using Newton-Raphson iteration
!     process
!     If (XC and YC were found) then
!       Procedure is ready; Return values of XC and YC
!       return
!     else
!     ------------------------------------------------------------------
!     For ix=1 to MXC-1 do
!         For iy=1 to MYC-1 do
!             Inmesh = True
!             For iside=1 to 4 do
!                 Case iside=
!                 1: K1 = KGRPNT(ix,iy), K2 = KGRPNT(ix+1,iy)
!                 2: K1 = KGRPNT(ix+1,iy), K2 = KGRPNT(ix+1,iy+1)
!                 3: K1 = KGRPNT(ix+1,iy+1), K2 = KGRPNT(ix,iy+1)
!                 4: K1 = KGRPNT(ix,iy+1), K2 = KGRPNT(ix,iy)
!                 ----------------------------------------------------------
!                 If K1>0 and K2>0
!                 Then Det = (xp-xpg(K1))*(ypg(K2)-ypg(K1)) -
!                            (yp-ypg(K1))*(xpg(K2)-xpg(K1))
!                      If ((CVleft and Det>0) or (not CVleft and Det<0))
!                      Then Make Inmesh = False
!                      Else  Inmesh = true and XC = IX and YC = IY
!                 Else Make Inmesh = False
!             --------------------------------------------------------
!             If Inmesh
!             Then Determine XC and YC using Newton-Raphson iteration
!                  process
!                  Procedure is ready; Return values of XC and YC
!     ------------------------------------------------------------------
!     No mesh is found: Make XC and YC = exception value
!     Return values of XC and YC
!     ------------------------------------------------------------------
!
!****************************************************************


   IF (LTRACE) CALL STRACE (IENT,'CVMESH')

   IF (ONED) THEN
      CALL NEWT1D  (XP, YP, XCGRID, YCGRID, KGRPNT,&
      &XC ,YC ,FINDXY)
      IF (.NOT.FINDXY) THEN
         XC = -99.
         YC = -99.
         IF (ITEST .GE. 150 .OR. INTES .GE. 20) THEN
            WRITE(PRINTF, "(' CVMESH: (XP,YP)=','(',F12.4,',',F12.4, ') is outside grid')") XP+XOFFS, YP+YOFFS
         ENDIF
      ENDIF
      RETURN
   ELSE
!       two-dimensional computation
      XC = 1.
      YC = 1.
!       --- First attempt, to find XC,YC with Newton-Raphson method
      MXITNR = 5
      CALL NEWTON  (XP, YP, XCGRID, YCGRID,&
      &MXITNR ,ITER, XC ,YC ,FINDXY)
      IF ((ITEST .GE. 150 .OR. INTES .GE. 20) .AND. FINDXY) THEN
         WRITE(PRINTF,"(' CVMESH: (XP,YP)=','(',F12.4,',',F12.4, '), (XC,YC)=','(',F9.2,',',F9.2,')')") XP+XOFFS ,YP+YOFFS ,XC ,YC
      ENDIF
      IF (.NOT. FINDXY .AND. INMESH (XP, YP, XCGRID ,YCGRID, KGRBND)) THEN
!         --- select grid point closest to (XP,YP)
         DISMIN = 1.E20
         do IX = 1,MXC
            do IY = 1,MYC
               K1  = KGRPNT(IX,IY)
               IF (K1.GT.1) THEN
                  XPC1 = XCGRID(IX,IY)
                  YPC1 = YCGRID(IX,IY)
                  DISXY = SQRT ((XP-XPC1)**2 + (YP-YPC1)**2)
                  IF (DISXY .LT. DISMIN) THEN
                     IXMIN  = IX
                     IYMIN  = IY
                     DISMIN = DISXY
                  ENDIF
               ENDIF
            end do
         end do
!         second attempt using closest grid point as first guess
         MXITNR = 20
         XC0 = REAL(IXMIN)
         YC0 = REAL(IYMIN)
!         ITEST condition changed from 20 to 120
         IF (ITEST.GE.120) WRITE (PRTEST, "(' CVMESH 2nd attempt, (XP,YP)=','(',F12.4,',',F12.4, '), (XC,YC)=','(',F9.2,',',F9.2,')')") XP+XOFFS ,YP+YOFFS ,&
         &XC0-1. ,YC0-1.
         DO KORNER = 1, 4
            IF (KORNER.EQ.1) THEN
               XC = XC0 + 0.2
               YC = YC0 + 0.2
            ELSE IF (KORNER.EQ.2) THEN
               XC = XC0 - 0.2
               YC = YC0 + 0.2
            ELSE IF (KORNER.EQ.3) THEN
               XC = XC0 - 0.2
               YC = YC0 - 0.2
            ELSE
               XC = XC0 + 0.2
               YC = YC0 - 0.2
            ENDIF
            CALL NEWTON  (XP, YP, XCGRID, YCGRID,&
            &MXITNR ,ITER, XC ,YC ,FINDXY)
            IF (FINDXY) THEN
               IF (ITEST .GE. 150 .OR. INTES .GE. 20) THEN
                  WRITE(PRINTF,"(' CVMESH: (XP,YP)=','(',F12.4,',',F12.4, '), (XC,YC)=','(',F9.2,',',F9.2,')')") XP+XOFFS ,YP+YOFFS ,XC ,YC
               ENDIF
               EXIT
            ENDIF
         ENDDO
         IF (.NOT. FINDXY .AND. ITER.GE.MXITNR) THEN
            WRITE (PRINTF, "(' search for point with location ', 2F12.4, ' fails in', I3, ' iterations')") XP+XOFFS, YP+YOFFS, MXITNR
         END IF
      ELSE IF (.NOT. FINDXY) THEN
!         scan boundary to see whether the point is close to the boundary
         DISMIN=99999.
         ONBND =.FALSE.
         IX1 = 0
         IY1 = 0
         IX2 = 0
         DO IBND = 1, NGRBND
            IF (IX2.NE.0) THEN
               IX1 = IX2
               IY1 = IY2
               XP1 = XP2
               YP1 = YP2
            END IF
            IX2 = KGRBND(2*IBND-1)
            IY2 = KGRBND(2*IBND)
            IF (IX2.NE.0 .AND. (ABS(IX2-IX1).GT.1 .OR.&
            &ABS(IY2-IY1).GT.1)) IX1 = 0
            IF (IX2.GT.0) THEN
               XP2 = XCGRID(IX2,IY2)
               YP2 = YCGRID(IX2,IY2)
               IF (IBND.GT.1 .AND. IX1.GT.0) THEN
!               --- determine relative distance from boundary segment
!                   with respect to the length of that segment
                  SLEN2  = (XP2-XP1)**2 + (YP2-YP1)**2
                  RELDIS = ABS((XP-XP1)*(YP2-YP1)-(YP-YP1)*(XP2-XP1)) /&
                  &SLEN2
                  IF (RELDIS.LT.0.01) THEN
!                 --- determine location on the boundary section
                     IF (RELDIS-DISMIN.LE.0.01) THEN
                        DISMIN = RELDIS
                        RELLOC = ((XP-XP1)*(XP2-XP1)+(YP-YP1)*(YP2-YP1)) /&
                        &SLEN2
                        IF (RELLOC.GE.-0.001 .AND. RELLOC.LE.1.001) THEN
                           RELLCM = RELLOC
                           IF (RELLCM.LT.0.01) RELLCM=0.
                           IF (RELLCM.GT.0.99) RELLCM=1.
                           IX1M  = IX1
                           IX2M  = IX2
                           IY1M  = IY1
                           IY2M  = IY2
                           ONBND = .TRUE.
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         IF (ONBND) THEN
            XC = FLOAT(IX1M) + RELLCM * FLOAT(IX2M-IX1M) - 1.
            YC = FLOAT(IY1M) + RELLCM * FLOAT(IY2M-IY1M) - 1.
            IF (ITEST .GE. 150 .OR. INTES .GE. 20) THEN
               WRITE(PRINTF, "(' CVMESH: (XP,YP)=','(',F12.4,',',F12.4, ') is on the boundary, (XC,YC)=(', F9.2,',',F9.2,')')") XP+XOFFS, YP+YOFFS, XC, YC
            ENDIF
         ELSE
            XC = -99.
            YC = -99.
            IF (ITEST .GE. 150 .OR. INTES .GE. 20) THEN
               WRITE(PRINTF, "(' CVMESH: (XP,YP)=','(',F12.4,',',F12.4, ') is outside grid')") XP+XOFFS, YP+YOFFS
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   IF (XC > -90. .AND. &
       KGRPNT(INT(XC+3.001)-2,INT(YC+3.001)-2).LE.1) THEN
      WRITE (PRINTF, "(' point with location ',2F12.4,' is not active')") XP+XOFFS, YP+YOFFS
      XC = -99.
      YC = -99.
   ENDIF
end subroutine CVMESH
!************************************************************************
!                                                                      *
LOGICAL FUNCTION INMESH (XP, YP, XCGRID ,YCGRID, KGRBND)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid
   USE M_PARALL


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
!     Nico Booij
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!
!  1. Updates
!
!       New function for curvilinear version (ver. 40.00). May '98
!       40.03, Dec 99: test output added; commons swcomm2 and ocpcomm4 added
!       40.41, Oct. 04: common blocks replaced by modules, include files removed
!       40.41, Nov. 04: search for points restricted to subdomain
!       40.51, Feb. 05: determining number of crossing points improved
!
!  2. Purpose
!
!       procedure to find whether a given location is
!       in the (curvilinear) computational grid
!
!  3. Method  suggested by Gerbrant van Vledder
!
!       draw a line from the point (XP,YP) in vertical direction
!       determine the number of crossings with the boundary of the
!       grid; if this number is even the point is outside
!
!  4. Argument variables
!
!
!     KGRBND   int  input   array containing boundary grid points

   INTEGER  KGRBND(*)

!     XP, YP    real, input   a point given in problem coordinates
!     XCGRID    real, input   array(IX,IY) x-coordinate of a grid point
!     YCGRID    real, input   array(IX,IY) y-coordinate of a grid point

   REAL     XCGRID(MXC,MYC) ,YCGRID(MXC,MYC),&
   &XP, YP

!  5. Parameter variables
!
!  6. Local variables
!
!     NUMCRS   number of crossings with boundary outline

   INTEGER  NUMCRS, IX1, IY1, IX2, IY2
   REAL     XP1, XP2, YP1, YP2, YPS, RELDIS, RELDO

!  8. Subroutines used
!
!  9. Subroutines calling
!
!       CVMESH
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!     --------------------------------------------------------------
!     numcros = 0
!     For all sections of the boundary do
!         determine coordinates of end points (XP1,YP1) and (XP2,YP2)
!         If (XP1<XP and XP2>XP) or (XP1>XP and XP2<XP)
!         then If not (YP1<YP and YP2<YP)
!                   if YPS>YP
!                   then numcros = numcros + 1
!     ---------------------------------------------------------------
!     If numcros is even
!     Then Inmesh = False
!     Else Inmesh = True
!     ---------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER  IBND
   CALL STRACE (IENT,'INMESH')

   IF (XP.LT.XCLMIN .OR. XP.GT.XCLMAX .OR.&
   &YP.LT.YCLMIN .OR. YP.GT.YCLMAX) THEN
      IF (ITEST.GE.70) WRITE (PRTEST, "(1X, 2F12.4, ' is outside region ', 4F12.4)") XP+XOFFS, YP+YOFFS,&
      &XCLMIN+XOFFS, XCLMAX+XOFFS, YCLMIN+YOFFS, YCLMAX+YOFFS
      INMESH = .FALSE.
      RETURN
   ENDIF

   IF (NGRBND.LE.0) THEN
      CALL MSGERR (3, 'grid outline not yet determined')
      RETURN
   ENDIF

   NUMCRS = 0
   IX1    = 0
   IY1    = 0
   IX2    = 0
   RELDIS = -1.

!     loop over the boundary of the computational grid
   DO IBND = 1, NGRBND+1
      IF (IX2.NE.0) THEN
         IX1 = IX2
         IY1 = IY2
         XP1 = XP2
         YP1 = YP2
      END IF
      IF (IBND.GT.NGRBND) THEN
         IX2 = KGRBND(2*1-1)
         IY2 = KGRBND(2*1)
      ELSE
         IX2 = KGRBND(2*IBND-1)
         IY2 = KGRBND(2*IBND)
      ENDIF
      IF (IX2.NE.0 .AND. (ABS(IX2-IX1).GT.1 .OR.&
      &ABS(IY2-IY1).GT.1)) IX1 = 0
      IF (IX2.GT.0) THEN
         XP2 = XCGRID(IX2,IY2)
         YP2 = YCGRID(IX2,IY2)
         IF (ITEST.GE.180) WRITE (PRTEST, "(' boundary point ', 2F12.4)") XP2+XOFFS,&
         &YP2+YOFFS
         IF (IBND.GT.1 .AND. IX1.GT.0) THEN
            IF (((XP1.GT.XP).AND.(XP2.LE.XP)).OR.&
            &((XP1.LE.XP).AND.(XP2.GT.XP))) THEN
               IF (YP1.GT.YP .OR. YP2.GT.YP) THEN
!               determine y-coordinate of crossing point
                  YPS = YP1 + (XP-XP1) * (YP2-YP1) / (XP2-XP1)
!               determine relative distance from boundary segment
!               with respect to the length of that segment
                  RELDO  = RELDIS
                  RELDIS = ABS(YP-YPS) / SQRT((XP2-XP1)**2 + (YP2-YP1)**2)
                  IF (YPS.GT.YP.AND.ABS(RELDIS-RELDO).GT.0.1) THEN
                     NUMCRS = NUMCRS + 1
                     IF (ITEST.GE.70) WRITE (PRTEST, "(' crossing ', I1, ' point ', 3F12.4)") NUMCRS,&
                     &XP+XOFFS, YP+YOFFS, YPS+YOFFS
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
!     point is inside the grid is number of crossings is odd
   IF (MOD(NUMCRS,2) .EQ. 1) THEN
      INMESH = .TRUE.
   ELSE
      INMESH = .FALSE.
   ENDIF
end function INMESH
!************************************************************************
!                                                                      *
SUBROUTINE NEWTON (XP, YP, XCGRID, YCGRID,&
&MXITNR, ITER, XC, YC, FIND)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid
   USE swan_test_output


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
!  0. Authors
!
!     30.72: IJsbrand Haagsma
!     30.80: Nico Booij
!     30.82: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.21, Jun. 96: New for curvilinear version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description of several variables
!     30.80, Oct. 98: computation of update of XC,YC modified to avoid
!                     division by 0
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Solve eqs. and find a point  (XC,YC) in a curvilinear grid (compt.
!     grid) for a given point (XP ,YP) in a cartesian grid (problem coord).
!
!  3. Method
!
!     In this subroutine the next equations are solved :
!
!                  @XP             @XP
!     XP(xc,yc) +  --- * @XC   +   --- * @YC  - XP(x,y) = 0
!                  @XC             @YC
!
!                  @YP             @YP
!     YP(xc,yc) +  --- * @XC   +    --- * @YC  - YP(x,y) = 0
!                  @XC             @YC
!
!     In the subroutine, next notation is used for the previous eqs.
!     XVC       + DXDXC * DXC   + DXDYC * DYC - XP  = 0.
!     YVC       + DYDXC * DXC   + DYDYC * DYC - YP  = 0.
!
!
!  4. Argument variables
!
! i   MXITNR: Maximum number of iterations

   INTEGER MXITNR, ITER

!   o XC    : X-coordinate in computational coordinates
! i   XCGRID: Coordinates of computational grid in x-direction
! i   XP    : X-coordinate in problem coordinates
!   o YC    : Y-coordinate in computational coordinates
! i   YCGRID: Coordinates of computational grid in y-direction
! i   YP    : Y-coordinate in problem coordinates

   REAL    XC, XCGRID(MXC,MYC), XP
   REAL    YC, YCGRID(MXC,MYC), YP

!   o FIND  : Whether XC and YC are found

   LOGICAL FIND

!  6. SUBROUTINES USED
!
!     STRACE
!
!
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
!       ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER I, I1, I2, J, J1, J2, K
   REAL DDEN, DXC, DXDXC, DXDYC, DXP, DYC, DYDXC, DYDYC, DYP
   REAL FI1, FI2, FJ1, FJ2, TOLDC, XVC, YVC
   IF (LTRACE) CALL STRACE (IENT,'NEWTON')

   DXC    = 1000.
   DYC    = 1000.
   TOLDC  = 0.001
   FIND   = .FALSE.

   IF (ITEST .GE. 200) THEN
      WRITE(PRINTF,*) ' Coordinates in subroutine NEWTON '
      DO J = 1, MYC
         DO I = 1, MXC
            WRITE(PRINTF,"(2(2X,I5),2(2X,E12.4))") I ,J ,XCGRID(I,J) ,YCGRID(I,J)
         ENDDO
      ENDDO
   ENDIF

   do K = 1 ,MXITNR
      ITER = K
      I1   = INT(XC)
      J1   = INT(YC)
      IF (I1 .EQ. MXC) I1 = I1 - 1
      IF (J1 .EQ. MYC) J1 = J1 - 1
      I2  = I1 + 1
      J2  = J1 + 1
      FJ1 = FLOAT(J1)
      FI1 = FLOAT(I1)
      FJ2 = FLOAT(J2)
      FI2 = FLOAT(I2)

      XVC   = (YC-FJ1)*((XC-FI1)*XCGRID(I2,J2)  +&
      &(FI2-XC)*XCGRID(I1,J2)) +&
      &(FJ2-YC)*((XC-FI1)*XCGRID(I2,J1)  +&
      &(FI2-XC)*XCGRID(I1,J1))
      YVC   = (YC-FJ1)*((XC-FI1)*YCGRID(I2,J2)  +&
      &(FI2-XC)*YCGRID(I1,J2)) +&
      &(FJ2-YC)*((XC-FI1)*YCGRID(I2,J1)  +&
      &(FI2-XC)*YCGRID(I1,J1))
      DXDXC = (YC -FJ1)*(XCGRID(I2,J2) - XCGRID(I1,J2)) +&
      &(FJ2-YC )*(XCGRID(I2,J1) - XCGRID(I1,J1))
      DXDYC = (XC -FI1)*(XCGRID(I2,J2) - XCGRID(I2,J1)) +&
      &(FI2-XC )*(XCGRID(I1,J2) - XCGRID(I1,J1))
      DYDXC = (YC -FJ1)*(YCGRID(I2,J2) - YCGRID(I1,J2)) +&
      &(FJ2-YC )*(YCGRID(I2,J1) - YCGRID(I1,J1))
      DYDYC = (XC -FI1)*(YCGRID(I2,J2) - YCGRID(I2,J1)) +&
      &(FI2-XC )*(YCGRID(I1,J2) - YCGRID(I1,J1))

      IF (ITEST .GE. 150)&
      &WRITE(PRINTF,"(' NEWTON iter=', I2, ' (XC,YC)=', 2(1X,F10.2),/, ' (XP,YP)=', 2(1X,F10.2), ' X,Y(XC,YC) = ', 2(1X,F10.2))") K, XC-1., YC-1., XP, YP, XVC, YVC
      IF (ITEST .GE. 180) WRITE(PRINTF,"(' NEWTON grid coord:', 8(1x, F10.0), / ' deriv=', 4(1X,F10.2))")&
      &XCGRID(I1,J1), XCGRID(I1,J2), XCGRID(I2,J1), XCGRID(I2,J2),&
      &YCGRID(I1,J1), YCGRID(I1,J2), YCGRID(I2,J1), YCGRID(I2,J2),&
      &DXDXC, DXDYC, DYDXC, DYDYC

!       *** the derivated terms of the eqs. are evaluated and  ***
!       *** the eqs. are solved                                ***
      DDEN = DXDXC*DYDYC - DYDXC*DXDYC
      DXP  = XP - XVC
      DYP  = YP - YVC
      IF ( DDEN.NE.0. ) THEN
         DXC = ( DYDYC*DXP - DXDYC*DYP) / DDEN
         DYC = (-DYDXC*DXP + DXDXC*DYP) / DDEN
      ENDIF

      XC = XC + DXC
      YC = YC + DYC

!       *** If the guess point (XC,YC) is outside of compt. ***
!       *** grid, put that point in the closest boundary    ***
      IF (XC .LT. 1. ) XC = 1.
      IF (YC .LT. 1. ) YC = 1.
      IF (XC .GT. MXC) XC = FLOAT(MXC)
      IF (YC .GT. MYC) YC = FLOAT(MYC)

      IF (ITEST .GE. 120 .OR. INTES .GE. 50 .OR. IOUTES .GE. 50)&
      &WRITE(PRINTF,"(' (DXC,DYC)=', 2(1X,F10.2), ' (XC,YC)=', 2(1X,F10.2))") DXC, DYC, XC-1., YC-1.

!       *** If the accuracy is reached stop the iteration,  ***
      IF (ABS(DXC) .LE. TOLDC .AND. ABS(DYC) .LE. TOLDC) THEN

         FIND = .TRUE.
         XC = XC -1.
         YC = YC -1.
         RETURN
      ENDIF

   end do
   RETURN
!     *** end of subroutine NEWTON ***
end subroutine NEWTON
!************************************************************************
!                                                                      *
SUBROUTINE NEWT1D (XP, YP, XCGRID, YCGRID, KGRPNT,&
&XC, YC, FIND)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid


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
!  0. Authors
!
!     40.00, 40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.00, Feb. 99: New (adaptation from subr NEWTON for 1D case)
!     40.13, Feb. 01: DX and DY renamed to DELX and DELY (DX and DY are
!                     common var.); error in expression for RS corrected
!                     PRINTF replaced by PRTEST in test output
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Finds broken coordinate XC for a given point XP in a rectilinear grid
!
!  3. Method
!
!     In this subroutine the step on the computational grid is selected
!     for which
!
!           (X-X1).(X2-X1)
!     0 <= --------------- <= 1
!          (X2-X1).(X2-X1)
!
!     where X, X1 and X2 are vectors; X corresponds to (Xp,Yp)
!     X1 and X2 are two neighbouring grid points
!
!  4. Argument variables
!
! i   KGRPNT: Grid adresses

   INTEGER KGRPNT(MXC,MYC)

!   o XC    : X-coordinate in computational coordinates
! i   XCGRID: Coordinates of computational grid in x-direction
! i   XP    : X-coordinate in problem coordinates
!   o YC    : Y-coordinate in computational coordinates
! i   YCGRID: Coordinates of computational grid in y-direction
! i   YP    : Y-coordinate in problem coordinates

   REAL    XC, XCGRID(MXC,MYC), XP
   REAL    YC, YCGRID(MXC,MYC), YP

!   o FIND  : Whether XC and YC are found

   LOGICAL FIND

!     Local variables:

   REAL :: DELX, DELY   ! grid line
   INTEGER, SAVE :: IENT = 0
   INTEGER :: I, IX
   REAL :: RS, X1, X2, Y1, Y2

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
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       ----------------------------------------------------------------
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'NEWT1D')

   IF (ITEST .GE. 120) THEN
      WRITE(PRTEST,*) ' Coordinates in subroutine NEWT1D '
      DO I = 1, MXC
         WRITE(PRTEST,"(2X,I5,2(2X,E12.4))") I, XCGRID(I,1)+XOFFS ,YCGRID(I,1)+YOFFS
      ENDDO
   ENDIF

   FIND = .FALSE.
   do IX = 2 ,MXC
      IF (KGRPNT(IX-1,1).GT.1) THEN
         X1 = XCGRID(IX-1,1)
         Y1 = YCGRID(IX-1,1)
      ELSE
         CYCLE
      ENDIF
      IF (KGRPNT(IX,1).GT.1) THEN
         X2 = XCGRID(IX,1)
         Y2 = YCGRID(IX,1)
      ELSE
         CYCLE
      ENDIF
!       both ends of the step are valid grid points
!       now verify whether projection of (Xp,Yp) is within the step
      DELX = X2 - X1
      DELY = Y2 - Y1
      RS = ((XP - X1) * DELX + (YP - Y1) * DELY) /&
      &(DELX * DELX + DELY * DELY)
      IF (RS.GE.0. .AND. RS.LE.1.) THEN
         FIND = .TRUE.
         XC = REAL(IX-2) + RS
         YC = 0.
         EXIT
      ENDIF
   end do
!     *** end of subroutine NEWT1D ***
end subroutine NEWT1D
!************************************************************************
!                                                                      *
SUBROUTINE EVALF (XC ,YC ,XVC ,YVC ,XCGRID ,YCGRID)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_computational_grid


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
!     30.72: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.21, Jun. 96: New for curvilinear version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Evaluate the coordinates (in problem coordinates) of point (XC,YC)
!     given in computational coordinates
!
!  3. Method
!
!     Bilinear interpolation
!
!  4. Argument variables
!
!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!       XC, YC      real, outp    point in computational grid coordinates
!       XVC, YCV    real, OUTP    same point  but in problem coordinates
!
!  6. SUBROUTINES USED
!
!       none
!
!  7. ERROR MESSAGES
!
!       ---
!
!  8. REMARKS
!
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER, SAVE :: IENT = 0
   INTEGER I, J
   REAL P1, P2, P3, P4, T, U, XC, XVC, YC, YVC
   IF (LTRACE) CALL STRACE (IENT,'EVALF')

   I  = INT(XC)
   J  = INT(YC)

!     *** If the guess point (XC,YC) is in the boundary   ***
!     *** where I = MXC or/and J = MYC the interpolation  ***
!     *** is done in the mesh with pivoting point         ***
!     *** (MXC-1, J) or/and (I,MYC-1)                     ***

   IF (I .EQ. MXC) I = I - 1
   IF (J .EQ. MYC) J = J - 1
   T = XC - FLOAT(I)
   U = YC - FLOAT(J)
!     *** For x-coord. ***
   P1 = XCGRID(I,J)
   P2 = XCGRID(I+1,J)
   P3 = XCGRID(I+1,J+1)
   P4 = XCGRID(I,J+1)
   XVC = (1.-T)*(1.-U)*P1+T*(1.-U)*P2+T*U*P3+(1.-T)*U*P4
!     *** For y-coord. ***
   P1 = YCGRID(I,J)
   P2 = YCGRID(I+1,J)
   P3 = YCGRID(I+1,J+1)
   P4 = YCGRID(I,J+1)
   YVC = (1.-T)*(1.-U)*P1+T*(1.-U)*P2+T*U*P3+(1.-T)*U*P4
   RETURN
!     *** end of subroutine EVALF ***
end subroutine EVALF

!************************************************************************
!                                                                      *
SUBROUTINE SWOBST (XCGRID, YCGRID, KGRPNT, CROSS)
   USE swan_geometry, ONLY: TCROSS
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_numerics
   USE swan_computational_grid
   USE M_OBSTA

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
!     30.70
!     30.72  IJsbrand Haagsma
!     30.74  IJsbrand Haagsma
!     40.04  Annette Kieftenburg
!     40.28  Annette Kieftenburg
!     40.31  Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.70, Feb. 98: check if neighbouring point is a true grid point
!                     loop over grid points moved from calling routine into this
!                     argument list changed
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.04, Nov. 99: IMPLICIT NONE added, header updated
!                   : Removed include files that are not used
!     40.28, Feb. 02: Adjustments for extended REFLECTION option
!     40.31, Oct. 03: changes w.r.t. obstacles
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Obtains all the data required to find obstacles and
!     use subroutine TCROSS to find them
!
!  3. Method
!
!  4. Argument variables
!
!     CROSS   output Array which contains 0's if there is no
!                    obstacle crossing
!                    if an obstacle is crossing between the
!                    central point and its neighbour CROSS is equal
!                    to the number of the obstacle
!     KGRPNT  input  Indirect addressing for computational grid points
!     XCGRID  input  Coordinates of computational grid in x-direction
!     YCGRID  input  Coordinates of computational grid in y-direction

   INTEGER KGRPNT(MXC,MYC), CROSS(2,MCGRD)
   REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)

!  5. Parameter variables
!
!  6. Local variables
!
!     ICC     index
!     ICGRD   index
!     IENT    number of entries of this subroutine
!     ILINK   indicates which link is analyzed: 1 -> neighbour in x
!                                               2 -> neighbour in y
!     IX      counter of gridpoints in x-direction
!     IY      counter of gridpoints in y-direction
!     JJ      counter for number of obstacles
!     JP      counter for number of corner points of obstacles
!     NUMCOR  number of corner points of obstacle
!     X1, Y1  user coordinates of one end of grid link
!     X2, Y2  user coordinates of other end of grid link
!     X3, Y3  user coordinates of one end of obstacle side
!     X4, Y4  user coordinates of other end of obstacle side

   INTEGER, SAVE :: IENT = 0
   INTEGER    ICC, ICGRD, ILINK, IX, IY, JJ, JP
   INTEGER    NUMCOR
   REAL       X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL    XONOBST
   TYPE(OBSTDAT), POINTER :: COBST

!  8. Subroutines used
!
!     TCROSS
!     STRACE


!  9. Subroutines calling
!
!     SWPREP
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!       ----------------------------------------------------------------
!       Read number of obstacles from array OBSTA
!       For every obstacle do
!           Read number of corners of the obstacle
!           For every corner of the obstacle do
!               For every grid point do
!                   call function TCROSS to search if there is crossing
!                   point
!                   between the line of two points of the stencil and the
!                   line of the corners of the obstacle.
!                   If there is crossing point then
!                   then CROSS(link,kcgrd) = number of the crossing obstacle
!                   else CROSS(link,kcgrd) = 0
!       ----------------------------------------------------------------
!
! 13. Source text
! ======================================================================
   IF (LTRACE) CALL STRACE (IENT,'SWOBST')

   IF (NUMOBS .GT. 0) THEN
!       NUMOBS is the number of obstacles ***
      COBST => FOBSTAC
      do JJ = 1, NUMOBS
!         number of corner points of the obstacle
         NUMCOR = COBST%NCRPTS
         IF (ITEST.GE. 120) THEN
            WRITE(PRINTF,"( ' Obstacle number : ', I4,' has ',I4,' corners')") JJ, NUMCOR
         ENDIF
!         *** X1 X2 X3 ETC. are the coordinates of point according ***
!         *** with the scheme in the subroutine TCROSS header      ***
         X3 = COBST%XCRP(1)
         Y3 = COBST%YCRP(1)
         IF (ITEST.GE. 120)  WRITE(PRINTF,"(' Corner number:', I4,' XP: ',E10.4,' YP: ',E11.4)") 1,X3,Y3
         do JP = 2, NUMCOR
            X4 = COBST%XCRP(JP)
            Y4 = COBST%YCRP(JP)
            IF (ITEST.GE. 120) WRITE(PRINTF,"(' Corner number:', I4,' XP: ',E10.4,' YP: ',E11.4)") JP,X4,Y4
            do IX = 1, MXC
               do IY = 1, MYC
                  ICC = KGRPNT(IX,IY)
                  IF (ICC .GT. 1) THEN
                     X1 = XCGRID(IX,IY)
                     Y1 = YCGRID(IX,IY)

!                 *** "ILINK" indicates which link is analyzed. Initial
!                 *** neighbour in x , second link with neighbouring in
                     do ILINK = 1, 2
                        IF (ILINK.EQ.1 .AND. IX.GT.1) THEN
                           X2    = XCGRID(IX-1,IY)
                           Y2    = YCGRID(IX-1,IY)
                           ICGRD = KGRPNT(IX-1,IY)
                        ELSE IF (ILINK.EQ.2 .AND. IY.GT.1) THEN
                           X2    = XCGRID(IX,IY-1)
                           Y2    = YCGRID(IX,IY-1)
                           ICGRD = KGRPNT(IX,IY-1)
                        ELSE
                           ICGRD = 0
                        ENDIF
                        IF (ICGRD.GT.1) THEN

!                     *** All links are analyzed in each point otherwise the   ***
!                     *** boundaries can be excluded

                           IF (TCROSS(X1, X2, X3, X4, Y1, Y2, Y3, Y4,&
                           &XONOBST)) THEN
                              CROSS(ILINK,ICC) = JJ
                           ENDIF
                        ENDIF
                     end do
                  ENDIF
               end do
            end do
            X3 = X4
            Y3 = Y4
         end do
         IF (.NOT.ASSOCIATED(COBST%NEXTOBST)) EXIT
         COBST => COBST%NEXTOBST
      end do
   ENDIF

   RETURN
! * end of subroutine SWOBST *
end subroutine SWOBST
!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *
SUBROUTINE SWTRCF (DEP2  , WLEV2 , CHS   ,&
&LINK  , OBREDF,&
&AC2   , REFLSO, KGRPNT, XCGRID,&
&YCGRID, CAX,    CAY   , RDX   , RDY , ANYBIN,&
&SPCSIG, SPCDIR, CGO   , KWAVE , HSS2, TSS2  ,&
&DSS2  , KCGRD, IXCGRD, IYCGRD )
   USE swan_geometry, ONLY: TCROSS
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_input_grids
   USE swan_stencil, ONLY: MICMAX
   USE swan_numerics
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE M_OBSTA
   USE M_PARALL
   USE SwanGriddata
   USE SwanCompdata
   USE SwanIEM, only: ntf, Ebig

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
!     30.70
!     40.03  Nico Booij
!     40.08  Erick Rogers
!     40.09  Annette Kieftenburg
!     40.13  Nico Booij
!     40.14  Annette Kieftenburg
!     40.18  Annette Kieftenburg
!     40.28  Annette Kieftenburg
!     40.30  Marcel Zijlema
!     40.31  Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.66: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.65: Marcel Zijlema
!     41.71: Gerbrant van Vledder
!     41.82: Dirk Rijnsdorp
!     41.85: Ad Reniers
!     41.93: Marcel Zijlema
!
!  1. Updates
!
!     30.70, Feb. 98: water level (WLEV2) replaced depth
!                     incident wave height introduced using argument
!                     CHS (sign. wave height in whole comput. grid)
!     40.03, Jul. 00: LINK1 and LINK2 in argumentlist replaced by LINK
!     40.09, Nov. 99: IMPLICIT NONE added, Method corrected
!                     Reflection option for obstacle added
!     40.14, Dec. 00: Reflection call corrected: reduced to neighbouring
!                     linepiece of obstacle (bug fix 40.11D)
!            Jan. 01: Constant waterlevel taken into account as well (bug fix 40.11E)
!     40.18, Apr. 01: Scattered reflection against obstacles added
!     40.28, Dec. 01: Frequency dependent reflection added
!     40.13, Aug. 02: subroutine restructured:
!                     loop in reflection procedure changed to avoid double
!                     reflection
!                     argument list of subr REFLECT revised
!                     argument SPCDIR added
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.08, Mar. 03: Dimensioning of RDX, RDX changed to be consistent
!                     with other subroutines
!     40.31, Oct. 03: changes w.r.t. obstacles
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.66, Mar. 07: extension with d'Angremond and Van der Meer transmission
!     40.80, Mar. 08: extension to unstructured grids
!     41.65, Jun. 16: extension frequency and direction dependent tranmission coefficients
!     41.71, Dec. 18: extension freeboard dependent transmission and reflection
!     41.82, Aug. 21: introduce FIG source term
!     41.85, May  19: implementation of IEM (surfbeat model)
!     41.93, May  22: radiated seaward FIG
!
!  2. Purpose
!
!      take the value of transmission coefficient given
!      by the user in case obstacle TRANSMISSION
!
!      or
!
!      compute the transmision coeficient in case obstacle DAM
!      based on Goda (1967) [from Seelig (1979)]
!      or d'Angremond and Van der Meer formula's (1996)
!
!      if reflections are switched on, calculate sourceterm in
!      subroutine REFLECT
!
!  3. Method
!
!     Calculate transmission coefficient based on Goda (1967)
!     from Seelig (1979)
!     Kt = 0.5*(1-sin {pi/(2*alpha)*(WATHIG/Hi +beta)})
!     where
!     Kt         transmission coefficient
!
!     alpha,beta coefficients dependent on structure of obstacle
!                and waves
!     WATHIG     = F = h-d is the freeboard of the dam, where h is the
!                crest level of the dam above the reference level and d
!                is the mean water level (relative to reference level)
!     Hi         incident (significant) wave height
!
!     If reflection are switched on and obstacle is not exactly on line
!     of two neighbouring gridpoints, calculate reflections
!
!  4. Argument variables
!
!     AC2      input     Action density array
!     ANYBIN   input     Set a particular bin TRUE or FALSE depending on  40.09
!                        SECTOR
!     CAX      input     Propagation velocity
!     CAY      input     Propagation velocity
!     CHS      input     Hs in all computational grid points
!     DEP2     input     Water depth in grid points
!     DSS2     input     sea-swell mean wave direction in all grid points 42.06
!     HSS2     input     sea-swell sig wave height in all grid points
!     KCGRD    input     Grid address of points of computational stencil
!     LINK     input     indicates whether link in stencil
!                        crosses an obstacle
!     OBREDF   output    Array of action density reduction coefficients
!                        (reduction at the obstacle)
!     REFLSO   inp/outp  contribution to the source term of action
!                        balance equation due to reflection
!     RDX,RDY  input     Array containing spatial derivative coefficients 40.09
!     TSS2     input     sea-swell mean wave period in all grid points
!     WLEV2    input     Water level in grid points

!  KGRPNT/XCGRID/YCGRID are dereferenced only on the OPTG /= 5 branches below;
!  unstructured callers omit them (see the KGRPNT history note in swancom2).
   INTEGER, OPTIONAL :: KGRPNT(MXC,MYC)
   INTEGER  LINK(2)
   INTEGER, INTENT(IN) :: KCGRD(MICMAX), IXCGRD(MICMAX), IYCGRD(MICMAX)
   REAL     CHS(MCGRD), OBREDF(MDC,MSC,2), WLEV2(MCGRD), DEP2(MCGRD)
   REAL     :: AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL     :: CAX(MDC,MSC,MICMAX), CAY(MDC,MSC,MICMAX)
!  RDX/RDY assumed-size: only RDX(1:2) is read (link_loop runs 1..2) and the
!  unstructured caller passes a 2-element array.
   REAL     :: REFLSO(MDC,MSC), RDX(*), RDY(*)
   REAL     :: SPCSIG(MSC), SPCDIR(MDC,6)
   REAL     :: CGO(MSC,MICMAX), KWAVE(MSC,MICMAX)
   REAL     :: HSS2(MCGRD), TSS2(MCGRD), DSS2(MCGRD)
   LOGICAL  :: ANYBIN(MDC,MSC)

!  5. Parameter variables
!
!  6. Local variables
!
!     ALOW     Lower limit for FVH
!     BK       crest width
!     BUPL     Upper limit for FVH
!     BVH      Bk/Hsin
!     FD1      Coeff. for freq. dep. reflection: vertical displacement
!     FD2      Coeff. for freq. dep. reflection: shape parameter
!     FD3      Coeff. for freq. dep. reflection: directional coefficient  40.28
!     FD4      Coeff. for freq. dep. reflection: bending point of freq.
!     FVH      WATHIG/Hsin
!     HGT      elevation of top of obstacle above reference level
!     HSIN     incoming significant wave height
!     ID       counter in directional space
!     IENT     number of entries of this subroutine
!     ILINK    indicates which link is analyzed: 1 -> neighbour in x
!                                                2 -> neighbour in y
!     IS       counter in frequency space
!     ITRAS    indicates kind of obstacle: 0 -> constant transm
!                                          1 -> dam, Goda
!                                          2 -> dam, d'Angremond and
!                                                    Van der Meer
!     JP       counter for number of corner points of obstacles
!     L0P      wave length in deep water
!     LREFDIFF indicates whether reflected energy should be
!              scattered (1) or not (0)
!     LREFL    if LREFL=0, no reflection; if LREFL=1, constant
!              reflection coeff.
!     LRFRD    Indicates whether frequency dependent reflection is
!              active (#0.) or not (=0.)
!     NMPO     link number
!     NUMCOR   number of corner points of obstacle
!     OBET     user defined coefficient (beta) in formulation of
!              Goda/Seelig (1967/1979)
!     OBHKT    transmission coefficient in terms of wave height
!     OGAM     user defined coefficient (alpha) in formulation of
!              Goda/Seelig (1967/1979)
!     POWN     user defined power of redistribution function
!     REFLCOEF reflection coefficient in terms of action density
!     REFLTST  used to test Refl^2+Transm^2 <=1
!     SLOPE    slope of obstacle
!     SQRTREF  dummy variable
!     TRCF     transmission coefficient in terms of action density
!              (user defined or calculated (in terms of waveheight))
!     X1, Y1   user coordinates of one end of grid link
!     X2, Y2   user coordinates of other end of grid link
!     X3, Y3   user coordinates of one end of obstacle side
!     X4, Y4   user coordinates of other end of obstacle side
!     XCGRID   Coordinates of computational grid in x-direction
!     XI0P     breaker parameter
!     XOBS     x-coordinate of obstacle point
!     XONOBST  Indicates whether computational point (X1,Y1) is on
!              obstacle
!     XV       x-coordinate of vertex of face
!     YCGRID   Coordinates of computational grid in y-direction
!     YOBS     y-coordinate of obstacle point
!     YV       y-coordinate of vertex of face
!     WATHIG   freeboard of the dam (= HGT-waterlevel)

   INTEGER, SAVE :: IENT = 0
   INTEGER    ID, ILINK, ITRAS, IS, JP, ICGRD, LREFL,&
   &NUMCOR, NMPO, ISIGM, IFIG
   INTEGER    LREFDIFF, LRFRD
   INTEGER    LFREE, LQUAY
   REAL       ALOW, BUPL, FVH, HGT, HSIN, OBET, OBHKT,&
   &SLOPE, BK, L0P, XI0P, BVH, EMAX, ETD, TP,&
   &FAC1, FAC2,&
   &POWN, OGAM, REFLCOEF,&
   &X1, X2, X3, X4, Y1, Y2, Y3, Y4, WATHIG
   REAL       TRCF(MSC,MDC)
   REAL       FD1, FD2, FD3, FD4
   REAL       GAMR, GAMT, FBR, FBT
   REAL       ALPHA, AIG, ACOEF, SIG, SFAC, FRQD, FIGSRC, HSS, TSS
   REAL       BNORM, SDET, X, Y
   REAL       ACOS, CDIR, CTOT, DSS, FIGS, MS, SSTH, TOUT
   REAL       SQRTREF
   LOGICAL    XONOBST
   LOGICAL :: REFLTST, CROSSING_FOUND
   REAL, OPTIONAL :: XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   INTEGER    ICC, JJ
   REAL    :: XOBS(2), XV(2), YOBS(2), YV(2)
!  (local LOGICAL declaration removed: SwanCrossObstacle is now a module function)
   TYPE(OBSTDAT), POINTER :: COBST

!  8. Subroutines used
!
!     DEGCNV           direction in Cartesian or nautical degrees
!     EQREAL           indicates whether two reals are equal or not
!     GAMMAF           the gamma function
!     MSGERR           writes error message
!     REFLECT          computes effect of reflection
!     TCROSS           searches for crossing point if exist


!  9. Subroutines calling
!
!     SWOMPU
!
! 10. Error messages
!
! 11. Remarks
!
!     Here the formulation of the transmission coefficients concerns the  40.09
!     ratio of action densities!
!
! 12. Structure
!
!     ------------------------------------------------------------------
!     For both links from grid point (X1,Y1) do
!         calculate transmission coefficients
!         assign values to OBREDF
!         If there is reflection
!         Then select obstacle side which crosses the grid link
!              calculate reflection source terms
!     ------------------------------------------------------------------
!
! 13. Source text
! ======================================================================

   IF (LTRACE) CALL STRACE (IENT,'SWTRCF')

   REFLTST = .TRUE.
   link_loop: do ILINK = 1 ,2
!       default transmission coefficient
      TRCF = 1.
      NMPO = LINK(ILINK)
      IF (NMPO .EQ. 0) THEN
         OBREDF(1:MDC,1:MSC,ILINK) = 1.
         CYCLE link_loop
      END IF
!       incoming wave height
      HSIN = CHS(KCGRD(ILINK+1))
      IF (HSIN.LT.0.1E-4) HSIN = 0.1E-4
      COBST => FOBSTAC
      DO JJ = 1, NMPO-1
         IF (.NOT.ASSOCIATED(COBST%NEXTOBST)) EXIT
         COBST => COBST%NEXTOBST
      END DO
!       in case of freeboard dependent transmission/reflection
      LFREE = COBST%FBTYP1
      LQUAY = COBST%FBTYP2
      IF ( LFREE.EQ.1 ) THEN
         HGT  = COBST%FBCOEF(1)
         GAMT = COBST%FBCOEF(2)
         GAMR = COBST%FBCOEF(3)
!          compute relative freeboard
         WATHIG = HGT - WLEV2(KCGRD(1)) - WLEV
         FVH    = WATHIG/HSIN
!          compute freeboard dependent transmission/reflection factors
!          NOTE: these factors only to be applied to constant coeffs
         FBT = 0.5 * ( 1. - TANH(2.*FVH/GAMT) )
         FBR = 0.5 * ( 1. + TANH(2.*FVH/GAMR) )
         IF (TESTFL) WRITE (PRTEST, "(' test FREEB ', 2X, 3I5, ' dam level=', F6.2, ' FVH=', F6.2, ' FBT=', F6.2, ' FBR=', F6.2)") IXCGRD(1)+MXF-2,&
         &IYCGRD(1)+MYF-2,&
         &ILINK, HGT, FVH, FBT, FBR
         IF ( LQUAY.EQ.1 ) THEN
            IF ( DEP2(KCGRD(ILINK+1)).LT.DEP2(KCGRD(1)) ) THEN
               FBT = 99999.
               FBR = 0.
            ENDIF
            IF (TESTFL .AND. ITEST.GE.140) WRITE(PRTEST, "(8X, I3, 4E12.4)")&
            &ILINK,DEP2(KCGRD(ILINK+1)),DEP2(KCGRD(1)),FBT,FBR
         ENDIF
      ELSE
         FBT = 1.
         FBR = 1.
      ENDIF
      ITRAS  = COBST%TRTYPE
      IF (ITRAS .EQ. 0) THEN
!       constant transmission coefficient
!         User defined transmission coefficient concerns ratio of
!         wave heights, so
         OBHKT = MIN(1.,FBT * COBST%TRCOEF(1))
         TRCF(1,1) = OBHKT * OBHKT
      ELSE IF (ITRAS .EQ. 1) THEN
!       transmission coefficient according to Goda and Seelig
         HGT    =  COBST%TRCOEF(1)
         OGAM   =  COBST%TRCOEF(2)
         OBET   =  COBST%TRCOEF(3)
!         level of dam above the water surface (freeboard)
         WATHIG =  HGT - WLEV2(KCGRD(1)) - WLEV

!         *** Here the transmission coeff. is that of Goda and Seelig ***
         FVH  = WATHIG/HSIN
         ALOW = -OBET-OGAM
         BUPL = OGAM-OBET

         IF (FVH.LT.ALOW) FVH = ALOW
         IF (FVH.GT.BUPL) FVH = BUPL
         OBHKT = 0.5*(1.0-SIN(PI*(FVH+OBET)/(2.0*OGAM)))
         IF (TESTFL) WRITE (PRTEST, "(' test SWTRCF ', 2X, 3I5, ' dam level=', F6.2, ' depth=', F6.2, ' Hs=', F6.2, ' transm=', F6.3)") IXCGRD(1)+MXF-2,&
         &IYCGRD(1)+MYF-2,&
         &ILINK, HGT, WATHIG, HSIN, OBHKT
         IF (TESTFL .AND. ITEST.GE.140) WRITE (PRTEST, "(8X, 5E12.4)")&
         &OGAM, OBET, ALOW, BUPL, FVH

!         Formulation of Goda/Seelig concerns ratio of waveheights.
!         Here we use action density, so
         TRCF(1,1) = OBHKT * OBHKT
      ELSE IF (ITRAS.EQ.2) THEN
!       d'Angremond and Van der Meer formulae (1996)
         HGT   = COBST%TRCOEF(1)
         SLOPE = COBST%TRCOEF(2)
         BK    = COBST%TRCOEF(3)

!         level of dam above the water surface
         WATHIG =  HGT - WLEV2(KCGRD(1)) - WLEV

!         compute peak frequency of incoming wave
         EMAX = 0.
         ISIGM = -1
         DO IS = 1, MSC
            ETD = 0.
            DO ID = 1, MDC
               ETD = ETD + SPCSIG(IS)*AC2(ID,IS,KCGRD(ILINK+1))*DDIR
            END DO
            IF (ETD.GT.EMAX) THEN
               EMAX  = ETD
               ISIGM = IS
            END IF
         END DO
         IF (ISIGM.LE.0) ISIGM=MSC
         TP=2.*PI/SPCSIG(ISIGM)

!         compute breaker parameter
         L0P  = MAX(1.E-8,1.5613*TP*TP)
         XI0P = TAN(SLOPE*PI/180.)/SQRT(HSIN/L0P)

!         compute transmission coefficient
         FVH = WATHIG/HSIN
         BVH = BK/HSIN
         IF (BVH.EQ.0.) THEN
            OBHKT= -0.40*FVH
            IF (OBHKT.LT.0.075) OBHKT = 0.075
            IF (OBHKT.GT.0.900) OBHKT = 0.9
         ELSE IF (BVH.LT.8.) THEN
            OBHKT= -0.40*FVH + 0.64*(BVH**(-0.31))*(1.-EXP(-0.50*XI0P))
            IF (OBHKT.LT.0.075) OBHKT = 0.075
            IF (OBHKT.GT.0.900) OBHKT = 0.9
         ELSE IF (BVH.GT.12.) THEN
            OBHKT= -0.35*FVH + 0.51*(BVH**(-0.65))*(1.-EXP(-0.41*XI0P))
            IF (OBHKT.GT.0.93-0.006*BVH) OBHKT = 0.93-0.006*BVH
            IF (OBHKT.LT.0.05          ) OBHKT = 0.05
         ELSE
!            linear interpolation
            FAC1 = -0.40*FVH + 0.64*( 8.**(-0.31))*(1.-EXP(-0.50*XI0P))
            IF (FAC1.LT.0.075) FAC1 = 0.075
            IF (FAC1.GT.0.900) FAC1 = 0.9
            FAC2 = -0.35*FVH + 0.51*(12.**(-0.65))*(1.-EXP(-0.41*XI0P))
            IF (FAC2.LT.0.050) FAC2 = 0.050
            IF (FAC2.GT.0.858) FAC2 = 0.858
            OBHKT = 3.*FAC1 - 2.*FAC2 + BVH*(FAC2 - FAC1)/4.
         END IF
         IF(OPTG.NE.5) THEN
            X1 = XCGRID(IXCGRD(1),IYCGRD(1))+XOFFS
            Y1 = YCGRID(IXCGRD(1),IYCGRD(1))+YOFFS
         ELSE
            X1 = xcugrd(KCGRD(1))+XOFFS
            Y1 = ycugrd(KCGRD(1))+YOFFS
         ENDIF
         WRITE (PRINTF, "(' Transmission: ', 2X, 2F12.4, I5, ' dam level=',F6.2, ' board=', F6.2, ' Hs=', F6.2, ' Tp=', F6.2, ' Xi0p=', F6.3, ' Kt=', F6.3)") X1, Y1,&
         &ILINK, HGT, WATHIG, HSIN, SQRT(L0P/1.5613), XI0P, OBHKT
         IF (TESTFL .AND. ITEST.GE.140) WRITE (PRTEST, "(8X, 5E12.4)")&
         &TAN(SLOPE*PI/180.), FVH, BVH, L0P, XI0P

!         Formulation of d'Angremond concerns ratio of waveheights
!         Here we use action density, so
         TRCF(1,1) = OBHKT * OBHKT
      ELSE IF (ITRAS .EQ. 11) THEN
!       frequency dependent transmission coefficients
!       user defined values concerns ratio of wave heights, so
         DO IS = 1, MSC
            OBHKT      = COBST%TRCF1D(IS)
            TRCF(IS,1) = OBHKT * OBHKT
         ENDDO
      ELSE IF (ITRAS .EQ. 12) THEN
!       frequency and direction dependent transmission coefficients
!       user defined values concerns ratio of wave heights, so
         DO ID = 1, MDC
            DO IS = 1, MSC
               OBHKT       = COBST%TRCF2D(ID,IS)
               TRCF(IS,ID) = OBHKT * OBHKT
            ENDDO
         ENDDO
      ENDIF

!       assign values to array OBREDF
      IF (ITRAS.EQ.11) THEN
         DO IS = 1, MSC
            DO ID = 1, MDC
               OBREDF(ID,IS,ILINK) = TRCF(IS,1)
            ENDDO
         ENDDO
      ELSE IF (ITRAS.EQ.12) THEN
         DO IS = 1, MSC
            DO ID = 1, MDC
               OBREDF(ID,IS,ILINK) = TRCF(IS,ID)
            ENDDO
         ENDDO
      ELSE
         DO IS = 1, MSC
            DO ID = 1, MDC
               OBREDF(ID,IS,ILINK) = TRCF(1,1)
            ENDDO
         ENDDO
      ENDIF

!       reflection and FIG energy
      LREFL = COBST%RFTYP1
      IFIG  = COBST%IGTYP
      IF ( LREFL.GT.0 .OR. IFIG.NE.0 ) THEN
         CROSSING_FOUND = .FALSE.

!          check crossing with obstacle
         IF (OPTG.NE.5) THEN
!             determine grid points (X1,Y1) and (X2,Y2) of link
            ICC   = KCGRD(1)
            ICGRD = 0
            IF ( ICC.GT.1 ) THEN
               X1 = XCGRID(IXCGRD(1),IYCGRD(1))
               Y1 = YCGRID(IXCGRD(1),IYCGRD(1))
               IF (KGRPNT(IXCGRD(ILINK+1),IYCGRD(ILINK+1)).GT.1) THEN
                  X2    = XCGRID(IXCGRD(ILINK+1),IYCGRD(ILINK+1))
                  Y2    = YCGRID(IXCGRD(ILINK+1),IYCGRD(ILINK+1))
                  ICGRD = KCGRD(ILINK+1)
               ENDIF
            ENDIF
            IF (ICGRD.EQ.0) CYCLE link_loop
!             select obstacle side crossing the grid link
            X3 = COBST%XCRP(1)
            Y3 = COBST%YCRP(1)
            NUMCOR = COBST%NCRPTS
            DO JP = 2, NUMCOR
               X4 = COBST%XCRP(JP)
               Y4 = COBST%YCRP(JP)
               IF (TCROSS(X1,X2,X3,X4,Y1,Y2,Y3,Y4,XONOBST)) THEN
                  CROSSING_FOUND = .TRUE.
                  EXIT
               END IF
               X3 = X4
               Y3 = Y4
            ENDDO
         ELSE
!             determine begin and end points of link (unstructured)
            X1 = xcugrd(KCGRD(1))
            Y1 = ycugrd(KCGRD(1))
            X2 = xcugrd(KCGRD(ILINK+1))
            Y2 = ycugrd(KCGRD(ILINK+1))
            XV(1) = X1
            YV(1) = Y1
            XV(2) = X2
            YV(2) = Y2
!             select obstacle side crossing the grid link
            X3 = COBST%XCRP(1)
            Y3 = COBST%YCRP(1)
            XOBS(1) = X3
            YOBS(1) = Y3
            DO JP = 2, COBST%NCRPTS
               X4 = COBST%XCRP(JP)
               Y4 = COBST%YCRP(JP)
               XOBS(2) = X4
               YOBS(2) = Y4
               IF ( SwanCrossObstacle( XV, YV, XOBS, YOBS ) ) THEN
                  CROSSING_FOUND = .TRUE.
                  EXIT
               END IF
               X3 = X4
               Y3 = Y4
               XOBS(1) = X3
               YOBS(1) = Y3
            ENDDO
         ENDIF
!          no crossing found, skip procedure
         IF (.NOT. CROSSING_FOUND) CYCLE link_loop

         IF ( LREFL.GT.0 ) THEN
!             reflections are activated
            SQRTREF  = FBR * COBST%RFCOEF(1)
            REFLCOEF = SQRTREF * SQRTREF
            LREFDIFF = COBST%RFTYP2
            POWN     = COBST%RFCOEF(2)
            FD1      = COBST%RFCOEF(3)
            FD2      = COBST%RFCOEF(4)
            FD3      = COBST%RFCOEF(5)
            FD4      = COBST%RFCOEF(6)
            LRFRD    = COBST%RFTYP3
            IF ( LSRFB .AND. ntf.GT.0 ) THEN
!                impose bound ig components at obstacle
               CALL REFLECT(Ebig, REFLSO, X1, Y1, X2, Y2,&
               &X3, Y3, X4, Y4, CAX,&
               &CAY, RDX, RDY, ILINK,&
               &REFLCOEF, LREFDIFF, POWN, ANYBIN,&
               &LRFRD, SPCSIG, SPCDIR, FD1, FD2, FD3, FD4,&
               &OBREDF, REFLTST, KCGRD(1), IXCGRD(1), IYCGRD(1))
            ELSE
               CALL REFLECT(AC2, REFLSO, X1, Y1, X2, Y2,&
               &X3, Y3, X4, Y4, CAX,&
               &CAY, RDX, RDY, ILINK,&
               &REFLCOEF, LREFDIFF, POWN, ANYBIN,&
               &LRFRD, SPCSIG, SPCDIR, FD1, FD2, FD3, FD4,&
               &OBREDF, REFLTST, KCGRD(1), IXCGRD(1), IYCGRD(1))
            ENDIF
         ENDIF

         IF ( IFIG.NE.0 ) THEN
!             compute seaward-radiated FIG source energy and
!             add to right hand side of matrix
            X = X4 - X3
            Y = Y4 - Y3
            IF ( EQREAL(X,0.) .AND. EQREAL(Y,0.) ) CYCLE link_loop
!             sign of inner product with normal finds position
!             of (X1,Y1) with respect to obstacle line
            SDET = Y*(X1-X3) - X*(Y1-Y3)
            IF ( .NOT. SDET.LT.0. ) CYCLE link_loop
!             direction of normal pointing outwards from the obstacle,
!             i.e. towards point (X1,Y1)
            BNORM = ATAN2(Y,X) + 0.5*PI
            ALPHA = COBST%IGCOEF(1)
            IF ( VARHSS ) THEN
               HSS = HSS2(KCGRD(1))
            ELSE
               HSS = COBST%IGCOEF(2)
            ENDIF
            IF ( VARTSS ) THEN
               TSS = TSS2(KCGRD(1))
            ELSE
               TSS = COBST%IGCOEF(3)
            ENDIF
            IF ( VARDSS ) THEN
               DSS = DSS2(KCGRD(1))
            ELSE
               DSS = COBST%IGCOEF(4)
            ENDIF
            MS = COBST%IGCOEF(5)
            IF ( .NOT. DSS.NE.-999. ) MS = 0.
!             DSS is incoming sea-swell direction
            DSS = DSS + 180.
            SSTH = PI * DEGCNV(DSS) / 180.
!             limit sea-swell direction to range [normal-90,normal+90]
            SSTH = MAX(SSTH,-0.5*PI+BNORM)
            SSTH = MIN(SSTH, 0.5*PI+BNORM)
!             compute outgoing sea-swell direction (specular reflection)  42.06
            TOUT = 2.*BNORM - SSTH
            IF (MS.LT.12.) THEN
               CTOT = GAMMAF(0.5*MS+1.)/(SQRT(PI)*GAMMAF(0.5*MS+0.5))
            ELSE
               CTOT = SQRT (0.5*MS/PI)/(1. - 0.25/MS)
            ENDIF
            AIG   = HSS * TSS**2.
            ACOEF = 1.2 * ALPHA**2. * (AIG/4.)**2.
            DO IS = 1, MSC
!                factor to convert back to N(sigma,theta)
               SIG = SPCSIG(IS)
!                shoaling factor
               SFAC   = (KWAVE(IS,1)*GRAV**2.)/(CGO(IS,1)*SPCSIG(IS))
!                frequency distribution for FIG source
               FRQD   = COBST%IGFRQD(IS)
!                FIG source contribution according to the
!                parametrization of Ardhuin et al. (2014)
               FIGS = ACOEF * SFAC * FRQD / SIG
               DO ID = 1, MDC
                  ACOS = ABS( COS( 0.5*(SPCDIR(ID,1) - TOUT) ) )
                  IF ( .NOT. MS.NE.0. ) THEN
                     CDIR = 1./PI
                  ELSE IF ( COS( BNORM - SPCDIR(ID,1) ).GT.0. ) THEN
                     CDIR = CTOT * MAX (ACOS**MS, 1.E-10)
                  ELSE
                     CDIR = 0.
                  ENDIF
                  FIGSRC = CDIR * FIGS
                  IF ( ANYBIN(ID,IS) ) THEN
!                      only FIG energy away from obstacle is
!                      taken into account
                     IF ( COS( BNORM - SPCDIR(ID,1) ).GT.0. )&
                     &REFLSO(ID,IS) = REFLSO(ID,IS) + FIGSRC *&
                     &(RDX(ILINK)*CAX(ID,IS,1) + RDY(ILINK)*CAY(ID,IS,1))
                  ENDIF
               ENDDO
            ENDDO
         ENDIF
      ENDIF

      IF (ITEST .GE. 120)  WRITE (PRTEST,"(' SWTRCF: Point=', 2I5, ' NMPO = ', I5, ' transm ', F8.3)")&
      &IXCGRD(1)-1, IYCGRD(1)-1, NMPO, TRCF(1,1)
   end do link_loop
   IF (.NOT.REFLTST) THEN
      CALL MSGERR(3,'Kt^2 + Kr^2 > 1 ')
      IF (ITEST.LT.50) THEN
         WRITE (PRTEST, "(' Kt^2 + Kr^2 > 1 in grid point:', 2I4)") IXCGRD(1)-1, IYCGRD(1)-1
      ENDIF
   ENDIF
   RETURN
!     * end of SUBROUTINE SWTRCF
end subroutine SWTRCF

!************************************************************************

SUBROUTINE REFLECT (AC2, REFLSO, X1, Y1, X2, Y2, X3, Y3,&
&X4, Y4, CAX, CAY, RDX, RDY,&
&ILINK, REF0, LREFDIFF, POWN, ANYBIN,&
&LRFRD, SPCSIG, SPCDIR, FD1, FD2, FD3, FD4,&
&OBREDF, REFLTST, IGP, IXCG, IYCG)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_stencil, ONLY: MICMAX
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output

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
!     40.09  Annette Kieftenburg
!     40.13  Nico Booij
!     40.18  Annette Kieftenburg
!     40.28  Annette Kieftenburg
!     40.38  Annette Kieftenburg
!     40.41: Marcel Zijlema
!     41.73: Ad Reniers
!
!  1. Updates
!
!     40.09, Nov. 99: Subroutine created
!     40.18, Apr. 01: Scattered reflection against obstacles added
!     40.28, Dec. 01: Frequency dependent reflection added
!     40.38, Feb. 02: Diffuse reflection against obstacles added
!     40.13, Sep. 02: Subroutine restructured
!                     assumptions changed: reflected energy can come
!                     from Th_norm-PI/2 to Th_norm+PI/2
!     40.08, Mar. 03: Dimensioning of RDX, RDX changed to be consistent
!                     with other subroutines
!     40.13, Nov. 03: test on refl + transm added
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.73, Apr. 20: bug fix reflection in case of 360->0
!
!  2. Purpose
!
!     Computation of REFLECTIONS near obstacles
!
!  3. Method
!
!     Determine the angle of the obstacle,
!     Determine the angles between which reflections should be taken
!     into account
!     Determine redistribution function
!     determine expression of reflection coefficient for frequency
!     dependency, if appropriate
!     Determine reflected action density (corrected for angle obstacle
!     and if option is on: redistribute energy)
!     Add reflected spectrum to contribution for the right hand side
!     of matrix equation
!
!  4. Modules used
!
!     --
!
!  5. Argument variables
!
!     AC2      inp  action density
!     ANYBIN   inp  Determines whether a bin fall within a sweep
!     CAX      inp  Propagation velocity in x-direction
!     CAY      inp  Propagation velocity in y-direction
!     FD1      inp  Coeff. freq. dep. reflection: vertical displacement
!     FD2      inp  Coeff. freq. dep. reflection: shape parameter
!     FD3      inp  Coeff. freq. dep. reflection: directional coefficient 40.28
!     FD4      inp  Coeff. freq. dep. reflection: bending point of freq.  40.28
!     ILINK    inp  Indicates which link is analyzed: 1 -> neighbour in
!                                                     2 -> neighbour in
!     LREFDIFF inp  Indicates whether reflected energy should be
!                   scattered (1) or not (0)
!     LRFRD    inp  Indicates whether frequency dependent reflection is
!                   active (#0.) or not (=0.)
!     OBREDF   inp  transmission coefficients
!     POWN     inp  User defined power of redistribution function
!     REF0     inp  reflection coefficient in terms of action density
!     REFLSO   i/o  contribution to the source term due to reflection
!     REFLTST  i/o  used to test Refl^2+Transm^2 <=1
!     RDX,RDY  inp  Array containing spatial derivative coefficients
!     SPCDIR(*,1)   spectral directions (radians)
!     SPCSIG   inp  Relative frequency (= 2*PI*Freq.)
!     X1, Y1   inp  Coordinates of computational grid point under
!                   consideration
!     X2, Y2   inp  Coordinates of computational grid point neighbour
!     X3, Y3   inp  User coordinates of one end of obstacle side
!     X4, Y4   inp  User coordinates of other end of obstacle side

   REAL       :: AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL       :: CAX(MDC,MSC,MICMAX), CAY(MDC,MSC,MICMAX)
   REAL       :: REFLSO(MDC,MSC), OBREDF(MDC,MSC,2)
   REAL       :: RDX(*), RDY(*)   ! only RDX(1:2) is read; see SWTRCF
   REAL       :: FD1, FD2, FD3, FD4, SPCSIG(MSC), SPCDIR(MDC,6)
   REAL       :: REF0
   REAL       :: X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL    :: ANYBIN(MDC,MSC)
   INTEGER    :: ILINK
   INTEGER, INTENT(IN) :: IGP, IXCG, IYCG
   REAL       :: POWN
   INTEGER    :: LREFDIFF, LRFRD
   LOGICAL    :: REFLTST

   INTENT (IN)     AC2, CAX, CAY, OBREDF,&
   &FD1, FD2, FD3, FD4, LRFRD,&
   &RDX, RDY, SPCSIG, SPCDIR, X1, X2,&
   &X3, X4, Y1, Y2, Y3, Y4, ANYBIN, ILINK,&
   &POWN, LREFDIFF
   INTENT (IN OUT) REF0, REFLSO

!  6. Parameter variables
!
!  7. Local variables

   REAL :: AC2REF     ! reflected action density of one spectral bin
   REAL :: BETA       ! local angle of obstacle
   REAL :: X, Y
   REAL, ALLOCATABLE :: PRDIF(:)   ! scattering filter
   REAL    :: TH_INC         ! direction of incident wave
   REAL    :: TH_NORM        ! direction of normal to obstacle
   REAL    :: TH_OUT         ! direction of outgoing wave
   REAL    :: IANG           ! angle divided by DTheta (DDIR)
   REAL    :: SUMRD          ! sum of PRDIF array
   REAL    :: W1, W2         ! interpolation coefficients

   INTEGER :: ID             ! counter of directions
   INTEGER :: IS             ! counter of frequencies
   INTEGER :: MAXIDR         ! width of scattering filter
   INTEGER :: IDR            ! relative directional counter
   INTEGER :: ID_I1, ID_I2   ! counters of incoming directions
   INTEGER :: IDA, IDB       ! counters of incoming directions
   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWTRCF
!
! 11. Remarks
!
!    -In case the obstacle cuts exactly through computational grid point, 40.09
!    -The length of the obstacle linepiece is assumed to be
!     'long enough' compared to grid resolution (> 0.5*sqrt(dx^2+dy^2))
!     (if this restriction is violated, the reflections due to an obsta-  40.09
!     cle of one straight line can be very different from a similar line  40.09
!     consisting of several pieces (because only the directions of the
!     spectrum that are directed towards the obstacle linepiece are
!     reflected).
!    -There should be only one intersection per computational gridcell.
!     Therefore it is better to avoid sharp edges in obstacles.
!
! 12. Structure
!
!     -----------------------------------------------------------------
!     Determine angle of obstacle, Beta
!     Determine angle of normal from (X1,Y1) to obstacle
!     If there is constant diffuse reflection
!     Then determine scattering distribution
!     -----------------------------------------------------------------
!     For all frequencies do
!         If amount of reflection varies with frequency
!         Then determine reflection coefficient
!         -------------------------------------------------------------
!         If there is diffuse reflection
!         Then if scattering varies with frequency
!              Then determine power of cos
!              --------------------------------------------------------
!              Determine distribution
!         -------------------------------------------------------------
!         For all active directions do
!             Determine specular incoming direction
!             For directions of scattering filter do
!                 multiply incoming action with scattering coefficient
!                 add this to array AC2REF
!     -----------------------------------------------------------------
!
!     Add reflected spectrum to right hand side of matrix equation
!
! 13. Source text

   CALL STRACE (IENT, 'REFLECT')

   IF ( LREFDIFF.EQ.0 ) THEN
      ALLOCATE (PRDIF(0:0))
      MAXIDR = 0
      PRDIF(0) = 1.
   ELSE
      MAXIDR = MDC/2
      ALLOCATE (PRDIF(0:MDC/2))
   ENDIF

!     determine angle of obstacle BETA

   X = X4 - X3
   Y = Y4 - Y3
   IF ( X.NE.0. .OR. Y.NE.0. ) THEN
      BETA = ATAN2(Y,X)
   ELSE
      CALL MSGERR (2, 'obstacle is a zero-dimensional point')
   END IF
!     determine direction of normal                         (4)
!     this is the normal from (X1,Y1)                        |
!     towards the obstacle                                   |
!     see sketch to establish sign                    (2)----+------(1)
!                                                            |
!                                                           (3)
   IF ( (X1-X3)*Y - (Y1-Y3)*X.GT.0. ) THEN
      TH_NORM = BETA + 0.5*PI
   ELSE
      TH_NORM = BETA - 0.5*PI
   ENDIF

!     prepare directional filter in case of diffuse reflection
   IF (LREFDIFF.EQ.1) THEN
      PRDIF(1:MAXIDR) = 0.
      PRDIF(0) = 1.
      SUMRD = 1.
      DO ID = 1, MAXIDR
         PRDIF(ID) = (COS(ID*DDIR))**POWN
         IF (PRDIF(ID) .GT. 0.01) THEN
            SUMRD = SUMRD + 2.*PRDIF(ID)
         ELSE
            MAXIDR = ID-1
            EXIT
         ENDIF
      ENDDO
      DO ID = 0, MAXIDR
         PRDIF(ID) = PRDIF(ID) / SUMRD
      ENDDO
      IF (TESTFL .AND. ITEST.GE.50) THEN
         WRITE (PRTEST, "(' power scattering filter:', F4.1, I3)") POWN, MAXIDR
         IF (ITEST.GE.130) WRITE (PRTEST, "(10 F7.3)")&
         &(PRDIF(IDR), IDR=0, MAXIDR)
      ENDIF
   ENDIF

   DO IS = 1, MSC
      IF (LRFRD.EQ.1) THEN
!         amount of reflection varies with wave frequency
         REF0 = FD1 +&
         &FD2/PI * ATAN2(PI*FD3*(SPCSIG(IS)-FD4),FD2)
         IF (REF0 > 1.) REF0 = 1.
         IF (REF0 < 0.) REF0 = 0.
!         >>> should REF0 not be squared? <<<
      ENDIF
!       check whether reflection + transmission <= 1
      DO ID = 1, MDC
         IF ((REF0 + OBREDF(ID,IS,ILINK)) .GT. 1.) THEN
            REFLTST = .FALSE.
            IF (ITEST.GE.50) THEN
               WRITE (PRTEST, "(' Refl+Transm>1 in ', 2I4, 2X, 3I3, 2X, 2F6.2)") IXCG-1, IYCG-1, ILINK,&
               &IS, ID, REF0, OBREDF(ID,IS,ILINK)
            ENDIF
         ENDIF
      ENDDO

      IF (LREFDIFF.EQ.2) THEN
!         spreading varies with frequency; not yet implemented
      ENDIF
      DO ID = 1, MDC
         IF (ANYBIN(ID,IS)) THEN
            AC2REF = 0.
            TH_OUT = SPCDIR(ID,1)
!           corresponding incident direction (assuming specular reflection)
            TH_INC = 2.*BETA-TH_OUT
            IF ( TH_INC.LT.0. ) TH_INC = TH_INC + 2.*PI
!           determine counter for which direction is TH_INC:
            IANG = MOD (TH_INC-SPCDIR(1,1), 2.*PI) / DDIR
            IF ( IANG.LT.0. ) IANG = IANG + REAL(MDC)
!           incident angle is between ID_I1 and ID_I2
            ID_I1 = 1 + INT (IANG)
            ID_I2 = ID_I1+1
            IF ( ID_I2.GT.MDC ) ID_I2 = ID_I2 - MDC
!           W1 and W2 are weighting coefficients for the above directions
!           by linear interpolation
            W2 = IANG + 1. - REAL(ID_I1)
            W1 = 1. - W2
            DO IDR = -MAXIDR, MAXIDR
               IDA = ID_I1 + IDR
               IDB = ID_I2 + IDR
               IF (FULCIR) THEN
                  IDA = 1+MOD(2*MDC+IDA-1,MDC)
!               only outgoing reflected waves, i.e. not towards obstacle
                  IF (COS(TH_NORM-SPCDIR(IDA,1)) .GT. 0.)&
                  &AC2REF = AC2REF +&
                  &REF0 * W1 * PRDIF(ABS(IDR)) * AC2(IDA,IS,IGP)
                  IDB = 1+MOD(2*MDC+IDB-1,MDC)
                  IF (COS(TH_NORM-SPCDIR(IDB,1)) .GT. 0.)&
                  &AC2REF = AC2REF +&
                  &REF0 * W2 * PRDIF(ABS(IDR)) * AC2(IDB,IS,IGP)
               ELSE
                  IF (IDA.GE.1 .AND. IDA.LE.MDC) THEN
                     IF (COS(TH_NORM-SPCDIR(IDA,1)) .GT. 0.)&
                     &AC2REF = AC2REF +&
                     &REF0 * W1 * PRDIF(ABS(IDR)) * AC2(IDA,IS,IGP)
                  ENDIF
                  IF (IDB.GE.1 .AND. IDB.LE.MDC) THEN
                     IF (COS(TH_NORM-SPCDIR(IDB,1)) .GT. 0.)&
                     &AC2REF = AC2REF +&
                     &REF0 * W2 * PRDIF(ABS(IDR)) * AC2(IDB,IS,IGP)
                  ENDIF
               ENDIF
            ENDDO
!           add reflected energy to right hand side of matrix
            REFLSO(ID,IS) = REFLSO(ID,IS) + AC2REF *&
            &(RDX(ILINK)*CAX(ID,IS,1) + RDY(ILINK)*CAY(ID,IS,1))
         END IF
      END DO
   END DO

   IF (LREFDIFF .GT. 0) DEALLOCATE (PRDIF)
   RETURN
!     End of subroutine REFLECT
end subroutine REFLECT

!************************************************************************
!                                                                      *
!*******************************************************************
!                                                                  *
!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *
SUBROUTINE HSOBND (AC2   ,SPCSIG,HSIBC ,KGRPNT)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE M_PARALL


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
!     30.72: IJsbrand Haagsma
!     32.01: Roeland Ris
!     30.70: Nico Booij
!     40.00: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     32.01, Sep. 97: new for SWAN
!     30.72, Jan. 98: Changed number of elements for HSI to MCGRD
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: structure scheme corrected
!     40.00, Mar. 98: integration method changed (as in SNEXTI)
!                     structure corrected
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Compare computed significant wave height with the value of
!     the significant wave height as predescribed by the user. If
!     the values differ more than e.g. 10 % give an error message
!     and the gridpoints where the error has been located
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     SPCSIG: input  Relative frequencies in computational domain in
!                    sigma-space

   REAL    SPCSIG(MSC)

!       REALS:
!       ------
!       AC2        action density
!       HSI        significant wave height at boundary (using SWAN
!                  resolution (has thus not to be equal to the WAVEC
!                  significant wave height )
!       ETOT       total energy in a gridpoint
!       DS         increment in frequency space
!       DDIR       increment in directional space
!       HSC        computed wave height after SWAN computation
!       EFTAIL     contribution of tail to spectrum
!
!       INTEGERS:
!       ---------
!       KGRPNT     values of grid indices
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       TRACE
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
!     ------------------------------------------------------------------
!     for all computational grid points do
!         if HSI is non-zero
!         then compute Hs from action density array
!              if relative difference is large than HSRERR
!              then write error message
!    -------------------------------------------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   REAL      AC2(MDC,MSC,MCGRD) ,HSIBC(MCGRD)

   REAL      ETOT, HSC, HSREL

   INTEGER   ID    ,IS     ,IX     ,IY    ,INDX

   LOGICAL, SAVE :: HSRR = .TRUE.

   INTEGER   KGRPNT(MXC,MYC)

   INTEGER, SAVE :: IENT = 0
   CALL STRACE (IENT, 'HSOBND')

!     *** initializing ***

   HSRR = .TRUE.

   DO IY = MYC, 1, -1
      DO IX = 1, MXC
         INDX = KGRPNT(IX,IY)
         IF ( HSIBC(INDX) .GT. 1.E-25 ) THEN
!           *** compute Hs for boundary point (without tail) ***
            ETOT  = 0.
            DO ID = 1, MDC
               DO IS = 1, MSC
                  ETOT = ETOT + SPCSIG(IS)**2 * AC2(ID,IS,INDX)
               ENDDO
            ENDDO
            IF (ETOT .GT. 1.E-8) THEN
               HSC = 4. * SQRT(ETOT*FRINTF*DDIR)
            ELSE
               HSC = 0.
            ENDIF
            HSREL = ABS(HSIBC(INDX) - HSC) / HSIBC(INDX)
            IF (HSREL .GT. HSRERR) THEN
               IF ( HSRR ) THEN
                  WRITE (PRINTF,*) ' ** WARNING : ',&
                  &'Differences in wave height at the boundary'
                  WRITE (PRINTF,"(' Relative difference between input and ', 'computation >= ', F6.2)") HSRERR
                  WRITE (PRINTF,*) '                        Hs[m]',&
                  &'      Hs[m]      Hs[-]'
                  WRITE (PRINTF,*) '    ix    iy  index   (input)',&
                  &' (computed) (relative)'
                  WRITE (PRINTF,*) ' ----------------------------',&
                  &'----------------------'
                  HSRR = .FALSE.
               ENDIF
               WRITE (PRINTF,'(2(1x,I5),I7,3(1x,F10.2))')&
               &IX+MXF-1, IY+MYF-1, INDX, HSIBC(INDX), HSC, HSREL
            ENDIF
         ENDIF
      ENDDO
   ENDDO
   WRITE(PRINTF,*)

   IF ( ITEST .GE. 150 ) THEN
      WRITE(PRINTF,*) 'Values of wave height at boundary (HSOBND)'
      WRITE(PRINTF,*) '------------------------------------------'
      DO IY = MYC, 1, -1
         WRITE (PRINTF,'(13F8.3)') ( HSIBC(KGRPNT(IX,IY)), IX=1 , MXC)
      ENDDO
   ENDIF

!     *** end of subroutine HSOBND ***

   RETURN
end subroutine HSOBND

!*****************************************************************
!                                                                *

!********************************************************************
!                                                                   *
!********************************************************************
!                                                                   *
!************************************************************************
!                                                                      *
!****************************************************************

SUBROUTINE SWACC(AC2, AC2OLD, ACNRMS, WINDOW, IGP)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_diagnostics_level

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(spectral_window_t) :: WINDOW


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
!     40.23, Sep. 02: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Determine some infinity norms meant for stop criterion
!
!  4. Argument variables
!
!     AC2         action density
!     AC2OLD      action density at previous iteration
!     ACNRMS      array containing infinity norms
!     IDCMIN      integer array containing minimum counter of directions
!     IDCMAX      integer array containing maximum counter of directions
!     ISSTOP      maximum frequency counter in this sweep

   INTEGER, INTENT(IN) :: IGP
   REAL    AC2(MDC,MSC,MCGRD), AC2OLD(MDC,MSC), ACNRMS(2)

!  6. Local variables
!
!     DIFFAC:     difference between AC2 and AC2OLD
!     ID    :     counter of direction
!     IDDUM :     uncorrected counter of direction
!     IENT  :     number of entries
!     IS    :     counter of frequency

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IDDUM, IS
   REAL    DIFFAC

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWOMPU (in SWANCOM1)
!
! 12. Structure
!
!     determine infinity norms |ac2 - ac2old| and |ac2|
!     in selected sweep
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWACC')

   DO IS = 1, WINDOW%ISSTOP
      DO IDDUM = WINDOW%IDCMIN(IS), WINDOW%IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

!           *** determine infinity norms |ac2 - ac2old| and |ac2|

         DIFFAC = ABS(AC2(ID,IS,IGP) - AC2OLD(ID,IS))
         IF (DIFFAC.GT.ACNRMS(1)) ACNRMS(1) = DIFFAC
         IF (ABS(AC2(ID,IS,IGP)).GT.ACNRMS(2))&
         &ACNRMS(2) = ABS(AC2(ID,IS,IGP))

      END DO
   END DO

   RETURN
end subroutine SWACC
!****************************************************************

SUBROUTINE MKPATH ( PATH, IERR )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE swan_diagnostics_level

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
!     41.95: Marcel Zijlema
!
!  1. Updates
!
!     41.95, Jul. 22: New subroutine
!
!  2. Purpose
!
!     Creates a directory on OS (e.g. Windows, Linux and macOS)
!
!  3. Method
!
!     Use of a Fortran 2008 standard EXECUTE_COMMAND_LINE
!
!  4. Argument variables
!
!     IERR  :     status error
!                 =0 : creating path successful
!                 /=0: creating path failed
!     PATH  :     string to pass path

   INTEGER          :: IERR
   CHARACTER(LEN=*) :: PATH

!  6. Local variables
!
!     CSTAT :     command status
!     CMSG  :     command error message
!     ESTAT :     exit status
!     IENT  :     number of entries
!     MSGSTR:     string to pass message

   INTEGER, SAVE :: IENT = 0
   INTEGER            :: CSTAT, ESTAT

   CHARACTER(LEN=100) :: CMSG
   CHARACTER(LEN=140) :: MSGSTR

! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'MKPATH')

   IERR = 0

   CALL EXECUTE_COMMAND_LINE('mkdir '//TRIM(PATH), EXITSTAT=ESTAT,&
   &CMDSTAT=CSTAT, CMDMSG=CMSG)
   IF (CSTAT.GT.0) THEN
      WRITE (MSGSTR,'(A)') 'Command execution failed with error '//&
      &TRIM(CMSG)
      CALL MSGERR( 1, TRIM(MSGSTR) )
      IERR = 1
   ELSE IF (CSTAT.LT.0) THEN
      CALL MSGERR( 2, ' Command execution not supported' )
      IERR = 2
   ELSE IF (ESTAT.NE.0) THEN
      WRITE (MSGSTR, '(A,I5)')&
      &'Error while creating path '//TRIM(PATH)//&
      &' - exit status number is ', ESTAT
      CALL MSGERR( 2, TRIM(MSGSTR) )
      IERR = 3
   END IF

   RETURN
end subroutine MKPATH

end module swan_services

! The timing backend and TXPBLA procedure below stay external on purpose;
! swan_service_interfaces supplies their checked interfaces.
!****************************************************************
!
SUBROUTINE SWTSTA (ITIMER)
!
!****************************************************************
!
   USE swan_time, ONLY: LASTTM, LISTTM, MXTIMR, NSECTM, TIMERS
   USE swan_diagnostics_level
   USE swan_io_units
!
   IMPLICIT NONE
!
!
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
!     40.23, Aug. 02: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Start timing
!
!  3. Method
!
!     Get cpu and wall-clock times and store
!
!  4. Argument variables
!
!     ITIMER      number of timer to be used
!
   INTEGER :: ITIMER
!
!  6. Local variables
!
!     C     :     clock count of processor
!     I     :     index in LISTTM, loop variable
!     IFOUND:     index in LISTTM, location of ITIMER
!     IFREE :     index in LISTTM, first free position
!     M     :     maximum clock count
!     R     :     number of clock counts per second
!     TIMER1:     current cpu-time used
!     TIMER2:     current wall-clock time used
!
   INTEGER          :: I, IFOUND, IFREE
   INTEGER          :: C, R, M
   REAL(KIND=KIND(0.0D0)) :: TIMER1, TIMER2
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     CPU_TIME         Returns real value from cpu-time clock
!     SYSTEM_CLOCK     Returns integer values from a real-time clock
!
!  9. Subroutines calling
!
!     SWMAIN, SWCOMP, SWOMPU
!
! 12. Structure
!
!     Get and store the cpu and wall-clock times
!
! 13. Source text
!

!
!     --- check whether a valid timer number is given
!
   IF (ITIMER.LE.0 .OR. ITIMER.GT.NSECTM) THEN
      WRITE(PRINTF,*) 'SWTSTA: ITIMER out of range: ',&
      &ITIMER, 1, NSECTM
      STOP
   END IF
!
!     --- check whether timing for ITIMER was started already,
!         also determine first free location in LISTTM
!
   IFOUND=0
   IFREE =0
   I     =0
   DO WHILE (I.LT.LASTTM .AND. (IFOUND.EQ.0 .OR. IFREE.EQ.0))
      I=I+1
      IF (LISTTM(I).EQ.ITIMER) THEN
         IFOUND=I
      END IF
      IF (IFREE.EQ.0 .AND. LISTTM(I).EQ.-1) THEN
         IFREE =I
      END IF
   END DO

   IF (IFOUND.EQ.0 .AND. IFREE.EQ.0 .AND. LASTTM.LT.MXTIMR) THEN
      LASTTM=LASTTM+1
      IFREE =LASTTM
   END IF
!
!     --- produce warning if found in the list
!
   IF (IFOUND.GT.0) THEN
      WRITE(PRINTF,*)&
      &'SWTSTA: warning: previous timing for section ',&
      &ITIMER,' not closed properly/will be ignored.'
   END IF
!
!     --- produce error if not found and no free position available
!
   IF (IFOUND.EQ.0 .AND. IFREE.EQ.0) THEN
      WRITE(PRINTF,*)&
      &'SWTSTA: maximum number of simultaneous timers',&
      &' exceeded:',MXTIMR
      STOP
   END IF
!
!     --- register ITIMER in appropriate location of LISTTM
!
   IF (IFOUND.EQ.0) THEN
      IFOUND=IFREE
   END IF
   LISTTM(IFOUND)=ITIMER
!
!     --- get current cpu/wall-clock time and store in TIMERS
!
   CALL CPU_TIME (TIMER1)
   CALL SYSTEM_CLOCK (C,R,M)
   TIMER2=DBLE(C)/DBLE(R)

   TIMERS(IFOUND,1)=TIMER1
   TIMERS(IFOUND,2)=TIMER2

   RETURN
end subroutine SWTSTA
!****************************************************************
!
SUBROUTINE SWTSTO (ITIMER)
!
!****************************************************************
!
   USE swan_time, ONLY: DCUMTM, LASTTM, LISTTM, NCUMTM, NSECTM, TIMERS
   USE swan_io_units
!
   IMPLICIT NONE
!
!
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
!     40.23, Aug. 02: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Stop timing
!
!  3. Method
!
!     Get cpu and wall-clock times and store
!
!  4. Argument variables
!
!     ITIMER      number of timer to be used
!
   INTEGER :: ITIMER
!
!  6. Local variables
!
!     C     :     clock count of processor
!     I     :     index in LISTTM, loop variable
!     IFOUND:     index in LISTTM, location of ITIMER
!     M     :     maximum clock count
!     R     :     number of clock counts per second
!     TIMER1:     current cpu-time used
!     TIMER2:     current wall-clock time used
!
   INTEGER          :: I, IFOUND
   INTEGER          :: C, R, M
   REAL(KIND=KIND(0.0D0)) :: TIMER1, TIMER2
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     CPU_TIME         Returns real value from cpu-time clock
!     SYSTEM_CLOCK     Returns integer values from a real-time clock
!
!  9. Subroutines calling
!
!     SWMAIN, SWCOMP, SWOMPU
!
! 12. Structure
!
!     Get and store the cpu and wall-clock times
!
! 13. Source text
!

!
!     --- check whether a valid timer number is given
!
   IF (ITIMER.LE.0 .OR. ITIMER.GT.NSECTM) THEN
      WRITE(PRINTF,*) 'SWTSTO: ITIMER out of range: ',&
      &ITIMER, 1, NSECTM
      STOP
   END IF
!
!     --- check whether timing for ITIMER was started already,
!         also determine first free location in LISTTM
!
   IFOUND=0
   I     =0
   DO WHILE (I.LT.LASTTM .AND. IFOUND.EQ.0)
      I=I+1
      IF (LISTTM(I).EQ.ITIMER) THEN
         IFOUND=I
      END IF
   END DO
!
!     --- produce error if not found
!
   IF (IFOUND.EQ.0) THEN
      WRITE(PRINTF,*)&
      &'SWTSTO: section ',ITIMER,' not found',&
      &' in list of active timings'
      STOP
   END IF
!
!     --- get current cpu/wall-clock time
!
   CALL CPU_TIME (TIMER1)
   CALL SYSTEM_CLOCK (C,R,M)
   TIMER2=DBLE(C)/DBLE(R)
!
!     --- calculate elapsed time since start of timing,
!         store in appropriate location in DCUMTM,
!         increment number of timings for current section
!
   DCUMTM(ITIMER,1)=DCUMTM(ITIMER,1)+(TIMER1-TIMERS(IFOUND,1))
   DCUMTM(ITIMER,2)=DCUMTM(ITIMER,2)+(TIMER2-TIMERS(IFOUND,2))
   NCUMTM(ITIMER)  =NCUMTM(ITIMER)+1
!
!     --- free appropriate location of LISTTM,
!         adjust last occupied position of LISTTM
!
   IF (IFOUND.GT.0) THEN
      LISTTM(IFOUND)=-1
   END IF
   DO WHILE (LASTTM.GT.1 .AND. LISTTM(LASTTM).EQ.-1)
      LASTTM=LASTTM-1
   END DO
   IF (LISTTM(LASTTM).EQ.-1) LASTTM=0

   RETURN
end subroutine SWTSTO
!****************************************************************
!
SUBROUTINE SWPRTI
   USE swan_build_config, ONLY: jacobi_sweep_enabled
   USE swan_service_interfaces, ONLY: STRACE
!
!****************************************************************
!
   USE swan_time, ONLY: DCUMTM, NCUMTM, NSECTM
   USE swan_diagnostics_level
   USE swan_io_units
   USE M_PARALL

   IMPLICIT NONE
!
!
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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     40.23, Aug. 02: New subroutine
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Print timings info
!
!  6. Local variables
!
!     IDEBUG:     level of timing output requested:
!                 0 - no output for detailed timings
!                 1 - aggregate output for detailed timings
!                 2 - complete output for all detailed timings
!     IENT  :     number of entries
!     J     :     loop counter
!     K     :     loop counter
!     MYPRC :     own process number
!     TABLE :     array for computing aggregate cpu- and wallclock-times
!
   INTEGER          :: J, K, MYPRC
   INTEGER, PARAMETER :: IDEBUG = 0
   INTEGER, SAVE :: IENT = 0
   REAL(KIND=KIND(0.0D0)) :: TABLE(33,2)
   CHARACTER(LEN=*), PARAMETER :: FMT110 = "(i3,1x,'#')"
   CHARACTER(LEN=*), PARAMETER :: FMT111 = "(i3,' # Details on timings of the simulation:')"
   CHARACTER(LEN=*), PARAMETER :: FMT112 = "(i3,1x,'#',26x,'cpu-time',1x,'wall-clock')"
   CHARACTER(LEN=*), PARAMETER :: FMT113 = "(i3,' # Splitting up calc. + comm. times:')"
   CHARACTER(LEN=*), PARAMETER :: FMT114 = "(i3,' # Overview source contributions:')"
   CHARACTER(LEN=*), PARAMETER :: FMT115 = "(i3,1x,'#',1x,a22,2f11.2)"
   CHARACTER(LEN=*), PARAMETER :: FMT120 = "(/,i3,' #    item     cpu-time    real time     count')"
   CHARACTER(LEN=*), PARAMETER :: FMT121 = "(i3,1x,'#',4x,i4,2f13.4,i10)"
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWMAIN (in SWANMAIN)
!
! 12. Structure
!
!     Compile table with overview of cpu/wall clock time used in
!     important parts of SWAN and write to PRINT file
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWPRTI')
!
   MYPRC = INODE
!
!     --- compile table with overview of cpu/wall clock time used in
!         important parts of SWAN and write to PRINT file
!
   IF ( ITEST.GE.1 .OR. IDEBUG.GE.1 ) THEN
!
!        --- initialise table to zero
!
      DO K = 1, 33
         DO J = 1, 2
            TABLE(K,J) = 0D0
         END DO
      END DO
!
!        --- compute times for basic blocks
!
      DO J = 1, 2
!
!           --- total run-time
!
         TABLE(1,J) = DCUMTM(1,J)
!
!           --- initialisation, reading, preparation:
!
         DO K = 2, 7
            TABLE(2,J) = TABLE(2,J) + DCUMTM(K,J)
         END DO
!
!           --- domain decomposition:
!
         TABLE(2,J) = TABLE(2,J) + DCUMTM(211,J)
         TABLE(2,J) = TABLE(2,J) + DCUMTM(212,J)
         IF (jacobi_sweep_enabled) &
            TABLE(2,J) = TABLE(2,J) + DCUMTM(215,J)
         TABLE(2,J) = TABLE(2,J) + DCUMTM(201,J)
!
!           --- total calculation including communication:
!
         TABLE(3,J) = TABLE(3,J) + DCUMTM(8,J)
!
!           --- output:
!
         TABLE(5,J) = TABLE(5,J) + DCUMTM(9,J)
!
!           --- exchanging data:
!
         TABLE(7,J) = TABLE(7,J) + DCUMTM(213,J)
!
!           --- solving system:
!
         TABLE(9,J) = TABLE(9,J) + DCUMTM(119,J)
         TABLE(9,J) = TABLE(9,J) + DCUMTM(120,J)
!
!           --- global reductions:
!
         TABLE(10,J) = TABLE(10,J) + DCUMTM(202,J)
!
!           --- collecting data:
!
         TABLE(11,J) = TABLE(11,J) + DCUMTM(214,J)
!
!           --- setup:
!
         TABLE(12,J) = TABLE(12,J) + DCUMTM(106,J)
!
!           --- propagation velocities:
!
         TABLE(14,J) = TABLE(14,J) + DCUMTM(111,J)
         TABLE(14,J) = TABLE(14,J) + DCUMTM(113,J)
         TABLE(14,J) = TABLE(14,J) + DCUMTM(114,J)
!
!           --- x-y advection:
!
         TABLE(15,J) = TABLE(15,J) + DCUMTM(140,J)
!
!           --- sigma advection:
!
         TABLE(16,J) = TABLE(16,J) + DCUMTM(141,J)
!
!           --- theta advection:
!
         TABLE(17,J) = TABLE(17,J) + DCUMTM(142,J)
!
!           --- wind:
!
         TABLE(18,J) = TABLE(18,J) + DCUMTM(132,J)
!
!           --- whitecapping:
!
         TABLE(19,J) = TABLE(19,J) + DCUMTM(133,J)
!
!           --- bottom friction:
!
         TABLE(20,J) = TABLE(20,J) + DCUMTM(130,J)
!
!           --- wave breaking:
!
         TABLE(21,J) = TABLE(21,J) + DCUMTM(131,J)
!
!           --- quadruplets:
!
         TABLE(22,J) = TABLE(22,J) + DCUMTM(135,J)
!
!           --- triads:
!
         TABLE(23,J) = TABLE(23,J) + DCUMTM(134,J)
!
!           --- limiter:
!
         TABLE(24,J) = TABLE(24,J) + DCUMTM(122,J)
!
!           --- rescaling:
!
         TABLE(25,J) = TABLE(25,J) + DCUMTM(121,J)
!
!           --- reflections:
!
         TABLE(26,J) = TABLE(26,J) + DCUMTM(136,J)
!
!           --- diffraction:
!
         TABLE(27,J) = TABLE(27,J) + DCUMTM(137,J)
!
!           --- fluid mud:
!
         TABLE(28,J) = TABLE(28,J) + DCUMTM(138,J)
!
!           --- vegetation:
!
         TABLE(29,J) = TABLE(29,J) + DCUMTM(139,J)
!
!           --- turbulence:
!
         TABLE(30,J) = TABLE(30,J) + DCUMTM(143,J)

!           --- sea ice:
!
         TABLE(31,J) = TABLE(31,J) + DCUMTM(144,J)

!           --- Bragg scattering:
!
         TABLE(32,J) = TABLE(32,J) + DCUMTM(145,J)

!           --- quasi-coherent scattering:
!
         TABLE(33,J) = TABLE(33,J) + DCUMTM(146,J)

      END DO
!
!        --- add up times for some basic blocks
!
      DO J = 1, 2
!
!           --- total calculation:
!
         TABLE(3,J) = TABLE(3,J) - TABLE( 7,J)
         TABLE(3,J) = TABLE(3,J) - TABLE(10,J)
         IF ( TABLE(3,J).LT.0D0 ) TABLE(3,J) = 0D0
!
!           --- total communication:
!                * exchanging data
!                * global reductions
!                * collecting data
!
         TABLE(4,J) = TABLE(4,J) + TABLE( 7,J)
         TABLE(4,J) = TABLE(4,J) + TABLE(10,J)
         TABLE(4,J) = TABLE(4,J) + TABLE(11,J)
!
!           --- total propagation:
!                * velocities and derivatives
!
         TABLE(6,J) = TABLE(6,J) + TABLE(14,J)
         TABLE(6,J) = TABLE(6,J) + TABLE(15,J)
         TABLE(6,J) = TABLE(6,J) + TABLE(16,J)
         TABLE(6,J) = TABLE(6,J) + TABLE(17,J)
!
!           --- sources:
!                * wind, whitecapping, friction, breaking,
!                * quadruplets, triads, limiter, rescaling,
!                * reflections
!
         DO K = 18, 26
            TABLE(8,J) = TABLE(8,J) + TABLE(K,J)
         END DO
!
!                * diffraction
!
         TABLE(8,J) = TABLE(8,J) + TABLE(27,J)
!
!                * fluid mud
!
         TABLE(8,J) = TABLE(8,J) + TABLE(28,J)
!
!                * vegetation
!
         TABLE(8,J) = TABLE(8,J) + TABLE(29,J)
!
!                * turbulence
!
         TABLE(8,J) = TABLE(8,J) + TABLE(30,J)
!
!                * sea ice
!
         TABLE(8,J) = TABLE(8,J) + TABLE(31,J)
!
!                * Bragg scattering
!
         TABLE(8,J) = TABLE(8,J) + TABLE(32,J)
!
!                * quasi-coherent scattering
!
         TABLE(8,J) = TABLE(8,J) + TABLE(33,J)
!
!           --- other computing:
!
         TABLE(13,J) = TABLE(13,J) + TABLE( 3,J)
         TABLE(13,J) = TABLE(13,J) - TABLE( 6,J)
         TABLE(13,J) = TABLE(13,J) - TABLE( 8,J)
         TABLE(13,J) = TABLE(13,J) - TABLE( 9,J)
         TABLE(13,J) = TABLE(13,J) - TABLE(12,J)
         IF ( TABLE(13,J).LT.0D0 ) TABLE(13,J) = 0D0

      END DO
!
!        --- print CPU-times used in important parts of SWAN
!
      WRITE(PRINTF,'(/)')
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT111) MYPRC
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT112) MYPRC
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT115) MYPRC,'total time:'       ,(TABLE(1,J),J=1,2)
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT115) MYPRC,'total pre-processing:',&
      &(TABLE(2,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'total calculation:',(TABLE(3,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'total communication:',&
      &(TABLE(4,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'total post-processing:',&
      &(TABLE(5,j),j=1,2)
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT113) MYPRC
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT115) MYPRC,'calc. propagation:',(TABLE(6,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'exchanging data:'  ,(TABLE(7,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'calc. sources:'    ,(TABLE(8,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'solving system:'   ,(TABLE(9,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'reductions:'      ,(TABLE(10,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'collecting data:' ,(TABLE(11,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'calc. setup:'     ,(TABLE(12,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'other computing:' ,(TABLE(13,j),j=1,2)
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT114) MYPRC
      WRITE(PRINTF,FMT110) MYPRC
      WRITE(PRINTF,FMT115) MYPRC,'prop. velocities:',(TABLE(14,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'x-y advection:'   ,(TABLE(15,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'sigma advection:' ,(TABLE(16,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'theta advection:' ,(TABLE(17,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'wind:'            ,(TABLE(18,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'whitecapping:'    ,(TABLE(19,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'bottom friction:' ,(TABLE(20,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'fluid mud:'       ,(TABLE(28,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'vegetation:'      ,(TABLE(29,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'turbulence:'      ,(TABLE(30,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'sea ice:'         ,(TABLE(31,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'wave breaking:'   ,(TABLE(21,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'quadruplets:'     ,(TABLE(22,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'triads:'          ,(TABLE(23,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'Bragg scattering:',(TABLE(32,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'QC scattering:'   ,(TABLE(33,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'limiter:'         ,(TABLE(24,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'rescaling:'       ,(TABLE(25,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'reflections:'     ,(TABLE(26,j),j=1,2)
      WRITE(PRINTF,FMT115) MYPRC,'diffraction:'     ,(TABLE(27,j),j=1,2)

   END IF

   IF ( IDEBUG.GE.2 ) THEN
      WRITE(PRINTF,FMT120) MYPRC
      DO J = 1, NSECTM
         IF (NCUMTM(J).GT.0)&
         &WRITE(PRINTF,FMT121) MYPRC,J,DCUMTM(J,1),DCUMTM(J,2),&
         &NCUMTM(J)
      END DO
   END IF


   RETURN
end subroutine SWPRTI
!****************************************************************

SUBROUTINE TXPBLA(TEXT,IF,IL)

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
!     determines the position of the first and the last non-blank
!     (or non-tabulator) character in the text-string
!
!  4. Argument variables
!
!     IF          position of the first non-blank character in TEXT
!     IL          position of the last non-blank character in TEXT
!     TEXT        text string

   INTEGER, INTENT(OUT)             :: IF, IL
   CHARACTER(LEN=*), INTENT(INOUT)  :: TEXT

!  6. Local variables
!
!     FOUND :     TEXT is found or not
!     ITABVL:     integer value of tabulator character
!     LENTXT:     length of TEXT

   INTEGER LENTXT, ITABVL
   LOGICAL FOUND

! 12. Structure
!
!     Trivial.
!
! 13. Source text
!
   ITABVL = 9
   LENTXT = LEN (TEXT)
   IF = 1
   FOUND = .FALSE.
   DO WHILE (IF .LE. LENTXT .AND. .NOT. FOUND)
      IF (.NOT. (TEXT(IF:IF) .EQ. ' ' .OR.&
      &ICHAR(TEXT(IF:IF)) .EQ. ITABVL)) THEN
         FOUND = .TRUE.
      ELSE
         IF = IF + 1
      ENDIF
   END DO
   IL = LENTXT + 1
   FOUND = .FALSE.
   DO WHILE (IL .GT. 1 .AND. .NOT. FOUND)
      IL = IL - 1
      IF (.NOT. (TEXT(IL:IL) .EQ. ' ' .OR.&
      &ICHAR(TEXT(IL:IL)) .EQ. ITABVL)) THEN
         FOUND = .TRUE.
      ENDIF
   END DO

   RETURN
end subroutine TXPBLA
!****************************************************************

!****************************************************************

!****************************************************************

!****************************************************************


!****************************************************************
!
