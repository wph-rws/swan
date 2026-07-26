MODULE swan_input_point_validation
   use swan_find_point, only: SwanFindPoint
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: SINUPT, SINBTG

CONTAINS

SUBROUTINE SINUPT (PSNAME, XP, YP, XCGRID, YCGRID, KGRPNT, KGRBND)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_services, ONLY: CVMESH
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
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
!     30.72: IJsbrand Haagsma
!     40.04: Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!      0.0 , Mar. 87: Heading added, IF..GOTO.. changed into IF..THEN..
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, Feb. 99: test skipped for irregular bottom grid
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Checking whether the point XP, YP (given in problem coordinates)
!     of the output pointset SNAME is located in the computational grid
!     and bottom grid or not. If not, a warning is generated.
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     KGRPNT: input  Adresses of the computational grid points
!     KGRBND: input

   INTEGER KGRPNT(MXC,MYC), KGRBND(*)

!     XCGRID: input  Coordinates of computational grid in x-direction
!     XP    : input  X-coordinate of the point (problem coordinates)
!     YCGRID: input  Coordinates of computational grid in y-direction
!     YP    : input  Y-coordinate of the point (problem coordinates)

   REAL    XP, YP
   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     PSNAME: input  Name of the output pointset (any type)

   CHARACTER(LEN=*) :: PSNAME

!  5. SUBROUTINES CALLING
!
!     SPRCON (SWAN/SWREAD)
!
!  6. SUBROUTINES USED
!
!     SINBTG, SINCMP (both SWAN/SER) and MSGERR (Ocean Pack)


!  7. ERROR MESSAGES
!
!     ---
!
!  8. REMARKS
!
!     ---
!
!  9. STRUCTURE
!
!     ----------------------------------------------------------------
!     If point (XP,YP) is not in the bottom grid (SINBTG = FALSE), then
!         Call MSGERR to generate a warning
!     If point (XP,YP) is not in the comp. grid (SINCMP = FALSE), then
!         Call MSGERR to generate a warning
!     ----------------------------------------------------------------
!
! 10. SOURCE TEXT


   INTEGER, SAVE :: IENT = 0
   CALL STRACE(IENT,'SINUPT')

   IF (.NOT. SINBTG (XP,YP) ) THEN
      CALL MSGERR(1,'(corner)point outside bottom grid')
      WRITE (PRINTF, "(' Set of output locations: ',A8, ' coordinates:', 2F12.2)") PSNAME, XP+XOFFS, YP+YOFFS
   ENDIF
   IF (.NOT.SINCMP (XP, YP, XCGRID, YCGRID, KGRPNT, KGRBND)) THEN
      CALL MSGERR(1,'(corner)point outside comp. grid')
      WRITE (PRINTF, "(' Set of output locations: ',A8, ' coordinates:', 2F12.2)") PSNAME, XP+XOFFS, YP+YOFFS
   ENDIF

   RETURN
!     end of subroutine SINUPT *
end subroutine SINUPT

LOGICAL FUNCTION SINBTG (XP, YP)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE SWCOMM2


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
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!      0.0 , Mar. 87: name of function changed from INBODP into SINBTG
!     32.02, Jan. 98: Introduced 1D-version
!     40.00, Feb. 99: 1D procedure simplified, tolerance introduced
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Checking whether a point given in problem coordinates is in the
!     bottom grid (SINBTG = true) or not (SINBTG = false).
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     XP      REAL   input    X-coordinate (problem grid) of the point
!     YP      REAL   input    Y-coordinate (problem grid) of the point
!
!  6. Local variables
!
!     XB      x-coordinate (of bottom grid)
!     YB      y-coordinate (of bottom grid)
!     XLENB   length of bottom grid in x-direction (of bottom grid)
!     YLENB   length of bottom grid in y-direction (of bottom grid)
!     BTOL    tolerance length

   REAL :: XP, YP
   REAL :: XB, YB, XLENB, YLENB, BTOL
   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SPRCON (SWAN/MAIN)
!     SINUPT (SWAN/SER)
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
!     ----------------------------------------------------------------
!     If the bottom grid is defined (DXB>0 and DYB>0), then
!         Compute coordinates XB,YB in the bottom grid
!         Give SINBTG initial value TRUE
!         If XB < 0, XB > X-length of grid, YB < 0 or YB > .. then
!            SINBTG is FALSE
!     ----------------------------------------------------------------
!
! 13. Source text

   CALL  STRACE (IENT,'SINBTG')

   SINBTG = .TRUE.
   IF ( IGTYPE(1).NE.1 ) RETURN

   XLENB = (MXG(1)-1)*DXG(1)
   YLENB = (MYG(1)-1)*DYG(1)
   BTOL  = 0.01 * (XLENB+YLENB)

!     ***** compute bottom grid coordinates from problem coordinates ****

   XB =  (XP-XPG(1))*COSPG(1) + (YP-YPG(1))*SINPG(1)
   YB = -(XP-XPG(1))*SINPG(1) + (YP-YPG(1))*COSPG(1)

!     ***** check location of point *****
   IF (XB .LT. -BTOL) SINBTG = .FALSE.
   IF (XB .GT. XLENB+BTOL) SINBTG = .FALSE.
   IF (YB .LT. -BTOL) SINBTG = .FALSE.
   IF (YB .GT. YLENB+BTOL) SINBTG = .FALSE.

   RETURN
!   * end of subroutine SINBTG *
end function SINBTG

LOGICAL FUNCTION SINCMP (XP, YP ,XCGRID ,YCGRID ,KGRPNT, KGRBND)
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_services, ONLY: CVMESH
!                                                                      *
!************************************************************************

   USE SWCOMM2
   USE SWCOMM3
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
!     32.02: Roeland Ris & Cor van der Schelde
!     30.60, 40.00: Nico Booij
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     00.00, Mar. 87: name changed from INREKP into SINCMP, heading added
!     30.60, Aug. 97: assignment of SINCMP moved
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     32.02, Jan. 98: Introduced 1D-version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, June 98: argument KGRBND added, call CVMESH modified
!            Febr 99: separate 1D code removed, margin introduced
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!
!  2. Purpose
!
!     Checking whether a point given in problem coordinates is in the
!     computational grid (SINCMP = true) or not (SINCMP = false).
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     KGRPNT  input  grid point addresses
!     KGRBND  input  describes computational grid boundary

   INTEGER KGRPNT(MXC,MYC), KGRBND(*)

!     XCGRID: input  Coordinates of computational grid in x-direction
!     XP      REAL   input    X-coordinate (problem grid) of the point
!     YCGRID: input  Coordinates of computational grid in y-direction
!     YP      REAL   input    Y-coordinate (problem grid) of the point

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)
   REAL    XP,     YP

!  6. Local variables
!
!     CTOL    tolerance value (margin around comput. grid)

   REAL :: CTOL, XC, YC
   INTEGER :: K
   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SINUPT
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
!     ----------------------------------------------------------------
!     Compute coordinates XC,YC in the computational grid
!     Give SINCMP initial value TRUE
!     If XC < 0, XC > XCLEN, YC < 0 or YC > YCLEN, then
!         SINCMP = FALSE
!     ----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE(IENT,'SINCMP')

!     *** Different procedure depending on grid type **
   IF (OPTG .EQ. 1) THEN

!       regular grid: compute comp. coordinates from problem coordinates

      XC   =  (XP-XPC)*COSPC+(YP-YPC)*SINPC
      YC   = -(XP-XPC)*SINPC+(YP-YPC)*COSPC
!       XC and YC are in m
!
!       ***** check for location *****
      SINCMP = .TRUE.
      CTOL   = 0.01 * (XCLEN+YCLEN)
      IF (XC .LT. -CTOL) SINCMP = .FALSE.
      IF (XC .GT. XCLEN+CTOL) SINCMP = .FALSE.
      IF (YC .LT. -CTOL) SINCMP = .FALSE.
      IF (YC .GT. YCLEN+CTOL) SINCMP = .FALSE.
   ELSE IF (OPTG .EQ. 3) THEN

!       curvilinear grid

      CALL CVMESH (XP, YP, XC, YC, KGRPNT, XCGRID ,YCGRID, KGRBND)
!       XC and YC are nondimensional; equivalent to grid index
!
!       ***** check for location *****
      SINCMP = .TRUE.
      IF (XC .LT. -0.01) SINCMP = .FALSE.
      IF (XC .GT. REAL(MXC-1)+0.01) SINCMP = .FALSE.
      IF (YC .LT. -0.01) SINCMP = .FALSE.
      IF (YC .GT. REAL(MYC-1)+0.01) SINCMP = .FALSE.
   ELSE IF (OPTG.EQ.5) THEN

!       unstructured grid

      SINCMP = .TRUE.
      CALL SwanFindPoint ( XP, YP, K )
      IF ( K.LT.0 ) SINCMP = .FALSE.

   ENDIF

!     --- check if output location is in global subdomain

   IF ( PARLL .AND. .NOT.SINCMP ) THEN
      IF ( XP.GE.XCGMIN .AND. XP.LE.XCGMAX .AND.&
      &YP.GE.YCGMIN .AND. YP.LE.YCGMAX ) SINCMP = .TRUE.
   END IF

   RETURN
!   * end of subroutine SINCMP *
end function SINCMP

END MODULE swan_input_point_validation
