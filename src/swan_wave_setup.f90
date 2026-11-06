!  The wave-induced set-up: SETUPP and SETUP2D.
!
!  Upstream keeps these two routines at the end of swancom1, with the rest of
!  the computation. They form a separate computation: once the wave field is
!  known, SETUPP turns the radiation stress into a change of the water level,
!  in a one-dimensional run directly and in a two-dimensional run by solving
!  a Poisson equation in SETUP2D. They are called only from SWCOMP, share no
!  data with the rest of swan_computation, and only SETUPP needs to be
!  visible outside this module. The routines are otherwise as they were in
!  swancom1.f90; doc/upstream-map.json records the move.
!
module swan_wave_setup
   implicit none(type, external)
   private
   public :: SETUPP
contains
!********************************************************************
!                                                                   *
SUBROUTINE SETUPP (KGRPNT, MSTPDA, SETPDA, AC2, DEP2, DEPSAV, &
   SETUP2, XCGRID, YCGRID, SPCSIG, SPCDIR, CONVERGED )
!                                                                   *
!********************************************************************

   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA
   USE swan_wave_physics, ONLY: KSCIP1
   USE swan_physics_selection
   USE swan_numerics
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid

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
!     it under the terms of the GNU General Public License as published by
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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     31.03: Annette Kieftenburg
!     31.04: Nico Booij
!     32.01: Roeland Ris
!     32.03: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     32.01, Sept 97: New Subroutine
!     32.03, Feb. 98: Comma added in FORMAT to prevent compilation error
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: transformation of radiation stress in 1D case
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     34.01, Feb. 99: Introducing STPNOW
!     30.82, July 99: Corrected argumentlist SETUPP and SETUP2D
!     30.82, July 99: Corrected argumentlist KSCIP1
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: this routine is reconsidered, cleaned up and moved to SWANCOM1
!
!  2. Purpose
!
!     computes the wave-induced forces and adds the set-up to the depth
!
!  3. Method
!
!     The wave-induced setup is calculated for the one-dimensional
!     mode of SWAN using the following equation:
!
!        d Sxx                d eta
!        ----- +  ( d + eta ) ----- = 0
!         d x                  d x
!
!     This equation is integrated using the forward Euler technique
!
!     For the two-dimensional case, a 2D Poisson equation in general coordinates
!     is solved by means of vertex-centered finite volume method
!
!  4. Argument variables
!
!     AC2       input    action density
!     CONVERGED output   false if the solver for the set-up did not
!                        converge (CSETUP in upstream, a module variable)
!     DEPSAV    input    depth following from original bottom and water level
!     DEP2      i/o      total depth including set-up
!                        on entry: includes previous estimate of set-up
!                        on exit : includes new estimate of set-up
!     KGRPNT    input    indirect addresses for grid points
!     MSTPDA    input    number of (aux.) data per grid point
!                        value is set at 23 in SWANCOM1
!     SETPDA    i/o      auxiliary data for computation of set-up
!                        1: x-comp of force, 2: y-comp of force,
!                        3: radiation stress component RSxx, 4: RSxy,
!                        5: RSyy
!                        SETPDA(*,6..MSTPDA) is used as work array
!     SETUP2    output   computed set-up
!     SPCDIR    input    (*,1); spectral directions (radians)             30.82
!                        (*,2); cosine of spectral directions             30.82
!                        (*,3); sine of spectral directions
!                        (*,4); cosine^2 of spectral directions           30.82
!                        (*,5); cosine*sine of spectral directions        30.82
!                        (*,6); sine^2 of spectral directions             30.82
!     SPCSIG    input    Relative frequencies in computational domain     30.72
!                        in sigma-space
!     XCGRID    input    Coordinates of computational grid in x-direction 30.72
!     YCGRID    input    Coordinates of computational grid in y-direction 30.72

   LOGICAL, INTENT(OUT) :: CONVERGED
   INTEGER MSTPDA, KGRPNT(MXC,MYC)

   REAL    AC2(MDC,MSC,MCGRD)
   REAL    DEP2(MCGRD)
   REAL    DEPSAV(MCGRD)
   REAL    SETPDA(MCGRD,MSTPDA)
   REAL    SETUP2(MCGRD)
   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)

!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     CG          group velocity
!     CHARS       array to pass character info to MSGERR
!     CK          CGO*KWAVE
!     DDET        determinant
!     DEPMAX      maximum depth
!     DEPLOC      local depth
!     DIX         di/dx
!     DIY         di/dy
!     DJX         dj/dx
!     DJY         dj/dy
!     DP1         depth in point i in 1-D case
!     DP2         depth in point i+1 in 1-D case
!     DS2         square of mesh length in x- or y-direction
!     DXI         dx/di
!     DXJ         dx/dj
!     DYI         dy/di
!     DYJ         dy/dj
!     ELOC        local energy
!     ETA1        setup in  point i in 1-D case
!     ETA2        setup in  point i+1 in 1-D case
!     ID          counter in directional space
!     IDXMAX      index of location with maximum depth
!     IENT        number of entries
!     IF1         first non-character in string1
!     IL1         last non-character in string1
!     INDX        address of current grid point (ix,iy)
!     INDXB       address of grid point (ix,iy-1)
!     INDXL       address of grid point (ix-1,iy)
!     INDXR       address of grid point (ix+1,iy)
!     INDXU       address of grid point (ix,iy+1)
!     IS          counter in frequency space
!     IX          counter in x-direction
!     IXLO        counter in x-direction for neighbouring grid point
!     IXUP        counter in x-direction for neighbouring grid point
!     IY          counter in y-direction
!     IYLO        counter in y-direction for neighbouring grid point
!     IYUP        counter in y-direction for neighbouring grid point
!     K           wavenumber
!     LINK        counter for neighbouring grid points
!     MSGSTR      string to pass message to call MSGERR
!     N           CGroup/CPhase
!     ND          derivative of N with respect to depth
!     NEIGHB      boolean variable indicating whether neighbouring point is wet
!     RRDI        1/number of steps in i-direction
!     RRDJ        1/number of steps in j-direction
!     RSXX        xx-component of the radiation stress
!     RSXXI       derivative of RSXX in i-direction
!     RSXXJ       derivative of RSXX in j-direction
!     RSXY        xy-component of the radiation stress
!     RSXYI       derivative of RSXY in i-direction
!     RSXYJ       derivative of RSXY in j-direction
!     RSYY        yy-component of the radiation stress
!     RSYYI       derivative of RSYY in i-direction
!     RSYYJ       derivative of RSYY in j-direction
!     S_UPCOR     total correction to setup (user defined and S_UPDP)
!     S_UPDP      setup at location with maximum depth, before correction
!     SIG         dummy variable for frequency
!     SXX1        radiation stress in  point i in 1-D case
!     SXX2        radiation stress in  point i+1 in 1-D case

   INTEGER, SAVE :: IENT = 0
   INTEGER  IDXMAX, INDXR, INDXU, INDXB, &
      ID, INDX, INDXL, IS, IX, &
      IXLO, IXUP, IY, IYLO, IYUP, LINK

   REAL     CK, DDET, DEPLOC, DEPMAX, DIX, DIY, DJX, DJY, &
      DP1, DP2, DS2, DXI, DXJ, DYI, DYJ, ELOC, ETA1, ETA2, &
      RRDI, RRDJ, RSXX,  RSXXI, RSXXJ, RSXY, &
      RSXYI, RSXYJ, RSYY, RSYYI, RSYYJ, &
      S_UPCOR, &
      S_UPDP, &
      SXX1  ,SXX2
   REAL     CG(1), K(1), N(1), ND(1), SIG(1)
   INTEGER      IF1, IL1
   CHARACTER(LEN=20) CHARS(1)
   CHARACTER(LEN=80) MSGSTR

   LOGICAL  NEIGHB

   CONVERGED = .TRUE.

!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     KSCIP1           Calculates KWAVE, CGO
!     MSGERR           Writes error message
!     SETUP2D          Computation of the change of waterlevel by waves,
!                      a 2D Poisson equation in general coordinates is solved
!     STRACE           Tracing routine for debugging
!     TXPBLA           Removes leading and trailing blanks in string      40.41
!
!  9. Subroutines calling
!
!     SWCOMP
!
! 10. Error messages
!
!     setup in dry point is unequal to zero
!
! 11. Remarks
!
! 12. Structure
!
!     ---------------------------------------------------------
!     For all grid points do
!         If depth > DEPMIN
!         Then Integrate over spectrum to compute RSxx, RSxy, RSyy
!     ---------------------------------------------------------
!     If one-dimensional mode of SWAN
!     Then Calculate Setup in all grid points
!     Else Call SETUP2 to compute setup in all grid points
!     ---------------------------------------------------------
!     S_UPDP is setup in deepest point
!     Add user defined correction to setup
!     For all grid points do
!         If dep2 > DEPMIN
!            SETUP2 := SETUP2 - S_UPCOR
!         If dep2 > DEPMIN
!            compute new value for DEP2
!     ---------------------------------------------------------
!     For all grid points do
!         If depth < DEPMIN
!         Then If water level + setup in neighbouring point above
!                   bottom level in current point
!              Then make depth equal to neighbouring water level
!                   + SETUP - bottom level in current point
!     ---------------------------------------------------------
!
! 13. Source text
!
!************************************************************************

   CALL STRACE (IENT, 'SETUPP')

   DEPMAX = 0.
   IDXMAX = 0

!     --- initializing SETPDA array

   SETPDA = 0.

   DO IY = 1, MYC
      DO IX = 1, MXC
         INDX = KGRPNT(IX,IY)
         IF (INDX.GT.1) THEN
            IF (DEP2(INDX).GT.DEPMIN) THEN

!             --- seek deepest point

               IF (DEPSAV(INDX).GT.DEPMAX) THEN
                  DEPMAX = DEPSAV(INDX)
                  IDXMAX = INDX
               ENDIF

!             --- compute radiation stress components RSXX, RSXY and RSYY

               RSXX = 0.
               RSXY = 0.
               RSYY = 0.
               DEPLOC = DEP2(INDX)
               DO IS = 1, MSC
                  SIG(1) = SPCSIG(IS)
                  CALL KSCIP1 (1,SIG,DEPLOC,K,CG,N,ND)
                  CK = CG(1) * K(1)
                  DO ID = 1, MDC
                     ELOC = SIG(1) * AC2(ID,IS,INDX)
!                                  -
!                                  |{cos(Theta)}^2         for i = 4      31.03
!                 SPCDIR(ID,i) is <| sin(Theta)cos(Theta)  for i = 5      31.03
!                                  |{sin(Theta)}^2         for i = 6      31.03
!                                  -

                     RSXX = RSXX + (CK*SPCDIR(ID,4)+CK - SIG(1)/2.) * ELOC
                     RSXY = RSXY + CK*SPCDIR(ID,5) * ELOC
                     RSYY = RSYY + (CK*SPCDIR(ID,6)+CK - SIG(1)/2.) * ELOC
                  ENDDO
               ENDDO

!             --- store radiation stress components in array SETPDA
!
!             DDIR   is width of directional band
!             FRINTF is frequency integration factor df/f

               IF (ONED) THEN
!               transform to computational direction
                  SETPDA(INDX,3) = DDIR * FRINTF * &
                     ((COSPC*RSXX + SINPC*RSXY) * COSPC + &
                     (COSPC*RSXY + SINPC*RSYY) * SINPC)
               ELSE
                  SETPDA(INDX,3) = RSXX * DDIR * FRINTF
                  SETPDA(INDX,4) = RSXY * DDIR * FRINTF
                  SETPDA(INDX,5) = RSYY * DDIR * FRINTF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDDO

   IF ( ONED ) THEN

!       *** compute on the basis of the radiation stresses the setup ***

      DO IY = 1, MYC
!          *** boundary condition ***
         SETUP2(KGRPNT(1,IY)) = 0.
         ETA2 = 0.
         DO IX = 1, MXC-1
            INDX  = KGRPNT(IX  ,IY)
            INDXR = KGRPNT(IX+1,IY)
            DP1   = DEP2(INDX )
            DP2   = DEP2(INDXR)
            IF ( INDX .GT.1 .AND. DP1.GT.DEPMIN .AND. &
               INDXR.GT.1 .AND. DP2.GT.DEPMIN ) THEN
               ETA1 = SETUP2(INDX)
               SXX1 = SETPDA(INDX ,3)
               SXX2 = SETPDA(INDXR,3)
               ETA2 = ETA1 + ( SXX1 - SXX2 ) / ( 0.5 * ( DP2 + DP1 ) )
               SETUP2(INDXR) = ETA2
            ELSE
               SETUP2(INDXR) = 0.
            END IF
         END DO
      END DO

   ELSE

!       --- compute forces by taking derivative of radiation stress

      DO IY = 1, MYC
         DO IX = 1, MXC
            INDX = KGRPNT(IX,IY)
            DEPLOC = DEP2(INDX)
            IF (INDX.GT.1 .AND. DEPLOC.GT.DEPMIN) THEN
               IF (IX.EQ.1) THEN
                  IXLO = 1
                  IXUP = 2
               ELSE IF (IX.EQ.MXC) THEN
                  IXLO = MXC-1
                  IXUP = MXC
               ELSE
                  IXLO = IX-1
                  IXUP = IX+1
               END IF
               IF (DEP2(KGRPNT(IXLO,IY)).LE.DEPMIN) IXLO = IX
               IF (DEP2(KGRPNT(IXUP,IY)).LE.DEPMIN) IXUP = IX
               INDXL = KGRPNT(IXLO,IY)
               INDXR = KGRPNT(IXUP,IY)
               IF (IXLO.EQ.IXUP) THEN
                  RRDI = 1.E-20
               ELSE
                  RRDI = 1. / REAL(IXUP-IXLO)
               END IF
               IF (IY.EQ.1) THEN
                  IYLO = 1
                  IYUP = 2
               ELSE IF (IY.EQ.MYC) THEN
                  IYLO = MYC-1
                  IYUP = MYC
               ELSE
                  IYLO = IY-1
                  IYUP = IY+1
               ENDIF
               IF (DEP2(KGRPNT(IX,IYLO)).LE.DEPMIN) IYLO = IY
               IF (DEP2(KGRPNT(IX,IYUP)).LE.DEPMIN) IYUP = IY
               INDXB = KGRPNT(IX,IYLO)
               INDXU = KGRPNT(IX,IYUP)
               IF (IYLO.EQ.IYUP) THEN
                  RRDJ = 1.E-20
               ELSE
                  RRDJ = 1. / REAL(IYUP-IYLO)
               END IF

!                --- determine (x,y) derivatives w.r.t. i and j

               DXI = RRDI * (XCGRID(IXUP,IY)-XCGRID(IXLO,IY))
               DYI = RRDI * (YCGRID(IXUP,IY)-YCGRID(IXLO,IY))
               DXJ = RRDJ * (XCGRID(IX,IYUP)-XCGRID(IX,IYLO))
               DYJ = RRDJ * (YCGRID(IX,IYUP)-YCGRID(IX,IYLO))

               RSXXI = RRDI * (SETPDA(INDXR,3)-SETPDA(INDXL,3))
               RSXXJ = RRDJ * (SETPDA(INDXU,3)-SETPDA(INDXB,3))
               RSXYI = RRDI * (SETPDA(INDXR,4)-SETPDA(INDXL,4))
               RSXYJ = RRDJ * (SETPDA(INDXU,4)-SETPDA(INDXB,4))
               RSYYI = RRDI * (SETPDA(INDXR,5)-SETPDA(INDXL,5))
               RSYYJ = RRDJ * (SETPDA(INDXU,5)-SETPDA(INDXB,5))

               IF (IXLO.EQ.IXUP.AND.IYLO.EQ.IYUP) THEN
!                point surrounded by dry points
                  DIX = 0.
                  DIY = 0.
                  DJX = 0.
                  DJY = 0.
               ELSE IF (IXLO.EQ.IXUP) THEN
!                no forces in i-direction
                  DS2 = DXJ**2 + DYJ**2
                  DIX = 0.
                  DIY = 0.
                  DJX = DXJ/DS2
                  DJY = DYJ/DS2
               ELSE IF (IYLO.EQ.IYUP) THEN
!                no forces in j-direction
                  DS2 = DXI**2 + DYI**2
                  DIX = DXI/DS2
                  DIY = DYI/DS2
                  DJX = 0.
                  DJY = 0.
               ELSE
!                coefficients for transformation from
!                (i,j)-gradients to (x,y)-gradients
                  DDET = DXI*DYJ - DXJ*DYI
                  DIX  =  DYJ / DDET
                  DIY  = -DXJ / DDET
                  DJX  = -DYI / DDET
                  DJY  =  DXI / DDET
               END IF

!                --- forces based on spatial gradients of radiation stresses
               SETPDA(INDX,1) = &
                  -(RSXXI*DIX + RSXXJ*DJX + RSXYI*DIY + RSXYJ*DJY)
               SETPDA(INDX,2) = &
                  -(RSXYI*DIX + RSXYJ*DJX + RSYYI*DIY + RSYYJ*DJY)

            END IF
         END DO
      END DO

!       --- compute set-up in two dimensions

      CALL SETUP2D( SETUP2, XCGRID, YCGRID, SETPDA(1,1), SETPDA(1,2), &
         KGRPNT, DEP2, SETPDA(1,6), SETPDA(1,15), &
         SETPDA(1,16), CONVERGED )

   END IF

   IF (LSETUP.EQ.1) THEN
!       set set-up to 0 for deepest point (This is allowed because the    31.03
!       solution of a Poisson equation + constant is again a solution of  31.03
!       the same Poisson equation)
      S_UPDP = SETUP2(IDXMAX)
      S_UPCOR = S_UPDP - PSETUP(2)
      DO IY = 1, MYC
         DO IX = 1, MXC
            INDX = KGRPNT(IX,IY)
            IF (INDX.GT.1) THEN
               IF (DEP2(INDX).GT.DEPMIN) THEN
                  SETUP2(INDX) = SETUP2(INDX) - S_UPCOR
               ELSE
                  IF (ABS(SETUP2(INDX)).GT.1.E-7) THEN
                     CHARS(1) = INTSTR(INDX)
                     CALL TXPBLA(CHARS(1),IF1,IL1)
                     MSGSTR = 'Set-up in dry point with index '// &
                        CHARS(1)(IF1:IL1)
                     CALL MSGERR ( 2, MSGSTR )
                  END IF
               END IF
            END IF
         END DO
      END DO
   END IF

!     --- include computed set-up to depth

   DO IY = 1, MYC
      DO IX = 1, MXC
         INDX = KGRPNT(IX,IY)
         IF (INDX.GT.1) THEN
            DEP2(INDX) = DEPSAV(INDX) + SETUP2(INDX)
         END IF
      END DO
   END DO

!     --- check whether dry points should be inundated

   DO IY = 1, MYC
      DO IX = 1, MXC
         INDX = KGRPNT(IX,IY)
!         Note:    KGRPNT(.,.) = 1 means a permanently dry point!         31.03
         IF (INDX.GT.1) THEN
            IF (DEP2(INDX).LE.DEPMIN) THEN
               DO LINK = 1, 4
                  NEIGHB = .TRUE.
                  IF (LINK.EQ.1) THEN
                     IF (IX.EQ.1) THEN
                        NEIGHB = .FALSE.
                     ELSE
                        INDXL = KGRPNT(IX-1,IY)
                        IF (INDXL.LE.1) NEIGHB = .FALSE.
                     ENDIF
                  ELSE IF (LINK.EQ.2) THEN
                     IF (IY.EQ.1) THEN
                        NEIGHB = .FALSE.
                     ELSE
                        INDXL = KGRPNT(IX,IY-1)
                        IF (INDXL.LE.1) NEIGHB = .FALSE.
                     ENDIF
                  ELSE IF (LINK.EQ.3) THEN
                     IF (IX.EQ.MXC) THEN
                        NEIGHB = .FALSE.
                     ELSE
                        INDXL = KGRPNT(IX+1,IY)
                        IF (INDXL.LE.1) NEIGHB = .FALSE.
                     ENDIF
                  ELSE IF (LINK.EQ.4) THEN
                     IF (IY.EQ.MYC) THEN
                        NEIGHB = .FALSE.
                     ELSE
                        INDXL = KGRPNT(IX,IY+1)
                        IF (INDXL.LE.1) NEIGHB = .FALSE.
                     ENDIF
                  ENDIF
                  IF (NEIGHB) THEN
                     IF (DEPSAV(INDX) + SETUP2(INDXL) .GT. DEPMIN) THEN
                        SETUP2(INDX) = SETUP2(INDXL)
                        DEP2(INDX) = DEPSAV(INDX) + SETUP2(INDXL)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ENDDO

   RETURN
!     end of subroutine SETUPP
end subroutine SETUPP
!****************************************************************

SUBROUTINE SETUP2D ( SETUP , XCGRID, YCGRID, WFRCX, WFRCY, &
   KGRPNT, DEPTH , AMAT  , RHS  , JCTA, CONVERGED )

!****************************************************************

   USE swan_service_interfaces, ONLY: STRACE
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_numerics
   USE swan_physical_settings
   USE swan_computational_grid

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
!     it under the terms of the GNU General Public License as published by
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
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Dec. 04: New subroutine
!
!  2. Purpose
!
!     Computation of the change of waterlevel due to waves
!
!  3. Method
!
!     A 2D Poisson equation in general coordinates is solved
!     Vertex-centered finite volume method is employed
!     The system of equations is solved using a SOR method
!
!  4. Argument variables
!
!     AMAT        the coefficient matrix used in the linear system
!     CONVERGED   output: false if the iterative solver did not converge
!                 (CSETUP in upstream, a module variable)
!     DEPTH       water depth
!     JCTA        Jacobian times contravariant base vectors:
!                                          (K)
!                 JCTA(I,J,K,L)  contains a    in cell with index point I
!                                          L
!                 and point type J
!     KGRPNT      indirect addressing for grid points
!     RHS         the right-hand side vector of the system of equations
!     SETUP       set-up
!     WFRCX       x-component of wave-induced force
!     WFRCY       y-component of wave-induced force
!     XCGRID      x-coordinates of computational grid
!     YCGRID      y-coordinates of computational grid

   LOGICAL, INTENT(OUT) :: CONVERGED
   INTEGER KGRPNT(MXC,MYC)
   REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   REAL    DEPTH(MCGRD), SETUP(MCGRD), WFRCX(MCGRD), WFRCY(MCGRD)
   REAL    AMAT(MCGRD,9), JCTA(MCGRD,2,2,2), RHS(MCGRD)

!  5. Parameter variables
!
!     RELAX :     relaxation parameter
!                 =  -1: relaxation parameter based on gridsizes
!                 <> -1: a fixed (initial) relaxation parameter

   REAL, PARAMETER :: RELAX=-1.

!  6. Local variables
!
!     CONTRB:     auxiliary variable containing contribution to the matrix
!     DEPF  :     water depth in flux point
!     FACT  :     a factor
!     IAMOUT:     control parameter indicating the amount of
!                 output required
!                 0: no output
!                 1: only fatal errors will be printed
!                 2: gives output concerning the iteration process
!                 3: additional information about the iteration
!                    is printed
!     ICONV :     indicator for convergence (1=yes, 0=no)
!     IENT  :     number of entries
!     II    :     iteration count in case of omega#1
!     INDX  :     index counter for point (ix  ,iy  ) in computational grid
!     INDXB :     index counter for point (ix  ,iy-1) in computational grid
!     INDXL :     index counter for point (ix-1,iy  ) in computational grid
!     INDXLB:     index counter for point (ix-1,iy-1) in computational grid
!     INDXLU:     index counter for point (ix-1,iy+1) in computational grid
!     INDXR :     index counter for point (ix+1,iy  ) in computational grid
!     INDXRB:     index counter for point (ix+1,iy-1) in computational grid
!     INDXRU:     index counter for point (ix+1,iy+1) in computational grid
!     INDXU :     index counter for point (ix  ,iy+1) in computational grid
!     IT    :     iteration count
!     IX    :     counter in x-direction
!     IXINF :     point in x-direction with largest error in solution
!     IY    :     counter in y-direction
!     IYINF :     point in y-direction with largest error in solution
!     JAC   :     Jacobian
!     MAXIT :     the maximum number of iterations to be performed in
!                 the linear solver
!     RES   :     residual
!     RESM  :     inf-norm of residual vector
!     RESMI :     intermediate inf-norm of residual vector
!     RESMO :     inf-norm of residual vector at previous iteration
!     RESM0 :     inf-norm of initial residual vector
!     REPS  :     relative accuracy of the final approximation
!     RHOV  :     estimated largest real eigenvalue
!     SETPI :     intermediate solution for set-up
!     XL    :     measure of convergence speed
!     XOM   :     actual relaxation parameter
!     XOMEG :     optimal overrelaxation parameter for SOR method

   INTEGER, SAVE :: IENT = 0
   INTEGER IAMOUT, ICONV, INDX, INDXB, INDXL, INDXLB, INDXLU, &
      INDXR, INDXRB, INDXRU, INDXU, IT, IX, IXINF, IY, IYINF, &
      II, MAXIT
   REAL    CONTRB, DEPF, FACT, JAC, RES, RESM, RESMI, RESMO, RESM0, &
      REPS, RHOV, SETPI, XL, XOM, XOMEG

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SETUPP
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     1) point type (=1,2) defines the position of the point in a cell:
!
!        *-------*
!        |       |
!        2       |
!        |       |
!        *---1---*
!
!        * = cell corner, depth point
!
!     2) Neumann boundary condition is imposed on all the boundaries
!
!     3) The determination of the overrelaxation factor by means of
!        alternatively switching between 1 and optimal omega is based
!        on the method as described in
!
!        E.F.F. Botta and M.H.M. Ellenbroek
!        A modified SOR method for the Poisson equation in unsteady
!        free-surface flow calculations
!        J. Comput. Phys., vol. 60, 119-134, 1985
!
! 12. Structure
!
!     initialize some arrays
!     determine contravariant base vectors times Jacobian
!     build right-hand side of the system of equations
!     build the matrix of the linear system
!     in case of nesting put Dirichlet boundary condition
!     set parameters for the solver
!     determine relaxation factor
!     solve the system of equations
!     investigate the reason to stop
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SETUP2D')

!     --- initialize some arrays

   AMAT = 0.
   JCTA = 0.
   RHS  = 0.

!     --- determine contravariant base vector a^(1) times Jacobian
!         in point type 1

   DO IX = 1, MXC-1
      INDX = KGRPNT(IX,1)
      IF ( INDX.GT.1 ) THEN
         JCTA(INDX,1,1,1) = 0.5*( YCGRID(IX+1,2) + &
            YCGRID(IX  ,2) - &
            YCGRID(IX+1,1) - &
            YCGRID(IX  ,1) )
         JCTA(INDX,1,1,2) = 0.5*( XCGRID(IX+1,1) + &
            XCGRID(IX  ,1) - &
            XCGRID(IX+1,2) - &
            XCGRID(IX  ,2) )
      END IF
   END DO
   DO IY = 2, MYC-1
      DO IX = 1, MXC-1
         INDX = KGRPNT(IX,IY)
         IF ( INDX.GT.1 ) THEN
            JCTA(INDX,1,1,1) = 0.25*( YCGRID(IX+1,IY+1) + &
               YCGRID(IX  ,IY+1) - &
               YCGRID(IX+1,IY-1) - &
               YCGRID(IX  ,IY-1) )
            JCTA(INDX,1,1,2) = 0.25*( XCGRID(IX+1,IY-1) + &
               XCGRID(IX  ,IY-1) - &
               XCGRID(IX+1,IY+1) - &
               XCGRID(IX  ,IY+1) )
         END IF
      END DO
   END DO
   DO IX = 1, MXC-1
      INDX = KGRPNT(IX,MYC)
      IF ( INDX.GT.1 ) THEN
         JCTA(INDX,1,1,1) = 0.5*( YCGRID(IX+1,MYC  ) + &
            YCGRID(IX  ,MYC  ) - &
            YCGRID(IX+1,MYC-1) - &
            YCGRID(IX  ,MYC-1) )
         JCTA(INDX,1,1,2) = 0.5*( XCGRID(IX+1,MYC-1) + &
            XCGRID(IX  ,MYC-1) - &
            XCGRID(IX+1,MYC  ) - &
            XCGRID(IX  ,MYC  ) )
      END IF
   END DO

!     --- determine contravariant base vector a^(1) times Jacobian
!         in point type 2

   DO IY = 1, MYC-1
      DO IX = 1, MXC
         INDX = KGRPNT(IX,IY)
         IF ( INDX.GT.1 ) THEN
            JCTA(INDX,2,1,1) = YCGRID(IX,IY+1) - YCGRID(IX,IY  )
            JCTA(INDX,2,1,2) = XCGRID(IX,IY  ) - XCGRID(IX,IY+1)
         END IF
      END DO
   END DO

!     --- determine contravariant base vector a^(2) times Jacobian
!         in point type 1

   DO IY = 1, MYC
      DO IX = 1, MXC-1
         INDX = KGRPNT(IX,IY)
         IF ( INDX.GT.1 ) THEN
            JCTA(INDX,1,2,1) = YCGRID(IX  ,IY) - YCGRID(IX+1,IY)
            JCTA(INDX,1,2,2) = XCGRID(IX+1,IY) - XCGRID(IX  ,IY)
         END IF
      END DO
   END DO

!     --- determine contravariant base vector a^(2) times Jacobian
!         in point type 2

   DO IY = 1, MYC-1
      INDX = KGRPNT(1,IY)
      IF ( INDX.GT.1 ) THEN
         JCTA(INDX,2,2,1) = 0.5*( YCGRID(1,IY+1) + &
            YCGRID(1,IY  ) - &
            YCGRID(2,IY+1) - &
            YCGRID(2,IY  ) )
         JCTA(INDX,2,2,2) = 0.5*( XCGRID(2,IY+1) + &
            XCGRID(2,IY  ) - &
            XCGRID(1,IY+1) - &
            XCGRID(1,IY  ) )
      END IF
      DO IX = 2, MXC-1
         INDX = KGRPNT(IX,IY)
         IF ( INDX.GT.1 ) THEN
            JCTA(INDX,2,2,1) = 0.25*( YCGRID(IX-1,IY+1) + &
               YCGRID(IX-1,IY  ) - &
               YCGRID(IX+1,IY+1) - &
               YCGRID(IX+1,IY  ) )
            JCTA(INDX,2,2,2) = 0.25*( XCGRID(IX+1,IY+1) + &
               XCGRID(IX+1,IY  ) - &
               XCGRID(IX-1,IY+1) - &
               XCGRID(IX-1,IY  ) )
         END IF
      END DO
      INDX = KGRPNT(MXC,IY)
      IF ( INDX.GT.1 ) THEN
         JCTA(INDX,2,2,1) = 0.5*( YCGRID(MXC-1,IY+1) + &
            YCGRID(MXC-1,IY  ) - &
            YCGRID(MXC  ,IY+1) - &
            YCGRID(MXC  ,IY  ) )
         JCTA(INDX,2,2,2) = 0.5*( XCGRID(MXC  ,IY+1) + &
            XCGRID(MXC  ,IY  ) - &
            XCGRID(MXC-1,IY+1) - &
            XCGRID(MXC-1,IY  ) )
      END IF
   END DO

!     --- build right-hand side of the system of equations
!
!     --- interior domain
   DO IY = 2, MYC-1
      DO IX = 2, MXC-1
         INDX  = KGRPNT(IX  ,IY  )
         INDXL = KGRPNT(IX-1,IY  )
         INDXR = KGRPNT(IX+1,IY  )
         INDXB = KGRPNT(IX  ,IY-1)
         INDXU = KGRPNT(IX  ,IY+1)
         IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!              --- contribution of right flux
            IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
               RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1) &
                  *(WFRCX(INDX)+WFRCX(INDXR)) &
                  - 0.5*JCTA(INDX,1,1,2) &
                  *(WFRCY(INDX)+WFRCY(INDXR))
            END IF
!              --- contribution of left flux
            IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
               RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1) &
                  *(WFRCX(INDXL)+WFRCX(INDX)) &
                  + 0.5*JCTA(INDXL,1,1,2) &
                  *(WFRCY(INDXL)+WFRCY(INDX))
            END IF
!              --- contribution of upper flux
            IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
               RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1) &
                  *(WFRCX(INDX)+WFRCX(INDXU)) &
                  - 0.5*JCTA(INDX,2,2,2) &
                  *(WFRCY(INDX)+WFRCY(INDXU))
            END IF
!              --- contribution of bottom flux
            IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
               RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1) &
                  *(WFRCX(INDXB)+WFRCX(INDX)) &
                  + 0.5*JCTA(INDXB,2,2,2) &
                  *(WFRCY(INDXB)+WFRCY(INDX))
            END IF
         END IF
      END DO
   END DO

!     --- lower boundary (IY=1)
   DO IX = 2, MXC-1
      INDX  = KGRPNT(IX  ,1)
      INDXL = KGRPNT(IX-1,1)
      INDXR = KGRPNT(IX+1,1)
      INDXU = KGRPNT(IX  ,2)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1) &
               *(WFRCX(INDX)+WFRCX(INDXR)) &
               - 0.5*JCTA(INDX,1,1,2) &
               *(WFRCY(INDX)+WFRCY(INDXR))
         END IF
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1) &
               *(WFRCX(INDXL)+WFRCX(INDX)) &
               + 0.5*JCTA(INDXL,1,1,2) &
               *(WFRCY(INDXL)+WFRCY(INDX))
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1) &
               *(WFRCX(INDX)+WFRCX(INDXU)) &
               - JCTA(INDX,2,2,2) &
               *(WFRCY(INDX)+WFRCY(INDXU))
         END IF
      END IF
   END DO

!     --- upper boundary (IY=MYC)
   DO IX = 2, MXC-1
      INDX  = KGRPNT(IX  ,MYC  )
      INDXL = KGRPNT(IX-1,MYC  )
      INDXR = KGRPNT(IX+1,MYC  )
      INDXB = KGRPNT(IX  ,MYC-1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1) &
               *(WFRCX(INDX)+WFRCX(INDXR)) &
               - 0.5*JCTA(INDX,1,1,2) &
               *(WFRCY(INDX)+WFRCY(INDXR))
         END IF
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1) &
               *(WFRCX(INDXL)+WFRCX(INDX)) &
               + 0.5*JCTA(INDXL,1,1,2) &
               *(WFRCY(INDXL)+WFRCY(INDX))
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1) &
               *(WFRCX(INDXB)+WFRCX(INDX)) &
               + JCTA(INDXB,2,2,2) &
               *(WFRCY(INDXB)+WFRCY(INDX))
         END IF
      END IF
   END DO

!     --- left boundary (IX=1)
   DO IY = 2, MYC-1
      INDX  = KGRPNT(1,IY  )
      INDXR = KGRPNT(2,IY  )
      INDXB = KGRPNT(1,IY-1)
      INDXU = KGRPNT(1,IY+1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1) &
               *(WFRCX(INDX)+WFRCX(INDXR)) &
               - JCTA(INDX,1,1,2) &
               *(WFRCY(INDX)+WFRCY(INDXR))
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1) &
               *(WFRCX(INDX)+WFRCX(INDXU)) &
               - 0.5*JCTA(INDX,2,2,2) &
               *(WFRCY(INDX)+WFRCY(INDXU))
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1) &
               *(WFRCX(INDXB)+WFRCX(INDX)) &
               + 0.5*JCTA(INDXB,2,2,2) &
               *(WFRCY(INDXB)+WFRCY(INDX))
         END IF
      END IF
   END DO

!     --- right boundary (IX=MXC)
   DO IY = 2, MYC-1
      INDX  = KGRPNT(MXC  ,IY  )
      INDXL = KGRPNT(MXC-1,IY  )
      INDXB = KGRPNT(MXC  ,IY-1)
      INDXU = KGRPNT(MXC  ,IY+1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1) &
               *(WFRCX(INDXL)+WFRCX(INDX)) &
               + JCTA(INDXL,1,1,2) &
               *(WFRCY(INDXL)+WFRCY(INDX))
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1) &
               *(WFRCX(INDX)+WFRCX(INDXU)) &
               - 0.5*JCTA(INDX,2,2,2) &
               *(WFRCY(INDX)+WFRCY(INDXU))
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1) &
               *(WFRCX(INDXB)+WFRCX(INDX)) &
               + 0.5*JCTA(INDXB,2,2,2) &
               *(WFRCY(INDXB)+WFRCY(INDX))
         END IF
      END IF
   END DO

!     --- left-lower corner (IX=1,IY=1)
   INDX  = KGRPNT(1,1)
   INDXR = KGRPNT(2,1)
   INDXU = KGRPNT(1,2)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
      IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1) &
            *(WFRCX(INDX)+WFRCX(INDXR)) &
            - JCTA(INDX,1,1,2) &
            *(WFRCY(INDX)+WFRCY(INDXR))
      END IF
!        --- contribution of upper flux
      IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1) &
            *(WFRCX(INDX)+WFRCX(INDXU)) &
            - JCTA(INDX,2,2,2) &
            *(WFRCY(INDX)+WFRCY(INDXU))
      END IF
   END IF

!     --- right-lower corner (IX=MXC,IY=1)
   INDX  = KGRPNT(MXC  ,1)
   INDXL = KGRPNT(MXC-1,1)
   INDXU = KGRPNT(MXC  ,2)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
      IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1) &
            *(WFRCX(INDXL)+WFRCX(INDX)) &
            + JCTA(INDXL,1,1,2) &
            *(WFRCY(INDXL)+WFRCY(INDX))
      END IF
!        --- contribution of upper flux
      IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1) &
            *(WFRCX(INDX)+WFRCX(INDXU)) &
            - JCTA(INDX,2,2,2) &
            *(WFRCY(INDX)+WFRCY(INDXU))
      END IF
   END IF

!     --- left-upper corner (IX=1,IY=MYC)
   INDX  = KGRPNT(1,MYC  )
   INDXR = KGRPNT(2,MYC  )
   INDXB = KGRPNT(1,MYC-1)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
      IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1) &
            *(WFRCX(INDX)+WFRCX(INDXR)) &
            - JCTA(INDX,1,1,2) &
            *(WFRCY(INDX)+WFRCY(INDXR))
      END IF
!        --- contribution of bottom flux
      IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1) &
            *(WFRCX(INDXB)+WFRCX(INDX)) &
            + JCTA(INDXB,2,2,2) &
            *(WFRCY(INDXB)+WFRCY(INDX))
      END IF
   END IF

!     --- right-upper corner (IX=MXC,IY=MYC)
   INDX  = KGRPNT(MXC  ,MYC  )
   INDXL = KGRPNT(MXC-1,MYC  )
   INDXB = KGRPNT(MXC  ,MYC-1)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
      IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1) &
            *(WFRCX(INDXL)+WFRCX(INDX)) &
            + JCTA(INDXL,1,1,2) &
            *(WFRCY(INDXL)+WFRCY(INDX))
      END IF
!        --- contribution of bottom flux
      IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
         RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1) &
            *(WFRCX(INDXB)+WFRCX(INDX)) &
            + JCTA(INDXB,2,2,2) &
            *(WFRCY(INDXB)+WFRCY(INDX))
      END IF
   END IF

!     --- build the matrix of the linear system
!
!     --- interior domain
   DO IY = 2, MYC-1
      DO IX = 2, MXC-1
         INDX  = KGRPNT(IX  ,IY  )
         INDXL = KGRPNT(IX-1,IY  )
         INDXR = KGRPNT(IX+1,IY  )
         INDXB = KGRPNT(IX  ,IY-1)
         INDXU = KGRPNT(IX  ,IY+1)
         IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!              --- contribution of right flux
            IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
               DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
               JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
                  JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
               IF ( JAC.NE.0. ) THEN
                  FACT = -DEPF/JAC
               ELSE
                  FACT = 0.
               END IF
               CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
                  JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
               AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
               AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
               CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
                  JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
               AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
               AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
               AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
               AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
            END IF
!              --- contribution of left flux
            IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
               DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
               JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
                  JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
               IF ( JAC.NE.0. ) THEN
                  FACT = -DEPF/JAC
               ELSE
                  FACT = 0.
               END IF
               CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
                  JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
               AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
               AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
               CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
                  JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
               AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
               AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
               AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
               AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
            END IF
!              --- contribution of upper flux
            IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
               DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
               JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
                  JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
               IF ( JAC.NE.0. ) THEN
                  FACT = -DEPF/JAC
               ELSE
                  FACT = 0.
               END IF
               CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
                  JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
               AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
               AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
               CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
                  JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
               AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
               AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
               AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
               AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
            END IF
!              --- contribution of bottom flux
            IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
               DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
               JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
                  JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
               IF ( JAC.NE.0. ) THEN
                  FACT = -DEPF/JAC
               ELSE
                  FACT = 0.
               END IF
               CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
                  JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
               AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
               AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
               CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
                  JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
               AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
               AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
               AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
               AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
            END IF
         END IF
         IF ( AMAT(INDX,1).EQ.0. ) THEN
            AMAT(INDX,:) = 0.0
            AMAT(INDX,1) = 1.0
            RHS (INDX  ) = 0.0
         END IF
      END DO
   END DO

!     --- lower boundary (IY=1)
   DO IX = 2, MXC-1
      INDX  = KGRPNT(IX  ,1)
      INDXL = KGRPNT(IX-1,1)
      INDXR = KGRPNT(IX+1,1)
      INDXU = KGRPNT(IX  ,2)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
            JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
            JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            DEPF = DEPTH(INDX)+DEPTH(INDXU)
            JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
               JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
         AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,2)
         AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,2)
         AMAT(INDX,2) = 0.
         AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
         AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
         AMAT(INDX,3) = 0.
         AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,4)
         AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,4)
         AMAT(INDX,4) = 0.
      END IF
      IF ( AMAT(INDX,1).EQ.0. ) THEN
         AMAT(INDX,:) = 0.0
         AMAT(INDX,1) = 1.0
         RHS (INDX  ) = 0.0
      END IF
   END DO

!     --- upper boundary (IY=MYC)
   DO IX = 2, MXC-1
      INDX  = KGRPNT(IX  ,MYC  )
      INDXL = KGRPNT(IX-1,MYC  )
      INDXR = KGRPNT(IX+1,MYC  )
      INDXB = KGRPNT(IX  ,MYC-1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
            JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
            JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            DEPF = DEPTH(INDXB)+DEPTH(INDX)
            JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
               JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
         END IF
         AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,7)
         AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,7)
         AMAT(INDX,7) = 0.
         AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
         AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
         AMAT(INDX,8) = 0.
         AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,9)
         AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,9)
         AMAT(INDX,9) = 0.
      END IF
      IF ( AMAT(INDX,1).EQ.0. ) THEN
         AMAT(INDX,:) = 0.0
         AMAT(INDX,1) = 1.0
         RHS (INDX  ) = 0.0
      END IF
   END DO

!     --- left boundary (IX=1)
   DO IY = 2, MYC-1
      INDX  = KGRPNT(1,IY  )
      INDXR = KGRPNT(2,IY  )
      INDXB = KGRPNT(1,IY-1)
      INDXU = KGRPNT(1,IY+1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
         IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
            DEPF = DEPTH(INDX)+DEPTH(INDXR)
            JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
            CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
               JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
            JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
               JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
            JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
               JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
         END IF
         AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,2)
         AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,2)
         AMAT(INDX,2) = 0.
         AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
         AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
         AMAT(INDX,5) = 0.
         AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,7)
         AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,7)
         AMAT(INDX,7) = 0.
      END IF
      IF ( AMAT(INDX,1).EQ.0. ) THEN
         AMAT(INDX,:) = 0.0
         AMAT(INDX,1) = 1.0
         RHS (INDX  ) = 0.0
      END IF
   END DO

!     --- right boundary (IX=MXC)
   DO IY = 2, MYC-1
      INDX  = KGRPNT(MXC  ,IY  )
      INDXL = KGRPNT(MXC-1,IY  )
      INDXB = KGRPNT(MXC  ,IY-1)
      INDXU = KGRPNT(MXC  ,IY+1)
      IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of left flux
         IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
            DEPF = DEPTH(INDXL)+DEPTH(INDX)
            JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
            CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
               JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
         END IF
!           --- contribution of upper flux
         IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
            JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
               JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
            AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
            CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
               JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
            AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
            AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
         END IF
!           --- contribution of bottom flux
         IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
            DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
            JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
               JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
            IF ( JAC.NE.0. ) THEN
               FACT = -DEPF/JAC
            ELSE
               FACT = 0.
            END IF
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
            AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
            AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
            CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
               JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
            AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
            AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
            AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
            AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
         END IF
         AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,4)
         AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,4)
         AMAT(INDX,4) = 0.
         AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
         AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
         AMAT(INDX,6) = 0.
         AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,9)
         AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,9)
         AMAT(INDX,9) = 0.
      END IF
      IF ( AMAT(INDX,1).EQ.0. ) THEN
         AMAT(INDX,:) = 0.0
         AMAT(INDX,1) = 1.0
         RHS (INDX  ) = 0.0
      END IF
   END DO

!     --- left-lower corner (IX=1,IY=1)
   INDX  = KGRPNT(1,1)
   INDXR = KGRPNT(2,1)
   INDXU = KGRPNT(1,2)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
      IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDX)+DEPTH(INDXR)
         JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
         AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
         CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
         AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
         AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
         AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
      END IF
!        --- contribution of upper flux
      IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDX)+DEPTH(INDXU)
         JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
            JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
            JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
         AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
         CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
            JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
         AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
         AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
         AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
      END IF
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
      AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
      AMAT(INDX,3) = 0.
      AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,4)
      AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,4)
      AMAT(INDX,4) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
      AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
      AMAT(INDX,5) = 0.
      AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,7)
      AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,7)
      AMAT(INDX,7) = 0.
   END IF
   IF ( AMAT(INDX,1).EQ.0. ) THEN
      AMAT(INDX,:) = 0.0
      AMAT(INDX,1) = 1.0
      RHS (INDX  ) = 0.0
   END IF

!     --- right-lower corner (IX=MXC,IY=1)
   INDX  = KGRPNT(MXC  ,1)
   INDXL = KGRPNT(MXC-1,1)
   INDXU = KGRPNT(MXC  ,2)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
      IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDXL)+DEPTH(INDX)
         JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
         AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
         AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
         CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
         AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
         AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
         AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
      END IF
!        --- contribution of upper flux
      IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDX)+DEPTH(INDXU)
         JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) - &
            JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+ &
            JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
         AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
         CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+ &
            JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
         AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
         AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
         AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
      END IF
      AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,2)
      AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,2)
      AMAT(INDX,2) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
      AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
      AMAT(INDX,3) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
      AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
      AMAT(INDX,6) = 0.
      AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,9)
      AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,9)
      AMAT(INDX,9) = 0.
   END IF
   IF ( AMAT(INDX,1).EQ.0. ) THEN
      AMAT(INDX,:) = 0.0
      AMAT(INDX,1) = 1.0
      RHS (INDX  ) = 0.0
   END IF

!     --- left-upper corner (IX=1,IY=MYC)
   INDX  = KGRPNT(1,MYC  )
   INDXR = KGRPNT(2,MYC  )
   INDXB = KGRPNT(1,MYC-1)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
      IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDX)+DEPTH(INDXR)
         JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) - &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+ &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
         AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
         CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+ &
            JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
         AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
         AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
         AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
      END IF
!        --- contribution of bottom flux
      IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDXB)+DEPTH(INDX)
         JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
            JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
            JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
         AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
         AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
         CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
            JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
         AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
         AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
         AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
      END IF
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
      AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
      AMAT(INDX,8) = 0.
      AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,9)
      AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,9)
      AMAT(INDX,9) = 0.
      AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,2)
      AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,2)
      AMAT(INDX,2) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
      AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
      AMAT(INDX,5) = 0.
   END IF
   IF ( AMAT(INDX,1).EQ.0. ) THEN
      AMAT(INDX,:) = 0.0
      AMAT(INDX,1) = 1.0
      RHS (INDX  ) = 0.0
   END IF

!     --- right-upper corner (IX=MXC,IY=MYC)
   INDX  = KGRPNT(MXC  ,MYC  )
   INDXL = KGRPNT(MXC-1,MYC  )
   INDXB = KGRPNT(MXC  ,MYC-1)
   IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
      IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDXL)+DEPTH(INDX)
         JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) - &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+ &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
         AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
         AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
         CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+ &
            JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
         AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
         AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
         AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
         AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
      END IF
!        --- contribution of bottom flux
      IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
         DEPF = DEPTH(INDXB)+DEPTH(INDX)
         JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) - &
            JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
         IF ( JAC.NE.0. ) THEN
            FACT = -DEPF/JAC
         ELSE
            FACT = 0.
         END IF
         CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+ &
            JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
         AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
         AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
         CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+ &
            JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
         AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
         AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
         AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
         AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
      END IF
      AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,7)
      AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,7)
      AMAT(INDX,7) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
      AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
      AMAT(INDX,8) = 0.
      AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,4)
      AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,4)
      AMAT(INDX,4) = 0.
      AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
      AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
      AMAT(INDX,6) = 0.
   END IF
   IF ( AMAT(INDX,1).EQ.0. ) THEN
      AMAT(INDX,:) = 0.0
      AMAT(INDX,1) = 1.0
      RHS (INDX  ) = 0.0
   END IF

!     --- in case of nesting put Dirichlet boundary condition

   IF ( LSETUP.EQ.2 ) THEN
!        --- left and right boundaries
      DO IY = 1, MYC
         DO IX = 1, MXC, MXC-1
            INDX  = KGRPNT(IX,IY)
            AMAT(INDX,:) = 0.0
            AMAT(INDX,1) = 1.0
            IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
               RHS(INDX) = SETUP(INDX)
            ELSE
               RHS(INDX) = 0.0
            END IF
         END DO
      END DO
!        --- lower and upper boundaries
      DO IY = 1, MYC, MYC-1
         DO IX = 1, MXC
            INDX  = KGRPNT(IX,IY)
            AMAT(INDX,:) = 0.0
            AMAT(INDX,1) = 1.0
            IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
               RHS(INDX) = SETUP(INDX)
            ELSE
               RHS(INDX) = 0.0
            END IF
         END DO
      END DO
   END IF

!     --- set parameters for the solver

   REPS   = PNUMS(PNUMS_SUPEPS)
   IAMOUT = INT(PNUMS(PNUMS_SUPPRN))
   MAXIT  = INT(PNUMS(PNUMS_SUPMAX))

   CONVERGED = .TRUE.

!     --- determine relaxation factor

   IF ( RELAX.EQ.-1. ) THEN
      IF ( MCGRD.LT.100 ) THEN
         RHOV = 1. - 1./REAL(MCGRD)
      ELSE IF ( MCGRD.LT.1000 ) THEN
         RHOV = 1. - 3./REAL(MCGRD)
      ELSE
         RHOV = 1. - 10./REAL(MCGRD)
      END IF
      XOMEG = 2./(1.+SQRT(1.-RHOV*RHOV))
   ELSE
      XOMEG = RELAX
   END IF

   IT    = 0
   ICONV = 0
   RESM  = 1.
   XOM   = 1.

!     --- solve the system of equations

   setup_iterations: DO WHILE ( ICONV.EQ.0 .AND. IT.LT.MAXIT )

      IT    = IT + 1
      ICONV = 1

      RESMO = RESM
      RESM  = 0.
      IXINF = 0
      IYINF = 0

!        --- interior domain
      DO IY = 2, MYC-1
         DO IX = 2, MXC-1
            INDX   = KGRPNT(IX  ,IY  )
            INDXL  = KGRPNT(IX-1,IY  )
            INDXR  = KGRPNT(IX+1,IY  )
            INDXB  = KGRPNT(IX  ,IY-1)
            INDXU  = KGRPNT(IX  ,IY+1)
            INDXLB = KGRPNT(IX-1,IY-1)
            INDXRB = KGRPNT(IX+1,IY-1)
            INDXLU = KGRPNT(IX-1,IY+1)
            INDXRU = KGRPNT(IX+1,IY+1)

            SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB) &
               - AMAT(INDX,3)*SETUP(INDXB ) &
               - AMAT(INDX,4)*SETUP(INDXRB) &
               - AMAT(INDX,5)*SETUP(INDXL ) &
               - AMAT(INDX,6)*SETUP(INDXR ) &
               - AMAT(INDX,7)*SETUP(INDXLU) &
               - AMAT(INDX,8)*SETUP(INDXU ) &
               - AMAT(INDX,9)*SETUP(INDXRU)
            SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

            RES = ABS(SETUP(INDX) - SETPI)
            IF ( RES.GT.RESM ) THEN
               RESM  = RES
               IXINF = IX
               IYINF = IY
            END IF
            SETUP(INDX) = SETPI

         END DO
      END DO

!        --- lower boundary (IY=1)
      DO IX = 2, MXC-1
         INDX   = KGRPNT(IX  ,1)
         INDXL  = KGRPNT(IX-1,1)
         INDXR  = KGRPNT(IX+1,1)
         INDXU  = KGRPNT(IX  ,2)
         INDXLU = KGRPNT(IX-1,2)
         INDXRU = KGRPNT(IX+1,2)

         SETPI = RHS(INDX) - AMAT(INDX,5)*SETUP(INDXL ) &
            - AMAT(INDX,6)*SETUP(INDXR ) &
            - AMAT(INDX,7)*SETUP(INDXLU) &
            - AMAT(INDX,8)*SETUP(INDXU ) &
            - AMAT(INDX,9)*SETUP(INDXRU)
         SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

         RES = ABS(SETUP(INDX) - SETPI)
         IF ( RES.GT.RESM ) THEN
            RESM  = RES
            IXINF = IX
            IYINF = 1
         END IF
         SETUP(INDX) = SETPI

      END DO

!        --- upper boundary (IY=MYC)
      DO IX = 2, MXC-1
         INDX   = KGRPNT(IX  ,MYC  )
         INDXL  = KGRPNT(IX-1,MYC  )
         INDXR  = KGRPNT(IX+1,MYC  )
         INDXB  = KGRPNT(IX  ,MYC-1)
         INDXLB = KGRPNT(IX-1,MYC-1)
         INDXRB = KGRPNT(IX+1,MYC-1)

         SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB) &
            - AMAT(INDX,3)*SETUP(INDXB ) &
            - AMAT(INDX,4)*SETUP(INDXRB) &
            - AMAT(INDX,5)*SETUP(INDXL ) &
            - AMAT(INDX,6)*SETUP(INDXR )
         SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

         RES = ABS(SETUP(INDX) - SETPI)
         IF ( RES.GT.RESM ) THEN
            RESM  = RES
            IXINF = IX
            IYINF = MYC
         END IF
         SETUP(INDX) = SETPI

      END DO

!        --- left boundary (IX=1)
      DO IY = 2, MYC-1
         INDX   = KGRPNT(1,IY  )
         INDXR  = KGRPNT(2,IY  )
         INDXB  = KGRPNT(1,IY-1)
         INDXU  = KGRPNT(1,IY+1)
         INDXRB = KGRPNT(2,IY-1)
         INDXRU = KGRPNT(2,IY+1)

         SETPI = RHS(INDX) - AMAT(INDX,3)*SETUP(INDXB ) &
            - AMAT(INDX,4)*SETUP(INDXRB) &
            - AMAT(INDX,6)*SETUP(INDXR ) &
            - AMAT(INDX,8)*SETUP(INDXU ) &
            - AMAT(INDX,9)*SETUP(INDXRU)
         SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

         RES = ABS(SETUP(INDX) - SETPI)
         IF ( RES.GT.RESM ) THEN
            RESM  = RES
            IXINF = 1
            IYINF = IY
         END IF
         SETUP(INDX) = SETPI

      END DO

!        --- right boundary (IX=MXC)
      DO IY = 2, MYC-1
         INDX   = KGRPNT(MXC  ,IY  )
         INDXL  = KGRPNT(MXC-1,IY  )
         INDXB  = KGRPNT(MXC  ,IY-1)
         INDXU  = KGRPNT(MXC  ,IY+1)
         INDXLB = KGRPNT(MXC-1,IY-1)
         INDXLU = KGRPNT(MXC-1,IY+1)

         SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB) &
            - AMAT(INDX,3)*SETUP(INDXB ) &
            - AMAT(INDX,5)*SETUP(INDXL ) &
            - AMAT(INDX,7)*SETUP(INDXLU) &
            - AMAT(INDX,8)*SETUP(INDXU )
         SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

         RES = ABS(SETUP(INDX) - SETPI)
         IF ( RES.GT.RESM ) THEN
            RESM  = RES
            IXINF = MXC
            IYINF = IY
         END IF
         SETUP(INDX) = SETPI

      END DO

!        --- left-lower corner (IX=1,IY=1)
      INDX   = KGRPNT(1,1)
      INDXR  = KGRPNT(2,1)
      INDXU  = KGRPNT(1,2)
      INDXRU = KGRPNT(2,2)

      SETPI = RHS(INDX) - AMAT(INDX,6)*SETUP(INDXR ) &
         - AMAT(INDX,8)*SETUP(INDXU ) &
         - AMAT(INDX,9)*SETUP(INDXRU)
      SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

      RES = ABS(SETUP(INDX) - SETPI)
      IF ( RES.GT.RESM ) THEN
         RESM  = RES
         IXINF = 1
         IYINF = 1
      END IF
      SETUP(INDX) = SETPI

!        --- right-lower corner (IX=MXC,IY=1)
      INDX   = KGRPNT(MXC  ,1)
      INDXL  = KGRPNT(MXC-1,1)
      INDXU  = KGRPNT(MXC  ,2)
      INDXLU = KGRPNT(MXC-1,2)

      SETPI = RHS(INDX) - AMAT(INDX,5)*SETUP(INDXL ) &
         - AMAT(INDX,7)*SETUP(INDXLU) &
         - AMAT(INDX,8)*SETUP(INDXU )
      SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

      RES = ABS(SETUP(INDX) - SETPI)
      IF ( RES.GT.RESM ) THEN
         RESM  = RES
         IXINF = MXC
         IYINF = 1
      END IF
      SETUP(INDX) = SETPI

!        --- left-upper corner (IX=1,IY=MYC)
      INDX   = KGRPNT(1,MYC  )
      INDXR  = KGRPNT(2,MYC  )
      INDXB  = KGRPNT(1,MYC-1)
      INDXRB = KGRPNT(2,MYC-1)

      SETPI = RHS(INDX) - AMAT(INDX,3)*SETUP(INDXB ) &
         - AMAT(INDX,4)*SETUP(INDXRB) &
         - AMAT(INDX,6)*SETUP(INDXR )
      SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

      RES = ABS(SETUP(INDX) - SETPI)
      IF ( RES.GT.RESM ) THEN
         RESM  = RES
         IXINF = 1
         IYINF = MYC
      END IF
      SETUP(INDX) = SETPI

!        --- right-upper corner (IX=MXC,IY=MYC)
      INDX   = KGRPNT(MXC  ,MYC  )
      INDXL  = KGRPNT(MXC-1,MYC  )
      INDXB  = KGRPNT(MXC  ,MYC-1)
      INDXLB = KGRPNT(MXC-1,MYC-1)

      SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB) &
         - AMAT(INDX,3)*SETUP(INDXB ) &
         - AMAT(INDX,5)*SETUP(INDXL )
      SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

      RES = ABS(SETUP(INDX) - SETPI)
      IF ( RES.GT.RESM ) THEN
         RESM  = RES
         IXINF = MXC
         IYINF = MYC
      END IF
      SETUP(INDX) = SETPI

      IF ( RESM.GT.1.E8 ) THEN
         IT = MAXIT + 1
         ICONV = 0
         CYCLE setup_iterations
      END IF
      IF ( IAMOUT.EQ.2 ) THEN
         WRITE (PRINTF,'(A,I3,A,E12.6,A,I3,A,I3,A)') &
            ' ++ SETUP2D: iter = ',IT,'    res = ',RESM, &
            ' in (IX,IY) = (',IXINF,',',IYINF,')'
      END IF
      IF ( IAMOUT.EQ.3 .AND. IT.EQ.1 ) RESM0 = RESM

      IF ( RESM.GT.REPS ) THEN
         IF ( XOM.NE.1. ) THEN
            II = II + 1
            IF ( II.EQ.0 ) RESMI = RESM
            IF ( II.GT.0 .AND. &
               RESM.GT.10.*RESMI*(XOMEG-1.)**II ) XOM = 1.
         ELSE
            XL = RESM/RESMO
            IF ( XL.GT.0.9*RHOV*RHOV ) THEN
               XOM = XOMEG
               II  = -10
            END IF
         END IF
         ICONV = 0
      END IF

   END DO setup_iterations

!     --- investigate the reason to stop

   IF ( ICONV.EQ.0 ) THEN
      CONVERGED = .FALSE.
      IF ( RESM.GT.1.E8 ) SETUP = 0.
   END IF
   IF ( ICONV.EQ.0 .AND. IAMOUT.GE.1 ) THEN
      WRITE (PRINTF,'(A)') &
         ' ++ SETUP2D: no convergence for 2D Poisson equation'
      WRITE (PRINTF,'(A,I4)') &
         '             total number of iterations       = ',IT
      WRITE (PRINTF,'(A,E12.6)') &
         '             inf-norm of the residual         = ',RESM
      WRITE (PRINTF,'(A,E12.6)') &
         '             required accuracy                = ',REPS
   ELSE IF ( IAMOUT.EQ.3 ) THEN
      WRITE (PRINTF,'(A,E12.6)') &
         ' ++ SETUP2D: inf-norm of the initial residual = ',RESM0
      WRITE (PRINTF,'(A,I4)') &
         '             total number of iterations       = ',IT
      WRITE (PRINTF,'(A,E12.6)') &
         '             inf-norm of the residual         = ',RESM
   END IF

   RETURN
end subroutine SETUP2D

end module swan_wave_setup
