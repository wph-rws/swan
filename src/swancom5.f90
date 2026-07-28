
!     SWAN/COMPU    file 5 of 5
!
!
!     PROGRAM SWANCOM5.FOR
!
!     This file SWANCOM5 of the main program SWAN
!     includes the next subroutines (mainly subroutines for
!     the propagation in x,y,s,d space and parameters ) :
!
!     SWGEOM  ( determines geometric quantities )
!     SWPSEL  ( determine spectral counters in presence
!               or absence of a current )
!     SPROXY  ( compute spatial propagation velocities CAX, CAY )
!     SPROSD  ( compute spectral propagation velocities CAS, CAD )
!     DSPHER  ( compute Ctheta for propagation over the globe )
!     STRSXY  ( compute derivative in space and time )
!     SORDUP  ( compute spatial derivatives with SORDUP scheme )
!     SANDL   ( compute spatial derivatives with S&L scheme )
!     STRSSI  ( compute derivative in s-space implicit scheme )
!     STRSSB  ( compute derivative in s-space explicit scheme and
!               remove (or dissipate bin's that are blocked) )
!     STRSD   ( compute derivative in d-space implicit )
!     SPREDT  ( calculate action density in central point: first guess )
!     SWAPAR  ( compute wave parameters k, cgo and cg )
!     ADDDIS  ( adds leak and dissipation to arrays in COMPDA, after
!               action densities have been computed )
!     SWFLXD  ( compute derivative in theta-space by means of
!               flux-limiting )
!     DIFPAR  ( compute diffraction parameter and its derivatives )
!
!****************************************************************

module swan_propagation
   use swan_diffraction_state, only: diffraction_state_t
   use swan_output_settings, only: ERRPTS
   implicit none(type, external)
   private
   public :: SWGEOM, SWPSEL, SPROXY, SPROSD, DSPHER, STRSXY, SORDUP, SANDL
   public :: STRSSI, STRSSB, STRSD, SPREDT, SWAPAR, SWAPRE, ADDDIS, SWFLXD, DIFPAR
contains

SUBROUTINE SWGEOM ( RDX, RDY, XCGRID, YCGRID, SWPDIR )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_coordinate_offset
   USE swan_stencil
   USE swan_computational_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_spherical_geometry
   USE swan_diagnostics_level
   USE swan_io_units
   USE M_PARALL

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
!     40.41: Marcel Zijlema
!     40.98: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Sep. 04: New subroutine (taken from routine SWPSEL)
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.98, Feb. 09: SORDUP scheme is made consistent
!
!  2. Purpose
!
!     Determine geometric quantities due to curvilinear grid
!
!  3. Method
!
!     Trivial
!
!  4. Argument variables
!
!     RDX         contains derivatives of (ksi,eta) to x-direction
!                 (i.e. first component of contravariant base
!                  vector RDX(b) = a^(b)_1)
!     RDY         contains derivatives of (ksi,eta) to y-direction
!                 (i.e. second component of contravariant base
!                  vector RDY(b) = a^(b)_2)
!     SWPDIR      sweep direction
!     XCGRID      coordinates of computational grid in x-direction
!     YCGRID      coordinates of computational grid in y-direction

   INTEGER SWPDIR
   REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC),&
   &RDX(MICMAX)    , RDY(MICMAX)

!  6. Local variables
!
!     DET   :     determinant or volume of cell
!     DX1   :     first component of covariant base vector a_(1)
!     DX2   :     second component of covariant base vector a_(1)
!     DY1   :     first component of covariant base vector a_(2)
!     DY2   :     second component of covariant base vector a_(2)
!     IC    :     counter
!     IENT  :     number of entries
!     IXY   :     counter
!     VIRT  :     indicates virtual point for 1D mode

   INTEGER, SAVE :: IENT = 0
   INTEGER IC, IXY
   REAL    VIRT, DET, DX1, DX2, DY1, DY2

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWOMPU
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWGEOM')

   IF (KREPTX.GT.0) THEN
!       repeating x-axis (only regular grids)
      IF (SWPDIR.EQ.1 .OR. SWPDIR.EQ.4) THEN
         DX1 = DX * COSPC
         DY1 = DX * SINPC
      ELSE
         DX1 = -DX * COSPC
         DY1 = -DX * SINPC
      ENDIF
      IF ( ONED ) THEN
!         *** Inclusion of virtual point ***
         VIRT = 1.E6
         IF ( SWPDIR .EQ. 1 .OR. SWPDIR .EQ. 3 ) THEN
            DX2 = -VIRT * DY1
            DY2 =  VIRT * DX1
         ELSE
            DX2 =  VIRT * DY1
            DY2 = -VIRT * DX1
         ENDIF
      ELSE
         IF (SWPDIR.LE.2) THEN
            DX2 = - DY * SINPC
            DY2 =   DY * COSPC
         ELSE
            DX2 =   DY * SINPC
            DY2 = - DY * COSPC
         ENDIF
      ENDIF
   ELSE
      DX1 = XCGRID(IXCGRD(1),IYCGRD(1)) - XCGRID(IXCGRD(2),IYCGRD(2))
      DY1 = YCGRID(IXCGRD(1),IYCGRD(1)) - YCGRID(IXCGRD(2),IYCGRD(2))
      IF  ( ONED ) THEN
!         *** Inclusion of virtual point ***
         VIRT = 1.E6
         IF ( SWPDIR .EQ. 1 .OR. SWPDIR .EQ. 3 ) THEN
            DX2 = -VIRT * DY1
            DY2 =  VIRT * DX1
         ELSE IF ( SWPDIR .EQ. 2 .OR. SWPDIR .EQ. 4 ) THEN
            DX2 =  VIRT * DY1
            DY2 = -VIRT * DX1
         ENDIF
      ELSE
         DX2 = XCGRID(IXCGRD(1),IYCGRD(1))-XCGRID(IXCGRD(3),IYCGRD(3))
         DY2 = YCGRID(IXCGRD(1),IYCGRD(1))-YCGRID(IXCGRD(3),IYCGRD(3))
      ENDIF
   ENDIF

   DET    =  DY2*DX1 - DY1*DX2
   RDX(1) =  DY2/DET
   RDY(1) = -DX2/DET
   RDX(2) = -DY1/DET
   RDY(2) =  DX1/DET

!     in case of spherical coordinates determine cos of latitude
!     note: latitude is in degrees

   IF (KSPHER.GT.0) THEN
      DO IC = 1, ICMAX
         IF ( KCGRD(IC).EQ.1 ) CYCLE   ! if point is not valid, then cy
         COSLAT(IC) =&
         &COS(DEGRAD*(YCGRID(IXCGRD(IC),IYCGRD(IC))+YOFFS))
      ENDDO
      DO IXY = 1, 2
         RDY(IXY) = RDY(IXY) / LENDEG
         RDX(IXY) = RDX(IXY) / (COSLAT(1) * LENDEG)
      ENDDO
   ENDIF

   IF (TESTFL .AND. ITEST .GE. 30) THEN
      WRITE(PRINTF,"(' ...POINTS IN STENCIL IN SUBROUTINE SWGEOM...', /,'Point: IC, Ix, Iy, INDEX, Xc, Yc')")
      DO IC = 1, 3
         WRITE(PRINTF,"(3(1X,I4),3X,I5,5X,F10.2,4X,F10.2)") IC, IXCGRD(IC)+MXF-1, IYCGRD(IC)+MYF-1,&
         &KCGRD(IC),&
         &XCGRID(IXCGRD(IC),IYCGRD(IC)),YCGRID(IXCGRD(IC),IYCGRD(IC))
      ENDDO
      WRITE(PRINTF,"(' DET, RDX1, RDX2, RDY1, RDY2',/, 5(E10.4,1X))") DET,RDX(1),RDX(2),RDY(1),RDY(2)
   ENDIF

   RETURN
end subroutine SWGEOM

!******************************************************************

SUBROUTINE SWPSEL(SWPDIR    ,           IDCMIN    ,&
&IDCMAX    ,CAX       ,&
&CAY       ,ANYBIN    ,&
&ISCMIN    ,&
&ISCMAX    ,IDTOT     ,ISTOT     ,&
&IDDLOW    ,IDDTOP    ,ISSTOP    ,&
&DEP2      ,UX2       ,UY2       ,&
&SPCDIR    ,RDX       ,RDY       ,&
&KGRPNT&
&)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!******************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   USE M_PARALL

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
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     30.82: IJsbrand Haagsma
!     33.09, 40.00, 40.13: Nico Booij
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     20.44, Sep. 96: Subroutine completely reorganised subroutine has new name
!                     instead of COUNT
!     32.02, Feb. 98: Introduced 1D-version
!     40.00, July 98: common swcomm3 introduced, argument list changed
!     30.82, Oct. 98: Updated description of several variables
!     33.09         : spherical ccordinates introduced
!                     repeating x-axis introduced
!     40.03, Nov. 99: error messages (see formats 555 and 556) corrected
!     40.13, Mar. 01: argument KGRPNT added in view of debug output
!                     error severity changed for "blocked" points
!                     "blocked" points written to error points file
!                     comments added
!                     minimal value of ISSTOP is 4 (in view of CGSTAB solver)
!     40.13, July 01: values of DX2 and DY2 corrected in repeating coordinates
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.41, Sep. 04: part concerning computation of geometric quantities
!                     moved to new routine SWGEOM
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     compute the frequency dependent counters in directional space
!     in a situation with a current and without a current.
!     The counters are only computed for the gridpoint
!     considered. This means IC = 1 (see loop with CALL for ICCODE
!     function)
!
!  3. METHOD
!
!     In absence of a current the fully 360 degrees sector is
!     subdivided in 4 sectors of 90 degrees each.
!
!     In presence of a current this is not the case anymore. The
!     counters of the directional space are frequency dependent.
!     It is first determined which bins have to taken into account
!     for a particular sweep (unconditionally stable for a specific
!     sector). To which sector a bin belongs is determined by its
!     propagation velocity Cx and Cy.
!
!     For the first sweep, all the bins with a positive propagation
!     velocity Cx and Cy have to taken into account.
!     For one particular frequency IS:
!
!
!                           #
!                        -  #  +     +
!                    -      #              +     CAX, CAY > 0
!               -           #      |
!                    IDCMAX # ..*..*..*          +
!            -              #\.....|.....*
!                          *#  \...|.......*       +
!          -                #    \.|........
!                       --*-#------O--------*--     +
!                           #      | \......
!          -               *#      |   \...*        +
!     # # # # # # # # # # # # # # # # # #\# # # # # # # # #
!                           #  *   *   *  IDCMIN
!           -               #      |               -
!                           #
!                           #                    -
!              -            #
!                                            -
!                  -                       -
!                          -     -   -
!
!
!     As can be seen from the figure, the minimum and maximum
!     counter of the directional space are determined by the
!     vectorial sum of its groupvelocity and its current
!     velocity, c_g + U. Especially the higher frequencies are
!     modified by the current. The lower frequencies (due to
!     the larger propagation velocity) are less modified by a
!     current.
!
!     In general, we can distinguish 4 cases:
!
!     SWEEP 1:
!                                              ..*..
!                                           *.........*
!                                         |. ............      |   *..*
!            |              |            *|    ..o.......*     | *......*
!            |             *|*            |...... .......      | *......*
!            |           *  |..*          |*...     ....*      |   *..*
!       -----|-----    -*---|---*-   -----|--*-------*--    ---|-------------
!            |           *  |  *          |      *             |
!       * *  |             *|*            |                    |
!     *     *|              |             |                    |
!       * *
!
!     SECTOR = 0        SECTOR = 2        SECTOR = 4       SECTOR = 1
!
!
!     The integer array SECTOR denotes which case is present for
!     a certain frequency:
!
!     0  : no bins belongs to first sweep, no sector lies within the
!          first sweep
!     2  : circle has 2 intersections with sector boundary
!     4  : circle has 4 intersections with sector boundary
!     1  : full circle lies within the first quadrant, all directions
!          have to taken into account
!
!     Furthermore it is detemined whether a certain BIN lies within
!     a specific quadrant. This is denoted by a logical array ANYBIN
!     In case of SECTOR = 4, this array is used to clear the rows
!     in the matrix IMATDA, IMATRA, IMATLA, IMATUA, which do not
!     belong to the first (or other) sweep (see subroutine SOLPRE).
!
!  4. Argument variables
!
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions

   REAL    SPCDIR(MDC,6)

!     INTEGERS:
!     --------------------------------------------------------------
!     IS                Counter of relative frequency band
!     ID                Counter of directional distribution
!     ICUR              Indicator for current
!     ICMAX             Indicator for nearby nodes
!     MSC               Maximum counter of relative frequency in
!                       computational model
!     MDC               Maximum counter of directional distribution in
!                       computational model
!     IDTOT             Maximum value between the lowest and highest
!                       counter in directional space
!     ISTOT             Maximum value between the lowest and highest
!                       counter in frequency space
!     FULCIR            logical: if true, computation on a full circle
!
!     REALS:
!     --------------------------------------------------------------
!     DD       input    Width of directional band
!
!     array's
!     -------
!
!     CAX     3D  propagation velocity
!     CAY     3D  propagation velocity
!     IDCMIN  1D  minimum frequency dependent counter (INTEGER)
!     IDCMAX  1D  maximum frequency dependent counter (INTEGER)
!     ISCMIN  1D  minimum counter in frequency space
!     ISCMAX  1D  maximum counter in frequency space
!     SECTOR  1D  Counter for number enclosed sectors (INTEGER)
!     ANYBIN  2D  Is a certain bin enclosed in a sweep (LOGICAL)
!     SPCDIR  1D  spectral directions
!     RDX,RDY 1D  array  containing spatial derivative coeff
!                 (determined in routine SWGEOM)

   INTEGER, INTENT(IN)  :: KGRPNT(1:MXC,1:MYC)  ! grid addresses

!  6. Local variables
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SWOMPU
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
!     ----------------------------------------------------------
!     If current is on AND the current velocity is not equal zero then
!         compute for every frequency for every sweep the minimum
!         and maximum counters.
!         The minimum counter denotes the conversion from -- to ++
!         The maximum counter denotes the conversion from ++ to --
!
!         ++++++++++ ---------- +++++++++
!                 IDCMAX      IDCMIN
!
!         --------- ++++++++++ ----------
!                IDCMIN      IDCMAX
!
!     else if current is off or Ux=0 m/s and Uy = 0 m/s.
!         Without currents, the directional space counters
!         are constant during the computation, i.e. 4 sectors
!         of 90 degrees each
!     --------------------------------------------------------
!     End of SWPSEL
!     --------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS    ,ID    ,                     SWPDIR,&
   &IDSUM ,IDCLOW,IDCHGH,&
   &IDTOT ,ISTOT ,&
   &IDDLOW,IDDTOP,ISSLOW,ISSTOP,&
   &IDDUM, ISCLOW, ISCHGH, IX, IY ,IC

   REAL      CAXMID,CAYMID,&
   &GROUP, UABS, THDIR

   INTEGER   IDCMIN(MSC)     ,&
   &IDCMAX(MSC)     ,&
   &ISCMIN(MDC)     ,&
   &ISCMAX(MDC)     ,&
   &SECTOR(MSC)

!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  ::  CAX(MDC,MSC,MICMAX)
   REAL  ::  CAY(MDC,MSC,MICMAX)
   REAL  ::  DEP2(MCGRD)         ,&
   &UX2(MCGRD)          ,&
   &UY2(MCGRD)          ,&
   &RDX(MICMAX), RDY(MICMAX)

   LOGICAL   ANYBIN(MDC,MSC)     ,&
   &LOWEST, LOWBIN, HGHBIN

   CALL STRACE (IENT,'SWPSEL')

!     *** initialize array's in theta direction ***

   do IS = 1, MSC
      IDCMIN(IS) = 0
      IDCMAX(IS) = 0
      SECTOR(IS) = 0
      do ID = 1, MDC
         ANYBIN(ID,IS) = .FALSE.
      end do
   end do

!     *** initialize arrays in frequency direction ***

   do ID = 1, MDC
      ISCMIN(ID) = 1
      ISCMAX(ID) = 1
   end do

!     *** set variables ***

   IDTOT  =     1
   ISTOT  =     1
   ISSLOW =  9999
   ISSTOP = -9999

!     --- part of computation of RDXs and RDYs moved to routine SWGEOM
!
!     *** For curvilinear version we do not distinguish if      ***
!     *** there is current or not, to know if certain bin       ***
!     *** belongs to certain sweep              VER. 30.21      ***
!
!     *** calculate minimum and maximum counters in theta space ***
!     *** if a current is present: IDCMIN and IDCMAX            ***
!
!     *** DO LOOP totally organized for curvilinear 30.21       ***

   do IS = 1, MSC
      IDCLOW  = 0
      IDCHGH  = 0
      IDSUM   = 0
      DO ID = 1, MDC
         IF (IS .EQ. 1 .OR. ICUR .GT. 0) THEN
            CAXMID = CAX(ID,IS,1)*RDX(1) + CAY(ID,IS,1)*RDY(1)
            CAYMID = CAX(ID,IS,1)*RDX(2) + CAY(ID,IS,1)*RDY(2)
            IF (CAXMID .GE. 0. .AND. CAYMID .GE. 0.) THEN
               ANYBIN(ID,IS) = .TRUE.
               IDSUM = IDSUM + 1
               ISSLOW = MIN(IS,ISSLOW)
               ISSTOP = MAX(IS,ISSTOP)
            ENDIF
            IF (TESTFL .AND. ITEST .GE. 190)&
            &WRITE(PRINTF,"( ' IS ID CXM CYM ANYBIN :',2(1X,I4),2(1X,E11.4),L2)") IS,ID,CAXMID,CAYMID,ANYBIN(ID,IS)
         ELSE
!           no current: if bin IS=1 is in sweep, all with same ID are
            ANYBIN(ID,IS) = ANYBIN(ID,1)
            IF (ANYBIN(ID,1)) THEN
               IDSUM = IDSUM + 1
               ISSTOP = MAX(IS,ISSTOP)
            ENDIF
         ENDIF
      ENDDO

!       determine boundaries of sector and array SECTOR

      do ID = 1, MDC
         LOWBIN = .FALSE.
         HGHBIN = .FALSE.
         IF (ANYBIN(ID,IS)) THEN
!           check if this active bin is a lower Theta-boundary
            IF ( ID .EQ. 1 ) THEN
               IF (FULCIR) THEN
                  IF (.NOT.ANYBIN(MDC,IS)) LOWBIN = .TRUE.
               ELSE
                  LOWBIN = .TRUE.
               ENDIF
            ELSE
               IF (.NOT.ANYBIN(ID-1,IS)) LOWBIN = .TRUE.
            ENDIF
!           check if this active bin is a higher Theta-boundary
            IF ( ID .EQ. MDC ) THEN
               IF (FULCIR) THEN
                  IF (.NOT.ANYBIN(1,IS)) HGHBIN = .TRUE.
               ELSE
                  HGHBIN = .TRUE.
               ENDIF
            ELSE
               IF (.NOT.ANYBIN(ID+1,IS)) HGHBIN = .TRUE.
            ENDIF
         END IF
         IF (LOWBIN) THEN
            SECTOR(IS) = SECTOR(IS) + 1
            IDCLOW = ID
         ENDIF
         IF (HGHBIN) THEN
            SECTOR(IS) = SECTOR(IS) + 1
            IDCHGH = ID
         ENDIF
      end do
!       check value of SECTOR
      IF (SECTOR(IS).EQ.1 .OR. SECTOR(IS).EQ.3) WRITE (PRTEST, "(' error SWPSEL directions ', 6I6)")&
      &SWPDIR, IS, SECTOR(IS), IDSUM, IDCLOW, IDCHGH
!        *** set the minimum and maximum counters for a sweep ***

      IF ( IDSUM .EQ. MDC ) THEN
         IF (FULCIR .AND. SECTOR(IS).NE.0) WRITE (PRTEST, "(' error SWPSEL directions ', 6I6)")&
         &SWPDIR, IS, SECTOR(IS), IDSUM, IDCLOW, IDCHGH
         IDCMIN(IS) = 1
         IDCMAX(IS) = MDC
         SECTOR(IS) = 1
      ELSE IF ( IDSUM .EQ. 0 ) THEN
!         for this IS there are no active bins
         IF (SECTOR(IS).NE.0) WRITE (PRTEST, "(' error SWPSEL directions ', 6I6)") SWPDIR, IS,&
         &SECTOR(IS), IDSUM, IDCLOW, IDCHGH
!         new values assigned because old ones cause problems in SWSNL2
         IDCMIN(IS) = 9
         IDCMAX(IS) = -9
         SECTOR(IS) = 0
      ELSE
         IF ( IDCLOW .GT. IDCHGH ) IDCLOW = IDCLOW - MDC
         IDCMIN(IS) = IDCLOW
         IDCMAX(IS) = IDCHGH
      END IF

!       *** if 4 sectors are present then set counters ***

      IF ( SECTOR(IS) .GT. 2 ) THEN
         IDCMIN(IS) = 1
         IDCMAX(IS) = MDC
      END IF

   end do

!     *** calculate minimum and maximum counters in frequency ***
!     *** space if a current is present: ISCMIN and ISCMAX    ***

   IDDLOW =  9999
   IDDTOP = -9999
   DO IS = 1 , MSC
      IF ( SECTOR(IS) .GT. 0 ) THEN
         IDDLOW = MIN ( IDDLOW , IDCMIN(IS) )
         IDDTOP = MAX ( IDDTOP , IDCMAX(IS) )
      END IF
   ENDDO

!     *** Determine counters for a certain sweep ***

   do IDDUM = IDDLOW, IDDTOP
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      LOWEST = .TRUE.
      do IS = 1, MSC
         IF (ANYBIN(ID,IS)) THEN
            IF ( LOWEST ) THEN
               ISCLOW = IS
               LOWEST = .FALSE.
            ENDIF
            ISCHGH = IS
         END IF
      end do

!       *** set the minimum and maximum counters in arrays ***

      IF (.NOT.LOWEST) THEN
         ISCMIN(ID) = ISCLOW
         ISCMAX(ID) = ISCHGH
         IF (ISCMIN(ID).LT.ISSLOW) WRITE (PRINTF,*)&
         &' error SWPSEL, ISSLOW=', ISSLOW, 'ISCMIN=', ISCMIN(ID),&
         &' for ID=', ID
         IF (ISCMAX(ID).GT.ISSTOP) WRITE (PRINTF,*)&
         &' error SWPSEL, ISSTOP=', ISSTOP, 'ISCMAX=', ISCMAX(ID),&
         &' for ID=', ID
      ELSE
!         *** no frequencies fall within the sweep ***
         ISCMIN(ID) = 0
         ISCMAX(ID) = 0
      ENDIF

   end do

!     *** calculate the maximum number of counters in both ***
!     *** directional space and frequency space            ***

   IF (IDDLOW.NE.9999) THEN
      IF (IDDTOP.EQ.-9999) WRITE (PRTEST, "(' error SWPSEL min max dir ', 5I7)") IDDLOW, IDDTOP
      IDTOT = ( IDDTOP - IDDLOW ) + 1
      IF (ICUR .EQ. 1) THEN
         IF (IDTOT.LT.3) THEN
            IDDTOP = IDDTOP + 1
            IF (IDTOT.EQ.1) IDDLOW = IDDLOW - 1
            IDTOT = 3
         ENDIF
      ENDIF
   ELSE
      IF (IDDTOP.NE.-9999) WRITE (PRTEST, "(' error SWPSEL min max dir ', 5I7)") IDDLOW, IDDTOP
      IDTOT = 0
   ENDIF

   IF (ISSLOW.NE.9999) THEN
      IF (ITEST.GE.20) THEN
         IF (ISSLOW.NE.1 .OR. ISSTOP.EQ.-9999)&
         &WRITE (PRTEST, "(' error SWPSEL in:', 2I5,', min max freq ', 5I7)") IXCGRD(1)-1, IYCGRD(1)-1, ISSLOW, ISSTOP
      ENDIF
      ISSLOW = 1
!       minimal value of ISSTOP is 4 (or MSC if MSC<4)
      IF (ICUR.GT.0) ISSTOP = MAX(MIN(4,MSC),ISSTOP)
      ISTOT = ( ISSTOP - ISSLOW ) + 1
   ELSE
      IF (ISSTOP.NE.-9999) WRITE (PRTEST, "(' error SWPSEL in:', 2I5,', min max freq ', 5I7)") IXCGRD(1)-1,&
      &IYCGRD(1)-1, ISSLOW, ISSTOP
      ISTOT = 0
      IF (IDTOT.NE.0) WRITE (PRTEST, "(' error SWPSEL in:', 2I5,' min max freq dir ', 5I7)") IXCGRD(1)-1,&
      &IYCGRD(1)-1, ISSLOW, ISSTOP, IDDLOW, IDDTOP
   ENDIF

!     *** check if IDTOT is less then MDC ***

   IF ( IDTOT .GT. MDC ) THEN
      IDDLOW = 1
      IDDTOP = MDC
      IDTOT  = MDC
   END IF

!     *** check if the lowest frequency is not blocked !    ***
!     *** this can occur in real cases if the depth is very ***
!     *** small and the current velocity is large           ***
!     *** the propagation velocity Cg = sqrt (gd) < U       ***

   IF (ICUR .EQ. 1 .AND. FULCIR .AND.&
   &ISSLOW.NE.1 .AND. ISSLOW.NE.9999) THEN
      CALL MSGERR (2,'The lowest freqency is blocked')
      WRITE (PRINTF, "(A, 2I4, A, F6.2, A, 2F6.2)") ' at point:', IXCGRD(1)+MXF-2,&
      &IYCGRD(1)+MYF-2,&
      &' dep=', DEP2(KCGRD(1)),&
      &'  U=', UX2(KCGRD(1)), UY2(KCGRD(1))
      IF (ITEST.GE.10) THEN
         WRITE (PRINTF, "(A, 6I8,A,I1)") ' spectral limits:', ISTOT, ISSLOW,&
         &ISSTOP, IDTOT, IDDLOW, IDDTOP, ' sweep=',SWPDIR
         IF (ITEST.GE.60) THEN
            IF (IXCGRD(1).GT.1 .AND. IXCGRD(1).LT.MXC .AND.&
            &IYCGRD(1).GT.1 .AND. IYCGRD(1).LT.MYC) THEN
               WRITE (PRINTF, *) ' surrounding points'
               DO IY=-1,1
                  WRITE (PRINTF, "(1X, 3I6, 3(' | ', 3F9.2))")&
                  &(KGRPNT(IXCGRD(1)+IX,IYCGRD(1)+IY), IX=-1,1),&
                  &(DEP2(KGRPNT(IXCGRD(1)+IX,IYCGRD(1)+IY)), IX=-1,1),&
                  &(UX2(KGRPNT(IXCGRD(1)+IX,IYCGRD(1)+IY)), IX=-1,1),&
                  &(UY2(KGRPNT(IXCGRD(1)+IX,IYCGRD(1)+IY)), IX=-1,1)
               ENDDO
            ENDIF
         ENDIF
      ENDIF
!       write this point to ERRPTS file (BLOCKed option)
      IF (ERRPTS.GT.0.AND.IAMMASTER) THEN
         WRITE(ERRPTS,"(I4, 1X, I4, 1X, I2)") IXCGRD(1)+MXF-1, IYCGRD(1)+MYF-1, 3
      END IF
      IC = 1
      GROUP = SQRT ( GRAV * DEP2(KCGRD(IC)) )
      UABS  = SQRT ( UX2(KCGRD(IC))**2 + UY2(KCGRD(IC))**2 )
      IF ( UABS .GT. GROUP ) THEN
         WRITE(PRINTF,"(' warning, at point:',2I4,' |U|=',F8.2,' > Cg=',F8.2)") IXCGRD(IC)-1, IYCGRD(1)-1, UABS, GROUP
      ENDIF
   ENDIF

!     *** test output ***

   IF ( TESTFL .AND. ITEST .GE. 30 ) THEN
      IC = 1
      WRITE (PRTEST,"(' subr SWPSEL: Point SWPDIR ICUR :',3I5 )") KCGRD(IC),SWPDIR,ICUR
      WRITE (PRTEST,"(' IDDLOW IDDTOP ISSLOW ISSTOP:',4I4 )") IDDLOW, IDDTOP ,ISSLOW, ISSTOP
      WRITE (PRTEST,"(' IDTOT ISTOT :',4I4 )") IDTOT , ISTOT
      IF (ITEST.GE.120) THEN
         WRITE(PRTEST,*) ' Counters in directional space '
         WRITE(PRTEST,*) '       IS     IDCMIN  IDCMAX  SECTOR'
         DO IS = ISSLOW, ISSTOP
            WRITE(PRTEST,"(2X,I5,3X,3I8)") IS, IDCMIN(IS), IDCMAX(IS) , SECTOR(IS)
         ENDDO
         WRITE(PRTEST,*) ' Counters in frequency space '
         WRITE(PRTEST,*) '       ID     ISCMIN  ISCMAX  THETA'
         DO IDDUM = IDDTOP, IDDLOW, -1
            ID = MOD ( IDDUM - 1 + MDC, MDC) + 1
            THDIR = SPCDIR(ID,1) * 180. / PI
            WRITE(PRTEST,"(2X,I5,3X,2I8,3X,F8.2)") ID, ISCMIN(ID), ISCMAX(ID), THDIR
         ENDDO
         WRITE(PRTEST,*)
      ENDIF
      IF (IDTOT.GT.0) THEN
         IF (ITEST.GE.90) THEN
            WRITE(PRTEST,"(' Active bins in spectral space -> ID: ', I3,' to ',I3)") IDDLOW, IDDTOP
            DO IDDUM = IDDTOP+1, IDDLOW-1, -1
               ID = MOD ( IDDUM - 1 + MDC, MDC) + 1
               WRITE(PRTEST,"(I4,25L3)")&
               &ID, (ANYBIN(ID,IS),IS=ISSLOW, MIN(ISSTOP,25))
            ENDDO
            WRITE(PRTEST,"(6X,'1',9X,5(I3,12X))")(IS, IS=ISSLOW+4, MIN(ISSTOP,25), 5 )
            WRITE(PRTEST,*)
         ENDIF
      ELSE
         WRITE(PRTEST,"(' No active bins in sweep', I2)") SWPDIR
      ENDIF
      IF ( ICUR .EQ. 0 ) THEN
         WRITE (PRTEST,"(' SWPSEL: IDDLOW IDDTOP :',5(1X,I3))") IDDLOW, IDDTOP
      END IF
   END IF

!     End of the subroutine SWPSEL

   RETURN
end subroutine SWPSEL

!****************************************************************

SUBROUTINE SPROXY (CAX        ,&
&CAY        ,CGO        ,ECOS       ,&
&ESIN       ,UX2        ,UY2        ,&
&SWPDIR     ,DIFFR&
&)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(diffraction_state_t), INTENT(IN) :: DIFFR


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
!     1. UPDATE
!
!        40.13, Oct. 01: loop over IC now inside this subroutine
!        40.21, Aug. 01: adaption of velocities in case of diffraction
!        40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!     2. PURPOSE
!
!        computes the propagation velocities of energy in X-, Y-
!        -space, i.e., CAX, CAY, in the presence or absence of
!        currents, for the action balance equation.
!
!        The propagation velocities are computed for the fully 360
!        degrees sector.
!
!     3. METHOD
!
!        The next equation are calculated:
!
!              @X     _
!        CAX = -- = n C cos (id) + Ux  = CGO cos(id) + Ux
!              @T
!
!              @Y     _
!        CAY = -- = n C sin(id)  + Uy  = CGO sin(id) + Uy
!              @T
!                                                         _
!     4. PARAMETERLIST
!
!        IC       Dummy variable: ICode gridpoint:
!                 IC = 1  Top or Bottom gridpoint
!                 IC = 2  Left or Right gridpoint
!                 IC = 3  Central gridpoint
!                Whether which value IC has, depends of the sweep
!                If necessary ic can be enlarged by increasing
!                the array size of ICMAX
!        IX      Counter of gridpoints in x-direction
!        IY      Counter of gridpoints in y-direction
!        IS      Counter of relative frequency band
!        ID      Counter of directional distribution
!        ICUR    Indicator for current
!        ICMAX   Maximum array size for the points of the molecule
!        MXC     Maximum counter of gridppoints in x-direction
!        MYC     Maximum counter of gridppoints in y-direction
!        MSC     Maximum counter of relative frequency
!        MDC     Maximum counter of spectral directions
!
!        REAL:
!        ----
!        COEF    auxiliary coefficient
!        VLSINH  value of the SINH for a certain value of 2KD
!
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        CAX    3D    Wave transport velocity in x-dirction, function of
!                     (ID,IS,IC)
!        CAY    3D    Wave transport velocity in y-dirction, function of
!                     (ID,IS,IC)
!        CGO    2D    group velocity
!        DEP2   2D    (Nonstationary case) depth as function of X and Y
!                     at time T+DIT
!        ECOS   1D    Represent the values of cos(d) of each spectral
!                     direction
!        ESIN   1D    Represent the values of sin(d) of each spectral
!                     direction
!        KWAVE  2D    wavenumber as function of the relative frequency S
!        UX2    2D    X-component of current velocity of X and Y at
!                     time T+1
!        UY2    2D    Y-component of current velocity of X and Y at
!                     time T+1
!
!     5. SUBROUTINES CALLING
!
!        ---
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!     8. REMARKS
!
!     9. STRUCTURE
!
!       ******************************************************************
!       *  attention! in the action balance equation the term
!       *  dx
!       *  -- = CGO + U = CX  with x, CGO, U and CX vectors
!       *  dt
!       *  is in the literature the term dx/dt often indicated
!       *  with CX and CY in the action balance equation.
!       *  In this program we use:    CAX = CGO + U
!       ******************************************************************
!
!   ------------------------------------------------------------
!   If depth is negative ( DEP(IX,IY) <= 0), then,
!     For every point in S and D-direction do,
!       Give propagation velocities default values :
!       CAX(ID,IS,IC)     = 0.   {propagation velocity of energy in X-dir.}
!       CAY(ID,IS,IC)     = 0.   {propagation velocity of energy in Y-dir.}
!     ---------------------------------------------------------
!   Else if current is on (ICUR > 0) then,
!     For every point in S and D-direction do,  {using the output of SWAPAR}
!       S = logaritmic distributed via LOGSIG
!       Compute propagation velocity in X-direction:
!
!               1    K(IS,IC)DEP2(IX,IY)      S cos(D)
!       CAX = ( - + ------------------------) --------- + UX2(IX,IY)
!               2   sinh 2K(IS,IC)DEP2(IX,IY) |K(IS,IC)|
!
!       ------------------------------------------------------
!       Compute propagation velocity in Y-direction:
!
!               1    K(IS,IC)DEP2(IX,IY)      S sin(D)
!       CAY = ( - + ------------------------) -------- + UY2(IX,IY)
!               2   sinh 2K(IS,IC)DEP2(IX,IY) |K(IS,IC)|
!
!       ------------------------------------------------------
!   Else if current is not on (ICUR = 0)
!     For every point in S and D-direction do
!       S = logarithmic distributed via LOGSIG
!       Compute propagation velocity in X-direction:
!
!               1    K(IS,IC)DEP2(IX,IY)        S cos(D)
!       CAX = ( - + ------------------------) ----------
!               2   sinh 2K(IS,IC)DEP2(IX,IY)  |K(IS,IC)|
!
!       ------------------------------------------------------
!       Compute propagation velocity in Y-direction:
!
!               1    K(IS,IC)DEP2(IX,IY)        S sin(D)
!       CAY = ( - + ------------------------) ----------
!               2   sinh 2K(IS,IC)DEP2(IX,IY)  |K(IS,IC)|
!
!     ----------------------------------------------------------
!   End IF
!   ------------------------------------------------------------
!   End of SPROXY
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER  IP, IC    ,IS    ,ID    ,SWPDIR

   REAL     CAX(MDC,MSC,ICMAX)          ,&
   &CAY(MDC,MSC,ICMAX)          ,&
   &CGO(MSC,ICMAX)              ,&
   &ECOS(MDC)                   ,&
   &ESIN(MDC)                   ,&
   &UX2(MCGRD)                ,&
   &UY2(MCGRD)

   IF (LTRACE) CALL STRACE (IENT,'SPROXY')

   IF (TESTFL .AND. ITEST .GE. 5 ) WRITE (PRTEST,"(' Start SPROXY ', 4I5)")SWPDIR,KCGRD(1)

   DO IC = 1, ICMAX
      IF ( KCGRD(IC) .LE. 1 ) THEN
         do IS = 1, MSC
            do ID = 1 , MDC
               CAX(ID,IS,IC) = 0.
               CAY(ID,IS,IC) = 0.
            end do
         end do
      ELSE

         do IS = 1, MSC
            do ID = 1, MDC
               CAX(ID,IS,IC) = CGO(IS,IC) * ECOS(ID)
               CAY(ID,IS,IC) = CGO(IS,IC) * ESIN(ID)
            end do
         end do

!         --- adapt the velocities in case of diffraction

         IF (IDIFFR.EQ.1 .AND. PDIFFR(3).NE.0.) THEN
            do IS = 1, MSC
               do ID = 1 ,MDC
                  CAX(ID,IS,IC) = CAX(ID,IS,IC)*diffr%param(KCGRD(IC))
                  CAY(ID,IS,IC) = CAY(ID,IS,IC)*diffr%param(KCGRD(IC))
               end do
            end do
         END IF

!         --- ambient currents added

         IF (ICUR.EQ.1)  THEN
            do IS = 1, MSC
               do ID = 1, MDC
                  CAX(ID,IS,IC) = CAX(ID,IS,IC) + UX2(KCGRD(IC))
                  CAY(ID,IS,IC) = CAY(ID,IS,IC) + UY2(KCGRD(IC))
               end do
            end do
         END IF

      ENDIF

!       *** test output ***

      IF ( IC .EQ. 1 .AND. TESTFL .AND. ITEST .GE. 120 ) THEN
         DO IP = 1, ICMAX
            WRITE(PRINTF,"(' SPROXY: IC INDEX UX2 UY2 :', 2I5, ' UX,UY:', 2(1X,E12.4))") IP,KCGRD(IP),&
            &UX2(KCGRD(IP)),UY2(KCGRD(IP))
         ENDDO
         IF (ITEST.GE.220) THEN
            do IS = 1, MSC
               do ID = 1, MDC
                  WRITE(PRINTF,"(' IS ID <CAX CAY>:',2I4,10(1X,2E11.4))") IS, ID,&
                  &(CAX(ID,IS,IP), CAY(ID,IS,IP), IP=1,ICMAX)
               end do
            end do
         ENDIF
      ENDIF
   ENDDO            ! end loop over IC

   RETURN
end subroutine SPROXY

!****************************************************************

SUBROUTINE SPROSD (SPCSIG     ,KWAVE      ,CAS        ,&
&CAD        ,CGO        ,&
&DEP2       ,DEP1       ,ECOS       ,&
&ESIN       ,UX2        ,UY2        ,&
&COSCOS     ,SINSIN     ,SINCOS     ,&
&RDX        ,RDY        ,&
&CAX        ,CAY        ,&
&XCGRID     ,YCGRID     ,&
&IDDLOW     ,IDDTOP     ,DIFFR&
&)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_coordinate_offset
   USE swan_run_mode
   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE M_PARALL
   USE SwanIEM, ONLY: ntf, dfiem, sflog

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(diffraction_state_t), INTENT(IN) :: DIFFR


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
!     30.72: IJsbrand Haagsma
!     30.80: Nico Booij
!     40.03: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.14: Annette Kieftenburg
!     40.21: Agnieszka Herman
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.59: Erick Rogers
!     40.61: John Warner
!     41.06: Gerbrant van Vledder
!     41.35: Casey Dietrich
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.80, Nov. 98: Provision for limitation on Ctheta (refraction)
!     30.80, Aug. 99: SWCOMM3.INC included
!     30.80, Sep. 99: SWCOMM2.INC included, limitation modified
!     40.03, Dec. 99: for directions outside the current sweep the depth and
!                     current gradients are computed using the gradient
!                     proper side of the grid point.
!                     argument KGRPNT added.
!                     argument IC removed (is always 1)
!                     argument DT removed, shared time state used
!                     code completely revised
!     40.02, Jan. 00: Introduction limiter dependent on Cx, Cy, Dx and Dy
!     40.02, Sep. 00: Corrected order of handling sweeps
!     40.02, Sep. 00: Limiter on refraction only activated when IREFR=-1
!     40.14, Nov. 00: Land points excluded (bug fix)
!     40.21, Aug. 01: adaption of velocities in case of diffraction
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.59, Aug. 07: replace upwind scheme with centered scheme;
!                     replace @h/@x method for computation of CAD with @C/@x method
!                     limitation procedure removed
!                     sweeping procedure removed
!     40.61, Dec. 06: correction DO loop 60 (IDCMIN, IDCMAX -> IDDLOW,IDDTOP)
!     41.06, Mar. 09: add option of limitation of velocity in theta-direction
!     41.35, Mar. 12: add option of limitation on csigma and ctheta
!
!  2. Purpose
!
!     computes the propagation velocities of energy in S- and
!     D-space, i.e., CAS, CAD, in the presence or absence of
!     currents, for the action balance equation.
!
!  3. Method
!
!     The next equation are solved numerically
!
!           @S   @S   @D   _     @D   @D          _   @U
!     CAS = -- = -- [ -- + U . ( -- + --) ] - CGO K . --
!           @T   @D   @T         @X   @Y              @s
!
!           with:   @S       KS
!                   -- =  ---------
!                   @D    sinh(2KD)
!
!           @D      Cg     @C         @C           @Ux   @Uy
!     CAD = -- = ------- [ --sin(D) - --cos(D)] + [--- - ---] *
!           @T      C      @X         @Y            @X   @Y
!
!                        @Uy               @Ux
!     * sin(D)cos(D) +   ---sin(D)sin(D) - ---cos(D)cos(D)
!                        @X                @Y
!
!     @C/@x appr by:   0.5*RDX(1) * (DEP(KCGRD(5)) - DEP(KCGRD(2)))
!                    + 0.5*RDX(2) * (DEP(KCGRD(4)) - DEP(KCGRD(3)))
!     @C/@y appr by:   0.5*RDY(1) * (DEP(KCGRD(5)) - DEP(KCGRD(2)))
!                    + 0.5*RDY(2) * (DEP(KCGRD(4)) - DEP(KCGRD(3)))
!     etc.
!
!  4. Argument variables
!
!     IDDLOW: minimum direction that is propagated within a sweep
!     IDDTOP: maximum direction that is propagated within a sweep

   INTEGER, INTENT(IN) :: IDDLOW, IDDTOP

!     CAS   : Wave transport velocity in S-direction, function of (ID,IS,IC)
!     CAD   : Wave transport velocity in D-dirctiion, function of (ID,IS,IC)
!     CAX   : Wave transport velocity in X-direction, function of (ID,IS,IC)
!     CAY   : Wave transport velocity in Y-direction, function of (ID,IS,IC)
!     CGO   : Group velocity as function of X and Y and sigma in the
!             direction of wave propagation in absence of currents
!     DEP1  : Depth as function of X and Y at time T
!     DEP2  : (Nonstationary case) depth as function of X and Y at time
!     ECOS  : Represent the values of cos(d) of each spectral direction
!     ESIN  : Represent the values of sin(d) of each spectral direction
!     KWAVE : wavenumber as function of the relative frequency sigma
!     SPCSIG: Relative frequencies in computational domain in sigma-space
!     UX2   : X-component of current velocity of X and Y at time T+1
!     UY2   : Y-component of current velocity of X and Y at time T+1
!     XCGRID: x-coordinate of comput. grid points
!     YCGRID: y-coordinate of comput. grid points

   REAL  :: SPCSIG(MSC)
   REAL  :: XCGRID(MXC,MYC), YCGRID(MXC,MYC)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAS(MDC,MSC,MICMAX)
   REAL  :: CAD(MDC,MSC,MICMAX)
   REAL  :: CAX(MDC,MSC,MICMAX)
   REAL  :: CAY(MDC,MSC,MICMAX)
   REAL  :: CGO(MSC,MICMAX)
   REAL  :: DEP2(MCGRD)                 ,&
   &DEP1(MCGRD)                 ,&
   &ECOS(MDC)                   ,&
   &ESIN(MDC)                   ,&
   &COSCOS(MDC)                 ,&
   &SINSIN(MDC)                 ,&
   &SINCOS(MDC)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: KWAVE(MSC,MICMAX)
   REAL  :: UX2(MCGRD)                  ,&
   &UY2(MCGRD)                  ,&
   &RDX(MICMAX)                 ,&
   &RDY(MICMAX)

!     variables from common
!
!        ICUR    Indicator for current
!        NSTATC  Indicator if computation is stationary or not
!        MXC     Maximum counter of gridppoints in x-direction
!        MYC     Maximum counter of gridppoints in y-direction
!        MSC     Maximum counter of relative frequency
!        MDC     Maximum counter of spectral directions
!        DYNDEP  if True depths vary with time
!        DT      Time step
!        RDTIM   1/DT
!
!     local integer variables :

   INTEGER, SAVE :: IENT = 0
   INTEGER  :: IS                        ! Counter of relative freque
   INTEGER  :: ID ,ID1   ,ID2            ! Counter of directions  !
   INTEGER  :: IDDUM                     ! aux. counter of directions
   INTEGER  :: KCG1                      ! grid address of the active
   INTEGER  :: KCG2  ,KCG3 , KCG4, KCG5  ! grid addresses of neighbou
   INTEGER  :: IX1                       ! IX1   Counter of gridpoint
   INTEGER  :: IY1                       ! IY1   Counter of gridpoint

!     local real variables :

   REAL  :: KD1                                    ! KD1           wa
   REAL  :: VLSINH                                 ! VLSINH        si
   REAL  :: COEF                                   ! COEF          au
   REAL  :: CAST1,CAST2,CAST3,CAST4,CAST5          ! aux. quantities
   REAL  :: CAD_TMP                                ! aux. quantity to
   REAL  :: DHDX   ,DHDY                           ! depth gradient
   REAL  :: DCDX   ,DCDY                           ! celerity gradien
   REAL  :: DUXDX ,DUXDY ,DUYDX ,DUYDY             ! current velocity
   REAL  :: DLOC1, DLOC2, DLOC3, DLOC4, DLOC5      ! depths at active
   REAL  :: CLOC1, CLOC2, CLOC3, CLOC4, CLOC5      ! depths at active
   REAL  :: CGLOC1                                 ! group velocity a
   REAL  :: KLOC1, KLOC2, KLOC3, KLOC4, KLOC5      ! wavenumbers at a
   REAL  :: UXLOC1, UXLOC2, UXLOC3, UXLOC4, UXLOC5 ! Ux at active and
   REAL  :: UYLOC1, UYLOC2, UYLOC3, UYLOC4, UYLOC5 ! Uy at active and

   REAL  :: ALPHA                                  ! upper limit of C
   REAL  :: FAC                                    ! a factor ! 41.06
   REAL  :: FAC2                                   ! another factor !
   REAL  :: FRLIM                                  ! frequency range
   REAL  :: PP                                     ! power of the fre

!  8. Remarks
!
!  Motivation for using @C/@x instead of @h/@x :
!  Formulae for computing CAD via @h/@x and @C/@x are identical. However, they differ in result due to numerics.                     40.59
!     Experiments suggest that @C/@x method with coarse resolution yields results that are similar to those                          40.59
!     using @C/@x or @h/@x with high resolution.
!     By contrast, @h/@x method with coarse resolution yields considerably different result.                                         40.59
!  Further, using @C/@x allows for adding refraction by additional variables included in dispersion relation.                        40.59
!     Specifically, non-rigid seafloor (mud) can cause refraction even when water layer thickness (i.e. depths) are uniform.         40.59
!     Obviously, this type of refraction does require the mud to be non-uniform.                                                     40.59
!
!  Motivation for using centered scheme instead of upwind scheme :
!  Similar to above, the difference is only in numerics; experimental results suggest                                                40.59
!     better representation of unknown true solution via centered scheme.                                                            40.59
!  Further, upwind scheme can lead to non-physical asymmetry in CAD, and therefore wave action field                                 40.59
!  Further still, implementation of the upwind scheme in versions such as 40.41 and 40.51 used sweeping to                           40.59
!     keep track of results from neighboring sweeps. This involved large amounts of additional code, which is obviously undesirable. 40.59
!     The utility of this sweeping is not known to this author, since it does not seem to be strictly required by the upwind scheme. 40.59
!     As evidence, note that v30.75 used the upwind scheme, but did not
!
!  Note that since we are using a centered scheme now, we stop before we get to the last grid point.                                 40.59
!  It would be possible to have separate code for falling back to the upwind scheme,                                                 40.59
!  but this may require sweeping, which would mean much additional code, see e.g. code of public release v40.51                      40.59
!
!  Note: RDX and RDY are unavailable for P5-P2 and P4-P3, so we use as approximation, 0.5*RDX(1), etc.                               40.59
!  Experience with implementation of more precise calculations of RDX,RDY, specifically with the SORDUP scheme,                      40.59
!      yielded imperceptible change in results. However, this could be added later if sufficiently motivated.                        40.59
!
!  The depth refraction limitation procedure (i.e. IREFR=-1) has been removed, because it is basically a "dirty fix" which           40.59
!      we hope/expect has been made unnecessary by the other changes here. If we are wrong about this, the "dirty fix" can be        40.59
!      restored, but this is not straightforward, since C is computed from k, which is an input argument not affected by IREFR.      40.59
!      To make this work, the depths would be limited here in SPROSD and k would then need to be calculated                          40.59
!      using the limited depths within SPROSD.
!
!  9. STRUCTURE
!   ------------------------------------------------------------
!       determine celerity and current gradients
!   ------------------------------------------------------------
!   For each frequency do
!       determine auxiliary quantities depending on sigma
!           ----------------------------------------------------
!           using gradients ,  determine
!           Csigma (CAS) and Ctheta (CAD)
!   ------------------------------------------------------------
!   If ITFRE=0
!   Then make values of CAS=0
!   ------------------------------------------------------------
!   If IREFR=0
!   Then make values of CAD=0
!   ------------------------------------------------------------
!
!   10. SOURCE
!
!************************************************************************

   IF (LTRACE) CALL STRACE (IENT,'SPROSD')

   CAST1 = 0.
   CAST2 = 0.
   CAST3 = 0.
   CAST4 = 0.
   CAST5 = 0.

   DHDX = 0.
   DHDY = 0.

   KCG1 = KCGRD(1)
   KCG2 = KCGRD(2)
   KCG3 = KCGRD(3)
   KCG4 = KCGRD(4)
   KCG5 = KCGRD(5)

!     Refraction and frequency shift are not defined for points
!     neighbouring to landpoints

   IF ( (KCG1.EQ.1).OR.(DEP1(KCG1).LE.DEPMIN).OR.&
   &(KCG2.EQ.1).OR.(DEP1(KCG2).LE.DEPMIN).OR.&
   &(KCG3.EQ.1).OR.(DEP1(KCG3).LE.DEPMIN).OR.&
   &(KCG4.EQ.1).OR.(DEP1(KCG4).LE.DEPMIN).OR.&
   &(KCG5.EQ.1).OR.(DEP1(KCG5).LE.DEPMIN) ) THEN
      DO IS = 1, MSC
         DO ID = 1, MDC
            CAD(ID,IS,1) = 0.
            CAS(ID,IS,1) = 0.
         ENDDO
      ENDDO
      RETURN
   ENDIF

   IX1  = IXCGRD(1)
   IY1  = IYCGRD(1)

   DLOC1 = DEP2(KCG1)
   DLOC2 = DEP2(KCG2)
   DLOC3 = DEP2(KCG3)
   DLOC4 = DEP2(KCG4)
   DLOC5 = DEP2(KCG5)

   IF ( ICUR .EQ. 1 ) THEN

      UXLOC1 = UX2(KCG1)
      UXLOC2 = UX2(KCG2)
      UXLOC3 = UX2(KCG3)
      UXLOC4 = UX2(KCG4)
      UXLOC5 = UX2(KCG5)

      UYLOC1 = UY2(KCG1)
      UYLOC2 = UY2(KCG2)
      UYLOC3 = UY2(KCG3)
      UYLOC4 = UY2(KCG4)
      UYLOC5 = UY2(KCG5)

   ENDIF

!     *** test output ***

   IF (TESTFL .AND. ITEST .GE. 100 ) THEN
      WRITE(PRINTF, "(' test SPROSD, location:',2I5,2e12.4,', depth:',F9.2)") IX1+MXF-2, IY1+MYF-2,&
      &XCGRID(IX1,IY1)+XOFFS, YCGRID(IX1,IY1)+YOFFS,&
      &DLOC1
   ENDIF

!     *** set some terms = 0 ***

   DUXDX = 0.
   DUYDY = 0.
   DUYDX = 0.
   DUXDY = 0.

!     *** compute the derivatives of the depth and the current velocity
!
! old upwind scheme:   1DX USING P1-P2
!                      1DY USING P1-P3
! new centered scheme: 2DX USING P5-P2
!                      2DY USING P4-P3
!
!     *** @D/@X ***
!      DHDX = 1.0*RDX(1) * (DLOC1-DLOC2) + 1.0*RDX(2) * (DLOC1-DLOC3) !
   DHDX = 0.5*RDX(1) * (DLOC5-DLOC2) + 0.5*RDX(2) * (DLOC4-DLOC3) ! c
!     *** @D/@Y ***
!      DHDY = 1.0*RDY(1) * (DLOC1-DLOC2) + 1.0*RDY(2) * (DLOC1-DLOC3) !
   DHDY = 0.5*RDY(1) * (DLOC5-DLOC2) + 0.5*RDY(2) * (DLOC4-DLOC3) ! c

   IF ( ICUR .EQ. 1 .AND. IQCM.EQ.0 ) THEN  !           *** current i

!     *** @Ux/@X ***
!         DUXDX = 1.0*RDX(1) * (UXLOC1 - UXLOC2) +
!     &           1.0*RDX(2) * (UXLOC1 - UXLOC3)
      DUXDX = 0.5*RDX(1) * (UXLOC5 - UXLOC2) +&
      &0.5*RDX(2) * (UXLOC4 - UXLOC3)
!     *** @Ux/@Y ***
!         DUXDY = 1.0*RDY(1) * (UXLOC1 - UXLOC2) +
!     &           1.0*RDY(2) * (UXLOC1 - UXLOC3)
      DUXDY = 0.5*RDY(1) * (UXLOC5 - UXLOC2) +&
      &0.5*RDY(2) * (UXLOC4 - UXLOC3)
!     *** @Uy/@X ***
!         DUYDX = 1.0*RDX(1) * (UYLOC1 - UYLOC2) +
!     &           1.0*RDX(2) * (UYLOC1 - UYLOC3)
      DUYDX = 0.5*RDX(1) * (UYLOC5 - UYLOC2) +&
      &0.5*RDX(2) * (UYLOC4 - UYLOC3)
!     *** @Uy/@Y ***
!         DUYDY = 1.0*RDY(1) * (UYLOC1 - UYLOC2) +
!     &           1.0*RDY(2) * (UYLOC1 - UYLOC3)
      DUYDY = 0.5*RDY(1) * (UYLOC5 - UYLOC2) +&
      &0.5*RDY(2) * (UYLOC4 - UYLOC3)

      CAST3 = UXLOC1 * DHDX
      CAST4 = UYLOC1 * DHDY

   ELSE     !           *** current is off ***

      DUXDX = 0.
      DUXDY = 0.
      DUYDX = 0.
      DUYDY = 0.
      CAST3 = 0.
      CAST4 = 0.

   ENDIF

!      *** test output ***

   IF (TESTFL .AND. ITEST .GE. 100 ) THEN
      IF (ICUR .EQ. 1) THEN
         WRITE(PRINTF, "(10X, 'UX:',3(1X,F8.3),/, 10X, 'UY:',3(1X,F8.3))") UXLOC1,UXLOC2,UXLOC3,UYLOC1,UYLOC2,UYLOC3
      ENDIF
      WRITE(PRINTF, "(10X, 'RDX etc.:',4(1X,E12.4))") RDX(1),RDX(2),RDY(1),RDY(2)
      WRITE(PRINTF, "(10x, 'DHDX,DHDY:',2(1X,E12.4))") DHDX,  DHDY
   ENDIF

!      *** coefficients for CAS -> function of IX and IY only ***

   IF ( NSTATC.EQ.0 .OR. .NOT.DYNDEP) THEN
      !       *** stationary calculation ***
      CAST2 = 0.
   ELSE
      !       nonstationary depth, CAST2 is @D/@t
      CAST2 = ( DLOC1 - DEP1(KCG1) ) * RDTIM
   ENDIF

   DO IS = 1, MSC

      KLOC1=KWAVE(IS,1)
      KLOC2=KWAVE(IS,2)
      KLOC3=KWAVE(IS,3)
      KLOC4=KWAVE(IS,4)
      KLOC5=KWAVE(IS,5)

      CGLOC1 = CGO(IS,1)

      CLOC1=SPCSIG(IS)/KLOC1
      CLOC2=SPCSIG(IS)/KLOC2
      CLOC3=SPCSIG(IS)/KLOC3
      CLOC4=SPCSIG(IS)/KLOC4
      CLOC5=SPCSIG(IS)/KLOC5

!       upwind scheme
!        DCDX = 1.0*RDX(1) * (CLOC1-CLOC2) + 1.0*RDX(2) * (CLOC1-CLOC3)
!        DCDY = 1.0*RDY(1) * (CLOC1-CLOC2) + 1.0*RDY(2) * (CLOC1-CLOC3)
!       centered scheme
      DCDX = 0.5*RDX(1) * (CLOC5-CLOC2) + 0.5*RDX(2) * (CLOC4-CLOC3)
      DCDY = 0.5*RDY(1) * (CLOC5-CLOC2) + 0.5*RDY(2) * (CLOC4-CLOC3)

!       *** coefficients for CAS -> function of IS only ***

      KD1 = KWAVE(IS,1) * DLOC1
      IF ( KD1 .GT. 30.0 ) KD1 = 30.
      VLSINH = SINH (2.* KD1 )
      COEF   = SPCSIG(IS) / VLSINH   ! also needed for CAD, if @h/@x m
      CAST1  = KWAVE(IS,1) * COEF
      CAST5  = CGO(IS,1) * KWAVE(IS,1)

!       loop over spectral directions

      DO IDDUM = IDDLOW-1, IDDTOP+1 !            40.61 40.03
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

!          *** computation of CAS and CAD ***

         IF ( INT(PNUMS(32)).EQ.0 ) THEN
            CAD_TMP=COEF*(ESIN(ID)*DHDX-ECOS(ID)*DHDY) ! @h/@x method,
         ELSE IF ( INT(PNUMS(32)).EQ.1 ) THEN
            CAD_TMP=(CGLOC1/CLOC1)*(ESIN(ID)*DCDX-ECOS(ID)*DCDY) ! @C/@
!             CAD_TMP=(CGLOC1/KLOC1)*(-1.0)*(ESIN(ID)*DKDX-ECOS(ID)*DKDY)   ! @k/@x method, differs only very slightly from @C/dx
         ENDIF

!         Intuitively, one may expect that variable currents could be included via @C/@x. However, this is not the case.
!         C is determined from sigma and k (and the latter is determined from sigma).
!         Thus, if dealing with a fixed sigma value, as in SWAN,
!            having non-uniform currents does not result in non-uniform

         IF (ICUR .EQ. 0) THEN
            CAS(ID,IS,1)=CAST1*CAST2
            CAD(ID,IS,1)=CAD_TMP

!            --- adapt the velocity in case of diffraction
            IF (IDIFFR.EQ.1) THEN
               CAD(ID,IS,1) = diffr%param(KCG1)*CAD(ID,IS,1)&
               &- diffr%dpardx(KCG1)*CGO(IS,1)*ESIN(ID)&
               &+ diffr%dpardy(KCG1)*CGO(IS,1)*ECOS(ID)
            ENDIF

         ELSE
            IF (IDIFFR.EQ.0) THEN
               CAS(ID,IS,1) = CAST1*(CAST2+CAST3+CAST4) -&
               &CAST5*&
               &(COSCOS(ID)*DUXDX +&
               &SINCOS(ID)*(DUXDY+DUYDX) +&
               &SINSIN(ID)*DUYDY)

               CAD(ID,IS,1) = CAD_TMP +&
               &SINCOS(ID)*(DUXDX-DUYDY) +&
               &SINSIN(ID)*DUYDX -&
               &COSCOS(ID)*DUXDY ! add currents, Christof
            ELSE IF (IDIFFR.EQ.1) THEN
               CAS(ID,IS,1) = CAST1*(CAST2+CAST3+CAST4) -&
               &diffr%param(KCG1)*CAST5*&
               &(COSCOS(ID)*DUXDX +&
               &SINCOS(ID)*(DUXDY+DUYDX) +&
               &SINSIN(ID)*DUYDY)

               CAD(ID,IS,1) = diffr%param(KCG1)*CAD_TMP -&
               &diffr%dpardx(KCG1)*CGO(IS,1)*ESIN(ID) +&
               &diffr%dpardy(KCG1)*CGO(IS,1)*ECOS(ID) +&
               &SINCOS(ID)*(DUXDX-DUYDY) +&
               &SINSIN(ID)*DUYDX -&
               &COSCOS(ID)*DUXDY
            ENDIF
         ENDIF

      ENDDO
   ENDDO

!     *** for most cases CAS and CAD will be activated. Therefore ***
!     *** for IREFR is set 0 (no refraction) or ITFRE = 0 (no     ***
!     *** frequency shift) we have put the IF statement outside   ***
!     *** the internal loop above                                 ***

   IF (ITFRE .EQ. 0) THEN
      DO IS = 1, MSC
         DO ID = 1, MDC
            CAS(ID,IS,1) = 0.0
         ENDDO
      ENDDO
   ENDIF

   IF (IREFR .EQ. 0) THEN
      DO IS = 1, MSC
         DO ID = 1, MDC
            CAD(ID,IS,1) = 0.0
         ENDDO
      ENDDO
   ENDIF

!     --- limit Ctheta in some frequency range if requested

   IF ( INT(PNUMS(29)).EQ.1 ) THEN
      FRLIM = PI2*PNUMS(26)
      PP    =     PNUMS(27)
      DO IS = 1, MSC
         FAC = MIN(1.,(SPCSIG(IS)/FRLIM)**PP)
         DO ID = 1, MDC
            CAD(ID,IS,1) = FAC*CAD(ID,IS,1)
         ENDDO
      ENDDO
   ENDIF

!     --- limit Csigma using Courant number

   IF ( INT(PNUMS(33)).EQ.1 ) THEN

      ALPHA = PNUMS(34)

      DO IS = 1, MSC

         IF ( LSRFB .AND. ntf.GT.0 .AND. .NOT.sflog ) THEN
            FAC2 = ALPHA * 2. * PI * dfiem
         ELSE
            FAC2 = ALPHA * FRINTF * SPCSIG(IS)
         ENDIF

         DO IDDUM = IDDLOW-1, IDDTOP+1
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

            FAC = FAC2 * ( ABS((RDX(1)+RDX(2))*CAX(ID,IS,1)) +&
            &ABS((RDY(1)+RDY(2))*CAY(ID,IS,1)) )

            IF ( ABS(CAS(ID,IS,1)) > FAC ) THEN
               CAS(ID,IS,1) = CAS(ID,IS,1) * FAC / ABS(CAS(ID,IS,1))
            ENDIF

         ENDDO

      ENDDO

   ENDIF

!     --- limit Ctheta using Courant number

   IF ( INT(PNUMS(35)) == 1 ) THEN

      ALPHA = PNUMS(36)

      FAC2 = ALPHA * DDIR

      DO IS = 1, MSC

         DO IDDUM = IDDLOW-1, IDDTOP+1
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

            FAC = FAC2 * ( ABS((RDX(1)+RDX(2))*CAX(ID,IS,1)) +&
            &ABS((RDY(1)+RDY(2))*CAY(ID,IS,1)) )

            IF ( ABS(CAD(ID,IS,1)) > FAC ) THEN
               CAD(ID,IS,1) = CAD(ID,IS,1) * FAC / ABS(CAD(ID,IS,1))
            ENDIF

         ENDDO

      ENDDO

   ENDIF

!     *** test output ***

   IF (TESTFL .AND. ITEST.GE.140) THEN
      IF (DYNDEP .OR. ICUR.GT.0) THEN
         WRITE(PRINTF, *) ' IS ID1 ID2        values of CAS'
         DO IS = 1, MSC
            ID1 = IDDLOW-1
            ID2 = IDDTOP+1
            WRITE(PRINTF, "(3I4, 2X, 600E12.4)") IS, ID1, ID2,&
            &(CAS(MOD(IDDUM-1+MDC,MDC)+1, IS, 1), IDDUM=ID1,ID2)
         ENDDO
      ENDIF
      WRITE(PRINTF, *) ' IS ID1 ID2        values of CAD'
      DO IS = 1, MSC
         ID1 = IDDLOW-1
         ID2 = IDDTOP+1
         WRITE(PRINTF,"(3I4, 2X, 600E12.4)") IS, ID1, ID2,&
         &(CAD(MOD(IDDUM-1+MDC,MDC)+1, IS, 1), IDDUM=ID1,ID2)
      ENDDO
   ENDIF

!     end of the subroutine SPROSD
   RETURN
end subroutine SPROSD
!****************************************************************

SUBROUTINE DSPHER (CAD, CAX, CAY, ANYBIN, YCGRID, ECOS, ESIN)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_coordinate_offset
   USE swan_stencil
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_spherical_geometry
   USE swan_diagnostics_level

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
!     33.09: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     33.09, Aug. 99: new subroutine
!     40.41, Aug. 04: CG replaced by CAX*COS(D)+CAY*SIN(D)
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     computes the propagation velocities of energy in Theta-
!     space, i.e., CAD, due to use of spherical coordinates
!
!  3. Method
!
!     References:
!     W. E. Rogers, J. M. Kaihatu, H. A. H. Petit, N. Booij and L. H. Holthuijsen,
!     "Multiple-scale Propagation in a Third-Generation Wind Wave Model"
!     in preparation
!
!             Cg Cos(theta) Tan(latitude)
!     CAD = - ---------------------------
!                    Rearth
!
!     The group velocity CG in the direction of the wave propagation
!     in case with a current is equal to:
!
!                     1      K(IS,IC)DEP(IX,IY)        S
!     CG(ID,IS,IC)= ( - + -----------------------) --------- +
!                     2  sinh 2K(IS,IC)DEP(IX,IY)  |k(IS,IC)|
!
!                     + (UX2(IX,IY)cos(D) + UY2(IX,IY)sin(D))
!
!     which is equivalent with CAX*cos(D) + CAY*sin(D)
!
!
!  4. Argument variables
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!     i  ANYBIN 2D    if True the spectral component (ID,IS) is to be
!                     computed

   LOGICAL  ANYBIN(MDC,MSC)

!     o  CAD    3D    Wave transport velocity in D-direction, function of
!                     (ID,IS,IC)
!     i  CAX    3D    propagation velocity in X-direction (CGO+UX)
!     i  CAY    3D    propagation velocity in Y-direction (CGO+UY)
!     i  YCGRID 2D    Y-coordinate (latitude) for each geographic grid point
!     i  ECOS   1D    Represent the values of Cos(Theta) of each spectral
!                     direction
!     i  ESIN   1D    Represent the values of Sin(Theta) of each spectral
!                     direction
!
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAD(MDC,MSC,MICMAX)
   REAL  :: CAX(MDC,MSC,MICMAX)
   REAL  :: CAY(MDC,MSC,MICMAX)
   REAL  :: YCGRID(MXC,MYC)
   REAL  :: ECOS(MDC)
   REAL  :: ESIN(MDC)


!  4. Local variables
!
!        IX, IY       grid indices
!        ID, IS       spectral indices

   INTEGER :: IS    ,ID     ,IX    ,IY

!        TANLAT       tan of latitude
!        CTTMP        temp. value used to compute contribution to Ctheta

   REAL     TANLAT, CTTMP

!     5. SUBROUTINES CALLING
!
!        ACTION
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
!     9. STRUCTURE
!
!        ------------------------------------------------------------
!        Calculate tan of latitude (TANLAT)
!        Then For every spectral direction do
!                 calculate Cspher
!                 For every spectral frequency do
!                     add Cspher to value of CAD
!        ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

   INTEGER, SAVE :: IENT=0
   IF (LTRACE) CALL STRACE (IENT,'DSPHER')

!     *** TANLAT is Tan of Latitude

   IX     = IXCGRD(1)
   IY     = IYCGRD(1)
   TANLAT = TAN(DEGRAD*(YCGRID(IX,IY)+YOFFS))

   DO ID = 1, MDC
      CTTMP = ECOS(ID) * TANLAT / REARTH
      DO IS = 1, MSC
         CAD(ID,IS,1) = CAD(ID,IS,1) -&
         &(CAX(ID,IS,1)*ECOS(ID) + CAY(ID,IS,1)*ESIN(ID)) * CTTMP
      ENDDO
   ENDDO

!     end of the subroutine DSPHER
   RETURN
end subroutine DSPHER

!****************************************************************

SUBROUTINE STRSXY (         ISSTOP  ,IDCMIN  ,IDCMAX  ,CAX     ,&
&CAY     ,AC2     ,AC1     ,IMATRA  ,IMATDA  ,&
&RDX     ,RDY     ,&
&OBREDF  ,TRAC0   ,TRAC1   )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_run_mode
   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_spherical_geometry
   USE swan_diagnostics_level
   USE swan_io_units


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
!     0. AUTHORS
!
!        30.72: IJsbrand Haagsma
!        33.08: W. Erick Rogers (a few changes related to the S&L scheme)
!        33.09: Nico Booij (changes related to spherical coordinates)
!        40.08: Erick Rogers
!        40.41: Marcel Zijlema
!        40.85: Marcel Zijlema
!
!     1. UPDATE
!
!        30.72, Oct. 97: changed floating point comparison to avoid equality
!                        comparisons
!        new subroutine replacing STRSX and STRSY
!        time derivative is included here
!        33.08, July 98: STRSXY must use the rolled back AC when S&L is
!                        elsewhere in the domain.
!        33.09, June 99: commons swcomm2 and swcomm3 introduced, argument list
!                        modified; introduction of spherical coordinates
!        40.08, Mar. 03: Removed artifact from code
!        40.41, Oct. 04: common blocks replaced by modules, include files removed
!        40.85, Aug. 08: store xy-propagation for output purposes
!
!     2. PURPOSE
!
!        computation of space derivative of action transport
!
!     3. METHOD
!
!        Compute the derivative in x-direction:
!        The nearby points are indicated with the index IC (see
!        FUNCTION ICODE(_,_) ):
!        Central grid point     : IC = 1, grid index KCGRD(1)
!        Point in X-direction   : IC = 2, grid index KCGRD(2)
!        Point in Y-direction   : IC = 3, grid index KCGRD(3)
!
!        @[CAX AC2]
!        --------- =
!            @x
!
!      RDX(1) *
!      [CAX(ID,IS,1).AC2(ID,IS,KCGRD(1)) - CAX(ID,IS,2).AC2(ID,IS,KCGRD(2))]
!   +  RDX(2) *
!      [CAX(ID,IS,1).AC2(ID,IS,KCGRD(1)) - CAX(ID,IS,3).AC2(ID,IS,KCGRD(3))]
!
!        @[CAY AC2]
!        --------- =
!            @y
!
!      RDY(1) *
!      [CAY(ID,IS,1).AC2(ID,IS,KCGRD(1)) - CAY(ID,IS,2).AC2(ID,IS,KCGRD(2))]
!   +  RDY(2) *
!      [CAY(ID,IS,1).AC2(ID,IS,KCGRD(1)) - CAY(ID,IS,3).AC2(ID,IS,KCGRD(3))]
!
!        in diagonal matrix: 1/DT + (RDX(1)+RDX(2)) * CAX(ID,IS,1)
!                                 + (RDY(1)+RDY(2)) * CAY(ID,IS,1)
!        in r.h.s.: AC2/DT + RDX(1) * CAX(ID,IS,2).AC2(ID,IS,KCGRD(2))
!                          + RDX(2) * CAX(ID,IS,3).AC2(ID,IS,KCGRD(3))
!                          + RDY(1) * CAY(ID,IS,2).AC2(ID,IS,KCGRD(2))
!                          + RDY(2) * CAY(ID,IS,3).AC2(ID,IS,KCGRD(3))
!
!     4. PARAMETERLIST
!
!        KCGRD   int, i     Point index for grid points in comp molecule  30.40
!                           array of length ICMAX
!        MDC     int, i     Maximum counter of directional distribution
!        MSC     int, i     Maximum counter of relative frequency
!        MCGRD   int, i     Maximum counter of gridpoints in space
!        ICMAX   int, i     Maximum counter for the points of the molecule
!        ISSTOP  int, i     highest spectral frequency counter in the sweep
!        IDCMIN  int, i     minimum value of direction counter in this sweep
!        IDCMAX  int, i     maximum value of direction counter in this sweep
!        CAX     rea, i     3D array    propagation velocity in x
!        CAY     rea, i     3D array    propagation velocity in y
!        AC2     rea, i     array  spectral action density, function of
!                           x, y, theta, sigma
!        IMATDA  rea, i/o   array  Coefficients of diagonal of matrix
!        IMATRA  rea, i/o   array  Coefficients of right hand side of matrix
!        OBREDF  rea, i     action reduction factors, function of freq and
!                           direction
!        RDX,RDY 1D   i     array  containing spatial derivative coeff
!        NUMOBS  int, i     number of obstacles in comp grid
!
!     5. SUBROUTINES CALLING
!
!        ACTION
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
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   For every spectral bin do
!       If bin is in present sweep
!       Then If LOBST
!            Then For IC = 2 to ICMAX do
!                     multiply contribution from upwave point
!                     with reduction factor
!            ---------------------------------------------------
!            Compute the derivative in x-direction
!            Compute the derivative in y-direction
!            If computation is nonstationary
!            Then compute the derivative in t-direction
!            ---------------------------------------------------
!            Store the terms in arrays IMATRA and IMATDA
!   ------------------------------------------------------------

   INTEGER  IC, IND2, IND3, IS, ID, IDDUM, ISSTOP

   REAL     ACOLD, FXY1, FXY2, TCF1, TCF2

   REAL  :: AC1(MDC,MSC,MCGRD) ,AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAX(MDC,MSC,MICMAX) ,CAY(MDC,MSC,MICMAX)
   REAL  :: IMATRA(MDC,MSC)    ,IMATDA(MDC,MSC)            ,&
   &RDX(MICMAX)        ,RDY(MICMAX)                ,&
   &TRSCF(3)            ,&
   &OBREDF(MDC,MSC,2)
   REAL  :: TRAC0(MDC,MSC,MTRNP)
   REAL  :: TRAC1(MDC,MSC,MTRNP)

   INTEGER  IDCMIN(MSC)                ,&
   &IDCMAX(MSC)

   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT,'STRSXY')

   IF (TESTFL .AND. ITEST .GE. 120) THEN
      WRITE(PRINTF,*) ' Initial matrix coefficients at STRSXY : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF

   do IS = 1, ISSTOP
!       test output     ver 30.50

      IND2 = KCGRD(2)
      IND3 = KCGRD(3)


      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
!         test output     ver 30.50

         IF (NUMOBS .GT. 0) THEN
            TCF1 = OBREDF(ID,IS,1)
            TCF2 = OBREDF(ID,IS,2)
            IF (TESTFL .AND. ITEST.GE.80) THEN
               WRITE(PRINTF,"(' STRSXY obst ',3(1X,I5),2(1X,E10.4))") KCGRD(1),ID,IS,TCF1,TCF2
            ENDIF
         ELSE
            TCF1 = 1.
            TCF2 = 1.
         ENDIF

         FXY1 = 0.
         FXY2 = 0.

         FXY1 =   (RDX(1)+RDX(2)) * CAX(ID,IS,1)&
         &+ (RDY(1)+RDY(2)) * CAY(ID,IS,1)

         IF (KSPHER.EQ.0) THEN

            FXY2 =  RDX(1) * CAX(ID,IS,2)* TCF1 * AC2(ID,IS,IND2)&
            &+ RDX(2) * CAX(ID,IS,3)* TCF2 * AC2(ID,IS,IND3)&
            &+ RDY(1) * CAY(ID,IS,2)* TCF1 * AC2(ID,IS,IND2)&
            &+ RDY(2) * CAY(ID,IS,3)* TCF2 * AC2(ID,IS,IND3)
         ELSE
!           spherical coordinates

            TRSCF(2) = TCF1
            TRSCF(3) = TCF2
            DO IC = 2, 3
               FXY2 = FXY2 +&
               &RDX(IC-1) * CAX(ID,IS,IC) * TRSCF(IC) *&
               &AC2(ID,IS,KCGRD(IC))&
               &+ RDY(IC-1) * CAY(ID,IS,IC) * TRSCF(IC) *&
               &AC2(ID,IS,KCGRD(IC)) * COSLAT(IC) / COSLAT(1)
            ENDDO
         ENDIF

!         *** the term FXY2 is known, store in IMATRA ***
!         *** the term FXY1 is unknown, store in IMATDA ***
!
!         This business of doing rollback regardless of ITERMX was an artifact 40.08
!         and has been removed. Thus, the code reverts to its form in v40.01   40.08

         IF (NSTATC.EQ.1) THEN
            IF (ITERMX.EQ.1) THEN
               ACOLD = AC2(ID,IS,KCGRD(1))
            ELSE
               ACOLD = AC1(ID,IS,KCGRD(1))
            ENDIF
            IMATRA(ID,IS) = IMATRA(ID,IS) + FXY2 + ACOLD*RDTIM
            IMATDA(ID,IS) = IMATDA(ID,IS) + FXY1 + RDTIM
!           TRACx represent material derivative of action density
            TRAC0(ID,IS,1) = TRAC0(ID,IS,1) - FXY2 - ACOLD*RDTIM
            TRAC1(ID,IS,1) = TRAC1(ID,IS,1) + FXY1 + RDTIM
         ELSE
            IMATRA(ID,IS) = IMATRA(ID,IS) + FXY2
            IMATDA(ID,IS) = IMATDA(ID,IS) + FXY1
            TRAC0(ID,IS,1) = TRAC0(ID,IS,1) - FXY2
            TRAC1(ID,IS,1) = TRAC1(ID,IS,1) + FXY1
         ENDIF
!         --- Using an if statement like this--in conjunction with
!             inclusion of all directions in calculation, i.e. non-use
!             of IDCMIN, IDCMAX feature throughout code--would be a
!             "brute force" method of correcting problems with sweeping
!             and curvilinear scheme. I'm leaving it as-is for now,
!             since this would slow calculations. (Erick Rogers)
!          IF((FXY1.GE.0).AND.(FXY2.GE.0))THEN
!            IMATRA(ID,IS) = IMATRA(ID,IS) + FXY2
!            IMATDA(ID,IS) = IMATDA(ID,IS) + FXY1
!          ENDIF
!
!         *** test output ***

         IF ( ITEST .GE. 150 .AND. TESTFL ) THEN
            IF (NSTATC.EQ.1) THEN
               WRITE(PRINTF,"(' - ID FXY1 FXY2 ACOLD:', I4, 3(1X,E12.4))") ID, FXY1, FXY2, ACOLD
            ELSE
               WRITE(PRINTF,"(' - ID FXY1 FXY2:', I4, 2(1X,E12.4))") ID, FXY1, FXY2
            ENDIF
         ENDIF

      end do
   end do

   IF (TESTFL .AND. ITEST .GE. 100) THEN
      WRITE(PRINTF,*) '  matrix coefficients at STRSXY : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF
!     End of subroutine STRSXY
   RETURN
end subroutine STRSXY
!****************************************************************

SUBROUTINE SORDUP (         ISSTOP  ,IDCMIN  ,IDCMAX  ,CAX     ,&
&CAY     ,AC2     ,IMATRA  ,IMATDA  ,&
&RDX     ,RDY     ,TRAC0   ,TRAC1   )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_spherical_geometry
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
!     0. AUTHORS
!
!        33.10: Nico Booij and Erick Rogers (changes related to the SORDUP scheme)
!        33.09: Nico Booij (changes related to spherical coordinates)
!        40.08: Erick Rogers
!        40.41: Marcel Zijlema
!        40.59: W. Erick Rogers
!        40.85: Marcel Zijlema
!        40.98: Marcel Zijlema
!
!     1. UPDATE
!
!        33.10, Jan. 2000: subroutine SORDUP created. It is a modified STRSXY.
!        40.08, Mar. 2003: Improve scheme to use dx,dy calculated over two grid
!                          spaces (instead of one) where appropriate. This involves
!                          the use of RDX(3),RDX(4),RDY(3),RDY(4). This
!                          be a noticeable improvement. (I have not seen an example of a
!                          case where the original SORDUP does poorly relative to BSBT,
!                          so this is a speculative improvement).
!                          Remove option for controllable 1st order diffusion ("XYMU",
!                          "THETAK", etc.)
!        40.41, Oct. 04: common blocks replaced by modules, include files removed
!        40.59, Aug. 07: stencil modification
!        40.85, Aug. 08: store xy-propagation for output purposes
!        40.98, Feb. 09: SORDUP scheme is made consistent
!
!     2. PURPOSE
!
!        Purpose is to compute the space derivative of action transport
!        the SORDUP scheme.
!        This is for stationary runs only (no time derivative).
!        The scheme is 2nd order accurate.
!        The scheme reduces to the "best" approximation of
!             d/dx which can be determined using Taylor Series for the
!             stencil (ix),(ix-1),(ix-2):
!                         3/2*mu*phi(ix)-2*mu*phi(ix-1)+1/2*mu*phi(ix-2)  40.08
!
!     3. METHOD
!
!     References:
!     W. E. Rogers, J. M. Kaihatu, H. A. H. Petit, N. Booij and L. H. Holthuijsen,
!     "Multiple-scale Propagation in a Third-Generation Wind Wave Model"
!     in preparation
!
!        Compute the derivative in x-direction:
!        The nearby points are indicated by KCGRD
!        KCGRD(1) :   IX  ,IY
!        KCGRD(2) :   IX-1,IY
!        KCGRD(3) :   IX  ,IY-1
!        KCGRD(6) :   IX-2,IY
!        KCGRD(7) :   IX  ,IY-2
!
!        The scheme is:
!
!        @[CAX AC2]
!        --------- =
!            @x
!
!        [1.5*CAX(ID,IS,1)*AC2(ID,IS,KCGRD(1))-2.0*CAX(ID,IS,2)*AC2(ID,IS,KCGRD(2))
!        +0.5*CAX(ID,IS,6)*AC2(ID,IS,KCGRD(6))]/DX
!
!        @[CAY AC2]
!        --------- =
!            @y
!
!        [1.5*CAY(ID,IS,1)*AC2(ID,IS,KCGRD(1))-2.0*CAY(ID,IS,3)*AC2(ID,IS,KCGRD(3))
!        +0.5*CAY(ID,IS,7)*AC2(ID,IS,KCGRD(7))]/DY
!
!        ADD TO DIAGONAL:
!        +1.5*CAX(ID,IS,1)/DX+1.5*CAY(ID,IS,1)/DY
!        ADD TO RHS:
!        +[2.0*CAX(ID,IS,2)*AC2(ID,IS,KCGRD(2)-0.5*CAX(ID,IS,6)*AC2(ID,IS,KCGRD(6)]/DX
!        +[2.0*CAY(ID,IS,3)*AC2(ID,IS,KCGRD(3)-0.5*CAY(ID,IS,7)*AC2(ID,IS,KCGRD(7)]/DY
!
!     4. PARAMETERLIST
!
!        KCGRD   int, i     Point index for grid points in comp molecule  30.40
!                           array of length ICMAX
!        MDC     int, i     Maximum counter of directional distribution
!        MSC     int, i     Maximum counter of relative frequency
!        MCGRD   int, i     Maximum counter of gridpoints in space
!        ICMAX   int, i     Maximum counter for the points of the molecule
!        ISSTOP  int, i     highest spectral frequency counter in the sweep
!        IDCMIN  int, i     minimum value of direction counter in this sweep
!        IDCMAX  int, i     maximum value of direction counter in this sweep
!        CAX     rea, i     3D array    propagation velocity in x
!        CAY     rea, i     3D array    propagation velocity in y
!        AC2     rea, i     array  spectral action density, function of
!                           x, y, theta, sigma
!        IMATDA  rea, i/o   array  Coefficients of diagonal of matrix
!        IMATRA  rea, i/o   array  Coefficients of right hand side of matrix
!        RDX,RDY 1D   i     array  containing spatial derivative coeff
!
!     5. SUBROUTINES CALLING
!
!        ACTION
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
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   For every spectral bin do
!       If bin is in present sweep
!            ---------------------------------------------------
!            Compute the derivative in x-direction
!            Compute the derivative in y-direction
!            If computation is nonstationary
!            Then compute the derivative in t-direction
!            ---------------------------------------------------
!            Store the terms in arrays IMATRA and IMATDA
!   ------------------------------------------------------------

   INTEGER  IS,ID,IDDUM,ISSTOP&
   &,IND2,IND3,IND6,IND7

   REAL  :: FXY1 ,FXY2

   REAL  :: AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAX(MDC,MSC,MICMAX) ,CAY(MDC,MSC,MICMAX)
   REAL  :: IMATRA(MDC,MSC)    ,IMATDA(MDC,MSC)            ,&
   &RDX(MICMAX)        ,RDY(MICMAX)                ,&
   &XMU(7)             ,YMU(7)
   REAL  :: TRAC0(MDC,MSC,MTRNP)
   REAL  :: TRAC1(MDC,MSC,MTRNP)

   INTEGER, SAVE :: IENT = 0
   INTEGER  IDCMIN(MSC), IDCMAX(MSC), IXY
   LOGICAL  XNUM

   IF (LTRACE) CALL STRACE (IENT,'SORDUP')

   IF(NSTATC.EQ.1)THEN
      CALL MSGERR (3, 'SORDUP scheme is for stationary mode only.')
   END IF
   IF (TESTFL .AND. ITEST .GE. 120) THEN
      WRITE(PRINTF,*) ' Initial matrix coefficients at SORDUP : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF

   do IS = 1, ISSTOP
      IND2 = KCGRD(2)
      IND3 = KCGRD(3)
      IND6 = KCGRD(6)
      IND7 = KCGRD(7)
      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
!         find Courant number values: XMU, YMU
!         depending on relative size of XMU and YMU, XNUM is true or
!         false because of RDX and RDY, XMU and YMU are always positive
         DO IXY=1,3
            XMU(IXY) = RDX(1)*CAX(ID,IS,IXY) + RDY(1)*CAY(ID,IS,IXY)
            YMU(IXY) = RDX(2)*CAX(ID,IS,IXY) + RDY(2)*CAY(ID,IS,IXY)
         END DO
         DO IXY=6,7
            XMU(IXY) = RDX(1)*CAX(ID,IS,IXY) + RDY(1)*CAY(ID,IS,IXY)
            YMU(IXY) = RDX(2)*CAX(ID,IS,IXY) + RDY(2)*CAY(ID,IS,IXY)
         END DO
         IF(YMU(1).GT.XMU(1))THEN
!            propagation mainly from grid point 3
            XNUM=.TRUE.
         ELSE
!            propagation mainly from grid point 2
            XNUM=.FALSE.
         END IF

!         now calculate diagonal and rhs

         IF(XNUM)THEN

!           diagonal  FXY1
            FXY1 = 1.5*XMU(1) + 1.5*YMU(1)

            IF (KSPHER.EQ.0) THEN
!             Cartesian coordinates
!
!             the known, rhs part FXY2
               FXY2 = AC2(ID,IS,IND2) * 2.0*XMU(2)&
               &-AC2(ID,IS,IND6) * 0.5*XMU(6)&
               &+AC2(ID,IS,IND3) * 2.0*YMU(3)&
               &-AC2(ID,IS,IND7) * 0.5*YMU(7)

            ELSE
!             Spherical coordinates
!
!             the known, rhs part FXY2

               FXY2 =&
               &AC2(ID,IS,IND2) * CAX(ID,IS,2) * RDX(1) * 2.0&
               &-AC2(ID,IS,IND6) * CAX(ID,IS,6) * RDX(1) * 0.5&
               &+AC2(ID,IS,IND3) * CAX(ID,IS,3) * RDX(2) * 2.0&
               &-AC2(ID,IS,IND7) * CAX(ID,IS,7) * RDX(2) * 0.5&
               &+(AC2(ID,IS,IND2) * CAY(ID,IS,2) * RDY(1) * COSLAT(2) * 2.0&
               &-AC2(ID,IS,IND6) * CAY(ID,IS,6) * RDY(1) * COSLAT(6) * 0.5&
               &+AC2(ID,IS,IND3) * CAY(ID,IS,3) * RDY(2) * COSLAT(3) * 2.0&
               &-AC2(ID,IS,IND7) * CAY(ID,IS,7) * RDY(2) * COSLAT(7) * 0.5&
               &) / COSLAT(1) !33.10
            ENDIF

         ELSE      ! switch 2<==>3, 6<==>7 and YMU<==>XMU

! The diag part FXY1
            FXY1 = 1.5*YMU(1)+ 1.5*XMU(1)

            IF (KSPHER.EQ.0) THEN
!             Cartesian coordinates
!
!             the known, rhs part  FXY2
               FXY2 = AC2(ID,IS,IND3) * 2.0*YMU(3)&
               &-AC2(ID,IS,IND7) * 0.5*YMU(7)&
               &+AC2(ID,IS,IND2) * 2.0*XMU(2)&
               &-AC2(ID,IS,IND6) * 0.5*XMU(6)

            ELSE
!             Spherical coordinates
!
!             the known, rhs part  FXY2
               FXY2 =&
               &AC2(ID,IS,IND2) * CAX(ID,IS,2) * RDX(1) * 2.0&
               &-AC2(ID,IS,IND6) * CAX(ID,IS,6) * RDX(1) * 0.5&
               &+AC2(ID,IS,IND3) * CAX(ID,IS,3) * RDX(2) * 2.0&
               &-AC2(ID,IS,IND7) * CAX(ID,IS,7) * RDX(2) * 0.5&
               &+(AC2(ID,IS,IND2) * CAY(ID,IS,2) * RDY(1) * COSLAT(2) * 2.0&
               &-AC2(ID,IS,IND6) * CAY(ID,IS,6) * RDY(1) * COSLAT(6) * 0.5&
               &+AC2(ID,IS,IND3) * CAY(ID,IS,3) * RDY(2) * COSLAT(3) * 2.0&
               &-AC2(ID,IS,IND7) * CAY(ID,IS,7) * RDY(2) * COSLAT(7) * 0.5&
               &)/ COSLAT(1)
            ENDIF
         END IF

         IF (TESTFL .AND. ITEST.GE.120) WRITE (PRTEST, "(2I3, 6(1X,E12.4))") ID, IS,&
         &CAX(ID,IS,2), CAY(ID,IS,2), AC2(ID,IS,IND2),&
         &CAX(ID,IS,3), CAY(ID,IS,3), AC2(ID,IS,IND3)

!         *** the term FXY2 is known, store in IMATRA ***
!         *** the term FXY1 is unknown, store in IMATDA ***

         IMATRA(ID,IS) = IMATRA(ID,IS) + FXY2
         IMATDA(ID,IS) = IMATDA(ID,IS) + FXY1
         TRAC0(ID,IS,1) = TRAC0(ID,IS,1) - FXY2
         TRAC1(ID,IS,1) = TRAC1(ID,IS,1) + FXY1

!         *** test output ***
         IF ( ITEST .GE. 150 .AND. TESTFL ) THEN
            WRITE(PRINTF,"(' - ID FXY1 FXY2:', I4, 2(1X,E12.4))") ID, FXY1, FXY2
         ENDIF

      end do
   end do

   IF (TESTFL .AND. ITEST .GE. 100) THEN
      WRITE(PRINTF,*) '  matrix coefficients at SORDUP : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF
!     End of subroutine SORDUP
   RETURN
end subroutine SORDUP
!****************************************************************

SUBROUTINE SANDL ( ISSTOP  ,IDCMIN  ,IDCMAX  ,CGO     ,CAX     ,&
&CAY     ,AC2     ,AC1     ,IMATRA  ,IMATDA  ,&
&RDX     ,RDY     ,CAX1    ,CAY1    ,SPCDIR  ,&
&TRAC0   ,TRAC1   )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE swan_run_mode
   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_propagation_scheme
   USE swan_spherical_geometry
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_time, ONLY: default_time_context

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
!     0. AUTHORS
!
!        33.08: W. Erick Rogers
!        33.09: Nico Booij
!        40.02: IJsbrand Haagsma
!        40.08: W. Erick Rogers
!        40.41: Marcel Zijlema
!        40.85: Marcel Zijlema
!
!     1. UPDATE
!
!        33.08, July 98: SANDL: New subroutine using a Stelling and Leenderste
!                        SANDL: scheme (Qo=0,Q1=1/6) is created.
!        33.09, Aug. 99: extension with spherical coordinates
!        40.02, Aug. 00: Avoid more than 19 continuation lines
!        40.08, Feb. 03: Check for exceedence of soft CFL criterion
!        40.41, Oct. 04: common blocks replaced by modules, include files removed
!        40.85, Aug. 08: store xy-propagation for output purposes
!
!     2. PURPOSE
!
!        computation of space derivative of action transport
!
!     3. METHOD
!
!        References:
!     1) Rogers, W.E., J.M. Kaihatu, N. Booij and L.H. Holthuijsen,
!        "Improving the numerics of a third-generation wave action
!        model", Naval Research Laboratory, NRL/FR/7320-99-9695, 79p.
!     2) Rogers, W.E., J.M. Kaihatu, N. Booij, L.H. Holthuijsen,
!        and H. Petit, 2002: "Diffusion Reduction in an Arbitrary
!        Scale Wave Action Model", Ocean Eng, 29, 1357-1390.
!
!        computational stencil:                                     40.03
!                                                                   33.08
!      IY+1                 o 11 o 4  o 12                          33.08
!                           |    |    |                             33.08
!                 8    6    | 2  | 1  | 5                           33.08
!      IY         o----o----o----*----o                             33.08
!                           |    |    |                             33.08
!                           10   | 3  | 13                          33.08
!      IY-1                 o----o----o                             33.08
!                                |                                  33.08
!                                |                                  33.08
!      IY-2                      o 7                                33.08
!                                |                                  33.08
!                                |                                  33.08
!      IY-3                      o 9                                33.08
!
!                 ^    ^    ^    ^    ^
!                 |    |    |    |    |                             33.08
!               IX-3 IX-2 IX-1  IX  IX+1
!
!        Compute the derivative in x-direction:
!        The nearby points are indicated with the index IC (see
!        above scheme):
!        Central grid point     : IC = 1, grid index KCGRD(1)
!        Point in X-direction   : IC = 2, grid index KCGRD(2)
!        Point in Y-direction   : IC = 3, grid index KCGRD(3)
!
!        @[CAX AC2]
!        --------- =
!            @x
!
!       (1/4DX)*(CAX1(KCGRD(5))*AC1(KCGRD(5))-CAX1(KCGRD(2))*AC1(KCGRD(2)))          33.08
!       +(1/12DX)*( 10*CAX(KCGRD(1))*AC2(KCGRD(1))-15*CAX(KCGRD(2))*AC2(KCGRD(2))    33.08
!       +6*CAX(KCGRD(6))*AC2(KCGRD(6))-1*CAX(KCGRD(8))*AC2(KCGRD(8)) )
!
!
!        @[CAY AC2]
!        --------- =
!            @y
!
!       (1/4DY)*(CAY1(KCGRD(4))*AC1(KCGRD(4))-CAY1(KCGRD(3))*AC1(KCGRD(3)))          33.08
!       +(1/12DY)*( 10*CAY(KCGRD(1))*AC2(KCGRD(1))-15*CAY(KCGRD(3))*AC2(KCGRD(3))    33.08
!       +6*CAY(KCGRD(7))*AC2(KCGRD(7))-1*CAY(KCGRD(9))*AC2(KCGRD(9)) )
!
!        in diagonal matrix: 1/DT + (5./6.)*(RDX(1)+RDX(2)) * CAX(ID,IS,1)           33.08
!                                 + (5./6.)*(RDY(1)+RDY(2)) * CAY(ID,IS,1)           33.08
!
!        in r.h.s.: AC1/DT + RDX(1)*CAX(ID,IS,2)*AC2(ID,IS,KCGRD(2))
!                  +(5./4.) *RDY(2)*CAY(ID,IS,3)*AC2(ID,IS,KCGRD(3))
!                  -(1./2.) *RDX(1)*CAX(ID,IS,6)*AC2(ID,IS,KCGRD(6))
!                  -(1./2.) *RDY(2)*CAY(ID,IS,7)*AC2(ID,IS,KCGRD(7))
!                  +(1./12.)*RDX(1)*CAX(ID,IS,8)*AC2(ID,IS,KCGRD(8))
!                  +(1./12.)*RDY(2)*CAY(ID,IS,9)*AC2(ID,IS,KCGRD(9))
!                  +(0.25*RDX(1))*(CAX1(ID,IS,2)*AC1(ID,IS,KCGRD(2))
!                  -               CAX1(ID,IS,5)*AC1(ID,IS,KCGRD(5)))
!                  +(0.25*RDY(2))*(CAY1(ID,IS,3)*AC1(ID,IS,KCGRD(3))
!                  -               CAY1(ID,IS,4)*AC1(ID,IS,KCGRD(4)))
!
!        Anti-GSE correction:
!        reference: Booij and Holthuijsen (JCP 1987) equations 32-35.
!        To produce anisotrophic diffusion, we add to the r.h.s.:
!
!                  +RDX**2
!                    *(+DXX(1)*(AC1(ID,IS,IND5)-AC1(ID,IS,IND1))
!                      -DXX(2)*(AC1(ID,IS,IND1)-AC1(ID,IS,IND2)))
!                  +RDY**2
!                    *(+DYY(1)*(AC1(ID,IS,IND4)-AC1(ID,IS,IND1))
!                      -DYY(3)*(AC1(ID,IS,IND1)-AC1(ID,IS,IND3)))
!                  +(2.*DXY(1)*RDX*RDY)
!                           *(+AC1(ID,IS,IND1)-AC1(ID,IS,IND2)
!                             -AC1(ID,IS,IND3)+AC1(ID,IS,IND10))
!        note: factor 2 here since we have @@xy and @@yx
!
!                  Where DXX, DYY, and DXY are diffusion coefficients.
!
!         Notes (Rogers, Jan 10 2013) : I noticed a few years ago that there is
!            some asymmetry to this anti-GSE implementation that can be
!            In some tests, the asymmetry results in something that looks like a lima bean
!            where we would expect an ellipsoid.
!            If we do this, it may make some swell dispersion look a bit more "natural"
!            Good news: the change is to the variable "D12AC", which means that it is not complicated by RDX RDY
!            Bad news: the change requires 3 new points in our stencil.
!
!         Notes (Rogers, Feb 21 2013) : this is now done.
!            from ./RDX_FD_NOSUBS/swan_clone.f90 :
!         < !            D12AC = 1.00*(QDENS(IX  ,JY  ) -     QDENS(IX-1,JY  ) - QDENS(IX  ,JY-1) + QDENS(IX-1,JY-1)) ! lima bean
!         <              D12AC = 0.25*(QDENS(IX+1,JY+1) -     QDENS(IX-1,JY+1) - QDENS(IX+1,JY-1) + QDENS(IX-1,JY-1)) ! corrected
!                        thus indices are :   + IND12              -IND11              -IND13            +IND10
!
!        The finite difference scheme for @^2/@x^2 is created by taking
!          @A/@x=(A(i+0.5)-A(i-0.5))/dx
!        ...and applying it twice, @(@A/@x)/@x=(A(i+1)-2*A(i)+A(i-1))/(dx^2)
!
!        The OLD finite difference scheme for @(@A/@x)/@y is mentioned in our paper, Rogers et al. (2002)
!          However, I don't know the origins of this scheme.
!
!        The NEW finite difference scheme for @(@A/@x)/@y is created by
!          @A/@x=(A(i+1)-A(i-1))/(2*dx) @A/@y=(A(j+1)-A(j-1))/(2*dy)
!        ...and applying it together, giving @(@A/@x)/@y =
!           [A(i+1,j+1)-A(i-1,j+1)-A(i+1,j-1)+A(i-1,j-1)]/[4*dx*dy]
!
!     4. PARAMETERLIST
!
!        ISSTOP  int, i     highest spectral frequency counter in the sweep
!        IDCMIN  int, i     minimum value of direction counter in this sweep
!        IDCMAX  int, i     maximum value of direction counter in this sweep
!        CGO     rea, i     2D array    group velocity
!        CAX     rea, i     3D array    propagation velocity in x  new time level
!        CAY     rea, i     3D array    propagation velocity in y
!        CAX1    rea, i     3D array    propagation velocity in x  old time level
!        CAY1    rea, i     3D array    propagation velocity in y
!        AC2     rea, i     array  spectral action density, function of
!                           x, y, theta, sigma
!        IMATDA  rea, i/o   array  Coefficients of diagonal of matrix
!        IMATRA  rea, i/o   array  Coefficients of right hand side of matrix
!        RDX,RDY 1D   i     array  containing spatial derivative coeff
!
!     5. SUBROUTINES CALLING
!
!        ACTION
!
!     6. Local variables
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
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   For every spectral bin do
!       If bin is in present sweep
!       Then If LOBST
!            Then For IC = 2 to ICMAX do
!                     multiply contribution from upwave point
!                     with reduction factor
!            ---------------------------------------------------
!            Compute the derivative in x-direction
!            Compute the derivative in y-direction
!            If computation is nonstationary
!            Then compute the derivative in t-direction
!            ---------------------------------------------------
!            Store the terms in arrays IMATRA and IMATDA
!   ------------------------------------------------------------

   INTEGER  IS      ,ID      ,IDDUM   ,ISSTOP  ,IC    ,&
   &IND1,IND2,IND3,IND4,IND5,IND6,IND7,IND8,IND9,IND10,&
   &IND11,IND12,IND13

   REAL     FXY1 ,FXY2, ACOLD,&
   &DSS, DNN, D11AC, D12AC, D22AC

   REAL  :: TRAC0(MDC,MSC,MTRNP)
   REAL  :: TRAC1(MDC,MSC,MTRNP)
   REAL  :: AC1(MDC,MSC,MCGRD) ,AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CGO(MSC,MICMAX)
   REAL  :: CAX(MDC,MSC,MICMAX) ,CAY(MDC,MSC,MICMAX)
   REAL  :: IMATRA(MDC,MSC)    ,IMATDA(MDC,MSC)
   REAL  :: RDX(MICMAX)        ,RDY(MICMAX)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAX1(MDC,MSC,MICMAX),CAY1(MDC,MSC,MICMAX)
   REAL  :: DCG, SPCDIR(MDC,6), DXX, DYY, DXY
   REAL  :: MYU,DX1DUM,DY1DUM,DX2DUM,DY2DUM,DXMYU,DYMYU
   LOGICAL, SAVE :: NOWARN = .FALSE.

   INTEGER :: IDCMIN(MSC), IDCMAX(MSC)

   INTEGER, SAVE :: IENT=0
   IF (LTRACE) CALL STRACE (IENT,'SANDL')

   IF (TESTFL .AND. ITEST .GE. 120) THEN
      WRITE(PRINTF,*) ' Initial matrix coefficients at SANDL : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF

   IND1  = KCGRD(1)
   IND2  = KCGRD(2)
   IND3  = KCGRD(3)
   IND4  = KCGRD(4)
   IND5  = KCGRD(5)
   IND6  = KCGRD(6)
   IND7  = KCGRD(7)
   IND8  = KCGRD(8)
   IND9  = KCGRD(9)
   IND10 = KCGRD(10)
   IND11 = KCGRD(11)
   IND12 = KCGRD(12)
   IND13 = KCGRD(13)

   IF (TESTFL .AND. KSPHER.GT.0 .AND. ITEST.GE.60) THEN
      WRITE (PRTEST, "(' Cos(Lat) ',10(1X,F7.4))") (COSLAT(IC), IC=1, 10)
   ENDIF
   IF (WAVAGE.GT.0. .AND. ITEST.GE.120 .AND. TESTFL) THEN
      WRITE (PRTEST, *) '  ID  IS  DSS    DNN   ',&
      &'  DXX   DXY   DYY    D11AC   D12AC   D22AC'
   ENDIF

!     --- I have tested this code with Cartesian, curvilinear, and
!         spherical coords (Erick Rogers).
!         Note that this might be improved by only performing the
!         check at the first time step (assuming that cgo does not
!         change greatly from one time step to the next)

   IF(RDX(1).EQ.0.0)THEN
      DX1DUM=0.0
   ELSE
      DX1DUM=1.0/RDX(1)
   END IF

   IF(RDY(1).EQ.0.0)THEN
      DY1DUM=0.0
   ELSE
      DY1DUM=1.0/RDY(1)
   END IF

   IF(RDX(2).EQ.0.0)THEN
      DX2DUM=0.0
   ELSE
      DX2DUM=1.0/RDX(2)
   END IF

   IF(RDY(2).EQ.0.0)THEN
      DY2DUM=0.0
   ELSE
      DY2DUM=1.0/RDY(2)
   END IF

!     --- Even if KSPHER=1, dx and dy are already in meters
   DXMYU=SQRT(DY1DUM**2+DX1DUM**2)
   DYMYU=SQRT(DY2DUM**2+DX2DUM**2)
   MYU=ABS(default_time_context%DT*CGO(1,1)/MIN(DXMYU,DYMYU))
!     --- Since there is no hard stability limit, we use a nonexact
!         definition of CFL. I only check IS=1, since that is the
!         fastest wave.
   IF(MYU.GT.10.0.AND..NOT.NOWARN)THEN
      CALL MSGERR(2,'It is inadvisable to use the higher order '//&
      &'scheme for nonstationary computation with '    //&
      &'CFL greater than 10. Consider using PROP BSBT.'//&
      &' If you are having this problem because you '  //&
      &'are trying to run a high resolution model '    //&
      &'with a large (e.g. 1 hour) time step and '     //&
      &'COMP NONSTAT: Note that for smaller domains, ' //&
      &'you can avoid this problem by using MODE '     //&
      &'NONSTAT with multiple COMP STAT lines. (COMP ' //&
      &'STAT is often ok when domain is less than '    //&
      &'100km or 1 deg on a side, as a rule of thumb)')
!        --- Note that code will stop even without this line
!            (at beginning of next time step).
!         IF(MAXERR.LT.2) STOP
      NOWARN=.TRUE.
   END IF

   do IS = 1, ISSTOP

      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

         IF (WAVAGE.GT.0.) THEN
!           calculate DSS,DXX,DXY for the central grid point (IC=1)
!           we need DCG first. we calculate DCG (delta of Cg)
            IC = 1
            IF (IS.EQ.1) THEN
               DCG = ABS(CGO(IS+1,IC)-CGO(IS,IC))

            ELSE IF (IS.EQ.ISSTOP) THEN
               DCG = ABS(CGO(IS,IC)-CGO(IS-1,IC))

            ELSE
               DCG = 0.5 * ABS(CGO(IS+1,IC)-CGO(IS-1,IC))
            END IF
!           we obtain DSS etc. by using the wave age
            DSS = DCG**2*WAVAGE/12.
            DNN = (CGO(IS,IC)*DDIR)**2 * WAVAGE/12.
!           we obtain DXX etc. by multiplication with Cos(theta)^2 etc.
            DXX = DSS*SPCDIR(ID,4) + DNN*SPCDIR(ID,6)
            DYY = DSS*SPCDIR(ID,6) + DNN*SPCDIR(ID,4)
            DXY = (DSS-DNN)*SPCDIR(ID,5)
         END IF

!         the unknown, diagonal part:

         FXY1 = 0.83333*(RDX(1)+RDX(2)) * CAX(ID,IS,1)&
         &+ 0.83333*(RDY(1)+RDY(2)) * CAY(ID,IS,1)
         IF (KSPHER.EQ.0) THEN
!           Cartesian coordinates
!
!           the known, rhs part
!
!
!         To avoid violation of the ANSI standard this statement is split 40.02

            FXY2 =&
            &+1.25   * RDX(1) * CAX(ID,IS,2) * AC2(ID,IS,IND2)&
            &+1.25   * RDY(1) * CAY(ID,IS,2) * AC2(ID,IS,IND2)&
            &+1.25   * RDX(2) * CAX(ID,IS,3) * AC2(ID,IS,IND3)&
            &+1.25   * RDY(2) * CAY(ID,IS,3) * AC2(ID,IS,IND3)&
            &-0.5    * RDX(1) * CAX(ID,IS,6) * AC2(ID,IS,IND6)&
            &-0.5    * RDY(1) * CAY(ID,IS,6) * AC2(ID,IS,IND6)&
            &-0.5    * RDX(2) * CAX(ID,IS,7) * AC2(ID,IS,IND7)&
            &-0.5    * RDY(2) * CAY(ID,IS,7) * AC2(ID,IS,IND7)&
            &+0.08333* RDX(1) * CAX(ID,IS,8) * AC2(ID,IS,IND8)&
            &+0.08333* RDY(1) * CAY(ID,IS,8) * AC2(ID,IS,IND8)&
            &+0.08333* RDX(2) * CAX(ID,IS,9) * AC2(ID,IS,IND9)&
            &+0.08333* RDY(2) * CAY(ID,IS,9) * AC2(ID,IS,IND9)
            FXY2 = FXY2 + (&
            &+(0.25*RDX(1)) * (CAX1(ID,IS,2) * AC1(ID,IS,IND2)&
            &-                 CAX1(ID,IS,5) * AC1(ID,IS,IND5))&
            &+(0.25*RDY(1)) * (CAY1(ID,IS,2) * AC1(ID,IS,IND2)&
            &-                 CAY1(ID,IS,5) * AC1(ID,IS,IND5))&
            &+(0.25*RDX(2)) * (CAX1(ID,IS,3) * AC1(ID,IS,IND3)&
            &-                 CAX1(ID,IS,4) * AC1(ID,IS,IND4))&
            &+(0.25*RDY(2)) * (CAY1(ID,IS,3) * AC1(ID,IS,IND3)&
            &-                 CAY1(ID,IS,4) * AC1(ID,IS,IND4)) )
         ELSE
!           spherical coordinates

            FXY2 =&
            &1.25   * RDX(1)*CAX(ID,IS,2)*AC2(ID,IS,IND2)&
            &+1.25   * RDX(2)*CAX(ID,IS,3)*AC2(ID,IS,IND3)&
            &-0.5    * RDX(1)*CAX(ID,IS,6)*AC2(ID,IS,IND6)&
            &-0.5    * RDX(2)*CAX(ID,IS,7)*AC2(ID,IS,IND7)&
            &+0.08333* RDX(1)*CAX(ID,IS,8)*AC2(ID,IS,IND8)&
            &+0.08333* RDX(2)*CAX(ID,IS,9)*AC2(ID,IS,IND9)&
            &+(0.25*RDX(1))*(CAX1(ID,IS,2)*AC1(ID,IS,IND2)&
            &-               CAX1(ID,IS,5)*AC1(ID,IS,IND5))&
            &+(0.25*RDX(2))*(CAX1(ID,IS,3)*AC1(ID,IS,IND3)&
            &-               CAX1(ID,IS,4)*AC1(ID,IS,IND4))
            FXY2 = FXY2 + (&
            &+1.25   * RDY(1)*CAY(ID,IS,2)*AC2(ID,IS,IND2)*COSLAT(2)&
            &+1.25   * RDY(2)*CAY(ID,IS,3)*AC2(ID,IS,IND3)*COSLAT(3)&
            &-0.5    * RDY(1)*CAY(ID,IS,6)*AC2(ID,IS,IND6)*COSLAT(6)&
            &-0.5    * RDY(2)*CAY(ID,IS,7)*AC2(ID,IS,IND7)*COSLAT(7)&
            &+0.08333* RDY(1)*CAY(ID,IS,8)*AC2(ID,IS,IND8)*COSLAT(8)&
            &+0.08333* RDY(2)*CAY(ID,IS,9)*AC2(ID,IS,IND9)*COSLAT(9)&
            &+(0.25*RDY(1))*(CAY1(ID,IS,2)*AC1(ID,IS,IND2)*COSLAT(2)&
            &-               CAY1(ID,IS,5)*AC1(ID,IS,IND5)*COSLAT(5))&
            &+(0.25*RDY(2))*(CAY1(ID,IS,3)*AC1(ID,IS,IND3)*COSLAT(3)&
            &-               CAY1(ID,IS,4)*AC1(ID,IS,IND4)*COSLAT(4))&
            &) / COSLAT(1)
         ENDIF

         IF (WAVAGE.GT.0.0) THEN      ! add the anti-GSE stuff
            D11AC = AC1(ID,IS,IND5) - 2.*AC1(ID,IS,IND1) +&
            &AC1(ID,IS,IND2)
!           old method
!           D12AC = AC1(ID,IS,IND1) - AC1(ID,IS,IND2) -
!    &              AC1(ID,IS,IND3) + AC1(ID,IS,IND10)
!           new method
            D12AC = 0.25*(AC1(ID,IS,IND12) - AC1(ID,IS,IND11)&
            &-AC1(ID,IS,IND13) + AC1(ID,IS,IND10))
            D22AC = AC1(ID,IS,IND4) - 2.*AC1(ID,IS,IND1) +&
            &AC1(ID,IS,IND3)
            FXY2 = FXY2 +&
            &DXX * (RDX(1)*RDX(1)*D11AC + 2.*RDX(1)*RDX(2)*D12AC +&
            &RDX(2)*RDX(2)*D22AC) +&
            &2.*DXY * (RDX(1)*RDY(1)*D11AC + RDX(2)*RDY(2)*D22AC +&
            &(RDX(1)*RDY(2)+RDX(2)*RDY(1))*D12AC) +&
            &DYY * (RDY(1)*RDY(1)*D11AC + 2.*RDY(1)*RDY(2)*D12AC +&
            &RDY(2)*RDY(2)*D22AC)
            IF (ITEST.GE.120 .AND. TESTFL) WRITE (PRTEST, "(1X, 2I4, 1X, 2E12.4, 1X, 3E12.4, 1X, 4E12.4)")&
            &ID, IS, DSS, DNN, DXX, DXY, DYY, D11AC, D12AC, D22AC
         END IF

!         *** the term FXY2 is known, store in IMATRA ***
!         *** the term FXY1 is unknown, store in IMATDA ***
!
!         This business of doing rollback regardless of ITERMX is an
!         artifact and has been removed.
         IF (ITERMX.EQ.1) THEN
            ACOLD = AC2(ID,IS,KCGRD(1))
         ELSE
            ACOLD = AC1(ID,IS,KCGRD(1))
         ENDIF
         IMATRA(ID,IS) = IMATRA(ID,IS) + FXY2 + ACOLD*RDTIM
         IMATDA(ID,IS) = IMATDA(ID,IS) + FXY1 + RDTIM
!         TRACx represent material derivative of action density
         TRAC0(ID,IS,1) = TRAC0(ID,IS,1) - FXY2 - ACOLD*RDTIM
         TRAC1(ID,IS,1) = TRAC1(ID,IS,1) + FXY1 + RDTIM

!         *** test output ***

         IF ( ITEST .GE. 150 .AND. TESTFL ) THEN
            WRITE(PRINTF,"(' - ID FXY1 FXY2 ACOLD:', I4, 3(1X,E12.4))") ID, FXY1, FXY2, ACOLD
         ENDIF

      end do
   end do

   IF (TESTFL .AND. ITEST .GE. 100) THEN
      WRITE(PRINTF,*) '  matrix coefficients at SANDL : '
      WRITE(PRINTF,*)&
      &'IS ID IDDUM     IMATDA    IMATRA'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE(PRINTF,"(3I3,2E12.4)") IS,IDDUM,ID,&
            &IMATDA(ID,IS), IMATRA(ID,IS)
         ENDDO
      ENDDO
   END IF
!     End of subroutine SANDL
   RETURN
end subroutine SANDL

!****************************************************************

SUBROUTINE STRSSI(SPCSIG  ,&
&CAS     ,IMAT5L  ,IMATDA  ,IMAT6U  ,ANYBIN  ,&
&IMATRA  ,AC2     ,ISCMIN  ,ISCMAX  ,IDDLOW  ,&
&IDDTOP  ,TRAC0   ,TRAC1                     )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units


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
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: store sigma-propagation for output purposes
!
!  2. Purpose
!
!     comp. of @[CAS AC2]/@S initial & boundary : IMPLICIT SCHEME
!
!  3. Method
!
!     Compute the derivative in S-direction only n the central
!     gridpoint considered:
!                             Central grid point     : IC = 1
!
!     Depending on the parameter PNUMS(7) either a central difference
!     scheme (PNUMS(7) = 0) or an upstream scheme (PNUMS(7) = 1) is
!     used. Points 1, 2 and 3 are three consecutive points on the
!     T-axis. 2 is the central point for which @(C*A)/@SIGMA and
!     @(C*W*A)/@SIGMA is computed.
!
!               1       2       3
!            ---O-------O-------O--- > SIGMA
!
!
!     PNUMS() = 0.  central difference scheme
!     PNUMS() = 1.  upwind scheme
!
!     @[CAS AC2]
!     ----------  =
!        @S
!
!     CAS(ID,IS+1,1) AC2(ID,IS+1,IX,IY) - CAS(ID,IS-1,1) AC2(ID,IS-1,IX,IY)
!     ------------------------------------------------------------------
!                                      2 DS
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!        IC          Dummy variable: ICode gridpoint:
!                      IC = 1  Top or Bottom gridpoint
!                      IC = 2  Left or Right gridpoint
!                      IC = 3  Central gridpoint
!                    Whether which value IC has, depends of the sweep
!                    If necessary IC can be enlarged by increasing
!                    the array size of ICMAX
!        IS          Counter of relative frequency band
!        ID          Counter of directional distribution
!        ICMAX       Maximum counter for the points of the molecule
!        MXC         Maximum counter of gridpoints in x-direction
!        MYC         Maximum counter of gridpoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!                    one sweep
!
!
!        REALS:
!        ---------
!
!        DD          Width of spectral direction band
!        PNH         Equal to (1/2)*DD
!        PI          (3,14)
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        CAS     3D  Wave transport velocity in S-dirction, function of
!                    (ID,IS,IC)
!        IMATDA  2D  Coefficients of diagonal of matrix
!        IMAT5L  2D  Coefficients of lower diagonal of matrix
!        IMAT6U  2D  Coefficients of upper diagonal of matrix
!        ISCMIN  1D  Minimum counter in frequency space per direction
!        ISCMIN  1D  Maximum counter in frequency space per direction
!
!     5. SUBROUTINES CALLING
!
!        ACTION
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!        ---
!
!     8. REMARKS
!
!        ---
!
!     9. STRUCTURE
!
!   -----------------------------------------------------------
!   For every S and D-direction in direction of sweep do
!     Compute the derivative in S-direction:
!     ---------------------------------------------------------
!     Store the results of the transport terms in the
!     arrays IMATDA, IMAT5L, IMAT6U
!   -------------------------------------------------------------
!   End of STRSSI
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!****************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER  IS      ,ID      ,IDDLOW  ,IDDTOP  ,IDDUM

   REAL     DS      ,PNH     ,PN1     ,PN2     ,C1      ,C2      ,&
   &C3      ,A1      ,A3      ,PCD1    ,PCD2    ,PCD3, RHS12   ,&
   &RHS23   ,DIAG12  ,DIAG23
   REAL     TC12    ,TC23    ,S1      ,S3

   LOGICAL  BIN1    ,BIN3

   REAL     AC2(MDC,MSC,MCGRD)         ,&
   &CAS(MDC,MSC,ICMAX)         ,&
   &IMAT5L(MDC,MSC)            ,&
   &IMATDA(MDC,MSC)            ,&
   &IMAT6U(MDC,MSC)            ,&
   &IMATRA(MDC,MSC)
   REAL  :: TRAC0(MDC,MSC,MTRNP)
   REAL  :: TRAC1(MDC,MSC,MTRNP)

   INTEGER  ISCMIN(MDC)                ,&
   &ISCMAX(MDC)

   LOGICAL  ANYBIN(MDC,MSC)

   IF (LTRACE) CALL STRACE (IENT,'STRSSI')

   direction_loop: do IDDUM = IDDLOW, IDDTOP
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      IF (ISCMIN(ID).EQ.0) CYCLE direction_loop
      do IS = ISCMIN(ID), ISCMAX(ID)
         A1 = 0.
         A3 = 0.
         C2 = CAS(ID,IS,1)
         IF ( IS .EQ. 1 ) THEN
            C1   = 0.
            A1   = 0.
            BIN1 = .FALSE.
            C3   = CAS(ID,IS+1,1)
            BIN3 = ANYBIN(ID,IS+1)
            IF (.NOT.BIN3) A3 = AC2(ID,IS+1,KCGRD(1))
            DS   = SPCSIG(IS+1) - SPCSIG(IS)
            S1 = 0.
            S3 = SPCSIG(IS+1)
         ELSE IF ( IS .EQ. MSC ) THEN
            C1   = CAS(ID,IS-1,1)
            BIN1 = ANYBIN(ID,IS-1)
            IF (.NOT.BIN1) A1 = AC2(ID,IS-1,KCGRD(1))
            C3   = C2
            A3   = 0.
            BIN3 = .FALSE.
            DS   = SPCSIG(IS) - SPCSIG(IS-1)
            S1 = SPCSIG(IS-1)
            S3 = 0.
         ELSE
            C1   = CAS(ID,IS-1,1)
            C3   = CAS(ID,IS+1,1)
            BIN1 = ANYBIN(ID,IS-1)
            BIN3 = ANYBIN(ID,IS+1)
            IF (.NOT.BIN1) A1 = AC2(ID,IS-1,KCGRD(1))
            IF (.NOT.BIN3) A3 = AC2(ID,IS+1,KCGRD(1))
            DS   = 0.5 * ( SPCSIG(IS+1) - SPCSIG(IS-1) )
            S1 = SPCSIG(IS-1)
            S3 = SPCSIG(IS+1)
         END IF

         PNH = 1. / (2. * DS)
         PN1 =  (1. - PNUMS(7) ) * PNH
         PN2 =  (1. + PNUMS(7) ) * PNH

!         *** fill the lower diagonal and the diagonal ***

         IF ( C1 .GT. 1.E-8 .AND. C2 .GT. 1.E-8 ) THEN
            PCD1 = PN2 * C1
            PCD2 = PN1 * C2
         ELSE IF ( C1 .LT. -1.E-8 .AND. C2 .LT. -1.E-8 ) THEN
            PCD1 = PN1 * C1
            PCD2 = PN2 * C2
         ELSE
            PCD1 = PNH * C1
            PCD2 = PNH * C2
         END IF

         RHS12 = 0.
         TC12  = 0.
         IF ( IS .EQ. 1 .AND. C2.LT.0.) THEN
!           fully upwind approximation at the boundary of the frequency
            DIAG12 = - PCD1 - PCD2
         ELSE
            DIAG12 = - PCD2
            IF (BIN1) THEN
               IMAT5L(ID,IS) = IMAT5L(ID,IS) - PCD1
            ELSE
               RHS12 = PCD1 * A1
               TC12  = RHS12* S1
            ENDIF
         ENDIF

         IF ( C2 .GT. 1.E-8 .AND. C3 .GT. 1.E-8 ) THEN
            PCD2 = PN2 * C2
            PCD3 = PN1 * C3
         ELSE IF ( C2 .LT. -1.E-8 .AND. C3 .LT. -1.E-8 ) THEN
            PCD2 = PN1 * C2
            PCD3 = PN2 * C3
         ELSE
            PCD2 = PNH * C2
            PCD3 = PNH * C3
         END IF

         RHS23 = 0.
         TC23  = 0.
         IF (IS .EQ. MSC .AND. C2.GT.0.) THEN
!           full upwind approximation at the boundary
            DIAG23 = PCD2 + PCD3
         ELSE
            DIAG23 = PCD2
            IF (BIN3) THEN
               IMAT6U(ID,IS) = IMAT6U(ID,IS) + PCD3
            ELSE
               RHS23 = - PCD3 * A3
               TC23  = RHS23 * S3
            ENDIF
         ENDIF
         IMATDA(ID,IS) = IMATDA(ID,IS) + DIAG12 + DIAG23
         IMATRA(ID,IS) = IMATRA(ID,IS) + RHS12 + RHS23
         TRAC0(ID,IS,3) = TRAC0(ID,IS,3) - TC12 - TC23
         TRAC1(ID,IS,3) = TRAC1(ID,IS,3) + DIAG12 + DIAG23
      end do
   end do direction_loop

!     *** test output ***

   IF ( TESTFL .AND. ITEST .GE. 35 ) THEN
      WRITE(PRINTF,"(' STRSSI: POINT IDDLOW IDDTOP :',3I5)") KCGRD(1), IDDLOW, IDDTOP
      WRITE(PRINTF,"(' STRSSI: CSS :',2E12.4)") PNUMS(7)
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' matrix coefficients in STRSSI'
      WRITE(PRINTF,*)
      WRITE(PRINTF,*)&
      &'   IS   ID    IMAT5L       IMATDA       IMAT6U    IMATRA    CAS'
      DO IDDUM = IDDLOW, IDDTOP
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
         IF (ISCMIN(ID).GT.0) THEN
            DO IS = ISCMIN(ID), ISCMAX(ID)
               WRITE(PRINTF,"(1X,2I4,4X,4E12.4,E10.2)") IS, ID, IMAT5L(ID,IS),IMATDA(ID,IS),&
               &IMAT6U(ID,IS),IMATRA(ID,IS),CAS(ID,IS,1)
            ENDDO
         ENDIF
      ENDDO
   END IF

!     End of subroutine STRSSI
   RETURN
end subroutine STRSSI

!****************************************************************

SUBROUTINE STRSSB (IDDLOW  ,IDDTOP  ,&
&IDCMIN  ,IDCMAX  ,ISSTOP  ,CAX     ,CAY     ,&
&CAS     ,AC2     ,SPCSIG  ,IMATRA  ,&
&ANYBLK  ,RDX     ,RDY     ,TRAC0            )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
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
!     30.72: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!     41.07: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.07, Jul. 09: also central scheme blended with upwind scheme
!
!  2. Purpose
!
!     comp. of @[CAS AC2]/@S initial & boundary with an explicit
!     scheme. The energy near the blocking point is removed
!     from the spectrum based on a CFL criterion
!
!     The frequencies beyond ISSTOP are blocked in a 1-D situation
!     For a 2-D case the situation is somewhat more complicated (
!     see below)
!
!
!        ^  |                       1-D case
!     E()|  |          *            ========
!           |        *   *
!           |              *
!           |       *        *      / blocking frequency
!           |                .... /
!           |      *         ....| *
!           |        SWEEP 1 ....| o o *
!           |     *          ....| o o o o o*
!          0---------------------|-----------|---------
!           0                  ISSTOP       MSC   --> s
!
!                           -|---|-
!                              ^
!                              |---- CFL > 0.5sqrt(2) -> ANYBLK = true
!
!
!               ANYBIN = TRUE     ANYBIN = FALSE
!           |--------------------|-----------|
!
!
!  3. Method
!
!     Compute the derivative in s-direction:
!     The nearby points are indicated with the index IC (see
!     FUNCTION ICODE(_,_):
!     Central grid point     : IC = 1
!     Point in X-direction   : IC = 2
!     Point in Y-direction   : IC = 3
!
!     @[CAS AC2]
!     --------- =
!        @S
!
!     CAS*AC2(ID,IS) - CAS*AC2(ID,IS-1)     F(IS+0.5) - F(IS-0.5)
!     ---------------------------------- = -----------------------
!                   DS                                DS
!
!                  /  CAS(IS+0.5) * ( (1-0.5mu)*AC2(IS+1) + 0.5mu*AC2(IS) )    IF CAS(IS+0.5) < 0
!     F(IS+0.5) =  |
!                  \  CAS(IS+0.5) * ( (1-0.5mu)*AC2(IS) + 0.5mu*AC2(IS+1) )    IF CAS(IS+0.5) > 0
!
!                  /  CAS(IS-0.5) * ( (1-0.5mu)*AC2(IS-1) + 0.5mu*AC2(IS) )    IF CAS(IS-0.5) > 0
!     F(IS-0.5) =  |
!                  \  CAS(IS-0.5) * ( (1-0.5mu)*AC2(IS) + 0.5mu*AC2(IS-1) )    IF CAS(IS-0.5) < 0
!
!     with
!
!           0 <= mu <= 1 a blending factor
!
!           mu = 0 corresponds to 1st order upwind scheme
!           mu = 1 corresponds to 2nd order central scheme
!
!           default value, mu = 0.5
!
!    and
!
!           CAS(IS+0.5) = ( CAS(IS+1) + CAS(IS) ) / 2.
!
!           CAS(IS-0.5) = ( CAS(IS) + CAS(IS-1) ) / 2.
!
!
!     ------------------------------------------------------------
!     Courant-Friedlich-Levich criterion :
!
!                  | Cs |
!                  | -- |
!                  | ds |         <
!               ---------------   =  0.5 * sqrt(2.0)
!      CFL  =  | Cx |   | Cy |
!              | -- | + | -- |
!              | dx |   | dy |
!
!     For a bin in which the CFL criterion is larger two
!     ways are possible:
!
!            1)  Cs can be limited
!            2)  Action in bin can be set equal zero
!
!     --------------------------------------------------------------
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!        IC          Dummy variable: ICode gridpoint:
!                      IC = 1  Top or Bottom gridpoint
!                      IC = 2  Left or Right gridpoint
!                      IC = 3  Central gridpoint
!                    Whether which value IC has, depends of the sweep
!                    If necessary IC can be enlarged by increasing
!                    the array size of ICMAX
!        IX          Counter of gridpoints in x-direction
!        IY          Counter of gridpoints in y-direction
!        IS          Counter of relative frequency band
!        ID          Counter of directional distribution
!        ICMAX       Maximum counter for the points of the molecule
!        MXC         Maximum counter of gridpoints in x-direction
!        MYC         Maximum counter of gridpoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!        ISSTOP      Maximum frequency counter for wave components
!                    that are propagated within a sweep
!        IDDLOW      Minimum direction that is propagated within a
!                    sweep
!        IDDTOP      Idem maximum
!
!        REALS:
!        ---------
!
!        FSA_        Dummy variable
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        AC2     4D  Action density as function of D,S,X,Y at time T
!        CAS     3D  Wave transport velocity in S-dirction, function of
!                    (ID,IS,IC)
!        CAX, CAY    Propagation velocities in x-y space
!        IMATRA  2D  Coefficients of right hand side of matrix
!        ISCMIN  1D  Diractional dependent counter
!        ISCMIN  1D  Directional dependent counter
!        ANYBLK  2D  Determines if a bin is BLOCKED by a counter current
!                    based on a CFL criterion
!
!     5. SUBROUTINES CALLING
!
!        ACTION
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!     8. REMARKS
!
!        ---
!
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   For every S and D-direction in direction of sweep do,
!     Determine if CFL criterion is satisfied
!     Compute the derivative in s-direction:
!     ---------------------------------------------------------
!     Compute transportation terms
!     Store the terms in the array IMATRA
!   -------------------------------------------------------------
!   End of STRSSB
!   -------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER  IS      ,ID      ,ISSTOP  ,&
   &IDDLOW  ,IDDTOP  ,IDDUM

   REAL     FSA     ,FLEFT   ,FRGHT   ,DS      ,CFLMAX  ,CFLCEN  ,&
   &CAXCEN  ,CAYCEN  ,CASCEN  ,TX      ,TY      ,TS      ,&
   &CASL    ,CASR    ,PN1     ,PN2

   REAL     CAS(MDC,MSC,ICMAX)       ,&
   &CAX(MDC,MSC,ICMAX)       ,&
   &CAY(MDC,MSC,ICMAX)       ,&
   &AC2(MDC,MSC,MCGRD)       ,&
   &IMATRA(MDC,MSC)          ,&
   &RDX(*)                   ,&
   &RDY(*)
!  RDX/RDY assumed-size: the unstructured caller passes a 2-element array and
!  STRSSB only reads RDX(1:2). The module's explicit interface rejected the old
!  RDX(MICMAX) declaration against that shorter actual argument.
   REAL  :: TRAC0(MDC,MSC,MTRNP)

   INTEGER  IDCMIN(MSC)              ,&
   &IDCMAX(MSC)

   LOGICAL  ANYBLK(MDC,MSC)

   IF (LTRACE) CALL STRACE (IENT,'STRSSB')

!     --- determine blending factor

   PN1 = 0.5*(1.+PNUMS(7))
   PN2 = 0.5*(1.-PNUMS(7))

!     *** initialization of ANYBLK and CFLMAX value ***

   DO IS = 1, MSC
      DO ID = 1, MDC
         ANYBLK(ID,IS) = .FALSE.
      ENDDO
   ENDDO
   CFLMAX = PNUMS(19)

   DO IS = 1, ISSTOP
      IF ( IS .EQ. 1 ) THEN
         DS = SPCSIG(IS+1) - SPCSIG(IS)
      ELSE IF ( IS .EQ. MSC ) THEN
         DS = SPCSIG(IS) - SPCSIG(IS-1)
      ELSE
         DS = 0.5 * ( SPCSIG(IS+1) - SPCSIG(IS-1) )
      END IF
      DO IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
         CAXCEN = ABS ( CAX(ID,IS,1) )
         CAYCEN = ABS ( CAY(ID,IS,1) )
         CASCEN = ABS ( CAS(ID,IS,1) )

         TX     = RDX(1) * CAXCEN + RDX(2) * CAXCEN
         TY     = RDY(1) * CAYCEN + RDY(2) * CAYCEN

         TS     = CASCEN / DS
         CFLCEN = TS / MAX( 1.E-20 , ( TX + TY ) )
         FRGHT = 0.
         FLEFT = 0.

!         *** check if a bin can be propagated or if it is blocked ***

         IF ( CFLCEN .GT. CFLMAX ) THEN

!           *** de-activate bin in solver by ANYBLK ***

            ANYBLK(ID,IS) = .TRUE.

         ELSE

!           *** calculate transport in frequency space ***

            IF ( IS .EQ. 1 ) THEN
!             *** for first point an upwind scheme is used ***
               CASR  = 0.5 * ( CAS(ID,IS,1) + CAS(ID,IS+1,1) )
               IF ( CASR .LT. 0. ) THEN
                  FRGHT = CASR * AC2(ID,IS+1,KCGRD(1))
               ELSE
                  FRGHT = CASR * AC2(ID,IS  ,KCGRD(1))
               END IF
               FLEFT = 0.
            ELSE IF ( IS .EQ. MSC ) THEN
!             *** for the last discrete point in frequency space ***
!             *** an upwind scheme is used                       ***
               CASL = CAS(ID,IS-1,1)
               CASR = CAS(ID,IS  ,1)
               IF ( CASL .LT. 0. ) THEN
                  FLEFT = CASL * AC2(ID,IS  ,KCGRD(1))
               ELSE
                  FLEFT = CASL * AC2(ID,IS-1,KCGRD(1))
               END IF
               IF ( CASR .LT. 0. ) THEN
!               *** assumption has been made that the flux is ***
!               *** zero for the bin beyond MSC               ***
                  FRGHT = 0.
               ELSE
                  FRGHT = CASR * AC2(ID,IS,KCGRD(1))
               END IF
            ELSE
!             *** point in frequency range ***
               CASL  = 0.5 * ( CAS(ID,IS,1) + CAS(ID,IS-1,1) )
               CASR  = 0.5 * ( CAS(ID,IS,1) + CAS(ID,IS+1,1) )
               IF ( CASL .LT. 0. ) THEN
                  FLEFT = CASL * ( PN1*AC2(ID,IS  ,KCGRD(1)) +&
                  &PN2*AC2(ID,IS-1,KCGRD(1)) )
               ELSE
                  FLEFT = CASL * ( PN1*AC2(ID,IS-1,KCGRD(1)) +&
                  &PN2*AC2(ID,IS  ,KCGRD(1)) )
               END IF
               IF ( CASR .LT. 0. ) THEN
                  FRGHT = CASR * ( PN1*AC2(ID,IS+1,KCGRD(1)) +&
                  &PN2*AC2(ID,IS  ,KCGRD(1)) )
               ELSE
                  FRGHT = CASR * ( PN1*AC2(ID,IS  ,KCGRD(1)) +&
                  &PN2*AC2(ID,IS+1,KCGRD(1)) )
               END IF
            END IF

            FSA  = ( FRGHT - FLEFT ) / DS

!           *** all the terms are known, store in IMATRA ***

            IMATRA(ID,IS) = IMATRA(ID,IS) - FSA
            TRAC0(ID,IS,3) = TRAC0(ID,IS,3) + FSA
         ENDIF

!         *** test output ***

         IF ( ITEST .GE. 50 .AND. TESTFL ) THEN
            WRITE(PRINTF,"(' STRSSB: FR FL CFLC ANYBLK:',2I3,3E12.4,L3)") IS,ID,FRGHT,FLEFT,CFLCEN,ANYBLK(ID,IS)
         END IF


      ENDDO
   ENDDO

!     *** test output ***

   IF ( ITEST .GE. 50 .AND. TESTFL ) THEN
      WRITE(PRINTF,"(' BLOCKB : MDC MSC MCGRD : ',3I5)") MDC,MSC,MCGRD
      WRITE(PRINTF,"(' BLOCKB : POINT ISSTOP CFLMAX: ',2I5,F8.4)") KCGRD(1), ISSTOP, CFLMAX
      WRITE(PRINTF,"(' Active bins within a sweep -> ID: ',I3,' to ',I3)") IDDLOW, IDDTOP
      WRITE(PRINTF,*)
      WRITE(PRINTF,*)(' Propagation of bin if blocking can occur')
      WRITE(PRINTF,*)('   1) No blocking of bin -> ANYBLK = .F.')
      WRITE(PRINTF,*)('   2) Blocking of bin    -> ANYBLK = .T.')
      WRITE(PRINTF,*)
      DO IDDUM = IDDTOP+1, IDDLOW-1, -1
         ID = MOD ( IDDUM - 1 + MDC, MDC) + 1
         WRITE(PRINTF,"(I4,25L3)") ID, (ANYBLK(ID,IS),IS=1,MIN(ISSTOP,25))
      ENDDO
      WRITE(PRINTF,"(6X,'1',9X,5(I3,12X))")(IS, IS=1+4, MIN(ISSTOP,25), 5 )
      WRITE(PRINTF,*)

   ENDIF

!     End of subroutine STRSSB
   RETURN
end subroutine STRSSB

!****************************************************************

SUBROUTINE STRSD (DD      ,IDCMIN  ,&
&IDCMAX  ,CAD     ,IMATLA  ,IMATDA  ,IMATUA  ,&
&IMATRA  ,AC2     ,ISSTOP  ,&
&ANYBIN  ,LEAKC1  ,TRAC0   ,TRAC1            )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units


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
!     1. UPDATE
!
!        40.41, Oct. 04: common blocks replaced by modules, include files removed
!        40.85, Aug. 08: store theta-propagation for output purposes
!
!     2. PURPOSE
!
!        comp. of @[CAD AC2]/@D initial & boundary
!
!     3. METHOD
!
!        Compute the derivative in D-direction only n the central
!        gridpoint considered:
!                                Central grid point     : IC = 1
!
!       Depending on the parameter PNUMS(6) either a central difference
!       scheme (PNUMS(6) = 0) or an upstream scheme (PNUMS(6) = 1) is
!       used. Points 1, 2 and 3 are three consecutive points on the
!       T-axis. 2 is the central point for which @(C*A)/@THETA and
!       @(C*W*A)/@THETA is computed.
!
!                 1       2       3
!              ---O-------O-------O--- > THETA
!
!
!        PNUMS() = 0.  central difference scheme
!        PNUMS() = 1.  upwind scheme
!
!        @[CAD AC2]
!        ----------  =
!           @D
!
!        CAD(ID+1,IS,1) AC2(ID+1,IS,IX,IY) - CAD(ID-1,IS,1) AC2(ID-1,IS,IX,IY)
!        --------------------------------------------------------------------
!                                         2*DD
!
!     4. PARAMETERLIST
!
!        IC          Dummy variable: ICode gridpoint:
!                      IC = 1  Top or Bottom gridpoint
!                      IC = 2  Left or Right gridpoint
!                      IC = 3  Central gridpoint
!                    Whether which value IC has, depends of the sweep
!                    If necessary IC can be enlarged by increasing
!                    the array size of ICMAX
!        IS          Counter of relative frequency band
!        ID          Counter of directional distribution
!        ICMAX       Maximum counter for the points of the molecule
!        MXC         Maximum counter of gridpoints in x-direction
!        MYC         Maximum counter of gridpoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!        FULCIR      logical: if true, computation on a full circle
!
!        REALS:
!        ---------
!
!        DD          Width of spectral direction band
!        PNH         Equal to (1/2)*DD
!        PI          (3,14)
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        CAD     3D  Wave transport velocity in S-dirction, function of
!                    (ID,IS,IC)
!        IMATDA  2D  Coefficients of diagonal of matrix
!        IMATLA  2D  Coefficients of lower diagonal of matrix
!        IMATUA  2D  Coefficients of upper diagonal of matrix
!        IDCMIN  1D  frequency dependent counter
!        IDCMIN  1D  frequency dependent counter
!        ANYBIN  2D  see subr SWPSEL
!        LEAKC1  2D  leak coefficient
!
!     5. SUBROUTINES CALLING
!
!        ACTION
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!     8. REMARKS
!
!        ---
!
!     9. STRUCTURE
!
!   -----------------------------------------------------------
!   For every S and D-direction in direction of sweep do
!     Compute the derivative in D-direction:
!     ---------------------------------------------------------
!     Store the results of the transport terms in the
!     arrays IMATDA, IMATLA, IMATUA
!   -------------------------------------------------------------
!   End of STRSD
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!****************************************************************

   LOGICAL  BIN1, BIN2, BIN3

   INTEGER, SAVE :: IENT = 0
   INTEGER  IS    ,ID    ,IIDM  ,IIDP  ,&
   &ISSTOP,IDDUM

   REAL     DD, PNH, PN1, PN2, A1, A3, C1, C2, C3
   REAL     DIAG12, DIAG23, PCD1, PCD2, PCD3, RHS12, RHS23

   REAL     AC2(MDC,MSC,MCGRD)         ,&
   &CAD(MDC,MSC,ICMAX)         ,&
   &IMATLA(MDC,MSC)            ,&
   &IMATDA(MDC,MSC)            ,&
   &IMATUA(MDC,MSC)            ,&
   &IMATRA(MDC,MSC)            ,&
   &LEAKC1(MDC,MSC)
   REAL  :: TRAC0(MDC,MSC,MTRNP)
   REAL  :: TRAC1(MDC,MSC,MTRNP)

   INTEGER  IDCMIN(MSC)                ,&
   &IDCMAX(MSC)

   LOGICAL  ANYBIN(MDC,MSC)

   IF (LTRACE) CALL STRACE (IENT,'STRSD')

   PNH = 1. / (2. * DD)
   PN1 =  (1. - PNUMS(6) ) * PNH
   PN2 =  (1. + PNUMS(6) ) * PNH

   do IS = 1, ISSTOP
      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD (IDDUM-1+MDC, MDC) + 1
         C2 = CAD(ID,IS,1)
         BIN2 = ANYBIN(ID,IS)
         IF (BIN2) THEN
            IF (FULCIR .OR. ID.GT.1) THEN
               IIDM = MOD (IDDUM-2+MDC, MDC) + 1
               C1   = CAD(IIDM,IS,1)
               BIN1 = ANYBIN(IIDM,IS)
               IF (.NOT.BIN1) A1 = AC2(IIDM,IS,KCGRD(1))
            ELSE
               IIDM = 0
               C1   = C2
               BIN1 = .FALSE.
               A1   = 0.
            ENDIF
            IF (FULCIR .OR. ID.LT.MDC) THEN
               IIDP = MOD (IDDUM+MDC, MDC) + 1
               C3   = CAD(IIDP,IS,1)
               BIN3 = ANYBIN(IIDP,IS)
               IF (.NOT.BIN3) A3 = AC2(IIDP,IS,KCGRD(1))
            ELSE
               IIDP = 0
               C3   = C2
               BIN3 = .FALSE.
               A3   = 0.
            ENDIF

!           *** fill the lower diagonal and the diagonal ***

            IF ( C1 .GT. 1.E-8 .AND. C2 .GT. 1.E-8 ) THEN
               PCD1 = PN2 * C1
               PCD2 = PN1 * C2
            ELSE IF ( C1 .LT. -1.E-8 .AND. C2 .LT. -1.E-8 ) THEN
               PCD1 = PN1 * C1
               PCD2 = PN2 * C2
            ELSE
               PCD1 = PNH * C1
               PCD2 = PNH * C2
            END IF

            RHS12 = 0.
            IF (IIDM.EQ.0 .AND. C2.LT.0.) THEN
!             fully upwind approximation at the boundary of the directional
!             sector
               DIAG12 = - PCD1 - PCD2
               LEAKC1(ID,IS) = -C2
            ELSE
               DIAG12 = - PCD2
               IF (BIN1) THEN
                  IMATLA(ID,IS) = IMATLA(ID,IS) - PCD1
               ELSE
                  RHS12 = PCD1 * A1
               ENDIF
            ENDIF

            IF ( C2 .GT. 1.E-8 .AND. C3 .GT. 1.E-8 ) THEN
               PCD2 = PN2 * C2
               PCD3 = PN1 * C3
            ELSE IF ( C2 .LT. -1.E-8 .AND. C3 .LT. -1.E-8 ) THEN
               PCD2 = PN1 * C2
               PCD3 = PN2 * C3
            ELSE
               PCD2 = PNH * C2
               PCD3 = PNH * C3
            END IF

            RHS23 = 0.
            IF (IIDP.EQ.0 .AND. C2.GT.0.) THEN
!             full upwind approximation at the boundary
               DIAG23 = PCD2 + PCD3
               LEAKC1(ID,IS) = C2
            ELSE
               DIAG23 = PCD2
               IF (BIN3) THEN
                  IMATUA(ID,IS) = IMATUA(ID,IS) + PCD3
               ELSE
                  RHS23 = - PCD3 * A3
               ENDIF
            ENDIF
            IMATDA(ID,IS) = IMATDA(ID,IS) + DIAG12 + DIAG23
            IMATRA(ID,IS) = IMATRA(ID,IS) + RHS12 + RHS23
            TRAC0(ID,IS,2) = TRAC0(ID,IS,2) - RHS12 - RHS23
            TRAC1(ID,IS,2) = TRAC1(ID,IS,2) + DIAG12 + DIAG23
         ENDIF

      end do
   end do

!     *** test output

   IF ( ITEST .GE. 80 .AND. TESTFL ) THEN
      WRITE(PRINTF,"(' FULL CIRCLE ',L4)") FULCIR
      WRITE(PRINTF,"(' STRSD :POINT ISTOP CDD :',2I5,E12.4)") KCGRD(1), ISSTOP, PNUMS(6)
      WRITE(PRINTF,"(' STRSD : PN1 PN2 PNH DD :',4E12.4)") PN1, PN2, PNH ,DD
   END IF

!     End of subroutine STRSD
   RETURN
end subroutine STRSD

!****************************************************************

SUBROUTINE STRSDFV (DD      ,IDCMIN  ,&
&IDCMAX  ,CAD     ,IMATLA  ,IMATDA  ,IMATUA  ,&
&IMATRA  ,AC2     ,ISSTOP  ,&
&ANYBIN  ,LEAKC1  ,TRAC0   ,TRAC1            )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_physics_selection
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units

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
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     40.23, Nov. 02: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: store theta-propagation for output purposes
!
!  2. Purpose
!
!     computation of @[CAD AC2]/@D based on finite volume method
!
!  4. Argument variables
!
!     AC2         action density
!     ANYBIN      logical: indicate whether current bin is selected
!     CAD         wave transport velocity in D-direction
!     DD          width of spectral direction band
!     IDCMAX      frequency dependent counter
!     IDCMIN      frequency dependent counter
!     IMATDA      coefficients of diagonal of matrix
!     IMATLA      coefficients of lower diagonal of matrix
!     IMATRA      coefficients of right hand side
!     IMATUA      coefficients of upper diagonal of matrix
!     ISSTOP      maximum frequency counter in a sweep
!     LEAKC1      leak coefficient
!     TRAC0       transport coefficient for output
!     TRAC1       transport coefficient for output

   INTEGER ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)
   REAL    DD
   REAL    AC2(MDC,MSC,MCGRD),&
   &CAD(MDC,MSC,ICMAX),&
   &IMATLA(MDC,MSC)   ,&
   &IMATDA(MDC,MSC)   ,&
   &IMATUA(MDC,MSC)   ,&
   &IMATRA(MDC,MSC)   ,&
   &LEAKC1(MDC,MSC)
   REAL    TRAC0(MDC,MSC,MTRNP)
   REAL    TRAC1(MDC,MSC,MTRNP)
   LOGICAL ANYBIN(MDC,MSC)

!  6. Local variables
!
!     ACM   :     lower action
!     ACP   :     upper action
!     BINM  :     indicate whether lower bin is selected
!     BINP  :     indicate whether upper bin is selected
!     CADM  :     lower wave transport velocity
!     CADP  :     upper wave transport velocity
!     CAN   :     negative wave transport velocity
!     CAP   :     positive wave transport velocity
!     CAV   :     averaged wave transport velocity; may be
!                 regarded as flux velocity
!     DDI   :     inverse of width of spectral direction band
!     ID    :     counter
!     IDDUM :     loop counter
!     IDM   :     index of point ID-1
!     IDP   :     index of point ID+1
!     IENT  :     number of entries
!     IS    :     loop counter
!     MU    :     blending parameter in between 0 and 1
!                 =0; central differences
!                 =1; first order upwind

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IDM, IDP, IDDUM, IS
   REAL    ACM, ACP, CADM, CADP,&
   &CAN, CAP, CAV, DDI, MU
   LOGICAL BINM, BINP

!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     ACTION (in SWANCOM1)
!
! 12. Structure
!
!     For every S and D-direction in direction of sweep do
!
!        Compute the derivative in D-direction and
!        store the results of this calculation in the
!        arrays IMATDA, IMATLA, IMATUA and IMATRA
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'STRSDFV')

   DDI = 1./DD
   MU  = PNUMS(6)

   do IS = 1, ISSTOP

      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD (IDDUM-1+MDC, MDC) + 1

         IF ( ANYBIN(ID,IS) ) THEN

            IF ( IDDUM.EQ.IDCMIN(IS) ) THEN

               IF ( FULCIR .OR. ID.GT.1 ) THEN
                  IDM  = MOD (IDDUM-2+MDC, MDC) + 1
                  CADM = CAD(IDM,IS,1)
                  BINM = ANYBIN(IDM,IS)
                  IF (.NOT.BINM) ACM = AC2(IDM,IS,KCGRD(1))
               ELSE
                  IDM  = 0
                  CADM = CAD(ID,IS,1)
                  BINM = .FALSE.
                  ACM  = 0.
               END IF

               CAV = 0.5*(CAD(ID,IS,1) + CADM)
               CAP = 0.5*(CAV + ABS(CAV))
               CAN = 0.5*(CAV - ABS(CAV))

!                 put them in blended form
               CAP = CAP*MU + 0.5*CAV*(1.-MU)
               CAN = CAN*MU + 0.5*CAV*(1.-MU)

               IF ( .NOT.BINM ) THEN
                  IMATDA(ID,IS) = IMATDA(ID,IS)  - DDI * CAN
                  IMATRA(ID,IS) = IMATRA(ID,IS)  + DDI * CAP * ACM
                  TRAC0(ID,IS,2) = TRAC0(ID,IS,2) - DDI * CAP * ACM
                  TRAC1(ID,IS,2) = TRAC1(ID,IS,2) - DDI * CAN
               END IF

               IF ( IDM.EQ.0 ) LEAKC1(ID,IS) = -CAN

            END IF

            IF ( FULCIR .OR. ID.LT.MDC ) THEN
               IDP  = MOD (IDDUM+MDC, MDC) + 1
               CADP = CAD(IDP,IS,1)
               BINP = ANYBIN(IDP,IS)
               IF (.NOT.BINP) ACP = AC2(IDP,IS,KCGRD(1))
            ELSE
               IDP  = 0
               CADP = CAD(ID,IS,1)
               BINP = .FALSE.
               ACP  = 0.
            END IF

            CAV = 0.5*(CAD(ID,IS,1) + CADP)
            CAP = 0.5*(CAV + ABS(CAV))
            CAN = 0.5*(CAV - ABS(CAV))

!              put them in blended form
            CAP = CAP*MU + 0.5*CAV*(1.-MU)
            CAN = CAN*MU + 0.5*CAV*(1.-MU)

            IF ( BINP ) THEN
               IMATDA(ID ,IS) = IMATDA(ID ,IS) + DDI * CAP
               IMATUA(ID ,IS) = IMATUA(ID ,IS) + DDI * CAN
               IMATDA(IDP,IS) = IMATDA(IDP,IS) - DDI * CAN
               IMATLA(IDP,IS) = IMATLA(IDP,IS) - DDI * CAP
               TRAC1(ID ,IS,2) = TRAC1(ID ,IS,2) + DDI * CAP
               TRAC1(IDP,IS,2) = TRAC1(IDP,IS,2) - DDI * CAN
            ELSE
               IMATDA(ID,IS) = IMATDA(ID,IS) + DDI * CAP
               IMATRA(ID,IS) = IMATRA(ID,IS) - DDI * CAN * ACP
               TRAC0(ID,IS,2) = TRAC0(ID,IS,2) + DDI * CAN * ACP
               TRAC1(ID,IS,2) = TRAC1(ID,IS,2) + DDI * CAP
            END IF

            IF ( IDP.EQ.0 ) LEAKC1(ID,IS) = CAP

         END IF

      end do
   end do

!     --- test output

   IF ( TESTFL .AND. ITEST.GE.80 ) THEN
      WRITE(PRINTF,"(' STRSDFV: POINT ISSTOP :',2I5)") KCGRD(1), ISSTOP
      WRITE(PRINTF,"(' STRSDFV: CDD :',E12.4)") PNUMS(6)
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' matrix coefficients in STRSDFV'
      WRITE(PRINTF,*)
      WRITE(PRINTF,*)&
      &'   IS ID      IMATLA      IMATDA      IMATUA      IMATRA     CAD'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD (IDDUM-1+MDC, MDC) + 1
            WRITE(PRINTF,"(1X,2I4,4X,4E12.4,E10.2)") IS, ID, IMATLA(ID,IS),IMATDA(ID,IS),&
            &IMATUA(ID,IS),IMATRA(ID,IS),CAD(ID,IS,1)
         END DO
      END DO
   END IF

   RETURN
end subroutine STRSDFV

!****************************************************************

SUBROUTINE SPREDT (SWPDIR     ,AC2        ,CAX       ,&
&CAY        ,IDCMIN     ,IDCMAX    ,&
&ISSTOP     ,ANYBIN     ,&
&XCGRID     ,YCGRID     ,&
&RDX        ,RDY        ,OBREDF    )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_stencil
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_spherical_geometry
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
!     0. Authors
!
!     40.00, 40.13: Nico Booij
!     40.41: Marcel Zijlema
!     41.53: Marcel Zijlema
!
!     1. UPDATE
!
!        40.00, Aug 98: introduction of obstacle reduction factor to
!                       obtain correct initialisation
!                       argument list changed, swcomm3 added
!        40.13, Aug 01: modification of action densities is skipped
!                       in case of Mode Noupdate
!        40.41, Oct 04: common blocks replaced by modules, include files removed
!        41.53, Oct 14: correction curvilinear grid
!
!     2. PURPOSE
!
!        to estimate the action density depending of the sweep
!        direction during the first iteration of a stationary
!        computation. The reason for this is that AC2 is zero
!        at first iteration and no initialisation is given in
!        case of stationarity (NSTATC=0). Action density should
!        be nonzero because of the computation of the source
!        terms. The estimate is based on solving the equation
!
!            dN       dN
!        CAX -- + CAY -- = 0
!            dx       dy
!
!        in an explicit manner. In the estimate, the transmission
!        through obstacles or reflection at obstacles is taken into
!        account
!
!     3. METHOD
!
!
!          [RDX1*CAX + RDY1*CAY]*N(i-1,j) + [RDX2*CAX + RDY2*CAY]*N(i,j-1)
! N(i,j) = ---------------------------------------------------------------
!                      (RDX1+RDX2) * CAX  +  (RDY1+RDY2) * CAY
!
!     4. PARAMETERLIST
!
!       INTEGERS:
!       ---------
!       IC           Dummy variable: ICode gridpoint:
!                    IC = 1  Top or Bottom gridpoint
!                    IC = 2  Left or Right gridpoint
!                    IC = 3  Central gridpoint
!                    Whether which value IC has, depends of the sweep
!                    If necessary ic can be enlarged by increasing
!                    the array size of ICMAX
!       IX           Counter of gridpoints in x-direction
!       IY           Counter of gridpoints in y-direction
!       IS           Counter of relative frequency band
!       ID           Counter of directional distribution
!       ICMAX        Maximum array size for the points of the molecule
!       MXC          Maximum counter of gridppoints in x-direction
!       MYC          Maximum counter of gridppoints in y-direction
!       MSC          Maximum counter of relative frequency
!       MDC          Maximum counter of directional distribution
!       KSX          Dummy variable to get the right sign in the
!                    numerical difference scheme in X-direction
!                    depending on the sweep direction, KSX = -1 or +1
!       KSY          Dummy variable to get the right sign in the
!                    numerical difference scheme in Y-direction
!                    depending on the sweep direction, KSY = -1 or +1
!       SWPDIR       Sweep direction (..) (identical at the description
!                    of the direction the wind is blowing)
!
!       REALS:
!       ------
!
!       DX           Length of spatial cell in X-direction
!       DY           Length of spatial cell in Y-direction
!       ALEN         Part of side length of an angle side
!       BLEN         Part of side length of an angle side
!       LDIAG        Length of the diagonal of grid cel
!       ALPHA        angle of propagation velocity
!       BETA         angle between DX end DY
!       GAMMA        PI - alpha - beta
!       PI           3,14.......
!       FAC_A        Factor representing the influence of the action-
!                    density depening of the propagation velocity
!       FAC_B        Factor representing the influence of the action-
!                    density depening of the propagation velocity
!
!       REAL arrays:
!       -------------
!
!       AC2    4D    Action density as function of D,S,X,Y at time T
!       CAX    3D    Wave transport velocity in x-direction, function of
!                    (ID,IS,IC)
!       CAY    3D    Wave transport velocity in y-direction, function of
!                    (ID,IS,IC)
!       IDCMIN 1D    frequency dependent counters in case of a current
!       IDCMAX 1D    frequency dependent counters in case of a current
!       ANYBIN 2D    Determines if a bin fall within a sweep
!       XCGRID       coordinates of computational grid in x-direction
!       YCGRID       coordinates of computational grid in y-direction
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!     8. REMARKS
!
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   For every sweep direction do,
!     For every point in S and D direction in sweep direction do,
!       predict values for action density at new point from values
!       of neighbour gridpoints taking into account spectral propagation
!       direction (with currents !!) and the boundary conditions.
!       --------------------------------------------------------
!       If wave action AC2 is negative, then
!         Give wave action initial value 1.E-10
!     ---------------------------------------------------------
!   End of SPREDT
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

   INTEGER  IS    ,ID    ,&
   &SWPDIR,IDDUM ,ISSTOP

   REAL     FAC_A ,FAC_B, WEIG1, WEIG2, TCF1, TCF2, CDEN, CNUM

   REAL  :: AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL  :: CAX(MDC,MSC,MICMAX)
   REAL  :: CAY(MDC,MSC,MICMAX)
!  RDX/RDY assumed-size: unstructured callers pass a 2-element array; SPREDT
!  only reads RDX(1:2). XCGRID/YCGRID are OPTIONAL because they are dereferenced
!  only on the OPTG==3 (curvilinear) branch below, which unstructured (OPTG==5)
!  never reaches. Passing under-sized/scalar placeholders here was a latent
!  mismatch that the module's explicit interface now rejects.
   REAL  :: RDX(*),  RDY(*),&
   &OBREDF(MDC,MSC,2)
   REAL, OPTIONAL  :: XCGRID(MXC,MYC), YCGRID(MXC,MYC)

   INTEGER  IDCMIN(MSC)              ,&
   &IDCMAX(MSC)

   LOGICAL  ANYBIN(MDC,MSC)

   INTEGER  IDIR, IDCUM, IDMIN, IDMAX, NCURID, NID(4)
   REAL     IDX, IDY, CLAT

   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT,'SPREDT')

   IF ( OPTG.EQ.3 .AND. CCURV ) THEN

!        --- curvilinear grid
!            consider the equation on a rectangular grid
!            so that all bins will be filled

      IDX=1./(XCGRID(IXCGRD(1),IYCGRD(1))-XCGRID(IXCGRD(2),IYCGRD(2)))
      IDY=1./(YCGRID(IXCGRD(1),IYCGRD(1))-YCGRID(IXCGRD(3),IYCGRD(3)))

      IF ( KSPHER.GT.0 ) THEN
         CLAT = COS(DEGRAD*(YCGRID(IXCGRD(1),IYCGRD(1))+YOFFS))
         IDX  = IDX / (CLAT * LENDEG)
         IDY  = IDY / LENDEG
      ENDIF

      IDCUM = 0
      DO IDIR = 1, 4
         NID(IDIR) = (MDC*IDIR)/4 - IDCUM
         IDCUM     = (MDC*IDIR)/4
      ENDDO

      ! determine loop bounds in spectral space for current sweep

      IDMIN = MDC+1
      IDMAX = 0

      IDIR   = 1
      NCURID = 0

      DO ID = 1, MDC

         IF ( IDIR.EQ.SWPDIR ) THEN
            IDMIN = MIN(ID,IDMIN)
            IDMAX = MAX(ID,IDMAX)
         ENDIF
         NCURID = NCURID + 1

         IF ( NCURID.GE.NID(IDIR) ) THEN
            IDIR   = IDIR + 1
            NCURID = 0
         ENDIF

      ENDDO

      DO IS = 1, MSC
         DO ID = IDMIN, IDMAX

            IF ( NUMOBS.GT.0 ) THEN
               TCF1 = OBREDF(ID,IS,1)
               TCF2 = OBREDF(ID,IS,2)
            ELSE
               TCF1 = 1.
               TCF2 = 1.
            ENDIF

            CDEN = IDX * CAX(ID,IS,1) + IDY * CAY(ID,IS,1)

            CNUM = IDX * CAX(ID,IS,2) * TCF1 * AC2(ID,IS,KCGRD(2)) +&
            &IDY * CAY(ID,IS,3) * TCF2 * AC2(ID,IS,KCGRD(3))

            IF (ACUPDA) AC2(ID,IS,KCGRD(1)) = CNUM / CDEN

         ENDDO
      ENDDO

   ELSE
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            IF ( ANYBIN(ID,IS) ) THEN

!             *** Computation of weighting coefs WEIG1 AND WEIG2 ***

               CDEN = RDX(1) * CAX(ID,IS,1) + RDY(1) * CAY(ID,IS,1)
               CNUM =  (RDX(1) + RDX(2)) * CAX(ID,IS,1)&
               &+ (RDY(1) + RDY(2)) * CAY(ID,IS,1)
               WEIG1 = CDEN/CNUM
               WEIG2 = 1. - WEIG1

               IF (NUMOBS .GT. 0) THEN
                  TCF1 = OBREDF(ID,IS,1)
                  TCF2 = OBREDF(ID,IS,2)
               ELSE
                  TCF1 = 1.
                  TCF2 = 1.
               ENDIF
               FAC_A = TCF1 * WEIG1 * AC2(ID,IS,KCGRD(2))
               FAC_B = TCF2 * WEIG2 * AC2(ID,IS,KCGRD(3))

               IF (ACUPDA)&
               &AC2(ID,IS,KCGRD(1)) = MAX ( 0. , (FAC_A + FAC_B))

            END IF
         END DO
      END DO
   END IF

   IF ( ITEST .GE. 140 .AND. TESTFL ) THEN
      WRITE(PRINTF,"(' PREDT : POINT INDX SWPDIR :',2I5)") KCGRD(1), SWPDIR
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS)-1, IDCMAX(IS)+1
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
            WRITE (PRINTF,"(' : IS ID AC2 AC2(2) AC2(3) ANYBIN :', 2I5,3(E12.4),L4)") IS, ID, AC2(ID,IS,KCGRD(1)),&
            &AC2(ID,IS,KCGRD(2)),&
            &AC2(ID,IS,KCGRD(3)),&
            &ANYBIN(ID,IS)
         END DO
      END DO
   END IF

!     End of the subroutine SPREDT
   RETURN
end subroutine SPREDT

!****************************************************************

SUBROUTINE SWAPAR ( DEP, MUDL, KWAVE, CGO, DMW, SPCSIG )
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_wave_physics, ONLY: KSCIP1, KSCIP2

!****************************************************************

   USE swan_input_grids
   USE swan_stencil
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   USE m_propcache, ONLY: prop_cache_valid, prop_kwave,&
   &prop_cgo, prop_dmw


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
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!     40.59: Erick Rogers
!
!  1. Updates
!
!     20.96, Jan. 96: Computation of CGO etc. taken out of ID loop
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     30.82, July 99: Corrected argumentlist KSCIP1
!     40.13, Oct. 01: single call to KSCIP1 instead of loop over call
!                     N and ND declared as arrays
!                     loop over IC now inside routine SWAPAR
!     40.41, Aug. 04: CG moved to DSPHER and code optimized
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.59, Aug. 07: muddy bottom included
!
!  2. Purpose
!
!     computes the wave parameters K and CGO in the nearby
!     points, depending of the sweep direction.
!     The nearby points are indicated with the index IC (see
!     FUNCTION ICODE(_,_)
!
!  3. Method
!
!     The wave number K(IS,iC) is computed with the dispersion relation:
!
!     S = GRAV K(IS,IC)tanh(K(IS,IC)DEP(IX,IY))
!
!     where S = is logarithmic distributed via LOGSIG
!
!     The group velocity CGO in the case without current is equal to
!
!                    1       K(IS,IC)DEP(IX,IY)          S
!     CGO(IS,IC) = ( - + --------------------------) -----------
!                    2   2 sinh 2K(IS,IC)DEP(IX,IY)  |k(IS,IC)|
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!        INTEGERS:
!        ---------
!
!        IX          Counter of gridpoints in x-direction
!        IY          Counter of gridpoints in y-direction
!        IS          Counter of relative frequency band
!        ID          Counter of directional distribution
!        ICMAX       Maximum array size for the points of the molecule
!        MXC         Maximum counter of gridppoints in x-direction
!        MYC         Maximum counter of gridppoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!
!        REALS:
!        ---------
!
!        GRAV        Gravitational acceleration
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        CGO       2D    Group velocity as function of X and Y and S in
!                        direction of wave propagation in absence of currents
!        DEP       2D    Depth as function of X and Y at certain time
!        KWAVE     2D    wavenumber as function of the relative frequency S
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. Common blocks used
!
!
!     8. REMARKS
!
!     9. STRUCTURE
!
!   -------------------------------------------------------------
!   If depth is negative ( D(IX,IY) <= 0), then,
!     For every point in S and D-direction do,
!       Give wave parameters default values :
!       CGO(IS,IC)  =  0.    ,  {group velocity in absence of a current}
!       K(IS,IC)    = -1.    ,                             {wave number}
!     ---------------------------------------------------------
!   Else
!         Then for every IS do
!           call KSCIP1 to compute wave number and group velocity
!           call KSCIP2 to compute muddy wave number and group velocity
!         ------------------------------------------------------
!   end if
!   ------------------------------------------------------------
!   End of SWAPAR
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************
!
!        IC          Dummy variable: ICode gridpoint:
!                      IC = 1  Top or Bottom gridpoint
!                      IC = 2  Left or Right gridpoint
!                      IC = 3  Central gridpoint
!                    Whether which value IC has, depends of the sweep
!                    If necessary IC can be enlarged by increasing
!                    the array size of ICMAX
   INTEGER      IC, IS, ID, INDX
   REAL         DEPLOC, DM
   REAL      :: SWAPAR_REFRACTIVE_INDEX(1:MSC)
   REAL      :: SWAPAR_REFRACTIVE_DERIVATIVE(1:MSC)

   REAL         DEP(MCGRD)         ,&
   &MUDL(MCGRD)        ,&
   &KWAVE(MSC,ICMAX)   ,&
   &CGO(MSC,ICMAX)     ,&
   &DMW(MSC,ICMAX)


   INTEGER, SAVE :: IENT=0
   IF (LTRACE) CALL STRACE (IENT,'SWAPAR')

   DO IC = 1, ICMAX
      INDX   = KCGRD(IC)
      DEPLOC = DEP(INDX)
      IF ( prop_cache_valid ) THEN
         KWAVE(1:MSC,IC) = prop_kwave(1:MSC,INDX)
         CGO  (1:MSC,IC) = prop_cgo  (1:MSC,INDX)
         DMW  (1:MSC,IC) = prop_dmw  (1:MSC,INDX)
      ELSE
         IF (VARMUD) THEN
            DM = MUDL(INDX)
         ELSE
            DM = PMUD(1)
         ENDIF
         IF ( DEPLOC .LE. DEPMIN) THEN
!         *** depth is negative ***
            do IS = 1, MSC
               KWAVE(IS,IC) = -1.
               CGO(IS,IC)   = 0.
               DMW(IS,IC)   = 0.
            end do
         ELSE
!       *** call KSCIP1 to compute KWAVE and CGO ***
            CALL KSCIP1 (MSC, SPCSIG, DEPLOC, KWAVE(1,IC) ,&
            &CGO(1,IC), SWAPAR_REFRACTIVE_INDEX, &
            &SWAPAR_REFRACTIVE_DERIVATIVE)
            IF (IMUD.EQ.1) THEN
!         *** call KSCIP2 to compute KWAVE, CGO and DMW ***
               CALL KSCIP2 (MSC, SPCSIG, DEPLOC, KWAVE(1,IC) ,&
               &CGO(1,IC), SWAPAR_REFRACTIVE_INDEX, &
               &SWAPAR_REFRACTIVE_DERIVATIVE, DMW(1,IC), DM)
            ELSE
               DMW(1:MSC,IC) = 0.
            ENDIF
         ENDIF
      ENDIF

      IF ( TESTFL .AND. IC .EQ. 1 .AND. ITEST.GE. 100 ) THEN
         WRITE(PRINTF,"(' SWAPAR : DEP :',E12.4, /, ' IS K CGO :')") DEP(KCGRD(IC))
         do IS = 1, MSC
            WRITE(PRINTF,"(I4, 2E12.4)") IS, KWAVE(IS,IC), CGO(IS,IC)
         end do
      END IF
   ENDDO

!     end of subroutine SWAPAR
   RETURN
end subroutine SWAPAR

!*******************************************************************

SUBROUTINE SWAPRE ( DEP, MUDL, SPCSIG )
   USE swan_service_interfaces, ONLY: MSGERR
   USE swan_wave_physics, ONLY: KSCIP1, KSCIP2

!*******************************************************************
!
!     Precompute dispersion quantities for a stationary computation.
!     The resulting module arrays are read-only inside the OpenMP sweeps

   USE swan_input_grids
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE m_propcache, ONLY: prop_cache_valid, prop_kwave,&
   &prop_cgo, prop_dmw, prop_cache_reset

   IMPLICIT NONE(TYPE, EXTERNAL)

   REAL DEP(MCGRD), MUDL(MCGRD), SPCSIG(MSC)
   INTEGER IP, ISTAT
   REAL DEPLOC, DM
   REAL SWAPRE_REFRACTIVE_INDEX(MSC)
   REAL SWAPRE_REFRACTIVE_DERIVATIVE(MSC)

   CALL prop_cache_reset()
   ISTAT = 0
   ALLOCATE(prop_kwave(MSC,MCGRD), prop_cgo(MSC,MCGRD),&
   &prop_dmw(MSC,MCGRD), STAT=ISTAT)
   IF ( ISTAT.NE.0 ) THEN
      CALL prop_cache_reset()
      CALL MSGERR (4,&
      &'Allocation problem in SWAPRE: propagation cache')
      RETURN
   ENDIF

   DO IP = 1, MCGRD
      DEPLOC = DEP(IP)
      IF ( DEPLOC.LE.DEPMIN ) THEN
         prop_kwave(:,IP) = -1.
         prop_cgo(:,IP)   = 0.
         prop_dmw(:,IP)   = 0.
      ELSE
         CALL KSCIP1 (MSC, SPCSIG, DEPLOC, prop_kwave(1,IP),&
         &prop_cgo(1,IP), SWAPRE_REFRACTIVE_INDEX, &
         &SWAPRE_REFRACTIVE_DERIVATIVE)
         IF ( IMUD.EQ.1 ) THEN
            IF ( VARMUD ) THEN
               DM = MUDL(IP)
            ELSE
               DM = PMUD(1)
            ENDIF
            CALL KSCIP2 (MSC, SPCSIG, DEPLOC, prop_kwave(1,IP),&
            &prop_cgo(1,IP), SWAPRE_REFRACTIVE_INDEX, &
            &SWAPRE_REFRACTIVE_DERIVATIVE, prop_dmw(1,IP), DM)
         ELSE
            prop_dmw(:,IP) = 0.
         ENDIF
      ENDIF
   ENDDO
   prop_cache_valid = .TRUE.

   RETURN
end subroutine SWAPRE

!*******************************************************************

SUBROUTINE ADDDIS (DISSXY     ,LEAKXY     ,&
&AC2        ,ANYBIN     ,&
&DISC0      ,DISC1      ,&
&GENC0      ,GENC1      ,&
&REDC0      ,REDC1      ,&
&TRAC0      ,TRAC1      ,&
&IMATLA     ,IMATUA     ,&
&IMAT5L     ,IMAT6U     ,&
&DSXBOT     ,&
&DSXSRF     ,&
&DSXWCP     ,&
&DSXVEG     ,DSXTUR     ,&
&DSXMUD     ,&
&DSXICE     ,&
&DSXSWL     ,&
&GSXWND     ,GENRXY     ,&
&RSXQUA     ,RSXTRI     ,&
&RSXBRA     ,RSXSQC     ,REDSXY     ,&
&TSXGEO     ,TSXSPT     ,&
&TSXSPS     ,TRANXY     ,&
&LEAKC1     ,RADSXY     ,SPCSIG     )
   USE swan_service_interfaces, ONLY: STRACE

!*******************************************************************

   USE swan_stencil
   USE swan_physics_selection
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
!     40.61: Marcel Zijlema
!     40.67: Nico Booij
!     40.85: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     20.53, Aug. 95: New subroutine
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.61, Sep. 06: introduction of all separate dissipation coefficients
!     40.67, Jun. 07: more accurate computation of dissipation terms
!     40.85, Aug. 08: add also propagation, generation and redistribution terms
!                     and radiation stress
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Adds propagation, generation, dissipation, redistribution, leak and radiation stress terms
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!     IX          Counter of gridpoints in x-direction
!     IY          Counter of gridpoints in y-direction
!     MXC         Maximum counter of gridppoints in x-direction
!     MYC         Maximum counter of gridppoints in y-direction
!     MSC         Maximum counter of relative frequency
!     MDC         Maximum counter of directional distribution
!
!     one and more dimensional arrays:
!     ---------------------------------
!     AC2       4D    Action density as function of D,S,X,Y and T
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SWOMPU
!
! 11. Remarks
!
!     DISSXY and LEAKXY are dissipation and leak integrated over the
!     spectrum for each point in the computational grid
!     The same holds for DSXBOT, DSXSRF and DSXWCP for bottom friction-,
!     surf- and whitecapping dissipation, respectively
!     DISSC0 and DISSC1 give the dissipation distributed over the
!     spectral space in one point of the computational grid
!     The same holds for DISBOT, DISSRF and DISWCP for bottom friction-,
!     surf- and whitecapping dissipation, respectively
!
!     Note on different source terms and transport terms for output purposing:
!     these terms in absolute value are integrated over the spectral space for
!     each grid point. In this way we can estimate the associated time scale.
!     Besides these terms we may also compute energy transfer between waves
!     and currents due to radiation stress.
!
!     Further details can be found in the ICCE paper of
!     Holthuijsen, L.H., Zijlema, M. and Van der Ham, P.J. (2009)
!     Wave physics in a tidal inlet, in: J.M. Smith (Ed.),
!     Proc. 31st Int. Conf. on Coast. Engng.,
!     ASCE, World Scientific Publishing, Singapore, pp. 437-448
!
! 12. Structure
!
!     -------------------------------------------------------------
!     -------------------------------------------------------------
!
! 13. Source text

   INTEGER :: II               ! counter
   REAL    :: ADISSIP(1:MDISP)
   REAL    :: AGENERT(1:MGENR)
   REAL    :: AREDIST(1:MREDS)
   REAL    :: ATRANSP(1:MTRNP)
   REAL     DISSXY(MCGRD)    ,LEAKXY(MCGRD)      ,&
   &DSXBOT(MCGRD)      ,&
   &DSXSRF(MCGRD)      ,&
   &DSXWCP(MCGRD)      ,&
   &DSXMUD(MCGRD)      ,&
   &DSXVEG(MCGRD)      ,&
   &DSXTUR(MCGRD)      ,&
   &DSXICE(MCGRD)      ,&
   &DSXSWL(MCGRD)      ,&
   &GSXWND(MCGRD)      ,&
   &RSXQUA(MCGRD)      ,&
   &RSXTRI(MCGRD)      ,&
   &RSXBRA(MCGRD)      ,&
   &RSXSQC(MCGRD)      ,&
   &TSXGEO(MCGRD)      ,&
   &TSXSPT(MCGRD)      ,&
   &TSXSPS(MCGRD)      ,&
   &GENRXY(MCGRD)      ,REDSXY(MCGRD)    ,TRANXY(MCGRD),&
   &RADSXY(MCGRD)      ,&
   &LEAKC1(MDC,MSC)  ,AC2(MDC,MSC,MCGRD)
   REAL :: DISC0(1:MDC,1:MSC,1:MDISP)       ! dissipation coeff.
   REAL :: DISC1(1:MDC,1:MSC,1:MDISP)       ! dissipation coeff.
   REAL :: GENC0(1:MDC,1:MSC,1:MGENR)       ! generation coeff.
   REAL :: GENC1(1:MDC,1:MSC,1:MGENR)       ! generation coeff.
   REAL :: REDC0(1:MDC,1:MSC,1:MREDS)       ! redistribution coeff.
   REAL :: REDC1(1:MDC,1:MSC,1:MREDS)       ! redistribution coeff.
   REAL :: TRAC0(1:MDC,1:MSC,1:MTRNP)       ! transport coeff.
   REAL :: TRAC1(1:MDC,1:MSC,1:MTRNP)       ! transport coeff.
   REAL    IMATLA(MDC,MSC)           ,&
   &IMATUA(MDC,MSC)           ,&
   &IMAT5L(MDC,MSC)           ,&
   &IMAT6U(MDC,MSC)

   REAL  ARADSTR, DSDD, SDSDD, S1, ACT1, S2
   REAL  ACT2, S3, ACT3, ACT4, ACT5, ACONTR
   INTEGER ISC, IDC, IDM, IDP

   LOGICAL  ANYBIN(MDC,MSC)
   INTEGER, SAVE :: IENT=0
   CALL STRACE (IENT, 'ADDDIS')

   ADISSIP(1:MDISP) = 0.
   AGENERT(1:MGENR) = 0.
   AREDIST(1:MREDS) = 0.
   ATRANSP(1:MTRNP) = 0.
   ARADSTR          = 0.
   do ISC = 1, MSC
      DSDD  = DDIR * FRINTF * SPCSIG(ISC)
      SDSDD = DSDD * SPCSIG(ISC)
      do IDC = 1, MDC
         IDM = MOD ( IDC - 2 + MDC , MDC ) + 1
         IDP = MOD ( IDC     + MDC , MDC ) + 1

         S1   = SPCSIG(ISC)
         ACT1 = AC2(IDC,ISC,KCGRD(1))
         IF (ISC.EQ.1) THEN
            S2   = 0.
            ACT2 = 0.
         ELSE
            S2   = SPCSIG(ISC-1)
            ACT2 = AC2(IDC,ISC-1,KCGRD(1))
         ENDIF
         IF (ISC.EQ.MSC) THEN
            S3   = 0.
            ACT3 = 0.
         ELSE
            S3   = SPCSIG(ISC+1)
            ACT3 = AC2(IDC,ISC+1,KCGRD(1))
         ENDIF
         IF (.NOT.FULCIR .AND. IDC.EQ.1) THEN
            ACT4 = 0.
         ELSE
            ACT4 = AC2(IDM,ISC,KCGRD(1))
         ENDIF
         IF (.NOT.FULCIR .AND. IDC.EQ.MDC) THEN
            ACT5 = 0.
         ELSE
            ACT5 = AC2(IDP,ISC,KCGRD(1))
         ENDIF

         IF (ANYBIN(IDC,ISC)) THEN
            LEAKXY(KCGRD(1)) = LEAKXY(KCGRD(1)) + SDSDD*&
            &LEAKC1(IDC,ISC) * AC2(IDC,ISC,KCGRD(1))

!           --- compute for each dissipation term

            DO II = 1, MDISP
               ACONTR= SDSDD*(DISC0(IDC,ISC,II) + DISC1(IDC,ISC,II)*ACT1)
               ADISSIP(II) = ADISSIP(II) + ACONTR
               ARADSTR     = ARADSTR     - ACONTR
            ENDDO

!           --- compute for each generation term

            DO II = 1, MGENR
               ACONTR= SDSDD*(GENC0(IDC,ISC,II) + GENC1(IDC,ISC,II)*ACT1)
               AGENERT(II) = AGENERT(II) + ACONTR
               ARADSTR     = ARADSTR     + ACONTR
            ENDDO

!           --- compute for each redistribution term

            DO II = 1, MREDS
               ACONTR= SDSDD*(REDC0(IDC,ISC,II) + REDC1(IDC,ISC,II)*ACT1)
               AREDIST(II) = AREDIST(II) + ABS(ACONTR)
               ARADSTR     = ARADSTR     + ACONTR
            ENDDO

!           --- compute for each propagation term

            ACONTR = SDSDD* (TRAC0(IDC,ISC,1) + TRAC1(IDC,ISC,1)*ACT1)
            ATRANSP(1) = ATRANSP(1) + ABS(ACONTR)
            ARADSTR    = ARADSTR    - ACONTR

            ACONTR = SDSDD* (TRAC0(IDC,ISC,2)      +&
            &TRAC1(IDC,ISC,2)*ACT1 +&
            &IMATLA(IDC,ISC) *ACT4 +&
            &IMATUA(IDC,ISC) *ACT5 )
            ATRANSP(2) = ATRANSP(2) + ABS(ACONTR)
            ARADSTR    = ARADSTR    - ACONTR

            ACONTR = DSDD * (TRAC0(IDC,ISC,3)           +&
            &TRAC1(IDC,ISC,3)* S1 *ACT1 +&
            &IMAT5L(IDC,ISC) * S2 *ACT2 +&
            &IMAT6U(IDC,ISC) * S3 *ACT3 )
            ATRANSP(3) = ATRANSP(3) + ABS(ACONTR)
            ARADSTR    = ARADSTR    - ACONTR

            ARADSTR    = ABS(ARADSTR)
         ENDIF
      end do
   end do

   DSXWCP(KCGRD(1)) = DSXWCP(KCGRD(1)) + ADISSIP(1)     ! whitecappin
   DSXSRF(KCGRD(1)) = DSXSRF(KCGRD(1)) + ADISSIP(2)     ! surf break
   DSXBOT(KCGRD(1)) = DSXBOT(KCGRD(1)) + ADISSIP(3)     ! bottom fric
   DSXSWL(KCGRD(1)) = DSXSWL(KCGRD(1)) + ADISSIP(4)     ! swell dissi
   DSXVEG(KCGRD(1)) = DSXVEG(KCGRD(1)) + ADISSIP(5)     ! vegetation
   DSXTUR(KCGRD(1)) = DSXTUR(KCGRD(1)) + ADISSIP(6)     ! turbulence
   DSXMUD(KCGRD(1)) = DSXMUD(KCGRD(1)) + ADISSIP(7)     ! mud dissip
   DSXICE(KCGRD(1)) = DSXICE(KCGRD(1)) + ADISSIP(8)     ! ice dissip

   DISSXY(KCGRD(1)) = DISSXY(KCGRD(1)) + SUM(ADISSIP)   ! total dissi

   GSXWND(KCGRD(1)) = GSXWND(KCGRD(1)) + AGENERT(1)     ! wind input
   GENRXY(KCGRD(1)) = GENRXY(KCGRD(1)) + SUM(AGENERT)   ! total gener

   RSXQUA(KCGRD(1)) = RSXQUA(KCGRD(1)) + AREDIST(1)     ! quadruplets
   RSXTRI(KCGRD(1)) = RSXTRI(KCGRD(1)) + AREDIST(2)     ! triads
   RSXBRA(KCGRD(1)) = RSXBRA(KCGRD(1)) + AREDIST(3)     ! Bragg scatt
   RSXSQC(KCGRD(1)) = RSXSQC(KCGRD(1)) + AREDIST(4)     ! QC scatteri
   REDSXY(KCGRD(1)) = REDSXY(KCGRD(1)) + SUM(AREDIST)   ! total redis

   TSXGEO(KCGRD(1)) = TSXGEO(KCGRD(1)) + ATRANSP(1)     ! xy-propagat
   TSXSPT(KCGRD(1)) = TSXSPT(KCGRD(1)) + ATRANSP(2)     ! theta-propa
   TSXSPS(KCGRD(1)) = TSXSPS(KCGRD(1)) + ATRANSP(3)     ! sigma-propa
   TRANXY(KCGRD(1)) = TRANXY(KCGRD(1)) + SUM(ATRANSP)   ! total propa

!       energy transfer between waves and currents due to radiation stress, see page 439 of
!       the ICCE paper of Holthuijsen, L.H., Zijlema, M. and Van der Ham, P.J. (2009)
!       Wave physics in a tidal inlet, in: J.M. Smith (Ed.), Proc. 31st
!       ASCE, World Scientific Publishing, Singapore, pp. 437-448

   RADSXY(KCGRD(1)) = RADSXY(KCGRD(1)) + ARADSTR

   IMATLA = 0.
   IMATUA = 0.
   IMAT5L = 0.
   IMAT6U = 0.

   RETURN
end subroutine ADDDIS

!****************************************************************

SUBROUTINE SWFLXD (CAD   , IMATLA, IMATDA, IMATUA, IMATRA,&
&AC2   , DD    , ANYBIN, LEAKC1, IDCMIN,&
&IDCMAX, ISSTOP)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_stencil
   USE swan_numerics
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units

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
!     40.23, Nov. 02: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     computation of @[CAD AC2]/@D by means of flux-limiting
!
!  3. Method
!
!     Discretization is based on flux-limiting and is regarded
!     as the sum of first order upwind scheme and anti-diffusive
!     parts containing the PL-kappa slope limiter
!
!  4. Argument variables
!
!     AC2         action density
!     ANYBIN      logical: indicate whether current bin is selected
!     CAD         wave transport velocity in D-direction
!     DD          width of spectral direction band
!     IDCMAX      frequency dependent counter
!     IDCMIN      frequency dependent counter
!     IMATDA      coefficients of diagonal of matrix
!     IMATLA      coefficients of lower diagonal of matrix
!     IMATRA      coefficients of right hand side
!     IMATUA      coefficients of upper diagonal of matrix
!     ISSTOP      maximum frequency counter in a sweep
!     LEAKC1      leak coefficient

   INTEGER ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)
   REAL    DD
   REAL    AC2(MDC,MSC,MCGRD),&
   &CAD(MDC,MSC,ICMAX),&
   &IMATLA(MDC,MSC)   ,&
   &IMATDA(MDC,MSC)   ,&
   &IMATUA(MDC,MSC)   ,&
   &IMATRA(MDC,MSC)   ,&
   &LEAKC1(MDC,MSC)
   LOGICAL ANYBIN(MDC,MSC)

!  6. Local variables
!
!     ACM   :     lower action
!     ACP   :     upper action
!     ACT0  :     action in centroid (=ID)
!     ACT1  :     action in lower node (=ID-1)
!     ACT2  :     action in upper node (=ID+1)
!     ACT3  :     action in 2 points away from centre (=ID+2)
!     BINM  :     indicate whether lower bin is selected
!     BINP  :     indicate whether upper bin is selected
!     CADM  :     lower wave transport velocity
!     CADP  :     upper wave transport velocity
!     CAN   :     negative wave transport velocity
!     CAP   :     positive wave transport velocity
!     CAV   :     averaged wave transport velocity; may be
!                 regarded as flux velocity
!     DDI   :     inverse of width of spectral direction band
!     DFCOR :     auxiliary real containing anti-diffusive parts
!                 to be regarded as defect correction
!     FACT  :     auxiliary factor
!     ID    :     counter
!     IDDUM :     loop counter
!     IDM   :     index of point ID-1
!     IDP   :     index of point ID+1
!     IENT  :     number of entries
!     IS    :     loop counter
!     RAN   :     ratio of consecutive gradients i.c. negative velocity
!     RAP   :     ratio of consecutive gradients i.c. positive velocity
!     XKAP  :     control parameter meant for the kappa-scheme
!     XLIMN :     flux-limiter i.c. negative velocity
!     XLIMP :     flux-limiter i.c. positive velocity

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IDM, IDP, IDDUM, IS
   REAL    ACM, ACP, ACT0, ACT1, ACT2, ACT3, CADM, CADP,&
   &CAN, CAP, CAV, DDI, DFCOR, FACT, RAN, RAP, XKAP,&
   &XLIMN, XLIMP
   LOGICAL BINM, BINP

!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     ACTION (in SWANCOM1)
!
! 12. Structure
!
!     For every S and D-direction in direction of sweep do
!
!        Compute the derivative in D-direction and
!        store the results of this calculation in the
!        arrays IMATDA, IMATLA, IMATUA and IMATRA
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWFLXD')

   DDI  = 1./DD
   XKAP = PNUMS(6)

   do IS = 1, ISSTOP

      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD (IDDUM-1+MDC, MDC) + 1

         IF ( ANYBIN(ID,IS) ) THEN

            IF ( IDDUM.EQ.IDCMIN(IS) ) THEN

               IF ( FULCIR .OR. ID.GT.1 ) THEN
                  IDM  = MOD (IDDUM-2+MDC, MDC) + 1
                  CADM = CAD(IDM,IS,1)
                  BINM = ANYBIN(IDM,IS)
                  IF (.NOT.BINM) ACM = AC2(IDM,IS,KCGRD(1))
               ELSE
                  IDM  = 0
                  CADM = CAD(ID,IS,1)
                  BINM = .FALSE.
                  ACM  = 0.
               END IF

               CAV = 0.5*(CAD(ID,IS,1) + CADM)
               CAP = 0.5*(CAV + ABS(CAV))
               CAN = 0.5*(CAV - ABS(CAV))

               IF ( FULCIR .OR. ID.GT.1 ) THEN
                  ACT0 = AC2(MOD(IDDUM-2+MDC,MDC)+1,IS,KCGRD(1))
               ELSE
                  ACT0 = 0.
               END IF
               IF ( FULCIR .OR. ID.GT.2 ) THEN
                  ACT1 = AC2(MOD(IDDUM-3+MDC,MDC)+1,IS,KCGRD(1))
               ELSE
                  ACT1 = 0.
               END IF
               ACT2 = AC2(ID,IS,KCGRD(1))
               IF ( FULCIR .OR. ID.LT.MDC ) THEN
                  ACT3 = AC2(MOD(IDDUM+MDC,MDC)+1,IS,KCGRD(1))
               ELSE
                  ACT3 = 0.
               END IF

               RAP = (ACT2-ACT0+1.E-12)/(ACT0-ACT1+1.E-12)
               RAN = (ACT2-ACT0+1.E-12)/(ACT3-ACT2+1.E-12)

               FACT  = MIN( 2., 0.5*(1.+XKAP)*RAP + 0.5*(1.-XKAP) )
               XLIMP = MAX( 0., MIN(2.*RAP,FACT) )

               FACT  = MIN( 2., 0.5*(1.+XKAP)*RAN + 0.5*(1.-XKAP) )
               XLIMN = MAX( 0., MIN(2.*RAN,FACT) )

               DFCOR = 0.5*(CAP*XLIMP*(ACT0 - ACT1) -&
               &CAN*XLIMN*(ACT3 - ACT2))

               IF ( .NOT.BINM ) THEN
                  IMATDA(ID,IS) = IMATDA(ID,IS) - DDI * CAN
                  IMATRA(ID,IS) = IMATRA(ID,IS) + DDI * CAP * ACM
                  IF (IDM.NE.0)&
                  &IMATRA(ID,IS) = IMATRA(ID,IS) + DDI * DFCOR
               END IF

               IF ( IDM.EQ.0 ) LEAKC1(ID,IS) = -CAN

            END IF

            IF ( FULCIR .OR. ID.LT.MDC ) THEN
               IDP  = MOD (IDDUM+MDC, MDC) + 1
               CADP = CAD(IDP,IS,1)
               BINP = ANYBIN(IDP,IS)
               IF (.NOT.BINP) ACP = AC2(IDP,IS,KCGRD(1))
            ELSE
               IDP  = 0
               CADP = CAD(ID,IS,1)
               BINP = .FALSE.
               ACP  = 0.
            END IF

            CAV = 0.5*(CAD(ID,IS,1) + CADP)
            CAP = 0.5*(CAV + ABS(CAV))
            CAN = 0.5*(CAV - ABS(CAV))

            ACT0 = AC2(ID,IS,KCGRD(1))
            IF ( FULCIR .OR. ID.GT.1 ) THEN
               ACT1 = AC2(MOD(IDDUM-2+MDC,MDC)+1,IS,KCGRD(1))
            ELSE
               ACT1 = 0.
            END IF
            IF ( FULCIR .OR. ID.LT.MDC ) THEN
               ACT2 = AC2(MOD(IDDUM+MDC,MDC)+1,IS,KCGRD(1))
            ELSE
               ACT2 = 0.
            END IF
            IF ( FULCIR .OR. ID.LT.MDC-1 ) THEN
               ACT3 = AC2(MOD(IDDUM+1+MDC,MDC)+1,IS,KCGRD(1))
            ELSE
               ACT3 = 0.
            END IF

            RAP = (ACT2-ACT0+1.E-12)/(ACT0-ACT1+1.E-12)
            RAN = (ACT2-ACT0+1.E-12)/(ACT3-ACT2+1.E-12)

            FACT  = MIN( 2., 0.5*(1.+XKAP)*RAP + 0.5*(1.-XKAP) )
            XLIMP = MAX( 0., MIN(2.*RAP,FACT) )

            FACT  = MIN( 2., 0.5*(1.+XKAP)*RAN + 0.5*(1.-XKAP) )
            XLIMN = MAX( 0., MIN(2.*RAN,FACT) )

            DFCOR = 0.5*(CAP*XLIMP*(ACT0-ACT1)-CAN*XLIMN*(ACT3-ACT2))

            IF ( BINP ) THEN
               IMATDA(ID ,IS) = IMATDA(ID ,IS) + DDI * CAP
               IMATUA(ID ,IS) = IMATUA(ID ,IS) + DDI * CAN
               IMATDA(IDP,IS) = IMATDA(IDP,IS) - DDI * CAN
               IMATLA(IDP,IS) = IMATLA(IDP,IS) - DDI * CAP
               IMATRA(ID ,IS) = IMATRA(ID ,IS) - DDI * DFCOR
               IMATRA(IDP,IS) = IMATRA(IDP,IS) + DDI * DFCOR
            ELSE
               IMATDA(ID,IS) = IMATDA(ID,IS) + DDI * CAP
               IMATRA(ID,IS) = IMATRA(ID,IS) - DDI * CAN * ACP
               IF (IDP.NE.0)&
               &IMATRA(ID,IS) = IMATRA(ID,IS) - DDI * DFCOR
            END IF

            IF ( IDP.EQ.0 ) LEAKC1(ID,IS) = CAP

         END IF

      end do
   end do

!     --- test output

   IF ( TESTFL .AND. ITEST.GE.80 ) THEN
      WRITE(PRINTF,"(' SWFLXD: POINT ISSTOP :',2I5)") KCGRD(1), ISSTOP
      WRITE(PRINTF,"(' SWFLXD: CDD :',E12.4)") PNUMS(6)
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' matrix coefficients in SWFLXD'
      WRITE(PRINTF,*)
      WRITE(PRINTF,*)&
      &'   IS ID      IMATLA      IMATDA      IMATUA      IMATRA     CAD'
      DO IS = 1, ISSTOP
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD (IDDUM-1+MDC, MDC) + 1
            WRITE(PRINTF,"(1X,2I4,4X,4E12.4,E10.2)") IS, ID, IMATLA(ID,IS),IMATDA(ID,IS),&
            &IMATUA(ID,IS),IMATRA(ID,IS),CAD(ID,IS,1)
         END DO
      END DO
   END IF

   RETURN
end subroutine SWFLXD
!****************************************************************

SUBROUTINE DIFPAR( AC2   , SPCSIG, KGRPNT, DEP2  , DIFFR ,&
&CROSS , XCGRID, YCGRID, XYTST )
   USE swan_service_interfaces, ONLY: STRACE, EQREAL, STPNOW
   USE swan_parallel, ONLY: SWEXCHG
   USE swan_wave_physics, ONLY: KSCIP1

!****************************************************************

   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_physics_selection
   USE swan_numerics
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_spherical_geometry
   USE swan_diagnostics_level
   USE swan_io_units
   USE M_PARALL

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(diffraction_state_t), INTENT(INOUT) :: DIFFR


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
!     40.21: Agnieszka Herman, Nico Booij
!     40.41: Marcel Zijlema
!     40.68: Marcel Zijlema
!
!  1. Updates
!
!     40.21, Aug. 01: New subroutine
!     40.41, Mar. 04: parallelization of diffraction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.68, Aug. 07: extension to spherical coordinates
!
!  2. Purpose
!
!     Computes diffraction parameter and its derivatives
!
!  3. Method
!
!     Parameters governing smoothing of the energy field.
!
!     Effectively, E(i,j) = (1-4*alpha) * E(i,j) +
!                           alpha * (E(i-1,j)+E(i+1,j)+E(i,j-1)+E(i,j+1))
!
!     Parameters governing numerical computation of diffraction
!     coefficient and its spatial derivatives:
!
!     diffr%param=SQRT(1+delta)
!
!     Near land and obstacles derivatives are assumed to be zero
!
!  4. Argument variables
!
!     AC2         action density
!     CROSS       integer array indicating obstacle crossing
!                 (0=no, 1=yes)
!     DEP2        current total depth
!     KGRPNT      indirect address for grid points
!     SPCSIG      relative frequencies in sigma-space
!     XCGRID      x-coordinate of computational grid
!     YCGRID      y-coordinate of computational grid
!     XYTST       grid indices of test points

   INTEGER, INTENT(IN)    :: KGRPNT(MXC,MYC)
   REAL   , INTENT(IN)    :: SPCSIG(MSC)
   REAL   , INTENT(INOUT) :: AC2(MDC,MSC,MCGRD)
   REAL   , INTENT(IN)    :: DEP2(MCGRD)
   INTEGER, INTENT(IN)    :: CROSS(2,MCGRD)
   REAL   , INTENT(IN)    :: XCGRID(MXC,MYC)
   REAL   , INTENT(IN)    :: YCGRID(MXC,MYC)
   INTEGER, INTENT(IN)    :: XYTST(*)

!  6. Local variables
!
!     IENT  :     number of entries
!     IX1   :     lower index in x-direction
!     IX2   :     upper index in x-direction
!     IY1   :     lower index in y-direction
!     IY2   :     upper index in y-direction
!     NOOBST:     indicates obstacles in the model (FALSE) or not (TRUE)

   INTEGER, SAVE :: IENT = 0
   INTEGER           :: IS, ISM
   INTEGER           :: IX, IY, IND, INDL, INDR, INDB, INDT
   INTEGER           :: IX1, IX2, IY1, IY2, IXB, IXE, IYB, IYE
   INTEGER           :: MXCL, MYCL, IXX, IYY, IXXL, IXXR, IYYB, IYYT
   REAL              :: CETAIL, PPTAIL, CKTAIL
   REAL              :: ETOT, EKTOTL, ECGTOT, TMP
   REAL              :: EAD, TMP_X, TMP_Y, CSLAT
   REAL              :: DXLOC, DYLOC
   REAL, ALLOCATABLE :: KLOC(:), CGLOC(:)
   REAL, ALLOCATABLE :: REFRACTIVE_INDEX(:)
   REAL, ALLOCATABLE :: REFRACTIVE_DERIVATIVE(:)
   REAL, ALLOCATABLE :: EN(:), LAPE(:), DENOM(:), K(:), CG(:)
   REAL, ALLOCATABLE :: ENTMP(:,:)
   LOGICAL           :: NOOBST

!  8. Subroutines used
!
!     EQREAL           logical function, true if arguments are equal
!     KSCIP1           calculates wave number and group velocity
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWEXCHG          exchanges some data at subdomain boundaries


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
!     ---
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'DIFPAR')

!     --- allocate arrays
   ALLOCATE (KLOC(1:MSC), CGLOC(1:MSC))
   ALLOCATE (REFRACTIVE_INDEX(1:MSC), REFRACTIVE_DERIVATIVE(1:MSC))
   ALLOCATE (EN(1:MCGRD), LAPE(1:MCGRD), DENOM(1:MCGRD))
   ALLOCATE (K(1:MCGRD), CG(1:MCGRD))

!     --- determine bounds of own subdomain

   IX1 = 1
   IF (.NOT.LMXF) IX1 = 1+IHALOX
   IX2 = MXC
   IF (.NOT.LMXL) IX2 = MXC-IHALOX
   IY1 = 1
   IF (.NOT.LMYF) IY1 = 1+IHALOY
   IY2 = MYC
   IF (.NOT.LMYL) IY2 = MYC-IHALOY
   MXCL = IX2 - IX1 + 1
   MYCL = IY2 - IY1 + 1
   ALLOCATE (ENTMP(0:MXCL+1,0:MYCL+1))

   K (1) = 10.
   CG(1) = 0.
   EN(1) = 0.

!     --- compute total energy and mean wave number
!         in each computational grid point

   DO IND = 2, MCGRD
      IF (DEP2(IND).GE.DEPMIN) THEN
!           --- compute Cg and wave number for all spectral frequencies
         CALL KSCIP1( MSC, SPCSIG, DEP2(IND), KLOC, CGLOC, &
         &REFRACTIVE_INDEX, REFRACTIVE_DERIVATIVE )
         ETOT   = 0.
         EKTOTL = 0.
         ECGTOT = 0.
         DO IS = 1, MSC
!              --- integrate energy density over directions
            EAD = SUM(AC2(:,IS,IND)) * DDIR * SPCSIG(IS)**2
            ETOT   = ETOT   + EAD
            EKTOTL = EKTOTL + KLOC (IS) * EAD
            ECGTOT = ECGTOT + CGLOC(IS) * EAD
         END DO
         ETOT   = FRINTF * ETOT
         EKTOTL = FRINTF * EKTOTL
         ECGTOT = FRINTF * ECGTOT
         IF (MSC .GT. 3) THEN
            PPTAIL = PWTAIL(1) - 1.
            CETAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            PPTAIL = PWTAIL(1) - 1. - 2.
            CKTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            ETOT   = ETOT   + CETAIL * EAD
            EKTOTL = EKTOTL + CKTAIL * EAD
            ECGTOT = ECGTOT + CKTAIL * EAD
         END IF
         IF (ETOT.LE.0.) THEN
            K (IND) = 10.
            CG(IND) = 0.
            EN(IND) = 0.
         ELSE
            K (IND) = EKTOTL / ETOT
            CG(IND) = ECGTOT / ETOT
            EN(IND) = MAX(ETOT,1.E-8)
         END IF
      ELSE
         K (IND) = 10.
         CG(IND) = 0.
         EN(IND) = 0.
      END IF
   END DO

!     --- apply a smoothing filter to the energy field

   DO ISM = 1, INT(PDIFFR(2))
      IXB = MAX(IX1-1,1  )
      IXE = MIN(IX2+1,MXC)
      IYB = MAX(IY1-1,1  )
      IYE = MIN(IY2+1,MYC)
      ENTMP = 0.
      DO IX= IXB, IXE
         DO IY = IYB, IYE
            ENTMP(IX-IX1+1,IY-IY1+1) = EN(KGRPNT(IX,IY))
         END DO
      END DO
      DO IX= 1, MXCL
         DO IY = 1, MYCL
            IXX  = IX + IX1 - 1
            IYY  = IY + IY1 - 1
            IXXL = MAX(IXX-1,1  )
            IXXR = MIN(IXX+1,MXC)
            IYYB = MAX(IYY-1,1  )
            IYYT = MIN(IYY+1,MYC)
            IND  = KGRPNT(IXX ,IYY )
            INDL = KGRPNT(IXXL,IYY )
            INDR = KGRPNT(IXXR,IYY )
            INDB = KGRPNT(IXX ,IYYB)
            INDT = KGRPNT(IXX ,IYYT)
            TMP = PDIFFR(1)
            IF (NUMOBS.EQ.0) THEN
               NOOBST=.TRUE.
            ELSE IF (CROSS(1,IND).EQ.0) THEN
               NOOBST=.TRUE.
            ELSE
               NOOBST=.FALSE.
            END IF
            IF (NOOBST&
            &.AND. DEP2(IND ) .GT. DEPMIN&
            &.AND. DEP2(INDL) .GT. DEPMIN&
            &.AND. (.NOT.LMXF.OR.IX.NE.1)&
            &.AND. (.NOT.LMYF.OR.IY.NE.1) ) THEN
               EN(IND) = ENTMP(IX,IY) -&
               &TMP*(ENTMP(IX,IY)-ENTMP(IX-1,IY))
            END IF
            IF (NUMOBS.EQ.0) THEN
               NOOBST=.TRUE.
            ELSE IF (CROSS(1,INDR).EQ.0) THEN
               NOOBST=.TRUE.
            ELSE
               NOOBST=.FALSE.
            END IF
            IF (NOOBST&
            &.AND. DEP2(IND ) .GT. DEPMIN&
            &.AND. DEP2(INDR) .GT. DEPMIN&
            &.AND. (.NOT.LMXL.OR.IX.NE.MXCL)&
            &.AND. (.NOT.LMYF.OR.IY.NE.1   ) ) THEN
               EN(IND) = EN(IND) - TMP*(ENTMP(IX,IY)-ENTMP(IX+1,IY))
            END IF
            IF (NUMOBS.EQ.0) THEN
               NOOBST=.TRUE.
            ELSE IF (CROSS(2,IND).EQ.0) THEN
               NOOBST=.TRUE.
            ELSE
               NOOBST=.FALSE.
            END IF
            IF (NOOBST&
            &.AND. DEP2(IND ) .GT. DEPMIN&
            &.AND. DEP2(INDB) .GT. DEPMIN&
            &.AND. (.NOT.LMYF.OR.IY.NE.1)&
            &.AND. (.NOT.LMXF.OR.IX.NE.1) ) THEN
               EN(IND) = EN(IND) - TMP*(ENTMP(IX,IY)-ENTMP(IX,IY-1))
            END IF
            IF (NUMOBS.EQ.0) THEN
               NOOBST=.TRUE.
            ELSE IF (CROSS(2,INDT).EQ.0) THEN
               NOOBST=.TRUE.
            ELSE
               NOOBST=.FALSE.
            END IF
            IF (NOOBST&
            &.AND. DEP2(IND ) .GT. DEPMIN&
            &.AND. DEP2(INDT) .GT. DEPMIN&
            &.AND. (.NOT.LMYL.OR.IY.NE.MYCL)&
            &.AND. (.NOT.LMXF.OR.IX.NE.1   ) ) THEN
               EN(IND) = EN(IND) - TMP*(ENTMP(IX,IY)-ENTMP(IX,IY+1))
            END IF
         END DO
      END DO
!WFR      CALL SWEXCHG(EN,KGRPNT)
!JAC      CALL SWEXCHG(EN,0,KGRPNT)
      IF (STPNOW()) RETURN
   END DO

!     --- transform energy density into wave amplitude
   EN(1:MCGRD) = SQRT(MAX(EN(1:MCGRD),0.))

!     --- compute Laplacian of SQRT(energy) in each computational grid point
!
!     --- initially, set all values to zero
   DENOM(1:MCGRD) = 0.
   LAPE (1:MCGRD) = 0.
   diffr%dpardx(1:MCGRD) = 0.
   diffr%dpardy(1:MCGRD) = 0.

!     --- loop over all X-connections
   DO IX = MAX(IX1,2), MIN(IX2+1,MXC)
      DO IY = IY1, IY2
         IND  = KGRPNT(IX  ,IY)
         INDL = KGRPNT(IX-1,IY)
         IF (NUMOBS.EQ.0) THEN
            NOOBST=.TRUE.
         ELSE IF (CROSS(1,IND).EQ.0) THEN
            NOOBST=.TRUE.
         ELSE
            NOOBST=.FALSE.
         END IF
         IF (NOOBST&
         &.AND.DEP2(IND ).GT.DEPMIN&
         &.AND.DEP2(INDL).GT.DEPMIN) THEN
            IF ( OPTG.EQ.1 ) THEN
               DXLOC = DX
               DYLOC = DY
            ELSE IF ( OPTG.EQ.3 ) THEN
               DXLOC = XCGRID(IX,IY) - XCGRID(IX-1,IY)
               IF (LMYF .AND. IY.EQ.1) THEN
                  DYLOC = YCGRID(IX,IY+1) - YCGRID(IX,IY)
               ELSE IF (LMYL .AND. IY.EQ.MYC) THEN
                  DYLOC = YCGRID(IX,IY) - YCGRID(IX,IY-1)
               ELSE
                  DYLOC = 0.5*( YCGRID(IX,IY+1) -&
                  &YCGRID(IX,IY-1) )
               END IF
            END IF
            IF ( KSPHER.GT.0 ) THEN
               CSLAT = COS(DEGRAD*(YOFFS+YCGRID(IX,IY)))
               DXLOC = DXLOC * LENDEG * CSLAT
               DYLOC = DYLOC * LENDEG
            ENDIF
            DXLOC = ABS(DXLOC)
            DYLOC = ABS(DYLOC)
            IF (EQREAL(DXLOC,0.)) DXLOC = 0.01
            IF (EQREAL(DYLOC,0.)) DYLOC = 0.01
            TMP = 0.5*(CG(INDL)/K(INDL)+CG(IND)/K(IND)) *&
            &DYLOC * (EN(IND)-EN(INDL)) / DXLOC
            EAD = DXLOC * DYLOC * EN(IND)*CG(IND)*K(IND)
            LAPE (INDL) = LAPE (INDL) + TMP
            LAPE (IND ) = LAPE (IND ) - TMP
            DENOM(IND ) = DENOM(IND ) + EAD
         END IF
      END DO
   END DO

!     --- loop over all Y-connections
   DO IX = IX1, IX2
      DO IY = MAX(IY1,2), MIN(IY2+1,MYC)
         IND  = KGRPNT(IX,IY  )
         INDB = KGRPNT(IX,IY-1)
         IF (NUMOBS.EQ.0) THEN
            NOOBST=.TRUE.
         ELSE IF (CROSS(2,IND).EQ.0) THEN
            NOOBST=.TRUE.
         ELSE
            NOOBST=.FALSE.
         END IF
         IF (NOOBST&
         &.AND.DEP2(IND ).GT.DEPMIN&
         &.AND.DEP2(INDB).GT.DEPMIN) THEN
            IF ( OPTG.EQ.1 ) THEN
               DXLOC = DX
               DYLOC = DY
            ELSE IF ( OPTG.EQ.3 ) THEN
               DYLOC = YCGRID(IX,IY) - YCGRID(IX,IY-1)
               IF (LMXF .AND. IX.EQ.1) THEN
                  DXLOC = XCGRID(IX+1,IY) - XCGRID(IX,IY)
               ELSE IF (LMXL .AND. IX.EQ.MXC) THEN
                  DXLOC = XCGRID(IX,IY) - XCGRID(IX-1,IY)
               ELSE
                  DXLOC = 0.5*( XCGRID(IX+1,IY) -&
                  &XCGRID(IX-1,IY) )
               END IF
            END IF
            IF ( KSPHER.GT.0 ) THEN
               CSLAT = COS(DEGRAD*(YOFFS+YCGRID(IX,IY)))
               DXLOC = DXLOC * LENDEG * CSLAT
               DYLOC = DYLOC * LENDEG
            ENDIF
            DXLOC = ABS(DXLOC)
            DYLOC = ABS(DYLOC)
            IF (EQREAL(DXLOC,0.)) DXLOC = 0.01
            IF (EQREAL(DYLOC,0.)) DYLOC = 0.01
            TMP = 0.5*(CG(INDB)/K(INDB)+CG(IND)/K(IND)) *&
            &DXLOC * (EN(IND)-EN(INDB)) / DYLOC
            EAD = DXLOC * DYLOC * EN(IND)*CG(IND)*K(IND)
            LAPE (INDB) = LAPE (INDB) + TMP
            LAPE (IND ) = LAPE (IND ) - TMP
            DENOM(IND ) = DENOM(IND ) + EAD
         END IF
      END DO
   END DO

!     --- calculate the diffraction coefficient

   DO IND = 1, MCGRD
      IF ( DENOM(IND).GT.0.0 ) THEN
         TMP = 2.*LAPE(IND)/DENOM(IND)
      ELSE
         TMP = 0.
      END IF
      IF (TMP.LT.-1.) THEN
         diffr%param(IND) = 0.
      ELSE
         diffr%param(IND) = SQRT(1.+TMP)
      END IF
   END DO
!WFR   CALL SWEXCHG(diffr%param(:),KGRPNT)
!JAC   CALL SWEXCHG(diffr%param(:),0,KGRPNT)
   IF (STPNOW()) RETURN

!     --- calculate spatial derivatives of diffr%param
!
!     --- loop over all X-connections
   DO IX = MAX(IX1,2), MIN(IX2,MXC-1)
      DO IY = IY1, IY2
         IND  = KGRPNT(IX  ,IY)
         INDL = KGRPNT(IX-1,IY)
         INDR = KGRPNT(IX+1,IY)
         IF (NUMOBS.EQ.0) THEN
            NOOBST=.TRUE.
         ELSE IF (CROSS(1,IND).EQ.0) THEN
            NOOBST=.TRUE.
         ELSE
            NOOBST=.FALSE.
         END IF
         IF (NOOBST&
         &.AND. DEP2(IND ).GT.DEPMIN&
         &.AND. DEP2(INDL).GT.DEPMIN&
         &.AND. DEP2(INDR).GT.DEPMIN) THEN
            IF ( OPTG.EQ.1 ) THEN
               DXLOC = DX
            ELSE IF ( OPTG.EQ.3 ) THEN
               DXLOC = 0.5*(XCGRID(IX+1,IY)-XCGRID(IX-1,IY))
            END IF
            IF ( KSPHER.GT.0 ) THEN
               CSLAT = COS(DEGRAD*(YOFFS+YCGRID(IX,IY)))
               DXLOC = DXLOC * LENDEG * CSLAT
            ENDIF
            DXLOC = ABS(DXLOC)
            IF (EQREAL(DXLOC,0.)) DXLOC=0.01
            TMP = (diffr%param(INDR) - diffr%param(INDL))/(2.*DXLOC)
            diffr%dpardx(IND) = diffr%dpardx(IND) + TMP
         END IF
      END DO
   END DO

!     --- loop over all Y-connections
   DO IX = IX1, IX2
      DO IY = MAX(IY1,2), MIN(IY2,MYC-1)
         IND  = KGRPNT(IX,IY  )
         INDB = KGRPNT(IX,IY-1)
         INDT = KGRPNT(IX,IY+1)
         IF (NUMOBS.EQ.0) THEN
            NOOBST=.TRUE.
         ELSE IF (CROSS(2,IND).EQ.0) THEN
            NOOBST=.TRUE.
         ELSE
            NOOBST=.FALSE.
         END IF
         IF (NOOBST&
         &.AND. DEP2(IND ).GT.DEPMIN&
         &.AND. DEP2(INDB).GT.DEPMIN&
         &.AND. DEP2(INDT).GT.DEPMIN) THEN
            IF ( OPTG.EQ.1 ) THEN
               DYLOC = DY
            ELSE IF ( OPTG.EQ.3 ) THEN
               DYLOC = 0.5*(YCGRID(IX,IY+1)-YCGRID(IX,IY-1))
            END IF
            IF ( KSPHER.GT.0 ) DYLOC = DYLOC * LENDEG
            DYLOC = ABS(DYLOC)
            IF (EQREAL(DYLOC,0.)) DYLOC=0.01
            TMP = (diffr%param(INDT) - diffr%param(INDB))/(2.*DYLOC)
            diffr%dpardy(IND) = diffr%dpardy(IND) + TMP
         END IF
      END DO
   END DO

!     --- rotation over ALPC needed for non-standard orientation
!         of the computational grid

   IF ( OPTG.EQ.1 ) THEN
      DO IND = 1, MCGRD
         TMP_X = diffr%dpardx(IND)
         TMP_Y = diffr%dpardy(IND)
         diffr%dpardx(IND) = COSPC * TMP_X - SINPC * TMP_Y
         diffr%dpardy(IND) = SINPC * TMP_X + COSPC * TMP_Y
      END DO
   END IF

!     --- test output
   IF (NPTST .GT. 0) THEN
      WRITE (PRTEST, "(' test DIFPAR, IDIFFR=',I1, /, ' ampl laplacian ', ' difpar @/@x @/@y')") IDIFFR
      DO IS = 1, NPTST
         IX  = XYTST(2*IS-1)
         IY  = XYTST(2*IS)
         IND = KGRPNT(IX,IY)
         WRITE (PRTEST, "(10(1X,E12.4))") EN(IND), LAPE(IND),&
         &diffr%param(IND),&
         &diffr%dpardx(IND), diffr%dpardy(IND)
      END DO
   END IF

!     --- deallocate arrays
   DEALLOCATE (KLOC, CGLOC, REFRACTIVE_INDEX, REFRACTIVE_DERIVATIVE)
   DEALLOCATE (EN, LAPE, DENOM, K, CG)
   DEALLOCATE (ENTMP)

!     End of subroutine DIFPAR
   RETURN
end subroutine DIFPAR

end module swan_propagation
