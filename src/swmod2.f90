!                 ALLOCATABLE DATA RELATED MODULES, file 2 of 3
!
!     Contents of this file
!
!     OUTP_DATA          information for output data
!     M_BNDSPEC          information for boundary conditions
!     M_OBSTA            information for obstacles
!     M_GENARR           contains a number of general arrays
!     M_PARALL           information for parallelisation with MPI
!     M_DIFFR            information for diffraction

MODULE OUTP_DATA


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
!     40.31: Marcel Zijlema
!     41.78: Bert Jagers
!     41.95: Marcel Zijlema
!
!  1. Updates
!
!     40.13, July 01: New Module
!     40.13, Oct. 01: Longer filenames for output requests
!     40.31, Dec. 03: derive types OPSDAT, ORQDAT added
!     41.78, Mar. 21: delete linked lists
!     41.95, Jul. 22: introduction VTK and PVD attributes
!
!  2. Purpose
!
!     Contains data needed during generation of output
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used


   use swan_io_limits, only: LENFNM
   IMPLICIT NONE(TYPE, EXTERNAL)


!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables

   INTEGER, PARAMETER :: MAX_OUTP_REQ = 250 ! max. number of output r

   LOGICAL            :: LCOMPGRD = .FALSE.

!     longer filenames for output requests
   CHARACTER (LEN=LENFNM) :: OUTP_FILES(1:MAX_OUTP_REQ) = ''
   ! filenames for output; index is output request sequence number

   INTEGER, SAVE :: NREOQ = 0         ! actual number of requests sav

   TYPE OPSDAT
      CHARACTER (LEN=1)     :: PSTYPE = ''                ! type (F,
      CHARACTER (LEN=8)     :: PSNAME = ''                ! name of p
      INTEGER               :: OPI(2) = 0                 ! integer c
      REAL                  :: OPR(5) = 0.0               ! real coef
      INTEGER               :: MIP = 0                    ! number of
      REAL, POINTER         :: XP(:) => NULL(), YP(:) => NULL()
      REAL, POINTER         :: XQ(:) => NULL(), YQ(:) => NULL()
      TYPE(OPSDAT), POINTER :: NEXTOPS => NULL()
   end type OPSDAT

   TYPE(OPSDAT), SAVE, TARGET  :: FOPS
   TYPE(OPSDAT), SAVE, POINTER :: COPS => NULL()
   LOGICAL, SAVE :: LOPS = .FALSE.

   TYPE ORQDAT
      CHARACTER (LEN=4)      :: RQTYPE = '' ! type (BLK, TAB, SPC ...)
      CHARACTER (LEN=8)      :: PSNAME = '' ! name of point set
      INTEGER                :: OQI(4) = 0  ! integer coefficients
      REAL(KIND=KIND(0.0D0)) :: OQR(2) = 0.0D0 ! real coefficients
      INTEGER, POINTER       :: IVTYP(:) => NULL() ! type of output variable
      REAL, POINTER          :: FAC(:) => NULL()   ! multiplication factor of b
      TYPE(ORQDAT), POINTER  :: NEXTORQ => NULL()
   end type ORQDAT

   TYPE(ORQDAT), SAVE, TARGET  :: FORQ
   LOGICAL, SAVE :: LORQ = .FALSE.

!  8. Subroutines and functions used

   INTERFACE DELETE
      MODULE PROCEDURE DELETEOPS
      MODULE PROCEDURE DELETEORQ
   end interface DELETE

   PUBLIC :: CLEAR_OUTPUT_STATE, OUTPUT_STATE_IS_CLEAR

!  9. Subroutines and functions calling
!
!     SWREAD : reads data (command OUTPut OPTions)
!     SWBLOK : produces block output
!     SWTABP : produces table output
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

CONTAINS

   RECURSIVE SUBROUTINE DELETEOPS ( OPS )

      TYPE(OPSDAT) :: OPS

         IF ( ASSOCIATED( OPS%XP ) ) THEN
            DEALLOCATE( OPS%XP )
            NULLIFY( OPS%XP )
         ENDIF
         IF ( ASSOCIATED( OPS%YP ) ) THEN
            DEALLOCATE( OPS%YP )
            NULLIFY( OPS%YP )
         ENDIF
         IF ( ASSOCIATED( OPS%XQ ) ) THEN
            DEALLOCATE( OPS%XQ )
            NULLIFY( OPS%XQ )
         ENDIF
         IF ( ASSOCIATED( OPS%YQ ) ) THEN
            DEALLOCATE( OPS%YQ )
            NULLIFY( OPS%YQ )
         ENDIF
         IF ( ASSOCIATED( OPS%NEXTOPS ) ) THEN
            CALL DELETEOPS ( OPS%NEXTOPS )
            DEALLOCATE( OPS%NEXTOPS )
            NULLIFY( OPS%NEXTOPS )
         ENDIF
   end subroutine DELETEOPS

   RECURSIVE SUBROUTINE DELETEORQ ( ORQ )

      TYPE(ORQDAT) :: ORQ

         IF ( ASSOCIATED( ORQ%IVTYP ) ) THEN
            DEALLOCATE( ORQ%IVTYP )
            NULLIFY( ORQ%IVTYP )
         ENDIF
         IF ( ASSOCIATED( ORQ%FAC ) ) THEN
            DEALLOCATE( ORQ%FAC )
            NULLIFY( ORQ%FAC )
         ENDIF
         IF ( ASSOCIATED( ORQ%NEXTORQ ) ) THEN
            CALL DELETEORQ ( ORQ%NEXTORQ )
            DEALLOCATE( ORQ%NEXTORQ )
            NULLIFY( ORQ%NEXTORQ )
         ENDIF
   end subroutine DELETEORQ

   SUBROUTINE CLEAR_OUTPUT_STATE ()
      CALL DELETEOPS ( FOPS )
      CALL DELETEORQ ( FORQ )
      FOPS = OPSDAT()
      FORQ = ORQDAT()
      NULLIFY(COPS)
      LOPS = .FALSE.
      LORQ = .FALSE.
      LCOMPGRD = .FALSE.
      NREOQ = 0
      OUTP_FILES = ''
   END SUBROUTINE CLEAR_OUTPUT_STATE

   LOGICAL FUNCTION OUTPUT_STATE_IS_CLEAR ()
      OUTPUT_STATE_IS_CLEAR = .NOT.LOPS .AND. .NOT.LORQ .AND. &
         .NOT.LCOMPGRD .AND. NREOQ.EQ.0 .AND. .NOT.ASSOCIATED(COPS) .AND. &
         .NOT.ASSOCIATED(FOPS%XP) .AND. .NOT.ASSOCIATED(FOPS%YP) .AND. &
         .NOT.ASSOCIATED(FOPS%XQ) .AND. .NOT.ASSOCIATED(FOPS%YQ) .AND. &
         .NOT.ASSOCIATED(FOPS%NEXTOPS) .AND. &
         .NOT.ASSOCIATED(FORQ%IVTYP) .AND. .NOT.ASSOCIATED(FORQ%FAC) .AND. &
         .NOT.ASSOCIATED(FORQ%NEXTORQ) .AND. ALL(OUTP_FILES.EQ.'')
   END FUNCTION OUTPUT_STATE_IS_CLEAR

end module OUTP_DATA

MODULE M_BNDSPEC


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
!     40.31: Marcel Zijlema
!     41.78: Bert Jagers
!
!  1. Updates
!
!     40.31, Nov. 03: New Module
!     41.78, Mar. 21: delete linked lists
!
!  2. Purpose
!
!     Contains data with respect to specification
!     of boundary conditions
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---

   IMPLICIT NONE(TYPE, EXTERNAL)

!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     ALOBND  : if true, user has specified boundary conditions
!     BFILED  : data concerning boundary condition files
!     BGP     : array containing data w.r.t. boundary grid points
!     BSPLOC  : place in array BSPECS where to store interpolated spectra
!     BSPDIR  : spectral directions of input spectrum
!     BSPFRQ  : spectral frequencies of input spectrum
!     CUBGP   : current item in list of boundary grid points
!     DSHAPE  : indicates option for computation of directional distribution
!               in the spectrum (boundary spectra etc.)
!               =1: directional spread in degrees is given
!               =2: power of COS is given
!     FBNDFIL : first boundary condition file in list of files
!     FBGP    : first item in list of boundary grid points
!     FBS     : first item in list of boundary spectrum parameters
!     FSHAPE  : indicates option for computation of frequency distribution
!               in the spectrum (boundary spectra etc.)
!               =1: Pierson-Moskowitz
!               =2: Jonswap
!               =3: bin
!               =4: Gaussian
!     NBS     : index of BSPECS
!     NEXTBGP : pointer to next item in list of boundary grid points
!     NEXTBS  : pointer to next item in list of boundary spectrum parameters
!     NEXTBSPC: pointer to next boundary condition file in list
!     SPPARM  : integral parameters used for computation of
!               incident spectrum. Meaning:
!               1: significant wave height
!               2: wave period (peak or mean)
!               3: average wave direction
!               4: directional distribution coefficient

   LOGICAL :: ALOBND = .FALSE.

   TYPE BSPCDAT
      INTEGER                :: BFILED(20) = 0
      INTEGER, POINTER       :: BSPLOC(:) => NULL()
      REAL, POINTER          :: BSPDIR(:) => NULL(), BSPFRQ(:) => NULL()
      TYPE(BSPCDAT), POINTER :: NEXTBSPC => NULL()
   end type BSPCDAT

   TYPE(BSPCDAT), SAVE, TARGET :: FBNDFIL
   LOGICAL, SAVE :: LBFILS = .FALSE.

   TYPE BSDAT
      INTEGER                :: NBS = 0
      INTEGER                :: FSHAPE = 0, DSHAPE = 0
      REAL                   :: SPPARM(4) = 0.0
      TYPE(BSDAT), POINTER   :: NEXTBS => NULL()
   end type BSDAT

   TYPE(BSDAT), SAVE, TARGET :: FBS
   LOGICAL, SAVE :: LBS = .FALSE.

   TYPE BGPDAT
      INTEGER                :: BGP(6) = 0
      TYPE(BGPDAT), POINTER  :: NEXTBGP => NULL()
   end type BGPDAT

   TYPE(BGPDAT), SAVE, TARGET  :: FBGP
   TYPE(BGPDAT), SAVE, POINTER :: CUBGP => NULL()
   LOGICAL, SAVE :: LBGP = .FALSE.

!  8. Subroutines and functions used

   INTERFACE DELETE
      MODULE PROCEDURE DELETEBSPC
      MODULE PROCEDURE DELETEBS
      MODULE PROCEDURE DELETEBGP
   end interface DELETE

   PUBLIC :: CLEAR_BOUNDARY_STATE, BOUNDARY_STATE_IS_CLEAR

!  9. Subroutines and functions calling
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

CONTAINS

   RECURSIVE SUBROUTINE DELETEBSPC ( BSPC )

      TYPE(BSPCDAT) :: BSPC

         IF ( ASSOCIATED( BSPC%BSPLOC ) ) THEN
            DEALLOCATE( BSPC%BSPLOC )
            NULLIFY( BSPC%BSPLOC )
         ENDIF
         IF ( ASSOCIATED( BSPC%BSPDIR ) ) THEN
            DEALLOCATE( BSPC%BSPDIR )
            NULLIFY( BSPC%BSPDIR )
         ENDIF
         IF ( ASSOCIATED( BSPC%BSPFRQ ) ) THEN
            DEALLOCATE( BSPC%BSPFRQ )
            NULLIFY( BSPC%BSPFRQ )
         ENDIF
         IF ( ASSOCIATED( BSPC%NEXTBSPC ) ) THEN
            CALL DELETEBSPC ( BSPC%NEXTBSPC )
            DEALLOCATE( BSPC%NEXTBSPC )
            NULLIFY( BSPC%NEXTBSPC )
         ENDIF
   end subroutine DELETEBSPC

   RECURSIVE SUBROUTINE DELETEBS ( BS )

      TYPE(BSDAT) :: BS

         IF ( ASSOCIATED( BS%NEXTBS ) ) THEN
            CALL DELETEBS ( BS%NEXTBS )
            DEALLOCATE( BS%NEXTBS )
            NULLIFY( BS%NEXTBS )
         ENDIF
   end subroutine DELETEBS

   RECURSIVE SUBROUTINE DELETEBGP ( BGP )

      TYPE(BGPDAT) :: BGP

         IF ( ASSOCIATED( BGP%NEXTBGP ) ) THEN
            CALL DELETEBGP ( BGP%NEXTBGP )
            DEALLOCATE( BGP%NEXTBGP )
            NULLIFY( BGP%NEXTBGP )
         ENDIF
   end subroutine DELETEBGP

   SUBROUTINE CLEAR_BOUNDARY_STATE ()
      CALL DELETEBSPC ( FBNDFIL )
      CALL DELETEBS ( FBS )
      CALL DELETEBGP ( FBGP )
      FBNDFIL = BSPCDAT()
      FBS = BSDAT()
      FBGP = BGPDAT()
      NULLIFY(CUBGP)
      LBFILS = .FALSE.
      LBS = .FALSE.
      LBGP = .FALSE.
      ALOBND = .FALSE.
   END SUBROUTINE CLEAR_BOUNDARY_STATE

   LOGICAL FUNCTION BOUNDARY_STATE_IS_CLEAR ()
      BOUNDARY_STATE_IS_CLEAR = .NOT.ALOBND .AND. .NOT.LBFILS .AND. &
         .NOT.LBS .AND. .NOT.LBGP .AND. .NOT.ASSOCIATED(CUBGP) .AND. &
         .NOT.ASSOCIATED(FBNDFIL%BSPLOC) .AND. &
         .NOT.ASSOCIATED(FBNDFIL%BSPDIR) .AND. &
         .NOT.ASSOCIATED(FBNDFIL%BSPFRQ) .AND. &
         .NOT.ASSOCIATED(FBNDFIL%NEXTBSPC) .AND. &
         .NOT.ASSOCIATED(FBS%NEXTBS) .AND. &
         .NOT.ASSOCIATED(FBGP%NEXTBGP)
   END FUNCTION BOUNDARY_STATE_IS_CLEAR

end module M_BNDSPEC

MODULE M_OBSTA


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
!     40.31: Marcel Zijlema
!
!  1. Updates
!
!     Nov. 03: New Module
!
!  2. Purpose
!
!     Contains data with respect to obstacles
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---

   IMPLICIT NONE(TYPE, EXTERNAL)

!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     FOBSTAC : first obstacle in list of obstacles
!     FBCOEF  : freeboard dependent coefficients
!     FBTYP1  : freeboard type: freeboard or not
!     FBTYP2  : freeboard type: includes quay or not
!     IGCOEF  : FIG coefficients
!     IGFRQD  : frequency distribution for FIG source
!     IGTYP   : FIG type: FIG source term or not
!     NCRPTS  : number of corner points in obstacle
!     NEXTOBST: pointer to next obstacle in list
!     OBSTDONE: check handling obstacles done or not
!     RFCOEF  : reflection coefficients
!     RFTYP1  : reflection type: standard (REFL)
!     RFTYP2  : reflection type: diffusive/specular (RDIFF/RSPEC)
!     RFTYP3  : reflection type: frequency-dependent (RFD)
!     TRCF1D  : frequency dependent transmission coefficients
!     TRCF2D  : frequency and direction dependent transmission coefficients
!     TRCOEF  : transmission coefficients
!     TRTYPE  : transmission type
!     XCRP    : x-coordinate of corner point
!     YCRP    : y-coordinate of corner point

   LOGICAL, SAVE             :: OBSTDONE = .FALSE.

   TYPE OBSTDAT
      INTEGER                :: TRTYPE = 0
      REAL                   :: TRCOEF(3) = 0.0
      REAL, POINTER          :: TRCF1D(:) => NULL()
      REAL, POINTER          :: TRCF2D(:,:) => NULL()
      INTEGER                :: RFTYP1 = 0, RFTYP2 = 0, RFTYP3 = 0
      REAL                   :: RFCOEF(6) = 0.0
      INTEGER                :: FBTYP1 = 0, FBTYP2 = 0
      REAL                   :: FBCOEF(3) = 0.0
      INTEGER                :: IGTYP = 0
      REAL                   :: IGCOEF(7) = 0.0
      REAL, POINTER          :: IGFRQD(:) => NULL()
      INTEGER                :: NCRPTS = 0
      REAL, POINTER          :: XCRP(:) => NULL(), YCRP(:) => NULL()
      TYPE(OBSTDAT), POINTER :: NEXTOBST => NULL()
   end type OBSTDAT

   TYPE(OBSTDAT), SAVE, TARGET  :: FOBSTAC

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
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

   PUBLIC :: CLEAR_OBSTACLE_STATE, OBSTACLE_STATE_IS_CLEAR

CONTAINS

   RECURSIVE SUBROUTINE CLEAR_OBSTACLE_NODE ( NODE )
      TYPE(OBSTDAT), INTENT(INOUT) :: NODE

      IF ( ASSOCIATED(NODE%NEXTOBST) ) THEN
         CALL CLEAR_OBSTACLE_NODE ( NODE%NEXTOBST )
         DEALLOCATE(NODE%NEXTOBST)
      END IF
      IF ( ASSOCIATED(NODE%TRCF1D) ) DEALLOCATE(NODE%TRCF1D)
      IF ( ASSOCIATED(NODE%TRCF2D) ) DEALLOCATE(NODE%TRCF2D)
      IF ( ASSOCIATED(NODE%IGFRQD) ) DEALLOCATE(NODE%IGFRQD)
      IF ( ASSOCIATED(NODE%XCRP) )   DEALLOCATE(NODE%XCRP)
      IF ( ASSOCIATED(NODE%YCRP) )   DEALLOCATE(NODE%YCRP)
      NODE = OBSTDAT()
   END SUBROUTINE CLEAR_OBSTACLE_NODE

   SUBROUTINE CLEAR_OBSTACLE_STATE ()
      CALL CLEAR_OBSTACLE_NODE ( FOBSTAC )
      OBSTDONE = .FALSE.
   END SUBROUTINE CLEAR_OBSTACLE_STATE

   LOGICAL FUNCTION OBSTACLE_STATE_IS_CLEAR ()
      OBSTACLE_STATE_IS_CLEAR = .NOT.OBSTDONE .AND. &
         .NOT.ASSOCIATED(FOBSTAC%TRCF1D) .AND. &
         .NOT.ASSOCIATED(FOBSTAC%TRCF2D) .AND. &
         .NOT.ASSOCIATED(FOBSTAC%IGFRQD) .AND. &
         .NOT.ASSOCIATED(FOBSTAC%XCRP) .AND. &
         .NOT.ASSOCIATED(FOBSTAC%YCRP) .AND. &
         .NOT.ASSOCIATED(FOBSTAC%NEXTOBST)
   END FUNCTION OBSTACLE_STATE_IS_CLEAR

end module M_OBSTA

MODULE M_GENARR


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
!     40.31: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     Oct. 03: New Module
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Create several allocatable arrays for SWAN computation
!
!  3. Method
!
!     The following arrays will be created:
!
!     KGRPNT, KGRBND
!     XYTST
!     AC2
!     XCGRID, YCGRID
!     SPCSIG, SPCDIR
!     DEPTH , FRIC
!     UXB   , UYB
!     WXI   , WYI
!     WLEVL , ASTDF
!     NPLAF, TURBF
!     MUDLF
!     AICEF, HICEF
!     HSSF, TSSF, DSSF
!
!  4. Modules used
!
!     ---

   IMPLICIT NONE(TYPE, EXTERNAL)

!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     ---
!
!  7. Local variables
!
!     AC2   : Contains action density at present time step
!     AICEF : input field containing ice concentration (fraction)
!     ASTDF : input field of air-sea temperature difference
!     DEPTH : input field of depth
!     DSSF  : input field containing sea-swell mean wave direction
!     FRIC  : input field of friction
!     HICEF : input field containing ice thickness (meters)
!     HSSF  : input field containing sea-swell sig wave height
!     KGRBND: array containing all boundary points
!             (+ 2 extra zeros as area separator for all separated areas)
!     KGRPNT: array containing indirect addresses for grid points
!     LAYH  : layer thickness for vegetation model
!     MUDLF : input field containing fluid mud layer
!     NPLAF : input field containing number of plants per square meter
!     SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
!     SPCSIG: Relative frequencies in computational domain in sigma-space
!     TSSF  : input field containing sea-swell mean wave period
!     TURBF : input field containing turbulent viscosity
!     UXB   : input field of contravariant U-velocity
!     UYB   : input field of contravariant V-velocity
!     VEGDIL: vegetation diameter for each layer and grid point
!     VEGDRL: drag coefficient for each layer and grid point
!     VEGNSL: number of plants / m2 for each layer and grid point
!     WLEVL : input field of water level
!     WXI   : input field of wind U-velocity (contravariant)
!     WYI   : input field of wind V-velocity (contravariant)
!     XCGRID: Coordinates of computational grid in x-direction
!     XYTST : Grid point indices of test points
!     YCGRID: Coordinates of computational grid in y-direction

   INTEGER, SAVE, ALLOCATABLE :: KGRPNT(:,:), KGRBND(:)
   INTEGER, SAVE, ALLOCATABLE :: XYTST(:)
   REAL   , SAVE, ALLOCATABLE :: AC2(:,:,:)
   REAL   , SAVE, ALLOCATABLE :: XCGRID(:,:), YCGRID(:,:)
   REAL   , SAVE, ALLOCATABLE :: SPCSIG(:)  , SPCDIR(:,:)
!
!     Exponential wind-input state exposed for ESMF coupling. The selected
!     coupling backend is a no-op unless ESMF support is enabled.
   LOGICAL, SAVE :: SAVE_SINBAC = .FALSE.
   REAL   , SAVE, ALLOCATABLE :: SINBAC(:,:,:)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
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

!     Several arrays here and in M_PARALL are only filled when the deck asks
!     for them: an input field when a READINP command supplies it, the global
!     grid arrays when the grid is structured. They are passed on to the
!     reading, output and computation routines either way, and whether the data
!     exists is decided by flags such as LEDS, never by ALLOCATED. Passing an
!     unallocated allocatable as an actual argument is invalid, so SWINIT gives
!     each of them the empty state and the routines below grow it to the size
!     that is actually needed. Sizing rather than merely testing for allocation
!     is what makes that safe: the earlier `IF (.NOT.ALLOCATED(..))` guard would
!     have kept the empty array instead. It also covers the same grid being read
!     again at a different size, which that guard silently ignored.

   INTERFACE ENSURE_FIELD_SIZE
      MODULE PROCEDURE ENSURE_FIELD_SIZE_R, ENSURE_FIELD_SIZE_I
      MODULE PROCEDURE ENSURE_FIELD_SIZE_R2, ENSURE_FIELD_SIZE_I2
   END INTERFACE ENSURE_FIELD_SIZE

   PUBLIC :: CLEAR_GENERAL_ARRAY_STATE, GENERAL_ARRAY_STATE_IS_CLEAR

contains

   SUBROUTINE ENSURE_FIELD_SIZE_R (FIELD, LENGTH)
      REAL, ALLOCATABLE, INTENT(INOUT) :: FIELD(:)
      INTEGER, INTENT(IN)              :: LENGTH

      IF (ALLOCATED(FIELD)) THEN
         IF (SIZE(FIELD).EQ.LENGTH) RETURN
         DEALLOCATE(FIELD)
      END IF
      ALLOCATE(FIELD(LENGTH))
   END SUBROUTINE ENSURE_FIELD_SIZE_R

   SUBROUTINE ENSURE_FIELD_SIZE_I (FIELD, LENGTH)
      INTEGER, ALLOCATABLE, INTENT(INOUT) :: FIELD(:)
      INTEGER, INTENT(IN)                 :: LENGTH

      IF (ALLOCATED(FIELD)) THEN
         IF (SIZE(FIELD).EQ.LENGTH) RETURN
         DEALLOCATE(FIELD)
      END IF
      ALLOCATE(FIELD(LENGTH))
   END SUBROUTINE ENSURE_FIELD_SIZE_I

   SUBROUTINE ENSURE_FIELD_SIZE_R2 (FIELD, LENGTH1, LENGTH2)
      REAL, ALLOCATABLE, INTENT(INOUT) :: FIELD(:,:)
      INTEGER, INTENT(IN)              :: LENGTH1, LENGTH2

      IF (ALLOCATED(FIELD)) THEN
         IF (SIZE(FIELD,1).EQ.LENGTH1 .AND. SIZE(FIELD,2).EQ.LENGTH2) RETURN
         DEALLOCATE(FIELD)
      END IF
      ALLOCATE(FIELD(LENGTH1,LENGTH2))
   END SUBROUTINE ENSURE_FIELD_SIZE_R2

   SUBROUTINE ENSURE_FIELD_SIZE_I2 (FIELD, LENGTH1, LENGTH2)
      INTEGER, ALLOCATABLE, INTENT(INOUT) :: FIELD(:,:)
      INTEGER, INTENT(IN)                 :: LENGTH1, LENGTH2

      IF (ALLOCATED(FIELD)) THEN
         IF (SIZE(FIELD,1).EQ.LENGTH1 .AND. SIZE(FIELD,2).EQ.LENGTH2) RETURN
         DEALLOCATE(FIELD)
      END IF
      ALLOCATE(FIELD(LENGTH1,LENGTH2))
   END SUBROUTINE ENSURE_FIELD_SIZE_I2

   SUBROUTINE CLEAR_GENERAL_ARRAY_STATE ()
      IF (ALLOCATED(KGRPNT)) DEALLOCATE(KGRPNT)
      IF (ALLOCATED(KGRBND)) DEALLOCATE(KGRBND)
      IF (ALLOCATED(XYTST )) DEALLOCATE(XYTST )
      IF (ALLOCATED(AC2   )) DEALLOCATE(AC2   )
      IF (ALLOCATED(XCGRID)) DEALLOCATE(XCGRID)
      IF (ALLOCATED(YCGRID)) DEALLOCATE(YCGRID)
      IF (ALLOCATED(SPCSIG)) DEALLOCATE(SPCSIG)
      IF (ALLOCATED(SPCDIR)) DEALLOCATE(SPCDIR)
      IF (ALLOCATED(SINBAC)) DEALLOCATE(SINBAC)
      SAVE_SINBAC = .FALSE.
   END SUBROUTINE CLEAR_GENERAL_ARRAY_STATE

   LOGICAL FUNCTION GENERAL_ARRAY_STATE_IS_CLEAR ()
      GENERAL_ARRAY_STATE_IS_CLEAR = .NOT.ALLOCATED(KGRPNT) .AND. &
         .NOT.ALLOCATED(KGRBND) .AND. .NOT.ALLOCATED(XYTST) .AND. &
         .NOT.ALLOCATED(AC2) .AND. .NOT.ALLOCATED(XCGRID) .AND. &
         .NOT.ALLOCATED(YCGRID) .AND. .NOT.ALLOCATED(SPCSIG) .AND. &
         .NOT.ALLOCATED(SPCDIR) .AND. .NOT.ALLOCATED(SINBAC) .AND. &
         .NOT.SAVE_SINBAC
   END FUNCTION GENERAL_ARRAY_STATE_IS_CLEAR

end module M_GENARR

MODULE M_PARALL
   USE swan_build_config, ONLY: jacobi_sweep_enabled
   USE swan_parallel_state, ONLY: MASTER, INODE, NPROC, IAMMASTER, PARLL
   USE swan_mpi_backend, ONLY: mpi_backend_broadcast_character, &
      mpi_backend_broadcast_integer_array, &
      mpi_backend_broadcast_integer_matrix, &
      mpi_backend_broadcast_integer_scalar, &
      mpi_backend_broadcast_real_array, mpi_backend_broadcast_real_matrix, &
      mpi_backend_broadcast_real_scalar, &
      mpi_backend_broadcast_real_tensor3, &
      mpi_backend_broadcast_real_tensor4, mpi_backend_gather_counts, &
      mpi_backend_gatherv_integer_i12, mpi_backend_gatherv_integer_i21, &
      mpi_backend_gatherv_real, mpi_backend_receive_real_matrix, &
      mpi_backend_receive_real_vector, mpi_backend_reduce_integer_array, &
      mpi_backend_reduce_integer_scalar, mpi_backend_reduce_real_array, &
      mpi_backend_reduce_real_scalar, mpi_backend_send_real_matrix, &
      mpi_backend_send_real_vector, swan_mpi_error_count, swan_mpi_success
   USE swan_service_interfaces, ONLY: MSGERR
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
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     Dec. 03: New Module
!     Jul. 04: introduction logicals
!
!  2. Purpose
!
!     Contains data with respect to parallel process
!     based on distributed-memory apprach using MPI
!
!  3. Method
!
!     ---
!
!  4. Modules used
!
!     ---

   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE :: jacobi_sweep_enabled
   PRIVATE :: MSGERR
   PRIVATE :: mpi_backend_broadcast_character
   PRIVATE :: mpi_backend_broadcast_integer_array
   PRIVATE :: mpi_backend_broadcast_integer_matrix
   PRIVATE :: mpi_backend_broadcast_integer_scalar
   PRIVATE :: mpi_backend_broadcast_real_array
   PRIVATE :: mpi_backend_broadcast_real_matrix
   PRIVATE :: mpi_backend_broadcast_real_scalar
   PRIVATE :: mpi_backend_broadcast_real_tensor3
   PRIVATE :: mpi_backend_broadcast_real_tensor4
   PRIVATE :: mpi_backend_gather_counts
   PRIVATE :: mpi_backend_gatherv_integer_i12
   PRIVATE :: mpi_backend_gatherv_integer_i21
   PRIVATE :: mpi_backend_gatherv_real
   PRIVATE :: mpi_backend_receive_real_matrix, mpi_backend_receive_real_vector
   PRIVATE :: mpi_backend_reduce_integer_array
   PRIVATE :: mpi_backend_reduce_integer_scalar
   PRIVATE :: mpi_backend_reduce_real_array, mpi_backend_reduce_real_scalar
   PRIVATE :: mpi_backend_send_real_matrix, mpi_backend_send_real_vector
   PRIVATE :: swan_mpi_error_count, swan_mpi_success

!     Type-safe interfaces for collective communication.  The legacy
!     implementations receive a typed first element and a count; callers
!     no longer pass an MPI datatype that can disagree with the Fortran
!     datatype of the actual argument.
   INTERFACE SWREDUCE
      MODULE PROCEDURE SWREDUCE_I0, SWREDUCE_I1
      MODULE PROCEDURE SWREDUCE_R0, SWREDUCE_R1
   end interface SWREDUCE

   INTERFACE SWBROADC
      MODULE PROCEDURE SWBROADC_I0, SWBROADC_I1, SWBROADC_I2
      MODULE PROCEDURE SWBROADC_R0, SWBROADC_R1, SWBROADC_R2
      MODULE PROCEDURE SWBROADC_R3, SWBROADC_R4
      MODULE PROCEDURE SWBROADC_C0
   end interface SWBROADC

   INTERFACE SWSENDNB
      MODULE PROCEDURE SWSENDNB_R1, SWSENDNB_R2
   end interface SWSENDNB

   INTERFACE SWRECVNB
      MODULE PROCEDURE SWRECVNB_R1, SWRECVNB_R2
   end interface SWRECVNB

   INTERFACE SWGATHER
      MODULE PROCEDURE SWGATHER_I21, SWGATHER_I12
      MODULE PROCEDURE SWGATHER_R11
   end interface SWGATHER

!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     IBLACK  : integer used to colour subdomains 'black' for
!               determining sequence of sweeps (=4,1,2,3)
!     IGREEN  : integer used to colour subdomains 'green' for
!               determining sequence of sweeps (=3,4,1,2)
!     IHALOX  : width of halo area in x-direction
!     IHALOY  : width of halo area in y-direction
!     IRED    : integer used to colour subdomains 'red' for
!               determining sequence of sweeps (=1,2,3,4)
!     IYELOW  : integer used to colour subdomains 'yellow' for
!               determining sequence of sweeps (=2,3,4,1)
!     MASTER  : rank of master process

!  MASTER now comes from swan_parallel_state (re-exported below).
   INTEGER, PARAMETER :: IRED=1, IYELOW=2, IGREEN=3, IBLACK=4
   INTEGER, PARAMETER :: IHALOX=MERGE(1,3,jacobi_sweep_enabled)
   INTEGER, PARAMETER :: IHALOY=MERGE(1,3,jacobi_sweep_enabled)

!  7. Local variables
!
!     *** variables for parallel process with MPI:
!
!     IAMMASTER: indicate whether this CPU is master or not
!     INODE   : rank of present node
!     NPROC   : number of nodes
!     PARLL   : flag to denote run as parallel (.TRUE.) or not (.FALSE.)
!     SWCHAR  : MPI datatype for characters
!     SWINT   : MPI datatype for integers
!     SWMAX   : MPI collective maximum operation
!     SWMIN   : MPI collective minimum operation
!     SWREAL  : MPI datatype for reals
!     SWSUM   : MPI collective summation

!  INODE/NPROC now come from swan_parallel_state.
   INTEGER SWCHAR, SWINT, SWREAL
   INTEGER SWMAX, SWMIN, SWSUM
!  IAMMASTER/PARLL now come from swan_parallel_state.

!     *** information related to global domain and subdomains
!
!     IBCOL   : integer indicating the color of own subdomain
!     IBLKAD  : administration array for subdomain interfaces
!               contents:
!               pos. 1                     number of neighbouring subdomains
!                                          =m
!               pos. 3*i-1                 number of i-th neighbour
!               pos. 3*i                   position of i-th neighbour with
!                                          respect to present subdomain
!               pos. 3*i+1                 pointer of i-th neighbour in
!                                          last part of this array
!               pos. 3*m+2                 number of overlapping unknowns
!                                          on subdomain interface
!               pos. 3*m+3 ... 3*m+2+n     position of unknown in array
!                                          to be sent to neighbour
!               pos. 3*m+3+n ... 3*m+2*n+2 position of unknown in array
!                                          to be received from neighbour
!     IWEIG   : weights to determine load per part
!     KGRBGL  : array containing all boundary points in global domain
!               (+ 2 extra zeros as area separator for all separated areas)
!     KGRPGL  : indirect addressing for grid points in global domain
!               =1: not active point
!               >1: active point
!     LENSPO  : format length for spectral output
!     LMXF    : logical indicating whether first x-point of subdomain equals
!               first x-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMXL    : logical indicating whether last x-point of subdomain equals
!               last x-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMYF    : logical indicating whether first y-point of subdomain equals
!               first y-point of global domain (=.TRUE.) or not (=.FALSE.)
!     LMYL    : logical indicating whether last y-point of subdomain equals
!               last y-point of global domain (=.TRUE.) or not (=.FALSE.)
!     MCGRDGL : number of wet grid points in global computational grid
!     MXCGL   : number of grid points in x-direction in global
!               computational grid
!     MXF     : first index w.r.t. global grid in x-direction
!     MXL     : last index w.r.t. global grid in x-direction
!     MYCGL   : number of grid points in y-direction in global
!               computational grid
!     MYF     : first index w.r.t. global grid in y-direction
!     MYL     : last index w.r.t. global grid in y-direction
!     NBGGL   : number of grid points for which boundary condition holds
!               in global domain
!     NGRBGL  : number of boundary points in global domain
!     XCLMAX  : maximum x-coordinate in subdomain
!     XCLMIN  : minimum x-coordinate in subdomain
!     YCLMAX  : maximum y-coordinate in subdomain
!     YCLMIN  : minimum y-coordinate in subdomain
!     XGRDGL  : x-coordinate of computational grid in global domain
!     YGRDGL  : y-coordinate of computational grid in global domain
!
   INTEGER IBCOL
   INTEGER MXF, MXL, MYF, MYL
   REAL    XCLMAX, XCLMIN, YCLMAX, YCLMIN

   INTEGER :: LENSPO = 1000

   LOGICAL LMXF, LMXL, LMYF, LMYL

   INTEGER, SAVE, ALLOCATABLE :: IBLKAD(:)
   INTEGER, SAVE, ALLOCATABLE :: IWEIG(:)

!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
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

CONTAINS

   SUBROUTINE SWREDUCE_I0 ( VALUE, ILEN, ITYPRD )
      INTEGER, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(IN)    :: ILEN, ITYPRD
      INTEGER                :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, 1, 'SWREDUCE')) RETURN
      CALL mpi_backend_reduce_integer_scalar(VALUE, ILEN, SWINT, ITYPRD, &
         IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_ALLREDUCE')
   end subroutine SWREDUCE_I0

   SUBROUTINE SWREDUCE_I1 ( VALUES, ILEN, ITYPRD )
      INTEGER, INTENT(INOUT) :: VALUES(:)
      INTEGER, INTENT(IN)    :: ILEN, ITYPRD
      INTEGER                :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWREDUCE')) RETURN
      CALL mpi_backend_reduce_integer_array(VALUES, ILEN, SWINT, ITYPRD, &
         IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_ALLREDUCE')
   end subroutine SWREDUCE_I1

   SUBROUTINE SWREDUCE_R0 ( VALUE, ILEN, ITYPRD )
      REAL, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(IN) :: ILEN, ITYPRD
      INTEGER             :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, 1, 'SWREDUCE')) RETURN
      CALL mpi_backend_reduce_real_scalar(VALUE, ILEN, SWREAL, ITYPRD, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_ALLREDUCE')
   end subroutine SWREDUCE_R0

   SUBROUTINE SWREDUCE_R1 ( VALUES, ILEN, ITYPRD )
      REAL, INTENT(INOUT) :: VALUES(:)
      INTEGER, INTENT(IN) :: ILEN, ITYPRD
      INTEGER             :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWREDUCE')) RETURN
      CALL mpi_backend_reduce_real_array(VALUES, ILEN, SWREAL, ITYPRD, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_ALLREDUCE')
   end subroutine SWREDUCE_R1

   SUBROUTINE SWBROADC_I0 ( VALUE, ILEN )
      INTEGER, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(IN)    :: ILEN
      INTEGER                :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, 1, 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_integer_scalar(VALUE, ILEN, SWINT, &
         MASTER-1, IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_I0

   SUBROUTINE SWBROADC_I1 ( VALUES, ILEN )
      INTEGER, INTENT(INOUT)             :: VALUES(*)
      INTEGER, INTENT(IN)                :: ILEN
      INTEGER                            :: IERR
      IF (.NOT.PARLL) RETURN
      CALL mpi_backend_broadcast_integer_array(VALUES, ILEN, SWINT, &
         MASTER-1, IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_I1

   SUBROUTINE SWBROADC_I2 ( VALUES, ILEN )
      INTEGER, CONTIGUOUS, INTENT(INOUT) :: VALUES(:,:)
      INTEGER, INTENT(IN)                :: ILEN
      INTEGER                            :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_integer_matrix(VALUES, ILEN, SWINT, &
         MASTER-1, IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_I2

   SUBROUTINE SWBROADC_R0 ( VALUE, ILEN )
      REAL, INTENT(INOUT) :: VALUE
      INTEGER, INTENT(IN) :: ILEN
      INTEGER             :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, 1, 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_real_scalar(VALUE, ILEN, SWREAL, MASTER-1, &
         IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_R0

   SUBROUTINE SWBROADC_R1 ( VALUES, ILEN )
      REAL, INTENT(INOUT)             :: VALUES(*)
      INTEGER, INTENT(IN)             :: ILEN
      INTEGER                         :: IERR
      IF (.NOT.PARLL) RETURN
      CALL mpi_backend_broadcast_real_array(VALUES, ILEN, SWREAL, MASTER-1, &
         IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_R1

   SUBROUTINE SWBROADC_R2 ( VALUES, ILEN )
      REAL, CONTIGUOUS, INTENT(INOUT) :: VALUES(:,:)
      INTEGER, INTENT(IN)             :: ILEN
      INTEGER                         :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_real_matrix(VALUES, ILEN, SWREAL, MASTER-1, &
         IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_R2

   SUBROUTINE SWBROADC_R3 ( VALUES, ILEN )
      REAL, CONTIGUOUS, INTENT(INOUT) :: VALUES(:,:,:)
      INTEGER, INTENT(IN)             :: ILEN
      INTEGER                         :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_real_tensor3(VALUES, ILEN, SWREAL, &
         MASTER-1, IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_R3

   SUBROUTINE SWBROADC_R4 ( VALUES, ILEN )
      REAL, CONTIGUOUS, INTENT(INOUT) :: VALUES(:,:,:,:)
      INTEGER, INTENT(IN)             :: ILEN
      INTEGER                         :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_real_tensor4(VALUES, ILEN, SWREAL, &
         MASTER-1, IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_R4

   SUBROUTINE SWBROADC_C0 ( VALUE, ILEN )
      CHARACTER(LEN=*), INTENT(INOUT) :: VALUE
      INTEGER, INTENT(IN)             :: ILEN
      INTEGER                         :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, LEN(VALUE), 'SWBROADC')) RETURN
      CALL mpi_backend_broadcast_character(VALUE, ILEN, SWCHAR, MASTER-1, &
         IERR)
      CALL SWBROADC_CHECK(IERR)
   end subroutine SWBROADC_C0

   SUBROUTINE SWBROADC_CHECK ( IERR )
      INTEGER, INTENT(IN) :: IERR
      CHARACTER(LEN=80) :: MSGSTR
      IF (IERR.NE.swan_mpi_success) THEN
         WRITE(MSGSTR,'(A,I0)')&
         &'MPI_BCAST failed with return code ', IERR
         CALL MSGERR(4, MSGSTR)
      END IF
   end subroutine SWBROADC_CHECK

   SUBROUTINE SWSENDNB_R1 ( VALUES, ILEN, IDEST, ITAG )
      REAL, CONTIGUOUS, INTENT(IN) :: VALUES(:)
      INTEGER, INTENT(IN)          :: ILEN, IDEST, ITAG
      INTEGER                      :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWSENDNB')) RETURN
      CALL mpi_backend_send_real_vector(VALUES, ILEN, SWREAL, IDEST-1, &
         ITAG, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_SEND')
   end subroutine SWSENDNB_R1

   SUBROUTINE SWSENDNB_R2 ( VALUES, ILEN, IDEST, ITAG )
      REAL, CONTIGUOUS, INTENT(IN) :: VALUES(:,:)
      INTEGER, INTENT(IN)          :: ILEN, IDEST, ITAG
      INTEGER                      :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWSENDNB')) RETURN
      CALL mpi_backend_send_real_matrix(VALUES, ILEN, SWREAL, IDEST-1, &
         ITAG, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_SEND')
   end subroutine SWSENDNB_R2

   SUBROUTINE SWRECVNB_R1 ( VALUES, ILEN, ISOURCE, ITAG )
      REAL, CONTIGUOUS, INTENT(OUT) :: VALUES(:)
      INTEGER, INTENT(IN)           :: ILEN, ISOURCE, ITAG
      INTEGER                       :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWRECVNB')) RETURN
      CALL mpi_backend_receive_real_vector(VALUES, ILEN, SWREAL, ISOURCE-1, &
         ITAG, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_RECV')
   end subroutine SWRECVNB_R1

   SUBROUTINE SWRECVNB_R2 ( VALUES, ILEN, ISOURCE, ITAG )
      REAL, CONTIGUOUS, INTENT(OUT) :: VALUES(:,:)
      INTEGER, INTENT(IN)            :: ILEN, ISOURCE, ITAG
      INTEGER                        :: IERR
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(ILEN, SIZE(VALUES), 'SWRECVNB')) RETURN
      CALL mpi_backend_receive_real_matrix(VALUES, ILEN, SWREAL, ISOURCE-1, &
         ITAG, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_RECV')
   end subroutine SWRECVNB_R2

   SUBROUTINE SWGATHER_I21 ( OUTPUT, IOLEN, INPUT, IILEN )
      INTEGER, INTENT(OUT) :: OUTPUT(:,:)
      INTEGER, INTENT(IN)  :: INPUT(:)
      INTEGER, INTENT(IN)  :: IOLEN, IILEN
      INTEGER              :: IERR
      INTEGER, ALLOCATABLE :: ICOUNT(:), IDSPLC(:)
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(IOLEN, SIZE(OUTPUT), 'SWGATHER')) RETURN
      IF (.NOT.SWVALID_COUNT(IILEN, SIZE(INPUT), 'SWGATHER')) RETURN
      CALL SWGATHER_LAYOUT ( IILEN, IOLEN, ICOUNT, IDSPLC, IERR )
      IF (IERR.EQ.swan_mpi_success) &
         CALL mpi_backend_gatherv_integer_i21(INPUT, IILEN, OUTPUT, ICOUNT, &
            IDSPLC, SWINT, MASTER-1, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_GATHERV')
      DEALLOCATE(ICOUNT,IDSPLC)
   end subroutine SWGATHER_I21

   SUBROUTINE SWGATHER_I12 ( OUTPUT, IOLEN, INPUT, IILEN )
      INTEGER, INTENT(OUT) :: OUTPUT(:)
      INTEGER, INTENT(IN)  :: INPUT(:,:)
      INTEGER, INTENT(IN)  :: IOLEN, IILEN
      INTEGER              :: IERR
      INTEGER, ALLOCATABLE :: ICOUNT(:), IDSPLC(:)
      IF (.NOT.PARLL) RETURN
      IF (.NOT.SWVALID_COUNT(IOLEN, SIZE(OUTPUT), 'SWGATHER')) RETURN
      IF (.NOT.SWVALID_COUNT(IILEN, SIZE(INPUT), 'SWGATHER')) RETURN
      CALL SWGATHER_LAYOUT ( IILEN, IOLEN, ICOUNT, IDSPLC, IERR )
      IF (IERR.EQ.swan_mpi_success) &
         CALL mpi_backend_gatherv_integer_i12(INPUT, IILEN, OUTPUT, ICOUNT, &
            IDSPLC, SWINT, MASTER-1, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_GATHERV')
      DEALLOCATE(ICOUNT,IDSPLC)
   end subroutine SWGATHER_I12

   SUBROUTINE SWGATHER_R11 ( OUTPUT, IOLEN, INPUT, IILEN )
      REAL, INTENT(OUT)    :: OUTPUT(*)
      REAL, INTENT(IN)     :: INPUT(*)
      INTEGER, INTENT(IN)  :: IOLEN, IILEN
      INTEGER              :: IERR
      INTEGER, ALLOCATABLE :: ICOUNT(:), IDSPLC(:)
      IF (.NOT.PARLL) RETURN
      CALL SWGATHER_LAYOUT ( IILEN, IOLEN, ICOUNT, IDSPLC, IERR )
      IF (IERR.EQ.swan_mpi_success) &
         CALL mpi_backend_gatherv_real(INPUT, IILEN, OUTPUT, ICOUNT, &
            IDSPLC, SWREAL, MASTER-1, IERR)
      CALL SWMPI_CHECK(IERR, 'MPI_GATHERV')
      DEALLOCATE(ICOUNT,IDSPLC)
   end subroutine SWGATHER_R11

   SUBROUTINE SWGATHER_LAYOUT ( IILEN, IOLEN, ICOUNT, IDSPLC,&
   &IERR )
      INTEGER, INTENT(IN)                 :: IILEN, IOLEN
      INTEGER, ALLOCATABLE, INTENT(OUT)   :: ICOUNT(:), IDSPLC(:)
      INTEGER, INTENT(OUT)                :: IERR
      INTEGER                             :: I
      ALLOCATE(ICOUNT(0:NPROC-1),IDSPLC(0:NPROC-1))
      ICOUNT = 0
      IDSPLC = 0
      IERR = 0
      CALL mpi_backend_gather_counts(IILEN, ICOUNT, SWINT, MASTER-1, IERR)
      IF (IERR.NE.swan_mpi_success) RETURN
      IF (IAMMASTER) THEN
         IF ( SUM(ICOUNT).GT.IOLEN ) THEN
            CALL MSGERR ( 4,&
            &'Not enough space allocated for gathered data' )
            IERR = swan_mpi_error_count
            RETURN
         END IF
         DO I = 1, NPROC-1
            IDSPLC(I) = ICOUNT(I-1) + IDSPLC(I-1)
         END DO
      END IF
   end subroutine SWGATHER_LAYOUT

   SUBROUTINE SWMPI_CHECK ( IERR, ROUTINE_NAME )
      INTEGER, INTENT(IN)          :: IERR
      CHARACTER(LEN=*), INTENT(IN) :: ROUTINE_NAME
      CHARACTER(LEN=80)         :: MSGSTR
      IF (IERR.NE.swan_mpi_success) THEN
         WRITE(MSGSTR,'(A,A,A,I0)') TRIM(ROUTINE_NAME),&
         &' failed on this process with return code ', '', IERR
         CALL MSGERR(4, MSGSTR)
      END IF
   end subroutine SWMPI_CHECK

   LOGICAL FUNCTION SWVALID_COUNT ( COUNT, AVAILABLE, ROUTINE_NAME )
      INTEGER, INTENT(IN)          :: COUNT, AVAILABLE
      CHARACTER(LEN=*), INTENT(IN) :: ROUTINE_NAME
      CHARACTER(LEN=160)           :: MSGSTR

      SWVALID_COUNT = COUNT >= 0 .AND. COUNT <= AVAILABLE
      IF (SWVALID_COUNT) RETURN

      WRITE(MSGSTR,'(A,A,I0,A,I0)') TRIM(ROUTINE_NAME),&
      &' received an invalid element count ', COUNT,&
      &' for a buffer of size ', AVAILABLE
      CALL MSGERR ( 4, TRIM(MSGSTR) )
   end function SWVALID_COUNT

   SUBROUTINE CLEAR_PARALLEL_STORAGE ()
      IF (ALLOCATED(IBLKAD)) DEALLOCATE(IBLKAD)
   END SUBROUTINE CLEAR_PARALLEL_STORAGE

   LOGICAL FUNCTION PARALLEL_STORAGE_IS_CLEAR ()
      PARALLEL_STORAGE_IS_CLEAR = .NOT.ALLOCATED(IBLKAD)
   END FUNCTION PARALLEL_STORAGE_IS_CLEAR

end module M_PARALL
