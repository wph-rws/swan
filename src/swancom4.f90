
!     SWAN/COMPU   file 4 of 5
!
!
!     PROGRAM SWANCOM4.FOR
!
!
!     This file SWANCOM4 of the main program SWAN
!     include the next subroutines
!
!     *** nonlinear 4 wave-wave interactions ***
!
!     FAC4WW (compute the constants for the nonlinear wave
!             interactions)
!     RANGE4 (compute the counters for the different types of
!             computations for the nonlinear wave interactions)
!     SWSNL1 (nonlinear four wave interactions; semi-implicit and computed
!             for all bins that fall within a sweep with DIA technique.
!             Interaction are calculated per sweep)
!     SWSNL2 (nonlinear four wave interactions; fully explicit and computed
!             for all bins that fall within a sweep with DIA technique.
!             Interaction are calculated per sweep)
!     SWSNL3 (calculate nonlinear four wave interactions fully explicitly
!             for the full circle per iteration by means of DIA approach
!             and store results in auxiliary array MEMNL4)
!     SWSNL4 (calculate nonlinear four wave interactions fully explicitly
!             for the full circle per iteration by means of MDIA approach
!             and store results in auxiliary array MEMNL4)
!     SWSNL8 (calculate nonlinear four wave interactions fully explicitly
!             for the full circle per iteration by means of DIA approach
!             and store results in auxiliary array MEMNL4. Neighbouring
!             interactions are interpolated in piecewise constant manner)
!     FILNL3 (fill main diagonal and right-hand side of the system with
!             results of array MEMNL4)
!
!     SWINTFXNL (interface with SWAN model to compute nonlinear transfer
!                with the XNL method for given action density spectrum)
!
!     *** nonlinear 3 wave-wave interactions ***
!
!     TCOEF   (compute transfer coefficients)
!     FAC3WW  (compute scaling factors for the triad-wave interaction)
!     SWLTA   (triad-wave interactions calculated with the Lumped Triad
!              Approximation of Eldeberky, 1996)
!     SWDCTA  (triad-wave interactions calculated with the Distributed
!              Collinear Triad Approximation of Booij et al, 2009)
!     SWDNCTA (triad-wave interactions calculated with the Distributed
!              NonCollinear Triad Approximation)
!     SWFTIM  (triad-wave interactions calculated using the full integration
!              and the parametrized bispectrum)
!     PEREXC  (includes periodic exchange between first and second harmonics
!              for estimating biphase based on Saprykina et al, 2017)
!     SWBIDW  (compute the biphase based on the parametrization of De Wit, 2022)
!     SWBIPM  (spatially filter the De Wit's biphase)
!
!----------------------------------------------------------------------
!
!******************************************************************

module swan_nonlinear_interactions
   use swan_triad_state, only: triad_state_t
   use swan_snl4_tables, only: snl4_tables_t
   implicit none(type, external)
   private
   public :: FAC4WW, RANGE4, SWPRE4W, SWSNL1, SWSNL2, SWSNL3, SWSNL4, SWSNL8
   public :: FILNL3, SWINTFXNL, FAC3WW, SWLTA, SWDCTA, SWDNCTA, SWFTIM
   public :: PEREXC, SWBIDW, SWBIPM
contains

SUBROUTINE FAC4WW (XIS   ,SNLC1 ,&
&DAL1  ,DAL2  ,DAL3         ,SPCSIG,&
&WWINT ,WWAWG ,WWSWG, SNL4          )
   USE swan_service_interfaces, ONLY: STRACE

!******************************************************************

   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_diagnostics_level
   USE swan_io_units
   TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!     40.17: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.17, Dec. 01: Implementation of Multiple DIA
!     40.41, Sep. 04: compute indices for interactions which will be
!                     interpolated in piecewise constant manner
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose :
!
!     Calculate interpolation constants for Snl.
!
!  3. Method :
!
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!     INTEGERS:
!     ---------
!     MSC2,MSC1         Auxiliary variables
!     MSC,MDC           Maximum counters in spectral space
!     IDP,IDP1          Positive range for ID
!     IDM,IDM1          Negative range for ID
!     ISP,ISP1          idem for IS
!     ISM,ISM1          idem for IS
!     ISCLW,ISCHG       Minimum and maximum counter for discrete
!                       computations in frequency space
!     ISLOW,ISHGH       Minimum and maximum range in frequency space
!     IDLOW,IDHGH       idem in directional space
!     IS                Frequency counter
!     MSC4MI,MSC4MA     Array dimensions in frequency space
!     MDC4MI,MDC4MA     Array dimensions in direction space
!
!     REALS:
!     ------
!     LAMBDA            Coefficient set 0.25
!     GRAV              Gravitational acceleration
!     SNLC1             Coefficient for the subroutines SWSNLn
!     LAMM2,LAMP2
!     DELTH3,DELTH4     Angles between the interacting wavenumbers
!     DAL1,DAL2,DAL3    Coefficients for the non linear interactions
!     CIDP,CIDM
!     WIDP,WIDP1,WIDM,WIDM1  Weight factors
!     WISP,WISP1,WISM,WISM1  idem
!     AWGn              Interpolation weight factors
!     SWGn              Quadratic interpolation weight factors
!     XIS,XISLN         Difference between succeeding frequencies
!     PI                3.14
!     FREQ              Auxiliary frequency to fill scaling array
!     DDIR,RADE         band width in directional space and factor
!
!     ARRAYS
!     ------
!     AF11    1D   Scaling frequency
!     WWINT   1D   counters for 4WAVE interactions
!     WWAWG   1D   values for the interpolation
!     WWSWG   1D   vaules for the interpolation
!
!     WWINT ( 1 = IDP    WWAWG ( = AGW1    WWSWG ( = SWG1
!             2 = IDP1           = AWG2            = SWG2
!             3 = IDM            = AWG3            = SWG3
!             4 = IDM1           = AWG4            = SWG4
!             5 = ISP            = AWG5            = SWG5
!             6 = ISP1           = AWG6            = SWG6
!             7 = ISM            = AWG7            = SWG7
!             8 = ISM1           = AWG8 )          = SWG8  )
!             9 = ISLOW
!             10= ISHGH
!             11= ISCLW
!             12= ISCHG
!             13= IDLOW
!             14= IDHGH
!             15= MSC4MI
!             16= MSC4MA
!             17= MDC4MI
!             18= MDC4MA
!             19= MSCMAX
!             20= MDCMAX
!             21= IDPP
!             22= IDMM
!             23= ISPP
!             24= ISMM )
!
!  7. Common blocks used
!
!
!  9. Source code :
!
!     -----------------------------------------------------------------
!     Calculate :
!       1. counters for frequency and direction for NL-interaction
!       2. weight factors
!       3. the minimum and maximum counter in IS and ID space
!       4. the interpolation weights
!       5. the quadratic interpolation rates
!       6. fill the array for the frequency**11
!     ----------------------------------------------------------
!
!****************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER     MSC2  ,MSC1  ,IS    ,IDP   ,IDP1  ,&
   &IDM   ,IDM1  ,ISP   ,ISP1  ,ISM   ,ISM1  ,&
   &IDPP  ,IDMM  ,ISPP  ,ISMM  ,&
   &ISLOW ,ISHGH ,ISCLW ,ISCHG ,IDLOW ,IDHGH ,&
   &MSCMAX,MDCMAX

   REAL        SNLC1 ,LAMM2 ,LAMP2 ,DELTH3,&
   &AUX1  ,DELTH4,DAL1  ,DAL2  ,DAL3  ,CIDP  ,WIDP  ,&
   &WIDP1 ,CIDM  ,WIDM  ,WIDM1 ,XIS   ,XISLN ,WISP  ,&
   &WISP1 ,WISM  ,WISM1 ,AWG1  ,AWG2  ,AWG3  ,AWG4  ,&
   &AWG5  ,AWG6  ,AWG7  ,AWG8  ,SWG1  ,SWG2  ,SWG3  ,&
   &SWG4  ,SWG5  ,SWG6  ,SWG7  ,SWG8  ,FREQ  ,&
   &RADE

   REAL       WWAWG(*)               ,&
   &WWSWG(*)

   INTEGER    WWINT(*)


   IF (LTRACE) CALL STRACE (IENT,'FAC4WW')

   IF (ALLOCATED(SNL4%frequency_power_11))&
   &DEALLOCATE(SNL4%frequency_power_11)

!     *** Compute frequency indices                               ***
!     *** XIS is the relative increment of the relative frequency ***

   MSC2   = INT ( FLOAT(MSC) / 2.0 )
   MSC1   = MSC2 - 1
   XIS    = SPCSIG(MSC2) / SPCSIG(MSC1)

!     *** set values for the nonlinear four-wave interactions ***

   SNLC1  = 1. / GRAV**4

   LAMM2  = (1.-PQUAD(1))**2
   LAMP2  = (1.+PQUAD(1))**2
   DELTH3 = ACOS( (LAMM2**2+4.-LAMP2**2) / (4.*LAMM2) )
   AUX1   = SIN(DELTH3)
   DELTH4 = ASIN(-AUX1*LAMM2/LAMP2)

   DAL1   = 1. / (1.+PQUAD(1))**4
   DAL2   = 1. / (1.-PQUAD(1))**4
   DAL3   = 2. * DAL1 * DAL2

!     *** Compute directional indices in sigma and theta space ***

   CIDP   = ABS(DELTH4/DDIR)
   IDP   = INT(CIDP)
   IDP1  = IDP + 1
   WIDP   = CIDP - REAL(IDP)
   WIDP1  = 1.- WIDP

   CIDM   = ABS(DELTH3/DDIR)
   IDM   = INT(CIDM)
   IDM1  = IDM + 1
   WIDM   = CIDM - REAL(IDM)
   WIDM1  = 1.- WIDM
   XISLN  = LOG( XIS )

   ISP    = INT( LOG(1.+PQUAD(1)) / XISLN )
   ISP1   = ISP + 1
   WISP   = (1.+PQUAD(1) - XIS**ISP) / (XIS**ISP1 - XIS**ISP)
   WISP1  = 1. - WISP

   ISM    = INT( LOG(1.-PQUAD(1)) / XISLN )
   ISM1   = ISM - 1
   WISM   = (XIS**ISM -(1.-PQUAD(1))) / (XIS**ISM - XIS**ISM1)
   WISM1  = 1. - WISM

!     *** Range of calculations ***

   ISLOW =  1  + ISM1
   ISHGH = MSC + ISP1 - ISM1
   ISCLW =  1
   ISCHG = MSC - ISM1
   IDLOW = 1 - MDC - MAX(IDM1,IDP1)
   IDHGH = MDC + MDC + MAX(IDM1,IDP1)

   MSC4MI = ISLOW
   MSC4MA = ISHGH
   MDC4MI = IDLOW
   MDC4MA = IDHGH
   MSCMAX = MSC4MA - MSC4MI + 1
   MDCMAX = MDC4MA - MDC4MI + 1

!     *** Interpolation weights ***

   AWG1   = WIDP  * WISP
   AWG2   = WIDP1 * WISP
   AWG3   = WIDP  * WISP1
   AWG4   = WIDP1 * WISP1

   AWG5   = WIDM  * WISM
   AWG6   = WIDM1 * WISM
   AWG7   = WIDM  * WISM1
   AWG8   = WIDM1 * WISM1

!     *** quadratic interpolation ***

   SWG1   = AWG1**2
   SWG2   = AWG2**2
   SWG3   = AWG3**2
   SWG4   = AWG4**2

   SWG5   = AWG5**2
   SWG6   = AWG6**2
   SWG7   = AWG7**2
   SWG8   = AWG8**2

!     --- determine discrete counters for piecewise
!         constant interpolation

   IF (AWG1.LT.AWG2) THEN
      IF (AWG2.LT.AWG3) THEN
         IF (AWG3.LT.AWG4) THEN
            ISPP=ISP
            IDPP=IDP
         ELSE
            ISPP=ISP
            IDPP=IDP1
         END IF
      ELSE IF (AWG2.LT.AWG4) THEN
         ISPP=ISP
         IDPP=IDP
      ELSE
         ISPP=ISP1
         IDPP=IDP
      END IF
   ELSE IF (AWG1.LT.AWG3) THEN
      IF (AWG3.LT.AWG4) THEN
         ISPP=ISP
         IDPP=IDP
      ELSE
         ISPP=ISP
         IDPP=IDP1
      END IF
   ELSE IF (AWG1.LT.AWG4) THEN
      ISPP=ISP
      IDPP=IDP
   ELSE
      ISPP=ISP1
      IDPP=IDP1
   END IF
   IF (AWG5.LT.AWG6) THEN
      IF (AWG6.LT.AWG7) THEN
         IF (AWG7.LT.AWG8) THEN
            ISMM=ISM
            IDMM=IDM
         ELSE
            ISMM=ISM
            IDMM=IDM1
         END IF
      ELSE IF (AWG6.LT.AWG8) THEN
         ISMM=ISM
         IDMM=IDM
      ELSE
         ISMM=ISM1
         IDMM=IDM
      END IF
   ELSE IF (AWG5.LT.AWG7) THEN
      IF (AWG7.LT.AWG8) THEN
         ISMM=ISM
         IDMM=IDM
      ELSE
         ISMM=ISM
         IDMM=IDM1
      END IF
   ELSE IF (AWG5.LT.AWG8) THEN
      ISMM=ISM
      IDMM=IDM
   ELSE
      ISMM=ISM1
      IDMM=IDM1
   END IF

!     *** fill the arrays *

   WWINT(1) = IDP
   WWINT(2) = IDP1
   WWINT(3) = IDM
   WWINT(4) = IDM1
   WWINT(5) = ISP
   WWINT(6) = ISP1
   WWINT(7) = ISM
   WWINT(8) = ISM1
   WWINT(9) = ISLOW
   WWINT(10)= ISHGH
   WWINT(11)= ISCLW
   WWINT(12)= ISCHG
   WWINT(13)= IDLOW
   WWINT(14)= IDHGH
   WWINT(15)= MSC4MI
   WWINT(16)= MSC4MA
   WWINT(17)= MDC4MI
   WWINT(18)= MDC4MA
   WWINT(19)= MSCMAX
   WWINT(20)= MDCMAX
   WWINT(21)= IDPP
   WWINT(22)= IDMM
   WWINT(23)= ISPP
   WWINT(24)= ISMM

   WWAWG(1) = AWG1
   WWAWG(2) = AWG2
   WWAWG(3) = AWG3
   WWAWG(4) = AWG4
   WWAWG(5) = AWG5
   WWAWG(6) = AWG6
   WWAWG(7) = AWG7
   WWAWG(8) = AWG8

   WWSWG(1) = SWG1
   WWSWG(2) = SWG2
   WWSWG(3) = SWG3
   WWSWG(4) = SWG4
   WWSWG(5) = SWG5
   WWSWG(6) = SWG6
   WWSWG(7) = SWG7
   WWSWG(8) = SWG8

   ALLOCATE (SNL4%frequency_power_11(MSC4MI:MSC4MA))

!     *** Fill scaling array (f**11)                     ***
!     *** compute the radian frequency**11 for IS=1, MSC ***

   do IS=1, MSC
      SNL4%frequency_power_11(IS) = ( SPCSIG(IS) / ( 2. * PI ) )**11
   end do

!     *** compute the radian frequency for the IS = MSC+1, ISHGH ***

   FREQ   = SPCSIG(MSC) / ( 2. * PI )
   do IS = MSC+1, ISHGH
      FREQ   = FREQ * XIS
      SNL4%frequency_power_11(IS) = FREQ**11
   end do

!     *** compute the radian frequency for IS = 0, ISLOW ***

   FREQ   = SPCSIG(1) / ( 2. * PI )
   do IS = 0, ISLOW, -1
      FREQ   = FREQ / XIS
      SNL4%frequency_power_11(IS) = FREQ**11
   end do

!     *** test output ***

   IF (ISLOW .LT. MSC4MI .OR. ISHGH .GT. MSC4MA .OR.&
   &IDLOW .LT. MDC4MI .OR. IDHGH .GT. MDC4MA) THEN
      WRITE (PRINTF,"( ' ** Error : array bounds and maxima in subr FAC4WW', /,' ISL,ISH : ',2I4, ' IDL,IDH : ',2I4, /,' SMI,SMA : ',2I4, ' DMI,DMA : ',2I4)")&
      &ISLOW, ISHGH, IDLOW, IDHGH,&
      &MSC4MI,MSC4MA, MDC4MI, MDC4MA
   ENDIF

   IF (ITEST .GE. 40) THEN
      RADE = 360.0 / ( 2. * PI )
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' FAC4WW subroutine '
      WRITE(PRINTF,"(' THET3 THET4 DDIR XIS :',4E12.4)") DELTH4*RADE, DELTH3*RADE, DDIR*RADE, XIS
      WRITE(PRINTF,"(' IDP IDP1 IDM IDM1 :',4I5)") IDP, IDP1, IDM, IDM1
      WRITE(PRINTF,"(' WIDP WIDP1 WIDM WIDM1 :',4E12.4)") WIDP, WIDP1, WIDM, WIDM1
      WRITE (PRINTF,"(' ISP ISP1 ISM ISM1 :',4I5)") ISP, ISP1, ISM, ISM1
      WRITE (PRINTF,"(' WISP WISP1 WISM WISM1 :',4E12.4)") WISP, WISP1, WISM, WISM1
      WRITE(PRINTF,"(' ICLW ICHG :',2I5)") ISCLW, ISCHG
      WRITE (PRINTF,"(' AWG1 AWG2 AWG3 AWG4 :',4E12.4)") AWG1, AWG2, AWG3, AWG4
      WRITE (PRINTF,"(' AWG5 AWG6 AWG7 AWG8 :',4E12.4)") AWG5, AWG6, AWG7, AWG8
      WRITE (PRINTF,"(' S4MI S4MA D4MI D4MA :',4I6)") MSC4MI, MSC4MA, MDC4MI, MDC4MA
      WRITE (PRINTF,"(' ISLOW ISHG IDLOW IDHG :',4I5)") ISLOW, ISHGH, IDLOW,IDHGH
      WRITE(PRINTF,*)
   END IF

   RETURN
!     End of FAC4WW
end subroutine FAC4WW

!******************************************************************

SUBROUTINE RANGE4 (WWINT, IDDLOW, IDDTOP, IXCG, IYCG)
   USE swan_service_interfaces, ONLY: STRACE

!******************************************************************

   USE swan_physics_selection
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
!     40.00: Nico Booij
!     40.10: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.10, Mar. 00: Made modification for exact quadruplets
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose :
!
!     calculate the minimum and maximum counters in frequency and
!     directional space which fall with the calculation for the
!     nonlinear wave-wave interactions.
!
!  3. Method :  review for the counters :
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |    ^    |
!             ISLOW   1                     MSC  |  ISHGH
!                     ^                          |
!                     |                          |
!                    ISCLW                     ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!
!       The directional counters depend on the numerical method that
!       is used.
!
!  4. Parameters :
!
!     INTEGER
!     -------
!     IQUAD         Counter for 4 wave interactions
!     ISLOW,ISHGH   Minimum and maximum counter in frequency space
!     ISCLW,ISCHG   idem for discrete computations
!     IDLOW,IDHGH   Minimum and maximum counters in directional space
!     MSC,MDC       Range of the original arrays
!     ISM1,ISP1,
!     IDM1,IDP1     see subroutine FAC4WW
!     IDDLOW        minimum counter of the bin that is propagated
!                   within a sweep
!     IDDTOP        minimum counter of the bin that is propagated
!                   within a sweep
!
!     array:
!     ------
!     WWINT         counters for the nonlinear interactions
!
!     WWINT ( 1  = IDP      2  = IDP1     3  = IDM     4  = IDM1
!             5  = ISP      6  = ISP1     7  = ISM     8  = ISM1
!             9  = ISLOW    10 = ISHGH    11 = ISCLW   12 = ISCHG
!             13 = IDLOW    14 = IDHGH    15 = MSC4MI  16 = MSC4MA
!             17 = MDC4MI   18 = MDC4MA
!             19 = MSCMAX   20 = MDCMAX )
!
!  5. Subroutines used :
!
!     ---
!
!  6. Called by :
!
!     SOURCE
!
!  7. Common blocks used
!
!
!  9. Source code :
!
!     -----------------------------------------------------------------
!     Calculate :
!       In absence of a current there are always four sectors
!         equal 90 degrees within a sweep that are propagated
!         Extend the boundaries to calculate the source term
!       In presence of a current and if IDTOT .eq. MDC then calculate
!         boundaries for calculation of interaction using the
!         unfolded area.
!     ----------------------------------------------------------
!
!****************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER, INTENT(IN) :: IDDLOW, IDDTOP, IXCG, IYCG

   INTEGER     WWINT(*)

   IF (LTRACE) CALL STRACE (IENT,'RANGE4')

!     *** Range in directional domain ***

   IF ( IQUAD .LT. 3 .AND. IQUAD .GT. 0 ) THEN
!       *** counters based on bins which fall within a sweep ***
      WWINT(13) = IDDLOW - MAX( WWINT(4), WWINT(2) )
      WWINT(14) = IDDTOP + MAX( WWINT(4), WWINT(2) )
   ELSE
!       *** counters initially based on full circle ***
      WWINT(13) = 1   - MAX( WWINT(4), WWINT(2) )
      WWINT(14) = MDC + MAX( WWINT(4), WWINT(2) )
   END IF

!     *** error message ***

   IF (WWINT(9)  .LT. WWINT(15) .OR. WWINT(10) .GT. WWINT(16) .OR.&
   &WWINT(13) .LT. WWINT(17) .OR. WWINT(14) .GT. WWINT(18) ) THEN
      WRITE (PRINTF,"( ' ** Error : array bounds and maxima in subr RANGE4, ', ' point ', 2I5, /,' ISL,ISH : ',2I4, ' IDL,IDH : ',2I4, /,' SMI,SMA : ',2I4, ' DMI,DMA : ',2I4)") IXCG, IYCG,&
      &WWINT(9) ,WWINT(10) ,WWINT(13) ,WWINT(14),&
      &WWINT(15),WWINT(16) ,WWINT(17) ,WWINT(18)
      IF (ITEST.GE.50) WRITE (PRTEST, "(' MSC, MDC, IDDLOW, IDDTOP: ', 4I5)") MSC, MDC, IDDLOW, IDDTOP
   ENDIF

!     test output

   IF (TESTFL .AND. ITEST .GE. 60) THEN
      WRITE(PRTEST,"(' RANGE4: IDM1 IDP1 ISM1 ISP1 :',4I5)") WWINT(4), WWINT(2), WWINT(8), WWINT(6)
      WRITE(PRTEST,"(' RANGE4: ISCLW ISCHG IQUAD :',3I5)") WWINT(11), WWINT(12), IQUAD
      WRITE (PRTEST,"(' RANGE4: ISLOW ISHGH IDLOW IDHGH:',4I5)") WWINT(9), WWINT(10), WWINT(13), WWINT(14)
      WRITE (PRTEST,"(' RANGE4: MS4MI MS4MA MD4MI MD4MA:',4I5)") WWINT(15), WWINT(16), WWINT(17), WWINT(18)
      WRITE(PRINTF,*)
   END IF

   RETURN
!     End of RANGE4
end subroutine RANGE4

!********************************************************************

SUBROUTINE SWPRE4W (XIS   ,SNLC1 ,&
&DAL1  ,DAL2  ,DAL3  ,SPCSIG,&
&WWINT ,WWAWG ,WWSWG, SNL4  )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!********************************************************************

   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_diagnostics_level
   TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4

!  2. Purpose
!
!     Computes the interaction coefficients of all quadruplets of the
!     MDIA (IQUAD=4) once per computation and caches them in module
!     M_SNL4, so that the source term evaluation does not have to call
!     FAC4WW again for every grid point and quadruplet. The spectral
!     ranges MSC4MI/MSC4MA/MDC4MI/MDC4MA are widened to cover all
!     quadruplets, so work arrays allocated with these bounds fit each
!     of them, and AF11 is filled for that widest range (its values do
!     not depend on the quadruplet)
!
!  4. Argument variables (see FAC4WW)

   INTEGER WWINT(24)
   REAL    XIS, SNLC1, DAL1, DAL2, DAL3
   REAL    SPCSIG(MSC)
   REAL    WWAWG(8), WWSWG(8)

!  6. Local variables

   INTEGER IDIA, IS, ISTAT, MI4S, MA4S, MI4D, MA4D, MSCMAX, MDCMAX
   LOGICAL REBUILD
   REAL    FREQ

!     Cache identity. FAC4WW depends on the spectral grid, directional
!     resolution, gravity and the MDIA lambda values. Preserve exact inp
!     copies so a following non-stationary time step can reuse the cache
!     while a changed COMPUTE configuration rebuilds it automatically.
   INTEGER, SAVE :: MDISAV = -1, MSCSAV = -1, MDCSAV = -1
   REAL, SAVE :: DDIRSAV = 0., GRAVSAV = 0., PISAV = 0.
   REAL, SAVE :: XISSAV = 0., SNLSAV = 0.
   REAL, ALLOCATABLE, SAVE :: SIGSAV(:), LAMSAV(:)
   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT,'SWPRE4W')

   REBUILD = .TRUE.
   IF ( ALLOCATED(SNL4%cached_indices) .AND.&
   &ALLOCATED(SNL4%cached_angular_weights) .AND.&
   &ALLOCATED(SNL4%cached_spectral_weights) .AND.&
   &ALLOCATED(SNL4%cached_dal1) .AND. ALLOCATED(SNL4%cached_dal2) .AND.&
   &ALLOCATED(SNL4%cached_dal3) .AND.&
   &ALLOCATED(SNL4%frequency_power_11) .AND. ALLOCATED(SIGSAV) .AND.&
   &ALLOCATED(LAMSAV) ) THEN
      IF ( SIZE(SNL4%cached_indices,1).EQ.24 .AND.&
      &SIZE(SNL4%cached_indices,2).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SNL4%cached_angular_weights,1).EQ.8 .AND.&
      &SIZE(SNL4%cached_angular_weights,2).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SNL4%cached_spectral_weights,1).EQ.8 .AND.&
      &SIZE(SNL4%cached_spectral_weights,2).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SNL4%cached_dal1).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SNL4%cached_dal2).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SNL4%cached_dal3).EQ.SNL4%quadruplet_count .AND.&
      &SIZE(SIGSAV).EQ.MSC .AND.&
      &SIZE(LAMSAV).EQ.SNL4%quadruplet_count ) THEN
         REBUILD = .FALSE.
         IF ( MDISAV.NE.SNL4%quadruplet_count .OR.&
         &MSCSAV.NE.MSC .OR. MDCSAV.NE.MDC )&
         &REBUILD = .TRUE.
         IF ( DDIRSAV.NE.DDIR .OR. GRAVSAV.NE.GRAV .OR. PISAV.NE.PI )&
         &REBUILD = .TRUE.
         IF ( .NOT.REBUILD ) THEN
            IF ( ANY(SIGSAV.NE.SPCSIG) .OR.&
            &ANY(LAMSAV.NE.SNL4%lambda) )&
            &REBUILD = .TRUE.
         ENDIF
         IF ( .NOT.REBUILD ) THEN
            IF ( LBOUND(SNL4%frequency_power_11,1).NE.&
            &SNL4%cached_indices(15,1) .OR.&
            &UBOUND(SNL4%frequency_power_11,1).NE.&
            &SNL4%cached_indices(16,1) ) REBUILD = .TRUE.
         ENDIF
      ENDIF
   ENDIF

   IF ( .NOT.REBUILD ) THEN
      XIS   = XISSAV
      SNLC1 = SNLSAV
      WWINT(1:24) = SNL4%cached_indices(1:24,SNL4%quadruplet_count)
      WWAWG(1:8) = SNL4%cached_angular_weights(1:8,SNL4%quadruplet_count)
      WWSWG(1:8) = SNL4%cached_spectral_weights(1:8,SNL4%quadruplet_count)
      DAL1 = SNL4%cached_dal1(SNL4%quadruplet_count)
      DAL2 = SNL4%cached_dal2(SNL4%quadruplet_count)
      DAL3 = SNL4%cached_dal3(SNL4%quadruplet_count)
      PQUAD(1) = SNL4%lambda(SNL4%quadruplet_count)
      MSC4MI = SNL4%cached_indices(15,1)
      MSC4MA = SNL4%cached_indices(16,1)
      MDC4MI = SNL4%cached_indices(17,1)
      MDC4MA = SNL4%cached_indices(18,1)
      MSCMAX = SNL4%cached_indices(19,1)
      MDCMAX = SNL4%cached_indices(20,1)
      RETURN
   ENDIF

   IF (ALLOCATED(SNL4%cached_indices)) DEALLOCATE(SNL4%cached_indices)
   IF (ALLOCATED(SNL4%cached_angular_weights))&
   &DEALLOCATE(SNL4%cached_angular_weights)
   IF (ALLOCATED(SNL4%cached_spectral_weights))&
   &DEALLOCATE(SNL4%cached_spectral_weights)
   IF (ALLOCATED(SNL4%cached_dal1)) DEALLOCATE(SNL4%cached_dal1)
   IF (ALLOCATED(SNL4%cached_dal2)) DEALLOCATE(SNL4%cached_dal2)
   IF (ALLOCATED(SNL4%cached_dal3)) DEALLOCATE(SNL4%cached_dal3)
   ISTAT = 0
   ALLOCATE (SNL4%cached_indices(24,SNL4%quadruplet_count),&
   &SNL4%cached_angular_weights(8,SNL4%quadruplet_count),&
   &SNL4%cached_spectral_weights(8,SNL4%quadruplet_count),&
   &SNL4%cached_dal1(SNL4%quadruplet_count),&
   &SNL4%cached_dal2(SNL4%quadruplet_count),&
   &SNL4%cached_dal3(SNL4%quadruplet_count),&
   &STAT=ISTAT)
   IF ( ISTAT.NE.0 ) THEN
      CALL MSGERR (4,&
      &'Allocation problem in SWPRE4W: MDIA cache arrays')
      RETURN
   ENDIF

   DO IDIA = 1, SNL4%quadruplet_count
      PQUAD(1) = SNL4%lambda(IDIA)
      CALL FAC4WW (XIS   ,SNLC1 ,&
      &DAL1  ,DAL2  ,DAL3  ,SPCSIG,&
      &WWINT ,WWAWG ,WWSWG, SNL4 )
      SNL4%cached_indices(1:24,IDIA) = WWINT(1:24)
      SNL4%cached_angular_weights(1:8,IDIA) = WWAWG(1:8)
      SNL4%cached_spectral_weights(1:8,IDIA) = WWSWG(1:8)
      SNL4%cached_dal1(IDIA) = DAL1
      SNL4%cached_dal2(IDIA) = DAL2
      SNL4%cached_dal3(IDIA) = DAL3
      IF ( IDIA.EQ.1 ) THEN
         MI4S = MSC4MI
         MA4S = MSC4MA
         MI4D = MDC4MI
         MA4D = MDC4MA
      ELSE
         MI4S = MIN( MI4S, MSC4MI )
         MA4S = MAX( MA4S, MSC4MA )
         MI4D = MIN( MI4D, MDC4MI )
         MA4D = MAX( MA4D, MDC4MA )
      ENDIF
   ENDDO

!     *** widest spectral range over all quadruplets ***

   MSC4MI = MI4S
   MSC4MA = MA4S
   MDC4MI = MI4D
   MDC4MA = MA4D
   MSCMAX = MSC4MA - MSC4MI + 1
   MDCMAX = MDC4MA - MDC4MI + 1

   DO IDIA = 1, SNL4%quadruplet_count
      SNL4%cached_indices(15,IDIA) = MSC4MI
      SNL4%cached_indices(16,IDIA) = MSC4MA
      SNL4%cached_indices(17,IDIA) = MDC4MI
      SNL4%cached_indices(18,IDIA) = MDC4MA
      SNL4%cached_indices(19,IDIA) = MSCMAX
      SNL4%cached_indices(20,IDIA) = MDCMAX
   ENDDO

!     *** refill scaling array (f**11) for the widest range; ***
!     *** the values do not depend on the quadruplet         ***

   IF (ALLOCATED(SNL4%frequency_power_11))&
   &DEALLOCATE(SNL4%frequency_power_11)
   ALLOCATE (SNL4%frequency_power_11(MSC4MI:MSC4MA))
   DO IS = 1, MSC
      SNL4%frequency_power_11(IS) = ( SPCSIG(IS) / ( 2. * PI ) )**11
   ENDDO
   FREQ = SPCSIG(MSC) / ( 2. * PI )
   DO IS = MSC+1, MSC4MA
      FREQ = FREQ * XIS
      SNL4%frequency_power_11(IS) = FREQ**11
   ENDDO
   FREQ = SPCSIG(1) / ( 2. * PI )
   DO IS = 0, MSC4MI, -1
      FREQ = FREQ / XIS
      SNL4%frequency_power_11(IS) = FREQ**11
   ENDDO

!     *** remember the exact cache identity and reusable scalar outputs

   IF (ALLOCATED(SIGSAV)) DEALLOCATE(SIGSAV)
   IF (ALLOCATED(LAMSAV)) DEALLOCATE(LAMSAV)
   ISTAT = 0
   ALLOCATE (SIGSAV(MSC), LAMSAV(SNL4%quadruplet_count), STAT=ISTAT)
   IF ( ISTAT.NE.0 ) THEN
      IF (ALLOCATED(SIGSAV)) DEALLOCATE(SIGSAV)
      IF (ALLOCATED(LAMSAV)) DEALLOCATE(LAMSAV)
      CALL MSGERR (4,&
      &'Allocation problem in SWPRE4W: cache identity arrays')
      RETURN
   ENDIF
   SIGSAV = SPCSIG
   LAMSAV = SNL4%lambda
   MDISAV = SNL4%quadruplet_count
   MSCSAV = MSC
   MDCSAV = MDC
   DDIRSAV = DDIR
   GRAVSAV = GRAV
   PISAV   = PI
   XISSAV  = XIS
   SNLSAV  = SNLC1

   RETURN
!     End of SWPRE4W
end subroutine SWPRE4W

!********************************************************************

SUBROUTINE SWSNL1 (WWINT   ,WWAWG   ,WWSWG   ,&
&IDCMIN  ,IDCMAX  ,UE      ,SA1     ,&
&SA2     ,DA1C    ,DA1P    ,DA1M    ,DA2C    ,&
&DA2P    ,DA2M    ,SPCSIG  ,SNLC1   ,KMESPC  ,&
&FACHFR  ,ISSTOP  ,DAL1    ,DAL2    ,DAL3    ,&
&SFNL    ,DSNL    ,DEP2    ,AC2     ,IMATDA  ,&
&IMATRA  ,PLNL4S  ,PLNL4D  ,&
&IDDLOW  ,IDDTOP  ,REDC0   ,REDC1, AF11 ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!********************************************************************

   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   INTEGER, INTENT(IN) :: IGP
   REAL, INTENT(IN) :: AF11(MSC4MI:MSC4MA)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!     40.13: Nico Booij
!     40.17: IJsbrand Haagsma
!     40.23: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.17, Dec. 01: Implentation of Multiple DIA
!     40.23, Aug. 02: some corrections
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: store quadruplets for output purposes
!
!  2. Purpose
!
!     Calculate non-linear interaction using the discrete interaction
!     approximation (Hasselmann and Hasselmann 1985; WAMDI group 1988),
!     including the diagonal term for the implicit integration.
!
!     The interactions are calculated for all bin's that fall
!     within a sweep. No additional auxiliary array is required (see
!     SWSNL3)
!
!  3. Method
!
!     Discrete interaction approximation.
!
!     Since the domain in directional domain is by definition not
!     periodic, the spectral space can not beforehand
!     folded to the side angles. This can only be done if the
!     full circle has to be calculated
!
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |    ^    |
!             ISLOW   1                     MSC  |    ISHGH
!                     ^                          |
!                     |                          |
!                    ISCLW                     ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!                            1 : Extra tail added beyond MSC
!                            2 : Spectrum copied outside ID range
!                            3 : Empty bins at low frequencies
!
!     ISLOW =  1  + ISM1
!     ISHGH = MSC + ISP1 - ISM1
!     ISCLW =  1
!     ISCHG = MSC - ISM1
!     IDLOW =  IDDLOW - MAX(IDM1,IDP1)
!     IDHGH =  IDDTOP + MAX(IDM1,IDP1)
!
!     For the meaning of the counters on the right hand side of the
!     above equations see section 4.
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!     Data in PARAMETER statements :
!     ----------------------------------------------------------------
!       DAL1    Real  LAMBDA dependend weight factors (see FAC4WW)
!       DAL2    Real
!       DAL3    Real
!       ITHP, ITHP1, ITHM, ITHM1, IFRP, IFRP1, IFRM, IFRM1
!               Int.  Counters of interpolation point relative to
!                     central bin, see figure below (set in FAC4WW).
!       NFRLOW, NFRHGH, NFRCHG, NTHLOW, NTHHGH
!               Int.  Range of calculations, see section 2.
!       AF11    R.A.  Scaling array (Freq**11).
!       AWGn    Real  Interpolation weights, see numbers in fig.
!       SWGn    Real  Id. squared.
!       UE      R.A.  "Unfolded" spectrum.
!       SA1     R.A.  Interaction constribution of first and second
!       SA2     R.A.    quadr. respectively (unfolded space).
!       DA1C, DA1P, DA1M, DA2C, DA2P, DA2M
!               R.A.  Idem for diagonal matrix.
!       PERCIR        full circle or sector
!     ----------------------------------------------------------------
!
!       Relative offsets of interpolation points around central bin
!       "#" and corresponding numbers of AWGn :
!
!               ISM1  ISM
!                5        7    T |
!          IDM1   +------+     H +
!                 |      |     E |      ISP      ISP1
!                 |   \  |     T |       3           1
!           IDM   +------+     A +        +---------+  IDP1
!                6       \8      |        |         |
!                                |        |  /      |
!                           \    +        +---------+  IDP
!                                |      /4           2
!                              \ |  /
!          -+-----+------+-------#--------+---------+----------+
!                                |           FREQ.
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
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!       Initialisations.
!       Calculate proportionality constant.
!       Prepare auxiliary spectrum.
!       Calculate interactions :
!       -----------------------------------------
!         Energy at interacting bins
!         Contribution to interactions
!         Fold interactions to side angles
!       -----------------------------------------
!       Put source term together
!     -------------------------------------------
!
! 13. Source text
!
!*************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS, ID, ID0, I, J, IDDUM, IIID, ISLOW, ISSTOP, &
   &ISHGH  ,IDLOW  ,ISP    ,ISP1   ,IDP    ,IDP1   ,&
   &ISM    ,ISM1   ,IDHGH  ,IDM    ,IDM1   ,ISCLW  ,&
   &ISCHG  ,IDDLOW ,IDDTOP, IDCLOW, IDCHGH

   REAL      X      ,X2     ,CONS   ,FACTOR ,SNLCS1 ,SNLCS2 ,SNLCS3,&
   &E00    ,EP1    ,EM1    ,EP2    ,EM2    ,SA1A   ,SA1B  ,&
   &SA2A   ,SA2B   ,KMESPC ,FACHFR ,AWG1   ,AWG2   ,AWG3  ,&
   &AWG4   ,AWG5   ,AWG6   ,AWG7   ,AWG8   ,DAL1   ,DAL2  ,&
   &DAL3   ,SNLC1  ,SWG1   ,SWG2   ,SWG3   ,SWG4   ,SWG5  ,&
   &SWG6   ,SWG7   ,SWG8, JACOBI, SIGPI, PI3

   REAL      AC2(MDC,MSC,MCGRD)                    ,&
   &DEP2(MCGRD)                           ,&
   &UE(MSC4MI:MSC4MA , MDC4MI:MDC4MA )    ,&
   &SA1(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
   &SA2(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
   &DA1C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DA1P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DA1M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DA2C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DA2P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DA2M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &SFNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &DSNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
   &IMATDA(MDC,MSC)                       ,&
   &IMATRA(MDC,MSC)                       ,&
   &PLNL4S(MDC,MSC,NPTST)                 ,&
   &PLNL4D(MDC,MSC,NPTST)                 ,&
   &WWAWG(*)                              ,&
   &WWSWG(*)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)

   INTEGER   IDCMIN(MSC)        ,&
   &IDCMAX(MSC)        ,&
   &WWINT(*)

   LOGICAL   PERCIR

   LOGICAL   LTSTFL

   IF (LTRACE) CALL STRACE (IENT,'SWSNL1')

!     evaluate the test-output condition once; the per-bin loop below
!     only tests this single flag
   LTSTFL = ITEST.GE.100 .AND. TESTFL

   IDP    = WWINT(1)
   IDP1   = WWINT(2)
   IDM    = WWINT(3)
   IDM1   = WWINT(4)
   ISP    = WWINT(5)
   ISP1   = WWINT(6)
   ISM    = WWINT(7)
   ISM1   = WWINT(8)
   ISLOW  = WWINT(9)
   ISHGH  = WWINT(10)
   ISCLW  = WWINT(11)
   ISCHG  = WWINT(12)
   IDLOW  = WWINT(13)
   IDHGH  = WWINT(14)

   AWG1 = WWAWG(1)
   AWG2 = WWAWG(2)
   AWG3 = WWAWG(3)
   AWG4 = WWAWG(4)
   AWG5 = WWAWG(5)
   AWG6 = WWAWG(6)
   AWG7 = WWAWG(7)
   AWG8 = WWAWG(8)

   SWG1 = WWSWG(1)
   SWG2 = WWSWG(2)
   SWG3 = WWSWG(3)
   SWG4 = WWSWG(4)
   SWG5 = WWSWG(5)
   SWG6 = WWSWG(6)
   SWG7 = WWSWG(7)
   SWG8 = WWSWG(8)

!     *** Calculate factor R(X) to calculate the NL wave-wave ***
!     *** interaction for shallow water                       ***
!     *** SNLC1 = 1/GRAV**4                                   ***

   SNLCS1 = PQUAD(3)
   SNLCS2 = PQUAD(4)
   SNLCS3 = PQUAD(5)
   X      = MAX ( 0.75 * DEP2(IGP) * KMESPC , 0.5 )
   X2     = MAX ( -1.E15, SNLCS3*X)
   CONS   = SNLC1 * ( 1. + SNLCS1/X * (1.-SNLCS2*X) * EXP(X2))
   JACOBI = 2. * PI

!     *** check whether the spectral domain is periodic in ***
!     *** directional space and if so, modify boundaries   ***

   PERCIR = .FALSE.
   IF ( IDDLOW .EQ. 1 .AND. IDDTOP .EQ. MDC ) THEN
!       *** periodic in theta -> spectrum can be folded    ***
!       *** (can only be present in presence of a current) ***
      IDCLOW = 1
      IDCHGH = MDC
      IIID   = 0
      PERCIR = .TRUE.
   ELSE
!       *** different sectors per sweep -> extend range with IIID ***
      IIID   = MAX ( IDM1 , IDP1 )
      IDCLOW = IDLOW
      IDCHGH = IDHGH
   ENDIF

!     Only low-frequency rows are read without first being assigned.
!     SFNL and DSNL are assigned before use; the interaction and diagonal
!     arrays are assigned over ISCLW:ISCHG below.

   DO IDDUM = IDLOW - IIID, IDHGH + IIID
      DO IS = MSC4MI, 0
         UE(IS,IDDUM) = 0.
      ENDDO
   ENDDO
   DO ID = IDLOW, IDHGH
      DO IS = MSC4MI, 0
         SA1(IS,ID)  = 0.
         SA2(IS,ID)  = 0.
         DA1C(IS,ID) = 0.
         DA1P(IS,ID) = 0.
         DA1M(IS,ID) = 0.
         DA2C(IS,ID) = 0.
         DA2P(IS,ID) = 0.
         DA2M(IS,ID) = 0.
      ENDDO
   ENDDO

!     *** Prepare auxiliary spectrum               ***
!     *** set action original spectrum in array UE ***

   DO IDDUM = IDLOW - IIID, IDHGH + IIID
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      DO IS = 1, MSC
         UE(IS,IDDUM) = AC2(ID,IS,IGP) * SPCSIG(IS) * JACOBI
      ENDDO
   ENDDO

!     *** set values in area 2 for IS > MSC+1  ***

   DO ID = IDLOW - IIID , IDHGH + IIID
      DO IS = MSC+1, ISHGH
         UE (IS,ID) = UE(IS-1,ID) * FACHFR
      ENDDO
   ENDDO

!     *** Calculate interactions      ***
!     *** Energy at interacting bins  ***

   DO IS = ISCLW, ISCHG
      DO ID = IDCLOW, IDCHGH
         E00    =        UE(IS      ,ID      )
         EP1    = AWG1 * UE(IS+ISP1,ID+IDP1) +&
         &AWG2 * UE(IS+ISP1,ID+IDP ) +&
         &AWG3 * UE(IS+ISP ,ID+IDP1) +&
         &AWG4 * UE(IS+ISP ,ID+IDP )
         EM1    = AWG5 * UE(IS+ISM1,ID-IDM1) +&
         &AWG6 * UE(IS+ISM1,ID-IDM ) +&
         &AWG7 * UE(IS+ISM ,ID-IDM1) +&
         &AWG8 * UE(IS+ISM ,ID-IDM )

         EP2    = AWG1 * UE(IS+ISP1,ID-IDP1) +&
         &AWG2 * UE(IS+ISP1,ID-IDP ) +&
         &AWG3 * UE(IS+ISP ,ID-IDP1) +&
         &AWG4 * UE(IS+ISP ,ID-IDP )
         EM2    = AWG5 * UE(IS+ISM1,ID+IDM1) +&
         &AWG6 * UE(IS+ISM1,ID+IDM ) +&
         &AWG7 * UE(IS+ISM ,ID+IDM1) +&
         &AWG8 * UE(IS+ISM ,ID+IDM )

!         *** Contribution to interactions                          ***
!         *** CONS is the shallow water factor for the NL interact. ***

         FACTOR = CONS * AF11(IS) * E00

         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 ) * PQUAD(2)
         SA1B   = SA1A - EP1*EM1*DAL3 * PQUAD(2)
         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 ) * PQUAD(2)
         SA2B   = SA2A - EP2*EM2*DAL3 * PQUAD(2)

         SA1 (IS,ID) = FACTOR * SA1B
         SA2 (IS,ID) = FACTOR * SA2B

         IF (LTSTFL) THEN
            WRITE(PRINTF,"(' E00 EP1 EM1 EP2 EM2 :',5E11.4)") E00,EP1,EM1,EP2,EM2
            WRITE(PRINTF,"(' SA1A SA1B SA2A SA2B :',4E11.4)") SA1A,SA1B,SA2A,SA2B
            WRITE(PRINTF,"(' IS ID SA1() SA2() :',2I4,2E12.4)") IS,ID,SA1(IS,ID),SA2(IS,ID)
            WRITE(PRINTF,"(' FACTOR : ',E12.4)") FACTOR
         END IF

         DA1C(IS,ID) = CONS * AF11(IS) * ( SA1A + SA1B )
         DA1P(IS,ID) = FACTOR * ( DAL1*E00 - DAL3*EM1 ) * PQUAD(2)
         DA1M(IS,ID) = FACTOR * ( DAL2*E00 - DAL3*EP1 ) * PQUAD(2)

         DA2C(IS,ID) = CONS * AF11(IS) * ( SA2A + SA2B )
         DA2P(IS,ID) = FACTOR * ( DAL1*E00 - DAL3*EM2 ) * PQUAD(2)
         DA2M(IS,ID) = FACTOR * ( DAL2*E00 - DAL3*EP2 ) * PQUAD(2)
      ENDDO
   ENDDO

!     *** Fold interactions to side angles if spectral domain ***
!     *** is periodic in directional space                    ***

   IF ( PERCIR ) THEN
      DO ID = 1, IDHGH - MDC
         ID0   = 1 - ID
         DO IS = ISCLW, ISCHG
            SA1 (IS,MDC+ID) = SA1 (IS,  ID   )
            SA2 (IS,MDC+ID) = SA2 (IS,  ID   )
            DA1C(IS,MDC+ID) = DA1C(IS,  ID   )
            DA1P(IS,MDC+ID) = DA1P(IS,  ID   )
            DA1M(IS,MDC+ID) = DA1M(IS,  ID   )
            DA2C(IS,MDC+ID) = DA2C(IS,  ID   )
            DA2P(IS,MDC+ID) = DA2P(IS,  ID   )
            DA2M(IS,MDC+ID) = DA2M(IS,  ID   )

            SA1 (IS,  ID0 ) = SA1 (IS, MDC+ID0)
            SA2 (IS,  ID0 ) = SA2 (IS, MDC+ID0)
            DA1C(IS,  ID0 ) = DA1C(IS, MDC+ID0)
            DA1P(IS,  ID0 ) = DA1P(IS, MDC+ID0)
            DA1M(IS,  ID0 ) = DA1M(IS, MDC+ID0)
            DA2C(IS,  ID0 ) = DA2C(IS, MDC+ID0)
            DA2P(IS,  ID0 ) = DA2P(IS, MDC+ID0)
            DA2M(IS,  ID0 ) = DA2M(IS, MDC+ID0)
         ENDDO
      ENDDO
   ENDIF

!     *** Put source term together (To save space I=IS and J=ID ***
!     *** is used)                                              ***

   PI3   = (2. * PI)**3
   DO I = 1, ISSTOP
      SIGPI = SPCSIG(I) * JACOBI
      DO J = IDCMIN(I), IDCMAX(I)
         ID = MOD ( J - 1 + MDC , MDC ) + 1
         SFNL(I,ID) =   - 2. * ( SA1(I,J) + SA2(I,J) )&
         &+ AWG1 * ( SA1(I-ISP1,J-IDP1) + SA2(I-ISP1,J+IDP1) )&
         &+ AWG2 * ( SA1(I-ISP1,J-IDP ) + SA2(I-ISP1,J+IDP ) )&
         &+ AWG3 * ( SA1(I-ISP ,J-IDP1) + SA2(I-ISP ,J+IDP1) )&
         &+ AWG4 * ( SA1(I-ISP ,J-IDP ) + SA2(I-ISP ,J+IDP ) )&
         &+ AWG5 * ( SA1(I-ISM1,J+IDM1) + SA2(I-ISM1,J-IDM1) )&
         &+ AWG6 * ( SA1(I-ISM1,J+IDM ) + SA2(I-ISM1,J-IDM ) )&
         &+ AWG7 * ( SA1(I-ISM ,J+IDM1) + SA2(I-ISM ,J-IDM1) )&
         &+ AWG8 * ( SA1(I-ISM ,J+IDM ) + SA2(I-ISM ,J-IDM ) )

         DSNL(I,ID) =   - 2. * ( DA1C(I,J) + DA2C(I,J) )&
         &+ SWG1 * ( DA1P(I-ISP1,J-IDP1) + DA2P(I-ISP1,J+IDP1) )&
         &+ SWG2 * ( DA1P(I-ISP1,J-IDP ) + DA2P(I-ISP1,J+IDP ) )&
         &+ SWG3 * ( DA1P(I-ISP ,J-IDP1) + DA2P(I-ISP ,J+IDP1) )&
         &+ SWG4 * ( DA1P(I-ISP ,J-IDP ) + DA2P(I-ISP ,J+IDP ) )&
         &+ SWG5 * ( DA1M(I-ISM1,J+IDM1) + DA2M(I-ISM1,J-IDM1) )&
         &+ SWG6 * ( DA1M(I-ISM1,J+IDM ) + DA2M(I-ISM1,J-IDM ) )&
         &+ SWG7 * ( DA1M(I-ISM ,J+IDM1) + DA2M(I-ISM ,J-IDM1) )&
         &+ SWG8 * ( DA1M(I-ISM ,J+IDM ) + DA2M(I-ISM ,J-IDM ) )

!         *** store results in IMATDA and IMATRA ***

         IF(TESTFL) THEN
            PLNL4S(ID,I,IPTST) = SFNL(I,ID) / SIGPI
            PLNL4D(ID,I,IPTST) = DSNL(I,ID) / PI3
         END IF
         REDC0(ID,I,1) = REDC0(ID,I,1) + SFNL(I,ID) / SIGPI
         REDC1(ID,I,1) = REDC1(ID,I,1) + DSNL(I,ID) / PI3

         IMATRA(ID,I) = IMATRA(ID,I) + SFNL(I,ID) / SIGPI
         IMATDA(ID,I) = IMATDA(ID,I) - DSNL(I,ID) / PI3

         IF(ITEST.GE.90 .AND. TESTFL) THEN
            WRITE(PRINTF,"(' IS ID SFNL DSNL SPCSIG:',2I4,3E12.4)") I,J,SFNL(I,ID),DSNL(I,ID),&
            &SPCSIG(I)
         END IF

      ENDDO
   ENDDO

!     *** test output ***

   IF (ITEST .GE. 50 .AND. TESTFL) THEN
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' SWSNL1 subroutine '
      WRITE(PRINTF,"(' IDP IDP1 IDM IDM1 :',4I5)") IDP, IDP1, IDM, IDM1
      WRITE (PRINTF,"(' ISP ISP1 ISM ISM1 :',4I5)") ISP, ISP1, ISM, ISM1
      WRITE (PRINTF,"(' ISLOW ISHGH IDLOW IDHG:',4I5)") ISLOW, ISHGH, IDLOW,IDHGH
      WRITE(PRINTF,"(' ICLW ICHG IDDLOW IDDTO:',2I5)") ISCLW, ISCHG, IDDLOW, IDDTOP
      WRITE (PRINTF,"(' AWG1 AWG2 AWG3 AWG4 :',4E12.4)") AWG1, AWG2, AWG3, AWG4
      WRITE (PRINTF,"(' AWG5 AWG6 AWG7 AWG8 :',4E12.4)") AWG5, AWG6, AWG7, AWG8
      WRITE (PRINTF,"(' S4MI S4MA D4MI D4MA :',4I6)") MSC4MI, MSC4MA, MDC4MI, MDC4MA
      WRITE(PRINTF,"(' SNLC1 X X2 CONS :',4E12.4)") SNLC1,X,X2,CONS
      WRITE(PRINTF,"(' DEPTH KMESPC FACHFR PI:',4E12.4)") DEP2(IGP),KMESPC, FACHFR, PI
      WRITE(PRINTF,"(' JACOBI :',E12.4)") JACOBI
      WRITE(PRINTF,*)
   END IF

   RETURN
!     End of the subroutine SWSNL1
end subroutine SWSNL1

!*******************************************************************

SUBROUTINE SWSNL2 (IDDLOW  ,IDDTOP  ,WWINT   ,&
&WWAWG   ,UE      ,SA1     ,ISSTOP  ,&
&SA2     ,SPCSIG  ,SNLC1   ,DAL1    ,DAL2    ,&
&DAL3    ,SFNL    ,DEP2    ,AC2     ,KMESPC  ,&
&REDC0   ,REDC1   ,IMATDA  ,IMATRA  ,&
&FACHFR  ,PLNL4S  ,         IDCMIN  ,IDCMAX, AF11 ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!*******************************************************************

   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   INTEGER, INTENT(IN) :: IGP
   REAL, INTENT(IN) :: AF11(MSC4MI:MSC4MA)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!     40.13: Nico Booij
!     40.17: IJsbrand Haagsma
!     40.23: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.23, Aug. 02: rhs and main diagonal adjusted according to Patankar-rules
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: store quadruplets for output purposes
!
!  2. Purpose
!
!     Calculate non-linear interaction using the discrete interaction
!     approximation (Hasselmann and Hasselmann 1985; WAMDI group 1988)
!
!  3. Method
!
!     Discrete interaction approximation.
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |     ^   |
!              ISLOW  1                    MSC    |   ISHGH
!                     |                           |
!                   ISCLW                        ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!                            1 : Extra tail added beyond MSC
!                            2 : Spectrum copied outside ID range
!                            3 : Empty bins at low frequencies
!
!     ISLOW =  1  + ISM1
!     ISHGH = MSC + ISP1 - ISM1
!     ISCLW =  1
!     ISCHG = MSC - ISM1
!     IDLOW = IDDLOW - MAX(IDM1,IDP1)
!     IDHGH = IDDTOP + MAX(IDM1,IDP1)
!
!       Relative offsets of interpolation points around central bin
!       "#" and corresponding numbers of AWGn :
!
!               ISM1  ISM
!                5        7    T |
!          IDM1   +------+     H +
!                 |      |     E |      ISP      ISP1
!                 |   \  |     T |       3           1
!           IDM   +------+     A +        +---------+  IDP1
!                6       \8      |        |         |
!                                |        |  /      |
!                           \    +        +---------+  IDP
!                                |      /4           2
!                              \ |  /
!          -+-----+------+-------#--------+---------+----------+
!                                |           FREQ.
!
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCSIG(MSC)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!       Initialisations.
!       Calculate proportionality constant.
!       Prepare auxiliary spectrum.
!       Calculate (unfolded) interactions :
!       -----------------------------------------
!         Energy at interacting bins
!         Contribution to interactions
!         Fold interactions to side angles
!       -----------------------------------------
!       Put source term together
!     -------------------------------------------
!
! 13. Source text
!
!*******************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS, ID, ID0, I, J, IDDUM, IIID, ISLOW, ISHGH, &
   &ISSTOP ,ISP    ,ISP1   ,IDP    ,IDP1   ,ISM    ,ISM1   ,&
   &IDM    ,IDM1   ,ISCLW  ,ISCHG  ,&
   &IDLOW  ,IDHGH  ,IDDLOW ,IDDTOP ,IDCLOW ,IDCHGH

   REAL      X      ,X2     ,CONS   ,FACTOR ,SNLCS1 ,SNLCS2 ,SNLCS3 ,&
   &E00    ,EP1    ,EM1    ,EP2    ,EM2    ,SA1A   ,SA1B   ,&
   &SA2A   ,SA2B   ,KMESPC ,FACHFR ,AWG1   ,AWG2   ,AWG3   ,&
   &AWG4   ,AWG5   ,AWG6   ,AWG7   ,AWG8   ,DAL1   ,DAL2   ,&
   &DAL3, SNLC1, JACOBI, SIGPI

   REAL      AC2(MDC,MSC,MCGRD)                    ,&
   &DEP2(MCGRD)                           ,&
   &UE(MSC4MI:MSC4MA , MDC4MI:MDC4MA )    ,&
   &SA1(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
   &SA2(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
   &SFNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA)   ,&
   &IMATRA(MDC,MSC)                       ,&
   &IMATDA(MDC,MSC)                       ,&
   &PLNL4S(MDC,MSC,NPTST)                 ,&
   &WWAWG(*)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)

   INTEGER   WWINT(*)         ,&
   &IDCMIN(MSC)      ,&
   &IDCMAX(MSC)

   LOGICAL   PERCIR

   LOGICAL   LTSTFL

   IF (LTRACE) CALL STRACE (IENT,'SWSNL2')

!     evaluate the test-output condition once; the per-bin loop below
!     only tests this single flag
   LTSTFL = ITEST.GE.100 .AND. TESTFL

   IDP    = WWINT(1)
   IDP1   = WWINT(2)
   IDM    = WWINT(3)
   IDM1   = WWINT(4)
   ISP    = WWINT(5)
   ISP1   = WWINT(6)
   ISM    = WWINT(7)
   ISM1   = WWINT(8)
   ISLOW  = WWINT(9)
   ISHGH  = WWINT(10)
   ISCLW  = WWINT(11)
   ISCHG  = WWINT(12)
   IDLOW  = WWINT(13)
   IDHGH  = WWINT(14)

   AWG1 = WWAWG(1)
   AWG2 = WWAWG(2)
   AWG3 = WWAWG(3)
   AWG4 = WWAWG(4)
   AWG5 = WWAWG(5)
   AWG6 = WWAWG(6)
   AWG7 = WWAWG(7)
   AWG8 = WWAWG(8)

!     *** Calculate prop. constant.                           ***
!     *** Calculate factor R(X) to calculate the NL wave-wave ***
!     *** interaction for shallow water                       ***
!     *** SNLC1 = 1/GRAV**4                                   ***

   SNLCS1 = PQUAD(3)
   SNLCS2 = PQUAD(4)
   SNLCS3 = PQUAD(5)
   X      = MAX ( 0.75 * DEP2(IGP) * KMESPC , 0.5 )
   X2     = MAX ( -1.E15, SNLCS3*X)
   CONS   = SNLC1 * ( 1. + SNLCS1/X * (1.-SNLCS2*X) * EXP(X2))
   JACOBI = 2. * PI

!     *** check whether the spectral domain is periodic in ***
!     *** direction space and if so modify boundaries      ***

   PERCIR = .FALSE.
   IF ( IDDLOW .EQ. 1 .AND. IDDTOP .EQ. MDC ) THEN
!       *** periodic in theta -> spectrum can be folded  ***
!       *** (can only occur in presence of a current)    ***
      IDCLOW = 1
      IDCHGH = MDC
      IIID   = 0
      PERCIR = .TRUE.
   ELSE
!       *** different sectors per sweep -> extend range with IIID ***
      IIID   = MAX ( IDM1 , IDP1 )
      IDCLOW = IDLOW
      IDCHGH = IDHGH
   ENDIF

!DINV!     Validate the bounds on which the limited initialization below relies.
!DINV
!DINV   IF (LTSTFL) THEN
!DINV      IF (ISCLW.GT.1 .OR. ISCLW+ISM1.LT.MSC4MI .OR.&
!DINV      &ISCHG+ISP1.GT.ISHGH .OR. ISHGH.GT.MSC4MA) &
!DINV      &ERROR STOP 'SWSNL2 frequency-range invariant violated'
!DINV      IF (IDLOW-IIID.LT.MDC4MI .OR. IDHGH+IIID.GT.MDC4MA) &
!DINV      &ERROR STOP 'SWSNL2 UE direction range exceeds workspace'
!DINV      IF (IDCLOW-MAX(IDP1,IDM1).LT.IDLOW-IIID .OR.&
!DINV      &IDCHGH+MAX(IDP1,IDM1).GT.IDHGH+IIID) &
!DINV      &ERROR STOP 'SWSNL2 interaction direction hull is uninitialized'
!DINV      IF (ISSTOP.GT.0) THEN
!DINV         IF (1-ISP1.LT.MSC4MI .OR. ISSTOP-ISM1.GT.ISCHG) &
!DINV         &ERROR STOP 'SWSNL2 source stencil frequency range is invalid'
!DINV         IF (MINVAL(IDCMIN(1:ISSTOP))-MAX(IDP1,IDM1).LT.MDC4MI .OR.&
!DINV         &MAXVAL(IDCMAX(1:ISSTOP))+MAX(IDP1,IDM1).GT.MDC4MA) &
!DINV         &ERROR STOP 'SWSNL2 source stencil direction range is invalid'
!DINV      END IF
!DINV   END IF

!     *** Zero only the array parts that are read below without being ***
!     *** assigned first: the frequency rows below the lowest         ***
!     *** discrete bin (IS <= 0). UE is assigned for rows 1:ISHGH     ***
!     *** over IDLOW-IIID:IDHGH+IIID, SA1/SA2 for rows 1:ISCHG over   ***
!     *** IDLOW:IDHGH and reads stay within those column ranges;      ***
!     *** SFNL is assigned before it is read                          ***

   DO IDDUM = IDLOW - IIID, IDHGH + IIID
      DO IS = MSC4MI, 0
         UE(IS,IDDUM) = 0.
      ENDDO
   ENDDO
   DO ID = IDLOW, IDHGH
      DO IS = MSC4MI, 0
         SA1(IS,ID) = 0.
         SA2(IS,ID) = 0.
      ENDDO
   ENDDO

!     *** Prepare auxiliary spectrum               ***
!     *** set action original spectrum in array UE ***

   DO IDDUM = IDLOW - IIID , IDHGH + IIID
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      DO IS = 1, MSC
         UE(IS,IDDUM) = AC2(ID,IS,IGP) * SPCSIG(IS) * JACOBI
      ENDDO
   ENDDO

!     *** set values in the areas 2 for IS > MSC+1 ***

   DO ID = IDLOW - IIID , IDHGH + IIID
      DO IS = MSC+1, ISHGH
         UE (IS,ID) = UE(IS-1,ID) * FACHFR
      ENDDO
   ENDDO

!     *** Calculate interactions      ***
!     *** Energy at interacting bins  ***

   DO ID = IDCLOW , IDCHGH
!$OMP SIMD
      DO IS = ISCLW, ISCHG
         E00    =        UE(IS      ,ID      )
         EP1    = AWG1 * UE(IS+ISP1,ID+IDP1) +&
         &AWG2 * UE(IS+ISP1,ID+IDP ) +&
         &AWG3 * UE(IS+ISP ,ID+IDP1) +&
         &AWG4 * UE(IS+ISP ,ID+IDP )
         EM1    = AWG5 * UE(IS+ISM1,ID-IDM1) +&
         &AWG6 * UE(IS+ISM1,ID-IDM ) +&
         &AWG7 * UE(IS+ISM ,ID-IDM1) +&
         &AWG8 * UE(IS+ISM ,ID-IDM )

         EP2    = AWG1 * UE(IS+ISP1,ID-IDP1) +&
         &AWG2 * UE(IS+ISP1,ID-IDP ) +&
         &AWG3 * UE(IS+ISP ,ID-IDP1) +&
         &AWG4 * UE(IS+ISP ,ID-IDP )
         EM2    = AWG5 * UE(IS+ISM1,ID+IDM1) +&
         &AWG6 * UE(IS+ISM1,ID+IDM ) +&
         &AWG7 * UE(IS+ISM ,ID+IDM1) +&
         &AWG8 * UE(IS+ISM ,ID+IDM )

!         *** Contribution to interactions                          ***
!         *** CONS is the shallow water factor for the NL interact. ***

         FACTOR = CONS * AF11(IS) * E00

         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 ) * PQUAD(2)
         SA1B   = SA1A - EP1*EM1*DAL3 * PQUAD(2)
         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 ) * PQUAD(2)
         SA2B   = SA2A - EP2*EM2*DAL3 * PQUAD(2)

         SA1 (IS,ID) = FACTOR * SA1B
         SA2 (IS,ID) = FACTOR * SA2B
      ENDDO
   ENDDO

!     Keep diagnostic output out of the vectorizable production loop.
!     The scalar quantities are recomputed only when detailed tracing is
!     requested; SA1 and SA2 retain the values from the production loop.

   IF (LTSTFL) THEN
      DO IS = ISCLW, ISCHG
         DO ID = IDCLOW , IDCHGH
            E00    =        UE(IS      ,ID      )
            EP1    = AWG1 * UE(IS+ISP1,ID+IDP1) +&
            &AWG2 * UE(IS+ISP1,ID+IDP ) +&
            &AWG3 * UE(IS+ISP ,ID+IDP1) +&
            &AWG4 * UE(IS+ISP ,ID+IDP )
            EM1    = AWG5 * UE(IS+ISM1,ID-IDM1) +&
            &AWG6 * UE(IS+ISM1,ID-IDM ) +&
            &AWG7 * UE(IS+ISM ,ID-IDM1) +&
            &AWG8 * UE(IS+ISM ,ID-IDM )
            EP2    = AWG1 * UE(IS+ISP1,ID-IDP1) +&
            &AWG2 * UE(IS+ISP1,ID-IDP ) +&
            &AWG3 * UE(IS+ISP ,ID-IDP1) +&
            &AWG4 * UE(IS+ISP ,ID-IDP )
            EM2    = AWG5 * UE(IS+ISM1,ID+IDM1) +&
            &AWG6 * UE(IS+ISM1,ID+IDM ) +&
            &AWG7 * UE(IS+ISM ,ID+IDM1) +&
            &AWG8 * UE(IS+ISM ,ID+IDM )
            FACTOR = CONS * AF11(IS) * E00
            SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 ) * PQUAD(2)
            SA1B   = SA1A - EP1*EM1*DAL3 * PQUAD(2)
            SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 ) * PQUAD(2)
            SA2B   = SA2A - EP2*EM2*DAL3 * PQUAD(2)
            WRITE(PRINTF,"(' E00 EP1 EM1 EP2 EM2 :',5E11.4)") E00,EP1,EM1,EP2,EM2
            WRITE(PRINTF,"(' SA1A SA1B SA2A SA2B :',4E11.4)") SA1A,SA1B,SA2A,SA2B
            WRITE(PRINTF,"(' IS ID SA1() SA2() :',2I4,2E12.4)") IS,ID,SA1(IS,ID),SA2(IS,ID)
            WRITE(PRINTF,"(' FACTOR ISLOW : ',E12.4,I4)") FACTOR ,ISLOW
         ENDDO
      ENDDO
   END IF

!     *** Fold interactions to side angles if spectral domain ***
!     *** is periodic in directional space                    ***

   IF ( PERCIR ) THEN
      DO ID = 1, IDHGH - MDC
         ID0   = 1 - ID
         DO IS = ISCLW, ISCHG
            SA1 (IS,MDC+ID) = SA1 (IS ,  ID    )
            SA2 (IS,MDC+ID) = SA2 (IS ,  ID    )
            SA1 (IS,  ID0 ) = SA1 (IS , MDC+ID0)
            SA2 (IS,  ID0 ) = SA2 (IS , MDC+ID0)
         ENDDO
      ENDDO
   ENDIF

!     *** Put source term together. Keep this arithmetic stencil ***
!     *** separate from the sign-dependent Patankar update below ***
!     *** so the compiler can optimize both loops independently.  ***

   DO I = 1, ISSTOP
      DO J = IDCMIN(I), IDCMAX(I)
         ID = MOD ( J - 1 + MDC , MDC ) + 1
         SFNL(I,ID) =   - 2. * ( SA1(I,J) + SA2(I,J) )&
         &+ AWG1 * ( SA1(I-ISP1,J-IDP1) + SA2(I-ISP1,J+IDP1) )&
         &+ AWG2 * ( SA1(I-ISP1,J-IDP ) + SA2(I-ISP1,J+IDP ) )&
         &+ AWG3 * ( SA1(I-ISP ,J-IDP1) + SA2(I-ISP ,J+IDP1) )&
         &+ AWG4 * ( SA1(I-ISP ,J-IDP ) + SA2(I-ISP ,J+IDP ) )&
         &+ AWG5 * ( SA1(I-ISM1,J+IDM1) + SA2(I-ISM1,J-IDM1) )&
         &+ AWG6 * ( SA1(I-ISM1,J+IDM ) + SA2(I-ISM1,J-IDM ) )&
         &+ AWG7 * ( SA1(I-ISM ,J+IDM1) + SA2(I-ISM ,J-IDM1) )&
         &+ AWG8 * ( SA1(I-ISM ,J+IDM ) + SA2(I-ISM ,J-IDM ) )
      ENDDO
   ENDDO

!     *** Store results in rhs and main diagonal according to ***
!     *** Patankar rules.                                      ***

   DO I = 1, ISSTOP
      SIGPI = SPCSIG(I) * JACOBI
      DO J = IDCMIN(I), IDCMAX(I)
         ID = MOD ( J - 1 + MDC , MDC ) + 1
         IF(TESTFL) PLNL4S(ID,I,IPTST) =  SFNL(I,ID) / SIGPI
         IF (SFNL(I,ID).GT.0.) THEN
            IMATRA(ID,I) = IMATRA(ID,I) + SFNL(I,ID) / SIGPI
            REDC0(ID,I,1)= REDC0(ID,I,1)+ SFNL(I,ID) / SIGPI
         ELSE
            IMATDA(ID,I) = IMATDA(ID,I) - SFNL(I,ID) /&
            &MAX(1.E-18,AC2(ID,I,IGP)*SIGPI)
            REDC1(ID,I,1)= REDC1(ID,I,1)+ SFNL(I,ID) /&
            &MAX(1.E-18,AC2(ID,I,IGP)*SIGPI)
         END IF
      ENDDO
   ENDDO

!     *** test output ***

   IF (ITEST .GE. 40 .AND. TESTFL) THEN
      WRITE(PRINTF,*) ' SWSNL2 subroutine '
      WRITE(PRINTF,"(' IDP IDP1 IDM IDM1 :',4I5)") IDP, IDP1, IDM, IDM1
      WRITE (PRINTF,"(' ISP ISP1 ISM ISM1 :',4I5)") ISP, ISP1, ISM, ISM1
      WRITE (PRINTF,"(' ISHG IDDLOW IDDTOP :',3I5)") ISHGH, IDDLOW, IDDTOP
      WRITE(PRINTF,"(' ICLW ICHG IDLOW IDHGH :',4I5)") ISCLW, ISCHG, IDLOW, IDHGH
      WRITE (PRINTF,"(' AWG1 AWG2 AWG3 AWG4 :',4E12.4)") AWG1, AWG2, AWG3, AWG4
      WRITE (PRINTF,"(' AWG5 AWG6 AWG7 AWG8 :',4E12.4)") AWG5, AWG6, AWG7, AWG8
      WRITE (PRINTF,"(' S4MI S4MA D4MI D4MA :',4I6)") MSC4MI, MSC4MA, MDC4MI, MDC4MA
      WRITE(PRINTF,"(' SNLC1 X X2 CONS :',4E12.4)") SNLC1,X,X2,CONS
      WRITE(PRINTF,"(' DEPTH KMESPC FACHFR PI:',4E12.4)") DEP2(IGP),KMESPC, FACHFR,PI
      WRITE(PRINTF,"(' JACOBI ISLOW :',E12.4,I4)") JACOBI,ISLOW
      WRITE(PRINTF,*)
   END IF

   RETURN
!     End of SWSNL2
end subroutine SWSNL2

!************************************************************


SUBROUTINE SWSNL3 (                  WWINT   ,WWAWG   ,&
&UE      ,SA1     ,SA2     ,SPCSIG  ,SNLC1   ,&
&DAL1    ,DAL2    ,DAL3    ,SFNL    ,DEP2    ,&
&AC2     ,KMESPC  ,MEMNL4  ,FACHFR, AF11     ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!*******************************************************************

   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   INTEGER, INTENT(IN) :: IGP
   REAL, INTENT(IN) :: AF11(MSC4MI:MSC4MA)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!     40.17: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Calculate non-linear interaction using the discrete interaction
!     approximation (Hasselmann and Hasselmann 1985; WAMDI group 1988)
!     for the full circle (option if a current is present). Note: using
!     this subroutine requires an additional array with size
!     (MXC*MYC*MDC*MSC). This requires more internal memory but can
!     speed up the computations sigificantly if a current is present.
!
!  3. Method
!
!     Discrete interaction approximation. To make interpolation simple,
!     the interactions are calculated in a "folded" space.
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |     ^   |
!              ISLOW  1                    MSC    |   ISHGH
!                     |                           |
!                   ISCLW                        ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!                            1 : Extra tail added beyond MSC
!                            2 : Spectrum copied outside ID range
!                            3 : Empty bins at low frequencies
!
!     ISLOW =  1  + ISM1
!     ISHGH = MSC + ISP1 - ISM1
!     ISCLW =  1
!     ISCHG = MSC - ISM1
!     IDLOW =  1  - MAX(IDM1,IDP1)
!     IDHGH = MDC + MAX(IDM1,IDP1)
!
!       Relative offsets of interpolation points around central bin
!       "#" and corresponding numbers of AWGn :
!
!               ISM1  ISM
!                5        7    T |
!          IDM1   +------+     H +
!                 |      |     E |      ISP      ISP1
!                 |   \  |     T |       3           1
!           IDM   +------+     A +        +---------+  IDP1
!                6       \8      |        |         |
!                                |        |  /      |
!                           \    +        +---------+  IDP
!                                |      /4           2
!                              \ |  /
!          -+-----+------+-------#--------+---------+----------+
!                                |           FREQ.
!
!
!  4. Argument variables
!
!     MCGRD : number of wet grid points of the computational grid
!     MDC   : grid points in theta-direction of computational grid
!     MDC4MA: highest array counter in directional space (Snl4)
!     MDC4MI: lowest array counter in directional space (Snl4)
!     MSC   : grid points in sigma-direction of computational grid
!     MSC4MA: highest array counter in frequency space (Snl4)
!     MSC4MI: lowest array counter in frequency space (Snl4)
!     WWINT : counters for quadruplet interactions

   INTEGER WWINT(*)

!     AC2   : action density
!     AF11  : scaling frequency
!     DAL1  : coefficient for the quadruplet interactions
!     DAL2  : coefficient for the quadruplet interactions
!     DAL3  : coefficient for the quadruplet interactions
!     DEP2  : depth
!     FACHFR
!     KMESPC: mean average wavenumber over full spectrum
!     MEMNL4
!     PI    : circular constant
!     SA1   : interaction contribution of first quadruplet (unfolded space)
!     SA2   : interaction contribution of second quadruplet (unfolded space)
!     SFNL
!     SNLC1
!     SPCSIG: relative frequencies in computational domain in sigma-space
!     UE    : "unfolded" spectrum
!     WWAWG : weight coefficients for the quadruplet interactions

   REAL    DAL1, DAL2, DAL3, FACHFR, KMESPC, SNLC1
   REAL    AC2(MDC,MSC,MCGRD)
   REAL    DEP2(MCGRD)
   REAL    MEMNL4(MDC,MSC,MCGRD)
   REAL    SA1(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SA2(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SFNL(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SPCSIG(MSC)
   REAL    UE(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    WWAWG(*)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!       Initialisations.
!       Calculate proportionality constant.
!       Prepare auxiliary spectrum.
!       Calculate (unfolded) interactions :
!       -----------------------------------------
!         Energy at interacting bins
!         Contribution to interactions
!         Fold interactions to side angles
!       -----------------------------------------
!       Put source term together
!     -------------------------------------------
!
! 13. Source text
!
!*******************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS, ID, ID0, I, J, IDDUM, ISLOW, &
   &ISHGH   ,IDLOW   ,IDHGH   ,ISP     ,ISP1    ,&
   &IDP     ,IDP1    ,ISM     ,ISM1    ,IDM     ,IDM1    ,&
   &ISCLW   ,ISCHG

   REAL      X       ,X2      ,CONS    ,FACTOR  ,SNLCS2  ,&
   &SNLCS3  ,E00     ,EP1     ,EM1     ,EP2     ,EM2     ,&
   &SA1A    ,SA1B    ,SA2A    ,SA2B    ,&
   &AWG1    ,AWG2    ,AWG3    ,AWG4    ,AWG5    ,AWG6    ,&
   &AWG7    ,AWG8    ,&
   &JACOBI, SIGPI, SNLCS1

   LOGICAL   LTSTFL

   IF (LTRACE) CALL STRACE (IENT,'SWSNL3')

!     evaluate the test-output condition once; the per-bin loop below
!     only tests this single flag
   LTSTFL = ITEST.GE.100 .AND. TESTFL

   IDP    = WWINT(1)
   IDP1   = WWINT(2)
   IDM    = WWINT(3)
   IDM1   = WWINT(4)
   ISP    = WWINT(5)
   ISP1   = WWINT(6)
   ISM    = WWINT(7)
   ISM1   = WWINT(8)
   ISLOW  = WWINT(9)
   ISHGH  = WWINT(10)
   ISCLW  = WWINT(11)
   ISCHG  = WWINT(12)
   IDLOW  = WWINT(13)
   IDHGH  = WWINT(14)

   AWG1 = WWAWG(1)
   AWG2 = WWAWG(2)
   AWG3 = WWAWG(3)
   AWG4 = WWAWG(4)
   AWG5 = WWAWG(5)
   AWG6 = WWAWG(6)
   AWG7 = WWAWG(7)
   AWG8 = WWAWG(8)

!     *** Calculate prop. constant.                           ***
!     *** Calculate factor R(X) to calculate the NL wave-wave ***
!     *** interaction for shallow water                       ***
!     *** SNLC1 = 1/GRAV**4                                   ***

   SNLCS1 = PQUAD(3)
   SNLCS2 = PQUAD(4)
   SNLCS3 = PQUAD(5)
   X      = MAX ( 0.75 * DEP2(IGP) * KMESPC , 0.5 )
   X2     = MAX ( -1.E15, SNLCS3*X)
   CONS   = SNLC1 * ( 1. + SNLCS1/X * (1.-SNLCS2*X) * EXP(X2))
   JACOBI = 2. * PI

!     Only the low-frequency rows can be read before assignment.
!     UE is assigned for rows 1:ISHGH and SA1/SA2 for 1:ISCHG;
!     SFNL is assigned directly by the source stencil.

   DO ID = IDLOW, IDHGH
      DO IS = MSC4MI, 0
         UE(IS,ID)  = 0.
         SA1(IS,ID) = 0.
         SA2(IS,ID) = 0.
      ENDDO
   ENDDO

!     *** extend the area with action density at periodic boundaries ***

   DO IDDUM = IDLOW, IDHGH
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      DO IS=1, MSC
         UE (IS,IDDUM) = AC2(ID,IS,IGP) * SPCSIG(IS) * JACOBI
      ENDDO
   ENDDO

   DO IS = MSC+1, ISHGH
      DO ID = IDLOW, IDHGH
         UE(IS,ID) = UE(IS-1,ID) * FACHFR
      ENDDO
   ENDDO

!     *** Calculate (unfolded) interactions ***
!     *** Energy at interacting bins        ***

   DO IS = ISCLW, ISCHG
      DO ID = 1, MDC
         E00    =        UE(IS      ,ID      )
         EP1    = AWG1 * UE(IS+ISP1,ID+IDP1) +&
         &AWG2 * UE(IS+ISP1,ID+IDP ) +&
         &AWG3 * UE(IS+ISP ,ID+IDP1) +&
         &AWG4 * UE(IS+ISP ,ID+IDP )
         EM1    = AWG5 * UE(IS+ISM1,ID-IDM1) +&
         &AWG6 * UE(IS+ISM1,ID-IDM ) +&
         &AWG7 * UE(IS+ISM ,ID-IDM1) +&
         &AWG8 * UE(IS+ISM ,ID-IDM )
         EP2    = AWG1 * UE(IS+ISP1,ID-IDP1) +&
         &AWG2 * UE(IS+ISP1,ID-IDP ) +&
         &AWG3 * UE(IS+ISP ,ID-IDP1) +&
         &AWG4 * UE(IS+ISP ,ID-IDP )
         EM2    = AWG5 * UE(IS+ISM1,ID+IDM1) +&
         &AWG6 * UE(IS+ISM1,ID+IDM ) +&
         &AWG7 * UE(IS+ISM ,ID+IDM1) +&
         &AWG8 * UE(IS+ISM ,ID+IDM )

!         Contribution to interactions

         FACTOR = CONS * AF11(IS) * E00

         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 ) * PQUAD(2)
         SA1B   = SA1A - EP1*EM1*DAL3 * PQUAD(2)
         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 ) * PQUAD(2)
         SA2B   = SA2A - EP2*EM2*DAL3 * PQUAD(2)

         SA1 (IS,ID) = FACTOR * SA1B
         SA2 (IS,ID) = FACTOR * SA2B

         IF (LTSTFL) THEN
            WRITE(PRINTF,"(' E00 EP1 EM1 EP2 EM2 :',5E11.4)") E00,EP1,EM1,EP2,EM2
            WRITE(PRINTF,"(' SA1A SA1B SA2A SA2B :',4E11.4)") SA1A,SA1B,SA2A,SA2B
            WRITE(PRINTF,"(' IS ID SA1() SA2() :',2I4,2E12.4)") IS,ID,SA1(IS,ID),SA2(IS,ID)
            WRITE(PRINTF,"(' FACTOR JACOBI : ',2E12.4)") FACTOR,JACOBI
         END IF

      ENDDO
   ENDDO

!     *** Fold interactions to side angles -> domain in theta is ***
!     *** periodic                                               ***

   DO ID = 1, IDHGH - MDC
      ID0   = 1 - ID
      DO IS = ISCLW, ISCHG
         SA1 (IS,MDC+ID) = SA1 (IS,  ID   )
         SA2 (IS,MDC+ID) = SA2 (IS,  ID   )
         SA1 (IS,  ID0 ) = SA1 (IS,MDC+ID0)
         SA2 (IS,  ID0 ) = SA2 (IS,MDC+ID0)
      ENDDO
   ENDDO

!     *** Put source term together (To save space I=IS and ***
!     *** J=MDC is used)  ----                             ***

   DO I = 1, MSC
      SIGPI = SPCSIG(I) * JACOBI
      DO J = 1, MDC
         SFNL(I,J) =   - 2. * ( SA1(I,J) + SA2(I,J) )&
         &+ AWG1 * ( SA1(I-ISP1,J-IDP1) + SA2(I-ISP1,J+IDP1) )&
         &+ AWG2 * ( SA1(I-ISP1,J-IDP ) + SA2(I-ISP1,J+IDP ) )&
         &+ AWG3 * ( SA1(I-ISP ,J-IDP1) + SA2(I-ISP ,J+IDP1) )&
         &+ AWG4 * ( SA1(I-ISP ,J-IDP ) + SA2(I-ISP ,J+IDP ) )&
         &+ AWG5 * ( SA1(I-ISM1,J+IDM1) + SA2(I-ISM1,J-IDM1) )&
         &+ AWG6 * ( SA1(I-ISM1,J+IDM ) + SA2(I-ISM1,J-IDM ) )&
         &+ AWG7 * ( SA1(I-ISM ,J+IDM1) + SA2(I-ISM ,J-IDM1) )&
         &+ AWG8 * ( SA1(I-ISM ,J+IDM ) + SA2(I-ISM ,J-IDM ) )

!         *** store value in auxiliary array and use values in ***
!         *** next four sweeps (see subroutine FILNL3)         ***

         MEMNL4(J,I,IGP) = SFNL(I,J) / SIGPI
      ENDDO
   ENDDO

!     *** test output ***

   IF (ITEST .GE. 50 .AND. TESTFL) THEN
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' SWSNL3 subroutine '
      WRITE(PRINTF,"(' IDP IDP1 IDM IDM1 :',4I5)") IDP, IDP1, IDM, IDM1
      WRITE (PRINTF,"(' ISP ISP1 ISM ISM1 :',4I5)") ISP, ISP1, ISM, ISM1
      WRITE (PRINTF,"(' ISLOW ISHG IDLOW IDHG :',4I5)") ISLOW, ISHGH, IDLOW, IDHGH
      WRITE(PRINTF,"(' ICLW ICHG JACOBI :',2I5,E12.4)") ISCLW, ISCHG, JACOBI
      WRITE (PRINTF,"(' AWG1 AWG2 AWG3 AWG4 :',4E12.4)") AWG1, AWG2, AWG3, AWG4
      WRITE (PRINTF,"(' AWG5 AWG6 AWG7 AWG8 :',4E12.4)") AWG5, AWG6, AWG7, AWG8
      WRITE (PRINTF,"(' S4MI S4MA D4MI D4MA :',4I6)") MSC4MI, MSC4MA, MDC4MI, MDC4MA
      WRITE(PRINTF,"(' SNLC1 X X2 CONS :',4E12.4)") SNLC1,X,X2,CONS
      WRITE(PRINTF,"(' DEPTH KMESPC FACHFR PI:',4E12.4)") DEP2(IGP),KMESPC,FACHFR,PI
      WRITE(PRINTF,*)

!       *** value source term in every bin ***

      IF(ITEST.GE. 150 ) THEN
         DO I=1, MSC
            DO J=1, MDC
               WRITE(PRINTF,"(' I J MEMNL() SFNL() SPCSIG:',2I4,3E12.4)") I,J,MEMNL4(J,I,IGP),SFNL(I,J),&
               &SPCSIG(I)
            ENDDO
         ENDDO
      END IF
   END IF

   RETURN

end subroutine SWSNL3

!*******************************************************************

SUBROUTINE SWSNL4 (WWINT   ,WWAWG   ,&
&SPCSIG  ,SNLC1   ,&
&DAL1    ,DAL2    ,DAL3    ,DEP2    ,&
&AC2     ,KMESPC  ,MEMNL4  ,FACHFR  ,&
&IDIA    ,ITER    ,UE      ,SA1     ,&
&SA2     ,SFNL    ,AF11, CNL4_1, CNL4_2,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!*******************************************************************

   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units
   INTEGER, INTENT(IN) :: IGP
   REAL, INTENT(IN) :: AF11(MSC4MI:MSC4MA)
   REAL, INTENT(IN) :: CNL4_1(MSC4MI:MSC4MA)
   REAL, INTENT(IN) :: CNL4_2(MSC4MI:MSC4MA)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!     40.17: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.17, Dec. 01: New Subroutine based on SWSNL3
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Calculate non-linear interaction using the discrete interaction
!     approximation (Hasselmann and Hasselmann 1985; WAMDI group 1988)
!     for the full circle (option if a current is present). Note: using
!     this subroutine requires an additional array with size
!     (MXC*MYC*MDC*MSC). This requires more internal memory but can
!     speed up the computations sigificantly if a current is present.
!
!  3. Method
!
!     Discrete interaction approximation. To make interpolation simple,
!     the interactions are calculated in a "folded" space.
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |     ^   |
!              ISLOW  1                    MSC    |   ISHGH
!                     |                           |
!                   ISCLW                        ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!                            1 : Extra tail added beyond MSC
!                            2 : Spectrum copied outside ID range
!                            3 : Empty bins at low frequencies
!
!     ISLOW =  1  + ISM1
!     ISHGH = MSC + ISP1 - ISM1
!     ISCLW =  1
!     ISCHG = MSC - ISM1
!     IDLOW =  1  - MAX(IDM1,IDP1)
!     IDHGH = MDC + MAX(IDM1,IDP1)
!
!       Relative offsets of interpolation points around central bin
!       "#" and corresponding numbers of AWGn :
!
!               ISM1  ISM
!                5        7    T |
!          IDM1   +------+     H +
!                 |      |     E |      ISP      ISP1
!                 |   \  |     T |       3           1
!           IDM   +------+     A +        +---------+  IDP1
!                6       \8      |        |         |
!                                |        |  /      |
!                           \    +        +---------+  IDP
!                                |      /4           2
!                              \ |  /
!          -+-----+------+-------#--------+---------+----------+
!                                |           FREQ.
!
!
!  4. Argument variables
!
!     MCGRD : number of wet grid points of the computational grid
!     MDC   : grid points in theta-direction of computational grid
!     MDC4MA: highest array counter in directional space (Snl4)
!     MDC4MI: lowest array counter in directional space (Snl4)
!     MSC   : grid points in sigma-direction of computational grid
!     MSC4MA: highest array counter in frequency space (Snl4)
!     MSC4MI: lowest array counter in frequency space (Snl4)
!     WWINT : counters for quadruplet interactions

   INTEGER WWINT(*)
   INTEGER IDIA

!     AC2   : action density
!     AF11  : scaling frequency
!     DAL1  : coefficient for the quadruplet interactions
!     DAL2  : coefficient for the quadruplet interactions
!     DAL3  : coefficient for the quadruplet interactions
!     DEP2  : depth
!     FACHFR
!     KMESPC: mean average wavenumber over full spectrum
!     MEMNL4
!     PI    : circular constant
!     SA1   : interaction contribution of first quadruplet (unfolded space)
!     SA2   : interaction contribution of second quadruplet (unfolded space)
!     SFNL
!     SNLC1
!     SPCSIG: relative frequencies in computational domain in sigma-space
!     UE    : "unfolded" spectrum
!     WWAWG : weight coefficients for the quadruplet interactions

   REAL    DAL1, DAL2, DAL3, FACHFR, KMESPC, SNLC1
   REAL    AC2(MDC,MSC,MCGRD)
   REAL    DEP2(MCGRD)
   REAL    MEMNL4(MDC,MSC,MCGRD)
   REAL    SA1(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SA2(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SFNL(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SPCSIG(MSC)
   REAL    UE(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    WWAWG(*)

!  6. Local variables
!
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
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!       Initialisations.
!       Calculate proportionality constant.
!       Prepare auxiliary spectrum.
!       Calculate (unfolded) interactions :
!       -----------------------------------------
!         Energy at interacting bins
!         Contribution to interactions
!         Fold interactions to side angles
!       -----------------------------------------
!       Put source term together
!     -------------------------------------------
!
! 13. Source text
!
!*******************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS, ID, ID0, I, J, IDDUM, ISLOW, ITER, &
   &ISHGH   ,IDLOW   ,IDHGH   ,ISP     ,ISP1    ,&
   &IDP     ,IDP1    ,ISM     ,ISM1    ,IDM     ,IDM1    ,&
   &ISCLW   ,ISCHG

   REAL      X       ,X2      ,CONS    ,FACTOR  ,SNLCS2  ,&
   &SNLCS3  ,E00     ,EP1     ,EM1     ,EP2     ,EM2     ,&
   &SA1A    ,SA1B    ,SA2A    ,SA2B    ,&
   &AWG1    ,AWG2    ,AWG3    ,AWG4    ,AWG5    ,AWG6    ,&
   &AWG7    ,AWG8    ,&
   &JACOBI, SIGPI, SNLCS1, FAC

   LOGICAL   LTSTFL

   IF (LTRACE) CALL STRACE (IENT,'SWSNL4')

!     evaluate the test-output condition once; the per-bin loop below
!     only tests this single flag
   LTSTFL = ITEST.GE.100 .AND. TESTFL

   IDP    = WWINT(1)
   IDP1   = WWINT(2)
   IDM    = WWINT(3)
   IDM1   = WWINT(4)
   ISP    = WWINT(5)
   ISP1   = WWINT(6)
   ISM    = WWINT(7)
   ISM1   = WWINT(8)
   ISLOW  = WWINT(9)
   ISHGH  = WWINT(10)
   ISCLW  = WWINT(11)
   ISCHG  = WWINT(12)
   IDLOW  = WWINT(13)
   IDHGH  = WWINT(14)

   AWG1 = WWAWG(1)
   AWG2 = WWAWG(2)
   AWG3 = WWAWG(3)
   AWG4 = WWAWG(4)
   AWG5 = WWAWG(5)
   AWG6 = WWAWG(6)
   AWG7 = WWAWG(7)
   AWG8 = WWAWG(8)

!     *** Initialize auxiliary arrays per gridpoint ***

   DO ID = MDC4MI, MDC4MA
      DO IS = MSC4MI, MSC4MA
         UE(IS,ID)   = 0.
         SA1(IS,ID)  = 0.
         SA2(IS,ID)  = 0.
         SFNL(IS,ID) = 0.
      ENDDO
   ENDDO

!     *** Calculate prop. constant.                           ***
!     *** Calculate factor R(X) to calculate the NL wave-wave ***
!     *** interaction for shallow water                       ***
!     *** SNLC1 = 1/GRAV**4                                   ***

   SNLCS1 = PQUAD(3)
   SNLCS2 = PQUAD(4)
   SNLCS3 = PQUAD(5)
   X      = MAX ( 0.75 * DEP2(IGP) * KMESPC , 0.5 )
   X2     = MAX ( -1.E15, SNLCS3*X)
   CONS   = SNLC1 * ( 1. + SNLCS1/X * (1.-SNLCS2*X) * EXP(X2))
   JACOBI = 2. * PI

!     *** extend the area with action density at periodic boundaries ***

   DO IDDUM = IDLOW, IDHGH
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      DO IS=1, MSC
         UE (IS,IDDUM) = AC2(ID,IS,IGP) * SPCSIG(IS) * JACOBI
      ENDDO
   ENDDO

   DO IS = MSC+1, ISHGH
      DO ID = IDLOW, IDHGH
         UE(IS,ID) = UE(IS-1,ID) * FACHFR
      ENDDO
   ENDDO

!     *** Calculate (unfolded) interactions ***
!     *** Energy at interacting bins        ***

   DO IS = ISCLW, ISCHG
      DO ID = 1, MDC
         E00    =        UE(IS      ,ID      )
         EP1    = AWG1 * UE(IS+ISP1,ID+IDP1) +&
         &AWG2 * UE(IS+ISP1,ID+IDP ) +&
         &AWG3 * UE(IS+ISP ,ID+IDP1) +&
         &AWG4 * UE(IS+ISP ,ID+IDP )
         EM1    = AWG5 * UE(IS+ISM1,ID-IDM1) +&
         &AWG6 * UE(IS+ISM1,ID-IDM ) +&
         &AWG7 * UE(IS+ISM ,ID-IDM1) +&
         &AWG8 * UE(IS+ISM ,ID-IDM )
         EP2    = AWG1 * UE(IS+ISP1,ID-IDP1) +&
         &AWG2 * UE(IS+ISP1,ID-IDP ) +&
         &AWG3 * UE(IS+ISP ,ID-IDP1) +&
         &AWG4 * UE(IS+ISP ,ID-IDP )
         EM2    = AWG5 * UE(IS+ISM1,ID+IDM1) +&
         &AWG6 * UE(IS+ISM1,ID+IDM ) +&
         &AWG7 * UE(IS+ISM ,ID+IDM1) +&
         &AWG8 * UE(IS+ISM ,ID+IDM )

!         Contribution to interactions

         FACTOR = CONS * AF11(IS) * E00

         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 ) * CNL4_1(IDIA)
         SA1B   = SA1A - EP1*EM1*DAL3 * CNL4_2(IDIA)
         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 ) * CNL4_1(IDIA)
         SA2B   = SA2A - EP2*EM2*DAL3 * CNL4_2(IDIA)


         SA1 (IS,ID) = FACTOR * SA1B
         SA2 (IS,ID) = FACTOR * SA2B

         IF (LTSTFL) THEN
            WRITE(PRINTF,"(' E00 EP1 EM1 EP2 EM2 :',5E11.4)") E00,EP1,EM1,EP2,EM2
            WRITE(PRINTF,"(' SA1A SA1B SA2A SA2B :',4E11.4)") SA1A,SA1B,SA2A,SA2B
            WRITE(PRINTF,"(' IS ID SA1() SA2() :',2I4,2E12.4)") IS,ID,SA1(IS,ID),SA2(IS,ID)
            WRITE(PRINTF,"(' FACTOR JACOBI : ',2E12.4)") FACTOR,JACOBI
         END IF

      ENDDO
   ENDDO

!     *** Fold interactions to side angles -> domain in theta is ***
!     *** periodic                                               ***

   DO ID = 1, IDHGH - MDC
      ID0   = 1 - ID
      DO IS = ISCLW, ISCHG
         SA1 (IS,MDC+ID) = SA1 (IS,  ID   )
         SA2 (IS,MDC+ID) = SA2 (IS,  ID   )
         SA1 (IS,  ID0 ) = SA1 (IS,MDC+ID0)
         SA2 (IS,  ID0 ) = SA2 (IS,MDC+ID0)
      ENDDO
   ENDDO

!     *** Put source term together (To save space I=IS and ***
!     *** J=MDC is used)                                   ***

   FAC = 1.

   DO I = 1, MSC
      SIGPI = SPCSIG(I) * JACOBI
      DO J = 1, MDC
         SFNL(I,J) =   - 2. * ( SA1(I,J) + SA2(I,J) )&
         &+ AWG1 * ( SA1(I-ISP1,J-IDP1) + SA2(I-ISP1,J+IDP1) )&
         &+ AWG2 * ( SA1(I-ISP1,J-IDP ) + SA2(I-ISP1,J+IDP ) )&
         &+ AWG3 * ( SA1(I-ISP ,J-IDP1) + SA2(I-ISP ,J+IDP1) )&
         &+ AWG4 * ( SA1(I-ISP ,J-IDP ) + SA2(I-ISP ,J+IDP ) )&
         &+ AWG5 * ( SA1(I-ISM1,J+IDM1) + SA2(I-ISM1,J-IDM1) )&
         &+ AWG6 * ( SA1(I-ISM1,J+IDM ) + SA2(I-ISM1,J-IDM ) )&
         &+ AWG7 * ( SA1(I-ISM ,J+IDM1) + SA2(I-ISM ,J-IDM1) )&
         &+ AWG8 * ( SA1(I-ISM ,J+IDM ) + SA2(I-ISM ,J-IDM ) )

!         *** store value in auxiliary array and use values in ***
!         *** next four sweeps (see subroutine FILNL3)         ***

         IF (IDIA.EQ.1) THEN
            MEMNL4(J,I,IGP) = FAC * SFNL(I,J) / SIGPI
         ELSE
            MEMNL4(J,I,IGP) = MEMNL4(J,I,IGP) +&
            &FAC * SFNL(I,J) / SIGPI
         END IF
      ENDDO
   ENDDO

!     *** test output ***

   IF (ITEST .GE. 50 .AND. TESTFL) THEN
      WRITE(PRINTF,*)
      WRITE(PRINTF,*) ' SWSNL4 subroutine '
      WRITE(PRINTF,"(' IDP IDP1 IDM IDM1 :',4I5)") IDP, IDP1, IDM, IDM1
      WRITE (PRINTF,"(' ISP ISP1 ISM ISM1 :',4I5)") ISP, ISP1, ISM, ISM1
      WRITE (PRINTF,"(' ISLOW ISHG IDLOW IDHG :',4I5)") ISLOW, ISHGH, IDLOW, IDHGH
      WRITE(PRINTF,"(' ICLW ICHG JACOBI :',2I5,E12.4)") ISCLW, ISCHG, JACOBI
      WRITE (PRINTF,"(' AWG1 AWG2 AWG3 AWG4 :',4E12.4)") AWG1, AWG2, AWG3, AWG4
      WRITE (PRINTF,"(' AWG5 AWG6 AWG7 AWG8 :',4E12.4)") AWG5, AWG6, AWG7, AWG8
      WRITE (PRINTF,"(' S4MI S4MA D4MI D4MA :',4I6)") MSC4MI, MSC4MA, MDC4MI, MDC4MA
      WRITE(PRINTF,"(' SNLC1 X X2 CONS :',4E12.4)") SNLC1,X,X2,CONS
      WRITE(PRINTF,"(' DEPTH KMESPC FACHFR PI:',4E12.4)") DEP2(IGP),KMESPC,FACHFR,PI
      WRITE(PRINTF,*)

!       *** value source term in every bin ***

      IF(ITEST.GE. 150 ) THEN
         DO I=1, MSC
            DO J=1, MDC
               WRITE(PRINTF,"(' I J MEMNL() SFNL() SPCSIG:',2I4,3E12.4)") I,J,MEMNL4(J,I,IGP),SFNL(I,J),&
               &SPCSIG(I)
            ENDDO
         ENDDO
      END IF
   END IF

   RETURN

end subroutine SWSNL4

!*********************************************************************
SUBROUTINE SWSNL8 (WWINT   ,UE      ,SA1     ,SA2     ,SPCSIG  ,&
&SNLC1   ,DAL1    ,DAL2    ,DAL3    ,SFNL    ,&
&DEP2    ,AC2     ,KMESPC  ,MEMNL4  ,FACHFR, AF11 ,IGP)
   USE swan_service_interfaces, ONLY: STRACE
!*********************************************************************

   USE swan_physics_selection
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output
   USE swan_diagnostics_level
   USE swan_io_units

   IMPLICIT NONE(TYPE, EXTERNAL)
   INTEGER, INTENT(IN) :: IGP
   REAL, INTENT(IN) :: AF11(MSC4MI:MSC4MA)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: H.L. Tolman, R.C. Ris                        |
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
!
!  1. Updates
!
!     40.41, Sep. 04: piecewise constant interpolation instead
!                     of bi-linear one
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Calculate non-linear interaction using the discrete interaction
!     approximation (Hasselmann and Hasselmann 1985; WAMDI group 1988)
!     for the full circle.
!
!  3. Method
!
!     Discrete interaction approximation. To make interpolation simple,
!     the interactions are calculated in a "folded" space.
!
!                            Frequencies -->
!                 +---+---------------------+---------+- IDHGH
!              d  | 3 :          2          :    2    |
!              i  + - + - - - - - - - - - - + - - - - +- MDC
!              r  |   :                     :         |
!              e  | 3 :  original spectrum  :    1    |
!              c  |   :                     :         |
!              t. + - + - - - - - - - - - - + - - - - +- 1
!                 | 3 :          2          :    2    |
!                 +---+---------------------+---------+- IDLOW
!                 |   |                     |     ^   |
!              ISLOW  1                    MSC    |   ISHGH
!                     |                           |
!                   ISCLW                        ISCHG
!              lowest discrete               highest discrete
!                central bin                   central bin
!
!                            1 : Extra tail added beyond MSC
!                            2 : Spectrum copied outside ID range
!                            3 : Empty bins at low frequencies
!
!     ISLOW =  1  + ISM1
!     ISHGH = MSC + ISP1 - ISM1
!     ISCLW =  1
!     ISCHG = MSC - ISM1
!     IDLOW =  1  - MAX(IDM1,IDP1)
!     IDHGH = MDC + MAX(IDM1,IDP1)
!
!     Note: using this subroutine requires an additional array
!           with size MXC*MYC*MDC*MSC.
!
!  4. Argument variables
!
!     MCGRD : number of wet grid points of the computational grid
!     MDC   : grid points in theta-direction of computational grid
!     MDC4MA: highest array counter in directional space (Snl4)
!     MDC4MI: lowest array counter in directional space (Snl4)
!     MSC   : grid points in sigma-direction of computational grid
!     MSC4MA: highest array counter in frequency space (Snl4)
!     MSC4MI: lowest array counter in frequency space (Snl4)
!     WWINT : counters for quadruplet interactions

   INTEGER WWINT(*)

!     AC2   : action density
!     AF11  : scaling frequency
!     DAL1  : coefficient for the quadruplet interactions
!     DAL2  : coefficient for the quadruplet interactions
!     DAL3  : coefficient for the quadruplet interactions
!     DEP2  : depth
!     FACHFR
!     KMESPC: mean average wavenumber over full spectrum
!     MEMNL4
!     PI    : circular constant
!     SA1   : interaction contribution of first quadruplet (unfolded space)
!     SA2   : interaction contribution of second quadruplet (unfolded space)
!     SFNL
!     SNLC1
!     SPCSIG: relative frequencies in computational domain in sigma-space
!     UE    : "unfolded" spectrum

   REAL    DAL1, DAL2, DAL3, FACHFR, KMESPC, SNLC1
   REAL    AC2(MDC,MSC,MCGRD)
   REAL    DEP2(MCGRD)
   REAL    MEMNL4(MDC,MSC,MCGRD)
   REAL    SA1(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SA2(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SFNL(MSC4MI:MSC4MA,MDC4MI:MDC4MA)
   REAL    SPCSIG(MSC)
   REAL    UE(MSC4MI:MSC4MA,MDC4MI:MDC4MA)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!       Initialisations.
!       Calculate proportionality constant.
!       Prepare auxiliary spectrum.
!       Calculate (unfolded) interactions :
!       -----------------------------------------
!         Energy at interacting bins
!         Contribution to interactions
!         Fold interactions to side angles
!       -----------------------------------------
!       Put source term together
!     -------------------------------------------
!
! 13. Source text
!
!*******************************************************************

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS      ,ID      ,ID0     ,I       ,J       ,&
   &ISLOW   ,ISHGH   ,IDLOW   ,IDHGH   ,&
   &IDPP    ,IDMM    ,ISPP    ,ISMM    ,&
   &ISCLW   ,ISCHG   ,IDDUM

   REAL      X       ,X2      ,CONS    ,FACTOR  ,SNLCS1  ,SNLCS2  ,&
   &SNLCS3  ,E00     ,EP1     ,EM1     ,EP2     ,EM2     ,&
   &SA1A    ,SA1B    ,SA2A    ,SA2B    ,&
   &JACOBI  ,SIGPI

   IF (LTRACE) CALL STRACE (IENT,'SWSNL8')

   ISLOW  = WWINT(9)
   ISHGH  = WWINT(10)
   ISCLW  = WWINT(11)
   ISCHG  = WWINT(12)
   IDLOW  = WWINT(13)
   IDHGH  = WWINT(14)
   IDPP   = WWINT(21)
   IDMM   = WWINT(22)
   ISPP   = WWINT(23)
   ISMM   = WWINT(24)

!     *** Calculate prop. constant.                           ***
!     *** Calculate factor R(X) to calculate the NL wave-wave ***
!     *** interaction for shallow water                       ***
!     *** SNLC1 = 1/GRAV**4                                   ***

   SNLCS1 = PQUAD(3)
   SNLCS2 = PQUAD(4)
   SNLCS3 = PQUAD(5)
   X      = MAX ( 0.75 * DEP2(IGP) * KMESPC , 0.5 )
   X2     = MAX ( -1.E15, SNLCS3*X)
   CONS   = SNLC1 * ( 1. + SNLCS1/X * (1.-SNLCS2*X) * EXP(X2))
   JACOBI = 2. * PI

!     *** extend the area with action density at periodic boundaries ***

   DO IDDUM = IDLOW, IDHGH
      ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
      DO IS=1, MSC
         UE (IS,IDDUM) = AC2(ID,IS,IGP) * SPCSIG(IS) * JACOBI
      ENDDO
   ENDDO

   DO ID = IDLOW, IDHGH
      DO IS = MSC+1, ISHGH
         UE(IS,ID) = UE(IS-1,ID) * FACHFR
      ENDDO
   ENDDO

!     *** Calculate (unfolded) interactions ***
!     *** Energy at interacting bins        ***

   DO ID = 1, MDC
      DO IS = ISCLW, ISCHG
         E00 = UE(IS     ,ID     )
         EP1 = UE(IS+ISPP,ID+IDPP)
         EM1 = UE(IS+ISMM,ID-IDMM)
         EP2 = UE(IS+ISPP,ID-IDPP)
         EM2 = UE(IS+ISMM,ID+IDMM)

!         Contribution to interactions

         FACTOR = CONS * AF11(IS) * PQUAD(2) * E00

         SA1A   = E00 * ( EP1*DAL1 + EM1*DAL2 )
         SA1B   = SA1A - EP1*EM1*DAL3
         SA2A   = E00 * ( EP2*DAL1 + EM2*DAL2 )
         SA2B   = SA2A - EP2*EM2*DAL3

         SA1 (IS,ID) = FACTOR * SA1B
         SA2 (IS,ID) = FACTOR * SA2B

      ENDDO
   ENDDO

!     *** Fold interactions to side angles -> domain in theta is ***
!     *** periodic                                               ***

   DO ID = 1, IDHGH - MDC
      ID0   = 1 - ID
      DO IS = ISCLW, ISCHG
         SA1 (IS,MDC+ID) = SA1 (IS,  ID   )
         SA2 (IS,MDC+ID) = SA2 (IS,  ID   )
         SA1 (IS,  ID0 ) = SA1 (IS,MDC+ID0)
         SA2 (IS,  ID0 ) = SA2 (IS,MDC+ID0)
      ENDDO
   ENDDO

!     *** Put source term together (To save space I=IS and ***
!     *** J=MDC is used)  ----                             ***

   DO I = 1, MSC
      SIGPI = SPCSIG(I) * JACOBI
      DO J = 1, MDC
         SFNL(I,J) =   - 2. * ( SA1(I,J) + SA2(I,J) )&
         &+ ( SA1(I-ISPP,J-IDPP) + SA2(I-ISPP,J+IDPP) )&
         &+ ( SA1(I-ISMM,J+IDMM) + SA2(I-ISMM,J-IDMM) )

!         *** store value in auxiliary array and use values in ***
!         *** next four sweeps (see subroutine FILNL3)         ***

         MEMNL4(J,I,IGP) = SFNL(I,J) / SIGPI
      ENDDO
   ENDDO

!     *** value source term in every bin ***

   IF ( ITEST.GE.150 .AND. TESTFL ) THEN
      DO I=1, MSC
         DO J=1, MDC
            WRITE(PRINTF,"(' I J MEMNL() SFNL() SPCSIG:',2I4,3E12.4)") I,J,MEMNL4(J,I,IGP),SFNL(I,J),&
            &SPCSIG(I)
         ENDDO
      ENDDO
   END IF

   RETURN

end subroutine SWSNL8

!*******************************************************************

SUBROUTINE FILNL3 (IDCMIN  ,IDCMAX  ,IMATRA  ,IMATDA  ,AC2     ,&
&MEMNL4  ,PLNL4S  ,ISSTOP  ,REDC0   ,REDC1   ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!*******************************************************************

   USE swan_physics_selection
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
!     40.23: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     40.23, Aug. 02: rhs and main diagonal adjusted according to Patankar-rules
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: store quadruplets for output purposes
!
!  2. Purpose
!
!     Fill the IMATRA/IMATDA arrays with the nonlinear wave-wave interaction
!     source term for a gridpoint ix,iy per sweep direction
!
!  3. Method
!
!
!  4. Argument variables
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
!     SOURCE (in SWANCOM1)
!
! 12. Structure
!
!     -------------------------------------------
!     Do for every frequency and spectral direction within a sweep
!         fill IMATRA/IMATDA
!     -------------------------------------------
!     End of FILNL3
!     -------------------------------------------
!
! 13. Source text
!
!*******************************************************************

   INTEGER, INTENT(IN) :: IGP
   INTEGER, SAVE :: IENT = 0
   INTEGER   IS, ID, IDDUM, ISSTOP

   REAL      IMATRA(MDC,MSC)           ,&
   &IMATDA(MDC,MSC)           ,&
   &AC2(MDC,MSC,MCGRD)        ,&
   &PLNL4S(MDC,MSC,NPTST)     ,&
   &MEMNL4(MDC,MSC,MCGRD)

   INTEGER   IDCMIN(MSC)         ,&
   &IDCMAX(MSC)
   REAL ::   REDC0 (MDC,MSC,MREDS)
   REAL ::   REDC1 (MDC,MSC,MREDS)

   IF (LTRACE) CALL STRACE (IENT,'FILNL3')

   do IS=1, ISSTOP
      do IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
         IF(TESTFL) PLNL4S(ID,IS,IPTST) = MEMNL4(ID,IS,IGP)
         IF (MEMNL4(ID,IS,IGP).GT.0.) THEN
            IMATRA(ID,IS) = IMATRA(ID,IS) + MEMNL4(ID,IS,IGP)
            REDC0(ID,IS,1)= REDC0(ID,IS,1)+ MEMNL4(ID,IS,IGP)
         ELSE
            IMATDA(ID,IS) = IMATDA(ID,IS) - MEMNL4(ID,IS,IGP) /&
            &MAX(1.E-18,AC2(ID,IS,IGP))
            REDC1(ID,IS,1)= REDC1(ID,IS,1)+ MEMNL4(ID,IS,IGP) /&
            &MAX(1.E-18,AC2(ID,IS,IGP))
         END IF
      end do
   end do

   IF ( TESTFL .AND. ITEST.GE.50 ) THEN
      WRITE(PRINTF,"(' FILNL3: ID_MIN ID_MAX MSC ISTOP :',4I6)") IDCMIN(1),IDCMAX(1),MSC,ISSTOP
      IF ( ITEST .GE. 100 ) THEN
         DO IS=1, ISSTOP
            DO IDDUM = IDCMIN(IS), IDCMAX(IS)
               ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
               WRITE(PRINTF,"(' FILNL3: IS ID MEMNL() :',2I6,E12.4)") IS,ID,MEMNL4(ID,IS,IGP)
            ENDDO
         ENDDO
      ENDIF
   ENDIF

   RETURN
!     End of FILNL3
end subroutine FILNL3

!----------------------------------------------------------------------
SUBROUTINE SWINTFXNL ( ASWAN,SIGMA,DIR,NDIR,NSIG,NGRID,DEPTH,&
&IQTYPE,SNL,KCGRD,ICMAX,IERROR,IGP )
!----------------------------------------------------------------------
!
!   +-------+    ALKYON Hydraulic Consultancy & Research
!   |       |    Gerbrant Ph. van Vledder
!   |   +---+
!   |   | +---+  Last update:  9 September 2002
!   +---+ |   |  Release: 5.0
!         +---+
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


   USE M_PARALL
   USE serv_xnl4v5
   USE m_xnldata

   IMPLICIT NONE(TYPE, EXTERNAL)
!-----------------------------------------------------------------------
!
!  0. Update history
!
!     Date Modification
!
!     25/02/1999 Initial version
!     16/03/1999 Interface with SWAN updated
!     24/03/1999 Interface extended with KCGRD and ICMAX
!     22/06/1999 Parameter IERR added to interface
!     20/07/1999 Call to Q_QMAIN modified
!     25/07/1999 Output files updated
!     24/09/1999 Finite depth effects included
!     25/11/1999 Bug fixed in deleting files
!     27/12/1999 Interface extended with grav,rho and ftail
!     02/02/2001 Interface modified, was N(k), now A(sigma) like SWAN
!     06/11/2001 Bug fixed in initialisation of Snl
!     08/08/2002 Version 4
!     22/08/2002 Size of direction array modified to conform with SWAN 40.11
!     09/09/2002 Release 5
!     18/05/2004 Implemented in SWAN 40.41, with adapted values of IQTYPE
!
!
!  1. Purpose:
!
!     interface with SWAN model to compute nonlinear transfer with
!     the XNL method for given action density spectrum
!
!  2. Method
!
!     Resio/Tracy deep water geometric scaling
!     Rewritten by Gerbrant van Vledder
!
!  3. Parameter list:
!
! Type     I/O         Name      Description
!-----------------------------------------------------------------------
   INTEGER, INTENT(IN) :: NDIR                    ! number of directi
   INTEGER, INTENT(IN) :: NSIG                    ! number of sigma v
   INTEGER, INTENT(IN) :: NGRID                   ! number of sea poi
   INTEGER, INTENT(IN) :: IQTYPE                  ! method of computi
!                                                      interactions
   REAL   , INTENT(IN) :: ASWAN(NDIR,NSIG,NGRID)  ! action density sp
!                                                    ! function of (sigma,dir)
   REAL   , INTENT(IN) :: SIGMA(NSIG)             ! Intrinsic frequen
   REAL   , INTENT(IN) :: DIR(NDIR,6)             ! directions in rad
   REAL   , INTENT(IN) :: DEPTH(NGRID)            ! depth array
   INTEGER, INTENT(IN) :: ICMAX                   ! number of points
   INTEGER, INTENT(IN) :: KCGRD(ICMAX)            ! grid addresses fo
   INTEGER, INTENT(IN) :: IGP                     ! current grid address
   REAL   , INTENT(OUT):: SNL(NDIR,NSIG,NGRID)    ! nonlinear quadrup
!                                                    ! a certain exact method (sigma,dir)
   INTEGER, INTENT(OUT):: IERROR                  ! Error indicator.
!-----------------------------------------------------------------------
!
!  4. Error messages
!
!     An error message is produced within the QUAD system.
!     If no errors are detected IERROR=0
!     1, incorrect IQUAD
!     2, depth < 0
!
!  5. Subroutines calling
!
!     SOURCE
!
!  6. Subroutines used
!
!  7. Remarks
!
!     The SWAN spectrum is given as an action density spectrum
!     as a function of Sigma and Theta: ASWAN(itheta,isig)
!
!     SWINTFXNL is called for each active grid point in a stencil
!     and for each time the complete array with all grid points
!     is given. Related grid points are specified in the array
!     KCGRD and ICMAX.
!
!  8. Structure
!
!  9. Switches
!
! 10. Source code
!-----------------------------------------------------------------------
!     Local parameters

   INTEGER ISIG,IDIR            ! counters
   INTEGER IGRID                ! grid index
   INTEGER IQUAD                ! type of computational method for Xn

!     --- assign arrays for intermediate storage of results

   REAL AQUAD(NSIG,NDIR)        ! action density spectrum A(sigma,dir
   REAL XNL(NSIG,NDIR)          ! transfer rate dA/dt(sigma,dir)
   REAL DIAG(NSIG,NDIR)         ! diagonal term (dXnl/dA)
   REAL DIRR(NDIR)              ! single array with directions in rad
!-----------------------------------------------------------------------
!
!     --- initialisations

   IERROR  = 0

   IGRID   = IGP ! set index of current grid index
   DIRR(:) = DIR(:,1) ! copy radian directions to single array

   SNL(:,:,IGRID)  = 0.
   DIAG            = 0.

!     --- check value of iquad

   IF ((IQTYPE.NE.51).AND.(IQTYPE.NE.52).AND.(IQTYPE.NE.53)) THEN
      IERROR = 1
      RETURN
   END IF

!  N.B. Take care of different order of indices in QUAD compared to SWAN
!
!     --- compute nonlinear interactions per individual spectrum
!         switch order of indices

   DO ISIG = 1, NSIG
      DO IDIR = 1, NDIR
         AQUAD(ISIG,IDIR) = ASWAN(IDIR,ISIG,IGRID)
      END DO
   END DO

!     --- transform parameter iquad of SWAN to the parameter iq_quad as
!         needed by the QUAD suite

   IQUAD = IQTYPE - 50

!     IQTYPE/IQUAD = 51/1   ! deep water transfer
!     IQTYPE/IQUAD = 52/2   ! deep water transfer with WAM depth scaling
!     IQTYPE/IQUAD = 53/3   ! finite depth transfer
!
!     --- call of main subroutine to compute nonlinear quadruplet interactions
!         for a given action density spectrum on a given spectral grid

   XNL   = 0.
   DIAG  = 0.
   CALL XNL_MAIN( AQUAD,SIGMA,DIRR,NSIG,NDIR,DEPTH(IGRID),IQUAD,&
   &XNL,DIAG,INODE,IERROR )

   IF (IERROR.NE.0) RETURN

!     --- convert nonlinear transfer to SWAN convention, only sequence of indices

   DO ISIG = 1, NSIG
      DO IDIR = 1, NDIR
         SNL(IDIR,ISIG,IGRID) = XNL(ISIG,IDIR)
      END DO
   END DO

   RETURN

end subroutine SWINTFXNL

!********************************************************************


!****************************************************************

SUBROUTINE FAC3WW ( DEP, SPCSIG, TRIADS )
   USE swan_triads, ONLY: TCOEF
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_wave_physics, ONLY: KSCIP1

!****************************************************************

   USE swan_diagnostics_level
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants

   IMPLICIT NONE(TYPE, EXTERNAL)
   TYPE(triad_state_t), INTENT(INOUT) :: TRIADS


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
!     42.01: Marcel Zijlema
!     42.15: Marcel Zijlema
!
!  1. Updates
!
!     42.01, November 2022: new subroutine
!     42.15,      May 2024: extenstion FTIM
!
!  2. Purpose
!
!     computes frequency-dependent interpolation factors
!     and frequency- and space-dependent scaling factors
!     for the triad-wave interactions
!
!  4. Argument variables
!
!     DEP         water depth
!     SPCSIG      relative frequencies

   REAL :: DEP(MCGRD)
   REAL :: SPCSIG(MSC)

!  6. Local variables
!
!     A     :     = (kd)^2
!     ARR   :     auxiliary array (not used)
!     C0    :     phase velocity at central bin
!     CG    :     group velocity as function of frequency
!     CG1   :     group velocity at first frequency bin
!     CG2   :     group velocity at second frequency bin
!     DEPLOC:     local water depth
!     DF    :     frequency step
!     DK    :     wave number mismatch
!     FT    :     = tanh(kd)/kd
!     I1    :     auxiliary integer
!     I2    :     auxiliary integer
!     IENT  :     number of entries
!     IP    :     counter
!     IS    :     loop counter
!     IS1   :     first loop counter in frequency space
!     IS2   :     second loop counter in frequency space
!     IS3   :     frequency index of third component
!     ISB   :     broken frequency index for third component
!     ISLOW :     lowest frequency index for second component
!     J     :     counter
!     K     :     wave number as function of frequency
!     K1    :     wave number at first frequency bin
!     K2    :     wave number at second frequency bin
!     K3    :     wave number at third interpolated bin
!     KB    :     SPB (empirical) parameter that represents
!                 the broadness of resonance condition
!     KM    :     mean wave number
!     P     :     shape coefficient (=4/3)
!     R     :     the numerator of the transfer function
!     RINT  :     interaction coefficient
!     S     :     the denominator of the transfer function
!     SIG1  :     frequency of first component
!     SIG2  :     frequency of second component
!     SIG3  :     frequency of third component
!     W0    :     radian frequency of bound super harmonic (p)
!     WIS   :     interpolation weight factor
!     WM    :     radian frequency of secondary harmonic (m)
!     WPM   :     radian frequency of primary harmonic (p-m)
!     WN0   :     wave number of bound super harmonic (p)
!     WNM   :     wave number of secondary harmonic (m)
!     WNPM  :     wave number of primary harmonic (p-m)
!     XIS   :     rate between two succeeding frequency counters
!     XISLN :     log of XIS

   INTEGER, SAVE :: IENT = 0
   INTEGER I1, I2, IP, IS, IS1, IS2, IS3, ISLOW, J
   REAL    C0, R, S, RINT, W0, WM, WPM, WN0, WNM, WNPM,&
   &XIS, XISLN
   REAL    A, ARR(MSC), CG(MSC), CG1, CG2, DEPLOC, DF, DK, FT, ISB,&
   &K(MSC), K1, K2, K3, KB, KM, P, SIG1, SIG2, SIG3, WIS
   REAL    K3A(1), SIG3A(1)

! 13. Source text

   ASSOCIATE(ISM => TRIADS%lower_index,&
   &ISM1 => TRIADS%lower_index_next,&
   &ISP => TRIADS%upper_index,&
   &ISP1 => TRIADS%upper_index_next,&
   &WISM => TRIADS%lower_weight,&
   &WISM1 => TRIADS%lower_weight_next,&
   &WISP => TRIADS%upper_weight,&
   &WISP1 => TRIADS%upper_weight_next,&
   &QTRI1 => TRIADS%interpolation,&
   &QTRI2 => TRIADS%scaling,&
   &TCOLL => TRIADS%collinear)
   IF (LTRACE) CALL STRACE (IENT,'FAC3WW')

   QTRI2 = 0.

   IF ( ITRIAD.EQ.1 .OR. ITRIAD.EQ.11 ) THEN

!        --- compute some indices in sigma space

      I2    = INT (FLOAT(MSC) / 2.)
      I1    = I2 - 1
      XIS   = SPCSIG(I2) / SPCSIG(I1)
      XISLN = LOG( XIS )

!        --- indices of second harmonic (self interaction)

      ISP  (2) = INT( LOG(2.) / XISLN )
      ISP1 (2) = ISP(2) + 1
      WISP (2) = (2. - XIS**ISP(2)) / (XIS**ISP1(2) - XIS**ISP(2))
      WISP1(2) = 1. - WISP(2)

      ISM  (2,1) = INT( LOG(0.5) / XISLN )
      ISM1 (2,1) = ISM(2,1) - 1
      WISM (2,1) = (XIS**ISM(2,1) -0.5) /&
      &(XIS**ISM(2,1) - XIS**ISM1(2,1))
      WISM1(2,1) = 1. - WISM(2,1)

!        --- indices of third harmonic (sum interaction)

      ISP  (3) = INT( LOG(3.) / XISLN )
      ISP1 (3) = ISP(3) + 1
      WISP (3) = (3. - XIS**ISP(3)) / (XIS**ISP1(3) - XIS**ISP(3))
      WISP1(3) = 1. - WISP(3)

      ISM  (3,1) = INT( LOG(1./3.) / XISLN )
      ISM1 (3,1) = ISM(3,1) - 1
      WISM (3,1) = (XIS**ISM(3,1) -1./3.) /&
      &(XIS**ISM(3,1) - XIS**ISM1(3,1))
      WISM1(3,1) = 1. - WISM(3,1)

      ISM  (3,2) = INT( LOG(2./3.) / XISLN )
      ISM1 (3,2) = ISM(3,2) - 1
      WISM (3,2) = (XIS**ISM(3,2) -2./3.) /&
      &(XIS**ISM(3,2) - XIS**ISM1(3,2))
      WISM1(3,2) = 1. - WISM(3,2)

      DO IP = 2, MCGRD

         DEPLOC = DEP(IP)

!           --- compute wave number and group velocity

         IF ( DEPLOC.GT.DEPMIN ) THEN
            CALL KSCIP1 (MSC, SPCSIG, DEPLOC, K, CG)
         ELSE
            K  = -1.
            CG =  0.
         ENDIF

         CG1 = 1.

!           --- compute LTA scaling factor

         DO IS = 1, MSC

            IF (ITRIAD.EQ.11) CG1 = CG(IS)

!              --- bound super harmonic (sum frequency)
            W0  = SPCSIG(IS)
            WN0 = K     (IS)
            C0  = W0 / WN0

!              --- primary wave (half the sum frequency)
            IF ( IS.GT.-ISM1(2,1) ) THEN
               WM  = WISM (2,1) * SPCSIG(IS+ISM1(2,1)) +&
               &WISM1(2,1) * SPCSIG(IS+ISM (2,1))
               WNM = WISM (2,1) * K     (IS+ISM1(2,1)) +&
               &WISM1(2,1) * K     (IS+ISM (2,1))
            ELSE
               WM  = 0.
               WNM = 0.
            END IF

!              compute interaction coefficient of self-self component
            IF ( DEPLOC.GT.DEPMIN ) THEN
               CALL TCOEF (W0,WM,WM,WN0,WNM,WNM,DEPLOC,R,S)
               RINT = R / S
            ELSE
               RINT = 0.
            ENDIF

            QTRI2(IS,IP,1) = PTRIAD(1) * CG1 * C0 * RINT**2

!              --- primary wave (one third of the sum frequency)
            IF ( IS.GT.-ISM1(3,1) ) THEN
               WPM  = WISM (3,1) * SPCSIG(IS+ISM1(3,1)) +&
               &WISM1(3,1) * SPCSIG(IS+ISM (3,1))
               WNPM = WISM (3,1) * K     (IS+ISM1(3,1)) +&
               &WISM1(3,1) * K     (IS+ISM (3,1))
            ELSE
               WPM  = 0.
               WNPM = 0.
            END IF

!              --- secondary wave (two third of the sum frequency)
            IF ( IS.GT.-ISM1(3,2) ) THEN
               WM  = WISM (3,2) * SPCSIG(IS+ISM1(3,2)) +&
               &WISM1(3,2) * SPCSIG(IS+ISM (3,2))
               WNM = WISM (3,2) * K     (IS+ISM1(3,2)) +&
               &WISM1(3,2) * K     (IS+ISM (3,2))
            ELSE
               WM  = 0.
               WNM = 0.
            END IF

!              compute interaction coefficient for third harmonic
            IF ( DEPLOC.GT.DEPMIN ) THEN
               CALL TCOEF (W0,WM,WPM,WN0,WNM,WNPM,DEPLOC,R,S)
               RINT = R / S
            ELSE
               RINT = 0.
            ENDIF

            QTRI2(IS,IP,2) = PTRIAD(1) * CG1 * C0 * RINT**2

         ENDDO

      ENDDO

   ELSE IF ( ITRIAD.EQ.2 .OR. ITRIAD.EQ.3 ) THEN

      QTRI1 = 0.

      DO IP = 2, MCGRD

         DEPLOC = DEP(IP)

!           --- compute wave number and group velocity

         IF ( DEPLOC.GT.DEPMIN ) THEN
            CALL KSCIP1 (MSC, SPCSIG, DEPLOC, K, CG)
         ELSE
            K  = -1.
            CG =  0.
         ENDIF

         J = 0

         DO IS1 = 1, MSC

!              --- bound super harmonic

            SIG1 = SPCSIG(IS1)
            K1   = K     (IS1)
            CG1  = CG    (IS1)

!              --- sum interactions

            DO IS2 = 1, IS1-1

!                 --- secondary wave

               SIG2 = SPCSIG(IS2)
               K2   = K     (IS2)

!                 --- primary wave
               SIG3 = SIG1 - SIG2

               J = J + 1

!                 --- obtain and store interpolation factors

               IF ( SIG3.GT.SPCSIG(1) ) THEN

                  ISB = LOG( SIG3/SPCSIG(1) ) / FRINTF

                  IS3 = INT(ISB)
                  WIS = ISB - REAL(IS3)
                  IS3 = IS3 + 1

                  QTRI1(J,1) = WIS
                  QTRI1(J,2) = FLOAT(IS3)

               ENDIF

!                 --- compute the wave number of primary component

               IF ( DEPLOC.GT.DEPMIN ) THEN
                  SIG3A(1) = SIG3
                  CALL KSCIP1 (1, SIG3A, DEPLOC, K3A)
                  K3 = K3A(1)
               ELSE
                  K3 = -1.
               ENDIF

!                 --- compute the wave number mismatch and frequency step

               DK = K3 + K2 - K1
               DF = FRINTF * SIG2 / PI2

!                 --- compute and store transfer functions

               IF ( DEPLOC.GT.DEPMIN ) THEN
                  CALL TCOEF(SIG1,SIG2, SIG3,K1,K2, K3,DEPLOC,R,S)
                  RINT = R / S
                  QTRI2(J,IP,2) = R / S
                  CALL TCOEF(SIG2,SIG1,-SIG3,K2,K1,-K3,DEPLOC,R,S)
                  QTRI2(J,IP,3) = R / S
                  CALL TCOEF(SIG3,SIG1,-SIG2,K3,K1,-K2,DEPLOC,R,S)
                  QTRI2(J,IP,4) = R / S
               ELSE
                  RINT = 0.
                  QTRI2(J,IP,2:4) = 0.
               ENDIF

!                 --- compute and store proportionality factor
!                     depending on the bispectrum parametrization

               IF ( ITRIAD.EQ.2 ) THEN
!                    SPB of Becq-Girard et al (1999)
                  KM = MIN(K1,K2,K3) ! according to James Salmon
                  KB = PTRIAD(6)*KM + PTRIAD(7)
                  FT = KB * DF / (DK*DK + KB*KB)
               ELSE IF ( ITRIAD.EQ.3 ) THEN
!                    based on the quasi-normal closure using parametrized biphase
!                    (see routine SWFTIM)
                  FT = DF / MAX( ABS(DK), 0.1*K1 )
               ENDIF

               QTRI2(J,IP,1) = PTRIAD(1) * FT * CG1 * RINT

            ENDDO

!              --- difference interactions

            DO IS2 = 1, MSC

!                 --- secondary wave

               SIG2 = SPCSIG(IS2)
               K2   = K     (IS2)

!                 --- primary wave
               SIG3 = SIG1 + SIG2

               J = J + 1

!                 --- obtain and store interpolation factors

               IF ( SIG3.LT.SPCSIG(MSC) ) THEN

                  ISB = LOG( SIG3/SPCSIG(1) ) / FRINTF

                  IS3 = INT(ISB)
                  WIS = ISB - REAL(IS3)
                  IS3 = IS3 + 1

                  QTRI1(J,1) = WIS
                  QTRI1(J,2) = FLOAT(IS3)

               ENDIF

!                 --- compute the wave number of primary component

               IF ( DEPLOC.GT.DEPMIN ) THEN
                  SIG3A(1) = SIG3
                  CALL KSCIP1 (1, SIG3A, DEPLOC, K3A)
                  K3 = K3A(1)
               ELSE
                  K3 = -1.
               ENDIF

!                 --- compute the wave number mismatch and frequency step

               DK = K1 + K2 - K3
               DF = FRINTF * SIG2 / PI2

!                 --- compute and store transfer functions

               IF ( DEPLOC.GT.DEPMIN ) THEN
                  CALL TCOEF(SIG3,SIG2, SIG1,K3,K2, K1,DEPLOC,R,S)
                  QTRI2(J,IP,2) = R / S
                  CALL TCOEF(SIG2,SIG3,-SIG1,K2,K3,-K1,DEPLOC,R,S)
                  QTRI2(J,IP,3) = R / S
                  CALL TCOEF(SIG1,SIG3,-SIG2,K1,K3,-K2,DEPLOC,R,S)
                  RINT = R / S
                  QTRI2(J,IP,4) = R / S
               ELSE
                  RINT = 0.
                  QTRI2(J,IP,2:4) = 0.
               ENDIF

!                 --- compute and store proportionality factor
!                     depending on the bispectrum parametrization

               IF ( ITRIAD.EQ.2 ) THEN
!                    SPB of Becq-Girard et al (1999)
                  KM = MIN(K1,K2,K3) ! according to James Salmon
                  KB = PTRIAD(6)*KM + PTRIAD(7)
                  FT = KB * DF / (DK*DK + KB*KB)
               ELSE IF ( ITRIAD.EQ.3 ) THEN
!                    based on the quasi-normal closure using parametrized biphase
!                    (see routine SWFTIM)
                  FT = DF / MAX( ABS(DK), 0.1*K1 )
               ENDIF

               QTRI2(J,IP,1) = PTRIAD(1) * FT * CG1 * RINT

            ENDDO

         ENDDO

      ENDDO

   ELSE IF ( ITRIAD.EQ.5 ) THEN

      P = PTRIAD(2)

      QTRI1 = 0.

      DO IP = 2, MCGRD

         DEPLOC = DEP(IP)

!           --- compute wave number and group velocity

         IF ( DEPLOC.GT.DEPMIN ) THEN
            CALL KSCIP1 (MSC, SPCSIG, DEPLOC, K, CG)
         ELSE
            K  = -1.
            CG =  0.
         ENDIF

         J = 0

         DO IS1 = 1, MSC

!              --- get first wave component

            SIG1 = SPCSIG(IS1)
            K1   = K     (IS1)
            CG1  = CG    (IS1)

            IF (TCOLL) THEN
               ISLOW = IS1+1
            ELSE
               ISLOW = 1
            ENDIF

            DO IS2 = ISLOW, MSC

!                 --- get second wave component

               SIG2 = SPCSIG(IS2)
               K2   = K     (IS2)
               CG2  = CG    (IS2)

!                 --- determine third component by means of quasi-resonance condition
               SIG3 = ABS(SIG2 - SIG1)

               J = J + 1

               IF ( SIG3.GT.SPCSIG(1) ) THEN

!                    --- obtain and store interpolation factors

                  ISB = LOG( SIG3/SPCSIG(1) ) / FRINTF

                  IS3 = INT(ISB)
                  WIS = ISB - REAL(IS3)
                  IS3 = IS3 + 1

                  QTRI1(J,1) = WIS
                  QTRI1(J,2) = FLOAT(IS3)

!                    --- compute the wave number of third component

                  K3 = (1.-WIS) * K(IS3) + WIS * K(IS3+1)

!                    --- compute the mean of the three wave numbers

                  KM = ( K1 + K2 + K3 ) / 3.

!                    --- the interactions will be scaled with depth
!                        note: use truncation of a continued fraction
!                              to approximate tanh efficiently

                  A = KM*DEPLOC
                  IF ( A.GT.6 ) THEN
                     FT = 1./A
                  ELSE
                     A  = A*A
                     FT = 1./(1.+A/(3.+A/(5.+A/(7.+A/(9.+A/11.)))))
                  ENDIF

!                    --- compute and store the DCTA scaling factors

                  QTRI2(J,IP,1) = FT**4 * CG1 * K1**P
                  QTRI2(J,IP,2) = FT**4 * CG2 * K2**P

               ENDIF

            ENDDO
         ENDDO

      ENDDO

   ENDIF

   END ASSOCIATE
   RETURN
end subroutine FAC3WW

!****************************************************************

SUBROUTINE SWLTA ( AC2   , DEP2  , CGO   , SPCSIG,&
&IMATRA, IMATDA, REDC0 , REDC1 ,&
&IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
&SMEBRK, PLTRI , URSELL, BIPHAS, QTL2, TRIADS ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_stencil, ONLY: MICMAX
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE swan_test_output

   IMPLICIT NONE(TYPE, EXTERNAL)
   INTEGER, INTENT(IN) :: IGP
   TYPE(triad_state_t), INTENT(IN) :: TRIADS


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
!     40.56: Marcel Zijlema
!     40.85: Marcel Zijlema
!     41.44: James Salmon, Pieter Smit
!     42.01: Marcel Zijlema
!     42.11: Ad Reniers
!
!  1. Updates
!
!     40.56, Feb. 06: New subroutine
!     40.85, Aug. 08: store triads for output purposes
!     41.44, Oct. 13: include consistent collinear approximation (CCA)
!     42.01, Nov. 22: directional integration for CCA made tunable
!     42.11, May  24: add interaction at third harmonic
!
!  2. Purpose
!
!     In this subroutine the triad-wave interactions are calculated
!     with the Lumped Triad Approximation of Eldeberky (1996). His
!     expression is based on a parametrization of the biphase (as
!     function of the Ursell number), is directionally uncoupled and
!     takes into account for the self-self interactions only.
!
!     For a full description of the equations reference is made
!     to PhD thesis of Eldeberky (1996). Here only the main expressions
!     are given.
!
!  3. Method
!
!     The source term as function of frequency p is:
!
!                          +      -
!     S(p) = alpha Cg,p [ S(p) + S(p) ]
!
!     in which
!
!      +
!     S(p) = Cp (R(p/2,p/2))**2 sin (-beta) ( E(p/2)**2 -2 E(p) E(p/2) )
!
!     and
!
!      -          +
!     S(p) = - 2 S(2p)
!
!     with alpha a tunable coefficient, beta a parametrized biphase and
!     R(p/2,p/2) is the interaction coefficient
!
!     The biphase is computed in routine SINTGRL
!
!     The interaction coefficient is computed in routine TCOEF
!
!     Note that a slightly adapted formulation of the LTA is used in
!     in the SWAN model:
!
!     - Only positive contributions to higher harmonics are considered
!       here (no energy is transferred to lower harmonics).
!
!     - The mean frequency in the expression of the Ursell number
!       is calculated according to the first order moment over the
!       zeroth order moment (personal communication, Y.Eldeberky, 1997).
!
!     - The interactions are calculated up to 2.5 times the mean
!       frequency only.
!
!     - The consistent collinear approximation (CCA) of Salmon et al (2016)
!       is applied. The directional integration as given by their Eq. 13
!       is determined by a tunable parameter.
!
!     - Since the spectral grid is logarithmically distributed in frequency
!       space, the interactions between central bin and interacting bin
!       are interpolated such that the distance between these bins is
!       factor 2 (nearly).
!
!     - The interactions are calculated in terms of energy density
!       instead of action density. So the action density spectrum
!       is firstly converted to the energy density grid, then the
!       interactions are calculated and then the spectrum is converted
!       to the action density spectrum back.
!
!     - To ensure numerical stability the Patankar rule is used.
!
!  4. Argument variables
!
!     AC2         action density
!     BIPHAS      parameterized biphase of the spectrum
!     CGO         group velocity
!     DEP2        water depth
!     IDCMIN      minimum counter in directional space
!     IDCMAX      maximum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMATDA      main diagonal of the linear system
!     IMATRA      right-hand side of system of equations
!     ISSTOP      maximum frequency counter in a sweep
!     PLTRI       triad contribution in TEST points
!     QTL2        frequency-dependent scaling factor
!     REDC0       explicit part of energy redistribution for output purposes
!     REDC1       implicit part of energy redistribution for output purposes
!     SMEBRK      average (angular) frequency
!     SPCSIG      relative frequencies in computational domain in sigma-space
!     URSELL      Ursell number

   INTEGER IDDLOW, IDDTOP, ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)

   REAL :: SMEBRK
   REAL :: AC2(MDC,MSC,MCGRD)

   REAL :: DEP2(MCGRD)
   REAL :: IMATDA(MDC,MSC), IMATRA(MDC,MSC)
   REAL :: SPCSIG(MSC)
   REAL :: CGO(MSC,MICMAX)
   REAL :: PLTRI(MDC,MSC,NPTST)
   REAL :: URSELL(MCGRD)
   REAL :: BIPHAS(MCGRD)
   REAL :: QTL2(MSC,2)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)

!  6. Local variables
!
!     BIPH  :     local biphase
!     CG    :     local group velocity
!     DEP   :     water depth
!     E     :     energy density as function of frequency
!     E0    :     energy density of bound super harmonic (=p)
!     ED    :     integral energy density over directions
!     ED0   :     integral energy density over directions of harmonic p
!     EDM   :     integral energy density over directions of second
!                 primary harmonic (m)
!     EDPM  :     integral energy density over directions of first
!                 primary harmonic (p-m)
!     EEx   :     quadratic products of energy density
!     EM    :     energy density of second primary harmonic (m)
!     EPM   :     energy density of first primary harmonic (p-m)
!     FT    :     multiplication factor for triad contribution
!     ID    :     counter
!     ID1   :     first directional index
!     ID2   :     last directional index
!     IDD   :     another counter
!     IDDUM :     loop counter in direction space
!     IDW   :     directional range / 2
!     IENT  :     number of entries
!     II    :     loop counter
!     IS    :     loop counter in frequency space
!     ISMAX :     maximum of the counter in frequency space for
!                 which the triad interactions are calculated (cut-off)
!     PWDTH :     integral range in rad. / 2
!     SA    :     contribution of triad self interaction
!     SA3   :     contribution of triad sum interaction
!     SIGPI :     frequency times 2pi
!     SINBPH:     sine of biphase
!     STRI  :     total triad contribution

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, ID1, ID2, IDD, IDDUM, IDW, II, IS, ISMAX
   REAL    BIPH, CG, DEP, E0, ED0, EDM, EDPM, EE1, EE2, EE3, EM, EPM,&
   &FT, PWDTH, SIGPI, SINBPH, STRI
   REAL    E(MSC), ED(MSC),&
   &SA(MDC,MSC+TRIADS%upper_index_next(2)),&
   &SA3(MDC,MSC+TRIADS%upper_index_next(3))

!  9. Subroutines calling
!
!     SOURCE
!
! 12. Structure
!
!     Determine resonance condition and the maximum discrete freq.
!     for which the interactions are calculated.
!
!     If Ursell number larger than prescribed value compute interactions
!        determine biphase
!        Do for each direction
!           Convert action density to energy density
!           Do for all frequencies
!             Calculate interaction coefficient and interaction factor
!             Compute interactions and store results in matrix
!
! 13. Source text

   ASSOCIATE(ISM => TRIADS%lower_index,&
   &ISM1 => TRIADS%lower_index_next,&
   &ISP => TRIADS%upper_index,&
   &ISP1 => TRIADS%upper_index_next,&
   &WISM => TRIADS%lower_weight,&
   &WISM1 => TRIADS%lower_weight_next,&
   &WISP => TRIADS%upper_weight,&
   &WISP1 => TRIADS%upper_weight_next)
   IF (LTRACE) CALL STRACE (IENT,'SWLTA')

   DEP  = DEP2  (IGP)
   BIPH = BIPHAS(IGP)

   CG  = 1.

   E   = 0.
   ED  = 0.
   SA  = 0.
   SA3 = 0.

!     --- compute maximum frequency for which interactions are calculated

   ISMAX = 1
   DO IS = 1, MSC
      IF ( SPCSIG(IS) .LT. PTRIAD(2) * SMEBRK ) THEN
         ISMAX = IS
      ENDIF
   ENDDO
   ISMAX = MIN ( ISMAX, MIN( MSC+ISP1(2),MSC+ISP1(3) ) )
   IF (ITRIAD.EQ.1 .AND. .NOT.PTRIAD(2).NE.-1.) ISMAX = MSC

!     --- compute 3 wave-wave interactions

   IF ( .NOT.URSELL(IGP).LT.PTRIAD(5) ) THEN

!        --- determine sine of biphase

      SINBPH = SIN(-BIPH)

!        --- determine directional range for CCA integration

      IF ( PTRIAD(8).NE.-1. ) THEN
         PWDTH = PTRIAD(8) * PI/180.
         IDW = NINT(PWDTH/(2.*DDIR))
      ELSE
!           full directional integration
         IDW = -1
      ENDIF

!        --- calculate integral of E(f,t) over all directions, if desired
      IF ( IDW.EQ.-1 ) THEN
         ED(:) = SUM(AC2(:,:,IGP),DIM=1) * 2.*PI*SPCSIG(:) *DDIR
      ENDIF

      DO II = IDDLOW, IDDTOP
         ID = MOD ( II - 1 + MDC , MDC ) + 1

!           --- initialize array with E(f) for the direction theta considered

         E(:) = AC2(ID,:,IGP) * 2. * PI * SPCSIG(:)

!           --- integrate E(f,t) over range dir-p <= theta <= dir+p

         IF ( IDW.NE.-1 ) THEN
            ID1 = II - IDW
            ID2 = II + IDW
            IF ( .NOT.FULCIR ) THEN
               ID1 = MAX(ID1,  1)
               ID2 = MIN(ID2,MDC)
            ENDIF
            ED(:) = 0.
            DO IDDUM = ID1, ID2
               IDD = MOD( IDDUM - 1 + MDC , MDC ) + 1
               ED(:) = ED(:) + AC2(IDD,:,IGP)
            ENDDO
            ED(:) = ED(:) * 2. * PI * SPCSIG(:)
            IF ( IDW.NE.0 ) ED = ED * DDIR
         ENDIF

!           --- compute LTA contribution

         DO IS = 1, ISMAX

!              --- bound super harmonic
            E0  = E (IS)
            ED0 = ED(IS)

!              --- primary wave (self interaction)
            IF ( IS.GT.-ISM1(2,1) ) THEN
               EM  = WISM (2,1) * E (IS+ISM1(2,1)) +&
               &WISM1(2,1) * E (IS+ISM (2,1))
               EDM = WISM (2,1) * ED(IS+ISM1(2,1)) +&
               &WISM1(2,1) * ED(IS+ISM (2,1))
            ELSE
               EM  = 0.
               EDM = 0.
            END IF

!              --- compute contribution
!                  (improved collinear approximation)

            FT = QTL2(IS,1) * SINBPH

            SA(ID,IS) = MAX(0., FT * ( EDM * (EM - E0) - ED0 * EM ))

!              --- primary wave (sum interaction)
            IF ( IS.GT.-ISM1(3,1) ) THEN
               EPM  = WISM (3,1) * E (IS+ISM1(3,1)) +&
               &WISM1(3,1) * E (IS+ISM (3,1))
               EDPM = WISM (3,1) * ED(IS+ISM1(3,1)) +&
               &WISM1(3,1) * ED(IS+ISM (3,1))
            ELSE
               EPM  = 0.
               EDPM = 0.
            END IF

!              --- secondary wave (sum interaction)
            IF ( IS.GT.-ISM1(3,2) ) THEN
               EM  = WISM (3,2) * E (IS+ISM1(3,2)) +&
               &WISM1(3,2) * E (IS+ISM (3,2))
               EDM = WISM (3,2) * ED(IS+ISM1(3,2)) +&
               &WISM1(3,2) * ED(IS+ISM (3,2))
            ELSE
               EM  = 0.
               EDM = 0.
            END IF

!              --- compute quadratic products of energy density
!                  (improved collinear approximation)

            EE1 = 0.5 * ( EM*EDPM + EDM*EPM )
            EE2 = 0.5 * ( E0*EDPM + ED0*EPM )
            EE3 = 0.5 * ( EM*ED0  + EDM*E0  )

!              --- compute contribution

            FT = QTL2(IS,2) * SINBPH
            IF (ITRIAD.EQ.11) FT = 0.

            SA3(ID,IS) = MAX(0., FT * ( EE1 - EE2 - EE3 ))

         END DO
      END DO

!         ---  put source term together

      DO IS = 1, ISSTOP
         SIGPI = SPCSIG(IS) * 2. * PI
         IF (ITRIAD.NE.11) CG = CGO(IS,1)
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

!              --- self interaction
            STRI = SA(ID,IS) - 2.*(WISP (2) * SA(ID,IS+ISP1(2)) +&
            &WISP1(2) * SA(ID,IS+ISP (2)))

!              --- interaction at third harmonic
            STRI = STRI +&
            &SA3(ID,IS) - 2.*(WISP (3) * SA3(ID,IS+ISP1(3)) +&
            &WISP1(3) * SA3(ID,IS+ISP (3)))

!              --- make energy flux conservative
            STRI = CG * STRI

!              --- store results in rhs and main diagonal according
!                  to Patankar-rules

            IF(TESTFL) PLTRI(ID,IS,IPTST) = STRI / SIGPI
            IF (STRI.GT.0.) THEN
               IMATRA(ID,IS) = IMATRA(ID,IS) + STRI / SIGPI
               REDC0(ID,IS,2)= REDC0(ID,IS,2)+ STRI / SIGPI
            ELSE
               IMATDA(ID,IS) = IMATDA(ID,IS) - STRI /&
               &MAX(1.E-18,AC2(ID,IS,IGP)*SIGPI)
               REDC1(ID,IS,2)= REDC1(ID,IS,2)+ STRI /&
               &MAX(1.E-18,AC2(ID,IS,IGP)*SIGPI)
            END IF
         END DO
      END DO

   END IF

!     --- test output

   IF ( ITEST .GE. 5 .AND. TESTFL ) THEN
      WRITE(PRINTF,"(' SWLTA: KCGRD ISMAX :',2I4)") IGP, ISMAX
      WRITE(PRINTF,"(' SWLTA: G DEP :',2E12.4)") GRAV, DEP
      WRITE(PRINTF,"(' SWLTA: P(1) P(2) P4) URSELL :',4E12.4)") PTRIAD(1), PTRIAD(2), URSELL(IGP)
      WRITE(PRINTF,"(' SWLTA: SMEBRK B SIN(-B) :',3E12.4)") SMEBRK, BIPH, SIN(-BIPH)
   END IF

   END ASSOCIATE
   RETURN
end subroutine SWLTA

!****************************************************************

SUBROUTINE SWDCTA ( AC2   , DEP2  , CGO   , SPCSIG,&
&IMATRA, IMATDA, REDC0 , REDC1 ,&
&IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
&SIGM  , PLTRI , URSELL, BIPHAS,&
&QTL1  , QTL2  ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_stencil, ONLY: MICMAX
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
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
!     40.45: Nico Booij
!     40.96: Matthijs Benit
!     41.45: James Salmon
!     42.01: Marcel Zijlema
!
!  1. Updates
!
!     40.45,       July 04: new subroutine
!     41.45,  October 2013: energy conservative form
!     42.01, November 2022: code revised
!
!  2. Purpose
!
!     the triad wave-wave interactions are calculated with
!     the distributed collinear triad approximation (DCTA)
!     as described in Booij et al (2009)
!
!  3. Method
!
!     Transfer of energy between two components under influence
!     of a third one is formulated as follows:
!
!                                           p          p
!     S    (f ,t) = T * sum R * E  * ( cg  k  E - cg  k  E  ) df
!      nl3   1                   3       2  2  2    1  1  1     2
!
!     in which
!
!     T  is a dimensional empirical coefficient
!     R  is a scaling factor depending on depth
!     E  is energy density
!     cg is group velocity
!     k  is wave number
!     p  is a shape coefficient to force the high-frequency tail
!
!     Note that factor T is a heuristically determined coefficient that
!     on the group velocity, water depth, mean frequency and mean wave number
!
!     Note that the interactions are calculated in terms of energy
!     density instead of action density
!
!  4. Argument variables
!
!     AC2         action density
!     BIPHAS      parameterized biphase of the spectrum
!     CGO         group velocity
!     DEP2        water depth
!     IDCMIN      minimum counter in directional space
!     IDCMAX      maximum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMATDA      main diagonal of the linear system
!     IMATRA      right-hand side of system of equations
!     ISSTOP      maximum frequency counter in a sweep
!     PLTRI       triad contribution in TEST points
!     QTL1        frequency-dependent interpolation factors
!     QTL2        frequency-dependent scaling factors
!     REDC0       explicit part of energy redistribution for output purposes
!     REDC1       implicit part of energy redistribution for output purposes
!     SIGM        mean angular frequency
!     SPCSIG      relative frequencies in computational domain in sigma-space
!     URSELL      Ursell number

   INTEGER, INTENT(IN) :: IGP
   INTEGER IDDLOW, IDDTOP, ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)

   REAL :: SIGM
   REAL :: AC2(MDC,MSC,MCGRD)
   REAL :: CGO(MSC,MICMAX)
   REAL :: DEP2(MCGRD)
   REAL :: IMATDA(MDC,MSC), IMATRA(MDC,MSC)
   REAL :: SPCSIG(MSC)
   REAL :: PLTRI(MDC,MSC,NPTST)
   REAL :: URSELL(MCGRD)
   REAL :: BIPHAS(MCGRD)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)
   REAL :: QTL1(:,:), QTL2(:,:)

!  6. Local variables
!
!     BETA  :     proportionality factor for DCTA
!     BIPH  :     local biphase
!     CG    :     local group velocity
!     DEP   :     water depth
!     E     :     energy density as function of frequency
!     E1    :     energy density at first frequency bin
!     E2    :     energy density at second frequency bin
!     E3    :     energy density at third interpolated bin
!     FT    :     auxiliary factor
!     FT2   :     = FT * FT
!     ID    :     counter
!     IDDUM :     loop counter in direction space
!     IENT  :     number of entries
!     IS    :     loop counter
!     IS1   :     first loop counter in frequency space
!     IS2   :     second loop counter in frequency space
!     IS3   :     frequency index of third component
!     J     :     counter
!     KM    :     mean wave number
!     P     :     shape coefficient (=4/3)
!     SAN   :     negative contribution of triad interaction
!     SAP   :     positive contribution of triad interaction
!     SIG1  :     frequency of first component
!     SIG2  :     frequency of second component
!     SIG3  :     frequency of third component
!     SINBPH:     sine of biphase
!     STRI  :     total triad contribution
!     STRI1 :     triad contribution related to E1
!     STRI2 :     triad contribution related to E2
!     WIS   :     interpolation weight factor

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IDDUM, IS, IS1, IS2, IS3, J
   REAL    BETA, BIPH, CG, DEP, E1, E2, E3, FT, FT2,&
   &KM, P, SIG1, SIG2, SIG3, SINBPH,&
   &STRI, STRI1, STRI2, WIS
   REAL    E(MSC), SAN(MDC,MSC), SAP(MDC,MSC)

! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWDCTA')

   DEP  = DEP2(IGP)
   BIPH = BIPHAS(IGP)
   P    = PTRIAD(2)

   E   = 0.
   SAN = 0.
   SAP = 0.

!     --- compute 3 wave-wave interactions

   IF ( .NOT.URSELL(IGP).LT.PTRIAD(5) ) THEN

!       --- determine sine of biphase

      SINBPH = SIN(-BIPH)

!       --- scaling factor

      KM   = SIGM / SQRT(GRAV*DEP)
      FT   = DEP * SIGM
      FT2  = FT * FT
      BETA = PTRIAD(1) / FT2 * SINBPH * KM**(2.-P)

      DO IDDUM = IDDLOW, IDDTOP
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

!          --- compute E(sigma) for each direction

         E(:) = AC2(ID,:,IGP) * SPCSIG(:)

         J = 0

!          --- compute interactions based on DCTA

         DO IS1 = 1, MSC

!             --- get first wave component

            SIG1 = SPCSIG(IS1)
            E1   = E     (IS1)

            DO IS2 = IS1+1, MSC

!                --- get second wave component
!                    note: only difference interactions included

               SIG2 = SPCSIG(IS2)
               E2   = E     (IS2)

!                --- determine third component by means of quasi-resonance condition
               SIG3 = SIG2 - SIG1

               J = J + 1

               IF ( SIG3.GT.SPCSIG(1) ) THEN

!                   --- obtain third energy density by means of interpolation

                  WIS = QTL1(J,1)
                  IS3 = INT(QTL1(J,2))

                  E3 = (1.-WIS) * E(IS3) + WIS * E(IS3+1)

!                   --- assemble the triad contributions

                  STRI1 = QTL2(J,1) * E3 * E1
                  STRI2 = QTL2(J,2) * E3 * E2

                  SAN(ID,IS1) = SAN(ID,IS1) + STRI1 * FRINTF * SIG2
                  SAP(ID,IS1) = SAP(ID,IS1) + STRI2 * FRINTF * SIG2

!                   --- to include sum interactions as well

                  SAN(ID,IS2) = SAN(ID,IS2) + STRI2 * FRINTF * SIG1
                  SAP(ID,IS2) = SAP(ID,IS2) + STRI1 * FRINTF * SIG1

               ENDIF

            ENDDO
         ENDDO
      ENDDO

!       --- store results in rhs and main diagonal according
!           to Patankar-rules

      DO IS = 1, ISSTOP
         CG = CGO(IS,1)
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

            STRI = BETA * CG * ( SAP(ID,IS) - SAN(ID,IS) )
            IF(TESTFL) PLTRI(ID,IS,IPTST) = STRI

            STRI = BETA * CG * SAP(ID,IS)
            IMATRA(ID,IS)  = IMATRA(ID,IS)  + STRI
            REDC0(ID,IS,2) = REDC0(ID,IS,2) + STRI

            STRI = BETA * CG * SAN(ID,IS) /&
            &MAX(1.E-18,AC2(ID,IS,IGP))
            IMATDA(ID,IS)  = IMATDA(ID,IS)  + STRI
            REDC1(ID,IS,2) = REDC1(ID,IS,2) - STRI

         ENDDO
      ENDDO

   ENDIF

   RETURN
end subroutine SWDCTA

!******************************************************************

SUBROUTINE SWDNCTA ( AC2   , DEP2  , CGO   , SPCSIG, SPCDIR,&
&KWAVE , IMATRA, IMATDA, REDC0 , REDC1 ,&
&IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
&ETOT  , SIGM  , PLTRI , URSELL, BIPHAS,&
&QTL1  , QTL2  ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!******************************************************************

   USE swan_diagnostics_level
   USE swan_stencil, ONLY: MICMAX
   USE swan_physics_selection
   USE swan_physical_settings
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
!     42.01: Matthijs Benit, Ad Reniers, Marcel Zijlema
!
!  1. Updates
!
!     42.01, November 2022: New subroutine
!
!  2. Purpose
!
!     the triad wave-wave interactions are calculated with
!     the distributed collinear triad approximation (DCTA)
!     as described in Booij et al (2009), and additionally,
!     the effect of noncollinear interactions is included
!
!     Note: this method is referred to as DNCTA
!           (Distributed NonCollinear Triad Approximation)
!
!  3. Method
!
!     Transfer of energy between two components under influence
!     of a third one is formulated as follows:
!
!                                            p          p
!     S    (f ,t ) = T * sum R * E  * ( cg  k  E - cg  k  E  ) df dt
!      nl3   1  1                 3       2  2  2    1  1  1     2  2
!
!     in which
!
!     T  is a dimensional empirical coefficient
!     R  is a scaling factor depending on depth and angle
!        difference between second and third component
!     E  is energy density
!     cg is group velocity
!     k  is wave number
!     p  is a shape coefficient to force the high-frequency tail
!
!     Note that factor T is a heuristically determined coefficient that
!     on the group velocity, water depth, mean frequency and mean wave number
!
!     Note that the interactions are calculated in terms of energy
!     density instead of action density
!
!  4. Argument variables
!
!     AC2         action density
!     BIPHAS      parameterized biphase of the spectrum
!     CGO         group velocity
!     DEP2        water depth
!     ETOT        total energy in grid point
!     IDCMIN      minimum counter in directional space
!     IDCMAX      maximum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMATDA      main diagonal of the linear system
!     IMATRA      right-hand side of system of equations
!     ISSTOP      maximum frequency counter in a sweep
!     KWAVE       wave number
!     PLTRI       triad contribution in TEST points
!     QTL1        frequency-dependent interpolation factors
!     QTL2        frequency-dependent scaling factors
!     REDC0       explicit part of energy redistribution for output purposes
!     REDC1       implicit part of energy redistribution for output purposes
!     SIGM        mean angular frequency
!     SPCDIR      (*,1); spectral directions (radians)
!                 (*,2); cosine of spectral directions
!                 (*,3); sine of spectral directions
!                 (*,4); cosine^2 of spectral directions
!                 (*,5); cosine*sine of spectral directions
!                 (*,6); sine^2 of spectral directions
!     SPCSIG      relative frequencies in computational domain in sigma-space
!     URSELL      Ursell number

   INTEGER, INTENT(IN) :: IGP
   INTEGER IDDLOW, IDDTOP, ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)

   REAL :: ETOT, SIGM
   REAL :: AC2(MDC,MSC,MCGRD)
   REAL :: CGO(MSC,MICMAX)
   REAL :: DEP2(MCGRD)
   REAL :: IMATDA(MDC,MSC), IMATRA(MDC,MSC)
   REAL :: SPCSIG(MSC), SPCDIR(MDC,6)
   REAL :: KWAVE(MSC,MICMAX)
   REAL :: PLTRI(MDC,MSC,NPTST)
   REAL :: URSELL(MCGRD)
   REAL :: BIPHAS(MCGRD)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)
   REAL :: QTL1(:,:), QTL2(:,:)

!  6. Local variables
!
!     BETA  :     proportionality factor for DCTA
!     BIPH  :     local biphase
!     CG    :     local group velocity
!     COS12 :     = cos(th1-th2)
!     COS23 :     = cos(th2-th3)
!     DEP   :     water depth
!     DS2DD :     = dsigma2 * ddir
!     E     :     energy density
!     E1    :     energy density at first frequency bin
!     E2    :     energy density at second frequency bin
!     E3    :     energy density at third interpolated bin
!     ECOS1 :     cosine of first spectral direction
!     ECOS2 :     cosine of second spectral direction
!     ESIN1 :     sine of first spectral direction
!     ESIN2 :     sine of second spectral direction
!     ETRSH :     threshold to exclude very small contributions
!     FT    :     auxiliary factor
!     FT2   :     = FT * FT
!     I3    :     directional index of third component
!     ID    :     counter
!     ID1   :     first loop counter in directional space
!     ID2   :     second loop counter in directional space
!     ID3   :     directional index of third component
!     ID3P  :     = ID3 + 1
!     IDDUM :     loop counter in direction space
!     IDP   :     broken directional index for third component
!     IENT  :     number of entries
!     IS    :     loop counter
!     IS1   :     first loop counter in frequency space
!     IS2   :     second loop counter in frequency space
!     IS3   :     frequency index of third component
!     J     :     counter
!     K1    :     wave number at first frequency bin
!     K2    :     wave number at second frequency bin
!     K3    :     wave number at third interpolated bin
!     K12   :     wave number of difference wave number
!                 vector k1 - k2
!     KM    :     mean wave number
!     OUTSID:     indicates if interpolated value is/is not in grid
!     P     :     shape coefficient (=4/3)
!     SAN   :     negative contribution of triad interaction
!     SAP   :     positive contribution of triad interaction
!     SIG1  :     frequency of first component
!     SIG2  :     frequency of second component
!     SIG3  :     frequency of third component
!     SINBPH:     sine of biphase
!     SIN12 :     = sin(th1-th2)
!     STRI  :     total triad contribution
!     STRI1 :     triad contribution related to E1
!     STRI2 :     triad contribution related to E2
!     TH1   :     direction of first component
!     TH2   :     direction of second component
!     TH3   :     direction of third component
!     WID   :     interpolation weight factor in dir space
!     WIS   :     interpolation weight factor in freq space

   INTEGER I3, ID, ID1, ID2, ID3, IDDUM, ID3P, IS, IS1, IS2, IS3, J
   REAL    BETA, BIPH, CG, COS12, COS23, DEP, DS2DD,&
   &E1, E2, E3, ECOS1, ECOS2, ESIN1, ESIN2, ETRSH, FT, FT2,&
   &IDP, K1, K2, K3, K12, KM, P, SIG1, SIG2, SIG3,&
   &SIN12, SINBPH, STRI, STRI1, STRI2, TH1, TH2, TH3,&
   &WIS, WID
   REAL    E(MDC,MSC), SAN(MDC,MSC), SAP(MDC,MSC)
   LOGICAL OUTSID

!  9. Subroutines calling
!
!     SOURCE
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT,'SWDNCTA')

   DEP   = DEP2(IGP)
   BIPH  = BIPHAS(IGP)
   P     = PTRIAD(2)
   ETRSH = 1.E-4 * ETOT

   SAN = 0.
   SAP = 0.

!     --- consider energy densities

   DO ID = 1, MDC
      E(ID,:) = AC2(ID,:,IGP) * SPCSIG(:)
   END DO

!     --- compute 3 wave-wave interactions

   IF ( .NOT.URSELL(IGP).LT.PTRIAD(5) ) THEN

!       --- determine sine of biphase

      SINBPH = SIN(-BIPH)

!       --- scaling factor

      KM   = SIGM / SQRT(GRAV*DEP)
      FT   = DEP * SIGM
      FT2  = FT * FT
      BETA = PTRIAD(1) / FT2 * SINBPH * KM**(2.-P)

      DO IDDUM = IDDLOW, IDDTOP
         ID1 = MOD ( IDDUM - 1 + MDC , MDC ) + 1

         J = 0

         DO IS1 = 1, MSC

!             --- get first wave component

            SIG1 = SPCSIG(IS1)
            K1   = KWAVE (IS1,1  )
            E1   = E     (ID1,IS1)

            TH1   = SPCDIR(ID1,1)
            ECOS1 = SPCDIR(ID1,2)
            ESIN1 = SPCDIR(ID1,3)

            DO IS2 = 1, MSC

               J = J + 1

               DO ID2 = 1, MDC

!                   --- get second wave component

                  SIG2 = SPCSIG(IS2)
                  K2   = KWAVE (IS2,1  )
                  E2   = E     (ID2,IS2)

                  TH2   = SPCDIR(ID2,1)
                  ECOS2 = SPCDIR(ID2,2)
                  ESIN2 = SPCDIR(ID2,3)

!                   --- determine third component by means of quasi-resonance condition
                  SIG3 = ABS(SIG2 - SIG1)

                  IF ( SIG3.GT.SPCSIG(1) ) THEN

!                      --- compute wave number of the sum or difference

                     SIN12 = ESIN1*ECOS2 - ECOS1*ESIN2
                     COS12 = ECOS1*ECOS2 + ESIN1*ESIN2

                     K12 = SQRT(K1*K1 + K2*K2 - 2. * K1 * K2 * COS12)

!                      --- compute the corresponding wave direction

                     IF (SIG2.GT.SIG1) THEN
!                         difference interactions
                        TH3 = TH2 - ASIN(K1/K12*SIN12)
                     ELSE
!                         sum interactions
                        TH3 = TH1 - ASIN(-K2/K12*SIN12)
                     ENDIF

!                      --- obtain third energy density by means of interpolation

                     WIS = QTL1(J,1)
                     IS3 = INT(QTL1(J,2))

                     OUTSID = .FALSE.

                     IF ( FULCIR ) THEN
                        IDP  = ( TH3 - SPCDIR(1,1) ) / DDIR
                        I3   = FLOOR(IDP)
                        WID  = IDP - REAL(I3)
                        ID3  = MOD( I3     + MDC , MDC ) + 1
                        ID3P = MOD( I3 + 1 + MDC , MDC ) + 1
                     ELSE
                        TH3 = MOD( (TH3 + PI2), PI2 )
                        IF ( SPCDIR(1,1).LT.0. .AND.&
                        &TH3.GT.SPCDIR(MDC,1) ) TH3 = TH3 - PI2
                        IDP = ( TH3 - SPCDIR(1,1) ) / DDIR
                        IF ( IDP.LT.0. ) THEN
                           OUTSID = .TRUE.
                        ELSE IF ( IDP.GT.REAL(MDC-1) ) THEN
                           OUTSID = .TRUE.
                        ELSE IF ( .NOT. IDP.NE.REAL(MDC-1) ) THEN
                           ID3 = MDC - 1
                           WID = 1.
                        ELSE
                           ID3 = INT(IDP)
                           WID = IDP - REAL(ID3)
                           ID3 = ID3 + 1
                        ENDIF
                        ID3P = ID3 + 1
                     ENDIF

                     K3 = (1.-WIS)*KWAVE(IS3,1) + WIS*KWAVE(IS3+1,1)

                     IF ( OUTSID ) THEN
                        E3 = -1.
                     ELSE
                        E3 = (1.-WIS)* (1.-WID) * E(ID3 ,IS3  ) +&
                        &(1.-WIS)*     WID  * E(ID3P,IS3  ) +&
                        &WIS * (1.-WID) * E(ID3 ,IS3+1) +&
                        &WIS *     WID  * E(ID3P,IS3+1)
                     ENDIF

                     IF ( E3 * FRINTF * SIG3 * DDIR .GT. ETRSH ) THEN

!                         --- scale with angle difference between
!                             the second and third component using
!                             the transfer function of Sand (1982)

                        COS23 = (K2-K1*COS12) / K12
                        IF (.NOT.SIG2.GT.SIG1) COS23 = -COS23

                        IF (COS23.NE.1.) THEN
                           FT = SANDN(COS23)
                        ELSE
                           FT = 1.
                        ENDIF
                        FT2 = FT * FT

!                         --- assemble the triad contributions

                        STRI1 = QTL2(J,1) * FT2 * E3 * E1
                        STRI2 = QTL2(J,2) * FT2 * E3 * E2

                        DS2DD = FRINTF * SIG2 * DDIR

                        SAN(ID1,IS1) = SAN(ID1,IS1) + STRI1 * DS2DD
                        SAP(ID1,IS1) = SAP(ID1,IS1) + STRI2 * DS2DD

                     ENDIF

                  ENDIF

               ENDDO
            ENDDO

         ENDDO
      ENDDO

!       --- store results in rhs and main diagonal according
!           to Patankar-rules

      DO IS = 1, ISSTOP
         CG = CGO(IS,1)
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

            STRI = BETA * CG * ( SAP(ID,IS) - SAN(ID,IS) )
            IF(TESTFL) PLTRI(ID,IS,IPTST) = STRI

            STRI = BETA * CG * SAP(ID,IS)
            IMATRA(ID,IS)  = IMATRA(ID,IS)  + STRI
            REDC0(ID,IS,2) = REDC0(ID,IS,2) + STRI

            STRI = BETA * CG * SAN(ID,IS) /&
            &MAX(1.E-18,AC2(ID,IS,IGP))
            IMATDA(ID,IS)  = IMATDA(ID,IS)  + STRI
            REDC1(ID,IS,2) = REDC1(ID,IS,2) - STRI

         ENDDO
      ENDDO

   ENDIF

CONTAINS

   REAL FUNCTION SAND( COSNM )

      REAL A, COSNM, DD, DM, DM2, DN, DN2, FAC, KDM, KDN, KH,&
      &KM, KN, KNM, SM, SN, GH, ROOTDG

      ROOTDG = SQRT(DEP/GRAV)

      SM = SIG2 - SIG1
      SN = SIG2

      KM = K3 * SIGN(1.,SM)
      KN = K2

      KDM = KM * DEP
      KDN = KN * DEP

      DM = ROOTDG * SM
      DN = ROOTDG * SN

      DM2 = DM * DM
      DN2 = DN * DN

      DD = DN - DM

      FAC = KDN * KDM * COSNM

      KNM = SQRT(KN*KN + KM*KM - 2.*KN*KM*COSNM)

      A = KNM*DEP
      IF ( A.GT.6 ) THEN
         KH = A
      ELSE
         A  = A*A
         KH = A/(1.+A/(3.+A/(5.+A/(7.+A/(9.+A/11.)))))
      ENDIF

      IF ( DD*DD.NE.KH ) THEN
         GH = ( DD * ( DM*(KDN*KDN - DN2*DN2) - DN*(KDM*KDM - DM2*DM2) )&
         &+ 2.*DD*DD * ( FAC + DN2*DM2 ) ) / (DD*DD - KH)
         SAND = 0.5 * ( (GH - FAC - DM2*DN2) / (DN*DM) + DN2 + DM2 )
      ELSE
         SAND = 0.
      ENDIF

      RETURN
   end function SAND

   REAL FUNCTION SANDN( COSNM )

      REAL A, B, C, COSNM, D, DD, DM, DM2, DN, DN2, E, FAC1, FACA, FACB,&
      &FACC, KDM, KDN, KH1, KHC, KM, KN, KNM1, KNMC, SM, SN,&
      &GH1, GHC, ROOTDG

      ROOTDG = SQRT(DEP/GRAV)

      SM = SIG2 - SIG1
      SN = SIG2

      KM = K3 * SIGN(1.,SM)
      KN = K2

      KDM = KM * DEP
      KDN = KN * DEP

      DM = ROOTDG * SM
      DN = ROOTDG * SN

      DM2 = DM * DM
      DN2 = DN * DN

      DD = DN - DM

      FAC1 = KDN * KDM
      FACC = FAC1 * COSNM

      FACA = KN*KN + KM*KM
      FACB = 2.*KN*KM

      KNM1 = SQRT(FACA - FACB)
      KNMC = SQRT(FACA - FACB*COSNM)

      A = KNM1*DEP
      IF ( A.GT.6 ) THEN
         KH1 = A
      ELSE
         A  = A*A
         KH1 = A/(1.+A/(3.+A/(5.+A/(7.+A/(9.+A/11.)))))
      ENDIF

      A = KNMC*DEP
      IF ( A.GT.6 ) THEN
         KHC = A
      ELSE
         A  = A*A
         KHC = A/(1.+A/(3.+A/(5.+A/(7.+A/(9.+A/11.)))))
      ENDIF

      A = DD * DD

      IF ( A.NE.KH1 .AND. A.NE.KHC ) THEN
         B  = DM2 * DN2
         C  = DM  * DN * (DM2 + DN2) - B
         D  = 2. * A
         E  = DD * ( DM*(KDN*KDN - DN2*DN2) - DN*(KDM*KDM - DM2*DM2) ) +&
         &B*D

         GH1 = ( E + D * FAC1 ) / (A - KH1)
         GHC = ( E + D * FACC ) / (A - KHC)

         SANDN = (GHC - FACC + C) / (GH1 - FAC1 + C)
      ELSE
         SANDN = 0.
      ENDIF

      RETURN
   end function SANDN

end subroutine SWDNCTA

!****************************************************************

SUBROUTINE SWFTIM ( AC2   , SPCSIG,&
&IMATRA, IMATDA, REDC0 , REDC1 ,&
&IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
&PLTRI , URSELL, BIPHAS,&
&QTL1  , QTL2  ,IGP)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_physics_selection
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
!     42.15: Marcel Zijlema
!
!  1. Updates
!
!     42.15, May 2024: New subroutine
!
!  2. Purpose
!
!     Computes triad source term by means of the full triad
!     interaction model (FTIM) with the parametrized bispectrum
!
!  3. Method
!
!     The spatial evolution equation for energy density is
!     expressed in terms of the full integration of the
!     imaginary part of the parametrized bispectrum
!
!     The integration over the entire frequency space is splitted
!     into the sum interaction and the difference interaction
!
!     The parametrization of the bispectrum is related either
!     to the quasi-normal closure with parametrized biphase or
!     the closure hypothesis of Holloway (1980) using a
!     parameter that represents the broadness of the resonance
!     condition
!
!     Both closures express the fourth-order moments of wave
!     energy distribution in terms of the second-order ones
!
!     Only collinear interactions are included
!
!     The consistent collinear approximation (CCA) of
!     Salmon et al (2016) is applied
!
!  4. Argument variables
!
!     AC2         action density
!     BIPHAS      parameterized biphase of the spectrum
!     IDCMIN      minimum counter in directional space
!     IDCMAX      maximum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMATDA      main diagonal of the linear system
!     IMATRA      right-hand side of system of equations
!     ISSTOP      maximum frequency counter in a sweep
!     PLTRI       triad contribution in TEST points
!     QTL1        frequency-dependent interpolation factors
!     QTL2        frequency-dependent scaling factors
!     REDC0       explicit part of energy redistribution for output purposes
!     REDC1       implicit part of energy redistribution for output purposes
!     SPCSIG      relative frequencies in computational domain in sigma-space
!     URSELL      Ursell number

   INTEGER, INTENT(IN) :: IGP
   INTEGER IDDLOW, IDDTOP, ISSTOP
   INTEGER IDCMIN(MSC), IDCMAX(MSC)

   REAL :: AC2(MDC,MSC,MCGRD)
   REAL :: IMATDA(MDC,MSC), IMATRA(MDC,MSC)
   REAL :: SPCSIG(MSC)
   REAL :: PLTRI(MDC,MSC,NPTST)
   REAL :: URSELL(MCGRD)
   REAL :: BIPHAS(MCGRD)
   REAL :: REDC0 (MDC,MSC,MREDS)
   REAL :: REDC1 (MDC,MSC,MREDS)
   REAL :: QTL1(:,:), QTL2(:,:)

!  6. Local variables
!
!     BIPH  :     local biphase
!     CCx   :     the quadratic transfer functions
!     CDIF  :     contribution due to difference interactions
!     CSUM  :     contribution due to sum interactions
!     E     :     energy density as function of frequency
!     E0    :     energy density of bound super harmonic (=p)
!     ED    :     integral energy density over directions
!     ED0   :     integral energy density over directions of harmonic p
!     EDM   :     integral energy density over directions of second
!                 primary harmonic (m)
!     EDPM  :     integral energy density over directions of first
!                 primary harmonic (p-m)
!     EEx   :     quadratic products of energy density
!     EM    :     energy density of second primary harmonic (m)
!     EPM   :     energy density of first primary harmonic (p-m)
!     FT    :     multiplication factor for triad contribution
!     ID    :     counter
!     ID1   :     first directional index
!     ID2   :     last directional index
!     IDD   :     another counter
!     IDDUM :     loop counter in direction space
!     IDW   :     directional range / 2
!     IENT  :     number of entries
!     II    :     loop counter
!     IM    :     frequency counter of second primary harmonic
!     IP    :     frequency counter of bound super harmonic
!     IPM   :     frequency counter of first primary frequency
!     IS    :     loop counter in frequency space
!     J     :     counter
!     PWDTH :     integral range in rad. / 2
!     SIGPI :     Jacobian
!     SINBPH:     sine of biphase
!     STRI  :     total triad contribution
!     W0    :     radian frequency of bound super harmonic (p)
!     WIS   :     interpolation weight factor
!     WM    :     radian frequency of secondary harmonic (m)
!     WPM   :     radian frequency of primary harmonic (p-m)

   INTEGER, SAVE :: IENT = 0
   INTEGER II, J
   INTEGER ID, ID1, ID2, IDD, IDDUM, IDW, IM, IP, IPM, IS
   REAL    BIPH, E0, ED0, EDM, EDPM, EM, EPM, FT,&
   &PWDTH, SIGPI, SINBPH, STRI, W0, WIS, WM, WPM
   REAL    CC1, CC2, CC3, EE1, EE2, EE3
   REAL    E(MSC), ED(MSC), CDIF(MDC,MSC), CSUM(MDC,MSC)

!  9. Subroutines calling
!
!     SOURCE
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWFTIM')

   BIPH = BIPHAS(IGP)

   E    = 0.
   ED   = 0.
   CDIF = 0.
   CSUM = 0.

!     --- compute 3 wave-wave interactions

   IF ( .NOT.URSELL(IGP).LT.PTRIAD(5) ) THEN

!        --- determine sine of biphase, if required

      IF ( ITRIAD.EQ.2 ) THEN
         SINBPH = 1.
      ELSE IF ( ITRIAD.EQ.3 ) THEN
         SINBPH = SIN(-BIPH)
      ENDIF

!        --- determine directional range for CCA integration

      IF ( PTRIAD(8).NE.-1. ) THEN
         PWDTH = PTRIAD(8) * PI/180.
         IDW = NINT(PWDTH/(2.*DDIR))
      ELSE
!           full directional integration
         IDW = -1
      ENDIF

!        --- calculate integral of E(f,t) over all directions, if desired
      IF ( IDW.EQ.-1 ) THEN
         ED(:) = SUM(AC2(:,:,IGP),DIM=1) * 2.*PI*SPCSIG(:) *DDIR
      ENDIF

      DO II = IDDLOW, IDDTOP
         ID = MOD ( II - 1 + MDC , MDC ) + 1

!           --- initialize array with E(f) for the direction theta considered

         E(:) = AC2(ID,:,IGP) * 2. * PI * SPCSIG(:)

!           --- integrate E(f,t) over range dir-p <= theta <= dir+p

         IF ( IDW.NE.-1 ) THEN
            ID1 = II - IDW
            ID2 = II + IDW
            IF ( .NOT.FULCIR ) THEN
               ID1 = MAX(ID1,  1)
               ID2 = MIN(ID2,MDC)
            ENDIF
            ED(:) = 0.
            DO IDDUM = ID1, ID2
               IDD = MOD( IDDUM - 1 + MDC , MDC ) + 1
               ED(:) = ED(:) + AC2(IDD,:,IGP)
            ENDDO
            ED(:) = ED(:) * 2. * PI * SPCSIG(:)
            IF ( IDW.NE.0 ) ED = ED * DDIR
         ENDIF

         J = 0

!           --- compute full triad integration (collinear)

         DO IP = 1, MSC

!              --- bound super harmonic
            E0  = E     (IP)
            ED0 = ED    (IP)
            W0  = SPCSIG(IP)

!              --- sum contribution (m, p - m)

            DO IM = 1, IP-1

!                 --- secondary wave

               EM  = E     (IM)
               EDM = ED    (IM)
               WM  = SPCSIG(IM)

!                 --- primary wave

               WPM = W0 - WM

               J = J + 1

!                 --- obtain primary energy density by means of interpolation

               IF ( WPM.GT.SPCSIG(1) ) THEN

                  WIS = QTL1(J,1)
                  IPM = INT(QTL1(J,2))

                  EPM  = (1.-WIS) * E (IPM) + WIS * E (IPM+1)
                  EDPM = (1.-WIS) * ED(IPM) + WIS * ED(IPM+1)

               ELSE

                  EPM  = 0.
                  EDPM = 0.

               ENDIF

!                 --- compute quadratic products of energy density
!                     (improved collinear approximation)

               EE1 = 0.5 * ( EM*EDPM + EDM*EPM )
               EE2 = 0.5 * ( E0*EDPM + ED0*EPM )
               EE3 = 0.5 * ( EM*ED0  + EDM*E0  )

!                 --- assemble the triad contributions

               FT = QTL2(J,1) * SINBPH

               CC1 = QTL2(J,2)
               CC2 = QTL2(J,3)
               CC3 = QTL2(J,4)

               STRI = CC1*EE1 - CC2*EE2 - CC3*EE3

               CSUM(ID,IP) = CSUM(ID,IP) + FT * STRI

            ENDDO

!              --- difference contribution (p + m, m)

            DO IM = 1, MSC

!                 --- secondary wave

               EM  = E     (IM)
               EDM = ED    (IM)
               WM  = SPCSIG(IM)

!                 --- primary wave

               WPM = W0 + WM

               J = J + 1

!                 --- obtain primary energy density by means of interpolation

               IF ( WPM.LT.SPCSIG(MSC) ) THEN

                  WIS = QTL1(J,1)
                  IPM = INT(QTL1(J,2))

                  EPM  = (1.-WIS) * E (IPM) + WIS * E (IPM+1)
                  EDPM = (1.-WIS) * ED(IPM) + WIS * ED(IPM+1)

               ELSE

                  EPM  = 0.
                  EDPM = 0.

               ENDIF

!                 --- compute quadratic products of energy density
!                     (improved collinear approximation)

               EE1 = 0.5 * ( EM*ED0  + EDM*E0  )
               EE2 = 0.5 * ( E0*EDPM + ED0*EPM )
               EE3 = 0.5 * ( EM*EDPM + EDM*EPM )

!                 --- assemble the triad contributions

               FT = QTL2(J,1) * SINBPH

               CC1 = QTL2(J,2)
               CC2 = QTL2(J,3)
               CC3 = QTL2(J,4)

               STRI = CC1*EE1 - CC2*EE2 - CC3*EE3

               CDIF(ID,IP) = CDIF(ID,IP) + FT * STRI

            ENDDO

         ENDDO

      ENDDO

!        --- put source term together

      DO IS = 1, ISSTOP
         SIGPI = SPCSIG(IS) * 2. * PI
         DO IDDUM = IDCMIN(IS), IDCMAX(IS)
            ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

            STRI = 2.*CSUM(ID,IS) - 4.*CDIF(ID,IS)

!              --- store results in rhs and main diagonal according
!                  to Patankar-rules

            IF (TESTFL) PLTRI(ID,IS,IPTST) = STRI / SIGPI
            IF (STRI.GT.0.) THEN
               IMATRA(ID,IS) = IMATRA(ID,IS) + STRI / SIGPI
               REDC0(ID,IS,2)= REDC0(ID,IS,2)+ STRI / SIGPI
            ELSE
               IMATDA(ID,IS) = IMATDA(ID,IS) - STRI /&
               &MAX(1.E-18,AC2(ID,IS,IGP)*SIGPI)
               REDC1(ID,IS,2)= REDC1(ID,IS,2)+ STRI /&
               &MAX(1.E-18,AC2(ID,IS,IGP)*SIGPI)
            END IF
         END DO
      END DO

   END IF

   RETURN
end subroutine SWFTIM

!****************************************************************

SUBROUTINE PEREXC ( DELL, DEP2, AC2, SPCSIG, RDX, RDY, BOTLV, IGP,&
&KGRD2, KGRD3 )
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_wave_physics, ONLY: KSCIP1

!****************************************************************

   USE swan_diagnostics_level
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants

   IMPLICIT NONE(TYPE, EXTERNAL)
   INTEGER, INTENT(IN) :: IGP, KGRD2, KGRD3


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
!     41.97: Marcel Zijlema
!
!  1. Updates
!
!     41.97, Sep. 22: New subroutine
!
!  2. Purpose
!
!     Computes ratio of distance of maximum wave run to the coast
!     to the spatial period of energy exchange between first and
!     second harmonics
!
!  3. Method
!
!     See Saprykina et al., 2017, Oceanology, vol. 57, 253-264
!
!  4. Argument variables

   REAL, INTENT(IN)  :: AC2(MDC,MSC,MCGRD) ! action densities
   REAL, INTENT(IN)  :: BOTLV(MCGRD)       ! bottom levels
   REAL, INTENT(OUT) :: DELL               ! delta l expressing the
   ! ratio related to periodi
   ! energy exchange
   REAL, INTENT(IN)  :: DEP2(MCGRD)        ! depths at grid points
!  RDX/RDY assumed-size: reached from the unstructured path with a 2-element
!  array; only RDX(1:2) is ever read. See the SWTRCF note in swanser.
   REAL, INTENT(IN)  :: RDX(*),&       ! geometric coeffs for
   &RDY(*)             ! spatial derivatives
   REAL, INTENT(IN)  :: SPCSIG(MSC)        ! relative frequencies

!  6. Local variables

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IS, ISPK
   REAL    CG, DEP, DDDS, DDDX, DDDY, EMAX, ETD,&
   &K1, K2, LB, N, ND, SIG1, SIG2
   REAL    CGA(1), KA(1), NA(1), NDA(1), SIGA(1)

! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'PEREXC')

!     --- determine local depth

   DEP = DEP2(IGP)

!     --- calculate peak frequency

   EMAX = 0.
   ISPK = -1
   DO IS = 1, MSC
      ETD = 0.
      DO ID = 1, MDC
         ETD = ETD + SPCSIG(IS)*AC2(ID,IS,IGP)*DDIR
      ENDDO
      IF ( ETD.GT.EMAX ) THEN
         EMAX = ETD
         ISPK = IS
      ENDIF
   ENDDO

!     --- calculate wave numbers of first and second harmonics

   IF ( ISPK.GT.0 ) THEN
!        first harmonic
      SIG1 = SPCSIG(ISPK)
      SIGA(1) = SIG1
      CALL KSCIP1 (1, SIGA, DEP, KA, CGA, NA, NDA)
      K1 = KA(1)
!        second harmonic
      SIG2 = 2.*SPCSIG(ISPK)
      SIGA(1) = SIG2
      CALL KSCIP1 (1, SIGA, DEP, KA, CGA, NA, NDA)
      K2 = KA(1)
   ELSE
      K1 = 1.
      K2 = 2.*K1 + 1.E-8
   ENDIF

!     --- compute detuning length due to mismatch between
!         first and second harmonics

   LB = MAX ( 1.E-8, PI2 / (K2 - 2.*K1) )

!     --- determine absolute bottom slope

   DDDX =  RDX(1) * (BOTLV(IGP) - BOTLV(KGRD2))&
   &+ RDX(2) * (BOTLV(IGP) - BOTLV(KGRD3))
   DDDY =  RDY(1) * (BOTLV(IGP) - BOTLV(KGRD2))&
   &+ RDY(2) * (BOTLV(IGP) - BOTLV(KGRD3))

   DDDS = -1. * ( DDDX + DDDY )
   DDDS = MAX( 1.E-8, ABS(DDDS) )

!     --- compute delta l

   DELL = ( DEP / DDDS ) / LB

   RETURN
end subroutine PEREXC
!****************************************************************

SUBROUTINE SWBIDW( BIP, AC2, SPCSIG, RDX, RDY, BOTLV, ECOS, ESIN, IGP,&
&KGRD2, KGRD3 )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants

   IMPLICIT NONE(TYPE, EXTERNAL)
   INTEGER, INTENT(IN) :: IGP, KGRD2, KGRD3


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
!     41.97: Floris de Wit, Ad Reniers
!
!  1. Updates
!
!     41.97, November 2022: New subroutine
!
!  2. Purpose
!
!     Computes the biphase based on the parametrization of De Wit (2022)
!
!  3. Method
!
!     The local biphase is computed by means of the lookup tables of
!     the bed slope and the peak period
!
!     The lookup tables are derived on the basis of Figure 4.8 of
!     De Wit, 2022, PhD thesis, Delft University of Technology
!
!     Note that the computed biphase is not scaled with the Ursell
!     number (see routine SINTGRL)
!
!  4. Argument variables

   REAL, INTENT(IN)  :: AC2(MDC,MSC,MCGRD)   ! action densities
   REAL, INTENT(OUT) :: BIP                  ! unscaled biphase
   REAL, INTENT(IN)  :: BOTLV(MCGRD)         ! bottom levels
   REAL, INTENT(IN)  :: ECOS(MDC), ESIN(MDC) ! cos/sin of spectral di
!  RDX/RDY assumed-size: reached from the unstructured path with a 2-element
!  array; only RDX(1:2) is ever read. See the SWTRCF note in swanser.
   REAL, INTENT(IN)  :: RDX(*),&         ! geometric coeffs for
   &RDY(*)               ! spatial derivatives
   REAL, INTENT(IN)  :: SPCSIG(MSC)          ! relative frequencies

!  5. Parameter variables
!
!     IDIM        number of interpolation nodes related to the bed slopes
!     JDIM        number of interpolation nodes related to the peak periods

   INTEGER, PARAMETER :: IDIM = 8
   INTEGER, PARAMETER :: JDIM = 5

!  6. Local variables
!
!     COSDIR    cosine of mean wave direction
!     DDDS      local bed slope in mean wave direction
!     DDDX      bed slope in x-direction
!     DDDY      bed slope in y-direction
!     EEX       x-component of integrated wave energy
!     EEY       y-component of integrated wave energy
!     EMAX      maximum energy in spectrum
!     ET        energy integrated over spectral freq or dir
!     ETOT      total wave energy
!     ID        loop counter over directions
!     IENT      number of entries
!     IK        index of closest given value in array of bed slopes
!     IS        loop counter over frequencies
!     ISPK      index peak frequency
!     JK        index of closest given value in array of peak periods
!     NDPER     lookup table containing wave period nodes
!     NDSLP     lookup table containing bed slope nodes
!     PLBIP     lookup table of parametrized biphase polynomials as
!               function of local bed slope and local peak period
!     POFF      offset value w.r.t. given value in array peak period
!     RSGN      sign of slope
!               = -1; slope is positive
!               = +1; slope is negative
!     SINDIR    sine of mean wave direction
!     SOFF      offset value w.r.t. given value in array bed slopes
!     TP        local peak period
!     WI1       first weight factor related to the bed slope
!     WI2       second weight factor related to the bed slope
!     WJ1       first weight factor related to the peak period
!     WJ2       second weight factor related to the peak period

   INTEGER, SAVE :: IENT = 0
   INTEGER ID, IS, ISPK
   INTEGER IK(1), JK(1)

   REAL    EEX, EEY, EMAX, ET, ETOT, COSDIR, SINDIR
   REAL    DDDX, DDDY, DDDS, RSGN, TP
   REAL, PARAMETER :: NDPER(JDIM) = [1., 6., 8., 10., 12.]
   REAL, PARAMETER :: NDSLP(IDIM) = [0., 0.001, 0.005, 0.01, 0.02, 0.05, 0.111, 0.5]
   REAL, PARAMETER :: PLBIP(IDIM,JDIM) = RESHAPE([&
   &0.,0.1,0.5,0.9,2. ,4. ,4. ,4. ,&
   &0.,0.1,0.4,0.8,1.5,4. ,4. ,4. ,&
   &0.,0.1,0.3,0.6,1.2,3.3,3.3,3.3,&
   &0.,0.1,0.3,0.6,1.2,3.3,3.3,3.3,&
   &0.,0.1,0.3,0.6,1.2,3.3,3.3,3.3], [IDIM, JDIM])
   REAL    POFF(JDIM)
   REAL    SOFF(IDIM)
   REAL    WI1, WI2, WJ1, WJ2

! 13. Source text


   IF (LTRACE) CALL STRACE (IENT,'SWBIDW')

!     --- initialize biphase

   BIP = 0.

!     --- first, calculate the mean wave direction ...

   EEX  = 0.
   EEY  = 0.
   ETOT = 0.
   DO ID = 1, MDC
      ET = 0.
      DO IS = 1, MSC
         ET = ET + SPCSIG(IS)**2 * AC2(ID,IS,IGP)
      ENDDO
      ETOT = ETOT + ET
      EEX  = EEX  + ET * ECOS(ID)
      EEY  = EEY  + ET * ESIN(ID)
   ENDDO

   IF ( ETOT.GT.0. ) THEN
      COSDIR = EEX / ETOT
      SINDIR = EEY / ETOT
   ELSE
!        if no wave direction found, return
      RETURN
   ENDIF

!     ... next, calculate bottom slope in mean wave direction

   DDDX =  RDX(1) * (BOTLV(IGP) - BOTLV(KGRD2))&
   &+ RDX(2) * (BOTLV(IGP) - BOTLV(KGRD3))
   DDDY =  RDY(1) * (BOTLV(IGP) - BOTLV(KGRD2))&
   &+ RDY(2) * (BOTLV(IGP) - BOTLV(KGRD3))

   DDDS = -1. * ( DDDX * COSDIR + DDDY * SINDIR )

!     --- determine sign of the local slope

   RSGN = SIGN(1.,-DDDS)
   DDDS = ABS(DDDS)

!     --- calculate peak period

   EMAX = 0.
   ISPK = -1
   DO IS = 1, MSC
      ET = 0.
      DO ID = 1, MDC
         ET = ET + SPCSIG(IS) * AC2(ID,IS,IGP)
      ENDDO
      IF ( ET.GT.EMAX ) THEN
         EMAX = ET
         ISPK = IS
      ENDIF
   ENDDO
   IF ( ISPK.GT.0 ) THEN
      TP = PI2 / SPCSIG(ISPK)
   ELSE
!        if no peak period found, return
      RETURN
   ENDIF

!     --- search for closest given values of bed slope and peak period

   SOFF = NDSLP - DDDS
   IK = MINLOC(SOFF, MASK=SOFF.GT.0.)

   POFF = NDPER - TP
   JK = MINLOC(POFF, MASK=POFF.GT.0.)

!     --- compute interpolation factors

   IF ( DDDS.LT.NDSLP(1) ) THEN
      IK(1) = 2
      WI2   = 0.
   ELSE IF ( DDDS.GT.NDSLP(IDIM) ) THEN
      IK(1) = IDIM
      WI2   = 1.
   ELSE
      WI2 = (DDDS - NDSLP(IK(1)-1)) / (NDSLP(IK(1))-NDSLP(IK(1)-1))
   ENDIF
   WI1 = 1. - WI2

   IF ( TP.LT.NDPER(1) ) THEN
      JK(1) = 2
      WJ2   = 0.
   ELSE IF ( TP.GT.NDPER(JDIM) ) THEN
      JK(1) = JDIM
      WJ2   = 1.
   ELSE
      WJ2 = (TP - NDPER(JK(1)-1)) / (NDPER(JK(1))-NDPER(JK(1)-1))
   ENDIF
   WJ1 = 1. - WJ2

!     --- determine unscaled biphase based on linear interpolation

   BIP = WI1 * WJ1 * PLBIP(IK(1)-1,JK(1)-1) +&
   &WI1 * WJ2 * PLBIP(IK(1)-1,JK(1)  ) +&
   &WI2 * WJ1 * PLBIP(IK(1)  ,JK(1)-1) +&
   &WI2 * WJ2 * PLBIP(IK(1)  ,JK(1)  )

!     --- sign of biphase is made consistent, i.e. negative for a
!         positive slope and vice versa

   BIP = RSGN * BIP * PI/6.

   RETURN
end subroutine SWBIDW

!****************************************************************

SUBROUTINE SWBIPM( BIPHAS, DEP2, HSIBC, BPHTMP )
   USE swan_service_interfaces, ONLY: STPNOW, STRACE
   USE swan_wave_physics, ONLY: KSCIP1

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind
   USE swan_physics_selection
   USE swan_physical_settings
   USE swan_computational_grid
   USE swan_spectral_grid
   USE swan_math_constants
   USE M_GENARR
   USE M_PARALL
   USE swan_global_grid

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
!     41.97: Ad Reniers, Marcel Zijlema
!
!  1. Updates
!
!     41.97, November 2022: New subroutine
!
!  2. Purpose
!
!     Spatially averages the De Wit's biphase
!
!  4. Argument variables

   REAL   , INTENT(OUT) :: BIPHAS(MCGRD)
   REAL   , INTENT(IN)  :: DEP2(MCGRD)
   REAL   , INTENT(IN)  :: HSIBC(MCGRD)
   REAL   , INTENT(IN)  :: BPHTMP(:)

!  6. Local variables

   INTEGER, SAVE :: IENT = 0
   INTEGER :: I, J, K, ID, IS, IE, JS, JE, MLX, MLY
   INTEGER :: IX, IY, IX1, IX2, IY1, IY2, IXI, IYJ, IND, INDL, ISPK

   REAL    :: DXA, DYA, DEPLOC, EMAX, ET, KW, LW
   REAL    :: SIGMA(1), KWAVE(1), ARR(1)
   REAL    :: LPAR, BETA

! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWBIPM')

   LPAR = PTRIAD(9)

!     --- peak wave length at boundary
   K  = 0
   KW = 0.
   IF (LPAR.NE.0.) THEN
      DO IY = MYC, 1, -1
         DO IX = 1, MXC
            IND = KGRPNT(IX,IY)
            IF ( HSIBC(IND).GT.1.E-25 ) THEN
               K = K + 1
               EMAX = 0.
               ISPK = -1
               DO IS = 1, MSC
                  ET = 0.
                  DO ID = 1, MDC
                     ET = ET + SPCSIG(IS) * AC2(ID,IS,IND)
                  ENDDO
                  IF ( ET.GT.EMAX ) THEN
                     EMAX = ET
                     ISPK = IS
                  ENDIF
               ENDDO
               IF ( ISPK.GT.0 ) THEN
                  SIGMA(1) = SPCSIG(ISPK)
               ELSE
                  SIGMA(1) = SPCSIG(1)
               ENDIF
               DEPLOC = DEP2(IND)
               IF ( DEPLOC.GT.DEPMIN ) THEN
                  CALL KSCIP1(1, SIGMA, DEPLOC, KWAVE)
               ELSE
                  KWAVE(1) = 0.
               ENDIF
               KW = KW + KWAVE(1)
            ENDIF
         ENDDO
      ENDDO
      ! perform global reductions in parallel run
      CALL SWREDUCE( K , 1, SWSUM )
      CALL SWREDUCE( KW, 1, SWSUM )
      IF ( STPNOW() ) RETURN
   ENDIF
   IF (K.GE.1) THEN
      KW = KW / FLOAT(K)
   ELSE
      KW   = 1.
      LPAR = 0.  ! no smoothing...
   ENDIF

   LW = LPAR * PI2 / KW
   IF (ITEST.GE.80.AND.IAMMASTER)&
   &WRITE(PRINTF,'(A,2F12.5)') 'SWBIPM: KWAVE, LW ',KW,LW

!     --- compute computational area for smoothing purposes
   IF (.NOT.ONED) THEN
!        --- first, estimate average step sizes
      IF (OPTG.EQ.1) THEN
!           rectilinear grid
         DXA = DX*COSPC**2 + DY*SINPC**2
         DYA = DX*SINPC**2 + DY*COSPC**2
      ELSEIF (OPTG.EQ.3) THEN
!           curvilinear grid
         DXA = (XCLEN+YCLEN)/FLOAT(MXCGL+MYCGL)
         DYA = DXA
      ELSE
         DXA = 1.
         DYA = 1.
      ENDIF
!        --- next, compute the number of step sizes per
!            peak wave length
      MLX = FLOOR(CEILING(LW/DXA/SQRT(2.))/2.)
      MLY = FLOOR(CEILING(LW/DYA/SQRT(2.))/2.)
      IF (LPAR.NE.0.) THEN
         MLX = MAX(MLX,1)
         MLY = MAX(MLY,1)
      ENDIF
   ELSE
      MLX = FLOOR(CEILING(LW/DX)/2.)
      IF (LPAR.NE.0.) MLX = MAX(MLX,1)
      MLY = 0
   ENDIF
   IF (ITEST.GE.80)&
   &WRITE(PRINTF,'(A,2I4)') '       MLX, MLY = ',MLX, MLY
   IS = -MLX
   IE =  MLX
   JS = -MLY
   JE =  MLY

   IF (OPTG.NE.5) THEN
!        --- 2D smoothing of the De Wit's biphase around
!            each structured internal grid point
      IX1 = 1
      IF (.NOT.LMXF) IX1 = 1+IHALOX
      IX2 = MXC
      IF (.NOT.LMXL) IX2 = MXC-IHALOX
      IY1 = 1
      IF (.NOT.LMYF) IY1 = 1+IHALOY
      IY2 = MYC
      IF (.NOT.LMYL) IY2 = MYC-IHALOY
      DO IY = IY1, IY2
         DO IX = IX1, IX2
            IND = KGRPNT(IX,IY)
            IF ( IND.GT.1 ) THEN
               K = 0
               BETA = 0.
               DO J = JS, JE
                  DO I = IS, IE
                     K = K + 1
                     IXI = MIN(MAX(IX+I,IX1),IX2)
                     IYJ = MIN(MAX(IY+J,IY1),IY2)
                     INDL = KGRPNT(IXI,IYJ)
                     IF (INDL.GT.1) BETA = BETA + BPHTMP(INDL)
                  ENDDO
               ENDDO
!                 --- store the spatially filtered biphase for triads
               IF (K.GE.1) THEN
                  BIPHAS(IND) = BETA / FLOAT(K)
               ELSE
                  BIPHAS(IND) = BPHTMP(IND)
               ENDIF
            ENDIF
         ENDDO
      ENDDO
   ELSE
!        unstructered mesh: remain unfiltered
      BIPHAS = BPHTMP
   ENDIF

!     End of subroutine SWBIPM
   RETURN
end subroutine SWBIPM

end module swan_nonlinear_interactions
