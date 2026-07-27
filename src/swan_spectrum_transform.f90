MODULE swan_spectrum_transform
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: SSHAPE, SINTRP, CHGBAS, GAMMAF, GAMMAF_KERNEL

CONTAINS

SUBROUTINE SSHAPE (ACLOC, SPCSIG, SPCDIR, FSHAPL, DSHAPL)
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_wave_physics, ONLY: KSCIP1
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
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
!            Roeland Ris
!            Roberto Padilla
!     30.73: Nico Booij
!     30.80: Nico Booij
!     30.82: IJsbrand Haagsma
!     40.02: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!     41.99: Marcel Zijlema
!
!  1. Updates
!
!            Dec. 92: new for SWAN
!            Dec. 96: option MEAN freq. introduced see LOGPM
!     30.73, Nov. 97: revised in view of new boundary treatment
!     30.82, Sep. 98: Added error message in case of non-convergence
!     30.80, Oct. 98: correction suggested by Mauro Sclavo, and renames
!                     computation of tail added to improve accuracy
!     30.82, Oct. 98: Updated description of several variables
!     40.02, Oct. 00: Modified test write statement to avoid division by MS=0
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.99, Aug. 22: correction to cos m-model
!
!  2. Purpose
!
!     Calculating of energy density at boundary point (x,y,sigma,theta)
!
!  3. Method (updated...)
!
!     see: M. Yamaguchi: Approximate expressions for integral properties
!          of the JONSWAP spectrum; Proc. JSCE, No. 345/II-1, pp. 149-152,
!          1984.
!
!     computation of mean period: see Swan system documentation
!
!  4. Argument variables
!
!   o ACLOC : Energy density at a point in space
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    ACLOC(MDC,MSC)
   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)

! i   DSHAPL: Directional distribution
! i   FSHAPL: Shape of spectrum:
!             =1; Pierson-Moskowitz spectrum
!             =2; Jonswap spectrum
!             =3; bin
!             =4; Gauss curve
!             =5; TMA
!             (if >0: period is interpreted as peak per.
!              if <0: period is interpreted as mean per.)

   INTEGER FSHAPL, DSHAPL

!  5. Parameter variables
!
!  6. Local variables
!
!     ID       counter of directions
!     IS       counter of frequencies
!     LSHAPE   absolute value of FSHAPL

   INTEGER  ID, IS, LSHAPE

!     PKPER    peak period
!     APSHAP   aux. var. used in computation of spectrum
!     AUX1     auxiliary variable
!     AUX2     auxiliary variable
!     AUX3     auxiliary variable
!     COEFF    coefficient for behaviour around the peak (Jonswap)
!     CPSHAP   aux. var. used in computation of spectrum
!     CTOT     total energy
!     CTOTT    total energy (used for comparison)
!     DD       directional width (in degrees)
!     DIFPER   auxiliary variable used to select bin closest
!              to given frequency
!     MPER
!     MS       power in directional distribution
!     RA       action density
!     SALPHA
!     SF       frequency (Hz)
!     SF4      SF**4
!     SF5      SF**5
!     FPK      frequency corresponding to peak period (1/PKPER)
!     FPK4     FPK**4
!     SYF      peakedness parameter

   REAL     APSHAP, AUX1, AUX2, AUX3
   REAL     COEFF ,SYF   ,MPER  ,CTOT  ,CTOTT,PKPER  ,DIFPER
   REAL     MS
   REAL     RA    ,SALPHA,SF   ,SF4   ,SF5   ,FPK   ,FPK4, FAC
   REAL     DP, K, N, CG, ND
   REAL     CGA(1), KA(1), NA(1), NDA(1), SIGA(1)
   REAL     DD

!     LOGPM    indicates whether peak or mean frequency is used
!     DVERIF   logical used in verification of incident direction

   LOGICAL  LOGPM, DVERIF

!     PSHAPE   coefficients of spectral distribution (see remarks)
!     SPPARM   array containing integral wave parameters (see remarks)
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
! 10. Error messages
!
! 11. Remarks
!
!     PSHAPE(1): SY0, peak enhancement factor (gamma) in Jonswap spectrum
!     PSHAPE(2): spectral width in case of Gauss spectrum in rad/s
!
!     SPPARM    real     input    incident wave parameters (Hs, Period,
!                                 direction, Ms (dir. spread))
!     SPPARM(1): Hs, sign. wave height
!     SPPARM(2): Wave period given by the user (either peak or mean)
!     SPPARM(3): average direction
!     SPPARM(4): directional spread
!
!     ------------------------------------------------------------------
!
!     In the case of a JONSWAP spectrum the initial conditions are given by
!                   _               _       _       _       _
!                  |       _   _ -4  |     |       | S - S   |
!             2    |      |  S  |    |     |       |      p  |
!          a g     |      |  _  |    |  exp|-1/2 * |________ |* 2/pi COS(T-T  )
! E(S,D )= ___  exp|-5/4 *|  S  |    | G   |       | e * S   |
!      wa    5     |      |   p |    |     |_      |_     p _|
!           S      |      |_   _|    |
!                  |_               _|
!
!   where
!         S   : rel. frequency
!
!         D   : Dir. of wave component
!          wa
!
!         a   : equili. range const. (Phillips' constant)
!         g   : gravity acceleration
!
!         S   : Peak frequency
!          p
!
!         G   : Peak enhancement factor
!         e   : Peak width
!
!         T   : local wind direction
!          wi
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       case shape
!       =1:   calculate value of Pierson-Moskowitz spectrum
!       =2:   calculate value of Jonswap spectrum
!       =3:   calculate value of bin spectrum
!       =4:   calculate value of Gauss spectrum
!       =5:   calculate value of TMA spectrum
!       else: Give error message because of wrong shape
!       ----------------------------------------------------------------
!       if LOGPM is True
!       then calculate average period
!            if it differs from given average period
!            then recalculate peak period
!                 restart procedure to compute spectral shape
!       ----------------------------------------------------------------
!       for all spectral bins do
!            multiply all action densities by directional distribution
!       ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER ISP, ITPER, JJ
   REAL ACOS, ADIR, AM0, AM1, APTAIL, AS2, AS3, CDIR, CPSHAP
   REAL DSPR, EPTAIL, ESOM, GAM1, GAM2, HSTMP
   REAL PPSHAP, PPTAIL
   CALL STRACE(IENT,'SSHAPE')

   IF (ITEST.GE.80) WRITE (PRTEST, "(' entry SSHAPE ', 2I3, 4E12.4)") FSHAPL, DSHAPL,&
   &(SPPARM(JJ), JJ = 1,4)
   IF (FSHAPL.LT.0) THEN
      LSHAPE = - FSHAPL
      LOGPM  = .FALSE.
   ELSE
      LSHAPE = FSHAPL
      LOGPM  = .TRUE.
   ENDIF

   IF (SPPARM(1).LE.0.)&
   &CALL MSGERR(1,'sign. wave height at boundary is not positive')

   PKPER = SPPARM(2)
   ITPER = 0
   IF (LSHAPE.EQ.3) THEN
!       select bin closest to given period
      DIFPER = 1.E10
      DO IS = 1, MSC
         IF (ABS(PKPER - PI2/SPCSIG(IS)) .LT. DIFPER) THEN
            ISP = IS
            DIFPER = ABS(PKPER - PI2/SPCSIG(IS))
         ENDIF
      ENDDO
   ENDIF

!     compute spectral shape using peak period PKPER

   FAC  = 1.
   spectrum_iteration: DO
   FPK  = (1./PKPER)
   FPK4 = FPK**4
   IF (LSHAPE.EQ.1) THEN
      SALPHA = ((SPPARM(1) ** 2) * (FPK4)) * 5. / 16.
   ELSE IF (LSHAPE.EQ.2 .OR. LSHAPE.EQ.5) THEN
!       *** SALPHA = alpha*(grav**2)/(2.*pi)**4)
      SALPHA = (SPPARM(1)**2 * FPK4) /&
      &((0.06533*(PSHAPE(1)**0.8015)+0.13467)*16.)
   ELSE IF (LSHAPE.EQ.4) THEN
      AUX1 = SPPARM(1)**2 / ( 16.* SQRT (PI2) * PSHAPE(2))
      AUX3 = 2. * PSHAPE(2)**2
   ENDIF

   CTOTT = 0.
   do IS = 1, MSC

      IF (LSHAPE.EQ.1) THEN
!         *** LSHAPE = 1 : Pierson and Moskowitz ***
         SF = SPCSIG(IS) / PI2
         SF4 = SF**4
         SF5 = SF**5
         RA = (SALPHA/SF5)*EXP(-(5.*FPK4)/(4.*SF4))/(PI2*SPCSIG(IS))
         ACLOC(MDC,IS) = RA
      ELSE IF (LSHAPE.EQ.2 .OR. LSHAPE.EQ.5) THEN
!         *** LSHAPE = 2 : JONSWAP ***
!         *** LSHAPE = 5 : TMA     ***
         SF = SPCSIG(IS)/(PI2)
         SF4 = SF**4
         SF5 = SF**5
         CPSHAP = 1.25 * FPK4 / SF4
         IF (CPSHAP.GT.10.) THEN
            RA = 0.
         ELSE
            RA = (SALPHA/SF5) * EXP(-CPSHAP)
         ENDIF
         IF (LSHAPE.EQ.5) THEN
            DP = PSHAPE(3)
            SIGA(1) = SPCSIG(IS)
            CALL KSCIP1 (1, SIGA, DP, KA, CGA, NA, NDA)
            K = KA(1)
            N = NA(1)
            RA = RA * (TANH(K*DP))**2 / (2.*N)
         ENDIF
         IF (SF .LT. FPK) THEN
            COEFF = 0.07
         ELSE
            COEFF = 0.09
         ENDIF
         APSHAP =  0.5 * ((SF-FPK) / (COEFF*FPK)) **2
         IF (APSHAP.GT.10.) THEN
            SYF = 1.
         ELSE
            PPSHAP = EXP(-APSHAP)
            SYF = PSHAPE(1)**PPSHAP
         ENDIF
         RA = SYF*RA/(SPCSIG(IS)*PI2)
         ACLOC(MDC,IS) = RA
         IF (ITEST.GE.120) WRITE (PRTEST, "(' SSHAPE freq. ', 8E12.4)")&
         &SF, SALPHA, CPSHAP, APSHAP, SYF, RA
      ELSE IF (LSHAPE.EQ.3) THEN

!         *** all energy concentrated in one BIN ***

         IF (IS.EQ.ISP) THEN
            ACLOC(MDC,IS) = ( SPPARM(1)**2 ) /&
            &( 16. * SPCSIG(IS)**2 * FRINTF )
         ELSE
            ACLOC(MDC,IS) = 0.
         ENDIF
      ELSE IF (LSHAPE.EQ.4) THEN

!         *** energy Gaussian distributed (wave-current tests) ***

         AUX2 = ( SPCSIG(IS) - ( PI2 / PKPER ) )**2
         RA = AUX1 * EXP ( -1. * AUX2 / AUX3 ) / SPCSIG(IS)
         ACLOC(MDC,IS) = RA
      ELSE
         IF (IS.EQ.1) THEN
            CALL MSGERR (2,'Wrong type for frequency shape')
            WRITE (PRINTF, *) ' -> ', FSHAPL, LSHAPE
         ENDIF
      ENDIF
      IF (ITEST.GE.10)&
      &CTOTT = CTOTT + FRINTF * ACLOC(MDC,IS) * SPCSIG(IS)**2
   end do
   IF (ITEST.GE.10) THEN
      IF (SPPARM(1).GT.0.01) THEN
         HSTMP = 4. * SQRT(CTOTT)
         IF (ABS(HSTMP-SPPARM(1)) .GT. 0.1*SPPARM(1))&
         &WRITE (PRINTF, "(' SSHAPE, deviation in Hs, should be ', F8.3, ', calculated ', F8.3)") SPPARM(1), HSTMP
      ENDIF
   ENDIF

!     if mean frequency was given recalculate PKPER and restart

   IF (.NOT.LOGPM .AND. ITPER.LT.10) THEN
      ITPER = ITPER + 1
!       calculate average frequency
      AM0 = 0.
      AM1 = 0.
      DO IS = 1, MSC
         AS2 = ACLOC(MDC,IS) * (SPCSIG(IS))**2
         AS3 = AS2 * SPCSIG(IS)
         AM0 = AM0 + AS2
         AM1 = AM1 + AS3
      ENDDO
!       contribution of tail to total energy density
      PPTAIL = PWTAIL(1) - 1.
      APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
      AM0 = AM0 * FRINTF + APTAIL * AS2
      PPTAIL = PWTAIL(1) - 2.
      EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
      AM1 = AM1 * FRINTF + EPTAIL * AS3
!       Mean period:
      IF ( AM1.NE.0. ) THEN
         MPER = PI2 * AM0 / AM1
      ELSE
         CALL MSGERR(3, ' first moment is zero in calculating the')
         CALL MSGERR(3, ' spectrum at boundary using param. bc.')
      END IF
      IF (ITEST.GE.80) WRITE (PRTEST, "(' SSHAPE iter=', I2, ' period values:', 3F7.2)") ITPER, SPPARM(2), MPER,&
      &PKPER
      IF (ABS(MPER-SPPARM(2)) .GT. 0.01*SPPARM(2)) THEN
!         modification suggested by Mauro Sclavo
         PKPER = (SPPARM(2) / MPER) * PKPER
         CYCLE spectrum_iteration
      ENDIF
   ENDIF
   EXIT spectrum_iteration
   END DO spectrum_iteration

   IF (ITPER.GE.10) THEN
      CALL MSGERR(3, 'No convergence calculating the spectrum')
      CALL MSGERR(3, 'at the boundary using parametric bound. cond.')
   ENDIF

!     now introduce distribution over directions

   ADIR = PI * DEGCNV(SPPARM(3)) / 180.
   IF (DSHAPL.EQ.1) THEN
      DD = SPPARM(4)
      IF (DD.GT.23.) THEN
         FAC = 1.2
      ELSEIF (DD.GT.17.) THEN
         FAC = 1.096
      ELSE
         FAC = 1.01
      ENDIF
      DSPR = PI * SPPARM(4) / 180.
      MS = MAX (FAC*DSPR**(-2) - 2., 1.)
   ELSE
      MS = SPPARM(4)
   ENDIF
   IF (MS.LT.12.) THEN
      CTOT = GAMMAF(0.5*MS+1.) / (SQRT(PI) * GAMMAF(0.5*MS+0.5))
   ELSE
      CTOT =  SQRT (0.5*MS/PI) / (1. - 0.25/MS)
   ENDIF
   IF (ITEST.GE.100) THEN
      ESOM = 0.
      DO IS = 1, MSC
         ESOM = ESOM + FRINTF * SPCSIG(IS)**2 * ACLOC(MDC,IS)
      ENDDO
      GAM1 = GAMMAF(0.5*MS+1. )
      GAM2 = GAMMAF(0.5*MS+0.5)
      WRITE (PRTEST, *) ' SSHAPE dir ', 4.*SQRT(ABS(ESOM)),&
      &SPPARM(1), CTOT, MS, GAM1, GAM2, CTOT
   ENDIF
   DVERIF = .FALSE.
   CTOTT = 0.
   DO ID = 1, MDC
      ACOS = COS(SPCDIR(ID,1) - ADIR)
      IF (ACOS .GT. 0.) THEN
         CDIR = CTOT * MAX (ACOS**MS, 1.E-10)
         IF (.NOT.FULCIR) THEN
            IF (ACOS .GE. COS(DDIR)) DVERIF = .TRUE.
         ENDIF
      ELSE
         CDIR = 0.
      ENDIF
      IF (ITEST.GE.10) CTOTT = CTOTT + CDIR * DDIR
      IF (ITEST.GE.100) WRITE (PRTEST, "(' ID Spcdir Cdir: ',I3,3(1X,E10.4))") ID,SPCDIR(ID,1),CDIR
      DO IS = 1, MSC
         ACLOC(ID,IS) = CDIR * ACLOC(MDC,IS)
      ENDDO
   ENDDO
   IF (ITEST.GE.10) THEN
      IF (ABS(CTOTT-1.) .GT. 0.1) WRITE (PRINTF, "(' SSHAPE, integral of Cdir is not 1, but:', F6.3)") CTOTT
   ENDIF
   IF (.NOT.FULCIR .AND. .NOT.DVERIF)&
   &CALL MSGERR (1, 'incident direction is outside sector')

   RETURN

! End of subroutine SSHAPE
end subroutine SSHAPE

SUBROUTINE SINTRP (W1, W2, FL1, FL2, FL, SPCDIR, SPCSIG)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                  *
!*******************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE SWCOMM3


!   --|-----------------------------------------------------------|--
!     |            Delft University of Technology                 |
!     | Faculty of Civil Engineering, Fluid Mechanics Group       |
!     | P.O. Box 5048,  2600 GA  Delft, the Netherlands           |
!     |                                                           |
!     | Authors :  Weimin Luo, Roeland Ris, Nico Booij            |
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
!     30.73: Nico Booij
!     30.82: IJsbrand Haagsma
!     40.00: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.01, Jan. 96: New subroutine for SWAN Ver. 30.01
!     30.73, Nov. 97: revised
!     40.00, Apr. 98: procedure to maintain peakedness introduced
!     30.82, Oct. 98: Update description of several variables
!     30.82, Oct. 98: Made arguments in ATAN2 REAL(KIND=KIND(0.0D0)) to preven
!                     underflows
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     interpolation of spectra
!
!  3. Method (updated...)
!
!     linear interpolation with peakedness maintained
!     interpolated average direction and frequency are determined
!     average direction and frequency of interpolated spectrum are determ.
!     shifts in frequency and direction are determined from spectrum 1 and
!     2 to the interpolated spectrum
!     bilinear interpolation in spectral space is used to calculate
!     contributions from spectrum 1 and 2.
!     in full circle cases interpolation crosses the boundary 0-360 degr.
!
!  4. Argument variables
!
!   o FL    : Interpolated spectrum.
! i   FL1   : Input spectrum 1.
! i   FL2   : Input spectrum 2.
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   W1    : Weighting coefficient for spectrum 1.
! i   W2    : Weighting coefficient for spectrum 2.

   REAL    FL1(MDC,MSC), FL2(MDC,MSC), FL(MDC, MSC)
   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    W1, W2

!  5. Parameter variables
!
!  6. Local variables
!
!     ID       counter of directions
!     IS       counter of frequencies

   INTEGER  ID, IS

!     DOADD    indicates whether or not values have to be added

   LOGICAL  DOADD

!     ATOT1    integral over spectrum 1
!     ATOT2    integral over spectrum 2
!     AXTOT1   integral over x-component of spectrum 1
!     AXTOT2   integral over x-component of spectrum 2
!     AYTOT1   integral over y-component of spectrum 1
!     AYTOT2   integral over y-component of spectrum 2
!     ASTOT1   integral over Sigma * spectrum 1
!     ASTOT2   integral over Sigma * spectrum 2
!     ASIG1    average Sigma of spectrum 1
!     ASIG2    average Sigma of spectrum 2
!     DELD1    difference in direction between spectrum 1 and
!              the interpolated spectrum in number of directional steps
!     DELD2    same for spectrum 2
!     DELSG1   shift in frequency between spectrum 1 and interpolated
!              spectrum in number of frequency steps
!     DELSG2   same for spectrum 2

   REAL     ATOT1,  ATOT2,  AXTOT1, AXTOT2, AYTOT1, AYTOT2,&
   &ASTOT1, ASTOT2
   REAL     ASIG1,  ASIG2
   REAL     DELD1,  DELD2,  DELSG1, DELSG2

!  8. Subroutines used
!
!  9. Subroutines calling
!
!      SNEXTI, RBFILE
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!      -----------------------------------------------------------------
!      If W1 close to 1
!      Then copy FL from FL1
!      Else If W2 close to 1
!           Then copy FL from FL2
!           Else determine total energy in FL1 and FL2
!                If energy of FL1 = 0
!                Then make FL = W2 * FL2
!                Else If energy of FL2 = 0
!                     Then make FL = W1 * FL1
!                     Else determine average direction of FL1 and FL2
!                          make ADIR = W1 * ADIR1 + W2 * ADIR2
!                          determine average frequency of FL1 and FL2
!                          make ASIG = W1 * ASIG1 + W2 * ASIG2
!                          determine directional shift from FL1
!                          determine directional shift from FL2
!                          determine frequency shift from FL1
!                          determine frequency shift from FL2
!                          For all spectral components do
!                              compose FL from components of FL1 and FL2
!      -----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER ID1A, ID1B, ID2A, ID2B, IDD1A, IDD1B, IDD2A, IDD2B
   INTEGER IDS1A, IDS1B, IDS2A, IDS2B
   REAL A1, A2, AA, ASIG, ASTOT, ATOT, AXTOT, AYTOT
   REAL RDD1A, RDD1B, RDD2A, RDD2B, RDS1A, RDS1B, RDS2A, RDS2B
   CALL STRACE(IENT,'SINTRP')

!     interpolation of spectra
!     ------------------------

   IF (W1.GT.0.99) THEN
      do ID=1,MDC
         do IS=1,MSC
            FL(ID,IS) = FL1(ID,IS)
         end do
      end do
   ELSE IF (W1.LT.0.01) THEN
      do ID=1,MDC
         do IS=1,MSC
            FL(ID,IS) = FL2(ID,IS)
         end do
      end do
   ELSE
      ATOT1  = 0.
      ATOT2  = 0.
      AXTOT1 = 0.
      AXTOT2 = 0.
      AYTOT1 = 0.
      AYTOT2 = 0.
      ASTOT1 = 0.
      ASTOT2 = 0.
      do ID=1,MDC
         do IS=1,MSC
            ATOT1  = ATOT1  + FL1(ID,IS)
            AXTOT1 = AXTOT1 + FL1(ID,IS) * SPCDIR(ID,2)
            AYTOT1 = AYTOT1 + FL1(ID,IS) * SPCDIR(ID,3)
            ASTOT1 = ASTOT1 + FL1(ID,IS) * SPCSIG(IS)
            ATOT2  = ATOT2  + FL2(ID,IS)
            AXTOT2 = AXTOT2 + FL2(ID,IS) * SPCDIR(ID,2)
            AYTOT2 = AYTOT2 + FL2(ID,IS) * SPCDIR(ID,3)
            ASTOT2 = ASTOT2 + FL2(ID,IS) * SPCSIG(IS)
         end do
      end do
      IF (ATOT1.LT.1.E-9) THEN
         do ID=1,MDC
            do IS=1,MSC
               FL(ID,IS) = W2*FL2(ID,IS)
            end do
         end do
      ELSE IF (ATOT2.LT.1.E-9) THEN
         do ID=1,MDC
            do IS=1,MSC
               FL(ID,IS) = W1*FL1(ID,IS)
            end do
         end do
      ELSE
!         determine interpolation factors in Theta space
         AXTOT  = W1 * AXTOT1 + W2 * AXTOT2
         AYTOT  = W1 * AYTOT1 + W2 * AYTOT2
         IF (ITEST.GE.80) THEN
            WRITE (PRTEST, "(' SINTRP factors ', 8E11.4, /, 15X, 4F7.3)")  ATOT1, ATOT2,&
            &AXTOT, AXTOT1, AXTOT2, AYTOT, AYTOT1, AYTOT2
         ENDIF
!         DELD1 is the difference in direction between spectrum 1 and
!         the interpolated spectrum in number of directional steps
         DELD1  = REAL(ATAN2(DBLE(AXTOT*AYTOT1 - AYTOT*AXTOT1),&
         &DBLE(AXTOT*AXTOT1 + AYTOT*AYTOT1))) / DDIR
!         DELD2 is the difference between spectrum 2 and
!         the interpolated spectrum
         DELD2  = REAL(ATAN2(DBLE(AXTOT*AYTOT2 - AYTOT*AXTOT2),&
         &DBLE(AXTOT*AXTOT2 + AYTOT*AYTOT2))) / DDIR
         IDD1A  = NINT(DELD1)
         RDD1B  = DELD1 - REAL(IDD1A)
         IF (RDD1B .LT. 0.) THEN
            IDD1A = IDD1A - 1
            RDD1B = RDD1B + 1.
         ENDIF
         IDD1B  = IDD1A + 1
         RDD1B  = W1 * RDD1B
         RDD1A  = W1 - RDD1B
         IDD2A  = NINT(DELD2)
         RDD2B  = DELD2 - REAL(IDD2A)
         IF (RDD2B .LT. 0.) THEN
            IDD2A = IDD2A - 1
            RDD2B = RDD2B + 1.
         ENDIF
         IDD2B  = IDD2A + 1
         RDD2B  = W2 * RDD2B
         RDD2A  = W2 - RDD2B

!         determine interpolation factors in Sigma space
         ASIG1  = ASTOT1 / ATOT1
         ASIG2  = ASTOT2 / ATOT2
         ATOT   = W1 * ATOT1  + W2 * ATOT2
         ASTOT  = W1 * ASTOT1 + W2 * ASTOT2
         ASIG   = ASTOT / ATOT

!         DELSG1 is shift in frequency between spectrum 1 and interpolated
!         spectrum in number of frequency steps
         DELSG1 = ALOG (ASIG1 / ASIG) / FRINTF
         IDS1A  = NINT(DELSG1)
         RDS1B  = DELSG1 - REAL(IDS1A)
         IF (RDS1B .LT. 0.) THEN
            IDS1A = IDS1A - 1
            RDS1B = RDS1B + 1.
         ENDIF
         IDS1B  = IDS1A + 1
         RDS1A  = 1. - RDS1B

!         DELSG2 is shift in frequency between spectrum 2 and interpolated
!         spectrum in number of frequency steps
         DELSG2 = ALOG (ASIG2 / ASIG) / FRINTF
         IDS2A  = NINT(DELSG2)
         RDS2B  = DELSG2 - REAL(IDS2A)
         IF (RDS2B .LT. 0.) THEN
            IDS2A = IDS2A - 1
            RDS2B = RDS2B + 1.
         ENDIF
         IDS2B  = IDS2A + 1
         RDS2A  = 1. - RDS2B
!         test output
         IF (ITEST.GE.80) THEN
            WRITE (PRTEST, "(' SINTRP factors ', 9E11.4, /, 15X, 4F7.3)") ATOT, ATOT1, ATOT2,&
            &AXTOT, AXTOT1, AXTOT2, AYTOT, AYTOT1, AYTOT2,&
            &DELD1, DELD2, DELSG1, DELSG2
            WRITE (PRTEST, "(' SINTRP ', 8(I2, F7.3))") IDS1A, RDS1A, IDS1B, RDS1B,&
            &IDS2A, RDS2A, IDS2B, RDS2B,&
            &IDD1A, RDD1A, IDD1B, RDD1B,&
            &IDD2A, RDD2A, IDD2B, RDD2B
         ENDIF

         do ID=1,MDC
            do IS=1,MSC
               FL(ID,IS) = 0.
            end do
         end do
         do ID=1,MDC
            DOADD = .TRUE.
            ID1A = ID + IDD1A
            IF (FULCIR) THEN
               IF (ID1A.LT.1)   ID1A = ID1A + MDC
               IF (ID1A.GT.MDC) ID1A = ID1A - MDC
            ELSE
               IF (ID1A.LT.1)   DOADD = .FALSE.
               IF (ID1A.GT.MDC) DOADD = .FALSE.
            ENDIF
            IF (DOADD) THEN
               do IS = MAX(1,1-IDS1A), MIN(MSC,MSC-IDS1A)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD1A * RDS1A * FL1(ID1A,IS+IDS1A)
               end do
               do IS = MAX(1,1-IDS1B), MIN(MSC,MSC-IDS1B)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD1A * RDS1B * FL1(ID1A,IS+IDS1B)
               end do
            ENDIF
         end do
         do ID=1,MDC
            DOADD = .TRUE.
            ID1B = ID + IDD1B
            IF (FULCIR) THEN
               IF (ID1B.LT.1)   ID1B = ID1B + MDC
               IF (ID1B.GT.MDC) ID1B = ID1B - MDC
            ELSE
               IF (ID1B.LT.1)   DOADD = .FALSE.
               IF (ID1B.GT.MDC) DOADD = .FALSE.
            ENDIF
            IF (DOADD) THEN
               do IS = MAX(1,1-IDS1A), MIN(MSC,MSC-IDS1A)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD1B * RDS1A * FL1(ID1B,IS+IDS1A)
               end do
               do IS = MAX(1,1-IDS1B), MIN(MSC,MSC-IDS1B)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD1B * RDS1B * FL1(ID1B,IS+IDS1B)
               end do
            ENDIF
         end do
         do ID=1,MDC
            DOADD = .TRUE.
            ID2A = ID + IDD2A
            IF (FULCIR) THEN
               IF (ID2A.LT.1)   ID2A = ID2A + MDC
               IF (ID2A.GT.MDC) ID2A = ID2A - MDC
            ELSE
               IF (ID2A.LT.1)   DOADD = .FALSE.
               IF (ID2A.GT.MDC) DOADD = .FALSE.
            ENDIF
            IF (DOADD) THEN
               do IS = MAX(1,1-IDS2A), MIN(MSC,MSC-IDS2A)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD2A * RDS2A * FL2(ID2A,IS+IDS2A)
               end do
               do IS = MAX(1,1-IDS2B), MIN(MSC,MSC-IDS2B)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD2A * RDS2B * FL2(ID2A,IS+IDS2B)
               end do
            ENDIF
         end do
         do ID=1,MDC
            DOADD = .TRUE.
            ID2B = ID + IDD2B
            IF (FULCIR) THEN
               IF (ID2B.LT.1)   ID2B = ID2B + MDC
               IF (ID2B.GT.MDC) ID2B = ID2B - MDC
            ELSE
               IF (ID2B.LT.1)   DOADD = .FALSE.
               IF (ID2B.GT.MDC) DOADD = .FALSE.
            ENDIF
            IF (DOADD) THEN
               do IS = MAX(1,1-IDS2A), MIN(MSC,MSC-IDS2A)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD2B * RDS2A * FL2(ID2B,IS+IDS2A)
               end do
               do IS = MAX(1,1-IDS2B), MIN(MSC,MSC-IDS2B)
                  FL(ID,IS) = FL(ID,IS) +&
                  &RDD2B * RDS2B * FL2(ID2B,IS+IDS2B)
               end do
            ENDIF
         end do
      ENDIF
   ENDIF

!     Test output
   IF (ITEST.GE.80) THEN
      A1 = 0.
      A2 = 0.
      AA = 0.
      do ID=1,MDC
         do IS=1,MSC
            A1 = MAX(A1,FL1(ID,IS))
            A2 = MAX(A2,FL2(ID,IS))
            AA = MAX(AA,FL(ID,IS))
         end do
      end do
      WRITE (PRTEST, *) ' SINTRP, maxima ', A1, A2, AA
   ENDIF

   RETURN
!  end of subroutine of SINTRP
end subroutine SINTRP

SUBROUTINE CHGBAS (X1, X2, PERIOD, Y1, Y2, N1, N2,&
&ITEST, PRTEST)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                *
!*****************************************************************
!
!   --|-----------------------------------------------------------|--
!     |            Delft University of Technology                 |
!     | Faculty of Civil Engineering, Fluid Mechanics Group       |
!     | P.O. Box 5048,  2600 GA  Delft, the Netherlands           |
!     |                                                           |
!     | Authors :  G. van Vledder, N. Booij                       |
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
!  0. Update history
!
!       ver 20.48: also accomodates periodic variables such as directions
!
!  1. Purpose
!
!       change x-basis of a discretized y-function
!
!  2. Method
!
!     A piecewise constant representation of the functions is assumed
!
!     first boundaries of a cell in X1 are determined
!     then it is determined whether there are overlaps with cells
!     in X2. if so Y1*common length is added to Y2
!     Finally Y2 values are divided by cell lengths
!
!  3. Parameter list
!
!     Name    I/O  Type  Description
!
!     X1       i    ra   x-coordinates of input grid
!     X2       i    ra   x-coordinates of output grid
!     PERIOD   i    r    period, i.e. x-axis is periodic if period>0
!                        e.g. spectral directions
!     Y1       i    ra   function values of input grid
!     Y2       o    ra   function values of output grid
!     N1       i    i    number of x-values of input grid
!     N2       i    i    number of x-values of output grid
!
!  4. Subroutines used
!
!     ---
!
!  5. Error messages
!
!  6. Remarks
!
!       Cell boundaries in X1 are: X1A and X1B
!       X2 is assumed to be monotonically increasing; this is checked
!       X1 is assumed to be monotonous but not necessarily increasing
!
!  7. Structure
!
!       ----------------------------------------------------------------
!       Make all values of Y2 = 0
!       For each cell in X1 do
!           determine boundaries of cell in X1
!           --------------------------------------------------------------
!           For each cell in X2 do
!               determine overlap with cell in X1; limits: RLOW and RUPP
!               add to Y2: Y1 * length of overlapping interval
!       ----------------------------------------------------------------
!       For each cell in X2 do
!           divide Y2 value by cell length
!       ----------------------------------------------------------------
!
!  8. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER  I1, I2, IADD, II, N1, N2, ITEST, PRTEST
   REAL     X1(N1), Y1(N1), X2(N2), Y2(N2), PERIOD
   REAL     CELLEN, RR, X1A, X1B, X2A, X2B, X2HI, X2LO, RLOW, RUPP
   LOGICAL  TWICE
   CALL STRACE (IENT, 'CHGBAS')

!     initialize output data

   DO I2 = 1, N2
      Y2(I2) = 0.
   ENDDO
   DO I2 = 2, N2
      IF (X2(I2).LE.X2(I2-1))&
      &CALL MSGERR (2, 'subr. CHGBAS: values of X2 not increasing')
   ENDDO
!     boundaries of the range in X2
   X2LO  = 1.5 * X2(1)  - 0.5 * X2(2)
   X2HI  = 1.5 * X2(N2) - 0.5 * X2(N2-1)
   TWICE = .FALSE.

!     loop over cells in X1

   input_cells: do I1 = 1, N1
      IF (ABS(Y1(I1)) .LT. 1.E-20) CYCLE input_cells

!       determine cell boundaries in X1

      IF (I1.EQ.1) THEN
         X1A = 1.5 * X1(1) - 0.5 * X1(2)
      ELSE
         X1A = 0.5 * (X1(I1) + X1(I1-1))
      ENDIF

      IF (I1.EQ.N1) THEN
         X1B = 1.5 * X1(N1) - 0.5 * X1(N1-1)
      ELSE
         X1B = 0.5 * (X1(I1) + X1(I1+1))
      ENDIF

!       swap X1A and X1B if X1A > X1B

      IF (X1A.GT.X1B) THEN
         RR  = X1A
         X1A = X1B
         X1B = RR
      ENDIF

      IF (PERIOD.LE.0.) THEN
         IF (X1A.GT.X2HI .OR. X1B.LT.X2LO) CYCLE input_cells
      ELSE
!         X is periodic; move interval in X1 if necessary
         TWICE = .FALSE.
         IADD = 0
         DO WHILE (X1B.GT.X2HI)
            X1A = X1A - PERIOD
            X1B = X1B - PERIOD
            IADD = IADD + 1
            IF (IADD.GT.99)&
            &CALL MSGERR (2, 'endless loop in CHGBAS')
         END DO
         DO WHILE (X1A.LT.X2LO)
            X1A = X1A + PERIOD
            X1B = X1B + PERIOD
            IADD = IADD + 1
            IF (IADD.GT.99)&
            &CALL MSGERR (2, 'endless loop in CHGBAS')
         END DO
         IF (X1A.GT.X2HI .OR. X1B.LT.X2LO) CYCLE input_cells
         IF (X1A.LT.X2LO .AND. X1A+PERIOD.LT.X2HI) TWICE = .TRUE.
         IF (X1B.GT.X2HI .AND. X1B-PERIOD.GT.X2LO) TWICE = .TRUE.
      ENDIF

!       loop over cells in X2

      overlap_passes: DO
      do I2 = 1, N2

         IF (I2.EQ.1) THEN
            X2A = X2LO
         ELSE
            X2A = 0.5 * (X2(I2) + X2(I2-1))
         ENDIF

         IF (I2.EQ.N2) THEN
            X2B = X2HI
         ELSE
            X2B = 0.5 * (X2(I2) + X2(I2+1))
         ENDIF

!         (RLOW,RUPP) is overlapping interval of (X1A,X1B) and (X2A,X2B)

         IF (X1A.LT.X2B) THEN
            RLOW = MAX (X1A, X2A)
         ELSE
            CYCLE
         ENDIF

         IF (X1B.GT.X2A) THEN
            RUPP = MIN (X1B, X2B)
         ELSE
            CYCLE
         ENDIF

         IF (RUPP.LT.RLOW) THEN
            CALL MSGERR (3, 'interpolation error')
            WRITE (PRTEST, "(' I, XA, XB ', 2(I3, 2(1X,E12.4)))") I1, X1A, X1B, I2, X2A, X2B
         ELSE
            Y2(I2) = Y2(I2) + Y1(I1) * (RUPP-RLOW)
         ENDIF
      end do

!       Cell in X1 covers both ends of sector boundary
      IF (TWICE) THEN
         IF (X1A.LT.X2LO) THEN
            X1A = X1A + PERIOD
            X1B = X1B + PERIOD
         ENDIF
         IF (X1B.GT.X2HI) THEN
            X1A = X1A - PERIOD
            X1B = X1B - PERIOD
         ENDIF
         TWICE = .FALSE.
         CYCLE overlap_passes
      ENDIF
      EXIT overlap_passes
      END DO overlap_passes
   end do input_cells

   DO I2 = 1, N2
      IF (I2.EQ.1) THEN
         CELLEN = X2(2) - X2(1)
      ELSE IF (I2.EQ.N2) THEN
         CELLEN = X2(N2) - X2(N2-1)
      ELSE
         CELLEN = 0.5 * (X2(I2+1) - X2(I2-1))
      ENDIF
!       divide Y2 by cell length
      Y2(I2) = Y2(I2) / CELLEN
   ENDDO
   IF (ITEST.GE.160) THEN
      WRITE (PRTEST, "(' test CHGBAS ', 2I5)") N1, N2
      WRITE (PRTEST, "(10 (1X,E10.3))") (X1(II), II = 1, N1)
      WRITE (PRTEST, "(10 (1X,E10.3))") (Y1(II), II = 1, N1)
      WRITE (PRTEST, "(10 (1X,E10.3))") (X2(II), II = 1, N2)
      WRITE (PRTEST, "(10 (1X,E10.3))") (Y2(II), II = 1, N2)
   ENDIF

   RETURN
end subroutine CHGBAS

REAL FUNCTION GAMMAF(XX)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                   *
!********************************************************************
!
!   Updates
!     ver 30.70, Oct 1997 by N.Booij: new subroutine
!
!   Purpose
!     Compute the transcendental function Gamma
!
!   Subroutines used
!     GAMMLN  (Numerical Recipes)

   INTEGER, SAVE :: IENT = 0
   REAL XX
   CALL STRACE (IENT, 'GAMMAF')
   GAMMAF = GAMMAF_KERNEL(XX)
   RETURN
end function GAMMAF

PURE ELEMENTAL REAL FUNCTION GAMMAF_KERNEL(XX)
   REAL, INTENT(IN) :: XX
   REAL :: YY
   REAL, PARAMETER :: ABIG = 30.

   YY = GAMMLN(XX)
   IF (YY.GT.ABIG) YY = ABIG
   IF (YY.LT.-ABIG) YY = -ABIG
   GAMMAF_KERNEL = EXP(YY)
end function GAMMAF_KERNEL

PURE ELEMENTAL REAL FUNCTION GAMMLN(XX)
!                                                                   *
!********************************************************************
!
!   Method:
!     function is copied from: Press et al., "Numerical Recipes"

   REAL(KIND=KIND(0.0D0)), PARAMETER :: COF(6) = &
      [76.18009173D0, -86.50532033D0, 24.01409822D0, &
       -1.231739516D0, .120858003D-2, -.536382D-5]
   REAL(KIND=KIND(0.0D0)), PARAMETER :: STP = 2.50662827465D0
   REAL(KIND=KIND(0.0D0)), PARAMETER :: HALF = 0.5D0, ONE = 1.0D0, FPF = 5.5D0
   REAL(KIND=KIND(0.0D0)) :: X, TMP, SER
   INTEGER J
   REAL, INTENT(IN) :: XX
   X=XX-ONE
   TMP=X+FPF
   TMP=(X+HALF)*LOG(TMP)-TMP
   SER=ONE
   do J=1,6
      X=X+ONE
      SER=SER+COF(J)/X
   end do
   GAMMLN=TMP+LOG(STP*SER)
   RETURN
end function GAMMLN

END MODULE swan_spectrum_transform
