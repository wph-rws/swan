
!     SWAN - SERVICE ROUTINES
!
!  Contents of this file
!
!     READXY
!     REFIXY
!     INFRAM
!     DISTR
!     KSCIP1
!     KSCIP2
!     NG
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
!TIMG!     SWTSTA
!TIMG!     SWTSTO
!TIMG!     SWPRTI
!     TXPBLA
!     INTSTR
!     NUMSTR
!     SWCOPI
!     SWCOPR
!MatL4!     SWI2B
!MatL4!     SWR2B
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
SUBROUTINE READXY (NAMX, NAMY, XX, YY, KONT, XSTA, YSTA)
!                                                                      *
!************************************************************************

   USE OCPCOMM1
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
!     40.22: John Cazes and Tim Campbell
!     40.13: Nico Booij
!     40.51: Marcel Zijlema
!
!  1. UPDATE
!
!       Nov. 1996               offset values are added to standard values
!                               because they will be subtracted later
!     40.13, Nov. 01: a valid value for YY is required if a valid value
!                     for XX has been given; ocpcomm1.inc reactivated
!     40.51, Feb. 05: correction to location points equal to offset values
!
!  2. PURPOSE
!
!       Read x and y, initialize offset values XOFFS and YOFFS
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       NAMX, NAMY   inp char    names of the two coordinates as given in
!                                the user manual
!       XX, YY       out real    values of x and y taking into account offset
!       KONT         inp char    what to be done if values are missing
!                                see doc. of INDBLE (Ocean Pack doc.)
!       XSTA, YSTA   inp real    standard values of x and y
!
!  5. SUBROUTINES CALLING
!
!
!
!  6. SUBROUTINES USED
!
!       INDBLE (Ocean Pack)
   LOGICAL EQREAL

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
!       Read x and y in double prec.
!       If this is first couple of values
!       Then assign values to XOFFS and YOFFS
!            make LXOFFS True
!       ---------------------------------------------------------------
!       make XX and YY equal to x and y taking into account offset
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   REAL(KIND=KIND(0.0D0)) XTMP, YTMP
   INTEGER, SAVE :: IENT = 0
   REAL XX, YY, XSTA, YSTA
   CHARACTER(LEN=*) :: NAMX, NAMY, KONT
   CALL  STRACE (IENT,'READXY')

   CALL INDBLE (NAMX, XTMP, KONT, DBLE(XSTA)+DBLE(XOFFS))
   IF (CHGVAL) THEN
!       a valid value was given for XX
      CALL INDBLE (NAMY, YTMP, 'REQ', DBLE(YSTA)+DBLE(YOFFS))
   ELSE
      CALL INDBLE (NAMY, YTMP, KONT, DBLE(YSTA)+DBLE(YOFFS))
   ENDIF
   IF (.NOT.LXOFFS) THEN
      XOFFS = REAL(XTMP)
      YOFFS = REAL(YTMP)
      LXOFFS = .TRUE.
   ENDIF
   IF (.NOT.EQREAL(XOFFS,REAL(XTMP))) THEN
      XX = REAL(XTMP-DBLE(XOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      XX = 1.E-5
   ELSE
      XX = 0.
   END IF
   IF (.NOT.EQREAL(YOFFS,REAL(YTMP))) THEN
      YY = REAL(YTMP-DBLE(YOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      YY = 1.E-5
   ELSE
      YY = 0.
   END IF

   RETURN
! * end of subroutine READXY  *
end subroutine READXY
!************************************************************************
!                                                                      *
SUBROUTINE REFIXY (NDS, XX, YY, IERR)
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
!     40.22: John Cazes and Tim Campbell
!     40.51: M. Zijlema
!
!  1. UPDATE
!
!       first version: 10.18 (Sept 1994)
!
!  2. PURPOSE
!
!       initialize offset values XOFFS and YOFFS, and shift XX and YY
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       NDS          in  int     file reference number
!       XX, YY       out real    values of x and y taking into account offset
!       IERR         out int     error indicator: IERR=0: no error, =-1: end-
!                                of-file, =-2: read error
!
!  5. SUBROUTINES CALLING
!
!
!
!  6. SUBROUTINES USED

   LOGICAL EQREAL

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
!       If this is first couple of values
!       Then assign values to XOFFS and YOFFS
!            make LXOFFS True
!       ---------------------------------------------------------------
!       make XX and YY equal to x and y taking into account offset
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   REAL(KIND=KIND(0.0D0)) XTMP, YTMP
   REAL             XX, YY
   INTEGER, SAVE :: IENT = 0
   INTEGER          IERR, NDS
   CALL  STRACE (IENT,'REFIXY')

   READ (NDS, *, IOSTAT=IERR) XTMP, YTMP
   IF (IERR.NE.0) THEN
      IF (IS_IOSTAT_END(IERR)) THEN
         IERR = -1
      ELSE
         IERR = -2
      ENDIF
      RETURN
   ENDIF
   IF (.NOT.LXOFFS) THEN
      XOFFS = REAL(XTMP)
      YOFFS = REAL(YTMP)
      LXOFFS = .TRUE.
   ENDIF
   IF (.NOT.EQREAL(XOFFS,REAL(XTMP))) THEN
      XX = REAL(XTMP-DBLE(XOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      XX = 1.E-5
   ELSE
      XX = 0.
   END IF
   IF (.NOT.EQREAL(YOFFS,REAL(YTMP))) THEN
      YY = REAL(YTMP-DBLE(YOFFS))
   ELSE IF (OPTG.EQ.3) THEN
      YY = 1.E-5
   ELSE
      YY = 0.
   END IF

   IERR = 0
   RETURN
! * end of subroutine REFIXY  *
end subroutine REFIXY
!************************************************************************
!                                                                      *
LOGICAL FUNCTION  INFRAM (XQQ, YQQ)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM1


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
SUBROUTINE DISTR (CDIR, DIR, COEF, SPCDIR)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
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
!
!  0. Authors
!
!     30.82: IJsbrand Haagsma
!     40.22: John Cazes and Tim Campbell
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!      0.1 , Jul. 87: Standard heading added
!      0.2 , Dec. 89: Value for energy outside of the sector changed
!                     from 0. to 1.E-6
!            Oct. 90: Value for energy outside the sector changed to 1.E-10
!                     logical BDIR introduced to take care for case where
!                     none of the values is positive
!     30.82, Oct. 98: Updated description of several variables
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!       Computation of the distribution of the wave energy over the
!       sectors, according to the given directional spread.
!
!  3. METHOD
!
!       ---
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

!       CDIR    REAL   output   array containing the coefficients of
!                               (energy) distribution
!       DIR     REAL   input    main wave direction, in radians
!       COEF    REAL   input    coefficient of the directional distri-
!                               bution (cos**COEF)
!
!  5. SUBROUTINES CALLING
!
!       REINVA (HISWA/SWREAD), STARTB and SWIND (both HISWA/COMPU)
!
!  6. SUBROUTINES USED
!
!       none
!
!  7. Common blocks used
!
!
!  8. REMARKS
!
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       For every direction of the grid do
!           If the direction deviates less than PI/2 from the main wave
!            direction, then
!               Compute the coefficient cos**n
!           Else
!               Coefficient is 1.E-10
!       ----------------------------------------------------------------
!       If any of the directions deviated less than PI/2
!       Then Compute the total of the coefficients
!            For every direction of the grid do
!                Divide the fraction of the distribution by the total
!       ----------------------------------------------------------------
!
! 13. Source text

   LOGICAL BDIR
   INTEGER, SAVE :: IENT = 0
   INTEGER ID0, JJ
   REAL CDIR(*)
   REAL ACOS, CNORM, COEF, DIR, SOMC, TETA
   IF (LTRACE) CALL STRACE (IENT, 'DISTR')

   SOMC = 0.
   BDIR = .FALSE.
   do ID0 = 1, MDC
      TETA = SPCDIR(ID0,1)
      ACOS = COS(TETA-DIR)
      IF (ACOS .GT. 0.) THEN
         BDIR = .TRUE.
         CDIR(ID0) = MAX (ACOS**COEF, 1.E-10)
         SOMC = SOMC + CDIR(ID0)
      ELSE
         CDIR(ID0) = 1.E-10
      ENDIF
   end do
   IF (BDIR) THEN
      CNORM = 1./(SOMC*DDIR)
      do ID0 = 1, MDC
         CDIR(ID0) = CDIR(ID0) * CNORM
      end do
   ENDIF

!     ***** test *****
!      IF(TESTFL .AND. ITEST .GE. 200)
   IF( ITEST .GE. 200)&
   &WRITE(PRINTF,"(' Test DISTR',F10.3/(10E12.4))") CNORM,(CDIR(JJ) , JJ=1,MDC)

   RETURN
! * end of subroutine DISTR *
end subroutine DISTR
!************************************************************************
!                                                                      *
SUBROUTINE KSCIP1 (MMT, SIG, D, K, CG, N, ND)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3

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
!     30.81: Annette Kieftenburg
!     40.22: John Cazes and Tim Campbell
!     40.41: Marcel Zijlema
!     41.16: Greg Wilson
!
!  1. Updates
!
!     Aug. 94, ver. 10.10: arguments N and ND added
!     Dec. 98, ND corrected, argument list adjusted and IMPLICIT NONE added
!     40.41, Aug. 04: tables replaced by Pade and other formulas
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.16, Mar. 11: correction: add dk/dh to dn/dh
!
!  2. Purpose
!
!     Calculation of the wave number, group velocity, group number N
!     and the derivative of N w.r.t. depth (=ND)
!
!  3. Method
!
!     --
!
!  4. Argument variables
!
!     MMT     input    number of frequency points

   INTEGER   MMT

!     CG      output   group velocity
!     D       input    local depth
!     K       output   wave number
!     N       output   ratio of group and phase velocity
!     ND      output   derivative of N with respect to D
!     SIG     input    rel. frequency for which wave parameters
!                      must be determined

   REAL      CG(MMT), D,&
   &K(MMT), N(MMT), ND(MMT), SIG(MMT)

!  6. Local variables
!
!     C         phase velocity
!     FAC1      auxiliary factor
!     FAC2      auxiliary factor
!     FAC3      auxiliary factor
!     IENT      number of entries
!     IS        counter in frequency (sigma-space)
!     KND       dimensionless wave number
!     ROOTDG    square root of D/GRAV
!     WGD       square root of GRAV*D
!     SND       dimensionless frequency
!     SND2      = SND*SND

   INTEGER, SAVE :: IENT = 0
   INTEGER   IS
   REAL      KND, ROOTDG, SND, WGD, SND2, C, FAC1, FAC2, FAC3

!  8. Subroutines used
!
!     --
!
!  9. Subroutines calling
!
!     SWOEXA, SWOEXF (Swan/Output)
!
! 10. Error messages
!
!     --
!
! 11. Remarks
!
!     --
!
! 12. Structure
!
!     -----------------------------------------------------------------
!      Compute non-dimensional frequency SND
!      IF SND >= 2.5, then
!        Compute wave number K, group velocity CGO, ratio of group
!        and phase velocity N and its derivative ND according to
!        deep water theory
!      ELSE IF SND =< 1.e-6
!        Compute wave number K, group velocity CGO, ratio of group
!        and phase velocity N and its derivative ND
!        according to extremely shallow water
!      ELSE
!        Compute wave number K, group velocity CGO and the ratio of
!        group and phase velocity N by Pade and other simple formulas.
!        Compute the derivative of N w.r.t. D = ND.
!     -----------------------------------------------------------------
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT, 'KSCIP1')

   ROOTDG = SQRT(D/GRAV)
   WGD    = ROOTDG*GRAV
   do IS = 1, MMT
!       SND is dimensionless frequency
      SND = SIG(IS) * ROOTDG
      IF (SND .GE. 2.5) THEN
!       ******* deep water *******
         K(IS)  = SIG(IS) * SIG(IS) / GRAV
         CG(IS) = 0.5 * GRAV / SIG(IS)
         N(IS)  = 0.5
         ND(IS) = 0.
      ELSE IF (SND.LT.1.E-6) THEN
!       *** very shallow water ***
         K(IS)  = SND/D
         CG(IS) = WGD
         N(IS)  = 1.
         ND(IS) = 0.
      ELSE
         SND2  = SND*SND
         C     = SQRT(GRAV*D/(SND2+1./(1.+0.666*SND2+0.445*SND2**2&
         &-0.105*SND2**3+0.272*SND2**4)))
         K(IS) = SIG(IS)/C
         KND   = K(IS)*D
         FAC1  = 2.*KND/SINH(2.*KND)
         N(IS) = 0.5*(1.+FAC1)
         CG(IS)= N(IS)*C
         FAC2  = SND2/KND
         FAC3  = 2.*FAC2/(1.+FAC2*FAC2)
         FAC2  = -K(IS)*(2.*N(IS)-1.)/(2.*D*N(IS))
         ND(IS)= FAC1*(0.5/D - K(IS)/FAC3 + FAC2*(0.5/K(IS) - D/FAC3))
      ENDIF
   end do

   RETURN
!     end of subroutine KSCIP1 *
end subroutine KSCIP1
!************************************************************************
!                                                                      *
SUBROUTINE KSCIP2 (MMT, SIG, D, K, CG, N, ND, DMW, DM)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3

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
!     40.59: Erick Rogers
!
!  1. Updates
!
!     40.59, Aug. 07: copy from KSCIP1 and adaption to muddy bottom
!
!  2. Purpose
!
!     Calculation of the wave number, group velocity, group number N
!     and the derivative of N w.r.t. depth (=ND) in case of muddy bottom
!
!  3. Method
!
!     Wave number based on Ng (2000) and other quantities derived thereof
!
!  4. Argument variables
!
!     MMT     input    number of frequency points

   INTEGER   MMT

!     CG      output   group velocity
!     D       input    local depth
!     DM      input    local depth of mud layer
!     DMW     output   mud dissipation rate
!                      (is the imaginary part of the wave number)
!     K       in/out   (the real part of) the wave number
!     N       output   ratio of group and phase velocity
!     ND      output   derivative of N with respect to D
!     SIG     input    rel. frequency for which wave parameters
!                      must be determined

   REAL      CG(MMT), D, DM,&
   &K(MMT), N(MMT), ND(MMT), SIG(MMT), DMW(MMT)

!  6. Local variables
!
!     C         phase velocity
!     DTILDE    normalized mud depth = mud depth / delta_m,
!               delta is the sblt= sqrt(2*visc/sigma)
!     FAC1      auxiliary factor
!     FAC2      auxiliary factor
!     FAC3      auxiliary factor
!     GAMMA     this is the gamma used in Ng pg. 238. this is
!               density(water)/density(mud)
!     IENT      number of entries
!     IS        counter in frequency space
!     KINVISM   kinematic viscosity of mud
!     KINVISW   kinematic viscosity of water
!     KMUD      (local) muddy wave number
!     KND       dimensionless wave number
!     RHOM      density of mud
!     RHOW      density of water
!     SBLTM     a function of viscosity and frequency
!     ZETA      this is zeta as used in Ng pg. 238. it is the
!               ratio of Stokes boundary layer thicknesses,
!               or delta_m/delta_w

   INTEGER, SAVE :: IENT = 0
   INTEGER IS
   REAL    C, DTILDE, FAC1, FAC2, FAC3, GAMMA, KINVISM,&
   &KINVISW, KMUD, KND, RHOM, RHOW, SBLTM, ZETA

!  8. Subroutines used
!
!     --
!
!  9. Subroutines calling
!
!     --
!
! 10. Error messages
!
!     --
!
! 11. Remarks
!
!     --
!
! 12. Structure
!
!     --
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT, 'KSCIP2')

   RHOM    = PMUD(2)
   KINVISM = PMUD(3)
   RHOW    = PMUD(4)
   KINVISW = PMUD(5)

   ZETA  = SQRT(KINVISM/KINVISW)
   GAMMA = RHOW/RHOM

   DO IS = 1, MMT
      KND = K(IS)*D
      IF ( KND.LT.10. .AND. DM.GT.1.E-5 ) THEN
         SBLTM  = SQRT(2.*KINVISM/SIG(IS))
         DTILDE = DM/SBLTM
!         calculate muddy wave number and dissipation rate using Ng (2000)
         CALL NG(SIG(IS),D,DTILDE,ZETA,SBLTM,GAMMA,K(IS),KMUD,DMW(IS))
         K(IS)  = KMUD
!         calculate ratio N, CG and derivative of N w.r.t. D
         KND = K(IS)*D
         IF ( KND.LT.35. ) THEN
            FAC1 = 2.*KND/SINH(2.*KND)
         ELSE
            FAC1 = 2.E-30*KND
         ENDIF
         N(IS)  = 0.5*(1.+FAC1)
         C      = SIG(IS)/K(IS)
         CG(IS) = N(IS)*C
         FAC2   = SIG(IS) * C / GRAV
         FAC3   = 2.*FAC2/(1.+FAC2*FAC2)
         FAC2   = -K(IS)*(2.*N(IS)-1.)/(2.*D*N(IS))
         ND(IS) = FAC1*(0.5/D - K(IS)/FAC3 + FAC2*(0.5/K(IS) - D/FAC3))
      ELSE
         DMW(IS)= 0.
      ENDIF
   ENDDO

   RETURN
!     end of subroutine KSCIP2 *
end subroutine KSCIP2
!************************************************************************

SUBROUTINE NG(SIGMA,H_WDEPTH,DTILDE,ZETA,SBLTM,GAMMA,WK,WKDR,DISS)

!************************************************************************

   USE OCPCOMM4

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
!      1.00: Jim Kaihatu
!     40.??: Erick Rogers
!     40.59: Erick Rogers
!
!  1. Updates
!
!      1.00, Dec. 04: original
!     40.??, Mar. 05: adapted for SWAN
!     40.59, Aug. 07: included patch provided by Jim Kaihatu
!                     and finalized
!
!  2. Purpose
!
!     Computes the muddy wavenumber using Ng (2000)
!
!  3. Method
!
!     Reference: Ng, C.O., 2000. Water waves over a muddy bed:
!                a two-layer Stokes boundary layer model
!                Coastal Engineering 40(3), 221-242
!
!  4. Argument variables

   REAL, INTENT(IN)  ::  SIGMA   ! radian frequency (rad)
   REAL, INTENT(IN)  ::  H_WDEPTH! water depth, denoted "h" in Ng (m)
   REAL, INTENT(IN)  ::  DTILDE  ! normalized mud depth = mud depth /
   ! delta is the sblt= sqrt(2*visc/sig
   REAL, INTENT(IN)  ::  ZETA    ! this is zeta as used in Ng pg. 238
   ! the ratio of Stokes boundary layer
   ! thicknesses, or SBLTM/delta_w
   REAL, INTENT(IN)  ::  GAMMA   ! this is the gamma used in Ng pg. 2
   ! this is density(water)/density(mud
   REAL, INTENT(IN)  ::  SBLTM   ! SBLTM is what you get if you calcu
   ! sblt using the viscosity of the mu
   ! SBLTM=sqrt(2*visc_m/sigma)
   ! .....also delta_m
   REAL, INTENT(IN)  :: WK       ! unmuddy wavenumber

   REAL, INTENT(OUT) :: WKDR     ! muddy wavenumber
   REAL, INTENT(OUT) :: DISS     ! dissipation rate

!  5. Local variables

   INTEGER, SAVE :: IENT = 0
   REAL    :: B1  !  an Ng coefficient
   REAL    :: B2  !  an Ng coefficient
   REAL    :: B3  !  an Ng coefficient
   REAL    :: BR  !  an Ng coefficient
   REAL    :: BI  !  an Ng coefficient
   REAL    :: BRP !  an Ng coefficient
   REAL    :: BIP !  an Ng coefficient
   REAL    :: DM  !  mud depth, added June 2 2006

! 11. Remarks
!
!     Calculations for the "B coefficients" came from a code by Jim Kaihatu
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'NG')

   DM=DTILDE*SBLTM !  DTILDE=DM/SBLTM

   ! now calculate Ng's B coefficients : see Ng pg 238
   B1=GAMMA*(-2.*GAMMA**2+2.*GAMMA-1.-ZETA**2)*SINH(DTILDE)*&
   &COSH(DTILDE)-GAMMA**2*ZETA*((COSH(DTILDE))**2+&
   &(SINH(DTILDE))**2)-(GAMMA-1.)**2*ZETA*((COSH(DTILDE))**2&
   &*(COS(DTILDE))**2+(SINH(DTILDE))**2*(SIN(DTILDE))**2)-2.&
   &*GAMMA*(1.-GAMMA)*(ZETA*COSH(DTILDE)+GAMMA*SINH(DTILDE))&
   &*COS(DTILDE)

   B2=GAMMA*(-2.*GAMMA**2+2.*GAMMA-1.+ZETA**2)*SIN(DTILDE)*&
   &COS(DTILDE) -2.*GAMMA*(1.-GAMMA)*(ZETA*SINH(DTILDE)+GAMMA&
   &*COSH(DTILDE))*SIN(DTILDE)

   B3=(ZETA*COSH(DTILDE)+GAMMA*SINH(DTILDE))**2*(COS(DTILDE))**2&
   &+(ZETA*SINH(DTILDE)+GAMMA*COSH(DTILDE))**2*(SIN(DTILDE))**2

   BR=WK*SBLTM*(B1-B2)/(2.*B3)+GAMMA*WK*DM

   BI=WK*SBLTM*(B1+B2)/(2.*B3)
   BRP=B1/B3  ! "B_R PRIME"
   BIP=B2/B3  ! "B_I PRIME"

   ! now calculate dissipation rate and wavenumber
   DISS=-SBLTM*(BRP+BIP)*WK**2/(SINH(2.*WK*H_WDEPTH)+2.*WK*H_WDEPTH)
   WKDR=WK-BR*WK/(SINH(WK*H_WDEPTH)*COSH(WK*H_WDEPTH)+WK*H_WDEPTH)

   RETURN

end subroutine NG
!************************************************************************
!                                                                      *
SUBROUTINE AC2TST (XYTST, AC2,KGRPNT)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
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

!****************************************************************

   USE OCPCOMM4
   USE SWCOMM2
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
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
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

   LOGICAL  INMESH ,FINDXY, ONBND

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
!                                                                      *
!************************************************************************

   USE OCPCOMM4
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
!                                                                      *
!************************************************************************

   USE OCPCOMM4
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
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE M_OBSTA

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

   LOGICAL    TCROSS

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
LOGICAL FUNCTION TCROSS (X1, X2, X3, X4, Y1, Y2, Y3, Y4, X1ONOBST)
!                                                                      *
!************************************************************************

   USE OCPCOMM4

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

   REAL       EPS, X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL    X1ONOBST

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
   REAL       A, B, C, D, DIV1, E, F, LMBD, MIU

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
      IF (ITEST .GE. 120) THEN
         WRITE(PRINTF,"(' Obstacle crossing :',/, ' Coordinates of comp grid points and corners of obstacle:',/, ' P1(',E10.4,',',E10.4,')',' P2(',E10.4,',',E10.4,')',/, ' P3(',E10.4,',',E10.4,')',' P4(',E10.4,',',E10.4,')')")X1,Y1,X2,Y2,X3,Y3,X4,Y4
      ENDIF

      TCROSS = .TRUE.
   ELSE
      TCROSS = .FALSE.
   ENDIF

!     End of subroutine TCROSS
   RETURN
end function TCROSS

!************************************************************************
!                                                                      *
SUBROUTINE SWTRCF (DEP2  , WLEV2 , CHS   ,&
&LINK  , OBREDF,&
&AC2   , REFLSO, KGRPNT, XCGRID,&
&YCGRID, CAX,    CAY   , RDX   , RDY , ANYBIN,&
&SPCSIG, SPCDIR, CGO   , KWAVE , HSS2, TSS2  ,&
&DSS2  )
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_OBSTA
   USE M_PARALL
   USE SwanGriddata
   USE SwanCompdata
   USE SwanIEM, only: ntf, Ebig

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

   INTEGER  KGRPNT(MXC,MYC)
   INTEGER  LINK(2)
   REAL     CHS(MCGRD), OBREDF(MDC,MSC,2), WLEV2(MCGRD), DEP2(MCGRD)
   REAL     :: AC2(MDC,MSC,MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
   REAL     :: CAX(MDC,MSC,MICMAX), CAY(MDC,MSC,MICMAX)
   REAL     :: REFLSO(MDC,MSC), RDX(MICMAX), RDY(MICMAX)
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
   REAL       XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   INTEGER    ICC, JJ
   REAL    :: XOBS(2), XV(2), YOBS(2), YV(2)
   LOGICAL :: SwanCrossObstacle
   TYPE(OBSTDAT), POINTER :: COBST

!  8. Subroutines used
!
!     DEGCNV           direction in Cartesian or nautical degrees
!     EQREAL           indicates whether two reals are equal or not
!     GAMMAF           the gamma function
!     MSGERR           writes error message
!     REFLECT          computes effect of reflection
!     TCROSS           searches for crossing point if exist

   REAL    DEGCNV
   REAL    GAMMAF
   LOGICAL EQREAL
   LOGICAL TCROSS

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
            X1 = xcugrd(vs(1))+XOFFS
            Y1 = ycugrd(vs(1))+YOFFS
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
            X1 = xcugrd(vs(1))
            Y1 = ycugrd(vs(1))
            X2 = xcugrd(vs(ILINK+1))
            Y2 = ycugrd(vs(ILINK+1))
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
               &OBREDF, REFLTST)
            ELSE
               CALL REFLECT(AC2, REFLSO, X1, Y1, X2, Y2,&
               &X3, Y3, X4, Y4, CAX,&
               &CAY, RDX, RDY, ILINK,&
               &REFLCOEF, LREFDIFF, POWN, ANYBIN,&
               &LRFRD, SPCSIG, SPCDIR, FD1, FD2, FD3, FD4,&
               &OBREDF, REFLTST)
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
&OBREDF, REFLTST)

!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE SWCOMM4

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
   REAL       :: RDX(MICMAX), RDY(MICMAX)
   REAL       :: FD1, FD2, FD3, FD4, SPCSIG(MSC), SPCDIR(MDC,6)
   REAL       :: REF0
   REAL       :: X1, X2, X3, X4, Y1, Y2, Y3, Y4
   LOGICAL    :: ANYBIN(MDC,MSC)
   INTEGER    :: ILINK
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
               WRITE (PRTEST, "(' Refl+Transm>1 in ', 2I4, 2X, 3I3, 2X, 2F6.2)") IXCGRD(1)-1, IYCGRD(1)-1, ILINK,&
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
                  &REF0 * W1 * PRDIF(ABS(IDR)) * AC2(IDA,IS,KCGRD(1))
                  IDB = 1+MOD(2*MDC+IDB-1,MDC)
                  IF (COS(TH_NORM-SPCDIR(IDB,1)) .GT. 0.)&
                  &AC2REF = AC2REF +&
                  &REF0 * W2 * PRDIF(ABS(IDR)) * AC2(IDB,IS,KCGRD(1))
               ELSE
                  IF (IDA.GE.1 .AND. IDA.LE.MDC) THEN
                     IF (COS(TH_NORM-SPCDIR(IDA,1)) .GT. 0.)&
                     &AC2REF = AC2REF +&
                     &REF0 * W1 * PRDIF(ABS(IDR)) * AC2(IDA,IS,KCGRD(1))
                  ENDIF
                  IF (IDB.GE.1 .AND. IDB.LE.MDC) THEN
                     IF (COS(TH_NORM-SPCDIR(IDB,1)) .GT. 0.)&
                     &AC2REF = AC2REF +&
                     &REF0 * W2 * PRDIF(ABS(IDR)) * AC2(IDB,IS,KCGRD(1))
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
SUBROUTINE SSHAPE (ACLOC, SPCSIG, SPCDIR, FSHAPL, DSHAPL)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
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
   REAL DEGCNV, DSPR, EPTAIL, ESOM, GAM1, GAM2, GAMMAF, HSTMP
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
!*******************************************************************
!                                                                  *
SUBROUTINE SINTRP (W1, W2, FL1, FL2, FL, SPCDIR, SPCSIG)
!                                                                  *
!*******************************************************************

   USE OCPCOMM4
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
!************************************************************************
!                                                                      *
REAL FUNCTION DEGCNV (DEGREE)
!                                                                      *
!************************************************************************

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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform degrees from nautical to cartesian or vice versa.
!
!  3. METHOD
!
!       DEGCNV = 180 + dnorth - degree
!
!  4. PARAMETERLIST
!
!       DEGCNV      direction in cartesian or nautical degrees.
!       DEGREE      direction in nautical or cartesian degrees.
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
!
!  7. ERROR MESSAGES
!
!       NONE
!
!  8. REMARKS
!
!           Nautical convention           Cartesian convention
!
!                    0                             90
!                    |                              |
!                    |                              |
!                    |                              |
!                    |                              |
!        270 --------+-------- 90       180 --------+-------- 0
!                    |                              |
!                    |                              |
!                    |                              |
!                    |                              |
!                   180                            270
!
!  9. STRUCTURE
!
!     ---------------------------------
!     IF (NAUTICAL DEGREES) THEN
!       CONVERT DEGREES
!     IF (DEGREES > 360 OR < 0) THEN
!       CORRECT DEGREES WITHIN 0 - 360
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL DEGREE
   CALL STRACE(IENT,'DEGCNV')

   IF ( BNAUT ) THEN
      DEGCNV = 180. + DNORTH - DEGREE
   ELSE
      DEGCNV = DEGREE
   ENDIF

   IF (DEGCNV .GE. 360.) THEN
      DEGCNV = MOD (DEGCNV, 360.)
   ELSE IF (DEGCNV .LT. 0.) THEN
      DEGCNV = MOD (DEGCNV, 360.) + 360.
   ELSE
!       DEGCNV between 0 and 360; do nothing
   ENDIF


!     *** end of subroutine DEGCNV ***

   RETURN
end function DEGCNV

!************************************************************************
!                                                                      *
REAL FUNCTION ANGRAD (DEGREE)
!                                                                      *
!************************************************************************

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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform degrees to radians
!
!  3. METHOD
!
!       ANGRAD = DEGREE * PI / 180
!
!  4. PARAMETERLIST
!
!       ANGRAD      radians
!       DEGREE      degrees
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
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
!     ---------------------------------
!     ANGLE[radian] = ANGLE[degrees} * PI / 180
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL DEGREE
   CALL STRACE(IENT,'ANGRAD')

   ANGRAD = DEGREE * PI / 180.


!     *** end of subroutine ANGRAD ***

   RETURN
end function ANGRAD

!************************************************************************
!                                                                      *
REAL FUNCTION ANGDEG (RADIAN)
!                                                                      *
!************************************************************************

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
!  1. UPDATE
!
!       SEP 1997: New for SWAN 32.01
!                 Cor van der Schelde - Delft Hydraulics
!       30.70, Feb. 98: test output suppressed (causes problem if subr
!                       is used during output
!
!  2. PURPOSE
!
!       Transform radians to degrees
!
!  3. METHOD
!
!       ANGDEG = RADIAN * 180 / PI
!
!  4. PARAMETERLIST
!
!       RADIAN      radians
!       ANGDEG      degrees
!
!  5. SUBROUTINES CALLING
!
!       ---
!
!  6. SUBROUTINES USED
!
!       NONE
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
!     ---------------------------------
!     ANGLE[degrees] = ANGLE[radians} * 180 / PI
!     ---------------------------------
!
! 10. SOURCE TEXT
!
!************************************************************************

   INTEGER, SAVE :: IENT = 0
   REAL RADIAN
   CALL STRACE(IENT,'ANGDEG')

   ANGDEG = RADIAN * 180. / PI


!     *** end of subroutine ANGDEG ***

   RETURN
end function ANGDEG

!************************************************************************
!                                                                      *
SUBROUTINE HSOBND (AC2   ,SPCSIG,HSIBC ,KGRPNT)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
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
SUBROUTINE CHGBAS (X1, X2, PERIOD, Y1, Y2, N1, N2,&
&ITEST, PRTEST)
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

!********************************************************************
!                                                                   *
REAL FUNCTION GAMMAF(XX)
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
   REAL XX, YY, GAMMLN
   REAL, PARAMETER :: ABIG = 30.
   CALL STRACE (IENT, 'GAMMAF')
   YY = GAMMLN(XX)
   IF (YY.GT.ABIG) YY = ABIG
   IF (YY.LT.-ABIG) YY = -ABIG
   GAMMAF = EXP(YY)
   RETURN
end function GAMMAF
!********************************************************************
!                                                                   *
REAL FUNCTION GAMMLN(XX)
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
   REAL XX
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
!************************************************************************
!                                                                      *
SUBROUTINE WRSPEC (NREF, ACLOC)
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE OUTP_DATA

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
!     40.00, 40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATE
!
!     new subroutine, update 40.00
!     40.03, Mar. 00: precision increased; 2 decimals more in output table
!     40.13, July 01: variable format using module OUTP_DATA
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Writing of action density spectrum in Swan standard format
!
!  3. METHOD
!
!
!  4. Argument variables
!
!       NREF    int    input    unit ref. number of output file
!       ACLOC   real   local    2-D spectrum or source term at one
!                               output location

   INTEGER, INTENT(IN) :: NREF
   REAL, INTENT(IN)    :: ACLOC(1:MDC,1:MSC)

!  5. Parameter variables
!
!  6. Local variables
!
!       ID      counter of spectral directions
!       IS      counter of spectral frequencies

   INTEGER :: ID, IS

!       EFAC    multiplication factor written to file

   REAL    :: EFAC

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWOUTP (SWAN/OUTP)
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       determine maximum value of ACLOC
!       if maximum = 0
!       then write 'ZERO' to file
!       else write 'FACTOR'
!            determine multiplication factor, write this to file
!            write values of ACLOC/factor to file
!       ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT, 'WRSPEC')

!     first determine maximum energy density
   EFAC = 0.
   DO ID = 1, MDC
      DO IS = 1, MSC
         IF (ACLOC(ID,IS).GE.0.) THEN
            EFAC = MAX (EFAC, ACLOC(ID,IS))
         ELSE
            EFAC = MAX (EFAC, 10.*ABS(ACLOC(ID,IS)))
         ENDIF
      ENDDO
   ENDDO
   IF (EFAC .LE. 1.E-10) THEN
      WRITE (NREF, "(A4)") 'ZERO'
   ELSE
      EFAC = 1.01 * EFAC * 10.**(-DEC_SPEC)
!       factor PI/180 introduced to account for change from rad to degr
!       factor 2*PI to account for transition from rad/s to Hz
      WRITE (NREF, "('FACTOR', /, E18.8)") EFAC * 2. * PI**2 / 180.
      DO IS = 1, MSC
!         write spectral energy densities to file
         WRITE (NREF, FIX_SPEC) (NINT(ACLOC(ID,IS)/EFAC), ID=1,MDC)
      ENDDO
   ENDIF
   RETURN
!     end of subroutine WRSPEC
end subroutine WRSPEC
!****************************************************************

SUBROUTINE SWACC(AC2, AC2OLD, ACNRMS, ISSTOP, IDCMIN, IDCMAX)

!****************************************************************

   USE SWCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


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

   INTEGER IDCMIN(MSC), IDCMAX(MSC), ISSTOP
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

   DO IS = 1, ISSTOP
      DO IDDUM = IDCMIN(IS), IDCMAX(IS)
         ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1

!           *** determine infinity norms |ac2 - ac2old| and |ac2|

         DIFFAC = ABS(AC2(ID,IS,KCGRD(1)) - AC2OLD(ID,IS))
         IF (DIFFAC.GT.ACNRMS(1)) ACNRMS(1) = DIFFAC
         IF (ABS(AC2(ID,IS,KCGRD(1))).GT.ACNRMS(2))&
         &ACNRMS(2) = ABS(AC2(ID,IS,KCGRD(1)))

      END DO
   END DO

   RETURN
end subroutine SWACC
!TIMG!****************************************************************
!TIMG!
!TIMGSUBROUTINE SWTSTA (ITIMER)
!TIMG!
!TIMG!****************************************************************
!TIMG!
!TIMG   USE TIMECOMM
!TIMG   USE OCPCOMM4
!TIMG!
!TIMG   IMPLICIT NONE
!TIMG!
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
!TIMG!
!TIMG!  0. Authors
!TIMG!
!TIMG!     40.23: Marcel Zijlema
!TIMG!     40.41: Marcel Zijlema
!TIMG!
!TIMG!  1. Updates
!TIMG!
!TIMG!     40.23, Aug. 02: New subroutine
!TIMG!     40.41, Oct. 04: common blocks replaced by modules, include files r
!TIMG!
!TIMG!  2. Purpose
!TIMG!
!TIMG!     Start timing
!TIMG!
!TIMG!  3. Method
!TIMG!
!TIMG!     Get cpu and wall-clock times and store
!TIMG!
!TIMG!  4. Argument variables
!TIMG!
!TIMG!     ITIMER      number of timer to be used
!TIMG!
!TIMG   INTEGER :: ITIMER
!TIMG!
!TIMG!  6. Local variables
!TIMG!
!TIMG!     C     :     clock count of processor
!TIMG!     I     :     index in LISTTM, loop variable
!TIMG!     IFOUND:     index in LISTTM, location of ITIMER
!TIMG!     IFREE :     index in LISTTM, first free position
!TIMG!     M     :     maximum clock count
!TIMG!     R     :     number of clock counts per second
!TIMG!F95!     TIMER :     current real cpu-time
!TIMG!     TIMER1:     current cpu-time used
!TIMG!     TIMER2:     current wall-clock time used
!TIMG!
!TIMG   INTEGER          :: I, IFOUND, IFREE
!TIMG   INTEGER          :: C, R, M
!TIMG!F95   REAL             :: TIMER
!TIMG   REAL(KIND=KIND(0.0D0)) :: TIMER1, TIMER2
!TIMG!
!TIMG!  7. Common blocks used
!TIMG!
!TIMG!
!TIMG!  8. Subroutines used
!TIMG!
!TIMG!F95!     CPU_TIME         Returns real value from cpu-time clock
!TIMG!     SYSTEM_CLOCK     Returns integer values from a real-time clock
!TIMG!
!TIMG!  9. Subroutines calling
!TIMG!
!TIMG!     SWMAIN, SWCOMP, SWOMPU
!TIMG!
!TIMG! 12. Structure
!TIMG!
!TIMG!     Get and store the cpu and wall-clock times
!TIMG!
!TIMG! 13. Source text
!TIMG!
!TIMG
!TIMG!
!TIMG!     --- check whether a valid timer number is given
!TIMG!
!TIMG   IF (ITIMER.LE.0 .OR. ITIMER.GT.NSECTM) THEN
!TIMG      WRITE(PRINTF,*) 'SWTSTA: ITIMER out of range: ',&
!TIMG      &ITIMER, 1, NSECTM
!TIMG      STOP
!TIMG   END IF
!TIMG!
!TIMG!     --- check whether timing for ITIMER was started already,
!TIMG!         also determine first free location in LISTTM
!TIMG!
!TIMG   IFOUND=0
!TIMG   IFREE =0
!TIMG   I     =0
!TIMG   DO WHILE (I.LT.LASTTM .AND. (IFOUND.EQ.0 .OR. IFREE.EQ.0))
!TIMG      I=I+1
!TIMG      IF (LISTTM(I).EQ.ITIMER) THEN
!TIMG         IFOUND=I
!TIMG      END IF
!TIMG      IF (IFREE.EQ.0 .AND. LISTTM(I).EQ.-1) THEN
!TIMG         IFREE =I
!TIMG      END IF
!TIMG   END DO
!TIMG
!TIMG   IF (IFOUND.EQ.0 .AND. IFREE.EQ.0 .AND. LASTTM.LT.MXTIMR) THEN
!TIMG      LASTTM=LASTTM+1
!TIMG      IFREE =LASTTM
!TIMG   END IF
!TIMG!
!TIMG!     --- produce warning if found in the list
!TIMG!
!TIMG   IF (IFOUND.GT.0) THEN
!TIMG      WRITE(PRINTF,*)&
!TIMG      &'SWTSTA: warning: previous timing for section ',&
!TIMG      &ITIMER,' not closed properly/will be ignored.'
!TIMG   END IF
!TIMG!
!TIMG!     --- produce error if not found and no free position available
!TIMG!
!TIMG   IF (IFOUND.EQ.0 .AND. IFREE.EQ.0) THEN
!TIMG      WRITE(PRINTF,*)&
!TIMG      &'SWTSTA: maximum number of simultaneous timers',&
!TIMG      &' exceeded:',MXTIMR
!TIMG      STOP
!TIMG   END IF
!TIMG!
!TIMG!     --- register ITIMER in appropriate location of LISTTM
!TIMG!
!TIMG   IF (IFOUND.EQ.0) THEN
!TIMG      IFOUND=IFREE
!TIMG   END IF
!TIMG   LISTTM(IFOUND)=ITIMER
!TIMG!
!TIMG!     --- get current cpu/wall-clock time and store in TIMERS
!TIMG!
!TIMG   TIMER1=0D0
!TIMG!F95   CALL CPU_TIME (TIMER)
!TIMG!F95   TIMER1=DBLE(TIMER)
!TIMG   CALL SYSTEM_CLOCK (C,R,M)
!TIMG   TIMER2=DBLE(C)/DBLE(R)
!TIMG
!TIMG   TIMERS(IFOUND,1)=TIMER1
!TIMG   TIMERS(IFOUND,2)=TIMER2
!TIMG
!TIMG   RETURN
!TIMGend subroutine SWTSTA
!TIMG!****************************************************************
!TIMG!
!TIMGSUBROUTINE SWTSTO (ITIMER)
!TIMG!
!TIMG!****************************************************************
!TIMG!
!TIMG   USE TIMECOMM
!TIMG   USE OCPCOMM4
!TIMG!
!TIMG   IMPLICIT NONE
!TIMG!
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
!TIMG!
!TIMG!  0. Authors
!TIMG!
!TIMG!     40.23: Marcel Zijlema
!TIMG!     40.41: Marcel Zijlema
!TIMG!
!TIMG!  1. Updates
!TIMG!
!TIMG!     40.23, Aug. 02: New subroutine
!TIMG!     40.41, Oct. 04: common blocks replaced by modules, include files r
!TIMG!
!TIMG!  2. Purpose
!TIMG!
!TIMG!     Stop timing
!TIMG!
!TIMG!  3. Method
!TIMG!
!TIMG!     Get cpu and wall-clock times and store
!TIMG!
!TIMG!  4. Argument variables
!TIMG!
!TIMG!     ITIMER      number of timer to be used
!TIMG!
!TIMG   INTEGER :: ITIMER
!TIMG!
!TIMG!  6. Local variables
!TIMG!
!TIMG!     C     :     clock count of processor
!TIMG!     I     :     index in LISTTM, loop variable
!TIMG!     IFOUND:     index in LISTTM, location of ITIMER
!TIMG!     M     :     maximum clock count
!TIMG!     R     :     number of clock counts per second
!TIMG!F95!     TIMER :     current real cpu-time
!TIMG!     TIMER1:     current cpu-time used
!TIMG!     TIMER2:     current wall-clock time used
!TIMG!
!TIMG   INTEGER          :: I, IFOUND
!TIMG   INTEGER          :: C, R, M
!TIMG!F95   REAL             :: TIMER
!TIMG   REAL(KIND=KIND(0.0D0)) :: TIMER1, TIMER2
!TIMG!
!TIMG!  7. Common blocks used
!TIMG!
!TIMG!
!TIMG!  8. Subroutines used
!TIMG!
!TIMG!F95!     CPU_TIME         Returns real value from cpu-time clock
!TIMG!     SYSTEM_CLOCK     Returns integer values from a real-time clock
!TIMG!
!TIMG!  9. Subroutines calling
!TIMG!
!TIMG!     SWMAIN, SWCOMP, SWOMPU
!TIMG!
!TIMG! 12. Structure
!TIMG!
!TIMG!     Get and store the cpu and wall-clock times
!TIMG!
!TIMG! 13. Source text
!TIMG!
!TIMG
!TIMG!
!TIMG!     --- check whether a valid timer number is given
!TIMG!
!TIMG   IF (ITIMER.LE.0 .OR. ITIMER.GT.NSECTM) THEN
!TIMG      WRITE(PRINTF,*) 'SWTSTO: ITIMER out of range: ',&
!TIMG      &ITIMER, 1, NSECTM
!TIMG      STOP
!TIMG   END IF
!TIMG!
!TIMG!     --- check whether timing for ITIMER was started already,
!TIMG!         also determine first free location in LISTTM
!TIMG!
!TIMG   IFOUND=0
!TIMG   I     =0
!TIMG   DO WHILE (I.LT.LASTTM .AND. IFOUND.EQ.0)
!TIMG      I=I+1
!TIMG      IF (LISTTM(I).EQ.ITIMER) THEN
!TIMG         IFOUND=I
!TIMG      END IF
!TIMG   END DO
!TIMG!
!TIMG!     --- produce error if not found
!TIMG!
!TIMG   IF (IFOUND.EQ.0) THEN
!TIMG      WRITE(PRINTF,*)&
!TIMG      &'SWTSTO: section ',ITIMER,' not found',&
!TIMG      &' in list of active timings'
!TIMG      STOP
!TIMG   END IF
!TIMG!
!TIMG!     --- get current cpu/wall-clock time
!TIMG!
!TIMG   TIMER1=0D0
!TIMG!F95   CALL CPU_TIME (TIMER)
!TIMG!F95   TIMER1=DBLE(TIMER)
!TIMG   CALL SYSTEM_CLOCK (C,R,M)
!TIMG   TIMER2=DBLE(C)/DBLE(R)
!TIMG!
!TIMG!     --- calculate elapsed time since start of timing,
!TIMG!         store in appropriate location in DCUMTM,
!TIMG!         increment number of timings for current section
!TIMG!
!TIMG   DCUMTM(ITIMER,1)=DCUMTM(ITIMER,1)+(TIMER1-TIMERS(IFOUND,1))
!TIMG   DCUMTM(ITIMER,2)=DCUMTM(ITIMER,2)+(TIMER2-TIMERS(IFOUND,2))
!TIMG   NCUMTM(ITIMER)  =NCUMTM(ITIMER)+1
!TIMG!
!TIMG!     --- free appropriate location of LISTTM,
!TIMG!         adjust last occupied position of LISTTM
!TIMG!
!TIMG   IF (IFOUND.GT.0) THEN
!TIMG      LISTTM(IFOUND)=-1
!TIMG   END IF
!TIMG   DO WHILE (LASTTM.GT.1 .AND. LISTTM(LASTTM).EQ.-1)
!TIMG      LASTTM=LASTTM-1
!TIMG   END DO
!TIMG   IF (LISTTM(LASTTM).EQ.-1) LASTTM=0
!TIMG
!TIMG   RETURN
!TIMGend subroutine SWTSTO
!TIMG!****************************************************************
!TIMG!
!TIMGSUBROUTINE SWPRTI
!TIMG!
!TIMG!****************************************************************
!TIMG!
!TIMG   USE OCPCOMM4
!TIMG   USE TIMECOMM
!TIMG   USE M_PARALL
!TIMG
!TIMG   IMPLICIT NONE
!TIMG!
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
!TIMG!
!TIMG!  0. Authors
!TIMG!
!TIMG!     40.23: Marcel Zijlema
!TIMG!     40.30: Marcel Zijlema
!TIMG!     40.41: Marcel Zijlema
!TIMG!     41.75: Erick Rogers
!TIMG!
!TIMG!  1. Updates
!TIMG!
!TIMG!     40.23, Aug. 02: New subroutine
!TIMG!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!TIMG!     40.41, Oct. 04: common blocks replaced by modules, include files r
!TIMG!     41.75, Jan. 19: adding sea ice
!TIMG!
!TIMG!  2. Purpose
!TIMG!
!TIMG!     Print timings info
!TIMG!
!TIMG!  6. Local variables
!TIMG!
!TIMG!     IDEBUG:     level of timing output requested:
!TIMG!                 0 - no output for detailed timings
!TIMG!                 1 - aggregate output for detailed timings
!TIMG!                 2 - complete output for all detailed timings
!TIMG!     IENT  :     number of entries
!TIMG!     J     :     loop counter
!TIMG!     K     :     loop counter
!TIMG!     MYPRC :     own process number
!TIMG!     TABLE :     array for computing aggregate cpu- and wallclock-times
!TIMG!
!TIMG   INTEGER          :: J, K, MYPRC
!TIMG   INTEGER, PARAMETER :: IDEBUG = 0
!TIMG   INTEGER, SAVE :: IENT = 0
!TIMG   REAL(KIND=KIND(0.0D0)) :: TABLE(33,2)
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT110 = "(i3,1x,'#')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT111 = "(i3,' # Details on timings of the simulation:')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT112 = "(i3,1x,'#',26x,'cpu-time',1x,'wall-clock')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT113 = "(i3,' # Splitting up calc. + comm. times:')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT114 = "(i3,' # Overview source contributions:')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT115 = "(i3,1x,'#',1x,a22,2f11.2)"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT120 = "(/,i3,' #    item     cpu-time    real time     count')"
!TIMG   CHARACTER(LEN=*), PARAMETER :: FMT121 = "(i3,1x,'#',4x,i4,2f13.4,i10)"
!TIMG!
!TIMG!  7. Common blocks used
!TIMG!
!TIMG!
!TIMG!  8. Subroutines used
!TIMG!
!TIMG!     STRACE           Tracing routine for debugging
!TIMG!
!TIMG!  9. Subroutines calling
!TIMG!
!TIMG!     SWMAIN (in SWANMAIN)
!TIMG!
!TIMG! 12. Structure
!TIMG!
!TIMG!     Compile table with overview of cpu/wall clock time used in
!TIMG!     important parts of SWAN and write to PRINT file
!TIMG!
!TIMG! 13. Source text
!TIMG!
!TIMG   IF (LTRACE) CALL STRACE (IENT,'SWPRTI')
!TIMG!
!TIMG   MYPRC = INODE
!TIMG!
!TIMG!     --- compile table with overview of cpu/wall clock time used in
!TIMG!         important parts of SWAN and write to PRINT file
!TIMG!
!TIMG   IF ( ITEST.GE.1 .OR. IDEBUG.GE.1 ) THEN
!TIMG!
!TIMG!        --- initialise table to zero
!TIMG!
!TIMG      DO K = 1, 30
!TIMG         DO J = 1, 2
!TIMG            TABLE(K,J) = 0D0
!TIMG         END DO
!TIMG      END DO
!TIMG!
!TIMG!        --- compute times for basic blocks
!TIMG!
!TIMG      DO J = 1, 2
!TIMG!
!TIMG!           --- total run-time
!TIMG!
!TIMG         TABLE(1,J) = DCUMTM(1,J)
!TIMG!
!TIMG!           --- initialisation, reading, preparation:
!TIMG!
!TIMG         DO K = 2, 7
!TIMG            TABLE(2,J) = TABLE(2,J) + DCUMTM(K,J)
!TIMG         END DO
!TIMG!
!TIMG!           --- domain decomposition:
!TIMG!
!TIMG         TABLE(2,J) = TABLE(2,J) + DCUMTM(211,J)
!TIMG         TABLE(2,J) = TABLE(2,J) + DCUMTM(212,J)
!TIMG!JAC         TABLE(2,J) = TABLE(2,J) + DCUMTM(215,J)
!TIMG         TABLE(2,J) = TABLE(2,J) + DCUMTM(201,J)
!TIMG!
!TIMG!           --- total calculation including communication:
!TIMG!
!TIMG         TABLE(3,J) = TABLE(3,J) + DCUMTM(8,J)
!TIMG!
!TIMG!           --- output:
!TIMG!
!TIMG         TABLE(5,J) = TABLE(5,J) + DCUMTM(9,J)
!TIMG!
!TIMG!           --- exchanging data:
!TIMG!
!TIMG         TABLE(7,J) = TABLE(7,J) + DCUMTM(213,J)
!TIMG!
!TIMG!           --- solving system:
!TIMG!
!TIMG         TABLE(9,J) = TABLE(9,J) + DCUMTM(119,J)
!TIMG         TABLE(9,J) = TABLE(9,J) + DCUMTM(120,J)
!TIMG!
!TIMG!           --- global reductions:
!TIMG!
!TIMG         TABLE(10,J) = TABLE(10,J) + DCUMTM(202,J)
!TIMG!
!TIMG!           --- collecting data:
!TIMG!
!TIMG         TABLE(11,J) = TABLE(11,J) + DCUMTM(214,J)
!TIMG!
!TIMG!           --- setup:
!TIMG!
!TIMG         TABLE(12,J) = TABLE(12,J) + DCUMTM(106,J)
!TIMG!
!TIMG!           --- propagation velocities:
!TIMG!
!TIMG         TABLE(14,J) = TABLE(14,J) + DCUMTM(111,J)
!TIMG         TABLE(14,J) = TABLE(14,J) + DCUMTM(113,J)
!TIMG         TABLE(14,J) = TABLE(14,J) + DCUMTM(114,J)
!TIMG!
!TIMG!           --- x-y advection:
!TIMG!
!TIMG         TABLE(15,J) = TABLE(15,J) + DCUMTM(140,J)
!TIMG!
!TIMG!           --- sigma advection:
!TIMG!
!TIMG         TABLE(16,J) = TABLE(16,J) + DCUMTM(141,J)
!TIMG!
!TIMG!           --- theta advection:
!TIMG!
!TIMG         TABLE(17,J) = TABLE(17,J) + DCUMTM(142,J)
!TIMG!
!TIMG!           --- wind:
!TIMG!
!TIMG         TABLE(18,J) = TABLE(18,J) + DCUMTM(132,J)
!TIMG!
!TIMG!           --- whitecapping:
!TIMG!
!TIMG         TABLE(19,J) = TABLE(19,J) + DCUMTM(133,J)
!TIMG!
!TIMG!           --- bottom friction:
!TIMG!
!TIMG         TABLE(20,J) = TABLE(20,J) + DCUMTM(130,J)
!TIMG!
!TIMG!           --- wave breaking:
!TIMG!
!TIMG         TABLE(21,J) = TABLE(21,J) + DCUMTM(131,J)
!TIMG!
!TIMG!           --- quadruplets:
!TIMG!
!TIMG         TABLE(22,J) = TABLE(22,J) + DCUMTM(135,J)
!TIMG!
!TIMG!           --- triads:
!TIMG!
!TIMG         TABLE(23,J) = TABLE(23,J) + DCUMTM(134,J)
!TIMG!
!TIMG!           --- limiter:
!TIMG!
!TIMG         TABLE(24,J) = TABLE(24,J) + DCUMTM(122,J)
!TIMG!
!TIMG!           --- rescaling:
!TIMG!
!TIMG         TABLE(25,J) = TABLE(25,J) + DCUMTM(121,J)
!TIMG!
!TIMG!           --- reflections:
!TIMG!
!TIMG         TABLE(26,J) = TABLE(26,J) + DCUMTM(136,J)
!TIMG!
!TIMG!           --- diffraction:
!TIMG!
!TIMG         TABLE(27,J) = TABLE(27,J) + DCUMTM(137,J)
!TIMG!
!TIMG!           --- fluid mud:
!TIMG!
!TIMG         TABLE(28,J) = TABLE(28,J) + DCUMTM(138,J)
!TIMG!
!TIMG!           --- vegetation:
!TIMG!
!TIMG         TABLE(29,J) = TABLE(29,J) + DCUMTM(139,J)
!TIMG!
!TIMG!           --- turbulence:
!TIMG!
!TIMG         TABLE(30,J) = TABLE(30,J) + DCUMTM(143,J)
!TIMG
!TIMG!           --- sea ice:
!TIMG!
!TIMG         TABLE(31,J) = TABLE(31,J) + DCUMTM(144,J)
!TIMG
!TIMG!           --- Bragg scattering:
!TIMG!
!TIMG         TABLE(32,J) = TABLE(32,J) + DCUMTM(145,J)
!TIMG
!TIMG!           --- quasi-coherent scattering:
!TIMG!
!TIMG         TABLE(33,J) = TABLE(33,J) + DCUMTM(146,J)
!TIMG
!TIMG      END DO
!TIMG!
!TIMG!        --- add up times for some basic blocks
!TIMG!
!TIMG      DO J = 1, 2
!TIMG!
!TIMG!           --- total calculation:
!TIMG!
!TIMG         TABLE(3,J) = TABLE(3,J) - TABLE( 7,J)
!TIMG         TABLE(3,J) = TABLE(3,J) - TABLE(10,J)
!TIMG         IF ( TABLE(3,J).LT.0D0 ) TABLE(3,J) = 0D0
!TIMG!
!TIMG!           --- total communication:
!TIMG!                * exchanging data
!TIMG!                * global reductions
!TIMG!                * collecting data
!TIMG!
!TIMG         TABLE(4,J) = TABLE(4,J) + TABLE( 7,J)
!TIMG         TABLE(4,J) = TABLE(4,J) + TABLE(10,J)
!TIMG         TABLE(4,J) = TABLE(4,J) + TABLE(11,J)
!TIMG!
!TIMG!           --- total propagation:
!TIMG!                * velocities and derivatives
!TIMG!
!TIMG         TABLE(6,J) = TABLE(6,J) + TABLE(14,J)
!TIMG         TABLE(6,J) = TABLE(6,J) + TABLE(15,J)
!TIMG         TABLE(6,J) = TABLE(6,J) + TABLE(16,J)
!TIMG         TABLE(6,J) = TABLE(6,J) + TABLE(17,J)
!TIMG!
!TIMG!           --- sources:
!TIMG!                * wind, whitecapping, friction, breaking,
!TIMG!                * quadruplets, triads, limiter, rescaling,
!TIMG!                * reflections
!TIMG!
!TIMG         DO K = 18, 26
!TIMG            TABLE(8,J) = TABLE(8,J) + TABLE(K,J)
!TIMG         END DO
!TIMG!
!TIMG!                * diffraction
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(27,J)
!TIMG!
!TIMG!                * fluid mud
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(28,J)
!TIMG!
!TIMG!                * vegetation
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(29,J)
!TIMG!
!TIMG!                * turbulence
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(30,J)
!TIMG!
!TIMG!                * sea ice
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(31,J)
!TIMG!
!TIMG!                * Bragg scattering
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(32,J)
!TIMG!
!TIMG!                * quasi-coherent scattering
!TIMG!
!TIMG         TABLE(8,J) = TABLE(8,J) + TABLE(33,J)
!TIMG!
!TIMG!           --- other computing:
!TIMG!
!TIMG         TABLE(13,J) = TABLE(13,J) + TABLE( 3,J)
!TIMG         TABLE(13,J) = TABLE(13,J) - TABLE( 6,J)
!TIMG         TABLE(13,J) = TABLE(13,J) - TABLE( 8,J)
!TIMG         TABLE(13,J) = TABLE(13,J) - TABLE( 9,J)
!TIMG         TABLE(13,J) = TABLE(13,J) - TABLE(12,J)
!TIMG         IF ( TABLE(13,J).LT.0D0 ) TABLE(13,J) = 0D0
!TIMG
!TIMG      END DO
!TIMG!
!TIMG!        --- print CPU-times used in important parts of SWAN
!TIMG!
!TIMG      WRITE(PRINTF,'(/)')
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT111) MYPRC
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT112) MYPRC
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'total time:'       ,(TABLE(1,J),J=1,2)
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'total pre-processing:',&
!TIMG      &(TABLE(2,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'total calculation:',(TABLE(3,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'total communication:',&
!TIMG      &(TABLE(4,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'total post-processing:',&
!TIMG      &(TABLE(5,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT113) MYPRC
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'calc. propagation:',(TABLE(6,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'exchanging data:'  ,(TABLE(7,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'calc. sources:'    ,(TABLE(8,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'solving system:'   ,(TABLE(9,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'reductions:'      ,(TABLE(10,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'collecting data:' ,(TABLE(11,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'calc. setup:'     ,(TABLE(12,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'other computing:' ,(TABLE(13,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT114) MYPRC
!TIMG      WRITE(PRINTF,FMT110) MYPRC
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'prop. velocities:',(TABLE(14,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'x-y advection:'   ,(TABLE(15,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'sigma advection:' ,(TABLE(16,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'theta advection:' ,(TABLE(17,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'wind:'            ,(TABLE(18,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'whitecapping:'    ,(TABLE(19,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'bottom friction:' ,(TABLE(20,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'fluid mud:'       ,(TABLE(28,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'vegetation:'      ,(TABLE(29,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'turbulence:'      ,(TABLE(30,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'sea ice:'         ,(TABLE(31,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'wave breaking:'   ,(TABLE(21,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'quadruplets:'     ,(TABLE(22,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'triads:'          ,(TABLE(23,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'Bragg scattering:',(TABLE(32,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'QC scattering:'   ,(TABLE(33,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'limiter:'         ,(TABLE(24,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'rescaling:'       ,(TABLE(25,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'reflections:'     ,(TABLE(26,j),j=1,2)
!TIMG      WRITE(PRINTF,FMT115) MYPRC,'diffraction:'     ,(TABLE(27,j),j=1,2)
!TIMG
!TIMG   END IF
!TIMG
!TIMG   IF ( IDEBUG.GE.2 ) THEN
!TIMG      WRITE(PRINTF,FMT120) MYPRC
!TIMG      DO J = 1, NSECTM
!TIMG         IF (NCUMTM(J).GT.0)&
!TIMG         &WRITE(PRINTF,FMT121) MYPRC,J,DCUMTM(J,1),DCUMTM(J,2),&
!TIMG         &NCUMTM(J)
!TIMG      END DO
!TIMG   END IF
!TIMG
!TIMG
!TIMG   RETURN
!TIMGend subroutine SWPRTI
!****************************************************************

SUBROUTINE TXPBLA(TEXT,IF,IL)

!****************************************************************

   IMPLICIT NONE


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

   INTEGER IF, IL
   CHARACTER(LEN=*) :: TEXT

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
!DOS   ITABVL = 9
!UNIX  ITABVL = 9
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

CHARACTER(LEN=20) FUNCTION INTSTR ( IVAL )

!****************************************************************

   IMPLICIT NONE


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
!     Convert integer to string
!
!  4. Argument variables
!
!     IVAL        integer to be converted

   INTEGER IVAL

!  6. Local variables
!
!     CVAL  :     character represented an integer of mantisse
!     I     :     counter
!     IPOS  :     position in mantisse
!     IQUO  :     whole quotient

   INTEGER I, IPOS, IQUO
   CHARACTER(LEN=1), ALLOCATABLE :: CVAL(:)

! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IPOS = 1
   DO WHILE (IVAL/10**IPOS.GE.1.)
      IPOS = IPOS + 1
   END DO
   ALLOCATE(CVAL(IPOS))

   DO I=IPOS,1,-1
      IQUO=IVAL/10**(I-1)
      CVAL(IPOS-I+1)=CHAR(INT(IQUO)+48)
      IVAL=IVAL-IQUO*10**(I-1)
   END DO

   WRITE (INTSTR,*) (CVAL(I), I=1,IPOS)

   RETURN
end function INTSTR
!****************************************************************

CHARACTER(LEN=20) FUNCTION NUMSTR ( IVAL, RVAL, FORM )

!****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


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
!     40.23, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Convert integer or real to string with given format
!
!  4. Argument variables
!
!     IVAL        integer to be converted
!     FORM        given format
!     RVAL        real to be converted

   INTEGER   IVAL
   REAL      RVAL
   CHARACTER(LEN=20) :: FORM

!  6. Local variables
!
! 12. Structure
!
!     Trivial.
!
! 13. Source text

   IF ( IVAL.NE.INAN ) THEN
      WRITE (NUMSTR,FORM) IVAL
   ELSE IF ( RVAL.NE.RNAN ) THEN
      WRITE (NUMSTR,FORM) RVAL
   ELSE
      NUMSTR = ''
   END IF

   RETURN
end function NUMSTR
!****************************************************************

SUBROUTINE SWCOPI ( IARR1, IARR2, LENGTH )

!****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


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
!     40.23, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Copies integer array IARR1 to IARR2
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     IARR1       source array
!     IARR2       target array
!     LENGTH      array length

   INTEGER LENGTH
   INTEGER IARR1(LENGTH), IARR2(LENGTH)

!  6. Local variables
!
!     I     :     loop counter
!     IENT  :     number of entries

   INTEGER, SAVE :: IENT = 0
   INTEGER I

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
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
!     Trivial.
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOPI')

!     --- check array length

   IF ( LENGTH.LE.0 ) THEN
      CALL MSGERR( 3, 'Array length should be positive' )
   END IF

!     --- copy elements of array IARR1 to IARR2

   do I = 1, LENGTH
      IARR2(I) = IARR1(I)
   end do

   RETURN
end subroutine SWCOPI
!****************************************************************

SUBROUTINE SWCOPR ( ARR1, ARR2, LENGTH )

!****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


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
!     40.23, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Copies real array ARR1 to ARR2
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     ARR1        source array
!     ARR2        target array
!     LENGTH      array length

   INTEGER LENGTH
   REAL    ARR1(LENGTH), ARR2(LENGTH)

!  6. Local variables
!
!     I     :     loop counter
!     IENT  :     number of entries

   INTEGER, SAVE :: IENT = 0
   INTEGER I

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
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
!     Trivial.
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOPR')

!     --- check array length

   IF ( LENGTH.LE.0 ) THEN
      CALL MSGERR( 3, 'Array length should be positive' )
   END IF

!     --- copy elements of array ARR1 to ARR2

   do I = 1, LENGTH
      ARR2(I) = ARR1(I)
   end do

   RETURN
end subroutine SWCOPR
!MatL4!****************************************************************
!MatL4!
!MatL4SUBROUTINE SWI2B ( IVAL, BVAL )
!MatL4!
!MatL4!****************************************************************
!MatL4!
!MatL4   USE OCPCOMM4
!MatL4!
!MatL4   IMPLICIT NONE
!MatL4!
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
!MatL4!
!MatL4!  0. Authors
!MatL4!
!MatL4!     40.30: Marcel Zijlema
!MatL4!     40.41: Marcel Zijlema
!MatL4!
!MatL4!  1. Updates
!MatL4!
!MatL4!     40.30, May 03: New subroutine
!MatL4!     40.41, Oct. 04: common blocks replaced by modules, include files r
!MatL4!
!MatL4!  2. Purpose
!MatL4!
!MatL4!     Calculates 32-bit representation of an integer number
!MatL4!
!MatL4!  3. Method
!MatL4!
!MatL4!     The representation of an integer number is divided into 4 parts
!MatL4!     of 8 bits each, resulting in 32-bit word in memory. Generally,
!MatL4!     storage words are represented with bits counted from the right,
!MatL4!     making bit 0 the lower-order bit and bit 31 the high-order bit,
!MatL4!     which is also the sign bit.
!MatL4!
!MatL4!     The integer number is always an exact representation of an
!MatL4!     integer of value positive, negative, or zero. Each bit, except
!MatL4!     the leftmost bit, corresponds to the actual exponent as power
!MatL4!     of two.
!MatL4!
!MatL4!     For representing negative numbers, the method called
!MatL4!     "excess 2**(m - 1)" is used, which represents an m-bit number by
!MatL4!     storing it as the sum of itself and 2**(m - 1). For a 32-bit
!MatL4!     machine, m = 32. This results in a positive number, so the
!MatL4!     leftmost bit need to be reversed. This method is identical to the
!MatL4!     two's complement method.
!MatL4!
!MatL4!     An example:
!MatL4!
!MatL4!        the 32-bit representation of 5693 is
!MatL4!
!MatL4!        decimal    :     0        0       22       61
!MatL4!        hexidecimal:     0        0       16       3D
!MatL4!        binary     : 00000000 00000000 00010110 00111101
!MatL4!
!MatL4!        since,
!MatL4!
!MatL4!        5693 = 2^12 + 2^10 + 2^9 + 2^5 + 2^4 + 2^3 + 2^2 + 2^0
!MatL4!
!MatL4!  4. Argument variables
!MatL4!
!MatL4!     BVAL        a byte value as a part of the representation of
!MatL4!                 integer number
!MatL4!     IVAL        integer number
!MatL4!
!MatL4   INTEGER BVAL(4), IVAL
!MatL4!
!MatL4!  6. Local variables
!MatL4!
!MatL4!     I     :     loop counter
!MatL4!     IENT  :     number of entries
!MatL4!     IQUOT :     auxiliary integer with quotient
!MatL4!     M     :     maximal exponent number possible (for 32-bit machine,
!MatL4!
!MatL4   INTEGER I, IQUOT
!MatL4   INTEGER, PARAMETER :: M = 32
!MatL4   INTEGER, SAVE :: IENT = 0
!MatL4!
!MatL4! 12. Structure
!MatL4!
!MatL4!     initialise 4 parts of the representation
!MatL4!     if integer < 0, increased it by 2**(m-1)
!MatL4!     compute the actual part of the representation
!MatL4!     determine the sign bit
!MatL4!
!MatL4! 13. Source text
!MatL4!
!MatL4   IF (LTRACE) CALL STRACE (IENT,'SWI2B')
!MatL4
!MatL4!     --- initialise 4 parts of the representation
!MatL4
!MatL4   DO I = 1, 4
!MatL4      BVAL(I) = 0
!MatL4   END DO
!MatL4
!MatL4   IQUOT = IVAL
!MatL4
!MatL4!     --- clear the sign bit before splitting a negative integer
!MatL4
!MatL4   IF ( IVAL < 0 ) IQUOT = IBCLR(IQUOT, M - 1)
!MatL4
!MatL4!     --- compute the actual part of the representation
!MatL4
!MatL4   DO I = 4, 1, -1
!MatL4      BVAL(I) = MOD(IQUOT,256)
!MatL4      IQUOT = INT(IQUOT/256)
!MatL4   END DO
!MatL4
!MatL4!     --- determine the sign bit
!MatL4
!MatL4   IF ( IVAL.LT.0 ) BVAL(1) = BVAL(1) + 128
!MatL4
!MatL4   RETURN
!MatL4end subroutine SWI2B
!MatL4!****************************************************************
!MatL4!
!MatL4SUBROUTINE SWR2B ( RVAL, BVAL )
!MatL4!
!MatL4!****************************************************************
!MatL4!
!MatL4   USE OCPCOMM4
!MatL4!
!MatL4   IMPLICIT NONE
!MatL4!
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
!MatL4!
!MatL4!  0. Authors
!MatL4!
!MatL4!     40.30: Marcel Zijlema
!MatL4!     40.41: Marcel Zijlema
!MatL4!
!MatL4!  1. Updates
!MatL4!
!MatL4!     40.30, May 03: New subroutine
!MatL4!     40.41, Oct. 04: common blocks replaced by modules, include files r
!MatL4!
!MatL4!  2. Purpose
!MatL4!
!MatL4!     Calculates 32-bit representation of a floating-point number
!MatL4!
!MatL4!  3. Method
!MatL4!
!MatL4!     The representation of a floating-point number is divided into 4
!MatL4!     parts of 8 bits each, resulting in 32-bit word in memory.
!MatL4!     Generally, storage words are represented with bits counted from
!MatL4!     the right, making bit 0 the lower-order bit and bit 31 the
!MatL4!     high-order bit, which is also the sign bit.
!MatL4!
!MatL4!     The floating-point number is a processor approximation. Its format
!MatL4!     has an 8-bit biased exponent and a 23-bit fraction or mantissa. Th
!MatL4!     leftmost bit is the sign bit which is zero for plus and 1 for
!MatL4!     minus. The biased exponent equals the bias and the actual exponent
!MatL4!     (power of two) of the number. For a 32-bit machine, bias=127.
!MatL4!
!MatL4!     Furthermore, the floating-point number is usually stored in the
!MatL4!     normalized form, i.e. it has a binary point to the left of the
!MatL4!     mantissa and an implied leading 1 to the left of the binary point.
!MatL4!     Thus, if X is a floating-point number, then it is calculated as
!MatL4!     follows:
!MatL4!
!MatL4!         X = (-1)**sign bit 1.fraction * 2**(biased exponent-bias)
!MatL4!
!MatL4!     There are several exceptions. Let a fraction, biased exponent
!MatL4!     and sign bit be denoted as F, E and S, respectively. The following
!MatL4!     formats adhere to IEEE standard:
!MatL4!
!MatL4!     S = 0, E = 00000000 and F  = 00 ... 0 : X = 0
!MatL4!     S = 0, E = 00000000 and F <> 00 ... 0 : X = +0.fraction * 2**(1-bi
!MatL4!     S = 1, E = 00000000 and F <> 00 ... 0 : X = -0.fraction * 2**(1-bi
!MatL4!     S = 0, E = 11111111 and F  = 00 ... 0 : X = +Inf
!MatL4!     S = 1, E = 11111111 and F  = 00 ... 0 : X = -Inf
!MatL4!     S = 0, E = 11111111 and F <> 00 ... 0 : X = NaN
!MatL4!
!MatL4!     A NaN (Not a Number) is a value reserved for signalling an
!MatL4!     attempted invalid operation, like 0/0. Its representation
!MatL4!     equals the representation of +Inf plus 1, i.e. 2**31 - 2**23 + 1
!MatL4!
!MatL4!     An example:
!MatL4!
!MatL4!        the 32-bit representation of 23.1 is
!MatL4!
!MatL4!        decimal    :    65      184      204      205
!MatL4!        hexidecimal:    41       B8       CC       CD
!MatL4!        binary     : 01000001 10111000 11001100 11001101
!MatL4!
!MatL4!        since,
!MatL4!
!MatL4!        23.1 = 2^4 + 2^2 + 2^1 + 2^0 + 2^-4 + 2^-5 + 2^-8 + 2^-9 +
!MatL4!               2^-12 + 2^-13 + 2^-16 + 2^-17 + 2^-19
!MatL4!
!MatL4!        so that the biased exponent = 4 + 127 = 131 = 10000011 = E
!MatL4!        and the sign bit = 0 = S. The remaining of the 32-bit word is
!MatL4!        the fraction, which is
!MatL4!
!MatL4!    3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16 -17
!MatL4!
!MatL4! F= 0 1 1 1  0  0  0  1  1  0  0  1  1   0   0   1   1   0   0   1   1
!MatL4!
!MatL4!  4. Argument variables
!MatL4!
!MatL4!     BVAL        a byte value as a part of the representation of
!MatL4!                 floating-point number
!MatL4!     RVAL        floating-point number
!MatL4!
!MatL4   INTEGER BVAL(4)
!MatL4   REAL    RVAL
!MatL4!
!MatL4!  6. Local variables
!MatL4!
!MatL4!     ACTEXP:     actual exponent in the representation
!MatL4!     BEXPO :     biased exponent
!MatL4!     BIAS  :     bias (for 32-bit machine, bias=127)
!MatL4!     EXPO  :     calculated exponent of floating-point number
!MatL4!     FRAC  :     fraction of floating-point number
!MatL4!     I     :     loop counter
!MatL4!     IENT  :     number of entries
!MatL4!     IPART :     i-the part of the representation
!MatL4!     IQUOT :     auxiliary integer with quotient
!MatL4!     LEADNR:     leading number of floating-point number
!MatL4!     LFRAC :     length of fraction in representation
!MatL4!           :     (for 32-bit machine, lfrac=23)
!MatL4!     RFRAC :     auxiliary real with fraction
!MatL4!
!MatL4   INTEGER ACTEXP, EXPO, I, IPART, BEXPO
!MatL4   INTEGER, PARAMETER :: LFRAC = 23, BIAS = 127
!MatL4   INTEGER, SAVE :: IENT = 0
!MatL4   INTEGER(KIND=SELECTED_INT_KIND(18)) LEADNR, IQUOT
!MatL4   REAL FRAC, RFRAC, MAXREAL
!MatL4!
!MatL4! 12. Structure
!MatL4!
!MatL4!     initialise 4 parts of the representation and biased exponent
!MatL4!     determine leading number and fraction
!MatL4!     do while leading number >= 1
!MatL4!        calculate positive exponent as power of two
!MatL4!     or while fraction > 0
!MatL4!        calculate negative exponent as power of two
!MatL4!     end do
!MatL4!     compute the actual part of the representation
!MatL4!
!MatL4! 13. Source text
!MatL4!
!MatL4   IF (LTRACE) CALL STRACE (IENT,'SWR2B')
!MatL4
!MatL4!     --- initialise 4 parts of the representation and biased exponent
!MatL4
!MatL4   DO I = 1, 4
!MatL4      BVAL(I) = 0
!MatL4   END DO
!MatL4   BEXPO  = -1
!MatL4
!MatL4!     --- clamp reals to the max INTEGER(KIND=SELECTED_INT_KIND(18)), to prevent overflow
!MatL4
!MatL4   MAXREAL=9.0E+18
!MatL4   IF ( RVAL .GT. MAXREAL ) THEN
!MatL4      RVAL=MAXREAL
!MatL4   ELSEIF ( RVAL .LT. -MAXREAL ) THEN
!MatL4      RVAL=-MAXREAL
!MatL4   END IF
!MatL4
!MatL4!     --- determine leading number and fraction
!MatL4
!MatL4   IF ( ABS(RVAL).LT.1.E-7 ) THEN
!MatL4      LEADNR = 0
!MatL4      FRAC   = 0.
!MatL4   ELSE
!MatL4      LEADNR = INT(ABS(RVAL),KIND=8)
!MatL4      FRAC   = ABS(RVAL) - REAL(LEADNR)
!MatL4   END IF
!MatL4
!MatL4   conversion_loop: DO
!MatL4   IF ( LEADNR.GE.1 ) THEN
!MatL4
!MatL4!        --- calculate positive exponent as power of two
!MatL4
!MatL4      EXPO  = 0
!MatL4      IQUOT = LEADNR
!MatL4      DO WHILE (IQUOT.GE.2)
!MatL4
!MatL4         IQUOT = INT(IQUOT/2,KIND=8)
!MatL4         EXPO  = EXPO + 1
!MatL4
!MatL4      END DO
!MatL4
!MatL4   ELSE IF ( FRAC.GT.0. ) THEN
!MatL4
!MatL4!        --- calculate negative exponent as power of two
!MatL4
!MatL4      EXPO = 0
!MatL4      RFRAC = FRAC
!MatL4      DO WHILE (RFRAC.LT.1.)
!MatL4
!MatL4         RFRAC = RFRAC * 2.
!MatL4         EXPO  = EXPO - 1
!MatL4
!MatL4      END DO
!MatL4
!MatL4   ELSE
!MatL4
!MatL4      EXIT conversion_loop
!MatL4
!MatL4   END IF
!MatL4
!MatL4!     --- compute the actual part of the representation
!MatL4
!MatL4   IF ( BEXPO.EQ.-1 ) THEN
!MatL4
!MatL4!        --- determine biased exponent
!MatL4
!MatL4      BEXPO = EXPO + BIAS
!MatL4
!MatL4!        --- the first seven bits of biased exponent belong
!MatL4!            to first part of the representation
!MatL4
!MatL4      BVAL(1) = INT(BEXPO/2)
!MatL4
!MatL4!        --- determine the sign bit
!MatL4
!MatL4      IF ( RVAL.LT.0. ) BVAL(1) = BVAL(1) + 128
!MatL4
!MatL4!        --- the eighth bit of biased component is the leftmost
!MatL4!            bit of second part of the representation
!MatL4
!MatL4      BVAL(2) = MOD(BEXPO,2)*2**7
!MatL4      IPART = 2
!MatL4
!MatL4   ELSE
!MatL4
!MatL4!        --- compute the actual exponent of bit 1 in i-th part of
!MatL4!            the representation
!MatL4
!MatL4      ACTEXP = (IPART-2)*8 + 7 - BEXPO + BIAS + EXPO
!MatL4      IF ( ACTEXP.LT.0 ) THEN
!MatL4         ACTEXP = ACTEXP + 8
!MatL4         IPART = IPART + 1
!MatL4         IF ( IPART.GT.4 ) EXIT conversion_loop
!MatL4      END IF
!MatL4      BVAL(IPART) = BVAL(IPART) + 2**ACTEXP
!MatL4
!MatL4   END IF
!MatL4
!MatL4   IF ( EXPO.LT.(BEXPO-BIAS-LFRAC) ) EXIT conversion_loop
!MatL4   LEADNR = LEADNR - 2.**EXPO
!MatL4   IF ( EXPO.LT.0 ) FRAC = FRAC - 2.**EXPO
!MatL4
!MatL4   END DO conversion_loop
!MatL4
!MatL4   RETURN
!MatL4end subroutine SWR2B
!****************************************************************

SUBROUTINE MKPATH ( PATH, IERR )

!****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


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
