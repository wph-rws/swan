MODULE swan_triads
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: TCOEF

CONTAINS

SUBROUTINE TCOEF ( W1, W2, W12, K1, K2, K12, DEP, R, S )
   USE swan_service_interfaces, ONLY: STRACE

!********************************************************************

   USE swan_diagnostics_level
   USE swan_physics_selection
   USE swan_physical_settings

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
!     41.46: James Salmon
!     42.11: Gal Akrish
!
!  1. Updates
!
!     41.46, October 2013: new subroutine
!     42.11,   April 2024: interaction coeff QuadWave included
!
!  2. Purpose
!
!     Calculates transfer coefficients
!
!  3. Method
!
!     deterministic Boussinesq model of Madsen and Sorensen (1993)
!     (Eqs. 5.4a and 5.4f):
!
!     R_m,p-m = (k_m + k_p-m)^2 * [0.5 + (w_m*w_p-m / g*h*k_m*k_p-m)]
!
!     S_p     = -2/g * ( g*h*k_p + 2*B*g*h^3*k_p^3 - (B+(1/3))*h^2*w_p^2*k_p )
!
!     where: B = 1/15
!
!     See also Becq-Girard et al (1999), Eqs. 2.5 and 2.6
!     Further details can be found in Akrish et al (2024), Eqs. (13)-(15)
!     and Appendix A
!
!  4. Argument variables
!
!     DEP         water depth
!     R           numerator of transfer function
!     S           denominator of transfer function
!
!     W1, W2, W12 w_p, w_m, w_I      where I represents the sum or difference, i.e.
!     K1, K2, K12 k_p, k_m, k_I      I=p-m and I=p+m, respectively

   REAL, INTENT(IN) :: W1, W2, W12
   REAL, INTENT(IN) :: K1, K2, K12

   REAL, INTENT(IN) :: DEP

   REAL, INTENT(OUT) :: R
   REAL, INTENT(OUT) :: S

!  5. Parameter variables
!
!     A1          first optimization parameter for QuadWave1D
!     A2          second optimization parameter for QuadWave1D
!     A3          third optimization parameter for QuadWave1D
!
!     Note: these optimization parameters minimize the error in nonlinearity
!           while maintaining the dispersion properties of the Bredmose
!           model

   REAL, PARAMETER :: A1 = 1.
   REAL, PARAMETER :: A2 = 0.4  ! note: original value of 1.4 yields
   !       energy in high-frequency part
   REAL, PARAMETER :: A3 = 5.5

!  6. Local variables
!
!     ITRF  : indicates type of transfer function for triad interaction
!             =1; classic Boussinesq: Freilich and Guza (1984), Herbers
!             =2; deterministic Boussinesq of Madsen and Sorensen (1993)
!             =3; exact second order transfer coefficient of Bredmose et al (2005)
!             =4; QuadWave of Akrish et al (2024)

   INTEGER, SAVE :: IENT = 0
   INTEGER :: ITRF
   REAL    :: DEP_2, DEP_3
   REAL    :: B, B2, B3
   REAL    :: PROD, KLM, WLM2, FAC1, FAC2

!  7. SUBROUTINES USED
!
!  8. SUBROUTINES CALLING
!
!     ---
!
!  9. ERROR MESSAGES
!
!     ---
!
! 10. REMARKS
!
!     ---
!
! 11. STRUCTURE
!
!     ---
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'TCOEF')

   R = 0.
   S = 1.
   IF (.NOT.W1.NE.0. .OR. .NOT.K1.NE.0.) RETURN

   ITRF = INT(PTRIAD(10))

   DEP_2 = DEP**2
   DEP_3 = DEP**3

   B     = 1./15.
   B2    = 2.*B
   B3    = B + 1./3.

   IF ( ITRF.EQ.1 ) THEN !classic Boussinesq
!          R = 0.75 * (W2 + W12) should be W1! See Herbers and Burton, Eq. 11
      R = 0.75 * W1
      S = -DEP * SQRT(GRAV*DEP)

   ELSEIF ( ITRF.EQ.2 ) THEN !deterministic Boussinesq
!         to avoid NaN when W2*W12=0=K2*K12, adapt product of K2 and K12
      PROD = SIGN(MAX(1.E-20, ABS(K2*K12)),K2*K12)
      R     =  (0.5 + ((W2*W12)/(GRAV*DEP*PROD))) * (K2 + K12)**2

      S     = (-2./GRAV) * (  (GRAV*DEP*K1)&
      &+ (B2*GRAV*DEP_3*K1**3)&
      &- (B3*DEP_2*K1*W1**2)   )

   ELSEIF ( ITRF.EQ.3 .OR. ITRF.EQ.4 ) THEN !Bredmose or QuadWave1D
      KLM  = K2 + K12
      WLM2 = GRAV * KLM * TANH(KLM*DEP)

      FAC1 = ABS(KLM) * DEP * ( ABS(KLM)/ABS(K1) )**A1
      FAC2 = EXP( -(FAC1/A3)**A2 )
      IF (ITRF.EQ.3) FAC2 = 1.

!         to avoid NaN when W2*W12=0, adapt product of W2 and W12
      PROD = SIGN(MAX(1.E-20, ABS(W2*W12)),W2*W12)
      R = -0.5 * FAC2 * GRAV / PROD *&
      &( WLM2 * K2 * K12 + W1 * KLM * (K2*W12 + K12*W2) )&
      &-0.5 * FAC2 * WLM2 / GRAV * ( W2 * W12 - W1*W1 )
      S = ( W1*W1 - WLM2 ) / ( K1 - KLM )

   ENDIF

   RETURN
end subroutine TCOEF

END MODULE swan_triads
