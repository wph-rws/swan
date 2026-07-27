MODULE swan_spectrum_output
   use swan_output_variables, only: OVEXCV
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: WRSPEC, SWCMSP

CONTAINS

SUBROUTINE WRSPEC (NREF, ACLOC)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE OUTP_DATA

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

!************************************************************************
!                                                                      *
      SUBROUTINE SWCMSP (OTYPE     ,XC        ,YC        ,&
      &AC2       ,ACLOC     ,SPCSIG    ,&
      &DEP       ,DEP2      ,UX        ,&
      &UY        ,ECOS      ,ESIN      ,&
      &OFAC      ,KGRPNT    ,CROSS     ,IERR         )
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_structured_output_interpolation, ONLY: SWOINA
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
         USE swan_service_interfaces, ONLY: EQREAL, STRACE
         USE swan_wave_physics, ONLY: KSCIP1
         USE swan_action_interpolation, ONLY: SwanInterpolateAc
!                                                                      *
!************************************************************************

         USE OCPCOMM4
         USE SWCOMM1
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
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     40.00: Nico Booij
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.90: Nico Booij
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!
!  1. Updates
!
!     20.xx         : New subroutine
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.81, Nov. 98: Adjustment for 1-D case of new boundary conditions
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     40.00, Jan. 98: number of output points is always 1
!                     subr produces 3 parameters in case 1D
!                     in 2D cases, loops over ID and ISIGM swapped
!                     interpolation changed: if a corner of the mesh is
!                     exception values are written
!                     argument DEP2 added
!     30.82, Apr. 99: Conversion from m^2/rad/s to m^2/Hz correctly implemented
!     30.82, July 99: Corrected argumentlist KSCIP1
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!     40.90, June 08: interpolation near obstacles improved
!     41.90, Dec. 21: allow negative 1D spectrum for QCM
!
!  2. Purpose
!
!     Computation of energy density spectrum 1-D or 2-D
!
!  3. Method
!
!     Energy is assumed to be distributed evenly over the interval
!     from Sigma/Frinth to Sigma*Frinth
!     This energy is tranferred to the Omega axis; it is determined
!     how much energy is to be assigned to each interval
!
!     Bilinear interpolation within a mesh of the computational grid
!     to obtain action density in an output location.
!     To transform from relative to absolute frequency, an interval in
!     sigma-space is partitioned, the energy in a submesh is determined
!     and transferred to omega-space where it is added to the energy for
!     a grid step; to obtain energy density this value is divided
!     by the length of the interval.
!     To obtain frequency spectra (1-D) energy density is integrated
!     over theta (spectral direction)
!
!  4. Argument variables
!
!     SPCSIG: input  Relative frequencies in computational domain in
!                    sigma-space

         REAL    SPCSIG(MSC)

!       OTYPE   int    input    type of spectrum wanted: 2 or -2 for 2-D
!                               spectrum, 1 or -1 for 1-D freq. spectrum
!                               positive: relative freq, negative: abs.
!       MPP     int    input    number of output points in set PSNAME
!       XC, YC  real   input    coordinates of output location(s)
!       ACLOC   real   local    |OTYPE|=2: 2-D spectrum at one output location
!                               |OTYPE|=1: 1-D spectra at output locations
!       DEP     real   input    depths at output location
!       UX, UY  real   input    current velocities at output location
!       ECOS  real   input    cosines of spectral directions
!       ESIN  real   input    sines of spectral directions
!       OFAC    real   input    output factor (if INRHOG=1, equal to Rho*Grav)

         LOGICAL, INTENT(IN) :: CROSS(1:4) ! true if obstacle is between
         ! output point and computational grid point
!
!  5. SUBROUTINES CALLING
!
!       SWSPEC (SWAN/OUTP)
!
!  6. SUBROUTINES USED
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
! 10. SOURCE TEXT


         INTEGER, SAVE :: IENT = 0
         INTEGER   IERR, ID, II, IOM, ISIGM, JJ, OTYPE
         INTEGER   KGRPNT(MXC,MYC)
         REAL      XC, YC, UX, UY, DEP, DEP2(MCGRD), AC2(MDC,MSC,MCGRD),&
         &ACLOC(*)        ,ECOS(MDC)         ,ESIN(MDC)
         REAL      CG(1), K1(1), K2(1), N(1), ND(1), SIG1(1), SIG2(1)
         REAL      ACLL, DOMEG, DSIG, EADD, ECLL, EE, EX, EY, FF, OFAC
         REAL      OMEG1, OMEG2, OMEGA, OMEGB, RLOW, RR, RUPP, UDIR
         LOGICAL EXCPT
         REAL, ALLOCATABLE :: ACL(:,:)

         IF (LTRACE) CALL STRACE(IENT,'SWCMSP')

!     make initial value of energy density 0

         IF (ABS(OTYPE).EQ.1) THEN
!       1-D spectra
            DO II = 1, 3*MSC
               ACLOC(II) = 0.
            ENDDO
         ELSE
!       2-D spectra
            DO II = 1, MDC*MSC
               ACLOC(II) = 0.
            ENDDO
         ENDIF

!     ***** determine energy densities *****

         IERR = 1
         IF (DEP.LE.0.) RETURN

!     the action density spectrum is interpolated

         ALLOCATE(ACL(MDC,MSC))
         IF (OPTG.EQ.5) THEN
            CALL SwanInterpolateAc ( ACL, XC, YC, AC2, EXCPT )
         ELSE
            IF (KREPTX.EQ.0) THEN
!         non-repeating grid
               IF (XC .LT. -0.01)            RETURN
               IF (XC .GT. REAL(MXC-1)+0.01) RETURN
            ENDIF
            IF (YC .LT. -0.01)            RETURN
            IF (YC .GT. REAL(MYC-1)+0.01) RETURN
            CALL SWOINA (XC, YC, AC2, ACL, KGRPNT, DEP2, CROSS(1), EXCPT)
         ENDIF
         IF (EXCPT) THEN
            DEALLOCATE(ACL)
            RETURN
         END IF

         do ID = 1, MDC
            IF (ICUR.GT.0 .AND. OTYPE.LT.0) THEN
               UDIR = UX * ECOS(ID) + UY * ESIN(ID)
            ENDIF
            do ISIGM = 1, MSC

               ACLL = ACL(ID,ISIGM)

!         energy density interpolated in space:
               ECLL = OFAC * ACLL * SPCSIG(ISIGM)

               IF (ICUR.EQ.0 .OR. OTYPE.GT.0&
               &) THEN

!           spectrum as function of relative frequency (SPCSIG)

                  IF (ABS(OTYPE).EQ.2) THEN
                     ACLOC(ID+(ISIGM-1)*MDC) = ECLL
                  ELSE
!             1-D spectrum of rel. frequency
                     ECLL = ECLL * DDIR
                     ACLOC(ISIGM) = ACLOC(ISIGM) + ECLL
                     ACLOC(ISIGM+  MSC) = ACLOC(ISIGM+  MSC) + ECLL * ECOS(ID)
                     ACLOC(ISIGM+2*MSC) = ACLOC(ISIGM+2*MSC) + ECLL * ESIN(ID)
                     IF (ITEST.GE.250 .OR. IOUTES .GE. 40) WRITE (PRTEST, "(' Test SWCMSP ', I6, 4(1X,E12.4))")&
                     &ISIGM, ECLL, (ACLOC(ISIGM+JJ*MSC),JJ=0,2)
                  ENDIF
               ELSE

!           spectrum as function of absolute frequency (OMEGA)
!           WK is wavenumber
!           energy density is assumed constant over the interval from
!           SIG1 to SIG2

                  SIG1(1)  = SPCSIG(ISIGM) / FRINTH
                  CALL KSCIP1 (1, SIG1, DEP, K1, CG, N, ND)
                  OMEG1 = SIG1(1) + K1(1) * UDIR
                  SIG2(1)  = SPCSIG(ISIGM) * FRINTH
                  CALL KSCIP1 (1, SIG2, DEP, K2, CG, N, ND)
                  OMEG2 = SIG2(1) + K2(1) * UDIR
                  DSIG  = FRINTF * SPCSIG(ISIGM)

!           EE is energy density in Omega:

                  IF ( .NOT.EQREAL(OMEG1,OMEG2) ) THEN
                     EE = ECLL * DSIG / ABS(OMEG2-OMEG1)
                  ELSE
                     EE = ECLL * DSIG
                  ENDIF
                  IF (ITEST.GE.250 .OR. IOUTES .GE. 40) WRITE (PRTEST, "(' Test SWCMSP/86 ', 2I6, 8(1X,E12.4))")&
                  &ID, ISIGM, SIG1(1), K1(1), OMEG1, SIG2(1), K2(1), OMEG2

!           assign the energy to omega interval

                  IF (OMEG1.GT.OMEG2) THEN
!             swap the two values
                     RR    = OMEG2
                     OMEG2 = OMEG1
                     OMEG1 = RR
                  ENDIF
                  do IOM = 1, MSC
                     OMEGA = SPCSIG(IOM) / FRINTH
                     OMEGB = SPCSIG(IOM) * FRINTH
                     IF (OMEG1.LT.OMEGB) THEN
                        RLOW = MAX (OMEG1,OMEGA)
                     ELSE
                        CYCLE
                     ENDIF
                     IF (OMEG2.GT.OMEGA) THEN
                        RUPP = MIN (OMEG2,OMEGB)
                     ELSE
                        CYCLE
                     ENDIF
                     IF (RUPP.LT.RLOW) THEN
                        WRITE (PRINTF, "(' error SWCMSP:', 2I4, 8(1X,E12.4))") ISIGM, IOM, OMEG1, OMEG2,&
                        &OMEGA, OMEGB, RUPP, RLOW
                     ELSE
                        IF ( .NOT.EQREAL(OMEG1,OMEG2) ) THEN
                           DOMEG = RUPP - RLOW
                        ELSE
                           DOMEG = 1.
                        ENDIF
                        IF (OTYPE.EQ.-2) THEN
                           ACLOC(ID+(IOM-1)*MDC) =&
                           &ACLOC(ID+(IOM-1)*MDC) + EE * DOMEG
                        ELSE
                           EADD = EE * DDIR * DOMEG
                           ACLOC(IOM) = ACLOC(IOM) + EADD
                           ACLOC(IOM+  MSC) = ACLOC(IOM+  MSC) + EADD * ECOS(ID)
                           ACLOC(IOM+2*MSC) = ACLOC(IOM+2*MSC) + EADD * ESIN(ID)
                        ENDIF
                     ENDIF
                  end do
               ENDIF
            end do
            IF (OTYPE.EQ.-2 .AND. ICUR.GT.0) THEN
               do IOM = 1, MSC
                  DOMEG = FRINTF * SPCSIG(IOM)
                  ACLOC(ID+(IOM-1)*MDC) =&
                  &ACLOC(ID+(IOM-1)*MDC) / DOMEG
                  IF (ITEST.GE.250 .OR. IOUTES .GE. 40) WRITE (PRTEST, "(' Test SWCMSP ', 2I6, 4(1X,E12.4))")&
                  &ID, IOM, ACLOC(ID+(IOM-1)*MDC), DOMEG
               end do
            ENDIF
         end do
         IF (ABS(OTYPE).EQ.1) THEN
!       1-D spectrum
            IF (ICUR.GT.0 .AND. OTYPE.EQ.-1) THEN
               do IOM = 1, MSC
                  DOMEG = FRINTF * SPCSIG(IOM)
                  do JJ = 0, 2
                     ACLOC(IOM+JJ*MSC) = ACLOC(IOM+JJ*MSC) / DOMEG
                  end do
               end do
            ENDIF
            do IOM = 1, MSC
               IF (ACLOC(IOM).GT.1.E-12.OR.IQCM.NE.0) THEN
                  EX = ACLOC(IOM+MSC) / ACLOC(IOM)
                  EY = ACLOC(IOM+2*MSC) / ACLOC(IOM)
                  ACLOC(IOM+MSC) = DEGCNV (ATAN2(EY,EX) * 180./PI)
                  FF = MIN (1.,SQRT(EX**2+EY**2))
                  ACLOC(IOM+2*MSC) = SQRT(2.-2.*FF)*180./PI

!           To convert ACLOC from m^2/rad/s to m^2/Hz (1D spectrum)

                  ACLOC(IOM) = ACLOC(IOM) * PI2

               ELSE
!           exception values for VaDens, DIR and DSPR
                  ACLOC(IOM)       = OVEXCV(22)
                  ACLOC(IOM+MSC)   = OVEXCV(13)
                  ACLOC(IOM+2*MSC) = OVEXCV(16)
               ENDIF
            end do
         ENDIF

         IERR = 0
         DEALLOCATE(ACL)
         RETURN
! * end of subroutine SWCMSP *
      end subroutine SWCMSP

END MODULE swan_spectrum_output
