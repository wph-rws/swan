MODULE swan_field_file_update
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: FLFILE

CONTAINS

SUBROUTINE FLFILE (IGR1, IGR2,&
&ARR, ARR2, JX1, JX2, JX3, JY1, JY2, JY3,&
&COSFC, SINFC, COMPDA,&
&XCGRID, YCGRID,&
&KGRPNT, IERR)
   USE swan_legacy_io, ONLY: INAR2D
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_interpolation, ONLY: SVALQI

!**********************************************************************

   USE swan_time, ONLY: default_time_context
   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE M_PARALL
   USE SwanGriddata

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
!     30.90: IJsbrand Haagsma (Equivalence version)
!     40.00: Nico Booij
!     34.01: Jeroen Adema
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.20: Casey Dietrich
!
!  1. Updates
!
!     40.00, Jan. 98: new subroutine replacing code in subr SNEXTI
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Aug. 00: condition added for calling INAR2D to prevent error
!                     in case command INP GRID is present and corresponding
!                     command READ is not.
!     40.02, Oct. 00: Avoided real/int conflict by replacing RPOOL for POOL in
!                     INAR2D
!     40.13, Mar. 01: misplaced error message moved to proper place
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!
!  2. PURPOSE
!
!     Update boundary conditions, update nonstationary input fields
!
!  3. METHOD
!
!
!  4. Argument list
!
!     ARR      real  i  array holding values read from file (x-comp)
!     ARR2     real  i  array holding values read from file (y-comp)
!     INTRV    real  i  time interval between input fields
!     TMENDR   real  i  end time of input field
!     IGR1     int   i  location in array COMPDA for interpolated input
!     IGR2     int   i  location in array COMPDA for interpolated input
!                       for a scalar field IGR2=0
!     JX1      int   i  location in array COMPDA for interpolated input
!     JX2      int   i  location in array COMPDA for interpolated input
!     JX3      int   i  location in array COMPDA for interpolated input
!     JY1      int   i  location in array COMPDA for interpolated input
!     JY2      int   i  location in array COMPDA for interpolated input
!     JY3      int   i  location in array COMPDA for interpolated input
!     COSFC    real  i  cos of angle between input grid and computational grid
!     SINFC    real  i  sin of angle between input grid and computational grid
!     COMPDA   real i/o array holding values for computational grid points
!     XCGRID   real  i  x-coordinate of computational grid points
!     YCGRID   real  i  y-coordinate of computational grid points
!     KGRPNT   int   i  indirect addresses of computational grid points
!     NHDF     int   i  number of heading lines for a data file
!     NHDT     int   i  number of heading lines per time step
!     NHDC     int   i  number of heading lines before second component
!     IDLA     int   i  lay-out identifier for a data file
!     IDFM     int   i  format identifier for a data file
!     DFORM    char  i  format to read a data file
!     VFAC     real  i  multiplication factor applied to values from data file
!     IERR     int   o  error status: 0=no error, 9=end-of-file
!
!
!  5. SUBROUTINES CALLING
!
!     SNEXTI
!
!  6. SUBROUTINES USED
!
!     INAR2D
!     MSGERR
!     STRACE
!     SWBROADC


!  7. ERROR MESSAGES
!
!        ---
!
!  8. REMARKS
!
!
!  9. Structure
!
!     --------------------------------------------------------------
!     for all comp. grid points do
!         copy new values to old
!     --------------------------------------------------------------
!     repeat
!         if present time > time of last reading
!         then read new values from file
!              update time of last reading
!              interpolate values to computational grid
!         else exit from repeat
!     --------------------------------------------------------------
!     for all comp. grid points do
!         interpolate new values
!     --------------------------------------------------------------
!
! 10. SOURCE
!
!****************************************************************

   INTEGER    KGRPNT(MXC,MYC),&
   &IGR1, IGR2, JX1, JX2, JX3, JY1, JY2, JY3, IERR

   REAL       COMPDA(MCGRD,MCMVAR),&
   &XCGRID(MXC,MYC), YCGRID(MXC,MYC),&
   &COSFC, SINFC
   REAL       ARR(*), ARR2(*)

!     local variables

   INTEGER, SAVE :: IENT = 0
   INTEGER    INDX, IX, IY, JVERT
!     INDX       counter of comp. grid points
!     IX         index in x-dir of comput grid point
!     IY         index in y-dir of comput grid point
!     JVERT      global index of unstructured mesh

!     SVALQI     real function giving interpolated value of an input array

   REAL       XP, YP, UU, VV, VTOT, W1, W3,&
   &SIZE1, SIZE2, SIZE3
   REAL(KIND=KIND(0.0D0))     FAC
   REAL(KIND=KIND(0.0D0))     TIMR1
!     TIMR1      time of one but last input field
!     XP         x-coord of one comput grid point
!     YP         y-coord of one comput grid point
!     UU         x-component of vector, or scalar value
!     VV         y-component of vector
!     VTOT       length of vector
!     W1         weighting coeff for interpolation in time
!     W3         weighting coeff for interpolation in time
!     DIRE       direction of interpolated vector
!     SIZE1      length of vector at time TIMR1
!     SIZE2      length of vector at time TIMCO
!     SIZE3      length of vector at time TIMR2

   CALL STRACE (IENT, 'FLFILE')

   IERR = 0

   IF (JX1.GT.1) THEN
      DO INDX = 1, MCGRD
         COMPDA(INDX,JX1)=COMPDA(INDX,JX2)
      ENDDO
   ENDIF
   IF (IGR2.GT.0 .AND. JY1.GT.1) THEN
      DO INDX = 1, MCGRD
         COMPDA(INDX,JY1)=COMPDA(INDX,JY2)
      ENDDO
   ENDIF
   TIMR1 = default_time_context%TIMCO - default_time_context%DT

   field_updates: DO WHILE (default_time_context%TIMCO > IFLTIM(IGR1))
   TIMR1 = IFLTIM(IGR1)
   IFLTIM(IGR1) = IFLTIM(IGR1) + IFLINT(IGR1)
   IF (IFLTIM(IGR1) .GT. IFLEND(IGR1)) THEN
      IFLTIM(IGR1) = 1.E10
      IF (IGR2.GT.0) IFLTIM(IGR2) = IFLTIM(IGR1)
      EXIT field_updates
   ENDIF
   IF (IFLNDS(IGR1).GT.0) THEN
      IF (INODE.EQ.MASTER) THEN
         CALL INAR2D( ARR, MXG(IGR1), MYG(IGR1),&
         &IFLNDF(IGR1),&
         &IFLNDS(IGR1), IFLIFM(IGR1), IFLFRM(IGR1),&
         &IFLIDL(IGR1), IFLFAC(IGR1),&
         &IFLNHD(IGR1), IFLNHF(IGR1))
         IF (STPNOW()) RETURN
      END IF
      CALL SWBROADC(IFLIDL(IGR1),1)
      IF (IFLIDL(IGR1).LT.0) THEN
!         end of file was encountered
         IFLTIM(IGR1) = 1.E10
         IF (IGR2.GT.0) IFLTIM(IGR2) = IFLTIM(IGR1)
         EXIT field_updates
      ELSE
         CALL SWBROADC(ARR,MXG(IGR1)*MYG(IGR1))
      ENDIF
   ELSE
      IF (ITEST.GE.20) THEN
         CALL MSGERR (1,&
         &'no read of input field because unit nr=0')
         WRITE (PRINTF, "(' field nr.', I2)") IGR1
      ENDIF
   ENDIF
   IF (IGR2.GT.0) THEN
      IFLTIM(IGR2) = IFLTIM(IGR1)
      IF (IFLNDS(IGR2).GT.0) THEN
         IF (INODE.EQ.MASTER) THEN
            CALL INAR2D( ARR2, MXG(IGR2), MYG(IGR2),&
            &IFLNDF(IGR2),&
            &IFLNDS(IGR2), IFLIFM(IGR2), IFLFRM(IGR2),&
            &IFLIDL(IGR2), IFLFAC(IGR2), IFLNHD(IGR2), 0)
            IF (STPNOW()) RETURN
         END IF
         CALL SWBROADC(ARR2,MXG(IGR2)*MYG(IGR2))
      ENDIF
   ENDIF
!     Interpolation over the computational grid
!     structured grid
   do IX = 1, MXC
      do IY = 1, MYC
         INDX = KGRPNT(IX,IY)
         IF (INDX.GT.1) THEN
            XP = XCGRID(IX,IY)
            YP = YCGRID(IX,IY)
            UU = SVALQI (XP, YP, IGR1, ARR, 0, IX, IY)
            IF (IGR2.EQ.0) THEN
               COMPDA(INDX,JX3) = UU
            ELSE
               VV = SVALQI (XP, YP, IGR2, ARR2, 0, IX, IY)
               COMPDA(INDX,JX3) =  UU*COSFC + VV*SINFC
               COMPDA(INDX,JY3) = -UU*SINFC + VV*COSFC
            ENDIF
         ENDIF
      end do
   end do
!     unstructured grid
   DO INDX = 1, nverts
      XP = xcugrd(INDX)
      YP = ycugrd(INDX)
      IF (.NOT.PARLL) THEN
         JVERT=INDX
      ELSE
         JVERT=ivertg(INDX)
      ENDIF
      IF ( IGTYPE(IGR1).EQ.3 ) THEN
         UU = ARR(JVERT)
      ELSE
         UU = SVALQI (XP, YP, IGR1, ARR, 0, 0, 0)
      ENDIF
      IF (IGR2.EQ.0) THEN
         COMPDA(INDX,JX3) = UU
      ELSE
         IF ( IGTYPE(IGR2).EQ.3 ) THEN
            VV = ARR2(JVERT)
         ELSE
            VV = SVALQI (XP, YP, IGR2, ARR2, 0, 0, 0)
         ENDIF
         COMPDA(INDX,JX3) =  UU*COSFC + VV*SINFC
         COMPDA(INDX,JY3) = -UU*SINFC + VV*COSFC
      ENDIF
   ENDDO
   END DO field_updates

!         Interpolation in time

   FAC = (default_time_context%TIMCO-TIMR1) / (IFLTIM(IGR1)-TIMR1)
   W3 = REAL(FAC)
   W1 = 1.-W3
   IF (ITEST.GE.60) WRITE(PRTEST,"(' input field', I2, ' interp at ', 2F9.0, 2F8.3, 6I3)") IGR1,&
   &default_time_context%TIMCO,IFLTIM(IGR1),W1,W3,JX1,JY1,JX2,JY2,JX3,JY3
   do INDX = 1, MCGRD
      UU = W1 * COMPDA(INDX,JX2) + W3 * COMPDA(INDX,JX3)
      IF (IGR2.LE.0) THEN
         COMPDA(INDX,JX2) = UU
      ELSE
         VV = W1 * COMPDA(INDX,JY2) + W3 * COMPDA(INDX,JY3)
         VTOT = SQRT (UU*UU + VV*VV)

!         procedure to prevent loss of magnitude due to interpolation

         IF (VTOT.GT.0.) THEN
            SIZE1 = SQRT(COMPDA(INDX,JX2)**2 + COMPDA(INDX,JY2)**2)
            SIZE3 = SQRT(COMPDA(INDX,JX3)**2 + COMPDA(INDX,JY3)**2)
            SIZE2 = W1*SIZE1 + W3*SIZE3
!           SIZE2 is to be length of vector
            COMPDA(INDX,JX2) = SIZE2*UU/VTOT
            COMPDA(INDX,JY2) = SIZE2*VV/VTOT
         ELSE
            COMPDA(INDX,JX2) = UU
            COMPDA(INDX,JY2) = VV
         ENDIF
      ENDIF
   end do
   RETURN

!     End of subroutine FLFILE
end subroutine FLFILE

END MODULE swan_field_file_update
