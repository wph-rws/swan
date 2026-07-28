
!     SWAN/OUTPUT       file 2 of 2
!
!  Contents of this file:
!     SWBLOK
!     SBLKPT
!     SWBLKP
!     SWBLKV
!     SRAWPT
!     SWTABP
!     SUHEAD
!     SWSPEC
!     SWCMSP
!     SWRMAT
!
!************************************************************************

module swan_output_writers
   use swan_service_interfaces, only: MSGERR, TXPBLA, SWI2B, SWR2B   ! SWI2B/SWR2B feed the !MatL4 SWRMAT variants
   use swan_vtk_write_data, only: SwanVTKWriteData
   use swan_vtk_write_header, only: SwanVTKWriteHeader
   use swan_vtkp_data_sets, only: SwanVTKPDataSets
   use swan_io_limits, only: LENFNM
   use swan_output_variables, only: OVEXCV, OVHEXP, OVLNAM, OVSNAM, OVSVTY, OVUNIT
   use swan_output_quadrature, only: ALCQ, COSCQ, SINCQ
   use swan_project_metadata, only: PROJID, PROJNR, VERTXT
   use swan_path_separators, only: DIRCH2
   use swan_time, only: CHTIME
   use swan_output_settings, only: INRHOG
   implicit none(type, external)
   private
!  SWTABP and SWRMAT are defined behind switch lines (!NCF/!NNCF, !MatL4/!MatL5)
!  further down; the end of this module therefore lies at the end of the file.
   public :: SWBLOK, SBLKPT, SWBLKP, SWBLKV, SRAWPT, SWSPEC
   public :: SWTABP, SWRMAT
contains

!                                                                      *
SUBROUTINE SWBLOK ( RTYPE, OQI , OQR , IVTYP, FAC, PSNAME,&
&MXK  , MYK , IRQ , VOQR , VOQ        )
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: STRACE, TXPBLA, STPNOW, TABHED
   USE swan_wave_physics, ONLY: KSCIP1
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_numerics, ONLY: NSTATM
   USE swan_spherical_geometry, ONLY: KSPHER
   USE OUTP_DATA
!NCF   USE swn_outnc
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine
!
!
!
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
!     34.01: Jeroen Adema
!     40.03: Nico Booij
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.62: Andre van der Westhuysen
!
!  1. UPDATE
!
!     30.81, Jan. 99: Replaced variable FROM by FROM_ (because FROM is
!                     a reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Nov. 99: NVAR in write statement replaced by OREQ(18)
!     40.13, Oct. 01: longer output filenames now obtained from array
!                     OUTP_FILES (in module OUTP_DATA)
!     40.30, May  03: extension to write block output to Matlab files
!     40.31, Jul. 03: small correction w.r.t. length of OVSNAM in
!                     call SWRMAT
!     40.31, Dec. 03: removing POOL construction
!     40.41, Jun. 04: some improvements with respect to MATLAB
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.62, Nov. 15: included wave partitioning output (raw partition file)
!
!  2. PURPOSE
!
!       Preparing output in the form of a block that is printed by
!       subroutine SBLKPT
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       RTYPE   ch*4   input    type of output request:
!                               'BLKP' for output on paper,
!                               'BLKD' and 'BLKV' for output to datafile
!       PSNAME  ch*8   input    name of output frame
!       MXK     int    input    number of grid points in x-direction
!       MYK     int    input    number of grid points in y-direction
!       VOQR
!       VOQ
!
!  5. SUBROUTINES CALLING
!
!       SWOUTP (SWAN/OUTP)
!
!  6. SUBROUTINES USED
!
!       SBLKPT, SCUNIT, SFLFUN (all SWAN/OUTP), TABHED,
!       MSGERR, COPYCH, FOR
!       SWRMAT, TXPBLA


!  7. ERROR MESSAGES
!
!       If the point set is not of the type frame, an error message
!       is printed and control returns to subroutine OUTPUT
!
!  8. REMARKS
!
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       If output is on paper then
!           Call TABHED to print heading
!       Else
!           Call FOR to open file
!       ----------------------------------------------------------------
!       For each required variable do
!           Determine type of variable and factor of multiplication
!           Call SBLKPT to write block output to printer or datafile
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   CHARACTER (LEN=8) :: PSNAME       ! name of output locations
   CHARACTER (LEN=4) :: RTYPE        ! output type
   INTEGER   MXK, MYK, IRQ
   INTEGER   VOQR(*), IPD
   INTEGER   OQI(4), IVTYP(OQI(3))
   REAL(KIND=KIND(0.0D0))    OQR(2)
   REAL      VOQ(MXK*MYK,*), FAC(OQI(3))
   INTEGER DFAC_INT, IDLA, IF, IFAC, IL, IOSTAT, IP, IVTYPE
   INTEGER JVAR, NREF, NVAR
   REAL DFAC, FMAX, FTIP, FTIP1, FTIP2
   INTEGER, SAVE :: IREC(MAX_OUTP_REQ)=0
   LOGICAL, SAVE :: MATLAB=.FALSE.
!NCF   LOGICAL, SAVE :: NCF   =.FALSE.
!NCF   LOGICAL       :: EXIST = .FALSE.
   LOGICAL, SAVE :: RAWPRT=.FALSE.
   CHARACTER (LEN=20) :: CTIM
   CHARACTER (LEN=30) :: NAMVAR
   CHARACTER (LEN=80) :: HTXT(3)

   INTEGER, SAVE :: IENT=0
   IF (LTRACE) CALL STRACE (IENT,'SWBLOK')

!     **** obtain destination and number of variables from array OUTR ***
   NREF = OQI(1)
   IF (RTYPE .EQ. 'BLKP') THEN
!       printer type output with header
      IPD   = 1
      IF (NREF.EQ.PRINTF) CALL TABHED ('SWAN', PRINTF)
   ELSE IF (RTYPE .EQ. 'BLKD') THEN
!       output to datafile without header
      IPD = 2
   ELSE
      IPD = 3
   ENDIF
   IF (ITEST.GE.90) WRITE (PRTEST, "(' Test SWBLOK: RTYPE NREF NVAR ',A4,2(1X,I6))")  RTYPE,NREF, OQI(3)
   FILENM = OUTP_FILES(OQI(2))
   MATLAB = INDEX( FILENM, '.MAT' ).NE.0 .OR.&
   &INDEX (FILENM, '.mat' ).NE.0
!NCF   NCF    = INDEX( FILENM, '.NC'  ).NE.0 .OR.&
!NCF   &INDEX (FILENM, '.nc'  ).NE.0
   RAWPRT = INDEX( FILENM, '.RAW' ).NE.0 .OR.&
   &INDEX (FILENM, '.raw' ).NE.0
!NNCF   IF (NREF.EQ.0) THEN
!NCF      IF (.NOT.NCF .AND. NREF.EQ.0) THEN
         IOSTAT = -1
         CALL FOR (NREF, FILENM, 'UF', IOSTAT)
         IF (STPNOW()) RETURN
         OQI(1) = NREF
         OUTP_FILES(OQI(2)) = FILENM
         IF (MATLAB) THEN
            CLOSE(NREF)
            OPEN(UNIT=NREF, FILE=FILENM, FORM='UNFORMATTED',&
            &STATUS='REPLACE',&
!MatL4            &ACCESS='DIRECT', RECL=1)
!MatL5            &ACCESS='DIRECT', RECL=4)
            IREC(IRQ) = 1
         END IF
         IF (RAWPRT.AND.IPD.EQ.1) THEN
            IF (NSTATM.EQ.1) THEN
               WRITE (HTXT(1),'(a)') ' yyyymmdd hhmmss'
            ELSE
               WRITE (HTXT(1),'(a)') ''
            ENDIF
            IF (KSPHER.EQ.0) THEN
               WRITE (HTXT(2),'(a)') '         x             y'
            ELSE
               WRITE (HTXT(2),'(a)') '       lat           lon'
            ENDIF
            WRITE (HTXT(3),'(a)')&
            &'       name       nprt depth uabs  udir cabs  cdir'

            WRITE(NREF,'(A26)') 'SWAN PARTITIONED DATA FILE'
            WRITE(NREF,'(A16,A24,A50)')  TRIM(HTXT(1)), TRIM(HTXT(2)),&
            &TRIM(HTXT(3))
            WRITE(NREF,'(A31,A20)') '        hs     tp     lp       ',&
            &'theta     sp      wf'
         END IF
!NCF      ELSE IF (NCF .AND. NCOFFSET(IRQ).EQ.0) THEN
!NCF         ! reserve free unit number
!NCF         IOSTAT = -1
!NCF         INQUIRE(FILE=FILENM, EXIST=EXIST)
!NCF         CALL FOR (NREF, TRIM(FILENM)//'.dum', 'UF', IOSTAT)
!NCF         IF (STPNOW()) RETURN
!NCF         IF (.NOT.EXIST) CLOSE(NREF, STATUS='DELETE')
!NCF         OQI(1) = NREF
!NCF         CALL swn_outnc_openblockfile(FILENM, MYK, MXK,&
!NCF         &OVLNAM, VOQ(:,VOQR(1)),&
!NCF         &VOQ(:,VOQR(2)),&
!NCF         &OQI, OQR, IVTYP, IRQ)
      ENDIF
      IDLA = OQI(4)
      NVAR = OQI(3)

      IF (ITEST.GE.90) WRITE (PRTEST, "(' Test SWBLOK: NREF FILENM ', I6, A40)")  NREF, FILENM

      CTIM = CHTIME
      CALL TXPBLA(CTIM,IF,IL)
      CTIM(9:9)='_'

      IF (RAWPRT) THEN
!        generate a dump of the raw partition data
         CALL SRAWPT ( NREF, VOQR, VOQ, MXK, MYK )
      ELSE
      DO JVAR = 1, NVAR
         IVTYPE = IVTYP(JVAR)
         DFAC   = FAC(JVAR)

         IF (IPD.EQ.1) THEN
            IF (DFAC.LE.0.) THEN
!           determine default factor for print output
               IF (OVHEXP(IVTYPE) .LT. 0.5E10) THEN
                  IFAC = INT (10.+LOG10(OVHEXP(IVTYPE))) - 13
               ELSE
                  IF (OVSVTY(IVTYPE).EQ.1) THEN
                     FMAX = 1.E-8
                     do IP = 1, MXK*MYK
                        FTIP = ABS(VOQ(IP,VOQR(IVTYPE)))
                        FMAX = MAX (FMAX, FTIP)
                     end do
                  ELSE IF (OVSVTY(IVTYPE).EQ.2) THEN
                     FMAX = 1000.
                  ELSE IF (OVSVTY(IVTYPE).EQ.3) THEN
                     FMAX = 1.E-8
                     do IP = 1, MXK*MYK
                        FTIP1 = ABS(VOQ(IP,VOQR(IVTYPE)))
                        FTIP2 = ABS(VOQ(IP,VOQR(IVTYPE)+1))
                        FMAX  = MAX (FMAX, FTIP1, FTIP2)
                     end do
                  ENDIF
                  IFAC = INT (10.+LOG10(FMAX)) - 13
               ENDIF
               DFAC = 10.**IFAC
            ENDIF
         ELSE
            IF (DFAC.LE.0.) DFAC = 1.
         ENDIF

         IF (ITEST .GE. 80) WRITE(PRTEST, "(' Test SWBLOK: jvar, ivtype, dfac, coscq, sincq', 2I10,3E12.5)") JVAR, IVTYPE, DFAC,&
         &COSCQ, SINCQ

         IF (OVSVTY(IVTYPE) .LT. 3) THEN
!                      scalar quantities
            IF (MATLAB) THEN
               IF (IL.EQ.1 .OR. IVTYPE.LT.3 .OR. IVTYPE.EQ.52) THEN
                  NAMVAR = OVSNAM(IVTYPE)
               ELSE
                  NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
                  &'_'//CTIM
               END IF
               CALL SWRMAT( MYK, MXK, NAMVAR,&
               &VOQ(1,VOQR(IVTYPE)), NREF, IREC(IRQ),&
               &IDLA, OVEXCV(IVTYPE) )
!NCF            ELSE IF (NCF) THEN
!NCF               IF (IVTYPE.GT.2.AND.IVTYPE.NE.40) THEN
!NCF                  CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
!NCF                  &IRQ, VOQ(1,VOQR(IVTYPE)),&
!NCF                  &OVEXCV(IVTYPE), 1)
!NCF               END IF
            ELSE
               CALL SBLKPT(IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
               &MXK, MYK, IDLA, OVLNAM(IVTYPE), VOQ(1,VOQR(IVTYPE)))
            END IF
         ELSE
!                     vectorial quantities
            IF (MATLAB) THEN
               IF (IL.EQ.1) THEN
                  NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
                  &'_x'
               ELSE
                  NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
                  &'_x_'//CTIM
               END IF
               CALL SWRMAT( MYK, MXK, NAMVAR,&
               &VOQ(1,VOQR(IVTYPE)), NREF, IREC(IRQ),&
               &IDLA, OVEXCV(IVTYPE))
               IF (IL.EQ.1) THEN
                  NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
                  &'_y'
               ELSE
                  NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
                  &'_y_'//CTIM
               END IF
               CALL SWRMAT( MYK, MXK, NAMVAR,&
               &VOQ(1,VOQR(IVTYPE)+1), NREF, IREC(IRQ),&
               &IDLA, OVEXCV(IVTYPE))
!NCF            ELSE IF (NCF) THEN
!NCF               IF ( IVTYPE.GT.3 ) THEN
!NCF                  CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
!NCF                  &IRQ, VOQ(1,VOQR(IVTYPE)),&
!NCF                  &OVEXCV(IVTYPE), 1)
!NCF                  CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
!NCF                  &IRQ, VOQ(1,VOQR(IVTYPE)+1),&
!NCF                  &OVEXCV(IVTYPE), 2)
!NCF               END IF
            ELSE
               CALL SBLKPT(IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
               &MXK, MYK, IDLA, OVLNAM(IVTYPE)//'X-comp',&
               &VOQ(1,VOQR(IVTYPE)))
               CALL SBLKPT(IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
               &MXK, MYK, IDLA, OVLNAM(IVTYPE)//'Y-comp',&
               &VOQ(1,VOQR(IVTYPE)+1))
            END IF
         ENDIF

      END DO
      END IF
!NCF      IF ( NCF ) CALL swn_outnc_close_on_end(OQI(1), IRQ)
      IF (IPD.EQ.1 .AND. NREF.EQ.PRINTF) WRITE (PRINTF, "(///)")

      RETURN
! * end of subroutine SWBLOK *
   end subroutine SWBLOK
!************************************************************************
!                                                                      *
   SUBROUTINE SBLKPT (IPD, NREF, DFAC, PSNAME, QUNIT,&
   &MXK, MYK, IDLA, STRING, OQVALS)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

      USE swan_diagnostics_level
      USE swan_io_units
      USE swan_numerics
      USE OUTP_DATA
      USE swan_time, ONLY: default_time_context


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
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     00.00, Mar. 87: subroutine heading added, some variable names
!                     line numbers changed, layout modified
!     00.04, Feb. 90: lay-out of output changed according to IDLA=1
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.82, Nov. 98: Corrected syntax format statement
!     40.13, July 01: variable formats introduced, using module OUTP_DATA
!     40.13, Oct. 01: longer output filenames now obtained from array
!                     OUTP_FILES (in module OUTP_DATA)
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Writing the block output either on paper or to datafile
!
!  3. Method
!
!     ---
!
!  4. PARAMETERLIST
!
!     IPD     INT    input    switch for printing on paper (IPD=1)
!                             or writing to datafile (IPD = 2 or 3)
!     NREF    INT    input    unit reference number of output file
!     DFAC    REAL   input    multiplication factor of block output
!     IVTYPE  INT    input    type of the output quantity
!                             Note: IVTYPE=0 for Y-component of a
!                             vectorial quantity
!     PSNAME  CH*8   input    name of output point set (frame)
!     QUNIT   CH*6   input    physical unit (dimension) of variable
!     MXK     int    input    number of points in x-direction of frame
!     MYK     int    input    number of points in y-direction of frame
!     IDLA    INT    input    controls lay-out of output (see user manual)
!     STRING  CH*(*) input    description of output variable
!
!  8. Subroutines used
!
!       ---
!
!  9. Subroutines calling
!
!       SWBLOK (SWAN/OUTP)
!
! 10. Error messages
!
!       ---
!
! 11. Remarks
!
!       ---
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       If IPD = 1 (output on paper) then
!           If DFAC < 0 (DFAC not given by the user) then
!               Compute maximum value of output variable
!               Compute multiplication factor DFAC
!           --------------------------------------------------------------
!           Print block heading
!           For each IX of the output frame do
!               Print IX and for every IY the value of the outputvariable
!           --------------------------------------------------------------
!       Else
!           If DFAC < 0 then DFAC = 1.
!           Write output variable line by line to datafile
!       ----------------------------------------------------------------
!
! 13. Source text


      CHARACTER (LEN=20) :: WFORM1 = '(A1, 2X, 151(I6))'
      CHARACTER (LEN=21) :: WFORM2 = '(1X,I4,1X, 151(F6.0))'
      CHARACTER (LEN=20) :: WFORM3 = '(5X, 151(F6.0))'

      CHARACTER(LEN=8) :: PSNAME
      CHARACTER(LEN=*) :: STRING, QUNIT
      REAL      DFAC, OQVALS(*)
      INTEGER, SAVE :: IENT = 0
      INTEGER   NREF, MXK, MYK, IPD, IDLA
      INTEGER   II, IP, ISP, IXK, IXP1, IXP2, IYK
      REAL      RPDFAC
      LOGICAL   BPRN
      IF (LTRACE) CALL STRACE (IENT,'SBLKPT')

      IF (ITEST.GE.150) WRITE (PRTEST, "(' SBLKPT', 4(I6))") NREF,IPD,MXK,MYK


!     divide all output values by the given factor (DFAC)

      IF (ABS(DFAC-1.) .GT. 0.001) THEN
         RPDFAC=1./DFAC
         do IP = 1, MXK*MYK
            OQVALS(IP) = OQVALS(IP)*RPDFAC
         end do
      ENDIF


!      IFF = VOQR(IVTYPE)

      IF (IPD.EQ.1) THEN

!       ***** output on paper *****

         WRITE (NREF, "(A)") OUT_COMMENT
         WRITE (NREF, "(A)") OUT_COMMENT
         WRITE (NREF, "(A,' Run:', A4, ' Frame: ',A8,' ** ',A,', Unit:', E12.4, 1X, A)") OUT_COMMENT, PROJNR, PSNAME, STRING,&
         &DFAC, QUNIT
         IF (NSTATM .GT. 0) THEN
            WRITE (NREF, "(A,' Time:', A)") OUT_COMMENT, CHTIME
         ELSE
            WRITE (NREF, "(A)") OUT_COMMENT
         ENDIF
         WRITE (NREF, "(A)") OUT_COMMENT

         ISP = 151
         do IXP1 = 1, MXK, ISP
            IXP2 = IXP1+ISP-1
            IF (IXP2.GT.MXK) IXP2=MXK

            WRITE (WFORM1(15:15), '(I1)') DEC_BLOCK
            WRITE (WFORM2(17:17), '(I1)') DEC_BLOCK
            WRITE (WFORM3(11:11), '(I1)') DEC_BLOCK
            IF (ITEST.GE.80) WRITE (PRTEST, "(' SBLKPT Formats: ', A, /, 17X, A, /, 17X, A)") WFORM1, WFORM2, WFORM3

            WRITE (NREF, "(A1,' X --->')") OUT_COMMENT
            WRITE (NREF, "(A)") OUT_COMMENT
            WRITE (NREF, WFORM1) OUT_COMMENT, (II-1,II=IXP1,IXP2)
            WRITE (NREF, "(A1, 'Y')") OUT_COMMENT

            BPRN = .TRUE.
            do IYK = MYK, 1, -1
               IP = (IYK-1)*MXK
               IF (BPRN) THEN
                  WRITE (NREF, WFORM2) IYK-1,&
                  &(OQVALS(IP+IXK), IXK=IXP1,IXP2)
               ELSE
                  WRITE (NREF, WFORM3)&
                  &(OQVALS(IP+IXK), IXK=IXP1,IXP2)
               ENDIF
!!!            BPRN = .NOT. BPRN
            end do
         end do
      ELSE

!       ***** output to datafile *****

         ISP=6
         IF (IDLA.EQ.4) THEN
            WRITE (NREF, FLT_BLOCK) (OQVALS(IP), IP=1, MXK*MYK)
         ELSE
            do IYK = 1, MYK
               IF (IDLA.EQ.3) THEN
                  IP = (IYK-1)*MXK
               ELSE
                  IP = (MYK-IYK)*MXK
               ENDIF
               WRITE (NREF, FLT_BLOCK) (OQVALS(IP+IXK), IXK=1,MXK)
            end do
         ENDIF
      ENDIF

      RETURN
! * end of subroutine SBLKPT *
   end subroutine SBLKPT
!****************************************************************

   SUBROUTINE SWBLKP ( OQI   , IVTYP, MXK  , MYK, VOQR, VOQ,&
   &IONOD )
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: STRACE, STPNOW

!****************************************************************

      USE swan_diagnostics_level
      USE OUTP_DATA
      USE M_PARALL

      IMPLICIT NONE(TYPE, EXTERNAL)
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine


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
!     40.51: Agnieszka Herman
!
!  1. Updates
!
!     40.31, Dec. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: further optimization
!
!  2. Purpose
!
!     Write block data to process output file
!
!  3. Method
!
!     Write data without header using the format FLT_BLKP
!
!  4. Argument variables
!
!     IONOD       array indicating in which subdomain output points
!                 are located
!     IVTYP       type of variable output
!     MXK         number of points in x-direction of output frame
!     MYK         number of points in y-direction of output frame
!     OQI         array containing output request data
!     VOQ         output variables
!     VOQR        array containing information for output

      INTEGER MXK, MYK, OQI(4), IVTYP(OQI(3)), VOQR(*)
      INTEGER IONOD(*)
      REAL    VOQ(MXK*MYK,*)

!  6. Local variables
!
!     IENT  :     number of entries
!     IOSTAT:     status of input/output
!     IP    :     pointer
!     IPROC :     processor number
!     IVTYPE:     type number output variable
!     IXK   :     loop counter
!     IYK   :     loop counter
!     JVAR  :     loop counter
!     NREF  :     unit reference number
!     NVAR  :     number of variables

      INTEGER, SAVE :: IENT = 0
      INTEGER IOSTAT, IP, IPROC,IVTYPE, IXK, IYK, JVAR, NREF, NVAR

!  8. Subroutines used
!
!     FOR              General open file routine
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging


!  9. Subroutines calling
!
!     SWOUTP
!
! 13. Source text

      IF (LTRACE) CALL STRACE (IENT,'SWBLKP')

      NREF = OQI(1)
      IF (NREF.EQ.0) THEN
         FILENM = OUTP_FILES(OQI(2))
         IOSTAT = -1
         CALL FOR (NREF, FILENM, 'UU', IOSTAT)
         IF (STPNOW()) RETURN
         OQI(1) = NREF
         OUTP_FILES(OQI(2)) = FILENM
      END IF
      NVAR = OQI(3)

      IPROC = INODE

      DO JVAR = 1, NVAR
         IVTYPE = IVTYP(JVAR)
         DO IYK = 1, MYK
            IP = (IYK-1)*MXK
            DO IXK = 1, MXK
               IF ( IONOD(IP+IXK).EQ.IPROC )&
               &WRITE (NREF) VOQ(IP+IXK,VOQR(IVTYPE))
            END DO
         END DO
         IF ( OVSVTY(IVTYPE).GE.3 ) THEN
            DO IYK = 1, MYK
               IP = (IYK-1)*MXK
               DO IXK = 1, MXK
                  IF ( IONOD(IP+IXK).EQ.IPROC )&
                  &WRITE (NREF) VOQ(IP+IXK,VOQR(IVTYPE)+1)
               END DO
            END DO
         END IF
      END DO

      RETURN
   end subroutine SWBLKP

!****************************************************************

   SUBROUTINE SWBLKV ( OQI, OQR, IVTYP, MXK, MYK, VOQR, VOQ,&
   &PSTYPE, PSNAME, IONOD )
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: STRACE, TXPBLA, STPNOW

!****************************************************************

      USE swan_diagnostics_level
      USE swan_number_formatting
      USE swan_computational_grid_kind, ONLY: OPTG
      USE M_PARALL
      USE OUTP_DATA

      IMPLICIT NONE(TYPE, EXTERNAL)
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine


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
!     Writes block output to VTK files
!
!  4. Argument variables

      INTEGER                       :: MXK    ! number of grid points in
      INTEGER                       :: MYK    ! number of grid points in
      INTEGER, DIMENSION(4)         :: OQI    ! integer coefficients (e.
      ! output variables)
      INTEGER, DIMENSION(*)         :: IONOD  ! array indicating in whic
      ! output points are locate
      INTEGER, DIMENSION(*)         :: IVTYP  ! types of output variable
      INTEGER, DIMENSION(*)         :: VOQR   ! pointer in list of varia

      REAL(KIND=KIND(0.0D0)) , DIMENSION(2)         :: OQR    ! real coefficients (e.g.
      ! time step)
      REAL   , DIMENSION(MXK*MYK,*) :: VOQ    ! output variables

      CHARACTER (LEN=8)             :: PSNAME ! name of output frame
      CHARACTER (LEN=1)             :: PSTYPE ! type of output point set

!  6. Local variables

      INTEGER IF, IL, ILPOS, ILPOS2, IP, IVAL(2), LEN, NVAR
      INTEGER UPVD, UPVT, UVTK
      INTEGER IXK, IYK, MXKF, MXKL, MYKF, MYKL, MXE, MYE
      INTEGER, SAVE :: IENT = 0
      INTEGER IOSTAT
      INTEGER IPRC, IARRL(4), IARRC(4,0:NPROC-1)
         LOGICAL :: LC
      CHARACTER (LEN=4) :: PNUM
      CHARACTER (LEN=20) :: CTIM
      CHARACTER (LEN=1024) :: PVDLINE
      CHARACTER (LEN=LENFNM) :: CDIR, VDIR, PVDFNM, PVTFIL, VTKFIL

! 13. Source text

      IF (LTRACE) CALL STRACE (IENT,'SWBLKV')

!     broadcast the necessary data, if desired
      UPVD=UPVDF(OQI(2))
      VDIR=TRIM(VTKDIR(OQI(2)))
      IF (IAMMASTER) THEN
         IVAL(1)=UPVD
         CDIR=VDIR
         IVAL(2)=LEN_TRIM(CDIR)
      ENDIF
      CALL SWBROADC (IVAL,2)
      CALL SWBROADC (CDIR(1:IVAL(2)),IVAL(2))
      IF (STPNOW()) RETURN

      IPRC = INODE

!     check file extension and add time counter
      FILENM=OUTP_FILES(OQI(2))
      LC=.FALSE.
      ILPOS=INDEX( FILENM, '.VT' )
      IF (ILPOS.EQ.0) THEN
         LC=.TRUE.
         ILPOS=INDEX( FILENM, '.vt' )
      ENDIF
      ILPOS=ILPOS-1
      IF (IVAL(1).GT.0) THEN
         WRITE(CTIM(1:20),'(I20)') NTVTK(OQI(2))
         CALL TXPBLA(CTIM,IF,IL)
         WRITE(FILENM(ILPOS+1:ILPOS+IL-IF+2),'(A)') '_'//CTIM(IF:IL)
         ILPOS=ILPOS+IL-IF+2
      ENDIF
      IF (PARLL) THEN
         ILPOS2 = ILPOS
         PVTFIL = FILENM
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('_',I3.3)") IPRC
         ILPOS=ILPOS+4
      ENDIF
      IF (PSTYPE.EQ.'F' .OR. PSTYPE.EQ.'H') THEN
!        output grid is structured
         IF (LC) THEN
            WRITE(FILENM(ILPOS+1:ILPOS+4),"(A4)") '.vts'
            IF (PARLL) WRITE(PVTFIL(ILPOS2+1:ILPOS2+5),"(A5)") '.pvts'
         ELSE
            WRITE(FILENM(ILPOS+1:ILPOS+4),"(A4)") '.VTS'
            IF (PARLL) WRITE(PVTFIL(ILPOS2+1:ILPOS2+5),"(A5)") '.PVTS'
         ENDIF
      ELSEIF (PSTYPE.EQ.'U') THEN
!        output grid is unstructured
         IF (LC) THEN
            WRITE(FILENM(ILPOS+1:ILPOS+4),"(A4)") '.vtu'
            IF (PARLL) WRITE(PVTFIL(ILPOS2+1:ILPOS2+5),"(A5)") '.pvtu'
         ELSE
            WRITE(FILENM(ILPOS+1:ILPOS+4),"(A4)") '.VTU'
            IF (PARLL) WRITE(PVTFIL(ILPOS2+1:ILPOS2+5),"(A5)") '.PVTU'
         ENDIF
      ENDIF
      VTKFIL = FILENM

!     update collection file
      IF (IVAL(1).GT.0) THEN
         IF (.NOT.PARLL) THEN
            VTKFIL = VDIR(1:IVAL(2))//DIRCH2//FILENM
            PVDFNM = VTKFIL
         ELSE
            IF (LC) THEN
               WRITE(PNUM(1:4),"(A1,I3.3)") 'p',IPRC
            ELSE
               WRITE(PNUM(1:4),"(A1,I3.3)") 'P',IPRC
            ENDIF
            VTKFIL = CDIR(1:IVAL(2))//DIRCH2//PNUM//DIRCH2//FILENM
            PVTFIL = CDIR(1:IVAL(2))//DIRCH2//PVTFIL
            PVDFNM = PVTFIL
         ENDIF
!        write timestep and VTK filename to collection file
         IF (IAMMASTER) THEN
            CTIM=NUMSTR(INAN,REAL(OQR(1)-OQR(2)),'(F15.5)')
            CALL TXPBLA(CTIM,IF,IL)
            PVDLINE = '    <DataSet timestep="'//CTIM(IF:IL)//&
            &'" part="0" file="'//PVDFNM(1:LEN_TRIM(PVDFNM))//&
            &'"/>'
            WRITE(UPVD,'(A)') TRIM(PVDLINE)
         ENDIF
      ENDIF

!     determine size of piece of output data
      IF (.NOT.PARLL) THEN
         MXKF = 1
         MXKL = MXK
         MYKF = 1
         MYKL = MYK
         IARRC= 0
      ELSE
         MXKF = MXK + 1
         MXKL = 0
         MYKF = MYK + 1
         MYKL = 0
         DO IYK = 1, MYK
            IP = (IYK-1)*MXK
            DO IXK = 1, MXK
               IF ( IONOD(IP+IXK).EQ.INODE ) THEN
                  MXKF = MIN(IXK,MXKF)
                  MXKL = MAX(IXK,MXKL)
                  MYKF = MIN(IYK,MYKF)
                  MYKL = MAX(IYK,MYKL)
               ENDIF
            ENDDO
         ENDDO
         IF (MXKF.GE.MXKL-1 .OR. (OPTG.NE.5 .AND. MYKF.GE.MYKL-1)) THEN
            MXKF = 1
            MXKL = 0
            MYKF = 1
            MYKL = 0
         ENDIF
         IARRL(1) = MXKF
         IARRL(2) = MXKL
         IARRL(3) = MYKF
         IARRL(4) = MYKL
         CALL SWGATHER (IARRC, 4*NPROC, IARRL, 4 )
         IF (STPNOW()) RETURN
      ENDIF
      LEN=(MXKL - MXKF + 1)*(MYKL - MYKF + 1)

!     set number of output variables
      NVAR = OQI(3)

!     create a parallel VTK file
      IF (PARLL.AND.IAMMASTER) THEN
!        reserve free unit number for PVT file
         UPVT   =  0
         IOSTAT = -1
         CALL FOR (UPVT, PVTFIL, 'UF', IOSTAT)
         IF (STPNOW()) RETURN
         MXE = MAXVAL(IARRC(2,:))
         MYE = MAXVAL(IARRC(4,:))
!        write references to pieces of output data
         CALL SwanVTKPDataSets ( UPVT, PSTYPE, NVAR, IVTYP, MXE, MYE,&
         &FILENM, UPVD, PSNAME, IARRC )
      ENDIF

!     reserve free unit number for VTK file
      UVTK   =  0
      IOSTAT = -1
      CALL FOR (UVTK, VTKFIL, 'UF', IOSTAT)
      IF (STPNOW()) RETURN

!     open VTK file and write header
      CLOSE(UVTK)
!     note: stream access is a Fortran 2003 standard
      OPEN(UNIT=UVTK, FILE=VTKFIL, FORM='UNFORMATTED',&
      &STATUS='REPLACE', ACCESS='STREAM')
      CALL SwanVTKWriteHeader ( UVTK, PSTYPE, NVAR, IVTYP,&
      &MXKF, MXKL  , MYKF, MYKL )

!     write appended data to VTK file
      CALL SwanVTKWriteData ( UVTK, PSTYPE, NVAR, IVTYP, VOQR, VOQ,&
      &LEN , MXK   , MYK , IONOD)

      RETURN
   end subroutine SWBLKV
!****************************************************************

   SUBROUTINE SRAWPT ( NREF, VOQR, VOQ, MXK, MYK )
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

      USE swan_physical_settings, ONLY: BNAUT
      USE swan_numerics, ONLY: NSTATM
      USE swan_math_constants, ONLY: PI
      USE swan_math_constants
      USE swan_spherical_geometry, ONLY: KSPHER
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
!     41.62: Andre van der Westhuysen
!
!  1. Updates
!
!     41.62, Nov. 15: New subroutine
!
!  2. Purpose
!
!     Generates a dump of the raw partition data at all grid points
!
!  4. Argument variables
!
!     MXK     int    input    number of points in x-direction of frame
!     MYK     int    input    number of points in y-direction of frame
!     NREF    int    input    unit reference number of output file
!     VOQR
!     VOQ

      INTEGER NREF, MXK, MYK
      INTEGER VOQR(*)
      REAL    VOQ(MXK*MYK,*)

!  6. Local variables
!
!     CABS    :     magnitude of current
!     CDIR    :     direction of current
!     IP      :     pointer
!     IXK     :     counter in x-direction
!     IYK     :     counter in y-direction
!     NPT     :     actual number of partitions
!     UABS    :     magnitude of wind
!     UDIR    :     direction of wind

      INTEGER, SAVE :: IENT = 0
      INTEGER II, IP, IXK, IYK
      INTEGER NPT
      REAL    UABS, UDIR, CABS, CDIR
      REAL    HS, TP, DIR, XP, YP, DEP, DSPR, WL

      REAL    HSPT(10), TPPT(10), WLPT(10), DIRPT(10),&
      &DSPT(10), WFPT(10), STPT(10)


!  9. Subroutines calling
!
!     SWBLOK
!
! 13. Source text

      IF (LTRACE) CALL STRACE (IENT,'SRAWPT')

      DO IYK = MYK, 1, -1
         DO IXK = 1, MXK
            IP = (IYK-1)*MXK+IXK

            HS  = VOQ(IP,VOQR(10))
            TP  = VOQ(IP,VOQR(12))
            DIR = VOQ(IP,VOQR(13))

!           --- if not an exception value of wave height, peak period
!               and wave direction, write values to file

            IF ( (HS  .NE. OVEXCV(10)) .AND.&
            &(TP  .NE. OVEXCV(12)) .AND.&
            &(DIR .NE. OVEXCV(13))      ) THEN

               XP   = VOQ(IP,VOQR(1))
               YP   = VOQ(IP,VOQR(2))
               DEP  = VOQ(IP,VOQR(4))
               DSPR = VOQ(IP,VOQR(16))
               WL   = VOQ(IP,VOQR(17))

               NPT = INT(VOQ(IP,VOQR(171)))

!              --- compute magnitude and direction of wind and ambient current

               UABS = SQRT(VOQ(IP,VOQR(26))**2+VOQ(IP,VOQR(26)+1)**2)
               UDIR = ATAN2(VOQ(IP,VOQR(26)+1),VOQ(IP,VOQR(26)))*180./PI
               IF (.NOT.BNAUT) UDIR = UDIR + ALCQ * 180./PI
               IF (UDIR.LT.0.) UDIR = UDIR + 360.
               UDIR = DEGCNV(UDIR)

               CABS = SQRT(VOQ(IP,VOQR(5))**2+VOQ(IP,VOQR(5)+1)**2)
               CDIR = ATAN2(VOQ(IP,VOQR(5)+1),VOQ(IP,VOQR(5)))*180./PI
               IF (CDIR.GT.360.) CDIR = CDIR - 360.
               IF (CDIR.LT.0.  ) CDIR = CDIR + 360.

!              store partition parameters per grid point

               DO II = 0, 9
                  HSPT (II+1) = VOQ(IP,VOQR(100+II))
                  TPPT (II+1) = VOQ(IP,VOQR(110+II))
                  WLPT (II+1) = VOQ(IP,VOQR(120+II))
                  DIRPT(II+1) = VOQ(IP,VOQR(130+II))
                  DSPT (II+1) = VOQ(IP,VOQR(140+II))
                  WFPT (II+1) = VOQ(IP,VOQR(150+II))
!                  STPT (II+1) = VOQ(IP,VOQR(160+II))
               END DO

!              write the partition data

               IF (NSTATM.EQ.1) THEN
                  IF (KSPHER.EQ.0) THEN
                     WRITE(NREF,'(A9,1X,A6,2F14.4,A14,I3,F7.1,&
                     &                 F5.1,F6.1,F5.1,F6.1)')&
                     &CHTIME(1:8),CHTIME(10:16),YP,XP,'''grid_point''',&
                     &NPT, DEP, UABS, UDIR, CABS, CDIR
                  ELSE
                     WRITE(NREF,'(A9,1X,A6,2F12.6,A14,I3,F7.1,&
                     &                 F5.1,F6.1,F5.1,F6.1)')&
                     &CHTIME(1:8),CHTIME(10:16),YP,XP,'''grid_point''',&
                     &NPT, DEP, UABS, UDIR, CABS, CDIR
                  ENDIF
               ELSE
                  IF (KSPHER.EQ.0) THEN
                     WRITE(NREF,'(16X,2F14.4,A14,I3,F7.1,&
                     &                 F5.1,F6.1,F5.1,F6.1)')&
                     &YP,XP,'''grid_point''',&
                     &NPT, DEP, UABS, UDIR, CABS, CDIR
                  ELSE
                     WRITE(NREF,'(16X,2F12.6,A14,I3,F7.1,&
                     &                 F5.1,F6.1,F5.1,F6.1)')&
                     &YP,XP,'''grid_point''',&
                     &NPT, DEP, UABS, UDIR, CABS, CDIR
                  ENDIF
               ENDIF

               WRITE(NREF,'(I3,F8.2,F8.2,F8.2,F9.2,F9.2,F7.2)')&
               &0, HS, TP, WL, DIR, DSPR, 999.99

               DO II = 1, NPT
                  WRITE(NREF,'(I3,F8.2,F8.2,F8.2,F9.2,F9.2,F7.2)')&
                  &II       , HSPT(II), TPPT(II), WLPT(II),&
                  &DIRPT(II), DSPT(II), WFPT(II)
               END DO

            END IF

         END DO
      END DO

      RETURN
   end subroutine SRAWPT
!************************************************************************
!                                                                      *
!NCF   SUBROUTINE SWTABP (RTYPE , OQI  , OQR , IVTYP, PSNAME, MIP, VOQR,&
!NNCF      SUBROUTINE SWTABP (RTYPE , OQI  , IVTYP, PSNAME, MIP, VOQR,&
      &VOQ, IONOD)
!NCF         USE swan_service_interfaces, ONLY: STPNOW, STRACE
!NNCF         USE swan_service_interfaces, ONLY: STPNOW, STRACE
!NCF         USE swan_file_opening, ONLY: FOR
!NNCF         USE swan_file_opening, ONLY: FOR
!                                                                      *
!************************************************************************

         USE swan_diagnostics_level
         USE swan_io_units
         USE swan_time
         USE swan_numerics
         USE swan_spherical_geometry
         USE OUTP_DATA
         USE swan_time, ONLY: default_time_context
         USE M_PARALL
!NCF         USE swn_outnc, only: swn_outnc_openblockfile,&
!NCF         &swn_outnc_appendblock,&
!NCF         &swn_outnc_close_on_end
!
!
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
!     30.62: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.80: Nico Booij
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     32.01: Roeland Ris & Cor van der Schelde
!     34.01: Jeroen Adema
!     40.00: Nico Booij (Non-stationary boundary conditions)
!     40.03, 40.13: Nico Booij
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Agnieszka Herman
!
!  1. Updates
!
!     30.50, Sep. 96: option TABI (indexed file) added
!     30.62, Jul. 97: corrected initialisation of table output
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     32.01, Jan. 98: Extended initialisation of NUMDEC for SETUP
!     30.80, Apr. 98: number of decimals for setup from 2 to 3
!     40.00, June 98: severely revised
!     30.82, Oct. 98: Header information is now also printed in PRINT file
!     30.81, Jan. 99: Replaced variable FROM by FROM_ (because FROM is
!                     a reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Mar. 00: number of decimals (NUMDEC) is made larger
!     40.13, Jan. 01: program version now written into table heading
!            Mar. 01: XOFFS and YOFFS were incorrectly added to coordinates
!                     (they are already included in VOQ values)
!     40.13, July 01: variable formats introduced, using module OUTP_DATA
!                     comment sign in front of heading lines
!     40.13, Oct. 01: longer output filenames now obtained from array
!                     OUTP_FILES (in module OUTP_DATA)
!     40.31, Dec. 03: removing POOL construction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: further optimization
!
!  2. Purpose
!
!     Printing of output in the form of a table for any type of
!     output point set
!
!  3. Method
!
!     A table is made in which for each point the required output
!     variables are printed in the order given by the user. If more
!     variables are required than one line can contain, writing is
!     continued on the next line before output for the next point is
!     started.
!
!  4. Argument variables
!
!     PSNAME
! i   RTYPE : Type of output request
!             ='TABD'; Output to datafile (no header information)
!             ='TABI'; Indexed output for table in ArcView format
!             ='TABP'; Output to paper (with header information)
!             ='TABS';
!             ='TABT';
!NCF!             ='TABC'; NETCDF output

         CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine
         CHARACTER(LEN=4) :: RTYPE
         CHARACTER(LEN=8) :: PSNAME

!     MIP
!     VOQR

         INTEGER   MIP, VOQR(*), OQI(4), IVTYP(OQI(3))
         INTEGER   IONOD(*)

!     VOQ

         REAL      VOQ(MIP,*)
!NCF         REAL(KIND=KIND(0.0D0))    OQR(2)
!
!  5. Parameter variables
!
!     MXOUTL

         INTEGER, PARAMETER :: MXOUTL=720

!  6. Local variables
!
!     NUMDEC

         INTEGER NUMDEC
         INTEGER IOSTAT, IP, ISTR, IVTYPE, JVAR, LFIELD, LINKAR
         INTEGER LSTR, NKOLS, NREF, NVAR
!NCF         LOGICAL EXIST
!
!  8. Subroutines used
!
!     SUHEAD (all SWAN/OUTP)
!     FOR
!     TABHED (all Ocean Pack)


!  9. Subroutines calling
!
!     OUTPUT (SWAN/OUTP)
!
! 10. ERROR MESSAGES
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       If unit ref. number = 0
!       Then Read filename from array IOUTR
!            Call FOR to open datafile
!            If Rtype = 'TABP' or 'TABI'
!            Then Print heading for required table
!       ----------------------------------------------------------------
!       If Rtype = 'TABS'
!       Then write time into file
!       ----------------------------------------------------------------
!       Make Output line blank
!       Make Linkar = 1
!       If Rtype = 'TABI'
!       Then write index into output line
!            update Linkar
!       ----------------------------------------------------------------
!       For every output point do
!           For all output quantities do
!               Get value for array VOQ
!               If output quantity is TIME
!               Then make Format='(A18)'
!                    write time into output line
!                    make lfield = 18
!               Else if Rtype = 'TABD'
!                    Then Format = '(E12.4)'
!                         make lfield = 12
!                    Else Format = '(F13.X)'
!                         make lfield = 13
!                         determine number of decimals and write into
!                         Format
!                    ---------------------------------------------------
!                    Write value into output line according to Format
!               --------------------------------------------------------
!               Make Linkar = Linkar + lfield + 1
!           --------------------------------------------------------------
!           Write Output line to file
!       ----------------------------------------------------------------
!
! 13. Source text

         CHARACTER (LEN=15)     :: FSTR
         CHARACTER (LEN=MXOUTL) :: OUTLIN
         CHARACTER (LEN=8)      :: CRFORM = '(2F14.4)'

         INTEGER, SAVE :: IENT=0
         IF (LTRACE) CALL STRACE(IENT,'SWTABP')

         NREF = OQI(1)
         NVAR = OQI(3)

!     Header information is printed once for each data file and for
!     each entry in this routine when the table is written to the PRINT

         IF (NREF .EQ. 0 .OR. NREF.EQ.PRINTF) THEN
            IF (NREF.EQ.0) THEN
               FILENM = OUTP_FILES(OQI(2))
               IOSTAT = -1
!NCF               INQUIRE(FILE=FILENM, EXIST=EXIST)
!NCF               IF ( RTYPE.EQ.'TABC' ) THEN
!NCF                  CALL FOR (NREF, TRIM(FILENM)//'.dum', 'UF', IOSTAT)
!NCF               ELSE
                  CALL FOR (NREF, FILENM, 'UF', IOSTAT)
!NCF               ENDIF
               IF (STPNOW()) RETURN
               OQI(1) = NREF
               OUTP_FILES(OQI(2)) = FILENM
!NCF               IF ( RTYPE .EQ. 'TABC' ) THEN
!NCF                  IF (.NOT.EXIST) CLOSE(NREF, STATUS='DELETE')
!NCF                  CALL swn_outnc_openblockfile(FILENM, 1, MIP,&
!NCF                  &OVLNAM, VOQ(:,VOQR(1)),&
!NCF                  &VOQ(:,VOQR(2)),&
!NCF                  &OQI, OQR, IVTYP, OQI(2))
!NCF               ENDIF
            END IF
            IF (RTYPE .NE. 'TABD') THEN
               OUTLIN = '    '

!         write heading into file

               IF (RTYPE.EQ.'TABP' .OR. RTYPE.EQ.'TABI') THEN
                  WRITE (NREF, "(A)") OUT_COMMENT
                  WRITE (NREF, "(A)") OUT_COMMENT
                  WRITE (NREF, "(A1, ' Run:', A4,' Table:',A8, 10X, 'SWAN version:', A)") OUT_COMMENT, PROJNR, PSNAME, TRIM(VERTXT)
                  WRITE (NREF, "(A)") OUT_COMMENT
!           write (short) names of output quantities
                  IF (RTYPE.EQ.'TABI') THEN
                     OUTLIN(1:12) = OUT_COMMENT // '           '
                     LINKAR = 12
                  ELSE
                     OUTLIN(1:3) = OUT_COMMENT
                     LINKAR = 4
                  ENDIF
                  DO  JVAR = 1, NVAR
                     IVTYPE = IVTYP(JVAR)
                     IF (IVTYPE.EQ.40) THEN
                        LFIELD = 18
                     ELSE
                        LFIELD = 13
                     ENDIF
                     IF (OVSVTY(IVTYPE).LE.2) THEN
                        OUTLIN(LINKAR:LINKAR+LFIELD) =&
                        &'     '//OVSNAM(IVTYPE)//'              '
                     ELSE
                        OUTLIN(LINKAR:LINKAR+LFIELD) =&
                        &'   X-'//OVSNAM(IVTYPE)//'              '
                        LINKAR = LINKAR+LFIELD+1
                        OUTLIN(LINKAR:LINKAR+LFIELD) =&
                        &'   Y-'//OVSNAM(IVTYPE)//'              '
                     ENDIF
                     LINKAR = LINKAR+LFIELD+1
                  ENDDO
                  WRITE (NREF, '(A)') OUTLIN(1:LINKAR-1)
!           write units of output quantities
                  OUTLIN = '    '
                  IF (RTYPE.EQ.'TABI') THEN
                     OUTLIN(1:12) = OUT_COMMENT // '           '
                     LINKAR = 12
                  ELSE
                     OUTLIN(1:3) = OUT_COMMENT
                     LINKAR = 4
                  ENDIF
                  DO  JVAR = 1, NVAR
                     IVTYPE = IVTYP(JVAR)
                     IF (IVTYPE.EQ.40) THEN
                        LFIELD = 18
                     ELSE
                        LFIELD = 13
                     ENDIF
                     LSTR = MAX(1, LEN_TRIM(OVUNIT(IVTYPE)))
                     OUTLIN(LINKAR:LINKAR+LFIELD) =&
                     &'     ['//OVUNIT(IVTYPE)(1:LSTR)//']            '
                     IF (OVSVTY(IVTYPE).GT.2) THEN
                        LINKAR = LINKAR+LFIELD+1
                        OUTLIN(LINKAR:LINKAR+LFIELD) =&
                        &'     ['//OVUNIT(IVTYPE)(1:LSTR)//']            '
                     ENDIF
                     LINKAR = LINKAR+LFIELD+1
                  ENDDO
                  WRITE (NREF, '(A)') OUTLIN(1:LINKAR-1)
                  WRITE (NREF, "(A)") OUT_COMMENT
               ELSE IF (RTYPE.EQ.'TABT' .OR. RTYPE.EQ.'TABS') THEN
                  WRITE (NREF, "('SWAN', I4, T41, 'Swan standard file, version')") 1
                  WRITE (NREF, "(A1, ' Data produced by SWAN version ', A)") OUT_COMMENT, VERTXT
                  WRITE (NREF, "(A1, ' Project: ', A, '; run number: ', A)") OUT_COMMENT, PROJID, PROJNR
                  IF (RTYPE.EQ.'TABT') THEN
                     WRITE (NREF,"(A, T41, A)") 'TABLE'
                  ELSE
                     IF (NSTATM.EQ.1) THEN
                        WRITE (NREF, "(A, T41, A)") 'TIME', 'time-dependent data'
                        WRITE (NREF, "(I6, T41, A)") ITMOPT, 'time coding option'
                     ENDIF
                     IF (KSPHER.EQ.0) THEN
                        WRITE (NREF, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
                        CRFORM = '(2F14.4)'
                     ELSE
                        WRITE (NREF, "(A, T41, A)") 'LONLAT',&
                        &'locations in spherical coordinates'
                        CRFORM = '(2F12.6)'
                     ENDIF
                     WRITE (NREF, "(I6, T41, A)") MIP, 'number of locations'
                     do IP = 1, MIP
                        WRITE (NREF, FMT=CRFORM)&
                        &DBLE(VOQ(IP,VOQR(1))), DBLE(VOQ(IP,VOQR(2)))
                     end do
                  ENDIF
                  WRITE (NREF, "(A, T41, A)") 'QUANT', 'description of quantities'
                  NKOLS = NVAR
                  DO  JVAR = 1, NVAR
                     IVTYPE = IVTYP(JVAR)
                     IF (OVSVTY(IVTYPE).GT.2) NKOLS = NKOLS + 1
                  ENDDO
                  WRITE (NREF, "(I6, T41, A)") NKOLS, 'number of quantities in table'
                  DO  JVAR = 1, NVAR
                     IVTYPE = IVTYP(JVAR)
                     IF (OVSVTY(IVTYPE).LE.2) THEN
                        WRITE (NREF, "(A, T41, A)") OVSNAM(IVTYPE), OVLNAM(IVTYPE)
                        WRITE (NREF, "(A, T41, A)") OVUNIT(IVTYPE), 'unit'
                        WRITE (NREF, "(E14.4, T41, A)") OVEXCV(IVTYPE), 'exception value'
                     ELSE
                        WRITE (NREF, "(A, T41, A)") 'X-'//OVSNAM(IVTYPE), OVLNAM(IVTYPE)
                        WRITE (NREF, "(A, T41, A)") OVUNIT(IVTYPE), 'unit'
                        WRITE (NREF, "(E14.4, T41, A)") OVEXCV(IVTYPE), 'exception value'
                        WRITE (NREF, "(A, T41, A)") 'Y-'//OVSNAM(IVTYPE)
                        WRITE (NREF, "(A, T41, A)") OVUNIT(IVTYPE), 'unit'
                        WRITE (NREF, "(E14.4, T41, A)") OVEXCV(IVTYPE), 'exception value'
                     ENDIF
                  ENDDO
               ENDIF
            ENDIF
         ENDIF

!     ***** printing of the table *****
!
!NCF         IF (RTYPE.EQ.'TABC') THEN
!NCF            DO JVAR = 1, NVAR
!NCF               IVTYPE = IVTYP(JVAR)
!NCF               IF (IVTYPE.GT.3.AND.IVTYPE.NE.40) THEN
!NCF                  CALL swn_outnc_appendblock(1, MIP, IVTYPE, OQI(1),&
!NCF                  &OQI(2), VOQ(1,VOQR(IVTYPE)),&
!NCF                  &OVEXCV(IVTYPE), 1)
!NCF                  IF ( OVSVTY(IVTYPE).EQ.3 ) THEN
!NCF                     CALL swn_outnc_appendblock(1, MIP, IVTYPE, OQI(1),&
!NCF                     &OQI(2), VOQ(1,VOQR(IVTYPE)+1),&
!NCF                     &OVEXCV(IVTYPE), 2)
!NCF                  ENDIF
!NCF               ENDIF
!NCF            ENDDO
!NCF            CALL swn_outnc_close_on_end(OQI(1), OQI(2))
!NCF            RETURN
!NCF         ENDIF
!NCF!
         IF (RTYPE.EQ.'TABS') THEN
            IF (NSTATM.EQ.1) WRITE (NREF, "(A, T41, A)") CHTIME, 'date and time'
         ENDIF
         do IP = 1, MIP
            IF ( .NOT.PARLL .OR. IONOD(IP).EQ.INODE ) THEN
               LINKAR = 1
               OUTLIN = '    '
               IF (RTYPE.EQ.'TABI') THEN
!         write point sequence number as first column
                  WRITE (OUTLIN(1:8), '(I8)') IP
                  LINKAR = 9
                  OUTLIN(LINKAR:LINKAR) = ' '
               ENDIF
               do JVAR = 1, NVAR
                  IVTYPE = IVTYP(JVAR)
                  IF (IVTYPE.EQ.40) THEN
!           For time 18 characters are needed
                     FSTR = '(A18)'
                     LFIELD = 18
                     OUTLIN(LINKAR:LINKAR+LFIELD-1) = CHTIME
                  ELSE
                     IF (RTYPE.EQ.'TABD') THEN
                        FSTR = FLT_TABLE
                        LFIELD = FLD_TABLE
                     ELSE
                        FSTR = '(F13.X)'
                        LFIELD = 13
!             NUMDEC is number of decimals in the table for each output
                        NUMDEC = MAX (0, 6-NINT(LOG10(ABS(OVHEXP(IVTYPE)))))
                        IF (NUMDEC.GT.9) NUMDEC = 9
                        WRITE (FSTR(6:6), '(I1)') NUMDEC
                     ENDIF
!           write value into OUTLIN
                     WRITE (OUTLIN(LINKAR:LINKAR+LFIELD-1), FMT=FSTR)&
                     &VOQ(IP,VOQR(IVTYPE))
                     IF (OVSVTY(IVTYPE).EQ.3) THEN
                        LINKAR = LINKAR + LFIELD + 1
!             write second component of a vectorial quantity
                        WRITE (OUTLIN(LINKAR:LINKAR+LFIELD-1), FMT=FSTR)&
                        &VOQ(IP,VOQR(IVTYPE)+1)
                     ENDIF
                  ENDIF
                  LINKAR = LINKAR + LFIELD + 1
                  OUTLIN(LINKAR-1:LINKAR) = '  '
               end do
               WRITE (NREF, '(A)') OUTLIN(1:LINKAR-1)
            END IF
         end do

         RETURN
! * end of subroutine SWTABP *
      end subroutine SWTABP
!************************************************************************
!                                                                      *
      CHARACTER(LEN=8) FUNCTION SUHEAD (QUNIT)
         USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************
!
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
!  1. UPDATE
!
!        3 JAN 1990 : 0.0, first draft
!
!  2. PURPOSE
!
!       Preparation of unit for the table print output
!       in the form:   [unit]
!
!  3. METHOD
!
!       ---
!
!  4. PARAMETERLIST
!
!       QUNIT   CH*(*) input    unit of the variable to be printed
!                               in the table headings
!
!  5. SUBROUTINES CALLING
!
!       SWTABP (SWAN/OUTP)
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
!       ---
!
! 10. SOURCE TEXT

         CHARACTER(LEN=*) :: QUNIT
         CHARACTER(LEN=6) :: TEXT1
         CHARACTER(LEN=8) :: TEXT2
         INTEGER, SAVE :: IENT = 0
         INTEGER I, IEND, J
         CALL STRACE(IENT,'SUHEAD')

         TEXT2 = '        '
!      L = LEN (QUNIT)
!      IF (L.EQ.0 .OR. L.GT.6) THEN
!        IF (ITEST.GE.10) WRITE(PRINTF,9910) L
! 9910   FORMAT(' ** Error SUHEAD, length of unit in heading =', i2,
!     &        ' out of range')
!      ENDIF
         TEXT1 = QUNIT(1:6)
!     determine the position of the last non-blank character
         IEND = LEN_TRIM(TEXT1)
         IF (IEND.EQ.0) THEN
            TEXT1 = '-'
            IEND = 1
         ENDIF
!     shift the unit-string one position to the right
         do I=1,IEND
            J = I+1
            TEXT2(J:J) = TEXT1(I:I)
         end do
!     enclose the unit by brackets
         TEXT2(1:1) = '['
         TEXT2(IEND+2:IEND+2) = ']'
         SUHEAD = TEXT2

RETURN
!     end of subroutine SUHEAD
      end function SUHEAD
!************************************************************************
!                                                                      *
      SUBROUTINE SWSPEC (RTYPE, OQI, OQR, MIP, VOQR, VOQ, AC2, ACLOC,&
      &SPCSIG, SPCDIR, DEP2, KGRPNT, CROSS, IONOD)
         USE swan_spectrum_output, ONLY: WRSPEC
   USE swan_spectrum_output, ONLY: SWCMSP
         USE swan_service_interfaces, ONLY: EQREAL, STPNOW, STRACE
         USE swan_file_opening, ONLY: FOR
!                                                                      *
!************************************************************************

         USE swan_diagnostics_level
         USE swan_time
         USE swan_coordinate_offset
         USE swan_computational_grid_kind
         USE swan_physics_selection
         USE swan_numerics
         USE swan_physical_settings
         USE swan_computational_grid
         USE swan_spectral_grid
         USE swan_math_constants
         USE swan_spherical_geometry
         USE OUTP_DATA
         USE M_PARALL
!NCF         USE swn_outnc, only: swn_outnc_spec
         use SwanGriddata, only: ivertg
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine


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
!     32.01: Roeland Ris & Cor van der Schelde
!     34.01: Jeroen Adema
!     40.00, 40.03, 40.13: Nico Booij
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Agnieszka Herman
!     40.90: Nico Booij
!
!  1. Updates
!
!     20.28         : completely new version
!     20.43         : arguments ECOS and ESIN replaced by SPCDIR
!     32.01, Jan. 98: Introduced nautical convention (project h3268)
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Jan. 99: Replaced variable FROM by FROM_ (because FROM is
!                     a reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.00, Aug. 99: new file structure introduced
!     40.03, May  00: correct time coding option written to heading of file
!            Oct. 00: write 'LOCATION' in upper case
!     40.13, Mar. 01: format for writing coordinates different for Cartesian
!                     and spherical coordinates
!     40.13, Oct. 01: longer output filenames now obtained from array
!                     OUTP_FILES (in module OUTP_DATA)
!     40.31, Dec. 03: removing POOL construction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: further optimization
!     40.90, June 08: argument CROSS added to enable subroutine SWCMSP to
!                     take into account obstacles
!
!  2. Purpose
!
!     Printing of action density spectrum in the form of a table
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     IONOD : array indicating in which subdomain output points
!             are located
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

         INTEGER MIP
         REAL    SPCDIR(MDC,6)
         REAL    SPCSIG(MSC)
         LOGICAL, INTENT(IN) :: CROSS(1:4,1:MIP) ! true if obstacle is
         ! between output point and computational grid point

!     RTYPE   ch*4   input    type of output request: 'SPEC' for 2-D spectral
!                             output, 'SPE1' for 1-D freq. spectrum
!     MIP     int    input    number of output points in set PSNAME
!     ACLOC   real   local    case SPEC: 2-D spectrum at one output location
!                             case SPE1: 1-D spectra at output locations
!     AK      real   input    wavenumber array at output location
!     UX, UY  real   input    current velocities at output location
!
!  8. Subroutines used
!
!     DEGCNV: Transforms dir. from nautical to cartesian or vice versa
!     ANGDEG: Transforms degrees to radians
!     SWCMSP


!  9. Subroutines calling
!
!     SWOUTP (SWAN/OUTP)
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!       ---
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       get NREF from array OREQ  (output requests)
!       if NREF = 0
!       then open output file
!            write heading into the file
!       ----------------------------------------------------------------
!
! 13. Source text

         CHARACTER (LEN=*) :: RTYPE
         CHARACTER (LEN=8) :: CRFORM = '(2F14.4)'
         INTEGER       :: VOQR(*), OQI(4), OTYPE, KGRPNT(MXC,MYC)
         INTEGER       :: IONOD(*)
         INTEGER       :: ID, IERR, IFR, IOSTAT, IP, IS, JJ, NREF
         INTEGER, SAVE :: IVERF = 1
         REAL(KIND=KIND(0.0D0))    OQR(2)
         REAL      VOQ(MIP,*), AC2(MDC,MSC,MCGRD),&
         &ACLOC(*), DEP2(MCGRD)
         REAL      DEP, OFAC, UX, UY, XC, YC
!NCF         LOGICAL, SAVE :: NCF =.FALSE.

         INTEGER, SAVE :: IENT=0
         IF (LTRACE) CALL STRACE(IENT,'SWSPEC')

!NCF         FILENM = OUTP_FILES(OQI(2))
!NCF         NCF = INDEX( FILENM, '.NC' ).NE.0 .OR.&
!NCF         &INDEX (FILENM, '.nc' ).NE.0
!NCF!
!NCF         IF ( NCF ) THEN
!NCF            ! When PARALLEL, write intermediate binary files
!NCF            CALL swn_outnc_spec ( RTYPE, OQI, OQR, MIP, VOQR,&
!NCF            &VOQ, AC2, SPCSIG, SPCDIR,&
!NCF            &DEP2, KGRPNT, CROSS, IONOD )
!NCF            RETURN
!NCF         ENDIF
!NCF!
         NREF = OQI(1)
         IF (INRHOG.EQ.1) THEN
            OFAC = RHO * GRAV
         ELSE
            OFAC = 1.
         ENDIF

         IF (NREF .EQ. 0) THEN
            IERR = -1
            FILENM = OUTP_FILES(OQI(2))
            IOSTAT = -1
            CALL FOR (NREF, FILENM, 'UF', IOSTAT)
            IF (STPNOW()) RETURN
            OQI(1) = NREF
            OUTP_FILES(OQI(2)) = FILENM
!       IF (IOSTAT.NE.0) WRITE (PRINTF, 6020) FILENM, IOSTAT
!6020   FORMAT (' Open error: ', A36, I6)
!       write heading into the file
!       write keyword SWAN and version number
            WRITE (NREF, "('SWAN', I4, T41, 'Swan standard spectral file, version')") IVERF
            WRITE (NREF, "('$ Data produced by SWAN version ', A)") VERTXT
            WRITE (NREF, "('$ Project: ', A, '; run number: ', A)") PROJID, PROJNR
            IF (NSTATM.EQ.1) THEN
               WRITE (NREF, "(A, T41, A)") 'TIME', 'time-dependent data'
               WRITE (NREF, "(I6, T41, A)") ITMOPT, 'time coding option'
            ENDIF
            IF (KSPHER.EQ.0) THEN
               WRITE (NREF, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
               CRFORM = '(2F14.4)'
            ELSE
               WRITE (NREF, "(A, T41, A)") 'LONLAT',&
               &'locations in spherical coordinates'
               CRFORM = '(2F12.6)'
            ENDIF
            IF ( .NOT.PARLL .OR. .NOT.LCOMPGRD ) THEN
               WRITE (NREF, "(I6, T41, A)") MIP, 'number of locations'
               do IP = 1, MIP
                  WRITE (NREF, FMT=CRFORM) DBLE(VOQ(IP,VOQR(1))),&
                  &DBLE(VOQ(IP,VOQR(2)))
               end do
            ENDIF
            IF (RTYPE(3:3).EQ.'R' .OR. RTYPE(3:3).EQ.'L') THEN
               WRITE (NREF, "(A, T41, A)") 'RFREQ', 'relative frequencies in Hz'
            ELSE
               WRITE (NREF, "(A, T41, A)") 'AFREQ', 'absolute frequencies in Hz'
            ENDIF
            WRITE (NREF, "(I6, T41, A)") MSC, 'number of frequencies'
            do IS = 1, MSC
               WRITE (NREF, "(F10.4)") SPCSIG(IS)/PI2
            end do
            IF (RTYPE(4:4).EQ.'C') THEN
!         full 2-D spectrum
               IF (BNAUT) THEN
                  WRITE (NREF, "(A, T41, A)") 'NDIR',&
                  &'spectral nautical directions in degr'
               ELSE
                  WRITE (NREF, "(A, T41, A)") 'CDIR',&
                  &'spectral Cartesian directions in degr'
               ENDIF
               WRITE (NREF, "(I6, T41, A)") MDC, 'number of directions'
               do ID = 1, MDC
                  IF (BNAUT) THEN
                     WRITE (NREF, "(F10.4)") 180. + DNORTH - SPCDIR(ID,1)*180./PI
                  ELSE
                     WRITE (NREF, "(F10.4)") SPCDIR(ID,1)*180./PI
                  ENDIF
               end do
               WRITE (NREF, "('QUANT', /, I6, T41, 'number of quantities in table')") 1
               IF (INRHOG.EQ.1) THEN
                  WRITE (NREF, "(A, T41, A)") 'EnDens',&
                  &'energy densities in J/m2/Hz/degr'
                  WRITE (NREF, "(A, T41, A)") 'J/m2/Hz/degr', 'unit'
                  WRITE (NREF, "(E14.4, T41, A)") OVEXCV(22), 'exception value'
               ELSE
                  WRITE (NREF, "(A, T41, A)") 'VaDens',&
                  &'variance densities in m2/Hz/degr'
                  WRITE (NREF, "(A, T41, A)") 'm2/Hz/degr', 'unit'
                  WRITE (NREF, "(E14.4, T41, A)") OVEXCV(22), 'exception value'
               ENDIF
            ELSE
!         1-D spectrum
               WRITE (NREF, "('QUANT', /, I6, T41, 'number of quantities in table')") 3
               IF (INRHOG.EQ.1) THEN
                  WRITE (NREF, "(A, T41, A)") 'EnDens',  'energy densities in J/m2/Hz'
                  WRITE (NREF, "(A, T41, A)") 'J/m2/Hz', 'unit'
                  WRITE (NREF, "(E14.4, T41, A)") OVEXCV(22), 'exception value'
               ELSE
                  WRITE (NREF, "(A, T41, A)") 'VaDens', 'variance densities in m2/Hz'
                  WRITE (NREF, "(A, T41, A)") 'm2/Hz',  'unit'
                  WRITE (NREF, "(E14.4, T41, A)") OVEXCV(22), 'exception value'
               ENDIF
               IF (BNAUT) THEN
                  WRITE (NREF, "(A, T41, A)") 'NDIR',&
                  &'average nautical direction in degr'
               ELSE
                  WRITE (NREF, "(A, T41, A)") 'CDIR',&
                  &'average Cartesian direction in degr'
               ENDIF
               WRITE (NREF, "(A, T41, A)") OVUNIT(13), 'unit'
               WRITE (NREF, "(E14.4, T41, A)") OVEXCV(13), 'exception value'
               WRITE (NREF, "(A, T41, A)") 'DSPRDEGR', OVLNAM(16)
               WRITE (NREF, "(A, T41, A)") OVUNIT(16), 'unit'
               WRITE (NREF, "(E14.4, T41, A)") OVEXCV(16), 'exception value'
            ENDIF
         ENDIF

!     writing of heading is completed, write time if nonstationary

         IF (NSTATM.EQ.1) THEN
            WRITE (NREF, "(A18, T41, 'date and time')") CHTIME
         ENDIF

         IF (RTYPE(4:4).EQ.'C') THEN
            IF (RTYPE.EQ.'SPEC') THEN
               OTYPE = -2
            ELSE
               OTYPE = 2
            ENDIF
         ELSE
            IF (RTYPE.EQ.'SPE1') THEN
               OTYPE = -1
            ELSE
               OTYPE = 1
            ENDIF
         ENDIF

         do IP = 1, MIP
            IF (OPTG.NE.5) THEN
               XC = VOQ(IP,VOQR(24))
               YC = VOQ(IP,VOQR(25))
            ELSE
               IF (.NOT.EQREAL(VOQ(IP,1),OVEXCV(1))) XC = VOQ(IP,1) - XOFFS
               IF (.NOT.EQREAL(VOQ(IP,2),OVEXCV(2))) YC = VOQ(IP,2) - YOFFS
            ENDIF
            DEP = VOQ(IP,VOQR(4))
            IF (DEP.LE.0. .OR. EQREAL(DEP,OVEXCV(4))) THEN
               IF ( .NOT.PARLL .OR. IONOD(IP).EQ.INODE )&
               &WRITE (NREF, "(A6)") 'NODATA'
               CYCLE
            ENDIF
            IF ( PARLL .AND. IONOD(IP).NE.INODE ) CYCLE
            IF (ICUR.GT.0) THEN
               UX = VOQ(IP,VOQR(5))
               UY = VOQ(IP,VOQR(5)+1)
            ELSE
               UX = 0.
               UY = 0.
            ENDIF

            CALL SWCMSP (OTYPE       ,XC         ,YC          ,&
            &AC2         ,ACLOC      ,SPCSIG      ,&
            &DEP         ,DEP2       ,UX          ,&
            &UY          ,SPCDIR(1,2),SPCDIR(1,3) ,&
            &OFAC        ,KGRPNT     ,CROSS(1,IP) ,IERR        )

            IF (IERR.GT.0) THEN
               WRITE (NREF, "(A6)") 'NODATA'
            ELSE
               IF (ABS(OTYPE).EQ.2) THEN
!           write 2d spectrum
                  CALL WRSPEC (NREF, ACLOC)
               ELSE
!           write 1d spectrum
                  IF ( OPTG.NE.5 .OR. .NOT.PARLL .OR. .NOT.LCOMPGRD ) THEN
                     WRITE (NREF, "('LOCATION', I6)") IP
                  ELSE
                     WRITE (NREF, "('LOCATION', I6)") ivertg(IP)
                  ENDIF
                  DO IFR = 1, MSC
!             write frequency spectra to file
                     WRITE (NREF, "(E12.4, 2F7.1)") (ACLOC(IFR+JJ*MSC), JJ=0,2)
                  ENDDO
               ENDIF
            ENDIF
         end do

         RETURN
! * end of subroutine SWSPEC *
      end subroutine SWSPEC
!MatL4!****************************************************************
!MatL4!
!MatL4      SUBROUTINE SWRMAT ( MROWS , NCOLS, MATNAM, RDATA,&
!MatL4      &IOUTMA, IREC , IDLA  , DUMVAL )
!MatL4         USE swan_service_interfaces, ONLY: STRACE
!MatL4         USE swan_number_formatting, ONLY: INTSTR
!MatL4!
!MatL4!****************************************************************
!MatL4!
!MatL4         USE swan_diagnostics_level
!MatL4!
!MatL4         IMPLICIT NONE
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
!MatL4!     Writes block output to a binary file in the MAT-format
!MatL4!     to be used in MATLAB
!MatL4!
!MatL4!  3. Method
!MatL4!
!MatL4!     1) The binary file BINFIL must be opened with the following
!MatL4!        statement:
!MatL4!
!MatL4!        OPEN(UNIT=IOUTMA, FILE=BINFIL, FORM='UNFORMATTED',
!MatL4!             ACCESS='DIRECT', RECL=1)
!MatL4!
!MatL4!        Furthermore, initialize record counter to IREC = 1
!MatL4!
!MatL4!     2) Be sure to close the binary file when there are no more
!MatL4!        matrices to be saved
!MatL4!
!MatL4!     3) The matrix may contain signed infinity and/or Not a Numbers.
!MatL4!        According to the IEEE standard, on a 32-bit machine, the real
!MatL4!        format has an 8-bit biased exponent (=actual exponent increased
!MatL4!        by bias=127) and a 23-bit fraction or mantissa. The leftmost
!MatL4!        bit is the sign bit. Let a fraction, biased exponent and sign
!MatL4!        bit be denoted as F, E and S, respectively. The following
!MatL4!        formats adhere to IEEE standard:
!MatL4!
!MatL4!          S = 0, E = 11111111 and F  = 00 ... 0 : X = +Inf
!MatL4!          S = 1, E = 11111111 and F  = 00 ... 0 : X = -Inf
!MatL4!          S = 0, E = 11111111 and F <> 00 ... 0 : X = NaN
!MatL4!
!MatL4!        Hence, the representation of +Inf equals 2**31 - 2**23. A
!MatL4!        representation of a NaN equals the representation of +Inf
!MatL4!        plus 1.
!MatL4!
!MatL4!        The Cray machine C916 at SARA Information Centre does not
!MatL4!        support the IEEE standard.
!MatL4!
!MatL4!     4) The NaN's or Inf's are indicated by a dummy value as given
!MatL4!        by dumval
!MatL4!
!MatL4!     For more information consult "Appendix - MAT-File Structure"
!MatL4!     of the MATLAB External Data Reference guide (Version 4.2)
!MatL4!
!MatL4!  4. Argument variables
!MatL4!
!MatL4!     DUMVAL      a dummy value meant for indicating NaN
!MatL4!     IDLA        controls lay-out of output (see user manual)
!MatL4!     IOUTMA      unit number of binary MAT-file
!MatL4!     IREC        direct access file record counter
!MatL4!     MATNAM      character array holding the matrix name
!MatL4!     MROWS       a 4-byte integer representing the number of
!MatL4!                 rows in matrix
!MatL4!     NCOLS       a 4-byte integer representing the number of
!MatL4!                 columns in matrix
!MatL4!     RDATA       real array consists of MROWS * NCOLS real
!MatL4!                 elements stored column wise
!MatL4!
!MatL4         INTEGER       MROWS, NCOLS, IDLA, IOUTMA, IREC
!MatL4         REAL          RDATA(*), DUMVAL
!MatL4         CHARACTER(LEN=*) :: MATNAM
!MatL4!
!MatL4!  5. Parameter variables
!MatL4!
!MatL4!     ---
!MatL4!
!MatL4!  6. Local variables
!MatL4!
!MatL4!     BVAL  :     a byte value
!MatL4!     CHARS :     array to pass character info to MSGERR
!MatL4!     I     :     loop variable
!MatL4!     IENT  :     number of entries
!MatL4!     IF    :     first non-character in string
!MatL4!     IL    :     last non-character in string
!MatL4!     IMAGF :     a 4-byte imaginary flag. Possible values are:
!MatL4!                 0: there is only real data
!MatL4!                 1: the data has also an imaginary part
!MatL4!     IOS   :     auxiliary integer with iostat-number
!MatL4!     ITYPE :     the type flag containing a 4-byte integer whose
!MatL4!                 decimal digits encode storage information.
!MatL4!                 If the integer is represented as ABCD then:
!MatL4!                 "A" indicates the format to write the binary
!MatL4!                 data to a file on the machine. Possible values are:
!MatL4!                   0: Intel based machines (PC 386/486, Pentium)
!MatL4!                   1: Motorola 68000 based machines (Macintosh,
!MatL4!                      HP 9000, SPARC, Apollo, SGI)
!MatL4!                   2: VAX-D format
!MatL4!                   3: VAX-G format
!MatL4!                   4: Cray
!MatL4!                 "B" is always zero
!MatL4!                 "C" indicates which format the data is stored.
!MatL4!                  Possible values are:
!MatL4!                   0: REAL(KIND=KIND(0.0D0)) (64 bit) floating point numbers
!MatL4!                   1: single precision (32 bit) floating point numbers
!MatL4!                   2: 32-bit signed integers
!MatL4!                   3: 16-bit signed integers
!MatL4!                   4: 16-bit unsigned integers
!MatL4!                   5: 8-bit unsigned integers
!MatL4!                 "D" indicates the type of data (matrix).
!MatL4!                  Possible values:
!MatL4!                   0: numeric matrix
!MatL4!                   1: textual matrix
!MatL4!                   2: sparse  matrix
!MatL4!     J     :     index
!MatL4!     M     :     loop variable
!MatL4!     MSGSTR:     string to pass message to call MSGERR
!MatL4!     N     :     loop variable
!MatL4!     NAMLEN:     a 4-byte integer representing the number of
!MatL4!                 characters in matrix name plus 1
!MatL4!     NANVAL:     an integer representing Not a Number
!MatL4!
!MatL4         INTEGER I, J, IF, IL, IOS, M, N
!MatL4         INTEGER, SAVE :: IENT = 0
!MatL4         INTEGER BVAL(4), IMAGF, ITYPE, NAMLEN, NANVAL
!MatL4         CHARACTER(LEN=20) CHARS
!MatL4         CHARACTER(LEN=80) MSGSTR
!MatL4!
!MatL4!  8. Subroutines used
!MatL4!
!MatL4!     INTSTR           Converts integer to string
!MatL4!     MSGERR           Writes error message
!MatL4!     TXPBLA           Removes leading and trailing blanks in string
!MatL4!     SWI2B            Calculates 32-bit representation of an
!MatL4!                      integer number
!MatL4!     SWR2B            Calculates 32-bit representation of a
!MatL4!                      floating-point number
!MatL4!
!MatL4!  9. Subroutines calling
!MatL4!
!MatL4!     ---
!MatL4!
!MatL4! 10. Error messages
!MatL4!
!MatL4!     ---
!MatL4!
!MatL4! 11. Remarks
!MatL4!
!MatL4!     ---
!MatL4!
!MatL4! 12. Structure
!MatL4!
!MatL4!     set Not a Number
!MatL4!
!MatL4!     set some flags
!MatL4!
!MatL4!     write header consisting of ITYPE, MROWS, NCOLS, IMAGF, NAMLEN and
!MatL4!     name of matrix MATNAM
!MatL4!
!MatL4!     write matrix
!MatL4!
!MatL4!     if necessary, give message that error occurred while writing file
!MatL4!
!MatL4! 13. Source text
!MatL4!
!MatL4         IF (LTRACE) CALL STRACE (IENT,'SWRMAT')
!MatL4
!MatL4!     --- set Not a Number
!MatL4
!MatL4         NANVAL = 255 * 2**23 + 1
!MatL4
!MatL4!     --- set some flags
!MatL4
!MatL4         ITYPE = 1010
!MatL4         IMAGF = 0
!MatL4         IOS   = 0
!MatL4
!MatL4!     --- write header consisting of ITYPE, MROWS, NCOLS, IMAGF,
!MatL4!         NAMLEN and name of matrix MATNAM
!MatL4!         the name should be ended by zero-byte terminator
!MatL4
!MatL4         CALL SWI2B ( ITYPE, BVAL )
!MatL4         DO I = 1, 4
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4
!MatL4         CALL SWI2B ( MROWS, BVAL )
!MatL4         DO I = 1, 4
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4
!MatL4         CALL SWI2B ( NCOLS, BVAL )
!MatL4         DO I = 1, 4
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4
!MatL4         CALL SWI2B ( IMAGF, BVAL )
!MatL4         DO I = 1, 4
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4
!MatL4         CALL TXPBLA(MATNAM,IF,IL)
!MatL4         NAMLEN = IL - IF + 2
!MatL4         CALL SWI2B ( NAMLEN, BVAL )
!MatL4         DO I = 1, 4
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4
!MatL4         DO I = IF, IL
!MatL4            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) MATNAM(I:I)
!MatL4            IREC = IREC + 1
!MatL4         END DO
!MatL4         IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(0)
!MatL4         IREC = IREC + 1
!MatL4
!MatL4!     --- write matrix
!MatL4
!MatL4         DO M = 1, NCOLS
!MatL4            DO N = 1, MROWS
!MatL4               IF ( IDLA.EQ.1 ) THEN
!MatL4                  J = (MROWS-N)*NCOLS + M
!MatL4               ELSE
!MatL4                  J = (N-1)*NCOLS + M
!MatL4               END IF
!MatL4               IF (RDATA(J).NE.DUMVAL ) THEN
!MatL4                  CALL SWR2B ( RDATA(J), BVAL )
!MatL4               ELSE IF (.NOT. DUMVAL.NE.0. ) THEN
!MatL4                  CALL SWR2B ( RDATA(J), BVAL )
!MatL4               ELSE
!MatL4                  CALL SWI2B ( NANVAL, BVAL )
!MatL4               END IF
!MatL4               DO I = 1, 4
!MatL4                  IF (IOS.EQ.0)&
!MatL4                  &WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
!MatL4                  IREC = IREC + 1
!MatL4               END DO
!MatL4            END DO
!MatL4         END DO
!MatL4
!MatL4!     --- if necessary, give message that error occurred while writing f
!MatL4
!MatL4         IF ( IOS.NE.0 ) THEN
!MatL4            CHARS = INTSTR(IOS)
!MatL4            CALL TXPBLA(CHARS,IF,IL)
!MatL4            MSGSTR = 'Error while writing binary MAT-file - '//&
!MatL4            &'IOSTAT number is '//CHARS(IF:IL)
!MatL4            CALL MSGERR ( 4, MSGSTR )
!MatL4            RETURN
!MatL4         END IF
!MatL4
!MatL4         RETURN
!MatL4      end subroutine SWRMAT
!MatL5!****************************************************************
!MatL5!
!MatL5      SUBROUTINE SWRMAT ( MROWS , NCOLS, MATNAM, RDATA,&
!MatL5      &IOUTMA, IREC , IDLA  , DUMVAL )
!MatL5         USE swan_service_interfaces, ONLY: STRACE
!MatL5!
!MatL5!****************************************************************
!MatL5!
!MatL5         USE swan_diagnostics_level
!MatL5!
!MatL5         IMPLICIT NONE
!MatL5!
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
!MatL5!
!MatL5!  0. Authors
!MatL5!
!MatL5!     40.30: Marcel Zijlema
!MatL5!     40.41: Marcel Zijlema
!MatL5!     41.08: Pieter Smit
!MatL5!
!MatL5!  1. Updates
!MatL5!
!MatL5!     40.30, May  03: New subroutine
!MatL5!     40.41, Oct. 04: common blocks replaced by modules, include files r
!MatL5!     41.08, Aug. 09: adapted to write binary file in the Level 5 MAT-fi
!MatL5!
!MatL5!  2. Purpose
!MatL5!
!MatL5!     Writes block output to a binary file in the MAT-format
!MatL5!     to be used in MATLAB
!MatL5!
!MatL5!  3. Method
!MatL5!
!MatL5!     1) The binary file BINFIL must be opened with the following
!MatL5!        statement:
!MatL5!
!MatL5!        OPEN(UNIT=IOUTMA, FILE=BINFIL, FORM='UNFORMATTED',
!MatL5!             ACCESS='DIRECT', RECL=4)
!MatL5!
!MatL5!        Furthermore, initialize record counter to IREC = 1
!MatL5!
!MatL5!     2) Be sure to close the binary file when there are no more
!MatL5!        matrices to be saved
!MatL5!
!MatL5!     3) The matrix may contain signed infinity and/or Not a Numbers.
!MatL5!        According to the IEEE 754 standard, on a 32-bit machine, the re
!MatL5!        format has an 8-bit biased exponent (=actual exponent increased
!MatL5!        by bias=127) and a 23-bit fraction or mantissa. The leftmost
!MatL5!        bit is the sign bit. Let a fraction, biased exponent and sign
!MatL5!        bit be denoted as F, E and S, respectively. The following
!MatL5!        formats adhere to IEEE standard:
!MatL5!
!MatL5!          S = 0, E = 11111111 and F  = 00 ... 0 : X = +Inf
!MatL5!          S = 1, E = 11111111 and F  = 00 ... 0 : X = -Inf
!MatL5!          S = 0, E = 11111111 and F <> 00 ... 0 : X = NaN
!MatL5!
!MatL5!        Hence, the representation of +Inf equals 2**31 - 2**23. A
!MatL5!        representation of a NaN equals the representation of +Inf
!MatL5!        plus 1.
!MatL5!
!MatL5!     4) The NaN's or Inf's are indicated by a dummy value as given
!MatL5!        by dumval
!MatL5!
!MatL5!     For more information on the Level 5 MAT-file format consult
!MatL5!     document "MAT-File Format" of MathWorks
!MatL5!
!MatL5!  4. Argument variables
!MatL5!
!MatL5!     DUMVAL      a dummy value meant for indicating NaN
!MatL5!     IDLA        controls lay-out of output (see user manual)
!MatL5!     IOUTMA      unit number of binary MAT-file
!MatL5!     IREC        direct access file record counter
!MatL5!     MATNAM      character array holding the matrix name
!MatL5!     MROWS       a 4-byte integer representing the number of
!MatL5!                 rows in matrix
!MatL5!     NCOLS       a 4-byte integer representing the number of
!MatL5!                 columns in matrix
!MatL5!     RDATA       real array consists of MROWS * NCOLS real
!MatL5!                 elements stored column wise
!MatL5!
!MatL5         INTEGER       MROWS, NCOLS, IDLA, IOUTMA, IREC
!MatL5         REAL          RDATA(*), DUMVAL
!MatL5         CHARACTER(LEN=*) :: MATNAM
!MatL5!
!MatL5!  5. Parameter variables
!MatL5!
!MatL5!     BlockSize   size of matlab data segment
!MatL5!     DataSize    number of bytes written per write statement
!MatL5!     HeaderSize  size of the header in bytes
!MatL5!     mChar       character data
!MatL5!     mInt32      signed   INTEGER(KIND=SELECTED_INT_KIND(9))
!MatL5!     mUInt32     unsigned INTEGER(KIND=SELECTED_INT_KIND(18))
!MatL5!     mSingle     real
!MatL5!
!MatL5!     --- standard sizes
!MatL5!
!MatL5         INTEGER, PARAMETER :: DataSize   = 4
!MatL5         INTEGER, PARAMETER :: HeaderSize = 128
!MatL5         INTEGER, PARAMETER :: BlockSize  = 8
!MatL5!
!MatL5!     --- Matlab data types
!MatL5!
!MatL5         INTEGER, PARAMETER :: mChar      = 1
!MatL5         INTEGER, PARAMETER :: mInt32     = 5
!MatL5         INTEGER, PARAMETER :: mUInt32    = 6
!MatL5         INTEGER, PARAMETER :: mSingle    = 7
!MatL5!
!MatL5!  6. Local variables
!MatL5!
!MatL5!     CTMP  :     a temporary character array
!MatL5!     HEADER:     header of binary MAT-file
!MatL5!     I     :     loop variable
!MatL5!     IENT  :     number of entries
!MatL5!     IOS   :     auxiliary integer with iostat-number
!MatL5!     IRECS :     size of array including tags and flags
!MatL5!     J     :     index
!MatL5!     M     :     loop variable
!MatL5!     MSGSTR:     string to pass message to call MSGERR
!MatL5!     N     :     loop variable
!MatL5!     NAMLEN:     a 4-byte integer representing the number of
!MatL5!                 characters in matrix name
!MatL5!     NANVAL:     an integer representing Not a Number
!MatL5!     NTOT  :     size of data array
!MatL5!
!MatL5         INTEGER I, J, IOS, M, N, NTOT
!MatL5         INTEGER, SAVE :: IENT = 0
!MatL5         INTEGER NAMLEN, NANVAL
!MatL5         INTEGER, SAVE :: IRECS
!MatL5         CHARACTER(LEN=80) MSGSTR
!MatL5         CHARACTER(LEN=HeaderSize) HEADER
!MatL5         CHARACTER(LEN=BlockSize) CTMP
!MatL5
!MatL5!
!MatL5!  8. Subroutines used
!MatL5!
!MatL5!     MSGERR           Writes error message
!MatL5!
!MatL5!  9. Subroutines calling
!MatL5!
!MatL5!     ---
!MatL5!
!MatL5! 10. Error messages
!MatL5!
!MatL5!     ---
!MatL5!
!MatL5! 11. Remarks
!MatL5!
!MatL5!     ---
!MatL5!
!MatL5! 12. Structure
!MatL5!
!MatL5!     set Not a Number
!MatL5!     length of name matrix
!MatL5!     size of data array
!MatL5!     write header once
!MatL5!     array name
!MatL5!     write matrix
!MatL5!     write the size of the array
!MatL5!     if necessary, give message that error occurred while writing file
!MatL5!
!MatL5! 13. Source text
!MatL5!
!MatL5         IF (LTRACE) CALL STRACE (IENT,'SWRMAT')
!MatL5
!MatL5         IOS = 0
!MatL5
!MatL5!     --- set Not a Number
!MatL5
!MatL5         NANVAL = 255 * 2**23 + 1
!MatL5
!MatL5!     --- length of name matrix
!MatL5
!MatL5         NAMLEN = LEN_TRIM(MATNAM)
!MatL5
!MatL5!     --- size of data array
!MatL5
!MatL5         NTOT = MROWS * NCOLS
!MatL5
!MatL5!     --- descriptive header
!MatL5
!MatL5         WRITE (HEADER, '(6A)') 'Data produced by SWAN version ',&
!MatL5         &TRIM(VERTXT),'; project: ',TRIM(PROJID),&
!MatL5         &'; run number: ',PROJNR
!MatL5
!MatL5!     --- data offset
!MatL5
!MatL5         HEADER(117:124) = CHAR(ICHAR(' '))
!MatL5
!MatL5!     --- version
!MatL5
!MatL5         HEADER(125:126) = CHAR(0) // CHAR(1)
!MatL5
!MatL5!     --- endian indicator
!MatL5
!MatL5         WRITE(HEADER(127:128),'(A)') INT(19785,KIND=2)
!MatL5
!MatL5!     --- write header once
!MatL5
!MatL5         IF ( IREC.EQ.1 ) THEN
!MatL5            DO I = 1, HeaderSize/DataSize
!MatL5               J = DataSize*(I-1) + 1
!MatL5               IF ( IOS.EQ.0 )&
!MatL5               &WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) HEADER(J:J+DataSize-1)
!MatL5               IREC = IREC + 1
!MatL5            END DO
!MatL5         END IF
!MatL5
!MatL5!     --- array tag
!MatL5
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 14
!MatL5         IREC = IREC + 1
!MatL5         IRECS = IREC
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
!MatL5         IREC = IREC + 1
!MatL5
!MatL5!     --- array flags
!MatL5
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mInt32
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 2*DataSize
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 7
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
!MatL5         IREC = IREC + 1
!MatL5
!MatL5         IF ( MOD(2,BlockSize/DataSize).NE.0 ) THEN
!MatL5            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
!MatL5            IREC = IREC + 1
!MatL5         END IF
!MatL5
!MatL5!     --- dimensions array
!MatL5
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mInt32
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 2*DataSize
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) MROWS
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) NCOLS
!MatL5         IREC = IREC + 1
!MatL5
!MatL5         IF ( MOD(2,BlockSize/DataSize).NE.0 ) THEN
!MatL5            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
!MatL5            IREC = IREC + 1
!MatL5         END IF
!MatL5
!MatL5!     --- array name
!MatL5
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mChar
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) NAMLEN
!MatL5         IREC = IREC + 1
!MatL5
!MatL5         I = 1
!MatL5         DO
!MatL5            CTMP(1:8) = CHAR(ICHAR(' '))
!MatL5            IF ( I.GT.NAMLEN ) THEN
!MatL5               EXIT
!MatL5            ELSE IF ( I+BlockSize.LE.NAMLEN ) THEN
!MatL5               CTMP(1:8) = MATNAM(I:I+BlockSize-1)
!MatL5            ELSE
!MatL5               CTMP(1:NAMLEN-I+1) = MATNAM(I:NAMLEN)
!MatL5            END IF
!MatL5
!MatL5            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CTMP(1:4)
!MatL5            IREC = IREC + 1
!MatL5            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CTMP(5:8)
!MatL5            I = I + BlockSize
!MatL5            IREC = IREC + 1
!MatL5         END DO
!MatL5
!MatL5!     --- write matrix
!MatL5
!MatL5         IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) mSingle
!MatL5         IREC = IREC + 1
!MatL5         IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) NTOT*DataSize
!MatL5         IREC = IREC + 1
!MatL5
!MatL5         DO M = 1, NCOLS
!MatL5            DO N = 1, MROWS
!MatL5               IF ( IDLA.EQ.1 ) THEN
!MatL5                  J = (MROWS-N)*NCOLS + M
!MatL5               ELSE
!MatL5                  J = (N-1)*NCOLS + M
!MatL5               END IF
!MatL5               IF (RDATA(J).NE.DUMVAL ) THEN
!MatL5                  WRITE (IOUTMA,REC=IREC) RDATA(J)
!MatL5               ELSE IF (.NOT. DUMVAL.NE.0. ) THEN
!MatL5                  WRITE (IOUTMA,REC=IREC) RDATA(J)
!MatL5               ELSE
!MatL5                  WRITE (IOUTMA,REC=IREC) NANVAL
!MatL5               END IF
!MatL5               IREC = IREC + 1
!MatL5            END DO
!MatL5         END DO
!MatL5
!MatL5         IF ( MOD(NTOT,BlockSize/DataSize).NE.0 ) THEN
!MatL5            IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) 0.
!MatL5            IREC = IREC + 1
!MatL5         END IF
!MatL5
!MatL5!     --- write the size of the array
!MatL5
!MatL5         IF ( IOS.EQ.0 )&
!MatL5         &WRITE (IOUTMA,REC=IRECS,IOSTAT=IOS) (IREC-IRECS-1)*DataSize
!MatL5
!MatL5!     --- if necessary, give message that error occurred while writing f
!MatL5
!MatL5         IF ( IOS.NE.0 ) THEN
!MatL5            WRITE (MSGSTR, '(A,I5)')&
!MatL5            &'Error while writing binary MAT-file - '//&
!MatL5            &'IOSTAT number is ', IOS
!MatL5            CALL MSGERR( 4, TRIM(MSGSTR) )
!MatL5            RETURN
!MatL5         END IF
!MatL5
!MatL5         RETURN
!MatL5      end subroutine SWRMAT

end module swan_output_writers
