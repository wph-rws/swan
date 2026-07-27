
!     SWAN main program and miscellaneous routines
!
!     Contents of this file:
!
!     SWAN:   Main program
!     SWMAIN: Calling SWINIT, SWREAD, SWCOMP and SWOUTP
!     SWINIT: Initialize several variables and arrays
!     SWPREP: Do some preparations before computation is started
!     SPRCON: Execution of some tests on the given model description
!     SWRBC
!     SVALQI
!     SINUPT
!     SINBTG
!     SINCMP
!     WRTEST
!     ERRCHK
!     SNEXTI
!     RBFILE: Read boundary spectra from one file
!     RESPEC: Read one 1-d OR 2-d boundary spectrum from file, and
!             transform to internal SWAN spectral resolution
!     FLFILE: Update boundary conditions, update nonstationary input
!             fields
!     SWINCO
!     SWCLME: Clean memory
!
!************************************************************************
!                                                                      *

module swan_driver
   use swan_diffraction_state, only: diffraction_state_t
   use swan_triad_state, only: triad_state_t
   use swan_snl4_tables, only: snl4_tables_t
   use swan_spectral_powers, only: spectral_powers_t
   use swan_source_workspaces, only: thread_workspaces_t
   use swan_input_helpers, only: LSPLIT
   use swan_comp_unstruc, only: SwanCompUnstruc
   use swan_prep_comp, only: SwanPrepComp
   use swan_vertlist, only: SwanVertlist
   use swan_io_limits, only: LENFNM
   use swan_output_variables, only: NMOVAR, OVEXCV, OVHEXP, OVKEYW, OVLEXP, OVLLIM, OVLNAM, OVSNAM, OVSVTY, OVULIM, OVUNIT
   use swan_output_quadrature, only: ALPQ, COSPQ, SINPQ, XPQ, XQLEN, YPQ, YQLEN
   use swan_project_metadata, only: PROJID, PROJNR, PROJT1, PROJT2, PROJT3, VERTXT
   use swan_output_variables, only: UF, UP, UST, UT
   use swan_time, only: CHTIME
   use swan_output_settings, only: ERRPTS, INRHOG, IUBOTR, OUTPAR, SNAME
   implicit none(type, external)
!  Used across several procedures of this module and nowhere else; moved out
!  of the central shared state.
   INTEGER :: JDP3
   INTEGER :: JVX3
   INTEGER :: JVY3
   INTEGER :: JWLV1
   INTEGER :: JWLV3
   REAL :: SY0
   private
!  SWMAIN is the only entry point the main program needs; the eighteen
!  remaining routines (initialisation, preparation, boundary and restart
!  handling, clean-up) are implementation detail.
   public :: SWMAIN
contains
!************************************************************************
!                                                                      *
SUBROUTINE SWMAIN
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_computation, ONLY: SWCOMP
   USE swan_parallel, ONLY: SWINITMPI, SWEXITMPI, SWSYNC, SWCOLLECT, SWCOLOUT
   USE swan_services, ONLY: HSOBND
   USE swan_command_reading, ONLY: SWREAD
   USE swan_output_orchestration, ONLY: SWOUTP
!TIMG   USE swan_time, ONLY: DCUMTM, NCUMTM
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, TXPBLA, STPNOW, SWTSTA, SWTSTO, SWPRTI
!                                                                      *
!************************************************************************

   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_time
   USE swan_number_formatting
   USE swan_computational_grid_kind
   USE swan_boundary_counters
   USE swan_run_mode
   USE SWCOMM3
   USE swan_test_output
   USE swan_propagation_scheme
   USE OUTP_DATA
   USE M_GENARR
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanIEM
   USE SwanBraggScat
   USE SwanQCM
   USE SwanGriddata
!METIS   USE SwanParallel

   IMPLICIT NONE(TYPE, EXTERNAL)
   CHARACTER(LEN=LENFNM) :: FILENM   ! file name buffer, local to this routine

!  De diffractietoestand hoort bij de run; de driver is de eigenaar en geeft
!  hem expliciet door aan voorbereiding, berekening, uitvoer en opruiming.
   TYPE(diffraction_state_t) :: DIFFRACTION
   TYPE(triad_state_t) :: TRIADS
   TYPE(snl4_tables_t) :: SNL4
   TYPE(spectral_powers_t) :: SPECTRAL_POWERS
   TYPE(thread_workspaces_t) :: THREAD_WORKSPACES


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
!     30.60: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence verion)
!     32.01: Roeland Ris & Cor van der Schelde
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     34.01: IJsbrand Haagsma
!     40.00, 40.13: Nico Booij
!     34.01: Jeroen Adema
!     33.08: W. Erick Rogers
!     40.22: John Cazes and Tim Campbell
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.95: Marcel Zijlema
!     41.20: Casey Dietrich
!     41.36: Marcel Zijlema
!     41.75: Erick Rogers
!     41.80: Dirk Rijnsdorp and Ad Reniers
!     41.85: Ad Reniers
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!     41.95: Marcel Zijlema
!
!  1. Updates
!
!            10 FEB   Subroutine SWMAIN introduced
!     30.60, Aug. 97: argument list of ERRCHK changed
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.72, Nov. 97: declaration of ITERMX removed because it is a common
!                     variable, which is declared in the INCLUDE file
!     30.72, Nov. 97: PWTAIL(3) is made dependent on PWTAIL(1), also in
!                     initialisation
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     32.01, Jan. 98: Nautical convention included (project h3268)
!     32.01, Jan. 98: Comparison of computed and prescribed significant
!                     wave height (project h3268)
!     32.02, Jan. 98: Introduction of 1D-version
!     30.72, Mar. 98: Added instruction to change [maxerr] in case of a
!                     terminating warning
!     40.00, Nov. 97: time step loop reorganized,
!                     argument list in call SNEXTI changed
!                     declaration of ITERMX removed
!                     argument added in call SWOUTP
!     30.82, Sep. 98: Added check on error level each time step to prevent
!                     continuation of computation
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     34.01, Feb. 99: Changed STOP statement in a jump to end of subroutine
!     34.01, Feb. 99: Close all files at end of this subroutine
!     34.01, Feb. 99: Introducing STPNOW
!     33.08, July 98: S&L scheme-related changes
!     40.13, July 01: coefficient PTRIAD(4) added
!                     make file 'norm_end' if program ends normally
!     40.22, Oct. 01: call SWCOMP changed in view of parallellization
!     40.23, Aug. 02: Print of CPU times added
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL mechanism and reconsidering this
!                     subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: re-design output process in parallel mode
!     40.80, Jun. 07: extension to unstructured grids
!     40.95, Jun. 08: parallelization of unSWAN
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!     41.36, Jun. 12: collecting data for PunSWAN
!     41.75, Jan. 19: adding sea ice
!     41.80, Sep. 21: adding Bragg scattering
!     41.85, Feb. 19: implementation of IEM (surfbeat model)
!     41.90, Jun. 21: adding quasi-coherent modelling
!     41.95, Jul. 22: extension to write block output to VTK files
!
!  2. Purpose
!
!     SWMAIN subroutine, calling SWINIT, SWREAD, SWCOMP and SWOUTP
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!  6. Local variables
!
!     AC1   :     Contains action density at previous time step
!     BGRIDP:     data concerning boundary grid points
!     BLKND :     array giving node number per subdomain
!     BLKNDC:     auxiliary array for collecting the array BLKND
!     BSPECS:     array containing boundary spectra
!     CHARS :     array to pass character info to MSGERR
!     COMPDA:     array containing various data depending on grid
!     CROSS :     integer array indicating obstacle crossing
!                 (0=no, >0=yes)
!     ILEN  :     length of array
!     INERR :     number of the initialisation error
!     OURQT :     array indicating at what time requested output
!                 is processed
!     IUNIT :     counter for file unit numbers
!     LOPEN :     indicates whether a file is open
!     MSGSTR:     string to pass message to call MSGERR

   INTEGER   IUNIT
   INTEGER   IOSTAT, IT0, IT, SAVITE, ILEN
   INTEGER   IGRID, IVT, IVTYPE, MXOUTAR
   INTEGER   INERR
   INTEGER   ISTAT, IF1, IL1
   CHARACTER(LEN=4)  :: COMPUT
   CHARACTER(LEN=20) CHARS(1)
   CHARACTER(LEN=80) MSGSTR
   LOGICAL   LOPEN
   INTEGER   IRQ, UPVD

   INTEGER, ALLOCATABLE :: CROSS(:)
   INTEGER, ALLOCATABLE :: BGRIDP(:)
   REAL   , ALLOCATABLE :: BSPECS(:,:,:,:)
   REAL   , ALLOCATABLE :: AC1(:,:,:), COMPDA(:,:)

   REAL, ALLOCATABLE    :: BLKND(:), BLKNDC(:)
   REAL(KIND=KIND(0.0D0)), ALLOCATABLE  :: OURQT(:)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     SWINIT
!     SWREAD
!     FOR
!     SWPREP
!     ERRCHK
!     SWRBC
!     SWINCO
!     SWCOLOUT
!     SWGATHER
!     SWSYNC
!     SWCOLLECT        Collects geographical field array from all nodes
!     SNEXTI
!     SWCOMP
!     HSOBND: Generates warning if comp. and prescr. Hs differ more than  32.01
!             a fraction HSRERR at the up-wave boundary
!     SWOUTP
!TIMG!     SWPRTI
!TIMG!     SWTSTA
!TIMG!     SWTSTO
!     MSGERR : Handles error messages according to severity
!     NUMSTR : Converts integer/real to string
!     TXPBLA : Removes leading and trailing blanks in string


!  9. Subroutines calling
!
!     MAIN program SWAN
!
! 10. Error Messages
!
!     ---
!
! 11. Remarks
!
!     The description of the structure of this subroutine is very
!     short as most of the source code can easily be understood with
!     the aid of the command descriptions in the user manual and the
!     purpose of the subroutines from the system documentation.
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     Call SWINIT to initialize various common data
!     Repeat
!         Call SWREAD to read and process user commands
!         If last command was STOP
!         Then exit from repeat
!         -------------------------------------------------------------
!         Call SWPREP to check input and prepare computation
!         If nonstationary computation is to be made
!         Then start time step loop at IT=0 and
!         Call SWINCO to calculate initial wave spectra
!         -------------------------------------------------------------
!         For requested number of time steps do
!             Call SNEXTI to update boundary conditions and input fields  40.00
!             If IT>0
!             Then Call SWCOMP to calculate the wave field
!             ---------------------------------------------------------
!             Call SWOUTP to postprocess the results and create output
!             Update time
!     ----------------------------------------------------------------
!
! 13. Source text
!
!     --- initialize various data
!TIMG
!TIMG   DCUMTM(:,1:2) = 0D0
!TIMG   NCUMTM(:)     = 0
!TIMG   CALL SWTSTA(1)

   LEVERR=0
   MAXERR=1
   ITRACE=0
   INERR =0
   ISTAT =0

!TIMG   CALL SWTSTA(2)
   CALL SWINIT (INERR, SNL4)
!TIMG   CALL SWTSTO(2)
   IF (INERR.GT.0) RETURN
   IF (STPNOW()) RETURN

   COMPUT = '    '
   RUNMADE=.FALSE.

!     --- repeat

   main_loop: DO

!       --- read and process user commands
!
!TIMG      CALL SWTSTA(3)
      CALL SWREAD (COMPUT, TRIADS, SNL4, SPECTRAL_POWERS)
!TIMG      CALL SWTSTO(3)
      IF (STPNOW()) RETURN

!       --- if last command was STOP then exit from repeat

      IF (COMPUT.EQ.'STOP') THEN
         IUNIT  = 0
         IOSTAT = 0
         FILENM = 'norm_end'
         CALL FOR (IUNIT, FILENM, 'UF', IOSTAT)
         WRITE (IUNIT, *) ' Normal end of run ', PROJNR
         EXIT main_loop
      ENDIF

!       --- surfbeat: initialize variables and arrays for 2nd COMPUTE

      IF ( ntf.GT.0 ) CALL SwanIEMinitig(SPECTRAL_POWERS)

!       --- allocate some arrays meant for computation

      IF (NUMOBS .GT. 0) THEN
         IF (OPTG.NE.5) THEN
!             structured grid
            ILEN = 2*MCGRD
         ELSE
!             unstructured grid
            ILEN = nfaces
         ENDIF
         IF (.NOT.ALLOCATED(CROSS)) ALLOCATE(CROSS(ILEN))
      ELSE
         IF (.NOT.ALLOCATED(CROSS)) ALLOCATE(CROSS(0))
      ENDIF
      IF (ALOBND.AND.ALLOCATED(BSPECS)) DEALLOCATE(BSPECS)
      IF (.NOT.ALLOCATED(BSPECS)) ALLOCATE(BSPECS(MDC,MSC,NBSPEC,2))
      IF (ALOBND.AND.ALLOCATED(BGRIDP)) DEALLOCATE(BGRIDP)
      IF (.NOT.ALLOCATED(BGRIDP)) ALLOCATE(BGRIDP(6*NBGRPT))

!       --- do some preparations before computation
!
!TIMG      CALL SWTSTA(4)
      CALL SWPREP ( BSPECS, BGRIDP, CROSS , XCGRID, YCGRID, KGRPNT,&
      &KGRBND, SPCDIR, SPCSIG, DIFFRACTION, TRIADS )
      IF (OPTG.EQ.5) CALL SwanPrepComp ( CROSS )
      IF (STPNOW()) RETURN
      ALOBND = .FALSE.
!TIMG      CALL SWTSTO(4)
!
!       --- check all possible flags and if necessary change
!           if option is not correct

      CALL ERRCHK
      IF (STPNOW()) RETURN

!       --- compute mean depths and bottom spectra
!           in case of Bragg scattering
      IF ( IBRAG.NE.0 ) CALL SWBRBOT
      IF (STPNOW()) RETURN

!       --- initialisation of necessary grids for depth,
!           current, wind and friction

      IF (ALOCMP.AND.ALLOCATED(COMPDA)) DEALLOCATE(COMPDA)
      IF (.NOT.ALLOCATED(COMPDA)) THEN
         ALLOCATE(COMPDA(MCGRD,MCMVAR),STAT=ISTAT)
         COMPDA = 0.
         ALOCMP = .FALSE.
      END IF
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array COMPDA and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF

!TIMG      CALL SWTSTA(5)
      CALL SWRBC(COMPDA)
!TIMG      CALL SWTSTO(5)

      IF ( IBRAG.NE.0 ) THEN
!          arrays dpmean and botspc are temporary, can be de-allocated
         IF (ALLOCATED(dpmean)) DEALLOCATE(dpmean)
         IF (ALLOCATED(botspc)) DEALLOCATE(botspc)

!          --- compute bed elevation spectrum at wave number difference
!              for the whole computational grid
         IF (IBRAG.EQ.1) CALL SWFBXY(COMPDA(1,JDP2), COMPDA(1,JMUDL2),&
         &SPCSIG        , SPCDIR          )
         IF (STPNOW()) RETURN
      ENDIF

!       --- setup a vertex list

      IF (OPTG.EQ.5) CALL SwanVertlist(COMPDA)

!       --- allocate AC1 in case of non-stationary situation or in case
!           of using the S&L scheme

      IF ( NSTATM.EQ.1 .AND. MXITNS.GT.1 .OR. PROPSC.EQ.3 ) THEN
         IF (.NOT.ALLOCATED(AC1)) THEN
            ALLOCATE(AC1(MDC,MSC,MCGRD),STAT=ISTAT)
         ELSE IF (SIZE(AC1).EQ.0) THEN
            DEALLOCATE(AC1)
            ALLOCATE(AC1(MDC,MSC,MCGRD),STAT=ISTAT)
         END IF
         IF ( ISTAT.NE.0 ) THEN
            CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
            CALL TXPBLA(CHARS(1),IF1,IL1)
            MSGSTR =&
            &'Allocation problem: array AC1 and return code is '//&
            &CHARS(1)(IF1:IL1)
            CALL MSGERR ( 4, MSGSTR )
            RETURN
         END IF
         AC1 = 0.
      ELSE
         IF(.NOT.ALLOCATED(AC1)) ALLOCATE(AC1(0,0,0))
      ENDIF

      IF (LEVERR.GT.MAXERR) THEN

         WRITE (PRINTF, "(' ** No start of computation because of error level:' ,I3)") LEVERR
         IF (LEVERR.LT.4) WRITE (PRINTF, "(' ** To ignore this error, change [maxerr] with the', ' SET command')")

      ELSE

         IF (ITEST.GE.40) THEN
            IF (NSTATC.EQ.1) THEN
               WRITE (PRINTF, '(" Type of computation: dynamic")')
            ELSE
               IF (ONED) THEN
                  WRITE (PRINTF, '(" Type of computation: static 1-D")')
               ELSE
                  WRITE (PRINTF, '(" Type of computation: static 2-D")')
               ENDIF
            ENDIF
         ENDIF

         IF (NSTATC.EQ.1) THEN
            IT0 = 0
            IF (ICOND.EQ.1) THEN

!             --- compute default initial conditions
!
!TIMG               CALL SWTSTA(6)
               CALL SWINCO ( AC2   , COMPDA, XCGRID, YCGRID,&
               &KGRPNT, SPCDIR, SPCSIG, XYTST )
!TIMG               CALL SWTSTO(6)
!
!             --- reset ICOND to prevent second computation of
!                 initial condition
               ICOND = 0

            ENDIF
         ELSE
            IT0 = 1
         ENDIF

!         --- synchronize nodes

         CALL SWSYNC
         IF (STPNOW()) RETURN

!         --- loop over time steps

         do IT = IT0, MTC

            IF (LEVERR.GT.MAXERR) THEN
               WRITE (PRINTF, "(' ** No continuation of computation because ', 'of error level:',I3)") LEVERR
               IF (LEVERR.LT.4) WRITE (PRINTF, "(' ** To ignore this error, change [maxerr] with the', ' SET command')")
               EXIT
            ENDIF

!           --- synchronize nodes

            CALL SWSYNC
            IF (STPNOW()) RETURN

!           --- update boundary conditions and input fields
!
!TIMG            CALL SWTSTA(7)
            CALL SNEXTI ( BSPECS, BGRIDP, COMPDA, AC1   , AC2   ,&
            &SPCSIG, SPCDIR, XCGRID, YCGRID, KGRPNT,&
            &XYTST , DEPTH , WLEVL , FRIC  , UXB   ,&
            &UYB   , NPLAF , TURBF , MUDLF , WXI   ,&
            &AICEF , HICEF , HSSF  , TSSF  , DSSF  ,&
            &WYI   )
!TIMG            CALL SWTSTO(7)
            IF (STPNOW()) RETURN

!           --- initialize quasi-coherent modelling framework
!               note: data of incident spectrum is required

            IF ( IQCM.NE.0 .AND. IT.EQ.IT0 )&
            &CALL SWQCINIT ( BGRIDP, COMPDA )
            IF (STPNOW()) RETURN

!           --- synchronize nodes

            CALL SWSYNC
            IF (STPNOW()) RETURN

            IF (COMPUT.NE.'NOCO' .AND. IT.GT.0) THEN

               SAVITE = ITEST
               IF (ICOTES .GT. ITEST) ITEST = ICOTES

!             --- compute action density for current time step
!
!TIMG               CALL SWTSTA(8)
               IF (OPTG.NE.5) THEN
!                structured grid
                  CALL SWCOMP( AC1   , AC2   , COMPDA, SPCDIR, SPCSIG,&
                  &XYTST , IT    , KGRPNT, XCGRID, YCGRID,&
                  &CROSS , DIFFRACTION, TRIADS, SNL4, SPECTRAL_POWERS,&
                  &THREAD_WORKSPACES )
               ELSE
!                unstructured grid
                  CALL SwanCompUnstruc ( AC2   , AC1   , COMPDA,&
                  &SPCSIG, SPCDIR, XYTST ,&
                  &CROSS , IT    , DIFFRACTION, TRIADS, SNL4,&
                  &SPECTRAL_POWERS, THREAD_WORKSPACES )
               ENDIF
!TIMG               CALL SWTSTO(8)
               IF (STPNOW()) RETURN

!             --- set ICOND=4 for stationary computation, for next
!                 (stationary) COMPUTE command
               ICOND = 4

!             --- check whether computed significant wave height at
!                 boundary differs from prescribed value given in
!                 boundary command values of incident Hs

               IF ( BNDCHK ) THEN
                  CALL HSOBND ( AC2, SPCSIG, COMPDA(1,JHSIBC), KGRPNT )
               ENDIF

!             --- compute surfbeat based on IEM, if appropriate

               IF ( LSRFB .AND. ntf.LT.0 ) THEN

                  CALL SwanIEMmeanwav ( AC2, COMPDA(1,JHSIBC), SPCSIG,&
                  &KGRPNT, COMPDA(1,JHS) )
                  CALL SwanIEMncalc
                  IF ( STPNOW() ) RETURN
                  CALL SwanIEMsrfbeat ( COMPDA(1,JHS ), AC2,&
                  &COMPDA(1,JDP2),&
                  &SPCDIR, SPCSIG, KGRPNT )

               ENDIF

               ITEST = SAVITE

            ENDIF

            IF ( IT.EQ.IT0 .AND. .NOT.ALLOCATED(OURQT) ) THEN
               ALLOCATE (OURQT(MAX_OUTP_REQ))
               OURQT = -9999.
            ENDIF

            SAVITE = ITEST
            IF (IOUTES .GT. ITEST) ITEST = IOUTES

!           --- synchronize nodes

            CALL SWSYNC
            IF (STPNOW()) RETURN

!           --- carry out the output requests
!
!TIMG            CALL SWTSTA(9)
            CALL SWOUTP ( AC2   , SPCSIG, SPCDIR, COMPDA, XYTST ,&
            &KGRPNT, XCGRID, YCGRID, OURQT , DIFFRACTION )
!TIMG            CALL SWTSTO(9)
            IF (STPNOW()) RETURN

            IF (ERRPTS.GT.0) REWIND(ERRPTS)
            ITEST = SAVITE

!           --- update time

            IF (NSTATC.EQ.1) THEN
               IF (IT.LT.MTC) THEN
                  default_time_context%TIMCO = default_time_context%TIMCO + default_time_context%DT
                  CHTIME = DTTIWR(ITMOPT, default_time_context%TIMCO)
                  WRITE (PRINTF, "(' Time of computation -> ',A,' in sec:', F12.0)") CHTIME, default_time_context%TIMCO
               ENDIF
            ENDIF

         end do

         IF (LEVERR.GT.MAXERR) EXIT main_loop

      END IF

   END DO main_loop

!TIMG   CALL SWTSTO(1)
!
!     finalize PVD collection files

   DO IRQ = 1, MAX_OUTP_REQ
      UPVD=UPVDF(IRQ)
      IF (UPVD.GT.0) THEN
         WRITE(UPVD,'(A)') TRIM(PVDLIN3)
         WRITE(UPVD,'(A)') TRIM(PVDLIN4)
      ENDIF
   ENDDO

   DO IUNIT=1,HIOPEN
      INQUIRE ( UNIT=IUNIT, OPENED=LOPEN, NAME=FILENM )
      IF (LOPEN.AND.IUNIT.NE.PRINTF.AND.&
      &FILENM.NE.'CONOUT$'.AND.FILENM.NE.'CONIN$') CLOSE(IUNIT)
   END DO

!     --- collect contents of individual process files for
!         output requests in case of parallel computation

   CALL SWSYNC
!TIMG   CALL SWTSTA(9)
   IF ( PARLL ) THEN
      IF (OPTG.NE.5) THEN
         ALLOCATE (BLKND(MXC*MYC))
         BLKND = REAL(INODE)
      ENDIF
      IF ( IAMMASTER ) THEN
         ALLOCATE(BLKNDC(MXCGL*MYCGL))
         BLKNDC = 0.
      END IF
      IF (OPTG.NE.5) THEN
         CALL SWCOLLECT ( BLKNDC, BLKND, .TRUE. )
         IF (STPNOW()) RETURN
      ELSE
!METIS         BLKNDC = REAL(ipown)
      ENDIF
      IF ( IAMMASTER ) THEN
         CALL SWCOLOUT ( OURQT, BLKNDC )
         DEALLOCATE(BLKNDC)
      END IF
   END IF
!TIMG   CALL SWTSTO(9)
!
!TIMG   CALL SWPRTI

   INQUIRE(UNIT=PRINTF,OPENED=LOPEN)
   IF (LOPEN) CLOSE(PRINTF)

!     --- deallocate all allocated arrays

   IF (ALLOCATED(AC1   )) DEALLOCATE(AC1   )
   IF (ALLOCATED(BGRIDP)) DEALLOCATE(BGRIDP)
   IF (ALLOCATED(BSPECS)) DEALLOCATE(BSPECS)
   IF (ALLOCATED(COMPDA)) DEALLOCATE(COMPDA)
   IF (ALLOCATED(CROSS )) DEALLOCATE(CROSS )
   IF (ALLOCATED(OURQT )) DEALLOCATE(OURQT )
   IF (ALLOCATED(BLKND )) DEALLOCATE(BLKND )
   CALL SWCLME (DIFFRACTION, TRIADS, SNL4, SPECTRAL_POWERS,&
   &THREAD_WORKSPACES)

   RETURN
!     end of subroutine SWMAIN
end subroutine SWMAIN
!************************************************************************
!                                                                      *
SUBROUTINE SWINIT (INERR, SNL4)
   USE swan_service_interfaces, ONLY: STPNOW, BUGFIX
   USE swan_ocean_pack_init, ONLY: OCPINI
!                                                                      *
!************************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_boundary_counters
   USE swan_run_mode
   USE swan_input_grids
   USE swan_input_field_files
   USE SWCOMM3
   USE swan_test_output
   USE swan_propagation_scheme
   USE swan_spherical_geometry
   USE swan_time, ONLY: default_time_context
   USE OUTP_DATA, ONLY: NREOQ, LOPS, LORQ, UPVDF
   USE M_GENARR, ONLY: XYTST, DEPTH, FRIC, UXB, UYB, WXI, WYI, WLEVL,&
   &ASTDF, MUDLF, NPLAF, TURBF, AICEF, HICEF, LAYH, VEGDIL, VEGDRL,&
   &VEGNSL, HSSF, TSSF, DSSF
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanGriddata
   USE SwanIEM, only: sflog
   USE SwanQCM
   CHARACTER(LEN=36) :: FBCL
   CHARACTER(LEN=36) :: FBCR
   CHARACTER(LEN=36) :: FNEST
   CHARACTER(LEN=6) :: UAP
   CHARACTER(LEN=6) :: UDI
   CHARACTER(LEN=6) :: UDL
   CHARACTER(LEN=6) :: UET
   CHARACTER(LEN=6) :: UH
   CHARACTER(LEN=6) :: UL
   CHARACTER(LEN=6) :: UV
   INTEGER :: JSTP
   INTEGER :: ICOMP
   INTEGER :: IDIF
   INTEGER :: IINC
   INTEGER :: NCOR

   TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4


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
!     30.60: Nico Booij
!     30.62: IJsbrand Haagsma
!     30.72: IJsbrand Haagsma
!     30.80: Nico Booij
!     32.01: Roeland Ris & Cor van der Schelde
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     32.06: Roeland Ris
!     33.08: Nico Booij and Erick Rogers (changes re: the S&L scheme)
!     33.09: Nico Booij (changes re: spherical coordinates)
!     33.10: Nico Booij and Erick Rogers (changes re: the SORDUP scheme)
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.13: Nico Booij
!     40.14: Annette Kieftenburg
!     40.16: IJsbrand Haagsma
!     40.17: IJsbrand Haagsma
!     40.21: Agnieszka Herman
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.61: Marcel Zijlema
!     40.64: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.13: Nico Booij
!     41.62: Andre van der Westhuysen
!     41.75: Erick Rogers
!
!  1. Updates
!
!     10.09, Aug. 94: PER now absolute period, RPER relative period
!     10.10, Aug. 94: arrays NE and NED added (subarrays of OUTDA)
!     20.62, Oct. 95: argument of DPBLDP made variable
!     30.60, July 97: initialisation of array EXCVAL
!     30.60, Aug. 97: initialisation of MCGRD
!     30.62, Aug. 97: initialisation of PSURF(3) (gamd=1. for HISWA)
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.72, Nov. 97: updated units in OVUNIT
!     30.72, Jan. 98: made default values for quadruplets and PNUMS(20)
!                     (=GRWMX) according the command GEN3 KOM
!     32.01, Jan. 98: Initialised BNAUT, BNDCHK and HSRERR
!     32.02, Jan. 98: Initialised output variable 'Setup', LSETUP, JSETUP,
!                     JDPSAV and ONED
!     32.01, Jan. 98: added pointers in the POOL for auxiliary arrays
!                     JAUX(5:7)
!     30.72, Mar. 98: Initialisation for UNDFLW added
!     30.70, Mar. 98: pool array CROSS initialized as data array (not pointer)
!     40.00, June 98: data for nonstat. boundary conditions initialised
!                     STATUS is renamed IERR, because STATUS is reserved word
!            Feb. 99: IDYNCU etc. removed; DYNDEP initialized
!     30.80, Nov. 98: Provision for limitation on Ctheta (refraction)
!     34.01, Feb. 99: Introducing STPNOW
!     33.08, July 98: minor changes related to the S&L scheme
!     32.06, June 99: Initialisation of IGEN
!     30.82, July 99: Initialisation of ITERMX changed from 6 to 15
!     30.80, Aug. 99: Ursell number init. as 0.
!     30.82, Aug. 99: Assigned values to PNUMS(15) and PNUMS(16). They indicate the
!                     allowed global errors in the iteration procedure
!     30.82, Aug. 99: Initialisation of CSETUP
!     33.10, Jan. 00: minor changes related to the SORDUP scheme
!     40.02, Sep. 00: IREFR default set to 1 (no limiter activated)
!     40.14, Jan. 01: JASTD1 removed (is not used in COMPDA array)
!     40.13, Jan. 01: COSPG is initialized at 1. (corresponding to ALPG)
!                     NUMOBS initialized
!                     subarray sequence numbers in array COMPDA changed
!     40.16, Dec. 01: Implemented limiter switches
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.21, Aug. 01: diffraction approximation added
!     40.23, Aug. 02: under-relaxation factor added
!     40.23, Sep. 02: coefficient PTRIAD(5) added
!     40.23, Nov. 02: parameter PROPFL added
!     40.23, Dec. 02: reset of some default variables
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.08, Mar. 03: Unneccessary variable deleted
!     40.31, Nov. 03: removing POOL construction and HPGL functionality
!     40.35, Jun. 04: output variables DISTUR and TURB added
!     40.41, Sep. 04: output variables TMM10 and RTMM10 added
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: output variable TMBOT added
!     40.51, Sep. 05: output variables WATLEV and BOTLEV added
!     40.51, Feb. 06: output variable TPS added
!     40.61, Sep. 06: output variables DISBOT, DISSRF and DISWCP added
!     40.61, Sep. 06: output variable DISMUD added
!     40.61, Sep. 06: output variable DISVEG added
!     40.64, Apr. 07: output variables Qp and BFI added
!     40.80, Jun. 07: extension to unstructured grids
!     41.12, Apr. 10: output quantity NPL added
!     41.13, Jul. 10: LWDATE introduced in view of nesting in WAM
!     41.62, Nov. 15: included output quantities for wave partitioning
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Initialize several variables and arrays
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     INERR : Number of the initialisation error

!   The version number is only needed to render VERTXT, so it is a named
!   constant here instead of a mutable module variable.
    REAL, PARAMETER :: SWAN_VERSION_NUMBER = 41.51
   INTEGER :: INERR, IGRID, IVT, IVTYPE, MXOUTAR

!  6. Local variables
!
!  7. Common blocks used
!
!
!  8. Subroutines used


!  9. Subroutines calling
!
!     SWREAD
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
!     ----------------------------------------------------------------
!     Call OCPINI to initialize installation dependent constants
!     Call VERSION to get valid version number
!     Give unit references initial value
!     Write heading above echo of input
!     Give common variables initial value
!     ----------------------------------------------------------------
!
! 13. Source text

   VERTXT = default_command_reader%BLANK
   WRITE (VERTXT, '(F5.2)') SWAN_VERSION_NUMBER
   CALL BUGFIX ('A')
   CALL BUGFIX ('B')

   CALL OCPINI ('swaninit', .TRUE.,INERR)
   IF (INERR.GT.0) RETURN
   IF (STPNOW()) RETURN

   WRITE (PRINTF, "(/,20X,'---------------------------------------', /,20X,' SWAN', /,20X,'SIMULATION OF WAVES IN NEAR SHORE AREAS', /,20X,' VERSION NUMBER ', A, /,20X,'---------------------------------------',//)") VERTXT

   IF (SCREEN.NE.PRINTF.AND.IAMMASTER) WRITE (SCREEN,"(/, ' SWAN is preparing computation',/)")

!     ***** initial values for common variables *****
!     ***** names *****
   PROJID = 'SWAN'
   PROJNR = default_command_reader%BLANK
   PROJT1 = default_command_reader%BLANK
   PROJT2 = default_command_reader%BLANK
   PROJT3 = default_command_reader%BLANK
   FNEST  = default_command_reader%BLANK
   FBCR   = default_command_reader%BLANK
   FBCL   = default_command_reader%BLANK
   UH     = 'm'
   UV     = 'm/s'
   UT     = 'sec'
   UL     = 'm'
   UET    = 'm3/s'
   UDI    = 'degr'
   UST    = 'm2/s2'
   UF     = 'N/m2'
   UP     = 'W/m'
   UAP    = 'W/m2'
   UDL    = 'm2/s'
!     ***** physical parameters *****
   GRAV   = 9.81
   WLEV   = 0.
   CASTD  = 0.               ! const. air-sea temp diff
   CDCAP  = 99999.
   USCAP  = 99999.
   PI     = 4.*ATAN(1.)
   PI2    = 2.*PI
   DNORTH = 90.
   DEGRAD = PI/180.
   RHO    = 1025.
!     power of tail in spectrum, 1: E with f, 2: E with k,
!                                3: A with f, 4: A with k
   PWTAIL(1) = 4.
   PWTAIL(2) = 2.5
   PWTAIL(3) = PWTAIL(1)+1.
   PWTAIL(4) = 3.
!     ***** number of computational grid points ****
   MCGRD   = 1
   MCGRDGL = 1
   NGRBND  = 0
   NGRBGL  = 0
   nverts  = 0
   ncells  = 0
   nfaces  = 0
!     time of computation
   default_time_context%TIMCO = -1.E10
   CHTIME = '    '
!     boundary conditions
   NBFILS = 0
   NBSPEC = 0
   NBGRPT = 0
   NBGGL  = 0
   FSHAPE = 2
   DSHAPE = 2
   PSHAPE(1) = 3.3
   PSHAPE(2) = 0.1
   ALOBND = .FALSE.
!     ***** input grids *****
   do IGRID = 1, NUMGRD
      XPG(IGRID)    = 0.
      YPG(IGRID)    = 0.
      ALPG(IGRID)   = 0.
      COSPG(IGRID)  = 1.
      SINPG(IGRID)  = 0.
      DXG(IGRID)    = 0.
      DYG(IGRID)    = 0.
      MXG(IGRID)    = 0
      MYG(IGRID)    = 0
      LEDS(IGRID)   = 0
      STAGX(IGRID)  = 0.
      STAGY(IGRID)  = 0.
      EXCFLD(IGRID) = -1.E20
      IFLDYN(IGRID) = 0
      IFLTIM(IGRID) = -1.E20
   end do
!     Each input field is only allocated once a READINP command supplies it,
!     but the arrays are passed on unconditionally; whether a field exists is
!     decided by LEDS above, never by ALLOCATED. A deck that leaves one out
!     therefore passed an unallocated allocatable as an actual argument, which
!     is invalid and which -fcheck=all stops on. Give them all the empty state,
!     so that "not read" means allocated with size zero rather than undefined.
   IF (.NOT.ALLOCATED(DEPTH )) ALLOCATE(DEPTH (0))
   IF (.NOT.ALLOCATED(FRIC  )) ALLOCATE(FRIC  (0))
   IF (.NOT.ALLOCATED(UXB   )) ALLOCATE(UXB   (0))
   IF (.NOT.ALLOCATED(UYB   )) ALLOCATE(UYB   (0))
   IF (.NOT.ALLOCATED(WXI   )) ALLOCATE(WXI   (0))
   IF (.NOT.ALLOCATED(WYI   )) ALLOCATE(WYI   (0))
   IF (.NOT.ALLOCATED(WLEVL )) ALLOCATE(WLEVL (0))
   IF (.NOT.ALLOCATED(ASTDF )) ALLOCATE(ASTDF (0))
   IF (.NOT.ALLOCATED(MUDLF )) ALLOCATE(MUDLF (0))
   IF (.NOT.ALLOCATED(NPLAF )) ALLOCATE(NPLAF (0))
   IF (.NOT.ALLOCATED(TURBF )) ALLOCATE(TURBF (0))
   IF (.NOT.ALLOCATED(AICEF )) ALLOCATE(AICEF (0))
   IF (.NOT.ALLOCATED(HICEF )) ALLOCATE(HICEF (0))
   IF (.NOT.ALLOCATED(LAYH  )) ALLOCATE(LAYH  (0))
   IF (.NOT.ALLOCATED(VEGDIL)) ALLOCATE(VEGDIL(0))
   IF (.NOT.ALLOCATED(VEGDRL)) ALLOCATE(VEGDRL(0))
   IF (.NOT.ALLOCATED(VEGNSL)) ALLOCATE(VEGNSL(0))
   IF (.NOT.ALLOCATED(HSSF  )) ALLOCATE(HSSF  (0))
   IF (.NOT.ALLOCATED(TSSF  )) ALLOCATE(TSSF  (0))
   IF (.NOT.ALLOCATED(DSSF  )) ALLOCATE(DSSF  (0))
!     The same holds for the global grid arrays in M_PARALL: CGINIT fills them
!     for a structured grid, an unstructured run never does, and both kinds
!     pass them on to SWBOUN and the mesh routines.
   IF (.NOT.ALLOCATED(XGRDGL)) ALLOCATE(XGRDGL(0,0))
   IF (.NOT.ALLOCATED(YGRDGL)) ALLOCATE(YGRDGL(0,0))
   IF (.NOT.ALLOCATED(KGRPGL)) ALLOCATE(KGRPGL(0,0))
   IF (.NOT.ALLOCATED(KGRBGL)) ALLOCATE(KGRBGL(0))
!     ***** computational grid *****
   OPTG   = 1
   MXC    = 0
   MYC    = 0
   MXCGL  = 0
   MYCGL  = 0
   MXF    = 1
   MXL    = 0
   MYF    = 1
   MYL    = 0
   MSC    = 0
   MDC    = 0
   MTC    = 1
   ICOMP  = 1
   ALPC   = 0.
   FULCIR = .TRUE.
   SPDIR1 = 0.
   excmark = 999
   asort  = -999.
   usort  = -999.
   nsweep = -999
   CCURV  = .FALSE.
!     number of points needed in computational stencil:
   ICMAX  = 5
!     ***** numerical scheme *****
   NCOR   = 1
   NSTATM = -1
   NSTATC = -1
   NCOMPT = 0

!     initialise number of iterations stationary and nonstationary
   MXITST = 50
   MXITNS = 1
   ITERMX = MXITST
   ICUR   = 0
   IDIF   = 0
   IINC   = 0

!     --- meaning IREFR:
!         IREFR = -1: limiter on Ctheta activated
!         IREFR =  1: No limiter on Ctheta
!         IREFR =  0: No refraction

   IREFR  = 1
   ITFRE  = 1
   IWIND  = 0
!     when coupled with ADCIRC, the default is to use ADCIRC drag formulation
   IDRAG  = 1
   IGEN   = 3
   IQUAD  = 2
   IWCAP  = 7
   ISURF  = 1
   IBOT   = 0
   ITRIAD = 0
   IBIPH  = 0
   IMUD   = 0
   IVEG   = 0
   ITURBV = 0
   IICE   = 0
   ICEWIND= 0.
   IBRAG  = 0
   IQCM   = 0
   VARWI  = .FALSE.
   VARFR  = .FALSE.
   VARWLV = .FALSE.
   VARAST = .FALSE.       ! True means spatially variable air-sea t.d
   VARMUD = .FALSE.
   VARAICE= .FALSE.
   VARHICE= .FALSE.
   VARNPL = .FALSE.
   VARTUR = .FALSE.
   VARHSS = .FALSE.
   VARTSS = .FALSE.
   VARDSS = .FALSE.
   U10    = 0.
   WDIP   = 0.
   INRHOG = 0
   DEPMIN = 0.05
   SY0    = 3.3
   SIGMAG = 0.1
   XOFFS  = 0.
   YOFFS  = 0.
   LXOFFS = .FALSE.
   DYNDEP = .FALSE.
   LWDATE = 0
   MXOUTAR = 0

   FBS%NBS = -999
   LOPS    = .FALSE.
   LORQ    = .FALSE.

!     Set the defaults for the MDIA:

   SNL4%quadruplet_count = 6
   ALLOCATE(SNL4%lambda(SNL4%quadruplet_count),&
   &SNL4%coefficient_1(SNL4%quadruplet_count),&
   &SNL4%coefficient_2(SNL4%quadruplet_count))
   SNL4%lambda = (/0.08,0.09,0.11,0.15,0.16,0.29/)
   SNL4%coefficient_1 = (/8.77,-13.82,10.02,-15.92,14.41,0.65/)
   SNL4%coefficient_2 = SNL4%coefficient_1
   SNL4%coefficient_1 = SNL4%coefficient_1 * ((2.*PI)**9)
   SNL4%coefficient_2 = SNL4%coefficient_2 * ((2.*PI)**9)

!     *** Initial conditions ***
   ICOND = 0

   BNAUT  = .FALSE.
   BNDCHK = .TRUE.
   BRESCL = .TRUE.
   ONED   = .FALSE.
   ACUPDA = .TRUE.
   OFFSRC = .FALSE.
   LADDS  = .FALSE.
   LSPNAR = .FALSE.
   HSRERR = 0.1

!     higher order propagation and spherical coordinates

   PROJ_METHOD = 0
   PROPSS = 2
   PROPSN = 3
   PROPSC = 1
   PROPSL = 1
   PROPFL = 0
   WAVAGE = 0.
   KSPHER = 0
   KREPTX = 0
   REARTH = 2.E7/PI
   LENDEG = 2.E7/180.

!     *** setup flag ***
   LSETUP = 0

!     *** flag for setup convergence

   CSETUP = .TRUE.

!     PSETUP(1) is currently unused, but can be used as setup nesting flag
!     PSETUP(2) is the user defined correction for the level of the setup

   PSETUP(1) = 0.0
   PSETUP(2) = 0.0

!     flag for frequency dependent surf breaking

   IFRSRF = 0

!     flag for wave directionality in surf breaking

   IDISRF = 0

!     surfbeat model

   LSRFB = .FALSE.
   sflog = .FALSE.

!     *** ACCURACY criterion ***
!
!     *** relative error in significant wave height and mean period ***
   PNUMS(1)  = 0.01
!     *** absolute error in significant wave heigth (m) ***
   PNUMS(2)  = -1.
!     *** absolute error in mean wave period (s) ***
!      PNUMS(3)  = 0.3
   PNUMS(3)  = 1000.
!     *** total number of wet gridpoints were accuracy has ***
!     *** been reached                                     ***
   PNUMS(4)  = 99.50

!     *** DIFFUSION schemes ***
!
!     *** Numerical diffusion over theta ***
   PNUMS(6)  = 0.5
!     *** Numerical diffusion over sigma ***
   PNUMS(7)  = 0.5
!     *** Explicit or implicit scheme in frequency space ***
!     *** default = implicit : PNUMS(8) = 1              ***
   PNUMS(8) = -999.
!     *** diffusion coefficient for explicit scheme ***
   PNUMS(9) = 0.01

!     *** parameters for the SIP solver                        ***
!
!     *** Required accuracy to terminate the solver            ***
!     ***                                                      ***
!     ***  || Ax-b ||  <  eps2 * || b ||                       ***
!     ***                                                      ***
!     ***  eps2 = PNUMS(12)                                    ***
!
!     *** PNUMS(13) output for the solver. Possible values:    ***
!     ***     <0  : no output                                  ***
!     ***      0  : only fatal errors will be printed          ***
!     ***      1  : additional information about the iteration ***
!     ***           is printed                                 ***
!     ***      2  : gives a maximal amount of output           ***
!     ***           concerning the iteration process           ***
!
!     *** PNUMS(14) : maximum number of iterations             ***

   PNUMS(12) = 1.E-4
   PNUMS(13) = 0.
   PNUMS(14) = 20.

!     For the setup calculation, next parameters for the solver are used:
!
!     PNUMS(23) : required accuracy to terminate the solver
!     PNUMS(24) : output for the solver (see PNUMS(13) for meanings)
!     PNUMS(25) : maximum number of iterations

   PNUMS(23) = 1.E-6
   PNUMS(24) = 0.
   PNUMS(25) = 1000.

!     Maximum growth in spectral bin
!     The value is the default in the command GEN3 KOM

   PNUMS(20) = 0.1

!     Added coefficient for use with limiter on action (Qb switch)

   PNUMS(28) = 1.

!     *** set the values of PNUMS that are not used equal 0. ***

   PNUMS(5)  = 0.

!     The allowed global errors in the iteration procedure:
!     PNUMS(15) for Hs and PNUMS(16) for Tm01
!
!      PNUMS(15) = 0.02
!      PNUMS(16) = 0.02
!     The next two values are meant for STOPC command
   PNUMS(15) = 0.005
   PNUMS(16) = 1000.

!     coefficient for limitation of Ctheta
!     default no limitation on refraction

   PNUMS(17) = -999.

!     Limitation on Froude number; current velocity is reduced if greater
!     than Pnums(18)*Sqrt(grav*depth)

   PNUMS(18) = 0.8

!     *** CFL criterion for explicit scheme in frequency space ***

   PNUMS(19) = 0.5 * sqrt (2.)

!     --- coefficient for type stopping criterion

   PNUMS(21) = 1.

!     --- under-relaxation factor

   PNUMS(30) = 0.00

!     --- parameters for limiting Ctheta

   PNUMS(26) = 0.2
   PNUMS(27) = 2.0
   PNUMS(29) = 0.0

!     --- computation of Ctheta based on wave number

   PNUMS(32) = 1.

!     --- parameters for limiting Csigma and Ctheta

   PNUMS(33) = 0.0
   PNUMS(34) = 0.9
   PNUMS(35) = 0.0
   PNUMS(36) = 0.9

!     --- parameter for BKD surf breaking

   PNUMS(37) = 95.

!     *** (1) and (2): Komen et al. (1984) formulation ***

   PWCAP(1)  = 2.36E-5
   PWCAP(2)  = 3.02E-3
   PWCAP(9)  = 2.
!     note that delta has been set to 1 since version 40.91A
   PWCAP(10) = 1.
   PWCAP(11) = 1.

!     *** (3): Coefficient for Janssen(1989,1991) formulation ***
!     ** according to Komen et al. (1994) ***

   PWCAP(3)  = 4.5
   PWCAP(4)  = 0.5

!     *** (5): Coefficient for Longuet-Higgins ***

   PWCAP(5) = 1.

!     *** (6): ALPHA in Battjes/Janssen ***

   PWCAP(6) = 0.88
   PWCAP(7) = 1.
   PWCAP(8) = 0.75

!     *** Alves and Banner formulation ***

   PWCAP(12) = 1.75E-3

!     flag for current-induced wave dissipation

   IWCCUR = 0

!     *** (14): coefficient for enhanced current-induced dissipation

   PWCAP(14) = 0.8

!       parameters to be used for Babanin physics and swell
   A1SDS       = 2.8E-6
   A2SDS       = 3.5E-5
   P1SDS       = 4.
   P2SDS       = 4.
   UPWARDS     = .TRUE.
   FESWELL     = 0.
   CDSV        = 1.2
   RDCOEF      = 0.
   WNDSCL      = 32.     ! see notes in SdsBabanin.f90
   CDFAC       = 1.      ! factor on Cdrag to counter bias in winds
   B1Z         = 0.00025
   ! Ref: B1Z=0.0014 was from Young et al. (2013)
   !      B1Z=0.00025 is from Zieger et al. (2015)
   ROGERS      = .FALSE.
   ARDHUIN     = .TRUE.
   ZIEGER      = .FALSE.
   FPI         = 1.

   PBOT(1)   = 0.0
   PBOT(2)   = 0.015
   PBOT(3)   = 0.038
   PBOT(4)   = -0.08
   PBOT(5)   = 0.05
   PBOT(6)   = 2.65
   PBOT(7)   = 0.0001

   PSURF(1)  = 1.0
   PSURF(2)  = 0.73

   PMUD(1)   = 0.
   PMUD(2)   = 1300.
   PMUD(3)   = 0.0076
   PMUD(4)   = RHO
   PMUD(5)   = 1.3E-6

!     parameters for Bragg scattering
   PBRAG = 0.
   PBRAG(2) = 5.

!     parameters for QC scattering
   PSCAT = 0.
   PSCAT(1) = 1.
   PSCAT(2) = 99999.
   PSCAT(7) = 1.

!     work arrays for QC scattering
   ncoz   = 0
   lenwft = 0
   lensav = 0

!     wave number space is initially void
   mkxc = -1
   mkyc = -1

!     parameters for dissipation by sea ice: all values (1:8) set to zero
   PSICE    = 0.
!     parameters for uniform ice fields all values (1:2) set to zero
   PICE    = 0.

!     triad interactions
   PTRIAD(1)  = 1.0
   PTRIAD(2)  = 2.5
   PTRIAD(3)  = 10.
   PTRIAD(4)  = 0.63
   PTRIAD(5)  = 0.1
   PTRIAD(6)  = 0.95
!     original value of B=-0.75 in SPB appears to be reasonable
!     for unidirectional laboratory cases
!     however, this choice may not be appropriate for true
!     2D cases as it does not scale with the wave field
!     thus, B = 0 (for now)
!      PTRIAD(7)  = -0.75
   PTRIAD(7)  = 0.
   PTRIAD(8)  = 60.
   PTRIAD(10) = -1.

!     quadruplet interactions
   PQUAD(1) = 0.25
   PQUAD(2) = 3.E7
   PQUAD(3) = 5.5
   PQUAD(4) = 0.833
   PQUAD(5) = -1.25

   PWIND(1)  = 188.0
   PWIND(2)  = 0.59
   PWIND(3)  = 0.12
   PWIND(4)  = 250.0
   PWIND(5)  = 0.0023
   PWIND(6)  = -0.223
   PWIND(7)  = 0.
   PWIND(8)  = -0.56
   PWIND(10) = 0.0036
   PWIND(11) = 0.00123
   PWIND(12) = 1.0
   PWIND(13) = 0.13
!     *** Janssen (1991) wave growth model ***
!     *** alpha ***
   PWIND(14) = 0.01
!      PWIND(14) = 0.0144
!     *** Charnock: Von Karman constant ***
   PWIND(15) = 0.41
!     *** rho air (density) ****
   PWIND(16) = 1.28
!     *** rho water (density) ***
   PWIND(17) = RHO
   PWIND(9)  = PWIND(16) / RHO
!     Coefficient in front of A term in 3d gen. growth term
!     default is 0; can be made non-zero in command GEN3 or GROWTH
   PWIND(31) = 0.

!     for DBYB wind input term, decide whether to integrate stress as a
!     vector (true) or a scalar (false)
   VECTOR_TAU = .TRUE.
!     also for DBYB wind input term, decide whether to use U10 as given
!     in DBYB or use 28*ustar as a proxy
   TRUE_U10 = .FALSE.

!     --- coefficients for diffraction approximation

   IDIFFR    = 0
   PDIFFR(:) = 0.

!     pointers in array COMPDA
   JDISS = 2
   JUBOT = 3
   JQB   = 4
   JSTP  = 5
   JDHS  = 6
   JDP1  = 7
   JDP2  = 8
   JVX1  = 9
   JVY1  = 10
   JVX2  = 11
   JVY2  = 12
   JVX3  = 13
   JVY3  = 14
   JDP3  = 15
   JWX2  = 16
   JWY2  = 17
   JWX3  = 18
   JWY3  = 19
   JDTM  = 20
   JLEAK = 21
   JWLV1 = 22
   JWLV3 = 23
   JWLV2 = 24
   JHSIBC = 25
   JHS    = 26
   JURSEL = 27
   JBOTLV = 28
   JBIPH  = 29
   MCMVAR = 29
!     subarray sequence number 1 is used only for unused subarrays
   JFRC2 = 1
   JFRC3 = 1
   JUSTAR= 1
   JZEL  = 1
   JTAUW = 1
   JCDRAG= 1
!     added for air-sea temp. diff.:
   JASTD2= 1
   JASTD3= 1
!     added for fluid mud layer
   JMUDL1= 1
   JMUDL2= 1
   JMUDL3= 1
!     added for number of plants per square meter
   JNPLA2= 1
   JNPLA3= 1
!     added for turbulent viscosity
   JTURB2= 1
   JTURB3= 1
!     added for breaker index
   JGAMMA= 1

!     added for ice input fields
   JAICE2= 1
   JAICE3= 1
   JHICE2= 1
   JHICE3= 1

!     *** added for wave setup ***

   JSETUP = 1
   JDPSAV = 1

!     added for sea-swell wave input fields
   JHSS2= 1
   JHSS3= 1
   JTSS2= 1
   JTSS3= 1
   JDSS2= 1
   JDSS3= 1

!     --- added for output purposes

   JPBOT  = 1
   JDSXB  = 1
   JDSXS  = 1
   JDSXW  = 1
   JDSXM  = 1
   JDSXV  = 1
   JDSXT  = 1
   JDSXI  = 1
   JDSXL  = 1
   JGENR  = 1
   JGSXW  = 1
   JREDS  = 1
   JRSXQ  = 1
   JRSXT  = 1
   JRSXB  = 1
   JRSXC  = 1
   JTRAN  = 1
   JTSXG  = 1
   JTSXT  = 1
   JTSXS  = 1
   JRADS  = 1

!     Next pointers are for the plot of the source terms (SWTSDA)

   JPWNDS = 1
   JPWNDD = 2
   JPWCAP = 3
   JPBTFR = 4
   JPWBRK = 5
   JP4S   = 6
   JP4D   = 7
   JPTRI  = 8
   JPVEGT = 9
   JPTURB = 10
   JPMUD  = 11
   JPICE  = 13
   JPBRAG = 14
   JPQCS  = 15
   MTSVAR = 15
   JPSWEL = 1

!     ***** test output control *****
   ITEST  = 1
   INTES  = 0
   ICOTES = 0
   IOUTES = 0
   LTRACE = .FALSE.
   TESTFL = .FALSE.
   NPTST  = 0
   NPTSTA = 1
!     XYTST holds the test point indices belonging to NPTST. Only the TEST
!     command fills it, so a deck without one left it unallocated while it is
!     still passed on to SWBOUN and the computation routines. Establish the
!     empty state here, so that NPTST = 0 and an allocated XYTST always agree.
   IF (.NOT.ALLOCATED(XYTST)) ALLOCATE(XYTST(0))
   LXDMP  = -1
   LYDMP  = 0
   IFPAR = 0
   IFS1D = 0
   IFS2D = 0
!     number of obstacles initialised at 0
   NUMOBS = 0
!     ***** output *****
   IUBOTR = 0
   NREOQ  = 0
   UPVDF  = 0
!     ***** plot output *****

   DO IVT = 1, NMOVAR
      OVKEYW(IVT) = 'XXXX'
   ENDDO

!     properties of output variables

   IVTYPE = 1
!     keyword used in SWAN command
   OVKEYW(IVTYPE) = 'XP'
!     short name
   OVSNAM(IVTYPE) = 'Xp'
!     long name
   OVLNAM(IVTYPE) = 'X user coordinate'
!     unit name
   OVUNIT(IVTYPE) = UL
!     type (scalar/vector etc.)
   OVSVTY(IVTYPE) = 1
!     lower and upper limit
   OVLLIM(IVTYPE) = -1.E10
   OVULIM(IVTYPE) = 1.E10
!     lowest and highest expected value
   OVLEXP(IVTYPE) = -1.E10
   OVHEXP(IVTYPE) = 1.E10
!     exception value
   OVEXCV(IVTYPE) = -1.E10

   IVTYPE = 2
   OVKEYW(IVTYPE) = 'YP'
   OVSNAM(IVTYPE) = 'Yp'
   OVLNAM(IVTYPE) = 'Y user coordinate'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.E10
   OVULIM(IVTYPE) = 1.E10
   OVLEXP(IVTYPE) = -1.E10
   OVHEXP(IVTYPE) = 1.E10
   OVEXCV(IVTYPE) = -1.E10

   IVTYPE = 3
   OVKEYW(IVTYPE) = 'DIST'
   OVSNAM(IVTYPE) = 'Dist'
   OVLNAM(IVTYPE) = 'distance along output curve'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.E10
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.E10
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 4
   OVKEYW(IVTYPE) = 'DEP'
   OVSNAM(IVTYPE) = 'Depth'
   OVLNAM(IVTYPE) = 'Depth'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.E4
   OVULIM(IVTYPE) = 1.E4
   OVLEXP(IVTYPE) = -100.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 5
   OVKEYW(IVTYPE) = 'VEL'
   OVSNAM(IVTYPE) = 'Vel'
   OVLNAM(IVTYPE) = 'Current velocity'
   OVUNIT(IVTYPE) = UV
   OVSVTY(IVTYPE) = 3
   OVLLIM(IVTYPE) = -100.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = -2.
   OVHEXP(IVTYPE) = 2.
   OVEXCV(IVTYPE) = 0.

   IVTYPE = 6
   OVKEYW(IVTYPE) = 'UBOT'
   OVSNAM(IVTYPE) = 'Ubot'
   OVLNAM(IVTYPE)='RMS of maxima of orbital velocity near the bottom'
   OVUNIT(IVTYPE) = UV
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 10.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -10.

   IVTYPE = 7
   OVKEYW(IVTYPE) = 'DISS'
   OVSNAM(IVTYPE) = 'Dissip'
   OVLNAM(IVTYPE) = 'Energy dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 8
   OVKEYW(IVTYPE) = 'QB'
   OVSNAM(IVTYPE) = 'Qb'
   OVLNAM(IVTYPE) = 'Fraction breaking waves'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -1.

   IVTYPE = 9
   OVKEYW(IVTYPE) = 'LEA'
   OVSNAM(IVTYPE) = 'Leak'
   OVLNAM(IVTYPE) = 'Energy leak over spectral boundaries'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 10
   OVKEYW(IVTYPE) = 'HS'
   OVSNAM(IVTYPE) = 'Hsig'
   OVLNAM(IVTYPE) = 'Significant wave height'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 11
   OVKEYW(IVTYPE) = 'TM01'
   OVSNAM(IVTYPE) = 'Tm01'
   OVLNAM(IVTYPE) = 'Average absolute wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 12
   OVKEYW(IVTYPE) = 'RTP'
   OVSNAM(IVTYPE) = 'RTpeak'
   OVLNAM(IVTYPE) = 'Relative peak period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 13
   OVKEYW(IVTYPE) = 'DIR'
   OVSNAM(IVTYPE) = 'Dir'
   OVLNAM(IVTYPE) = 'Average wave direction'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 14
   OVKEYW(IVTYPE) = 'PDI'
   OVSNAM(IVTYPE) = 'PkDir'
   OVLNAM(IVTYPE) = 'direction of the peak of the spectrum'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 15
   OVKEYW(IVTYPE) = 'TDI'
   OVSNAM(IVTYPE) = 'TDir'
   OVLNAM(IVTYPE) = 'direction of the energy transport'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 16
   OVKEYW(IVTYPE) = 'DSPR'
   OVSNAM(IVTYPE) = 'Dspr'
   OVLNAM(IVTYPE) = 'directional spreading'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 17
   OVKEYW(IVTYPE) = 'WLEN'
   OVSNAM(IVTYPE) = 'Wlen'
   OVLNAM(IVTYPE) = 'Average wave length'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 18
   OVKEYW(IVTYPE) = 'STEE'
   OVSNAM(IVTYPE) = 'Steepn'
   OVLNAM(IVTYPE) = 'Wave steepness'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 19
   OVKEYW(IVTYPE) = 'TRA'
   OVSNAM(IVTYPE) = 'Transp'
   OVLNAM(IVTYPE) = 'Wave energy transport'
   OVUNIT(IVTYPE) = 'm3/s'
   OVSVTY(IVTYPE) = 3
   OVLLIM(IVTYPE) = -100.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = -10.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = 0.

   IVTYPE = 20
   OVKEYW(IVTYPE) = 'FOR'
   OVSNAM(IVTYPE) = 'WForce'
   OVLNAM(IVTYPE) = 'Wave driven force per unit surface'
   OVUNIT(IVTYPE) = UF
   OVSVTY(IVTYPE) = 3
   OVLLIM(IVTYPE) = -1.E5
   OVULIM(IVTYPE) =  1.E5
   OVLEXP(IVTYPE) = -10.
   OVHEXP(IVTYPE) =  10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 21
   OVKEYW(IVTYPE) = 'AAAA'
   OVSNAM(IVTYPE) = 'AcDens'
   OVLNAM(IVTYPE) = 'spectral action density'
   OVUNIT(IVTYPE) = 'm2s'
   OVSVTY(IVTYPE) = 5
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 22
   OVKEYW(IVTYPE) = 'EEEE'
   OVSNAM(IVTYPE) = 'EnDens'
   OVLNAM(IVTYPE) = 'spectral energy density'
   OVUNIT(IVTYPE) = 'm2'
   OVSVTY(IVTYPE) = 5
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 23
   OVKEYW(IVTYPE) = 'AAAA'
   OVSNAM(IVTYPE) = 'Aux'
   OVLNAM(IVTYPE) = 'auxiliary variable'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.E10
   OVULIM(IVTYPE) = 1.E10
   OVLEXP(IVTYPE) = -1.E10
   OVHEXP(IVTYPE) = 1.E10
   OVEXCV(IVTYPE) = -1.E10

   IVTYPE = 24
   OVKEYW(IVTYPE) = 'XC'
   OVSNAM(IVTYPE) = 'Xc'
   OVLNAM(IVTYPE) = 'X computational grid coordinate'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 25
   OVKEYW(IVTYPE) = 'YC'
   OVSNAM(IVTYPE) = 'Yc'
   OVLNAM(IVTYPE) = 'Y computational grid coordinate'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 26
   OVKEYW(IVTYPE) = 'WIND'
   OVSNAM(IVTYPE) = 'Windv'
   OVLNAM(IVTYPE) = 'Wind velocity at 10 m above sea level'
   OVUNIT(IVTYPE) = UV
   OVSVTY(IVTYPE) = 3
   OVLLIM(IVTYPE) = -100.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = -50.
   OVHEXP(IVTYPE) = 50.
   OVEXCV(IVTYPE) = 0.

   IVTYPE = 27
   OVKEYW(IVTYPE) = 'FRC'
   OVSNAM(IVTYPE) = 'FrCoef'
   OVLNAM(IVTYPE) = 'Bottom friction coefficient'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.
!                                                                     new 10.09
   IVTYPE = 28
   OVKEYW(IVTYPE) = 'RTM01'
   OVSNAM(IVTYPE) = 'RTm01'
   OVLNAM(IVTYPE) = 'Average relative wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 29
   OVKEYW(IVTYPE) = 'EEEE'
   OVSNAM(IVTYPE) = 'EnDens'
   OVLNAM(IVTYPE) = 'energy density integrated over direction'
   OVUNIT(IVTYPE) = 'm2'
   OVSVTY(IVTYPE) = 5
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 30
   OVKEYW(IVTYPE) = 'DHS'
   OVSNAM(IVTYPE) = 'dHs'
   OVLNAM(IVTYPE) = 'difference in Hs between iterations'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 31
   OVKEYW(IVTYPE) = 'DRTM01'
   OVSNAM(IVTYPE) = 'dTm'
   OVLNAM(IVTYPE) = 'difference in Tm between iterations'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 2.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 32
   OVKEYW(IVTYPE) = 'TM02'
   OVSNAM(IVTYPE) = 'Tm02'
   OVLNAM(IVTYPE) = 'Zero-crossing period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 33
   OVKEYW(IVTYPE) = 'FSPR'
   OVSNAM(IVTYPE) = 'FSpr'
   OVLNAM(IVTYPE) = 'Frequency spectral width (Kappa)'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 34
   OVKEYW(IVTYPE) = 'URMS'
   OVSNAM(IVTYPE) = 'Urms'
   OVLNAM(IVTYPE) = 'RMS of orbital velocity near the bottom'
   OVUNIT(IVTYPE) = UV
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 10.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 35
   OVKEYW(IVTYPE) = 'UFRI'
   OVSNAM(IVTYPE) = 'Ufric'
   OVLNAM(IVTYPE) = 'Friction velocity'
   OVUNIT(IVTYPE) = UV
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 10.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 36
   OVKEYW(IVTYPE) = 'ZLEN'
   OVSNAM(IVTYPE) = 'Zlen'
   OVLNAM(IVTYPE) = 'Zero velocity thickness of boundary layer'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 37
   OVKEYW(IVTYPE) = 'TAUW'
   OVSNAM(IVTYPE) = 'TauW'
   OVLNAM(IVTYPE) = '    '
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 10.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 38
   OVKEYW(IVTYPE) = 'CDRAG'
   OVSNAM(IVTYPE) = 'Cdrag'
   OVLNAM(IVTYPE) = 'Drag coefficient'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

!     *** wave-induced setup ***

   IVTYPE = 39
   OVKEYW(IVTYPE) = 'SETUP'
   OVSNAM(IVTYPE) = 'Setup'
   OVLNAM(IVTYPE) = 'Setup due to waves'
   OVUNIT(IVTYPE) = 'm'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = -1.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 40
   OVKEYW(IVTYPE) = 'TIME'
   OVSNAM(IVTYPE) = 'Time'
   OVLNAM(IVTYPE) = 'Date-time'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -99999.

   IVTYPE = 41
   OVKEYW(IVTYPE) = 'TSEC'
   OVSNAM(IVTYPE) = 'Tsec'
   OVLNAM(IVTYPE) = 'Time in seconds from reference time'
   OVUNIT(IVTYPE) = 's'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100000.
   OVLEXP(IVTYPE) = -100000.
   OVHEXP(IVTYPE) = 1000000.
   OVEXCV(IVTYPE) = -99999.
!                                                        new
   IVTYPE = 42
   OVKEYW(IVTYPE) = 'PER'
   OVSNAM(IVTYPE) = 'Period'
   OVLNAM(IVTYPE) = 'Average absolute wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.
!                                                        new
   IVTYPE = 43
   OVKEYW(IVTYPE) = 'RPER'
   OVSNAM(IVTYPE) = 'RPer'
   OVLNAM(IVTYPE) = 'Average relative wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 44
   OVKEYW(IVTYPE) = 'HSWE'
   OVSNAM(IVTYPE) = 'Hswell'
   OVLNAM(IVTYPE) = 'Wave height of swell part'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 45
   OVKEYW(IVTYPE) = 'URSELL'
   OVSNAM(IVTYPE) = 'Ursell'
   OVLNAM(IVTYPE) = 'Ursell number'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 46
   OVKEYW(IVTYPE) = 'ASTD'
   OVSNAM(IVTYPE) = 'ASTD'
   OVLNAM(IVTYPE) = 'Air-Sea temperature difference'
   OVUNIT(IVTYPE) = 'K'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -50.
   OVULIM(IVTYPE) =  50.
   OVLEXP(IVTYPE) = -10.
   OVHEXP(IVTYPE) =  10.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 47
   OVKEYW(IVTYPE) = 'TMM10'
   OVSNAM(IVTYPE) = 'Tm_10'
   OVLNAM(IVTYPE) = 'Average absolute wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 48
   OVKEYW(IVTYPE) = 'RTMM10'
   OVSNAM(IVTYPE) = 'RTm_10'
   OVLNAM(IVTYPE) = 'Average relative wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 49
   OVKEYW(IVTYPE) = 'DIFPAR'
   OVSNAM(IVTYPE) = 'DifPar'
   OVLNAM(IVTYPE) = 'Diffraction parameter'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -50.
   OVULIM(IVTYPE) =  50.
   OVLEXP(IVTYPE) = -10.
   OVHEXP(IVTYPE) =  10.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 50
   OVKEYW(IVTYPE) = 'TMBOT'
   OVSNAM(IVTYPE) = 'TmBot'
   OVLNAM(IVTYPE) = 'Near bottom wave period'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 51
   OVKEYW(IVTYPE) = 'WATL'
   OVSNAM(IVTYPE) = 'Watlev'
   OVLNAM(IVTYPE) = 'Water level'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.E4
   OVULIM(IVTYPE) = 1.E4
   OVLEXP(IVTYPE) = -100.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 52
   OVKEYW(IVTYPE) = 'BOTL'
   OVSNAM(IVTYPE) = 'Botlev'
   OVLNAM(IVTYPE) = 'Bottom level'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = -1.E4
   OVULIM(IVTYPE) = 1.E4
   OVLEXP(IVTYPE) = -100.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 53
   OVKEYW(IVTYPE) = 'TPS'
   OVSNAM(IVTYPE) = 'TPsmoo'
   OVLNAM(IVTYPE) = 'Relative peak period (smooth)'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 54
   OVKEYW(IVTYPE) = 'DISB'
   OVSNAM(IVTYPE) = 'Sfric'
   OVLNAM(IVTYPE) = 'Bottom friction dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 55
   OVKEYW(IVTYPE) = 'DISSU'
   OVSNAM(IVTYPE) = 'Ssurf'
   OVLNAM(IVTYPE) = 'Surf breaking dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 56
   OVKEYW(IVTYPE) = 'DISW'
   OVSNAM(IVTYPE) = 'Swcap'
   OVLNAM(IVTYPE) = 'Whitecapping dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 57
   OVKEYW(IVTYPE) = 'DISV'
   OVSNAM(IVTYPE) = 'Sveg'
   OVLNAM(IVTYPE) = 'Vegetation dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 58
   OVKEYW(IVTYPE) = 'QP'
   OVSNAM(IVTYPE) = 'Qp'
   OVLNAM(IVTYPE) = 'Peakedness'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 59
   OVKEYW(IVTYPE) = 'BFI'
   OVSNAM(IVTYPE) = 'BFI'
   OVLNAM(IVTYPE) = 'Benjamin-Feir index'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1000.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 60
   OVKEYW(IVTYPE) = 'GENE'
   OVSNAM(IVTYPE) = 'Genera'
   OVLNAM(IVTYPE) = 'Energy generation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 61
   OVKEYW(IVTYPE) = 'GENW'
   OVSNAM(IVTYPE) = 'Swind'
   OVLNAM(IVTYPE) = 'Wind source term'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 62
   OVKEYW(IVTYPE) = 'REDI'
   OVSNAM(IVTYPE) = 'Redist'
   OVLNAM(IVTYPE) = 'Energy redistribution'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 63
   OVKEYW(IVTYPE) = 'REDQ'
   OVSNAM(IVTYPE) = 'Snl4'
   OVLNAM(IVTYPE) = 'Total absolute 4-wave interaction'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 64
   OVKEYW(IVTYPE) = 'REDT'
   OVSNAM(IVTYPE) = 'Snl3'
   OVLNAM(IVTYPE) = 'Total absolute 3-wave interaction'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 65
   OVKEYW(IVTYPE) = 'PROPA'
   OVSNAM(IVTYPE) = 'Propag'
   OVLNAM(IVTYPE) = 'Energy propagation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 66
   OVKEYW(IVTYPE) = 'PROPX'
   OVSNAM(IVTYPE) = 'Propxy'
   OVLNAM(IVTYPE) = 'xy-propagation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 67
   OVKEYW(IVTYPE) = 'PROPT'
   OVSNAM(IVTYPE) = 'Propth'
   OVLNAM(IVTYPE) = 'theta-propagation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 68
   OVKEYW(IVTYPE) = 'PROPS'
   OVSNAM(IVTYPE) = 'Propsi'
   OVLNAM(IVTYPE) = 'sigma-propagation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 69
   OVKEYW(IVTYPE) = 'RADS'
   OVSNAM(IVTYPE) = 'Radstr'
   OVLNAM(IVTYPE) = 'Radiation stress'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 70
   OVKEYW(IVTYPE) = 'NPL'
   OVSNAM(IVTYPE) = 'Nplant'
   OVLNAM(IVTYPE) = 'Plants per m2'
   OVUNIT(IVTYPE) = '1/m2'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 71
   OVKEYW(IVTYPE) = 'LWAVP'
   OVSNAM(IVTYPE) = 'Lwavp'
   OVLNAM(IVTYPE) = 'Peak wave length'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 72
   OVKEYW(IVTYPE) = 'DISTU'
   OVSNAM(IVTYPE) = 'Stur'
   OVLNAM(IVTYPE) = 'Turbulent dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 73
   OVKEYW(IVTYPE) = 'TURB'
   OVSNAM(IVTYPE) = 'Turb'
   OVLNAM(IVTYPE) = 'Turbulent viscosity'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 10.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -99.

   IVTYPE = 74
   OVKEYW(IVTYPE) = 'DISM'
   OVSNAM(IVTYPE) = 'Smud'
   OVLNAM(IVTYPE) = 'Fluid mud dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 75
   OVKEYW(IVTYPE) = 'DISSW'
   OVSNAM(IVTYPE) = 'Sswell'
   OVLNAM(IVTYPE) = 'Swell dissipation'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 76
   OVKEYW(IVTYPE)  = 'DISI'
   OVSNAM(IVTYPE)  = 'Sice'
   OVLNAM(IVTYPE)  = 'Sea ice dissipation'
   OVUNIT(IVTYPE)  = 'm2/s'
   OVSVTY(IVTYPE)  = 1
   OVLLIM(IVTYPE)  = 0.
   OVULIM(IVTYPE)  = 1000.
   OVLEXP(IVTYPE)  = 0.
   OVHEXP(IVTYPE)  = 0.1
   OVEXCV(IVTYPE)  = -9.

   IVTYPE = 77
   OVKEYW(IVTYPE)  = 'AICE'
   OVSNAM(IVTYPE)  = 'aice'
   OVLNAM(IVTYPE)  = 'ice concentration (fraction)'
   OVUNIT(IVTYPE)  = ' '
   OVSVTY(IVTYPE)  = 1
   OVLLIM(IVTYPE)  = 0.
   OVULIM(IVTYPE)  = 1.0
   OVLEXP(IVTYPE)  = 0.
   OVHEXP(IVTYPE)  = 1.0
   OVEXCV(IVTYPE)  = -9.

   IVTYPE = 78
   OVKEYW(IVTYPE)  = 'HICE'
   OVSNAM(IVTYPE)  = 'hice'
   OVLNAM(IVTYPE)  = 'ice thickness'
   OVUNIT(IVTYPE)  = 'm'
   OVSVTY(IVTYPE)  = 1
   OVLLIM(IVTYPE)  = 0.
   OVULIM(IVTYPE)  = 100.
   OVLEXP(IVTYPE)  = 0.
   OVHEXP(IVTYPE)  = 10.
   OVEXCV(IVTYPE)  = -9.

   IVTYPE = 79
   OVKEYW(IVTYPE) = 'REDB'
   OVSNAM(IVTYPE) = 'Sbragg'
   OVLNAM(IVTYPE) = 'Total absolute Bragg scattering'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 80
   OVKEYW(IVTYPE) = 'REDC'
   OVSNAM(IVTYPE) = 'Sqc'
   OVLNAM(IVTYPE) = 'Total absolute QC scattering'
   OVUNIT(IVTYPE) = 'm2/s'
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 81
   OVKEYW(IVTYPE) = 'HBIG'
   OVSNAM(IVTYPE) = 'HBig'
   OVLNAM(IVTYPE) = 'Bound ig wave height'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 82
   OVKEYW(IVTYPE) = 'GAMMA'
   OVSNAM(IVTYPE) = 'Gamma'
   OVLNAM(IVTYPE) = 'Breaker index'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 83
   OVKEYW(IVTYPE) = 'BIPH'
   OVSNAM(IVTYPE) = 'Biph'
   OVLNAM(IVTYPE) = 'Biphase'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 100
   OVKEYW(IVTYPE) = 'PTHS'
   OVSNAM(IVTYPE) = 'HsPT01'
   OVLNAM(IVTYPE) = 'Wave height of partition 01'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 101
   OVKEYW(IVTYPE) = 'PT02HS'
   OVSNAM(IVTYPE) = 'HsPT02'
   OVLNAM(IVTYPE) = 'Wave height of partition 02'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 102
   OVKEYW(IVTYPE) = 'PT03HS'
   OVSNAM(IVTYPE) = 'HsPT03'
   OVLNAM(IVTYPE) = 'Wave height of partition 03'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 103
   OVKEYW(IVTYPE) = 'PT04HS'
   OVSNAM(IVTYPE) = 'HsPT04'
   OVLNAM(IVTYPE) = 'Wave height of partition 04'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 104
   OVKEYW(IVTYPE) = 'PT05HS'
   OVSNAM(IVTYPE) = 'HsPT05'
   OVLNAM(IVTYPE) = 'Wave height of partition 05'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 105
   OVKEYW(IVTYPE) = 'PT06HS'
   OVSNAM(IVTYPE) = 'HsPT06'
   OVLNAM(IVTYPE) = 'Wave height of partition 06'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 106
   OVKEYW(IVTYPE) = 'PT07HS'
   OVSNAM(IVTYPE) = 'HsPT07'
   OVLNAM(IVTYPE) = 'Wave height of partition 07'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 107
   OVKEYW(IVTYPE) = 'PT08HS'
   OVSNAM(IVTYPE) = 'HsPT08'
   OVLNAM(IVTYPE) = 'Wave height of partition 08'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 108
   OVKEYW(IVTYPE) = 'PT09HS'
   OVSNAM(IVTYPE) = 'HsPT09'
   OVLNAM(IVTYPE) = 'Wave height of partition 09'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 109
   OVKEYW(IVTYPE) = 'PT10HS'
   OVSNAM(IVTYPE) = 'HsPT10'
   OVLNAM(IVTYPE) = 'Wave height of partition 10'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 110
   OVKEYW(IVTYPE) = 'PTRTP'
   OVSNAM(IVTYPE) = 'TpPT01'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 01'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 111
   OVKEYW(IVTYPE) = 'PT02RTP'
   OVSNAM(IVTYPE) = 'TpPT02'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 02'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 112
   OVKEYW(IVTYPE) = 'PT03RTP'
   OVSNAM(IVTYPE) = 'TpPT03'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 03'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 113
   OVKEYW(IVTYPE) = 'PT04RTP'
   OVSNAM(IVTYPE) = 'TpPT04'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 04'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 114
   OVKEYW(IVTYPE) = 'PT05RTP'
   OVSNAM(IVTYPE) = 'TpPT05'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 05'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 115
   OVKEYW(IVTYPE) = 'PT06RTP'
   OVSNAM(IVTYPE) = 'TpPT06'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 06'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 116
   OVKEYW(IVTYPE) = 'PT07RTP'
   OVSNAM(IVTYPE) = 'TpPT07'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 07'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 117
   OVKEYW(IVTYPE) = 'PT08RTP'
   OVSNAM(IVTYPE) = 'TpPT08'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 08'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 118
   OVKEYW(IVTYPE) = 'PT09RTP'
   OVSNAM(IVTYPE) = 'TpPT09'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 09'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 119
   OVKEYW(IVTYPE) = 'PT10RTP'
   OVSNAM(IVTYPE) = 'TpPT10'
   OVLNAM(IVTYPE) = 'Relative peak period of partition 10'
   OVUNIT(IVTYPE) = UT
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 100.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 120
   OVKEYW(IVTYPE) = 'PTWLEN'
   OVSNAM(IVTYPE) = 'WlPT01'
   OVLNAM(IVTYPE) = 'Average wave length of partition 01'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 121
   OVKEYW(IVTYPE) = 'PT02WLEN'
   OVSNAM(IVTYPE) = 'WlPT02'
   OVLNAM(IVTYPE) = 'Average wave length of partition 02'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 122
   OVKEYW(IVTYPE) = 'PT03WLEN'
   OVSNAM(IVTYPE) = 'WlPT03'
   OVLNAM(IVTYPE) = 'Average wave length of partition 03'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 123
   OVKEYW(IVTYPE) = 'PT04WLEN'
   OVSNAM(IVTYPE) = 'WlPT04'
   OVLNAM(IVTYPE) = 'Average wave length of partition 04'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 124
   OVKEYW(IVTYPE) = 'PT05WLEN'
   OVSNAM(IVTYPE) = 'WlPT05'
   OVLNAM(IVTYPE) = 'Average wave length of partition 05'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 125
   OVKEYW(IVTYPE) = 'PT06WLEN'
   OVSNAM(IVTYPE) = 'WlPT06'
   OVLNAM(IVTYPE) = 'Average wave length of partition 06'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 126
   OVKEYW(IVTYPE) = 'PT07WLEN'
   OVSNAM(IVTYPE) = 'WlPT07'
   OVLNAM(IVTYPE) = 'Average wave length of partition 07'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 127
   OVKEYW(IVTYPE) = 'PT08WLEN'
   OVSNAM(IVTYPE) = 'WlPT08'
   OVLNAM(IVTYPE) = 'Average wave length of partition 08'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 128
   OVKEYW(IVTYPE) = 'PT09WLEN'
   OVSNAM(IVTYPE) = 'WlPT09'
   OVLNAM(IVTYPE) = 'Average wave length of partition 09'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 129
   OVKEYW(IVTYPE) = 'PT10WLEN'
   OVSNAM(IVTYPE) = 'WlPT10'
   OVLNAM(IVTYPE) = 'Average wave length of partition 10'
   OVUNIT(IVTYPE) = UL
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1000.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 200.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 130
   OVKEYW(IVTYPE) = 'PTDIR'
   OVSNAM(IVTYPE) = 'DrPT01'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 01'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 131
   OVKEYW(IVTYPE) = 'PT02DIR'
   OVSNAM(IVTYPE) = 'DrPT02'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 02'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 132
   OVKEYW(IVTYPE) = 'PT03DIR'
   OVSNAM(IVTYPE) = 'DrPT03'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 03'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 133
   OVKEYW(IVTYPE) = 'PT04DIR'
   OVSNAM(IVTYPE) = 'DrPT04'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 04'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 134
   OVKEYW(IVTYPE) = 'PT05DIR'
   OVSNAM(IVTYPE) = 'DrPT05'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 05'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 135
   OVKEYW(IVTYPE) = 'PT06DIR'
   OVSNAM(IVTYPE) = 'DrPT06'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 06'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 136
   OVKEYW(IVTYPE) = 'PT07DIR'
   OVSNAM(IVTYPE) = 'DrPT07'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 07'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 137
   OVKEYW(IVTYPE) = 'PT08DIR'
   OVSNAM(IVTYPE) = 'DrPT08'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 08'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 138
   OVKEYW(IVTYPE) = 'PT09DIR'
   OVSNAM(IVTYPE) = 'DrPT09'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 09'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 139
   OVKEYW(IVTYPE) = 'PT10DIR'
   OVSNAM(IVTYPE) = 'DrPT10'
   OVLNAM(IVTYPE) = 'Average wave direction of partition 10'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 2
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 360.
   OVEXCV(IVTYPE) = -999.

   IVTYPE = 140
   OVKEYW(IVTYPE) = 'PTDSPR'
   OVSNAM(IVTYPE) = 'DsPT01'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 01'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 141
   OVKEYW(IVTYPE) = 'PT02DSPR'
   OVSNAM(IVTYPE) = 'DsPT02'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 02'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 142
   OVKEYW(IVTYPE) = 'PT03DSPR'
   OVSNAM(IVTYPE) = 'DsPT03'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 03'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 143
   OVKEYW(IVTYPE) = 'PT04DSPR'
   OVSNAM(IVTYPE) = 'DsPT04'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 04'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 144
   OVKEYW(IVTYPE) = 'PT05DSPR'
   OVSNAM(IVTYPE) = 'DsPT05'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 05'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 145
   OVKEYW(IVTYPE) = 'PT06DSPR'
   OVSNAM(IVTYPE) = 'DsPT06'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 06'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 146
   OVKEYW(IVTYPE) = 'PT07DSPR'
   OVSNAM(IVTYPE) = 'DsPT07'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 07'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 147
   OVKEYW(IVTYPE) = 'PT08DSPR'
   OVSNAM(IVTYPE) = 'DsPT08'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 08'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 148
   OVKEYW(IVTYPE) = 'PT09DSPR'
   OVSNAM(IVTYPE) = 'DsPT09'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 09'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 149
   OVKEYW(IVTYPE) = 'PT10DSPR'
   OVSNAM(IVTYPE) = 'DsPT10'
   OVLNAM(IVTYPE) = 'Directional spreading of partition 10'
   OVUNIT(IVTYPE) = UDI
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 360.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 60.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 150
   OVKEYW(IVTYPE) = 'PTWFRAC'
   OVSNAM(IVTYPE) = 'WfPT01'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 01'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 151
   OVKEYW(IVTYPE) = 'PT02WFRAC'
   OVSNAM(IVTYPE) = 'WfPT02'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 02'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 152
   OVKEYW(IVTYPE) = 'PT03WFRAC'
   OVSNAM(IVTYPE) = 'WfPT03'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 03'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 153
   OVKEYW(IVTYPE) = 'PT04WFRAC'
   OVSNAM(IVTYPE) = 'WfPT04'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 04'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 154
   OVKEYW(IVTYPE) = 'PT05WFRAC'
   OVSNAM(IVTYPE) = 'WfPT05'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 05'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 155
   OVKEYW(IVTYPE) = 'PT06WFRAC'
   OVSNAM(IVTYPE) = 'WfPT06'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 06'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 156
   OVKEYW(IVTYPE) = 'PT07WFRAC'
   OVSNAM(IVTYPE) = 'WfPT07'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 07'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 157
   OVKEYW(IVTYPE) = 'PT08WFRAC'
   OVSNAM(IVTYPE) = 'WfPT08'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 08'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 158
   OVKEYW(IVTYPE) = 'PT09WFRAC'
   OVSNAM(IVTYPE) = 'WfPT09'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 09'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 159
   OVKEYW(IVTYPE) = 'PT10WFRAC'
   OVSNAM(IVTYPE) = 'WfPT10'
   OVLNAM(IVTYPE) = 'Wind fraction of partition 10'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 1.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 160
   OVKEYW(IVTYPE) = 'PTSTEE'
   OVSNAM(IVTYPE) = 'StPT01'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 01'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 161
   OVKEYW(IVTYPE) = 'PT02STEE'
   OVSNAM(IVTYPE) = 'StPT02'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 02'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 162
   OVKEYW(IVTYPE) = 'PT03STEE'
   OVSNAM(IVTYPE) = 'StPT03'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 03'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 163
   OVKEYW(IVTYPE) = 'PT04STEE'
   OVSNAM(IVTYPE) = 'StPT04'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 04'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 164
   OVKEYW(IVTYPE) = 'PT05STEE'
   OVSNAM(IVTYPE) = 'StPT05'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 05'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 165
   OVKEYW(IVTYPE) = 'PT06STEE'
   OVSNAM(IVTYPE) = 'StPT06'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 06'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 166
   OVKEYW(IVTYPE) = 'PT07STEE'
   OVSNAM(IVTYPE) = 'StPT07'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 07'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 167
   OVKEYW(IVTYPE) = 'PT08STEE'
   OVSNAM(IVTYPE) = 'StPT08'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 08'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 168
   OVKEYW(IVTYPE) = 'PT09STEE'
   OVSNAM(IVTYPE) = 'StPT09'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 09'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 169
   OVKEYW(IVTYPE) = 'PT10STEE'
   OVSNAM(IVTYPE) = 'StPT10'
   OVLNAM(IVTYPE) = 'Wave steepness of partition 10'
   OVUNIT(IVTYPE) = ' '
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 1.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 0.1
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 170
   OVKEYW(IVTYPE) = 'PARTIT'
   OVSNAM(IVTYPE) = 'PARTIT'
   OVLNAM(IVTYPE) = 'Spectral partions of all parameters'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

   IVTYPE = 171
   OVKEYW(IVTYPE) = 'NPART'
   OVSNAM(IVTYPE) = 'Npart'
   OVLNAM(IVTYPE) = 'Number of spectral partions'
   OVUNIT(IVTYPE) = UH
   OVSVTY(IVTYPE) = 1
   OVLLIM(IVTYPE) = 0.
   OVULIM(IVTYPE) = 100.
   OVLEXP(IVTYPE) = 0.
   OVHEXP(IVTYPE) = 10.
   OVEXCV(IVTYPE) = -9.

!     various parameters for computation of output quantities
!
!     reference time for TSEC
   OUTPAR(1) = 0.
!     power in expression for PER and RPER
!     previous name: SPCPOW
   OUTPAR(2) = 1.
!     power in expression for WLEN
!     previous name: AKPOWR
   OUTPAR(3) = 1.
!     indicator for direction
!     =0: direction always w.r.t. user coordinates; =1: dir w.r.t. frame
   OUTPAR(4) = 0.
!     frequency limit for swell
   OUTPAR(5) = 0.1
!     number of output partitions
   OUTPAR(51) = 5.
!     0=integration over [0,inf], 1=integration over [fmin,fmax]
   OUTPAR(6:20) = 0.
!     lower bound of integration range for output parameters
   OUTPAR(21:35) = 0.
!     upper bound of integration range for output parameters
   OUTPAR(36:50) = 1000.

   RETURN
! * end of subroutine SWINIT *
end subroutine SWINIT

!************************************************************************
!                                                                      *
SUBROUTINE SWPREP ( BSPECS, BGRIDP, CROSS , XCGRID ,YCGRID ,&
&KGRPNT, KGRBND, SPCDIR, SPCSIG, DIFFR, TRIADS )
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_services, ONLY: SWOBST
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_number_formatting
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_boundary_counters
   USE swan_run_mode
   USE swan_input_grids
   USE swan_input_field_files
   USE SWCOMM3
   USE swan_propagation_scheme
   USE M_OBSTA
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanGriddata
   USE SwanCompdata
   USE SwanIEM
   REAL :: ALCP


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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.82: IJsbrand Haagsma
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.09: Annette Kieftenburg
!     40.21: Agnieszka Herman
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.82: Dirk Rijnsdorp
!     41.85: Ad Reniers
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!     43.01: Marcel Zijlema
!
!  1. Updates
!
!     20.70, Jan. 96: new name, SPRCON is now called from this subr
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Mar. 98: loop over grid points moved into subr SWOBST
!                     erroneous usage of SWPDIR removed
!                     close input files containing stationary input fields
!     30.82, Oct. 98: Added INTEGER declaration of array OBSTA(*)
!     40.00, Feb. 99: DYNDEP is made True, if depth or water level nonstationary
!     40.09, Aug. 00: If obstacle is on computational grid point it is moved a bit
!     40.02, Oct. 00: Array KGRBND now has a dimension
!     40.02, Oct. 00: Initialisation of IERR
!     40.21, Aug. 01: allocation of arrays for diffraction
!     40.31, Nov. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Jun. 07: extension to unstructured grids
!     41.82, Aug. 21: introduce FIG source term
!     41.85, Apr. 22: implementation of IEM (surfbeat model)
!     41.90, Nov. 21: adding QC scattering
!     43.01, Aug. 24: parallelization of unstructured boundaries and their conditions
!
!  2. Purpose
!
!     do some preparations before computation is started
!
!  3. Method
!
!  4. Argument variables

   INTEGER, INTENT(INOUT) :: KGRBND(*)

!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   TYPE(diffraction_state_t), INTENT(INOUT) :: DIFFR
   TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)
   REAL    SPCDIR(MDC,6)  ,    SPCSIG(MSC)

!  5. SUBROUTINES CALLING
!
!     SWREAD
!
!  6. SUBROUTINES USED
!
!     SWOBST
!
!  7. ERROR MESSAGES
!
!     ---
!
!  8. REMARKS
!
!     ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       Compute origin of computational grid in problem grid XPC, YPC
!       Compute origin of problem grid in computationl grid coordinates
!       Check input fields
!       Close files containing stationary input fields
!       Compute crossing of comp.grid lines with obstacles
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT
   INTEGER, PARAMETER :: NUNT = 20
   INTEGER   KGRPNT(MXC,MYC), CROSS(2,MCGRD)
   INTEGER   BGRIDP(6*NBGRPT)
   REAL      BSPECS(MDC,MSC,NBSPEC,2)
   INTEGER, ALLOCATABLE :: CROSSGL(:,:)
   INTEGER   ITMP(1)
   INTEGER, PARAMETER :: IUNT(NUNT) = [&
      7, 9, 54, 55, 56, 57, 60, 61, 62, 63, &
      64, 65, 66, 67, 68, 69, 72, 73, 74, 75]
   REAL      FDN, FIGF
   REAL      MFR, SHP
   INTEGER :: I1, I2, IB, IC, ICG, IFLD, IFIG, IFREE, II, IIXX, IIYY
   INTEGER :: INDX, INDXGR, IP, IRFRD, ITMP1, ITMP2, ITMP3, ITRA, ITRAS
   INTEGER :: IVTYPE, IX, IY, J, JJ, K, MTH, NBS
   REAL :: ALBC, ALTMP, ATMP, PPTAIL, PWDTH, TRCF

   INTEGER   ISTAT, IF1, IL1
   CHARACTER(LEN=20) CHARS(1)
   CHARACTER(120) :: MSGSTR

   TYPE(BSDAT) , POINTER :: CURRBS
   TYPE(BGPDAT), POINTER :: CURBGP
   TYPE(OBSTDAT), POINTER :: COBST
   INTEGER, SAVE :: IENT = 0
   CALL STRACE (IENT, 'SWPREP')

   IF ( ITEST.GE.100 ) THEN
      WRITE (PRTEST,*) ' BNAUT after SWREAD = ',BNAUT
   END IF

!     coefficients for transformation from user coordinates to comp. coord.

   COSPC = COS(ALPC)
   SINPC = SIN(ALPC)
   XCP   = -XPC*COSPC - YPC*SINPC
   YCP   =  XPC*SINPC - YPC*COSPC
   ALCP  = -ALPC

!     wind direction w.r.t. computational grid
!
!     ALTMP = (WDIP - ALPC) / PI2              removed 30.50
   ALTMP = (WDIP) / PI2
   WDIC  = PI2 * (ALTMP - NINT(ALTMP))

   IF (MXC.LE.0 .AND. OPTG.NE.5) CALL MSGERR&
   &(3, 'no valid computational grid; check command CGRID')
   IF (MCGRD.LE.1 .AND. nverts.LE.0) CALL MSGERR&
   &(3, 'no valid comp. grid; check command READ BOT or READ UNSTRU')

   IF (LEDS(1).EQ.0) CALL MSGERR (3,'Bottom grid not defined')
   IF (LEDS(1).EQ.1) CALL MSGERR (3,'No bottom levels read')
   IF (IUBOTR.EQ.1 .AND. IBOT.EQ.0)&
   &CALL MSGERR (1,'Bottom friction not on, UBOT not computed')

   IF (LEDS(2).EQ.2) THEN
      IF (LEDS(3).NE.2)&
      &CALL MSGERR (3, 'VY not read, while VX is read')
!       ALBC  = ALPC - ALPG(2)
      ALBC  = - ALPG(2)
      COSVC = COS(ALBC)
      SINVC = SIN(ALBC)
   ENDIF

   IF (LEDS(4).EQ.2) VARFR = .TRUE.

   IF (VARFR .AND. IBOT.EQ.5) THEN
      CALL MSGERR (1,&
      &'Ripples model active, space varying friction ignored')
      VARFR = .FALSE.
   ENDIF

   IF (LEDS(5).EQ.2) THEN
      IF (LEDS(6).NE.2)&
      &CALL MSGERR (3, 'WY not read, while WX is read')
      VARWI = .TRUE.
!       ALBC  = ALPC - ALPG(5)
      ALBC  = - ALPG(5)
      COSWC = COS(ALBC)
      SINWC = SIN(ALBC)
   ENDIF

   IF (LEDS(7).EQ.2) VARWLV = .TRUE.

   IF (IFLDYN(1).EQ.1 .OR. IFLDYN(7).EQ.1) DYNDEP = .TRUE.

   IF (OPTG.EQ.5) BNDCHK = .FALSE.

!     check number of sweeps meant for unstructured grid
   IF ( nsweep.EQ.-999 ) THEN
!        if nsweep = 1, the original crest method is recovered, however
!        for stationary runs including refraction and source terms,
!        nsweep is set to 3 (CPU time enhanced, but number of
!        iterations decreased compared to nsweep = 1)
      nsweep = 3
!        further optimization in case of no refraction/source terms
      IF ( IREFR == 0 ) nsweep = 4
      IF ( IQUAD == 0 .AND. IWCAP == 0 .AND. ISURF == 0 ) nsweep = 4
      IF ( OFFSRC ) nsweep = 4
      IF ( IGEN == 4 .OR. IQCM /= 0 ) nsweep = 3
   ENDIF
!     nsweep = 1 cannot work naturally in case of SECTOR
   IF ( nsweep.EQ.1 .AND. .NOT.FULCIR ) nsweep = 3

!     adapt units in case of true energy
   IF ( INRHOG.EQ.1 ) THEN
      DO J = 1, NUNT
         IVTYPE = IUNT(J)
         OVUNIT(IVTYPE) = 'W/m2'
      ENDDO
      OVUNIT(19) = 'W/m'
      OVUNIT(21) = 'Js/m2'
      OVUNIT(22) = 'J/m2'
      OVUNIT(29) = 'J/m2'
   ENDIF

!     check wind drag
   IF (IWIND.EQ.8) THEN
!        apply Hwang if wrong wind drag
      IF (IDRAG.LT.4) IDRAG = 4
   ELSE
!        apply parabolic fit if wrong wind drag
      IF (IDRAG.GT.3) IDRAG = 2
   ENDIF
   IF (IBOT.EQ.5) IDRAG = 1

!     initialize reduction factor for wind input term
!     - this array contains at most 100 frequencies (see module SWCOMM3)
!     - this factor will be assigned to LFACTOR in SdsBabanin.f90
   RDFSIN = 0.

!     check negative wind input, if appropriate
   IF ( ZIEGER ) THEN
      IF ( RDCOEF.LT.0. .OR. RDCOEF.GT.1. ) THEN
         CALL MSGERR (2,'NEGATINP convention: it should be a '&
         &//'positive fraction')
      END IF
   ELSE IF ( ROGERS ) THEN
      IF ( RDCOEF.NE.0. ) THEN
         CALL MSGERR (1,'NEGATINP and ROGERS are incompatible ')
         RDCOEF = 0.
      END IF
   ELSE IF ( ARDHUIN ) THEN
      IF ( RDCOEF.NE.0. ) THEN
         CALL MSGERR (1,'NEGATINP and ARDHUIN are incompatible ')
         RDCOEF = 0.
      END IF
   END IF

   IF (IWCAP.EQ.8) THEN
      JPSWEL = 12
   ENDIF

!     prepare QC model
   IF (IQCM.NE.0) THEN
      ! refraction due to depth variations only is already
      ! included in the QC scattering
      IF (IQCM.EQ.1) IREFR = 0
      ! frequency shift due to mean current is already included
      ! in the QC scattering
      ITFRE = 0
      ! rescaling is unwanted since action density may be negative
      BRESCL = .FALSE.
      ! no check on boundary because of coherence effects
      BNDCHK = .FALSE.
   ENDIF

!     close input files containing stationary input fields

   DO IFLD = 1, NUMGRD
      IF (IFLDYN(IFLD).EQ.0 .AND. IFLNDS(IFLD).NE.0) THEN
         CLOSE (IFLNDS(IFLD))
         IFLNDS(IFLD) = 0
      ENDIF
   ENDDO

!     computation of tail factors for moments of action spectrum
!     IP=0: action int. IP=1: energy int. IP=2: first moment of energy etc.

   DO IP = 0, 3
      PPTAIL = PWTAIL(1) - REAL(IP)
      PWTAIL(5+IP) = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
   ENDDO

!     check location of output areas

   CALL SPRCON ( XCGRID, YCGRID, KGRPNT, KGRBND )

!     *** find obstacles crossing the points in the stencil  ***
!     *** in structured grids                                ***

   IF (NUMOBS .GT. 0 .AND. OPTG.NE.5 .AND. .NOT.OBSTDONE) THEN
      DO INDX = 1, MCGRD
         CROSS(1,INDX) = 0
         CROSS(2,INDX) = 0
      ENDDO

      ITMP1  = MXC
      ITMP2  = MYC
      ITMP3  = MCGRD
      MXC    = MXCGL
      MYC    = MYCGL
      MCGRD  = MCGRDGL
      ALLOCATE(CROSSGL(2,MCGRDGL))
      CROSSGL = 0
      CALL SWOBST (XGRDGL, YGRDGL, KGRPGL, CROSSGL)
      MXC    = ITMP1
      MYC    = ITMP2
      MCGRD  = ITMP3

      II = 1
      DO IX = MXF, MXL
         DO IY = MYF, MYL
            INDX = KGRPGL(IX,IY)
            IF ( INDX.NE.1 ) THEN
               II = II + 1
               CROSS(1:2,II) = CROSSGL(1:2,INDX)
            END IF
         END DO
      END DO
      DEALLOCATE(CROSSGL)
      OBSTDONE = .TRUE.

      IF (ITEST .GE. 120) THEN
         WRITE(PRINTF,"('Links with obstacles crossing',/, ' COMP COORD LINK VALUE')")
         DO IIYY =2,MYC
            DO IIXX = 2,MXC
               I1 = KGRPNT(IIXX,IIYY)
               DO I2 = 1,2
                  IF (CROSS(I2,I1) .NE. 0)&
                  &WRITE(PRINTF,"(' POINT(',I4,',',I4,')', ' CROSS(',I3,',',I5,') = ',I5)") IIXX,IIYY,I2,I1,CROSS(I2,I1)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
   ENDIF

!     check freeboard dependent transmission/reflection

   IF ( NUMOBS.GT.0 ) THEN
      COBST => FOBSTAC
      DO II = 1, NUMOBS
         IFREE = COBST%FBTYP1
         ITRAS = COBST%TRTYPE
         IRFRD = COBST%RFTYP3
         IF ( IFREE.EQ.1 .AND. (ITRAS.NE.0 .OR. IRFRD.EQ.1) ) THEN
            CALL MSGERR(1,'freeboard dependent factors cannot be')
            CALL MSGERR(1,'applied to nonconstant transmission or')
            CALL MSGERR(1,'reflection coefficients')
            IFREE = 0
         ENDIF
         COBST%FBTYP1 = IFREE
         IF (.NOT.ASSOCIATED(COBST%NEXTOBST)) EXIT
         COBST => COBST%NEXTOBST
      ENDDO
   ENDIF

!     check radiating FIG energy and
!     determine frequency distribution to FIG source
!     (see Ardhuin et al., 2014)

   IF ( NUMOBS.GT.0 ) THEN
      COBST => FOBSTAC
      DO II = 1, NUMOBS
         IFIG  = COBST%IGTYP
         ITRA  = COBST%TRTYPE
         TRCF  = COBST%TRCOEF(1)
         IFREE = COBST%FBTYP1
         IF ( IFIG.EQ.1 .AND.&
         &( ITRA.NE.0  .OR. TRCF.NE.0. .OR. IFREE.NE.0 ) ) THEN
            WRITE(MSGSTR,'(A)')&
            &'transmission not allowed in case of'//&
            &' radiated FIG energy'
            CALL MSGERR( 1, TRIM(MSGSTR) )
            ITRA  = 0
            TRCF  = 0.
            IFREE = 0
         ENDIF
         COBST%TRTYPE    = ITRA
         COBST%TRCOEF(1) = TRCF
         COBST%FBTYP1    = IFREE
         IF ( IFIG.EQ.1 ) THEN
            MFR = COBST%IGCOEF(6)
            SHP = COBST%IGCOEF(7)
            ALLOCATE(COBST%IGFRQD(MSC))
            COBST%IGFRQD(:) = 0.
            FDN = 0.
            DO JJ = 1, MSC
               FIGF = MIN( 1., 2.*PI * MFR / SPCSIG(JJ) )**SHP
               COBST%IGFRQD(JJ) = FIGF
               FDN = FDN + SPCSIG(JJ) * FIGF
            ENDDO
            COBST%IGFRQD = COBST%IGFRQD / FDN / FRINTF
         ENDIF
         IF (.NOT.ASSOCIATED(COBST%NEXTOBST)) EXIT
         COBST => COBST%NEXTOBST
      ENDDO
   ENDIF

!     skip below if no boundary conditions have been specified
   IF (ALOBND) THEN

   ATMP = -999.
   J    = 0

   CURRBS => FBS
   DO
      NBS = CURRBS%NBS
      IF (NBS.EQ.-999) EXIT
      SPPARM(1:4) = CURRBS%SPPARM(1:4)
      J = J + 1
      IF ( J.EQ.1 ) ATMP = 0.
      ATMP = ATMP + SPPARM(3)
      FSHAPE      = CURRBS%FSHAPE
      DSHAPE      = CURRBS%DSHAPE
      CALL SSHAPE (BSPECS(1,1,NBS,1), SPCSIG, SPCDIR,&
      &FSHAPE, DSHAPE)
      IF (.NOT.ASSOCIATED(CURRBS%NEXTBS)) EXIT
      CURRBS => CURRBS%NEXTBS
   END DO

   IF (J.GT.0) ATMP = ATMP/REAL(J)

   IF ( ATMP.NE.-999. ) THEN
      ATMP = DEGCNV (ATMP)
      ALTMP = ATMP / 360.
      ATMP = PI2 * (ALTMP - NINT(ALTMP))
   ENDIF

!     set sweep direction based on either incoming wave direction
!     or wind direction, if user-given direction not specified
   IF ( .NOT. asort.NE.-999. ) THEN
      IF ( NSTATM == 0 .OR. NBFILS == 0 ) THEN
         IF (ATMP.NE.-999. .OR. IWIND.EQ.0 .OR. VARWI) THEN
            asort = ATMP
         ELSE
            asort = WDIP
         ENDIF
      ENDIF
   ENDIF

   BGRIDP = 0
   IF (OPTG.NE.5) THEN
      IF (NBGGL.EQ.0) NBGGL = NBGRPT
      NBGRPT = 0
      DO IX = MXF, MXL
         DO IY = MYF, MYL
            INDX = KGRPGL(IX,IY)
            CURBGP => FBGP
            DO II = 1, NBGGL
               INDXGR = CURBGP%BGP(1)
               IF ( INDXGR.EQ.INDX ) THEN
                  NBGRPT = NBGRPT + 1
                  BGRIDP(6*NBGRPT-5) = KGRPNT(IX-MXF+1,IY-MYF+1)
                  BGRIDP(6*NBGRPT-4) = CURBGP%BGP(2)
                  BGRIDP(6*NBGRPT-3) = CURBGP%BGP(3)
                  BGRIDP(6*NBGRPT-2) = CURBGP%BGP(4)
                  BGRIDP(6*NBGRPT-1) = CURBGP%BGP(5)
                  BGRIDP(6*NBGRPT  ) = CURBGP%BGP(6)
               END IF
               IF (.NOT.ASSOCIATED(CURBGP%NEXTBGP)) EXIT
               CURBGP => CURBGP%NEXTBGP
            END DO
         END DO
      END DO
   ELSE
      IF (NBGGL.EQ.0) NBGGL = NBGRPT
      NBGRPT = 0
      CURBGP => FBGP
      DO II = 1, NBGGL
         INDXGR = CURBGP%BGP(1)
         ITMP   = MINLOC(ABS(bvertg(:,2)-INDXGR))
         K      = ITMP(1)
         IF ( bvertg(K,2).EQ.INDXGR ) THEN
            IX = bvertg(K,1)
            NBGRPT = NBGRPT + 1
            BGRIDP(6*NBGRPT-5) = IX
            BGRIDP(6*NBGRPT-4) = CURBGP%BGP(2)
            BGRIDP(6*NBGRPT-3) = CURBGP%BGP(3)
            BGRIDP(6*NBGRPT-2) = CURBGP%BGP(4)
            BGRIDP(6*NBGRPT-1) = CURBGP%BGP(5)
            BGRIDP(6*NBGRPT  ) = CURBGP%BGP(6)
         END IF
         IF (.NOT.ASSOCIATED(CURBGP%NEXTBGP)) EXIT
         CURBGP => CURBGP%NEXTBGP
      ENDDO
   ENDIF

!     --- test BGRIDP for unstructured mesh
   IF ( OPTG.EQ.5 .AND. ITEST.GE.50 ) THEN
      IF ( NBGRPT.GT.0 ) THEN
         WRITE(PRTEST,*)&
         &' number of described boundary points in present subdomain ',&
         &NBGRPT
         DO IB = 1, NBGRPT
            IC = BGRIDP(6*IB-5)
            IF (.NOT.PARLL) THEN
               ICG = IC
            ELSE
               ICG = ivertg(IC)
            ENDIF
            IF (BGRIDP(6*IB-4).EQ.1)&
            &WRITE(PRTEST,'(A,I7,2F18.9,I7)')&
            &' BGRIDP: INDXGL, XP, YP, bound marker ',&
            &ICG,&
            &xcugrd(IC)+XOFFS,&
            &ycugrd(IC)+YOFFS,&
            &vmark(IC)
         ENDDO
      ENDIF
   ENDIF
   END IF

!     --- allocate arrays for diffraction and set prop scheme to BSBT

   IF ( IDIFFR.EQ.1 ) THEN
      CALL DIFFR%RESIZE(MCGRD)
      PROPSN = 1
      PROPSS = 1
   ELSE
      CALL DIFFR%RESIZE(0)
   END IF

!     --- check surfbeat

   IF (LSRFB) THEN

      ! check problem dimension

      IF ( ONED ) THEN
         CALL MSGERR(3,'surfbeat computation not allowed in 1D mode')
      ENDIF

      ! check stationarity

      IF ( NSTATC.NE.0 .OR. NSTATM.NE.0 ) THEN
         CALL MSGERR(3,&
         &'surfbeat computation not allowed in non-stationary mode')
      ELSE
         IF ( NCOMPT.EQ.1 .AND. ntf.LT.0 ) THEN
            WRITE(MSGSTR,'(A)')&
            &'current COMPUTE consists of computation of'//&
            &' sea-swell spectrum followed by bound ig waves'
            CALL MSGERR( 0, TRIM(MSGSTR) )
         ELSEIF ( NCOMPT.EQ.2 .AND. ntf.GT.0 ) THEN
            WRITE(MSGSTR,'(A)')&
            &'current COMPUTE consists of computation of'//&
            &' reflected (free) ig waves'
            CALL MSGERR( 0, TRIM(MSGSTR) )
         ELSEIF ( NCOMPT.GT.2 ) THEN
            WRITE(MSGSTR,'(A)')&
            &'command COMPUTE must appear no more than twice in '//&
            &'one simulation'
            CALL MSGERR( 3, TRIM(MSGSTR) )
         ENDIF
      ENDIF

      ! check grid and orientation

      IF ( OPTG.NE.1 ) THEN
         CALL MSGERR(3,&
         &'surfbeat only supported for rectilinear grids')
      ELSE
         IF ( ALPC.NE.0.) THEN
            WRITE(MSGSTR,'(A)')&
            &'grid orientation is restricted to [alpc] = 0'//&
            &' in case of surfbeat'
            CALL MSGERR( 3, TRIM(MSGSTR) )
         ENDIF
      ENDIF

      ! check FIG energy

      IF ( NUMOBS.GT.0 ) THEN
         COBST => FOBSTAC
         DO II = 1, NUMOBS
            IFIG = COBST%IGTYP
            IF ( IFIG.EQ.1 ) THEN
               CALL MSGERR(2,&
               &'radiated FIG energy not allowed in case surfbeat')
               IFIG = 0
            ENDIF
            COBST%IGTYP = IFIG
            IF (.NOT.ASSOCIATED(COBST%NEXTOBST)) EXIT
            COBST => COBST%NEXTOBST
         ENDDO
      ENDIF

      ! check west boundary during first COMPUTE

      IF ( ntf.LT.0 ) THEN
         IF ( NBGRPT.NE.MYC ) THEN
            CALL MSGERR(3,&
            &'boundary specification is not correct in case of surfbeat')
            CALL MSGERR(0,&
            &'please only west boundary should be specified')
         ELSE
            J = 0
            DO II = 1, NBGRPT
               INDXGR = BGRIDP(6*II-5)
               INDX   = KGRPNT(1,II)   ! ix=1: west
               IF (BGRIDP(6*II-4).EQ.1) THEN
                  IF (INDXGR.NE.INDX) J=J+1
               ENDIF
            ENDDO
            IF (J.NE.0) THEN
               CALL MSGERR(3,&
               &'location of boundary must be west in case of surfbeat')
            ENDIF
         ENDIF
      ENDIF
   ENDIF

!     --- preparation for triads

   IF ( ITRIAD.GT.0 ) THEN
!        *** allocate frequency- and space-dependent data for triads
      CALL TRIADS%resize(ITRIAD, MSC, MCGRD, IBIPH, ISTAT)
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array QTRI and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF
   ELSE
      CALL TRIADS%resize(ITRIAD, MSC, MCGRD, IBIPH, ISTAT)
   ENDIF

!     reset full directional integration parameter for CCA approach

   IF ( PTRIAD(8).NE.-1. ) THEN
      PWDTH = PTRIAD(8) * PI/180.
      MTH = MDC
      IF ( .NOT.FULCIR ) MTH = MDC - 1
      IF ( .NOT.PWDTH.LT.FLOAT(MTH)*DDIR ) PTRIAD(8) = -1.
   ENDIF

!     check biphase in case no triads

   IF ( ITRIAD.EQ.0 ) THEN
      IF ( ISURF.EQ.7 ) THEN
         IBIPH = 2
         PTRIAD(9) = 1.
      ENDIF
   ENDIF

   RETURN
! * end of subroutine SWPREP *
end subroutine SWPREP

!************************************************************************
!                                                                      *
SUBROUTINE SPRCON (XCGRID, YCGRID, KGRPNT, KGRBND)
   USE swan_input_point_validation, ONLY: SINUPT, SINBTG
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_input_grids
   USE SWCOMM3
   USE OUTP_DATA
   USE SwanGriddata


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
!  0. AUTHORS
!
!     30.72: IJsbrand Haagsma
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     32.03: Roeland Ris & Cor van der Schelde (1D-version)
!     40.02: IJsbrand Haagsma
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.72, Sept 97: Replaced DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     32.02, Jan. 98: Introduced 1D-version
!     32.03  Feb. 98: corrections processed
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.02, Feb. 00: Removed obsolescent DO-construct
!     40.02, Oct. 00: Avoided scalar/array conflict
!     40.31, Dec. 03: removing POOL construction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!
!  2. Purpose
!
!     Execution of some tests on the given model description
!
!  3. Method
!
!     This subroutine carries out the following tests:
!     - check if bottom and computational grid are defined (if MODIF=0)
!     - check on the location of the corner points of the computational
!       grid
!     - check on the location of output point sets
!
!  4. Argument variables
!
!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   INTEGER :: KGRBND(*)

   REAL    :: XCGRID(MXC,MYC), YCGRID(MXC,MYC)

!  6. Local variables
!
!     I, J    counters

   INTEGER I, J, IP, MIP, MXK

!     XR      x (comp. grid coord.)
!     YR      y (comp. grid coord.)

   REAL    XR, YR, XP, XQ, YP, YQ

!  8. Subroutines used
!
!     COPYCH
!     MSGERR (both Ocean Pack)
!     SINBTG
!     SINUPT (both SWAN/SER)
!
!  9. Subroutines calling
!
!     SWREAD
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
!     ----------------------------------------------------------------
!     Check whether bottom grid and computational grid are defined
!     and if the bottom and current are read
!     For every corner point of the computational grid do
!         Compute problem grid coordinates
!         If corner point is outside bottom grid (SINBTG = false), then
!             Call MSGERR to generate a warning
!     If computational grid is rotating (ICOMP = 0), then
!         If the rotation point is out of the bottom grid, then
!             Call MSGERR to generate an error message
!     Else
!         Compute coordinates of the center of the computational grid
!         If the center is outside the bottom grid, then
!             Call MSGERR to generate an error message
!     Read number of output pointsets in array IOUTD
!     If number of pointsets is not 0, then
!         For every pointset do
!             Call COPYCH to read the name from IOUTD
!             If the pointset is not the bottom grid, comp. grid or set
!               of lines or places and recordlength > 0, then
!                 Read type of pointset
!                 If pointset is of type F (frame), then
!                     Check location of the cornerpoints in bottom grid
!                       and computational grid
!                 If pointset is of type C (curve), then
!                     Check locations of all end points of the curves
!                 If pointset is of type P (points), then
!                     Check location of all the points
!                 If pointset is of typr R (rays), then
!                     Check end points of first and last ray
!                 If pointset is of type G (grid), then
!                     Check location of the cornerpoints in bottom grid
!                       and computational grid
!     ----------------------------------------------------------------
!
! 13. Source text

   INTEGER  KGRPNT(MXC,MYC)
   CHARACTER(LEN=1) :: STYPE
   TYPE(OPSDAT), POINTER :: CUOPS
   INTEGER, SAVE :: IENT = 0
   CALL STRACE (IENT,'SPRCON')

!     ***** test location of computational grid *****

   IF (ITEST.GE.10) WRITE (PRTEST, '(A)')&
   &'Check location of computational grid'

   IF (OPTG .EQ. 1) THEN

!       regular grid

      IF (ONED) THEN

!         For 1D-version:
!         *** Check angles of bottom grid and computational grid ***

         IF ( ABS((ALPG(1) - ALPC)) .GT. 0.0017 ) THEN
            CALL MSGERR( 2, ' Difference between angle of bottom grid'//&
            &' (alpinp) and computational grid (alpc)')
            CALL MSGERR( 2, ' greater than 0.1 degrees.')
         ENDIF

!         *** Check location of computational grid ***

         DO  I=0,1
            XR = I*XCLEN
            XP = XPC + XR*COSPC
            YP = YPC + XR*SINPC
            IF (.NOT.SINBTG(XP,YP) ) THEN
               CALL MSGERR(1,'Corner of comp grid outside bottom grid')
               WRITE (PRINTF, "(' Coordinates :',2F10.2)") XP+XOFFS, YP+YOFFS
            ENDIF
         ENDDO
      ELSE

!         two-dimensional case

         do I=0,1
            do J=0,1
               XR = I*XCLEN
               YR = J*YCLEN
               XP = XPC + XR*COSPC - YR*SINPC
               YP = YPC + XR*SINPC + YR*COSPC
               IF (.NOT.SINBTG(XP,YP) ) THEN
                  CALL MSGERR(1,'Corner of comp grid outside bottom grid')
                  WRITE (PRINTF, "(' Coordinates :',2F10.2)") XP+XOFFS, YP+YOFFS
               ENDIF
            end do
         end do
      ENDIF

      XR = 0.5*XCLEN
      YR = 0.5*YCLEN
      XP = XPC + XR*COSPC - YR*SINPC
      YP = YPC + XR*SINPC + YR*COSPC
      IF (.NOT. SINBTG(XP,YP) ) THEN
         CALL MSGERR (2,' Centre of comp. grid outside bottom grid')
      ENDIF
   ELSEIF (OPTG.EQ.5) THEN

!       --- check location of computational grid

      DO I = 1, nverts
         IF ( vmark(I) /= 0 ) THEN
            XP = xcugrd(I)
            YP = ycugrd(I)
            IF (.NOT.SINBTG(XP,YP) ) THEN
               CALL MSGERR(1,'Corner of comp grid outside bottom grid')
               WRITE (PRINTF, "(' Coordinates :',2F10.2)") XP+XOFFS, YP+YOFFS
            ENDIF
         ENDIF
      ENDDO
   ENDIF

!     ***** test location of output pointsets *****
   IF (LOPS) THEN
      CUOPS => FOPS
      output_sets: DO
!         --- get name of point set
         SNAME = CUOPS%PSNAME
         IF (ITEST.GE.80) WRITE (PRTEST, "(' test SPRCON ', A8)") SNAME

!         input and computational grids are excluded from the test
         IF (SNAME.EQ.'BOTTGRID' .OR. SNAME.EQ.'COMPGRID' .OR. &
             SNAME.EQ.'WXGRID'   .OR. SNAME.EQ.'WYGRID'   .OR. &
             SNAME.EQ.'VXGRID'   .OR. SNAME.EQ.'VYGRID'   .OR. &
             SNAME.EQ.'WLEVGRID' .OR. SNAME.EQ.'FRICGRID') THEN
            IF (.NOT.ASSOCIATED(CUOPS%NEXTOPS)) EXIT output_sets
            CUOPS => CUOPS%NEXTOPS
            CYCLE output_sets
         END IF

         IF (ITEST.GE.10) WRITE (PRTEST, '(A)')&
         &'Check location of output pointset '//TRIM(SNAME)

         STYPE = CUOPS%PSTYPE

!         *** Check other output locations ***

         IF (STYPE.EQ.'F' .AND. OPTG .EQ. 1) THEN

!           check the four corners of the frame

            XQLEN = CUOPS%OPR(3)
            YQLEN = CUOPS%OPR(4)
            XPQ   = CUOPS%OPR(1)
            YPQ   = CUOPS%OPR(2)
            ALPQ  = CUOPS%OPR(5)
            COSPQ = COS(ALPQ)
            SINPQ = SIN(ALPQ)
            IF (ONED) THEN
               DO  I=0,1
                  XQ = I*XQLEN
                  XP = XPQ + XQ*COSPQ
                  YP = YPQ + XQ*SINPQ
                  CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
                  &KGRBND)
               ENDDO
            ELSE
               do I=0,1
                  do J=0,1
                     XQ = I*XQLEN
                     YQ = J*YQLEN
                     XP = XPQ + XQ*COSPQ - YQ*SINPQ
                     YP = YPQ + XQ*SINPQ + YQ*COSPQ
                     CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
                     &KGRBND)
                  end do
               end do
            ENDIF
         ENDIF
!         --------------------------------------------------------------
         IF (STYPE .EQ. 'C') THEN

!           check first and last point of a curve

            MXK = CUOPS%MIP
            IF ( MXK/=0 ) THEN
               XP = CUOPS%XP(1)
               YP = CUOPS%YP(1)
               CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
               &KGRBND)
               XP = CUOPS%XP(MXK)
               YP = CUOPS%YP(MXK)
               CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
               &KGRBND)
            ENDIF
         ENDIF
!         --------------------------------------------------------------
         IF (STYPE .EQ. 'P' .OR. STYPE .EQ. 'U') THEN

!           check all individual output points

            MIP = CUOPS%MIP
            do IP=1,MIP
               XP = CUOPS%XP(IP)
               YP = CUOPS%YP(IP)
               CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
               &KGRBND)
            end do
         ENDIF
!         --------------------------------------------------------------
         IF (STYPE .EQ. 'R') THEN
            MIP = CUOPS%MIP
            do IP=1,MIP, MIP
               XP = CUOPS%XP(IP)
               YP = CUOPS%YP(IP)
               XQ = CUOPS%XQ(IP)
               YQ = CUOPS%YQ(IP)
               CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
               &KGRBND)
               CALL SINUPT (SNAME, XQ, YQ, XCGRID, YCGRID, KGRPNT,&
               &KGRBND)
            end do
         ENDIF
!         --------------------------------------------------------------
!         stype = 'N'     VERSION 20.63

         IF (STYPE.EQ.'N') THEN
            MIP   = CUOPS%MIP
            XQLEN = CUOPS%OPR(1)
            IF (XQLEN.NE.-999.) THEN
!              nested grid is regular, check corners
               YQLEN = CUOPS%OPR(2)
               XPQ   = CUOPS%OPR(3)
               YPQ   = CUOPS%OPR(4)
               ALPQ  = CUOPS%OPR(5)
               COSPQ = COS(ALPQ)
               SINPQ = SIN(ALPQ)
               DO I=0,1
                  DO J=0,1
                     XQ = I*XQLEN
                     YQ = J*YQLEN
                     XP = XPQ + XQ*COSPQ - YQ*SINPQ
                     YP = YPQ + XQ*SINPQ + YQ*COSPQ
                     CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
                     &KGRBND)
                  ENDDO
               ENDDO
            ELSE
!              nested grid is unstructured, check whole outline
               DO IP=1,MIP
                  XP = CUOPS%XP(IP)
                  YP = CUOPS%YP(IP)
                  CALL SINUPT (SNAME, XP, YP, XCGRID, YCGRID, KGRPNT,&
                  &KGRBND)
               ENDDO
            ENDIF
         ENDIF

         IF (.NOT.ASSOCIATED(CUOPS%NEXTOPS)) EXIT
         CUOPS => CUOPS%NEXTOPS
      END DO output_sets
   ENDIF

   RETURN
!   * end of subroutine SPRCON *
end subroutine SPRCON
!************************************************************************
!                                                                      *
SUBROUTINE SWRBC ( COMPDA )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_input_interpolation, ONLY: SVALQI
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_input_grids
   USE SWCOMM3
   USE swan_test_output
   USE M_GENARR
   USE M_PARALL
   USE SwanGriddata
   USE SwanBraggScat, only: dpmean


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
!     30.60: Nico Booij
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     33.08: W. Erick Rogers
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.75: Erick Rogers
!     41.80: Dirk Rijnsdorp and Ad Reniers
!
!  1. Updates
!
!     00.00, Nov. 86: existing version
!     00.01, Apr. 87: parameters added in call of subroutine SVALQI,
!                     some variable names changed (not common anymore)
!     30.60, Aug. 97: test on value of KGRPNT, skip part of code if
!                     KGRPNT(IX,IY)=1
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     32.01, Jan. 98: Initialise setup and saved depth for 1D-version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Mar. 98: proper water level stored in array COMPDA(*,JWLV2)
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Nov. 98: Now also interpolates curvilinear current input-fields
!                     (IGTYPE(2)=2)
!     40.00, Feb. 99: IDYNWI etc. replaced by IFLDYN(*)
!     33.08, July 98: minor changes related to the S&L scheme
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL construction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!     41.75, Jan. 19: adding sea ice
!     41.80, Sep. 21: adding Bragg scattering
!
!  2. Purpose
!
!     The depths and currents at a line in the computational grid are
!     determined and written to file with reference number NREF
!
!  3. Method
!
!     The depths and currents are computed by bilinear interpolation
!     and usually written to file INSTR.
!
!  4. Argument variables
!
!     COMPDA

   REAL    COMPDA(MCGRD,MCMVAR)

!  6. Local variables
!
!  8. Subroutines used
!
!     SVALQI (SWAN/SER)
!
!  9. Subroutines calling
!
!     SWMAIN
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
!     ----------------------------------------------------------------
!     For every line IX of the computational grid do
!         For every point IY of this line do
!             Compute bottom grid coordinates as number of meshes
!             Call SVALQI to interpolate depth and current for the point
!             If current is on (ICUR = 1), then
!                 Compute current components relative to comp. grid
!             Else
!                 Current components are zero
!     ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER :: INDX, IX, IY, JVERT
   REAL :: ASTD, CGFACT, CGMAX, DEP, DEPW, FRI, UU, VTOT, VV, WLVL
   REAL :: XAICE, XDSS, XHICE, XHSS, XMUD, XNPL, XP, XTSS, XTUR, YP
   CALL STRACE(IENT,'SWRBC')

   IF (ITEST .GE. 100 .OR. INTES .GE. 30) THEN
      WRITE(PRINTF,*) '  ', ICUR, IGTYPE(2)
      WRITE(PRINTF,*) '******** In subroutine SWRBC *******'
      IF (ICUR .EQ. 1 ) THEN
         WRITE(PRINTF,"('P.index',5X,'coord.',14X,'depth',13X,'UX',13X,'UY')")
      ELSE
         WRITE(PRINTF,"('P.index',5X,'coord.',14X,'depth')")
      ENDIF
   ENDIF

!     *** The arrays start to be filled in the second value ***
!     *** because in COMPDA(1,"variable"), is the default   ***
!     *** value for land points    version 30.21            ***
!
!     ***  Default values for land point  ***
   COMPDA(1,JDP2) = -1.
   IF (JDP1.GT.1) COMPDA(1,JDP1) = -1.
   IF (JDP3.GT.1) COMPDA(1,JDP3) = -1.
   IF (VARWLV) THEN
      COMPDA(1,JWLV2) = 0.
!       next two lines only for nonstat water level
      IF (JWLV1.GT.1) COMPDA(1,JWLV1) = 0.
      IF (JWLV3.GT.1) COMPDA(1,JWLV3) = 0.
   ENDIF
   IF (ICUR.GT.0) THEN
      COMPDA(1,JVX2) = 0.
      COMPDA(1,JVY2) = 0.
      IF (JVX1.GT.1) COMPDA(1,JVX1) = 0.
      IF (JVY1.GT.1) COMPDA(1,JVY1) = 0.
      IF (JVX3.GT.1) COMPDA(1,JVX3) = 0.
      IF (JVY3.GT.1) COMPDA(1,JVY3) = 0.
   ENDIF
   IF (VARFR) THEN
      COMPDA(1,JFRC2) = 0.
      COMPDA(1,JFRC3) = 0.
   ELSEIF (JFRC2.GT.1) THEN
      COMPDA(1,JFRC2) = 0.
   ENDIF
   IF (VARWI) THEN
      COMPDA(1,JWX2) = 0.
      COMPDA(1,JWY2) = 0.
      IF (JWX3.GT.1) COMPDA(1,JWX3) = 0.
      IF (JWY3.GT.1) COMPDA(1,JWY3) = 0.
   ENDIF
   IF (VARAST) THEN
      COMPDA(1,JASTD2) = 0.
      COMPDA(1,JASTD3) = 0.
   ENDIF
   IF (VARMUD) THEN
      COMPDA(1,JMUDL1) = 0.
      COMPDA(1,JMUDL2) = 0.
      COMPDA(1,JMUDL3) = 0.
   ENDIF
   IF (VARNPL) THEN
      COMPDA(1,JNPLA2) = 0.
      COMPDA(1,JNPLA3) = 0.
   ENDIF
   IF (VARTUR) THEN
      COMPDA(1,JTURB2) = 0.
      COMPDA(1,JTURB3) = 0.
   ENDIF
   IF (VARAICE) THEN
      COMPDA(1,JAICE2) = 0.
      COMPDA(1,JAICE3) = 0.
   ENDIF
   IF (VARHICE) THEN
      COMPDA(1,JHICE2) = 0.
      COMPDA(1,JHICE3) = 0.
   ENDIF
   IF (VARHSS) THEN
      COMPDA(1,JHSS2) = 0.
      COMPDA(1,JHSS3) = 0.
   ENDIF
   IF (VARTSS) THEN
      COMPDA(1,JTSS2) = 0.
      COMPDA(1,JTSS3) = 0.
   ENDIF
   IF (VARDSS) THEN
      COMPDA(1,JDSS2) = 0.
      COMPDA(1,JDSS3) = 0.
   ENDIF
   COMPDA(1,JBOTLV) = 0.

!     --- structured grid

   do IX = 1, MXC
      grid_rows: do IY = 1, MYC
         INDX = KGRPNT(IX,IY)
         IF (INDX.LE.0 .OR. INDX.GT.MCGRD) THEN
            CALL MSGERR (3, 'Grid error in subr. SWRBC')
            WRITE (PRINTF, "(' IX, IY, INDX, MCGRD: ', 4I7)") IX, IY, INDX, MCGRD
            CYCLE grid_rows
         ENDIF
         IF (INDX.EQ.1) CYCLE grid_rows

         XP = XCGRID(IX,IY)
         YP = YCGRID(IX,IY)

!         ***** compute depth and water level *****

         IF ( IBRAG.EQ.0 ) THEN
            DEP = SVALQI (XP, YP, 1, DEPTH, 1, IX, IY)
         ELSE
            DEP = dpmean(INDX)
         ENDIF
         COMPDA(INDX,JBOTLV) = DEP

         IF (VARWLV) THEN
            WLVL = SVALQI (XP, YP, 7, WLEVL, 1 ,IX ,IY)
            COMPDA(INDX,JWLV2) = WLVL
            IF (JWLV1.GT.1) COMPDA(INDX,JWLV1) = WLVL
            IF (JWLV3.GT.1) COMPDA(INDX,JWLV3) = WLVL
            DEP = DEP + WLVL
         ENDIF
!         add constant water level
         DEPW = DEP + WLEV
         COMPDA(INDX,JDP2) = DEPW
!         ***In this step the water level at T+DT is copied to the  ***
!         ***water level at T (Only for first time computation)     ***
         IF (JDP1.GT.1) COMPDA(INDX,JDP1) = DEPW
         IF (JDP3.GT.1) COMPDA(INDX,JDP3) = DEPW

!         ***** compute current velocity *****

         IF (ICUR.EQ.1 .AND. IGTYPE(2) .GE. 1) THEN
            IF (DEPW.GT.0.) THEN
               UU  = SVALQI (XP, YP, 2, UXB, 0 ,IX ,IY)
               VV  = SVALQI (XP, YP, 3, UYB, 0 ,IX ,IY)
               VTOT = SQRT (UU*UU + VV*VV)
               CGMAX = PNUMS(18)*SQRT(GRAV*DEPW)
               IF (VTOT .GT. CGMAX) THEN
                  CGFACT = CGMAX / VTOT
                  UU = UU * CGFACT
                  VV = VV * CGFACT
                  IF (ERRPTS.GT.0.AND.IAMMASTER) THEN
                     WRITE (ERRPTS, "(I4, 1X, I4, 1X, I2)") IX+MXF-1, IY+MYF-1, 1
                  ENDIF
               ENDIF
               COMPDA(INDX,JVX2) =  UU*COSVC + VV*SINVC
               COMPDA(INDX,JVY2) = -UU*SINVC + VV*COSVC
            ELSE
               COMPDA(INDX,JVX2) =  0.
               COMPDA(INDX,JVY2) =  0.
            ENDIF
            IF (JVX1.GT.1) COMPDA(INDX,JVX1) = COMPDA(INDX,JVX2)
            IF (JVY1.GT.1) COMPDA(INDX,JVY1) = COMPDA(INDX,JVY2)
            IF (JVX3.GT.1) COMPDA(INDX,JVX3) = COMPDA(INDX,JVX2)
            IF (JVY3.GT.1) COMPDA(INDX,JVY3) = COMPDA(INDX,JVY2)
         ENDIF

         IF (ITEST .GE. 100 .OR. INTES .GE. 30) THEN
            IF (ICUR .EQ. 1 ) THEN
               WRITE(PRINTF,"(1X,I5,2X,5(E11.4,2X))")KGRPNT(IX,IY),XP,YP,COMPDA(INDX,JDP2),&
               &COMPDA(INDX,JVX2),COMPDA(INDX,JVY2)
            ELSE
               WRITE(PRINTF,"(1X,I5,2X,3(E11.4,2X))")KGRPNT(IX,IY),XP,YP,COMPDA(INDX,JDP2)
            ENDIF
         ENDIF

!         ***** compute variable friction coefficient *****

         IF (VARFR) THEN
            FRI = SVALQI (XP, YP, 4, FRIC, 1 ,IX ,IY)
            COMPDA(INDX,JFRC2) = FRI
            COMPDA(INDX,JFRC3) = FRI
         ENDIF

!         ***** compute variable wind velocity *****

         IF (VARWI) THEN
            UU  = SVALQI (XP, YP, 5, WXI, 0 ,IX ,IY)
            VV  = SVALQI (XP, YP, 6, WYI, 0 ,IX ,IY)
            COMPDA(INDX,JWX2) =  UU*COSWC + VV*SINWC
            COMPDA(INDX,JWY2) = -UU*SINWC + VV*COSWC
            IF (JWX3.GT.1) COMPDA(INDX,JWX3) = COMPDA(INDX,JWX2)
            IF (JWY3.GT.1) COMPDA(INDX,JWY3) = COMPDA(INDX,JWY2)
         ENDIF

!     ***** compute variable air-sea temperature difference *****

         IF (VARAST) THEN
            ASTD = SVALQI (XP, YP, 10, ASTDF, 1 ,IX ,IY)
            COMPDA(INDX,JASTD2) = ASTD
            COMPDA(INDX,JASTD3) = ASTD
         ENDIF

!     ***** compute number of plants per square meter *****

         IF (VARNPL) THEN
            XNPL = SVALQI (XP, YP, 11, NPLAF, 1 ,IX ,IY)
            COMPDA(INDX,JNPLA2) = XNPL
            COMPDA(INDX,JNPLA3) = XNPL
         ENDIF

!     ***** compute turbulent viscosity *****

         IF (VARTUR) THEN
            IF (PTURBV(2).LT.0.) THEN
               XTUR = SVALQI (XP, YP, 12, TURBF, 0 ,IX ,IY)
            ELSE
               UU = COMPDA(INDX,JVX2)
               VV = COMPDA(INDX,JVY2)
               XTUR = PTURBV(2) * SQRT(UU*UU+VV*VV) * COMPDA(INDX,JDP2)
            ENDIF
            COMPDA(INDX,JTURB2) = XTUR
            COMPDA(INDX,JTURB3) = XTUR
         ENDIF

!     ***** compute fluid mud layer *****

         IF (VARMUD) THEN
            XMUD = SVALQI (XP, YP, 13, MUDLF, 1 ,IX ,IY)
            COMPDA(INDX,JMUDL1) = XMUD
            COMPDA(INDX,JMUDL2) = XMUD
            COMPDA(INDX,JMUDL3) = XMUD
         ENDIF

!     ***** compute ice concentration (fraction) *****

         IF (VARAICE) THEN
            XAICE = SVALQI (XP, YP, 14, AICEF, 1 ,IX ,IY)
            COMPDA(INDX,JAICE2) = XAICE
            COMPDA(INDX,JAICE3) = XAICE
         ENDIF

!     ***** compute ice thickness in meters *****

         IF (VARHICE) THEN
            XHICE = SVALQI (XP, YP, 15, HICEF, 1 ,IX ,IY)
            COMPDA(INDX,JHICE2) = XHICE
            COMPDA(INDX,JHICE3) = XHICE
         ENDIF

!     ***** compute sea-swell sig wave height *****

         IF (VARHSS) THEN
            XHSS = SVALQI (XP, YP, 16, HSSF, 1 ,IX ,IY)
            COMPDA(INDX,JHSS2) = XHSS
            COMPDA(INDX,JHSS3) = XHSS
         ENDIF

!     ***** compute sea-swell mean wave period *****

         IF (VARTSS) THEN
            XTSS = SVALQI (XP, YP, 17, TSSF, 1 ,IX ,IY)
            COMPDA(INDX,JTSS2) = XTSS
            COMPDA(INDX,JTSS3) = XTSS
         ENDIF

!     ***** compute sea-swell mean wave direction *****

         IF (VARDSS) THEN
            XDSS = SVALQI (XP, YP, 18, DSSF, 1 ,IX ,IY)
            COMPDA(INDX,JDSS2) = XDSS
            COMPDA(INDX,JDSS3) = XDSS
         ENDIF

      end do grid_rows
   end do

!     --- unstructured grid

   DO INDX = 1, nverts

      XP = xcugrd(INDX)
      YP = ycugrd(INDX)

      IF ( .NOT.PARLL ) THEN
         JVERT = INDX
      ELSE
         JVERT = ivertg(INDX)
      ENDIF

!        ***** compute depth and water level *****

      IF ( IGTYPE(1).EQ.3 ) THEN
         DEP = DEPTH(JVERT)
      ELSE
         IF ( IBRAG.EQ.0 ) THEN
            DEP = SVALQI (XP, YP, 1, DEPTH, 1, 0, 0)
         ELSE
            DEP = dpmean(INDX)
         ENDIF
      ENDIF
      COMPDA(INDX,JBOTLV) = DEP

      IF (VARWLV) THEN
         IF ( IGTYPE(7).EQ.3 ) THEN
            WLVL = WLEVL(JVERT)
         ELSE
            WLVL = SVALQI (XP, YP, 7, WLEVL, 1, 0, 0)
         ENDIF
         COMPDA(INDX,JWLV2) = WLVL
         IF (JWLV1.GT.1) COMPDA(INDX,JWLV1) = WLVL
         IF (JWLV3.GT.1) COMPDA(INDX,JWLV3) = WLVL
         DEP = DEP + WLVL
      ENDIF
!        add constant water level
      DEPW = DEP + WLEV
      COMPDA(INDX,JDP2) = DEPW
!        ***In this step the water level at T+DT is copied to the  ***
!        ***water level at T (Only for first time computation)     ***
      IF (JDP1.GT.1) COMPDA(INDX,JDP1) = DEPW
      IF (JDP3.GT.1) COMPDA(INDX,JDP3) = DEPW

!        ***** compute current velocity *****

      IF (ICUR.EQ.1 .AND. IGTYPE(2) .GE. 1) THEN
         IF (DEPW.GT.0.) THEN
            IF ( IGTYPE(2).EQ.3 ) THEN
               UU = UXB(JVERT)
            ELSE
               UU = SVALQI (XP, YP, 2, UXB, 0, 0, 0)
            ENDIF
            IF ( IGTYPE(3).EQ.3 ) THEN
               VV = UYB(JVERT)
            ELSE
               VV = SVALQI (XP, YP, 3, UYB, 0, 0, 0)
            ENDIF
            VTOT = SQRT (UU*UU + VV*VV)
            CGMAX = PNUMS(18)*SQRT(GRAV*DEPW)
            IF (VTOT .GT. CGMAX) THEN
               CGFACT = CGMAX / VTOT
               UU = UU * CGFACT
               VV = VV * CGFACT
               IF (ERRPTS.GT.0) THEN
                  WRITE (ERRPTS, "(I4, 1X, I2)") INDX, 1
               ENDIF
            ENDIF
            COMPDA(INDX,JVX2) =  UU*COSVC + VV*SINVC
            COMPDA(INDX,JVY2) = -UU*SINVC + VV*COSVC
         ELSE
            COMPDA(INDX,JVX2) =  0.
            COMPDA(INDX,JVY2) =  0.
         ENDIF
         IF (JVX1.GT.1) COMPDA(INDX,JVX1) = COMPDA(INDX,JVX2)
         IF (JVY1.GT.1) COMPDA(INDX,JVY1) = COMPDA(INDX,JVY2)
         IF (JVX3.GT.1) COMPDA(INDX,JVX3) = COMPDA(INDX,JVX2)
         IF (JVY3.GT.1) COMPDA(INDX,JVY3) = COMPDA(INDX,JVY2)
      ENDIF

!         ***** compute variable friction coefficient *****

      IF (VARFR) THEN
         IF ( IGTYPE(4).EQ.3 ) THEN
            FRI = FRIC(JVERT)
         ELSE
            FRI = SVALQI (XP, YP, 4, FRIC, 1, 0, 0)
         ENDIF
         COMPDA(INDX,JFRC2) = FRI
         COMPDA(INDX,JFRC3) = FRI
      ENDIF

!        ***** compute variable wind velocity *****

      IF (VARWI) THEN
         IF ( IGTYPE(5).EQ.3 ) THEN
            UU = WXI(JVERT)
         ELSE
            UU = SVALQI (XP, YP, 5, WXI, 0, 0, 0)
         ENDIF
         IF ( IGTYPE(6).EQ.3 ) THEN
            VV = WYI(JVERT)
         ELSE
            VV = SVALQI (XP, YP, 6, WYI, 0, 0, 0)
         ENDIF
         COMPDA(INDX,JWX2) =  UU*COSWC + VV*SINWC
         COMPDA(INDX,JWY2) = -UU*SINWC + VV*COSWC
         IF (JWX3.GT.1) COMPDA(INDX,JWX3) = COMPDA(INDX,JWX2)
         IF (JWY3.GT.1) COMPDA(INDX,JWY3) = COMPDA(INDX,JWY2)
      ENDIF

!     ***** compute variable air-sea temperature difference *****

      IF (VARAST) THEN
         IF ( IGTYPE(10).EQ.3 ) THEN
            ASTD = ASTDF(JVERT)
         ELSE
            ASTD = SVALQI (XP, YP, 10, ASTDF, 1, 0, 0)
         ENDIF
         COMPDA(INDX,JASTD2) = ASTD
         COMPDA(INDX,JASTD3) = ASTD
      ENDIF

!     ***** compute number of plants per square meter *****

      IF (VARNPL) THEN
         IF ( IGTYPE(11).EQ.3 ) THEN
            XNPL = NPLAF(JVERT)
         ELSE
            XNPL = SVALQI (XP, YP, 11, NPLAF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JNPLA2) = XNPL
         COMPDA(INDX,JNPLA3) = XNPL
      ENDIF

!     ***** compute turbulent viscosity *****

      IF (VARTUR) THEN
         IF (PTURBV(2).LT.0.) THEN
            IF ( IGTYPE(12).EQ.3 ) THEN
               XTUR = TURBF(JVERT)
            ELSE
               XTUR = SVALQI (XP, YP, 12, TURBF, 0 ,0, 0)
            ENDIF
         ELSE
            UU = COMPDA(INDX,JVX2)
            VV = COMPDA(INDX,JVY2)
            XTUR = PTURBV(2) * SQRT(UU*UU+VV*VV) * COMPDA(INDX,JDP2)
         ENDIF
         COMPDA(INDX,JTURB2) = XTUR
         COMPDA(INDX,JTURB3) = XTUR
      ENDIF

!     ***** compute fluid mud layer *****

      IF (VARMUD) THEN
         IF ( IGTYPE(13).EQ.3 ) THEN
            XMUD = MUDLF(JVERT)
         ELSE
            XMUD = SVALQI (XP, YP, 13, MUDLF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JMUDL1) = XMUD
         COMPDA(INDX,JMUDL2) = XMUD
         COMPDA(INDX,JMUDL3) = XMUD
      ENDIF

!     ***** compute aice, ice concentration (fraction)  *****

      IF (VARAICE) THEN
         IF ( IGTYPE(14).EQ.3 ) THEN
            XAICE = AICEF(JVERT)
         ELSE
            XAICE = SVALQI (XP, YP, 14, AICEF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JAICE2) = XAICE
         COMPDA(INDX,JAICE3) = XAICE
      ENDIF

!     ***** compute hice, ice thickness (m)  *****

      IF (VARHICE) THEN
         IF ( IGTYPE(15).EQ.3 ) THEN
            XHICE = HICEF(JVERT)
         ELSE
            XHICE = SVALQI (XP, YP, 15, HICEF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JHICE2) = XHICE
         COMPDA(INDX,JHICE3) = XHICE
      ENDIF


!     ***** compute sea-swell significant wave height  *****

      IF (VARHSS) THEN
         IF ( IGTYPE(16).EQ.3 ) THEN
            XHSS = HSSF(JVERT)
         ELSE
            XHSS = SVALQI (XP, YP, 16, HSSF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JHSS2) = XHSS
         COMPDA(INDX,JHSS3) = XHSS
      ENDIF

!     ***** compute sea-swell mean wave period  *****

      IF (VARTSS) THEN
         IF ( IGTYPE(17).EQ.3 ) THEN
            XTSS = TSSF(JVERT)
         ELSE
            XTSS = SVALQI (XP, YP, 17, TSSF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JTSS2) = XTSS
         COMPDA(INDX,JTSS3) = XTSS
      ENDIF

!     ***** compute sea-swell mean wave direction  *****

      IF (VARDSS) THEN
         IF ( IGTYPE(18).EQ.3 ) THEN
            XDSS = DSSF(JVERT)
         ELSE
            XDSS = SVALQI (XP, YP, 18, DSSF, 1 ,0, 0)
         ENDIF
         COMPDA(INDX,JDSS2) = XDSS
         COMPDA(INDX,JDSS3) = XDSS
      ENDIF
   ENDDO

!     *** initialise setup and saved depth ***

   IF (LSETUP.GT.0) THEN
      DO INDX = 1, MCGRD
         COMPDA(INDX,JSETUP) =  0.
         COMPDA(INDX,JDPSAV) = COMPDA(INDX,JDP2)
      ENDDO
   ENDIF

!     --- initialize HSIBC
   COMPDA(:,JHSIBC) = 0.

!     --- initialize ZELEN and USTAR
   COMPDA(:,JZEL  ) = 1.E-4
   COMPDA(:,JUSTAR) = 1.E-15

!     --- initialize UBOT and TMBOT
   COMPDA(:,JUBOT) = 0.
   IF (JPBOT.GT.1) COMPDA(:,JPBOT) = 0.

!     --- initialize GAMBR
   IF (ISURF.EQ.6.OR.ISURF.EQ.7) COMPDA(:,JGAMMA) = PSURF(2)

   RETURN
! * end of subroutine SWRBC *
end subroutine SWRBC
!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *
!************************************************************************
!                                                                      *
!************************************************************************
!                                                                      *
SUBROUTINE WRTEST (NAME, NA, IARR, RARR)
!                                                                      *
!************************************************************************

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
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     ---
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     IARR(*)
!     NA
!     NAME*(*)
!     RARR(*)

   INTEGER   IARR(*), NA, II
   REAL      RARR(*)
   CHARACTER(LEN=*) :: NAME

!  8. Subroutines used
!
!     ---
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
!     ---
!
! 13. Source text

   WRITE (PRINTF, "(1X, A, 10(1X, I8))") NAME, (IARR(II), II=1,NA)
   WRITE (PRINTF, "(10(1X, E12.4))") (RARR(II), II=1,NA)
   RETURN
! * end of subroutine WRTEST *
end subroutine WRTEST
!********************************************************************

SUBROUTINE ERRCHK
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STRACE

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind
   USE swan_input_grids
   USE SWCOMM3
   USE swan_propagation_scheme
   USE swan_spherical_geometry
   USE M_GENARR


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
!     30.60: Nico Booij
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     33.08: Nico Booij and Erick Rogers (changes re: the S&L scheme)
!     33.10: Nico Booij and Erick Rogers (changes re: the SORDUP scheme)
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.53: Andre van der Westhuysen
!     40.80: Marcel Zijlema
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!
!  1. Updates
!
!     30.60, Aug. 97: full common included, ICOND initialized (3d gen, nonstat)
!     30.60, Aug. 97: error message changed into warning
!     30.70, Oct. 97: ICOND made 1 only if it is 0
!     30.72, Feb. 98: Old messages deleted. Problems with quadruplets and
!                     SECTOR described. All change of options by this routine
!                     deleted
!     30.72, Mar. 98: Warning added for combination of no WIND and QUAD
!     30.72, Mar. 98: Added warning concerning TRIADS and MSC
!     40.00, Apr. 99: check whether size of pool is sufficient for computation
!                     and output
!     40.08, Mar. 03: "GE" changed to "EQ" and warning message related to
!                     curvilinear coordinates added
!     40.31, Dec. 03: removing POOL mechanism
!     40.41, Jul. 04: added warning concerning frequency-resolution and
!                     added check of resonance condition for triads
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.53, Mar. 05: change Alves and Banner parameters in case of XNL
!     40.80, Oct. 07: determine default setting for stopping criterion
!     41.90, Nov. 21: adding QC scattering
!
!  2. Purpose
!
!     Check all possible combinations of physical processes if
!     they are being activated and change value of settings if
!     necessary
!
!  3. Method
!
!     0      MESSAGE
!     1      WARNING
!     2      ERROR REPAIRABLE
!     3      SEVERE ERROR (calculation continues, however problems
!                          may arise)
!     4      TERMINATION ERROR (calculation is terminated )
!
!  4. Argument variables (updated 30.72)
!
!  6. Local variables
!
!     MSGSTR:     string to pass message to call MSGERR

   CHARACTER(LEN=80) MSGSTR

!  8. Subroutines used
!
!     MSGERR : Handles error messages according to severity
!
!  9. Subroutines calling
!
!     SWANCOM
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
!     ------------------------------------------------------------
!     End of the subroutine ERRCHK
!     ------------------------------------------------------------
!
! 13. Source text

   INTEGER :: II
   INTEGER, SAVE :: IENT = 0
   REAL :: GAMMA
   IF (LTRACE) CALL STRACE (IENT,'ERRCHK')

!     --- choose scheme for stationary or nonstationary computation

   IF (NSTATC.EQ.1) THEN
      PROPSC = PROPSN
   ELSE
      PROPSC = PROPSS
   ENDIF

!     --- reset PWIND(9) and PWIND(17) if RHO has a user value

   PWIND( 9) = PWIND(16)/RHO
   PWIND(17) = RHO

!     -----------------------------------------------------------------
!
!     *** WARNINGS AND ERROR MESSAGES ***
!
!     -----------------------------------------------------------------
!
!     check 4th generation model
   IF (IGEN.EQ.4) THEN
      IF (IWIND.NE.0 .OR. IQUAD.NE.0 .OR. IWCAP.NE.0) THEN
         CALL MSGERR (0, 'the deep water physics are excluded from')
         CALL MSGERR (0, 'the fourth-generation mode')
      ENDIF
      IWIND = 0
      IQUAD = 0
      IWCAP = 0
      PNUMS(20) = 1.E20
      IF (PWTAIL(1).LT.1.E8) THEN
         CALL MSGERR (1, 'the shape of the spectral tail cannot be')
         CALL MSGERR (1, 'specified as it is not fixed')
      ENDIF
      PWTAIL(1)    = 1.E8
      PWTAIL(2:10) = 0.
      IF ( KSPHER.GT.0 ) CALL MSGERR (3,&
      &'spherical coordinates not supported '//&
      &'in case of fourth-generation mode')
      IF ( LSETUP.GT.0 ) THEN
         CALL MSGERR (1, 'wave-induced setup has not been tested')
         CALL MSGERR (1, 'within the fourth-generation mode')
      ENDIF
      IF ( IDIFFR.GT.0 ) THEN
         CALL MSGERR (0, 'the phase-decoupled diffraction cannot be')
         CALL MSGERR (0, 'applied in case of fourth-generation mode')
      ENDIF
      IDIFFR = 0
   ENDIF

!     *** Check formulation for whitecapping ***

   IF ( IWCAP.GT.8 ) THEN
      WRITE (MSGSTR, '(A,I2,A)')&
      &'Unknown method for whitecapping (IWCAP=',IWCAP,')'
      CALL MSGERR( 1, TRIM(MSGSTR) )
      CALL MSGERR( 1,&
      &'Whitecapping according to Komen et al. (1984) will be used')
      IWCAP     = 1
      PWCAP(1)  = 2.36E-5
      PWCAP(2)  = 3.02E-3
      PWCAP(9)  = 2.
      PWCAP(10) = 1.
      PWCAP(11) = 1.
   ENDIF

!     *** WAM cycle 3 physics ***

   IF  ( (IWIND .EQ. 3 .OR. IWIND .EQ. 5) .AND. IWCAP .NE. 1&
   &.AND. IWCAP.NE.7&
   &) THEN
      CALL MSGERR(1,'Activate whitecapping mechanism according to')
      CALL MSGERR(1,'Komen et al. (1984) for wind option G3/YAN')
   ENDIF

   IF  ( IWCAP.EQ.7 .AND. IWIND.NE.5 ) THEN
      CALL MSGERR(1,'Activate wind option Yan (1987) in case of')
      CALL MSGERR(1,'Alves & Banner (2003) white-capping method')
   END IF

   IF  ( IWCAP.EQ.8 .AND. IWIND.NE.8 ) THEN
      CALL MSGERR(1,'Babanin dissipation with non-Babanin input')
      CALL MSGERR(1,'has not been tested                       ')
   END IF
   IF  ( IWIND.EQ.8 .AND. IWCAP.NE.8 ) THEN
      CALL MSGERR(1,'Babanin input with non-Babanin dissipation')
      CALL MSGERR(1,'has not been tested                       ')
   END IF

!     *** WAM cycle 4 physics ***

   IF  ( IWIND .EQ. 4 .AND. IWCAP .NE. 2) THEN
      CALL MSGERR(1,'Activate whitecapping mechanism according to')
      CALL MSGERR(1,'Janssen (1991) for wind option JANS        ')
   ENDIF

!     check QC scattering without ambient current
   IF ( ICUR.EQ.0 .AND. IQCM.GT.1 ) THEN
      CALL MSGERR(1,'Wave-current interaction is deactivated '//&
      &'in case of zero current')
      IQCM = 1
   ENDIF

   IF ( IQCM.NE.0 .AND. IDIFFR.GT.0 ) THEN
      CALL MSGERR (1, 'the phase-decoupled diffraction should not')
      CALL MSGERR (1, 'be activated in case of QC scattering')
      IDIFFR = 0
   ENDIF

!     check refraction scheme in case of QC scattering
   IF ( IQCM.EQ.1 .AND. INT(PNUMS(17)).EQ.-1 ) THEN
      CALL MSGERR (1, 'numerical scheme for refraction should not')
      CALL MSGERR (1, 'be activated in case of QC scattering')
      IREFR = 0
   ENDIF

!     check numerical scheme for transport in frequency space
   IF ( INT(PNUMS(8)).EQ.-999 ) THEN
      IF ( IQCM.EQ.0 ) THEN
         PNUMS(8) = 1.
      ELSE
         PNUMS(8) = 0.
      ENDIF
   ELSE
      IF ( IQCM.NE.0 ) THEN
         CALL MSGERR&
         &(1, 'numerical scheme for transport in frequency space')
         CALL MSGERR&
         &(1, 'should not be activated in case of QC scattering')
         PNUMS(8) = 0.
         ITFRE = 0
      ENDIF
   ENDIF

!     *** check option numerical scheme in presence of a current ***

   IF ( ICUR .EQ. 1 .AND. IQCM.EQ.0 ) THEN
      IF ( PNUMS(6) .EQ. 0. ) THEN
         CALL MSGERR(1,'In presence of a current it is recommended to')
         CALL MSGERR(1,'use an implicit upwind scheme in theta space ')
         CALL MSGERR(1,'-> set CDD = 1.')
         WRITE(PRINTF,*)
      ENDIF
      IF ( PNUMS(7) .EQ. 0. ) THEN
         CALL MSGERR(1,'In presence of a current it is recommended to')
         CALL MSGERR(1,'use an implicit upwind scheme in sigma space ')
         CALL MSGERR(1,'-> set CSS = 1.')
         WRITE(PRINTF,*)
      ENDIF
   END IF

!     check absolute stopping criterion
   IF ( .NOT. PNUMS(2).NE.-1. ) THEN
      IF ( IQCM.EQ.0 ) THEN
         IF ( ITRIAD.NE.3 ) THEN
            PNUMS(2) = 0.005
         ELSE
            PNUMS(2) = 0.01
         ENDIF
      ELSE
         PNUMS(2) = 0.05
      ENDIF
   ENDIF

!     check transfer function for triads, if appropriate

   IF ( ITRIAD.GT.0 ) THEN
      IF ( .NOT. PTRIAD(10).NE.-1. ) THEN
         IF (ITRIAD.EQ.2 .OR. ITRIAD.EQ.11) THEN
!              SPB or original LTA: use Madsen and Sorensen (1993)
            PTRIAD(10) = 2.
         ELSE
!              otherwise use QuadWave of Akrish et al (2024)
            PTRIAD(10) = 4.
         ENDIF
      ENDIF
   ENDIF

!     *** check vegetation Jacobsen in presence of a current ***

   IF ( ICUR .EQ. 1 ) THEN
      IF ( IVEG .EQ. 2 ) THEN
         CALL MSGERR(1,'Current effects are not included in')
         CALL MSGERR(1,'vegetation approach of Jacobsen et al (2019)')
      ENDIF
   END IF

   IF ( LSPNAR .AND. BRESCL ) THEN
      CALL MSGERR(1,'Rescaling is turned off since the directional')
      CALL MSGERR(1,'resolution is too coarse to represent energy')
      CALL MSGERR(1,'distribution properly, which deteriorates')
      CALL MSGERR(1,'the act of rescaling.')
      CALL MSGERR(1,'Also full upwind scheme in theta space is set.')
      BRESCL   = .FALSE.
      PNUMS(6) = 1.
   ENDIF

!     check combination of REPeating option and grid type and dimension

   IF (KREPTX.GT.0) THEN
!       --- "GE" changed to "EQ" since OPTG.EQ.3 FOR CURVILINEAR
      IF (OPTG.EQ.3)&
      &CALL MSGERR (3, 'Curvilinear grid cannot be REPeating')
      IF (OPTG.EQ.5)&
      &CALL MSGERR (3, 'Unstructured grid cannot be REPeating')
      IF (PROPSC.EQ.1 .AND. MXC.LT.1)&
      &CALL MSGERR (3, 'MXC must be >=1 for REPeating option')
      IF (PROPSC.EQ.2 .AND. MXC.LT.2)&
      &CALL MSGERR (3, 'MXC must be >=2 for REPeating option')
      IF (PROPSC.EQ.3 .AND. MXC.LT.3)&
      &CALL MSGERR (3, 'MXC must be >=3 for REPeating option')
   ENDIF

   IF (PROPSC.EQ.2 .AND. NSTATC.GT.0) THEN
      CALL MSGERR (3, 'SORDUP scheme only in stationary run')
   ENDIF
   IF (PROPSC.EQ.3 .AND. NSTATC.EQ.0) THEN
      CALL MSGERR (3, 'S&L scheme not in stationary run')
   ENDIF

!     --- A warning about curvilinear and S&L scheme
   IF ((PROPSC.EQ.3).AND.(OPTG.EQ.3)) THEN
      CALL MSGERR(1,'the S&L scheme (higher order nonstationary')
      CALL MSGERR(1,'IS NOT fully implemented for curvilinear')
      CALL MSGERR(1,'coordinates. This may or may not be noticeable')
      CALL MSGERR(1,'in simulations. DX and DY are approximated')
      CALL MSGERR(1,'with DX and DY of two nearest cells.')
      CALL MSGERR(1,'Note that SORDUP (higher order stationary)')
      CALL MSGERR(1,'IS fully implemented for curvilinear coord.')
      CALL MSGERR(1,'and differences are usually negligible.')
   ENDIF

!     Here the various problems with quadruplets are checked

   IF (ICUR.NE.0 .AND. (IQUAD.EQ.1 .OR. IQUAD.EQ.2)) THEN
      CALL MSGERR(0,'In presence of a current it is recommended to')
      CALL MSGERR(0,'update quadruplets per iteration instead of')
      CALL MSGERR(0,'per sweep. This will, however, increase the')
      CALL MSGERR(0,'amount of internal memory with a factor 2.')
   ENDIF

   IF (IWIND.EQ.3 .OR. IWIND.EQ.4&
   &.OR. IWIND.EQ.8&
   &) THEN
      IF (IQUAD .EQ. 0) THEN
         CALL MSGERR(2,'Quadruplets should be activated when SWAN  ')
         CALL MSGERR(2,'is running in a third generation mode and  ')
         CALL MSGERR(2,'wind is present                            ')
      ENDIF
   ENDIF

!     The combination of quadruplets and sectors is an error
!     in the calculation of quadruplets when the SECTOR option is
!     used in the CGRID command. This error should be corrected
!     in the future

   IF (IQUAD .GE. 1) THEN

      IF (.NOT. FULCIR) THEN
         IF ((SPDIR2-SPDIR1) .LT. (PI/12.)) THEN
            CALL MSGERR(2,'A combination of using quadruplets with a'    )
            CALL MSGERR(2,'sector of less than 30 degrees should be'     )
            CALL MSGERR(2,'avoided at all times, it is likely to produce')
            CALL MSGERR(2,'unreliable results and unexpected errors.'    )
            CALL MSGERR(2,'Refer to the manual (CGRID) for details'      )
         ELSE
            CALL MSGERR(1,'It is not recommended to use quadruplets'     )
            CALL MSGERR(1,'in combination with calculations on a sector.')
            CALL MSGERR(1,'Refer to the manual (CGRID) for details'      )
         END IF
      END IF

      IF (IWIND.EQ.0) THEN
         CALL MSGERR(2,'It is not recommended to use quadruplets'     )
         CALL MSGERR(2,'in combination with zero wind conditions.'    )
      END IF

      IF (MSC .EQ. 3 ) THEN
         CALL MSGERR(4,'Do not activate quadruplets for boundary ')
         CALL MSGERR(4,'option BIN -> use other option           ')
         RETURN
      END IF

   END IF

!     check parameters for Bragg scattering

   IF ( IBRAG.NE.0 ) THEN
      IF ( IGTYPE(1).NE.1 ) THEN
         CALL MSGERR (2,'A regular bottom grid is required')
         CALL MSGERR (2,'in case of Bragg scattering'      )
      ELSE
         IF (PBRAG(1).GT.0.5*MAX(MXG(1),MYG(1))) THEN
            CALL MSGERR(2,'region size over which a bottom spectrum')
            CALL MSGERR(2,'needs to be computed is too large'       )
         ENDIF
      ENDIF
   ENDIF

!     check whether limiter should be de-activated

   IF (IQUAD.EQ.0 .AND. PNUMS(20).LT.100.) THEN
      CALL MSGERR(1,&
      &'Limiter is de-activated in case of no quadruplets')
      PNUMS(20) = 1.E+20
   END IF

!     check resolution in frequency-space when DIA is used

   IF (IQUAD.GT.0 .AND. IQUAD.LE.3 .OR. IQUAD.EQ.8) THEN
      GAMMA = EXP(ALOG(SHIG/SLOW)/REAL(MSC-1))
      IF (ABS(GAMMA-1.1).GT.0.055) THEN
         CALL MSGERR(1,&
         &'relative frequency resolution (df/f) deviates more')
         CALL MSGERR(1,&
         &'than 5% from 10%-resolution. This may be problematic')
         CALL MSGERR(1,&
         &'when quadruplets are approximated by means of DIA.')
      END IF
   END IF

!     When Alves and Banner and XNL are applied change the parameters

   IF ( IWCAP.EQ.7 .AND. (IQUAD.EQ.51 .OR. IQUAD.EQ.52 .OR.&
   &IQUAD.EQ.53) ) THEN
      IF (EQREAL(PWCAP( 1), 5.0E-5)) PWCAP( 1) = 5.0E-5
      IF (EQREAL(PWCAP(12),1.75E-3)) PWCAP(12) = 1.95E-3
   END IF

!     check stopping criterion in case of 4th generation model
   IF (IGEN.EQ.4) THEN
      IF (PNUMS(21).EQ.0.) THEN
         CALL MSGERR(1,'command NUM ACCUR is obsolete')
         CALL MSGERR(0,'default stopping criterion is used instead')
         PNUMS(21) = 1.
         PNUMS(1)  = 0.01
         PNUMS(2)  = 0.05
         PNUMS(4)  = 99.
      ENDIF
   ENDIF

   IF ( ITEST .GE. 120 ) THEN
      WRITE(PRINTF,"(' ERRCHK : IWIND QUAD CUR WCAP MSC : ',5I4)") IWIND ,IQUAD, ICUR, IWCAP, MSC
      IF (IWIND .GT. 0) THEN
         DO II = 1, MWIND
            WRITE(PRINTF,"(' PWIND(',I2,') = ',E11.4)") II,PWIND(II)
         ENDDO
      ENDIF
   END IF

   RETURN
!     end of subroutine ERRCHK
end subroutine ERRCHK
!*********************************************************************
!                                                                    *
SUBROUTINE SNEXTI (BSPECS, BGRIDP, COMPDA, AC1   , AC2   ,&
&SPCSIG, SPCDIR, XCGRID, YCGRID, KGRPNT,&
&XYTST , DEPTH , WLEVL , FRIC  , UXB   ,&
&UYB   , NPLAF , TURBF , MUDLF , WXI   ,&
&AICEF , HICEF , HSSF  , TSSF  , DSSF  ,&
&WYI   )
   USE swan_field_file_update, ONLY: FLFILE
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_service_interfaces, ONLY: STPNOW, STRACE
   USE swan_input_interpolation, ONLY: SVALQI
!                                                                    *
!*********************************************************************

   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind
   USE swan_boundary_counters
   USE swan_input_grids
   USE swan_input_field_files
   USE SWCOMM3
   USE swan_test_output
   USE swan_propagation_scheme
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanGriddata


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
!     30.60: Nico Booij
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     40.00, 40.13: Nico Booij
!     34.01: Jeroen Adema
!     40.14: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     30.60, Jun. 97: condition for ATAN2 corrected
!     30.70, Sept 97: reduction of current only if depth is positive
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.72, Oct. 97: changed floating point comparison to avoid equality
!                     comparisons
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.70, Jan. 98: VNAM6 (nonstat current) corrected
!     30.70, Feb. 98: argument AUXW4 added in call of WAM nesting
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: wind is no longer set to 0, id depth is negative
!     40.00, Nov. 97: complete revision of boundary value update
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description of several variables
!     34.01, Feb. 99: Introducing STPNOW
!     40.13, Mar. 01: Loop over INDX replaced by loop over IX, IY
!     40.14, Jun. 01: Waterlevel updated in case set-up is on
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Updates boundary conditions and input fields
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
! i   BGRIDP: data for interpolating to computational grid points
! i   KGRPNT: computational grid point addresses
! i   XYTST : test points

   INTEGER  BGRIDP(*), XYTST(*), KGRPNT(MXC,MYC)

! i   AC1   : action density spectra on old time level
! i   AC2   : action density spectra on new time level
! i   BSPECS: boundary spectra
! i   COMPDA: values on computational grid
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

   REAL     AC1(MDC,MSC,MCGRD)
   REAL     AC2(MDC,MSC,MCGRD)
   REAL     BSPECS(MDC,MSC,NBSPEC,2)
   REAL     COMPDA(MCGRD,MCMVAR)
   REAL     SPCDIR(MDC,6)
   REAL     SPCSIG(MSC)
   REAL     XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   REAL     DEPTH(*), WLEVL(*), FRIC(*), UXB(*), UYB(*),&
   &NPLAF(*), TURBF(*),&
   &MUDLF(*),&
   &AICEF(*), HICEF(*),&
   &HSSF(*), TSSF(*), DSSF(*),&
   &WXI(*), WYI(*)

!     TIMCO ..... Time (date) of computation
!     TFINC ..... Final time (date) of computation
!     DT    ..... Increment time for computation
!     TIMCU ..... Date to read the next current file.
!     TIMFR .....        "              friction
!     TIMWI .....        "              wind
!     TIMWL .....        "              water level
!     WEI??#..... Weights for linear interpolation for
!                 (??=) CUR, FRC, WIN, WLV,
!                 (#=) 2 for field at Ti and 1 for field at Ti+1
!     VARWE?..... Variation of the WEI??? in each DT for C, F, W , L
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     SWBROADC


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
!     -------------------------------------------------------------
!     ------------------------------------------------------------
!
! 13. Source text

   INTEGER :: IERR
   INTEGER :: IBFILE, IBGRID, ID, IDD, II, INDX, INDXGR, IS, ISS
   INTEGER :: IVP, IX, IXP, IXY, IY, IYP, JVERT, K1, K2
   REAL        TSTVAL(10)
   REAL :: AA, AADD, ADEG, APER, ASADD, ASTOT, ATOT, AX, AY
   REAL :: CGFACT, CGMAX, DEP, DEPW, ETOT, HS, SIG, SIG2
   REAL :: UU, VTOT, VV, W1, W2, WLVL, XP, YP
   TYPE(BSPCDAT), POINTER :: CURBFL
   LOGICAL LPB
   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE(IENT,'SNEXTI')


!     **   All action densities are shifted from array T+DT
!     **   to the array at time T

   IF (NSTATC.EQ.1) THEN
      IF (ITERMX.GT.1&
      &.OR. PROPSC.EQ.3&
      &) THEN
         do IXY = 1, MCGRD
            do ISS = 1, MSC
               do IDD = 1, MDC
                  AC1(IDD,ISS,IXY) = AC2(IDD,ISS,IXY)
               end do
            end do
         end do
      ENDIF
   ENDIF

!     --- update boundary conditions

   IF (INODE.EQ.MASTER) THEN
      IF (ITEST.GE.80) WRITE (PRTEST,*) ' number of boundary files ',&
      &NBFILS
      CURBFL => FBNDFIL
      DO IBFILE = 1, NBFILS

!          --- read values from boundary file, and interpolate in time

         CALL RBFILE ( SPCSIG, SPCDIR,&
         &CURBFL%BFILED, CURBFL%BSPLOC,&
         &CURBFL%BSPDIR, CURBFL%BSPFRQ,&
         &BSPECS, XYTST )
         IF (STPNOW()) RETURN
         IF (.NOT.ASSOCIATED(CURBFL%NEXTBSPC)) EXIT
         CURBFL => CURBFL%NEXTBSPC

      END DO
   END IF

!     --- scatter array BSPECS to all nodes

   CALL SWBROADC ( BSPECS, 2*MDC*MSC*NBSPEC )
   IF (STPNOW()) RETURN

!     --- determine spectra on boundary points of grid

   IF ( NBGRPT.GT.0 ) THEN
      IF (ITEST.GE.80) WRITE(PRTEST,*) ' number of boundary points ',&
      &NBGRPT
      DO IBGRID = 1, NBGRPT
         INDXGR = BGRIDP(6*IBGRID-5)
         IF (BGRIDP(6*IBGRID-4).EQ.1) THEN
!            obtain spectrum in boundary point from interpolation in space
            W1 = 0.001 * REAL(BGRIDP(6*IBGRID-3))
            K1 = BGRIDP(6*IBGRID-2)
            W2 = 1.-W1
            K2 = BGRIDP(6*IBGRID)
            CALL SINTRP (W1, W2, BSPECS(1,1,K1,1), BSPECS(1,1,K2,1),&
            &AC2(1,1,INDXGR), SPCDIR, SPCSIG)
!            --- store Hs from boundary condition in array HSOBND
            ETOT = 0.
            DO IS = 1, MSC
               SIG2 = SPCSIG(IS) ** 2
               DO ID = 1, MDC
                  ETOT = ETOT + SIG2 * AC2(ID,IS,INDXGR)
               ENDDO
            ENDDO
            IF (ETOT.GT.0.) THEN
               HS = 4. * SQRT(FRINTF*DDIR*ETOT)
            ELSE
               HS = 0.
            ENDIF
            COMPDA(INDXGR,JHSIBC) = HS

!            --- test output: parameters in test points on boundary

            IF (NPTST.GT.0) THEN
               DO IPTST = 1, NPTST
                  IF (OPTG.NE.5) THEN
                     IXP = XYTST(2*IPTST-1)
                     IYP = XYTST(2*IPTST)
                     LPB = INDXGR.EQ.KGRPNT(IXP,IYP)
                  ELSE
                     IVP = XYTST(IPTST)
                     LPB = INDXGR.EQ.IVP
                  ENDIF
                  IF ( LPB ) THEN
                     IF (OPTG.NE.5) THEN
                        WRITE(PRTEST,"(' boundary point', 3I8, 2(F8.3, I4))") IBGRID, IXP-1, IYP-1,&
                        &W1, K1, W2, K2
                     ELSE
                        WRITE(PRTEST,"(' boundary vertex', 2I8, 2(F8.3, I4))") IBGRID, IVP,&
                        &W1, K1, W2, K2
                     ENDIF
                     AX = 0.
                     AY = 0.
                     ATOT = 0.
                     ASTOT = 0.
                     DO ID = 1, MDC
                        AADD = 0.
                        ASADD = 0.
                        DO IS = 1, MSC
                           SIG = SPCSIG(IS)
                           AA  = SIG*AC2(ID,IS,INDXGR)
                           AADD = AADD + AA
                           ASADD = ASADD + SIG*AA
                        ENDDO
                        AX = AX + AADD * SPCDIR(ID,2)
                        AY = AY + AADD * SPCDIR(ID,3)
                        ATOT = ATOT + AADD
                        ASTOT = ASTOT + ASADD
                     ENDDO
                     IF (ASTOT.GT.0.) THEN
                        HS = 4. * SQRT(FRINTF*DDIR*ASTOT)
                        APER = PI2 * ATOT / ASTOT
                        ADEG = 180./PI * ATAN2(AY,AX)
                     ELSE
                        HS = 0.
                        APER = -999.
                        ADEG = -999.
                     ENDIF
                     WRITE (PRTEST, "(' Hs, Per, Dir: ', 3E12.4)") HS, APER, ADEG
                  END IF
               END DO
            END IF
         END IF
      END DO
   END IF

!     --- update input fields (wind, water level etc.)
!
!     fields 5 and 6: wind
   IF (IFLDYN(5) .EQ. 1) THEN
      CALL FLFILE ( 5, 6, WXI, WYI,&
      &0, JWX2, JWX3, 0, JWY2, JWY3,&
      &COSWC, SINWC,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' wind from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JWX3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JWX3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' X-comp: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JWY3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JWY3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' Y-comp: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 4: friction coeff.
   IF (IFLDYN(4) .EQ. 1) THEN
      CALL FLFILE ( 4, 0, FRIC, (/0./),&
      &0, JFRC2, JFRC3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' fric coeff from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JFRC3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JFRC3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") 'friction: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 7: water level
   IF (IFLDYN(7) .EQ. 1) THEN
      CALL FLFILE ( 7, 0, WLEVL, (/0./),&
      &JWLV1, JWLV2, JWLV3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
!       Add bottom level to obtain depth
!       structured grid
      do IX = 1, MXC
         do IY = 1, MYC
            INDX = KGRPNT(IX,IY)
            IF (INDX.GT.1) THEN
               XP = XCGRID(IX,IY)
               YP = YCGRID(IX,IY)
               DEP = SVALQI (XP, YP, 1, DEPTH, 1 ,IX ,IY)
               COMPDA(INDX,JDP1) = COMPDA(INDX,JDP2)
               WLVL = COMPDA(INDX,JWLV2)
               DEPW = DEP + WLVL + WLEV
               COMPDA(INDX,JDP2) = DEPW
               IF (LSETUP.GT.0) THEN
                  COMPDA(INDX,JDPSAV) = COMPDA(INDX,JDP2)
               ENDIF
            ENDIF
         end do
      end do
!       unstructured grid
      DO INDX = 1, nverts
         XP = xcugrd(INDX)
         YP = ycugrd(INDX)
         IF (.NOT.PARLL) THEN
            JVERT = INDX
         ELSE
            JVERT = ivertg(INDX)
         ENDIF
         IF ( IGTYPE(1).EQ.3 ) THEN
            DEP = DEPTH(JVERT)
         ELSE
            DEP = SVALQI (XP, YP, 1, DEPTH, 1, 0, 0)
         ENDIF
         COMPDA(INDX,JDP1) = COMPDA(INDX,JDP2)
         WLVL = COMPDA(INDX,JWLV2)
         DEPW = DEP + WLVL + WLEV
         COMPDA(INDX,JDP2) = DEPW
         IF (LSETUP.GT.0) THEN
            COMPDA(INDX,JDPSAV) = COMPDA(INDX,JDP2)
         ENDIF
      ENDDO
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' water level from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JWLV3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JWLV3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' W-level: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 2 and 3: current velocity
   IF (IFLDYN(2) .EQ. 1) THEN
      CALL FLFILE ( 2, 3, UXB, UYB,&
      &JVX1, JVX2, JVX3, JVY1, JVY2, JVY3,&
      &COSVC, SINVC,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
!       reduce current velocity if Froude number is larger than PNUMS(18)
!       structured grid
      DO IX = 1, MXC
         DO IY = 1, MYC
            INDX = KGRPNT(IX,IY)
            IF (INDX.GT.1) THEN
               DEPW = COMPDA(INDX,JDP2)
               IF (DEPW.GT.0.) THEN
                  UU = COMPDA(INDX,JVX2)
                  VV = COMPDA(INDX,JVY2)
                  VTOT = SQRT (UU*UU + VV*VV)
                  CGMAX = PNUMS(18)*SQRT(GRAV*DEPW)
                  IF (VTOT .GT. CGMAX) THEN
                     CGFACT = CGMAX / VTOT
                     COMPDA(INDX,JVX2) = UU * CGFACT
                     COMPDA(INDX,JVY2) = VV * CGFACT
!                 write IX,IY to error points file
                     IF (ERRPTS.GT.0.AND.IAMMASTER) THEN
                        WRITE (ERRPTS, "(I4, 1X, I4, 1X, I2)") IX+MXF-1, IY+MYF-1, 1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
!       unstructured grid
      DO INDX = 1, nverts
         DEPW = COMPDA(INDX,JDP2)
         IF (DEPW.GT.0.) THEN
            UU = COMPDA(INDX,JVX2)
            VV = COMPDA(INDX,JVY2)
            VTOT = SQRT (UU*UU + VV*VV)
            CGMAX = PNUMS(18)*SQRT(GRAV*DEPW)
            IF (VTOT .GT. CGMAX) THEN
               CGFACT = CGMAX / VTOT
               COMPDA(INDX,JVX2) = UU * CGFACT
               COMPDA(INDX,JVY2) = VV * CGFACT
!                write INDX to error points file
               IF (ERRPTS.GT.0) THEN
                  WRITE (ERRPTS, "(I4, 1X, I2)") INDX, 1
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' current vel from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JVX3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JVX3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' X-comp: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JVY3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JVY3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' Y-comp: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 11: field containing number of plants per square meter
   IF (IFLDYN(11) .EQ. 1) THEN
      CALL FLFILE (11, 0, NPLAF, (/0./),&
      &0, JNPLA2, JNPLA3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' # plants/m2 from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JNPLA3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JNPLA3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' veg dens: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 12: field containing turbulent viscosity
   IF (IFLDYN(12) .EQ. 1) THEN
      CALL FLFILE (12, 0, TURBF, (/0./),&
      &0, JTURB2, JTURB3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' turb visc from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JTURB3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JTURB3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' turb visc: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 13: field containing fluid mud layer
   IF (IFLDYN(13) .EQ. 1) THEN
      CALL FLFILE (13, 0, MUDLF, (/0./),&
      &JMUDL1, JMUDL2, JMUDL3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' mud layer from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JMUDL3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JMUDL3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' mud layer: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 14: field containing ice concentration (fraction)
   IF (IFLDYN(14) .EQ. 1) THEN
      CALL FLFILE (14, 0, AICEF, (/0./),&
      &0, JAICE2, JAICE3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' ice frac. from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JAICE3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JAICE3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' ice frac.: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 15: field containing ice thickness in meters
   IF (IFLDYN(15) .EQ. 1) THEN
      CALL FLFILE (15, 0, HICEF, (/0./),&
      &0, JHICE2, JHICE3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' ice thick. from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JHICE3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JHICE3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' ice thick.: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 16: field containing sea-swell significant wave height
   IF (IFLDYN(16) .EQ. 1) THEN
      CALL FLFILE (16, 0, HSSF, (/0./),&
      &0, JHSS2, JHSS3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' sea-swell Hs from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JHSS3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JHSS3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' sea-swell Hs: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 17: field containing sea-swell mean wave period
   IF (IFLDYN(17) .EQ. 1) THEN
      CALL FLFILE (17, 0, TSSF, (/0./),&
      &0, JTSS2, JTSS3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST, '(A)') ' sea-swell Tm from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JTSS3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JTSS3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' sea-swell Tm: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     field 18: field containing sea-swell mean wave direction
   IF (IFLDYN(18) .EQ. 1) THEN
      CALL FLFILE (18, 0, DSSF, (/0./),&
      &0, JDSS2, JDSS3, 0, 0, 0,&
      &1., 0.,&
      &COMPDA, XCGRID, YCGRID,&
      &KGRPNT, IERR)
      IF (STPNOW()) RETURN
      IF (NPTST.GT.0) THEN
         WRITE (PRTEST,'(A)') ' sea-swell Dir from file in test points'
         IF (OPTG.NE.5) THEN
            DO IPTST = 1, MIN(10,NPTST)
               IXP = XYTST(2*IPTST-1)
               IYP = XYTST(2*IPTST)
               TSTVAL(IPTST) = COMPDA(KGRPNT(IXP,IYP),JDSS3)
            ENDDO
         ELSE
            DO IPTST = 1, MIN(10,NPTST)
               IVP = XYTST(IPTST)
               TSTVAL(IPTST) = COMPDA(IVP,JDSS3)
            ENDDO
         ENDIF
         WRITE (PRTEST, "(A, 10(1X,E11.4))") ' sea-swell Dir: ',&
         &(TSTVAL(IPTST), IPTST=1,MIN(10,NPTST))
      ENDIF
   ENDIF

!     End of subroutine SNEXTI

   RETURN
end subroutine SNEXTI

!****************************************************************

SUBROUTINE RBFILE (SPCSIG, SPCDIR, BFILED, BSPLOC,&
&BSPDIR, BSPFRQ, BSPECS, XYTST )
   USE swan_legacy_io, ONLY: INAR2D, COPYCH
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_boundary_counters
   USE swan_input_field_files
   USE SWCOMM3
   USE M_PARALL, ONLY: IAMMASTER

   IMPLICIT NONE(TYPE, EXTERNAL)
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
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.05: Ekaterini E. Kriezi
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.61: Roop Lalbeharry
!     41.13: Nico Booij
!     41.66: Erick Rogers
!
!  1. Updates
!
!     40.00, Nov. 97: new subroutine
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description of several variables
!     40.03, Nov. 99: after label 380 BFILED(15) is replaced by BFILED(14)
!            May  00: in calls of DTRETI now BFILED(6) is used as time option
!     40.05, Aug. 00: WW3 nesting and changes in the form of the code
!                     (use of f90 features), Revision of subroutine
!     40.02, Oct. 00: Avoided REWIND of uninitialised unit number NDSD
!     40.13, Apr. 01: GOTO 392 added for a single boundary file (case NDSL=0)
!     40.13, May  01: read heading lines in case of WAM free format file
!                     changed
!     40.31, Nov. 03: removing POOL-mechanism, reconsidering this subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.61, Nov. 06: variables USNEW, THWNEW no longer written in WAM4.5
!     41.13, Jul. 10: LWDATE introduced (length of date/time in WAM nest)
!     41.66, Dec. 16:  fix with respect to inefficient reading of WW3 nesting
!
!  2. Purpose
!
!     read boundary spectra from one file and additional information of
!     heading lines
!
!  3. Methode
!
!     read from boundary files, aditional information (like time),
!     form the head lines per time step, and the head lines per point spectrum,
!     read the spectrum of the boundary file.
!     Transform to spectral resolution used in SWAN to obtain boundary spectra.
!
!  4. Argument variables
!
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL,   INTENT(IN)     ::  SPCDIR(MDC,6)
   REAL,   INTENT(IN)     ::  SPCSIG(MSC)
   REAL,   INTENT(INOUT)  ::  BSPECS(MDC,MSC,NBSPEC,2)

!     BSPDIR: Spectral directions of input spectrum
!     BSPFRQ: Spectral frequencies of input spectrum

   REAL   , INTENT(INOUT)  :: BSPDIR(*)
   REAL   , INTENT(INOUT)  :: BSPFRQ(*)

!     BFILED  data concerning boundary condition files
!     BSPLOC  place in array BSPECS where to store interpolated spectra
!     XYTST   test points

   INTEGER, INTENT(INOUT)  ::  BFILED(*)
   INTEGER, INTENT(INOUT)  ::  BSPLOC(*)
   INTEGER, INTENT(IN)     ::  XYTST(*)

!  5. Parameter variables
!
!     --
!
!  6. Local variables

   INTEGER   DORDER, NDSL, NDSD, IHD, IBOUNC, IBSPEC, IERR, IOSTATUS
   INTEGER, SAVE :: IENT = 0
   INTEGER   ID, IS, JJ, NANG, NFRE, II
   INTEGER, SAVE :: COUNT_IT = 0
   INTEGER   WWDATE, WWTIME

!     DORDER    if <0, order of reading directions is reversed
!     NDSL      unit ref num for namelist file
!     NDSD      unit ref num for data file
!     IHD       counter for heading lines
!     IBOUNC    counter for boundary locations
!     IBSPEC    counter for spectra
!     ID        counter for directions
!     IS        counter for frequencies
!     JJ        counter
!     NANG      number of directions on file
!     NFRE      number of frequencies on file
!     COUNT_IT  counter of the time entering in the WW3 boundary file
!
!     WWDATE, WWTIME       time code in WaveWatch

   REAL      UFAC, W1, BDEPTH, DUM_A, RFAC
   REAL      XLON, XLAT, XDATE, EMEAN, THQ, FMEAN
   REAL(KIND=KIND(0.0D0))    TIMF1, TIMF2

!     TIMF1     time of reading old boundary condition
!     TIMF2     time of reading new boundary condition
!     UFAC      multiplication factor
!     W1        weighting coefficient used in interpolation
!     XLON      longitude
!     XLAT      latitude
!     XDATE     date-time read from WAM file
!     EMEAN     coefficient read from WAM file, ignored
!     THQ       coefficient read from WAM file, ignored
!     FMEAN     coefficient read from WAM file, ignored
!     DUM_A     dummy local variable (not used in any calculation)
!     BDEPTH    depth of the boundary points

   REAL(KIND=KIND(0.0D0)) DDATE
!     DDATE     date-time read from WAM file

   LOGICAL   NSTATF, UNFORM, READ_ERROR
!     NSTATF    if True time appears in bound. cond. file
!     UNFORM    if True reading is done unformatted

   CHARACTER(LEN=4)  :: BTYPE
   CHARACTER(LEN=80) :: HEDLIN
   CHARACTER(LEN=20) :: TIMSTR
   CHARACTER(LEN=18) :: DATITM(5)
   CHARACTER(LEN=10) :: PTNME
!     BTYPE     type of boundary condition
!     HEDLIN    heading line
!     TIMSTR    time string
   CHARACTER (LEN=14) :: CDATE     ! date-time

   REAL, ALLOCATABLE :: SPAUX(:,:)

!  8. SUBROUTINES USED
!
!     COPYCH, DTRETI, LSPLIT, SSHAPE, RESPEC, MSGERR, SINTRP
!
!  9. SUBROUTINES CALLING
!
!       SNEXTI
!
!  10. ERROR MESSAGES
!
!        ---
!
!  11. REMARKS
!
!
!  12. STRUCTURE
!
!       ---------------------------------------------------------
!       If file contains stationary wave data
!       Then If time2 < 0
!            Then Read boundary values from file
!                 Transform to spectral resolution used in SWAN to
!                 obtain boundary spectra
!                 Make time2 = + Inf
!            ----------------------------------------------------
!       Else Make time1 = timco - DT
!            Repeat
!                 If timco > time2
!                 Then Exit from repeat
!                 -----------------------------------------------
!                 Make time1 = time2
!                 Make old field values = new values
!                 Read new values from file
!            ----------------------------------------------------
!            Interpolate in time between old and new values
!            to update old values
!            Transform to spectral resolution used in SWAN to
!            obtain boundary spectra
!       ---------------------------------------------------------
!
! 13. SOURCE
!
!****************************************************************


   CALL STRACE (IENT, 'RBFILE')


!     if data file is exhausted, return
   IF (BFILED(1).EQ.-1) RETURN
   NSTATF = (BFILED(1) .GT. 0)
   IERR   = 0
   CALL COPYCH (BTYPE, 'F', BFILED(7), 1, IERR)

   IF (BTYPE.EQ.'WAMW' .OR. BTYPE.EQ.'WAMC'&
   &.OR. BTYPE.EQ.'WW3U') THEN
      UNFORM = .TRUE.
   ELSE
      UNFORM = .FALSE.
   ENDIF

   IF (BFILED(2).LT.0) COUNT_IT = 0

   DORDER = BFILED(9)
   TIMF1 = DBLE(BFILED(2))
   TIMF2 = DBLE(BFILED(3))
   IF (ITEST.GE.120) WRITE (PRINTF, "(' Boundary', I2, 2X, A, ' times: ', 3F10.1)") BFILED(1), BTYPE,&
   &TIMF1, TIMF2, default_time_context%TIMCO

   ALLOCATE(SPAUX(MDC,MSC))

!     if present time > time of last set of spectra, read new spectra
!
!     While Loop named GLOOP

   GLOOP : DO

      IF (default_time_context%TIMCO.GT.TIMF2) THEN

!     then read from boundary nesting files all the information
!     and the spectral ones
!
!     COUNT_IT : counter of the time which enter in a WW3F boundary file
!     and read spectral - used to calculate NHED (number of heading lines
!     per time) in WW3F case.

         COUNT_IT = COUNT_IT+1
         NDSL = BFILED(4)
         NDSD = BFILED(5)

!     REWIND statement removed. ER, Dec 21 2016
!         IF(BTYPE.EQ.'WW3F') REWIND(NDSD)

         TIMF1 = TIMF2

!         move new spectra to old for all boundary points
         DO IBOUNC = 1, BFILED(8)
            IBSPEC = BSPLOC(IBOUNC)

            DO ID = 1, MDC
               DO IS = 1, MSC
                  BSPECS(ID,IS,IBSPEC,1) = BSPECS(ID,IS,IBSPEC,2)
               ENDDO
            ENDDO

            IF (ITEST.GE.80) WRITE (PRTEST, *) ' spectrum moved ',&
            &IBSPEC, TIMF1
         ENDDO

         boundary_files: DO
         READ_ERROR = .FALSE.
         file_contents: BLOCK

!     BFILED(15) calculation changed: ER, Dec 21 2016
         IF (BTYPE.EQ.'WW3F') THEN
            IF (COUNT_IT.EQ.1)THEN
               BFILED(15) = BFILED(14)
            ELSE
               BFILED(15) = 0
            ENDIF
         ENDIF

!     read heading lines per time step
!
!     HBFL is loop over the number of heading lines per time step
         HBFL : DO IHD = 1, BFILED(15)
            IF (UNFORM) THEN
               READ (NDSD, IOSTAT=IOSTATUS)
            ELSEIF ((.NOT.UNFORM).AND.(BTYPE.EQ.'WW3F')) THEN
               READ (NDSD, '(A)', IOSTAT=IOSTATUS) HEDLIN
            ELSE
               READ (NDSD, '(A)', IOSTAT=IOSTATUS) HEDLIN
            ENDIF
            IF (IOSTATUS.NE.0) THEN
               IF (IS_IOSTAT_END(IOSTATUS)) EXIT file_contents
               CALL boundary_read_error()
               RETURN
            ENDIF
            IF (.NOT.UNFORM .AND. BTYPE.NE.'WW3F') THEN
               IF (ITEST.GE.90) WRITE (PRINTF, "(' heading line: ', A)") HEDLIN
               IF (BTYPE.EQ.'SWNT') THEN
!               convert time string to time in seconds
                  CALL DTRETI (HEDLIN(1:18), BFILED(6), TIMF2)
               ENDIF
            ENDIF
         ENDDO HBFL

         IF (.NOT.NSTATF) TIMF2 = 0D0
         IF (ITEST.GE.60) WRITE (PRINTF, "(' Boundary times ', 3F12.0, 2X, 4I4)")&
         &TIMF1, TIMF2, default_time_context%TIMCO, BFILED(8), BFILED(15)

!         read additional information from the headers and spectrum from
!         the nesting boundary files (for all the cases)
!
!
!       BP_LOOP loop over the boundary nesting points
         BP_LOOP : DO  IBOUNC = 1, BFILED(8)

            IBSPEC = BSPLOC(IBOUNC)
!           division by 2*PI to account for difference in definition of
!           Hz to rad/s
            NANG  = BFILED(10)

!           calculate UFAC for the different nesting cases
            IF (BTYPE(1:3).EQ.'SWN' .AND. NANG.GT.0) THEN
!           in addition multiply by 180/PI to account for directions in
!           instead of radians (SWAN 2D spectral files)
               UFAC = 180./ (2.*PI**2)
            ELSE
               UFAC = 1./ (2.*PI)
            ENDIF
!           divide by Rho*Grav if quantity in file is energy density
            IF ((BFILED(17).EQ.1))  UFAC = UFAC / (RHO*GRAV)

!           read information from heading lines per spectrum
!
!          do loop over the numbers of the heading lines per spectrum

            DO IHD = 1, BFILED(16)

               IF (IBOUNC.EQ.1) THEN
!             for the first spectrum (first point), read time from
!             heading line
                  IF (BTYPE.EQ.'WAMW') THEN
                     READ(NDSD, IOSTAT=IOSTATUS) XLON, XLAT,&
                     &CDATE(1:LWDATE),&
                     &EMEAN, THQ, FMEAN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     IF (LEN_TRIM(CDATE) == 10 ) THEN
                        TIMSTR = TRIM(CDATE)
                     ELSEIF (LEN_TRIM(CDATE) == 12 ) THEN
                        TIMSTR = CDATE(1:10)
                     ELSEIF (LEN_TRIM(CDATE) == 14 ) THEN
                        TIMSTR = CDATE(3:12)
                     ENDIF
!                 convert time string to time in seconds
                     CALL DTRETI (TIMSTR, BFILED(6), TIMF2)
                  ELSE IF (BTYPE.EQ.'WAMC') THEN
                     READ(NDSD, IOSTAT=IOSTATUS) XLON, XLAT, XDATE,&
                     &EMEAN, THQ, FMEAN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     WRITE (TIMSTR,'(F11.0,9X)') XDATE
!                 convert time string to time in seconds
                     CALL DTRETI (TIMSTR, BFILED(6), TIMF2)
                  ELSE IF (BTYPE.EQ.'WAMF') THEN
                     READ(NDSD,*, IOSTAT=IOSTATUS) XLON, XLAT, DDATE,&
                     &EMEAN, THQ, FMEAN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     WRITE (TIMSTR,'(F11.0,9X)') DDATE
!                 convert time string to time in seconds
                     CALL DTRETI (TIMSTR, BFILED(6), TIMF2)
                  ELSE IF (BTYPE.EQ.'WW3F') THEN
!                  read from heading lines per spectrum , date ,time
                     IF(IHD.EQ.1) THEN
                        READ(NDSD,*, IOSTAT=IOSTATUS) WWDATE, WWTIME
                        IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                        IF (SCREEN.NE.PRINTF.AND.IAMMASTER) THEN
                           WRITE(SCREEN,'(2A,I8.8,A,I6.6)')'Reading WW3'&
                           &,' bound. forcing......date and time are '&
                           &,WWDATE,'.',WWTIME
                        ENDIF
                        WRITE (TIMSTR, "(I8,'.',I6)") WWDATE, WWTIME
!                    convert time string to time in seconds
                        CALL DTRETI (TIMSTR, BFILED(6), TIMF2)
                     ELSE
!                   DUM_A dummy local variable used to read formatted
!                   files
!                   this variable will be not used in any calculation
                        READ (NDSD,"(1X,A10,1X,2F7.2,F10.1,2(F7.2,F6.1))") PTNME, DUM_A, DUM_A, BDEPTH,&
                        &DUM_A, DUM_A, DUM_A, DUM_A
                     ENDIF
                  ELSE IF (BTYPE.EQ.'WW3U') THEN
!                 read from heading lines per spectrum , date ,time
                     READ (NDSD, IOSTAT=IOSTATUS) WWDATE, WWTIME
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     WRITE (TIMSTR, "(I8,'.',I6)") WWDATE, WWTIME
!                 convert time string to time in seconds
                     CALL DTRETI (TIMSTR, BFILED(6), TIMF2)
                     READ (NDSD, IOSTAT=IOSTATUS)
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                  ELSE
!                 SWAN files
                     READ (NDSD, '(A)', IOSTAT=IOSTATUS) HEDLIN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     IF (ITEST.GE.100) WRITE (PRINTF, "(' heading line: ', A)") HEDLIN
                  ENDIF
               ELSE
                  IF (UNFORM) THEN
                     READ (NDSD, IOSTAT=IOSTATUS)
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                  ELSE IF (BTYPE.EQ.'WW3F') THEN
!                 read from heading lines per spectrum depth of the
!                 boundary point
                     IF (IHD.GT.1) EXIT
!                 exit is used because the header per spectrum in WW3F
!                 for IBOUNC>1 is one line
!                 DUM_A is a dummy local parameter used in formatted read 40.05
                     READ (NDSD,"(1X,A10,1X,2F7.2,F10.1,2(F7.2,F6.1))") PTNME, DUM_A, DUM_A, BDEPTH,&
                     &DUM_A, DUM_A, DUM_A, DUM_A
                  ELSE IF (BTYPE.EQ.'WAMF') THEN
!                 read HEDLIN replaced because data are sometimes written 40.13
!                 on two subsequent lines
                     READ(NDSD,*, IOSTAT=IOSTATUS) XLON, XLAT, DDATE,&
                     &EMEAN, THQ, FMEAN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                  ELSE
                     READ (NDSD, '(A)', IOSTAT=IOSTATUS) HEDLIN
                     IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
                     IF (ITEST.GE.100) WRITE (PRINTF, "(' heading line: ', A)") HEDLIN
                  ENDIF
               ENDIF

               IF (BTYPE(1:3).EQ.'SWN') THEN
!             SWAN nesting: take proper action if heading line contains
                  IF (HEDLIN(1:6).EQ.'NODATA' .OR. HEDLIN(1:4).EQ.'ZERO')&
                  &THEN
                     DO IS = 1, MSC
                        DO ID = 1, MDC
                           BSPECS(ID,IS,IBSPEC,2) = 0.
                        ENDDO
                     ENDDO
!                 skip reading of values
                     CYCLE BP_LOOP
                  ELSE IF (HEDLIN(1:6).EQ.'FACTOR') THEN
                     READ (NDSD, *) RFAC
                     UFAC = UFAC * RFAC
!                 multiply factor read from file by UFAC (factor following from
!                 type of file)
                  ELSE
!                 note: in case of 1D spectra heading line can be ignored
                     IF (NANG.GT.0) THEN
                        CALL MSGERR (3,&
                        &'incorrect code in b.c. file: '//HEDLIN(1:20))
                     ENDIF
                  ENDIF
               ENDIF
!           end loop over heading lines per spectrum
            ENDDO

!           test output: which spectrum is processed

            IF (ITEST.GE.60) THEN
               INQUIRE (UNIT=NDSD, NAME=FILENM)
               WRITE (PRTEST, "(' read spectrum ', A, '; time=', A, ' nr=', I3, F9.3)")&
               &FILENM, CHTIME, IBOUNC, UFAC
            ENDIF

!       start reading incoming wave data

            IF (BTYPE.EQ.'TPAR') THEN
               READ (NDSD, "(A)", IOSTAT=IOSTATUS) HEDLIN
               IF (boundary_data_exhausted(IOSTATUS)) EXIT file_contents
               CALL LSPLIT (HEDLIN, DATITM, 5)
               CALL DTRETI (DATITM(1), BFILED(6), TIMF2)
               DO II = 1, 4
                  READ (DATITM(II+1), '(G12.0)') SPPARM(II)
               ENDDO
               IF (ITEST.GE.60) WRITE (PRTEST, *) ' TPAR boundary ',&
               &TIMF2, (SPPARM(JJ), JJ=1,4)
               BFILED(3) = NINT(TIMF2)
               CALL SSHAPE (BSPECS(1,1,IBSPEC,2), SPCSIG, SPCDIR,&
               &FSHAPE, DSHAPE)
            ELSE
!         other (spectral) boundary conditions
               NANG  = BFILED(10)
               NFRE  = BFILED(12)

!         call RESPEC subroutine to read the spectum of the bound. files

               CALL RESPEC (BTYPE, NDSD, BFILED, UNFORM, DORDER,&
               &SPCSIG, SPCDIR, BSPFRQ, BSPDIR, BSPECS(1,1,IBSPEC,2),&
               &UFAC, IERR)
               IF (IERR.EQ.9) EXIT BP_LOOP
            ENDIF

         END DO BP_LOOP

         IF (IERR.NE.9) THEN
         IF (NSTATF) WRITE (PRINTF,&
         &"(' Boundary data type ', A, ' processed, time: ', F12.0)") BTYPE, TIMF2

!        cycle back to top of loop
         CYCLE GLOOP
         END IF

         END BLOCK file_contents
         IF (READ_ERROR) RETURN

!         if there are no more data on a boundary data file
!         close this file, and see if there is a next one

         CLOSE(NDSD)
!         read filename of next boundary file and open them
         IF (NDSL.GT.0) THEN
            READ (NDSL, '(A)', IOSTAT=IOSTATUS) FILENM
            IF (IOSTATUS.NE.0) THEN
               IF (.NOT.IS_IOSTAT_END(IOSTATUS)) THEN
                  CALL boundary_open_error()
                  RETURN
               ENDIF
               CLOSE (NDSL)
               BFILED(5) = 0
               CALL MSGERR (1, 'data on boundary file exhausted')
               BFILED(4) = 0
               BFILED(1) = -1
               TIMF2 = 999999999D0
               CYCLE GLOOP
            ENDIF
            IF (UNFORM) THEN
               OPEN (NDSD, FILE=FILENM, FORM='UNFORMATTED',&
               &STATUS='OLD', IOSTAT=IOSTATUS)
               IF (boundary_open_failed(IOSTATUS)) RETURN
!              read heading lines
               DO IHD = 1, BFILED(14)
                  READ (NDSD, IOSTAT=IOSTATUS)
                  IF (boundary_heading_read_failed(IOSTATUS)) RETURN
               ENDDO
            ELSEIF ((.NOT.UNFORM).AND.(BTYPE.EQ.'WW3F')) THEN
!             if it is WW3F open the new file only
               OPEN (NDSD, FILE=FILENM, FORM='FORMATTED',&
               &STATUS='OLD', IOSTAT=IOSTATUS)
               IF (boundary_open_failed(IOSTATUS)) RETURN
               COUNT_IT = 1
            ELSE
               OPEN (NDSD, FILE=FILENM, FORM='FORMATTED',&
               &STATUS='OLD', IOSTAT=IOSTATUS)
               IF (boundary_open_failed(IOSTATUS)) RETURN
               DO IHD = 1, BFILED(14)
!               read heading lines
                  READ (NDSD, '(A)', IOSTAT=IOSTATUS) HEDLIN
                  IF (boundary_heading_read_failed(IOSTATUS)) RETURN
                  IF (ITEST.GE.80) WRITE (PRINTF, "(' heading line: ', A)") HEDLIN
               ENDDO
            ENDIF

!      go back to statement 210 to start again the procedure of reading
!      info and spectrum from the new boundary file

            CYCLE boundary_files

         ELSE
!           boundary data are read from a single file
            CALL MSGERR (1, 'data on boundary file exhausted')
            BFILED(4) = 0
            BFILED(1) = -1
            TIMF2 = 999999999D0
            CYCLE GLOOP
         ENDIF
         END DO boundary_files

!        (if necessary) data have been read from file, now interpolate in time

      ELSE
!       if present time <= time of last set of spectra then
!       transform to spectral resolution used in SWAN to
!       obtain boundary spectra

         IF (TIMF1.NE.TIMF2) THEN
            W1 = REAL((TIMF2-default_time_context%TIMCO) / (TIMF2-TIMF1))
         ELSE
            W1 = 0.
         END IF
         DO IBOUNC = 1, BFILED(8)
            IBSPEC = BSPLOC(IBOUNC)
            IF (IBOUNC.EQ.1 .AND. ITEST.GE.80) WRITE (PRTEST, "(' interp in time ', F14.1, F8.3, 2F14.1, I4)")&
            &default_time_context%TIMCO, W1, TIMF1, TIMF2, IBSPEC

!       interpolate spectra in time; result has to be store in BSPECS(..,1)
!       first interpolate to auxiliary array

            CALL SINTRP (W1, 1.-W1, BSPECS(1,1,IBSPEC,1),&
            &BSPECS(1,1,IBSPEC,2), SPAUX,&
            &SPCDIR, SPCSIG)
!       use SINTRP to copy contents of aux. array to BSPECS(..,1)

            CALL SINTRP (1., 0., SPAUX,&
            &BSPECS(1,1,IBSPEC,2), BSPECS(1,1,IBSPEC,1),&
            &SPCDIR, SPCSIG)
         ENDDO
         BFILED(2) = NINT(default_time_context%TIMCO)
         BFILED(3) = NINT(TIMF2)
         EXIT GLOOP
!       end of time comparison
      ENDIF

!     end of while loop
   END DO GLOOP



   DEALLOCATE(SPAUX)

   RETURN

CONTAINS

   LOGICAL FUNCTION boundary_data_exhausted(status)
      INTEGER, INTENT(IN) :: status

      boundary_data_exhausted = status.NE.0
      IF (.NOT.boundary_data_exhausted) RETURN
      IF (.NOT.IS_IOSTAT_END(status)) THEN
         CALL boundary_read_error()
         READ_ERROR = .TRUE.
      ENDIF
   END FUNCTION boundary_data_exhausted

   LOGICAL FUNCTION boundary_heading_read_failed(status)
      INTEGER, INTENT(IN) :: status

      boundary_heading_read_failed = status.NE.0
      IF (.NOT.boundary_heading_read_failed) RETURN
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      IF (IS_IOSTAT_END(status)) THEN
         CALL MSGERR (4, 'unexpected end of file on boundary file '//FILENM)
      ELSE
         CALL MSGERR (4, 'error reading data from boundary file '//FILENM)
      ENDIF
   END FUNCTION boundary_heading_read_failed

   LOGICAL FUNCTION boundary_open_failed(status)
      INTEGER, INTENT(IN) :: status

      boundary_open_failed = status.NE.0
      IF (boundary_open_failed) CALL boundary_open_error()
   END FUNCTION boundary_open_failed

   SUBROUTINE boundary_read_error()
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      CALL MSGERR (4, 'error reading data from boundary file '//FILENM)
   END SUBROUTINE boundary_read_error

   SUBROUTINE boundary_open_error()
      CALL MSGERR (4, 'error opening boundary file '//FILENM)
   END SUBROUTINE boundary_open_error

!     End of subroutine RBFILE

end subroutine RBFILE
!****************************************************************

SUBROUTINE RESPEC (BTYPE, NDSD, BFILED, UNFORM, DORDER,&
&SPCSIG, SPCDIR, BSPFRQ, BSPDIR, LSPEC, UFAC,&
&IERR)
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STRACE

!****************************************************************

   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE SWCOMM3

   IMPLICIT NONE(TYPE, EXTERNAL)
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
!     30.81: Annette Kieftenburg
!     40.00, 40.13: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.05: Ekaterini E. Kriezi
!     40.31: Tim Campbell and John Cazes
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.61: Roop Lalbeharry
!
!  1. Update
!
!     40.00, Nov. 98: new subroutine
!     30.81, Feb. 99: approximation for MS > 10 corrected
!     40.02, Feb. 00: initialisation of ISIGTA
!     40.05, Aug. 00: WW3 nesting and changes in the form of the code
!                     (use of f90 features), Revise version of subroutine
!     40.02, Sep. 00: Made BAUX0 allocatable
!     40.13, Apr. 01: message concerning ISIGTA removed
!     40.31, Jul. 03: bug fix
!     40.31, Nov. 03: removing POOL construction
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Nov. 04: small corrections
!     40.61, Nov. 06: boundary spectra are read as written in WAM4.5
!
!  2. Purpose
!
!     read one 1D OR 2D boundary spectrum from file, and transform
!     to internal SWAN spectral resolution
!
!  3. Method
!
!  4. Argument variables

   INTEGER, INTENT(INOUT)  :: BFILED(*)

! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL,    INTENT(IN)     :: SPCDIR(MDC,6)
   REAL,    INTENT(IN)     :: SPCSIG(MSC)
   REAL,    INTENT(IN)     :: BSPDIR(*)
   REAL,    INTENT(IN)     :: BSPFRQ(*)
   REAL,    INTENT(INOUT)  :: LSPEC(MDC,MSC)
   REAL,    INTENT(IN)     :: UFAC

   INTEGER,  INTENT(IN) ::  NDSD
   INTEGER,  INTENT(IN) ::  DORDER
   INTEGER,  INTENT(INOUT) ::  IERR

   LOGICAL   UNFORM

   CHARACTER(LEN=4) :: BTYPE

!     BTYPE    char  inp   type of input
!     NDSD     int   inp   unit ref. number of input file
!     BFILED   int   inp   options for reading boundary condition file
!     UNFORM   log   inp   if True, unformatted reading is called for
!     DORDER   int   inp   if <0, order of directions has to be reversed
!     NANG     int   inp   num of spectral direction of input spectrum
!     NFRE     int   inp   num of spectral frequencies of input spectrum
!     BSPFRQ   real  inp   spectral frequencies of input spectrum
!     BSPDIR   real  inp   spectral directions of input spectrum
!     LSPEC    real  out   interpolated spectrum
!     UFAC     real  inp   factor used to multiply data
!     IERR     int   out   error status, 0: no error, 9: end of file
!
!  5. Parameter variables
!
!  6. Local variables
!
!     IANG      counter of directions
!     IFRE      counter of frequencies
!     ID        counter of directions
!     IS        counter of frequencies
!     ISIGTA    the last frequency which is determined by interpolation

   INTEGER, SAVE :: IENT = 0
   INTEGER   IANG, IFRE, ID, IS, ISIGTA, NFRE, NANG, IOSTATUS

   REAL, ALLOCATABLE :: BAUX0(:,:)
   REAL, ALLOCATABLE :: BAUX1(:,:)
   REAL, ALLOCATABLE :: BAUX2(:,:)
   REAL, ALLOCATABLE :: BAUX3(:)
   REAL, ALLOCATABLE :: BAUX4(:)

   REAL      ETOT, ADEG, DD, ADIR, MS, CTOT, ACOS, CDIR
   REAL      DSUM, DSPR, FAC

!     ETOT      energy integrated over directions
!     ADEG      average direction in degr
!     DD        parameter for directional distribution
!     ADIR      average direction in rad
!     MS        power of Cos in directional distribution
!     CTOT      coefficient
!     DSPR      directional spread in rad
!     ACOS      cos of angle between direction and average dir.
!     CDIR      energy in one directional bin
!     BAUX0     auxiliary array for energy density
!     BAUX1     auxiliary array 1
!     BAUX2     auxiliary array 2
!     BAUX3     auxiliary array 3
!     BAUX4     auxiliary array 4
!     FAC       factor to correct directional spreading
!
!  8. Subroutines used
!
!       GAMMAF (in SWANSER)

!  9. Subroutines calling
!
!     RBFILE
!
!  10. Error messages
!
!        ---
!
!  11. Remarks
!
!
!  12. Structure
!
!       ---------------------------------------------------------
!       for all frequencies of input spectrum do
!           if NANG = 0
!           then read energy density, av. direction and dir. spread
!                determine directional distribution
!           else read spectral energy densities (1 .. NANG)
!                redistribute to get densities for SWAN directions
!       ---------------------------------------------------------
!       for all spectral directions do
!           redistribute te get densities for SWAN frequencies
!       ---------------------------------------------------------
!
!  13. Source text
!
!****************************************************************

   CALL STRACE (IENT, 'RESPEC')

!     read the values from array BFILED for the number of
!     direction and frequencies

   NANG = BFILED(10)
   NFRE = BFILED(12)
   ALLOCATE (BAUX0(NFRE,NANG))
   ALLOCATE (BAUX1(NANG,NFRE))
   ALLOCATE (BAUX2(MDC ,NFRE))
   ALLOCATE (BAUX3(NFRE))
   ALLOCATE (BAUX4(MSC))

   IF (ITEST.GE.60) THEN
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      WRITE (PRTEST, "(' Entry RESPEC, reading file:', A, /, 6X, I3, ' angles ', I3, ' freqs; factor ', E12.4, ' dir.def:', I2, ' dir.spr.def:', I2, ' unform:', L2)") FILENM, NANG, NFRE, UFAC, BFILED(18),&
      &BFILED(19), UNFORM
   ENDIF

!     Initialisation

   ISIGTA = MSC

!     read spectral energy densities from b.c. file
   IF (NANG.EQ.0) THEN

!     1-D spectral input

      DO IFRE=1,NFRE
!         1-D spectral input (only function of frequency)
         IF (IFRE.EQ.1) THEN
            READ (NDSD,*,IOSTAT=IOSTATUS) ETOT, ADEG, DD
            IF (spectrum_read_failed(IOSTATUS, .TRUE.)) RETURN
         ELSE
            READ (NDSD,*,IOSTAT=IOSTATUS) ETOT, ADEG, DD
            IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
         ENDIF

         IF (EQREAL(ETOT,REAL(BFILED(11)))) THEN
!            in case of exception value, no energy
            ETOT = 0.
            ADEG = 0.
            DD   = 180./PI
         END IF

         IF (BFILED(18).EQ.1) THEN
!           conversion from degrees to radians
            ADIR = PI * ADEG / 180.
         ELSE
!           conversion from Nautical to Cartesian conv.
            ADIR = PI * (180.+DNORTH-ADEG) / 180.
         ENDIF

         IF (BFILED(19).EQ.1) THEN
            IF (DD.GT.23.) THEN
               FAC = 1.2
            ELSEIF (DD.GT.17.) THEN
               FAC = 1.096
            ELSE
               FAC = 1.01
            ENDIF
!           DSPR is directional spread in radians
            DSPR = PI * DD / 180.
            IF (DSPR.NE.0.) THEN
               MS = MAX (FAC*DSPR**(-2) - 2., 1.)
            ELSE
               MS = 1000.
            END IF
         ELSE
            MS = DD
         ENDIF

         IF (ITEST.GE.80) WRITE (PRTEST, "(' read freq ', I3, E10.3, '; Cart dir ', F7.1, '; Cos power ', F7.2)") IFRE, ETOT,&
         &180.*ADIR/PI, MS

!         generate distribution over directions
!
!         equations taken from Jahnke & Emde (chapter Factorial Function)
         IF (MS.GT.10.) THEN
            CTOT = SQRT(MS/(2.*PI)) * (1. + 0.25/MS)
         ELSE
            CTOT = GAMMAF(0.5*MS+1.) / (SQRT(PI) * GAMMAF(0.5*MS+0.5))
         ENDIF
         DSUM = 0.
         DO ID = 1, MDC
            ACOS = COS(SPCDIR(ID,1) - ADIR)
            IF (ACOS .GT. 0.) THEN
               CDIR = CTOT * MAX (ACOS**MS, 1.E-10)
            ELSE
               CDIR = 1.E-10
            ENDIF
            IF (ITEST.GE.20) DSUM = DSUM + CDIR * DDIR
            BAUX2(ID,IFRE) = CDIR * ETOT
         ENDDO
         IF (ITEST.GE.20) THEN
            IF (ABS(DSUM-1.).GT.0.1) WRITE (PRTEST, "(' integral over directions is ', F9.4, ' with CTOT=', F10.3,'; power=', F8.2)") DSUM, CTOT, MS
         ENDIF
      END DO

   ELSE

!   (2D) fully spectral input
      IF (UNFORM) THEN
!          unformatted reading
         IF (BTYPE.EQ.'WW3U') THEN
            READ (NDSD,IOSTAT=IOSTATUS)&
            &((BAUX0(IFRE,IANG),IFRE=1,NFRE),IANG=1,NANG)
            IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
            DO IANG = 1,NANG
               DO IFRE = 1,NFRE
                  BAUX1(IANG,IFRE) = BAUX0(IFRE,IANG)
               ENDDO
            ENDDO
         ELSE
            IF (DORDER.LT.0) THEN
               READ (NDSD,IOSTAT=IOSTATUS)&
               &((BAUX1(IANG,IFRE),IANG=NANG,1,-1),IFRE=1,NFRE)
               IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
            ELSE
               READ (NDSD,IOSTAT=IOSTATUS)&
               &((BAUX1(IANG,IFRE),IANG=1,NANG),IFRE=1,NFRE)
               IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
            ENDIF
         ENDIF
      ELSEIF ((.NOT.UNFORM).AND.(BTYPE.EQ.'WW3F')) THEN

!         WW3F reading
!         BAUX0 local array to read the energy spectra from boundary files

         READ (NDSD,"(7E11.3)",IOSTAT=IOSTATUS)&
         &((BAUX0(IFRE,IANG),IFRE=1,NFRE),IANG=1,NANG,1)
         IF (spectrum_read_failed(IOSTATUS, .TRUE.)) RETURN

!         energy (variance) density   from E(FRQ,TH) to  E(TH,FRQ)

         DO IANG = 1,NANG
            DO IFRE = 1,NFRE
               BAUX1(IANG,IFRE) = BAUX0(IFRE,IANG)
            ENDDO
         ENDDO
      ELSE
!         format reading (except WW3F)
         IF (DORDER.LT.0) THEN
            READ (NDSD,*,IOSTAT=IOSTATUS)&
            &((BAUX1(IANG,IFRE),IANG=NANG,1,-1),IFRE=1,NFRE)
            IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
         ELSE
            READ (NDSD,*,IOSTAT=IOSTATUS)&
            &((BAUX1(IANG,IFRE),IANG=1,NANG),IFRE=1,NFRE)
            IF (spectrum_read_failed(IOSTATUS, .FALSE.)) RETURN
         ENDIF
      ENDIF

      IF (ITEST.GE.120) THEN
         WRITE (PRINTF,*)' Spectra from file'
         DO IFRE = 1, NFRE
            WRITE (PRINTF,*) IFRE, (BAUX1(IANG,IFRE),IANG=1,NANG)
         ENDDO
      ENDIF

!       --- in case of exception value, no energy

      DO IANG = 1,NANG
         DO IFRE = 1,NFRE
            IF (EQREAL(BAUX1(IANG,IFRE),REAL(BFILED(11)))) THEN
               BAUX1(IANG,IFRE) = 0.
            END IF
         END DO
      END DO

!       --- transform to spectral directions used in SWAN
!           results appear in array BAUX2(MDC,NFRE)

      DO IFRE = 1, NFRE
         CALL CHGBAS (BSPDIR, SPCDIR, PI2, BAUX1(1,IFRE),&
         &BAUX2(1,IFRE), NANG, MDC, ITEST, PRTEST)
      END DO
   ENDIF

!     interpolate energy densities to SWAN frequencies distribution

   IF (BSPFRQ(NFRE) .LT. SPCSIG(MSC)) THEN
      DO IS = MSC, 1, -1
         IF (SPCSIG(IS).LT.BSPFRQ(NFRE)) THEN
!           ISIGTA is the last frequency which is determined by interpolation
!           higher frequencies are determined by tail expression
            ISIGTA = IS
            EXIT
         ENDIF
      ENDDO
   ELSE
      ISIGTA = MSC
   ENDIF


!     UFAC is the product of the multiplication factor read from file
!     and the factor to transform from energy/Hz to energy/(rad/s)
!     and from energy/degr to energy/rad (latter only for 2d spectra)

   DO  ID = 1,MDC
      DO  IFRE = 1,NFRE
         BAUX3(IFRE) = UFAC * BAUX2(ID,IFRE)
      ENDDO

!       interpolate over frequency keeping energy constant, output BAUX4(MSC)
      CALL CHGBAS (BSPFRQ, SPCSIG, 0., BAUX3, BAUX4, NFRE, MSC,&
      &ITEST, PRTEST)
      DO  IS=1,MSC
         IF (IS.LE.ISIGTA) THEN

!           to convert energy density to action density

            LSPEC(ID,IS) = BAUX4(IS)/SPCSIG(IS)
         ELSE

!           add a tail when IS > ISIGTA

            LSPEC(ID,IS) = LSPEC(ID,ISIGTA) *&
            &(SPCSIG(ISIGTA)/SPCSIG(IS))**(PWTAIL(1)+1)
         ENDIF
         IF (ITEST.GE.140) THEN
            WRITE (PRTEST, *) 'ID,IS,LSPEC(ID,IS)', ID,IS,LSPEC(ID,IS)
         ENDIF
      ENDDO
   ENDDO

   DEALLOCATE (BAUX0,BAUX1,BAUX2,BAUX3,BAUX4)

IERR = 0
   RETURN

CONTAINS

   LOGICAL FUNCTION spectrum_read_failed(status, eof_is_exhaustion)
      INTEGER, INTENT(IN) :: status
      LOGICAL, INTENT(IN) :: eof_is_exhaustion

      spectrum_read_failed = status.NE.0
      IF (.NOT.spectrum_read_failed) RETURN
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      IF (IS_IOSTAT_END(status)) THEN
         IF (eof_is_exhaustion) THEN
            IERR = 9
         ELSE
            CALL MSGERR (2, 'insufficient data in boundary condition file '//FILENM)
         ENDIF
      ELSE
         CALL MSGERR (2, 'read error in boundary condition file '//FILENM)
      ENDIF
   END FUNCTION spectrum_read_failed

end subroutine RESPEC
!**********************************************************************


!************************************************************************
!                                                                      *
SUBROUTINE SWINCO (AC2    ,COMPDA ,&
&XCGRID ,YCGRID ,&
&KGRPNT ,SPCDIR ,&
&SPCSIG ,XYTST   )
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_coordinate_offset
   USE swan_input_grids
   USE SWCOMM3
   USE swan_test_output
   USE swan_spherical_geometry
   USE M_PARALL
   USE SwanGriddata


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
!     30.60: Nico Booij
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.80, 40.13: Nico Booij
!     30.82: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!            June 97: new for SWAN
!     30.60, Aug. 97: for zero wind velocity no change in action density
!                     modification to make procedure work for uniform wind
!                     maximum set to dim.less fetch loops over IS and ID
!                     swapped for efficiency
!     30.70, Sep. 97: output for test point added, argument XYTST added
!     30.72, Sept 97: Replaced DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.70, Feb. 98: computation of initial values revised argument list added
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.80, Apr. 98: correction computation FDLSS
!     30.82, Apr. 98: Modified computation of FDLSS, FPDLSS, HSDLSS
!     30.82, Oct. 98: Updated description of several variables
!     40.13, Feb. 01: correction for 1D cases
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Sep. 07: extension to unstructured grids
!
!  2. Purpose
!
!     Imposing of wave initial conditions at a computational grid
!
!  3. Method
!
!     The initial conditions are given using the following equation
!     for dimensionless Hs as function of dimensionless fetch:
!
!     Hs = 0.00288 f**(0.45)
!     Tp = 0.46    f**(0.27)
!     average direction = wind direction
!     directional distribution: Cos**2
!
!     after computation of the integral parameters the subroutine SSHAPE  30.70
!     is used to compute the spectrum
!
!  4. Argument variables
!
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     AC2        real  i/o   action density spectra
!     COMPDA     real  inp   quantities in comp grid points
!     KGRPNT     real  inp   indirect addresses of comp grid points
!     XYTST      int   inp   test points
!
!  6. Local variables
!
!     FDLSS : Dimensionless fetch
!     TPDLSS: Dimensionless peak period
!     HSDLSS: Dimensionless significant wave height

   REAL    FDLSS,  TPDLSS, HSDLSS

!  7. Common blocks used
!
!
!  8. REMARKS
!
!  9. STRUCTURE
!
! 10. SOURCE TEXT

   REAL     COMPDA(MCGRD,MCMVAR)

   REAL     AC2(MDC,MSC,MCGRD)

   INTEGER  KGRPNT(MXC,MYC), XYTST(*)

   LOGICAL :: INTERN
   INTEGER :: INX, IX, IY, IY1, IY2, JJ
   INTEGER, SAVE :: IENT = 0
   REAL :: COSYG, FETCH, TDXY, TLEN, WDLOC, WSLOC, WX, WY
   CALL STRACE(IENT,'SWINCO')

!     *** Fetch Computation 'the mean delta' ***
   IF (KSPHER.EQ.0) THEN
      TLEN  = (XCLEN + YCLEN)/2.
   ELSE
      COSYG = COS (DEGRAD * (YOFFS + 0.5*(YCGMIN+YCGMAX)))
      TLEN  = LENDEG * (COSYG*XCLEN + YCLEN)/2.
   ENDIF
   TDXY  = FLOAT(MXCGL + MYCGL)/2.
   IF ( nvertsg.NE.0 ) TDXY = FLOAT(nvertsg)

   FETCH = TLEN/TDXY
   SY0   = 3.3
   IF (ITEST.GE.60 .OR. NPTST.GT.0) WRITE (PRTEST, "(' test SWINCO, fetch:', E12.4)") FETCH

!     --- structured grid

   IF (ONED) THEN
      IY1 = 1
      IY2 = 1
   ELSE
      IY1 = 2
      IY2 = MYC-1
   ENDIF
   DO IX = 2, MXC-1
      DO IY = IY1, IY2
!         check if the point is a true internal point
         INTERN = .TRUE.
         INX = KGRPNT(IX-1,IY)
         IF (INX.LE.1) INTERN = .FALSE.
         INX = KGRPNT(IX+1,IY)
         IF (INX.LE.1) INTERN = .FALSE.
         IF (.NOT.ONED) THEN
            INX = KGRPNT(IX,IY-1)
            IF (INX.LE.1) INTERN = .FALSE.
            INX = KGRPNT(IX,IY+1)
            IF (INX.LE.1) INTERN = .FALSE.
         ENDIF
         INX = KGRPNT(IX,IY)
         IF (INX.LE.1) INTERN = .FALSE.
         IF (INTERN) THEN
            TESTFL = .FALSE.
            DO IPTST = 1, NPTST
               IF (IX.EQ.XYTST(2*IPTST-1) .AND.&
               &IY.EQ.XYTST(2*IPTST)) TESTFL = .TRUE.
            ENDDO

            IF (VARWI) THEN
               WX  = COMPDA(INX,JWX2)
               WY  = COMPDA(INX,JWY2)

!             *** Local wind speed and direction ***
               WSLOC = SQRT(WX*WX + WY*WY)
               IF (WX .NE. 0. .OR. WY .NE. 0.) THEN
                  WDLOC = ATAN2(WY,WX)
               ELSE
                  WDLOC = 0.
               ENDIF
            ELSE
!             uniform wind field
               WSLOC = U10
               WDLOC = WDIP
            ENDIF

            IF (WSLOC .GT. 1.E-10) THEN

! Dimensionless Hs and Tp calculated according to K.K. Kahma & C.J. Calkoen,
! (JPO, 1992) and Pierson-Moskowitz for limit values.
!
!             calculate dimensionless fetch:
               FDLSS = GRAV * FETCH / (WSLOC*WSLOC)

!             calculate dimensionless significant wave height:
               HSDLSS = MIN (0.21, 0.00288*FDLSS**0.45)
               SPPARM(1) = HSDLSS * WSLOC**2 / GRAV
!             calculate dimensionless peak period:
               TPDLSS = MIN (1./0.13, 0.46*FDLSS**0.27)
               SPPARM(2) = WSLOC * TPDLSS / GRAV
               IF (SPPARM(1).LT.0.05) SPPARM(2) = 2.
               SPPARM(3) = 180. * WDLOC / PI
               SPPARM(4) = 2.
               IF (TESTFL) WRITE (PRTEST, "(' test point ',6(1X,E12.4))") XCGRID(IX,IY),&
               &YCGRID(IX,IY), FDLSS, (SPPARM(JJ), JJ = 1, 3)
            ELSE
               SPPARM(1) = 0.02
               SPPARM(2) = 2.
               SPPARM(3) = 0.
               SPPARM(4) = 0.
            ENDIF
            CALL SSHAPE (AC2(1,1,INX), SPCSIG, SPCDIR, 2, 2)

         ENDIF
      ENDDO
   ENDDO

!     --- unstructured grid

   DO INX = 1, nverts
!        internal, exception and ghost vertices only
      IF ( vmark(INX) == 0 .or. vmark(INX) >= excmark ) THEN
         TESTFL = .FALSE.
         DO IPTST = 1, NPTST
            IF (INX.EQ.XYTST(IPTST)) TESTFL = .TRUE.
         ENDDO

         IF (VARWI) THEN
            WX = COMPDA(INX,JWX2)
            WY = COMPDA(INX,JWY2)

!             *** Local wind speed and direction ***
            WSLOC = SQRT(WX*WX + WY*WY)
            IF (WX .NE. 0. .OR. WY .NE. 0.) THEN
               WDLOC = ATAN2(WY,WX)
            ELSE
               WDLOC = 0.
            ENDIF
         ELSE
!             uniform wind field
            WSLOC = U10
            WDLOC = WDIP
         ENDIF

         IF (WSLOC .GT. 1.E-10) THEN

! Dimensionless Hs and Tp calculated according to K.K. Kahma & C.J. Calkoen,
! (JPO, 1992) and Pierson-Moskowitz for limit values.
!
!             calculate dimensionless fetch:
            FDLSS = GRAV * FETCH / (WSLOC*WSLOC)

!             calculate dimensionless significant wave height:
            HSDLSS = MIN (0.21, 0.00288*FDLSS**0.45)
            SPPARM(1) = HSDLSS * WSLOC**2 / GRAV
!             calculate dimensionless peak period:
            TPDLSS = MIN (1./0.13, 0.46*FDLSS**0.27)
            SPPARM(2) = WSLOC * TPDLSS / GRAV
            IF (SPPARM(1).LT.0.05) SPPARM(2) = 2.
            SPPARM(3) = 180. * WDLOC / PI
            SPPARM(4) = 2.
            IF (TESTFL) WRITE (PRTEST, "(' test point ',6(1X,E12.4))") xcugrd(INX),&
            &ycugrd(INX), FDLSS, (SPPARM(JJ), JJ = 1, 3)
         ELSE
            SPPARM(1) = 0.02
            SPPARM(2) = 2.
            SPPARM(3) = 0.
            SPPARM(4) = 0.
         ENDIF
         CALL SSHAPE (AC2(1,1,INX), SPCSIG, SPCDIR, 2, 2)

      ENDIF
   ENDDO

   RETURN
! * end of subroutine SWINCO *
end subroutine SWINCO
!****************************************************************

SUBROUTINE SWCLME ( DIFFR, TRIADS, SNL4, SPECTRAL_POWERS, THREAD_WORKSPACES )

!****************************************************************

   USE OUTP_DATA
   USE M_BNDSPEC
   USE M_GENARR
   USE M_PARALL
   USE SwanGriddata
   USE SwanCompdata
   USE SwanIEM
   USE SwanBraggScat
   USE SwanQCM
!METIS   USE SwanParallel

   IMPLICIT NONE(TYPE, EXTERNAL)

   TYPE(diffraction_state_t), INTENT(INOUT) :: DIFFR
   TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
   TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4
   TYPE(spectral_powers_t), INTENT(INOUT) :: SPECTRAL_POWERS
   TYPE(thread_workspaces_t), INTENT(INOUT) :: THREAD_WORKSPACES


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
!     40.80: Marcel Zijlema
!     41.78: Bert Jagers
!
!  1. Updates
!
!     40.31, Oct. 03: New subroutine
!     40.80, Aug. 07: extension to unstructured grids
!     41.78, Mar. 21: delete linked lists
!
!  2. Purpose
!
!     Clean memory
!
!  3. Method
!
!     De-allocates several allocatable arrays
!     Deletes linked lists
!
!  9. Subroutines calling
!
!     SWMAIN
!
! 13. Source text

   CALL SPECTRAL_POWERS%CLEAR()
   CALL THREAD_WORKSPACES%CLEAR()
   CALL SNL4%CLEAR()
   IF (ALLOCATED(KGRPNT))   DEALLOCATE(KGRPNT)
   IF (ALLOCATED(KGRBND))   DEALLOCATE(KGRBND)
   IF (ALLOCATED(XYTST ))   DEALLOCATE(XYTST )
   IF (ALLOCATED(AC2   ))   DEALLOCATE(AC2   )
   IF (ALLOCATED(XCGRID))   DEALLOCATE(XCGRID)
   IF (ALLOCATED(YCGRID))   DEALLOCATE(YCGRID)
   IF (ALLOCATED(SPCSIG))   DEALLOCATE(SPCSIG)
   IF (ALLOCATED(SPCDIR))   DEALLOCATE(SPCDIR)
   IF (ALLOCATED(DEPTH ))   DEALLOCATE(DEPTH )
   IF (ALLOCATED(FRIC  ))   DEALLOCATE(FRIC  )
   IF (ALLOCATED(UXB   ))   DEALLOCATE(UXB   )
   IF (ALLOCATED(UYB   ))   DEALLOCATE(UYB   )
   IF (ALLOCATED(WXI   ))   DEALLOCATE(WXI   )
   IF (ALLOCATED(WYI   ))   DEALLOCATE(WYI   )
   IF (ALLOCATED(WLEVL ))   DEALLOCATE(WLEVL )
   IF (ALLOCATED(ASTDF ))   DEALLOCATE(ASTDF )
   IF (ALLOCATED(IBLKAD))   DEALLOCATE(IBLKAD)
   IF (ALLOCATED(XGRDGL))   DEALLOCATE(XGRDGL)
   IF (ALLOCATED(YGRDGL))   DEALLOCATE(YGRDGL)
   IF (ALLOCATED(KGRPGL))   DEALLOCATE(KGRPGL)
   IF (ALLOCATED(KGRBGL))   DEALLOCATE(KGRBGL)
   CALL DIFFR%CLEAR()
   IF (ALLOCATED(MUDLF ))   DEALLOCATE(MUDLF )
   IF (ALLOCATED(AICEF ))   DEALLOCATE(AICEF )
   IF (ALLOCATED(HICEF ))   DEALLOCATE(HICEF )
   IF (ALLOCATED(NPLAF ))   DEALLOCATE(NPLAF )
   IF (ALLOCATED(LAYH  ))   DEALLOCATE(LAYH  )
   IF (ALLOCATED(VEGDIL))   DEALLOCATE(VEGDIL)
   IF (ALLOCATED(VEGNSL))   DEALLOCATE(VEGNSL)
   IF (ALLOCATED(VEGDRL))   DEALLOCATE(VEGDRL)
   IF (ALLOCATED(TURBF ))   DEALLOCATE(TURBF )
   IF (ALLOCATED(HSSF  ))   DEALLOCATE(HSSF  )
   IF (ALLOCATED(TSSF  ))   DEALLOCATE(TSSF  )
   IF (ALLOCATED(DSSF  ))   DEALLOCATE(DSSF  )
   CALL TRIADS%CLEAR()

   IF (ALLOCATED(xcugrd  )) DEALLOCATE(xcugrd  )
   IF (ALLOCATED(ycugrd  )) DEALLOCATE(ycugrd  )
   IF (ALLOCATED(xcugrdgl)) DEALLOCATE(xcugrdgl)
   IF (ALLOCATED(ycugrdgl)) DEALLOCATE(ycugrdgl)
   IF (ALLOCATED(ivertg  )) DEALLOCATE(ivertg  )
   IF (ALLOCATED( vmark  )) DEALLOCATE( vmark  )
   IF (ALLOCATED( vlist  )) DEALLOCATE( vlist  )
   IF (ALLOCATED( blist  )) DEALLOCATE( blist  )
   IF (ALLOCATED(bvertg  )) DEALLOCATE(bvertg  )
   IF (ALLOCATED( bmark  )) DEALLOCATE( bmark  )
!GRAPH   IF (ALLOCATED( flist  )) DEALLOCATE( flist  )
!GRAPH   IF (ALLOCATED(  fptr  )) DEALLOCATE(  fptr  )
!FXFRO   IF (ALLOCATED(fronts  )) DEALLOCATE(fronts  )
!FXFRO   IF (ALLOCATED(fronte  )) DEALLOCATE(fronte  )
!GRAPH   IF (ALLOCATED(nfront  )) DEALLOCATE(nfront  )
!
!METIS   IF (ALLOCATED(ipown   )) DEALLOCATE(ipown   )
!METIS   IF (ALLOCATED(vres    )) DEALLOCATE(vres    )
!METIS   IF (ALLOCATED(vsubcm  )) DEALLOCATE(vsubcm  )
!METIS   IF (ALLOCATED(nvrecv  )) DEALLOCATE(nvrecv  )
!METIS   IF (ALLOCATED(nvsend  )) DEALLOCATE(nvsend  )
!METIS   IF (ALLOCATED(ivrecv  )) DEALLOCATE(ivrecv  )
!METIS   IF (ALLOCATED(ivsend  )) DEALLOCATE(ivsend  )
!METIS   IF (ALLOCATED(rrqst   )) DEALLOCATE(rrqst   )
!METIS   IF (ALLOCATED(srqst   )) DEALLOCATE(srqst   )
!METIS   IF (ALLOCATED(irbuf   )) DEALLOCATE(irbuf   )
!METIS   IF (ALLOCATED(isbuf   )) DEALLOCATE(isbuf   )
!METIS   IF (ALLOCATED( rbuf   )) DEALLOCATE( rbuf   )
!METIS   IF (ALLOCATED( sbuf   )) DEALLOCATE( sbuf   )

   IF (ALLOCATED( fb     )) DEALLOCATE( fb     )
   IF (ALLOCATED( fbdxy  )) DEALLOCATE( fbdxy  )

   IF (ALLOCATED( kx     )) DEALLOCATE( kx     )
   IF (ALLOCATED( ky     )) DEALLOCATE( ky     )
   IF (ALLOCATED( xpsc   )) DEALLOCATE( xpsc   )
   IF (ALLOCATED( xpd    )) DEALLOCATE( xpd    )
   IF (ALLOCATED( ypd    )) DEALLOCATE( ypd    )
   IF (ALLOCATED( kxd    )) DEALLOCATE( kxd    )
   IF (ALLOCATED( kyd    )) DEALLOCATE( kyd    )
   IF (ALLOCATED( disbk0 )) DEALLOCATE( disbk0 )
   IF (ALLOCATED( disbk1 )) DEALLOCATE( disbk1 )

   IF (ALLOCATED( iwt    )) DEALLOCATE( iwt    )
   IF (ALLOCATED( itt    )) DEALLOCATE( itt    )
   IF (ALLOCATED( iss    )) DEALLOCATE( iss    )
   IF (ALLOCATED( freq   )) DEALLOCATE( freq   )
   IF (ALLOCATED( E0     )) DEALLOCATE( E0     )
   IF (ALLOCATED( Ebig   )) DEALLOCATE( Ebig   )

   CALL DELETE ( FOPS )
   NULLIFY( COPS )
   LOPS = .FALSE.
   CALL DELETE ( FORQ )
   LORQ = .FALSE.
   CALL DELETE ( FBNDFIL )
   LBFILS = .FALSE.
   CALL DELETE ( FBS )
   LBS = .FALSE.
   CALL DELETE ( FBGP )
   LBGP = .FALSE.

   RETURN
end subroutine SWCLME

end module swan_driver

!************************************************************************

PROGRAM SWAN
   USE swan_service_interfaces, ONLY: STPNOW
   USE swan_driver, ONLY: SWMAIN
   USE swan_parallel, ONLY: SWINITMPI, SWEXITMPI
!                                                                      *
!************************************************************************

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
!     30.74: IJsbrand Haagsma (Include version)
!     30.90: IJsbrand Haagsma (Equivalence version)
!     32.01: Roeland Ris & Cor van der Schelde
!     34.01: Jeroen Adema
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!            Jan. 94: transition from old pool to new pool structure
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     32.01, Jan. 98: Array WL initialised (project h3268)
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     34.01, Feb. 99: Introducing STPNOW
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL mechanism and reconsidering
!                     this main program
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Main program
!
!  8. Subroutines used
!
!     SWEXITMPI
!     SWINITMPI
!     SWMAIN


! 11. Remarks
!
!     In case of coupling with ADCIRC, this program will not be executed  41.20
!     Instead, SWAN initialization and run will be done by PADCSWAN_INIT  41.20
!     and PADCSWAN_RUN, respectively, as they will pass a time step to
!     routine SWMAIN. See couple2swan.F
!
! 13. Source Code
!
!     --- initialize the MPI execution environment

   CALL SWINITMPI
   IF (.NOT. STPNOW()) THEN

!     --- start SWAN run

      CALL SWMAIN
   END IF

!     --- stop MPI

   CALL SWEXITMPI

!     --- end of MAIN PROGRAM

end program SWAN
