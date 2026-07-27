
!     SWAN/READ file 1 of 2
!
! Contents of this file:
!
!     SWREAD:  Reading and processing of the user commands describing the model
!     SINPGR:  Read parameters of an input grid
!     SREDEP
!     SSFILL
!     CGINIT
!     SWDIM
!     CGBOUN   determines boundary of true computational region
!     INITVA:  Processing command INIT and compute initial state of
!              the wave field
!     BACKUP
!
!************************************************************************

module swan_command_reading
   use swan_triad_state, only: triad_state_t
   use swan_snl4_tables, only: snl4_tables_t
   use swan_spectral_powers, only: spectral_powers_t
   use swan_input_helpers, only: REPARM
   use swan_create_edges, only: SwanCreateEdges
   use swan_grid_topology, only: SwanGridTopology
   use swan_init_comp_grid, only: SwanInitCompGrid
   use swan_read_grid, only: SwanReadGrid
   use swan_input_processing, only: SPROUT, SVARTP, SWBOUN, RETSTP
!  De !TIMG-timers worden uit meerdere procedures van deze module
!  aangeroepen, dus hun interface hoort op moduleniveau zichtbaar te zijn.
   use swan_service_interfaces, only: SWTSTA, SWTSTO
   implicit none(type, external)
   private
   public :: SWREAD
contains

!                                                                      *
SUBROUTINE SWREAD (COMPUT, TRIADS, SNL4, SPECTRAL_POWERS)
   USE swan_array_copy, ONLY: SWCOPI
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, ININTG, INKEYW, INREAL, KEYWIS, INCTIM, ININTV, INITVD, NWLINE, WRNKEY
!                                                                      *
!************************************************************************
!
!     Modules

   USE swan_time, ONLY: default_time_context
   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
   USE M_GENARR
   USE M_OBSTA
   USE M_PARALL
   USE SwanGriddata
   USE SwanIEM, only: nmax, dfiem, e_trsh, sflog
   USE SwanBraggScat, only: mkbx, mkby, dkbx, dkby, botspc
   USE SwanQCM, only: mkxc, mkyc
!METIS   USE SwanParallel

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
!     30.60: Nico Booij
!     30.61: Roberto Padilla
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.75: Nico Booij
!     30.80: Nico Booij
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma
!     32.01: Roeland Ris & Cor van der Schelde
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     32.06: Roeland Ris
!     33.08: W. Erick Rogers
!     33.09: Nico Booij
!     33.10: W. Erick Rogers and Nico Booij
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.09: Annette Kieftenburg
!     40.10: IJsbrand Haagsma
!     40.12: IJsbrand Haagsma
!     40.13: Nico Booij
!     40.14: Annette Kieftenburg
!     40.16: IJsbrand Haagsma
!     40.17: IJsbrand Haagsma
!     40.18: Annette Kieftenburg
!     40.21: Agnieszka Herman
!     40.23: Marcel Zijlema
!     40.28: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.38: Annette Kieftenburg
!     40.08: W. Erick Rogers
!     40.31: Marcel Zijlema
!     40.35: Nico Booij
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.55: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.87: Marcel Zijlema
!     41.65: Marcel Zijlema
!     41.71: Gerbrant van Vledder
!     41.72: Henrique Rapizo
!     41.75: Erick Rogers
!     41.77: Jaime Ascencio
!     41.82: Dirk Rijnsdorp
!     41.80: Dirk Rijnsdorp and Ad Reniers
!     41.85: Ad Reniers
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!
!  1. Updates
!
!     30.60, July 97: command CGRID, exception values added
!     30.60, Aug. 97: JCOOX and JCOOY used when addingco ordinate arrays change in
!                     command BOUND option SEGM
!     30.60, Aug. 97: PNUMS(20) is set to 0.1 in command WIND if third generation
!                     is used
!     30.60, Aug. 97: argument XYTST added in call of SWDIM
!     30.60, Aug. 97: activate initial condition in commands WIND and GEN3
!     30.60, Aug. 97: command CGRID, default keyword is REG keyword EXC
!                     required
!     30.60, Aug. 97: command OBST, names changed into ALPHA and BETA; control
!                     strings changed from UNC into STA
!     30.60, Aug. 97: uncommented statement CALL WRNKEY
!     30.70, Sep. 97: value of PWTAIL(1) is set to 5 in command GEN3 JANS,
!                     GROWTH JANS and WCAP JANS
!     30.72, Oct. 97: logical function EQREAL introduced for floating point
!                     comparisons
!     30.72, Nov. 97: Added the command syntax for all the commands as comments
!                     from the user manual
!     30.72, Nov. 97: Header renewed, updated method and argument variable
!                     description
!     30.72, Nov. 97: Did set the correct pointers for command GEN3 JANS, as
!                     was already done correctly for command WCAP JANS
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.72, Jan. 98: Removed reference to quadruplets in WIND command
!     32.01, Jan. 98: Introduced SET NAUT command (project h3268)
!     32.01, Jan. 98: Introduced keyword CON/VAR in command
!                     BOU STAT ... SPEC1D/SPEC2D
!     30.72, Jan. 98: Moved BOU NONSTAT non operational warning to end of
!                     command BOU
!     30.72, Jan. 98: Changed size of JAUX(7) array to MCGRD
!     32.01, Jan. 98: Modifications for nautical convention, interpolation of
!                     spectra at the boundary and warning (project h3268)
!     32.02, Feb. 98: Introduced 1D-version
!     30.70, Feb. 98: MODE DYN modified into MODE NONSTationary
!                     option WINDGrowth added in command OFF
!                     Nautical convention introduced into command CGRID
!                     in command SET name 'negmes' changed into 'maxmes'
!     30.72, Mar. 98: Leave limiter on when ITRIAD > 0 in command OFF QUAD
!     30.70, Mar. 98: option CON/VAR after options SPEC1D/SPEC2D
!                     keyword STAT made optional
!                     name GRWMX changed into LIMITER
!                     assignment of limiter in command OFF QUAD corrected
!     30.75, Mar. 98: set ICOND=1 (default init.cond.) for nonstationary mode
!     30.82, Apr. 98: removed reference to commons KAART and KAR
!     40.00, Sep. 98: in command OBST square of TRCOEF is stored (this is used
!                     as transmission coefficient for action density)
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.81, Nov. 98: Adjustment for 1-D case of new boundary conditions
!     30.80, Nov. 98: Provision for limitation on Ctheta (refraction)
!     40.00, Jan. 99: new command OUTPUT QUANTITY: allows user to change
!                     properties of output quantities.
!     34.01, Feb. 99: Introducing STPNOW
!     30.82, Mar. 99: Deactivate limiter for GEN1 and GEN2
!     40.00, Apr. 99: restructure command MODE: Nonstat Oned is now possible
!     33.08, July 98: input for model with higher order "S&L" scheme.
!     33.09, Aug. 98: input for spherical coordinates
!     32.06, June 99: Set correct values for IGEN
!     30.82, Aug. 99: Modified default values for PTRIAD, after deactivating
!                     limiter for only the triads.
!     30.82, Aug. 99: Modified command NUM to include settings for the SETUP
!                     and the global stop criterion
!     40.01, Sep. 99: XASM and YASM replace fixed numbers
!     40.03, Dec. 99: command QUANTITY corrected
!     40.10, Mar. 00: prepared for exact quadruplets
!     33.10, Jan. 00: input for model with higher order "SORDUP" scheme
!     40.03, May  00: command INCLude added, array INCNUM added
!     40.09, May  00: TRCOEF**2 replaced by TRCOEF (to make command options
!                     TRANSM and DAM consistent with one another in subroutine
!                     SWTRCOEF in swanser)
!     40.09, May. 00: Reflection command option added
!     40.03, Aug. 00: error message if start time is before current time (command COMP)
!            Sep. 00: inconsistency with manual corrected
!     40.02, Sep. 00: Changed SCHEME command to PROP and handling [cdlim] modified
!     40.02, Oct. 00: In case of Nautical directions then output in not
!     40.02, Oct. 00: Recalculate whitecapping coefficients for new SWCAP routine
!     40.02, Oct. 00: Avoided real/int conflict by introducing RPOOL in
!     40.02, Oct. 00: Avoided real/int conflict by introducing replacing
!                     RPOOL for POOL in various calls
!     40.02, Oct. 00: Initialisation of IERR
!     40.12, Feb. 01: Avoided type conflict for OUTPS
!     40.18, Apr. 01: Reflection option extended
!     40.13, July 01: reading of PTRIAD(4) added in command TRIad
!                     command OUTPut OPTions added; module OUTP_DATA added
!                     command OUTPUT QUANTITY is made obsolete
!     40.13, Aug. 01: [xpc] and [ypc] are required in case of spherical
!                     [ylenc] and [myc] not required in 1-D mode
!     40.13, Oct. 01: USE OUTP_DATA added in view of longer filenames
!     40.13, Oct. 01: value of SPDIR1 in full circle case changed
!     40.13, Nov. 01: size of array BSPAUX increased
!     40.13, Nov. 01: command OUTPUT OPTIONS added
!     40.18, Apr. 01: Reflection option extended, scatter
!     40.28, Dec. 01: Reflection option extended, freq. dep.
!     40.38, Feb. 02: Reflection option extended, diffuse
!     40.14, Dec. 01: Extra check on user defined coefficients added
!     40.16, Dec. 01: Implemented limiter switches
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.21, Aug. 01: Diffraction option added
!     40.23, Aug. 02: under-relaxation factor added
!     40.23, Sep. 02: coefficient urslim must be stored in PTRIAD(5)
!     40.23, Nov. 02: keyword FLUXLIM added
!     40.30, Feb. 03: introduction distributed-memory approach using MPI
!     40.08, Mar. 03: warning message added for use of curvilinear
!                     coordinate system
!     40.31, Dec. 03: removing POOL-mechanism
!     40.35, Jun. 04: introducing turbulent viscosity model
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Jun. 06: correct input of MDIA
!     40.55, Dec. 05: introducing vegetation model
!     40.80, Jun. 07: extension to unstructured grids
!     40.87, Apr. 08: in keyword QUANTITY more than 1 output parameter at once
!                     is possible and addition of [fmin] and [fmax]
!     41.65, Jun. 16: extension frequency and direction dependent tranmission coefficients
!     41.71, Dec. 18: extension freeboard dependent transmission and reflection
!     41.75, Jan. 19: adding sea ice
!     41.72, Nov. 19: add quantity for number of swell partitions
!     41.77, Feb. 20: adding Jacobsen vegetation formulation
!     41.82, Aug. 21: introduce FIG source term
!     41.80, Oct. 21: adding Bragg scattering
!     41.85, Feb. 19: implementation of IEM (surfbeat model)
!     41.90, Jun. 21: adding quasi-coherent modelling
!
!  2. Purpose
!
!     Reading and processing of the user commands describing the model
!
!  3. Method (updated 30.72)
!
!     A new line is read, in which the first keyword determines what the
!     command is. The command is read and processed. Common variable are
!     given proper values. After processing
!     the command the program returns to label 100, to process a new command.
!     This is repeated until the command STOP is found or the end of file is
!     reached.
!
!     Depending on the commands, the argument variable COMPUT is given a value,
!     depending on which the program will make a computation is some form or not.
!
!  4. Argument variables (updated 30.72)
!
!     COMPUT   Output variable that determine the sort of computation to be
!              performed by SWAN
!              ='COMP'; computation requested
!              ='NOCO'; no computation but output requested
!              ='RETR'; retrieve data previous computation
!              ='STOP'; make computation, output and stop
!
!  5. Parameter variables

   INTEGER, PARAMETER :: MXINCL = 10
   INTEGER, PARAMETER :: NVOTP  = 15

!  6. Local variables
!
!     DUM       Dummy variable
!     FD1       Coeff. for freq. dep. reflection: vertical displacement
!     FD2       Coeff. for freq. dep. reflection: shape parameter
!     FD3       Coeff. for freq. dep. reflection: directional coefficient 40.28
!     FD4       Coeff. for freq. dep. reflection: bending point of freq.  40.28
!     ICNL4     Counter for reading CNL4_? values
!     ILAMBDA   Counter for reading LAMBDA values
!     INCLEV    Include level, increases at INCL command,
!               decreases at end-of-file
!     INCNUM    Unit reference numbers of included files
!     ITMP1     auxiliary integer
!     ITMP2     auxiliary integer
!     ITMP3     auxiliary integer
!     ITMP4     auxiliary integer
!     LREF      Indicates whether reflection is active (#0.) or not (=0.) 40.09
!     LREFDIFF  Indicates whether scattered reflection is active (#0.)
!               or not (=0.)
!     LRFRD     Indicates whether frequency dependent reflection is
!               active (#0.) or not (=0.)
!     MORE      Indicates whether more entries of a loop need to be
!               performed
!     POWN      user defined power of redistribution function
!     RLAMBDA   Dummy array containing lambda values for quadruplets

   INTEGER           :: MPTST,IPP,NLEN,ITRAS,JJ,IGR2,IGRD,MXS,MYS
   INTEGER           :: IVAL,NUMCOR,MSS,I,J,NVAR,IVTYPE,IFTMAX
   REAL              :: TRCF,HGT,SLP,BK,OGAM,OBET,REF0,XP,YP
   REAL              :: GAMR, GAMT
   REAL              :: AFIG, HSS, TSS, DSS, MFR, SHP
   REAL              :: ALTMP,FRLOW,FRHIG,GAMMA,TMPDIR,VALX,VALY
   REAL(KIND=KIND(0.0D0))            :: DIFF
   REAL              :: HH, CC, DD
   INTEGER           :: NN
   INTEGER           :: NREGB, NDSD, IDLA, IK, JK
   REAL              :: VARB

   INTEGER, SAVE     :: INCNUM(1:MXINCL) = 0
   INTEGER, SAVE     :: INCLEV = 1

   INTEGER           :: IOSTAT = 0
   INTEGER           :: ICNL4, ILAMBDA
   INTEGER           :: ITMP1, ITMP2, ITMP3, ITMP4

   LOGICAL           :: MORE
   LOGICAL, SAVE     :: LOBST = .FALSE.
   LOGICAL           :: FILB

   LOGICAL CHGALF

   INTEGER        :: LREF, LREFDIFF, LRFRD, LFREE, LQUAY, LFIG
   REAL           :: POWN, DUM
   REAL           :: FD1, FD2, FD3, FD4
   REAL, ALLOCATABLE :: RLAMBDA(:)

   CHARACTER(LEN=6)  QOVSNM
   CHARACTER(LEN=40) QOVLNM
   REAL         QR(10)
   REAL(KIND=KIND(0.0D0))       DVAL

   TYPE(OBSTDAT), POINTER :: OBSTMP
   TYPE(OBSTDAT), SAVE, POINTER :: COBST

   TYPE(OPSDAT), POINTER :: OPSTMP

   TYPE XYPT
      REAL                :: X, Y
      TYPE(XYPT), POINTER :: NEXTXY
   end type XYPT

   TYPE(XYPT), TARGET  :: FRST
   TYPE(XYPT), POINTER :: CURR, TMP

   TYPE AUXT
      INTEGER             :: I
      TYPE(AUXT), POINTER :: NEXTI
   end type AUXT
   TYPE(AUXT), TARGET  :: FRSTQ
   TYPE(AUXT), POINTER :: CURRQ, TMPQ

   TYPE VEGPT
      INTEGER              :: N
      REAL                 :: H, D, C
      TYPE(VEGPT), POINTER :: NEXTV
   end type VEGPT

   TYPE(VEGPT), TARGET  :: FRSTV
   TYPE(VEGPT), POINTER :: CURRV, TMPV

   TYPE TRCFPT
      REAL                  :: C
      TYPE(TRCFPT), POINTER :: NEXTFT
   end type TRCFPT

   TYPE(TRCFPT), TARGET  :: FRSTFT
   TYPE(TRCFPT), POINTER :: CURRFT, TMPFT

!  8. Subroutines used
!
!     DEGCNV: Transforms dir. from nautical to cartesian or vice versa
!     INITVA: Processing comm. INIT and comp. initial state of wave field 30.70
!     SINPGR: Read parameters of an input grid
!     SWINIT
!     REINC
!     SWRBC
!     SREDEP
!     SPRCON
!     SPROUT
!     RETSTP  read test points
!     SWNDPR (SWAN/SWREAD)
!     OCPINI
!     NWLINE
!     INKEYW
!     INCSTR
!     ININTG
!     INREAL
!     FOR
!     KEYWIS
!     COPYCH
!     (all Ocean Pack)


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
!     The description of the structure of this subroutine is very
!     short as most of the source code can easily be understood with
!     the aid of the command descriptions in the user manual and the
!     purpose of the subroutines from the system documentation.
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     Call NWLINE for reading new line of user input
!     Call INKEYW to read a new command from user input
!     If the command is equal to one of the SWAN commands, then
!         Read and process the rest of the command
!     ----------------------------------------------------------------
!
! 13. Source text

   LOGICAL :: FOUND

!     *** The logical variable LOGCOM has a record about which    ***
!     *** commands have been given to know if all the information ***
!     *** for certain command is available:                       ***
!     *** LOGCOM(2): command CGRID has been carried out           ***
!     *** LOGCOM(3): command READINP BOTTOM has been carried out  ***
!     *** LOGCOM(4): command READ COOR has been carried out       ***
!     *** LOGCOM(5): command READ UNSTRUC has been carried out    ***
!     *** LOGCOM(6): array AC2 has been allocated                 ***
!     *** LOGCOM(7): mesh partitioning has been carried out       ***
!     *** In the current version, LOGCOM(1) has no meanings       ***

   LOGICAL, SAVE :: LOGCOM(1:7) = .FALSE.
   INTEGER   TIMARR(6)
   CHARACTER(LEN=8)  :: PSNAME, PNAME
   CHARACTER(LEN=*)  :: COMPUT
   TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
   TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4
   TYPE(spectral_powers_t), INTENT(INOUT) :: SPECTRAL_POWERS
   CHARACTER(LEN=1)  :: PTYPE
   INTEGER, SAVE :: IENT = 0       ! number of entries to this subr
   INTEGER, SAVE :: LWINDR = 0     ! if non-zero, there is wind
   INTEGER, SAVE :: LWINDM = 5     ! type of wind growth formulation
   INTEGER, ALLOCATABLE :: IARR(:)
   INTEGER, PARAMETER :: IVOTP(NVOTP) = &
      [10, 11, 13, 15, 16, 17, 18, 19, 28, 32, 33, 42, 43, 47, 48]
   INTEGER :: INDX(1)
   LOGICAL :: SWELLSET = .FALSE.
   LOGICAL :: WCAPSET  = .FALSE.

   ASSOCIATE(TCOLL => TRIADS%collinear)

   CALL STRACE (IENT, 'SWREAD')

!     ***** read command *****

command_loop: DO
CALL NWLINE
   IF (default_command_reader%ELTYPE.EQ.'EOF') THEN
!       end-of-file encountered in (included) input file
!       return to previous input file
      CLOSE (INCNUM(INCLEV))
      INCLEV = INCLEV - 1
      IF (INCLEV.EQ.0) THEN
         CALL MSGERR (4, ' unexpected end of command input')
         RETURN
      ENDIF
      default_command_reader%ELTYPE = 'USED'
      INPUTF = INCNUM(INCLEV)
   ENDIF

   IF ( ITEST .GE. 200) THEN
      WRITE (PRTEST,*) ' BNAUT NA LABEL 100 IN SWREAD =', BNAUT
   ENDIF

   CALL INKEYW ('REQ',' ')

!     ------------------------------------------------------------------
!                 PROCESSING OF COMMANDS
!     ------------------------------------------------------------------
!
!     STOP
!
! ============================================================
!
! STOP
!
! ============================================================

   IF (KEYWIS ('STOP')) THEN
      IF (RUNMADE) THEN
         COMPUT = 'STOP'
      ELSE
         WRITE (PRINTF,*)' ** No computation requested **'
         COMPUT = 'NOCO'
      ENDIF
      RETURN
   ENDIF

!     ------------------------------------------------------------------
!
!     PROJECT      reading of project title and description
!
! ===============================================================
!
! PROJect  'NAME'  'NR'
!
!          'title1'
!
!          'title2'
!
!          'title3'
!
! ===============================================================

   IF (KEYWIS ('PROJ')) THEN
      CALL INCSTR ('NAME', PROJID, 'UNC', default_command_reader%BLANK)
      CALL INCSTR ('NR', PROJNR, 'REQ', default_command_reader%BLANK)
      CALL NWLINE
      IF (STPNOW()) RETURN
      CALL INCSTR ('TITLE1',PROJT1,'UNC',' ')
      CALL NWLINE
      IF (STPNOW()) RETURN
      CALL INCSTR ('TITLE2',PROJT2,'UNC',' ')
      CALL NWLINE
      IF (STPNOW()) RETURN
      CALL INCSTR ('TITLE3',PROJT3,'UNC',' ')
      CYCLE command_loop
   ENDIF

!     INCLUDE     include another file in command file
!     ------------------------------------------------------------------
!     INCLude  'FILE'
!     ------------------------------------------------------------------

   IF (KEYWIS ('INCL')) THEN
      IF (INCLEV.EQ.1) INCNUM(INCLEV) = INPUTF
      CALL INCSTR ('FILE' , FILENM , 'REQ', ' ')
      INCLEV = INCLEV + 1
      IF (INCLEV.GT.MXINCL) THEN
         CALL MSGERR (4, 'too many INCLUDE levels')
         RETURN
      ENDIF
      IOSTAT = 0
      CALL FOR (INCNUM(INCLEV), FILENM, 'OF', IOSTAT)
      INPUTF = INCNUM(INCLEV)
      CYCLE command_loop
   ENDIF

! ===========================================================
!
!    POOL
!
! ===========================================================

   IF (KEYWIS ('POOL')) THEN
      CALL MSGERR(1,'Keyword POOL is not relevant anymore!')
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!     TEST       parameters for required test output
!
!     =============================================================
!
!                                  / -> IJ < [i] [j] > | < [k] >  \
!    TEST [itest] [itrace] POINTS <                                >  &
!                                  \    XY < [x] [y] >            /
!
!                          PAR 'fname'  S1D 'fname'  S2D 'fname'
!
!     ============================================================

   IF (KEYWIS ('TEST')) THEN
      CALL ININTG ('ITEST' , ITEST , 'STA', 30)
!       statements restructured:
      IF (ITEST.GE.30) THEN
         IF (ERRPTS.EQ.0.AND.IAMMASTER) THEN
            ERRPTS = 16
            OPEN (ERRPTS, FILE='ERRPTS',&
            &STATUS='UNKNOWN', FORM='FORMATTED')
         ENDIF
      ENDIF
      CALL ININTG ('ITRACE', ITRACE, 'UNC',  0)
      IF (ITRACE.GT.0) THEN
         LTRACE =.TRUE.
      ELSE
         LTRACE =.FALSE.
      ENDIF
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('POI')) THEN
         NPTST  = 0
         MPTST  = 50
         IPP    = 2
         IF (OPTG.EQ.5) IPP = 1
         IF (.NOT.ALLOCATED(IARR)) ALLOCATE(IARR(IPP*MPTST))

         CALL RETSTP (IPP*MPTST, IARR, KGRPNT, KGRBND, XCGRID, YCGRID,&
         &SPCSIG, SPCDIR)
         IF (STPNOW()) RETURN
         NPTSTA = MAX(1,NPTST)
         CALL ENSURE_FIELD_SIZE (XYTST, IPP*NPTSTA)
         CALL SWCOPI (IARR,XYTST,IPP*NPTSTA)
         DEALLOCATE(IARR)
      ELSE
!        Deliberately not a resize: NPTST keeps the count from an earlier
!        TEST POI, so shrinking the array here would leave the two disagreeing.
!        Since SWINIT establishes the empty state, this never fires any more.
         IF (.NOT.ALLOCATED(XYTST)) ALLOCATE(XYTST(0))
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     INTEst       parameters for required test output during input
!
!     ============================================================
!
!     INTE [intes]         (NOT documented)
!
!     ============================================================

   IF (KEYWIS ('INTE')) THEN
      CALL ININTG ('INTES' , INTES , 'STA', 30)
      CYCLE command_loop
   ENDIF
!     ------------------------------------------------------------------
!     COTEst       parameters for required test output during computation
!
!     ============================================================
!
!     COTE [cotes]         (NOT documented)
!
!     ============================================================
   IF (KEYWIS ('COTE')) THEN
      CALL ININTG ('COTES' , ICOTES , 'STA', 30)
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     OUTEst       parameters for required test output during output
!
!     ============================================================
!
!     OUTE [itest]         (NOT documented)
!
!     ============================================================

   IF (KEYWIS ('OUTE')) THEN
      CALL ININTG ('ITEST' , IOUTES , 'STA', 30)
      CYCLE command_loop
   ENDIF

!     ============================================================
!
!      OUTPut OPTIons  'comment'  (TABle [field])  (BLOck  [ndec]  [len])   &
!
!      (SPEC  [ndec])
!
!     ============================================================

   IF (KEYWIS('OUTP')) THEN
      CALL IGNORE ('OPT')
      CALL INCSTR ('COMMENT', OUT_COMMENT, 'UNC', ' ')
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('TAB')) THEN
         CALL ININTG ('FIELD', FLD_TABLE, 'STA', 11)
         IF (FLD_TABLE.GT.16) CALL MSGERR (2, '[field] is too large')
         IF (FLD_TABLE.LT.8)  CALL MSGERR (2, '[field] is too small')
         WRITE (FLT_TABLE, "('(E', I2, '.', I1, ')')") FLD_TABLE, FLD_TABLE-7
         IF (ITEST.GE.30) WRITE (PRINTF, "(' Format floating point table: ', A)") FLT_TABLE
         CALL INKEYW ('STA', ' ')
      ENDIF
      IF (KEYWIS('BLO')) THEN
         CALL ININTG ('NDEC', DEC_BLOCK, 'STA', 4)
         IF (DEC_BLOCK.GT.9) CALL MSGERR (2, '[ndec] is too large')
         CALL ININTG ('LEN', NLEN, 'STA', 200)
         IF (NLEN.GT.9999) CALL MSGERR (2, '[len] is too large')
         WRITE (FLT_BLOCK, "('(', I4, '(1X,E', I2, '.', I1, '))')") NLEN, DEC_BLOCK+7, DEC_BLOCK
         IF (ITEST.GE.30) WRITE (PRINTF, "(' Format floating point block: ', A)") FLT_BLOCK
         CALL INKEYW ('STA', ' ')
      ENDIF
      IF (KEYWIS('SPEC')) THEN
         CALL ININTG ('NDEC', DEC_SPEC, 'STA', 5)
         IF (DEC_SPEC.GT.9) CALL MSGERR (2, '[ndec] is too large')
         CALL ININTG ('LEN', NLEN, 'STA', 200)
         IF (NLEN.GT.9999) CALL MSGERR (2, '[len] is too large')
         WRITE (FIX_SPEC, "('(', I4, '(1X,I', I1, '))')") NLEN, DEC_SPEC
         IF (ITEST.GE.30) WRITE (PRINTF, "(' Format spectral output: ', A)") FIX_SPEC
         LENSPO = NLEN*(DEC_SPEC+1)
      ENDIF
      IF (KEYWIS('QUA')) THEN
         CALL MSGERR (3,&
         &'command OUTP QUANT is obsolete, use QUANTITY')
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     BOTTOM    definition of bottom grid
!
! ============================================================
!
! BOTtom ...         (OBSOLETE command)
!
! ============================================================

   IF (KEYWIS ('BOT')) THEN
      CALL MSGERR (2, 'command BOTTOM is replaced by INP BOT')
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     MODE  : Set STATionary, DYNamic (NONSTAtionary) or 1D SWAN model
!
! ========================================
!
!        | -> STAtionary    |     | -> TWODimensional |
! MODE  <                    >   <                     >  (NOUPDATe)
!        |    NONSTationary |     |    ONEDimensional |
!
! ========================================

   IF (KEYWIS ('MODE')) THEN
      CALL INKEYW ('STA',' ')
      IF (KEYWIS('NONST') .OR. KEYWIS ('DYN')) THEN
         IF (NSTATM.EQ.0) CALL MSGERR (2, 'Mode Nonst incorrect here')
         NSTATM = 1
         NSTATC = 1

!         switch on flag for computation of default initial condition
         ICOND = 1
!         no relaxation
         PNUMS(30) = 0.
      ELSEIF (KEYWIS ('STA')) THEN
         IF (NSTATM.EQ.1) CALL MSGERR (2, 'Mode STAT incorrect here')
         NSTATM = 0
         CALL INKEYW ('STA',' ')
      ENDIF

!       *** Logical ONED added for 1d-computations

      CALL INKEYW ('STA', ' ')
      IF (KEYWIS ('ONED')) THEN
         ONED = .TRUE.
         IF (PARLL) THEN
            CALL MSGERR(4,'1D mode is not supported in parallel run')
            RETURN
         END IF
      ELSEIF (KEYWIS ('TWOD')) THEN
         ONED = .FALSE.
      ENDIF

!       *** Logical ACUPDA added to avoid updating action densities

      CALL INKEYW ('STA', ' ')
      IF (KEYWIS ('NOUPDAT')) THEN
         ACUPDA = .FALSE.
      ENDIF

      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
! ======================================================================
!
!            |    BSBT                      |
!   PROP    <                     | SEC |   |
!            |    GSE  [waveage] <  MIN  >  |
!            |                    |  HR |   |
!            |                    | DAY |   |
!
!            FLUXLIM
!
! ======================================================================

   IF (KEYWIS ('PROP')) THEN
      CALL INKEYW ('STA','    ')
      IF (KEYWIS ('BSBT') .OR. KEYWIS ('BTBS')) THEN
         PROPSN = 1
         PROPSS = 1
      ELSE IF (KEYWIS ('GSE')) THEN
         IF (OPTG.NE.5 .AND. PROPSN.NE.3) THEN
            CALL MSGERR(2,&
            &'Anti-GSE only allowed for S&L scheme.')
         ENDIF
         CALL ININTV('WAVEAGE', WAVAGE, 'STA', 0.)
      ENDIF
      IF (KEYWIS ('FLUXLIM')) THEN
         PROPFL   = 1
         PNUMS(6) = 0.
      END IF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     COORD : spherical or cartesian coordinates
!
! =============================================================
!
!   COORDinates  /  -> CARTesian               \   REPeating
!                \ SPHErical [rearth]   UM/QC  /
!
! =============================================================

   IF (KEYWIS ('COORD')) THEN
      CALL INKEYW ('STA',' ')
      IF (KEYWIS ('CART')) THEN
         KSPHER = 0
      ELSE IF (KEYWIS ('SPHE')) THEN
         KSPHER = 1
         CALL INREAL ('REARTH', REARTH, 'UNC', 0.)
         LENDEG = REARTH * PI / 180.
!         change properties of output quantities Xp and Yp
         OVUNIT(1) = 'degr'
         OVLLIM(1) = -200.
         OVULIM(1) =  400.
         OVLEXP(1) = -180.
         OVHEXP(1) =  360.
         OVEXCV(1) = -999.
         OVUNIT(2) = 'degr'
         OVLLIM(2) = -100.
         OVULIM(2) =  100.
         OVLEXP(2) = -90.
         OVHEXP(2) =  90.
         OVEXCV(2) = -999.
         CALL INKEYW ('STA','CCM')
         IF (KEYWIS ('QC')) THEN
!           quasi-cartesian projection method
            PROJ_METHOD = 0
         ELSE IF (KEYWIS ('CCM')) THEN
!           uniform Mercator projection (default for spherical coordinates)
            PROJ_METHOD = 1
         ENDIF
      ELSE
         CALL WRNKEY
      ENDIF
      CALL INKEYW ('STA',' ')
      IF (KEYWIS ('REP')) THEN
         KREPTX = 1
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     **  Command COMPUTE   **
!
!       ==============================================================
!
!                  |  STATionary  [time]                      |
!       COMPute ( <                                            > )
!                  |                    | -> Sec  |           |
!                  |  ([tbegc] [deltc] <     MIn   > [tendc]) |
!                                       |    HR   |
!                                       |    DAy  |
!
!       ==============================================================

   IF (KEYWIS ('COMP')) THEN
      COMPUT = 'COMP'
      RUNMADE = .TRUE.
      CALL INKEYW ('STA','  ')
      IF (NSTATM.LE.0 .OR. KEYWIS('STAT')) THEN
         IF (NSTATM.EQ.-1) NSTATM = 0
         IF (NSTATM.GT.0) CALL INCTIM (ITMOPT,'TIME',default_time_context%TINIC,'REQ',0D0)
         IF (default_time_context%TINIC .LT. default_time_context%TIMCO) THEN
            CALL MSGERR (2, '[time] before current time')
            default_time_context%TINIC = default_time_context%TIMCO
         ENDIF
         default_time_context%TFINC = default_time_context%TINIC
         default_time_context%TIMCO = default_time_context%TINIC
         default_time_context%DT = 1.E10
         RDTIM = 0.
         NSTATC = 0
         MTC = 1
      ELSE
         CALL IGNORE ('NONST')
         IF (default_time_context%TIMCO .LT. -0.9E10) THEN
            CALL INCTIM (ITMOPT,'TBEGC',default_time_context%TINIC,'REQ',0D0)
         ELSE
            CALL INCTIM (ITMOPT,'TBEGC',default_time_context%TINIC,'STA',default_time_context%TIMCO)
         ENDIF
         IF (default_time_context%TINIC .LT. default_time_context%TIMCO) THEN
            CALL MSGERR (2, 'start time [tbegc] before current time')
            default_time_context%TINIC = default_time_context%TIMCO
         ENDIF
         CALL INITVD ('DELTC', default_time_context%DT, 'REQ', 0D0)
         CALL INCTIM (ITMOPT,'TENDC',default_time_context%TFINC,'REQ',0D0)
         NSTATC = 1

!           *** tfinc must be greater than tinic **
         DIFF = default_time_context%TFINC - default_time_context%TINIC
         IF (DIFF .LE. 0.) CALL MSGERR (3,&
         &'start time [tbegc] greater or equal end time [tendc]')

!           **The number of computational steps is calculated
         RDTIM = 1./default_time_context%DT
         MTC = NINT ((default_time_context%TFINC - default_time_context%TINIC)/default_time_context%DT)
         IF (MOD(default_time_context%TFINC-default_time_context%TINIC,default_time_context%DT).GT.0.01*default_time_context%DT .AND.&
         &MOD(default_time_context%TFINC-default_time_context%TINIC,default_time_context%DT).LT.0.99*default_time_context%DT)&
         &CALL MSGERR (1,&
         &'DT is not a fraction of the computational period')
         default_time_context%TIMCO = default_time_context%TINIC
      ENDIF
      IF (NSTATM.GT.0) CHTIME = DTTIWR(ITMOPT, default_time_context%TIMCO)
      NCOMPT = NCOMPT + 1
      IF (NCOMPT.GT.300) CALL MSGERR (2,&
      &'No more than 300 COMPUTE commands are allowed')
      RCOMPT(NCOMPT,1) = REAL(NSTATC)
      RCOMPT(NCOMPT,2) = REAL(MTC)
      RCOMPT(NCOMPT,3) = default_time_context%TFINC
      RCOMPT(NCOMPT,4) = default_time_context%TINIC
      RCOMPT(NCOMPT,5) = default_time_context%DT
!       set ITERMX equal to MXITST in case of stationary computations
!       and to MXITNS otherwise
      IF (NSTATC.EQ.0) THEN
         ITERMX = MXITST
      ELSE
         ITERMX = MXITNS
      ENDIF
!       reset asort
      asort = usort
      RETURN
   ENDIF

!     ------------------------------------------------------------------
!
!     *** OBSTACLE   Definition of obstacles in comp grid. ***
!
! ============================================================
!
!             | -> TRANSm  [trcoef]                       |
!             |  TRANS1d < [trcoef] >                     |
!             |  TRANS2d < [trcoef] >                     |
! OBSTacle   <                                             >  &
!             |       | -> GODA [hgt] [alpha] [beta]      |
!             |  DAM <                                    |
!                     |    DANGremond [hgt] [slope] [Bk]  |
!
!     ( FIG [alpha1] [hss] [tss] [dss] [dd] [minfr] [shape] ) &
!
!                       | -> RSPEC        |
!     ( REFLec [reflc] <                   > )                &
!                       |    RDIFF [pown] |
!
!     ( FREEboard [hgt] [gammat] [gammar] Quay )              &
!
!             LINe < [xp] [yp] >
!
! ============================================================

   IF (KEYWIS ('OBST')) THEN

      IF ( ONED ) THEN
         CALL MSGERR(2, 'use of obstacles not allowed in 1D mode')
      ENDIF

      OBSTDONE = .FALSE.

      ALLOCATE(OBSTMP)
      OBSTMP%TRCOEF(1) = 0.
      OBSTMP%TRCOEF(2) = 0.
      OBSTMP%TRCOEF(3) = 0.
      OBSTMP%RFCOEF(1) = 0.
      OBSTMP%RFCOEF(2) = 0.
      OBSTMP%RFCOEF(3) = 0.
      OBSTMP%RFCOEF(4) = 0.
      OBSTMP%RFCOEF(5) = 0.
      OBSTMP%RFCOEF(6) = 0.
      OBSTMP%FBCOEF(1) = 0.
      OBSTMP%FBCOEF(2) = 0.
      OBSTMP%FBCOEF(3) = 0.
      OBSTMP%IGCOEF(1) = 0.
      OBSTMP%IGCOEF(2) = 0.
      OBSTMP%IGCOEF(3) = 0.
      OBSTMP%IGCOEF(4) = 0.
      OBSTMP%IGCOEF(5) = 0.
      OBSTMP%IGCOEF(6) = 0.
      OBSTMP%IGCOEF(7) = 0.

!       data concerning transmission of energy over/through the obstacle

      CALL INKEYW ('STA', '  ')
      IF (KEYWIS ('TRANS1') .OR. KEYWIS ('TRFREQ')) THEN
         ITRAS    = 11
         IFTMAX   = 0
         FRSTFT%C = 0.
         NULLIFY(FRSTFT%NEXTFT)
         CURRFT => FRSTFT
         DO
            CALL INREAL ('TRCOEF',TRCF,'REP',-1.)
            IF ( TRCF.EQ.-1. ) EXIT
            IF ( TRCF.LT.0. .OR. TRCF.GT.1. ) THEN
               CALL MSGERR(3,'Transmission coefficient is not allowed')
               CALL MSGERR(3,'to be greater than 1 or smaller than 0!')
            END IF
            IFTMAX = IFTMAX + 1
            ALLOCATE(TMPFT)
            TMPFT%C = TRCF
            NULLIFY(TMPFT%NEXTFT)
            CURRFT%NEXTFT => TMPFT
            CURRFT => TMPFT
         END DO
         IF ( IFTMAX.EQ.0 ) THEN
            CALL MSGERR(2,'No frequency dep. transmission coeffs found')
         ELSE IF ( IFTMAX.NE.MSC ) THEN
            CALL MSGERR(2,'Number of transmission coeffs not equal to')
            CALL MSGERR(2,'number of frequencies!')
            CALL MSGERR(2,'When less, then these coeffs are filled up')
            CALL MSGERR(2,'with 1.')
            CALL MSGERR(2,'When more, then these coeffs are ignored')
         END IF
         ALLOCATE(OBSTMP%TRCF1D(MSC))
         OBSTMP%TRCF1D(:) = 1.
         CURRFT => FRSTFT%NEXTFT
         DO JJ = 1, MIN(MSC,IFTMAX)
            OBSTMP%TRCF1D(JJ) = CURRFT%C
            CURRFT => CURRFT%NEXTFT
         END DO
         DEALLOCATE(TMPFT)
      ELSE IF (KEYWIS ('TRANS2')) THEN
         ITRAS    = 12
         IFTMAX   = 0
         FRSTFT%C = 0.
         NULLIFY(FRSTFT%NEXTFT)
         CURRFT => FRSTFT
         DO
            CALL INREAL ('TRCOEF',TRCF,'REP',-1.)
            IF ( TRCF.EQ.-1. ) EXIT
            IF ( TRCF.LT.0. .OR. TRCF.GT.1. ) THEN
               CALL MSGERR(3,'Transmission coefficient is not allowed')
               CALL MSGERR(3,'to be greater than 1 or smaller than 0!')
            END IF
            IFTMAX = IFTMAX + 1
            ALLOCATE(TMPFT)
            TMPFT%C = TRCF
            NULLIFY(TMPFT%NEXTFT)
            CURRFT%NEXTFT => TMPFT
            CURRFT => TMPFT
         END DO
         IF ( IFTMAX.EQ.0 ) THEN
            CALL MSGERR(2,'No spectral dep. transmission coeffs found')
         ELSE IF ( IFTMAX.NE.MSC*MDC ) THEN
            CALL MSGERR(2,'Number of transmission coeffs not equal to')
            CALL MSGERR(2,'number of frequencies multiplied with')
            CALL MSGERR(2,'number of directions!')
            CALL MSGERR(2,'When less, then these coeffs are filled up')
            CALL MSGERR(2,'with 1.')
            CALL MSGERR(2,'When more, then these coeffs are ignored')
         END IF
         ALLOCATE(OBSTMP%TRCF2D(MDC,MSC))
         OBSTMP%TRCF2D(:,:) = 1.
         CURRFT => FRSTFT%NEXTFT
         I = 1
         J = 0
         DO JJ = 1, MIN(MSC*MDC,IFTMAX)
            J = J + 1
            IF ( J > MSC ) THEN
               J = 1
               I = I + 1
            ENDIF
            OBSTMP%TRCF2D(I,J) = CURRFT%C
            CURRFT => CURRFT%NEXTFT
         END DO
         DEALLOCATE(TMPFT)
      ELSE IF (KEYWIS ('TRANS')) THEN
         ITRAS = 0
         CALL INREAL ('TRCOEF', TRCF, 'REQ', 0.)
         IF ((TRCF.LT.0.) .OR. (TRCF.GT.1.)) THEN
            CALL MSGERR(3,'Transmission coefficient is not allowed')
            CALL MSGERR(3,'to be greater than 1 or smaller than 0!')
         ENDIF
         OBSTMP%TRCOEF(1) = TRCF
      ELSE IF (KEYWIS ('DAM')) THEN
         CALL INKEYW ('STA', 'GODA')
         IF (KEYWIS ('DANG')) THEN
            ITRAS = 2
            CALL INREAL ('HGT'  , HGT , 'REQ', 0.)
            CALL INREAL ('SLOPE', SLP , 'REQ', 0.)
            CALL INREAL ('BK'   , BK  , 'REQ', 0.)
            OBSTMP%TRCOEF(1) = HGT
            OBSTMP%TRCOEF(2) = SLP
            OBSTMP%TRCOEF(3) = BK
         ELSE
            CALL IGNORE ('GODA')
            ITRAS = 1
            CALL INREAL ('HGT'  , HGT , 'REQ', 0.  )
            CALL INREAL ('ALPHA', OGAM, 'STA', 2.6 )
            CALL INREAL ('BETA' , OBET, 'STA', 0.15)
            OBSTMP%TRCOEF(1) = HGT
            OBSTMP%TRCOEF(2) = OGAM
            OBSTMP%TRCOEF(3) = OBET
         END IF
      ELSE
!         if no transmission options are activated, there will be 0 transmission
         ITRAS = 0
         TRCF  = 0.
         OBSTMP%TRCOEF(1) = TRCF
      ENDIF
      OBSTMP%TRTYPE = ITRAS

!       FIG source term

      CALL INKEYW ('REQ', '  ')
      IF (KEYWIS ('FIG') .OR. KEYWIS ('IG')) THEN
         LFIG = 1
         IF ( .NOT.FULCIR ) THEN
            CALL MSGERR(3,'FIG source term will only be included if  ')
            CALL MSGERR(3,'if the spectral directions cover the      ')
            CALL MSGERR(3,'full circle                               ')
         ENDIF
         CALL INREAL ('ALPHA1', AFIG, 'REQ', 0.)
         CALL INREAL ('HSS'   , HSS , 'UNC', 0.)
         CALL INREAL ('TSS'   , TSS , 'UNC', 0.)
         CALL INREAL ('DSS'   , DSS , 'STA', -999.)
         CALL INREAL ('DD'    , DD  , 'STA', 0.)
         CALL INREAL ('MINFR' , MFR , 'STA', 0.015)
         CALL INREAL ('SHAPE' , SHP , 'STA', 1.5)
         OBSTMP%IGCOEF(1) = AFIG
         OBSTMP%IGCOEF(2) = HSS
         OBSTMP%IGCOEF(3) = TSS
         OBSTMP%IGCOEF(4) = DSS
         OBSTMP%IGCOEF(5) = DD
         OBSTMP%IGCOEF(6) = MFR
         OBSTMP%IGCOEF(7) = SHP
      ELSE
         LFIG = 0
      ENDIF
      OBSTMP%IGTYP = LFIG

!       data on reflection by the obstacle, activated by keyword REFL

      CALL INKEYW ('REQ', '  ')
      IF (KEYWIS ('REFL')) THEN
         LREF = 1
         IF (.NOT.FULCIR) THEN
            CALL MSGERR(3,'Reflections will only be calculated if     ')
            CALL MSGERR(3,'the spectral directions cover the full     ')
            CALL MSGERR(3,'circle.                                    ')
         ENDIF
         CALL INREAL ('REFLC', REF0, 'STA', 1.)
         OBSTMP%RFCOEF(1) = REF0

         CALL INKEYW ('REQ', '  ')
         IF (KEYWIS('RDIFF')) THEN
            LREFDIFF = 1
            CALL INREAL ('POWN', POWN, 'REQ', 1.)
            DUM = MOD(POWN,1.)
            IF (POWN.LT.0.) THEN
               CALL MSGERR(3,'Power POWN is not a positive number! ')
            ENDIF
            IF (DUM.NE.0.) THEN
               CALL MSGERR(3,'Power POWN is not an integer number! ')
            ENDIF
            OBSTMP%RFCOEF(2) = POWN

         ELSE
            CALL IGNORE ('RSPEC')
            LREFDIFF = 0
         ENDIF
         OBSTMP%RFTYP2 = LREFDIFF

         CALL INKEYW ('REQ', '  ')
         CALL INKEYW ('STA', 'RFD')
         IF (KEYWIS('RFD')) THEN
            LRFRD = 1
            CALL INREAL ('FD1', FD1, 'REQ', 0.565)
            CALL INREAL ('FD2', FD2, 'REQ', 0.75)
            CALL INREAL ('FD3', FD3, 'REQ', -21.)
            CALL INREAL ('FD4', FD4, 'REQ', 0.066)
            IF (FD3.GE.0.) THEN
               CALL MSGERR(3,'Positive frequency dependency! ')
            ENDIF
            OBSTMP%RFCOEF(3) = FD1
            OBSTMP%RFCOEF(4) = FD2
            OBSTMP%RFCOEF(5) = FD3
            OBSTMP%RFCOEF(6) = FD4
         ELSE
            LRFRD = 0
         ENDIF
         OBSTMP%RFTYP3 = LRFRD

         IF ((REF0.LT.0.) .OR. (REF0.GT.1.)) THEN
            CALL MSGERR(3,'Reflection coeff. [reflc] is not allowed ')
            CALL MSGERR(3,'to be greater than 1 or smaller than 0!  ')
         ENDIF
      ELSE
!         if there is no keyword REFL, there will be no reflection
         LREF = 0
      ENDIF
      OBSTMP%RFTYP1 = LREF

!       freeboard dependent transmission and reflection (optional)

      CALL INKEYW ('REQ', '  ')
      IF (KEYWIS ('FREE')) THEN
         LFREE = 1
         CALL INREAL ('HGT'   , HGT , 'REQ', 0.)
         CALL INREAL ('GAMMAT', GAMT, 'STA', 1.)
         CALL INREAL ('GAMMAR', GAMR, 'STA', 1.)
         IF ( GAMT.LT.0. .OR. GAMR.LT.0. ) THEN
            CALL MSGERR(3,'shape parameter gamma is not allowed')
            CALL MSGERR(3,'to be smaller than 0!')
         ELSEIF ( GAMT.LT.0.1 ) THEN
            CALL MSGERR(1,'gammat may not be smaller than 0.1')
            GAMT = 0.1
         ELSEIF ( GAMR.LT.0.1 ) THEN
            CALL MSGERR(1,'gammar may not be smaller than 0.1')
            GAMR = 0.1
         ENDIF
         OBSTMP%FBCOEF(1) = HGT
         OBSTMP%FBCOEF(2) = GAMT
         OBSTMP%FBCOEF(3) = GAMR
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('Q')) THEN
            LQUAY = 1
         ELSE
            LQUAY = 0
         ENDIF
      ELSE
         LFREE = 0
         LQUAY = 0
      ENDIF
      OBSTMP%FBTYP1 = LFREE
      OBSTMP%FBTYP2 = LQUAY

!       check constant transmission/reflection coefficients
!       in case of energy conservation

      IF (LFREE.EQ.0 .AND. ITRAS.EQ.0 .AND. LREF.EQ.1) THEN
         DUM = TRCF*TRCF + REF0*REF0
         IF (DUM.GT.1.) CALL MSGERR(3,'Kt^2 + Kr^2 > 1 ')
      ENDIF

!       location of obstacles
!
!       *** NUMCOR : Number of corners ***
      NUMCOR = 0
      CALL INKEYW ('REQ', '  ')
      IF (KEYWIS ('LIN')) THEN
         FRST%X = 0.
         FRST%Y = 0.
         NULLIFY(FRST%NEXTXY)
         CURR => FRST
         DO
!           read coordinates of one corner point of the obstacle
            CALL READXY ('XP', 'YP', XP, YP, 'REP', -1.E10, -1.E10)
            IF (XP.LT.-.9E10) EXIT
            NUMCOR = NUMCOR + 1
            ALLOCATE(TMP)
            TMP%X = XP
            TMP%Y = YP
            NULLIFY(TMP%NEXTXY)
            CURR%NEXTXY => TMP
            CURR => TMP
         ENDDO
      ENDIF
      OBSTMP%NCRPTS = NUMCOR
!       store coordinates for corner points in array of obstacle data
      ALLOCATE(OBSTMP%XCRP(NUMCOR))
      ALLOCATE(OBSTMP%YCRP(NUMCOR))
      CURR => FRST%NEXTXY
      DO JJ = 1, NUMCOR
         OBSTMP%XCRP(JJ) = CURR%X
         OBSTMP%YCRP(JJ) = CURR%Y
         CURR => CURR%NEXTXY
      END DO
      DEALLOCATE(TMP)
      NULLIFY(OBSTMP%NEXTOBST)
      IF (NUMCOR .LE. 1) THEN
         CALL MSGERR(1,'No corner points for obstacle were found')
      ELSE
!         *** NUMOBS : Number of obstacles ***
         NUMOBS = NUMOBS + 1
         IF ( .NOT.LOBST ) THEN
            FOBSTAC = OBSTMP
            COBST => FOBSTAC
            LOBST = .TRUE.
         ELSE
            COBST%NEXTOBST => OBSTMP
            COBST => OBSTMP
         END IF
      ENDIF

      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     ***'INITial conditions'  Definition of initial conditions  ***
!     *** for MODE DYNAMIC

   IF (KEYWIS ('INIT')) THEN
      CALL INITVA( AC2, SPCSIG, SPCDIR, KGRPNT )
      IF (STPNOW()) RETURN
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!     HOTFile    write current wave field to file for future use as
!     initial condition
!
! ======================================================================
!
!     HOTFile  'FNAME'
!
! ======================================================================

   IF (KEYWIS('REST') .OR. KEYWIS ('BACK') .OR. KEYWIS('HOTF')&
   &.OR. KEYWIS('SAVE')) THEN
      IF (MXC.LE.0 .AND. OPTG.NE.5) THEN
         CALL MSGERR (2, 'command CGRID must precede this command')
      ELSEIF (MCGRD .LE. 1 .AND. nverts .LE. 0) THEN
         CALL MSGERR(2,&
         &' command READ BOT or READ UNSTRUC must precede this command')
      ELSE
         CALL BACKUP( AC2, SPCSIG, SPCDIR, KGRPNT, XCGRID, YCGRID )
         IF (STPNOW()) RETURN
      ENDIF
      CYCLE command_loop
   ENDIF
!     ------------------------------------------------------------------
!
!     INPUT    definition of input grids
!
! ======================================================================
!
!   INPgrid
!      BOTtom / WLEVel / CURrent / VX / VY / FRiction / WInd / WX  / WY
!      NPLAnts / TURB / MUDL / AICE / HICE / HSS      / TSS  / DSS
!      | REG [xpinp] [ypinp] [alpinp]  [mxinp] [myinp]  [dxinp] [dyinp]
!     <  CURVilinear [stagrx] [stagry] [mxinp] [myinp]
!      | UNSTRUCtured
!      (NONSTATionary [tbeginp] [deltinp] SEC/MIN/HR/DAY [tendinp])
!
! ======================================================================

   IF (KEYWIS ('INP')) THEN
      CALL IGNORE ('GRID')
      CALL INKEYW ('STA', ' ')
      IGR2 = 0
      IF (KEYWIS ('BOT')) THEN
         IGRD = 1
         PSNAME = 'BOTTGRID'
      ELSE IF (KEYWIS ('CUR')) THEN
         IGRD = 2
         IGR2 = 3
         PSNAME = 'VXGRID  '
      ELSE IF (KEYWIS ('VX')) THEN
         IGRD = 2
         PSNAME = 'VXGRID  '
      ELSE IF (KEYWIS ('VY')) THEN
         IGRD = 3
         PSNAME = 'VYGRID  '
      ELSE IF (KEYWIS ('FR')) THEN
         IGRD = 4
         PSNAME = 'FRICGRID'
      ELSE IF (KEYWIS ('WI')) THEN
         IGRD = 5
         IGR2 = 6
         PSNAME = 'WXGRID  '
      ELSE IF (KEYWIS ('WX')) THEN
         IGRD = 5
         PSNAME = 'WXGRID  '
      ELSE IF (KEYWIS ('WY')) THEN
         IGRD = 6
         PSNAME = 'WYGRID  '
      ELSE IF (KEYWIS ('WLEV')) THEN
         IGRD = 7
         PSNAME = 'WLEVGRID'
!       note: 8 and 9 are for coordinates
      ELSE IF (KEYWIS ('ASTD')) THEN
!         air-sea temperature difference
         IGRD = 10
         PSNAME = 'ASTDGRID'
      ELSE IF (KEYWIS ('NPLA')) THEN
!         number of plants per square meter
         IGRD = 11
         PSNAME = 'NPLAGRID'
      ELSE IF (KEYWIS ('TURB')) THEN
!         value of turbulent viscosity
         IGRD = 12
         PSNAME = 'TURBGRID'
      ELSE IF (KEYWIS ('MUDL')) THEN
!         fluid mud layer
         IGRD = 13
         PSNAME = 'MUDLGRID'
      ELSE IF (KEYWIS ('AICE')) THEN
!         ice concentration (as a fraction)
         IGRD = 14
         PSNAME = 'AICEGRID'
      ELSE IF (KEYWIS ('HICE')) THEN
!         ice thickness (in meters)
         IGRD = 15
         PSNAME = 'HICEGRID'
      ELSE IF (KEYWIS ('HSS').OR.KEYWIS ('IGHS')) THEN
!         sea-swell significant wave height
         IGRD = 16
         PSNAME = 'HSSGRID'
      ELSE IF (KEYWIS ('TSS').OR.KEYWIS ('IGTM')) THEN
!         sea-swell mean wave period
         IGRD = 17
         PSNAME = 'TSSGRID'
      ELSE IF (KEYWIS ('DSS').OR.KEYWIS ('IGDIR')) THEN
!         sea-swell mean wave direction
         IGRD = 18
         PSNAME = 'DSSGRID'
      ELSE
         IGRD = 1
         PSNAME = 'BOTTGRID'
      ENDIF

      CALL SINPGR (IGRD, IGR2, PSNAME)
      IF (STPNOW()) RETURN
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!     READ   reading depths, coordinates and/or currents
!
!   ============================================================================
!
!   READinp    BOTtom/WLevel/CURrent/FRiction/WInd/COORdinates/
!              NPLAnts/TURB/MUDL/AICE/HICE/HSS/TSS/DSS
!        [fac]  / 'fname1'        \
!               \ SERIES 'fname2' /  [idla] [nhedf] ([nhedt]) (nhedvec])     &
!        FREE / FORMAT 'form' / [idfm] / UNFORMATTED
!
!   or
!
!                          | -> ADCirc
!   READgrid UNSTRUCtured <  TRIAngle \
!                          | EASYmesh / 'fname'
!
!   ============================================================================

   IF (KEYWIS ('READ')) THEN

!       --- check whether computational grid has been defined

      IF ( .NOT.LOGCOM(2) ) THEN
         CALL MSGERR (3, '* define computational grid before      *')
         CALL MSGERR (3, '* reading coordinates or input grids    *')
      ENDIF

      CALL INKEYW ('REQ',' ')
      IF (KEYWIS ('UNSTRUC')) THEN

         IF (OPTG.NE.5) THEN
            CALL MSGERR (3,&
            &'define computational grid before reading unstructured grid')
         ENDIF
         IF (ONED) THEN
            CALL MSGERR (4,&
            &'1D-simulation cannot be done with unstructured grid')
            RETURN
         ENDIF

!          --- read unstructured grid

         LOGCOM(5) = .TRUE.
         CALL INKEYW ('STA', 'ADC')
         IF (KEYWIS('ADC')) THEN
            grid_generator = meth_adcirc
!             bottom topography will be taken from fort.14, so
            LOGCOM(3) = .TRUE.
            IGTYPE(1) = 3
            LEDS(1)   = 2
         ELSEIF (KEYWIS('TRIA')) THEN
            grid_generator = meth_triangle
            CALL INCSTR ('FNAME',FILENM,'REQ', ' ')
         ELSEIF (KEYWIS('EASY')) THEN
            grid_generator = meth_easy
            CALL INCSTR ('FNAME',FILENM,'REQ', ' ')
         ENDIF
         CALL SwanReadGrid ( FILENM, LENFNM )
         IF (STPNOW()) RETURN
      ELSE
         CALL SREDEP ( LWINDR, LWINDM, LOGCOM )
         IF (STPNOW()) RETURN
      ENDIF
!       --- call CGINIT once as soon as both coordinates and bottom
!           have been read and AC2 is not allocated
      IF (OPTG .EQ. 1) THEN
!         regular grid
         IF (LOGCOM(3) .AND. .NOT. LOGCOM(6)) THEN
            CALL CGINIT(LOGCOM)
            IF (STPNOW()) RETURN
         ENDIF
      ELSEIF (OPTG.EQ.3) THEN
!         cuvilinear grid
         IF (LOGCOM(3) .AND. .NOT. LOGCOM(4)) THEN
            CALL MSGERR (3, '** Give CGRID command and               *')
            CALL MSGERR (3, '** read curvilinear coordinates         *')
            CALL MSGERR (3, '** before reading the bottom grid       *')
         ELSE IF (LOGCOM(2) .AND. LOGCOM(3) .AND.&
         &LOGCOM(4) .AND. .NOT. LOGCOM(6)) THEN
            CALL CGINIT(LOGCOM)
            IF (STPNOW()) RETURN
         ENDIF
      ELSEIF (OPTG.EQ.5) THEN
!         unstructured grid

         IF ( .NOT.LOGCOM(5) ) THEN
            CALL MSGERR (3, '* read unstructured grid by means of    *')
            CALL MSGERR (3, '* SMS/ADCIRC, Triangle or Easymesh      *')
            IF ( LOGCOM(4) )&
            &CALL MSGERR (3, '* instead of curvilinear coordinates    *')
         ELSEIF ( LOGCOM(5) .AND. LOGCOM(2) .AND.&
         &.NOT.LOGCOM(4) .AND. .NOT.LOGCOM(6) .AND.&
         &.NOT.LOGCOM(7) ) THEN
!METIS            CALL SwanDecomposition (LOGCOM)
!METIS            IF (STPNOW()) RETURN
            IF ( PARLL .AND. .NOT.LOGCOM(7) )&
            &CALL MSGERR (4,&
            &'to run in parallel the mesh must be partitioned first')

!           --- create copies of parts of xcugrd and ycugrd
!               for each subdomain
            IF (.NOT.ALLOCATED(xcugrdgl)) ALLOCATE(xcugrdgl(nvertsg))
            IF (.NOT.ALLOCATED(ycugrdgl)) ALLOCATE(ycugrdgl(nvertsg))
            xcugrdgl = xcugrd
            ycugrdgl = ycugrd
            DEALLOCATE(xcugrd,ycugrd)
            ALLOCATE(xcugrd(nverts))
            ALLOCATE(ycugrd(nverts))

            IF ( .NOT.PARLL ) THEN
               xcugrd = xcugrdgl
               ycugrd = ycugrdgl
            ELSE
               DO J = 1, nverts
                  I = ivertg(J)
                  xcugrd(J) = xcugrdgl(I)
                  ycugrd(J) = ycugrdgl(I)
               ENDDO
            ENDIF

!           --- create grid topology
            CALL SwanCreateEdges
            IF (STPNOW()) RETURN
            CALL SwanGridTopology
            IF (STPNOW()) RETURN

!           --- initialize arrays for unstructured mesh
            CALL SwanInitCompGrid (LOGCOM)
            IF (STPNOW()) RETURN

!           --- the computational grid is included in output data
            IF ( nverts.GT.0 ) THEN
               ALLOCATE(OPSTMP)
               OPSTMP%PSNAME = 'COMPGRID'
               OPSTMP%PSTYPE = 'U'
               OPSTMP%MIP    = nverts
               ALLOCATE(OPSTMP%XP(nverts))
               ALLOCATE(OPSTMP%YP(nverts))
               DO JJ = 1, nverts
                  OPSTMP%XP(JJ) = xcugrd(JJ)
                  OPSTMP%YP(JJ) = ycugrd(JJ)
               ENDDO
               NULLIFY(OPSTMP%NEXTOPS)
               IF ( .NOT.LOPS ) THEN
                  FOPS = OPSTMP
                  COPS => FOPS
                  LOPS = .TRUE.
               ELSE
                  COPS%NEXTOPS => OPSTMP
                  COPS => OPSTMP
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     CGRID     definition of computational grid
!
! ======================================================================
!
!          / REGular [xpc] [ypc] [alpc] [xlenc] [ylenc] [mxc] [myc] \
!   CGRID <  CURVilinear [mxc] [myc]  [excval] [alpc]                >
!          \ UNSTRUCtured                                           /
!
!          / CIRcle               \
!          \ SECtor [dir1] [dir2] /  [mdc]  [flow]  [fhig]  [msc]
!
! ======================================================================

   IF (KEYWIS('CGRID') .OR. KEYWIS('GRID')) THEN
!       ver 20.67: command reorganised in view of cycle 2 (parametric &
!                  curvilinear)
!       ver 30.20: order of data changed
!       [xpc], [ypc], [alpc] moved
!
!
!       ver 30.21: CURVILINEAR


      LOGCOM(2) = .TRUE.
      CALL INKEYW ('STA', 'REG')
      IF (KEYWIS('CURV')) THEN
         OPTG  = 3
         XPC   = 0.
         YPC   = 0.
         ALPC  = 0.
         XCLEN = 0.
         YCLEN = 0.
         IF (ITEST.GE.30) THEN
            CALL MSGERR(1,'there is an unusual issue with the')
            CALL MSGERR(1,'curvilinear mode, so use with caution.')
            CALL MSGERR(1,'This issue occurs when the upwind point')
            CALL MSGERR(1,'has not been solved for yet, because it')
            CALL MSGERR(1,'falls into a different sweep. Normally,')
            CALL MSGERR(1,'an upwind point will always fall within')
            CALL MSGERR(1,'the same sweep, but this is not necessarily')
            CALL MSGERR(1,'the case when using curvilinear coordinates.')
            CALL MSGERR(1,'To state the problem another way:')
            CALL MSGERR(1,'the axis of the grid bends about the')
            CALL MSGERR(1,'direction associated with a particular')
            CALL MSGERR(1,'directional bin.')
            CALL MSGERR(1,'You may use command SET CURV or enlarge')
            CALL MSGERR(1,'parameter [mxitst].')
         END IF
      ELSEIF (KEYWIS('UNSTRUC')) THEN
         OPTG  = 5
         XPC   = 0.
         YPC   = 0.
         ALPC  = 0.
         XCLEN = 0.
         YCLEN = 0.
      ELSE
         CALL IGNORE ('REG')
         OPTG = 1
         IF (KSPHER.EQ.0) THEN
            CALL READXY ('XPC', 'YPC', XPC, YPC, 'STA', 0., 0.)
            CALL INREAL ('ALPC',ALPC,'UNC',0.)
         ELSE
!           spherical coordinates; [xpc] and [ypc] are required
            CALL READXY ('XPC', 'YPC', XPC, YPC, 'REQ', 0., 0.)
            CALL INREAL ('ALPC',ALPC,'STA',0.)
         ENDIF
         CALL INREAL('XLENC',XCLEN,'RQI',0.)
         IF (ONED) THEN
            CALL INREAL('YLENC',YCLEN,'STA',0.)
            IF (YCLEN .NE. 0) THEN
               CALL MSGERR (1, '1D-simulation: [ylenc] set to zero !')
            ENDIF
            YCLEN = 0.
         ELSE
            CALL INREAL('YLENC',YCLEN,'RQI',0.)
         ENDIF

!         ALPC is made to be between -PI and PI

         ALTMP = ALPC / 360.
         ALPC = PI2 * (ALTMP - NINT(ALTMP))
         CVLEFT = .TRUE.
      ENDIF

      IF (OPTG.NE.5) THEN

!       ***** MXC is the number of steps in X direction *****
!       ***** MYC is the number of steps in Y direction *****

      CALL ININTG('MXC',MXS,'RQI',0)

      IF (ONED) THEN
         CALL ININTG('MYC',MYS,'STA',0)
         IF (MYS .NE. 0) THEN
            CALL MSGERR (1, '1D-simulation: [myc] set to zero !')
         ENDIF
         MYS = 0
      ELSE
         CALL ININTG('MYC',MYS,'RQI',-1)
      ENDIF

      IF (KREPTX.EQ.1) THEN
         MXC = MXS
      ELSE
         MXC = MXS+1
      ENDIF
      MYC = MYS+1
      MMCGR = MXC*MYC
      DX  = XCLEN/MXS

      IF (ONED) THEN
         DY  = DX
      ELSE
         DY  = YCLEN/MYS
      ENDIF
      END IF

!       for curvilinear grid, read exception values for grid point coordinates

      IF (OPTG.EQ.3) THEN
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS ('EXC')) THEN
            CALL INREAL ('EXCVAL', EXCFLD(8), 'REQ', 0.)
            CALL INREAL ('EXCVAL', EXCFLD(9), 'STA', EXCFLD(8))
         ENDIF
         CALL INREAL ('ALPC',ALPC,'STA',0.)
         ALTMP = ALPC / 360.
         ALPC = PI2 * (ALTMP - NINT(ALTMP))
      ENDIF

      CALL INKEYW ('STA', 'CIR')
      IF (KEYWIS('SEC')) THEN
         FULCIR = .FALSE.
         CALL INREAL ('DIR1', SPDIR1, 'REQ', 0.)
         CALL INREAL ('DIR2', SPDIR2, 'REQ', 0.)
         CALL ININTG('MDC',MDC,'RQI',0)
      ELSE
         CALL IGNORE ('CIR')
         FULCIR = .TRUE.
         CALL ININTG('MDC',MDC,'RQI',0)
      ENDIF

!       ***** MSC is the number of frequencies in sigma direction (logaritmic)

      CALL INREAL('FLOW',FRLOW,'STA',-999.)
      CALL INREAL('FHIGH',FRHIG,'STA',-999.)
      CALL ININTG('MSC',MSS,'STA',-999)
      IVAL = MAX(ABS(MSS*NINT(FRLOW)),ABS(MSS*NINT(FRHIG)),&
      &ABS(NINT(FRLOW)*NINT(FRHIG)))
      IF (IVAL.GE.999**2) THEN
         CALL MSGERR(3,&
         &'At least, FLOW and FHIGH or MSC must be given!')
      END IF
      IVAL = MAX(ABS(NINT(FRLOW)),ABS(NINT(FRHIG)),ABS(MSS))
      IF (IVAL.NE.999) THEN
         GAMMA = EXP(ALOG(FRHIG/FRLOW)/REAL(MSS)) - 1.
         WRITE (PRINTF,'(A,(F7.4))')&
         &' Resolution in sigma-space: df/f = ',GAMMA
      ELSE IF (MSS.EQ.-999) THEN
         MSS = NINT(ALOG(FRHIG/FRLOW)/ALOG(1.1))
         WRITE (PRINTF,'(A,I3)')&
         &' Number of meshes in sigma-space: MSC-1 = ',MSS
      ELSE IF (FRLOW.EQ.-999.) THEN
         FRLOW = FRHIG/EXP(ALOG(1.1)*REAL(MSS))
         WRITE (PRINTF,'(A,(F7.4))')&
         &' Lowest discrete frequency (in Hz): FLOW = ',FRLOW
      ELSE IF (FRHIG.EQ.-999.) THEN
         FRHIG = FRLOW*EXP(ALOG(1.1)*REAL(MSS))
         WRITE (PRINTF,'(A,(F7.4))')&
         &' Highest discrete frequency (in Hz): FHIGH = ',FRHIG
      END IF
      SLOW = 2.*PI*FRLOW
      SHIG = 2.*PI*FRHIG
      MSC  = MSS+1

!       ***** MDC is the number of steps in theta direction as part of a circle

      IF (FULCIR) THEN
         DDIR  = PI2 / MDC

!         modification of SPDIR1 first installed with version 30.72, then
!         reversed and reinstalled with 40.13
!         purpose: prevent problem with grids under 45 degrees
         SPDIR1 = ALPC + 0.5 * DDIR
      ELSE
         IF (BNAUT) THEN
!           swap values of SPDIR1 and SPDIR2, and transform
            TMPDIR = SPDIR1
            SPDIR1 = 180. + DNORTH - SPDIR2
            SPDIR2 = 180. + DNORTH - TMPDIR
         ENDIF
         SPDIR1 = SPDIR1 * PI / 180.
         SPDIR2 = SPDIR2 * PI / 180.
         IF (SPDIR2.LT.SPDIR1) SPDIR2 = SPDIR2 + PI2
         DDIR = (SPDIR2-SPDIR1) / REAL(MDC)
         MDC = MDC + 1
      ENDIF

      IF(.NOT.ALLOCATED(SPCSIG)) ALLOCATE(SPCSIG(MSC))
      IF(.NOT.ALLOCATED(SPCDIR)) ALLOCATE(SPCDIR(MDC,6))
      CALL SSFILL(SPCSIG,SPCDIR,SPECTRAL_POWERS)

      IF (ITEST.GE. 20) THEN
         IF(OPTG .EQ. 1)WRITE (PRINTF,"('GRID: REGULAR RECTANGULAR')")
         IF(OPTG .EQ. 3)WRITE (PRINTF,"('GRID: CURVILINEAR')")
         IF(OPTG .EQ. 5)WRITE (PRINTF,"('GRID: UNSTRUCTURED')")
         WRITE (PRINTF,"(' S-low: ', F6.3,' S-hig: ', F6.3, ' frintf: ', F6.3)") SLOW, SHIG, FRINTF
         IF (OPTG.NE.5) WRITE (PRINTF,"(' MXC: ',I6,' MYC: ',I6,' MDC: ',I6,' MSC: ',I6)") MXC,MYC,MDC,MSC
         IF (OPTG.NE.5) WRITE (PRINTF,"(' DX: ',E12.4,' DY: ',E12.4, ' DDIR: ', F6.3)") DX,DY,DDIR
         IF (OPTG.EQ.5) WRITE (PRINTF,"(' MDC: ',I6,' MSC: ',I6, ' DDIR: ', F6.3)") MDC,MSC,DDIR
      ENDIF

      IF(.NOT.ALLOCATED(XCGRID)) ALLOCATE(XCGRID(MXC,MYC))
      IF(.NOT.ALLOCATED(YCGRID)) ALLOCATE(YCGRID(MXC,MYC))

      IF (OPTG .EQ. 1) THEN
!         *** The coordinates of computational points in ***
!         *** regular grid are computed                  ***
         COSPC = COS(ALPC)
         SINPC = SIN(ALPC)
         do J = 1,MYC
            do I = 1, MXC
               VALX = XPC + COSPC*(I-1)*DX - SINPC*(J-1)*DY
               VALY = YPC + SINPC*(I-1)*DX + COSPC*(J-1)*DY
               XCGRID(I,J)=VALX
               YCGRID(I,J)=VALY
            end do
         end do
      ENDIF

!       *** the computational grid is included in output data  ***
      IF ((MXC.GT.0) .AND. (MYC.GT.0) ) THEN
         ALLOCATE(OPSTMP)
         OPSTMP%PSNAME = 'COMPGRID'
         IF (OPTG .EQ. 1) THEN
            OPSTMP%PSTYPE = 'F'
            OPSTMP%OPR(1) = XPC
            OPSTMP%OPR(2) = YPC
            OPSTMP%OPR(3) = XCLEN
            OPSTMP%OPR(4) = YCLEN
            OPSTMP%OPR(5) = ALPC
         ELSE IF (OPTG.EQ.3) THEN
            OPSTMP%PSTYPE = 'H'
            OPSTMP%OPR(1) = FLOAT(MXC-1)
            OPSTMP%OPR(2) = FLOAT(MYC-1)
            OPSTMP%OPR(3) = 0.
            OPSTMP%OPR(4) = 0.
            OPSTMP%OPR(5) = ALPC
         ENDIF
         OPSTMP%OPI(1) = MXC
         OPSTMP%OPI(2) = MYC
         ALLOCATE(OPSTMP%XP(0))
         ALLOCATE(OPSTMP%YP(0))
         NULLIFY(OPSTMP%NEXTOPS)
         IF ( .NOT.LOPS ) THEN
            FOPS = OPSTMP
            COPS => FOPS
            LOPS = .TRUE.
         ELSE
            COPS%NEXTOPS => OPSTMP
            COPS => OPSTMP
         END IF
      ENDIF

      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     BOUNdary  defining boundary conditions

   IF (KEYWIS ('BOU')) THEN
      ITMP1  = MXC
      ITMP2  = MYC
      ITMP3  = MCGRD
      ITMP4  = NGRBND
      MXC    = MXCGL
      MYC    = MYCGL
      MCGRD  = MCGRDGL
      NGRBND = NGRBGL
      CALL SWBOUN ( XGRDGL, YGRDGL, KGRPGL, XYTST, KGRBGL )
      IF (STPNOW()) RETURN
      MXC    = ITMP1
      MYC    = ITMP2
      MCGRD  = ITMP3
      NGRBND = ITMP4
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     LIM     setting parameters in conjunction with action limiter
!
! ============================================================
!
!   LIMiter [ursell] [qb]
!
! ============================================================

   IF (KEYWIS ('LIM')) THEN
      CALL INREAL('URSELL',PTRIAD(3),'UNC',0.)
      CALL INREAL('QB'    ,PNUMS(28),'UNC',0.)
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     NUM       setting numerical parameters for schemes, solvers, etc.
!
! =====================================================================
!
!         | -> STOPC [dabs] [drel] [curvat] [npnts] [dtabs] [curvt] |
!   NUM (<                                                           > &  40.41
!         | ACCUR [drel] [dhoval] [dtoval] [npnts]                  |
!
!                    | -> STAT  [mxitst] [alfa] |
!                   <                            >  [limiter]   )     &
!                    | NONSTat  [mxitns]        |
!
!           ( DIRimpl [cdd] [cdlim]  DEP|WNUM                      )  &
!
!           ( REFRLim [frlim] [power]          (NOT documented)    )  &
!
!           | -> SIGIMpl [css] [eps2] [outp] [niter]               |
!          (<                                                      >) &
!           |    SIGEXpl [css] [cfl]           (NOT documented)    |
!           |                                                      |
!           |    FIL     [diffc]               (NOT documented)    |
!
!           ( CTheta [cfl]                                         )  &
!
!           ( CSigma [cfl]                                         )  &
!
!           ( SETUP [eps2] [outp] [niter]                          )
!
! =====================================================================

   IF (KEYWIS ('NUM')) THEN
      CALL INKEYW ('REQ','  ')
!       *** accuracy and criterion to terminate the iteration ***
      IF (KEYWIS ('STOPC')) THEN
         PNUMS(21) = 1.
         CALL INREAL ('DABS'   , PNUMS(2) , 'UNC', 0.)
         CALL INREAL ('DREL'   , PNUMS(1) , 'UNC', 0.)
         CALL INREAL ('CURVAT' , PNUMS(15), 'UNC', 0.)
         CALL INREAL ('NPNTS'  , PNUMS(4) , 'UNC', 0.)
         CALL INREAL ('DTABS'  , PNUMS(3) , 'STA', 1000.)
         CALL INREAL ('CURVT'  , PNUMS(16), 'STA', 1000.)
         CALL INKEYW ('STA', 'STAT')
         IF (KEYWIS ('STAT')) THEN
            CALL ININTG ('MXITST' , MXITST   , 'STA', 50)
            CALL INREAL ('ALFA'   , PNUMS(30), 'UNC', 0.)
         ELSE IF (KEYWIS ('ITERMX')) THEN
            CALL ININTG ('MXITST' , MXITST   , 'REQ', 0 )
            MXITNS = MXITST
            CALL MSGERR (1, '[itermx] is replaced; see user manual')
         ELSE IF (KEYWIS ('NONST')) THEN
            CALL ININTG ('MXITNS' , MXITNS   , 'REQ', 0 )
         ENDIF
         CALL INREAL ('LIMITER', PNUMS(20), 'UNC', 0.)
      ELSE IF (KEYWIS ('ACCUR')) THEN
         PNUMS(21) = 0.
         CALL INREAL ('DREL'   , PNUMS(1) , 'UNC', 0.)
         CALL INREAL ('DHOVAL' , PNUMS(15), 'UNC', 0.)
         CALL INREAL ('DTOVAL' , PNUMS(16), 'UNC', 0.)
         CALL INREAL ('NPNTS'  , PNUMS(4) , 'UNC', 0.)
         CALL INKEYW ('STA', 'STAT')
         IF (KEYWIS ('STAT')) THEN
            CALL ININTG ('MXITST' , MXITST   , 'UNC', 0 )
            CALL INREAL ('ALFA'   , PNUMS(30), 'UNC', 0.)
         ELSE IF (KEYWIS ('ITERMX')) THEN
            CALL ININTG ('MXITST' , MXITST   , 'REQ', 0 )
            MXITNS = MXITST
            CALL MSGERR (1, '[itermx] is replaced; see user manual')
         ELSE IF (KEYWIS ('NONST')) THEN
            CALL ININTG ('MXITNS' , MXITNS   , 'REQ', 0 )
         ENDIF
         CALL INREAL ('LIMITER', PNUMS(20), 'UNC', 0.)
      END IF

!       *** numerical scheme in directional space (standard  ***
!       *** option implicit scheme)                          ***

      CALL INKEYW ('STA','  ')
      IF (KEYWIS ('DIR')) THEN
         CALL INREAL ('CDD'    , PNUMS(6) , 'UNC', 0.)
         CALL INREAL ('CDLIM'  , PNUMS(17), 'STA',-1.)
         IF (PNUMS(17).LT.0.) IREFR = 1
         IF (EQREAL(PNUMS(17),0.)) THEN
            IREFR = 0
            CALL MSGERR(0, 'Refraction deactivated')
         ENDIF
         CALL INKEYW ('STA', 'WNUM')
         IF (KEYWIS('DEP')) THEN
            PNUMS(32) = 0.
         ELSE IF (KEYWIS('WNUM')) THEN
            PNUMS(32) = 1.
         ENDIF
      ENDIF

!       limit Ctheta if user want so

      CALL INKEYW ('STA','  ')
      IF (KEYWIS ('REFRL')) THEN
         PNUMS(29) = 1.
         CALL INREAL ('FRLIM', PNUMS(26) ,'UNC', 0.)
         CALL INREAL ('POWER', PNUMS(27), 'UNC', 0.)
      ENDIF

!       *** numerical scheme in frequency space :                  ***
!       *** 1) Fully implicit scheme in frequency space (iterative ***
!       ***    SIP solver)                                         ***
!       *** 2) Explicit scheme in frequency space:                 ***
!       ***    Energy is removed from the spectrum based on a CFL  ***
!       ***    criterion                                           ***
!       *** 3) Explicit scheme in frequency space:                 ***
!       ***    No CFL limitation near blocking point --> unstable  ***
!       ***    integration. Therefore a filter is applied with a   ***
!       ***    diffusion coeff.                                    ***

      CALL INKEYW ('STA','  ')
      IF (KEYWIS('SIGIM') .OR. KEYWIS ('IMP')) THEN
!         *** implicit solver ***
!         ***  This is the default option   PNUMS(8) = 1.  ***
         PNUMS(8) = 1.
         CALL INREAL ('CSS'  , PNUMS (7), 'UNC', 0.)
         CALL INREAL ('EPS1' , PNUMS(11), 'UNC', 0.)
         CALL INREAL ('EPS2' , PNUMS(12), 'UNC', 0.)
         CALL INREAL ('OUTP' , PNUMS(13), 'UNC', 0.)
         CALL INREAL ('NITER', PNUMS(14), 'UNC', 0.)
      ELSE IF (KEYWIS('SIGEX') .OR. KEYWIS('EXP')) THEN

!         *** 2) explicit scheme ***
         PNUMS(8) = 2.
         CALL INREAL ('CSS'  , PNUMS (7), 'UNC', 0.)
         CALL INREAL ('CFL'  , PNUMS(19), 'UNC', 0.)

      ELSE IF (KEYWIS ('FIL')) THEN
!         *** 3) explicit scheme -> filter the spectrum  ***
         PNUMS(8) = 3.
         CALL INREAL ('DIFFC'  , PNUMS(9), 'UNC', 0.)

      END IF

!       limit Ctheta if user want so

      CALL INKEYW ('STA','  ')
      IF (KEYWIS ('CT')) THEN
         PNUMS(35) = 1.
         CALL INREAL ('CFL', PNUMS(36) ,'STA', 0.9)
      ENDIF

!       limit Csigma if user want so

      CALL INKEYW ('STA','  ')
      IF (KEYWIS ('CS')) THEN
         PNUMS(33) = 1.
         CALL INREAL ('CFL', PNUMS(34) ,'STA', 0.9)
      ENDIF

      CALL INKEYW ('STA','  ')
      IF (KEYWIS('SETUP')) THEN
!         *** iterative solver        ***
!         *** Settings for the setup  ***
         CALL INREAL ('EPS2' , PNUMS(23), 'UNC', 0.)
         CALL INREAL ('OUTP' , PNUMS(24), 'UNC', 0.)
         CALL INREAL ('NITER', PNUMS(25), 'UNC', 0.)
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!     SETUP     include wave-induced set-up in SWAN calculation
!
! ============================================================
!
!   SETUP [supcor]
!
! ============================================================

   IF (KEYWIS ('SETUP')) THEN
      IF (KSPHER.NE.0) CALL MSGERR (2,&
      &'setup will not be compute correctly with spherical coordinates')
      IF (PARLL .AND. .NOT.ONED) THEN
         CALL MSGERR(4,'setup is not supported in parallel run')
         RETURN
      END IF
      IF (OPTG.EQ.5) THEN
         CALL MSGERR(2, ' No setup for unstructured grid')
         LSETUP = 0
         CYCLE command_loop
      ENDIF
      LSETUP = 1
      CALL INREAL ('SUPCOR', PSETUP(2), 'UNC', 0.)
!       *** set pointers for setup and saved depth in array COMPDA ***
      JSETUP = MCMVAR + 1
      JDPSAV = MCMVAR + 2
      MCMVAR = MCMVAR + 2
      ALOCMP = .TRUE.
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     DIFFRac   include diffraction approximation
!
! =============================================================
!
!   DIFFRac  [idiffr]  [smpar]  [smnum]  [cgmod]
!
! =============================================================

   IF (KEYWIS ('DIFFR')) THEN
      CALL ININTG ('IDIFFR', IDIFFR, 'STA', 1)
      CALL INREAL ('SMPAR', PDIFFR(1), 'STA', 0.0)
      CALL INREAL ('SMNUM', PDIFFR(2), 'STA', 0.)
      CALL INREAL ('CGMOD', PDIFFR(3), 'STA', 1.)
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     SURFBeat  include infragravity energy modelling
!
! ============================================================
!
!   SURFBeat  [df] [nmax] [emin]  UNIF/LOG
!
! ============================================================

   IF ( KEYWIS ('SURFB') ) THEN
      IF ( ONED ) THEN
         CALL MSGERR(2,&
         &'surfbeat computation not allowed in 1D mode')
      ENDIF
      IF ( OPTG.NE.1 ) THEN
         CALL MSGERR(2,&
         &'surfbeat only supported for rectilinear grids')
         LSRFB = .FALSE.
         CYCLE command_loop
      ENDIF
      LSRFB = .TRUE.
      CALL INREAL ('DF'  , dfiem , 'STA', 0.01 )
      CALL ININTG ('NMAX', nmax  , 'STA', 50000)
      CALL INREAL ('EMIN', e_trsh, 'STA', 0.05 )

      CALL INKEYW ('STA',' ')
      IF (KEYWIS ('UNIF')) sflog = .FALSE.
      IF (KEYWIS ('LOG' )) sflog = .TRUE.
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     SET       setting physical parameters and error counters
!
! =============================================================
!
!   SET  [level]  [nor]  [depmin]  [maxmes]                &
!        [maxerr]  [grav]  [rho] [cdcap] [uscap] [inrhog]  &
!        [hsrerr]  CARTesian/NAUTical  [pwtail] [froudmax] &
!        [icewind]  [excmark]                              &
!        [sort]  [nsweep]  CURV  (not documented)
!        [printf]  [prtest] (not documented)
!
! =============================================================

   IF (KEYWIS ('SET')) THEN
      CALL INREAL ('LEVEL',  WLEV,   'UNC', 0.)
      CALL INREAL ('NOR',    DNORTH, 'UNC', 0.)
      CALL INREAL ('DEPMIN', DEPMIN, 'UNC', 0.)
      CALL ININTG ('MAXMES', MAXMES, 'UNC', 0)
      CALL ININTG ('MAXERR', MAXERR, 'UNC', 0)
      CALL INREAL ('GRAV',   GRAV,   'UNC', 0.)
      CALL INREAL ('RHO',    RHO,    'UNC', 0.)
      CALL INREAL ('CDCAP',  CDCAP,  'UNC', 0.)
      CALL INREAL ('USCAP',  USCAP,  'UNC', 0.)
      CALL ININTG ('INRHOG', INRHOG, 'UNC', 0)
      CALL INREAL ('HSRERR', HSRERR, 'UNC', 0.)

      CALL INKEYW ('STA',' ')
      IF (KEYWIS ('NAUT')) THEN
         BNAUT = .TRUE.
         OUTPAR(4) = 0.
      ENDIF
      IF (KEYWIS ('CART')) BNAUT = .FALSE.
      IF ( ITEST .GE. 20 ) THEN
         WRITE (PRTEST,*) ' Set NAUT command: BNAUT=', BNAUT
      ENDIF

      CALL INREAL ('PWTAIL', PWTAIL(1), 'UNC', 0.)
      IF (default_command_reader%CHGVAL) THEN
         IF (PWTAIL(1).LE.1.) CALL MSGERR (3, 'Incorrect PWTAIL')
         PWTAIL(3) = PWTAIL(1) + 1.
      ENDIF
      CALL INREAL ('FROUDMAX', PNUMS(18), 'UNC', 0.)
      CALL INREAL ('ICEWIND' , ICEWIND  , 'UNC', 0.)
      CALL ININTG ('EXCMARK' , excmark  , 'UNC', 0 )
      CALL INREAL ('SORT'    , usort    , 'UNC', 0.)
      CALL ININTG ('NSWEEP'  , nsweep   , 'UNC', 0 )
      IF (KEYWIS ('CURV')) CCURV = .TRUE.
!        the unit numbers PRINTF and PRTEST should be set in swaninit,
!        so not documented
      CALL ININTG ('PRINTF'  , PRINTF   , 'UNC', 0 )
      CALL ININTG ('PRTEST'  , PRTEST   , 'UNC', 0 )
      if ( usort.gt.-999. ) then
         usort = DEGCNV (usort)
         ALTMP = usort / 360.
         usort = PI2 * (ALTMP - NINT(ALTMP))
      endif
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     QUANTITY  setting parameters for output quantities
!
! =============================================================
!
!   QUANTity  <...>  'short'  'long'  [lexp]  [hexp]  [excv]  &
!
!         [ref]                    {for output quantity TSEC}
!         [power]                  {for output quantity WLEN, PER or RPER}
!         [fswell]                 {for output quantity HSWELL}
!         [fmin]  [fmax]           {for integral parameters}
!         PROBLEM/FRAME            {for directions and vectors)
!         [noswll]                 {for partition output quantities}
!
! =============================================================

   IF (KEYWIS('QUANT')) THEN
      NVAR    = 0
      FRSTQ%I = 0
      NULLIFY(FRSTQ%NEXTI)
      CURRQ => FRSTQ
      DO
         CALL SVARTP (IVTYPE)
         IF (IVTYPE.LT.1 .OR. IVTYPE.GT.NMOVAR) EXIT
         NVAR = NVAR+1
         ALLOCATE(TMPQ)
         TMPQ%I = IVTYPE
         NULLIFY(TMPQ%NEXTI)
         CURRQ%NEXTI => TMPQ
         CURRQ => TMPQ
         CALL INCSTR ('SHORT', QOVSNM, 'STA', ' ')
         CALL INCSTR ('LONG' , QOVLNM, 'STA', ' ')
         CALL INREAL ('LEXP' , QR(1) , 'STA',  999. )
         CALL INREAL ('HEXP' , QR(2) , 'STA', -999. )
         CALL INREAL ('EXCV' , QR(3) , 'STA',  999. )
         CALL INCTIM (ITMOPT, 'REF', DVAL, 'STA', -9.99D2)
         QR(4) = REAL(DVAL)
         CALL INREAL ('POWER', QR(5) , 'STA', -999.)
         CALL INREAL ('FSWELL', QR(6), 'STA', -999.)
         CALL INREAL ('FMIN' , QR(8) , 'STA',  999. )
         CALL INREAL ('FMAX' , QR(9) , 'STA', -999. )
         CALL INREAL ('NOSWLL', QR(10), 'STA', -999. )
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('PROBLEM') .OR. KEYWIS('USER')) THEN
            QR(7) = 0.
         ELSE IF (KEYWIS('FRAME')) THEN
            QR(7) = 1.
         ELSE
            QR(7) = -999.
         ENDIF
      END DO
      IF (IVTYPE.NE.999) THEN
         CALL MSGERR (2, 'unknown quantity ')
      ENDIF

      IF (NVAR.GT.0) THEN
         CURRQ => FRSTQ%NEXTI
         DO JJ = 1, NVAR
            IVTYPE = CURRQ%I
            IF (QOVSNM.NE.' ') OVSNAM(IVTYPE) = QOVSNM
            IF (QOVLNM.NE.' ') OVLNAM(IVTYPE) = QOVLNM
            IF (QR(1).NE. 999.) OVLEXP(IVTYPE) = QR(1)
            IF (QR(2).NE.-999.) OVHEXP(IVTYPE) = QR(2)
            IF (QR(3).NE. 999.) OVEXCV(IVTYPE) = QR(3)
            IF (IVTYPE.EQ.40 .OR. IVTYPE.EQ.41) THEN
               IF (NSTATM.EQ.0) CALL MSGERR (2,&
               &'Time output asked in stationary mode')
            ENDIF
            IF (IVTYPE.EQ.41) THEN
               IF (QR(4).NE.-999.) OUTPAR(1) = QR(4)
            ELSE IF (IVTYPE.EQ.42 .OR. IVTYPE.EQ.43) THEN
               IF (QR(5).NE.-999.) OUTPAR(2) = QR(5)
            ELSE IF (IVTYPE.EQ.17) THEN
               IF (QR(5).NE.-999.) OUTPAR(3) = QR(5)
            ELSE IF (IVTYPE.EQ.44) THEN
               IF (QR(6).NE.-999.) OUTPAR(5) = QR(6)
            ELSE IF ((IVTYPE.EQ.100).OR.(IVTYPE.EQ.110).OR.&
            &(IVTYPE.EQ.120).OR.(IVTYPE.EQ.130).OR.&
            &(IVTYPE.EQ.140).OR.(IVTYPE.EQ.150).OR.&
            &(IVTYPE.EQ.160).OR.(IVTYPE.EQ.170) ) THEN
               IF (QR(10).NE.-999.) OUTPAR(51) = QR(10)
            ENDIF
            IF ( ANY( IVTYPE == IVOTP ) ) THEN
               INDX = MINLOC(IVOTP, IVOTP==IVTYPE)
               IF (QR(8).NE.999.) THEN
                  OUTPAR(INDX(1)+ 5) = 1.
                  OUTPAR(INDX(1)+20) = QR(8)
               ENDIF
               IF (QR(9).NE.-999.) THEN
                  OUTPAR(INDX(1)+ 5) = 1.
                  OUTPAR(INDX(1)+35) = QR(9)
               ENDIF
               IF ( OUTPAR(INDX(1)+20).GT.OUTPAR(INDX(1)+35) )&
               &CALL MSGERR (2,'fmin cannot be larger than fmax')
            ELSE
               IF (QR(8).NE.999. .OR. QR(9).NE.-999.) CALL MSGERR (1,&
               &'Integration range not allowed for this quantity')
            ENDIF
            IF (OVSVTY(IVTYPE).EQ.2 .OR. OVSVTY(IVTYPE).EQ.3) THEN
!              direction or vector
               IF (QR(7).NE.-999.) THEN
                  OUTPAR(4) = QR(7)
                  IF (BNAUT.AND.OVSVTY(IVTYPE).EQ.2) CALL MSGERR (1,&
                  &'option not allowed with Nautical convention')
               ENDIF
            ENDIF
            CURRQ => CURRQ%NEXTI
         END DO
         DEALLOCATE(TMPQ)
      ENDIF
      CYCLE command_loop
   ENDIF

!CTGA 111003:  Full SWAN ice implementation, and giving SWAN the ability
!              to recognize and read in the ice parameters from the
!              control file (traditionally fort.26 for PADCSWAN).
!
!     ------------------------------------------------------------------
!
!     CICE      Ice-wave interaction
!
! ============================================================
!
!        |    ADCICE  [wbicethr]
! CICE  <
!
! ============================================================
   IF (KEYWIS('CICE')) THEN
      IICE=1
      CALL INKEYW('STA','ADCICE')
      IF (KEYWIS('ADCICE')) THEN
         IICE=2
         CALL INREAL('WBICETH',WBICETH,'STA',70.0)
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     BREAK     parameters surf breaking
!
! ============================================================
!
!           | -> CONstant [alpha] [gamma]
!           |
!           |    VARiable [alpha] [gammin] [gammax] [gamneg] [coeff1] [coeff2] |
! BREaking <
!           |    RUEssink [alpha] [a] [b]
!           |
!           |    BKD [alpha] [gamma0] [a1] [a2] [a3] [npnts]
!           |
!           |    TG [alpha] [gamma] [pown]
!
!       ( DIRectionality [spread] )
!
!       ( FREQDep [power] [fmin] [fmax] ) (fmin and fmax not documented)
!
! ============================================================

   IF (KEYWIS ('BRE')) THEN
      CALL INKEYW ('STA', 'CON')
      IF (KEYWIS('CON')) THEN
         ISURF = 1
         CALL INREAL ('ALPHA', PSURF(1), 'STA', 1.0)
         CALL INREAL ('GAMMA', PSURF(2), 'STA', 0.73)
      ELSE IF (KEYWIS('VAR') .OR. KEYWIS('NEL')) THEN
         ISURF = 2
         CALL INREAL ('ALPHA',  PSURF(1), 'STA', 1.5)
         CALL INREAL ('GAMMIN', PSURF(4), 'STA', 0.55)
         CALL INREAL ('GAMMAX', PSURF(5), 'STA', 0.81)
         CALL INREAL ('GAMNEG', PSURF(6), 'STA', 0.73)
         CALL INREAL ('COEFF1', PSURF(7), 'STA', 0.88)
         CALL INREAL ('COEFF2', PSURF(8), 'STA', 0.012)
      ELSE IF (KEYWIS('RUE')) THEN
         ISURF = 3
         CALL INREAL ('ALPHA', PSURF(1), 'STA', 1.0)
         CALL INREAL ('A'    , PSURF(4), 'STA', 0.76)
         CALL INREAL ('B'    , PSURF(5), 'STA', 0.29)
      ELSE IF (KEYWIS('TG')) THEN
         ISURF = 4
         CALL INREAL ('ALPHA',  PSURF(1), 'STA', 1.0)
         CALL INREAL ('GAMMA',  PSURF(4), 'STA', 0.42)
         CALL INREAL ('POWN' ,  PSURF(5), 'STA', 4.0)
      ELSE IF (KEYWIS('BKD')) THEN
         ISURF = 6
         CALL INREAL ('ALPHA' , PSURF(1) ,'STA', 1.00)
         CALL INREAL ('GAMMA0', PSURF(4) ,'STA', 0.54)
         CALL INREAL ('A1'    , PSURF(5) ,'STA', 7.59)
         CALL INREAL ('A2'    , PSURF(6) ,'STA',-8.06)
         CALL INREAL ('A3'    , PSURF(7) ,'STA', 8.09)
!         [npnts] not documented!
         CALL INREAL ('NPNTS' , PNUMS(37),'STA', 95. )

         JGAMMA = MCMVAR+1
         MCMVAR = MCMVAR+1
         ALOCMP = .TRUE.
      ELSE IF (KEYWIS('ASYM')) THEN
         ISURF = 7
         CALL INREAL ('ALPHA' , PSURF(1) ,'STA', 1.0)
         CALL INREAL ('GAMMA0', PSURF(4) ,'STA', 0.6)
         CALL INREAL ('A'     , PSURF(5) ,'STA', 0.8)
         JGAMMA = MCMVAR+1
         MCMVAR = MCMVAR+1
         ALOCMP = .TRUE.
      END IF

      CALL INKEYW ('STA', '  ')
      IF (KEYWIS ('DIR').OR.ISURF.EQ.6) THEN
         IDISRF = 1
         CALL INREAL ('SPREAD', PSURF(15), 'STA', 12.5)
      END IF

      CALL INKEYW ('STA', '  ')
      IF (KEYWIS ('FREQD')) THEN
         IFRSRF = 1
         CALL INREAL ('POWER', PSURF(16), 'STA', 2.0)
         CALL INREAL ('FMIN' , PSURF(17), 'STA', 0.0)
         CALL INREAL ('FMAX' , PSURF(18), 'STA', 1000.)
      END IF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     WCAP        parameters whitecapping (NOT documented)
!
! =============================================================
!
!        |    KOMen   [cds2] [stpm] [powst] [delta] [powk]
!        |
!        |    JANSsen [cds1]  [delta] [pwtail]
!        |
!        |    LHIG    [cflhig]
!        |
! WCAP  <     BJ      [bjstp] [bjalf]
!        |
!        |    KBJ     [bjstp] [bjalf] [kconv]
!        |
!        | -> AB      [cds2] [br]  CURrent [cds3]
!
! =============================================================

   IF (KEYWIS ('WCAP')) THEN
      CALL INKEYW ('STA','AB')
      IF (KEYWIS ('KOM')) THEN
!         *** whitecapping according to Komen et al. (1984) ***
!
!         error checking in case user has already selected Babanin physics
         IF (WCAPSET) CALL MSGERR (4, 'whitecapping is already set')
         IWCAP = 1
         WCAPSET = .TRUE.
         CALL INREAL ('CDS2',  PWCAP(1),  'UNC', 0.)
         CALL INREAL ('STPM',  PWCAP(2),  'UNC', 0.)
         CALL INREAL ('POWST', PWCAP(9),  'UNC', 0.)
         CALL INREAL ('DELTA', PWCAP(10), 'UNC', 0.)
         CALL INREAL ('POWK',  PWCAP(11), 'UNC', 0.)
      ELSE IF ( KEYWIS ('JANS')) THEN
!         *** whitecapping according to Janssen (1989, 1991) ***
         IWCAP = 2
         CALL INREAL ('CDS1',  PWCAP(3), 'UNC', 0.)
         CALL INREAL ('DELTA', PWCAP(4), 'UNC', 0.)

!         Recalculate coefficients that are actually used in the
!         whitecapping routines
         PWCAP(1)  = PWCAP(3) * (PWCAP(2) ** PWCAP(9))
         PWCAP(10) = PWCAP(4)
         JUSTAR = MCMVAR+1
         JZEL   = MCMVAR+2
         JCDRAG = MCMVAR+3
         JTAUW  = MCMVAR+4
         MCMVAR = MCMVAR+4
         ALOCMP = .TRUE.
         CALL INREAL ('PWTAIL', PWTAIL(1), 'STA', 5.)
         IF (PWTAIL(1).LE.1.) CALL MSGERR (3, 'Incorrect PWTAIL')
         PWTAIL(3) = PWTAIL(1) + 1.
      ELSE IF ( KEYWIS ('LHIG')) THEN
!         *** whitecapping according to Longuett Higgins ***
         IWCAP = 3
         CALL INREAL ('CFLHIG', PWCAP(5), 'UNC', 0.)
      ELSE IF ( KEYWIS ('BJ')) THEN
!         *** whitecapping according to Battjes/Janssen formulation ***
         IWCAP = 4
         CALL INREAL ('BJSTP' , PWCAP(6), 'UNC', 0.)
         CALL INREAL ('BJALF' , PWCAP(7), 'UNC', 0.)
      ELSE IF ( KEYWIS ('KBJ')) THEN
!         *** whitecapping according to a combination of Komen et al ***
!         *** and Battjes/Janssen                                    ***
         IWCAP = 5
         CALL INREAL ('BJSTP' , PWCAP(6), 'UNC', 0.)
         CALL INREAL ('BJALF' , PWCAP(7), 'UNC', 0.)
         CALL INREAL ('KCONV' , PWCAP(8), 'UNC', 0.)
      ELSE IF ( KEYWIS ('AB')) THEN
!         *** whitecapping according to Alves and Banner (2003) ***
         IWCAP = 7
         CALL INREAL ('CDS2',  PWCAP(1),  'STA', 5.0E-5)
         CALL INREAL ('BR',    PWCAP(12), 'STA', 1.75E-3)
         CALL INKEYW ('STA', '  ')
         IF (KEYWIS ('CUR')) THEN
            IWCCUR = 1
            CALL INREAL ('CDS3', PWCAP(14), 'STA', 0.8)
         ENDIF
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     FRIC  setting parameters for bottom friction
!
! ===============================================
!
!                   |              | -> CONstant [cfjon]
!                   | -> JONswap  <
!                   |              |    VARiable [cfj1] [cfj2] [dsp1] [dsp2]   (NOT documented)
!                   |
!                   |    COLLins   [cfw]    &
!   FRICtion       <
!                   |              [cfc]      (NOT documented)
!                   |
!                   |    MADsen    [kn]
!                   |
!                   |    RIPples   [S] [D]
!
! ===============================================

   IF (KEYWIS ('FRIC')) THEN
      IBOT = 1
      CALL INKEYW ('STA','JON')
      IF (KEYWIS('JON')) THEN
         CALL INKEYW ('STA', 'CON')
         IF (KEYWIS('VAR')) THEN
            IBOT = 4
            CALL INREAL ('CFJ1', PBOT(6), 'STA', 0.038)
            CALL INREAL ('CFJ2', PBOT(7), 'STA', 0.067)
            CALL INREAL ('DSP1', PBOT(8), 'STA', 10.)
            CALL INREAL ('DSP2', PBOT(9), 'STA', 30.)
         ELSE
            CALL IGNORE ('CON')
            IBOT = 1
            CALL INREAL('CFJON',PBOT(3),'UNC',0.)
         ENDIF
      ELSE IF (KEYWIS('COLL')) THEN
         IBOT = 2
         CALL INREAL('CFW',PBOT(2),'UNC',0.)
         CALL INREAL('CFC',PBOT(1),'UNC',0.)
      ELSE IF (KEYWIS('MAD')) THEN
         IBOT = 3
         CALL INREAL('KN',PBOT(5),'UNC',0.)
      ELSE IF (KEYWIS('RIP')) THEN
         IBOT = 5
         CALL INREAL('S',PBOT(6),'UNC',0.)
         CALL INREAL('D',PBOT(7),'UNC',0.)
         JFRC2  = MCMVAR+1
         MCMVAR = MCMVAR+1
         ALOCMP = .TRUE.
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

! ==========================================
!
!   MUD  [layer]  [rhom]  [viscm]  [rhow]  [viscw]
!
! ==========================================

   IF (KEYWIS ('MUD')) THEN
      IMUD = 1
      CALL INREAL('LAYER',PMUD(1),'UNC',0.)
      CALL INREAL('RHOM' ,PMUD(2),'UNC',0.)
      CALL INREAL('VISCM',PMUD(3),'UNC',0.)
      CALL INREAL('RHOW' ,PMUD(4),'UNC',0.)
      CALL INREAL('VISCW',PMUD(5),'UNC',0.)
      CYCLE command_loop
   ENDIF

! ==========================================
!
!   IC4M2 [aice] [c0] [c1] [c2] [c3] [c4] [c5] [c6]
!
! ==========================================
!
! Note that PSICE(1) is unused; this is okay
! Note that IC4M2 is deprecated in manual but left intact

   IF (KEYWIS ('IC4M2')) THEN
      IICE = 3
      CALL INREAL('AICE',PICE(1) ,'REQ',0.)
      CALL INREAL('C0'  ,PSICE(2),'REQ',0.)
      CALL INREAL('C1'  ,PSICE(3),'REQ',0.)
      CALL INREAL('C2'  ,PSICE(4),'REQ',0.)
      CALL INREAL('C3'  ,PSICE(5),'REQ',0.)
      CALL INREAL('C4'  ,PSICE(6),'REQ',0.)
      CALL INREAL('C5'  ,PSICE(7),'REQ',0.)
      CALL INREAL('C6'  ,PSICE(8),'REQ',0.)
      CYCLE command_loop
   ENDIF

! ==========================================
!
!   ICE [aice] [hice]
!
! ==========================================

   IF (KEYWIS ('ICE')) THEN
      CALL INREAL('AICE',PICE(1),'REQ',0.)
      CALL INREAL('HICE',PICE(2),'REQ',0.)
      CYCLE command_loop
   ENDIF

! ==========================================
!
!          | -> R19   [c0] [c1] [c2] [c3] [c4] [c5] [c6]
!   SICE < |    D15   [Chf]
!          |    M18   [Chf]
!          |    R21B  [Chf npf]
!
! ==========================================

   IF (KEYWIS ('SICE')) THEN
      IICE = 3 ! default: IICE=3 indicates R19
      CALL INKEYW ('STA','R19')
      IF (KEYWIS('R19')) THEN
         CALL INREAL('C0'  ,PSICE(2),'STA',0.)
         CALL INREAL('C1'  ,PSICE(3),'STA',0.)
         CALL INREAL('C2'  ,PSICE(4),'STA',1.06e-3)
         CALL INREAL('C3'  ,PSICE(5),'STA',0.)
         CALL INREAL('C4'  ,PSICE(6),'STA',2.30e-2)
         CALL INREAL('C5'  ,PSICE(7),'STA',0.)
         CALL INREAL('C6'  ,PSICE(8),'STA',0.)
      ELSE IF (KEYWIS('D15')) THEN
         IICE = 4 ! IICE=4 indicates D15
         CALL INREAL('CHF',PSICE(1),'STA',0.1)
      ELSE IF (KEYWIS('M18')) THEN
         IICE = 5 ! IICE=5 indicates M18
         CALL INREAL('CHF',PSICE(1),'STA',0.059)
      ELSE IF (KEYWIS('R21B')) THEN
         IICE = 6 ! IICE=6 indicates R21B
         CALL INREAL('CHF',PSICE(1),'STA',2.9)
         CALL INREAL('NPF',PSICE(2),'STA',4.5)
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

! ==========================================
!
!   VEGEtation [iveg] < [height]  [diamtr]  [nstems]  [drag] >
!
! ==========================================

   IF (KEYWIS ('VEGE')) THEN
      CALL ININTG ('IVEG', IVEG, 'STA', 1)
      ILMAX = 0
      FRSTV%H = 0.
      FRSTV%D = 0.
      FRSTV%N = 0
      FRSTV%C = 0.
      NULLIFY(FRSTV%NEXTV)
      CURRV => FRSTV
      DO
         CALL INREAL ('HEIGHT',HH,'REP',-1.)
         IF ( HH.EQ.-1. ) EXIT
         IF ( HH.LT.0. ) THEN
            CALL MSGERR (2,'height is negative')
            HH = 0.
         END IF
         CALL INREAL ('DIAMTR',DD,'REQ', 0.)
         IF ( DD.LT.0. ) THEN
            CALL MSGERR (2,'stem diameter is negative')
            DD = 0.
         END IF
         CALL ININTG ('NSTEMS',NN,'REQ', 1 )
         IF ( NN.LE.0 ) THEN
            CALL MSGERR (2,'number of stems is negative or zero')
            NN = 1
         END IF
         CALL INREAL ('DRAG'  ,CC,'REQ', 0.)
         IF ( CC.LT.0. ) THEN
            CALL MSGERR (2,'drag coefficient is negative')
            CC = 0.
         END IF
         ILMAX = ILMAX+1
         ALLOCATE(TMPV)
         TMPV%H = HH
         TMPV%D = DD
         TMPV%N = NN
         TMPV%C = CC
         NULLIFY(TMPV%NEXTV)
         CURRV%NEXTV => TMPV
         CURRV => TMPV
      END DO
      IF (ILMAX.EQ.0) CALL MSGERR(2,'No vegetation parameters found')
      IF (ILMAX.GT.1 .AND. IVEG.NE.1) THEN
         CALL MSGERR (2,&
         &'vertical distribution of vegetation is not allowed')
      END IF
      CALL ENSURE_FIELD_SIZE (LAYH, ILMAX)
      CALL ENSURE_FIELD_SIZE (VEGDIL, ILMAX)
      CALL ENSURE_FIELD_SIZE (VEGNSL, ILMAX)
      CALL ENSURE_FIELD_SIZE (VEGDRL, ILMAX)
      CURRV => FRSTV%NEXTV
      DO JJ = 1, ILMAX
         LAYH  (JJ) = CURRV%H
         VEGDIL(JJ) = CURRV%D
         VEGNSL(JJ) = REAL(CURRV%N)
         VEGDRL(JJ) = CURRV%C
         CURRV => CURRV%NEXTV
      END DO
      DEALLOCATE(TMPV)
      CYCLE command_loop
   END IF

! =================================================================
!
!      TURBulence  [ctb]  (CURrent [tbcur])
!
! =================================================================

   IF (KEYWIS ('TURB')) THEN
      ITURBV = 1
      CALL INREAL ('CTB',  PTURBV(1), 'STA', 0.01)
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('CUR')) THEN
         CALL INREAL ('TBCUR', PTURBV(2), 'STA', 0.004)
         IF (VARTUR) THEN
            CALL MSGERR (1, 'turbulence is read; option CUR ignored')
            PTURBV(2) = -1.
         ELSE
            VARTUR = .TRUE.
!           reserve space for storage of turbulence parameter
            MCMVAR = MCMVAR + 2
            JTURB2 = MCMVAR - 1
            JTURB3 = MCMVAR
            ALOCMP = .TRUE.
         ENDIF
      ELSE
         PTURBV(2) = -1.
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     WIND      parameters uniform wind field
!
! ==========================================================
!
!   WIND  [vel]  [dir]  [astd]
!
! ==========================================================

   IF (KEYWIS ('WIND')) THEN
      LWINDR = 1
      IWIND  = LWINDM
      IF (.NOT.VARWI) THEN
         VARWI = .FALSE.
         CALL INREAL('VEL',U10,'REQ',0.)
         CALL INREAL('DIR',WDIP,'REQ',0.)
         CALL INREAL('ASTD',CASTD,'STA',0.)
      ELSE
         U10   = 0.
         WDIP  = 0.
         CASTD = 0.
      ENDIF

!       *** Convert (if necessary) WDIP from nautical degrees ***
!       *** to cartesian degrees                              ***

      WDIP = DEGCNV (WDIP)

      IF (IWIND.EQ.0) IWIND = 4
      ALTMP = WDIP / 360.
      WDIP = PI2 * (ALTMP - NINT(ALTMP))

      CYCLE command_loop
   ENDIF
!     ------------------------------------------------------------------
!
!     GEN1        deep water with 1st generation
!
! ==========================================
!
! GEN1     [cf10]  [cf20]  [cf30]  [cf40]  [edmlpm]  [cdrag]  [umin]  [cfpm]
!
! ==========================================

   IF (KEYWIS('GEN1')) THEN
!       *** initialize first generation model ***
      IGEN = 1
      LWINDM = 1
      IF (LWINDR .GT. 0) IWIND = LWINDM
      IQUAD = 0
      IWCAP = 0
!       *** set value for the windparameters ***
!       *** first generation wind model ***
      CALL INREAL ('CF10', PWIND(1), 'UNC', 0.)
      CALL INREAL ('CF20', PWIND(2), 'UNC', 0.)
      CALL INREAL ('CF30', PWIND(3),'UNC',0.)
      CALL INREAL ('CF40', PWIND(4),'UNC',0.)
      CALL INREAL ('EDMLPM', PWIND(10), 'UNC', 0.)
      CALL INREAL ('CDRAG',  PWIND(11), 'UNC', 0.)
      CALL INREAL ('UMIN', PWIND(12), 'UNC', 0.)
      CALL INREAL ('CFPM',  PWIND(13), 'UNC', 0.)
!       if [pwtail] is not changed, make it 5
      IF (EQREAL(PWTAIL(1),4.)) THEN
         PWTAIL(1) = 5.
         PWTAIL(3) = PWTAIL(1) + 1.
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     GEN2        deep water with 2nd generation
!
! ==========================================
!
! GEN2 [cf10] [cf20] [cf30] [cf40] [cf50] [cf60] [edmlpm] [cdrag] [umin] [cfpm]
!
! ==========================================

   IF (KEYWIS('GEN2')) THEN
!       *** initialize second generation model ***
      IGEN = 2
      LWINDM = 2
      IF (LWINDR .GT. 0) IWIND = LWINDM
      IQUAD = 0
      IWCAP = 0
!       *** set value for the windparameters ***
      CALL INREAL ('CF10', PWIND(1), 'UNC', 0.)
      CALL INREAL ('CF20', PWIND(2), 'UNC', 0.)
      CALL INREAL ('CF30', PWIND(3),'UNC',0.)
      CALL INREAL ('CF40', PWIND(4),'UNC',0.)
      CALL INREAL ('CF50', PWIND(5),'UNC',0.)
      CALL INREAL ('CF60', PWIND(6), 'UNC', 0.)
      CALL INREAL ('EDMLPM', PWIND(10), 'UNC', 0.)
      CALL INREAL ('CDRAG',  PWIND(11), 'UNC', 0.)
      CALL INREAL ('UMIN', PWIND(12), 'UNC', 0.)
      CALL INREAL ('CFPM',  PWIND(13), 'UNC', 0.)
!       if [pwtail] is not changed, make it 5
      IF (EQREAL(PWTAIL(1),4.)) THEN
         PWTAIL(1) = 5.
         PWTAIL(3) = PWTAIL(1) + 1.
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     GEN3        deep water with 3d generation
!
! ======================================================================
!
!       |    JANSsen [cds1] [delta]      |                   |
!       |                                |                   |
!       |    KOMen   [cds2] [stpm]       |        | -> WU    |
! GEN3 <                                  > DRAG <     FIT    >
!       |    YAN     (NOT documented)    |        |    SWELL |
!       |                                |                   |
!       | -> WESTH [cds2] [br]           |                   |
!       |                                                              |
!       |    (*** old notation ; NOT documented ***)                   |
!       |    BABanin [a1sds] [a2sds] [p1sds] [p2sds]    &              |
!       |                                                              |
!       |                          | UP   |                            |
!       |        [cdsv] [feswell] <        > VECTAU TRUE10             |
!       |                          | DOWN |                            |
!       |                                                              |
!       |    ST6     [a1sds] [a2sds] [p1sds] [p2sds]    &              |
!       |                                                              |
!       |       | -> UP |    |-> HWANG |                               |
!       |      <         >  <    FAN    > VECTAU|SCATAU &              |
!       |       | DOWN  |    |   ECMWF |                               |
!       |                                                              |
!       |       |    TRUE10                 |                          |
!       |      <                             > DEB [cdfac]             |
!       |       | -> U10Proxy [windscaling] |                          |
!
!      (AGROW [a])
!
! ======================================================================

   IF (KEYWIS('GEN3')) THEN
!       *** initialize third generation model ***
      IGEN = 3
      CALL INKEYW ('STA', 'WESTH')
      IF (KEYWIS('JANS')) THEN
         LWINDM = 4
         IF (LWINDR .GT. 0) IWIND = LWINDM
!         *** whitecapping according to Janssen (1989, 1991) ***
         IWCAP = 2
         CALL INREAL ('CDS1',  PWCAP(3), 'UNC', 0.)
         CALL INREAL ('DELTA', PWCAP(4), 'UNC', 0.)

!         Recalculate coefficientes that are actually used in the
!         whitecapping routines
         PWCAP(1)  = PWCAP(3) * (PWCAP(2) ** PWCAP(9))
         PWCAP(10) = PWCAP(4)
         JUSTAR = MCMVAR+1
         JZEL   = MCMVAR+2
         JCDRAG = MCMVAR+3
         JTAUW  = MCMVAR+4
         MCMVAR = MCMVAR+4
         ALOCMP = .TRUE.
!         if [pwtail] is not changed, make it 5
         IF (EQREAL(PWTAIL(1),4.)) THEN
            PWTAIL(1) = 5.
            PWTAIL(3) = PWTAIL(1) + 1.
         ENDIF
      ELSE IF (KEYWIS('YAN')) THEN
!         option not documented in user manual
         LWINDM = 5
         IF (LWINDR .GT. 0) IWIND = LWINDM
      ELSE IF (KEYWIS('WESTH')) THEN
!         *** wind according to (adapted) Yan (1987) ***
         LWINDM = 5
         IF (LWINDR .GT. 0) IWIND = LWINDM
!         *** whitecapping according to Alves and Banner (2003) ***
         IWCAP = 7
         CALL INREAL ('CDS2',  PWCAP(1),  'STA', 5.0E-5)
         CALL INREAL ('BR',    PWCAP(12), 'STA', 1.75E-3)
      ELSE IF (KEYWIS('BAB')) THEN
!         "BABanin"=legacy mode of input ; "ST6"=new mode of input
!         *** wind according to Rogers et al. (JTECH 2012) based on work of
!             Donelan, Babanin, Tsagareli and others
         LWINDM = 8
         IF (LWINDR .GT. 0) IWIND = LWINDM
!         note: command "GEN3 BABANIN" supports Hwang wind drag only
         IDRAG = 4
!         *** whitecapping according to Rogers et al. (JTECH 2012) based on work of
!             Babanin, Young, Tsagareli, Ardhuin and others
         IF (WCAPSET) CALL MSGERR (4, 'whitecapping is already set')
         WCAPSET = .TRUE.
         IWCAP = 8
         CALL INREAL ('A1SDS', A1SDS, 'REQ', 0.)
         CALL INREAL ('A2SDS', A2SDS, 'REQ', 0.)
         CALL INREAL ('P1SDS', P1SDS, 'REQ', 0.)
         CALL INREAL ('P2SDS', P2SDS, 'REQ', 0.)
         ROGERS = .TRUE. ! legacy swell dissipation
         IF(SWELLSET) CALL MSGERR(4,'swell dissipation is already set')
         SWELLSET = .TRUE.
         CALL INREAL ('CDSV'   , CDSV   , 'REQ', 0.)
         CALL INREAL ('FESWELL', FESWELL, 'REQ', 0.)
         CALL INKEYW ('REQ', ' ')
         IF (KEYWIS ('UP')) THEN
            UPWARDS = .TRUE.
         ELSE IF (KEYWIS ('DOWN')) THEN
            UPWARDS = .FALSE.
         ELSE
            CALL WRNKEY
         ENDIF
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('VECTAU')) THEN
            VECTOR_TAU = .TRUE.
         ELSE
            VECTOR_TAU = .FALSE.
         ENDIF
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('TRUE10')) THEN
            TRUE_U10 = .TRUE.
         ENDIF
      ELSE IF (KEYWIS('ST6')) THEN
!         "BABanin"=legacy mode of input ; "ST6"=new mode of input
!         *** wind according to Rogers et al. (JTECH 2012) based on work of
!             Donelan, Babanin, Tsagareli and others
         LWINDM = 8
         IF (LWINDR .GT. 0) IWIND = LWINDM
!         *** whitecapping according to Rogers et al. (JTECH 2012) based on work of
!             Babanin, Young, Tsagareli, Ardhuin and others
         IWCAP = 8
         CALL INREAL ('A1SDS', A1SDS, 'UNC', 0.)
         CALL INREAL ('A2SDS', A2SDS, 'UNC', 0.)
         CALL INREAL ('P1SDS', P1SDS, 'STA', 4.)
         CALL INREAL ('P2SDS', P2SDS, 'STA', 4.)
         CALL INKEYW ('STA', 'UP')
         IF (KEYWIS ('DOWN')) THEN
            UPWARDS = .FALSE.
         ELSE
            CALL IGNORE ('UP')
            UPWARDS = .TRUE.
         ENDIF
         CALL INKEYW ('STA', 'HWANG')
         IF (KEYWIS('HWANG')) THEN
            IDRAG = 4
         ELSE IF (KEYWIS ('FAN')) THEN
            IDRAG = 5
         ELSE IF (KEYWIS ('ECMWF')) THEN
            IDRAG = 6
         ENDIF
         CALL INKEYW ('STA', 'VECTAU')
         IF (KEYWIS('VECTAU')) THEN
            VECTOR_TAU = .TRUE.
         ELSE IF (KEYWIS('SCATAU')) THEN
            VECTOR_TAU = .FALSE.
         ENDIF
         CALL INKEYW ('STA', 'U10P')
         IF (KEYWIS('TRUE10')) THEN
            TRUE_U10 = .TRUE.
         ELSEIF (KEYWIS ('U10P')) THEN
!            **** see notes for WINDSCALING in SdsBabanin.f90 ****
!            **** applies only to Babanin DBYB wind input     ****
!            **** applies only when TRUE10 is not used        ****
            CALL INREAL ( 'WINDSCALING', WNDSCL, 'UNC', 0. )
         ENDIF
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS ('DEB')) THEN
            CALL INREAL ( 'CDFAC', CDFAC, 'REQ', 1. )
         ENDIF
      ELSE
         IF ( IWCAP.EQ.8 .OR. IWIND.EQ.8 ) THEN
            CALL MSGERR(1,' * We prefer that you use GEN3 BABANIN. * ')
            CALL MSGERR(1,' * Example:                             * ')
            CALL MSGERR(1,'GEN3 BABANIN 5.7E-7 8.0E-6 4.0 4.0 1.2 0.006&
            &0 UP AGROW')
         ELSE IF (KEYWIS('KOM')) THEN
            LWINDM = 3
            IF (LWINDR .GT. 0) IWIND = LWINDM
!         *** whitecapping according to Komen et al. (1984) ***
            IWCAP = 1
            CALL INREAL ('CDS2', PWCAP(1), 'UNC', 0.)
            CALL INREAL ('STPM', PWCAP(2), 'UNC', 0.)
         ENDIF
      ENDIF
      IF (IWIND.NE.8) THEN
         CALL INKEYW ('STA','  ')
         IF ( KEYWIS('DRAG') ) THEN
            CALL INKEYW ('STA','WU')
            IF (KEYWIS ('WU')) THEN
               IDRAG = 1
            ELSE IF (KEYWIS ('FIT')) THEN
               IDRAG = 2
            ELSE IF (KEYWIS ('SWELL')) THEN
               IDRAG = 3
            ENDIF
         ENDIF
      ENDIF
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('AGROW')) THEN
         CALL INREAL ('A',PWIND(31),'STA',0.0015)
      ELSE
         IF (NSTATM.EQ.1 .AND. ICOND.EQ.0) ICOND = 1
      ENDIF
      IF (IWIND.NE.4) THEN
         JUSTAR = MCMVAR+1
         MCMVAR = MCMVAR+1
         ALOCMP = .TRUE.
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     GEN4        shallow water with 4th generation (quasi-coherent modelling)
!
! ==========================================
!
! GEN4
!
! ==========================================

   IF ( KEYWIS('GEN4') ) THEN
!        *** initialize fourth generation model ***
      IGEN  = 4
      ! deep water physics not included
      IWIND = 0
      IQUAD = 0
      IWCAP = 0
      ! activate the QC scattering
      IQCM  = 1
      PSCAT(1) = 1.
      ! other stuff which should not be included
      IDIFFR = 0
      LSETUP = 0
      ! action limiter deactivated
      PNUMS(20) = 1.E20
      ! do not include spectral tail in postprocessing
      PWTAIL(1)    = 1.E8
      PWTAIL(2:10) = 0.
      ! spherical coordinates not supported
      KSPHER = 0
      ! use suitable stopping criterion
      PNUMS(21) = 1.
      ! stopping criterion based on abs/rel changes in Hs only
      PNUMS(1) = 0.01
      PNUMS(2) = 0.05
      PNUMS(4) = 99.
      CYCLE command_loop
   ENDIF
!  -------------------------------------------------------------------
!           |    ROGers  [cdsv] [feswell]  (NOT DOCUMENTED)
!   SSWELL <  -> ARDhuin [cdsv]
!           |    ZIEger  [b1]
!  ------------------------------------------------------------------
   IF (KEYWIS('SSWELL')) THEN
      IF (SWELLSET) CALL MSGERR(4,'swell dissipation is already set')
      SWELLSET = .TRUE.
      CALL INKEYW ('STA', 'ARD')
      IF (KEYWIS ('ROG')) THEN
         ROGERS = .TRUE.
         CALL INREAL ('CDSV'   , CDSV   , 'STA', 1.2)
         CALL INREAL ('FESWELL', FESWELL, 'REQ', 0. )
      ELSE IF (KEYWIS ('ARD')) THEN
         ARDHUIN = .TRUE.
         CALL INREAL ('CDSV', CDSV, 'STA', 1.2)
      ELSE IF (KEYWIS ('ZIE')) THEN
         ZIEGER = .TRUE.
         CALL INREAL ('B1', B1Z, 'STA', 0.00025)
         ! Ref: B1Z=0.0014 was from Young et al. (2013)
         !      B1Z=0.00025 is from Zieger et al. (2015)
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

!   ------------------------------------------------------------------
!   NEGatinp  [rdcoef]
!   ------------------------------------------------------------------
   IF (KEYWIS ('NEG')) THEN
      CALL INREAL('RDCOEF',RDCOEF,'REQ',0.)
      CYCLE command_loop
   ENDIF
!     ----------------------------------------------------------------
!
!                               | CNL4 < [cnl4] >               |
!     MDIA LAMbda < [lambda] > <                                 >
!                               | CNL4_12 < [cnl4_1] [cnl4_2] > |
!
!     ----------------------------------------------------------------

   IF (KEYWIS('MDIA')) THEN
      IF (ALLOCATED(SNL4%lambda)) THEN
         DEALLOCATE (SNL4%lambda,SNL4%coefficient_1,SNL4%coefficient_2)
      ENDIF
      ALLOCATE (RLAMBDA(1000))
      CALL INKEYW ('STA', '   ')
      IF (KEYWIS('LAM')) THEN
         MORE = .TRUE.
         ILAMBDA = 0
         DO WHILE (MORE)
            CALL INREAL('LAMBDA',RLAMBDA(ILAMBDA+1),'STA',-1.)
            IF (RLAMBDA(ILAMBDA+1).LT.0.) THEN
               MORE = .FALSE.
            ELSE
               ILAMBDA = ILAMBDA + 1
            ENDIF
         ENDDO
         SNL4%quadruplet_count = ILAMBDA
!        Reject an empty lambda list. With MDIA = 0 the loop over the
!        quadruplets in SWPRE4W does not execute, so MSC4MI/MSC4MA and
!        MDC4MI/MDC4MA would be assigned from uninitialised locals and the
!        derived spectral range MSCMAX/MDCMAX would be meaningless. This is
!        the only path that can set MDIA below one.
         IF (SNL4%quadruplet_count.LT.1) CALL MSGERR (4,&
         &'MDIA LAMBDA requires at least one non-negative [lambda] value')
         ALLOCATE (SNL4%lambda(SNL4%quadruplet_count))
         SNL4%lambda(1:SNL4%quadruplet_count) =&
         &RLAMBDA(1:SNL4%quadruplet_count)
         DEALLOCATE (RLAMBDA)
      ELSE
         CALL WRNKEY
      END IF
      CALL INKEYW ('STA', '   ')
      ALLOCATE (SNL4%coefficient_1(SNL4%quadruplet_count),&
      &SNL4%coefficient_2(SNL4%quadruplet_count))
      IF (KEYWIS('CNL4_12')) THEN
         DO ICNL4=1,SNL4%quadruplet_count
            CALL INREAL('CNL4_1',SNL4%coefficient_1(ICNL4),'REQ',0.)
            CALL INREAL('CNL4_2',SNL4%coefficient_2(ICNL4),'REQ',0.)
         END DO
      ELSEIF (KEYWIS('CNL4')) THEN
         DO ICNL4=1,SNL4%quadruplet_count
            CALL INREAL('CNL4',SNL4%coefficient_1(ICNL4),'REQ',0.)
         END DO
         SNL4%coefficient_2 = SNL4%coefficient_1
      ELSE
         CALL WRNKEY
      END IF
      SNL4%coefficient_1 = SNL4%coefficient_1 * ((2.*PI)**9)
      SNL4%coefficient_2 = SNL4%coefficient_2 * ((2.*PI)**9)
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     GROWTH       parameters wind input source term
!     Note: command does not appear in user manual for SWAN
!
! ==========================================
!
!         |   G1 [cf10] [cf20] [cf30] [cf40]               & |
!         |                                                  |
!         |      [edmlpm] [cdrag] [umin] [cfpm]              |
!         |                                                  |
!         |   G2 [cf10] [cf20] [cf30] [cf40] [cf50] [cf60] & |
!         |                                                  |
! GROWTH <       [edmlpm] [cdrag] [umin] [cfpm]              | (NOT documented)
!         |                                                  |
!         |       |    JANSsen [pwtail] |                    |
!         | ->G3 <                       > (AGROW [a])       |
!         |       | -> KOMen            |                    |
!         |       |                     |                    |
!         |       |    YAN              |                    |


   IF (KEYWIS('GROWTH') .OR. KEYWIS ('PARW')) THEN
!         *** initialize first generation model ***
      CALL INKEYW ('STA', 'G3')
      IF (KEYWIS('G1') .OR. KEYWIS ('SNY1')) THEN
         IGEN = 1
         LWINDM = 1
         IQUAD = 0
!           *** set value for the windparameters ***
!           *** first generation wind model ***
         CALL INREAL ('CF10', PWIND(1), 'UNC', 0.)
         CALL INREAL ('CF20', PWIND(2), 'UNC', 0.)
         CALL INREAL ('CF30', PWIND(3),'UNC',0.)
         CALL INREAL ('CF40', PWIND(4),'UNC',0.)
         CALL INREAL ('CF50', PWIND(5),'UNC',0.)
         CALL INREAL ('CF60', PWIND(6), 'UNC', 0.)
         CALL INREAL ('CF70', PWIND(7), 'UNC', 0.)
         CALL INREAL ('CF80', PWIND(8), 'UNC', 0.)
         CALL INREAL ('RHOAW', PWIND(9), 'UNC', 0.)
         CALL INREAL ('EDMLPM', PWIND(10), 'UNC', 0.)
         CALL INREAL ('CDRAG',  PWIND(11), 'UNC', 0.)
         CALL INREAL ('UMIN', PWIND(12), 'UNC', 0.)
         CALL INREAL ('CFPM',  PWIND(13), 'UNC', 0.)
      ELSE IF (KEYWIS('G2') .OR. KEYWIS('SNY2')) THEN
         IGEN = 2
         LWINDM = 2
         IQUAD = 0
!           *** second generation wind model ***
         CALL INREAL ('CF10', PWIND(1), 'UNC', 0.)
         CALL INREAL ('CF20', PWIND(2), 'UNC', 0.)
         CALL INREAL ('CF30', PWIND(3),'UNC',0.)
         CALL INREAL ('CF40', PWIND(4),'UNC',0.)
         CALL INREAL ('CF50', PWIND(5),'UNC',0.)
         CALL INREAL ('CF60', PWIND(6), 'UNC', 0.)
         CALL INREAL ('CF70', PWIND(7), 'UNC', 0.)
         CALL INREAL ('CF80', PWIND(8), 'UNC', 0.)
         CALL INREAL ('RHOAW', PWIND(9), 'UNC', 0.)
         CALL INREAL ('EDMLPM', PWIND(10), 'UNC', 0.)
         CALL INREAL ('CDRAG',  PWIND(11), 'UNC', 0.)
         CALL INREAL ('UMIN', PWIND(12), 'UNC', 0.)
         CALL INREAL ('CFPM',  PWIND(13), 'UNC', 0.)
      ELSE IF (KEYWIS('G3')) THEN
         IGEN = 3
         CALL INKEYW ('STA', 'KOM')
         IF (KEYWIS('JANS')) THEN
            LWINDM = 4
            CALL INREAL ('PWTAIL', PWTAIL(1), 'STA', 5.)
            IF (PWTAIL(1).LE.1.) CALL MSGERR (3, 'Incorrect PWTAIL')
            PWTAIL(3) = PWTAIL(1) + 1.
         ELSE IF (KEYWIS('YAN')) THEN
            LWINDM = 5
         ELSE
            CALL IGNORE ('KOM')
            LWINDM = 3
         ENDIF
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('AGROW')) THEN
            CALL INREAL ('A',PWIND(31),'STA',0.0015)
         ELSE
            IF (NSTATM.EQ.1 .AND. ICOND.EQ.0) ICOND = 1
         ENDIF
      ELSE
         CALL WRNKEY
      END IF
      IF (LWINDR.GT.0) IWIND = LWINDM
      CYCLE command_loop
   ENDIF
!     ------------------------------------------------------------------
!     ***  parameters nonlinear 4 wave interactions ***
   IF (KEYWIS ('QUAD')) THEN

! ===================================================================
!
!  QUADrupl [iquad] [lambda] [cnl4] [csh1] [csh2] [csh3]
!
! ===================================================================

      CALL ININTG ('IQUAD', IQUAD, 'UNC',  0)
      CALL INREAL ('LAMBDA', PQUAD(1), 'UNC', 0.0)
      CALL INREAL ('CNL4', PQUAD(2), 'UNC', 0.0)
      CALL INREAL ('CSH1', PQUAD(3), 'UNC', 0.0)
      CALL INREAL ('CSH2', PQUAD(4), 'UNC', 0.0)
      CALL INREAL ('CSH3', PQUAD(5), 'UNC', 0.0)
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     ***  parameters nonlinear three-wave interactions
!
! ============================================================
!
!                                  / -> COLL
!           | -> DCTA [trfac] [p]  \    NONC
!           |
!           |    LTA [trfac] [cutfr] [dint]
!   TRIad  <                                               &
!           |    FTIM [trfac] [dint]
!           |
!           |    SPB [trfac] [a] [b] [dint]
!
!
!                  | -> ELDeberky [urcrit]
!                  |
!         BIPHase <  SAPRykina [a]   (NOT documented)      &
!                  |
!                  | WIT [lpar]
!
!
!                   |    FG
!         TRAnsfer <     MS
!                   |    BRedmose
!                   | -> QUadwave
!
!
!   TRIad [itriad] [trfac] [cutfr] [dint] [a] [b] [p] [urcrit] [urslim]
!
! ============================================================


   IF ( KEYWIS ('TRI') .OR. KEYWIS ('NL3') ) THEN

      ITRIAD = 5
      CALL INKEYW ('REQ',' ')
      IF ( KEYWIS('LTA') ) THEN

         ! ============================================================
         !
         ! TRIad LTA [trfac] [cutfr] [dint]
         !
         ! ============================================================

         ITRIAD = 1
         CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 1.0)
         CALL INREAL ('CUTFR', PTRIAD(2), 'STA', -1.)
         CALL INREAL ('DINT' , PTRIAD(8), 'STA',60.0)

      ELSE IF ( KEYWIS('DCTA') ) THEN

         ! ============================================================
         !
         ! TRIad DCTA [trfac] [p]
         !
         ! ============================================================

         ITRIAD = 5
         CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 4.4 )
         CALL INREAL ('P'    , PTRIAD(2), 'STA', 4./3.)

      ELSE IF ( KEYWIS('SPB') ) THEN

         ! ============================================================
         !
         ! TRIad SPB [trfac] [a] [b] [dint]
         !
         ! ============================================================

         ITRIAD = 2
         CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 0.90)
         CALL INREAL ('A'    , PTRIAD(6), 'STA', 0.95)
         CALL INREAL ('B'    , PTRIAD(7), 'STA', 0.0 )
         CALL INREAL ('DINT' , PTRIAD(8), 'STA',-1.  )

      ELSE IF ( KEYWIS('FTIM') ) THEN

         ! ============================================================
         !
         ! TRIad FTIM [trfac] [dint]
         !
         ! ============================================================

         ITRIAD = 3
         CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 1.)
         CALL INREAL ('DINT' , PTRIAD(8), 'STA',-1.)

      ELSE

         ! ============================================================
         !
         ! TRIad [itriad] [trfac] [cutfr] [dint] [a] [b] [p] [urcrit] [
         !
         ! ============================================================

         CALL ININTG ('ITRIAD', ITRIAD, 'STA', 5)
         IF (ITRIAD.EQ.1) THEN
            CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 1.0)
            CALL INREAL ('CUTFR', PTRIAD(2), 'STA', -1.)
            CALL INREAL ('DINT' , PTRIAD(8), 'STA',60.0)
         ELSEIF (ITRIAD.EQ.2) THEN
            CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 0.90)
            CALL INREAL ('A'    , PTRIAD(6), 'STA', 0.95)
            CALL INREAL ('B'    , PTRIAD(7), 'STA', 0.0 )
            CALL INREAL ('DINT' , PTRIAD(8), 'STA',-1.  )
         ELSEIF (ITRIAD.EQ.3) THEN
            CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 1.)
            CALL INREAL ('DINT' , PTRIAD(8), 'STA',-1.)
         ELSEIF (ITRIAD.EQ.5) THEN
            CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 4.4 )
            CALL INREAL ('P'    , PTRIAD(2), 'STA', 4./3.)
         ELSEIF (ITRIAD.EQ.11) THEN
!            original LTA (before version 41.01)
            CALL INREAL ('TRFAC', PTRIAD(1), 'STA', 0.05)
            CALL INREAL ('CUTFR', PTRIAD(2), 'STA', 2.5)
            PTRIAD(8) = 0.
         ENDIF
         CALL INREAL ('URCRIT', PTRIAD(4) , 'STA', 0.63)
         CALL INREAL ('URSLIM', PTRIAD(5) , 'STA', 0.1 )
         CALL INREAL ('IC'    , PTRIAD(10), 'STA', -1. )

      ENDIF

      CALL INKEYW ('STA', 'COLL')
      IF (KEYWIS('NONC')) THEN
         TCOLL = .FALSE.
      ELSE
         CALL IGNORE ('COLL')
         TCOLL = .TRUE.
      ENDIF
      IF (ITRIAD.NE.5) TCOLL = .TRUE.

      IBIPH = 1
      CALL INKEYW ('REQ',' ')
      IF (KEYWIS('BIPH')) THEN
         CALL INKEYW ('STA', 'ELD')
         IF (KEYWIS('SAPR')) THEN
            IBIPH = 2
            CALL INREAL ('A', PTRIAD(9), 'STA', 1.0)
            IF (.NOT.PTRIAD(9).NE.0.)&
            &CALL MSGERR (2, '[A] should not be set to zero')
         ELSEIF (KEYWIS('WIT') .OR. KEYWIS('DEWIT')) THEN
            IBIPH = 3
            CALL INREAL ('LPAR', PTRIAD(9), 'STA', 0.)
            IF ( OPTG.EQ.5 .AND. PTRIAD(9).NE.0. ) THEN
               CALL MSGERR(2,&
               &' filtered De Wit biphase not supported in unstructured grid')
               PTRIAD(9) = 0.
            ENDIF
         ELSE
            CALL IGNORE ('ELD')
            IBIPH = 1
            CALL INREAL ('URCRIT', PTRIAD(4), 'STA', 0.63)
         ENDIF
      ENDIF

      CALL INKEYW ('REQ',' ')
      IF (KEYWIS('TRA') .OR. KEYWIS('IC')) THEN
         CALL INKEYW ('STA', 'QU')
         IF (KEYWIS('FG')) THEN
            PTRIAD(10) = 1.
         ELSEIF (KEYWIS('MS')) THEN
            PTRIAD(10) = 2.
         ELSEIF (KEYWIS('BR')) THEN
            PTRIAD(10) = 3.
         ELSE
            CALL IGNORE ('QU')
            PTRIAD(10) = 4.
         ENDIF
      ENDIF

      CYCLE command_loop
   ENDIF

! ============================================================
!
!   BRAGg [ibrag] [nreg] [cutoff]                          &
!
!         | -> FT
!        <
!         |    FILE 'fname' [idla] [mkx] [mky] [dkx] [dky]
!
! ============================================================

   IF (KEYWIS ('BRAG')) THEN
      CALL ININTG ('IBRAG' , IBRAG   , 'STA', 1 )
      CALL ININTG ('NREG'  , NREGB   , 'UNC', 0 )
      IF (NREGB.LT.1) NREGB = 1
      PBRAG(1) = REAL(NREGB)
      CALL INREAL ('CUTOFF', PBRAG(2), 'STA', 5.)
      CALL INKEYW ('STA', 'FT')
      IF ( KEYWIS('FILE') ) THEN
         FILB = .TRUE.
         CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
         NDSD   = 0
         IOSTAT = 0
         CALL FOR (NDSD, FILENM, 'OF', IOSTAT)
         IF (STPNOW()) RETURN
         CALL INKEYW ('STA', ' ')
         CALL ININTG ('IDLA', IDLA, 'UNC', 1   )
         CALL ININTG ('MKX' , mkbx, 'REQ', 0   )
         CALL ININTG ('MKY' , mkby, 'STA', mkbx)
         CALL INREAL ('DKX' , dkbx, 'REQ', 0.  )
         CALL INREAL ('DKY' , dkby, 'STA', dkbx)
      ELSE
         CALL IGNORE ('FT')
         FILB = .FALSE.
         mkbx = NREGB
         mkby = NREGB
      ENDIF
      IF ( FILB ) THEN

         mkbx = mkbx + 1
         mkby = mkby + 1

         ! allocate bottom spectrum

         IF ( .NOT.ALLOCATED(botspc) ) ALLOCATE( botspc(mkbx,mkby) )

         ! read bottom spectrum

         SELECT CASE(IDLA)
          CASE(1)
            DO JK = mkby, 1, -1
               READ(NDSD,*,IOSTAT=IOSTAT) (botspc(IK,JK), IK=1,mkbx)
               IF (bottom_read_failed(IOSTAT)) RETURN
            ENDDO
          CASE(2)
            READ(NDSD,*,IOSTAT=IOSTAT)&
            &((botspc(IK,JK), IK=1,mkbx), JK=mkby,1,-1)
            IF (bottom_read_failed(IOSTAT)) RETURN
          CASE(3)
            DO JK = 1, mkby
               READ(NDSD,*,IOSTAT=IOSTAT) (botspc(IK,JK), IK=1,mkbx)
               IF (bottom_read_failed(IOSTAT)) RETURN
            ENDDO
          CASE(4)
            READ(NDSD,*,IOSTAT=IOSTAT)&
            &((botspc(IK,JK), IK=1,mkbx), JK=1,mkby)
            IF (bottom_read_failed(IOSTAT)) RETURN
          CASE(5)
            DO IK = 1, mkbx
               READ(NDSD,*,IOSTAT=IOSTAT) (botspc(IK,JK), JK=1,mkby)
               IF (bottom_read_failed(IOSTAT)) RETURN
            ENDDO
          CASE(6)
            READ(NDSD,*,IOSTAT=IOSTAT)&
            &((botspc(IK,JK), JK=1,mkby), IK=1,mkbx)
            IF (bottom_read_failed(IOSTAT)) RETURN
         END SELECT

         ! close file

         CLOSE(NDSD)

         VARB = 0.
         DO JK = 1, mkby
            DO IK = 1, mkbx
               VARB = VARB + botspc(IK,JK) * dkbx * dkby
            ENDDO
         ENDDO
         WRITE (PRINTF,'(A,(F12.5))') ' bottom variance = ',VARB

      ENDIF

      CYCLE command_loop

   ENDIF

! ============================================================
!
!   SCAT [iqcm]
!
!              | -> WIDth [rfac]                                   |
!      ( GRId <                                                     > )
!              | Kxy [kxlow] [kylow] [kxlen] [kylen] [mkxc] [mkyc] |
!
!      ( TRUnc [alpha] [qmax] )
!
! ============================================================

   IF ( KEYWIS('SCAT') .OR. KEYWIS('QC') ) THEN
      CALL ININTG ('IQCM', IQCM, 'STA', 1 )
      CALL INKEYW ('STA','  ')
      IF (KEYWIS('GRI')) THEN
         CALL INKEYW ('STA', 'WID')
         IF ( KEYWIS('K') .OR. KEYWIS('WNUM') ) THEN
            CALL INREAL ('KXLOW', PSCAT(3), 'REQ', 0.)
            CALL INREAL ('KYLOW', PSCAT(4), 'REQ', 0.)
            CALL INREAL ('KXLEN', PSCAT(5), 'REQ', 0.)
            CALL INREAL ('KYLEN', PSCAT(6), 'REQ', 0.)
            CALL ININTG ('MKXC' , mkxc    , 'STA', -1)
            CALL ININTG ('MKYC' , mkyc    , 'STA', -1)
         ELSE
            CALL IGNORE ('WID')
            CALL INREAL ('RFAC', PSCAT(7), 'STA', 1.)
            IF ( PSCAT(7).LT.1. )&
            &CALL MSGERR (3, '[rfac] must be larger or equal 1')
         ENDIF
      ENDIF
      CALL INKEYW ('STA', 'TRU')
      IF ( KEYWIS('TRU') ) THEN
         CALL INREAL ('ALPHA', PSCAT(1), 'STA', 1.)
         CHGALF = default_command_reader%CHGVAL
         CALL INREAL ('QMAX ', PSCAT(2), 'UNC', 0.)
         IF (default_command_reader%CHGVAL .AND. .NOT.CHGALF) PSCAT(1) = 99999.
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF

!     ------------------------------------------------------------------
!
!     OFF      switching standard options off
!
! ============================================================
!
!        |  BREaking
!        |
!        |  WCAPping
!        |
!        |  REFrac
! OFF   <
!        |  FSHift
!        |
!        |  QUADrupl
!        |
!        |  WINDGrowth
!        |
!        |  BNDCHK
!        |
!        |  RESCALE        (NOT documented)
!        |
!        |  SOURCES        (NOT documented)
!
! ============================================================

   IF (KEYWIS ('OFF')) THEN
      CALL INKEYW ('REQ', ' ')
      IF (KEYWIS ('REF')) THEN
         IREFR = 0
      ELSE IF (KEYWIS ('FSH')) THEN
         ITFRE = 0
      ELSE IF (KEYWIS ('BRE')) THEN
         ISURF = 0
      ELSE IF (KEYWIS ('WCAP')) THEN
         IWCAP = 0
      ELSE IF (KEYWIS ('QUAD')) THEN
         IQUAD = 0
      ELSE IF (KEYWIS ('WINDG')) THEN
         IWIND = 0
      ELSE IF (KEYWIS ('BNDCHK')) THEN
!         switch off checking of Hs on boundary
         BNDCHK = .FALSE.
      ELSE IF (KEYWIS ('RESCALE')) THEN
!         switch off rescaling solution
         BRESCL = .FALSE.
      ELSE IF (KEYWIS ('SOURCES')) THEN
         OFFSRC = .TRUE.
         IWIND  = 0
         IQUAD  = 0
         IWCAP  = 0
         ISURF  = 0
         ITRIAD = 0
         IBOT   = 0
      ELSE
         CALL WRNKEY
      ENDIF
      CYCLE command_loop
   ENDIF
!     ------------------------------------------------------------------
!
!     In case of an empty line in the command file proceed to next line

   IF (default_command_reader%KEYWRD .EQ. '    ') CYCLE command_loop

!     process output requests

   CALL SPROUT(FOUND, DEPTH, WLEVL)
   IF (STPNOW()) RETURN
   IF (.NOT.FOUND) THEN
      LEVERR = MAX(LEVERR,3)
      CALL WRNKEY
   ENDIF
   CYCLE command_loop

!   * end of subroutine SWREAD *
   END DO command_loop
   END ASSOCIATE

CONTAINS

   LOGICAL FUNCTION bottom_read_failed(status)
      INTEGER, INTENT(IN) :: status

      bottom_read_failed = status.NE.0
      IF (.NOT.bottom_read_failed) RETURN
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      IF (IS_IOSTAT_END(status)) THEN
         CALL MSGERR (4, 'unexpected end of file in bottom spectrum file '//FILENM)
      ELSE
         CALL MSGERR (4, 'error reading data from bottom spectrum file '//FILENM)
      ENDIF
   END FUNCTION bottom_read_failed

end subroutine SWREAD
!************************************************************************
!                                                                      *
SUBROUTINE SINPGR (IGRID1, IGRID2, SNAMEG)
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_input_parser, ONLY: IGNORE, ININTG, INKEYW, INREAL, KEYWIS, INCTIM, INITVD
!                                                                      *
!************************************************************************

   USE swan_time, ONLY: default_time_context
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
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
!  0. Authors
!
!     30.60, 30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     32.02: Roeland Ris & Cor van der Schelde
!     30.75, 31.04, 40.13: Nico Booij
!     34.01: Jeroen Adema
!     40.02: IJsbrand Haagsma
!     40.12: IJsbrand Haagsma
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     00.00, Jan. 92
!     30.60, July 97: exception value introduced
!     30.60, July 97: default values for STAGRX, STAGRY, MXINP, MYINP
!     30.60, Aug. 97: format 6021 changed
!     30.60, Aug. 97: keyword EXC not required
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.72, Nov. 97: Header renewed, Common Blocks updated
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     32.02, Feb. 98: Introduced 1D-version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: warning concerning DYINP removed; default MYINP changed
!     30.75, Mar. 98: correction nonstationary wind and current field
!     30.80, Apr. 98: default value for MYINP (curvil. grid) was lost
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Oct. 00: Avoided real/int conflict by introducing replacing
!                     RPOOL for POOL in DPPUTR
!     40.12, Feb. 01: Avoided type conflict for OUTPS
!     40.13, Aug. 01: [xpinp] and [ypinp] are now required in case of
!                     spherical coordinates
!                     Arguments XCGRID and YCGRID removed (not used)
!     40.31, Dec. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Dec. 07: extension to unstructured grids
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Read parameters of an input grid
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     IGRID1:   input  int grid number for which parameters are read
!     IGRID2:   input  int input grid number for which parameters are read
!                          only relevant if >0
!     SNAMEG:   input char name of output frame corresponding to input grid
!
!  9. Subroutines calling
!
!     SWREAD:  Reading and processing of the user commands describing the model
!
!  8. Subroutines used


! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!       subroutine computes the following common data:
!       XCGMIN, XCGMAX, YCGMIN, YCGMAX
!
! 12. Structure
!
!     ---
!
! 13. SOURCE TEXT

   INTEGER, SAVE :: IENT = 0
   INTEGER   IGRID1, IGRID2
   REAL      ALTMP, STAGRX, STAGRY
   CHARACTER(LEN=8) :: SNAMEG
   TYPE(OPSDAT), POINTER :: OPSTMP
   CALL STRACE(IENT,'SINPGR')

!     *** user gives coord. corner point input grid               ***
!     *** Regular or curvilinear grid option  for version 30.21   ***
!
!     *** Type of grid -Regular(1) or Curvilinear(2)- is indicated***
!     *** by array IGTYPE(NUMGRD) in module SWCOMM2               ***
!
!     ------------------------------------------------------------------
!
!     INPUT    definition of input grids
!
! ======================================================================
!
!            | BOTtom   |
!            |          |
!            | WLEVel   |
!            |          |
!            | CURrent  |
!            |          |
!            | VX       |
!            |          |
!            | VY       |
!            |          |
!            | FRiction |
!            |          |
!            | WInd     |
!            |          |
!            | WX       |
!            |          |
!            | WY       |
! INPgrid  (<            >) &
!            | ASTD     |
!            |          |
!            | NPLAnts  |
!            |          |
!            | TURB     |
!            |          |
!            | MUDLayer |
!            |          |
!            | AICE     |
!            |          |
!            | HICE     |
!            |          |
!            | HSS      |
!            |          |
!            | TSS      |
!            |          |
!            | DSS      |
!
!
!    | REGular [xpinp] [ypinp] [alpinp] [mxinp] [myinp] [dxinp] [dyinp]
!    |
!   <  CURVilinear [stagrx] [stagry] [mxinp] [myinp]
!    |
!    | UNSTRUCtured
!
!    (EXCeption  [excval])
!
!                                        | -> SEC  |
!    (NONSTATionary [tbeginp] [deltinp] <     MIN   >  [tendinp])
!                                        |    HR   |
!                                        |    DAY  |
!
! ======================================================================

   CALL INKEYW ('STA', 'REG')
   IF (KEYWIS('CURV')) THEN

      IF (ONED) THEN
         CALL MSGERR (4,&
         &'Impossible option: 1D-simulation with curvilinear grid')
         RETURN
      ENDIF
      IF (OPTG.EQ.5) THEN
         CALL MSGERR (4,&
         &'Curvilinear input grid cannot be used with unstructured grid')
         RETURN
      ENDIF

      IGTYPE(IGRID1) = 2
!       default values changed
      CALL INREAL ('STAGRX',STAGRX,'STA', 0.)
      CALL INREAL ('STAGRY',STAGRY,'STA', 0.)
      STAGX(IGRID1) = STAGRX
      STAGY(IGRID1) = STAGRY
      MXG(IGRID1) = MXG(IGRID1) - 1
      MYG(IGRID1) = MYG(IGRID1) - 1
!       default values changed
      CALL ININTG ('MXINP', MXG(IGRID1),'STA', MXC-1)
      MXG(IGRID1) = MXG(IGRID1) + 1
      IF (ONED) THEN
         CALL ININTG ('MYINP', MYG(IGRID1),'STA',0)
         IF (MYG(IGRID1) .NE. 0) THEN
            CALL MSGERR (1, '1D-simulation: [myinp] set to zero !')
         ENDIF
         MYG(IGRID1) = 0
      ELSE
         CALL ININTG ('MYINP', MYG(IGRID1),'STA',MYC-1)
      ENDIF
      MYG(IGRID1) = MYG(IGRID1) + 1
   ELSEIF (KEYWIS('UNSTRUC')) THEN

      IF (ONED) THEN
         CALL MSGERR (4,&
         &'1D-simulation cannot be done with unstructured grid')
         RETURN
      ENDIF
      IF (OPTG.EQ.3) THEN
         CALL MSGERR (4,&
         &'Unstructured input grid cannot be used with curvilinear grid')
         RETURN
      ENDIF

      IGTYPE(IGRID1) = 3
      MXG(IGRID1)    = nvertsg
      MYG(IGRID1)    = 1
      IF (IGRID2.GT.0) THEN
         IGTYPE(IGRID2) = 3
         MXG(IGRID2)    = nvertsg
         MYG(IGRID2)    = 1
      ENDIF
   ELSE
      CALL IGNORE('REG')
      IGTYPE(IGRID1) = 1
      IF (KSPHER .EQ. 0) THEN
         CALL READXY ('XPINP', 'YPINP', XPG(IGRID1), YPG(IGRID1),&
         &'UNC', 0., 0.)
         CALL INREAL ('ALPINP',ALPG(IGRID1),'UNC',0.)
      ELSE
!         [xpinp] and [ypinp] are required in case of spherical coordinates
         CALL READXY ('XPINP', 'YPINP', XPG(IGRID1), YPG(IGRID1),&
         &'REQ', 0., 0.)
         CALL INREAL ('ALPINP',ALPG(IGRID1),'STA',0.)
      ENDIF
!       ---------  ALPG(IGRID1) is always between -PI and PI   ---------
      ALTMP = ALPG(IGRID1)/360.
      ALPG(IGRID1)  = PI2 * (ALTMP - NINT(ALTMP))
      COSPG(IGRID1) = COS(ALPG(IGRID1))
      SINPG(IGRID1) = SIN(ALPG(IGRID1))
!       *** the user gives number of meshes in X resp. Y direction ***
!       *** the program uses the number of grid points             ***
      MXG(IGRID1) = MXG(IGRID1) - 1
      MYG(IGRID1) = MYG(IGRID1) - 1
      CALL ININTG ('MXINP', MXG(IGRID1),'RQI',-1)
      IF (ONED) THEN
         CALL ININTG ('MYINP', MYG(IGRID1),'STA',0)
         IF (MYG(IGRID1) .NE. 0) THEN
            CALL MSGERR (1, '1D-simulation: [myinp] set to zero !')
            MYG(IGRID1) = 0
         ENDIF
      ELSE
         CALL ININTG ('MYINP', MYG(IGRID1),'RQI',-1)
      ENDIF
      MXG(IGRID1) = MXG(IGRID1) + 1
      MYG(IGRID1) = MYG(IGRID1) + 1

      CALL INREAL ('DXINP',DXG(IGRID1),'RQI',0.)
      CALL INREAL ('DYINP',DYG(IGRID1),'STA',DXG(IGRID1))

   ENDIF

!     exception values for input variables

   CALL INKEYW ('STA', ' ')
   IF (KEYWIS ('EXC')) THEN
      CALL INREAL ('EXCVAL', EXCFLD(IGRID1), 'REQ', 0.)
      IF (IGRID2.GT.0) EXCFLD(IGRID2) = EXCFLD(IGRID1)
   ENDIF

   LEDS(IGRID1) = 1
   IF (IGRID2.GT.0) LEDS(IGRID2) = 1
!     next lines to process the option NONSTAT for wind, currents
!     and waterlevel                                                VER.  30.00
!     SUBR. INCTIM reads a time string  and gives a time from given
!     reference day
   CALL INKEYW ('STA', ' ')
   IF (KEYWIS ('NONSTAT')) THEN
      IF (NSTATM.EQ.0) CALL MSGERR (3,&
      &'keyword NONSTAT not allowed in stationary mode')
      NSTATM = 1
      IF (IGRID1.EQ.1 .OR. IGRID1.EQ.8) CALL MSGERR (2,&
      &'nonstationary input field not allowed in this case')
      CALL INCTIM (ITMOPT,'TBEGINP',IFLBEG(IGRID1),'REQ',0D0)
      CALL INITVD ('DELTINP', IFLINT(IGRID1), 'REQ', 0D0)
      CALL INCTIM (ITMOPT,'TENDINP',IFLEND(IGRID1),'STA',1.D20)
      IFLDYN(IGRID1) = 1
      IFLTIM(IGRID1) = IFLBEG(IGRID1)
      IF (IGRID2 .GT. 0) THEN
         IFLBEG(IGRID2) = IFLBEG(IGRID1)
         IFLINT(IGRID2) = IFLINT(IGRID1)
         IFLEND(IGRID2) = IFLEND(IGRID1)
         IFLDYN(IGRID2) = IFLDYN(IGRID1)
         IFLTIM(IGRID2) = IFLTIM(IGRID1)
      ENDIF
      IF (IFLEND(IGRID1).LT.0.9E20) THEN
         IF ( MOD(IFLEND(IGRID1)-IFLBEG(IGRID1),IFLINT(IGRID1)) >&
         &0.01*IFLINT(IGRID1) .AND.&
         &MOD(IFLEND(IGRID1)-IFLBEG(IGRID1),IFLINT(IGRID1)) <&
         &0.99*IFLINT(IGRID1) )&
         &CALL MSGERR (1,&
         &'[deltinp] is not a fraction of the period')
      ENDIF
   ENDIF
   IF ((MXG(IGRID1).EQ.0).OR.(MYG(IGRID1).EQ.0)) RETURN
   IF (IGTYPE(IGRID1).EQ.3) RETURN

!     ***** input grid is included in output data                 *****
!     ***** reference point coincides with origin of output frame *****
   ALLOCATE(OPSTMP)
   OPSTMP%PSNAME = SNAMEG
   ALLOCATE(OPSTMP%XP(0))
   ALLOCATE(OPSTMP%YP(0))

   IF (IGTYPE(IGRID1) .EQ. 1) THEN
      OPSTMP%PSTYPE = 'F'
      XQLEN          = (MXG(IGRID1)-1)*DXG(IGRID1)
      OPSTMP%OPR(3) = XQLEN

      IF (ONED) THEN
         YQLEN          = XQLEN
      ELSE
         YQLEN          = (MYG(IGRID1)-1)*DYG(IGRID1)
      ENDIF

      OPSTMP%OPR(4) = YQLEN
      OPSTMP%OPR(1) = XPG(IGRID1)
      OPSTMP%OPR(2) = YPG(IGRID1)
      OPSTMP%OPR(5) = ALPG(IGRID1)
      OPSTMP%OPI(1) = MXG(IGRID1)
      OPSTMP%OPI(2) = MYG(IGRID1)
   ELSE
      OPSTMP%PSTYPE = 'H'
      OPSTMP%OPR(1) = FLOAT(MXG(IGRID1)-1)
      OPSTMP%OPR(2) = FLOAT(MYG(IGRID1)-1)
      OPSTMP%OPR(3) = 0.
      OPSTMP%OPR(4) = 0.
      OPSTMP%OPR(5) = 0.
      OPSTMP%OPI(1) = MXG(IGRID1)
      OPSTMP%OPI(2) = MYG(IGRID1)
!       *** Because the input grid is staggered should have    ***
!       *** same length in X and Y than the computational grid ***
      IF (MCGRD.GT.1) THEN
         XQLEN = XCGMAX - XCGMIN
         YQLEN = YCGMAX - YCGMIN
      ENDIF
   ENDIF
   IF (ITEST .GE. 50 .OR. INTES .GE. 5) THEN
      IF (OPSTMP%PSTYPE .EQ. 'F') THEN
         WRITE(PRINTF,"(' INP GRID PARAMETERS: ',/, 'IGRID , FRAMTYPE,XLENFR ,YLENFR XPFR YPFR ALPFR', ' MXFR MYFR',/,I2,1X,A,4X, 2(1X,E8.3), 2(1X,E10.3), F7.3, 2(1X,I4))")IGRID1,'F',XQLEN,YQLEN,&
         &XPG(IGRID1),YPG(IGRID1),ALPG(IGRID1),MXG(IGRID1),MYG(IGRID1)
      ELSE
         WRITE(PRINTF,"(' INP GRID PARAMETERS: ',/, 'IGRID, FRAMTYPE ,XMAXFR ,YMAXFR XMINFR YMINFR ALPFR', ' MXFR MYFR',/, I3,8X,A,1X,4(3X,I4),5X,E8.3,2(1X,I4))")IGRID1,OPSTMP%PSTYPE,MXG(IGRID1)-1,&
         &MYG(IGRID1)-1,0,0,0.,MXG(IGRID1),MYG(IGRID1)
      ENDIF
   ENDIF
   NULLIFY(OPSTMP%NEXTOPS)
   IF ( .NOT.LOPS ) THEN
      FOPS = OPSTMP
      COPS => FOPS
      LOPS = .TRUE.
   ELSE
      COPS%NEXTOPS => OPSTMP
      COPS => OPSTMP
   END IF

   IF (IGRID2.GT.0) THEN
      XPG(IGRID2)   = XPG(IGRID1)
      YPG(IGRID2)   = YPG(IGRID1)
      ALPG(IGRID2)  = ALPG(IGRID1)
      COSPG(IGRID2) = COSPG(IGRID1)
      SINPG(IGRID2) = SINPG(IGRID1)
      DXG(IGRID2)   = DXG(IGRID1)
      DYG(IGRID2)   = DYG(IGRID1)
      MXG(IGRID2)   = MXG(IGRID1)
      MYG(IGRID2)   = MYG(IGRID1)
      STAGX(IGRID2) = STAGX(IGRID1)
      STAGY(IGRID2) = STAGY(IGRID1)
      IGTYPE(IGRID2)= IGTYPE(IGRID1)
   ENDIF

RETURN
!     end of subroutine SINPGR
end subroutine SINPGR
!************************************************************************
!                                                                      *
SUBROUTINE SREDEP ( LWINDR, LWINDM ,LOGCOM )
   USE swan_legacy_io, ONLY: INAR2D, COPYCH
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INKEYW, INREAL, KEYWIS, WRNKEY
   USE swan_array_copy, ONLY: SWCOPR
!                                                                      *
!************************************************************************

   USE swan_time, ONLY: default_time_context
   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_GENARR
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
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     34.01: Jeroen Adema
!     40.02: IJsbrand Haagsma
!     40.04: Annette Kieftenburg
!     40.31: Marcel Zijlema
!     40.35: Nico Booij
!     40.41: Marcel Zijlema
!     40.55: Marcel Zijlema
!     40.59: Erick Rogers
!     41.20: Casey Dietrich
!     41.75: Erick Rogers
!
!  1. Updates
!
!     20.05, Jan. 94: new pool
!     20.67, Dec. 95: call of REPARM is modified, VFAC is read in SREDEP itself
!     30.72, Nov. 97: Changed position of label 18 in block IF, as suggested by
!                     Richard Gorman
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.00, Jan. 98: calls of REPARM and INAR2D changed
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Oct. 00: Avoided real/int conflict by introducing RPOOL
!     40.31, Oct. 03: remove POOL construction
!     40.35, Jun. 04: introducing turbulent viscosity model
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.55, Dec. 05: introducing vegetation model
!     40.59, Aug. 07: introducing fluid mud-induced dissipation model
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Reading of depths and/or currents
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     LWINDR
!     LWINDM
!     LOGCOM
!
!  8. Subroutines used
!
!     INAR2D
!     INKEYW
!     KEYWIS
!     MSGERR
!     OTAR2D
!     REPARM (all Ocean Pack)


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
!     NDS   = unit reference number
!     DFORM = format string
!     VFAC  = multiplication factor for data to be read
!     DESCR = string used in heading of datafile
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     Call INKEYW to read keyword from user input
!     If bottom must be read (command BOT), then
!         If no current is read (LEDS < 2), then
!             LEDS = 1 (indicating INP BOT command has been given)
!             LEDS = 2 (indicating bottom is read)
!         Else
!             LEDS = 3 (indicating bottom and current are read)
!         ------------------------------------------------------------
!         Call REPARM to process the rest of the command
!         If depthfile is standard Ocean Pack file (IDLA > 100), then
!             Call OTAR2D to read bottom levels into array IDEB
!         Else
!             Call INAR2D to read bottom levels inyo array IDEB
!         ------------------------------------------------------------
!     Elseif current must be read (command CUR), then
!         Switch for current is on (ICUR = 1)
!         If no current is read (LEDS < 2), then
!             LEDS = LEDS + 2 (indicating current is read)
!         ------------------------------------------------------------
!         Call REPARM to read filename and file organisation
!         If depthfile is standard Ocean Pack file (IDLA > 100), then
!             Call OTAR2D to read current components into arrays IUXB
!               and IUYB
!         Else
!             Call INAR2D to read current components into arrays IUXB
!               and IUYB
!         ------------------------------------------------------------
!     Else
!         Call MSGERR to generate an error message
!     ----------------------------------------------------------------
!
! 13. Source text

   REAL, ALLOCATABLE :: TARR(:)
   INTEGER, SAVE :: IENT = 0
   INTEGER    :: IGR1, IGR2, LWINDR, LWINDM, NHEDC
      LOGICAL :: VECTOR
   LOGICAL    LOGCOM(7)
   CALL STRACE (IENT,'SREDEP')

!     ***** read instructions of the user *****
!
!   ============================================================================
!
!   READinp    BOTtom/WLevel/CURrent/FRiction/WInd/COORdinates/
!              NPLAnts/TURB/MUDL/AICE/HICE/HSS/TSS/DSS
!        [fac]  / 'fname1'        \
!               \ SERIES 'fname2' /  [idla] [nhedf] ([nhedt]) (nhedvec])     &
!        FREE / FORMAT 'form' / [idfm] / UNFORMATTED
!
!   ============================================================================

   CALL INKEYW ('REQ',' ')
   IGR2 = 0
   IF (KEYWIS ('BOT')) THEN
      IGR1      = 1
      LOGCOM(3) = .TRUE.
   ELSE IF (KEYWIS ('CUR')) THEN
      IGR1 = 2
      IGR2 = 3
      ICUR = 1
   ELSE IF (KEYWIS ('FR')) THEN
      IGR1   = 4
      VARFR  = .TRUE.
      MCMVAR = MCMVAR + 2
      JFRC2  = MCMVAR - 1
      JFRC3  = MCMVAR
      ALOCMP = .TRUE.
   ELSE IF (KEYWIS ('WI')) THEN
      LWINDR = 2
      IWIND  = LWINDM
      IGR1   = 5
      IGR2   = 6
      VARWI  = .TRUE.
   ELSE IF (KEYWIS ('WL')) THEN
      IGR1   = 7
      VARWLV = .TRUE.
   ELSE IF (KEYWIS ('COOR')) THEN
      IGR1   = 8
      IGR2   = 9
      LOGCOM(4) = .TRUE.
!       *** Next lines because there is no information for READ COORD  ***
!       *** command INPGRID was not used to read coordinates           ***
      MXG(IGR1) = MXC
      MYG(IGR1) = MYC
      MXG(IGR2) = MXC
      MYG(IGR2) = MYC
   ELSE IF (KEYWIS ('ASTD')) THEN
!       air-sea temperature difference
      IGR1   = 10
      VARAST = .TRUE.
      IF (JASTD2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JASTD2 = MCMVAR - 1
         JASTD3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('NPLA')) THEN
!       number of plants per square meter
      IGR1   = 11
      VARNPL = .TRUE.
      IF (JNPLA2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JNPLA2 = MCMVAR - 1
         JNPLA3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('TURB')) THEN
!       turbulent viscosity
      IGR1   = 12
      VARTUR = .TRUE.
      IF (JTURB2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JTURB2 = MCMVAR - 1
         JTURB3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('MUDL')) THEN
!       fluid mud layer
      IGR1   = 13
      VARMUD = .TRUE.
      IMUD   = 1
      IF (JMUDL2.LE.1) THEN
         MCMVAR = MCMVAR + 3
         JMUDL1 = MCMVAR - 2
         JMUDL2 = MCMVAR - 1
         JMUDL3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('AICE')) THEN
!       ice concentration (fraction)
      IGR1   = 14
      VARAICE = .TRUE.
      IF (JAICE2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JAICE2 = MCMVAR - 1
         JAICE3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('HICE')) THEN
!       ice thickness (in meters)
      IGR1   = 15
      VARHICE = .TRUE.
      IF (JHICE2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JHICE2 = MCMVAR - 1
         JHICE3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('HSS').OR.KEYWIS ('IGHS')) THEN
!       sea-swell significant wave height
      IGR1   = 16
      VARHSS = .TRUE.
      IF (JHSS2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JHSS2 = MCMVAR - 1
         JHSS3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('TSS').OR.KEYWIS ('IGTM')) THEN
!       sea-swell mean wave period
      IGR1   = 17
      VARTSS = .TRUE.
      IF (JTSS2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JTSS2 = MCMVAR - 1
         JTSS3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE IF (KEYWIS ('DSS').OR.KEYWIS ('IGDIR')) THEN
!       sea-swell mean wave direction
      IGR1   = 18
      VARDSS = .TRUE.
      IF (JDSS2.LE.1) THEN
         MCMVAR = MCMVAR + 2
         JDSS2 = MCMVAR - 1
         JDSS3 = MCMVAR
         ALOCMP = .TRUE.
      ENDIF
   ELSE
      CALL  WRNKEY
   ENDIF

!     read multiplication factor

   CALL INREAL ('FAC', IFLFAC(IGR1), 'STA', 1.)

   IF (IGR2.GT.0) THEN
      VECTOR = .TRUE.
      IFLFAC(IGR2) = IFLFAC(IGR1)
   ELSE
      VECTOR = .FALSE.
   ENDIF

   CALL REPARM (IFLNDF(IGR1), IFLNDS(IGR1), IFLIDL(IGR1),&
   &IFLIFM(IGR1), IFLFRM(IGR1), IFLNHF(IGR1),&
   &IFLDYN(IGR1), IFLNHD(IGR1), VECTOR, NHEDC)
   IF (STPNOW()) RETURN

   IF (ITEST.GE.60) WRITE (PRTEST, "(' Reading parameters: ', 5I4, A, /, 12X,I5,I5,I5,L2,I5)") IGR1, IFLNDF(IGR1),&
   &IFLNDS(IGR1), IFLIDL(IGR1), IFLIFM(IGR1), IFLFRM(IGR1),&
   &IFLNHF(IGR1), IFLDYN(IGR1), IFLNHD(IGR1), VECTOR, NHEDC

   IFLNHD(IGR1) = IFLNHD(IGR1) + NHEDC
   IF (IGR2.GT.0) THEN
      IFLNDF(IGR2) = IFLNDF(IGR1)
      IFLNDS(IGR2) = IFLNDS(IGR1)
      IFLIDL(IGR2) = IFLIDL(IGR1)
      IFLIFM(IGR2) = IFLIFM(IGR1)
      IFLFRM(IGR2) = IFLFRM(IGR1)
      IFLNHF(IGR2) = IFLNHF(IGR1)
      IFLNHD(IGR2) = NHEDC
   ENDIF

   IF (LEDS(IGR1).EQ.0 .AND. IGR1 .NE. 8) THEN
      CALL MSGERR (2, 'Input grid not given')
      RETURN
   ENDIF

   IF ( IGR1.EQ.1 .AND. grid_generator.EQ.meth_adcirc ) THEN
!        bottom topography will be taken from fort.14
      CALL MSGERR(1,'depth will be taken from grid file fort.14 ')
      IGTYPE(1) = 3
      LEDS(1)   = 2
      RETURN
   ENDIF

   IF (.NOT.ALLOCATED(TARR)) THEN
      ALLOCATE( TARR(MXG(IGR1)*MYG(IGR1)) )
      TARR = 0.
   END IF
   CALL INAR2D( TARR        , MXG(IGR1), MYG(IGR1), IFLNDF(IGR1),&
   &IFLNDS(IGR1), IFLIFM(IGR1), IFLFRM(IGR1),&
   &IFLIDL(IGR1), IFLFAC(IGR1),&
   &IFLNHD(IGR1), IFLNHF(IGR1))
   IF (STPNOW()) RETURN
   IF (IGR1.EQ.1) THEN
      CALL ENSURE_FIELD_SIZE (DEPTH, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, DEPTH, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.2) THEN
      CALL ENSURE_FIELD_SIZE (UXB, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, UXB, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.4) THEN
      CALL ENSURE_FIELD_SIZE (FRIC, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, FRIC, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.5) THEN
      CALL ENSURE_FIELD_SIZE (WXI, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, WXI, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.7) THEN
      CALL ENSURE_FIELD_SIZE (WLEVL, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, WLEVL, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.8) THEN
      CALL SWCOPR( TARR, XCGRID, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.10) THEN
      CALL ENSURE_FIELD_SIZE (ASTDF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, ASTDF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.11) THEN
      CALL ENSURE_FIELD_SIZE (NPLAF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, NPLAF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.12) THEN
      CALL ENSURE_FIELD_SIZE (TURBF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, TURBF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.13) THEN
      CALL ENSURE_FIELD_SIZE (MUDLF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, MUDLF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.14) THEN
      CALL ENSURE_FIELD_SIZE (AICEF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, AICEF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.15) THEN
      CALL ENSURE_FIELD_SIZE (HICEF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, HICEF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.16) THEN
      CALL ENSURE_FIELD_SIZE (HSSF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, HSSF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.17) THEN
      CALL ENSURE_FIELD_SIZE (TSSF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, TSSF, MXG(IGR1)*MYG(IGR1) )
   ELSE IF (IGR1.EQ.18) THEN
      CALL ENSURE_FIELD_SIZE (DSSF, MXG(IGR1)*MYG(IGR1))
      CALL SWCOPR( TARR, DSSF, MXG(IGR1)*MYG(IGR1) )
   END IF
   DEALLOCATE(TARR)
   IF (IGR2.GT.0) THEN

      IF (.NOT.ALLOCATED(TARR)) THEN
         ALLOCATE( TARR(MXG(IGR2)*MYG(IGR2)) )
         TARR = 0.
      END IF
      CALL INAR2D( TARR        , MXG(IGR2), MYG(IGR2), IFLNDF(IGR2),&
      &IFLNDS(IGR2), IFLIFM(IGR2), IFLFRM(IGR2),&
      &IFLIDL(IGR2), IFLFAC(IGR2),&
      &IFLNHD(IGR2), 0)
      IF (STPNOW()) RETURN
      IF (IGR2.EQ.3) THEN
         CALL ENSURE_FIELD_SIZE (UYB, MXG(IGR2)*MYG(IGR2))
         CALL SWCOPR( TARR, UYB, MXG(IGR2)*MYG(IGR2) )
      ELSE IF (IGR2.EQ.6) THEN
         CALL ENSURE_FIELD_SIZE (WYI, MXG(IGR2)*MYG(IGR2))
         CALL SWCOPR( TARR, WYI, MXG(IGR2)*MYG(IGR2) )
      ELSE IF (IGR2.EQ.9) THEN
         CALL SWCOPR( TARR, YCGRID, MXG(IGR2)*MYG(IGR2) )
      END IF
      DEALLOCATE(TARR)
   ENDIF
!     set time of reading
   IF (IFLDYN(IGR1) .EQ. 1) THEN
      IF (NSTATM.EQ.0) CALL MSGERR (2,&
      &'nonstationary input field requires MODE NONSTAT')
      IFLTIM(IGR1) = IFLBEG(IGR1)
      IF (IGR2.GT.0) IFLTIM(IGR2) = IFLTIM(IGR1)
   ENDIF

   LEDS(IGR1) = 2
   IF (VECTOR) LEDS(IGR2) = 2

   RETURN
! * end of subroutine SREDEP *
end subroutine SREDEP
!************************************************************************
!                                                                      *
SUBROUTINE SSFILL (SPCSIG, SPCDIR, SPECTRAL_POWERS)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM2
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
!     30.82: IJsbrand Haagsma
!     40.02: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     20.43         : Calculation of spectral directions added
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description of SPCDIR
!     40.02, Sep. 00: Calculate powers of Sigma for later use
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Discretisation in frequency (sigma) and direction (theta)
!
!  3. Method
!
!     Create a logarithmic distribution in frequency between lowest and
!     frequencies and store them in the array SPCSIG.
!
!     Create a distribution in direction that does not coincide with the orientation
!     of the computational grid, calculate some geometric derivatives and store
!     it in the array SPCDIR
!
!  4. Argument variables
!
!   o SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
!   o SPCSIG: Relative frequencies in computational domain in sigma-space 30.72

   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   TYPE(spectral_powers_t), INTENT(INOUT) :: SPECTRAL_POWERS


!  5. SUBROUTINES CALLING
!
!       none
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

   INTEGER, SAVE :: IENT = 0
   INTEGER :: ID, IS
   REAL    :: OLDDIR, SFAC
   CALL STRACE(IENT,'SSFILL')

!     distribution of spectral frequencies
!
!     FRINTF is the frequency integration factor (=df/f)
   FRINTF = ALOG(SHIG/SLOW) / FLOAT(MSC-1)
   SFAC   = EXP(FRINTF)
   FRINTH = SQRT(SFAC)
!     determine spectral frequencies (logarithmic distribution)
   SPCSIG(1) = SLOW
   do IS = 2, MSC
      SPCSIG(IS) = SPCSIG(IS-1) * SFAC
   end do

!     Calculate powers of sigma and store in global array

   CALL SPECTRAL_POWERS%REBUILD(SPCSIG)

!     distribution of spectral directions

   do ID = 1, MDC
      SPCDIR(ID,1) = SPDIR1 + FLOAT(ID-1)*DDIR
!        if a direction coincides with a direction of the (regular)
!        computational grid it is slightly changed
      IF (OPTG.EQ.1) THEN
         IF (ABS(MODULO(SPCDIR(ID,1)-ALPC+0.25*PI ,0.5*PI)-0.25*PI)&
         &.LT. 1.E-6) THEN
            OLDDIR = SPCDIR(ID,1)
            SPCDIR(ID,1) = OLDDIR + 2.E-6
            IF (ITEST.GE.50) WRITE (PRINTF, "(' Modified spectral direction', I4, 2(2X,F10.5))") ID,&
            &OLDDIR*180./PI, SPCDIR(ID,1)*180./PI
         ENDIF
      ENDIF
      SPCDIR(ID,2) = COS(SPCDIR(ID,1))
      SPCDIR(ID,3) = SIN(SPCDIR(ID,1))
      SPCDIR(ID,4) = SPCDIR(ID,2) **2
      SPCDIR(ID,5) = SPCDIR(ID,2) * SPCDIR(ID,3)
      SPCDIR(ID,6) = SPCDIR(ID,3) **2
   end do

   RETURN
! * end of function SSFILL *
end subroutine SSFILL
!************************************************************************
!                                                                      *
SUBROUTINE CGINIT (LOGCOM)
   USE swan_array_copy, ONLY: SWCOPI
   USE swan_parallel, ONLY: SWDECOMP
!JAC   USE swan_parallel, ONLY: SWBLKCOL
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE, TXPBLA
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_GENARR
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
!     30.81: Annette Kieftenburg
!     30.90: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.04: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     40.00, June 98: new subroutine replacing code inside SWREAD
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.81, Nov. 98: Adjustment for 1-D case of new boundary conditions
!     34.01, Feb. 99: Introducing STPNOW
!     40.04, Aug. 00: adjusted argumentlist of CGBOUN
!     40.02, Oct. 00: Avoided real/int conflict by introducing replacing
!                     RPOOL for POOL in DPPUTR
!     40.30, Apr. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL-mechanism and reconsidering this
!                     subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Jun. 07: extension to unstructured grids
!
!  2. PURPOSE
!
!     Initialise arrays for description of computational grid
!
!  3. METHOD
!
!
!  4. Argument variables
!
!     LOGCOM:

   LOGICAL LOGCOM(7)

!  6. Local variables
!
!     CHARS :     array to pass character info to MSGERR
!     IARR  :     help array
!     IENT  :     number of entries
!     INDX  :     index counter for global grid
!     IX    :     loop counter
!     IY    :     loop counter
!     MCGRDL:     number of wet grid points in own subdomain
!JAC!     MCOLR :     flag to indicate multi-colouring
!JAC!                 of subdomains (.TRUE.) or not (.FALSE.)
!     MSGSTR:     string to pass message to call MSGERR

   INTEGER, SAVE :: IENT = 0
   INTEGER INDX, IX, IY, MCGRDL
   INTEGER ISTAT, IF1, IL1
   INTEGER, ALLOCATABLE :: IARR(:)
!JAC   LOGICAL   MCOLR
   CHARACTER(LEN=20) CHARS(1)
   CHARACTER(LEN=80) MSGSTR

!  8. SUBROUTINES CALLING
!
!     SWREAD
!
!  9. SUBROUTINES USED
!
!     CGBOUN           Determines boundary of computational region
!     MSGERR : Handles error messages according to severity
!     NUMSTR : Converts integer/real to string
!     STRACE           Tracing routine for debugging
!     SWDECOMP
!JAC!     SWBLKCOL
!     SWCOPI
!TIMG!     SWTSTA
!TIMG!     SWTSTO
!     TXPBLA : Removes leading and trailing blanks in string


! 10. ERROR MESSAGES
!
!     ---
!
! 11. REMARKS
!
!     ---
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE(IENT,'CGINIT')

!     --- Calculation of computational grid dimension ***

   CALL ENSURE_FIELD_SIZE (KGRPGL, MXC, MYC)

   CALL SWDIM ( KGRPGL, DEPTH, XCGRID, YCGRID )

!     --- call CGBOUN to determine computational grid outline

   IF (ONED) THEN
      CALL ENSURE_FIELD_SIZE (KGRBGL, 4)
!JAC      IF(.NOT.ALLOCATED(IARR)) ALLOCATE(IARR(4))
      CALL CGBOUN ( KGRPGL, KGRBGL )
   ELSE
      IF(.NOT.ALLOCATED(IARR)) ALLOCATE(IARR(2*MCGRD))
      CALL CGBOUN ( KGRPGL, IARR )
      IF ( NGRBND.GT.0 ) THEN
         CALL ENSURE_FIELD_SIZE (KGRBGL, 2*NGRBND)
         CALL SWCOPI(IARR,KGRBGL,2*NGRBND)
      ELSE
         IF(.NOT.ALLOCATED(KGRBGL)) ALLOCATE(KGRBGL(0))
      END IF
   END IF
   NGRBGL = NGRBND

!     --- Carry out domain decomposition meant for
!         distributed-memory approach
!
!TIMG   CALL SWTSTA(211)
   CALL SWDECOMP
!TIMG   CALL SWTSTO(211)
   IF (STPNOW()) RETURN

!TIMG   CALL SWTSTA(212)
!
!     --- Create copy of parts of KGRPGL for each subdomain -> KGRPNT

   IF(.NOT.ALLOCATED(KGRPNT)) ALLOCATE(KGRPNT(MXC,MYC))
   KGRPNT = 1
   MCGRDL = 1
   DO IX = MXF, MXL
      DO IY = MYF, MYL
         INDX = KGRPGL(IX,IY)
         IF ( INDX.NE.1 ) THEN
            MCGRDL = MCGRDL + 1
            KGRPNT(IX-MXF+1,IY-MYF+1) = MCGRDL
         END IF
      END DO
   END DO

!     --- Create copies of parts of XCGRID, YCGRID for each subdomain

   CALL ENSURE_FIELD_SIZE (XGRDGL, MXCGL, MYCGL)
   CALL ENSURE_FIELD_SIZE (YGRDGL, MXCGL, MYCGL)
   XGRDGL = XCGRID
   YGRDGL = YCGRID
   DEALLOCATE(XCGRID,YCGRID)
   ALLOCATE(XCGRID(MXC,MYC))
   ALLOCATE(YCGRID(MXC,MYC))
   DO IX = MXF, MXL
      DO IY = MYF, MYL
         XCGRID(IX-MXF+1,IY-MYF+1) = XGRDGL(IX,IY)
         YCGRID(IX-MXF+1,IY-MYF+1) = YGRDGL(IX,IY)
      END DO
   END DO

!     --- Computation of XCLMIN, XCLMAX, YCLMIN, YCLMAX

   XCLMIN =  1.E09
   YCLMIN =  1.E09
   XCLMAX = -1.E09
   YCLMAX = -1.E09
   DO IX = 1, MXC
      DO IY = 1, MYC
         IF (KGRPNT(IX,IY).GT.1) THEN
            IF (XCGRID(IX,IY).LT.XCLMIN) XCLMIN = XCGRID(IX,IY)
            IF (YCGRID(IX,IY).LT.YCLMIN) YCLMIN = YCGRID(IX,IY)
            IF (XCGRID(IX,IY).GT.XCLMAX) XCLMAX = XCGRID(IX,IY)
            IF (YCGRID(IX,IY).GT.YCLMAX) YCLMAX = YCGRID(IX,IY)
         END IF
      END DO
   END DO

!     --- Create copy of parts of KGRBGL for each subdomain -> KGRBND

   IF ( NGRBND.GT.0 ) THEN
      IF (PARLL) THEN
         IARR = 0
         CALL CGBOUN ( KGRPNT, IARR )
         IF(.NOT.ALLOCATED(KGRBND)) ALLOCATE(KGRBND(2*NGRBND))
         IF (NGRBND.GT.0) CALL SWCOPI(IARR,KGRBND,2*NGRBND)
      ELSE
         IF(.NOT.ALLOCATED(KGRBND)) ALLOCATE(KGRBND(2*NGRBND))
         KGRBND = KGRBGL
      END IF
   ELSE
      IF(.NOT.ALLOCATED(KGRBND)) ALLOCATE(KGRBND(0))
   END IF
   IF(ALLOCATED(IARR)) DEALLOCATE(IARR)

!TIMG   CALL SWTSTO(212)
!JAC
!JAC!     --- Colour subdomains with red, yellow, green and black
!JAC
!JAC   MCOLR = .FALSE.
!JAC!TIMG   CALL SWTSTA(215)
!JAC   CALL SWBLKCOL ( MCOLR, KGRPNT )
!JAC!TIMG   CALL SWTSTO(215)
!JAC   IF (STPNOW()) RETURN

   ISTAT = 0
   IF(.NOT.ALLOCATED(AC2)) ALLOCATE(AC2(MDC,MSC,MCGRD),STAT=ISTAT)
   IF ( ISTAT.NE.0 ) THEN
      CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
      CALL TXPBLA(CHARS(1),IF1,IL1)
      MSGSTR = 'Allocation problem: array AC2 and return code is '//&
      &CHARS(1)(IF1:IL1)
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF
   AC2 = 0.
   LOGCOM(6) = .TRUE.

!     Note: piece of code w.r.t. defining COMPGRID has been
!           moved to command CGRID in routine SWREAD!
!
!     --- the following arrays for unstructured grids are allocated
!         as empty ones

   IF ( .NOT.ALLOCATED(xcugrd) ) ALLOCATE(xcugrd(0))
   IF ( .NOT.ALLOCATED(ycugrd) ) ALLOCATE(ycugrd(0))
   IF ( .NOT.ALLOCATED( vmark) ) ALLOCATE( vmark(0))

   RETURN
!     end of subroutine CGINIT
end subroutine CGINIT
!************************************************************************
!                                                                      *
SUBROUTINE SWDIM ( KGRPNT, DEPTH, XCGRID, YCGRID )
   USE swan_input_interpolation, ONLY: SVALQI
   USE swan_services, ONLY: CVCHEK
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM2
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
!     30.60: Nico Booij
!     30.72: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.60, July 97: exception values for coordinates are introduced
!                     subroutine restructured
!     30.60, Aug. 97: correction KGRPNT
!     30.60, Aug. 97: test point introduced
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.03, Dec. 99: computation of XCGMIN, XCGMAX, YCGMIN, YCGMAX is now done
!                     for curvilinear and regular grids.
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     ---
!
!  3. Method
!
!  4. Argument variables
!
!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!  5. SUBROUTINES CALLING
!
!       SWREAD
!
!  6. SUBROUTINES USED
!
!       SVALQI (SWAN/SER)
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
!       For all potential grid points do
!           Make grid address =1 (means invalid grid address)
!           Determine whether this point is a test point
!           If coordinates are valid
!           Then If grid offset is not yet defined (LXOFFS = False)
!                Then make grid offset equal to coordinates of this point
!                     Make LXOFFS = True
!                Else Subtract offset from coordinates
!                --------------------------------------------------------
!                If bottom level is not an exception value
!                Then assign a valid grid address to this grid point
!                     increase MCGRD by 1
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER     KGRPNT(MXC,MYC)
   INTEGER, SAVE :: IENT = 0
   INTEGER     IX, IY
   REAL        DEPTH(*)
   REAL        DEP, XP, YP
   CALL STRACE(IENT,'SWDIM')

   do IX = 1, MXC
      do IY = 1, MYC
!         at start make each grid point invalid
         KGRPNT(IX,IY) = 1

!         If coordinates are valid (excluding exception values)
!         Then If grid offset is not yet defined (LXOFFS = False)
!              Then make grid offset equal to coordinates of this point
!                   Make LXOFFS = True
!              Else Subtract offset from coordinates

         IF (EQREAL(XCGRID(IX,IY), EXCFLD(8))) THEN
            IF (.NOT. EQREAL(YCGRID(IX,IY), EXCFLD(9))) THEN
               CALL MSGERR (2, 'incorrect grid coordinates')
               WRITE (PRINTF, "(' X= ', E12.4, ' Y= ', E12.4)") XCGRID(IX,IY), YCGRID(IX,IY)
            ENDIF
         ELSE
            IF (EQREAL(YCGRID(IX,IY), EXCFLD(9))) THEN
               CALL MSGERR (2, 'incorrect grid coordinates')
               WRITE (PRINTF, "(' X= ', E12.4, ' Y= ', E12.4)") XCGRID(IX,IY), YCGRID(IX,IY)
            ELSE
               IF (OPTG.EQ.3) THEN
                  IF (.NOT. LXOFFS) THEN
                     XOFFS  = XCGRID(IX,IY)
                     YOFFS  = YCGRID(IX,IY)
                     LXOFFS = .TRUE.
                     XCGRID(IX,IY) = 0.
                     YCGRID(IX,IY) = 0.
                  ELSE
                     XCGRID(IX,IY) = REAL(XCGRID(IX,IY) - DBLE(XOFFS))
                     YCGRID(IX,IY) = REAL(YCGRID(IX,IY) - DBLE(YOFFS))
                  ENDIF
               ENDIF
               XP = XCGRID(IX,IY)
               YP = YCGRID(IX,IY)
!               ***** compute bottom level *****

               DEP = SVALQI (XP, YP, 1, DEPTH, 1 ,IX ,IY)

!             If bottom level is not an exception value
!             Then assign a valid grid address to this grid point
!                  increase MCGRD by 1
               IF (.NOT. EQREAL(DEP, EXCFLD(1))) THEN
                  MCGRD = MCGRD + 1
                  KGRPNT(IX,IY) = MCGRD
               ENDIF
               IF (ITEST .GE. 250 .OR. INTES .GE. 30)&
               &WRITE (PRINTF,"(2(I3,1X),1X,3(F10.1,1X),5X,I5)") IX, IY, XP, YP,&
               &DEP, KGRPNT(IX,IY)
            ENDIF
         ENDIF
      end do
   end do

   EXCFLD(8) = REAL(EXCFLD(8) - DBLE(XOFFS))
   EXCFLD(9) = REAL(EXCFLD(9) - DBLE(YOFFS))

   IF (MCGRD.LE.1) CALL MSGERR (3, 'No valid grid points found')
   IF (ITEST.GE.60) WRITE(PRINTF,*)&
   &' Offset values in SWDIM:', XOFFS, YOFFS,&
   &' ; ', MCGRD-1, ' grid points'

!     check geometric validity of the grid (all meshes must have same
!     orientation when going around the mesh)
   CALL CVCHEK (KGRPNT, XCGRID, YCGRID)

!     *** Computation of XCGMIN, XCGMAX, YCGMIN, YCGMAX ***
   XCGMIN =  1.E09
   YCGMIN =  1.E09
   XCGMAX = -1.E09
   YCGMAX = -1.E09
   do IX = 1, MXC
      do IY = 1, MYC
         IF (KGRPNT(IX,IY) .GT. 1) THEN
            IF (XCGRID(IX,IY) .LT. XCGMIN) XCGMIN = XCGRID(IX,IY)
            IF (YCGRID(IX,IY) .LT. YCGMIN) YCGMIN = YCGRID(IX,IY)
            IF (XCGRID(IX,IY) .GT. XCGMAX) XCGMAX = XCGRID(IX,IY)
            IF (YCGRID(IX,IY) .GT. YCGMAX) YCGMAX = YCGRID(IX,IY)
         ENDIF
      end do
   end do
!     *** Computation of xclen and yclen in a curvilinear grid case ***
   IF (OPTG .EQ. 3) THEN
      XCLEN = XCGMAX - XCGMIN
      YCLEN = YCGMAX - YCGMIN
   ENDIF
   IF (ITEST .GE. 100) THEN
      WRITE (PRINTF,*)' Min and Max X and Y from subr SWDIM',&
      &XCGMIN+XOFFS, XCGMAX+XOFFS, YCGMIN+YOFFS, YCGMAX+YOFFS
      WRITE (PRINTF,*)' Size in X and Y from subr SWDIM',&
      &XCLEN, YCLEN
   ENDIF

   RETURN
! * end of subroutine SWDIM *
end subroutine SWDIM
!************************************************************************
!                                                                      *
SUBROUTINE CGBOUN (KGRPNT, KGRBND)
   USE swan_grid_point_validation, ONLY: VALIDBP
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_grid_point_validation, ONLY: PVALID
!                                                                      *
!************************************************************************

   USE SWCOMM3
   USE OCPCOMM4
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
!     40.00, 40.03, 40.13  Nico Booij
!     30.81  Annette Kieftenburg
!     40.04  Annette Kieftenburg
!     40.30  Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     New function for curvilinear version (ver. 40.00). May '98
!     30.81, Nov. 98: Adjustment for 1-D case of new boundary conditions
!     40.03, Dec. 99: 2d procedure modified; boundary can now consist also
!                     of diagonals
!     40.04, Aug. 00: prevented that boundary is not closed: several checks
!                     added
!                     argument list adjusted
!     40.13, July 01: 1-D procedure corrected
!     40.30, Apr. 01: introduction distributed-memory approach using MPI
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Determine array containing all points of (a) (closed) boundary/boundaries
!     within the computational grid
!
!  3. Method (updated...)
!
!     1D: go through all gridpoints (ascending and descending) to check
!         for validity
!         save first and last point in boundary array
!     2D: For all grid points:
!         0) find first point which is possibly on the boundary
!         1) check whether neighbour is a valid point and valid boundary point
!            save information that point is scanned
!         2) If so store point in boundary array and repeat from 1
!            Else go to next neighbour (if not scanned already
!                                       Else remove isolated point from
!                                            computational grid and start from 0)
!
!  4. Argument variables
!
!     KGRPNT    in-& output   indirect addresses for grid points
!     KGRBND    output        array containing all boundary points
!                             (+ 2 extra zeros as area separator
!                             for all separated areas)

   INTEGER KGRPNT(MXC,MYC), KGRBND(*)

!  5. Parameter variables
!
!  6. Local variables
!
!     ICGRD         Counter for computational gridcells
!     IDIR          Direction number 1 = to the right
!                                    2 = upwards
!                                    3 = to the left
!                                    4 = downwards
!     IENT          Number of entries of this subroutine
!     IXC, IYC      X- and Y-index of point under consideration
!     IXNEW, IYNEW  X- and Y-index of point under consideration
!     IXOLD, IYOLD  X- and Y-index of point under consideration
!     KGRPNTNEW     Indirect addresses for grid points
!     MCGRDNEW      Number of points in computational grid after elimination
!                   of isolated and points that are part of 1D configurations
!                   or connections
!     MCGRDOLD      Number of points in computational grid before elimination
!                   of isolated and points that are part of 1D configurations
!                   or connections
!     WNP           Number of Wet Neighbouring Points
!     SCANND        Array containing information whether point is scanned


   INTEGER, SAVE :: IENT = 0
   INTEGER  ICGRD, IDIR, IXC, IXNEW, IXOLD
   INTEGER  IYC, IYNEW, IYOLD
   INTEGER  MCGRDNEW, MCGRDOLD, WNP
   INTEGER, ALLOCATABLE :: KGRPNTNEW(:,:), SCANND(:)

!  8. Subroutines used
!
!     Function PVALID
!     Function VALIDBP
!     STRACE
!     SEPARAREA
!     MSGERR


!  9. Subroutines calling
!
!     CGINIT
!
! 10. Error messages
!
! 11. Remarks
!
!     In case of 2D:
!     A boundary is scanned only once: 1D areas or connections between
!     areas are excluded
!
! 12. Structure
!
!     -----------------------------------------------------------------
!     If 1D: For all gridpoints (ascending)
!            If gridpoint is valid store it's value in KGRBND(1)
!            Else give error message
!            For all gridpoints (descending)
!            If gridpoint is valid store it's value in KGRBND(3)
!            Else give error message
!     Number of boundary outline points is 2
!     -----------------------------------------------------------------
!     Else:
!     Make number of boundary outline points = 0
!     For all computational grid points do
!         make SCANNED(ix,iy) = False
!     Save MCGRD
!     -----------------------------------------------------------------
!     For all computational grid points do
!         If (ix,iy) is a valid grid point
!         Then If point (ix,iy) is no valid boundary point or
!                 point (ix,iy) has neighbouring points that are all
!                               invalid boundary points
!              Then remove this point from computational grid
!                   SCANND(ix,iy) = True
!         If (ix,iy) is not scanned
!              If point (ix-1,iy) is not a valid grid point and
!                 (ix,iy) is a valid boundary point
!              Then Make ixold=ix; iyold=iy
!                   Increase number of boundary outline points by 1
!                   Store (ixold,iyold)
!                   Make SCANNED(ixold,iyold) = True
!                   Make idir=4
!                   If number of Wet Neighbouring points = 3
!                   possibly two areas should be separated
!                   Repeat
!                       Case idir=
!                       =1: Make ixnew=ixold+1; iynew=iyold
!                       =2: Make ixnew=ixold  ; iynew=iyold+1
!                       =3: Make ixnew=ixold-1; iynew=iyold
!                       =4: Make ixnew=ixold  ; iynew=iyold-1
!                       -----------------------------------------------
!                       If (ixnew,iynew) is a valid grid point and
!                          valid boundary point
!                       Then If SCANNED(ixnew,iynew)
!                            Then exit from repeat
!                            Else
!                              Increase number of boundary outline points by 1
!                              Store (ixnew,iynew) in KGRBND array
!                              Make SCANNED(ixnew,iynew) = True
!                              Make ixold=ixnew; iyold=iynew
!                              Make idir = idir-1
!                              If number of Wet Neighbouring points = 3
!                              possibly two areas should be separated
!                              If idir=0
!                              Then Make idir=4
!                        Else Make idir = idir+1
!                          If (ixnew,iynew) is an invalid boundary point
!                             (ixnew,iynew) is a valid grid point
!                             remove point from computational grid
!                             SCANND(ixnew,iynew) = True
!                          If idir=5
!                          Then Make idir=1
!                   ---------------------------------------------------
!                   Increase number of boundary outline points by 1
!                   Store (0,0)      {separation between outlines}
!     -----------------------------------------------------------------
!     If MCGRD has changed
!     Then count number of valid gridpoints
!          store  indirect addressing number in new array
!       give MCGRD new value
!       write new information to old array
!     -----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE (IENT,'CGBOUN')

   IF (ONED) THEN
      KGRBND(1) = -1
      KGRBND(2) = 1
      DO IXC = 1, MXC
         IF (KGRPNT(IXC,1).GT.1) THEN
            KGRBND(1) = IXC
            CYCLE
         END IF
      ENDDO
      IF (KGRBND(1) .LT. 0) THEN
         CALL MSGERR(3,'No valid gridpoint defined')
      END IF
      KGRBND(3) = -1
      KGRBND(4) = 1
      DO IXC = MXC, 1, -1
         IF (KGRPNT(IXC,1).GT.1) THEN
            KGRBND(3) = IXC
            CYCLE
         END IF
      ENDDO
      NGRBND = 2
   ELSE
      ALLOCATE(KGRPNTNEW(MXC,MYC))
      ALLOCATE(SCANND(MCGRD))
      NGRBND = 0
      DO ICGRD = 1, MCGRD
         SCANND(ICGRD) = 0
      ENDDO
      MCGRDOLD = MCGRD
      do IXC = 1, MXC
         do IYC = 1, MYC
            ICGRD = KGRPNT(IXC,IYC)
            IF (ICGRD.GT.1) THEN
!             point with four wet neighbouring points which are all
!             no valid boundary points is eliminated.
               IF ( NGRBGL.EQ.0 ) THEN
                  IF ((.NOT. VALIDBP(IXC,IYC,KGRPNT,WNP)).OR.&
                  &((.NOT. VALIDBP(IXC-1,IYC,KGRPNT,WNP)).AND.&
                  &(.NOT. VALIDBP(IXC+1,IYC,KGRPNT,WNP)).AND.&
                  &(.NOT. VALIDBP(IXC,IYC-1,KGRPNT,WNP)).AND.&
                  &(.NOT. VALIDBP(IXC,IYC+1,KGRPNT,WNP)))) THEN
                     KGRPNT(IXC,IYC) = 1
                     MCGRD = MCGRD - 1
                     SCANND(ICGRD) = 1
                     CALL MSGERR (1,&
                     &'Point removed from computational grid')
                     WRITE(PRINTF,"('Point with index (', I6, ',', I6,') was ', 'removed from computational grid, because ', 'it is part of a 1D configuration or 1D ', 'connection.')")  IXC, IYC
                  ENDIF
               END IF
               IF (SCANND(ICGRD).EQ.0) THEN
                  IF (.NOT. PVALID(IXC-1,IYC,KGRPNT).AND.&
                  &VALIDBP(IXC,IYC,KGRPNT,WNP)) THEN
                     IXOLD = IXC
                     IYOLD = IYC
                     NGRBND = NGRBND + 1
                     KGRBND(2*NGRBND-1) = IXOLD
                     KGRBND(2*NGRBND)   = IYOLD
                     SCANND(KGRPNT(IXOLD,IYOLD)) = 1
                     IDIR = 4
                     IF (WNP.EQ.3) THEN
                        CALL SEPARAREA(IXNEW, IYNEW, KGRPNT,IDIR)
                     ENDIF
                     boundary_walk: DO
                        IF (IDIR.EQ.1) THEN
                           IXNEW = IXOLD + 1
                           IYNEW = IYOLD
                        ELSE IF (IDIR.EQ.2) THEN
                           IXNEW = IXOLD
                           IYNEW = IYOLD + 1
                        ELSE IF (IDIR.EQ.3) THEN
                           IXNEW = IXOLD - 1
                           IYNEW = IYOLD
                        ELSE IF (IDIR.EQ.4) THEN
                           IXNEW = IXOLD
                           IYNEW = IYOLD - 1
                        ENDIF
                        IF (PVALID(IXNEW,IYNEW,KGRPNT) .AND.&
                        &VALIDBP(IXNEW,IYNEW,KGRPNT,WNP) ) THEN
                           IF (SCANND(KGRPNT(IXNEW,IYNEW)) .GE. 1) &
                              EXIT boundary_walk
                           NGRBND = NGRBND + 1
                           KGRBND(2*NGRBND-1) = IXNEW
                           KGRBND(2*NGRBND)   = IYNEW
                           SCANND(KGRPNT(IXNEW,IYNEW)) = 1
                           IXOLD = IXNEW
                           IYOLD = IYNEW
                           IDIR  = IDIR - 1
                           IF (WNP.EQ.3) THEN
                              CALL SEPARAREA(IXNEW, IYNEW, KGRPNT,IDIR)
                           ENDIF
                           IF (IDIR.EQ.0) IDIR = 4
                        ELSE
                           IDIR  = IDIR + 1
                           IF (.NOT. VALIDBP(IXNEW,IYNEW,KGRPNT,WNP).AND.&
                           &PVALID(IXNEW,IYNEW,KGRPNT) .AND.&
                           &NGRBGL.EQ.0 ) THEN
                              KGRPNT(IXNEW,IYNEW) = 1
                              MCGRD = MCGRD - 1
                              CALL MSGERR (1,&
                              &'Point removed from computational grid')
                              WRITE(PRINTF,"('Point with index (', I6 ,',', I6,') ', 'was removed from computational grid ', 'because it is an isolated wet point ', 'or is part of a 1D configuration or ', '1D connection.')") IXNEW, IYNEW
                              SCANND(KGRPNT(IXNEW,IYNEW)) = 1
                           END IF
                           IF (IDIR.EQ.5) IDIR = 1
                        ENDIF
                     ENDDO boundary_walk
!                 the following indicates that curve is closed
                     NGRBND = NGRBND + 1
                     KGRBND(2*NGRBND-1) = 0
                     KGRBND(2*NGRBND)   = 0
                  ENDIF
               ENDIF
            ENDIF
ENDDO
ENDDO
      IF (MCGRD.NE.MCGRDOLD) THEN
         MCGRDNEW = 1
         DO IXC = 1, MXC
            DO IYC = 1, MYC
               IF (KGRPNT(IXC,IYC).NE.1) THEN
                  MCGRDNEW = MCGRDNEW + 1
                  KGRPNTNEW(IXC,IYC) = MCGRDNEW
               ELSE
                  KGRPNTNEW(IXC,IYC) = 1
               END IF
            END DO
         END DO
         MCGRD = MCGRDNEW
         DO IXC = 1, MXC
            DO IYC = 1, MYC
               KGRPNT(IXC,IYC) = KGRPNTNEW(IXC,IYC)
            END DO
         END DO
      ENDIF
      DEALLOCATE(KGRPNTNEW,SCANND)
   END IF
   RETURN
end subroutine CGBOUN
!************************************************************************
!                                                                      *


!************************************************************************
!                                                                      *

!*******************************************************************
!                                                                  *
SUBROUTINE SEPARAREA(IX, IY, KGRPNT,IDIR)
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_grid_point_validation, ONLY: PVALID
!                                                                  *
!*******************************************************************

   USE SWCOMM3
   USE OCPCOMM4

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
!     40.04  Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     August 2000 new subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Separate areas that could be connected with a one cell connection
!
!  3. Method
!
!     Redirect original IDIR ( '<#' in plot below) to new direction('=>')
!
!     .    D    W -- W         X: point under consideration (wet)
!               |    |         W: wet point
!     W -- W <# X => W         D: dry point
!     |    |    |              .: either wet or dry point
!     W -- W    D    .
!
!     for all 8 different situations (4 rotation variations and their
!                                     mirrored situations)
!
!  4. Argument variables
!
!     IX, IY    input          x- and y-index of point under consideration
!     KGRPNT    input          indirect addresses for grid points
!     IDIR      input/output   index for direction (see subroutine CGBOUN)

   INTEGER IX, IY, KGRPNT(MXC,MYC), IDIR

!  5. Parameter variables
!
!  6. Local variables
!
!     IENT    number of entries of this subroutine

   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!     Function PVALID


!  9. Subroutines calling
!
!     CGBOUN
!
! 10. Error messages
!
! 11. Remarks
!
!     This subroutine is only used if number of Wet Neighbouring
!     Points WNP = 3
!
! 12. Structure
!
!     In this plot the first situation is illustrated
!
!     .    d    W -- W         X  : point under consideration (wet)
!               |    |         W  : wet point
!     W -- W <# X => W         D,d: dry point
!     |    |    |              .  : either wet or dry point
!     W -- W    D    .
!
!
!     IF dry neighbour point D is below X and upper left point is dry
!     redirect IDIR to 1
!     IF dry neighbour point D is right of X and lower left point is dry
!     redirect IDIR to 2
!     IF dry neighbour point D is above X and lower right point is dry
!     redirect IDIR to 3
!     IF dry neighbour point D is left of X and upper right point is dry
!     redirect IDIR to 4
!
!     mirrored situation
!
!     IF dry neighbour point D is above X and lower left point is dry
!     redirect IDIR to 4
!     IF dry neighbour point D is left of X and lower right point is dry
!     redirect IDIR to 1
!     IF dry neighbour point D is below X and upper right point is dry
!     redirect IDIR to 2
!     IF dry neighbour point D is right of X and upper left point is dry
!     redirect IDIR to 3
!
! 13. Source text
!
!************************************************************************

   CALL STRACE (IENT,'SEPARAREA')

!     In case there are 3 wet neighbouring points and validbp(ix,iy,.)

   IF(.NOT.PVALID(IX-1,IY+1,KGRPNT) .AND.&
   &.NOT.PVALID(IX,IY-1,KGRPNT)) IDIR = 1
   IF(.NOT.PVALID(IX-1,IY-1,KGRPNT) .AND.&
   &.NOT.PVALID(IX+1,IY,KGRPNT)) IDIR = 2
   IF(.NOT.PVALID(IX+1,IY-1,KGRPNT) .AND.&
   &.NOT.PVALID(IX,IY+1,KGRPNT)) IDIR = 3
   IF(.NOT.PVALID(IX+1,IY+1,KGRPNT) .AND.&
   &.NOT.PVALID(IX-1,IY,KGRPNT)) IDIR = 4

!     mirrored situation
   IF(.NOT.PVALID(IX-1,IY-1,KGRPNT) .AND.&
   &.NOT.PVALID(IX,IY+1,KGRPNT)) IDIR = 4
   IF(.NOT.PVALID(IX+1,IY-1,KGRPNT) .AND.&
   &.NOT.PVALID(IX-1,IY,KGRPNT)) IDIR = 1
   IF(.NOT.PVALID(IX+1,IY+1,KGRPNT) .AND.&
   &.NOT.PVALID(IX,IY-1,KGRPNT)) IDIR = 2
   IF(.NOT.PVALID(IX-1,IY+1,KGRPNT) .AND.&
   &.NOT.PVALID(IX+1,IY,KGRPNT)) IDIR = 3

   RETURN
end subroutine SEPARAREA

!*******************************************************************
!                                                                  *
SUBROUTINE INITVA( AC2, SPCSIG, SPCDIR, KGRPNT )
   USE swan_spectrum_transform, ONLY: SSHAPE, SINTRP, CHGBAS, GAMMAF
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, ININTG, INKEYW, INREAL, KEYWIS, EQCSTR
!                                                                  *
!*******************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE swan_time, ONLY: default_time_context
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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.82: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.03, 40.13: Nico Booij
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.62: Ben Payment (MSU), Tim Campbell (NRL)
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     30.70, Oct. 97: New subroutine
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description of SPCDIR
!     30.82, Dec. 98: Corrected the arguments in CALL SINTRP(..)
!     34.01, Feb. 99: Introducing STPNOW
!     40.00, Aug. 99: modification for 1D mode; new option INIT PAR
!                     init restart added
!     40.03, Nov. 99: after reading comment line, jump to 110 (not 100)
!                     additional test output added
!                     possibility added to initialize in limited region
!                     function EQCSTR used to compare strings
!     40.13, Jan. 01: option Spherical was not yet taken care of
!                     ! is now allowed as comment sign in a restart file
!     40.13, Oct. 01: error message removed, command MODE not required any more
!     40.31, Oct. 03: small changes
!     40.31, Dec. 03: appending number to file name i.c. of
!                     parallel computing
!     40.41, Sep. 04: small changes
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.62, Jul. 06: modified HOTSTART functionality to handle reading
!                     single hotfile or from multiple hotfiles when running in
!                     parallel with MPI
!     40.80, Jun. 07: extension to unstructured grids
!
!  2. Purpose
!
!     process command INIT and compute initial state of the wave field
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

   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)

!  8. Subroutines used


! 13. Source text


   INTEGER    KGRPNT(MXC,MYC)
   INTEGER JXMAX, JYMAX, NPTOT
   REAL       AC2(MDC,MSC,MCGRD)
   LOGICAL SINGLEHOT, PTNSUBGRD
      LOGICAL :: LERR
   CHARACTER(LEN=80) :: RLINE
   CHARACTER(LEN=16) :: RPROJID
   CHARACTER(LEN=4)  :: RPROJNR
   CHARACTER(LEN=20) :: RVERTXT, RCHTIME
   INTEGER IID, IUNITAC
   INTEGER, SAVE :: IENT = 0
   INTEGER :: ID, IINDX, IIOPT, ILPOS, INDX, IOPTG, IOSTAT
   INTEGER :: IP, IS, ISTAT, IX, IX1, IX2, IY, IY1, IY2
   INTEGER :: J, JX, JY, K, NQUA, NREF, NUMDIR, NUMFRE, NUMPTS
   REAL    ACTMP(MDC), DIRTMP(MDC), ACLOC(MDC,MSC)
   REAL    :: AFAC, AFAC1, FF, XX, YY

   CALL STRACE (IENT, 'INITVA')

!     ------------------------------------------------------------------
!
!     ***'initial conditions'  Definition of initial conditions  ***
!     *** for MODE DYNAMIC
!
! ============================================================
!
!               | -> DEFault
!               |
!     INITial  <   ZERO
!               |
!               |  PAR  [hs] [per] [dir] [dd]
!               |
!               |            | -> MULTiple |             | -> FREE
!               |  HOTStart <               >  'fname'  <
!               |            |    SINGle   |             | UNFormatted
!
! ============================================================

   CALL IGNORE ('COND')
   LERR =  .FALSE.
!     error message removed because MODE is not required any more,
!     and INIT can be useful also in stationary mode
   CALL INKEYW ('STA', 'DEF')
   IF (KEYWIS('PAR')) THEN
!       initial state defined by wave parameters
      IF (MXC.LE.0 .AND. OPTG.NE.5) THEN
         CALL MSGERR (2,&
         &'command INIT should follow CGRID')
         LERR = .TRUE.
      ENDIF
      IF (MCGRD .LE. 1 .AND. nverts.LE.0) THEN
         CALL MSGERR (2,&
         &'command INIT should follow READ BOT or READ UNSTRUC')
         LERR = .TRUE.
      ENDIF
      ICOND = 2
      CALL INREAL ('HSIG', SPPARM(1), 'REQ', 0.)
      CALL INKEYW ('STA', '    ')
      IF (KEYWIS('MEAN')) THEN
         IF (FSHAPE.GT.0) FSHAPE=-FSHAPE
      ELSE IF (KEYWIS('PEAK')) THEN
         IF (FSHAPE.LT.0) FSHAPE=-FSHAPE
      ENDIF
      CALL INREAL('PER', SPPARM(2), 'REQ', 0.)
      IF (SPPARM(2).LE.0.) CALL MSGERR (2, 'Period must be >0')
      IF (SPPARM(2).GT.PI2/SPCSIG(1)) THEN
         CALL MSGERR (2,&
         &'Inc. freq. lower than lowest spectral freq.')
         WRITE(PRINTF,"(' Inc. freq. = ',F9.5, A3, F9.5, '=',A7,' freq')") 1./SPPARM(2),' < ',SPCSIG(1)/PI2,&
         &' lowest'
      ENDIF
      IF (SPPARM(2).LT.PI2/SPCSIG(MSC)) THEN
         CALL MSGERR (2,&
         &'Inc. freq. higher than highest spectral freq.')
         WRITE(PRINTF,"(' Inc. freq. = ',F9.5, A3, F9.5, '=',A7,' freq')") 1./SPPARM(2),' > ',SPCSIG(MSC)/PI2,&
         &'highest'
      ENDIF
      CALL INREAL('DIR',  SPPARM(3), 'REQ', 0.)
      IF (DSHAPE.EQ.1) THEN
         CALL INREAL('DD', SPPARM(4), 'STA', 30.)
      ELSE
         CALL INREAL('DD', SPPARM(4), 'STA', 2.)
      ENDIF
!       give boundaries of region where the initial condition applies
      IF (OPTG.NE.5) THEN
         CALL ININTG ('IX1', IX1, 'STA', 0)
         CALL ININTG ('IX2', IX2, 'STA', MXC-1)
         CALL ININTG ('IY1', IY1, 'STA', 0)
         CALL ININTG ('IY2', IY2, 'STA', MYC-1)
      ENDIF
      IF (.NOT.LERR) THEN
         CALL SSHAPE (AC2(1,1,1), SPCSIG, SPCDIR, FSHAPE, DSHAPE)
!         copy computed spectrum to all internal grid points
         IF (OPTG.NE.5) THEN
!            --- structured grid
            DO IX = IX1+1, IX2+1
               DO IY = IY1+1, IY2+1
                  INDX = KGRPNT(IX,IY)
                  IF (INDX.GT.1)&
                  &CALL SINTRP (1., 0., AC2(1,1,1), AC2(1,1,1),&
                  &AC2(1,1,INDX),SPCDIR,SPCSIG)
               ENDDO
            ENDDO
!            reset action density AC2(*,*,1) to 0
            DO ID = 1, MDC
               DO IS = 1, MSC
                  AC2(ID,IS,1) = 0.
               ENDDO
            ENDDO
         ELSE
!            --- unstructured grid
            DO K = 2, nverts
               CALL SINTRP (1., 0., AC2(1,1,1), AC2(1,1,1),&
               &AC2(1,1,K),SPCDIR,SPCSIG)
            ENDDO
         ENDIF
      ENDIF
   ELSE IF (KEYWIS('ZERO')) THEN

!       zero initial state
!
!       the statements below work also for unstructured grids
!       since, MCGRD = nverts (see SwanInitCompGrid)
      DO INDX = 1, MCGRD
         DO ID = 1, MDC
            DO IS = 1, MSC
               AC2(ID,IS,INDX) = 0.
            ENDDO
         ENDDO
      ENDDO
      ICOND = 3
   ELSE IF (KEYWIS('HOTS') .OR. KEYWIS('REST')) THEN
      IID       = 0
      IUNITAC   = 0
      ACTMP     = 0.
      DIRTMP(:) = SPCDIR(:,1)
!       initialize using spectra from a HOTFILE
      IF (MXC.LE.0 .AND. OPTG.NE.5) CALL MSGERR (2,&
      &'command INIT should follow CGRID')
      IF (MCGRD.LE.1 .AND. nverts.LE.0) CALL MSGERR (2,&
      &'command INIT should follow READ BOT or READ UNSTRUC')
      ICOND = 4
      CALL INKEYW ('STA', 'MULT')
      IF (KEYWIS ('SING')) THEN
         SINGLEHOT = .TRUE.
         JXMAX = MXCGL
         JYMAX = MYCGL
         CALL IGNORE ('SING')
      ELSEIF (KEYWIS ('MULT')) THEN
         SINGLEHOT = .FALSE.
         JXMAX = MXC
         JYMAX = MYC
         CALL IGNORE ('MULT')
      END IF
      NPTOT = JXMAX*JYMAX
      IF (OPTG.EQ.5) NPTOT = nverts
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
!       --- append node number to FILENM in case of parallel computing
      IF ( PARLL .AND. .NOT.SINGLEHOT ) THEN
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      NREF   = 0
      IOSTAT = 0
      CALL INKEYW ('STA', 'FREE')
      IF (KEYWIS ('UNF') .OR. KEYWIS('BIN')) THEN
         CALL FOR (NREF, FILENM, 'OU', IOSTAT)
         IF (STPNOW()) RETURN
         READ (NREF) RVERTXT
         READ (NREF) RPROJID, RPROJNR
         READ (NREF) ISTAT
         IF (ISTAT.EQ.1) THEN
            READ (NREF) IIOPT
            IF (ITEST.GE.50) WRITE (PRTEST, "(' time coding option:', I2)") IIOPT
!          in stationary mode, warning
            IF (NSTATM.EQ.0) CALL MSGERR (1,&
            &'Time info in hotfile ignored')
         ELSE
            IIOPT = -1
            IF (NSTATM.EQ.1) CALL MSGERR (1,&
            &'No time info in hotfile')
         ENDIF
         READ (NREF) IOPTG
         IF (IOPTG.NE.OPTG) THEN
            CALL MSGERR (2,&
            &'grid on hotstart file differs from one in CGRID command')
            WRITE (PRINTF, "(1X, 'gridtype = ',I1, ' gridtype on file = ',I1)") OPTG, IOPTG
         ENDIF
         READ (NREF) NUMPTS
         IF (NUMPTS.NE.NPTOT) THEN
            CALL MSGERR (2,&
            &'grid on hotstart file differs from one in CGRID command')
            WRITE (PRINTF, "(1X, I8, ' points in comp.grid; on file:', I8)") NPTOT, NUMPTS
         ENDIF
         IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I8, ' output locations')") NUMPTS
         DO IP = 1, NUMPTS
            READ (NREF) XX, YY
         ENDDO
         READ (NREF) NUMFRE
         IF (NUMFRE.NE.MSC) CALL MSGERR (2,&
         &'grid on hotstart file differs from one in CGRID command')
         IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I6, ' frequencies')") NUMFRE
         DO IP = 1, NUMFRE
            READ (NREF) FF
         ENDDO
         READ (NREF) NUMDIR
         IF (NUMDIR.NE.MDC) CALL MSGERR (2,&
         &'grid on hotstart file differs from one in CGRID command')
         IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I6, ' directions')") NUMDIR
         DO IP = 1, NUMDIR
            READ (NREF) DIRTMP(IP)
         ENDDO
         IF (ABS(DIRTMP(1)*PI/180.-SPCDIR(1,1)).GT.0.5*DDIR)&
         &IID = NINT(REAL(MDC)*(DIRTMP(1)-ALPC)/360.)

!        reading of heading is completed, read time if nonstationary

         IF (IIOPT.GE.0) THEN
            READ (NREF) RCHTIME
            CALL DTRETI (RCHTIME, IIOPT, default_time_context%TIMCO)
            WRITE (PRINTF, "(' initial condition read for time: ', A)") RCHTIME
         ENDIF

         IF (OPTG.NE.5) THEN

!        --- structured grid

            DO JX = 1, JXMAX
               IF (SINGLEHOT) THEN
                  IX = JX-MXF+1
               ELSE
                  IX = JX
               ENDIF
               DO JY = 1, JYMAX
                  IF (SINGLEHOT) THEN
                     IY = JY-MYF+1
                  ELSE
                     IY = JY
                  ENDIF
                  PTNSUBGRD = .TRUE.
                  IF ( SINGLEHOT .AND.&
                  &(MXF.GT.JX .OR. MXL.LT.JX .OR.MYF.GT.JY .OR. MYL.LT.JY))&
                  &PTNSUBGRD = .FALSE.

                  IF (SINGLEHOT) THEN
                     IF (IAMMASTER) READ (NREF) IINDX
                     CALL SWBROADC (IINDX,1)
                  ELSE
                     READ (NREF) IINDX
                  ENDIF
                  INDX = KGRPNT(IX,IY)
                  IF (INDX.EQ.1 .AND. PTNSUBGRD) THEN
                     IF (IINDX.NE.1) THEN
                        CALL MSGERR (2,&
                        &'valid spectrum for non-existing grid point')
                        WRITE (PRINTF, *) IX-1, IY-1
                     ENDIF
                  ELSE
                     IF (IINDX.EQ.1) THEN
                        IF (PTNSUBGRD) THEN
                           DO IS = 1, MSC
                              DO ID = 1, MDC
                                 AC2(ID,IS,INDX) = 0.
                              ENDDO
                           ENDDO
                           IF (ITEST.GE.150) WRITE (PRTEST, "(' zero spectrum or no data for point:', 2I4)") IX-1, IY-1
                        ENDIF
                     ELSE
                        IF (SINGLEHOT) THEN
                           IF (IAMMASTER) READ (NREF) ACLOC(:,:)
                           CALL SWBROADC (ACLOC,MDC*MSC)
                           IF (PTNSUBGRD) AC2(:,:,INDX) = ACLOC(:,:)
                        ELSE
                           READ (NREF) AC2(:,:,INDX)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ELSE

!        --- unstructured grids

            DO K = 1, nverts
               READ (NREF) AC2(:,:,K)
            ENDDO
         ENDIF
      ELSE
         CALL IGNORE ('FREE')
         CALL FOR (NREF, FILENM, 'OF', IOSTAT)
         IF (STPNOW()) RETURN
         READ (NREF, "(A)") RLINE
         IF (RLINE(1:4).NE.'SWAN') CALL MSGERR (3, FILENM//&
         &' is not a correct hotstart file')
         DO
            READ (NREF, "(A)") RLINE
            IF (RLINE(1:1).NE.default_command_reader%COMID .AND. RLINE(1:1).NE.'!') EXIT
         END DO
         IF (EQCSTR(RLINE,'TIME')) THEN
            READ (NREF, *) IIOPT
            IF (ITEST.GE.50) WRITE (PRTEST, "(' time coding option:', I2)") IIOPT
            READ (NREF, "(A)") RLINE
!          in stationary mode, warning
            IF (NSTATM.EQ.0) CALL MSGERR (1,&
            &'Time info in hotfile ignored')
         ELSE
            IIOPT = -1
            IF (NSTATM.EQ.1) CALL MSGERR (1,&
            &'No time info in hotfile')
         ENDIF
         IF (EQCSTR(RLINE,'LOCA') .OR. EQCSTR(RLINE,'LONLAT')) THEN
            READ (NREF, *) NUMPTS
            IF (NUMPTS.NE.NPTOT) THEN
               CALL MSGERR (2,&
               &'grid on hotstart file differs from one in CGRID command')
               WRITE (PRINTF, "(1X, I8, ' points in comp.grid; on file:', I8)") NPTOT, NUMPTS
            ENDIF
            IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I8, ' output locations')") NUMPTS
            DO IP = 1, NUMPTS
               READ (NREF, *)
            ENDDO
            READ (NREF, "(A)") RLINE
         ENDIF
         IF (EQCSTR(RLINE(2:5),'FREQ')) THEN
            READ (NREF, *) NUMFRE
            IF (NUMFRE.NE.MSC) CALL MSGERR (2,&
            &'grid on hotstart file differs from one in CGRID command')
            IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I6, ' frequencies')") NUMFRE
            DO IP = 1, NUMFRE
               READ (NREF, *)
            ENDDO
            READ (NREF, "(A)") RLINE
         ENDIF
         IF (EQCSTR(RLINE(2:4),'DIR')) THEN
            READ (NREF, *) NUMDIR
            IF (NUMDIR.NE.MDC) CALL MSGERR (2,&
            &'grid on hotstart file differs from one in CGRID command')
            IF (ITEST.GE.50) WRITE (PRTEST, "(1X, I6, ' directions')") NUMDIR
            DO IP = 1, NUMDIR
               READ (NREF, *) DIRTMP(IP)
            ENDDO
            IF (ABS(DIRTMP(1)*PI/180.-SPCDIR(1,1)).GT.0.5*DDIR)&
            &IID = NINT(REAL(MDC)*(DIRTMP(1)-ALPC)/360.)
            READ (NREF, "(A)") RLINE
         ENDIF
         READ (NREF, *) NQUA
         IF (NQUA.NE.1) CALL MSGERR(2,'NQUA>1: incorrect hotstart file')
         READ (NREF, "(A)") RLINE
         IF (ITEST.GE.50) WRITE (PRTEST, "(1X, 'quantity: ', A)") RLINE
         READ (NREF, "(A)") RLINE
         IF (EQCSTR(RLINE(3:3),'S')) IUNITAC = 1
         READ (NREF, "(A)") RLINE

!        reading of heading is completed, read time if nonstationary

         IF (IIOPT.GE.0) THEN
            READ (NREF, "(A)") RLINE
            CALL DTRETI (RLINE(1:18), IIOPT, default_time_context%TIMCO)
            WRITE (PRINTF, "(' initial condition read for time: ', A)") RLINE(1:18)
         ENDIF

         IF (OPTG.NE.5) THEN

!        --- structured grid

            do JX = 1, JXMAX
               IF (SINGLEHOT) THEN
                  IX = JX-MXF+1
               ELSE
                  IX = JX
               ENDIF
               do JY = 1, JYMAX
                  IF (SINGLEHOT) THEN
                     IY = JY-MYF+1
                  ELSE
                     IY = JY
                  ENDIF
                  PTNSUBGRD = .TRUE.
                  IF ( SINGLEHOT .AND.&
                  &(MXF.GT.JX .OR. MXL.LT.JX .OR.MYF.GT.JY .OR. MYL.LT.JY))&
                  &PTNSUBGRD = .FALSE.

                  READ (NREF, "(A)") RLINE
                  INDX = KGRPNT(IX,IY)
                  IF (INDX.EQ.1 .AND. PTNSUBGRD) THEN
                     IF (RLINE(1:6).NE.'NODATA') THEN
                        CALL MSGERR (2,&
                        &'valid spectrum for non-existing grid point')
                        WRITE (PRINTF, *) IX-1, IY-1
                     ENDIF
                  ELSE
                     IF (EQCSTR(RLINE,'NODATA').OR.EQCSTR(RLINE,'ZERO')) THEN
                        IF (PTNSUBGRD) THEN
                           DO IS = 1, MSC
                              DO ID = 1, MDC
                                 AC2(ID,IS,INDX) = 0.
                              ENDDO
                           ENDDO
                           IF (ITEST.GE.150) WRITE (PRTEST, "(' zero spectrum or no data for point:', 2I4)") IX-1, IY-1
                        ENDIF
                     ELSE
!                 first determine factor
                        READ (NREF, *) AFAC
!                 multiply with factor to account for transition from
!                 energy/Hz/degr to energy/(2*pi rad/s)/rad
                        AFAC = AFAC * 90. / (PI**2)
                        DO IS = 1, MSC
                           AFAC1 = AFAC/SPCSIG(IS)
!                   Read spectral energy densities from file
                           READ (NREF, *) (ACTMP(ID), ID=1,MDC)
                           IF (.NOT.PTNSUBGRD) CYCLE
                           IF (IUNITAC.EQ.1) THEN
                              DO ID = 1, MDC
                                 J = MODULO ( IID - 1 + ID , MDC ) + 1
                                 AC2(J,IS,INDX) = AFAC * ACTMP(ID)
                              ENDDO
                           ELSE
                              DO ID = 1, MDC
                                 J = MODULO ( IID - 1 + ID , MDC ) + 1
                                 AC2(J,IS,INDX) = AFAC1 * ACTMP(ID)
                              ENDDO
                           END IF
                        ENDDO
                        IF (ITEST.GE.150) WRITE (PRTEST, "(' spectrum in point:', 2I4,' factor=', E12.4)") IX-1, IY-1, AFAC
                     ENDIF
                  ENDIF
               end do
            end do
         ELSE

!        --- unstructured grids

            DO K = 1, nverts
               READ (NREF, "(A)") RLINE
               IF (EQCSTR(RLINE,'NODATA') .OR. EQCSTR(RLINE,'ZERO')) THEN
                  DO IS = 1, MSC
                     DO ID = 1, MDC
                        AC2(ID,IS,K) = 0.
                     ENDDO
                  ENDDO
                  IF (ITEST.GE.150) WRITE (PRTEST, "(' zero spectrum or no data for vertex:', I6)") K
               ELSE
!                first determine factor
                  READ (NREF, *) AFAC
!                multiply with factor to account for transition from
!                energy/Hz/degr to energy/(rad/s)/rad
                  AFAC = AFAC * 90. / (PI**2)
                  DO IS = 1, MSC
                     AFAC1 = AFAC/SPCSIG(IS)
!                   Read spectral energy densities from file
                     READ (NREF, *) (ACTMP(ID), ID=1,MDC)
                     IF (IUNITAC.EQ.1) THEN
                        DO ID = 1, MDC
                           J = MOD ( IID - 1 + ID , MDC ) + 1
                           AC2(J,IS,K) = AFAC * ACTMP(ID)
                        ENDDO
                     ELSE
                        DO ID = 1, MDC
                           J = MOD ( IID - 1 + ID , MDC ) + 1
                           AC2(J,IS,K) = AFAC1 * ACTMP(ID)
                        ENDDO
                     END IF
                  ENDDO
                  IF (ITEST.GE.150) WRITE (PRTEST, "(' spectrum in vertex:', I6, ' factor=', E12.4)") K, AFAC
               ENDIF
            ENDDO
         ENDIF
      ENDIF
      CLOSE (NREF)
   ELSE
!       default initial wave state, will be computed later by subr SWINCO
      CALL IGNORE ('DEF')
      ICOND = 1
   ENDIF
   RETURN
!     end of subr INITVA
end subroutine INITVA
!*******************************************************************
!                                                                  *
SUBROUTINE BACKUP (AC2, SPCSIG, SPCDIR, KGRPNT,&
&XCGRID, YCGRID)
   USE swan_spectrum_output, ONLY: WRSPEC
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: EQREAL, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, INKEYW, KEYWIS
!                                                                  *
!*******************************************************************

   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
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
!     ver 40.00, Apr 1998 by N.Booij: new subroutine
!
!   Purpose
!
!  0. Authors
!
!     30.82: IJsbrand Haagsma
!     40.00, 40.13: Nico Booij
!     34.01: Jeroen Adema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.20: Casey Dietrich
!
!  1. Updates
!
!     40.00, Apr. 98: New subroutine
!     30.82, Oct. 98: Updated description of SPCDIR
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Nov. 99: ITMOPT is written as time coding option
!     40.13, Jan. 01: option Spherical was not yet taken care of
!     40.31, Dec. 03: appending number to file name i.c. of
!                     parallel computing
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Jun. 07: extension to unstructured grids
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!
!  2. Purpose
!
!     backup current state of the wave field to a file (specified in FILENM)
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

!  8. Subroutines used


! 13. Source text

   REAL     AC2(MDC,MSC,MCGRD)
   INTEGER  KGRPNT(MXC,MYC)
   INTEGER, SAVE :: IENT = 0
   INTEGER  ID, ILPOS, INDX, IOSTAT, IS, IX, IY, K, NREF
   CHARACTER (LEN=8) :: CRFORM = '(2F14.4)'
   CALL STRACE (IENT, 'BACKUP')

!     ==================================================================
!
!                       | -> FREE
!     HOTFile  'fname' <
!                       | UNFormatted
!
!     ==================================================================

   CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
!     --- append node number to FILENM in case of parallel computing
   IF ( PARLL ) THEN
      ILPOS = INDEX ( FILENM, ' ' )-1
      WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
   END IF
   NREF   = 0
   IOSTAT = 0
   CALL INKEYW ('STA', 'FREE')
   IF (KEYWIS ('UNF') .OR. KEYWIS('BIN')) THEN
      CALL FOR (NREF, FILENM, 'UU', IOSTAT)
      IF (STPNOW()) RETURN
      WRITE (NREF) VERTXT
      WRITE (NREF) PROJID, PROJNR
      WRITE (NREF) NSTATM
      IF (NSTATM.EQ.1) WRITE (NREF) ITMOPT
      WRITE (NREF) OPTG
      IF (OPTG.NE.5) THEN
         WRITE (NREF) MXC*MYC, MXC, MYC
         DO IX = 1, MXC
            DO IY = 1, MYC
               IF ( EQREAL(XCGRID(IX,IY), EXCFLD(8)) .AND.&
               &EQREAL(YCGRID(IX,IY), EXCFLD(9)) ) THEN
                  WRITE (NREF) EXCFLD(8)+XOFFS, EXCFLD(9)+YOFFS
               ELSE
                  WRITE (NREF) XCGRID(IX,IY)+XOFFS, YCGRID(IX,IY)+YOFFS
               ENDIF
            ENDDO
         ENDDO
      ELSE
         WRITE (NREF) nverts
         DO K = 1, nverts
            WRITE (NREF) xcugrd(K)+XOFFS, ycugrd(K)+YOFFS
         ENDDO
      ENDIF
      WRITE (NREF) MSC
      DO IS = 1, MSC
         WRITE (NREF) SPCSIG(IS)/PI2
      ENDDO
      WRITE (NREF) MDC
      DO ID = 1, MDC
         WRITE (NREF) SPCDIR(ID,1)*180./PI
      ENDDO

!      writing of heading is completed, write time if nonstationary

      IF (NSTATM.EQ.1) WRITE (NREF) CHTIME

      IF (OPTG.NE.5) THEN
         DO IX = 1, MXC
            DO IY = 1, MYC
               INDX = KGRPNT(IX,IY)
               WRITE (NREF) INDX
               IF (INDX.NE.1) WRITE (NREF) AC2(:,:,INDX)
            ENDDO
         ENDDO
      ELSE
         DO K = 1, nverts
            WRITE(NREF) AC2(:,:,K)
         ENDDO
      ENDIF
   ELSE
      CALL IGNORE ('FREE')
      CALL FOR (NREF, FILENM, 'UF', IOSTAT)
      IF (STPNOW()) RETURN
      WRITE (NREF, "(A, T41, A)") 'SWAN   1', 'Swan standard file, version'
      IF (NSTATM.EQ.1) THEN
         WRITE (NREF, "(A, T41, A)") 'TIME', 'time-dependent data'
         WRITE (NREF, "(I6, T41, A)") ITMOPT, 'time coding option'
      ENDIF
      IF (KSPHER.EQ.0) THEN
         WRITE (NREF, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
         CRFORM = '(2F14.4)'
      ELSE
         WRITE (NREF, "(A, T41, A)") 'LONLAT', 'locations on the globe'
         CRFORM = '(2F12.6)'
      ENDIF
      IF (OPTG.NE.5) THEN
         WRITE (NREF, "(I8, 2I6, T41, A)") MXC*MYC, MXC, MYC, 'number of locations'
         DO IX = 1, MXC
            DO IY = 1, MYC
               IF ( EQREAL(XCGRID(IX,IY), EXCFLD(8)) .AND.&
               &EQREAL(YCGRID(IX,IY), EXCFLD(9)) ) THEN
                  WRITE (NREF, FMT=CRFORM) DBLE(EXCFLD(8)) + DBLE(XOFFS),&
                  &DBLE(EXCFLD(9)) + DBLE(YOFFS)
               ELSE
                  WRITE (NREF, FMT=CRFORM) DBLE(XCGRID(IX,IY)) + DBLE(XOFFS),&
                  &DBLE(YCGRID(IX,IY)) + DBLE(YOFFS)
               ENDIF
            ENDDO
         ENDDO
      ELSE
         WRITE (NREF, "(I8, T41, A)") nverts, 'number of locations'
         DO K = 1, nverts
            WRITE (NREF, FMT=CRFORM) DBLE(xcugrd(K)) + DBLE(XOFFS),&
            &DBLE(ycugrd(K)) + DBLE(YOFFS)
         ENDDO
      ENDIF
      WRITE (NREF, "(A, T41, A)") 'RFREQ', 'relative frequencies in Hz'
      WRITE (NREF, "(I6, T41, A)") MSC, 'number of frequencies'
      do IS = 1, MSC
         WRITE (NREF, "(F10.4)") SPCSIG(IS)/PI2
      end do
      WRITE (NREF, "(A, T41, A)") 'CDIR', 'spectral Cartesian directions in degr'
      WRITE (NREF, "(I6, T41, A)") MDC, 'number of directions'
      do ID = 1, MDC
         WRITE (NREF, "(F10.4)") SPCDIR(ID,1)*180./PI
      end do
      WRITE (NREF, "('QUANT', /, I6, T41, 'number of quantities in table')") 1
      WRITE (NREF, "(A, T41, A)") 'AcDens', 'action densities'
      WRITE (NREF, "(A, T41, A)") 'm2s/Hz/deg', 'unit'
      WRITE (NREF, "(A, T41, A)") '0.',     'exception value'

!      writing of heading is completed, write time if nonstationary

      IF (NSTATM.EQ.1) THEN
         WRITE (NREF, "(A18, T41, 'date and time')") CHTIME
      ENDIF

      IF (OPTG.NE.5) THEN
         do IX = 1, MXC
            do IY = 1, MYC
               INDX = KGRPNT(IX,IY)
               IF (INDX.EQ.1) THEN
                  WRITE (NREF,"(A6)") 'NODATA'
               ELSE
                  CALL WRSPEC (NREF, AC2(1,1,INDX))
               ENDIF
            end do
         end do
      ELSE
         DO K = 1, nverts
            CALL WRSPEC (NREF, AC2(1,1,K))
         ENDDO
      ENDIF
   ENDIF
   CLOSE (NREF)
   RETURN
!     end of subr BACKUP
end subroutine BACKUP

end module swan_command_reading
