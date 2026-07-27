!     Last change:  NB   04 Jan 2001   12:00 pm
!
!          OCEAN PACK - Installation dependent subroutines
!
!*****************************************************************
!                                                                *

module swan_ocean_pack_init
   use swan_time, only: DTSTTI, DTTIST
   use swan_io_limits, only: LENFNM
   use swan_project_metadata, only: INST
   use swan_path_separators, only: DIRCH1, DIRCH2
   implicit none(type, external)
   private
!  OCPINI is the only entry point the driver needs; OCDTIM (processor-time
!  formatting) is used only from here.
!  DTSTTI and DTTIST stay external below on purpose: swan_time declares them in
!  its interface block, and they call UPCASE from swan_input_parser, which in
!  turn uses swan_time -- moving them into swan_time would create a cycle.
   public :: OCPINI
contains

SUBROUTINE OCPINI (INIFIL, LREAD, INERR)
   USE swan_input_parser, ONLY: RDINIT
   USE swan_parallel, ONLY: SWSYNC
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, TXPBLA
!                                                                *
!*****************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE swan_io_units
   USE swan_time
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
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     34.01: IJsbrand Haagsma
!     40.00, 40.03: Nico Booij
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.95: Marcel Zijlema
!
!  1. Updates
!
!     10.02, July 94: New argument INIFIL
!                     Check on validity period now uses OCDTIM
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.82, Nov. 98: Introduced recordlength of 1000 for file PRINT to
!                     avoid error-messages on the Cray-J90 and SGI Origin 200
!     34.01, Feb. 99: Changed STOP statements for MSGERR(4,'message')
!                     calls
!     34.01, Feb. 99: Opens a file 'screen' when unitnr in swaninit<>6
!     40.00, Feb. 99: Directory separation characters included in init file
!                     these characters are used in subr FOR
!     40.03, May  00: backslash replaced by CHAR(92) because of problems on Linux
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing HPGL-functionality
!     40.41, Sep. 04: includes speed processors in initialisation file
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.95, Jun. 08: parallelization of unSWAN
!
!  2. Purpose
!
!     subroutine initialises a number of common variables
!     opens standard input and output files, if necessary
!
!  4. Argument variables
!
!     INERR : output     Number of the initialisation error

   INTEGER :: INERR

!     INIFIL  inp  char  name of initialisation file
!     LREAD   inp  log   if True: command input file must be opened
!                        and command reading must be initialised

   LOGICAL :: LREAD, FILEXI
   CHARACTER(LEN=40) :: INPFIL, OUTFIL, TSTFIL
   CHARACTER(LEN=*) :: INIFIL
   CHARACTER(LEN=4) :: PLTOPT
   CHARACTER(LEN=24) :: TIMSTR
   CHARACTER(LEN=40) :: INPFO, OUTFO, TSTFO
   CHARACTER(LEN=120) :: TXT
   CHARACTER(LEN=LENFNM) :: SCRFIL
   INTEGER, SAVE :: PRCTIM(6) = [0, 0, 0, 0, 0, 0]
   INTEGER :: IF, II, IL, ILPOS, INIVER, INIVEF, IPOS, IW, J, JJ, K, NPLP, IOSTAT
   INTEGER :: PFROPT
   REAL :: PLPARM(10)
   INTEGER :: NUMM(10)

!     version of initialisation file
   INIVER = 4
   INIVEF = -1
   INERR  = 0

   IF (PARLL) THEN
      IF (.NOT.ALLOCATED(IWEIG)) ALLOCATE(IWEIG(NPROC))
      IWEIG = 100
   END IF

!     see whether initialisation file exists

   INQUIRE (FILE=INIFIL, EXIST=FILEXI)
   IF (FILEXI) THEN

!       read initialisation file

      OPEN (11, FILE=INIFIL, STATUS='OLD', IOSTAT=IOSTAT)
!CVIS      &SHARED,&
      IF (initialisation_open_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) INIVEF
      IF (initialisation_read_failed(IOSTAT)) RETURN
      IF (INIVEF.GT.INIVER .OR. INIVEF.LE.0) THEN
         INERR=935
         IF ( IAMMASTER ) WRITE(*,*) 'Incorrect version of initialisation file '
         RETURN
      END IF
      READ (11, "(A40)", IOSTAT=IOSTAT) INST
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) INPUTF
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, "(A40)", IOSTAT=IOSTAT) INPFIL
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) PRINTF
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, "(A40)", IOSTAT=IOSTAT) OUTFIL
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) PRTEST
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, "(A40)", IOSTAT=IOSTAT) TSTFIL
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) SCREEN
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, *, IOSTAT=IOSTAT) IUNMAX
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, "(A1)", IOSTAT=IOSTAT) default_command_reader%COMID
      IF (initialisation_read_failed(IOSTAT)) RETURN
      READ (11, "(A1)", IOSTAT=IOSTAT) default_command_reader%TABC
      IF (initialisation_read_failed(IOSTAT)) RETURN
      IF (INIVEF.GE.2) THEN
         READ (11, "(A1)", IOSTAT=IOSTAT) DIRCH1
         IF (initialisation_read_failed(IOSTAT)) RETURN
         READ (11, "(A1)", IOSTAT=IOSTAT) DIRCH2
         IF (initialisation_read_failed(IOSTAT)) RETURN
      ELSE
!DOS         DIRCH1 = CHAR(47)
!DOS         DIRCH2 = CHAR(92)
!UNIX         DIRCH1 = CHAR(92)
!UNIX         DIRCH2 = CHAR(47)
      ENDIF
      IF (INIVEF.LT.3) THEN
         READ (11, "(A4)", IOSTAT=IOSTAT) PLTOPT
         IF (initialisation_read_failed(IOSTAT)) RETURN
         READ (11, *, IOSTAT=IOSTAT) NPLP
         IF (initialisation_read_failed(IOSTAT)) RETURN
         READ (11, *, IOSTAT=IOSTAT) (PLPARM(II),II=1,NPLP)
         IF (initialisation_read_failed(IOSTAT)) RETURN
         READ (11, *, IOSTAT=IOSTAT) PFROPT
         IF (initialisation_read_failed(IOSTAT)) RETURN
      END IF
      READ (11, *, IOSTAT=IOSTAT) ITMOPT
      IF (initialisation_read_failed(IOSTAT)) RETURN
      IF (INIVEF.GT.3) THEN
         IF (PARLL) THEN
            DO JJ = 1, NPROC
               READ (11, "(I5,A)", IOSTAT=IOSTAT) IW, TXT
               IF (IOSTAT.NE.0) EXIT
               CALL TXPBLA(TXT,IF,IL)
               IPOS = 0
               DO II = IF, IL
                  K = ICHAR(TXT(II:II))
                  IF ( K.GE.48 .AND. K.LE.57 ) THEN
                     IPOS = IPOS + 1
                     NUMM(IPOS) = K - 48
                  END IF
               END DO
               J = 0
               DO II = 1, IPOS
                  J = J + NUMM(II)*10**(IPOS-II)
               END DO
               IWEIG(J) = IW
            END DO
         END IF
      END IF
      CLOSE (11)
   ELSE

!       REFERENCE NUMBERS AND NAMES OF STANDARD FILES

      INPUTF = 3
      INPFIL = 'INPUT'
      PRINTF = 4
      OUTFIL = 'PRINT'
!       unit ref. numbers for output to screen and to separate
!       test print file:
      PRTEST = PRINTF
      TSTFIL = '    '
      SCREEN = 6
      IUNMAX = 99999
!/SGI      IUNMAX = 199
!       TABC is the Tab character (interpreted as blank in command reading)
      default_command_reader%TABC = CHAR(9)
!       COMID is the comment identifier (usually $)
      default_command_reader%COMID  = '$'
!       DIRCH1 is directory separation character as appears in input file
!       DIRCH2 is directory separation character replacing DIRCH1
!DOS      DIRCH1 =  CHAR(47)
!DOS      DIRCH2 =  CHAR(92)
!UNIX      DIRCH1 =  CHAR(92)
!UNIX      DIRCH2 =  CHAR(47)
!       INST = name of institute, max. 40 characters
      INST = 'Delft University of Technology'
      ITMOPT = 1

   ENDIF
   INPFO = INPFIL
   OUTFO = OUTFIL
   TSTFO = TSTFIL

!     --- append node number to OUTFIL and TSTFIL
!         in case of parallel computing

   IF (PARLL) THEN
      ILPOS = INDEX ( OUTFIL, ' ' )-1
      IF (ILPOS.GT.0) THEN
         WRITE(OUTFIL(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      ELSE
         INERR=920
         IF ( IAMMASTER ) WRITE(*,*) 'Cannot open PRINT file '
         RETURN
      END IF
      ILPOS = INDEX ( TSTFIL, ' ' )-1
      IF (ILPOS.GT.0) THEN
         WRITE(TSTFIL(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      CALL SWSYNC
      IF (STPNOW()) RETURN
   END IF

   IF (INIVEF.LT.INIVER) THEN

!       write initialisation file

      IF ( IAMMASTER ) THEN
         OPEN  (12, FILE=INIFIL, STATUS='UNKNOWN', FORM='FORMATTED', IOSTAT=IOSTAT)
         IF (initialisation_open_failed(IOSTAT)) RETURN
         WRITE (12, "(I5, T41, A)") INIVER, 'version of initialisation file'
         WRITE (12, "(A40, A)") INST,   'name of institute'
         WRITE (12, "(I5, T41, A)") INPUTF, 'command file ref. number'
         WRITE (12, "(A40, A)") INPFO,  'command file name'
         WRITE (12, "(I5, T41, A)") PRINTF, 'print file ref. number'
         WRITE (12, "(A40, A)") OUTFO,  'print file name'
         WRITE (12, "(I5, T41, A)") PRTEST, 'test file ref. number'
         WRITE (12, "(A40, A)") TSTFO,  'test file name'
         WRITE (12, "(I5, T41, A)") SCREEN, 'screen ref. number'
         WRITE (12, "(I5, T41, A)") IUNMAX, 'highest file ref. number'
         WRITE (12, "(A1, T41, A)") default_command_reader%COMID,  'comment identifier'
         WRITE (12, "(A1, T41, A)") default_command_reader%TABC,   'TAB character'
         WRITE (12, "(A1, T41, A)") DIRCH1, 'dir sep char in input file'
         WRITE (12, "(A1, T41, A)") DIRCH2, 'dir sep char replacing previous one'
         WRITE (12, "(I5, T41, A)") ITMOPT, 'default time coding option'
         IF (PARLL) THEN
            DO II = 1, NPROC
               WRITE (12, "(I5, T41, A19, I3)") IWEIG(II), 'speed of processor ',II
            END DO
         END IF
         CLOSE (12)
      ENDIF
   ENDIF

   IUNMIN = 0
   FUNLO = 21
!/SGI   FUNLO = 103
   FUNHI = IUNMAX

   CALL OCDTIM (PRCTIM)

!     initialise command reader

   IF (OUTFIL.NE.'    ') THEN
!       WRITE (*,*) ' Open print file ', PRINTF, OUTFIL
      OPEN (UNIT=PRINTF, FILE=OUTFIL, STATUS='UNKNOWN',&
      &FORM='FORMATTED', IOSTAT=IOSTAT)
!/Cray      &RECL=2000,&
!/SGI      &RECL=2000,&
      IF (IOSTAT.NE.0) THEN
         INERR = 920
         IF (IAMMASTER) WRITE(*,*) 'Cannot open PRINT file '
         RETURN
      ENDIF
!       WRITE (*,*) ' Print file opened ', PRINTF, OUTFIL
      CALL DTTIST (ITMOPT, TIMSTR, PRCTIM)
      WRITE (PRINTF, "('1',//,20X, 'Execution started at ',A, //)") TIMSTR
   ENDIF
   IF (PRTEST.NE.PRINTF) THEN
      OPEN (UNIT=PRTEST, FILE=TSTFIL, IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
         CALL MSGERR(4,'Cannot open test file: '//TSTFIL)
         RETURN
      ENDIF
   ENDIF
   IF (SCREEN.NE.6) THEN
      SCRFIL = 'screen'
      OPEN (UNIT=SCREEN, FILE=SCRFIL, IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
         CALL MSGERR(4,'Error opening output file: screen')
         RETURN
      ENDIF
   ENDIF
   IF (LREAD) THEN
      IF (INPFIL.NE.'    ') THEN
         OPEN (UNIT=INPUTF, FILE=INPFIL, STATUS='OLD', IOSTAT=IOSTAT)
!CVIS         &SHARED,&
         IF (IOSTAT.NE.0) THEN
            CALL MSGERR(4,'Input file missing')
            RETURN
         ENDIF
      ENDIF
      CALL RDINIT
   ENDIF

   RETURN

CONTAINS

   LOGICAL FUNCTION initialisation_read_failed(status)
      INTEGER, INTENT(IN) :: status

      initialisation_read_failed = status.NE.0
      IF (.NOT.initialisation_read_failed) RETURN
      INERR = 930
      IF (IAMMASTER) WRITE(*,*) 'Error reading initialisation file '
   END FUNCTION initialisation_read_failed

   LOGICAL FUNCTION initialisation_open_failed(status)
      INTEGER, INTENT(IN) :: status

      initialisation_open_failed = status.NE.0
      IF (.NOT.initialisation_open_failed) RETURN
      INERR = 950
      IF (IAMMASTER) WRITE(*,*) 'Error opening initialisation file '
   END FUNCTION initialisation_open_failed

end subroutine OCPINI
!*****************************************************************
!                                                                *
SUBROUTINE OCDTIM (PRCTIM)
   IMPLICIT NONE(TYPE, EXTERNAL)
!                                                                *
!*****************************************************************
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
!     30.07
!     30.70: Nico Booij
!     30.82: IJsbrand Haagsma
!     40.02: IJsbrand Haagsma
!
!  1. Updates
!
!     30.07, Oct. 95: option DEC added
!     30.70, Sep. 97: adaptation in view of year 2000
!     30.82, Mar. 99: Adapted to Fortran 90 standard
!     40.02, Sep. 00: Removed all platform dependent Fortran 77 statements
!
!  2. PURPOSE
!
!       get time of processing, using processor dependent routines
!
!  3. PARAMETER LIST
!
!       PRCTIM  outp   int   time array: elements: year, month, day,
!                            hour, minute, second
!
!  4. SUBROUTINES USED
!
!       GETDAT, GETTIM or other
!
!  5. ERROR MESSAGES
!
!       ----
!
!  6. REMARKS
!
!       This function uses a processor dependent subroutines (GETDAT,
!       GETTIM), therefore adaptations are necessary when compiled
!       at a different computer system environment.
!
!  7. STRUCTURE
!
!       ---------------------------------------------------------
!       Call DATE and TIME routines (system dependent)
!       decode YEAR, MONTH and DAY
!       assemble DATE string
!       decode HOUR, MINUTE, SECOND
!       assemble TIME string
!       ---------------------------------------------------------
!
!  8. SOURCE TEXT

   INTEGER :: PRCTIM(6)

!     Call DATE and TIME routines
!
!     --------Fortran 90 date-time routines --------

   CHARACTER(LEN=24) :: TIMSTR
   CHARACTER(LEN=5) :: CDUMMY
   INTEGER :: IDUMMY(8)

   CALL DATE_AND_TIME (TIMSTR(1:8), TIMSTR(10:20), CDUMMY, IDUMMY)
   CALL DTSTTI (1, TIMSTR, PRCTIM)

   RETURN

end subroutine OCDTIM

end module swan_ocean_pack_init
