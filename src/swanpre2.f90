
!     SWAN/SWREAD  file 2 of 2
!
!  Contents of this file:
!     SPROUT: Reading and processing of the user output commands
!     SWREPS: Reading and processing of the commands defining output points
!     SWREOQ: Reading and processing of the output requests
!     SIRAY : Searching the first point on a ray where the depth is DP
!     SWNMPS
!     SVARTP
!     SWBOUN
!     BCFILE
!     BCWAMN
!     BCWW3N
!     SWBCPT
!     RETSTP
!
!************************************************************************

module swan_input_processing
   use swan_bnd_struc, only: SwanBndStruc
   use swan_bpntlist, only: SwanBpntlist
   use swan_find_point, only: SwanFindPoint
   use swan_pointin_mesh, only: SwanPointinMesh
   implicit none(type, external)
   private
   public :: SPROUT, SVARTP, SWBOUN, RETSTP
contains

!                                                                      *
SUBROUTINE SPROUT (FOUND, BOTLEV, WATLEV)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, STPNOW
   USE swan_input_parser, ONLY: KEYWIS
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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     32.02: Roeland Ris & Cor van der Schelde (1D version)
!     34.01: Jeroen Adema
!     40.02: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!    100.04, Nov. 92: Filename of plotfile will be given by user
!     30.70, Nov. 97: Arguments BOTLEV and WATLEV added
!     32.02, Feb. 98: 1D version introduced
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Apr. 98: Removed reference to commons KAART and KAR
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.81, Nov. 98: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     30.81, Jan. 99: Replaced variable FROM by FROM_ (because FROM is a
!                     reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Sep. 00: inconsistency with manual corrected
!     40.02, Oct. 00: Initialisation of IERR
!     40.31, Nov. 03: removing POOL construction and HPGL functionality
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Reading and processing of the user output commands
!
!  3. Method
!
!     If the first characters of the last read command are equal to a
!     given string (KEYWIS ('STRING')), the keywords and varia-
!     bles of this command are further read and processed
!
!  4. Argument variables
!
! i   BOTLEV: Bottom levels
! i   WATLEV: Water levels

   REAL    BOTLEV(*)
   REAL    WATLEV(*)

!  6. Local variables
!
!  8. Subroutines used
!
!     MSGERR
!     SWREPS
!     SWREOQ
!     STPNOW


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
!             Most of the source code will be clear with the
!             aid of the user manual, the system documentation
!             and the additional comments in the source code.
!     ----------------------------------------------------------------
!
! 13. Source text

   LOGICAL   FOUND
   INTEGER, SAVE :: IENT = 0
   CALL STRACE (IENT,'SPROUT')

   FOUND = .FALSE.

!     definition of output point sets

   CALL SWREPS ( FOUND, BOTLEV, WATLEV )
   IF (STPNOW()) RETURN
   IF (FOUND) RETURN

!     output requests

   CALL SWREOQ ( FOUND )
   IF (STPNOW()) RETURN
   IF (FOUND) RETURN

   IF (KEYWIS('SIT') .OR. KEYWIS('PLA')) THEN
      CALL MSGERR(2,'Keyword SITES is no longer maintained')
      FOUND = .TRUE.
      RETURN
   ENDIF

   IF (KEYWIS ('LIN')) THEN
      CALL MSGERR(2,'Keyword LINE is no longer maintained')
      FOUND = .TRUE.
      RETURN
   ENDIF
!     ------------------------------------------------------------------
!     ***** command name not found *****
   RETURN

FOUND = .TRUE.
   RETURN
! *   end of subroutine SPROUT *
end subroutine SPROUT
!************************************************************************
!                                                                      *
SUBROUTINE SWREPS ( FOUND, BOTLEV, WATLEV )
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, ININTG, INKEYW, INREAL, KEYWIS, NWLINE
!                                                                      *
!************************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
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
!     30.70, 40.03, 40.13: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     32.02: Roeland Ris & Cor van der Schelde (1D version)
!     34.01: Jeroen Adema
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.70, Nov. 97: comm ISO, inquire pointer added to get correct value
!                     for IADRAY
!     30.70, Nov. 97: comm ISO, offset origin added in message concerning rays
!                     declaration INT SIRAY added
!     30.70, Nov. 97: arguments BOTLEV and WATLEV added
!     30.72, Feb. 98: Declaration of Argument variables updated
!     32.02, Feb. 98: 1D version introduced
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Apr. 98: removed reference to commons KAART and KAR
!     30.81, Nov. 98: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.01, Sep. 99: XASM and YASM replace fixed numbers
!     33.09, Sep. 00: modifications in view of spherical coordinates
!     40.03, Sep. 00: inconsistency with manual corrected
!     40.13, Sep. 01: nesting in curvilinear grid: division by 0 prevented
!     40.30, May  03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Mar. 08: extension to unstructured grids
!
!  2. PURPOSE
!
!     Reading and processing of the commands defining output points
!
!  4. Argument variables (updated 30.72)
!
!     BOTLEV: input  bottom levels
!     WATLEV: input  water levels

   REAL      BOTLEV(*), WATLEV(*)

!     FOUND : output  parameter indicating whether command
!                     being processed is found (value True)
!                     or not (False)

   LOGICAL   FOUND

!     Local variables

   REAL :: XPFR, YPFR, XLENFR, YLENFR

   TYPE(OPSDAT), POINTER :: OPSTMP, ROPS

   TYPE XYPT
      REAL                :: X, Y, XQ, YQ
      TYPE(XYPT), POINTER :: NEXTXY
   end type XYPT

   TYPE(XYPT), TARGET  :: FRST
   TYPE(XYPT), POINTER :: CURR, TMP

   INTEGER, ALLOCATABLE :: VM(:)
   REAL, ALLOCATABLE :: XG(:), YG(:)
   CHARACTER (LEN=80) :: BASENM

!  8. Subroutines used
!
!     command reading routines
!     (all Ocean Pack)


!  9. Subroutines calling
!
!     SPROUT
!
! 10. Error messages
!
!     ---
!
! 13. Source text

   LOGICAL   PP
   INTEGER, SAVE :: IENT = 0
   INTEGER   I, II, IK, INTD, INTE, INTV, IOSTAT, ISTAT
   INTEGER   IERR, IX1, IX2, IY1, IY2, JJ, KK, MIP, MIPR
   INTEGER   MXK, MXN, MYK, MYN, NATTR, NBMARK, NDIM, NDS, NVTX
!  SIRAY is now a module procedure; the old local INTEGER declaration would
!  turn the reference back into an external function and break linking.
   REAL      ALON, ALPCN, ALPK, ALTNP, ANG, ANGLE, COSA2, DP
   REAL      DXN, DYN, RDUM, SINA2, XF, XI, XNLEN, XP, XP1, XPCN
   REAL      XQ, XQ1, XX, YF, YI, YNLEN, YP, YP1, YPCN, YQ, YQ1, YY
   CHARACTER(LEN=16) :: PSNAME, PRNAME
   CHARACTER(LEN=1)  :: STYPE
      LOGICAL :: BOTDEP
   CALL STRACE (IENT,'SWREPS')

!   --------------------------------------------------------------------
!   FRAME   'sname'  [xpfr] [ypfr] [alpfr] [xlenfr] [ylenfr]          &
!           [mxfr] [myfr]
!   --------------------------------------------------------------------

   IF (KEYWIS ('FRA')) THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (FRA) in combination with'//&
         &' 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
!         ver 30.20: names of input variables changed, order of data changed
         ALLOCATE(OPSTMP)
         CALL INCSTR ('SNAME',PSNAME,'REQ',' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
         OPSTMP%PSNAME = PSNAME
         CALL READXY ('XPFR', 'YPFR', XPFR, YPFR, 'REQ', 0., 0.)
         OPSTMP%OPR(1) = XPFR
         OPSTMP%OPR(2) = YPFR
         CALL INREAL('ALPFR',ALPK,'REQ',0.)
         IF (KSPHER.GT.0 .AND. .NOT.EQREAL(ALPK,0.)) CALL MSGERR (2,&
         &'[alpfr] must be 0 with spherical coordinates')
         CALL INREAL('XLENFR', XLENFR,'REQ',0.)
         CALL INREAL('YLENFR', YLENFR,'REQ',0.)
         OPSTMP%OPR(3) = XLENFR
         OPSTMP%OPR(4) = YLENFR
         OPSTMP%OPR(5) = PI2 * (ALPK/360.-NINT(ALPK/360.))
!           ***** the user gives number of meshes along each side *****
!           ***** program uses the number of points               *****
         CALL ININTG ('MXFR',MXK,'STA',20)
         CALL ININTG ('MYFR',MYK,'STA',20)
         OPSTMP%PSTYPE = 'F'
         OPSTMP%OPI(1) = MXK+1
         OPSTMP%OPI(2) = MYK+1
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
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF

!   ------------------------------------------------------------------
!   GROUP   'sname'  SUBGRID [ix1] [ix2] [iy1] [iy2]
!   ------------------------------------------------------------------

   IF (KEYWIS('GROUP') .OR. KEYWIS ('SUBG')) THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (GROUP) in combination'//&
         &' with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSEIF (OPTG.EQ.5) THEN
         CALL MSGERR(2,&
         &' Keyword GROUP not supported in unstructured grid')
         FOUND = .TRUE.
         RETURN
      ELSE
!         mod 970221: GROUP is introduced as a new command instead of
!         an option SUBG within the Frame command
         ALLOCATE(OPSTMP)
         CALL INCSTR ('SNAME',PSNAME,'REQ',' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
         OPSTMP%PSNAME = PSNAME
         CALL INKEYW ('STA', ' ')
         CALL IGNORE ('SUBG')
         CALL ININTG ('IX1', IX1, 'REQ', 0)
         CALL ININTG ('IX2', IX2, 'REQ', 0)
         CALL ININTG ('IY1', IY1, 'REQ', 0)
         CALL ININTG ('IY2', IY2, 'REQ', 0)
         IF (IX1 .LT. 0 .OR. IX2 .GT. MXCGL-1 .OR. IX1 .GT. IX2 .OR.&
         &IY1 .LT. 0 .OR. IY2 .GT. MYCGL-1 .OR. IY1 .GT. IY2) THEN
            CALL MSGERR (3, 'Check corners of GROUP (SUBGRID) command')
            CALL MSGERR (3, ' .........the values should be.........')
            CALL MSGERR (3, 'ix1<ix2 and both between 0 and MXC')
            CALL MSGERR (3, 'iy1<iy2 and both between 0 and MYC')
         ENDIF

         IF (OPTG .EQ. 3) THEN
!             *** If the comput grid is curvilinear then the next    ***
!             *** quantities are stored : 'H' ,FLOAT(IX2), FLOAT(IY2)***
!             *** FLOAT(IX1) ,FLOAT(IY1) , 0 ,MXK+1 ,MYK+1           ***
!              *** Here frame type H is introduce, means that regular***
!              *** frame is required from a curvilinear compt. grid  ***
            OPSTMP%PSTYPE = 'H'
            MXK = IX2-IX1
            MYK = IY2-IY1
            OPSTMP%OPR(1) = FLOAT(IX2)
            OPSTMP%OPR(2) = FLOAT(IY2)
            OPSTMP%OPR(3) = FLOAT(IX1)
            OPSTMP%OPR(4) = FLOAT(IY1)
            OPSTMP%OPR(5) = 0.
            OPSTMP%OPI(1) = MXK+1
            OPSTMP%OPI(2) = MYK+1
         ELSE IF (OPTG .EQ. 1) THEN
            OPSTMP%PSTYPE = 'F'
            IF (IX1.NE.IX2) THEN
               OPSTMP%OPR(3) = (IX2-IX1)*DX
            ELSE
               OPSTMP%OPR(3) = 0.01
            ENDIF
            IF (IY1.NE.IY2) THEN
               OPSTMP%OPR(4) = (IY2-IY1)*DY
            ELSE
               OPSTMP%OPR(4) = 0.01
            ENDIF
            OPSTMP%OPR(1) = XPC + IX1*DX*COSPC - IY1*DY*SINPC
            OPSTMP%OPR(2) = YPC + IX1*DX*SINPC + IY1*DY*COSPC
            OPSTMP%OPR(5) = ALPC
            MXK = IX2-IX1
            MYK = IY2-IY1
            OPSTMP%OPI(1) = MXK+1
            OPSTMP%OPI(2) = MYK+1
            IF (ITEST .GE. 20 .OR. INTES .GE. 10)&
            &WRITE (PRINTF, "(' Subgrid parms.', 5(1X,E12.4))") (OPSTMP%OPR(II), II=1,5)
         ENDIF
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
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF

!   ------------------------------------------------------------------
!   CURVE   'sname'  [xp1] [yp1]   < [int]  [xp]  [yp] >
!   ------------------------------------------------------------------


   IF (KEYWIS ('CURV')) THEN
      ALLOCATE(OPSTMP)
      CALL INCSTR('SNAME',PSNAME,'REQ',' ')
      IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
      OPSTMP%PSNAME = PSNAME
      OPSTMP%PSTYPE = 'C'
      MIP  = 0
      OPSTMP%MIP = MIP
!       ***** first point of a curve *****
CALL NWLINE
      IF (STPNOW()) RETURN
      CALL READXY ('XP1', 'YP1', XP, YP, 'REQ', 0., 0.)
      FRST%X = XP
      FRST%Y = YP
      NULLIFY(FRST%NEXTXY)
      CURR => FRST
      MIP = 1
!       ***** interval and next corner point *****
      DO
         CALL ININTG ('INT',INTV,'REP',-1)
         IF (INTV .EQ. -1) EXIT
         IF (INTV .LE. 0) THEN
            CALL MSGERR (2,'INT is negative or zero')
            INTV = 1
         ENDIF
         XP1 = XP
         YP1 = YP
         CALL READXY ('XP', 'YP', XP, YP, 'REQ', 0., 0.)
         IF (ITEST .GE. 200 .OR. INTES .GE. 20) THEN
            WRITE(PRINTF, "('COORDINATES OF OUTPUT POINTS FOR CURVE : ', A)") PSNAME
         ENDIF
         do JJ=1,INTV
            MIP = MIP+1
            ALLOCATE(TMP)
            TMP%X = XP1+REAL(JJ)*(XP-XP1)/REAL(INTV)
            TMP%Y = YP1+REAL(JJ)*(YP-YP1)/REAL(INTV)
            NULLIFY(TMP%NEXTXY)
            CURR%NEXTXY => TMP
            CURR => TMP
         end do
      END DO
      ALLOCATE(OPSTMP%XP(MIP))
      ALLOCATE(OPSTMP%YP(MIP))
      CURR => FRST
      DO JJ = 1, MIP
         OPSTMP%XP(JJ) = CURR%X
         OPSTMP%YP(JJ) = CURR%Y
         IF (ITEST .GE. 200 .OR. INTES .GE. 50) THEN
            WRITE(PRINTF,"(' POINT(',I4,')',' (IX,IY) -> ',2F10.2)") JJ, CURR%X, CURR%Y
         ENDIF
         CURR => CURR%NEXTXY
      END DO
      DEALLOCATE(TMP)
!       ***** store number of points of the curve *****
      OPSTMP%MIP = MIP
      IF (MIP .EQ. 0) CALL MSGERR(1,'No output points found')
      NULLIFY(OPSTMP%NEXTOPS)
      IF ( .NOT.LOPS ) THEN
         FOPS = OPSTMP
         COPS => FOPS
         LOPS = .TRUE.
      ELSE
         COPS%NEXTOPS => OPSTMP
         COPS => OPSTMP
      END IF
      FOUND = .TRUE.
      RETURN
   ENDIF

!   ------------------------------------------------------------------
!   POINTS  'sname'  < [xp]  [yp]  >     |    FILE 'fname'
!   ------------------------------------------------------------------

   IF (KEYWIS ('POIN')) THEN
      ALLOCATE(OPSTMP)
      CALL INCSTR('SNAME',PSNAME,'REQ',' ')
      IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
      OPSTMP%PSNAME = PSNAME
      OPSTMP%PSTYPE = 'P'
      MIP  = 0
      OPSTMP%MIP = MIP
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('FILE')) THEN
         IOSTAT = 0
         NDS    = 0
         PP     = .TRUE.
         CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
         CALL FOR (NDS, FILENM, 'OF', IOSTAT)
         IF (STPNOW()) RETURN
      ELSE
         PP = .FALSE.
      ENDIF
      FRST%X = 0.
      FRST%Y = 0.
      NULLIFY(FRST%NEXTXY)
      CURR => FRST
      DO
         IF (PP) THEN
            IERR = 0
            CALL REFIXY (NDS, XP, YP, IERR)
            IF (IERR.EQ.-1) EXIT
            IF (IERR.EQ.-2) THEN
               CALL MSGERR (2, 'Error reading point coord. from file')
               FOUND = .TRUE.
               RETURN
            ENDIF
         ELSE
            CALL READXY ('XP', 'YP', XP, YP, 'REP', -1.E10, -1.E10)
            IF (XP .LT. -0.9E10) EXIT
         ENDIF
         MIP = MIP+1
         ALLOCATE(TMP)
         TMP%X = XP
         TMP%Y = YP
         NULLIFY(TMP%NEXTXY)
         CURR%NEXTXY => TMP
         CURR => TMP
      ENDDO
      ALLOCATE(OPSTMP%XP(MIP))
      ALLOCATE(OPSTMP%YP(MIP))
      CURR => FRST%NEXTXY
      DO JJ = 1, MIP
         OPSTMP%XP(JJ) = CURR%X
         OPSTMP%YP(JJ) = CURR%Y
         CURR => CURR%NEXTXY
      END DO
      DEALLOCATE(TMP)
!       ***** store number of output points *****
      OPSTMP%MIP = MIP
      IF (MIP .EQ. 0) CALL MSGERR (2, 'No output points found')
      NULLIFY(OPSTMP%NEXTOPS)
      IF ( .NOT.LOPS ) THEN
         FOPS = OPSTMP
         COPS => FOPS
         LOPS = .TRUE.
      ELSE
         COPS%NEXTOPS => OPSTMP
         COPS => OPSTMP
      END IF
      FOUND = .TRUE.
      RETURN
   ENDIF

!   -------------------------------------------------------------------
!   RAY     'rname'  [xp1] [yp1] [xq1] [yq1]                       &
!          <  [int]  [xp]  [yp]  [xq]  [yq]  >
!   -------------------------------------------------------------------

   IF (KEYWIS ('RAY'))  THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (RAY) in combination'//&
         &' with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
         ALLOCATE(OPSTMP)
         CALL INCSTR('RNAME',PSNAME,'REQ',' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'RNAME is too long')
         OPSTMP%PSNAME = PSNAME
         OPSTMP%PSTYPE = 'R'
         MIP  = 1
         OPSTMP%MIP = MIP
!         first ray
         CALL NWLINE
         IF (STPNOW()) RETURN
         CALL READXY ('XP1', 'YP1', XP, YP, 'REQ', 0., 0.)
         CALL READXY ('XQ1', 'YQ1', XQ, YQ, 'REQ', 0., 0.)
         FRST%X  = XP
         FRST%Y  = YP
         FRST%XQ = XQ
         FRST%YQ = YQ
         NULLIFY(FRST%NEXTXY)
         CURR => FRST
!         following rays
         DO
            CALL ININTG ('INT',INTD,'REP',-1)
            IF (INTD .EQ. -1) EXIT
            IF (INTD .LE. 0) THEN
               CALL MSGERR(2, 'INT negative or zero')
               INTD = 1
            ENDIF
            XP1 = XP
            YP1 = YP
            XQ1 = XQ
            YQ1 = YQ
            CALL READXY ('XP', 'YP', XP, YP, 'REQ', 0., 0.)
            CALL READXY ('XQ', 'YQ', XQ, YQ, 'REQ', 0., 0.)
            do JJ=1,INTD
               MIP = MIP+1
               ALLOCATE(TMP)
               TMP%X  = XP1 + REAL(JJ)*(XP-XP1)/REAL(INTD)
               TMP%Y  = YP1 + REAL(JJ)*(YP-YP1)/REAL(INTD)
               TMP%XQ = XQ1 + REAL(JJ)*(XQ-XQ1)/REAL(INTD)
               TMP%YQ = YQ1 + REAL(JJ)*(YQ-YQ1)/REAL(INTD)
               NULLIFY(TMP%NEXTXY)
               CURR%NEXTXY => TMP
               CURR => TMP
            end do
         END DO
         ALLOCATE(OPSTMP%XP(MIP))
         ALLOCATE(OPSTMP%YP(MIP))
         ALLOCATE(OPSTMP%XQ(MIP))
         ALLOCATE(OPSTMP%YQ(MIP))
         CURR => FRST
         DO JJ = 1, MIP
            OPSTMP%XP(JJ) = CURR%X
            OPSTMP%YP(JJ) = CURR%Y
            OPSTMP%XQ(JJ) = CURR%XQ
            OPSTMP%YQ(JJ) = CURR%YQ
            CURR => CURR%NEXTXY
         END DO
         DEALLOCATE(TMP)

!         ***** termination *****
         OPSTMP%MIP = MIP
         IF (MIP .EQ. 1) CALL MSGERR (1,'Only one ray is defined')
         NULLIFY(OPSTMP%NEXTOPS)
         IF ( .NOT.LOPS ) THEN
            FOPS = OPSTMP
            COPS => FOPS
            LOPS = .TRUE.
         ELSE
            COPS%NEXTOPS => OPSTMP
            COPS => OPSTMP
         END IF
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF

!   -------------------------------------------------------------------
!   ISOline 'sname'  'rname'  DEPTH / BOTTOM [dep]
!   -------------------------------------------------------------------

   IF (KEYWIS ('ISO')) THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (ISO) in combination'//&
         &' with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
         ALLOCATE(OPSTMP)
         CALL INCSTR ('SNAME',PSNAME,'REQ',' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
         OPSTMP%PSNAME = PSNAME
         CALL INCSTR ('RNAME', PRNAME, 'REQ', ' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'RNAME is too long')
         CALL INKEYW ('STA', 'DEP')
         IF (KEYWIS ('BOT')) THEN
            BOTDEP = .TRUE.
         ELSE
            CALL IGNORE ('DEP')
            BOTDEP = .FALSE.
            IF (DYNDEP) CALL MSGERR (2,'depths will vary with time')
         ENDIF
         CALL INREAL ('DEP',DP,'REP',-1.E10)
         ROPS => FOPS
         DO
            IF (ROPS%PSNAME.EQ.PRNAME) EXIT
            IF (.NOT.ASSOCIATED(ROPS%NEXTOPS)) THEN
               CALL MSGERR(2,'Set of rays not defined')
               FOUND = .TRUE.
               RETURN
            END IF
            ROPS => ROPS%NEXTOPS
         END DO
         STYPE = ROPS%PSTYPE
         IF (STYPE .NE. 'R') THEN
            CALL MSGERR&
            &(2,'Ray name set assigned to set of output locations')
            FOUND = .TRUE.
            RETURN
         END IF
         MIPR  = ROPS%MIP
         OPSTMP%PSTYPE = 'C'
         MIP  = 0
         OPSTMP%MIP = MIP
         FRST%X = 0.
         FRST%Y = 0.
         NULLIFY(FRST%NEXTXY)
         CURR => FRST
         do IK=1,MIPR
            XP   = ROPS%XP(IK)
            YP   = ROPS%YP(IK)
            XQ   = ROPS%XQ(IK)
            YQ   = ROPS%YQ(IK)
            II   = SIRAY (DP, XP, YP, XQ, YQ, XX, YY, BOTDEP,&
            &BOTLEV, WATLEV)
            IF (II.EQ.0) THEN
               WRITE (PRINTF, "(' No point with depth ',F5.2, ' is found in ray :',4F10.2)") DP, XP+XOFFS, YP+YOFFS,&
               &XQ+XOFFS, YQ+YOFFS
            ELSE
               MIP = MIP+1
               ALLOCATE(TMP)
               TMP%X = XX
               TMP%Y = YY
               NULLIFY(TMP%NEXTXY)
               CURR%NEXTXY => TMP
               CURR => TMP
            ENDIF
         end do
         ALLOCATE(OPSTMP%XP(MIP))
         ALLOCATE(OPSTMP%YP(MIP))
         CURR => FRST%NEXTXY
         DO IK = 1, MIP
            OPSTMP%XP(IK) = CURR%X
            OPSTMP%YP(IK) = CURR%Y
            CURR => CURR%NEXTXY
         END DO
         DEALLOCATE(TMP)
         IF (MIP.EQ.0) CALL MSGERR&
         &(2, 'No points with valid depth found')
!             ***** store number of points of the curve *****
         OPSTMP%MIP = MIP
         NULLIFY(OPSTMP%NEXTOPS)
         IF ( .NOT.LOPS ) THEN
            FOPS = OPSTMP
            COPS => FOPS
            LOPS = .TRUE.
         ELSE
            COPS%NEXTOPS => OPSTMP
            COPS => OPSTMP
         END IF
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF

!   -------------------------------------------------------------------
!                    | [xpn] [ypn] [alpn] [xlenn] [ylenn] [mxn] [myn]
!   NGRID  'sname'  <
!                    | UNSTRUCtured / -> TRIAngle \
!                                   \    EASYmesh / 'fname'
!   -------------------------------------------------------------------

   IF (KEYWIS ('NGR')) THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (NGR) in combination'//&
         &' with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
!         ver 30.20: names changed, order changed
         ALLOCATE(OPSTMP)
         CALL INCSTR('SNAME',PSNAME,'REQ',' ')
         IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
         OPSTMP%PSNAME = PSNAME
         OPSTMP%PSTYPE = 'N'
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('UNSTRUC')) THEN
            PP = .TRUE.
            CALL INKEYW('STA','TRIA')
            IF (KEYWIS('EASY')) THEN
               IOSTAT = 0
               NDS    = 0
               CALL INCSTR ('FNAME', BASENM, 'REQ', ' ')
               FILENM = TRIM(BASENM)//'.n'
               CALL FOR (NDS, FILENM, 'OF', IOSTAT)
               IF (STPNOW()) RETURN

!               --- read first line to determine number of vertices

               READ(NDS, *, IOSTAT=IOSTAT) NVTX
               IF (grid_read_failed(IOSTAT)) RETURN
               ISTAT = 0
               IF(.NOT.ALLOCATED(XG)) ALLOCATE(XG(NVTX), STAT = ISTAT)
               IF ( ISTAT == 0 ) THEN
                  IF(.NOT.ALLOCATED(YG)) ALLOCATE(YG(NVTX), STAT=ISTAT)
               ENDIF
               IF ( ISTAT == 0 ) THEN
                  IF(.NOT.ALLOCATED(VM)) ALLOCATE(VM(NVTX), STAT=ISTAT)
               ENDIF
               IF ( ISTAT /= 0 ) THEN
                  CALL MSGERR ( 4,&
                  &'Allocation problem in SWREPS: array XG, YG or VM ' )
                  RETURN
               ENDIF

!               --- read coordinates of vertices and boundary marker

               DO KK = 1, NVTX
                  READ(NDS, "((6X,2E22.15,I3))", IOSTAT=IOSTAT) XG(I),YG(I),VM(I)
                  IF (grid_read_failed(IOSTAT)) RETURN
               ENDDO

!               --- close file <name>.n

               CLOSE(NDS)

            ELSE
               CALL IGNORE('TRIA')
               IOSTAT = 0
               NDS    = 0
               CALL INCSTR ('FNAME', BASENM, 'REQ', ' ')
               FILENM = TRIM(BASENM)//'.node'
               CALL FOR (NDS, FILENM, 'OF', IOSTAT)
               IF (STPNOW()) RETURN

!               --- read first line to determine number of vertices

               READ(NDS, *, IOSTAT=IOSTAT) NVTX, NDIM, NATTR, NBMARK
               IF (grid_read_failed(IOSTAT)) RETURN
               ISTAT = 0
               IF(.NOT.ALLOCATED(XG)) ALLOCATE(XG(NVTX), STAT = ISTAT)
               IF ( ISTAT == 0 ) THEN
                  IF(.NOT.ALLOCATED(YG)) ALLOCATE(YG(NVTX), STAT=ISTAT)
               ENDIF
               IF ( ISTAT == 0 ) THEN
                  IF(.NOT.ALLOCATED(VM)) ALLOCATE(VM(NVTX), STAT=ISTAT)
               ENDIF
               IF ( ISTAT /= 0 ) THEN
                  CALL MSGERR ( 4,&
                  &'Allocation problem in SWREPS: array XG, YG or VM ' )
                  RETURN
               ENDIF

!               --- check if boundary marker has been specified

               IF ( NBMARK == 0 ) THEN
                  CALL MSGERR ( 4,&
                  &'boundary marker for vertices/faces must be specified ' )
                  RETURN
               ENDIF

!               --- read coordinates of vertices and boundary marker

               IF ( NATTR == 0 ) THEN
                  DO KK = 1, NVTX
                     READ(NDS, *, IOSTAT=IOSTAT) I,XG(I),YG(I),VM(I)
                     IF (grid_read_failed(IOSTAT)) RETURN
                  ENDDO
               ELSE
                  DO KK = 1, NVTX
                     READ(NDS, *, IOSTAT=IOSTAT) I,XG(I),YG(I),RDUM,&
                     &VM(I)
                     IF (grid_read_failed(IOSTAT)) RETURN
                  ENDDO
               ENDIF

!               --- close file <name>.node

               CLOSE(NDS)

            ENDIF
         ELSE
            PP = .FALSE.
         ENDIF
         IF (.NOT.PP) THEN
!         structured grid
            CALL READXY ('XPN', 'YPN', XPCN, YPCN , 'REQ', 0., 0.)
            CALL INREAL('ALPN',ALPCN,'REQ',0.)
            CALL INREAL('XLENN',XNLEN,'REQ',0.)
            CALL INREAL('YLENN',YNLEN,'REQ',0.)
            ALTNP = ALPCN / 360.
            ANG = PI2 * (ALTNP - NINT(ALTNP))
!            estimate step size for output
            IF (OPTG.EQ.1) THEN
               COSA2 = (COS(ANG-ALPC))**2
               SINA2 = (SIN(ANG-ALPC))**2
               DXN = DX*COSA2 + DY*SINA2
               DYN = DX*SINA2 + DY*COSA2
            ELSEIF (OPTG.EQ.3) THEN
!              curvilinear grid, DXN and DYN are average step size
               DXN = (XCLEN+YCLEN)/REAL(MXCGL+MYCGL)
               DYN = DXN
            ELSEIF (OPTG.EQ.5) THEN
!              unstructured grid, DXN and DYN are average grid size
               DXN = 0.5*(mingsiz+maxgsiz)
               DYN = DXN
            ENDIF
            CALL ININTG ('MXN',MXN,'STA',MAX(1,NINT(XNLEN/DXN)))
            CALL ININTG ('MYN',MYN,'STA',MAX(1,NINT(YNLEN/DYN)))
            MIP = 0
            ALLOCATE(OPSTMP%XP(2*(MXN+MYN)))
            ALLOCATE(OPSTMP%YP(2*(MXN+MYN)))
            XF=XPCN
            YF=YPCN
!            *****   start to calculate the positions       ********
!            *****  of the boundary point in the four sides ********
            do I=1,4
               INTE=MXN
               ALON=XNLEN
               ANGLE=ANG+PI2*(90.*REAL(I-1))/360.
               IF (I .EQ. 2 .OR. I .EQ. 4) THEN
                  INTE=MYN
                  ALON=YNLEN
               ENDIF
               XI=XF
               YI=YF
               XF=XI+ALON*COS(ANGLE)
               YF=YI+ALON*SIN(ANGLE)
               do KK=1,INTE
                  MIP=MIP+1
                  OPSTMP%XP(MIP)=XI+REAL(KK)*(XF-XI)/REAL(INTE)
                  OPSTMP%YP(MIP)=YI+REAL(KK)*(YF-YI)/REAL(INTE)
               end do
            end do
!                ***** store number of points *****
            OPSTMP%MIP = MIP
            OPSTMP%OPR(1) = XNLEN
            OPSTMP%OPR(2) = YNLEN
            OPSTMP%OPR(3) = XPCN
            OPSTMP%OPR(4) = YPCN
            OPSTMP%OPR(5) = PI2 * (ALPCN/360.-NINT(ALPCN/360.))
            OPSTMP%OPI(1) = MXN
            OPSTMP%OPI(2) = MYN
         ELSE
!         unstructured grid

            MIP = COUNT(MASK=VM/=0)
            ALLOCATE(OPSTMP%XP(MIP))
            ALLOCATE(OPSTMP%YP(MIP))
            I = 0
            DO KK = 1, NVTX
               IF ( VM(KK) /= 0 ) THEN
                  I = I + 1
                  OPSTMP%XP(I)= XG(KK)-XOFFS
                  OPSTMP%YP(I)= YG(KK)-YOFFS
               ENDIF
            ENDDO
            IF (ALLOCATED(XG)) DEALLOCATE(XG)
            IF (ALLOCATED(YG)) DEALLOCATE(YG)
            IF (ALLOCATED(VM)) DEALLOCATE(VM)
            OPSTMP%MIP = MIP
!            next variable will be used for checking grid in
!            present computational and/or bottom grid
            OPSTMP%OPR(1) = -999.
         ENDIF
         IF (MIP .EQ. 0) CALL MSGERR(1,'No output points found')
         NULLIFY(OPSTMP%NEXTOPS)
         IF ( .NOT.LOPS ) THEN
            FOPS = OPSTMP
            COPS => FOPS
            LOPS = .TRUE.
         ELSE
            COPS%NEXTOPS => OPSTMP
            COPS => OPSTMP
         END IF
         FOUND = .TRUE.
         RETURN

      ENDIF
   ENDIF
!     ---------------------------------------------------------
!     command not found:
   RETURN
FOUND = .TRUE.
   RETURN

CONTAINS

   LOGICAL FUNCTION grid_read_failed(status)
      INTEGER, INTENT(IN) :: status

      grid_read_failed = status.NE.0
      IF (.NOT.grid_read_failed) RETURN
      INQUIRE (UNIT=NDS, NAME=FILENM)
      IF (IS_IOSTAT_END(status)) THEN
         CALL MSGERR (4, 'unexpected end of file in file '//FILENM)
      ELSE
         CALL MSGERR (4, 'error reading data from file '//FILENM)
      ENDIF
   END FUNCTION grid_read_failed

!*    end of subroutine SWREPS  **
end subroutine SWREPS
!************************************************************************
!                                                                      *
SUBROUTINE SWREOQ ( FOUND )
   USE swan_file_opening, ONLY: FOR
   USE swan_services, ONLY: MKPATH
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, ININTG, INKEYW, INREAL, KEYWIS, INCTIM, INITVD
!                                                                      *
!************************************************************************

   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
   USE M_PARALL
!NCF   USE swn_outnc
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
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     32.02: Roeland Ris & Cor van der Schelde (1D version)
!     34.01: Jeroen Adema
!     40.03, 40.13: Nico Booij
!     40.14: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.62: Andre van der Westhuysen
!     41.72: Henrique Rapizo
!     41.75: Erick Rogers
!
!  1. Updates
!
!     30.50         : option COORD added in command PLOT
!                     option STAR  added in command PLOT
!     32.02, Feb. 98: 1D version introduced
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Apr. 98: removed reference to commons KAART and KAR
!     30.81, Nov. 98: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     30.81, Jan. 99: Replaced variable TO by TO_ (because TO is a reserved
!                     word)
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Nov. 99: in case SPEC2D the value of MXOUTAR is increased by
!                     6*MIP
!     40.03, Mar. 00: NQUA increased in case of Isoline plot
!            Sep. 00: inconsistency with manual corrected
!     40.13, Mar. 01: option BLOCKed added in plot of problem points
!            Aug. 01: array for NESTOUT request extended to 20 (in view
!     40.13, Oct. 01: filenames are stored in array OUTP_FILES
!                     not any more in array containing output request parameters
!     40.14, Dec. 01: format for setup corrected.
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing POOL construction and HPGL funcationality
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.62, Nov. 15: included input fields for wave partitioning
!     41.75, Jan. 19: adding sea ice
!     41.72, Nov. 19: user defined number of swell partitions instead of hardcoded 9
!
!  2. Purpose
!
!     Reading and processing of the output requests
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     FOUND : output parameter indicating whether command
!                    being processed is found (value True)
!                    or not (False)

   LOGICAL FOUND

!  8. Subroutines used
!
!     command reading routines
!     (all Ocean Pack)


!  9. Subroutines calling
!
!     SPROUT
!
! 10. Error messages
!
!       ---
!
! 13. Source text

   INTEGER, PARAMETER :: NEXPT = 10
   INTEGER, SAVE :: IENT = 0
   INTEGER IERR, IDLAO, ILPOS, IOSTAT, IPROC, IVTYPE
   INTEGER JJ, MIP, NREF, NVAR
   REAL DFAC
   CHARACTER(LEN=16) :: PSNAME
   CHARACTER(LEN=1)  :: STYPE
   CHARACTER(LEN=4)  :: RTYPE
   TYPE(ORQDAT), POINTER :: ORQTMP
   TYPE(ORQDAT), SAVE, POINTER :: CORQ
   TYPE AUXT
      INTEGER             :: I
      REAL                :: R
      TYPE(AUXT), POINTER :: NEXTI
   end type AUXT
   TYPE(AUXT), TARGET  :: FRST
   TYPE(AUXT), POINTER :: CURR, TMP
   INTEGER, PARAMETER :: IEXPT(NEXPT) = [1, 2, 4, 5, 10, 12, 13, 16, 17, 26]
   INTEGER IVT
   INTEGER NOSWLL
   LOGICAL LC, VTK
   CHARACTER (LEN=4) :: PNUM
   CHARACTER (LEN=80) :: MSGSTR
   CHARACTER (LEN=LENFNM) :: OUTDIR
   CALL STRACE (IENT,'SWREOQ')

!   --------------------------------------------------------------------
!   BLOCK   'sname'  HEADER / NOHEADER  'fname' (LAY-OUT [idla])
!          <  DSPR/HSIGN/DIR/PDIR/TDIR/TM01/RTM01/RTP/TM02/FSPR/DEPTH/VEL/   &
!             FRCOEFF/WIND/DISSIP/QB/TRANSP/FORCE/UBOT/URMS/WLEN/STEEPNESS/  &
!             DHSIGN/DRTM01/LEAK/TSEC/XP/YP/DIST/SETUP/TMM10/RTMM10/
!             TMBOT/QP/BFI/WATLEV/BOTLEV/TPS/DISBOT/DISSURF/DISWCAP/
!             GENE/GENW/REDI/REDQ/REDT/REDB/REDC/PROPA/PROPX/PROPT/PROPS/    &
!             RADS/LWAVP/DISTUR/TURB/DISSWELL/AICE/DISICE/
!             PTHSIGN/PTRTP/PTWLEN/PTDIR/PTDSPR/PTWFRAC/PTSTEEPNESS>
!             ([unit]) (OUTPUT [tbegblk] [deltblk] SEC/MIN/HR/DAY)
!   --------------------------------------------------------------------
!   BLO   block type output
   IF (KEYWIS ('BLO')) THEN

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (BLO) in combination'//&
         &' with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
         CALL SWNMPS (PSNAME, STYPE, MIP, IERR)
         IF (IERR.NE.0) THEN
            FOUND = .TRUE.
            RETURN
         END IF
         IF (STYPE.NE.'F' .AND. STYPE.NE.'H' .AND. STYPE.NE.'U' ) THEN
            CALL MSGERR(2,'Set of output locations is not correct type')
            FOUND = .TRUE.
            RETURN
         ENDIF

!         output frame exists

         ALLOCATE(ORQTMP)
         NREOQ = NREOQ + 1
         IF (NREOQ.GT.MAX_OUTP_REQ) CALL MSGERR (2,&
         &'too many output requests')

         IDLAO = 1

         CALL INKEYW ('REQ',' ')
         IF (KEYWIS('NOHEAD') .OR. KEYWIS ('FIL')) THEN
            CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
            DFAC = 1.
            CALL INKEYW ('STA', ' ')
            IF (KEYWIS('LONG')) THEN
!             option disabled
               CALL MSGERR (2, 'option LONG disabled; use OUTP OPT')
            ENDIF
            RTYPE = 'BLKD'
         ELSE
            CALL IGNORE ('HEAD')
            CALL IGNORE ('PAP')
            DFAC = -1.
            CALL INCSTR ('FNAME', FILENM, 'STA', ' ')
            RTYPE = 'BLKP'
            IF ( INDEX( FILENM, '.MAT' ).NE.0 .OR.&
            &INDEX (FILENM, '.mat' ).NE.0 ) THEN
               CALL MSGERR(4,'No header allowed for Matlab files')
               RETURN
            END IF
         END IF
         VTK = INDEX( FILENM, '.VT' ).NE.0 .OR.&
         &INDEX (FILENM, '.vt' ).NE.0
         IF ( VTK ) THEN
            DFAC  = 1.
            RTYPE = 'BLKV'
         ENDIF
         IF (FILENM .EQ. ' ') THEN
            NREF = PRINTF
         ELSE
            NREF = 0
!           --- append node number to FILENM in case of
!               parallel computing
            IF ( PARLL.AND..NOT.VTK ) THEN
               ILPOS = INDEX ( FILENM, ' ' )-1
               WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
            END IF
         ENDIF
         CALL INKEYW ('STA', ' ')
         IF (KEYWIS('LAY')) THEN
            CALL ININTG ('IDLA', IDLAO, 'REQ', 0)
            CALL INKEYW ('REQ', ' ')
            IF (IDLAO.NE.1 .AND. IDLAO.NE.3 .AND. IDLAO.NE.4&
!NCF            &.AND. IDLAO.NE.5&
            &)&
            &CALL MSGERR (2, 'Illegal value for IDLA')
         ENDIF
         ORQTMP%OQR(1) = -1.
         ORQTMP%OQR(2) = -1.
         ORQTMP%RQTYPE = RTYPE
         ORQTMP%PSNAME = PSNAME
         ORQTMP%OQI(1) = NREF
         ORQTMP%OQI(2) = NREOQ
         NVAR = 0
         ORQTMP%OQI(3) = NVAR
         ORQTMP%OQI(4) = IDLAO
         OUTP_FILES(NREOQ) = FILENM

!         read types of output quantities

         FRST%I = 0
         FRST%R = 0.
         NULLIFY(FRST%NEXTI)
         CURR => FRST
         DO
            CALL SVARTP (IVTYPE)
            IF (IVTYPE .EQ. 98 .OR. IVTYPE .EQ. 999) EXIT
         IF (IVTYPE .NE. 999) THEN
!NCF            IF ( INDEX(FILENM,'.NC').NE.0  .OR.&
!NCF            &INDEX(FILENM,'.nc').NE.0 ) THEN
!NCF               IF ( IVTYPE.GT.2 ) THEN
!NCF                  call stnames_init()
!NCF                  IF ( STNAMES(IVTYPE,1).EQ. ' ' ) CALL MSGERR (2,&
!NCF                  &'netCDF output not allowed for '//TRIM(FILENM))
!NCF               ENDIF
!NCF            ENDIF
            CALL INREAL ('UNIT', DFAC, 'STA', -1.)
            IF (OVSVTY(IVTYPE).EQ.5) THEN
               CALL MSGERR (2,&
               &'Type of output not allowed for this quantity')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.100 .AND. IVTYPE.LT.110) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTHSIGN instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.110 .AND. IVTYPE.LT.120) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTRTP instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.120 .AND. IVTYPE.LT.130) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTWLEN instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.130 .AND. IVTYPE.LT.140) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTDIR instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.140 .AND. IVTYPE.LT.150) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTDSPR instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.150 .AND. IVTYPE.LT.160) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTWFRAC instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.160 .AND. IVTYPE.LT.170) THEN
               CALL MSGERR (2,'Invalid partitioning output '//&
               &'specification. Use PTSTEEPNESS instead.')
               WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
            ELSE IF (IVTYPE.GT.0 .AND. IVTYPE.LT.100) THEN
               NVAR = NVAR+1
               ALLOCATE(TMP)
               TMP%I = IVTYPE
               TMP%R = DFAC
               NULLIFY(TMP%NEXTI)
               CURR%NEXTI => TMP
               CURR => TMP
               IF (IVTYPE.EQ.6) IUBOTR = 1
               IF (IVTYPE.EQ.36 .AND. JZEL.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JZEL   = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.50 .AND. JPBOT.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JPBOT  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.54 .AND. JDSXB.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXB  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.55 .AND. JDSXS.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXS  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.56 .AND. JDSXW.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXW  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.57 .AND. JDSXV.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXV  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.60 .AND. JGENR.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JGENR  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.61 .AND. JGSXW.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JGSXW  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.62 .AND. JREDS.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JREDS  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.63 .AND. JRSXQ.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JRSXQ  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.64 .AND. JRSXT.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JRSXT  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.65 .AND. JTRAN.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JTRAN  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.66 .AND. JTSXG.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JTSXG  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.67 .AND. JTSXT.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JTSXT  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.68 .AND. JTSXS.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JTSXS  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.69 .AND. JRADS.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JRADS  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.72 .AND. JDSXT.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXT  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.74 .AND. JDSXM.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXM  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.75 .AND. JDSXL.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXL  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.76 .AND. JDSXI.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JDSXI  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.79 .AND. JRSXB.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JRSXB  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.80 .AND. JRSXC.LE.1) THEN
                  MCMVAR = MCMVAR+1
                  JRSXC  = MCMVAR
                  ALOCMP = .TRUE.
               ENDIF
               IF (IVTYPE.EQ.7  .OR. IVTYPE.EQ.9  .OR.&
               &IVTYPE.EQ.54 .OR. IVTYPE.EQ.55 .OR.&
               &IVTYPE.EQ.56 .OR. IVTYPE.EQ.57 .OR.&
               &IVTYPE.GE.60 ) LADDS = .TRUE.
            ELSE IF (IVTYPE.LT.170) THEN
               NOSWLL = INT(OUTPAR(51))
!               add NOSWLL partitions of requested partition parameter
               DO IVT = IVTYPE, IVTYPE+NOSWLL
                  NVAR = NVAR+1
                  ALLOCATE(TMP)
                  TMP%I = IVT
                  TMP%R = DFAC
                  NULLIFY(TMP%NEXTI)
                  CURR%NEXTI => TMP
                  CURR => TMP
               ENDDO
            ELSE IF (IVTYPE.EQ.170) THEN
!               in case of PARTITIONS, add all partition parameters
               NVAR = NVAR+1
               ALLOCATE(TMP)
               TMP%I = 171
               TMP%R = DFAC
               NULLIFY(TMP%NEXTI)
               CURR%NEXTI => TMP
               CURR => TMP
               DO IVT = 100, 159
                  NVAR = NVAR+1
                  ALLOCATE(TMP)
                  TMP%I = IVT
                  TMP%R = DFAC
                  NULLIFY(TMP%NEXTI)
                  CURR%NEXTI => TMP
                  CURR => TMP
               ENDDO
!               add some other parameters e.g. Xp, Yp, Depth, Hs, etc.
               DO IVT = 1, NEXPT
                  NVAR = NVAR+1
                  ALLOCATE(TMP)
                  TMP%I = IEXPT(IVT)
                  TMP%R = DFAC
                  NULLIFY(TMP%NEXTI)
                  CURR%NEXTI => TMP
                  CURR => TMP
               ENDDO
            ENDIF
         ENDIF
         END DO
         IF (NVAR.GT.0) THEN
            ALLOCATE(ORQTMP%IVTYP(NVAR))
            ALLOCATE(ORQTMP%FAC(NVAR))
            CURR => FRST%NEXTI
            DO JJ = 1, NVAR
               ORQTMP%IVTYP(JJ) = CURR%I
               ORQTMP%FAC  (JJ) = CURR%R
               CURR => CURR%NEXTI
            END DO
            DEALLOCATE(TMP)
         END IF

         IF (IVTYPE .EQ. 98) THEN
            IF (NSTATM.EQ.0) CALL MSGERR (3,&
            &'time information not allowed in stationary mode')
            NSTATM = 1
            CALL INCTIM (ITMOPT, 'TBEG', ORQTMP%OQR(1), 'REQ', 0D0)
            CALL INITVD ('DELT', ORQTMP%OQR(2), 'REQ', 0D0)
            IF ( VTK.AND.IAMMASTER ) THEN
!              a PVD file is created to collect time-varying output
               LC=.FALSE.
               ILPOS=INDEX( FILENM, '.VT' )
               IF (ILPOS.EQ.0) THEN
                  LC=.TRUE.
                  ILPOS=INDEX( FILENM, '.vt' )
               ENDIF
               IF (LC) THEN
                  WRITE(FILENM(ILPOS+1:ILPOS+3),"(A3)") 'pvd'
               ELSE
                  WRITE(FILENM(ILPOS+3:ILPOS+3),"(A3)") 'PVD'
               ENDIF
               NREF   =  0
               IOSTAT = -1
               CALL FOR (NREF, FILENM, 'UF', IOSTAT)
               IF (STPNOW()) RETURN
               UPVDF(NREOQ) = NREF
!              write the header lines
               WRITE(NREF,'(A)') TRIM(XMLLIN1)
               WRITE(NREF,'(A)') TRIM(XMLLIN2)
               VTKLINE = '  This file was generated by SWAN version '//&
               &TRIM(VERTXT)//'; project: '//TRIM(PROJID)//&
               &'; run number: '//TRIM(PROJNR)
               WRITE(NREF,'(A)') TRIM(VTKLINE)
               WRITE(NREF,'(A)') TRIM(XMLLIN3)
               WRITE(NREF,'(A)') TRIM(PVDLIN1)
               WRITE(NREF,'(A)') TRIM(PVDLIN2)
!              make output directory
               IF (LC) THEN
                  OUTDIR=FILENM(1:ILPOS-1)//'_output'
               ELSE
                  OUTDIR=FILENM(1:ILPOS-1)//'_OUTPUT'
               ENDIF
               CALL MKPATH ( OUTDIR, IERR )
               IF (IERR.NE.0) OUTDIR = '.'
               VTKDIR(NREOQ) = OUTDIR
!              create subdirectories to store each piece of output data
               IF (PARLL) THEN
                  DO IPROC = 1, NPROC
                     IF (LC) THEN
                        WRITE(PNUM(1:4),"(A1,I3.3)") 'p',IPROC
                     ELSE
                        WRITE(PNUM(1:4),"(A1,I3.3)") 'P',IPROC
                     ENDIF
                     CALL MKPATH ( TRIM(OUTDIR)//DIRCH2//PNUM, IERR )
                  ENDDO
                  IF (IERR.NE.0) THEN
                     WRITE (MSGSTR, '(A,I5)')&
                     &'Error while creating folders '//&
                     &'- status error =',IERR
                     CALL MSGERR( 3, TRIM(MSGSTR) )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         ORQTMP%OQI(3) = NVAR
         IF (NVAR.EQ.0) THEN
            ALLOCATE(ORQTMP%IVTYP(0))
            ALLOCATE(ORQTMP%FAC(0))
         ENDIF
         NULLIFY(ORQTMP%NEXTORQ)
         IF ( .NOT.LORQ ) THEN
            FORQ = ORQTMP
            CORQ => FORQ
            LORQ = .TRUE.
         ELSE
            CORQ%NEXTORQ => ORQTMP
            CORQ => ORQTMP
         END IF
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF
!   --------------------------------------------------------------------
!   TABLE   'sname'  HEADER / NOHEADER / INDEXED 'fname'
!          <  DSPR/HSIGN/DIR/PDIR/TDIR/TM01/RTM01/RTP/TM02/FSPR/DEPTH/VEL/   &
!             FRCOEFF/WIND/DISSIP/QB/TRANSP/FORCE/UBOT/URMS/WLEN/STEEPNESS/  &
!             DHSIGN/DRTM01/LEAK/TIME/TSEC/XP/YP/DIST/SETUP/TMM10/RTMM10/    &
!             TMBOT/QP/BFI/WATLEV/BOTLEV/TPS/DISBOT/DISSURF/DISWCAP/
!             GENE/GENW/REDI/REDQ/REDT/REDB/REDC/PROPA/PROPX/PROPT/PROPS/    &
!             RADS/LWAVP/DISTUR/TURB/DISSWELL/AICE/DISICE/
!             PTHSIGN/PTRTP/PTWLEN/PTDIR/PTDSPR/PTWFRAC/PTSTEEPNESS>
!             ([unit]) (OUTPUT [tbegtbl] [delttbl] SEC/MIN/HR/DAY)
!   --------------------------------------------------------------------
!   TABLE   output in the form of a table

   IF (KEYWIS ('TAB')) THEN
      CALL SWNMPS (PSNAME, STYPE, MIP, IERR)
      IF (IERR.NE.0) THEN
         FOUND = .TRUE.
         RETURN
      END IF

!       output points exist

      ALLOCATE(ORQTMP)
      NREOQ = NREOQ + 1
      IF (NREOQ.GT.MAX_OUTP_REQ) CALL MSGERR (2,&
      &'too many output requests')

      CALL INKEYW ('STA','HEAD')
      IF (KEYWIS('NOHEAD') .OR. KEYWIS ('FIL')) THEN
         RTYPE = 'TABD'
      ELSE IF (KEYWIS ('IND')) THEN
         RTYPE = 'TABI'
      ELSE IF (KEYWIS ('SWAN')) THEN
         RTYPE = 'TABS'
      ELSE IF (KEYWIS ('STAB')) THEN
         RTYPE = 'TABT'
      ELSE
         CALL IGNORE ('HEAD')
         CALL IGNORE ('PAP')
         RTYPE = 'TABP'
      END IF
      ORQTMP%OQR(1) = -1.
      ORQTMP%OQR(2) = -1.
      ORQTMP%RQTYPE = RTYPE
!       unit reference number NREF is 0, will be determined in output module
      CALL INCSTR ('FNAME', FILENM, 'STA', ' ')
      IF (FILENM .NE. '    ') THEN
!NCF         IF ( INDEX( FILENM, '.NC' ).NE.0 .OR.&
!NCF         &INDEX (FILENM, '.nc' ).NE.0 ) THEN
!NCF            RTYPE = 'TABC'
!NCF            ORQTMP%RQTYPE = RTYPE
!NCF         ENDIF
         NREF = 0
!         --- append node number to FILENM in case of
!             parallel computing
         IF ( PARLL ) THEN
            ILPOS = INDEX ( FILENM, ' ' )-1
            WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
         END IF
      ELSE
         NREF = PRINTF
      ENDIF
      ORQTMP%PSNAME = PSNAME
      ORQTMP%OQI(1) = NREF
      ORQTMP%OQI(2) = NREOQ
      OUTP_FILES(NREOQ) = FILENM

      NVAR = 0
      ORQTMP%OQI(3) = NVAR
!       read types of variables to be printed in the table
      FRST%I = 0
      NULLIFY(FRST%NEXTI)
      CURR => FRST
      DO
         CALL SVARTP (IVTYPE)
         IF (IVTYPE .EQ. 98 .OR. IVTYPE .EQ. 999) EXIT
      IF (IVTYPE .NE. 999) THEN
!NCF         IF ( INDEX(FILENM,'.NC').NE.0  .OR.&
!NCF         &INDEX(FILENM,'.nc').NE.0 ) THEN
!NCF            IF ( IVTYPE.GT.2.AND.IVTYPE.NE.40 ) THEN
!NCF               call stnames_init()
!NCF               IF ( STNAMES(IVTYPE,1).EQ. ' ' ) CALL MSGERR (2,&
!NCF               &'netCDF table does not support '//OVKEYW(IVTYPE))
!NCF            ENDIF
!NCF         ENDIF
         IF (OVSVTY(IVTYPE).EQ.5) THEN
            CALL MSGERR (2,&
            &'Type of output not allowed for this quantity')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.100 .AND. IVTYPE.LT.110) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTHSIGN instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.110 .AND. IVTYPE.LT.120) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTRTP instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.120 .AND. IVTYPE.LT.130) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTWLEN instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.130 .AND. IVTYPE.LT.140) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTDIR instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.140 .AND. IVTYPE.LT.150) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTDSPR instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.150 .AND. IVTYPE.LT.160) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTWFRAC instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.160 .AND. IVTYPE.LT.170) THEN
            CALL MSGERR (2,'Invalid partitioning output '//&
            &'specification. Use PTSTEEPNESS instead.')
            WRITE (PRINTF, *) ' -> ', OVSNAM(IVTYPE)
         ELSE IF (IVTYPE.GT.0 .AND. IVTYPE.LT.100) THEN
            NVAR = NVAR+1
            ALLOCATE(TMP)
            TMP%I = IVTYPE
            NULLIFY(TMP%NEXTI)
            CURR%NEXTI => TMP
            CURR => TMP
            IF (IVTYPE.EQ.18) IUBOTR = 1
            IF (IVTYPE.EQ.50 .AND. JPBOT.LE.1) THEN
               MCMVAR = MCMVAR+1
               JPBOT  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.54 .AND. JDSXB.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXB  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.55 .AND. JDSXS.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXS  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.56 .AND. JDSXW.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXW  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.57 .AND. JDSXV.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXV  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.60 .AND. JGENR.LE.1) THEN
               MCMVAR = MCMVAR+1
               JGENR  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.61 .AND. JGSXW.LE.1) THEN
               MCMVAR = MCMVAR+1
               JGSXW  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.62 .AND. JREDS.LE.1) THEN
               MCMVAR = MCMVAR+1
               JREDS  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.63 .AND. JRSXQ.LE.1) THEN
               MCMVAR = MCMVAR+1
               JRSXQ  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.64 .AND. JRSXT.LE.1) THEN
               MCMVAR = MCMVAR+1
               JRSXT  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.65 .AND. JTRAN.LE.1) THEN
               MCMVAR = MCMVAR+1
               JTRAN  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.66 .AND. JTSXG.LE.1) THEN
               MCMVAR = MCMVAR+1
               JTSXG  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.67 .AND. JTSXT.LE.1) THEN
               MCMVAR = MCMVAR+1
               JTSXT  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.68 .AND. JTSXS.LE.1) THEN
               MCMVAR = MCMVAR+1
               JTSXS  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.69 .AND. JRADS.LE.1) THEN
               MCMVAR = MCMVAR+1
               JRADS  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.72 .AND. JDSXT.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXT  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.74 .AND. JDSXM.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXM  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.75 .AND. JDSXL.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXL  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.76 .AND. JDSXI.LE.1) THEN
               MCMVAR = MCMVAR+1
               JDSXI  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.79 .AND. JRSXB.LE.1) THEN
               MCMVAR = MCMVAR+1
               JRSXB  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.80 .AND. JRSXC.LE.1) THEN
               MCMVAR = MCMVAR+1
               JRSXC  = MCMVAR
               ALOCMP = .TRUE.
            ENDIF
            IF (IVTYPE.EQ.7  .OR. IVTYPE.EQ.9  .OR.&
            &IVTYPE.EQ.54 .OR. IVTYPE.EQ.55 .OR.&
            &IVTYPE.EQ.56 .OR. IVTYPE.EQ.57 .OR.&
            &IVTYPE.GE.60 ) LADDS = .TRUE.
            CALL INKEYW ('STA', ' ')
            IF (KEYWIS('UNIT')) THEN
               CALL MSGERR (1, 'UNIT is ignored in this version')
            ENDIF
         ELSE IF (IVTYPE.LT.170) THEN
            NOSWLL = INT(OUTPAR(51))
!           add NOSWLL partitions of requested partition parameter
            DO IVT = IVTYPE, IVTYPE+NOSWLL
               NVAR = NVAR+1
               ALLOCATE(TMP)
               TMP%I = IVT
               TMP%R = DFAC
               NULLIFY(TMP%NEXTI)
               CURR%NEXTI => TMP
               CURR => TMP
            ENDDO
            CALL INKEYW ('STA', ' ')
            IF (KEYWIS('UNIT')) THEN
               CALL MSGERR (1, 'UNIT is ignored in this version')
            ENDIF
         ENDIF
      ENDIF
      END DO
      IF (NVAR.GT.0) THEN
         ALLOCATE(ORQTMP%IVTYP(NVAR))
         CURR => FRST%NEXTI
         DO JJ = 1, NVAR
            ORQTMP%IVTYP(JJ) = CURR%I
            CURR => CURR%NEXTI
         END DO
         DEALLOCATE(TMP)
      END IF
!NCF      IF ( RTYPE.EQ.'TABC') THEN
!NCF         ALLOCATE(ORQTMP%FAC(NVAR))
!NCF         ORQTMP%FAC=1.
!NCF      ELSE
         ALLOCATE(ORQTMP%FAC(0))
!NCF      ENDIF

      IF (IVTYPE .EQ. 98) THEN
         IF (NSTATM.EQ.0) CALL MSGERR (3,&
         &'time information not allowed in stationary mode')
         NSTATM = 1
         CALL INCTIM (ITMOPT, 'TBEG', ORQTMP%OQR(1), 'REQ', 0D0)
         CALL INITVD ('DELT', ORQTMP%OQR(2), 'REQ', 0D0)
      ENDIF
      ORQTMP%OQI(3) = NVAR
      IF (NVAR.EQ.0) THEN
         ALLOCATE(ORQTMP%IVTYP(0))
         ALLOCATE(ORQTMP%FAC(0))
      ENDIF
      NULLIFY(ORQTMP%NEXTORQ)
      IF ( .NOT.LORQ ) THEN
         FORQ = ORQTMP
         CORQ => FORQ
         LORQ = .TRUE.
      ELSE
         CORQ%NEXTORQ => ORQTMP
         CORQ => ORQTMP
      END IF
      FOUND = .TRUE.
      RETURN
   ENDIF

!   PLOT    plot iso lines and/or vector fields
!
!   --------------------------------------------------------------------

   IF (KEYWIS ('PLO')) THEN
      CALL MSGERR(2,'Keyword PLO... is no longer maintained')
      FOUND = .TRUE.
      RETURN
   ENDIF

!   --------------------------------------------------------------------
!   SPECout 'sname'  SPEC1D/SPEC2D  ABS/REL  S/L  'fname'
!NCF!                    (MONth  ESCAle MDGRID COMPress NOAUX) (NOT document
!                    (OUTPUT [tbegspc] [deltspc] SEC/MIN/HR/DAY)
!   --------------------------------------------------------------------
!   SPEC   output of spectra

   IF (KEYWIS ('SPEC')) THEN
      CALL SWNMPS (PSNAME, STYPE, MIP, IERR)
      IF (IERR.NE.0) THEN
         FOUND = .TRUE.
         RETURN
      END IF

!       output points exist

      ALLOCATE(ORQTMP)
      NREOQ = NREOQ + 1
      IF (NREOQ.GT.MAX_OUTP_REQ) CALL MSGERR (2,&
      &'too many output requests')

      CALL INKEYW ('STA', 'SPEC2D')
      IF (KEYWIS ('FS1D') .OR. KEYWIS('SPEC1D')) THEN
         RTYPE  = 'SPE1'
      ELSE
         CALL IGNORE ('SFD')
         CALL IGNORE ('SPEC2D')
         RTYPE  = 'SPEC'
      ENDIF
      CALL INKEYW ('STA', 'ABS')
      IF (KEYWIS ('REL')) THEN
         RTYPE(3:3)  = 'R'
      ELSE
         CALL IGNORE ('ABS')
      ENDIF

      CALL INKEYW ('STA', 'S')
      IF (KEYWIS ('L')) THEN
         IF (RTYPE(3:3).EQ.'R') THEN
            RTYPE(3:3) = 'L'
         ELSEIF (RTYPE(3:3).EQ.'E') THEN
            RTYPE(3:3) = 'B'
         ENDIF
      ELSE
         CALL IGNORE ('S')
      ENDIF

      ORQTMP%OQR(1) = -1.
      ORQTMP%OQR(2) = -1.
      ORQTMP%RQTYPE = RTYPE
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
      NREF = 0
!       --- append node number to FILENM in case of
!           parallel computing
      IF ( PARLL ) THEN
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      ORQTMP%PSNAME = PSNAME
      ORQTMP%OQI(1) = NREF
      ORQTMP%OQI(2) = NREOQ
      OUTP_FILES(NREOQ) = FILENM
!NCF!
!NCF      CALL INKEYW ('STA', ' ')
!NCF!       declare monthly netCDF file
!NCF!       store in oqi(4) differ from idla of block!
!NCF      ORQTMP%OQI(4) = 0
!NCF      IF (KEYWIS('MON')) THEN
!NCF         ORQTMP%OQI(4) = ORQTMP%OQI(4) + 1
!NCF      ENDIF
!NCF      CALL INKEYW ('STA', ' ')
!NCF      IF (KEYWIS('ESCA')) THEN
!NCF         ORQTMP%OQI(4) = ORQTMP%OQI(4) + 2
!NCF      ENDIF
!NCF      CALL INKEYW ('STA', ' ')
!NCF      IF (KEYWIS('COMP')) THEN
!NCF         ORQTMP%OQI(4) = ORQTMP%OQI(4) + 4
!NCF      ENDIF
!NCF      CALL INKEYW ('STA', ' ')
!NCF      IF (KEYWIS('MDGRID')) THEN
!NCF         ORQTMP%OQI(4) = ORQTMP%OQI(4) + 8
!NCF      ENDIF
!NCF      CALL INKEYW ('STA', ' ')
!NCF      IF (KEYWIS('NOAUX')) THEN
!NCF         ORQTMP%OQI(4) = ORQTMP%OQI(4) + 16
!NCF      ENDIF
!NCF      CALL INKEYW ('STA', ' ')

      NVAR = 0
      ORQTMP%OQI(3) = NVAR
      ALLOCATE(ORQTMP%IVTYP(0))
      ALLOCATE(ORQTMP%FAC(0))
!       read types of variables to be printed in the table
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS ('OUT')) THEN
         IF (NSTATM.EQ.0) CALL MSGERR (3,&
         &'time information not allowed in stationary mode')
         NSTATM = 1
         CALL INCTIM (ITMOPT, 'TBEG', ORQTMP%OQR(1), 'REQ', 0D0)
         CALL INITVD ('DELT', ORQTMP%OQR(2), 'REQ', 0D0)
         IF (NSTATM.EQ.0) CALL MSGERR (2,&
         &'time input not allowed in stationary mode')
      ENDIF
      NULLIFY(ORQTMP%NEXTORQ)
      IF ( .NOT.LORQ ) THEN
         FORQ = ORQTMP
         CORQ => FORQ
         LORQ = .TRUE.
      ELSE
         CORQ%NEXTORQ => ORQTMP
         CORQ => ORQTMP
      END IF
      FOUND = .TRUE.
      RETURN
   END IF

!   --------------------------------------------------------------------
!   NESTout 'sname'  'fname'
!             (OUTPUT [tbegnst] [deltnst] SEC/MIN/HR/DAY)
!   --------------------------------------------------------------------
!   NEST   output for nesting of models                        VER.

   IF (KEYWIS ('NEST')) THEN

!      =================================================================
!
!       NESTout  'sname'  'fname'  &
!
!                                              | -> Sec  |
!                OUTput  [tbegnst]  [deltnst] <     MIn   >
!                                              |    HR   |
!                                              |    DAy  |
!
!      =================================================================

      IF (ONED) THEN
         CALL MSGERR (2,' Illegal keyword (NEST) in'//&
         &' combination with 1D-computation')
         FOUND = .TRUE.
         RETURN
      ELSE
         CALL SWNMPS (PSNAME, STYPE, MIP, IERR)
         IF (IERR.NE.0) THEN
            FOUND = .TRUE.
            RETURN
         END IF
         IF (STYPE .NE. 'N') THEN
            CALL MSGERR(2,'Set of output locations is not correct type')
            FOUND = .TRUE.
            RETURN
         ENDIF
!         output points exist
         ALLOCATE(ORQTMP)
         NREOQ = NREOQ + 1
         IF (NREOQ.GT.MAX_OUTP_REQ) CALL MSGERR (2,&
         &'too many output requests')
         ORQTMP%OQR(1) = -1.
         ORQTMP%OQR(2) = -1.
         RTYPE  = 'SPRC'
         ORQTMP%RQTYPE = RTYPE
         CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
         NREF = 0
!         --- append node number to FILENM in case of
!             parallel computing
         IF ( PARLL ) THEN
            ILPOS = INDEX ( FILENM, ' ' )-1
            WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
         END IF
         ORQTMP%PSNAME = PSNAME
         ORQTMP%OQI(1) = NREF
         ORQTMP%OQI(2) = NREOQ
         OUTP_FILES(NREOQ) = FILENM
         NVAR = 0
         ORQTMP%OQI(3) = NVAR
!NCF!         scale spectra and do not store auxillary variables
!NCF         IF ( INDEX( FILENM, '.NC'  ).NE.0 .OR.&
!NCF         &INDEX (FILENM, '.nc'  ).NE.0 )&
!NCF         &ORQTMP%OQI(4) = 18
         ALLOCATE(ORQTMP%IVTYP(0))
         ALLOCATE(ORQTMP%FAC(0))

         CALL INKEYW ('STA', ' ')
         IF (KEYWIS ('OUT')) THEN
            IF (NSTATM.EQ.0) CALL MSGERR (3,&
            &'time information not allowed in stationary mode')
            NSTATM = 1
            CALL INCTIM (ITMOPT, 'TBEG', ORQTMP%OQR(1), 'REQ', 0D0)
            CALL INITVD ('DELT', ORQTMP%OQR(2), 'REQ', 0D0)
            IF (NSTATM.EQ.0) CALL MSGERR (2,&
            &'time input not allowed in stationary mode')
         ENDIF

         NULLIFY(ORQTMP%NEXTORQ)
         IF ( .NOT.LORQ ) THEN
            FORQ = ORQTMP
            CORQ => FORQ
            LORQ = .TRUE.
         ELSE
            CORQ%NEXTORQ => ORQTMP
            CORQ => ORQTMP
         END IF
         FOUND = .TRUE.
         RETURN
      ENDIF
   ENDIF
!     -------------------------------------------------------
!     command not found:
   RETURN
FOUND = .TRUE.
   RETURN
!*    end of subroutine SWREOQ  **
end subroutine SWREOQ
!************************************************************************
!                                                                      *
INTEGER FUNCTION SIRAY (DP, XP1, YP1, XP2, YP2, XX, YY, BOTDEP,&
&BOTLEV, WATLEV)
   USE swan_service_interfaces, ONLY: EQREAL, STRACE
   USE swan_input_interpolation, ONLY: SVALQI
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
!
!  0. AUTHORS
!
!     30.72: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. UPDATE
!
!     00.00, Mar. 87: heading added, name of routine changed from
!                     IRAAI in SIRAY
!     30.72, Oct. 97: logical function EQREAL introduced for floating point
!                     comparisons
!     30.70, Nov. 97: changed into INTEGER function
!                     test output added
!                     arguments BOTDEP, BOTLEV, WATLEV added
!     40.03, Nov. 99: X2= etc. moved out of IF-ENDIF group
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Searching the first point on a ray where the depth is DP
!
!  3. METHOD
!
!     ---
!
!  4. PARAMETERLIST
!
!     DP      REAL   input    depth
!     XP1     REAL   input    X-coordinate start point of ray
!     YP1     REAL   input    Y-coordinate start point of ray
!     XP2     REAL   input    X-coordinate end point of ray
!     YP2     REAL   input    Y-coordinate end point of ray
!     XX      REAL   input    X-coordinate point with depth DP
!     YY      REAL   input    Y-coordinate point with depth DP
!
!  5. SUBROUTINES CALLING
!
!     SWREPS (SWAN/READ)
!
!  6. SUBROUTINES USED
!
!     SVALQI (SWAN/READ)
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
!     ----------------------------------------------------------------
!     Give SIRAY initial value 0
!     Compute stepsize, raylength and number of steps along the ray
!     Compute bottom coordinates  of startpoint as number of meshes
!     Call SVALQI to interpolate depth in startpoint of ray
!     For every step along the ray do
!         Compute coordinates of the intermediate point in problem
!           grid and bottom grid
!         Call SVALQI to interpolate the depth for this point
!         If the required depth is in the interval, then
!             Compute coordinates of the point with depth DP
!             Set SIRAY 1
!         Else
!             Coordinates and depth at start of new interval are values
!               at end of old interval
!     ----------------------------------------------------------------
!  10. SOURCE TEXT

      LOGICAL :: BOTDEP
   REAL      BOTLEV(*), WATLEV(*)
   INTEGER, SAVE :: IENT = 0
   INTEGER   JDMINMAX, JJ, NSTEP
   REAL      DP, XP1, YP1, XP2, YP2, XX, YY
   REAL      D2, D3, DIFDEP, DSTEP, RAYLEN, X2, X3, Y2, Y3
   CALL STRACE (IENT,'SIRAY')

   SIRAY   = 0
   DIFDEP = 1e+10
   DSTEP  = MIN(DXG(1), DYG(1))
   RAYLEN = SQRT ((XP2-XP1)*(XP2-XP1) + (YP2-YP1)*(YP2-YP1))
   NSTEP  = 1 + INT(1.5*RAYLEN/DSTEP + 0.5)

   do JJ = 0, NSTEP
      X3  = XP1 + REAL(JJ)*(XP2-XP1)/REAL(NSTEP)
      Y3  = YP1 + REAL(JJ)*(YP2-YP1)/REAL(NSTEP)
      IF (BOTDEP) THEN
         D3   = SVALQI (X3, Y3, 1, BOTLEV, 1, 0, 0)
      ELSE
         D3   = SVALQI (X3, Y3, 1, BOTLEV, 1, 0, 0) + WLEV
         IF (LEDS(7).GE.2)&
         &D3 = D3 + SVALQI (X3, Y3, 7, WATLEV, 1, 0, 0)
      ENDIF
      IF (ITEST.GE.160) WRITE (PRTEST, "(' SIRAY, scan point', 2(1X,F8.0), 1X, F8.2)") X3+XOFFS, Y3+YOFFS, D3
      IF (ABS(D3-DP).LT.DIFDEP) THEN
         DIFDEP=ABS(D3-DP)
         JDMINMAX=JJ
      ENDIF
      IF (JJ.GT.0) THEN
         IF ((DP-D2)*(DP-D3).LE.0) THEN
            IF (EQREAL(D2,D3)) THEN
               XX = X2
               YY = Y2
            ELSE
               XX = X2+(X3-X2)*(D2-DP)/(D2-D3)
               YY = Y2+(Y3-Y2)*(D2-DP)/(D2-D3)
            ENDIF
            SIRAY = 1
            EXIT
         ENDIF
      ENDIF
      X2 = X3
      Y2 = Y3
      D2 = D3
   end do

!     exact depth not found, take closest value:

   IF (SIRAY == 0) THEN
      X3 = XP1 + REAL(JDMINMAX)*(XP2-XP1)/REAL(NSTEP)
      Y3 = YP1 + REAL(JDMINMAX)*(YP2-YP1)/REAL(NSTEP)
      XX = X3
      YY = Y3
   END IF

   IF (ITEST.GE.140) WRITE (PRTEST, "(' SIRAY, result ', 2(1X,F8.0))") XX+XOFFS, YY+YOFFS
   RETURN
! * end of function SIRAY *
end function SIRAY
!************************************************************************

SUBROUTINE SWNMPS (PSNAME, PSTYPE, MIP, IERR)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_input_parser, ONLY: INCSTR

!************************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM4
   USE SWCOMM1
   USE OUTP_DATA


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
!       Oct. 1996, ver. 30.50: new subr.
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!       Read name of set of output points; get type and number of
!       points in the set
!
!  3. METHOD
!
!
!  4. PARAMETERLIST
!
!       PSNAME   char   output   name
!       PSTYPE   char   output   type
!       MIP      int    output   number of points
!
!  5. SUBROUTINES CALLING
!
!       SPREOQ
!
!  6. SUBROUTINES USED
!
!       INCSTR (Ocean Pack)
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
!
! 10. SOURCE TEXT

   INTEGER, SAVE :: IENT = 0
   INTEGER   IERR, MIP
   CHARACTER(LEN=*) :: PSNAME
   CHARACTER(LEN=1) :: PSTYPE
   TYPE(OPSDAT), POINTER :: CUOPS
   CALL STRACE (IENT,'SWNMPS')

   IERR = 0
   CALL INCSTR ('SNAME', PSNAME, 'STA', 'BOTTGRID')
   IF (default_command_reader%LENCST.GT.8) CALL MSGERR (2, 'SNAME is too long')
   CUOPS => FOPS
   DO
      IF (CUOPS%PSNAME.EQ.PSNAME) EXIT
      IF (.NOT.ASSOCIATED(CUOPS%NEXTOPS)) THEN
         CALL MSGERR(2, 'Set of output locations is not known')
         PSTYPE = ' '
         MIP    = 0
         IERR   = 1
         RETURN
      END IF
      CUOPS => CUOPS%NEXTOPS
   END DO
   PSTYPE = CUOPS%PSTYPE
   IF (PSTYPE.EQ.'F' .OR. PSTYPE.EQ.'H') THEN
      MIP = CUOPS%OPI(1) * CUOPS%OPI(2)
!        get direction of frame in case of coordinates plotting
      ALPQ = CUOPS%OPR(5)
   ELSE
      MIP  = CUOPS%MIP
      ALPQ = 0.
   ENDIF
IF (ITEST.GE.100) WRITE (PRTEST, "(' exit SWNMPS, name:', A8, ' type:', A1, ' num of p:', I5)") PSNAME, PSTYPE, MIP
   RETURN
   RETURN
end subroutine SWNMPS
!************************************************************************

SUBROUTINE SVARTP (IVTYPE)
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_input_parser, ONLY: INKEYW, KEYWIS, WRNKEY

!************************************************************************

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
!
!     32.02: Roeland Ris & Cor van der Schelde
!     40.03: Nico Booij
!     40.04: Annette Kieftenburg
!
!  1. Updates
!
!     10.09, Aug. 94: output quantity RPER added
!     20.61, Sep. 95: quantities TM02 and FWID added
!     20.67, Dec. 95: FWID renamed FSPR (freq. spread)
!     32.02, Feb. 98: 1D-version introduced
!     40.00, Apr. 98: subr simplified using new array OVKEYW
!     40.03, Sep. 00: inconsistency with manual corrected
!
!  2. Purpose
!
!     Converting keyword into integer
!
!  3. Method
!
!     This subroutine determines an integer value indicating the
!     required output variable from the keyword denoting the same
!     for storage in array with output requests.
!
!  4. PARAMETERLIST
!
!     IVTYPE  INT    output   type number output variable
!
!  5. Subroutines calling
!
!     SPROUT
!
!  6. Subroutines used
!
!     KEYWIS (Ocean Pack)
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
!     -----------------------------------------------------------------
!     If the keyword is equal to given string, then
!         IVTYPE is given integer value
!     -----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   INTEGER IVT, IVTYPE
   CALL STRACE (IENT,'SVARTP')

   IVTYPE  =  0

   CALL INKEYW ('STA', 'ZZZZ')
!     check if given keyword corresponds to output quantity
   DO IVT = NMOVAR, 1, -1
!       loop in reverse order to check more specific names first
!       e.g. HSWE before HS
      IF (KEYWIS (OVKEYW(IVT))) THEN
         IVTYPE = IVT
         EXIT
      ENDIF
   ENDDO
!     aliases:
   IF (IVTYPE == 0) THEN
      IF (KEYWIS ('PPER')) IVTYPE = 12
      IF (KEYWIS ('RPER')) IVTYPE = 28
      IF (KEYWIS ( 'DTM')) IVTYPE = 31
      IF (KEYWIS ('FWID')) IVTYPE = 33
!     keyword OUTPUT means that output times will be entered
      IF (KEYWIS ('OUT')) IVTYPE = 98
!     keyword ZZZZ means end of list of output quantities
      IF (KEYWIS ('ZZZZ')) IVTYPE = 999
   END IF

   IF (IVTYPE .EQ. 0) CALL WRNKEY

!     end of subroutine SVARTP *
end subroutine SVARTP
!************************************************************************

SUBROUTINE SWBOUN ( XCGRID, YCGRID, KGRPNT, XYTST, KGRBND )
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_services, ONLY: CVMESH
   USE swan_service_interfaces, ONLY: EQREAL, MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, IGNORE, ININTG, INKEYW, INREAL, KEYWIS, WRNKEY

!************************************************************************

   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanGriddata
   USE SwanGridobjects
   USE SwanCompdata
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
!     30.73: Nico Booij
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     34.01: Jeroen Adema
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.05: Ekaterini E. Kriezi
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.92: Marcel Zijlema
!     41.14: Nico Booij
!     43.01: Marcel Zijlema
!
!  1. Updates
!
!     30.73, Nov. 97: New subroutine, replacing code in subr. SWREAD (file SWANPRE1)
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description several arrays
!     30.81, Nov. 98: Adjustment for 1-D case of new boundary conditions
!     34.01, Feb. 99: Introducing STPNOW
!     30.81, Apr. 99: Prevent negative powers for cosine directional spreading (DSPR);
!                     prevented DSPR > 360 and DSPR < 0 (except for exception value).
!     30.82, July 99: Used EQREAL for real equality comparisons
!     40.05, Aug  00: WW3 boundary nesting command, in Swan nesting option
!                     adding of a new option (same as WW3 command)
!     40.03, Sep. 00: inconsistency with manual corrected
!     40.02, Oct. 00: WWIII added as keyword (will appear in the manual)
!     40.13, Nov. 01: determination of side corrected (iside=3)
!     40.31, Nov. 03: removing POOL-mechanism, reconsideration of this subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Jun. 07: extension to unstructured grids
!     40.92, Jun. 08: changes with respect to boundary polygons
!     41.14, Jul. 10: call SwanBndStruc added
!     43.01, Aug. 24: parallelization of unstructured boundaries and their conditions
!
!  2. Purpose
!
!     Reading and processing BOUNDARY command
!
!  3. Method
!
!
!  4. Argument variables
!
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     KGRBND  int   inp    grid indices of boundary points
!     KGRPNT  int   inp    indirect addresses of grid points
!     XYTST   int   inp    ix, iy of test points

   INTEGER KGRPNT(MXC,MYC)
   INTEGER XYTST(*),  KGRBND(*)

!  5. Parameter variables
!
!
!  6. Local variables

   INTEGER, SAVE :: IENT = 0
   INTEGER   KOUNTR,IX1,IY1,IX2,IY2
   INTEGER   MM,IX,IY,ISIDM,ISIDE,KC,KC2,KC1,IX3,IY3,MP
   INTEGER   IP,II,NBSPSS,NFSEQ,IKO,IKO2,IBSPC1,IBSPC2
   INTEGER   VM
   INTEGER   IERR, IXB1, IXB2, IXI, IPP, ISH, JBG
   INTEGER   IXG, IXG1, IXG2, ITMP(1), K, NB

   INTEGER, DIMENSION(:), ALLOCATABLE :: IARR1, IARR2

   REAL      CRDP, CRDM, SOMX, SOMY
   REAL      XP,YP,XC,YC,RR,DIRSI,COSDIR,SINDIR,DIRSID,DIRREF
   REAL      RLEN1,RDIST,RLEN2,XC1,YC1,XC2,YC2,W1
   REAL      DET, DXLOC, DYLOC, X1, Y1, X2, Y2, X3, Y3

      LOGICAL :: LOCGRI, CCW, BPARF, DONALL
   LOGICAL   LFRST1, LFRST2, LFRST3
   LOGICAL, SAVE :: BNDDONE = .FALSE.
!  (local LOGICAL declaration removed: SwanPointinMesh is now a module function)

   INTEGER   NUMP

   TYPE(BSPCDAT), POINTER :: BFLTMP
   TYPE(BSPCDAT), SAVE, POINTER :: CUBFL

   TYPE(BSDAT), POINTER :: BSTMP
   TYPE(BSDAT), SAVE, POINTER :: CUBS

   TYPE(BGPDAT), POINTER :: BGPTMP

   TYPE XYPT
      INTEGER             :: JX, JY
      TYPE(XYPT), POINTER :: NEXTXY
   end type XYPT

   TYPE(XYPT), TARGET  :: FRST
   TYPE(XYPT), POINTER :: CURR, TMP

   CHARACTER(80) :: MSGSTR

   TYPE(verttype), DIMENSION(:), POINTER :: vert
   TYPE(facetype), DIMENSION(:), POINTER :: face

!  8. Subroutines used
!
!       Ocean Pack command reading routines
!       BOUNPT


!  9. Subroutines calling
!
!       SWREAD
!
!  10. Error messages
!
!
!  11. Remarks
!
!       data concerning boundary files are stored in array BFILES
!       see subr BCFILE for details
!
!  12. Structure
!
!       ----------------------------------------------------------------
!       Read keyword
!       Case keyword =
!       'SHAPE': Read spectral shape parameters
!       'WAMN': Read filename
!               Open WAM nesting file
!               Put file characteristics into array BFILES
!       'WW3N': Read filename
!               Open WW3 nesting file
!               Put file characteristics into array BFILES
!       'NEST': Read filename
!               Call BCFILE to obtain file characteristics
!       'SIDE': read side of boundary
!       'SEG':  read set of points on boundary of comp. grid
!               Read keyword
!               If keyword is 'UNIF'
!               Then Read keyword
!                    If keyword is 'PAR'
!                    Then read integral wave parameters
!                         Call SSHAPE to generate spectrum
!                         put spectrum into array BSPECS
!                    Else {keyword is 'FILE'}
!                         Read filename
!                         Call BCFILE to obtain file characteristics
!               Else {keyword is 'VAR'}
!                    Read keyword
!                    If keyword is 'PAR'
!                    Then Repeat until list is exhausted
!                             Read length and integral wave parameters
!                             Call SSHAPE to generate spectrum
!                             put spectrum into array BSPECS
!                    Else {keyword is 'FILE'}
!                         Repeat until list is exhausted
!                             Read filename
!                             Call BCFILE to obtain file characteristics
!       ----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE (IENT,'SWBOUN')

!     point to vertex and face objects

   vert => gridobject%vert_grid
   face => gridobject%face_grid

   IF (.NOT.BNDDONE) THEN
      IF (OPTG.EQ.5) THEN

!           in case of unstructured grid, make list of boundary points
!           in ascending order

         CALL SwanBpntlist
         IF (STPNOW()) RETURN
!METIS!
!METIS!           next, gather the lists of boundary points to all processes
!METIS!
!METIS         CALL SwanCollBpntlist
!METIS         IF (STPNOW()) RETURN

         IF (ITEST.GE.50.AND.IAMMASTER) THEN
            NB = SIZE(blist,1)
            K  = SIZE(blist,2)
            IF (.NOT.PARLL) K = 1
            WRITE(PRTEST,*)&
            &'test BLIST of the sea/mainland boundary '
            KOUNTR = 0
            DO IPP = 1, K
               DO IP = 1, NB
                  II = blist(IP,IPP)
                  IF (.NOT.PARLL) THEN
                     VM = vmark(II)
                  ELSE
                     VM = bmark(IP,IPP)
                  ENDIF
                  IF ( II.GT.0 ) THEN
                     KOUNTR = KOUNTR + 1
                     WRITE(PRTEST,'(A,2I7,2F18.9,I7)')&
                     &' I, BLIST(I), X, Y, bound marker ',&
                     &KOUNTR, II,&
                     &xcugrdgl(II)+XOFFS,&
                     &ycugrdgl(II)+YOFFS,&
                     &VM
                  ENDIF
               ENDDO
            ENDDO
         ENDIF

!           store global indices of boundary vertices in own subdomain

         IF ( PARLL ) THEN
            NB = nverts - count(mask=vmark==0 .or. vmark>=excmark)
            ALLOCATE(bvertg(NB,2))
            K = 0
            DO IP = 1, nverts
               IF ( vmark(IP).NE.0 .AND. vmark(IP).LT.excmark ) THEN
                  K = K + 1
                  bvertg(K,1) = IP
                  bvertg(K,2) = ivertg(IP)
               ENDIF
            ENDDO
         ELSE
            NB = nverts - count(mask=vmark==0)
            ALLOCATE(bvertg(NB,2))
            K = 0
            DO IP = 1, nverts
               IF ( vmark(IP).NE.0 ) THEN
                  K = K + 1
                  bvertg(K,1) = IP
                  bvertg(K,2) = IP
               ENDIF
            ENDDO
         ENDIF

      ELSE

!           generate output curves BOUNDARY and BOUND_** for structured

         CALL SwanBndStruc ( XCGRID, YCGRID )
      ENDIF
      BNDDONE = .TRUE.
   ENDIF

   CALL INKEYW ('REQ',' ')
   IF (KEYWIS ('SHAP')) THEN

!           specification of the spectral shape
!
! ======================================================================
!
!                      |  JONswap  [gamma]  |
!                      |                    |
!  BOUndspec  SHAPe   <   PM                |
!                      |                    |    | -> PEAK |
!                      |  GAUSs  [sigfr]     >  <           >   &
!                      |                    |    | MEAN    |
!                      |  BIN               |
!                      |                    |
!                      |  TMA  [gamma] [d]  |
!
!                     | DEGRees   |
!             DSPR   <             >
!                     | -> POWer  |
!
! ======================================================================

      CALL INKEYW ('STA', 'JON')
      IF (KEYWIS ('JON')) THEN
         FSHAPE = 2
         CALL INREAL ('GAMMA', PSHAPE(1), 'STA', 3.3)
      ELSE IF (KEYWIS ('BIN')) THEN
         FSHAPE = 3
      ELSE IF (KEYWIS ('PM')) THEN
         FSHAPE = 1
      ELSE IF (KEYWIS ('GAUS')) THEN
         FSHAPE = 4
         CALL INREAL ('SIGFR', SIGMAG, 'STA', 0.01)
!         convert from Hz to rad/s:
         PSHAPE(2) = PI2 * SIGMAG
      ELSE IF (KEYWIS ('TMA')) THEN
         FSHAPE = 5
         CALL INREAL ('GAMMA', PSHAPE(1), 'STA', 3.3)
         CALL INREAL ('D'    , PSHAPE(3), 'REQ', 0. )
      ENDIF
!       PEAK or MEAN frequency
      CALL INKEYW ('STA', ' ')
      IF (KEYWIS('MEAN')) THEN
         FSHAPE = -FSHAPE
      ELSE
         CALL IGNORE ('PEAK')
      ENDIF
!       directional distribution given by DEGR or by POWER
      CALL IGNORE ('DSPR')
      CALL INKEYW ('STA', 'POW')
      IF (KEYWIS('DEGR')) THEN
         DSHAPE = 1
      ELSE
         CALL IGNORE ('POW')
         DSHAPE = 2
      ENDIF
      IF (ITEST.GE.30) WRITE (PRINTF,"(' Shape of inc. spectrum, Freq:', I2, ' ; Dir:', I2)") FSHAPE, DSHAPE

   ELSE IF (KEYWIS ('WAMN')) THEN

!       SWAN in WAM nesting
!
!      =================================================================
!
!                                                   |-> CRAY |
!                                    | UNFormatted <          > |
!                                    |              | WKstat |  |
!                                    |                          |
!       BOUndnest2  WAMNest 'fname' <                            > [xgc] [ygc] [lwdate]
!                                    |                          |
!                                    | FREE                     |
!
!      =================================================================

      IF (MXC .LE. 0 .AND. OPTG.NE.5) THEN
         CALL MSGERR(3, ' command CGRID must precede this command')
         RETURN
      ENDIF
      IF (MCGRD .LE. 1 .AND. nverts .LE. 0) THEN
         CALL MSGERR(3,&
         &' command READ BOT or READ UNSTRUC must precede this command')
         RETURN
      ENDIF
      IF (.NOT.ALOBND) THEN
         NBGRPT = 0
         NBSPEC = 0
         NBFILS = 0
         NBGGL  = 0
         ALOBND = .TRUE.
      ENDIF

      IF (OPTG.EQ.5) THEN
         CALL MSGERR(2,&
         &' WAM b.c. are not supported in unstructured grid')
         RETURN
      ENDIF

      NBFILS = NBFILS + 1
      ALLOCATE(BFLTMP)
      CALL INCSTR ('FNAME',FILENM,'REQ', ' ')
      CALL BCWAMN (FILENM, 'NEST', BFLTMP,&
      &XCGRID, YCGRID, KGRPNT, XYTST)
      IF (STPNOW()) RETURN
      NULLIFY(BFLTMP%NEXTBSPC)
      IF ( .NOT.LBFILS ) THEN
         FBNDFIL = BFLTMP
         CUBFL => FBNDFIL
         LBFILS = .TRUE.
      ELSE
         CUBFL%NEXTBSPC => BFLTMP
         CUBFL => BFLTMP
      END IF

   ELSE IF (KEYWIS('WW3').OR.KEYWIS('WWIII')) THEN

!       SWAN in WaveWatch nesting
!
!      =================================================================
!                                | UNFormatted |   | -> CLOS |
!       BOUndnest3  WW3 'fname' <               > <           > [xgc] [ygc]
!                                | FREe        |   |    OPEN |
!      =================================================================

      IF (MXC .LE. 0 .AND. OPTG.NE.5) THEN
         CALL MSGERR(3, ' command CGRID must precede this command')
         RETURN
      ENDIF
      IF (MCGRD .LE. 1 .AND. nverts .LE. 0) THEN
         CALL MSGERR(3,&
         &' command READ BOT or READ UNSTRUC must precede this command')
         RETURN
      ENDIF
      IF (.NOT.ALOBND) THEN
         NBGRPT = 0
         NBSPEC = 0
         NBFILS = 0
         NBGGL  = 0
         ALOBND = .TRUE.
      ENDIF

      IF (OPTG.EQ.5) THEN
         CALL MSGERR(2,&
         &' WWIII b.c. are not supported in unstructured grid')
         RETURN
      ENDIF

      NBFILS = NBFILS + 1
      ALLOCATE(BFLTMP)
      CALL INCSTR ('FNAME',FILENM,'REQ', ' ')
      CALL BCWW3N (FILENM, 'NEST', BFLTMP,&
      &XCGRID, YCGRID, KGRPNT, XYTST, KGRBND)
      IF (STPNOW()) RETURN
      NULLIFY(BFLTMP%NEXTBSPC)
      IF ( .NOT.LBFILS ) THEN
         FBNDFIL = BFLTMP
         CUBFL => FBNDFIL
         LBFILS = .TRUE.
      ELSE
         CUBFL%NEXTBSPC => BFLTMP
         CUBFL => BFLTMP
      END IF

   ELSE IF (KEYWIS ('NE')) THEN

!       Nesting SWAN model in larger SWAN model
! ==========================================
!                                | -> CLOS |
!     BOUndnest1  NEst 'fname'  <           >
!                                |  OPEN   |
! ==========================================

      IF (MXC .LE. 0 .AND. OPTG.NE.5) THEN
         CALL MSGERR(3, ' command CGRID must precede this command')
         RETURN
      ENDIF
      IF (MCGRD .LE. 1 .AND. nverts .LE. 0) THEN
         CALL MSGERR(3,&
         &' command READ BOT or READ UNSTRUC must precede this command')
         RETURN
      ENDIF
      IF (.NOT.ALOBND) THEN
         NBGRPT = 0
         NBSPEC = 0
         NBFILS = 0
         NBGGL  = 0
         ALOBND = .TRUE.
      ENDIF

      NBFILS = NBFILS + 1
      ALLOCATE(BFLTMP)
      CALL INCSTR ('FNAME',FILENM,'REQ', ' ')

!       if keyword is OPEN then
!          DONALL is TRUE  and the nesting boundary remain open
!       else (default case)
!          DONALL is FALSE  and boundary is close and interpolation between
!          the last and the first point will be done

      CALL INKEYW ('STA', 'CLOS')
      IF (KEYWIS('OPEN')) THEN
         DONALL = .TRUE.
      ELSE IF (KEYWIS('CLOS')) THEN
         DONALL = .FALSE.
      ELSE
         CALL WRNKEY
      ENDIF

      CALL BCFILE (FILENM, 'NEST', BFLTMP,&
      &XCGRID, YCGRID, KGRPNT, XYTST,  KGRBND,&
      &DONALL)
      IF (STPNOW()) RETURN
      NULLIFY(BFLTMP%NEXTBSPC)
      IF ( .NOT.LBFILS ) THEN
         FBNDFIL = BFLTMP
         CUBFL => FBNDFIL
         LBFILS = .TRUE.
      ELSE
         CUBFL%NEXTBSPC => BFLTMP
         CUBFL => BFLTMP
      END IF

   ELSE

!       parametric or file boundary condition
!
!      =================================================================
!
!                             | North |
!                             | NW    |
!                             | West  |
!                             | SW    |          | -> CCW     |
!                 | -> SIDE  <  South  > | [k]  <              >   |
!                 |           | SE    |          | CLOCKWise  |    |
!                 |           | East  |                            |
!                 |           | NE    |                            |
!       BOUndary <                                                  >
!                 |           | -> XY  < [x] [y] >           |     |
!                 | SEGment  <                                >    |
!                             |    IJ  < [i] [j] > | < [k] > |
!
!
!                          |  PAR  [hs] [per] [dir] [dd]  |
!            |  UNIForm   <                                >
!            |             |  FILE  'fname'  [seq]        |
!           <
!            |             |  PAR  < [len] [hs] [per] [dir] [dd] >  |
!            |  VARiable  <                                          >
!                          |  FILE < [len] 'fname' [seq] >          |
!
!      =================================================================

      IF (MXC .LE. 0 .AND. OPTG.NE.5) THEN
         CALL MSGERR(3, ' command CGRID must precede this command')
         RETURN
      ENDIF
      IF (MCGRD .LE. 1 .AND. nverts .LE. 0) THEN
         CALL MSGERR(3,&
         &' command READ BOT or READ UNSTRUC must precede this command')
         RETURN
      ENDIF
      IF (.NOT.ALOBND) THEN
         NBGRPT = 0
         NBSPEC = 0
         NBFILS = 0
         NBGGL  = 0
         ALOBND = .TRUE.
      ENDIF

!       first define side or segment
!
!       *** definition of boundary segment ***

      CALL INKEYW ('REQ',' ')
      IF (KEYWIS ('STAT')) THEN
         CALL MSGERR (1, 'keyword STAT ignored')
         CALL INKEYW ('REQ',' ')
      ENDIF
      KOUNTR  = 0
      FRST%JX = 0
      FRST%JY = 0
      NULLIFY(FRST%NEXTXY)
      CURR => FRST
      IF (KEYWIS ('SEG')) THEN
         IERR = 0
         CALL INKEYW ('STA','XY')
         IF (KEYWIS('XY') .OR. KEYWIS ('LOC')) THEN
            LOCGRI = .TRUE.
         ELSE IF (KEYWIS('IJ') .OR. KEYWIS ('GRI')) THEN
            LOCGRI = .FALSE.
         ELSE
            CALL WRNKEY
         ENDIF
         IX1 = 1
         IY1 = 1
         LFRST1 = .TRUE.
!         loop over points describing the segment
         DO
            IF (LOCGRI) THEN
               CALL READXY ('XP','YP',XP,YP, 'REP', -1.E10, -1.E10)
               IF (XP.LT.-.9E10) EXIT
               IF (OPTG.NE.5) THEN
!             --- structured grid

                  CALL CVMESH (XP, YP, XC, YC, KGRPNT, XCGRID, YCGRID,&
                  &KGRBND)
                  IX2 = NINT(XC) + 1
                  IY2 = NINT(YC) + 1
                  IF (.NOT.BOUNPT(IX2,IY2,KGRPNT)) THEN
                     CALL MSGERR (2, 'invalid boundary point')
                     WRITE (PRTEST, "(' segment point ', 2F10.2, ' grid ', 2F8.2, 2I4)") XP+XOFFS, YP+YOFFS, XC, YC,&
                     &IX2, IY2
                  ENDIF
               ELSE
!             --- unstructured grid

                  CALL SwanFindPoint ( XP, YP, IX2 )

!                --- find global index of (XP,YP)

                  IF ( PARLL ) THEN
                     IF ( IX2.GT.0 ) THEN
                        IXG2 = ivertg(IX2)
                     ELSE
                        IXG2 = -999
                     ENDIF
                     CALL SWREDUCE( IXG2, 1, SWMAX )
                  ELSE
                     IXG2 = IX2
                  ENDIF
                  IF ( IXG2.LT.0 ) THEN
                     WRITE (MSGSTR, '(A,F12.4,A,F12.4,A)')&
                     &' Boundary point (',XP+XOFFS,',',YP+YOFFS,&
                     &') not part of computational grid'
                     CALL MSGERR( 2, TRIM(MSGSTR) )
                  ENDIF
                  IF ( IX2.GT.0 .AND. vert(IX2)%atti(VMARKER) /= 1 ) THEN
                     WRITE (MSGSTR, '(A,F12.4,A,F12.4,A)')&
                     &' Vertex (',XP+XOFFS,',',YP+YOFFS,&
                     &') is not a valid boundary point'
                     CALL MSGERR( 2, TRIM(MSGSTR) )
                  ENDIF
               ENDIF
            ELSE
               IF (OPTG.NE.5) THEN
                  CALL ININTG ('I' , IX2, 'REP', -1)
                  IF (IX2 .LT. 0) EXIT
                  CALL ININTG ('J' , IY2, 'REQ',  0)
                  IX2 = IX2 + 1
                  IY2 = IY2 + 1
               ELSE
                  CALL ININTG ('K' , IXG2, 'REP', -1)
                  IF (IXG2 .LT. 0) EXIT
                  IF (IXG2.LE.0 .OR. IXG2.GT.nvertsg) THEN
                     WRITE (MSGSTR,'(I4,A)') IXG2,&
                     &' is not a valid vertex index'
                     CALL MSGERR( 2, TRIM(MSGSTR) )
                  ENDIF
                  IF ( PARLL ) THEN
                     ITMP = MINLOC(ABS(ivertg-IXG2))
                     IX2  = ITMP(1)
                     IF ( ivertg(IX2) /= IXG2 ) IX2 = -1
                  ELSE
                     IX2 = IXG2
                  ENDIF
                  IF ( IX2.GT.0 .AND. vert(IX2)%atti(VMARKER) /= 1 ) THEN
                     WRITE (MSGSTR,'(A,I4,A)') ' Vertex with index ',IXG2,&
                     &' is not a valid boundary point'
                     CALL MSGERR( 2, TRIM(MSGSTR) )
                  ENDIF
               ENDIF
            ENDIF
            IF (ITEST.GE.80 .AND. OPTG.NE.5) WRITE (PRTEST, "(' segment point ', 2F10.2, ' grid ', 2F8.2, 2I4)")&
            &XCGRID(IX2,IY2)+XOFFS,&
            &YCGRID(IX2,IY2)+YOFFS, XC, YC, IX2-1, IY2-1

!           --- generate intermediate points on the segment
            IF ( OPTG.NE.5 ) THEN
!           --- structured grid

               IF (IX2 .GT. 0 .AND. IX2 .LE. MXC .AND.&
               &IY2 .GT. 0 .AND. IY2 .LE. MYC) THEN
                  IF (LFRST1) THEN
                     MM = 1
                     LFRST1 = .FALSE.
                  ELSE
                     MM = MAX (ABS(IX2-IX1), ABS(IY2-IY1))
                  ENDIF
                  DO IP = 1, MM
                     RR = REAL(IP) / REAL(MM)

                     IF (.NOT. ONED) THEN
                        IX = IX1 + NINT(RR*REAL(IX2-IX1))
                        IY = IY1 + NINT(RR*REAL(IY2-IY1))
                     ELSE
                        IX = IX1 + NINT(RR*REAL(IX2-IX1))
                        IY = IY1
                     END IF

                     IF (ITEST.GE.80) WRITE (PRTEST, *) ' b. point ',&
                     &RR, IX, IY
                     IF (KGRPNT(IX,IY) .GT. 1) THEN
                        KOUNTR = KOUNTR + 1
                        ALLOCATE(TMP)
                        TMP%JX = IX
                        TMP%JY = IY
                        NULLIFY(TMP%NEXTXY)
                        CURR%NEXTXY => TMP
                        CURR => TMP
                     ENDIF
                  ENDDO
               ELSE
                  MSGSTR =''
                  write (MSGSTR, "('(',2I5, ') is outside computational grid')") IX2-1, IY2-1
                  CALL MSGERR (2, MSGSTR)
               ENDIF
               IY1 = IY2
            ELSE
!           --- unstructured grid

               IF (LFRST1) THEN
                  IF ( IX2.GT.0 ) THEN
                     JBG = vert(IX2)%atti(BPOL)
                  ELSE
                     JBG = -1
                  ENDIF
                  CALL SWREDUCE( JBG, 1, SWMAX )
                  ITMP = MINLOC(ABS(blist(:,JBG)-IXG2))
                  IXB1 = ITMP(1)
                  DET  = 1.
                  LFRST1 = .FALSE.
               ELSE
                  IF ( IX1.GT.0 ) THEN
                     JBG = vert(IX1)%atti(BPOL)
                  ELSE
                     JBG = -1
                  ENDIF
                  CALL SWREDUCE( JBG, 1, SWMAX )
                  ITMP = MINLOC(ABS(blist(:,JBG)-IXG1))
                  IXB1 = ITMP(1)

!                 1) the wave spectrum along the given segment can be
!                    imposed in counterclockwise or clockwise direction
!                 2) content of array blist is ordered in counterclockwise
!                    manner for sea/mainland boundary (JBG=1) and
!                    clockwise for island boundary (JBG>1)
!                 3) therefore, determine orientation by means of the
!                    determinant of two endpoints of the given segment
!                    and an arbitrary point inside domain
!
!                 first endpoint of segment
                  X1 = xcugrdgl(IXG1)
                  Y1 = ycugrdgl(IXG1)

!                 second endpoint of segment
                  X2 = xcugrdgl(IXG2)
                  Y2 = ycugrdgl(IXG2)

!                 an arbitrary internal point
                  DXLOC = mingsiz
                  CALL SWREDUCE ( DXLOC, 1, SWMIN )
                  DYLOC = DXLOC
                  DO IP = 1, 4
                     X3 = X1 - DXLOC
                     Y3 = Y1 - DYLOC
                     IF ( SwanPointinMesh( X3, Y3 ) ) THEN
                        IXG =  1
                     ELSE
                        IXG = -1
                     ENDIF
                     CALL SWREDUCE( IXG, 1, SWMAX )
                     IF ( JBG.GT.1 .AND. IXG.LT.0 ) THEN
                        X3 = X1 + DXLOC
                        Y3 = Y1 + DYLOC
                        EXIT
                     ELSEIF ( JBG.EQ.1 .AND. IXG.GT.0 ) THEN
                        EXIT
                     ENDIF
                     IF ( MOD(IP,2).EQ.0 ) THEN
                        DXLOC = -DXLOC
                     ELSE
                        DYLOC = -DYLOC
                     ENDIF
                  ENDDO

                  DET= (Y3-Y1)*(X2-X1)-(Y2-Y1)*(X3-X1)
                  IF (DET.GT.0.) THEN
!                    take next boundary point in counterclockwise
!                    direction
                     IXB1 = MOD(IXB1,nbpt(JBG))+1
                  ELSE
!                    take next boundary point in clockwise direction
                     IXB1 = nbpt(JBG)-MOD(nbpt(JBG)+1-IXB1,nbpt(JBG))
                  ENDIF
               ENDIF
               ITMP = MINLOC(ABS(blist(:,JBG)-IXG2))
               IXB2 = ITMP(1)

!              determine order of counting
               IF (IXB1.GT.IXB2 ) THEN
                  IF (DET.LT.0.) THEN
                     IXI  = -1
                  ELSE
                     IXI  = 1
                     IXB2 = IXB2+nbpt(JBG)
                  ENDIF
               ELSE
                  IF (DET.GT.0.) THEN
                     IXI  = 1
                  ELSE
                     IXI  = -1
                     IXB1 = IXB1+nbpt(JBG)
                  ENDIF
               ENDIF

               DO IPP = IXB1, IXB2, IXI
                  IP = MOD(IPP,nbpt(JBG))
                  IF (IP.EQ.0) IP = nbpt(JBG)
                  IXG  = blist(IP,JBG)
                  ITMP = MINLOC(ABS(bvertg(:,2)-IXG))
                  K    = ITMP(1)
                  IF ( bvertg(K,2) == IXG ) THEN
                     IX = bvertg(K,1)
                     vert(IX)%atti(VBC) = 1
                  ENDIF
                  KOUNTR = KOUNTR + 1
                  ALLOCATE(TMP)
                  TMP%JX = IXG
                  NULLIFY(TMP%NEXTXY)
                  CURR%NEXTXY => TMP
                  CURR => TMP
               ENDDO
            ENDIF
            IX1  = IX2
            IXG1 = IXG2
         END DO
         IF (KOUNTR.EQ.0)&
         &CALL MSGERR(1,'No points on the boundaries found')
         IF (KOUNTR.EQ.1) CALL MSGERR (1,&
         &'At least two points needed for a segment')
      ELSE
!         boundary condition on one side of the computational grid
         IF (OPTG.EQ.3) THEN
            CALL MSGERR(2,&
            &' keyword SIDE should not be used for curvilinear grid')
         END IF
         CALL IGNORE ('SIDE')
!         *** specification of side for which boundary   ***
!         *** condition is given                         ***
         IF (OPTG.NE.5) THEN
            CALL INKEYW ('REQ',' ')
            IF (KEYWIS ('NW')) THEN
               DIRSI = 45.
            ELSE IF (KEYWIS ('SW')) THEN
               DIRSI = 135.
            ELSE IF (KEYWIS ('SE')) THEN
               DIRSI = -135.
            ELSE IF (KEYWIS ('NE')) THEN
               DIRSI = -45.
            ELSE IF (KEYWIS ('N')) THEN
               DIRSI = 0.
            ELSE IF (KEYWIS ('W')) THEN
               DIRSI = 90.
            ELSE IF (KEYWIS ('S')) THEN
               DIRSI = 180.
            ELSE IF (KEYWIS ('E')) THEN
               DIRSI = -90.
            ELSE
               CALL WRNKEY
            ENDIF
         ELSE
            CALL ININTG ('K', VM, 'REQ', 0)
         ENDIF

!         --- go along boundary clockwise or counterclockwise (default)

         CALL INKEYW ('STA', 'CCW')
         IF (KEYWIS('CLOCKW')) THEN
            CCW = .FALSE.
         ELSE
            CALL IGNORE ('CCW')
            CCW = .TRUE.
         ENDIF

!         select side in the chosen direction

         IF ( OPTG.NE.5 ) THEN
            CRDM   = -1.E10
            ISIDM  = 0
            IF (ONED) THEN
               COSDIR = COS(PI*(DNORTH+DIRSI)/180.)
               SINDIR = SIN(PI*(DNORTH+DIRSI)/180.)
               DO ISIDE = 1, 4
                  SOMX = 0.
                  SOMY = 0.
                  NUMP = 0
                  IF (ISIDE.EQ.2) THEN
                     KC = KGRPNT(MXC,1)
                     IF (KC.GT.1) THEN
                        SOMX = XCGRID(MXC,1)
                        SOMY = YCGRID(MXC,1)
                        NUMP = 1
                     ENDIF
                  ELSE IF (ISIDE.EQ.4) THEN
                     KC = KGRPNT(1,1)
                     IF (KC.GT.1) THEN
                        SOMX = XCGRID(1,1)
                        SOMY = YCGRID(1,1)
                        NUMP = 1
                     ENDIF
                  ENDIF
                  IF (NUMP.GT.0) THEN
                     CRDP = COSDIR*SOMX + SINDIR*SOMY
!                  side with largest CRDP is the one selected
                     IF (CRDP.GT.CRDM) THEN
                        CRDM = CRDP
                        ISIDM = ISIDE
                     ENDIF
                  ENDIF
               ENDDO
            ELSE
               DO ISIDE = 1, 4
                  SOMX = 0.
                  SOMY = 0.
                  NUMP = 0
                  IF (ISIDE.EQ.1) THEN
                     DO IX = 1, MXC
                        KC2 = KGRPNT(IX,1)
                        IF (IX.GT.1) THEN
                           IF (KC1.GT.1 .AND. KC2.GT.1) THEN
!                        if both grid points at ends of a step are valid, then
!                        take DX and DY into account when determining direction
                              SOMX = SOMX + XCGRID(IX,1)-XCGRID(IX-1,1)
                              SOMY = SOMY + YCGRID(IX,1)-YCGRID(IX-1,1)
                              NUMP = NUMP + 1
                           ENDIF
                        ENDIF
                        KC1 = KC2
                     ENDDO
                  ELSE IF (ISIDE.EQ.2) THEN
                     DO IY = 1, MYC
                        KC2 = KGRPNT(MXC,IY)
                        IF (IY.GT.1) THEN
                           IF (KC1.GT.1 .AND. KC2.GT.1) THEN
                              SOMX = SOMX + XCGRID(MXC,IY)-XCGRID(MXC,IY-1)
                              SOMY = SOMY + YCGRID(MXC,IY)-YCGRID(MXC,IY-1)
                              NUMP = NUMP + 1
                           ENDIF
                        ENDIF
                        KC1 = KC2
                     ENDDO
                  ELSE IF (ISIDE.EQ.3) THEN
                     DO IX = 1, MXC
                        KC2 = KGRPNT(IX,MYC)
                        IF (IX.GT.1) THEN
                           IF (KC1.GT.1 .AND. KC2.GT.1) THEN
                              SOMX = SOMX + XCGRID(IX-1,MYC)-XCGRID(IX,MYC)
                              SOMY = SOMY + YCGRID(IX-1,MYC)-YCGRID(IX,MYC)
                              NUMP = NUMP + 1
                           ENDIF
                        ENDIF
                        KC1 = KC2
                     ENDDO
                  ELSE IF (ISIDE.EQ.4) THEN
                     DO IY = 1, MYC
                        KC2 = KGRPNT(1,IY)
                        IF (IY.GT.1) THEN
                           IF (KC1.GT.1 .AND. KC2.GT.1) THEN
                              SOMX = SOMX + XCGRID(1,IY-1)-XCGRID(1,IY)
                              SOMY = SOMY + YCGRID(1,IY-1)-YCGRID(1,IY)
                              NUMP = NUMP + 1
                           ENDIF
                        ENDIF
                        KC1 = KC2
                     ENDDO
                  ENDIF
                  IF (NUMP.GT.0) THEN
                     DIRSID = ATAN2(SOMY,SOMX)
                     DIRREF = PI*(DNORTH+DIRSI)/180.
                     IF (CVLEFT) THEN
                        CRDP = COS(DIRSID - 0.5*PI - DIRREF)
                     ELSE
                        CRDP = COS(DIRSID + 0.5*PI - DIRREF)
                     ENDIF
!                  side with largest CRDP is the one selected
                     IF (CRDP.GT.CRDM) THEN
                        CRDM = CRDP
                        ISIDM = ISIDE
                     ENDIF
                  ENDIF
                  IF (ITEST.GE.60) WRITE (PRTEST, "(' side ', 2I4, 2(1X,E11.4), 2(1X,F5.0), 2X, F6.3, 2X, L1)") ISIDE, NUMP,&
                  &SOMX, SOMY, DIRSID*180/PI, DIRREF*180/PI, CRDP, CVLEFT
               ENDDO
            ENDIF
            IF (ISIDM.EQ.0) THEN
               CALL MSGERR (2, 'No open boundary found')
            ENDIF

IF (ISIDM.EQ.1) THEN
               IX1 = 1
               IY1 = 1
               IX2 = MXC
               IY2 = 1
            ELSE IF (ISIDM.EQ.2) THEN
               IX1 = MXC
               IY1 = 1
               IX2 = MXC
               IY2 = MYC
            ELSE IF (ISIDM.EQ.3) THEN
               IX1 = MXC
               IY1 = MYC
               IX2 = 1
               IY2 = MYC
            ELSE IF (ISIDM.EQ.4) THEN
               IX1 = 1
               IY1 = MYC
               IX2 = 1
               IY2 = 1
            ENDIF
            IF (.NOT.CCW .EQV. CVLEFT) THEN
!              swap end points
               IX3 = IX1
               IY3 = IY1
               IX1 = IX2
               IY1 = IY2
               IX2 = IX3
               IY2 = IY3
            ENDIF
            IF (ITEST.GE.50) WRITE (PRINTF, "(' Selected side:', I2, ' from ', 2I4, 2F9.0, ' to ', 2I4, 2F9.0)") ISIDM,&
            &IX1-1, IY1-1, XCGRID(IX1,IY1)+XOFFS, YCGRID(IX1,IY1)+YOFFS,&
            &IX2-1, IY2-1, XCGRID(IX2,IY2)+XOFFS, YCGRID(IX2,IY2)+YOFFS
            MP = MAX(ABS(IX2-IX1),ABS(IY2-IY1))
            DO IP = 0, MP
               IF (MP.EQ.0) THEN
                  RR = 0.
               ELSE
                  RR = REAL(IP) / REAL(MP)
               ENDIF
               IX = IX1 + NINT(RR*REAL(IX2-IX1))
               IY = IY1 + NINT(RR*REAL(IY2-IY1))
               IF (KGRPNT(IX,IY) .GT. 1) THEN
                  KOUNTR = KOUNTR + 1
                  ALLOCATE(TMP)
                  TMP%JX = IX
                  TMP%JY = IY
                  NULLIFY(TMP%NEXTXY)
                  CURR%NEXTXY => TMP
                  CURR => TMP
               ENDIF
            ENDDO
         ELSE
            ! unstructured grid

            DO JBG = 1, nbpol

               ! first boundary polyogon is assumed an outer one
               ! (sea/mainland boundary) and hence, content of blist
               ! is ordered in counterclockwise manner

               IF ( JBG==1 .EQV. CCW ) THEN
                  IXB1 = 1
                  IXB2 = nbpt(JBG)
                  IXI  = 1
               ELSE
                  IXB1 = nbpt(JBG)
                  IXB2 = 1
                  IXI  = -1
               ENDIF

               ALLOCATE(IARR1(nbpt(JBG)))
               K = 0
               IF ( .NOT.PARLL ) THEN
                  DO IP = IXB1, IXB2, IXI
                     IX = blist(IP,JBG)
                     IF ( vmark(IX) == VM ) THEN
                        K = K+1
                        IARR1(K) = IP
                     ENDIF
                  ENDDO
               ELSE
                  DO IP = IXB1, IXB2, IXI
                     IF ( bmark(IP,JBG) == VM ) THEN
                        K = K+1
                        IARR1(K) = IP
                     ENDIF
                  ENDDO
               ENDIF

               IF ( K/=0 ) THEN

                  ALLOCATE(IARR2(K))
                  IARR2(1:K) = IARR1(1:K)
                  ISH = 0
                  DO IPP = 2, K
                     IF ( IARR2(IPP)/=IARR2(IPP-1)+IXI ) THEN
                        ISH = IPP-1
                        EXIT
                     ENDIF
                  ENDDO
                  IARR2 = CSHIFT(IARR2,ISH)

                  DO IPP = 1, K
                     IP = IARR2(IPP)
                     IXG  = blist(IP,JBG)
                     ITMP = MINLOC(ABS(bvertg(:,2)-IXG))
                     II   = ITMP(1)
                     IF ( bvertg(II,2) == IXG ) THEN
                        IX = bvertg(II,1)
                        vert(IX)%atti(VBC) = 1
                     ENDIF
                     KOUNTR = KOUNTR + 1
                     ALLOCATE(TMP)
                     TMP%JX = IXG
                     NULLIFY(TMP%NEXTXY)
                     CURR%NEXTXY => TMP
                     CURR => TMP
                  ENDDO
                  DEALLOCATE(IARR2)

               ENDIF
               DEALLOCATE(IARR1)

            ENDDO

         ENDIF
      ENDIF

!       *** boundary condition from file, 1-d or 2-d spectrum

      CURR => FRST%NEXTXY
      CALL INKEYW ('REQ',' ')
      IF (KEYWIS('UNIF') .OR. KEYWIS('CON') .OR. KEYWIS('PAR')) THEN
         CALL INKEYW('STA', 'PAR')
         IF (KEYWIS('PAR')) THEN
            CALL INREAL ('HS',  SPPARM(1), 'REQ', 0.)
            CALL INKEYW ('STA', ' ')
            CALL INREAL ('PER', SPPARM(2), 'REQ', 0.)
            CALL INREAL ('DIR', SPPARM(3), 'REQ', 0.)
            IF (DSHAPE.EQ.1) THEN
               CALL INREAL ('DD',  SPPARM(4), 'STA', 30.)
               IF ((SPPARM(4).GT.360. .OR. SPPARM(4).LT. 0.).AND.&
               &.NOT.(EQREAL(SPPARM(4),OVEXCV(16)))) THEN
                  CALL MSGERR (2,'Directional spreading is less than '//&
                  &'0 or larger than 360 degrees, and no '//&
                  &'exception value')
               END IF
            ELSE
               CALL INREAL ('DD',  SPPARM(4), 'STA', 2.)
               IF (SPPARM(4).LE. 0.) THEN
                  CALL MSGERR (2,&
                  &'Power of cosine is less or equal to zero')
               END IF
               IF (.NOT.LSPNAR .AND.&
               &SPPARM(4)*DDIR**2/2. .GT. 1.) THEN
                  CALL MSGERR (2,&
                  &'distribution too narrow to be represented properly')
                  WRITE (PRINTF, "(' Advise: choose Dtheta < ', F8.3, ' degr')") SQRT(2./SPPARM(4))*180./PI
                  LSPNAR = .TRUE.
               END IF
            ENDIF
            NBSPEC = NBSPEC + 1
            IF (ITEST.GE.80) WRITE (PRTEST,*) ' bound. spectr.',&
            &NBSPEC, (SPPARM(II), II=1,4)
            NBSPSS = NBSPEC
            ALLOCATE(BSTMP)
            BSTMP%NBS    = NBSPEC
            BSTMP%FSHAPE = FSHAPE
            BSTMP%DSHAPE = DSHAPE
            BSTMP%SPPARM(1:4) = SPPARM(1:4)
            NULLIFY(BSTMP%NEXTBS)
            IF ( .NOT.LBS ) THEN
               FBS = BSTMP
               CUBS => FBS
               LBS = .TRUE.
            ELSE
               CUBS%NEXTBS => BSTMP
               CUBS => BSTMP
            END IF
         ELSE IF (KEYWIS('FILE') .OR. KEYWIS('SPEC')) THEN
            CALL INCSTR ('FNAME',FILENM,'REQ', ' ')
!           generate new set of file data
            NBFILS = NBFILS + 1
            NBSPSS = NBSPEC
            ALLOCATE(BFLTMP)
            CALL BCFILE (FILENM, 'PNTS', BFLTMP,&
            &XCGRID, YCGRID, KGRPNT, XYTST, KGRBND,&
            &DONALL)
            IF (STPNOW()) RETURN
            NULLIFY(BFLTMP%NEXTBSPC)
            IF ( .NOT.LBFILS ) THEN
               FBNDFIL = BFLTMP
               CUBFL => FBNDFIL
               LBFILS = .TRUE.
            ELSE
               CUBFL%NEXTBSPC => BFLTMP
               CUBFL => BFLTMP
            END IF
            CALL ININTG('SEQ', NFSEQ, 'STA', 1)
            NBSPSS = NBSPSS + NFSEQ
         ENDIF
         DO IKO = 1, KOUNTR
            IX = CURR%JX
            IF (OPTG.NE.5) IY = CURR%JY
            CURR => CURR%NEXTXY
            ALLOCATE(BGPTMP)
            IF (OPTG.NE.5) THEN
               BGPTMP%BGP(1) = KGRPNT(IX,IY)
            ELSE
               BGPTMP%BGP(1) = IX
            ENDIF
            BGPTMP%BGP(2) = 1
            BGPTMP%BGP(3) = 1000
            BGPTMP%BGP(4) = NBSPSS
            BGPTMP%BGP(5) = 0
            BGPTMP%BGP(6) = 1
            NULLIFY(BGPTMP%NEXTBGP)
            IF ( .NOT.LBGP ) THEN
               FBGP = BGPTMP
               CUBGP => FBGP
               LBGP = .TRUE.
            ELSE
               CUBGP%NEXTBGP => BGPTMP
               CUBGP => BGPTMP
            END IF
         ENDDO
         NBGRPT = NBGRPT + KOUNTR
      ELSE IF (KEYWIS('VAR')) THEN
         CALL INKEYW('STA', 'PAR')
         IF (KEYWIS('PAR')) THEN
            BPARF = .TRUE.
         ELSE IF (KEYWIS('FILE')) THEN
            BPARF = .FALSE.
         ENDIF
         RLEN1 = -1.E20
         IKO = 1
         RDIST = 0.
         IBSPC1 = 1
         LFRST1 = .TRUE.
         LFRST2 = .TRUE.
         boundary_parameters: DO
            IF (LFRST1) THEN
               CALL INREAL('LEN', RLEN2, 'REQ', 0.)
               LFRST1 = .FALSE.
            ELSE
               CALL INREAL('LEN', RLEN2, 'STA', 1.E20)
            ENDIF
            IF (RLEN2.LT.0.9E20) THEN
               IF (IKO.GT.KOUNTR) THEN
                  CALL MSGERR(1,&
                  &'Length of segment short, boundary values ignored')
                  WRITE (PRINTF, "(' segment length=', F9.2, '; [len]=', F9.2)") RDIST, RLEN2
               ENDIF
               IF (BPARF) THEN
                  CALL INREAL ('HS',  SPPARM(1), 'REQ', 0.)
                  CALL INKEYW ('STA', ' ')
                  CALL INREAL ('PER', SPPARM(2), 'REQ', 0.)
                  CALL INREAL ('DIR', SPPARM(3), 'REQ', 0.)
                  IF (DSHAPE.EQ.1) THEN
                     CALL INREAL ('DD',  SPPARM(4), 'STA', 30.)
                     IF ((SPPARM(4).GT.360. .OR. SPPARM(4).LT. 0.).AND.&
                     &.NOT.(EQREAL(SPPARM(4),OVEXCV(16)))) THEN
                        CALL MSGERR (2,'Directional spreading is less ' //&
                        &'than 0 or larger than 360 '//&
                        &'degrees and no exception value')
                     END IF
                  ELSE
                     CALL INREAL ('DD',  SPPARM(4), 'STA', 2.)
                     IF (SPPARM(4).LE. 0.) THEN
                        CALL MSGERR (2,'Power of cosine is less or equal '//&
                        &'to zero')
                     END IF
                     IF (.NOT.LSPNAR .AND.&
                     &SPPARM(4)*DDIR**2/2. .GT. 1.) THEN
                        CALL MSGERR (2,&
                        &'distribution too narrow to be represented properly')
                        WRITE (PRINTF, "(' Advise: choose Dtheta < ', F8.3, ' degr')") SQRT(2./SPPARM(4))*180./PI
                        LSPNAR = .TRUE.
                     END IF
                  ENDIF
                  NBSPEC = NBSPEC + 1
                  IBSPC2 = NBSPEC
                  ALLOCATE(BSTMP)
                  BSTMP%NBS    = NBSPEC
                  BSTMP%FSHAPE = FSHAPE
                  BSTMP%DSHAPE = DSHAPE
                  BSTMP%SPPARM(1:4) = SPPARM(1:4)
                  NULLIFY(BSTMP%NEXTBS)
                  IF ( .NOT.LBS ) THEN
                     FBS = BSTMP
                     CUBS => FBS
                     LBS = .TRUE.
                  ELSE
                     CUBS%NEXTBS => BSTMP
                     CUBS => BSTMP
                  END IF
               ELSE
                  IF (LFRST2) THEN
                     CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
                     LFRST2 = .FALSE.
                  ELSE
                     CALL INCSTR ('FNAME', FILENM, 'STA', ' ')
                  ENDIF
                  IF (FILENM.NE.'    ') THEN
!                 generate new set of file data
                     NBFILS = NBFILS + 1
                     NBSPSS = NBSPEC
                     ALLOCATE(BFLTMP)
                     CALL BCFILE (FILENM, 'PNTS', BFLTMP,&
                     &XCGRID, YCGRID, KGRPNT, XYTST, KGRBND,&
                     &DONALL)
                     IF (STPNOW()) RETURN
                     NULLIFY(BFLTMP%NEXTBSPC)
                     IF ( .NOT.LBFILS ) THEN
                        FBNDFIL = BFLTMP
                        CUBFL => FBNDFIL
                        LBFILS = .TRUE.
                     ELSE
                        CUBFL%NEXTBSPC => BFLTMP
                        CUBFL => BFLTMP
                     END IF
                  ENDIF
                  CALL ININTG ('SEQ', NFSEQ, 'STA', 1)
                  IBSPC2 = NBSPSS + NFSEQ
                  IF (IBSPC2.GT.NBSPEC)&
                  &CALL MSGERR (1,'too large value for SEQ')
               ENDIF
            ELSE
               IF (IKO.GT.KOUNTR) EXIT boundary_parameters
            ENDIF
            LFRST3 = .TRUE.
            boundary_points: DO
               IX = CURR%JX
               IF (OPTG.NE.5) THEN
                  IY = CURR%JY
                  XC2 = XCGRID(IX,IY)
                  YC2 = YCGRID(IX,IY)
               ELSE
                  XC2 = xcugrdgl(IX)
                  YC2 = ycugrdgl(IX)
               ENDIF
               IF (.NOT.LFRST3) THEN
                  RDIST = RDIST + SQRT ((XC2-XC1)**2 + (YC2-YC1)**2)
               ENDIF
               LFRST3 = .FALSE.
               XC1 = XC2
               YC1 = YC2
               IF (RDIST.GT.RLEN2) EXIT boundary_points
               ALLOCATE(BGPTMP)
               IF (OPTG.NE.5) THEN
                  BGPTMP%BGP(1) = KGRPNT(IX,IY)
               ELSE
                  BGPTMP%BGP(1) = IX
               ENDIF
               BGPTMP%BGP(2) = 1
               W1 = (RLEN2-RDIST)/(RLEN2-RLEN1)
               BGPTMP%BGP(3) = NINT(1000.*W1)
               BGPTMP%BGP(4) = IBSPC1
               BGPTMP%BGP(5) = NINT(1000.*(1.-W1))
               BGPTMP%BGP(6) = IBSPC2
               NULLIFY(BGPTMP%NEXTBGP)
               IF ( .NOT.LBGP ) THEN
                  FBGP = BGPTMP
                  CUBGP => FBGP
                  LBGP = .TRUE.
               ELSE
                  CUBGP%NEXTBGP => BGPTMP
                  CUBGP => BGPTMP
               END IF
               IKO = IKO + 1
               IF (IKO.GT.KOUNTR) EXIT boundary_points
               IF (.NOT.ASSOCIATED(CURR%NEXTXY)) EXIT
               CURR => CURR%NEXTXY
            ENDDO boundary_points
!           boundary values have been assigned, read new parameters
            IF (RLEN2.GT.0.9E20) EXIT boundary_parameters
            RLEN1  = RLEN2
            IBSPC1 = IBSPC2
         ENDDO boundary_parameters
!         update NBGRPT = number of boundary grid points
         NBGRPT = NBGRPT + KOUNTR
      ELSE
         CALL WRNKEY
      ENDIF
      IF (ASSOCIATED(TMP)) DEALLOCATE(TMP)
   ENDIF
RETURN
end subroutine SWBOUN
!*********************************************************************
!                                                                    *
SUBROUTINE BCFILE (FBCNAM, BCTYPE, BSPFIL,&
&XCGRID, YCGRID, KGRPNT,&
&XYTST,  KGRBND, DONALL)
   USE swan_legacy_io, ONLY: INAR2D, COPYCH
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_file_opening, ONLY: FOR
   USE swan_input_parser, ONLY: EQCSTR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
!                                                                    *
!*********************************************************************

   USE swan_input_parser, ONLY: default_command_reader
   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_BNDSPEC
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
!     30.73: Nico Booij
!     30.90: IJsbrand Haagsma (Equivalence version)
!     34.01: Jeroen Adema
!     40.03, 40.13: Nico Booij
!     40.05: Ekaterini E. Kriezi
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.73, Dec. 97: New subroutine
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, June 00: function EQCSTR used to compare strings
!            July 00: option LONLAT for location coordinates introduced
!     40.05, Aug. 00: replace the source text related with the grid points
!                     interpolation coef. with a new subroutine BC_POINTS
!     40.13, Jan. 01: ! is now allowed as comment sign in a boundary file
!                     checking coordinates only for nesting situation
!                     remove declarations of unused variables
!            Nov. 01: initial size of BSPAUX array enlarged
!     40.31, Nov. 03: removing POOL-mechanism, reconsideration of this
!                     subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Nov. 04: small corrections
!
!  2. Purpose
!
!     Reads file data for boundary condition
!
!  3. Method
!
!
!  4. Argument variables

   REAL      XCGRID(MXC,MYC), YCGRID(MXC,MYC)

   INTEGER   KGRPNT(MXC,MYC)
   INTEGER   XYTST(*), KGRBND(*)

!       FBCNAM  char  inp    filename of boundary data file
!       BCTYPE  char  inp    if value is "NEST": nesting b.c.
!       XCGRID  real  inp    x-coordinate of computational grid points
!       YCGRID  real  inp    y-coordinate of computational grid points
!       KGRPNT  int   inp    indirect addresses of grid points
!       XYTST   int   inp    ix, iy of test points
!
!     DONALL: logic arguments declare if the nesting  boundary is open or close
!             it is defined by the users

   LOGICAL, INTENT(INOUT)  ::  DONALL

   CHARACTER(LEN=*) :: FBCNAM, BCTYPE

   TYPE(BSPCDAT) :: BSPFIL

!  5. Parameter variables
!
!
!  6. Local variables

   INTEGER :: ISTATF, NDSL, NDSD, IOSTAT, IERR, NBOUNC, NANG, NFRE
   INTEGER :: IBOUNC, DORDER
   INTEGER, SAVE :: IENT = 0
   INTEGER :: IOPTT
   INTEGER :: NHEDF, NHEDT, NHEDS, IFRE , IANG
   INTEGER :: NQUANT, IQUANT, IBC, II, NBGRPT_PREV,IIPT2
   REAL    :: XP, YP, XP2, YP2
   REAL    :: FREQHZ, DIRDEG, DIRRD1,DIRRAD, EXCV
   CHARACTER(LEN=4)  :: BTYPE
   CHARACTER(LEN=80) :: HEDLIN

!    NBGRPT_PREV is the prevous number of NBGRPT
!    IIPT2 counter use for the chekinf if there are grid points on nested boundary
!
!  8. Subroutines Used
!
!       Ocean Pack command reading routines
!       SWBCPT : boundary points interpolation


!  9. Subroutines calling
!
!       SWREAD
!
!  10. Error messages
!
!
!  11. Remarks
!
!
!       This subroutine reads the heading of the file to determine locations
!       of boundary spectra, spectral frequencies and directions etc.
!       Reading and processing of spectral energy densities is done during
!       computation by subroutine RESPEC (file Swanmain.for)
!
!       data concerning boundary files are stored in array BFILED
!       there is a subarray for each file; it contains:
!       1.  status; 0: stationary, 1: nonstat, -1: exhausted
!       2.  time of boundary values read one before last
!       3.  time of boundary values read last
!       4.  NDSL: unit ref. num. of file containing filenames
!       5.  NDSD: unit ref. num. of file containing data
!       6.  time coding option for reading time from data file
!       8.  number of locations for which spectra are in the file
!       9.  order of reading directional information
!       10. number of spectral directions of spectra on file
!       12. number of spectral frequencies
!       14. number of heading lines per file
!       15. number of heading lines per time step
!       16. number of heading lines per spectrum
!       17. =1: energy dens., =2: variance density
!       18. =1: Cartesian direction, =2: Nautical dir.
!       19. =1: direction spread in degr, =2: Power of Cos.
!
!
!  12. Structure
!
!       ----------------------------------------------------------------
!       Open boundary condition data file
!       Read type of file from first line of file
!       Case filetype is:
!       TPAR: make filetype TPAR
!       SWAN: make filetype SWAN
!             If b.c. type is NEST
!             Then calculate data on grid points
!             put into array BGRIDP
!             -----------------------------------------------------------
!             Read spectral directions from file into array BSPDIR
!             Read spectral frequencies from file into array BSPFRQ
!       ----------------------------------------------------------------
!       Put file characteristics into array BFILED
!       ----------------------------------------------------------------
!
! 13. Source text

   LOGICAL         CCOORD

   CALL STRACE (IENT, 'BCFILE')

   NDSL = 0
   IIPT2 = 0
!     open data file
   NDSD = 0
   IOSTAT = 0
   CALL FOR (NDSD, FILENM, 'OF', IOSTAT)
   IF (STPNOW()) RETURN

!     --- initialize array BFILED of BSPFIL
   BSPFIL%BFILED = 0

!     start reading from the data file
   READ (NDSD, '(A)') HEDLIN
   IF (EQCSTR(HEDLIN,'TPAR')) THEN
      BTYPE  = 'TPAR'
      ISTATF = 1
      IOPTT  = 1
      NBOUNC = 1
      NANG   = 0
      NFRE   = 0
      NHEDF  = 0
      NHEDT  = 0
      NHEDS  = 0
      DORDER = 0
      ALLOCATE(BSPFIL%BSPFRQ(NFRE))
      ALLOCATE(BSPFIL%BSPDIR(NANG))
      IF (NSTATM.EQ.0) CALL MSGERR (3,&
      &'time information not allowed in stationary mode')
      NSTATM = 1
   ELSE IF (EQCSTR(HEDLIN,'SWAN')) THEN
      NHEDF  = 0
      DO
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.60) WRITE (PRTEST,"(' heading line: ', A)") HEDLIN
!        skip heading lines starting with comment sign
         IF (HEDLIN(1:1).NE.default_command_reader%COMID .AND. HEDLIN(1:1).NE.'!') EXIT
      END DO
      IF (EQCSTR(HEDLIN,'TIME')) THEN
         IF (NSTATM.EQ.0) CALL MSGERR (3,&
         &'nonstationary boundary condition not allowed '//&
         &'in stationary mode')
         NSTATM = 1
         ISTATF = 1
         BTYPE = 'SWNT'
         READ (NDSD, *) IOPTT
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.60) WRITE (PRTEST,"(' heading line: ', A)") HEDLIN
         NHEDF = 2
         NHEDT = 1
      ELSE
         ISTATF = 0
         BTYPE = 'SWNS'
         NHEDT  = 0
      ENDIF

!       read geographical locations
!
!       read number of boundary points
      CCOORD = .TRUE.
      IF (EQCSTR(HEDLIN,'LOC')) THEN
         IF (BCTYPE.EQ.'NEST' .AND. KSPHER.EQ.1) CALL MSGERR (3,&
         &'Boundary locations are Cartesian, while comp. is spherical')
      ELSE IF (EQCSTR(HEDLIN,'LONLAT')) THEN
         IF (BCTYPE.EQ.'NEST' .AND. KSPHER.EQ.0) CALL MSGERR (3,&
         &'Boundary locations are spherical, while comp. is Cartesian')
      ELSE
!         set CCOORD to False to indicate that no locations are defined
         CCOORD = .FALSE.
      ENDIF
      IF (CCOORD) THEN
         READ (NDSD, *) NBOUNC

         DO IBOUNC = 1, NBOUNC
            IERR = 0
            CALL REFIXY (NDSD, XP, YP, IERR)
            IF (ITEST.GE.80) THEN
               WRITE (PRTEST, *) ' B. spectrum ', IBOUNC, XP+XOFFS,&
               &YP+YOFFS, IERR
            ENDIF
!           in case of nesting coordinates on file are used to
!           determine interpolation coefficients
!           in other cases coordinates are ignored
            IF (BCTYPE .EQ. 'NEST') THEN
               XP2 = XP
               YP2 = YP

!             --- interpolate the boundaries points to the grid points of
!                 the SWAN computational grid

               NBGRPT_PREV = NBGRPT
               CALL SWBCPT (  XCGRID, YCGRID,&
               &KGRPNT, XYTST,  KGRBND,XP2,YP2,IBOUNC,&
               &NBOUNC,DONALL )
!             check if the grid points are on nested boundary.
!             if not, stop the calculation and give an error message
               IF (NBGRPT.NE.NBGRPT_PREV) THEN
                  IIPT2 = IIPT2+1
               ENDIF
            ENDIF
         ENDDO

         CALL SWREDUCE ( IIPT2, 1, SWMAX )
         IF ((BCTYPE.EQ.'NEST') .AND. (IIPT2.EQ.0) .AND. IAMMASTER)&
         &CALL MSGERR (2,'no grid points on nested boundary')

         NHEDF = NHEDF + 2 + NBOUNC
         IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' boundary locations')") NBOUNC
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.60) WRITE (PRTEST,"(' heading line: ', A)") HEDLIN
      ELSE
         IF (BCTYPE .EQ. 'NEST') THEN
            CALL MSGERR (3, 'this file is not a true nesting file')
         ENDIF
         NBOUNC = 1
      ENDIF

!       read spectral resolution information
!
!       number of spectral frequencies
      IF (EQCSTR(HEDLIN(2:5),'FREQ')) THEN
         READ (NDSD, *) NFRE
         ALLOCATE(BSPFIL%BSPFRQ(NFRE))
         DO IFRE = 1, NFRE
!           read frequency in Hz and convert to radians/sec
            READ (NDSD, *) FREQHZ
            BSPFIL%BSPFRQ(IFRE) = PI2 * FREQHZ
         ENDDO
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.60) WRITE (PRTEST,"(' heading line: ', A)") HEDLIN
         NHEDF = NHEDF + 2 + NFRE
      ELSE
         NFRE = 0
         IF (BCTYPE.EQ.'NEST') THEN
            CALL MSGERR (3, 'file is not a true nesting file')
         ENDIF
      ENDIF
      IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' boundary frequencies')") NFRE
!       number of spectral directions
      IF (EQCSTR(HEDLIN(2:4),'DIR')) THEN
         READ (NDSD, *) NANG
         ALLOCATE(BSPFIL%BSPDIR(NANG))
         DO IANG = 1, NANG
!           read direction in degr and convert to radians
            READ (NDSD, *) DIRDEG
            IF (EQCSTR(HEDLIN,'N')) THEN
               DIRDEG = 180. + DNORTH - DIRDEG
            ENDIF
            DIRRAD = DIRDEG * PI / 180.
!           reverse order if second direction is smaller than first
            IF (IANG.EQ.1) THEN
               DIRRD1 = DIRRAD
               DORDER = 1
            ELSE IF (IANG.EQ.2) THEN
               IF (DIRRAD.LT.DIRRD1) THEN
                  DORDER = -1
                  BSPFIL%BSPDIR(NANG) = DIRRD1
               ELSE
                  DORDER = 1
               ENDIF
               DIRRD1 = DIRRAD
            ELSE
               IF (DORDER.LT.0.) THEN
                  IF (DIRRAD.GT.DIRRD1) CALL MSGERR (3,&
                  &'spectral directions in file not in right order')
               ELSE
                  IF (DIRRAD.LT.DIRRD1) CALL MSGERR (3,&
                  &'spectral directions in file not in right order')
               ENDIF
               DIRRD1 = DIRRAD
            ENDIF
            IF (DORDER.LT.0) THEN
               BSPFIL%BSPDIR(NANG+1-IANG) = DIRRAD
            ELSE
               BSPFIL%BSPDIR(IANG) = DIRRAD
            ENDIF
         ENDDO
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.60) WRITE (PRTEST,"(' heading line: ', A)") HEDLIN
         NHEDF = NHEDF + 2 + NANG
         NHEDS = 1
      ELSE
         NANG   = 0
         ALLOCATE(BSPFIL%BSPDIR(NANG))
         NHEDS  = 1
         DORDER = 0
      ENDIF
      IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' boundary directions')") NANG

!       read quantities (name, unit, exc. value)

      IF (EQCSTR(HEDLIN,'QUANT')) THEN
         READ (NDSD, *) NQUANT
         IF (.NOT.((NQUANT.EQ.1 .AND. NANG.GT.0) .OR.&
         &(NQUANT.EQ.3 .AND. NANG.EQ.0))) THEN
            CALL MSGERR (2, 'incompatible data on b.c. file')
            WRITE (PRINTF, "(I3, ' quantities; ', I5, ' directions')") NQUANT, NANG
         ENDIF
         DO IQUANT = 1, NQUANT
            READ (NDSD, '(A)') HEDLIN
!           if first quantity is 'EnDens' divide by Rho*Grav
            IF (IQUANT.EQ.1) THEN
               IF ( EQCSTR(HEDLIN,'ENDENS')) THEN
!               quantity on file is energy density
                  BSPFIL%BFILED(17) = 1
               ELSE IF ( EQCSTR(HEDLIN,'VADENS')) THEN
!               quantity on file is variance density
                  BSPFIL%BFILED(17) = 2
               ELSE
                  CALL MSGERR (2,&
                  &'Incorrect quantity in b.c.file: ' // HEDLIN(1:10))
                  BSPFIL%BFILED(17) = 2
               ENDIF
            ELSE IF (IQUANT.EQ.2) THEN
!             if second quantity is 'NDIR' transform from Nautical to Cartesian dir.
               IF ( EQCSTR(HEDLIN,'NDIR')) THEN
!               quantity on file is Nautical direction
                  BSPFIL%BFILED(18) = 2
               ELSE IF (EQCSTR(HEDLIN,'CDIR')) THEN
!               quantity on file is Cartesian direction
                  BSPFIL%BFILED(18) = 1
               ELSE
                  CALL MSGERR (2,&
                  &'Incorrect quantity in b.c.file: ' // HEDLIN(1:10))
                  BSPFIL%BFILED(18) = 1
               ENDIF
            ELSE IF (IQUANT.EQ.3) THEN
!             if third quantity is 'DSPRP' or 'POWER' power is given,
!             otherwise calculate power from dir. spread in degrees
               IF (EQCSTR(HEDLIN,'DSPRP') .OR.&
               &EQCSTR(HEDLIN,'POWER')) THEN
!               quantity on file is power of cos
                  BSPFIL%BFILED(19) = 2
               ELSE IF (EQCSTR(HEDLIN,'DSPR') .OR.&
               &EQCSTR(HEDLIN,'DEGR')) THEN
!               quantity on file is Directional spread in degr
                  BSPFIL%BFILED(19) = 1
               ELSE
                  CALL MSGERR (2,&
                  &'Incorrect quantity in b.c.file: ' // HEDLIN(1:10))
                  BSPFIL%BFILED(19) = 1
               ENDIF
            ENDIF
!           check Unit and Exception value
            READ (NDSD, '(A)') HEDLIN
            IF (IQUANT.EQ.3 .AND. EQCSTR(HEDLIN,'DEGR')) THEN
               IF (BSPFIL%BFILED(19).NE.1) THEN
                  CALL MSGERR (2, 'incompatible options in boundary file')
                  BSPFIL%BFILED(19) = 1
               ENDIF
            ENDIF
            IF (IQUANT.EQ.1) THEN
               READ (NDSD, *) EXCV
               BSPFIL%BFILED(11) = NINT(EXCV)
            ELSE
               READ (NDSD, '(A)') HEDLIN
            END IF
         ENDDO
         NHEDF = NHEDF + 2 + 3*NQUANT
      ENDIF
      IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' quantities')") NQUANT
   ELSE
      CALL MSGERR (3, 'unsupported boundary data file')
   ENDIF

   ALLOCATE(BSPFIL%BSPLOC(NBOUNC))
   DO IBC = 1, NBOUNC
      BSPFIL%BSPLOC(IBC) = NBSPEC + IBC
   ENDDO
   NBSPEC = NBSPEC + NBOUNC

!     store file reading parameters in array BFILED

   BSPFIL%BFILED(1)  = ISTATF
   BSPFIL%BFILED(2)  = -999999999
   BSPFIL%BFILED(3)  = -999999999
   BSPFIL%BFILED(4)  = NDSL
   BSPFIL%BFILED(5)  = NDSD
   BSPFIL%BFILED(6)  = IOPTT
   CALL COPYCH (BTYPE, 'T', BSPFIL%BFILED(7), 1, IERR)
   BSPFIL%BFILED(8)  = NBOUNC
   BSPFIL%BFILED(9)  = DORDER
   BSPFIL%BFILED(10) = NANG
   BSPFIL%BFILED(12) = NFRE
!     ordering of data on file
   BSPFIL%BFILED(13) = 0
!     number of heading lines: per file, per time, per spectrum
   BSPFIL%BFILED(14) = NHEDF
   BSPFIL%BFILED(15) = NHEDT
   BSPFIL%BFILED(16) = NHEDS

   IF (ITEST.GE.80) WRITE(PRINTF,"(' array BFILED: ', 2I4, 2(/,8I10))") NBFILS, NBSPEC,&
   &(BSPFIL%BFILED(II), II=1,16)

   RETURN
!     end of subroutine BCFILE
end subroutine BCFILE
!*********************************************************************
!                                                                    *
SUBROUTINE BCWAMN (FBCNAM, BCTYPE, BSPFIL,&
&XCGRID, YCGRID, KGRPNT, XYTST)
   USE swan_legacy_io, ONLY: INAR2D, COPYCH
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: IGNORE, ININTG, INKEYW, KEYWIS, INDBLE
!                                                                    *
!*********************************************************************

   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_BNDSPEC
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
!     30.73: Nico Booij
!     30.90: IJsbrand Haagsma (Equivalence version)
!     34.01: Jeroen Adema
!     40.03: Nico Booij
!     40.13: N. Booij
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.61: Roop Lalbeharry
!
!  1. Updates
!
!     30.73, Jan. 98: new subroutine, based on older version by Weimin Luo
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Nov. 99: THD (first spectral direction in radians) added in
!                     expression for RBSDIR (directions of boundary spectrum)
!     40.03, Aug. 00: correction WAM nest with spherical SWAN
!     40.13, May  01: order of boundary points in WAM nesting file differed
!                     from order assumed in SWAN
!     40.31, Nov. 03: removing POOL-mechanism, reconsideration of this
!                     subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.61, Nov. 06: variables USNEW, THWNEW no longer written in WAM4.5
!
!  2. PURPOSE
!
!     reads file data for WAM nesting boundary condition
!
!  3. METHOD
!
!
!  4. Argument variables
!
!       FBCNAM  char  inp    filename of boundary data file
!       BCTYPE  char  inp    if value is "NEST": nesting b.c.
!       XCGRID  real  inp    x-coordinate of computational grid points
!       YCGRID  real  inp    y-coordinate of computational grid points
!       KGRPNT  int   inp    indirect addresses of grid points
!       XYTST   int   inp    ix, iy of test points
!
!  7. Common blocks used
!
!
!  5. SUBROUTINES CALLING
!
!       SWBOUN
!
!  6. SUBROUTINES USED
!
!       Ocean Pack command reading routines



!  7. ERROR MESSAGES
!
!       ---
!
!  8. REMARKS
!
!       data concerning boundary files are stored in array BFILED
!       there is a subarray for each file; it contains:
!       1.  status; 0: stationary, 1: nonstat, -1: exhausted
!       2.  time of boundary values read one before last
!       3.  time of boundary values read last
!       4.  NDSL: unit ref. num. of file containing filenames
!       5.  NDSD: unit ref. num. of file containing data
!       6.  time coding option for reading time from data file
!       8.  number of locations for which spectra are in the file
!       9.  order of reading directional information
!       10. number of spectral directions of spectra on file
!       12. number of spectral frequencies
!       14. number of heading lines per file
!       15. number of heading lines per time step
!       16. number of heading lines per spectrum
!       17. =1: energy dens., =2: variance density
!
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       Open file containing filnames
!       Read data file name
!       Open boundary condition data file
!       Read number of b.points, frequencies and directions
!       Generate spectral directions from file into array BSPDIR
!       Generate spectral frequencies from file into array BSPFRQ
!       For all boundary spectra do
!           read location from data file
!           transform into local cartesian or spherical coordinates
!       ----------------------------------------------------------------
!       Determine spatial step size in WAM nesting file
!       For all spatial points in WAM file do
!           For all other spatial points in WAM file do
!               If the two points are neighbours
!               Then For all computational grid points on boundary do
!                        if point is located between nest file grid points
!                        calculate interpolation coefficients
!                        and put these into array BGRIDP
!       ----------------------------------------------------------------
!       Write file characteristics into array BFILED
!       ----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER   KGRPNT(MXC,MYC), XYTST(*)
   REAL      XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   TYPE(BSPCDAT) :: BSPFIL
   CHARACTER(LEN=*) :: FBCNAM, BCTYPE

!     local variables

   INTEGER   ISTATF, NDSL, NDSD, IOSTAT, IERR, NBOUNC, NANG, NFRE,&
   &IBOUNC, IX1, IY1, IX2, IY2, IXP, IYP, IP, MIP, INDXGR,&
   &DORDER, IOPTT
!     ISTATF    if >0 file contains nonstationary data
!     NDSL      unit ref num of namelist file
!     NDSD      unit ref num of data file
!     IOSTAT    io status
!     IERR      error status
!     NBOUNC    number of boundary locations
!     NANG      number of directions on file
!     NFRE      number of frequencies on file
!     IBOUNC    counter of boundary spectra
!     IX1
!     IY1
!     IX2
!     IY2
!     IXP
!     IYP
!     IP
!     MIP
!     INDXGR    counter of boundary grid points
!     DORDER    if <0 order of reading directions is reversed
!     IOPTT     time reading option

   INTEGER :: IIPT1=0, IIPT2=0
!     local and overall number of interpolated boundary grid points

   INTEGER :: NHEDF       ! number of heading lines at begin of file
   INTEGER :: NHEDT       ! number of heading lines per time step
   INTEGER :: NHEDS       ! number of heading lines
   INTEGER :: IBC, IDW, ISW, IFRE, II, ISIDE, IHD    ! counters
   INTEGER :: IBNC1, IBNC2   ! counters of nesting points
   INTEGER :: IBSP1, IBSP2   ! counters of nesting points

   REAL      XP, YP, XP1, YP1, XP2, YP2, RR, RX, RY, RL2,&
   &XANG, XFRE, THD, FR1, CO, XBOU, XDELC,&
   &XLON, XLAT, XDATE, EMEAN, THQ, FMEAN
!     XP        problem coordinate of a comp. grid point on the boundary
!     YP        problem coordinate of a comp. grid point on the boundary
!     XP1       problem coordinate of a boundary location
!     YP1       problem coordinate of a boundary location
!     XP2       problem coordinate of a boundary location
!     YP2       problem coordinate of a boundary location
!     RR
!     RX        vector connecting two boundary locations
!     RY        vector connecting two boundary locations
!     RL2       length **2 of vector connecting two boundary locations

   REAL(KIND=KIND(0.0D0)), ALLOCATABLE :: XPWAM(:), YPWAM(:)
   REAL, ALLOCATABLE :: SPAUX(:)
   ! locations of nesting points
   REAL(KIND=KIND(0.0D0)) :: DXWAM, DYWAM   ! spatial step sizes in nesting
   REAL(KIND=KIND(0.0D0)) :: DXTEST, DYTEST ! distance between two nesting
   REAL :: DISXY          ! dim.less distance
   REAL :: PHI            ! direction of vector (RX,RY)
   REAL :: DPHI           ! difference in direction
   REAL :: EPS            ! tolerance
   REAL :: W2             ! interpolation coefficient


   CHARACTER (LEN=4)  :: BTYPE     ! type of boundary cond.
   CHARACTER (LEN=14) :: CDATE     ! date-time
   CHARACTER (LEN=80) :: HEDLIN    ! heading line

   REAL(KIND=KIND(0.0D0)) :: DDATE, XLON0, XLAT0
!     DDATE     date-time
!     XLON0     longitude of origin of computational grid
!     XLAT0     latitude of origin of computational grid

   TYPE(BGPDAT), POINTER :: BGPTMP

!     subroutines used


   INTEGER, SAVE :: IENT = 0
   CALL STRACE (IENT, 'BCWAMN')

   ISTATF = 1
   IOPTT = 6
   NDSL = 0
!     open file with list of names
   CALL FOR (NDSL, FILENM,'OF',IOSTAT)
   IF (STPNOW()) RETURN
   READ (NDSL,'(A36)') FILENM
   CALL INKEYW ('REQ', ' ')
   IF (KEYWIS('FRE')) THEN
      BTYPE = 'WAMF'
   ELSE IF (KEYWIS('UNF')) THEN
      CALL INKEYW ('REQ', ' ')
      IF (KEYWIS('WK')) THEN
         BTYPE = 'WAMW'
      ELSE
         CALL IGNORE ('CRAY')
         BTYPE = 'WAMC'
      ENDIF
   ENDIF
!     open WAM data file
   NDSD=0
   IOSTAT = 0
   IF (BTYPE.EQ.'WAMF') THEN
      CALL FOR(NDSD,FILENM,'OF',IOSTAT)
      IF (STPNOW()) RETURN
   ELSE
      CALL FOR(NDSD,FILENM,'OU',IOSTAT)
      IF (STPNOW()) RETURN
   ENDIF

!     --- initialize array BFILED of BSPFIL
   BSPFIL%BFILED = 0

!     read spherical coordinates of point corresponding to
!     [xpc], [ypc] in Cartesian coordinates
!     not necessary if SWAN uses spherical coordinates
!     if not given first point in data file is assumed
   CALL INDBLE('XGC',XLON0,'STA',-999.D0)
   CALL INDBLE('YGC',XLAT0,'STA',-999.D0)
   CALL ININTG('LWDATE',LWDATE,'STA',12)

!     start reading from the data file
!
!     read resolution information from WAM input

   IF (BTYPE.EQ.'WAMF') THEN
      READ (NDSD,*) XANG, XFRE, THD, FR1, CO, XBOU, XDELC
   ELSE
!       Cray and workstation version
      READ (NDSD) XANG, XFRE, THD, FR1, CO, XBOU, XDELC
   ENDIF

!     number of WAM boundary points
   NBOUNC  = NINT(XBOU)
!     number of direction of WAM spectrum
   NANG = NINT(XANG)
   DORDER = -1
!     number of frequencies of WAM spectrum
   NFRE = NINT(XFRE)
!     number of heading lines: per file, per time, per spectrum
   NHEDF = 1
   NHEDT = 0
   NHEDS = 1
   IF (ITEST.GE.80) THEN
      WRITE(PRINTF,*) ' Number of frequencies in WAM:',NFRE
      WRITE(PRINTF,*) ' Number of directions in WAM:',NANG
      WRITE(PRINTF,*) ' Lowest frequency in WAM:',FR1
      WRITE(PRINTF,*) ' fi/fi-1 in WAM:',CO
   ENDIF

!     convert WAM wave directions to SWAN convention
!     apparently WAM uses direction TO which waves propagate !!

   ALLOCATE(BSPFIL%BSPDIR(NANG))
   DO  IDW = NANG,1,-1
      BSPFIL%BSPDIR(NANG-IDW+1) = DNORTH*DEGRAD -&
      &THD - REAL(IDW-1)*PI2/REAL(NANG)
   ENDDO
   IF (ITEST.GE.50) WRITE (PRTEST,"(' WAMNEST dirs ', I3, (/, 20F6.0))") NANG,&
   &(BSPFIL%BSPDIR(IDW)*180./PI, IDW=1,NANG)

!     calculate WAM angular frequency array

   ALLOCATE(BSPFIL%BSPFRQ(NFRE))
   BSPFIL%BSPFRQ(1) = PI2*FR1
   DO  ISW = 2, NFRE
      BSPFIL%BSPFRQ(ISW) = CO * BSPFIL%BSPFRQ(ISW-1)
   ENDDO
   IF (ITEST.GE.50) WRITE (PRTEST,"(' WAMNEST freqs ', I3, (/, 20F6.2))") NFRE,&
   &(BSPFIL%BSPFRQ(ISW)*180./PI, ISW=1,NFRE)
   IF (NBOUNC.EQ.1) CALL MSGERR (3,&
   &'WAM nest does not work with only one nesting point')

!     allocate arrays XPWAM and YPWAM

   ALLOCATE (XPWAM(1:NBOUNC), YPWAM(1:NBOUNC))
   ALLOCATE (SPAUX(NANG*NFRE))

!     read geographical locations and determine DXWAM and DYWAM

   IIPT2 = 0
   DXWAM = 180.
   DYWAM = 180.
   DO IBOUNC = 1, NBOUNC
      IF (BTYPE.EQ.'WAMF') THEN
!         read boundary point coordinates from file
         READ(NDSD,*) XLON, XLAT, DDATE, EMEAN,&
         &THQ, FMEAN
         IF (IBOUNC.EQ.1 .AND. ITEST.GE.80) WRITE (PRTEST, *)&
         &' WAMNEST starting time ', DDATE
!         read spectral densities but ignore them for the moment
         DO IFRE=1,NFRE
            READ(NDSD,*) (SPAUX(II), II=1,NANG)
         ENDDO
      ELSE IF (BTYPE.EQ.'WAMC') THEN
!         read boundary point coordinates from file
         READ(NDSD) XLON, XLAT, XDATE, EMEAN,&
         &THQ, FMEAN
         IF (IBOUNC.EQ.1 .AND. ITEST.GE.80) WRITE (PRTEST, *)&
         &' WAMNEST starting time ', XDATE
!         read spectral densities but ignore them for the moment
         READ(NDSD) (SPAUX(II), II=1,NANG*NFRE)
      ELSE
!         read boundary point coordinates from file
         READ(NDSD) XLON, XLAT, CDATE(1:LWDATE), EMEAN, THQ, FMEAN
         IF (IBOUNC.EQ.1 .AND. ITEST.GE.80) WRITE (PRTEST, *)&
         &' WAMNEST starting time ', CDATE(1:LWDATE)
!         read spectral densities but ignore them for the moment
         READ(NDSD) (SPAUX(II), II=1,NANG*NFRE)
      ENDIF
      IF (ITEST.GE.50) WRITE (PRINTF, "(' boundary spectrum ', I3, ' at ', 2F12.4)") IBOUNC, XLON, XLAT
      XPWAM(IBOUNC) = XLON
      YPWAM(IBOUNC) = XLAT
!       determine DXWAM and DYWAM
      IF (IBOUNC.GT.1) THEN
         IF (ABS(XPWAM(IBOUNC)-XPWAM(IBOUNC-1)).GT.1.E-6)&
         &DXWAM = MIN (DXWAM, ABS(XPWAM(IBOUNC)-XPWAM(IBOUNC-1)))
         IF (ABS(YPWAM(IBOUNC)-YPWAM(IBOUNC-1)).GT.1.E-6)&
         &DYWAM = MIN (DYWAM, ABS(YPWAM(IBOUNC)-YPWAM(IBOUNC-1)))
      ENDIF
      IF (KSPHER.EQ.0) THEN
!         determine lower left corner of WAM nesting grid if not given by the user
         IF (IBOUNC.EQ.1) THEN
            IF (XLON0.LT.-900.) THEN
               XLON0 = XLON
               XLAT0 = XLAT
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF (ITEST.GE.50) WRITE (PRINTF, "(' WAM step sizes: ', 2F12.4)") DXWAM, DYWAM
   EPS = 0.01 * MIN(DXWAM,DYWAM)

!     determine interpolation coefficients for all couples of
!     neighbouring WAM nest points

   DO IBNC1 = 1, NBOUNC
      DO IBNC2 = IBNC1+1, NBOUNC
         DXTEST = ABS(XPWAM(IBNC1)-XPWAM(IBNC2))
         DYTEST = ABS(YPWAM(IBNC1)-YPWAM(IBNC2))
         IF ((DXTEST.LT.EPS .AND. ABS(DYTEST-DYWAM).LT.EPS) .OR.&
         &(DYTEST.LT.EPS .AND. ABS(DXTEST-DXWAM).LT.EPS)) THEN
!           points IBNC1 and IBNC2 are neighbours
            IF (KSPHER.EQ.0) THEN
!             transform to local Cartesian coordinates
               XP1 = XPC + LENDEG*COS(PI*XLAT0/180.)*(XPWAM(IBNC1)-XLON0)
               YP1 = YPC + LENDEG*(YPWAM(IBNC1)-XLAT0)
               XP2 = XPC + LENDEG*COS(PI*XLAT0/180.)*(XPWAM(IBNC2)-XLON0)
               YP2 = YPC + LENDEG*(YPWAM(IBNC2)-XLAT0)
            ELSE
               XP1 = XPWAM(IBNC1) - XOFFS
               YP1 = YPWAM(IBNC1) - YOFFS
               XP2 = XPWAM(IBNC2) - XOFFS
               YP2 = YPWAM(IBNC2) - YOFFS
            ENDIF
!           Determine interpolation coefficients
            IBSP1 = NBSPEC+IBNC1
            IBSP2 = NBSPEC+IBNC2
            IIPT1 = 0
            RX  = XP2 - XP1
            RY  = YP2 - YP1
            RL2 = RX**2 + RY**2
            IF (RL2.GT.0.) THEN
               RX  = RX/RL2
               RY  = RY/RL2
!             check whether direction of (RX,RY) corresponds to ALPC + k * 90 degr
               PHI = ATAN2(RY,RX)
               DPHI = MOD(PHI-ALPC+1.25*PI,0.5*PI)-0.25*PI
               IF (ABS(DPHI) .LT. 0.1) THEN
!               loop over boundary of comp. grid, select points between
!               (XP1,YP1) and (XP2,YP2)
                  DO ISIDE = 1, 4
                     IF (ISIDE.EQ.1) THEN
                        IX1 = 1
                        IY1 = 1
                        IX2 = MXC
                        IY2 = 1
                        MIP = MXC
                     ELSE IF (ISIDE.EQ.2) THEN
                        IX1 = MXC
                        IY1 = 1
                        IX2 = MXC
                        IY2 = MYC
                        MIP = MYC
                     ELSE IF (ISIDE.EQ.3) THEN
                        IX1 = MXC
                        IY1 = MYC
                        IX2 = 1
                        IY2 = MYC
                        MIP = MXC
                     ELSE IF (ISIDE.EQ.4) THEN
                        IX1 = 1
                        IY1 = MYC
                        IX2 = 1
                        IY2 = 1
                        MIP = MYC
                     ENDIF
                     DO IP = 1, MIP-1
                        RR  = REAL(IP-1) / REAL(MIP-1)
                        IXP = IX1 + NINT(RR*REAL(IX2-IX1))
                        IYP = IY1 + NINT(RR*REAL(IY2-IY1))
                        INDXGR = KGRPNT(IXP,IYP)
                        IF (INDXGR.GT.1) THEN
                           XP = XCGRID(IXP,IYP)
                           YP = YCGRID(IXP,IYP)
!                     DISXY is relative distance from (XP,YP) to line
!                     (XP1,YP1) to (XP2,YP2)
                           DISXY = ABS(RX*(YP-YP1)-RY*(XP-XP1))
                           IF (DISXY.LT.0.1) THEN
!                       W2 is relative length of projection on line
!                       (XP1,YP1) to (XP2,YP2)
                              W2 = RX*(XP-XP1)+RY*(YP-YP1)
                              IF (W2.GT.-0.001 .AND. W2.LT.1.001) THEN
                                 IF (W2.LT.0.01) W2 = 0.
                                 IF (W2.GT.0.99) W2 = 1.
                                 IF (ITEST.GE.80) WRITE (PRTEST, "(' B.pnt', 2I5, 2F14.4, F6.3, ' from ', 2I3)") IXP, IYP,&
                                 &XP+XOFFS, YP+YOFFS, W2, IBNC1, IBNC2
                                 NBGRPT = NBGRPT + 1
                                 IIPT1 = IIPT1 + 1
                                 IIPT2 = IIPT2 + 1
                                 ALLOCATE(BGPTMP)
                                 BGPTMP%BGP(1) = INDXGR
!                         next item indicates type of boundary condition
                                 BGPTMP%BGP(2) = 1
                                 BGPTMP%BGP(3) = NINT(1000. * W2)
                                 BGPTMP%BGP(4) = IBSP2
                                 BGPTMP%BGP(5) = NINT(1000. * (1.-W2))
                                 BGPTMP%BGP(6) = IBSP1
                                 NULLIFY(BGPTMP%NEXTBGP)
                                 IF ( .NOT.LBGP ) THEN
                                    FBGP = BGPTMP
                                    CUBGP => FBGP
                                    LBGP = .TRUE.
                                 ELSE
                                    CUBGP%NEXTBGP => BGPTMP
                                    CUBGP => BGPTMP
                                 END IF
!                         test output if point is a test point
                                 IF (NPTST.GT.0) THEN
                                    DO IPTST = 1, NPTST
                                       IF (IXP.EQ.XYTST(2*IPTST-1)+MXF-1 .AND.&
                                       &IYP.EQ.XYTST(2*IPTST  )+MYF-1)&
                                       &WRITE (PRTEST, "(' B.pnt', 2I5, 2F14.4, F6.3, ' from ', 2I3)") IXP, IYP,&
                                       &XP+XOFFS, YP+YOFFS, W2, IBSP2, IBSP1
                                    ENDDO
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO
               ENDIF
            ENDIF
            IF (IIPT1.EQ.0) THEN
               WRITE (PRINTF, "(' Warning: no grid points on interval from ', 2F14.4, ' to ', 2F14.4)") XP1+XOFFS, YP1+YOFFS,&
               &XP2+XOFFS, YP2+YOFFS
            ENDIF
         ENDIF
      ENDDO
   ENDDO
   IF (IIPT2.EQ.0) CALL MSGERR (2,&
   &'no grid points on nested boundary')
   IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' boundary locations')") NBOUNC

!     deallocate arrays XPWAM, YPWAM and SPAUX

   DEALLOCATE (XPWAM, YPWAM, SPAUX)

   ALLOCATE(BSPFIL%BSPLOC(NBOUNC))
   DO IBC = 1, NBOUNC
      BSPFIL%BSPLOC(IBC) = NBSPEC + IBC
   ENDDO
   NBSPEC = NBSPEC + NBOUNC

!     store file reading parameters in array BFILED

   BSPFIL%BFILED(1)  = ISTATF
   BSPFIL%BFILED(2)  = -999999999
   BSPFIL%BFILED(3)  = -999999999
   BSPFIL%BFILED(4)  = NDSL
   BSPFIL%BFILED(5)  = NDSD
   BSPFIL%BFILED(6)  = IOPTT
   CALL COPYCH (BTYPE, 'T', BSPFIL%BFILED(7), 1, IERR)
   BSPFIL%BFILED(8)  = NBOUNC
   BSPFIL%BFILED(9)  = DORDER
   BSPFIL%BFILED(10) = NANG
   BSPFIL%BFILED(11) = 0
   BSPFIL%BFILED(12) = NFRE
!     ordering of data on file
   BSPFIL%BFILED(13) = 0
!     number of heading lines: per file, per time, per spectrum
   BSPFIL%BFILED(14) = NHEDF
   BSPFIL%BFILED(15) = NHEDT
   BSPFIL%BFILED(16) = NHEDS
!     quantity on file is variance density
   BSPFIL%BFILED(17) = 2

   IF (ITEST.GE.80) WRITE(PRINTF,"(' array BFILED: ', 2I4, 2(/,8I10))") NBFILS, NBSPEC,&
   &(BSPFIL%BFILED(II), II=1,16)

!     Rewind input file for proper start
   REWIND (NDSD)
!     read heading line
   IF (BTYPE.EQ.'WAMF') THEN
      DO IHD = 1, BSPFIL%BFILED(14)
         READ (NDSD, '(A)') HEDLIN
         IF (ITEST.GE.80) WRITE (PRINTF, "(' heading line: ', A)") HEDLIN
      ENDDO
   ELSE
      DO IHD = 1, BSPFIL%BFILED(14)
         READ (NDSD)
      ENDDO
   ENDIF

   RETURN
end subroutine BCWAMN

!*********************************************************************
!                                                                    *
SUBROUTINE BCWW3N (FBCNAM, BCTYPE, BSPFIL,&
&XCGRID, YCGRID, KGRPNT,&
&XYTST,  KGRBND)
   USE swan_legacy_io, ONLY: INAR2D, COPYCH
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INKEYW, KEYWIS, INDBLE, WRNKEY
!                                                                    *
!*********************************************************************

   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_BNDSPEC

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
!     40.05 : Ekaterini E. Kriezi
!     40.13 : Nico Booij
!     40.31 : Marcel Zijlema
!     40.41 : Marcel Zijlema
!
!  1. Updates
!
!     40.05, Aug. 00: new subroutine
!     40.13, Jan. 01: remove declarations of unused variables
!     40.31, Nov. 03: removing POOL-mechanism, reconsideration this
!                     subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     reads file data for WaveWatch III boundary condition
!
!  3. Method
!
!      open boundaries files
!      read from ASCII files the points where the energy density is given and
!      interpolate them to the grid points of the SWAN computational grid
!
!  4. Argument variables

   INTEGER, INTENT(IN)     ::  KGRPNT(MXC,MYC)
!                                 indirect addresses of computational grid points
   INTEGER, INTENT(IN)     ::  KGRBND(*)
!                                 array of boundary grid points
   INTEGER, INTENT(IN)     ::  XYTST(*)
!                                 array of (ix,iy) of test points
   REAL, INTENT(IN)        ::  XCGRID(MXC,MYC), YCGRID(MXC,MYC)
!                                 coordinates of computational grid points
!     FBCNAM  char  inp    filename of boundary data file
!     BCTYPE  char  inp    boundary condition type, is 'WW3N' in this case

   CHARACTER(LEN=*) :: FBCNAM, BCTYPE

   TYPE(BSPCDAT) :: BSPFIL

!  5. Parameter variables
!
!     --
!
!  6. Local variables
!
!     IENT         number of entries into this subroutine
!     IBC          spectrum counter

   INTEGER, SAVE :: IENT = 0
   INTEGER   :: IBC

   REAL,    ALLOCATABLE :: FRQ_ARRAY(:), DIR_ARRAY(:)

!     IHD          counter of heading lines
!     WWDATE       date in boundary file
!     WWTIME       time in boundary file

   INTEGER   :: WWDATE, WWTIME

!     ISTATF    if >0 file contains nonstationary data
!     NDSL      unit ref num of namelist file
!     NDSD      unit ref num of data file
!     IOSTAT    io status
!     IERR      error status
!     NBOUNC    number of boundary locations
!     NANG      number of directions on file
!     NFRE      number of frequencies on file
!     DORDER    if <0 order of reading directions is reversed
!     IOPTT     time reading option
!     IBOUNC    counter of boundary points
!     IGRBND    counter of boundary grid(swan grid)  points
!     II        counter
!     NHEDF     number of heading lines per file
!     NHEDT     number of heading lines per time step
!     NHEDS     number of heading lines per spectrum

   INTEGER            :: ISTATF, NDSL, NDSD, IOSTAT, IERR
   INTEGER            :: NBOUNC, NANG, NFRE
   INTEGER            :: IBOUNC
   INTEGER            :: DORDER, IOPTT
   INTEGER            :: NHEDF, NHEDT, NHEDS, NBGRPT_PREV
   INTEGER            :: IHD, IFRE, IANG, II, IIPT2

!     DONALL : logic arguments declare if the boundary is open or close

   LOGICAL   :: DONALL

!     DUM_A     real number used for reading a file but not used in any
!     XLON      longitude
!     XLAT      latitude
!     XP2       problem coordinate of a boundary location
!     YP2       problem coordinate of a boundary location
!     DIRRD1
!     NBGRPT_PREV is the prevous number of NBGRPT
!     IIPT2 counter use for the chekinf if there are grid points on nested boundary

   REAL               :: DUM_A, XLON, XLAT,XP2,YP2,DIRRD1

   CHARACTER (LEN=4)  :: BTYPE
!                           type of boundary cond.
   CHARACTER (LEN=21) :: HEDLINT
!                           WW3 version
   CHARACTER (LEN=30) :: GNAME
!                           name of test case readed from b. file
   CHARACTER (LEN=10) :: PTNME
!                           name of b. point
!     XLON0     longitude of origin of computational grid
!     XLAT0     latitude of origin of computational grid

   REAL(KIND=KIND(0.0D0))   :: XLON0, XLAT0

!       FBCNAM  char  inp    filename of boundary data file
!       BCTYPE  char  inp    if value is "NEST": nesting b.c.
!       XCGRID  real  inp    x-coordinate of computational grid points
!       YCGRID  real  inp    y-coordinate of computational grid points
!       KGRPNT  int   inp    indirect addresses of grid points
!       XYTST   int   inp    ix, iy of test points
!
!  8. Subroutines used
!
!     Ocean Pack command reading routines
!     SWBCPT, STPNOW


!  9. Subroutines calling
!
!     SWREAD
!
! 10. Error messages
!
!     ---
!
!  11. Remarks
!
!       data concerning boundary files are stored in array BFILED
!       there is a subarray for each file; it contains:
!       1.  status; 0: stationary, 1: nonstat, -1: exhausted
!       2.  time of boundary values read one before last
!       3.  time of boundary values read last
!       4.  NDSL: unit ref. num. of file containing filenames
!       5.  NDSD: unit ref. num. of file containing data
!       6.  time coding option for reading time from data file
!       8.  number of locations for which spectra are in the file
!       9.  order of reading directional information
!       10. number of spectral directions of spectra on file
!       12. number of spectral frequencies
!       14. number of heading lines per file
!       15. number of heading lines per time step
!       16. number of heading lines per spectrum
!       17. =1: energy dens., =2: variance density, =3 variance energy density (k)
!       18. =1: Cartesian direction, =2: Nautical dir.
!       19. =1: direction spread in degr, =2: Power of Cos.
!       20.  depth of boundary points
!
!  12. Structure
!
!       ----------------------------------------------------------------
!       Unformatted WW3 point output transfer file -- b.c. type is WW3U.
!       Formatted WW3 point output transfer file -- b.c. type is WW3F.
!       -----------------------------------------------------------
!
!          Read spectral directions from file and write them into
!          array BSPDIR
!          Read spectral frequencies from fileand write them into
!          array BSPFRQ
!          For all boundary points do
!            read location from data file
!            transform into local cartesian coordinates (if nesesery)
!            Then calculate data on grid points
!
!       ----------------------------------------------------------------
!       Put file characteristics into array BFILED
!       ----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE (IENT, 'BCWW3N')

!     Process required UNFormatted/FREe keyword
   CALL INKEYW ('REQ', ' ')
   IF (KEYWIS('FRE')) THEN
      BTYPE = 'WW3F'
   ELSE IF (KEYWIS('UNF')) THEN
      BTYPE = 'WW3U'
   ELSE
      CALL WRNKEY
      CALL MSGERR (4, 'The BOUndnest3 WW3 command requires '//&
      &'UNFormatted or FREe as the first keyword.' )
   ENDIF
   IF (STPNOW()) RETURN

!     Process optional CLOS/OPEN keyword
!     if keyword is OPEN (boundary is not a closed contour) then
!        DONALL is TRUE  and the nesting boundary remain open
!     else (default case)
!        DONALL is FALSE  and boundary is close and interpolation
!        between the last and the first point will be done

   CALL INKEYW ('STA', 'CLOS')
   IF (KEYWIS('OPEN')) THEN
      DONALL = .TRUE.
   ELSE IF (KEYWIS('CLOS')) THEN
      DONALL = .FALSE.
   ELSE
      CALL WRNKEY
   ENDIF
   IF (STPNOW()) RETURN

!     NDSL unit ref number for namelist files
   NDSL = 0
   ISTATF = 1
!     number of heading lines: per file, per time, per spectrum
   NHEDF = 0
   NHEDT = 0
   NHEDS = 0
   DORDER  = -1
   IOPTT = 1
   IIPT2 = 0

!     open data file NDSD unit ref number for data files
   NDSD = 0
   IOSTAT = 0
   IF (BTYPE.EQ.'WW3F') THEN
      CALL FOR (NDSD, FILENM , 'OF', IOSTAT)
   ELSE
      CALL FOR (NDSD, FILENM , 'OU', IOSTAT)
   ENDIF
   IF (STPNOW()) RETURN

!     Read header
   IF (BTYPE.EQ.'WW3F') THEN
      READ (NDSD, "(1X,A21,1X,1X,3I6,1X,1X,A30,1X)") HEDLINT, NFRE, NANG, NBOUNC, GNAME
   ELSE
      READ (NDSD)       HEDLINT, NFRE, NANG, NBOUNC, GNAME
   ENDIF
   IF (HEDLINT .NE. 'WAVEWATCH III SPECTRA')&
   &CALL MSGERR (3, 'file is not a WW3 spectral file')
   IF (NBOUNC.LT.2)  CALL MSGERR&
   &(3, 'SWAN need at least 2 boundary points for nesting')

!     --- initialize array BFILED of BSPFIL
   BSPFIL%BFILED = 0

!     read frequencies from WW3 boundary file

   ALLOCATE (FRQ_ARRAY(1:NFRE), DIR_ARRAY(1:NANG) )

!     read frequency
!     FRQ_ARRAY(IFRE) =   SIG(IK)/(2*PI)

   IF (BTYPE.EQ.'WW3F') THEN
      READ (NDSD,"(8E10.3)") (FRQ_ARRAY(IFRE) ,IFRE=1,NFRE)
   ELSE
      READ (NDSD)      (FRQ_ARRAY(IFRE) ,IFRE=1,NFRE)
   ENDIF

   ALLOCATE(BSPFIL%BSPFRQ(NFRE))
   DO IFRE = 1, NFRE
      BSPFIL%BSPFRQ(IFRE) =  FRQ_ARRAY(IFRE)*2*PI
   ENDDO

   IF (ITEST.GE.60) THEN
      WRITE(PRTEST,*) ' HEDLINT ',' NFRE ',' NANG ',' NBOUNC ',&
      &' GNAME'
      WRITE (PRTEST,*) HEDLINT, NFRE, NANG, NBOUNC, GNAME
      WRITE (PRTEST,*) 'Frequencies read from boundary file ', FILENM
      WRITE (PRTEST,*) (FRQ_ARRAY(IFRE),IFRE = 1,NFRE)
   ENDIF

!     read direction from WW3 boundary file
!     DIR_ARRAY(IANG) = MOD(2.5*PI-TH(ITH),TPI)
!     there are in radians but is not in right order related to SWAN

   IF (BTYPE.EQ.'WW3F') THEN
      READ (NDSD,"(7E11.3)") (DIR_ARRAY(IANG),IANG=1,NANG)
   ELSE
      READ (NDSD)      (DIR_ARRAY(IANG),IANG=1,NANG)
   ENDIF

!     put values in right order. The value of the DIR_ARRAY(i) should be
!     smaller that the DIR_ARRAY(i-1)
!     in the opposite situation make DIR_ARRAY(i) = DIR_ARRAY(i) - 2*PI

   DIR_ARRAY(:) = PI*DNORTH/180 - DIR_ARRAY(:)

   ALLOCATE(BSPFIL%BSPDIR(NANG))
   DO IANG = 1, NANG
      IF (IANG.EQ.1) THEN
         BSPFIL%BSPDIR(1) = DIR_ARRAY(IANG)
         DIRRD1 = BSPFIL%BSPDIR(1)
      ELSE
         IF (DIR_ARRAY(IANG).LT.DIRRD1) THEN
            BSPFIL%BSPDIR(IANG) = 2*PI+DIR_ARRAY(IANG)
            DIRRD1 = BSPFIL%BSPDIR(IANG)
         ELSE
            BSPFIL%BSPDIR(IANG) = DIR_ARRAY(IANG)
            DIRRD1 =  BSPFIL%BSPDIR(IANG)
         ENDIF
      ENDIF
   ENDDO

   IF(ITEST.GE.60) THEN
      WRITE (PRTEST,*) 'Directions read from boundary file ',&
      &FILENM
      WRITE (PRTEST,"(7E11.3)") (DIR_ARRAY(IANG),IANG = 1,NANG)
   ENDIF

!     Time
   IF (BTYPE.EQ.'WW3F') THEN
      READ (NDSD, "(I8.8,I7.6)") WWDATE,WWTIME
   ELSE
      READ (NDSD)      WWDATE,WWTIME
   ENDIF

!     Read from boundary file info about the boundary points(b.p): name
!     geographical location of b.p., depth, wind u-velocity  and direction at the b.p.
!     current velocity and direction at the b.p.
!
!     If  DONALL = .TRUE. boundary data correspond to an open boundary otherwise
!     it is continue the interpolation of the grid point between the last and the
!     first point

   DO IBOUNC = 1, NBOUNC
      IERR = 0

!       latitude =  XLAT
!       longitude = XLON
!       A real which is not used in the computation

      IF (BTYPE.EQ.'WW3F') THEN
         READ (NDSD,"(1X,A10,1X,2F7.2,F10.1,2(F7.2,F6.1))") PTNME, XLAT, XLON, DUM_A, DUM_A,&
         &DUM_A, DUM_A, DUM_A
      ELSE
         READ (NDSD)     PTNME, XLAT, XLON, DUM_A, DUM_A,&
         &DUM_A, DUM_A, DUM_A
      ENDIF
!       Pass over the lines where the energy spectra is written in the boundary file.
!       The energy spectra is going to be read later, in the subroutine

      IF (BTYPE.EQ.'WW3F') THEN
         READ (NDSD,"(7E11.3)") ((DUM_A, IFRE = 1,NFRE),IANG = 1,NANG)
      ELSE
         READ (NDSD)     ((DUM_A, IFRE = 1,NFRE),IANG = 1,NANG)
      ENDIF

      IF (ITEST.GE.80) THEN
         WRITE (PRTEST, *) ' B. spectrum WW3 ', IBOUNC, XLON,&
         &XLAT, IERR
      ENDIF

!       in case of nesting coordinates on file are used to determine interpolation
!       coefficients

      IF (KSPHER.EQ.0) THEN

!       if SWAN uses Cartesian coordinates, then transform the spherical coordinates
!       of the boundary point to local Cartesian coordinates

         IF (IBOUNC.EQ.1) THEN
            CALL INDBLE('XGC',XLON0,'REQ',-999.D0)
            CALL INDBLE('YGC',XLAT0,'REQ',-999.D0)
            IF (XLON0.LT.-900.) THEN
               XLON0 = (XOFFS -XPC)/LENDEG
               XLAT0 = (YOFFS-YPC)/LENDEG
            ENDIF
         ENDIF

         XP2 = XPC + LENDEG*COS(PI*XLAT0/180.)*(XLON-XLON0)
         YP2 = YPC + LENDEG*(XLAT-XLAT0)

      ELSE
         XP2 = XLON-XOFFS
         YP2 = XLAT-YOFFS
      ENDIF

!       --- interpolate the boundaries points to the grid points of
!           the SWAN computational grid

      NBGRPT_PREV = NBGRPT
      CALL SWBCPT (  XCGRID, YCGRID,&
      &KGRPNT, XYTST,  KGRBND,XP2,YP2,IBOUNC,&
      &NBOUNC, DONALL )
!       check if the grid points are on nested boundary.
!       if not, stop the calculation and give an error message
      IF (NBGRPT.NE.NBGRPT_PREV) THEN
         IIPT2 = IIPT2+1
      ENDIF
   ENDDO

   IF (IIPT2.EQ.0) CALL MSGERR (2,&
   &'no grid points on nested boundary')

   IF (ITEST.GE.60) WRITE (PRTEST,"(I6, ' boundary locations')") NBOUNC

!     quantity on file is energy density
   BSPFIL%BFILED(17) = 1

!     number of heading lines: per file, per time, per spectrum
   IF (BTYPE.EQ.'WW3F') THEN
      NHEDF = NHEDF + CEILING(NFRE/8.) + CEILING(NANG/7.)+1
!       NHEDT: calculated in the RBFILE subroutine for each time step
      NHEDS = 2
   ELSE
      NHEDF = 3
      NHEDT = 0
      NHEDS = 1
   ENDIF

   ALLOCATE(BSPFIL%BSPLOC(NBOUNC))
   DO IBC = 1, NBOUNC
      BSPFIL%BSPLOC(IBC) = NBSPEC + IBC
   ENDDO

   NBSPEC = NBSPEC + NBOUNC

!     store file reading parameters in array BFILED

   BSPFIL%BFILED(1)  = ISTATF
   BSPFIL%BFILED(2)  = -999999999
   BSPFIL%BFILED(3)  = -999999999
   BSPFIL%BFILED(4)  = NDSL
   BSPFIL%BFILED(5)  = NDSD
   BSPFIL%BFILED(6)  = IOPTT
   CALL COPYCH (BTYPE, 'T', BSPFIL%BFILED(7), 1, IERR)
   BSPFIL%BFILED(8)  = NBOUNC
   BSPFIL%BFILED(9)  = DORDER
   BSPFIL%BFILED(10) = NANG
   BSPFIL%BFILED(11) = 0
   BSPFIL%BFILED(12) = NFRE
!     ordering of data on file
   BSPFIL%BFILED(13) = 0
!     number of heading lines: per file, per time, per spectrum
   BSPFIL%BFILED(14) = NHEDF
   BSPFIL%BFILED(15) = NHEDT
   BSPFIL%BFILED(16) = NHEDS
!     quantity on file is energy density (k)
   BSPFIL%BFILED(17) = 3

   IF (ITEST.GE.80) WRITE(PRINTF,"(' array BFILED: ', 2I4, 2(/,8I10))") NBFILS, NBSPEC,&
   &(BSPFIL%BFILED(II), II=1,16)

!      Rewind input file for proper start
   REWIND (NDSD)
!     read per file heading lines
   IF (BTYPE.EQ.'WW3U') THEN
      DO IHD = 1, BSPFIL%BFILED(14)
         READ (NDSD)
      ENDDO
   ENDIF


   DEALLOCATE (FRQ_ARRAY,DIR_ARRAY)

   RETURN

end subroutine BCWW3N

!************************************************************************

SUBROUTINE SWBCPT ( XCGRID, YCGRID,&
&KGRPNT, XYTST,  KGRBND,XP2,YP2,IBOUNC,&
&NBOUNC,DONALL )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!************************************************************************

   USE OCPCOMM4
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_BNDSPEC
   USE M_PARALL
   USE SwanGriddata
   USE SwanGridobjects

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
!     30.73, 40.13: Nico Booij
!     40.05: Ekaterini E. Kriezi
!     40.31: Tim Campbell and John Cazes
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.14: Nico Booij
!
!  1. Updates
!
!     40.05, Aug. 00: remove the code to a separate subroutine
!     40.13, Jan. 01: remove declarations of unused variables
!     40.31, Jul. 03: initializations XP0, XP1, YP0, YP1
!     40.31, Nov. 03: removing POOL mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.14, Jul. 10: error in nesting unstructured grid corrected
!
!  2. Purpose
!
!     interpolation of own boundary points to nesting boundary points
!
!  3. Method
!
!     calculate interpolation coefficients between the nesting boundary
!     the SWAN computational grid
!
!  4. Argument variables
!
!     NBOUNC:     max number of boundaries points
!     IBOUNC:     counter


   INTEGER, INTENT(IN)     ::  KGRPNT(MXC,MYC)  ! indirect addresses
   INTEGER, INTENT(IN)     ::  KGRBND(*)        ! array of boundary g
   INTEGER, INTENT(IN)     ::  XYTST(*)         ! array of (ix,iy) of

   INTEGER, INTENT(IN)     ::  IBOUNC,NBOUNC

   REAL                    ::  XP2, YP2         ! coordinates of a po
   REAL, INTENT(IN)        ::  XCGRID(MXC,MYC), YCGRID(MXC,MYC)  ! co

!     DONALL : logic arguments declare if the nesting boundary is open or close
!              it is defined by the users

   LOGICAL, INTENT(INOUT)  ::  DONALL

!  5. Parameter variables
!
!     --
!
!  6. Local variables
!
!     XP1       problem coordinate of a boundary location
!     YP1       problem coordinate of a boundary location
!     XP2       problem coordinate of a boundary location
!     YP2       problem coordinate of a boundary location
!     DISXY     distance in (x,y)-space
!     W2        is relative length of projection on line
!     IIPT1     counter checking the grid points related to the nesting

   INTEGER, SAVE      :: IBSP0 = 1, IBSP1 = 1, IENT = 0
   INTEGER            :: IBSP2,IGRBND,INDXGR
   INTEGER            :: IXP,IYP,IIPT1

   REAL, SAVE         :: XP0=0., XP1=0., YP0=0., YP1=0.
   REAL               :: XP, YP, RX, RY, RL2
   REAL               :: DX1P, DY1P, DXP2, DYP2, DXCIRC(3)
   REAL               :: DISXY, DOTR1, DOTR2, W2
   TYPE(BGPDAT), POINTER :: BGPTMP

   TYPE(verttype), DIMENSION(:), POINTER :: vert

!  7. Common blocks used
!
!
!  8. Subroutines Used
!
!
!  9. Subroutines calling
!
!      BCFILE : open and read Swan nesting files
!      BCWW3N : open and read WW3 nesting files
!
!  10. Error messages
!
!       ---
!
!  11. Remarks
!
!  12. Structure
!
!      For all computational grid points on boundary do
!          if point is located between nest file grid points
!             calculate interpolation coefficients
!          if DONALL is TRUE
!             the nesting boundary remain open
!          else DONALL is FALSE (default case)
!             boundary is close, it do interpolation between the last and the first point
!             put interpolation coefficients into array BGRIDP
!
!  13. Source text

   CALL STRACE (IENT, 'SWBCPT')

   IF (NBOUNC.EQ.1) CALL MSGERR (2,&
   &'Nesting procedure does not work if file has only 1 spectrum')

!     point to vertices

   vert => gridobject%vert_grid

   IBSP2 = NBSPEC+IBOUNC
   IIPT1 = 0

   boundary_interval: DO
   IF (IBOUNC.EQ.1) THEN
!        first point found in a spectral input file
      XP0   = XP2
      YP0   = YP2
      IBSP0 = IBSP2
      IIPT1 = 1
   ELSE
!        RX, RY difference vector between consecutive points of spectral file  41.14
      IF (KSPHER.EQ.0) THEN
         RX  = XP2 - XP1
      ELSE
         DXCIRC(1) = ( XP2 - XP1)-360.0
         DXCIRC(2) = ( XP2 - XP1)
         DXCIRC(3) = ( XP2 - XP1)+360.0
         RX = DXCIRC(MINLOC(ABS(DXCIRC),1))
      ENDIF
      RY  = YP2 - YP1
      RL2 = RX**2 + RY**2
      IF (RL2.GT.0.) THEN
         RX  = RX/RL2
         RY  = RY/RL2
!           WRITE(PRINTF,'(A,2I6,6F12.4)') 'BC_SEARCH_1: ',IBSP1,IBSP2,
!     &       XP1+XOFFS,YP1+YOFFS,XP2+XOFFS,YP2+YOFFS,RX,RY
!
!          loop over boundary of computational grid, select boundary points
!          between (XP1,YP1) and (XP2,YP2)

         IF (OPTG.EQ.5) THEN
            DO IXP = 1, nverts
               IF ( vert(IXP)%atti(VMARKER) == 1 .AND.&
               &vert(IXP)%atti(VBC) == 0     .AND.&
               &vmark(IXP) < excmark ) THEN
                  XP = vert(IXP)%attr(VERTX)
                  YP = vert(IXP)%attr(VERTY)

                  IF (KSPHER.EQ.0) THEN
                     DX1P = XP  - XP1
                     DY1P = YP  - YP1
                     DXP2 = XP2 - XP
                     DYP2 = YP2 - YP
                  ELSE
!                   compute delta_lon that allows for either, both,
!                   or neither XP,XP1 to be -180 to 180 or 0 to 360
                     DXCIRC(1) = ( XP - XP1)-360.0
                     DXCIRC(2) = ( XP - XP1)
                     DXCIRC(3) = ( XP - XP1)+360.0
                     DX1P = DXCIRC(MINLOC(ABS(DXCIRC),1))
                     DY1P = ( YP - YP1 )
                     DXCIRC(1) = ( XP2 - XP)-360.0
                     DXCIRC(2) = ( XP2 - XP)
                     DXCIRC(3) = ( XP2 - XP)+360.0
                     DXP2 = DXCIRC(MINLOC(ABS(DXCIRC),1))
                     DYP2 = ( YP2 - YP )
                  ENDIF

!                DISXY is relative distance from (XP,YP) to line
!                (XP1,YP1) to (XP2,YP2) with respect to the length of that line
!                DOTR1 is relative length of projection on line (XP1,YP1) to (XP2,YP2)

                  DISXY = ABS ( RX*DY1P - RY*DX1P )
                  DOTR1 = RX*DX1P + RY*DY1P
                  DOTR2 = RX*DXP2 + RY*DYP2
!                 WRITE(PRINTF,'(A,I6,5F12.4)') 'BC_SEARCH_2: ',IXP,
!     &             XP+XOFFS,YP+YOFFS,DOTR1,DOTR2,DISXY
!
!                check if boundary point is between (XP1,YP1) and (XP2,YP2)
                  IF ( DOTR1.GE.0.AND.DOTR2.GE.0.AND.DISXY.LE.0.1 ) THEN
                     W2 = DOTR1
                     IF (W2.LT.0.001) W2 = 0.
                     IF (W2.GT.0.999) W2 = 1.
                     IF (ITEST.GE.80) WRITE (PRTEST, *) ' B.pnt',&
                     &IXP, XP, YP, W2, IBSP2, 1.-W2, IBSP1
                     NBGRPT = NBGRPT + 1
                     IIPT1  = IIPT1  + 1
!                    WRITE(PRINTF,'(A,3I6,2(F6.3,I6))') 'BC_SEARCH_3: ',
!     &              IXP,NBGRPT,IIPT1,1.-W2,IBSP1,W2,IBSP2

                     ALLOCATE(BGPTMP)
                     IF (.NOT.PARLL) THEN
                        BGPTMP%BGP(1) = IXP
                     ELSE
                        BGPTMP%BGP(1) = ivertg(IXP)
                     ENDIF
!                   next item indicates type of boundary condition
                     BGPTMP%BGP(2) = 1
                     BGPTMP%BGP(3) = NINT(1000. * W2)
                     BGPTMP%BGP(4) = IBSP2
                     BGPTMP%BGP(5) = NINT(1000. * (1.-W2))
                     BGPTMP%BGP(6) = IBSP1
                     vert(IXP)%atti(VBC) = 1
                     NULLIFY(BGPTMP%NEXTBGP)
                     IF ( .NOT.LBGP ) THEN
                        FBGP = BGPTMP
                        CUBGP => FBGP
                        LBGP = .TRUE.
                     ELSE
                        CUBGP%NEXTBGP => BGPTMP
                        CUBGP => BGPTMP
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ELSE

!           KGRBND grid addresses on boundary points, NGRBND number of grid points
!           on computational grid boundary

            DO IGRBND = 1, NGRBND
               IXP = KGRBND(2*IGRBND-1)
               IYP = KGRBND(2*IGRBND)

               IF (IXP.GT.0 .AND.IYP.GT.0) THEN
                  INDXGR = KGRPNT(IXP,IYP)
                  XP = XCGRID(IXP,IYP)
                  YP = YCGRID(IXP,IYP)

                  IF (KSPHER.EQ.0) THEN
                     DX1P = XP  - XP1
                     DY1P = YP  - YP1
                     DXP2 = XP2 - XP
                     DYP2 = YP2 - YP
                  ELSE
!                   compute delta_lon that allows for either, both,
!                   or neither XP,XP1 to be -180 to 180 or 0 to 360
                     DXCIRC(1) = ( XP - XP1)-360.0
                     DXCIRC(2) = ( XP - XP1)
                     DXCIRC(3) = ( XP - XP1)+360.0
                     DX1P = DXCIRC(MINLOC(ABS(DXCIRC),1))
                     DY1P = ( YP - YP1 )
                     DXCIRC(1) = ( XP2 - XP)-360.0
                     DXCIRC(2) = ( XP2 - XP)
                     DXCIRC(3) = ( XP2 - XP)+360.0
                     DXP2 = DXCIRC(MINLOC(ABS(DXCIRC),1))
                     DYP2 = ( YP2 - YP )
                  ENDIF

!                DISXY is relative distance from (XP,YP) to line
!                (XP1,YP1) to (XP2,YP2) with respect to the length of that line
!                DOTR1 is relative length of projection on line (XP1,YP1) to (XP2,YP2)

                  DISXY = ABS( RX*DY1P - RY*DX1P )
                  DOTR1 = RX*DX1P + RY*DY1P
                  DOTR2 = RX*DXP2 + RY*DYP2
!                 WRITE(PRINTF,'(A,2I6,5F12.4)') 'BC_SEARCH_2: ',IXP,IYP,
!     &             XP+XOFFS,YP+YOFFS,DOTR1,DOTR2,DISXY
!
!                check if boundary point is between (XP1,YP1) and (XP2,YP2)
                  IF ( DOTR1.GE.0.AND.DOTR2.GE.0.AND.DISXY.LE.0.1 ) THEN
                     W2 = DOTR1
                     IF (W2.LT.0.001) W2 = 0.
                     IF (W2.GT.0.999) W2 = 1.
                     IF (ITEST.GE.80) WRITE (PRTEST, *) ' B.pnt',&
                     &IXP, XP, YP, W2, IBSP2, 1.-W2, IBSP1
                     NBGRPT = NBGRPT + 1
                     IIPT1  = IIPT1  + 1
!                    WRITE(PRINTF,'(A,4I6,2(F6.3,I6))') 'BC_SEARCH_3: ',
!     &              IXP,IYP,NBGRPT,IIPT1,1.-W2,IBSP1,W2,IBSP2

                     ALLOCATE(BGPTMP)
                     BGPTMP%BGP(1) = INDXGR
!                   next item indicates type of boundary condition
                     BGPTMP%BGP(2) = 1
                     BGPTMP%BGP(3) = NINT(1000. * W2)
                     BGPTMP%BGP(4) = IBSP2
                     BGPTMP%BGP(5) = NINT(1000. * (1.-W2))
                     BGPTMP%BGP(6) = IBSP1
                     NULLIFY(BGPTMP%NEXTBGP)
                     IF ( .NOT.LBGP ) THEN
                        FBGP = BGPTMP
                        CUBGP => FBGP
                        LBGP = .TRUE.
                     ELSE
                        CUBGP%NEXTBGP => BGPTMP
                        CUBGP => BGPTMP
                     END IF

!                   test output if point is a test point

                     IF (NPTST.GT.0) THEN
                        DO IPTST = 1, NPTST
                           IF (IXP.EQ.XYTST(2*IPTST-1)+MXF-1 .AND.&
                           &IYP.EQ.XYTST(2*IPTST  )+MYF-1)&
                           &WRITE (PRTEST, "(' B.pnt', 2I5, 2F9.0, F6.3, 2I3)")&
                           &IXP-1,IYP-1, XP+XOFFS, YP+YOFFS, W2, IBSP2,&
                           &IBSP1
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
      ENDIF
   ENDIF

!      IF (IIPT1.EQ.0) THEN
!         WRITE (PRINTF, 218) XP1+XOFFS, YP1+YOFFS,
!     &   XP2+XOFFS, YP2+YOFFS
! 218     FORMAT (' Warning: no grid points on interval from ', 2F12.4,
!     &           ' to ', 2F12.4)
!      ENDIF

   XP1   = XP2
   YP1   = YP2
   IBSP1 = IBSP2

   IF (IBOUNC.EQ.NBOUNC) THEN
      IF (.NOT. DONALL) THEN

!           process grid points between last and first boundary point

         DONALL = .TRUE.
         XP2    = XP0
         YP2    = YP0
         IBSP2  = IBSP0
         CYCLE boundary_interval
      ENDIF
   ENDIF

   EXIT boundary_interval
   END DO boundary_interval

   RETURN

end subroutine SWBCPT

!*********************************************************************
!                                                                    *
LOGICAL FUNCTION BOUNPT (IX,IY,KGRPNT)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                    *
!*********************************************************************

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
!     40.00, 40.03: Nico Booij
!
!  1. UPDATE
!
!       Feb. 1998, ver. 40.00: new subroutine
!       40.03, Sep. 00: inconsistency with manual corrected
!
!  2. PURPOSE
!
!       determine whether a grid point is a point where a boundary condition
!       can be applied
!
!  3. METHOD
!
!
!  4. PARAMETERLIST
!
!       IX, IY  int   inp    grid point indices
!       KGRPNT  int   inp    indirect addresses of grid points
!
!  5. SUBROUTINES CALLING
!
!       BCFILE
!
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
!     KGRPNT(IX,IY)=1 means that (IX,IY) is not an active grid point
!
!  9. STRUCTURE
!
!     -----------------------------------------------------------------
!     Make BOUNPT = False
!     If the grid point is not active
!     Then return
!     -----------------------------------------------------------------
!     If grid point is on the outer boundary
!     Then make BOUNPT = True
!          return
!     -----------------------------------------------------------------
!     If a neighbouring grid point is inactive
!     Then make BOUNPT = True
!          return
!     -----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER, SAVE :: IENT = 0
   INTEGER IX, IY, KGRPNT(MXC,MYC)
   CALL STRACE (IENT, 'BOUNPT')

   BOUNPT = .FALSE.

   IF (IX.LE.0)   RETURN
   IF (IY.LE.0)   RETURN
   IF (IX.GT.MXC) RETURN
   IF (IY.GT.MYC) RETURN

!     If the grid point is not active
!     Then return

   IF (KGRPNT(IX,IY).LE.1) RETURN

!     If grid point is on the outer boundary
!     Then make BOUNPT = True
!          return

   IF (IX.EQ.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (IX.EQ.MXC) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (IY.EQ.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (IY.EQ.MYC) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF

!     If a neighbouring grid point is inactive
!     Then make BOUNPT = True
!          return

   IF (KGRPNT(IX-1,IY).LE.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (KGRPNT(IX+1,IY).LE.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (KGRPNT(IX,IY-1).LE.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   IF (KGRPNT(IX,IY+1).LE.1) THEN
      BOUNPT = .TRUE.
      RETURN
   ENDIF
   RETURN
end function BOUNPT
!*********************************************************************
!                                                                    *
SUBROUTINE RETSTP (LXYTST, XYTST, KGRPNT, KGRBND, XCGRID, YCGRID,&
&SPCSIG, SPCDIR)
   USE swan_coordinate_input, ONLY: READXY, REFIXY
   USE swan_services, ONLY: CVMESH
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE
   USE swan_input_parser, ONLY: INCSTR, ININTG, INKEYW, KEYWIS, WRNKEY
!                                                                    *
!*********************************************************************

   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
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
!     30.82: IJsbrand Haagsma
!     40.00, 40.13: Nico Booij
!     34.01: Jeroen Adema
!     40.04: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.80: Marcel Zijlema
!     41.75: Erick Rogers
!
!  1. Updates
!
!     40.00, Apr. 98: New subroutine
!     30.82, Oct. 98: Updated description of several variables
!     34.01, Feb. 99: Introducing STPNOW
!     40.03, Dec. 99: XOFFS and YOFFS introduced in write statements
!            May  00: ITMOPT written to heading of file instead of 1
!     40.04, Aug. 00: Added error message if testpoints are defined
!                     before bottom grid is read
!     40.13, Jan. 01: two output strings corrected
!            May  01: two incorrect units changed from m2/2 to m2/s
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL-mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.80, Jun. 07: extension to unstructured grids
!     41.75, Jan. 19: adding sea ice
!
!  2. PURPOSE
!
!     read test points, generate output point set 'TESTPNTS',
!     read source term filenames
!
!  3. METHOD
!
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

! i   LXYTST: Maximum length of array XYTST
! i   XYTST : Grid point indices of test points
!
!     MPTST : Maximum number of test points

   INTEGER LXYTST, MPTST
   INTEGER XYTST(LXYTST)
   INTEGER ICHECK1,ICHECK2 ! for checking that
   ! hardwired # quantities is correct
!
!  5. SUBROUTINES CALLING
!
!     SWREAD
!
!  6. SUBROUTINES USED


!  7. Common blocks used
!
!
!  8. REMARKS
!
!
!  9. STRUCTURE
!
!     -----------------------------------------------------------------
!     Repeat
!         read (i,j) identifying a test point
!         store values in array XYTST
!     -----------------------------------------------------------------
!     Generate output point set 'TESTPNTS'
!     write coordinates of test points into array OUTDA
!     -----------------------------------------------------------------
!     If 1D output of source terms is requested
!     Then open file
!          write general data into the file
!     -----------------------------------------------------------------
!     If 2D output of source terms is requested
!     Then open file
!          write general data into the file
!     -----------------------------------------------------------------
!
! 10. SOURCE TEXT

   INTEGER   KGRPNT(MXC,MYC), KGRBND(*)
   INTEGER, SAVE :: IENT = 0
   INTEGER   ID, IERR, ILPOS, IS, K
   REAL      XC, XP, YC, YP
      LOGICAL :: LOCGRI
   TYPE(OPSDAT), POINTER :: OPSTMP
   CALL STRACE (IENT, 'RETSTP')

   IF (OPTG.NE.5) THEN
      MPTST = LXYTST/2
   ELSE
      MPTST = LXYTST
   ENDIF
   CALL INKEYW ('STA','IJ')
   IF (MCGRD.GT.1 .OR. nverts.GT.0) THEN
      IF (KEYWIS('XY')) THEN
         LOCGRI = .TRUE.
      ELSE IF (KEYWIS('IJ')) THEN
         LOCGRI = .FALSE.
      ELSE
         CALL WRNKEY
      ENDIF
   ELSE
      CALL MSGERR(3,&
      &'command READ BOT or READ UNSTRUC must precede command TEST')
   ENDIF

   test_point_loop: DO
      IF (NPTST.GT.MPTST) THEN
         CALL MSGERR (2, 'Too many test points')
         EXIT test_point_loop
      END IF
   point_input: BLOCK
   IF (LOCGRI) THEN
      CALL READXY ('X','Y',XP,YP, 'REP', -1.E10, -1.E10)
      IF (XP.LT.-.9E10) THEN
         LXDMP = 0
         EXIT test_point_loop
      ELSE
         IF (OPTG.NE.5) THEN
            CALL CVMESH (XP, YP, XC, YC, KGRPNT, XCGRID, YCGRID, KGRBND)
            IF (XC.LT.0.) THEN
               IF (XP.GE.XCGMIN .AND. XP.LE.XCGMAX .AND.&
               &YP.GE.YCGMIN .AND. YP.LE.YCGMAX ) THEN
                  CYCLE test_point_loop
               ELSE
                  EXIT point_input
               END IF
            END IF
            LXDMP = NINT(XC) + MXF -1
            LYDMP = NINT(YC) + MYF -1
            IF (ITEST.GE.30) WRITE (PRTEST, "(' test point ', 2F12.2, ' to grid point ', 2I4)") XP+XOFFS, YP+YOFFS,&
            &LXDMP, LYDMP
         ELSE
            CALL SwanFindPoint ( XP, YP, K )
            IF ( K.LT.0 ) THEN
               IF (XP.GE.XCGMIN .AND. XP.LE.XCGMAX .AND.&
               &YP.GE.YCGMIN .AND. YP.LE.YCGMAX ) THEN
                  CYCLE test_point_loop
               ELSE
                  EXIT point_input
               END IF
            END IF
            LXDMP = K
            IF (ITEST.GE.30) WRITE (PRTEST,"(' test point ', 2F12.2, ' to vertex ', I6)") XP+XOFFS,YP+YOFFS,LXDMP
         ENDIF
      ENDIF
   ELSE
      CALL ININTG ('I' , LXDMP, 'REP', -1)
      IF (LXDMP .LT. 0) EXIT test_point_loop
      IF (OPTG.NE.5) CALL ININTG ('J' , LYDMP, 'REQ',  0)
   ENDIF

   IF (OPTG.NE.5) THEN
      IF (LXDMP.GE.0 .AND. LXDMP.LE.MXCGL-1 .AND.&
      &LYDMP.GE.0 .AND. LYDMP.LE.MYCGL-1) THEN
         LXDMP = LXDMP - MXF + 1
         LYDMP = LYDMP - MYF + 1
         IF (LXDMP.GE.0 .AND. LXDMP.LE.MXC-1 .AND.&
         &LYDMP.GE.0 .AND. LYDMP.LE.MYC-1) THEN
            IF (KGRPNT(LXDMP+1,LYDMP+1) .GT. 1) THEN
               NPTST = NPTST + 1
               XYTST(2*NPTST-1) = LXDMP+1
               XYTST(2*NPTST)   = LYDMP+1
               CYCLE test_point_loop
            ENDIF
         ELSE
            CYCLE test_point_loop
         ENDIF
      ENDIF
   ELSE
      IF (LXDMP.GE.1 .AND. LXDMP.LE.nverts) THEN
         NPTST = NPTST + 1
         XYTST(NPTST) = LXDMP
         CYCLE test_point_loop
      ENDIF
   ENDIF

   END BLOCK point_input
   CALL MSGERR (1, 'test point is not active')
   WRITE (PRINTF, *) XP+XOFFS, YP+YOFFS
   END DO test_point_loop

!     generate output point set 'TESTPNTS'

   ALLOCATE(OPSTMP)
   OPSTMP%PSNAME = 'TESTPNTS'
   OPSTMP%PSTYPE = 'P'
   OPSTMP%MIP    = NPTST
   ALLOCATE(OPSTMP%XP(NPTST))
   ALLOCATE(OPSTMP%YP(NPTST))
   IF (OPTG.NE.5) THEN
      DO IPTST = 1, NPTST
         LXDMP = XYTST(2*IPTST-1)
         LYDMP = XYTST(2*IPTST)
         OPSTMP%XP(IPTST) = XCGRID(LXDMP,LYDMP)
         OPSTMP%YP(IPTST) = YCGRID(LXDMP,LYDMP)
      ENDDO
   ELSE
      DO IPTST = 1, NPTST
         K = XYTST(IPTST)
         OPSTMP%XP(IPTST) = xcugrd(K)
         OPSTMP%YP(IPTST) = ycugrd(K)
      ENDDO
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

!     open output file for test output of wave parameters

   CALL INKEYW ('STA', ' ')
   IF (KEYWIS('PAR')) THEN
      CALL INCSTR ('FNAME', FILENM, 'STA', 'SWSRCPA')
!       --- append node number to FILENM in case of
!           parallel computing
      IF ( PARLL ) THEN
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      IERR = 0
      CALL FOR (IFPAR, FILENM, 'UF', IERR)
      IF (STPNOW()) RETURN
      WRITE (IFPAR, "('SWAN', I4, T41, 'Swan standard spectral file, version')") 1
      WRITE (IFPAR, "('$ Data produced by SWAN version ', A)") VERTXT
      WRITE (IFPAR, "('$ Project: ', A, '; run number: ', A)") PROJID, PROJNR
      IF (NSTATM.EQ.1) THEN
         WRITE (IFPAR, "(A, T41, A)") 'TIME', 'time-dependent data'
         WRITE (IFPAR, "(I6, T41, A)") ITMOPT, 'time coding option'
      ELSE
         WRITE (IFPAR, "(A, T41, A)") 'ITER', 'iteration-dependent data'
         WRITE (IFPAR, "(I6, T41, A)") 0
      ENDIF
      IF (KSPHER.EQ.0) THEN
         WRITE (IFPAR, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
      ELSE
         WRITE (IFPAR, "(A, T41, A)") 'LONLAT',&
         &'locations in longitude, latitude'
      ENDIF
      WRITE (IFPAR, "(I6, T41, A)") NPTST, 'number of locations'
      IF (OPTG.NE.5) THEN
         do IPTST = 1, NPTST
            LXDMP = XYTST(2*IPTST-1)
            LYDMP = XYTST(2*IPTST)
            WRITE (IFPAR, "(2(1X,F12.2))") XCGRID(LXDMP,LYDMP)+XOFFS,&
            &YCGRID(LXDMP,LYDMP)+YOFFS
         end do
      ELSE
         DO IPTST = 1, NPTST
            K = XYTST(IPTST)
            WRITE (IFPAR, "(2(1X,F12.2))") xcugrd(K)+XOFFS, ycugrd(K)+YOFFS
         ENDDO
      ENDIF
! NB: If list of variables is expanded, the following hardwired integers
!   must be increased:
      IF(IWCAP.NE.8)THEN
         ICHECK1=14
      ELSE
         ICHECK1=15
      ENDIF
      WRITE (IFPAR, "('QUANT', /, I6, T41, 'number of quantities in table')") ICHECK1
      WRITE (IFPAR, "(A, T41, A)") OVSNAM(10), OVLNAM(10)
      WRITE (IFPAR, "(A, T41, A)") OVUNIT(10), 'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(10), 'exception value'
      WRITE (IFPAR, "(A, T41, A)") OVSNAM(28), OVLNAM(28)
      WRITE (IFPAR, "(A, T41, A)") OVUNIT(28), 'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(28), 'exception value'
      ICHECK2=3
      WRITE (IFPAR, "(A, T41, A)") 'Swind',  'wind source term (of var. dens.)'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Swcap',  'whitecapping dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF(IWCAP.EQ.8)THEN
         ICHECK2=ICHECK2+1
         WRITE (IFPAR, "(A, T41, A)") 'Sswell', 'swell dissipation'
         WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
         WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ENDIF
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Sfric',  'bottom friction dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Svege',  'vegetation dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Sturb',  'turbulent dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Smud',  'fluid mud dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Sice',   'dissipation by sea ice'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Ssurf',  'surf breaking dissipation'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Snl3',   'total absolute 3-wave interaction'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Snl4',   'total absolute 4-wave interaction'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Sbragg', 'Bragg scattering'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFPAR, "(A, T41, A)") 'Sqc',    'QC scattering'
      WRITE (IFPAR, "(A, T41, A)") 'm2/s',   'unit'
      WRITE (IFPAR, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF ( ICHECK1.NE.ICHECK2 ) THEN
         CALL MSGERR (3,'Internal error: mismatch in # quantities')
      ENDIF
   ENDIF

!     open output file for source terms if requested

   CALL INKEYW ('STA', ' ')
   IF (KEYWIS('S1D')) THEN
      CALL INCSTR ('FNAME', FILENM, 'STA', 'SWSRC1D')
!       --- append node number to FILENM in case of
!           parallel computing
      IF ( PARLL ) THEN
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      IERR = 0
      CALL FOR (IFS1D, FILENM, 'UF', IERR)
      IF (STPNOW()) RETURN
      WRITE (IFS1D, "('SWAN', I4, T41, 'Swan standard spectral file, version')") 1
      WRITE (IFS1D, "('$ Data produced by SWAN version ', A)") VERTXT
      WRITE (IFS1D, "('$ Project: ', A, '; run number: ', A)") PROJID, PROJNR
      IF (NSTATM.EQ.1) THEN
         WRITE (IFS1D, "(A, T41, A)") 'TIME', 'time-dependent data'
         WRITE (IFS1D, "(I6, T41, A)") ITMOPT, 'time coding option'
      ELSE
         WRITE (IFS1D, "(A, T41, A)") 'ITER', 'iteration-dependent data'
         WRITE (IFS1D, "(I6, T41, A)") 0
      ENDIF
      IF (KSPHER.EQ.0) THEN
         WRITE (IFS1D, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
      ELSE
         WRITE (IFS1D, "(A, T41, A)") 'LONLAT',&
         &'locations in longitude, latitude'
      ENDIF
      WRITE (IFS1D, "(I6, T41, A)") NPTST, 'number of locations'
      IF (OPTG.NE.5) THEN
         do IPTST = 1, NPTST
            LXDMP = XYTST(2*IPTST-1)
            LYDMP = XYTST(2*IPTST)
            WRITE (IFS1D, "(2(1X,F12.2))") XCGRID(LXDMP,LYDMP)+XOFFS,&
            &YCGRID(LXDMP,LYDMP)+YOFFS
         end do
      ELSE
         DO IPTST = 1, NPTST
            K = XYTST(IPTST)
            WRITE (IFS1D, "(2(1X,F12.2))") xcugrd(K)+XOFFS, ycugrd(K)+YOFFS
         ENDDO
      ENDIF
      IF (ICUR.GT.0) THEN
         WRITE (IFS1D, "(A, T41, A)") 'RFREQ', 'relative frequencies in Hz'
      ELSE
         WRITE (IFS1D, "(A, T41, A)") 'AFREQ', 'absolute frequencies in Hz'
      ENDIF
      WRITE (IFS1D, "(I6, T41, A)") MSC, 'number of frequencies'
      do IS = 1, MSC
         WRITE (IFS1D, "(F10.4)") SPCSIG(IS)/PI2
      end do
! NB: If list of variables is expanded, the following hardwired integers
!   must be increased:
      IF(IWCAP.NE.8)THEN
         ICHECK1=13
      ELSE
         ICHECK1=14
      ENDIF
      WRITE (IFS1D, "('QUANT', /, I6, T41, 'number of quantities in table')") ICHECK1
      WRITE (IFS1D, "(A, T41, A)") 'VaDens', 'variance densities'
      WRITE (IFS1D, "(A, T41, A)") 'm2/Hz',  'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") 0.,       'exception value'
      ICHECK2=2
      WRITE (IFS1D, "(A, T41, A)") 'Swind',  'wind source term'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Swcap',  'whitecapping dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF(IWCAP.EQ.8) THEN
         ICHECK2=ICHECK2+1
         WRITE (IFS1D, "(A, T41, A)") 'Sswell', 'swell dissipation'
         WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
         WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ENDIF
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Sfric',  'bottom friction dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Svege',  'vegetation dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Sturb',  'turbulent dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Smud',  'fluid mud dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Sice',   'dissipation by sea ice'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Ssurf',  'surf breaking dissipation'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Snl3',   'triad interactions'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Snl4',   'quadruplet interactions'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Sbragg', 'Bragg scattering'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS1D, "(A, T41, A)") 'Sqc',    'QC scattering'
      WRITE (IFS1D, "(A, T41, A)") 'm2',     'unit'
      WRITE (IFS1D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF ( ICHECK1.NE.ICHECK2 ) THEN
         CALL MSGERR (3,'Internal error: mismatch in # quantities')
      ENDIF
   ENDIF
!     2D source terms
   CALL INKEYW ('STA', ' ')
   IF (KEYWIS('S2D')) THEN
      CALL INCSTR ('FNAME', FILENM, 'STA', 'SWSRC2D')
!       --- append node number to FILENM in case of
!           parallel computing
      IF ( PARLL ) THEN
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
      END IF
      IERR = 0
      CALL FOR (IFS2D, FILENM, 'UF', IERR)
      IF (STPNOW()) RETURN
      WRITE (IFS2D, "('SWAN', I4, T41, 'Swan standard spectral file, version')") 1
      WRITE (IFS2D, "('$ Data produced by SWAN version ', A)") VERTXT
      WRITE (IFS2D, "('$ Project: ', A, '; run number: ', A)") PROJID, PROJNR
      IF (NSTATM.EQ.1) THEN
         WRITE (IFS2D, "(A, T41, A)") 'TIME', 'time-dependent data'
         WRITE (IFS2D, "(I6, T41, A)") ITMOPT, 'time coding option'
      ELSE
         WRITE (IFS2D, "(A, T41, A)") 'ITER', 'iteration-dependent data'
         WRITE (IFS2D, "(I6, T41, A)") 0
      ENDIF
      IF (KSPHER.EQ.0) THEN
         WRITE (IFS2D, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
      ELSE
         WRITE (IFS2D, "(A, T41, A)") 'LONLAT',&
         &'locations in longitude, latitude'
      ENDIF
      WRITE (IFS2D, "(I6, T41, A)") NPTST, 'number of locations'
      IF (OPTG.NE.5) THEN
         do IPTST = 1, NPTST
            LXDMP = XYTST(2*IPTST-1)
            LYDMP = XYTST(2*IPTST)
            WRITE (IFS2D, "(2(1X,F12.2))") XCGRID(LXDMP,LYDMP)+XOFFS,&
            &YCGRID(LXDMP,LYDMP)+YOFFS
         end do
      ELSE
         DO IPTST = 1, NPTST
            K = XYTST(IPTST)
            WRITE (IFS2D, "(2(1X,F12.2))") xcugrd(K)+XOFFS, ycugrd(K)+YOFFS
         ENDDO
      ENDIF
      IF (ICUR.GT.0) THEN
         WRITE (IFS2D, "(A, T41, A)") 'RFREQ', 'relative frequencies in Hz'
      ELSE
         WRITE (IFS2D, "(A, T41, A)") 'AFREQ', 'absolute frequencies in Hz'
      ENDIF
      WRITE (IFS2D, "(I6, T41, A)") MSC, 'number of frequencies'
      do IS = 1, MSC
         WRITE (IFS2D, "(F10.4)") SPCSIG(IS)/PI2
      end do
!       full 2-D spectrum
      WRITE (IFS2D, "(A, T41, A)") 'CDIR',&
      &'spectral Cartesian directions in degr'
      WRITE (IFS2D, "(I6, T41, A)") MDC, 'number of directions'
      do ID = 1, MDC
         WRITE (IFS2D, "(F10.4)") SPCDIR(ID,1)*180./PI
      end do
! NB: If list of variables is expanded, the following hardwired integers
!   must be increased:
      IF(IWCAP.NE.8)THEN
         ICHECK1=13
      ELSE
         ICHECK1=14
      ENDIF
      WRITE (IFS2D, "('QUANT', /, I6, T41, 'number of quantities in table')") ICHECK1
      WRITE (IFS2D, "(A, T41, A)") 'VaDens', 'variance densities'
      WRITE (IFS2D, "(A, T41, A)") 'm2/Hz/degr', 'unit'
      WRITE (IFS2D, "(F14.6, T41, A)") 0.,       'exception value'
      ICHECK2=2
      WRITE (IFS2D, "(A, T41, A)") 'Swind',  'wind source term'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Swcap',  'whitecapping dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF(IWCAP.EQ.8) THEN
         ICHECK2=ICHECK2+1
         WRITE (IFS2D, "(A, T41, A)") 'Sswell', 'swell dissipation'
         WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
         WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ENDIF
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Sfric',  'bottom friction dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Svege',  'vegetation dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Sturb',  'turbulent dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Smud',  'fluid mud dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Sice',  'dissipation by sea ice'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Ssurf',  'surf breaking dissipation'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Snl3',   'triad interactions'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Snl4',   'quadruplet interactions'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Sbragg', 'Bragg scattering'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      ICHECK2=ICHECK2+1
      WRITE (IFS2D, "(A, T41, A)") 'Sqc',    'QC scattering'
      WRITE (IFS2D, "(A, T41, A)") 'm2/degr','unit'
      WRITE (IFS2D, "(F14.6, T41, A)") OVEXCV(7),'exception value'
      IF ( ICHECK1.NE.ICHECK2 ) THEN
         CALL MSGERR (3,'Internal error: mismatch in # quantities')
      ENDIF
   ENDIF
   RETURN
!     end of subroutine RETSTP
end subroutine RETSTP

end module swan_input_processing
