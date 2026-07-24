
!     SWAN/OUTPUT       file 1 of 2
!
!  Contents of this file:
!     SWOUTP
!     SWORDC
!     SWODDC
!     SWOEXC
!     SWOEXD
!     SWIPOL
!     SWOEXA
!     SWOINA
!     SWOEXF
!
!     main output routine and computation of output quantities
!
!************************************************************************

module swan_output_orchestration
   implicit none
   private
   public :: SWOUTP, SWOEXC
contains

!                                                                      *
SUBROUTINE SWOUTP (AC2             ,&
&SPCSIG          ,SPCDIR  ,&
&COMPDA          ,XYTST   ,&
&KGRPNT          ,XCGRID  ,&
&YCGRID          ,OURQT   )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, STPNOW
   USE swan_output_writers, ONLY: SWBLOK, SWBLKP, SWBLKV, SWSPEC, SWTABP
!                                                                      *
!************************************************************************

   USE swan_time, ONLY: default_time_context
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
   USE M_PARALL
   USE SwanGriddata
   USE SwanIEM, ONLY: ntf, dfiem, Ebig


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
!     30.81: Annette  Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     34.01: Jeroen Adema
!     40.00, 40.13: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.86: Nico Booij
!     40.90: Nico Booij
!     41.85: Ad Reniers
!
!  1. Updates
!
!     10.10, Aug. 94: computation of force is added (subr. SWOEXF)
!                     arrays NE and NED added
!     30.72, Oct. 97: changed floating point comparison to avoid equality
!                     comparisons
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, June 98: argument KGRBND added, call SWPLOT and SWOEXC
!                     modified
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Jan. 99: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     40.00, Jan. 99: argument RTYPE added in call SWODDC
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Oct. 00: Made TYPE of several equivalenced arrays correct
!     40.02, Oct. 00: Modified argument list of SWPLOT to avoid int/real conflict
!     40.13, Oct. 01: Forces always computed by post-processing procedure
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing POOL construction and HPGL-functionality
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: optimization output process in parallel mode
!     40.80, Feb. 08: computation of wave-induced force on unstructured
!     40.86, Feb. 08: arguments added to calls of subroutines
!                     to prevent interpolation over obstacles
!     40.90, June 08: arguments added to call of subroutine SWSPEC
!                     to prevent interpolation over obstacles
!     41.85, June 19: implementation of IEM (surfbeat model)
!
!  2. Purpose
!
!     Processing of the output requests
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
! i   OURQT : array indicating at what time requested output
!             is processed
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

   REAL(KIND=KIND(0.0D0))  OURQT(MAX_OUTP_REQ)
   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     AC2     real arr input  action density in all computational points
!     SPCDIR  real arr input  spectral directions, cosines and sines
!     UX2     real arr input  current velocity x-comp.
!     UY2     real arr input  current velocity y-comp.
!     UBOT    real arr input  orbital velocities at bottom
!     WX2     real arr input  wind velocity x-comp.
!     WY2     real arr input  wind velocity y-comp.
!
!  8. Subroutines used
!
!     SWBLOK
!     SWTABP
!     SWSPEC
!     FOR


!  9. Subroutines calling
!
!     MAIN program
!
! 10. Error messages
!
!     If an output request is of an unknown type, an error message
!     is printed.
!
! 11. Remarks
!
!     Data:
!
!     output requests are encoded in array OUTREQ; these are set by
!     commands TABLE, BLOCK, SPEC, etc. (see subr SWREOQ in file SWANPRE2)
!     each output request refers to one set of output locations,
!     and to one or more output quantities.
!     1st value in OUTREQ: time of next output, 2nd value: interval between
!     outputs, 3d value: type of output request RTYPE (encoded as integer),
!     4&5: PSNAME (name of point set),
!     6: file unit number, 7..10: output filename,
!     other: dependent on type of output.
!
!     data on output locations are in array OUTDA; these are set by
!     commands FRAME, POINTS, CURVE etc. (see subr SWREPS in file SWANPRE2)
!     each set is characterized by its name (SNAME in the code)
!     STYPE is the type of set (i.e. 'F' for Frame etc.)
!
!     properties of output quantities are in arrays OVSNAM, OVLNAM,
!     OVUNIT, OVSVTY etc.; these are set in subr SWINIT (file SWANMAIN)
!     each output quantity is assigned a fixed number; i.e. 1=Xp, 2=Yp,
!     7=Dissip, 10=Hs, 11=Tm01 etc.
!     subr SVARTP determines the above number from the name of the
!     quantity as it appears in the user command; this is compared with
!     OVKEYW.
!
!     Procedure:
!
!     After the coordinates of all output locations have been determined,
!     values of all output quantities are calculated, and written into
!     2d array VOQ (one or two columns for each output quantity, one line
!     for each location). array VOQR shows with quantity is written in
!     each column.
!     After array VOQ is filled, the actual output starts; which subroutine
!     is called depends on RTYPE (see structure scheme below).
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     For all output requests do
!         Call SWORDC (analyzes output req.; gives name of output
!                      point set, type of output RTYPE
!                      and number of quantities per point)
!         Call SWODDC (analyzes output data; gives number of output
!                      points)
!         ------------------------------------------------------------
!         Call SWOEXC (compute coordinates of output points)
!         Call SWOEXD (compute depth, current vel. dissipation etc.)
!         Call SWOEXA (compute action density and related quant.)
!         Call SWOEXF (compute wave-driven force)
!         ------------------------------------------------------------
!         If RTYPE = 'BLKP', 'BLKD' or 'BLKV' then
!                     call SWBLOK for block output
!         If RTYPE = 'TABP' or 'TABD' then call SWTABP for output in
!                     table
!         If RTYPE = 'SPEC' then call SWSPEC for spectral output
!     ----------------------------------------------------------------
!     If program is run in stationary mode
!     Then Close all opened files
!     ----------------------------------------------------------------
!
! 13. Source text

   INTEGER   VOQR(NMOVAR)   ,BKC           ,&
   &XYTST(*)       ,KGRPNT(MXC,MYC),IERR

   REAL      AC2(MDC,MSC,MCGRD) ,&
   &COMPDA(MCGRD,MCMVAR)
   LOGICAL, ALLOCATABLE :: CROSS(:,:) ! true if obstacle is between
   ! output point and computationa
   ! grid point

   INTEGER, SAVE :: IENT = 0
   INTEGER INDX, ID, IS, ITMP1, ISTAT
   INTEGER II, IP, IRQ, JJ, MIP, MXK, MXN, MYK, MYN, NVOQP
   REAL ALPCN, XNLEN, XPCN, YNLEN, YPCN
   REAL DF, FREQS(ntf)
   REAL, ALLOCATABLE :: EBLOC(:,:,:)

   INTEGER, ALLOCATABLE :: IONOD(:)
   REAL, ALLOCATABLE :: ACLOC(:), AUX1(:), VOQ(:)
   REAL, ALLOCATABLE :: FORCE(:,:)
   REAL              :: KNUM(MSC), CG(MSC), NE(MSC), NED(MSC)
   TYPE(OPSDAT), POINTER :: CUOPS
   TYPE(ORQDAT), POINTER :: CORQ

   LOGICAL   OQPROC(NMOVAR)  , LOGACT
   CHARACTER(LEN=4) :: RTYPE
   CHARACTER(LEN=1) :: STYPE, PTYPE
   CHARACTER(LEN=8) :: PNAME
   CALL STRACE (IENT, 'SWOUTP')

!     processing of output requests

   IF (NPTST.GT.0) CALL AC2TST (XYTST, AC2 ,KGRPNT)

   IF (NREOQ.EQ.0) THEN
      CALL MSGERR (1, 'no output requested')
      RETURN
   ENDIF
   IF (ITEST.GE.10) WRITE (PRINTF, "(1X, I3, ' output requests')") NREOQ

   IF (LSRFB) THEN
      IF (.NOT.ALLOCATED(EBLOC)) THEN
         ALLOCATE(EBLOC(MDC,ntf,MCGRD),STAT=ISTAT)
      END IF
      IF ( ISTAT.NE.0 ) THEN
         CALL MSGERR ( 4, 'Allocation problem: array EBLOC' )
         WRITE(PRINTF,*) 'return code is ',ISTAT
         RETURN
      END IF
      EBLOC = 0.
   ELSE
      IF(.NOT.ALLOCATED(EBLOC)) ALLOCATE(EBLOC(0,0,0))
   ENDIF

!     Repeat for all output requests:

   CORQ => FORQ
   request_loop: do IRQ = 1, NREOQ

!       ***** processing of output instructions *****

      BKC   = 0
      NVOQP = 0

!       call SWORDC to analyse output request encoded in array OUTREQ
!       NVOQP (number of output quantities)

      RTYPE = CORQ%RQTYPE
      SNAME = CORQ%PSNAME
      CALL SWORDC (CORQ%OQI, CORQ%OQR, CORQ%IVTYP, RTYPE,&
      &SNAME, NVOQP, OQPROC, BKC, VOQR, OURQT(IRQ),&
      &LOGACT)
      IF (.NOT.LOGACT) THEN
         CORQ => CORQ%NEXTORQ
         CYCLE request_loop
      END IF

      request_action: BLOCK

      IF (SCREEN.NE.PRINTF.AND.IAMMASTER) WRITE (SCREEN, "('+SWAN is processing output request ', I4)") IRQ
      IF (ITEST.GE.10) WRITE(PRINTF,"(' SWAN is processing output request ', I4)") IRQ

      CUOPS => FOPS
      DO
         IF (CUOPS%PSNAME.EQ.SNAME) EXIT
         IF (.NOT.ASSOCIATED(CUOPS%NEXTOPS)) THEN
            CALL MSGERR (3, 'Output requested for non-existing points')
            WRITE (PRINTF, "(' Point set: ', A)") SNAME
            EXIT request_action
         END IF
         CUOPS => CUOPS%NEXTOPS
      END DO

      IF (ITEST.GE.80 .OR. IOUTES .GE. 10)&
      &WRITE (PRTEST, "(' Test SWOUTP ', 2I6, 2X, A4, 2X, A16)") IRQ, NVOQP, RTYPE, SNAME

!       call SWODDC to analyse output data; results: STYPE (type of output
!       point set), MIP (number of output locations) etc.

      STYPE = CUOPS%PSTYPE
      MIP   = CUOPS%MIP
      CALL SWODDC (CUOPS%OPI, CUOPS%OPR, SNAME, STYPE, MIP, MXK,&
      &MYK, XNLEN, YNLEN, MXN, MYN, XPCN, YPCN, ALPCN,&
      &XCGRID,YCGRID,RTYPE)

!       assign memory to array VOQ (contains output quantities for all
!                                   output points)
      ALLOCATE(VOQ(MIP*NVOQP))
      VOQ = 0.

!       assign memory to array CROSS (indicates crossing of obstacles
!                                     in between output and grid points)  40.86
      ALLOCATE(CROSS(4,MIP))
      CROSS = .FALSE.

!       assign memory to array IONOD (indicates in which subdomain
!                                     output points are located)
      ALLOCATE(IONOD(MIP))
      IF (.NOT.PARLL) THEN
         IONOD = MASTER
      ELSE
         IONOD = -999
      ENDIF

!       call SWOEXC to calculate quantities dependent only on coordinates

      CALL SWOEXC (STYPE               ,&
      &CUOPS%OPI           ,CUOPS%OPR           ,&
      &CUOPS%XP            ,CUOPS%YP            ,&
      &MIP                 ,VOQ(1)              ,&
      &VOQ(1+MIP)          ,VOQ(1+2*MIP)        ,&
      &VOQ(1+3*MIP)        ,KGRPNT              ,&
      &XCGRID              ,YCGRID              ,&
      &CROSS                                    )

!       Compute wave-induced force on unstructured grid

      IF (OQPROC(20) .AND. OPTG.EQ.5) THEN
         ALLOCATE(FORCE(nverts,2))
         CALL SwanComputeForce ( FORCE(1,1), FORCE(1,2), AC2,&
         &COMPDA(1,JDP2), COMPDA(1,JHS),&
         &SPCSIG, SPCDIR )
      ELSE
         ALLOCATE(FORCE(0,0))
      ENDIF

!       call SWOEXD to interpolate quantities which are computed during
!       SWAN computation, such as Qb, Dissipation, Ursell etc.

      CALL SWOEXD (RTYPE, OQPROC, MIP, VOQ(1+2*MIP),&
      &VOQ(1+3*MIP), VOQR, VOQ(1),&
      &COMPDA, KGRPNT, FORCE, CROSS, IONOD&
      &,IRQ&
      &)
      IF (STPNOW()) RETURN

      DEALLOCATE(FORCE)

      IF (BKC .GT. 0) THEN

!         assign memory to array ACLOC (contains spectrum for one output point)

         ALLOCATE(ACLOC(MDC*MSC))

!         call SWOEXA to compute quantities for which spectrum is needed
!         (except wave-induced force)

         CALL SWOEXA (OQPROC              ,BKC                 ,&
         &MIP                 ,VOQ(1+2*MIP)        ,&
         &VOQ(1+3*MIP)        ,VOQR                ,&
         &VOQ(1)              ,AC2                 ,&
         &ACLOC               ,SPCSIG              ,&
         &KNUM                ,CG                  ,&
         &SPCDIR              ,NE                  ,&
         &NED                 ,KGRPNT              ,&
         &COMPDA(1,JDP2)      ,CROSS               )

!         call SWOEXF to compute wave-driven force on regular grid

         IF (OQPROC(20) .AND. OPTG.NE.5)&
         &CALL SWOEXF (MIP                 ,VOQ(1+2*MIP)         ,&
         &VOQ(1+3*MIP)        ,VOQR                 ,&
         &VOQ(1)              ,AC2                  ,&
         &COMPDA(1,JDP2)      ,SPCSIG               ,&
         &KNUM                ,CG                   ,&
         &SPCDIR              ,NE                   ,&
         &NED                 ,KGRPNT               ,&
         &XCGRID              ,YCGRID               ,&
         &COMPDA(1,JHS)       ,IONOD&
         &)

         DEALLOCATE(ACLOC)
      ENDIF

      IF (ITEST.GE.100 ) THEN
         WRITE (PRTEST, "(' arrays VOQR and VOQ:', 30I3)") (VOQR(II), II=1, NMOVAR)
         do IP=1, MIN(MIP,20)
            WRITE (PRTEST, "(12(1X,E10.4))") (VOQ(IP+(JJ-1)*MIP),&
            &JJ=1, NVOQP)
         end do
      ENDIF

!       ***** block output *****
      IF (RTYPE(1:3) .EQ. 'BLK') THEN
         IF (RTYPE.EQ.'BLKV') THEN
            CALL SWBLKV ( CORQ%OQI, CORQ%OQR, CORQ%IVTYP,&
            &MXK, MYK, VOQR, VOQ(1), STYPE,&
            &SNAME, IONOD )
         ELSE IF (PARLL) THEN
            CALL SWBLKP ( CORQ%OQI, CORQ%IVTYP, MXK, MYK, VOQR,&
            &VOQ(1), IONOD )
         ELSE
            CALL SWBLOK ( RTYPE, CORQ%OQI, CORQ%OQR, CORQ%IVTYP,&
            &CORQ%FAC, SNAME, MXK, MYK, IRQ, VOQR,&
            &VOQ(1) )
         END IF
         IF (STPNOW()) RETURN
         EXIT request_action
      ENDIF

!       ***** table output *****
      IF (RTYPE(1:3) .EQ. 'TAB') THEN
!NCF         IF (PARLL.AND.(RTYPE.EQ.'TABC')) THEN
!NCF!            --- use "block" intermediate file facility to pass data bet
!NCF            CALL SWBLKP ( CORQ%OQI, CORQ%IVTYP, MIP, 1, VOQR,&
!NCF            &VOQ(1), IONOD )
!NCF         ELSE
!NCF            CALL SWTABP ( RTYPE, CORQ%OQI, CORQ%OQR, CORQ%IVTYP, SNAME,&
!NNCF            CALL SWTABP ( RTYPE, CORQ%OQI, CORQ%IVTYP, SNAME,&
            &MIP, VOQR, VOQ(1), IONOD )
!NCF         ENDIF
         IF (STPNOW()) RETURN
         EXIT request_action
      ENDIF

!       ***** spectral output *****
      IF (RTYPE(1:2) .EQ. 'SP') THEN
         IF ( .NOT.LSRFB .OR.&
         &(RTYPE(3:3).NE.'L' .AND. RTYPE(3:3).NE.'B') ) THEN
            IF (RTYPE(4:4).EQ.'C') THEN
               ALLOCATE(AUX1(MSC*MDC))
            ELSE
               ALLOCATE(AUX1(3*MSC))
            ENDIF
            CALL SWSPEC ( RTYPE, CORQ%OQI, CORQ%OQR, MIP, VOQR, VOQ(1),&
            &AC2, AUX1, SPCSIG, SPCDIR, COMPDA(1,JDP2),&
            &KGRPNT, CROSS, IONOD )
         ELSE
            ITMP1 = MSC
            MSC   = ntf
            IF (RTYPE(4:4).EQ.'C') THEN
               ALLOCATE(AUX1(MSC*MDC))
            ELSE
               ALLOCATE(AUX1(3*MSC))
            ENDIF
            DF = PI2 * dfiem
            FREQS(1) = DF
            DO IS = 2, MSC
               FREQS(IS) = FREQS(IS-1) + DF
            ENDDO
            DO INDX = 1, MCGRD
               DO ID = 1, MDC
                  DO IS = 1, MSC
                     EBLOC(ID,IS,INDX) = Ebig(ID,IS,INDX) / FREQS(IS) /&
                     &( DF * DDIR )
                  ENDDO
               ENDDO
            ENDDO
            CALL SWSPEC ( RTYPE, CORQ%OQI, CORQ%OQR, MIP, VOQR, VOQ(1),&
            &EBLOC, AUX1, FREQS, SPCDIR, COMPDA(1,JDP2),&
            &KGRPNT, CROSS, IONOD )
            MSC = ITMP1
         ENDIF
         IF (STPNOW()) RETURN
         DEALLOCATE(AUX1)
         EXIT request_action
      ENDIF

WRITE (PRINTF, "(' Error in output request ', 2I6, 2X, A4, 2X, A16, I6)") IRQ, NVOQP, RTYPE, SNAME, MIP

      END BLOCK request_action
      IF (ALLOCATED(VOQ)) DEALLOCATE(VOQ)
      IF (ALLOCATED(CROSS)) DEALLOCATE(CROSS)
      IF (ALLOCATED(IONOD)) DEALLOCATE(IONOD)
      CORQ => CORQ%NEXTORQ
   end do request_loop
   IF (ALLOCATED(EBLOC)) DEALLOCATE(EBLOC)

!     Termination of output

RETURN
end subroutine SWOUTP

!************************************************************************
!                                                                      *
SUBROUTINE SWORDC (OUTI, OUTR, IVTYP, RTYPE, PSNAME, NVOQP,&
&OQPROC, BKC,&
&VOQR, OURQT, LOGACT)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!                                                                      *
!************************************************************************

   USE swan_time, ONLY: default_time_context
!NCF   USE OCPCOMM2
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM3
   USE SWCOMM4
!NCF   USE OUTP_DATA
   USE M_PARALL
   USE OUTP_DATA, ONLY: NTVTK



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
!     30.61: Roberto Padilla
!     30.74: IJsbrand Haagsma (Include version)
!     30.81: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.62: Andre van der Westhuysen
!
!  1. Update
!
!     10.33, Jan. 95: computation of wind added in polar plot
!     30.61, Summ 97: give values for array VOQR when OQPROC = .TRUE.
!                     in case of 'nest'
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.81, Jan. 99: Replaced variable FROM by FROM_ (because FROM
!                     is a reserved word)
!     40.30, May  03: introduction distributed-memory approach using MPI
!     40.31, Nov. 03: removing HPGL-functionality
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     41.62, Nov. 15: included fields required for partitioning output
!
!  2. Purpose
!
!     Decodes output requests
!
!  3. Method
!
!     ---
!
!  4. Argument list
!
!     OUTR    Real   input    Code for one output request
!     RTYPE   Char   outp     type of output
!     PSNAME  Char   outp     name of output point set referred to
!     NVOQP   Int    outp     number of data per output point
!     OQPROC  logic  outp     whether or not an output quantity  must
!                             be processed
!     VOQR    Int ar outp     place of each output quantity
!                             (subscript: IVTYP)
!     OURQT   Int ar input    array indicating at what time requested
!                             output is processed
!
!  8. Subroutines used
!
!     SPSET
!     SUVIPL
!     SBLKPT
!     SCUNIT
!     SFLFUN (all SWAN/OUTP)
!     TABHED
!     MSGERR
!     FOR
!
!  9. Subroutines calling
!
!     SWOUTP (SWAN/OUTP)
!
! 10. Error messages
!
!     If the point set is not of the type frame, an error message
!     is printed and control returns to subroutine OUTPUT
!
! 11. Remarks
!
!     output interval negative means that output is made only at end
!     of computation
!
! 12. Structure
!
!     -----------------------------------------------------------------
!     If dynamic mode
!     Then determine TNEXT (time of next requested output)
!          determine DIF (interval between end time and present time)
!          If DIF is less than half time step and output interval is
!               negative
!          Then enable output (by making LOGACT = true)
!          Else If time of computation >= TNEXT and output interval is
!               positive
!          Then enable output
!          Else disable output (by making LOGACT = false)
!               Return
!          ------------------------------------------------------------
!     Else enable output
!     -----------------------------------------------------------------
!     Set all OQPROC = false (if OQPROC is true corresponding quantity
!                             must be computed)
!     Make OQPROC true for quantities Xp, Yp, Xc and Yc
!     Set all values of VOQR = 0 (VOQR indicates where value of a
!                                 quantity is stored in array VOQ)
!     Make VOQR nonzero for quantities Xp, Yp, Xc and Yc
!     Assign value to NVAR depending on type of output request
!     -----------------------------------------------------------------
!
! 13. Source text

   INTEGER    VOQR(*), OUTI(*), BKC
   INTEGER    IVTYP(*)
   INTEGER, SAVE :: IENT = 0
   INTEGER    IVAR, IVT, IVTYPE, NVAR, NVOQP
   REAL(KIND=KIND(0.0D0))     OUTR(*)
   REAL(KIND=KIND(0.0D0))     OURQT
   REAL(KIND=KIND(0.0D0))     DIF, TNEXT
   LOGICAL    OQPROC(NMOVAR), LOGACT
!NCF   LOGICAL    NCF
   CHARACTER(LEN=*) :: PSNAME, RTYPE
   CALL STRACE (IENT, 'SWORDC')

!     check time of output action:
   IF (NSTATM.EQ.1) THEN
!       check time of output action:
!       DIF  in case that timco is not a fraction of the
!       computational period and the user do not ask for a periodic plots
      DIF = default_time_context%TFINC - default_time_context%TIMCO
      IF (OUTR(1).LT.default_time_context%TINIC) THEN
         TNEXT = default_time_context%TINIC
      ELSE
         TNEXT = OUTR(1)
      ENDIF
      IF ( PARLL.AND.OURQT.EQ.-9999.) OURQT = OUTR(1)
      IF (ITEST.GE.60) WRITE (PRTEST, *) ' output times ', TNEXT,&
      &OUTR(2), default_time_context%DT, default_time_context%TFINC, default_time_context%TIMCO
      IF (ABS(DIF).LT.0.5*default_time_context%DT .AND. OUTR(2).LT.0.) THEN
         OUTR(1) = default_time_context%TIMCO
         LOGACT = .TRUE.
      ELSE IF (OUTR(2).GT.0. .AND. default_time_context%TIMCO.GE.TNEXT) THEN
         OUTR(1) = TNEXT + OUTR(2)
         LOGACT = .TRUE.
      ELSE
         LOGACT = .FALSE.
         RETURN
      ENDIF
      IF (LOGACT) NTVTK(OUTI(2)) = NTVTK(OUTI(2)) + 1
   ELSE
      LOGACT = .TRUE.
   ENDIF

!     action is taken, proceed with analysing output request
!
!     Ivtype 1 and 2 are Xp and Yp

   OQPROC(1) = .TRUE.
   VOQR(1)   = 1
   OQPROC(2) = .TRUE.
   VOQR(2)   = 2

!     clear VOQR and OQPROC from old information

   do IVT = 3, NMOVAR
      VOQR(IVT)   = 0
      OQPROC(IVT) = .FALSE.
   end do

!     Ivtype 24 and 25 are Xc and Yc

   OQPROC(24) = .TRUE.
   VOQR(24)   = 3
   OQPROC(25) = .TRUE.
   VOQR(25)   = 4
   NVOQP = 4

   NVAR = OUTI(3)

   do IVAR = 1, NVAR
      IVTYPE = IVTYP(IVAR)

      IF (IVTYPE.LT.1 .OR. IVTYPE.GT.NMOVAR) THEN
         CALL MSGERR (2, 'wrong value for IVTYPE')
         WRITE (PRINTF, "(' type, points, var: ', A4, 2X, A8, 2X, 2I8)") RTYPE, PSNAME, IVTYPE, NVAR
         CYCLE
      ENDIF

      IF (OVSVTY(IVTYPE).LE.2 .AND. .NOT.OQPROC(IVTYPE)) THEN
!           output quantity is a scalar
         NVOQP = NVOQP + 1
         VOQR(IVTYPE) = NVOQP
      ELSE IF (OVSVTY(IVTYPE).EQ.3 .AND. .NOT.OQPROC(IVTYPE)) THEN
!           output quantity is a vector
         NVOQP = NVOQP + 2
         VOQR(IVTYPE) = NVOQP-1
      ENDIF
      OQPROC(IVTYPE) = .TRUE.
      IF (ITEST.GE.80 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWORDC, output quantity:', 3I6)")&
      &IVAR, IVTYPE, VOQR(IVTYPE)

!        for spectral width add Tm02 as output quantity

      IF (IVTYPE.EQ.33) THEN
         IF (.NOT.OQPROC(32)) THEN
            NVOQP = NVOQP + 1
            VOQR(32) = NVOQP
            OQPROC(32) = .TRUE.
         ENDIF
      ENDIF

!        for BFI add steepness and Qp as output quantities

      IF (IVTYPE.EQ.59) THEN
         IF (.NOT.OQPROC(18)) THEN
            NVOQP = NVOQP + 1
            VOQR(18) = NVOQP
            OQPROC(18) = .TRUE.
         ENDIF
         IF (.NOT.OQPROC(58)) THEN
            NVOQP = NVOQP + 1
            VOQR(58) = NVOQP
            OQPROC(58) = .TRUE.
         ENDIF
      ENDIF

!        include depth and wind required for partitioning output

      IF ((IVTYPE.EQ.100).OR.(IVTYPE.EQ.110).OR.&
      &(IVTYPE.EQ.120).OR.(IVTYPE.EQ.130).OR.&
      &(IVTYPE.EQ.140).OR.(IVTYPE.EQ.150).OR.&
      &(IVTYPE.EQ.160)) THEN
         IF (.NOT.OQPROC(4)) THEN
            NVOQP = NVOQP + 1
            VOQR(4) = NVOQP
            OQPROC(4) = .TRUE.
         ENDIF
         IF (.NOT.OQPROC(26)) THEN
            ! increment by two because wind is a vector
            NVOQP = NVOQP + 2
            VOQR(26) = NVOQP-1
            OQPROC(26) = .TRUE.
         ENDIF
      ENDIF

!        for some quantities compute action densities

      IF (IVTYPE.EQ.10 .OR. IVTYPE.EQ.11 .OR. IVTYPE.EQ.12 .OR.&
      &IVTYPE.EQ.13 .OR. IVTYPE.EQ.14 .OR. IVTYPE.EQ.16 .OR.&
      &IVTYPE.EQ.21 .OR. IVTYPE.EQ.22 .OR. IVTYPE.EQ.43 .OR.&
      &IVTYPE.EQ.44 .OR. IVTYPE.EQ.48 .OR. IVTYPE.EQ.53 .OR.&
      &IVTYPE.EQ.58                                          )&
      &BKC = MAX (1, BKC)

!        for some quantities also compute Depth, current, K and Cg

      IF (IVTYPE.EQ.15 .OR. IVTYPE.EQ.17 .OR. IVTYPE.EQ.18 .OR.&
      &IVTYPE.EQ.19 .OR. IVTYPE.EQ.20 .OR. IVTYPE.EQ.28 .OR.&
      &IVTYPE.EQ.32 .OR. IVTYPE.EQ.33 .OR. IVTYPE.EQ.42 .OR.&
      &IVTYPE.EQ.47 .OR. IVTYPE.EQ.59 .OR. IVTYPE.EQ.71      )&
      &BKC = 2

      IF (IVTYPE.EQ.11 .AND. ICUR.GT.0) BKC = 2
      IF (BKC.GT.0) THEN
!           depth must be computed
         IF (.NOT.OQPROC(4)) THEN
            NVOQP = NVOQP + 1
            VOQR(4) = NVOQP
            OQPROC(4)=.TRUE.
         ENDIF
!           current velocity must be computed
         IF (.NOT.OQPROC(5) .AND. ICUR.GT.0) THEN
            NVOQP = NVOQP + 2
            VOQR(5) = NVOQP-1
            OQPROC(5)=.TRUE.
         ENDIF
      ENDIF

!        for partitioning output compute A, depth, current, K and Cg

      IF (IVTYPE.EQ.100 .OR. IVTYPE.EQ.110 .OR. IVTYPE.EQ.120 .OR.&
      &IVTYPE.EQ.130 .OR. IVTYPE.EQ.140 .OR. IVTYPE.EQ.150 .OR.&
      &IVTYPE.EQ.160)&
      &BKC = 2

   end do

!     in case of print of spectrum Ux and Uy have to be computed

   IF (     RTYPE(1:2) .EQ. 'SP'&
   &.OR. RTYPE(1:2) .EQ. 'NE') THEN
      OQPROC(4)  = .TRUE.
      VOQR(4)    = NVOQP+1
      NVOQP      = NVOQP+1
      BKC        = 1
      IF (ICUR.GT.0) THEN
         BKC = 2
         OQPROC(5) = .TRUE.
         VOQR(5)   = NVOQP+1
         NVOQP     = NVOQP+2
      ENDIF
   ENDIF
!NCF!
!NCF!     add significant wave height and wind to any netCDF file
!NCF!
!NCF   FILENM = OUTP_FILES(OUTI(2))
!NCF   NCF    = INDEX( FILENM, '.NC' ).NE.0 .OR.&
!NCF   &INDEX (FILENM, '.nc' ).NE.0
!NCF   IF ( NCF ) THEN
!NCF!        significant wave height must be added
!NCF      IF (.NOT.OQPROC(10)) THEN
!NCF         NVOQP = NVOQP + 1
!NCF         VOQR(10) = NVOQP
!NCF         OQPROC(10)=.TRUE.
!NCF      ENDIF
!NCF!        wind must be added
!NCF      IF (.NOT.OQPROC(26)) THEN
!NCF         NVOQP = NVOQP + 2
!NCF         VOQR(26) = NVOQP-1
!NCF         OQPROC(26)=.TRUE.
!NCF      ENDIF
!NCF   ENDIF

   RETURN
!*    end of subroutine SWORDC   **
end subroutine SWORDC
!************************************************************************
!                                                                      *
SUBROUTINE SWODDC (OPI, OPR, PSNAME, PSTYPE, MIP, MXK, MYK,&
&XNLEN, YNLEN, MXN, MYN, XPCN, YPCN, ALPCN,&
&XCGRID,YCGRID,RTYPE)
   USE swan_service_interfaces, ONLY: STRACE, EQREAL
!                                                                      *
!************************************************************************

   USE OCPCOMM3
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM3
   USE SWCOMM4
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
!
!     30.72: IJsbrand Haagsma
!     40.22: John Cazes and Tim Campbell
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!            Oct. 95: New subroutine
!     30.72, Sept 97: Replaced DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, Jan. 99: argument RTYPE added, computation of ALCQ changed
!                     output type (indicated by RTYPE) is PLOT
!     40.22, Sep. 01: small corrections
!     40.31, Nov. 03: removing HPGL-functionality
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Decodes output point set data
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!  PSNAME      Char   input   name of output point set referred to
!  PSTYPE      Char   input   type of output point set
!  MIP         Int    outp    number of output points
!  MXK         Int    outp    number of output points in X-direction (Frame)
!  MYK         Int    outp    number of output points in Y-direction (Frame)
!  XNLEN,YNLEN real   outp    (X,Y)lenght of the nested grid
!  MXN, MYN    int    outp    number of meshes in X, Y direction for
!                             the nested grid
!  XPCN, YPCN  real   outp    location of the origin of the nested grid
!  ALPCN       real   outp    angle of the nested grid with the positive
!                             x-axis, counterclockwise measured
!  RTYPE       char   input   indicates type of output; "PLOT" means that
!                             a spatial plot is made
   CHARACTER(LEN=*) :: RTYPE
   INTEGER OPI(2)
   REAL    OPR(5)

!  5. SUBROUTINES CALLING
!
!       SWOUTP (SWAN/OUTP)
!
!  6. SUBROUTINES USED
!
!  7. ERROR MESSAGES
!
!       If the point set is not of a known type an error message
!       is printed and control returns to subroutine SWOUTP
!       If the point set is not of the type frame or ngrid an error message
!       is printed and control returns to subroutine SWOUTP
!
!  8. REMARKS
!
!       ---
!
!  9. STRUCTURE
!
!       ----------------------------------------------------------------
!       Depending on type of set of output points
!       determine name, type and number of output points of
!       the output point set
!       ----------------------------------------------------------------

   CHARACTER(LEN=*) :: PSNAME
   CHARACTER(LEN=1) :: PSTYPE
   INTEGER, SAVE :: IENT = 0
   INTEGER    MIP, MXK, MXN, MYK, MYN
   REAL       ALPCN, XCLOSE, XCMAX, XCMIN, XNLEN, XPCN
   REAL       XPMAX, XPMIN, YCMAX, YCMIN, YNLEN, YPCN, YPMAX, YPMIN

   CALL STRACE (IENT, 'SWODDC')

   ALPQ  = 0.
   COSPQ = 1.
   SINPQ = 0.
!     ALCQ  = ALPC           removed 30.50: U and V now in user coordinates
   ALCQ  = 0.
   COSCQ = COS(ALCQ)
   SINCQ = SIN(ALCQ)

   IF (PSTYPE.EQ.'F') THEN
      MXK  = OPI(1)
      MYK  = OPI(2)
      MIP  = MXK * MYK
      XPQ  = OPR(1)
      YPQ  = OPR(2)
      ALPQ = OPR(5)
      COSPQ = COS(ALPQ)
      SINPQ = SIN(ALPQ)
      XQP   = -XPQ*COSPQ - YPQ*SINPQ
      YQP   =  XPQ*SINPQ - YPQ*COSPQ
      IF (EQREAL(OUTPAR(4),1.)) THEN
!         directions will be w.r.t. frame coordinate system
         ALCQ = -ALPQ
      ELSE
!         directions will be w.r.t. user coordinate system (default)
         ALCQ = 0.
      ENDIF
      COSCQ = COS(ALCQ)
      SINCQ = SIN(ALCQ)
      DXK   = OPR(3) / FLOAT(MXK-1)
      IF ( MYK.GT.1 ) THEN
         DYK   = OPR(4) / FLOAT(MYK-1)
      ELSE
         DYK   = 0.
      END IF
   ELSE IF (PSTYPE.EQ.'H') THEN
      MXK  = OPI(1)
      MYK  = OPI(2)
      MIP  = MXK * MYK
      ALPQ = OPR(5)
      COSPQ = COS(ALPQ)
      SINPQ = SIN(ALPQ)
      XCMAX = OPR(1)
      YCMAX = OPR(2)
      XCMIN = OPR(3)
      YCMIN = OPR(4)
!       *** Find XQLEN and YQLEN taken the extreme points    ***
!       *** that belongs to the frame                        ***
      XPMIN =  1.E09
      YPMIN =  1.E09
      XPMAX = -1.E09
      YPMAX = -1.E09

      XPQ   = XPMIN
      YPQ   = YPMIN
      XQP   = -XPQ
      YQP   = -YPQ
      ALCQ  = 0.
      COSCQ = COS(ALCQ)
      SINCQ = SIN(ALCQ)
      DXK   = (XPMAX - XPMIN)/ FLOAT(MXK-1)
      IF ( MYK.GT.1 ) THEN
         DYK   = (YPMAX - YPMIN) / FLOAT(MYK-1)
      ELSE
         DYK   = 0.
      END IF
      XNLEN = 0.
      YNLEN = 0.
      MXN   = 0
      MYN   = 0
      XPCN  = 0.
      YPCN  = 0.
      ALPCN = 0.
      IF (IOUTES .GE. 20) THEN
         WRITE(PRINTF, "('SWODDC :',/,' MXK ,MYK , XQP ,YQP ,DXK' ,' ,DYK' ,/,2(1X,I5), 4(1X,E9.3))") MXK ,MYK , XQP ,YQP , DXK ,DYK
         IF (PSTYPE .EQ. 'H') WRITE(PRINTF, "(' XCMIN ,XCMAX ,YCMIN ,YCMAX ,', 'XPMAX ,XPMIN :',/,6(1X,E9.3))")XCMIN,XCMAX,YCMIN,YCMAX,&
         &XPMAX ,XPMIN
      ENDIF
   ELSE IF (PSTYPE.EQ.'C' .OR. PSTYPE.EQ.'P') THEN
      MXK = 0
      MYK = 0
      XNLEN = 0.
      YNLEN = 0.
      MXN   = 0
      MYN   = 0
      XPCN  = 0.
      YPCN  = 0.
      ALPCN = 0.
   ELSE IF (PSTYPE.EQ.'N') THEN
!       nested grid
      MXK = 0
      MYK = 0
      XNLEN = OPR(1)
      YNLEN = OPR(2)
      XPCN  = OPR(3)
      YPCN  = OPR(4)
      ALPCN = OPR(5)
      MXN   = OPI(1)
      MYN   = OPI(2)
   ELSE IF (PSTYPE.EQ.'U') THEN
      MXK   = MIP
      MYK   = 1
      XNLEN = 0.
      YNLEN = 0.
      MXN   = 0
      MYN   = 0
      XPCN  = 0.
      YPCN  = 0.
      ALPCN = 0.
   ELSE
      WRITE (PRTEST,'(A)') ' error SWODDC: no PSTYPE defined'
   ENDIF

   IF (PSNAME.EQ.'COMPGRID') THEN
      LCOMPGRD=.TRUE.
   ELSE
      LCOMPGRD=.FALSE.
   ENDIF

   IF (ITEST.GE.100 .OR. IOUTES .GE. 30) THEN
      WRITE (PRTEST, "(' Exit SWODDC ', A16, 2X, A1, 3I6)") PSNAME, PSTYPE, MIP
      IF (PSTYPE.EQ.'F' .OR. PSTYPE .EQ. 'H') WRITE (PRTEST, "('SWODDC : MXK, MYK, ALPQ, DXK, DYK',/, 6X, 2I5, 4(1X,E9.3))")&
      &MXK, MYK, ALPQ, DXK, DYK
   ENDIF

   RETURN
!*    end of subroutine SWODDC   **
end subroutine SWODDC
!************************************************************************
!                                                                      *
SUBROUTINE SWOEXC ( PSTYPE    ,OPI        ,OPR   ,&
&X         ,Y          ,&
&MIP       ,XP         ,&
&YP        ,XC         ,&
&YC        ,KGRPNT     ,&
&XCGRID    ,YCGRID     ,&
&CROSS                 )
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM3
   USE OCPCOMM4
   USE SWCOMM1
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
!  0. Authors
!
!     30.72: IJsbrand Haagsma
!     32.02: Roeland Ris & Cor van der Schelde (1D version)
!     40.00, 40.13: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.30: Marcel Zijlema
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.86: Nico Booij
!
!  1. Update
!
!     30.72, Sept 97: placed a missing comma in FORMAT statement
!     30.72, Sept 97: Replaced DO-block with one CONTINUE to DO-block with
!                     two CONTINUE's
!     32.02, Feb. 98: Introduced 1D version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, June 98: argument KGRBND added, call CVMESH modified
!     40.02, Oct. 00: Gave KGRBND array dimension
!     40.13, Aug. 01: repeating grid (KREPTX>0) XC modified
!                     swcomm4.inc reactivated
!     40.30, Apr. 03: introduction distributed-memory approach using MPI
!     40.31, Dec. 03: removing POOL mechanism
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.86, Feb. 08: modifications to prevent interpolation over obstacles
!                     arguments added to list
!
!  2. Purpose
!
!     Calculates computational grid coordinates of the output points
!
!  3. Method
!
!     ---
!
!  4. Argument variables

   INTEGER MIP
   INTEGER OPI(2)

!     XCGRID: input  Coordinates of computational grid in x-direction
!     YCGRID: input  Coordinates of computational grid in y-direction

   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)
   REAL    OPR(5), X(MIP), Y(MIP)

!     PSTYPE  Char   input    type of output point set
!     MIP     Int    input    number of output points
!     XP, YP  real   outp     user coordinates of output point
!     XC, YC  real   outp     comp. grid coordinates

   LOGICAL CROSS(4,MIP) ! true if obstacle is between output point
   ! and computational grid point
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     OUTPUT (SWAN/OUTP)
!
! 11. Remarks
!
!     ---
!
! 13. Source text

   REAL       XC(*), YC(*), XP(MIP), YP(MIP)
   CHARACTER(LEN=1) :: PSTYPE
   INTEGER     KGRPNT(MXC,MYC)
   INTEGER, SAVE :: IENT = 0
   INTEGER   IP, IXQ, IYQ
   INTEGER   ITMP1, ITMP2, ITMP3, ITMP4, ITMP5, ITMP6
   REAL      RTMP1, RTMP2, RTMP3, RTMP4
   REAL      DCXQ, DCYQ, XCA, XCMAX, XCMIN, XP1, XPA, XPMAX, XPMIN
   REAL      XPP, XX, YCA, YCMAX, YCMIN, YP1, YPA, YPMAX, YPMIN, YPP, YY

   CALL STRACE (IENT, 'SWOEXC')

   coordinate_transform: BLOCK
   IF (PSTYPE.EQ.'F') THEN
      MXQ   = OPI(1)
      MYQ   = OPI(2)
      ALPQ  = OPR(5)
      COSPQ = COS(ALPQ)
      SINPQ = SIN(ALPQ)
      XPQ   = OPR(1)
      YPQ   = OPR(2)
      XQLEN = OPR(3)
      YQLEN = OPR(4)
      XQP   = -XPQ*COSPQ - YPQ*SINPQ
      YQP   =  XPQ*SINPQ - YPQ*COSPQ
      IF (MXQ.GT.1) THEN
         DXQ = XQLEN/(MXQ-1)
      ELSE
         DXQ = 0.01
      ENDIF
      IF (MYQ.GT.1) THEN
         DYQ = YQLEN/(MYQ-1)
      ELSE
         DYQ = 0.01
      ENDIF
      IP    = 0
      do IYQ = 1, MYQ
         YY  = (IYQ-1)*DYQ
         XP1 = XPQ - YY*SINPQ
         YP1 = YPQ + YY*COSPQ
         do IXQ = 1, MXQ
            XX = (IXQ-1)*DXQ
            IP = IP+1
            XP(IP) = XP1 + XX*COSPQ
            YP(IP) = YP1 + XX*SINPQ
         end do
      end do
   ELSE IF (PSTYPE .EQ. 'H') THEN
      XCMAX = OPR(1)
      YCMAX = OPR(2)
      XCMIN = OPR(3)
      YCMIN = OPR(4)
      ALPQ  = OPR(5)
      MXQ   = OPI(1)
      MYQ   = OPI(2)
      COSPQ = COS(ALPQ)
      SINPQ = SIN(ALPQ)
      IF (MXQ.GT.1) THEN
         DCXQ = (XCMAX - XCMIN)/(MXQ-1)
      ELSE
         DCXQ = 0.
      END IF
      IF (MYQ.GT.1) THEN
         DCYQ = (YCMAX - YCMIN)/(MYQ-1)
      ELSE
         DCYQ = 0.
      END IF
      XC(1) = XCMIN
      YC(1) = YCMIN
!       *** Find XQLEN and YQLEN taken the extreme points    ***
!       *** that belongs to the frame                        ***
      XPMIN =  1.E09
      YPMIN =  1.E09
      XPMAX = -1.E09
      YPMAX = -1.E09

      XPQ   = XPMIN
      YPQ   = YPMIN
      XQP   = 0.
      YQP   = 0.
      XQLEN = XPMAX - XPMIN
      YQLEN = YPMAX - YPMIN
      IF (MXQ.GT.1) THEN
         DXQ = (XQLEN)/(MXQ-1)
      ELSE
         DXQ = 0.
      END IF
      IF (MYQ.GT.1) THEN
         DYQ = (YQLEN)/(MYQ-1)
      ELSE
         DYQ = 0.
      END IF
      IF (ITEST.GE. 120 ) THEN
         WRITE(PRINTF,"(' SWOEXC FRAME DATA :',/,' XQLEN ,YQLEN ', ',MXQ ,MYQ , DCXQ ,DCYQ ,XC(1) ,YC(1)', ' ,DXQ ,DYQ',/,1X, 2(1X,E9.3),2(1X,I4),2X,6(1X,E9.3))") XQLEN ,YQLEN ,MXQ ,MYQ ,&
         &DCXQ  ,DCYQ ,XC(1) ,YC(1),DXQ ,DYQ
         WRITE(PRINTF,"('XCMIN ,XCMAX ,YCMIN ,YCMAX ,XPMIN', ' ,XPMAX ,YPMIN ,YPMAX ',/,8(1X,E9.3),/)")XCMIN,XCMAX,YCMIN,YCMAX,&
         &XPMIN,XPMAX,YPMIN,YPMAX
      ENDIF
      YY    = YCMIN - DCYQ
      IP    = 0
      do IYQ = 1 ,MYQ
         XX = XCMIN - DCXQ
         YY = YY    + DCYQ
         do IXQ = 1 ,MXQ
            IP = IP + 1
            XX = XX + DCXQ
            XC(IP) = XX - REAL(MXF) + 1.
            YC(IP) = YY - REAL(MYF) + 1.
            IF ( XC(IP).GE.-0.01 .AND. XC(IP).LE.REAL(MXC-1)+0.01 .AND.&
            &YC(IP).GE.-0.01 .AND. YC(IP).LE.REAL(MYC-1)+0.01 ) THEN
               IF ( KGRPNT(NINT(XC(IP))+1,NINT(YC(IP))+1).GT.1 ) THEN
                  CALL EVALF (XC(IP)+1.,YC(IP)+1.,XPP,YPP,XCGRID,YCGRID)
                  XP(IP) = XPP
                  YP(IP) = YPP
               ELSE
                  XP(IP) = OVEXCV(1)
                  YP(IP) = OVEXCV(2)
               END IF
            ELSE
               XP(IP) = OVEXCV(1)
               YP(IP) = OVEXCV(2)
            ENDIF
            IF (ITEST.GE.200) WRITE(PRTEST,"(' SWOEXC, PROBLEM COORD:', 2(1X,F12.4),/, ' COMPUT COORD:', 2(1X,F12.4))")&
            &XP(IP), YP(IP), XC(IP), YC(IP)
         end do
      end do
      EXIT coordinate_transform
   ELSE IF (PSTYPE.EQ.'C' .OR. PSTYPE.EQ.'P' .OR.&
   &PSTYPE.EQ.'N' .OR. PSTYPE.EQ.'U' ) THEN
      XP = X
      YP = Y
   ENDIF

!     transform to computational grid

   IF (ITEST.GE. 150 .AND. OPTG .EQ. 1)&
   &WRITE (PRTEST, "(' SWOEXC, transf. coeff.:', 8(1X,E12.4))") XCP, YCP, COSPC, SINPC, DX, DY
!     *** The transformation to computational grid depends ***
!     *** on the grid type: regular(1) , curvilinear(3)    ***
   do IP=1, MIP
      IF (OPTG .EQ. 1) THEN
         XC(IP) = (XCP + XP(IP)*COSPC + YP(IP)*SINPC) / DX
         XC(IP) = XC(IP) - REAL(MXF) + 1.
!         repeating grid: XC is shifted to be between 0 and MXC
         IF (KREPTX.GT.0) XC(IP) = MODULO (XC(IP), REAL(MXC))
         IF (ONED) THEN
            YC(IP) = 0
         ELSE
            YC(IP) = (YCP - XP(IP)*SINPC + YP(IP)*COSPC) / DY
            YC(IP) = YC(IP) - REAL(MYF) + 1.
         ENDIF
      ELSEIF (OPTG.EQ.3) THEN
         XPA = XP(IP)
         YPA = YP(IP)
         ITMP1  = MXC
         ITMP2  = MYC
         ITMP3  = MCGRD
         ITMP4  = NGRBND
         ITMP5  = MXF
         ITMP6  = MYF
         RTMP1  = XCLMIN
         RTMP2  = XCLMAX
         RTMP3  = YCLMIN
         RTMP4  = YCLMAX
         MXC    = MXCGL
         MYC    = MYCGL
         MCGRD  = MCGRDGL
         NGRBND = NGRBGL
         MXF    = 1
         MYF    = 1
         XCLMIN = XCGMIN
         XCLMAX = XCGMAX
         YCLMIN = YCGMIN
         YCLMAX = YCGMAX
         CALL CVMESH (XPA, YPA, XCA, YCA, KGRPGL, XGRDGL ,YGRDGL,&
         &KGRBGL)
         MXC    = ITMP1
         MYC    = ITMP2
         MCGRD  = ITMP3
         NGRBND = ITMP4
         MXF    = ITMP5
         MYF    = ITMP6
         XCLMIN = RTMP1
         XCLMAX = RTMP2
         YCLMIN = RTMP3
         YCLMAX = RTMP4
         XC(IP) = XCA - REAL(MXF) + 1.
         YC(IP) = YCA - REAL(MYF) + 1.
      ENDIF
      IF (ITEST.GE.250) WRITE(PRTEST,"(' SWOEXC, PROBLEM COORD:', 2(1X,F12.4),/, ' COMPUT COORD:', 2(1X,F12.4))") XP(IP), YP(IP),&
      &XC(IP), YC(IP)
   end do
   END BLOCK coordinate_transform

   RETURN
end subroutine SWOEXC
!************************************************************************
!                                                                      *
SUBROUTINE SWOEXD (RTYPE, OQPROC, MIP, XC, YC, VOQR, VOQ, COMPDA ,&
&KGRPNT, FORCE, CROSS, IONOD&
&,IRQ&
&)
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: STRACE, EQREAL, STPNOW
   USE swan_output_interpolation, ONLY: SwanInterpolateOutput
   USE swan_structured_output_interpolation, ONLY: SWIPOL
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE swan_time, ONLY: default_time_context
   USE M_PARALL
   USE M_DIFFR
   USE OUTP_DATA
   USE SwanGriddata
   USE SwanGridobjects
!METIS   USE SwanParallel

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
!     30.72: IJsbrand Haagsma
!     32.02: Roeland Ris & Cor van der Schelde (1D version)
!     31.02, 40.13: Nico Booij
!     40.21: Agnieszka Herman
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.61: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.86: Nico Booij
!     41.12: Nico Booij
!     41.75: Erick Rogers
!
!  1. Updates
!
!     10.07, July 94, error in current velocity repaired, wind velocity
!     30.72, Oct. 97: logical function EQREAL introduced for floating point
!                     comparisons
!     32.02, Feb. 98: Introduced 1D version
!     31.02, Sep. 97: computation of Setup, and computation of Force
!                     from setup computation
!     40.00, June 98: Tsec added
!     33.09, Mar. 00: in case of spherical coordinates, distance in m is
!                     calculated from coordinates in degrees
!     40.13, Oct. 01: Forces always computed by SWOEXF
!     40.21, Nov. 01: diffraction parameter added
!     40.41, Aug. 04: friction coefficient added
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: bottom wave period added
!     40.51, Sep. 05: water level and bottom level added
!     40.61, Sep. 06: separate dissipation coefficients added
!     40.80, Sep. 07: extension to unstructured grids
!     40.86, Feb. 08: interpolation near obstacles modified,
!                     points on the other side of the obstacle not taken into account
!                     calls of SWIPOL changed
!     41.12, Apr. 10: output quantity NPL (type nr 70) added
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Calculates  Dist, Depth, Ux, Uy, ..
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     FORCE   real   input    wave-induced force
!     IONOD   Int    outp     array indicating in which subdomain
!                             output points are located
!     OQPROC  logic  input    y/n process outp quantities
!     PSNAME  Char   input    name of output point set referred to
!     MIP     Int    input    number of output points
!     XP, YP  real   outp     user coordinates of output point
!     XC, YC  real   outp     comp. grid coordinates
!     WX2, WY2  real input    wind components
!
!  9. Subroutines calling
!
!     SWOUTP (SWAN/OUTPUT)
!
!  8. Subroutines used
!
!     SWIPOL
!
! 11. Remarks
!
!       ---
!
! 13. Source text

   INTEGER    MIP
   REAL       XC(*), YC(*), VOQ(MIP,*), COMPDA(MCGRD,MCMVAR)
   REAL       FORCE(nverts,2)
   INTEGER    VOQR(*), KGRPNT(MXC,MYC)
   INTEGER    IONOD(*)
   INTEGER    IRQ
   INTEGER    IVERTP, NOWNV
   INTEGER    NREF, IOSTAT
   INTEGER, ALLOCATABLE :: KVERT(:)
   CHARACTER(LEN=4) :: RTYPE
   LOGICAL    OQPROC(*)
   LOGICAL    CROSS(4,MIP)
   LOGICAL, ALLOCATABLE :: LTMP(:)

   INTEGER, SAVE :: IENT = 0
   INTEGER JJ,IVXP,IVYP,IVDIST,IP,JVQX,JVQY,&
   &KK, IXB, IXE, IYB, IYE, IX, IY, IHLX, IHLY
   INTEGER ILPOS
   REAL UXLOC,UYLOC,RDIST,RDX,RDY,RR,UBLOC,F1,RTMP,XP1,YP1
   REAL RVAL1, RVAL2

   INTEGER IVTYPE ! temporary counter for NMOVAR, used in VOQR,
   ! OQPROC, OVKEYW, etc.
   INTEGER JCOMPDA ! temporary index for COMPDA, corresponds to
   ! permanent variable Jw2o1x, etc.

   type(verttype), dimension(:), pointer :: vert
   CALL STRACE (IENT, 'SWOEXD')

   vert => gridobject%vert_grid
   IF (ITEST.GE. 100 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' Entry SWOEXD ', 11L2, I8)")&
   &(OQPROC(JJ), JJ=1,10), OQPROC(26), MIP
   IVXP   = 1
   IVYP   = 2

!     distance

IF (OQPROC(3)) THEN
      IVDIST = VOQR(3)
      DO IP = 1, MIP
         IF (IP.EQ.1) THEN
            RDIST = 0.
         ELSE
            RDX = VOQ(IP,IVXP) - VOQ(IP-1,IVXP)
            RDY = VOQ(IP,IVYP) - VOQ(IP-1,IVYP)
            IF (KSPHER.GT.0) THEN
!             spherical coordinates: distance is expressed in m
               RDX = RDX * LENDEG *&
               &COS(DEGRAD*(YOFFS+0.5*(VOQ(IP,IVYP)+VOQ(IP-1,IVYP))))
               RDY = RDY * LENDEG
            ENDIF
            RDIST = RDIST + SQRT(RDX*RDX+RDY*RDY)
         ENDIF
         VOQ(IP,IVDIST) = RDIST
      ENDDO
   ENDIF

   IF (OPTG.EQ.5) THEN

!        ---find closest vertex for given point in case of unstructured

      ALLOCATE(KVERT(MIP))
      IF (.NOT.LCOMPGRD) THEN
         DO IP = 1, MIP
            CALL SwanFindPoint ( VOQ(IP,1), VOQ(IP,2), KVERT(IP) )
         ENDDO
      ELSE
         DO IP = 1, MIP
            KVERT(IP) = IP
         ENDDO
      ENDIF

   ENDIF

!     depth

IF (OQPROC(4)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 4,&
      &VOQR(4), JDP2
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JDP2), OVEXCV(4), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(4)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(4)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JDP2),&
         &MIP, KVERT, OVEXCV(4) )
      ENDIF
   ENDIF

!     current velocity

IF (OQPROC(5)) THEN
      JVQX = VOQR(5)
      JVQY = JVQX+1
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 5,&
      &VOQR(5), JVX2
      IF (ICUR.EQ.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL (COMPDA(1,JVX2), OVEXCV(5), XC, YC, MIP, CROSS,&
            &VOQ(1,JVQX) ,KGRPNT, COMPDA(1,JDP2))
            CALL SWIPOL (COMPDA(1,JVY2), OVEXCV(5), XC, YC, MIP, CROSS,&
            &VOQ(1,JVQY) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,JVQX), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JVX2),&
            &MIP, KVERT, OVEXCV(5) )
            CALL SwanInterpolateOutput ( VOQ(1,JVQY), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JVY2),&
            &MIP, KVERT, OVEXCV(5) )
         ENDIF
         DO IP = 1, MIP
            UXLOC = VOQ(IP,JVQX)
            UYLOC = VOQ(IP,JVQY)
            VOQ(IP,JVQX) = COSCQ*UXLOC - SINCQ*UYLOC
            VOQ(IP,JVQY) = SINCQ*UXLOC + COSCQ*UYLOC
         ENDDO
      ELSE
         DO IP = 1, MIP
            VOQ(IP,JVQX) = 0.
            VOQ(IP,JVQY) = 0.
         ENDDO
      ENDIF
   ENDIF

!     Ubot

IF (OQPROC(6)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 6,&
      &VOQR(6)
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JUBOT), OVEXCV(6), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(6)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(6)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JUBOT),&
         &MIP, KVERT, OVEXCV(6) )
      ENDIF
      KK = VOQR(6)
      RR = SQRT(2.)
      DO IP = 1, MIP
         UBLOC = VOQ(IP,KK)
         IF (.NOT.EQREAL(UBLOC,OVEXCV(6))) VOQ(IP,KK) = RR * UBLOC
      ENDDO
   ENDIF

!     Urms

   IF (OQPROC(34)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 34,&
      &VOQR(34)
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JUBOT), OVEXCV(34), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(34)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(34)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JUBOT),&
         &MIP, KVERT, OVEXCV(34) )
      ENDIF
   ENDIF

!     TmBot

   IF (OQPROC(50)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 50,&
      &VOQR(50), JPBOT
      IF (JPBOT.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JPBOT),OVEXCV(50),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(50)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(50)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JPBOT),&
            &MIP, KVERT, OVEXCV(50) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(50)) = OVEXCV(50)
         ENDDO
      ENDIF
   ENDIF

!     dissipation

   IF (OQPROC(7)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 7,&
      &VOQR(7)
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JDISS), OVEXCV(7), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(7)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(7)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JDISS),&
         &MIP, KVERT, OVEXCV(7) )
      ENDIF
      IF (INRHOG.EQ.1) THEN
         do IP = 1, MIP
            F1 = VOQ(IP,VOQR(7))
            IF (.NOT.EQREAL(F1,OVEXCV(7))) VOQ(IP,VOQR(7))=F1*RHO*GRAV
         end do
      ENDIF
   ENDIF

!     bottom friction dissipation

   IF (OQPROC(54)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 54,&
      &VOQR(54), JDSXB
      IF (JDSXB.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXB),OVEXCV(54),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(54)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(54)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXB),&
            &MIP, KVERT, OVEXCV(54) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(54)) = OVEXCV(54)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(54))
            IF (.NOT.EQREAL(F1,OVEXCV(54))) VOQ(IP,VOQR(54))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     wave breaking dissipation

   IF (OQPROC(55)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 55,&
      &VOQR(55), JDSXS
      IF (JDSXS.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXS),OVEXCV(55),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(55)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(55)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXS),&
            &MIP, KVERT, OVEXCV(55) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(55)) = OVEXCV(55)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(55))
            IF (.NOT.EQREAL(F1,OVEXCV(55))) VOQ(IP,VOQR(55))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     whitecapping dissipation

   IF (OQPROC(56)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 56,&
      &VOQR(56), JDSXW
      IF (JDSXW.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXW),OVEXCV(56),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(56)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(56)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXW),&
            &MIP, KVERT, OVEXCV(56) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(56)) = OVEXCV(56)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(56))
            IF (.NOT.EQREAL(F1,OVEXCV(56))) VOQ(IP,VOQR(56))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     vegetation dissipation

   IF (OQPROC(57)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 57,&
      &VOQR(57), JDSXV
      IF (JDSXV.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXV),OVEXCV(57),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(57)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(57)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXV),&
            &MIP, KVERT, OVEXCV(57) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(57)) = OVEXCV(57)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(57))
            IF (.NOT.EQREAL(F1,OVEXCV(57))) VOQ(IP,VOQR(57))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     turbulent dissipation

   IF (OQPROC(72)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 72,&
      &VOQR(72), JDSXT
      IF (JDSXT.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXT),OVEXCV(72),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(72)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(72)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXT),&
            &MIP, KVERT, OVEXCV(72) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(72)) = OVEXCV(72)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(72))
            IF (.NOT.EQREAL(F1,OVEXCV(72))) VOQ(IP,VOQR(72))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     fluid mud dissipation

   IF (OQPROC(74)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 74,&
      &VOQR(74), JDSXM
      IF (JDSXM.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXM),OVEXCV(74),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(74)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(74)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXM),&
            &MIP, KVERT, OVEXCV(74) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(74)) = OVEXCV(74)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(74))
            IF (.NOT.EQREAL(F1,OVEXCV(74))) VOQ(IP,VOQR(74))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     swell dissipation

   IF (OQPROC(75)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 75,&
      &VOQR(75), JDSXL
      IF (JDSXL.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JDSXL),OVEXCV(75),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(75)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(75)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JDSXL),&
            &MIP, KVERT, OVEXCV(75) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(75)) = OVEXCV(75)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(75))
            IF (.NOT.EQREAL(F1,OVEXCV(75))) VOQ(IP,VOQR(75))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     dissipation by sea ice: integrated Sice term

   IVTYPE=76
   JCOMPDA=JDSXI ! give J-name here, JDSXI/JAICE2/JHICE2
!     begin block of code that is identical for all new variables
   IF (OQPROC(IVTYPE)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") IVTYPE,&
      &VOQR(IVTYPE), JCOMPDA
      IF (JCOMPDA.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JCOMPDA),OVEXCV(IVTYPE),XC,YC, MIP,&
            &CROSS,VOQ(1,VOQR(IVTYPE)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(IVTYPE)),VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JCOMPDA),&
            &MIP, KVERT, OVEXCV(IVTYPE) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDDO
      ENDIF
   ENDIF
   IVTYPE=-999
   JCOMPDA=-999
!     end block of code that is identical for all new variables
!
!     ice concentration (fraction)

   IVTYPE=77
   JCOMPDA=JAICE2 ! give J-name here, JDSXI/JAICE2/JHICE2
!     note special use of VARAICE and PICE
   IF (OQPROC(IVTYPE)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") IVTYPE,&
      &VOQR(IVTYPE), JCOMPDA
      IF (VARAICE) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JCOMPDA),OVEXCV(IVTYPE),XC,YC, MIP,&
            &CROSS,VOQ(1,VOQR(IVTYPE)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(IVTYPE)),VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JCOMPDA),&
            &MIP, KVERT, OVEXCV(IVTYPE) )
         ENDIF
      ELSE
         F1 = PICE(1)
         DO IP = 1, MIP
            IF (.NOT.EQREAL(F1,OVEXCV(IVTYPE)))&
            &VOQ(IP,VOQR(IVTYPE)) = F1
         END DO
      ENDIF
   ENDIF
   IVTYPE=-999
   JCOMPDA=-999

!     ice thickness (in meters)

   IVTYPE=78
   JCOMPDA=JHICE2 ! give J-name here, JDSXI/JAICE2/JHICE2
!     note special use of PICE
   IF (OQPROC(IVTYPE)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") IVTYPE,&
      &VOQR(IVTYPE), JCOMPDA
      IF (JCOMPDA.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JCOMPDA),OVEXCV(IVTYPE),XC,YC, MIP,&
            &CROSS,VOQ(1,VOQR(IVTYPE)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(IVTYPE)),VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JCOMPDA),&
            &MIP, KVERT, OVEXCV(IVTYPE) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(IVTYPE)) =  PICE(2)
         ENDDO
      ENDIF
   ENDIF
   IVTYPE=-999
   JCOMPDA=-999

!     energy generation

   IF (OQPROC(60)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 60,&
      &VOQR(60), JGENR
      IF (JGENR.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JGENR),OVEXCV(60),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(60)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(60)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JGENR),&
            &MIP, KVERT, OVEXCV(60) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(60)) = OVEXCV(60)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(60))
            IF (.NOT.EQREAL(F1,OVEXCV(60))) VOQ(IP,VOQR(60))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     wind source term

   IF (OQPROC(61)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 61,&
      &VOQR(61), JGSXW
      IF (JGSXW.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JGSXW),OVEXCV(61),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(61)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(61)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JGSXW),&
            &MIP, KVERT, OVEXCV(61) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(61)) = OVEXCV(61)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(61))
            IF (.NOT.EQREAL(F1,OVEXCV(61))) VOQ(IP,VOQR(61))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     energy redistribution

   IF (OQPROC(62)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 62,&
      &VOQR(62), JREDS
      IF (JREDS.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JREDS),OVEXCV(62),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(62)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(62)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JREDS),&
            &MIP, KVERT, OVEXCV(62) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(62)) = OVEXCV(62)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(62))
            IF (.NOT.EQREAL(F1,OVEXCV(62))) VOQ(IP,VOQR(62))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     total absolute 4-wave interaction

   IF (OQPROC(63)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 63,&
      &VOQR(63), JRSXQ
      IF (JRSXQ.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JRSXQ),OVEXCV(63),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(63)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(63)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JRSXQ),&
            &MIP, KVERT, OVEXCV(63) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(63)) = OVEXCV(63)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(63))
            IF (.NOT.EQREAL(F1,OVEXCV(63))) VOQ(IP,VOQR(63))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     total absolute 3-wave interaction

   IF (OQPROC(64)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 64,&
      &VOQR(64), JRSXT
      IF (JRSXT.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JRSXT),OVEXCV(64),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(64)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(64)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JRSXT),&
            &MIP, KVERT, OVEXCV(64) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(64)) = OVEXCV(64)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(64))
            IF (.NOT.EQREAL(F1,OVEXCV(64))) VOQ(IP,VOQR(64))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     total absolute Bragg scattering

   IF (OQPROC(79)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 79,&
      &VOQR(79), JRSXB
      IF (JRSXB.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JRSXB),OVEXCV(79),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(79)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(79)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JRSXB),&
            &MIP, KVERT, OVEXCV(79) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(79)) = OVEXCV(79)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(79))
            IF (.NOT.EQREAL(F1,OVEXCV(79))) VOQ(IP,VOQR(79))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     total absolute QC scattering

   IF (OQPROC(80)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 80,&
      &VOQR(80), JRSXC
      IF (JRSXC.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JRSXC),OVEXCV(80),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(80)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(80)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JRSXC),&
            &MIP, KVERT, OVEXCV(80) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(80)) = OVEXCV(80)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(80))
            IF (.NOT.EQREAL(F1,OVEXCV(80))) VOQ(IP,VOQR(80))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     energy propagation

   IF (OQPROC(65)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 65,&
      &VOQR(65), JTRAN
      IF (JTRAN.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTRAN),OVEXCV(65),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(65)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(65)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTRAN),&
            &MIP, KVERT, OVEXCV(65) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(65)) = OVEXCV(65)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(65))
            IF (.NOT.EQREAL(F1,OVEXCV(65))) VOQ(IP,VOQR(65))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     xy-propagation

   IF (OQPROC(66)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 66,&
      &VOQR(66), JTSXG
      IF (JTSXG.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTSXG),OVEXCV(66),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(66)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(66)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTSXG),&
            &MIP, KVERT, OVEXCV(66) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(66)) = OVEXCV(66)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(66))
            IF (.NOT.EQREAL(F1,OVEXCV(66))) VOQ(IP,VOQR(66))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     theta-propagation

   IF (OQPROC(67)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 67,&
      &VOQR(67), JTSXT
      IF (JTSXT.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTSXT),OVEXCV(67),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(67)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(67)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTSXT),&
            &MIP, KVERT, OVEXCV(67) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(67)) = OVEXCV(67)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(67))
            IF (.NOT.EQREAL(F1,OVEXCV(67))) VOQ(IP,VOQR(67))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     sigma-propagation

   IF (OQPROC(68)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 68,&
      &VOQR(68), JTSXS
      IF (JTSXS.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTSXS),OVEXCV(68),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(68)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(68)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTSXS),&
            &MIP, KVERT, OVEXCV(68) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(68)) = OVEXCV(68)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(68))
            IF (.NOT.EQREAL(F1,OVEXCV(68))) VOQ(IP,VOQR(68))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     radiation stress

   IF (OQPROC(69)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 69,&
      &VOQR(69), JRADS
      IF (JRADS.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JRADS),OVEXCV(69),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(69)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(69)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JRADS),&
            &MIP, KVERT, OVEXCV(69) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(69)) = OVEXCV(69)
         ENDDO
      ENDIF
      IF (INRHOG.EQ.1) THEN
         DO IP = 1, MIP
            F1 = VOQ(IP,VOQR(69))
            IF (.NOT.EQREAL(F1,OVEXCV(69))) VOQ(IP,VOQR(69))=F1*RHO*GRAV
         END DO
      ENDIF
   ENDIF

!     number of plants per square meter

   IF (OQPROC(70)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 70,&
      &VOQR(70), JNPLA2
      IF (JNPLA2.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JNPLA2),OVEXCV(70),XC,YC, MIP, CROSS,&
            &VOQ(1,VOQR(70)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(70)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JNPLA2),&
            &MIP, KVERT, OVEXCV(70) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(70)) = OVEXCV(70)
         ENDDO
      ENDIF
   ENDIF

!     turbulent viscosity

   IF (OQPROC(73)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 73,&
      &VOQR(73), JTURB2
      IF (JTURB2.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTURB2),OVEXCV(73),XC,YC, MIP, CROSS,&
            &VOQ(1,VOQR(73)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(73)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTURB2),&
            &MIP, KVERT, OVEXCV(73) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(73)) = OVEXCV(73)
         ENDDO
      ENDIF
   ENDIF

!     Qb

   IF (OQPROC(8)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 8,&
      &VOQR(8), JQB
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JQB), OVEXCV(8), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(8)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(8)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JQB),&
         &MIP, KVERT, OVEXCV(8) )
      ENDIF
   ENDIF

!     breaker index

   IF (OQPROC(82)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 82,&
      &VOQR(82), JGAMMA
      IF (JGAMMA.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JGAMMA),OVEXCV(82),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(82)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(82)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JGAMMA),&
            &MIP, KVERT, OVEXCV(82) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(82)) = PSURF(2)
         ENDDO
      ENDIF
   ENDIF

!     wind velocity

   IF (OQPROC(26)) THEN
      JVQX = VOQR(26)
      JVQY = JVQX+1
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 26,&
      &VOQR(26), JWX2
      IF (VARWI) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL (COMPDA(1,JWX2), OVEXCV(26),XC, YC, MIP, CROSS,&
            &VOQ(1,JVQX) ,KGRPNT, COMPDA(1,JDP2))
            CALL SWIPOL (COMPDA(1,JWY2), OVEXCV(26),XC, YC, MIP, CROSS,&
            &VOQ(1,JVQY) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,JVQX), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JWX2),&
            &MIP, KVERT, OVEXCV(26) )
            CALL SwanInterpolateOutput ( VOQ(1,JVQY), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JWY2),&
            &MIP, KVERT, OVEXCV(26) )
         ENDIF
         DO IP = 1, MIP
            UXLOC = VOQ(IP,JVQX)
            UYLOC = VOQ(IP,JVQY)
            VOQ(IP,JVQX) = COSCQ*UXLOC - SINCQ*UYLOC
            VOQ(IP,JVQY) = SINCQ*UXLOC + COSCQ*UYLOC
         ENDDO
      ELSE
         UXLOC = U10*COS(WDIP)
         UYLOC = U10*SIN(WDIP)
         DO IP = 1, MIP
            VOQ(IP,JVQX) = COSCQ*UXLOC - SINCQ*UYLOC
            VOQ(IP,JVQY) = SINCQ*UXLOC + COSCQ*UYLOC
         ENDDO
      ENDIF
   ENDIF

!     difference in Hs between iterations

IF (OQPROC(30)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 30,&
      &VOQR(30), JDHS
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JDHS), OVEXCV(30), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(30)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(30)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JDHS),&
         &MIP, KVERT, OVEXCV(30) )
      ENDIF
   ENDIF

!     difference in Tm between iterations

IF (OQPROC(31)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 31,&
      &VOQR(31), JDTM
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JDTM), OVEXCV(31), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(31)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(31)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JDTM),&
         &MIP, KVERT, OVEXCV(31) )
      ENDIF
   ENDIF

!     leak

IF (OQPROC(9)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 9,&
      &VOQR(9), JLEAK
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JLEAK), OVEXCV(9), XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(9)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(9)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JLEAK),&
         &MIP, KVERT, OVEXCV(9) )
      ENDIF
      IF (INRHOG.EQ.1) THEN
         do IP = 1, MIP
            F1 = VOQ(IP,VOQR(9))
            IF (.NOT.EQREAL(F1,OVEXCV(9))) VOQ(IP,VOQR(9))=F1*RHO*GRAV
         end do
      ENDIF
   ENDIF

!     Ufric

   IF (OQPROC(35)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 35,&
      &VOQR(35), JUSTAR
      IF (JUSTAR.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JUSTAR),OVEXCV(35),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(35)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(35)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JUSTAR),&
            &MIP, KVERT, OVEXCV(35) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(35)) = OVEXCV(35)
         ENDDO
      ENDIF
   ENDIF

!     zelen

   IF (OQPROC(36)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 36,&
      &VOQR(36), JZEL
      IF (JZEL.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JZEL), OVEXCV(36), XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(36)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(36)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JZEL),&
            &MIP, KVERT, OVEXCV(36) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(36)) = OVEXCV(36)
         ENDDO
      ENDIF
   ENDIF

!     TauW

   IF (OQPROC(37)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 37,&
      &VOQR(37), JTAUW
      IF (JTAUW.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JTAUW),OVEXCV(37), XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(37)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(37)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JTAUW),&
            &MIP, KVERT, OVEXCV(37) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(37)) = OVEXCV(37)
         ENDDO
      ENDIF
   ENDIF

!     Cdrag

   IF (OQPROC(38)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 38,&
      &VOQR(38), JCDRAG
      IF (JCDRAG.GT.1) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JCDRAG),OVEXCV(38),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(38)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(38)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JCDRAG),&
            &MIP, KVERT, OVEXCV(38) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(38)) = OVEXCV(38)
         ENDDO
      ENDIF
   ENDIF

!     wave-induced setup

   IF (OQPROC(39)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 39,&
      &VOQR(39), JSETUP
      IF (LSETUP.GT.0) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JSETUP),OVEXCV(39),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(39)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(39)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JSETUP),&
            &MIP, KVERT, OVEXCV(39) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(39)) = OVEXCV(39)
         ENDDO
      ENDIF
   ENDIF

!     wave-induced force (unstructured grids only!)

   IF (OQPROC(20).AND.OPTG.EQ.5) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 20,&
      &VOQR(20), 0
      CALL SwanInterpolateOutput ( VOQ(1,VOQR(20)), VOQ(1,1),&
      &VOQ(1,2), FORCE(1,1),&
      &MIP, KVERT, OVEXCV(20) )
      CALL SwanInterpolateOutput ( VOQ(1,VOQR(20)+1), VOQ(1,1),&
      &VOQ(1,2), FORCE(1,2),&
      &MIP, KVERT, OVEXCV(20) )
   ENDIF

!     Ursell

   IF (OQPROC(45)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 45,&
      &VOQR(45), JURSEL
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JURSEL), OVEXCV(45),XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(45)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(45)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JURSEL),&
         &MIP, KVERT, OVEXCV(45) )
      ENDIF
   ENDIF

!     biphase

   IF (OQPROC(83)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 83,&
      &VOQR(83), JBIPH
      IF (OPTG.NE.5) THEN
         CALL SWIPOL (COMPDA(1,JBIPH), OVEXCV(83),XC, YC, MIP, CROSS,&
         &VOQ(1,VOQR(83)) ,KGRPNT, COMPDA(1,JDP2))
      ELSE
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(83)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JBIPH),&
         &MIP, KVERT, OVEXCV(83) )
      ENDIF
      DO IP = 1, MIP
         F1 = VOQ(IP,VOQR(83))
         IF (.NOT.EQREAL(F1,OVEXCV(83))) VOQ(IP,VOQR(83))=F1*180./PI
      ENDDO
   ENDIF

!     Air-Sea temperature difference

   IF (OQPROC(46)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 46,&
      &VOQR(46), JASTD2
      IF (VARAST) THEN
         IF (OPTG.NE.5) THEN
            CALL SWIPOL(COMPDA(1,JASTD2),OVEXCV(46),XC,YC, MIP, CROSS,&
            &VOQ(1,VOQR(46)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(46)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JASTD2),&
            &MIP, KVERT, OVEXCV(46) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(46)) = OVEXCV(46)
         END DO
      END IF
   ENDIF

!       Diffraction parameter

   IF (OQPROC(49)) THEN
      IF (IDIFFR.EQ.1) THEN
         IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 49,&
         &VOQR(49), 0
         IF (OPTG.NE.5) THEN
            CALL SWIPOL (DIFPARAM(:), OVEXCV(49), XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(49)) ,KGRPNT, COMPDA(1,JDP2))
         ELSE
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(49)), VOQ(1,1),&
            &VOQ(1,2), DIFPARAM(:),&
            &MIP, KVERT, OVEXCV(49) )
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(49)) = 1.
         ENDDO
      ENDIF
   ENDIF

!     Tsec

   IF (OQPROC(41)) THEN
      DO IP = 1, MIP
         VOQ(IP,VOQR(41)) = REAL(default_time_context%TIMCO) - OUTPAR(1)
      ENDDO
   ENDIF

!     friction coefficient

   IF (OQPROC(27)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 27,&
      &VOQR(27), JFRC2
      IF (VARFR) THEN
         IF (OPTG.NE.5) THEN
!              interpolation done in all active and non-active points
            RTMP=DEPMIN
            DEPMIN=-10.*ABS(MINVAL(COMPDA(:,JDP2)))
            CALL SWIPOL(COMPDA(1,JFRC2),OVEXCV(27),XC,YC, MIP, CROSS,&
            &VOQ(1,VOQR(27)) ,KGRPNT, COMPDA(1,JDP2))
            DEPMIN=RTMP
         ELSE
!              interpolation done in all active and non-active points
            ALLOCATE(LTMP(nverts))
            LTMP(:) = vert(:)%active
            vert(:)%active = .TRUE.
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(27)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JFRC2),&
            &MIP, KVERT, OVEXCV(27) )
            vert(:)%active = LTMP(:)
            DEALLOCATE(LTMP)
         ENDIF
      ELSE
         F1=0.
         IF (IBOT.EQ.1) F1 = PBOT(3)
         IF (IBOT.EQ.2) F1 = PBOT(2)
         IF (IBOT.EQ.3) F1 = PBOT(5)
         IF (IBOT.EQ.5) F1 = PBOT(7)
         DO IP = 1, MIP
            IF (.NOT.EQREAL(F1,OVEXCV(27))) VOQ(IP,VOQR(27))=F1
         END DO
      END IF
   ENDIF

!     water level

   IF (OQPROC(51)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 51,&
      &VOQR(51), JWLV2
      IF (VARWLV) THEN
         IF (OPTG.NE.5) THEN
!             interpolation done in all active and non-active points
            RTMP=DEPMIN
            DEPMIN=-10.*ABS(MINVAL(COMPDA(:,JDP2)))
            CALL SWIPOL(COMPDA(1,JWLV2),OVEXCV(51),XC, YC, MIP, CROSS,&
            &VOQ(1,VOQR(51)) ,KGRPNT, COMPDA(1,JDP2))
            DEPMIN=RTMP
         ELSE
!             interpolation done in all active and non-active points
            ALLOCATE(LTMP(nverts))
            LTMP(:) = vert(:)%active
            vert(:)%active = .TRUE.
            CALL SwanInterpolateOutput ( VOQ(1,VOQR(51)), VOQ(1,1),&
            &VOQ(1,2), COMPDA(1,JWLV2),&
            &MIP, KVERT, OVEXCV(51) )
            vert(:)%active = LTMP(:)
            DEALLOCATE(LTMP)
         ENDIF
      ELSE
         DO IP = 1, MIP
            VOQ(IP,VOQR(51)) = 0.
         END DO
      END IF
      DO IP = 1, MIP
         F1 = VOQ(IP,VOQR(51))
         IF (.NOT.EQREAL(F1,OVEXCV(51))) VOQ(IP,VOQR(51))=F1 + WLEV
      END DO
   ENDIF

!     bottom level

   IF (OQPROC(52)) THEN
      IF (ITEST.GE.50 .OR. IOUTES .GE. 10) WRITE (PRTEST, "(' SWOEXD, type:', 4I3)") 52,&
      &VOQR(52), JBOTLV
      IF (OPTG.NE.5) THEN
!          interpolation done in all active and non-active points
         RTMP=DEPMIN
         DEPMIN=-10.*ABS(MINVAL(COMPDA(:,JDP2)))
         CALL SWIPOL(COMPDA(1,JBOTLV),OVEXCV(52),XC,YC, MIP, CROSS,&
         &VOQ(1,VOQR(52)) ,KGRPNT, COMPDA(1,JDP2))
         DEPMIN=RTMP
      ELSE
!          interpolation done in all active and non-active points
         ALLOCATE(LTMP(nverts))
         LTMP(:) = vert(:)%active
         vert(:)%active = .TRUE.
         CALL SwanInterpolateOutput ( VOQ(1,VOQR(52)), VOQ(1,1),&
         &VOQ(1,2), COMPDA(1,JBOTLV),&
         &MIP, KVERT, OVEXCV(52) )
         vert(:)%active = LTMP(:)
         DEALLOCATE(LTMP)
      ENDIF
   ENDIF

!     correct problem coordinates with offset values

   DO IP=1, MIP
      XP1 = VOQ(IP,IVXP)
      IF (.NOT.EQREAL(XP1,OVEXCV(1)))&
      &VOQ(IP,IVXP) = XP1 + XOFFS
      YP1 = VOQ(IP,IVYP)
      IF (.NOT.EQREAL(YP1,OVEXCV(2)))&
      &VOQ(IP,IVYP) = YP1 + YOFFS
   ENDDO

   IF (PARLL) THEN

!     --- in case of parallel run, mark location points inside own
!         subdomain

   IF ( OPTG.NE.5 ) THEN
      IF (RTYPE.NE.'BLKV') THEN
         IHLX = IHALOX
         IHLY = IHALOY
      ELSE
         IHLX = 2
         IHLY = 2
      ENDIF
      IXB = 1+IHLX
      IF ( LMXF ) IXB = 1
      IXE = MXC-IHLX
      IF ( LMXL ) IXE = MXC
      IYB = 1+IHLY
      IF ( LMYF ) IYB = 1
      IYE = MYC-IHLY
      IF ( LMYL ) IYE = MYC
      DO IP = 1, MIP
         IX = INT(XC(IP))
         IY = INT(YC(IP))
         RVAL1 = FLOAT(IX)
         RVAL2 = FLOAT(IY)
         IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR. EQREAL(XC(IP),0.) .OR.&
         &RTYPE.EQ.'BLKV' ) IX = IX + 1
         IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR. EQREAL(YC(IP),0.) .OR.&
         &RTYPE.EQ.'BLKV' ) IY = IY + 1
         IF ( IX.GE.IXB .AND. IX.LE.IXE .AND.&
         &IY.GE.IYB .AND. IY.LE.IYE ) IONOD(IP) = INODE
      END DO
   ELSE
      IF ( .NOT.LCOMPGRD .OR. RTYPE.NE.'BLKV' ) THEN
         NOWNV = 0
         DO IP = 1, MIP
            IF ( KVERT(IP).GT.0 ) THEN
!                 excludes ghost nodes
!METIS               IF ( vres(KVERT(IP)) ) THEN
!METIS                  NOWNV = NOWNV + 1
!METIS                  IONOD(IP) = INODE
!METIS               ENDIF
            ENDIF
         ENDDO
      ELSE
!           this includes ghost nodes as required by Paraview
         IONOD(1:MIP) = INODE
      ENDIF

      IF (.NOT.LCOMPGRD) THEN
         NREF   =  0
         IOSTAT = -1
         FILENM = 'output.set'
!           append node number to FILENM
         ILPOS = INDEX ( FILENM, ' ' )-1
         WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
         CALL FOR (NREF, FILENM, 'UU', IOSTAT)
         IF (STPNOW()) RETURN
         WRITE(NREF) IRQ, NOWNV
         DO IP = 1, MIP
            IF ( KVERT(IP).GT.0 ) THEN
               IVERTP = ivertg(KVERT(IP))
            ELSE
               IVERTP = -1
            ENDIF
            IF ( IONOD(IP).EQ.INODE ) WRITE(NREF) IP, IVERTP
         ENDDO
      ENDIF
   ENDIF

   END IF
   IF (ALLOCATED(KVERT)) DEALLOCATE(KVERT)

   RETURN
end subroutine SWOEXD
!************************************************************************
!                                                                      *

!************************************************************************
!                                                                      *
SUBROUTINE SWOEXA (OQPROC     ,BKC        ,&
&MIP        ,XC         ,&
&YC         ,VOQR       ,&
&VOQ        ,AC2        ,&
&ACLOC      ,SPCSIG     ,&
&WK         ,CG         ,&
&SPCDIR     ,NE         ,&
&NED        ,KGRPNT     ,&
&DEPXY      ,CROSS      )
   USE swan_angle_conversions, ONLY: DEGCNV, ANGRAD, ANGDEG
   USE swan_structured_output_interpolation, ONLY: SWOINA
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, EQREAL
   USE swan_wave_physics, ONLY: KSCIP1
   USE swan_action_interpolation, ONLY: SwanInterpolateAc
   USE swan_spectral_integration, ONLY: SwanIntgratSpc
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE OUTP_DATA
   USE SWPARTMD
   USE W3ODATMD, ONLY: WSCUT
   USE SwanIEM, ONLY: ntf, Ebig


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
!     30.70, 40.13: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     32.01: Roeland Ris & Cor van der Schelde
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     40.80: Marcel Zijlema
!     40.86: Nico Booij
!     40.87: Marcel Zijlema
!     41.62: Andre van der Westhuysen
!     41.72: Henrique Rapizo
!     41.85: Ad Reniers
!
!  1. Updates
!
!     10.09, Aug. 94: relative and absolute period distinguished
!                     NVOTP increased and type 28 added
!     10.10, Aug. 94: arrays ECOS, ESIN, NE and NED added to arg. list
!     10.22, Sep. 94: condition for tail changed from MSC.GE.3 to MSC.GT.3
!     20.59, Sep. 95: average wave number can be determined with
!                     other powers of k (i.e. OUTPAR(3))
!     20.61, Sep. 95: Tm02 and FWID added; computation of average period
!                     also changed
!     30.72, Oct. 97: logical function EQREAL introduced for floating point
!                     comparisons
!     32.01, Jan. 98: Nautical convention introduced (project h3268)
!     30.70, Feb. 98: ALCQ ignored if nautical direction is requested
!                     computation of kappa corrected (power of Sigma)
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     40.13, Aug. 01: provision for repeating grid (KREPTX>0)
!     40.30, May  03: introduction distributed-memory approach using MPI
!     40.41, Sep. 04: added Tm-10 and RTm-10
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 06: added Tp based on parabolic fitting
!     40.80, Sep. 07: extension to unstructured grids
!     40.86, Feb. 08: modification to prevent interpolation over an obstacle
!     40.87, Apr. 08: integration over [fmin,fmax] added
!     41.62, Nov. 15: included interface for computing wave partitions
!     41.72, Nov. 19: accommodate option for number of swells in output
!                     swell partitions starting always from second index
!     41.85, Feb. 19: implementation of IEM (surfbeat model)
!
!  2. Purpose
!
!     calculates quantities for which the spectral action density is
!     necessary
!
!  3. Method
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
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)

!     OQPROC  logic  input    processing of output quantities
!     MIP     Int    input    number of output points
!     XC, YC  real   input    comp. grid coordinates
!     VOQR    int a  input    location in VOQ of a certain outp quant.
!     VOQ     real a outp     values of output quantities
!     AC2     real a input    action densities
!     WK      real a local    wavenumber in output point
!     CG      real a local    group velocity in output point
!     NE      real a local    ratio of group and phase velocity
!     NED     real a local    derivative of NE with respect to depth
!     DEPXY   real a input    depth in points of computational grid
!
!     Local Variables
!
!     IVOTP   type indicators of output quantities processed by this subr.
!             used for assignment of exception values
!
!  8. Subroutines used
!
!     DEGCNV: Transforms dir. from nautical to cartesian or vice versa
!     ANGDEG: Transforms degrees to radians
!     SWOINA: interpolates 2D action density spectrum
!
!  9. Subroutines calling
!
!     OUTPUT (SWAN/OUTP)
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     For all output points do
!         interpolate action density to the output point
!         If processing of the quantity is requested
!         Then compute wave height, period etc.
!         ------------------------------------------------------------
!         If computation of Cg and K is necessary
!         Then get depth from array VOQ
!              Call KSCIP1 (computes Cg and K)
!         ------------------------------------------------------------
!         If processing of the quantity is requested
!         Then compute energy transport, wavelength etc.
!     ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, PARAMETER :: NVOTP = 94
   INTEGER    MIP
   REAL       XC(MIP)        ,YC(MIP)       ,AC2(MDC,MSC,MCGRD),&
   &VOQ(MIP,*)     ,&
   &WK(*)          ,&
   &CG(*)          ,ACLOC(MDC,MSC),&
   &NE(*)          ,NED(*)        ,DEPXY(MCGRD), ECS(MDC)
   LOGICAL    CROSS(4,MIP)

   INTEGER    VOQR(*), BKC, KGRPNT(MXC,MYC)
   INTEGER, PARAMETER :: IVOTP(NVOTP) = [&
   &10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 28, 32, 33,&
   &100, 101, 102, 103, 104, 105, 106, 107, 108, 109,&
   &110, 111, 112, 113, 114, 115, 116, 117, 118, 119,&
   &120, 121, 122, 123, 124, 125, 126, 127, 128, 129,&
   &130, 131, 132, 133, 134, 135, 136, 137, 138, 139,&
   &140, 141, 142, 143, 144, 145, 146, 147, 148, 149,&
   &150, 151, 152, 153, 154, 155, 156, 157, 158, 159,&
   &160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 171,&
   &42, 43, 44, 47, 48, 53, 58, 59, 71, 81]

   INTEGER, SAVE :: IENT = 0
   INTEGER    NP, DIMXPT, ITMP1
   INTEGER    ID, IDIRM, II, IP, IPT, IPTSW, IS, ISC, ISIGM, ITP, IVTYPE
   REAL       UABS           ,UDIR
   REAL       A, AHFR, APTAIL, APTOT, CETAIL, CEX, CEY, CGE
   REAL       CIA, CIAC, CIAS, CIB, CIBC, CIBS, CKTAIL
   REAL       COSFN1, COSFN2, COSFND, CTAIL, DEP, DEPLOC, DIRDEG
   REAL       DS, DSIG, DSSW, E1, E2, E3, EAD, EADD, ECTAIL, ECTOT
   REAL       ED, EDI, EEX, EEY, EFTAIL, EFTOT, EHFR, EKTOT
   REAL       EMAX, EMAXD, EMAXU, EPTAIL, EPTOT, ESTOT, ETAIL
   REAL       ETD, ETF, ETOT, FF, FMAX, FMIN, FND, FSWELL
   REAL       OMEG, OMEG1, OMEG1P, OMEG2, P, PPTAIL, Q, QP, R
   REAL       SIG, SIG1, SIG2, SIG2P, SIG3, SIGP
   REAL       SINFN1, SINFN2, SINFND, SKK, STPNS, SX, SY, T
   REAL       THETA, TM02, TPER, UXD, UXLOC, UYLOC, WLMEAN, XP, YP
   REAL, ALLOCATABLE :: XPT(:,:)

   REAL, ALLOCATABLE :: FLUX(:,:,:), FLOC(:,:)
   REAL, ALLOCATABLE :: EBLOC(:,:)

   LOGICAL    OQPROC(*)
   LOGICAL :: EXCPT, VALID_OUTPUT ! interpolation status for this point
   INTEGER    NOSWLL
   CALL STRACE (IENT, 'SWOEXA')

!     in case of transport of energy, compute the energy flux in all grid points
!     (energy flux will then be interpolated at output points!)

   IF (OQPROC(15).OR.OQPROC(19)) THEN
      IF(.NOT.ALLOCATED(FLUX)) ALLOCATE(FLUX(MDC,MSC,MCGRD))
      IF(.NOT.ALLOCATED(FLOC)) ALLOCATE(FLOC(MDC,MSC))
      DO IP=1, MCGRD
         DEPLOC = DEPXY(IP)
         CALL KSCIP1 (MSC, SPCSIG, DEPLOC, WK, CG, NE, NED)
         DO IS=1, MSC
            FLUX(:,IS,IP) = CG(IS)*AC2(:,IS,IP)
         ENDDO
      ENDDO
   ENDIF

!     in case of surfbeat, allocate help array for interpolation of bound infragravity energy

   IF(OQPROC(81).AND..NOT.ALLOCATED(EBLOC)) ALLOCATE(EBLOC(MDC,ntf))

!     loop over all output points

   do IP=1,MIP
      VALID_OUTPUT = .FALSE.
      point_output: BLOCK
      DEP = VOQ(IP,VOQR(4))

!       assign exception value if depth is negative or point is outside

      IF (DEP.LE.0.)                    EXIT point_output
      IF (EQREAL(DEP,OVEXCV(4)))        EXIT point_output
      IF (OPTG.NE.5) THEN
         IF (KREPTX.EQ.0) THEN
!            non-repeating grid
            IF (XC(IP) .LT. -0.01)            EXIT point_output
            IF (XC(IP) .GT. REAL(MXC-1)+0.01) EXIT point_output
         ENDIF
         IF (YC(IP) .LT. -0.01)            EXIT point_output
         IF (YC(IP) .GT. REAL(MYC-1)+0.01) EXIT point_output
      ENDIF

!       first the action density spectrum is interpolated

      IF (OPTG.NE.5) THEN
         CALL SWOINA (XC(IP), YC(IP), AC2, ACLOC, KGRPNT, DEPXY,&
         &CROSS(1,IP), EXCPT)
         IF (OQPROC(15).OR.OQPROC(19))&
         &CALL SWOINA (XC(IP), YC(IP), FLUX, FLOC, KGRPNT, DEPXY,&
         &CROSS(1,IP), EXCPT)
         IF (LSRFB.AND.OQPROC(81)) THEN
            ITMP1 = MSC
            MSC   = ntf
            CALL SWOINA (XC(IP), YC(IP), Ebig, EBLOC, KGRPNT, DEPXY,&
            &CROSS(1,IP), EXCPT)
            MSC = ITMP1
         ENDIF
      ELSE
         IF (.NOT.LCOMPGRD) THEN
            IF (.NOT.EQREAL(VOQ(IP,1),OVEXCV(1))) XP=VOQ(IP,1)-XOFFS
            IF (.NOT.EQREAL(VOQ(IP,2),OVEXCV(2))) YP=VOQ(IP,2)-YOFFS
            CALL SwanInterpolateAc ( ACLOC, XP, YP, AC2, EXCPT )
            IF (OQPROC(15).OR.OQPROC(19))&
            &CALL SwanInterpolateAc ( FLOC, XP, YP, FLUX, EXCPT )
         ELSE
            ACLOC(:,:) = AC2(:,:,IP)
            IF (OQPROC(15).OR.OQPROC(19)) FLOC(:,:) = FLUX(:,:,IP)
            EXCPT = .FALSE.
         ENDIF
      ENDIF
!       check variance on negativity in case of QCM
      IF (IQCM.NE.0) THEN
         ETOT = 0.
         DO IS = 1, MSC
            DO ID = 1, MDC
               ETOT = ETOT + SPCSIG(IS)**2 * ACLOC(ID,IS)
            ENDDO
         ENDDO
         IF (ETOT.LT.0.) EXCPT = .TRUE.
      ENDIF
      IF (EXCPT) EXIT point_output

!       coefficient for high frequency tail

      EFTAIL = 1. / (PWTAIL(1) - 1.)

!       significant wave height

      IVTYPE = 10
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(6).EQ.0.) THEN
!            integration over [0,inf]
            ETOT = 0.
!            trapezoidal rule is applied
            DO ID=1, MDC
               DO IS=2,MSC
                  DS=SPCSIG(IS)-SPCSIG(IS-1)
                  EAD = 0.5*(SPCSIG(IS)*ACLOC(ID,IS)+&
                  &SPCSIG(IS-1)*ACLOC(ID,IS-1))*DS*DDIR
                  ETOT = ETOT + EAD
               ENDDO
               IF (MSC .GT. 3) THEN
!                contribution of tail to total energy density
                  EHFR = ACLOC(ID,MSC) * SPCSIG(MSC)
                  ETOT = ETOT + DDIR * EHFR * SPCSIG(MSC) * EFTAIL
               ENDIF
            ENDDO
         ELSE
!            integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(21)
            FMAX = PI2*OUTPAR(36)
            ECS  = 1.
            ETOT = SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
         ENDIF
         IF (ETOT .GE. 0.) THEN
            VOQ(IP,VOQR(IVTYPE)) = 4.*SQRT(ETOT)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = 0.
         ENDIF
         IF (ITEST.GE.100) THEN
            WRITE(PRINTF, "(' SWOEXA: POINT ', I5, 2X, A, 1X, E12.4)") IP, OVSNAM(IVTYPE), VOQ(IP,VOQR(IVTYPE))
         ENDIF
      ENDIF

!       swell wave height

      IVTYPE = 44
      IF (OQPROC(IVTYPE)) THEN
         ETOT = 0.
         FSWELL = PI2 * OUTPAR(5)
!         trapezoidal rule is applied
         DO IS = 2, MSC
            DS = SPCSIG(IS)-SPCSIG(IS-1)
            IF (SPCSIG(IS).LE.FSWELL) THEN
               CIA = 0.5 * SPCSIG(IS-1) * DS * DDIR
               CIB = 0.5 * SPCSIG(IS  ) * DS * DDIR
            ELSE
               DSSW = FSWELL-SPCSIG(IS-1)
               CIB = 0.5 * FSWELL * DDIR * DSSW**2 / DS
               CIA = 0.5 * (SPCSIG(IS-1)+FSWELL) * DSSW * DDIR - CIB
            ENDIF
            DO ID = 1, MDC
               EAD = CIA * ACLOC(ID,IS-1) + CIB * ACLOC(ID,IS)
               ETOT = ETOT + EAD
            ENDDO
            IF (SPCSIG(IS).GT.FSWELL) EXIT
         ENDDO
         IF (ETOT .GE. 0.) THEN
            VOQ(IP,VOQR(IVTYPE)) = 4.*SQRT(ETOT)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = 0.
         ENDIF
         IF (ITEST.GE.100) THEN
            WRITE(PRINTF, "(' SWOEXA: POINT ', I5, 2X, A, 1X, E12.4)") IP, OVSNAM(IVTYPE), VOQ(IP,VOQR(IVTYPE))
         ENDIF
      ENDIF

!       average relative period                              modified 10.09

      IVTYPE = 28
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(14).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS) ** 2
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1) - 1.
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 2.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(29)
            FMAX = PI2*OUTPAR(44)
            ECS  = 1.
            APTOT=SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EPTOT=SwanIntgratSpc(1. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

      IVTYPE = 11
      IF (ICUR.EQ.0.AND.OQPROC(IVTYPE)) THEN
         IF (OUTPAR(7).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS) ** 2
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1) - 1.
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 2.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(22)
            FMAX = PI2*OUTPAR(37)
            ECS  = 1.
            APTOT=SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EPTOT=SwanIntgratSpc(1. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       average relative period                              modified 10.09

      IVTYPE = 43
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(18).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS) ** (OUTPAR(2)+1.)
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1) - OUTPAR(2)
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - OUTPAR(2) - 1.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(33)
            FMAX = PI2*OUTPAR(48)
            ECS  = 1.
            APTOT = SwanIntgratSpc(OUTPAR(2)-1., FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 1   )
            EPTOT = SwanIntgratSpc(OUTPAR(2)   , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 1   )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

      IVTYPE = 42
      IF (ICUR.EQ.0.AND.OQPROC(IVTYPE)) THEN
         IF (OUTPAR(17).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS) ** (OUTPAR(2)+1.)
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1) - OUTPAR(2)
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - OUTPAR(2) - 1.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(32)
            FMAX = PI2*OUTPAR(47)
            ECS  = 1.
            APTOT = SwanIntgratSpc(OUTPAR(2)-1., FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 1   )
            EPTOT = SwanIntgratSpc(OUTPAR(2)   , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 1   )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       peak period

      IVTYPE = 12
      IF (OQPROC(IVTYPE)) THEN
         EMAX = 0.
         ISIGM = -1
         DO IS = 1, MSC
            ETD = 0.
            DO ID = 1, MDC
               ETD = ETD + SPCSIG(IS)*ACLOC(ID,IS)*DDIR
            ENDDO
            IF (ETD.GT.EMAX) THEN
               EMAX  = ETD
               ISIGM = IS
            ENDIF
         ENDDO
         IF (ISIGM.GT.0) THEN
            VOQ(IP,VOQR(IVTYPE)) = 2.*PI/SPCSIG(ISIGM)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       peak period based on parabolic fitting

      IVTYPE = 53
      IF (OQPROC(IVTYPE)) THEN
         EMAX = 0.
         ETD  = 0.
         ISIGM = -1
         DO IS = 1, MSC
            ED  = ETD
            ETD = 0.
            DO ID = 1, MDC
               ETD = ETD + SPCSIG(IS)*ACLOC(ID,IS)*DDIR
            END DO
            IF (ETD.GT.EMAX) THEN
               EMAX  = ETD
               ISIGM = IS
               EMAXD = ED
               EMAXU = 0.
               IF (IS.LT.MSC) THEN
                  DO ID = 1, MDC
                     EMAXU = EMAXU + SPCSIG(IS+1)*ACLOC(ID,IS+1)*DDIR
                  END DO
               ELSE
                  EMAXU = EMAX
               END IF
            END IF
         END DO
         IF (ISIGM.GT.1 .AND. ISIGM.LT.MSC) THEN
            SIG1 = SPCSIG(ISIGM-1)
            SIG2 = SPCSIG(ISIGM+1)
            SIG3 = SPCSIG(ISIGM  )
            E1   = EMAXD
            E2   = EMAXU
            E3   = EMAX
            P    = SIG1+SIG2
            Q    = (E1-E2)/(SIG1-SIG2)
            R    = SIG1+SIG3
            T    = (E1-E3)/(SIG1-SIG3)
            A    = (T-Q)/(R-P)
            IF (A.LT.0) THEN
               SIGP = (-Q+P*A)/(2.*A)
            ELSE
               SIGP = SIG3
            END IF
            VOQ(IP,VOQR(IVTYPE)) = 2.*PI/SIGP
         ELSE IF (ISIGM.EQ.1) THEN
            VOQ(IP,VOQR(IVTYPE)) = 2.*PI/SPCSIG(1)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         END IF
      END IF

!       peak direction

      IVTYPE = 14
      IF (OQPROC(IVTYPE)) THEN
         EMAX = 0.
         IDIRM = -1
         DO ID = 1, MDC
            ETF = 0.
            DO IS = 2, MSC
               DS = SPCSIG(IS)-SPCSIG(IS-1)
               E1 = SPCSIG(IS-1)*ACLOC(ID,IS-1)
               E2 = SPCSIG(IS)*ACLOC(ID,IS)
               ETF = ETF + DS * (E1+E2)
            ENDDO
            IF (ETF.GT.EMAX) THEN
               EMAX  = ETF
               IDIRM = ID
            ENDIF
         ENDDO
         IF (IDIRM.GT.0) THEN

!            *** Convert (if necessary) from nautical degrees ***
!            *** to cartesian degrees                         ***

            IF (BNAUT) THEN
               VOQ(IP,VOQR(IVTYPE)) = ANGDEG( SPCDIR(IDIRM,1) )
            ELSE
               VOQ(IP,VOQR(IVTYPE)) = ANGDEG( (ALCQ + SPCDIR(IDIRM,1)) )
            ENDIF
            VOQ(IP,VOQR(IVTYPE)) = DEGCNV( VOQ(IP,VOQR(IVTYPE)) )

         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       mean direction

      IVTYPE = 13
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(8).EQ.0.) THEN
!             integration over [0,inf]
            ETOT = 0.
            EEX  = 0.
            EEY  = 0.
            DO ID=1, MDC
               EAD = 0.
               DO IS=2,MSC
                  DS=SPCSIG(IS)-SPCSIG(IS-1)
                  EDI = 0.5*(SPCSIG(IS)*ACLOC(ID,IS)+&
                  &SPCSIG(IS-1)*ACLOC(ID,IS-1))*DS
                  EAD = EAD + EDI
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EHFR = ACLOC(ID,MSC) * SPCSIG(MSC)
                  EAD = EAD + EHFR * SPCSIG(MSC) * EFTAIL
               ENDIF
               EAD = EAD * DDIR
               ETOT = ETOT + EAD
               EEX  = EEX + EAD * SPCDIR(ID,2)
               EEY  = EEY + EAD * SPCDIR(ID,3)
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(23)
            FMAX = PI2*OUTPAR(38)
            ECS  = 1.
            ETOT= SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EEX = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, SPCDIR(1,2), 0., 0., ACLOC, 1)
            EEY = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, SPCDIR(1,3), 0., 0., ACLOC, 1)
         ENDIF
         IF (ETOT.GT.0.) THEN
            IF (BNAUT) THEN
               DIRDEG = ATAN2(EEY,EEX) * 180./PI
            ELSE
               DIRDEG = (ALCQ + ATAN2(EEY,EEX)) * 180./PI
            ENDIF
            IF (DIRDEG.LT.0.) DIRDEG = DIRDEG + 360.

!             *** Convert (if necessary) from nautical degrees ***
!             *** to cartesian degrees                         ***

            VOQ(IP,VOQR(IVTYPE)) = DEGCNV( DIRDEG )

         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       directional spread

      IVTYPE = 16
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(10).EQ.0.) THEN
!             integration over [0,inf]
            ETOT = 0.
            EEX  = 0.
            EEY  = 0.
            DO ID=1, MDC
               EAD = 0.
               DO IS=2,MSC
                  DS=SPCSIG(IS)-SPCSIG(IS-1)
                  EDI = 0.5*(SPCSIG(IS)*ACLOC(ID,IS)+&
                  &SPCSIG(IS-1)*ACLOC(ID,IS-1))*DS
                  EAD = EAD + EDI
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EHFR = ACLOC(ID,MSC) * SPCSIG(MSC)
                  EAD = EAD + EHFR * SPCSIG(MSC) * EFTAIL
               ENDIF
               EAD = EAD * DDIR
               ETOT = ETOT + EAD
               EEX  = EEX + EAD * SPCDIR(ID,2)
               EEY  = EEY + EAD * SPCDIR(ID,3)
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(25)
            FMAX = PI2*OUTPAR(40)
            ECS  = 1.
            ETOT= SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EEX = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, SPCDIR(1,2), 0., 0., ACLOC, 1)
            EEY = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, SPCDIR(1,3), 0., 0., ACLOC, 1)
         ENDIF
         IF (ETOT.GT.0.) THEN
            FF = MIN (1., SQRT(EEX*EEX+EEY*EEY)/ETOT)
            VOQ(IP,VOQR(IVTYPE)) = SQRT(2.-2.*FF) *180./PI
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       average relative period (Tm-10)

      IVTYPE = 48
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(20).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS)
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1)
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 1.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(35)
            FMAX = PI2*OUTPAR(50)
            ECS  = 1.
            APTOT=SwanIntgratSpc(-1., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EPTOT=SwanIntgratSpc( 0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

      IVTYPE = 47
      IF (ICUR.EQ.0.AND.OQPROC(IVTYPE)) THEN
         IF (OUTPAR(19).EQ.0.) THEN
!             integration over [0,inf]
            APTOT = 0.
            EPTOT = 0.
            DO ID=1, MDC
               DO IS=1,MSC
                  SIG2P = SPCSIG(IS)
                  APTOT = APTOT + SIG2P * ACLOC(ID,IS)
                  EPTOT = EPTOT + SPCSIG(IS) * SIG2P * ACLOC(ID,IS)
               ENDDO
            ENDDO
            APTOT = APTOT * FRINTF
            EPTOT = EPTOT * FRINTF
            IF (MSC .GT. 3) THEN
               PPTAIL = PWTAIL(1)
               APTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 1.
               EPTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               DO ID = 1, MDC
!                  contribution of tail to total energy density
                  AHFR = SIG2P * ACLOC(ID,MSC)
                  APTOT = APTOT + APTAIL * AHFR
                  EHFR = SPCSIG(MSC) * AHFR
                  EPTOT = EPTOT + EPTAIL * EHFR
               ENDDO
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(34)
            FMAX = PI2*OUTPAR(49)
            ECS  = 1.
            APTOT=SwanIntgratSpc(-1., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
            EPTOT=SwanIntgratSpc( 0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , 0.  , 0.    , ACLOC      ,&
            &1  )
         ENDIF
         IF (EPTOT.GT.0.) THEN
            TPER = 2.*PI * APTOT / EPTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       peakedness (Qp)

      IVTYPE = 58
      IF (OQPROC(IVTYPE)) THEN
         ETOT  = 0.
         ESTOT = 0.
         DO ID=1, MDC
            DO IS = 1, MSC
               SIG   = SPCSIG(IS)
               EADD  = SIG**2 * ACLOC(ID,IS) * FRINTF * DDIR
               ETOT  = ETOT  + EADD
            ENDDO
         ENDDO
         DO IS = 1, MSC
            SIG = SPCSIG(IS)
            EADD = 0.
            DO ID=1, MDC
               EADD = EADD + SIG * ACLOC(ID,IS) * DDIR
            ENDDO
            ESTOT = ESTOT + SIG**2 * EADD**2 * FRINTF
         ENDDO
         IF (ETOT.GT.0.) THEN
            VOQ(IP,VOQR(IVTYPE)) = 2.*ESTOT/(ETOT * ETOT)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       bound infragravity wave height

      IVTYPE = 81
      IF (OQPROC(IVTYPE)) THEN
!          integration over infragravity frequencies
         ETOT = 0.
         DO ID=1, MDC
            DO IS=1, ntf
               ETOT = ETOT + EBLOC(ID,IS)
            ENDDO
         ENDDO
         IF (ETOT .GE. 0.) THEN
            VOQ(IP,VOQR(IVTYPE)) = 4.*SQRT(ETOT)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = 0.
         ENDIF
         IF (ITEST.GE.100) THEN
            WRITE(PRINTF, "(' SWOEXA: POINT ', I5, 2X, A, 1X, E12.4)") IP, OVSNAM(IVTYPE), VOQ(IP,VOQR(IVTYPE))
         ENDIF
      ENDIF

      IF (BKC.EQ.1) THEN
         VALID_OUTPUT = .TRUE.
         EXIT point_output
      END IF

!       compute k and Cg

      DEPLOC = VOQ(IP,VOQR(4))
      CALL KSCIP1 (MSC, SPCSIG, DEPLOC, WK, CG, NE, NED)
      IF (ITEST.GE.100 .OR. IOUTES .GE. 20) THEN
         WRITE (PRTEST, *) ' Depth: ', DEPLOC
         do ISC = 1, MIN(MSC,20)
            WRITE (PRTEST, "(' i, SPCSIG, sigma, k, cg ', I2, 3(1X, E12.4))") ISC, SPCSIG(ISC), WK(ISC), CG(ISC)
         end do
      ENDIF

!       transport direction

      IVTYPE = 15
      IF (OQPROC(IVTYPE)) THEN
         IF (ICUR.EQ.0) THEN
            UXLOC = 0.
            UYLOC = 0.
         ELSE
            UXLOC = VOQ(IP,VOQR(5))
            UYLOC = VOQ(IP,VOQR(5)+1)
         ENDIF
         IF (OUTPAR(9).EQ.0) THEN
!             integration over [0,inf]
            CEX = 0.
            CEY = 0.
            ETOT = 0.
            DO ISIGM = 1, MSC
               IF (ISIGM.EQ.1) THEN
                  DSIG = 0.5 * (SPCSIG(2) - SPCSIG(1))
               ELSE IF (ISIGM.EQ.MSC) THEN
                  DSIG = 0.5 * (SPCSIG(MSC) - SPCSIG(MSC-1))
               ELSE
                  DSIG = 0.5 * (SPCSIG(ISIGM+1) - SPCSIG(ISIGM-1))
               ENDIF
               SIG2 = SPCSIG(ISIGM)
               DO ID=1,MDC
                  CGE = DSIG * SIG2 * FLOC(ID,ISIGM)
                  CEX = CEX + CGE * SPCDIR(ID,2)
                  CEY = CEY + CGE * SPCDIR(ID,3)
                  IF (ICUR.EQ.1) THEN
                     ETOT = ETOT + DSIG * SIG2 * ACLOC(ID,ISIGM)
                  ENDIF
               ENDDO
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(24)
            FMAX = PI2*OUTPAR(39)
            ECS  = 1.
            IF (ICUR.EQ.1)&
            &ETOT=SwanIntgratSpc(0.,FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, ECS, 0., 0., ACLOC, 1)
            CEX = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &CG, SPCDIR(1,2), 0., 0., FLOC, 1)
            CEY = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &CG, SPCDIR(1,3), 0., 0., FLOC, 1)
         ENDIF

         IF (ICUR.EQ.1) THEN
            CEX = CEX + ETOT * UXLOC
            CEY = CEY + ETOT * UYLOC
         ENDIF

         IF (OQPROC(IVTYPE)) THEN
            IF (CEX.EQ.0. .AND. CEY.EQ.0.) THEN
               VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            ELSE
               IF (BNAUT) THEN
                  DIRDEG = ATAN2(CEY,CEX) * 180./PI
               ELSE
                  DIRDEG = (ALCQ + ATAN2(CEY,CEX)) * 180./PI
               ENDIF
               IF (DIRDEG.LT.0.) DIRDEG = DIRDEG + 360.

!               *** Convert (if necessary) from nautical degrees ***
!               *** to cartesian degrees                         ***

               VOQ(IP,VOQR(IVTYPE)) = DEGCNV( DIRDEG )

            ENDIF
         ENDIF
      ENDIF

!       transport vector

      IVTYPE = 19
      IF (OQPROC(IVTYPE)) THEN
         IF (ICUR.EQ.0) THEN
            UXLOC = 0.
            UYLOC = 0.
         ELSE
            UXLOC = VOQ(IP,VOQR(5))
            UYLOC = VOQ(IP,VOQR(5)+1)
         ENDIF
         IF (OUTPAR(13).EQ.0) THEN
!             integration over [0,inf]
            CEX = 0.
            CEY = 0.
            ETOT = 0.
            DO ISIGM = 1, MSC
               IF (ISIGM.EQ.1) THEN
                  DSIG = 0.5 * (SPCSIG(2) - SPCSIG(1))
               ELSE IF (ISIGM.EQ.MSC) THEN
                  DSIG = 0.5 * (SPCSIG(MSC) - SPCSIG(MSC-1))
               ELSE
                  DSIG = 0.5 * (SPCSIG(ISIGM+1) - SPCSIG(ISIGM-1))
               ENDIF
               SIG2 = SPCSIG(ISIGM)
               DO ID=1,MDC
                  CGE = DSIG * SIG2 * FLOC(ID,ISIGM)
                  CEX = CEX + CGE * SPCDIR(ID,2)
                  CEY = CEY + CGE * SPCDIR(ID,3)
                  IF (ICUR.EQ.1) THEN
                     ETOT = ETOT + DSIG * SIG2 * ACLOC(ID,ISIGM)
                  ENDIF
               ENDDO
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(28)
            FMAX = PI2*OUTPAR(43)
            ECS  = 1.
            IF (ICUR.EQ.1)&
            &ETOT=SwanIntgratSpc(0.,FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK, ECS, 0., 0., ACLOC, 1)
            CEX = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &CG, SPCDIR(1,2), 0., 0., FLOC, 1)
            CEY = SwanIntgratSpc(0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &CG, SPCDIR(1,3), 0., 0., FLOC, 1)
            CEX = CEX/DDIR
            CEY = CEY/DDIR
         ENDIF

         IF (ICUR.EQ.1) THEN
            CEX = CEX + ETOT * UXLOC
            CEY = CEY + ETOT * UYLOC
         ENDIF

         SX = CEX * DDIR
         SY = CEY * DDIR
         IF (INRHOG.EQ.1) THEN
            SX = SX * RHO * GRAV
            SY = SY * RHO * GRAV
         ENDIF
         VOQ(IP,VOQR(IVTYPE))   = COSCQ*SX - SINCQ*SY
         VOQ(IP,VOQR(IVTYPE)+1) = SINCQ*SX + COSCQ*SY
      ENDIF

!       average wave length

      IVTYPE = 17
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(11).EQ.0) THEN
!             integration over [0,inf]
            ETOT  = 0.
            EKTOT = 0.
!             new integration method involving FRINTF
            DO IS=1, MSC
               SIG2 = (SPCSIG(IS))**2
               SKK  = SIG2 * (WK(IS))**OUTPAR(3)
               DO ID=1,MDC
                  ETOT  = ETOT + SIG2 * ACLOC(ID,IS)
                  EKTOT = EKTOT + SKK * ACLOC(ID,IS)
               ENDDO
            ENDDO
            ETOT  = FRINTF * ETOT
            EKTOT = FRINTF * EKTOT
            IF (MSC .GT. 3) THEN
!                contribution of tail to total energy density
               PPTAIL = PWTAIL(1) - 1.
               CETAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 1. - 2.*OUTPAR(3)
               IF (PPTAIL.LE.0.) THEN
                  CALL MSGERR (2,'error tail computation')
               ELSE
                  CKTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
                  DO ID=1,MDC
                     ETOT  = ETOT + CETAIL * SIG2 * ACLOC(ID,MSC)
                     EKTOT = EKTOT + CKTAIL * SKK * ACLOC(ID,MSC)
                  ENDDO
               ENDIF
            ENDIF
            IF (EKTOT.GT.0.) THEN
               WLMEAN = PI2 * (ETOT / EKTOT) ** (1./OUTPAR(3))
               VOQ(IP,VOQR(IVTYPE)) = WLMEAN
            ELSE
               VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(26)
            FMAX = PI2*OUTPAR(41)
            ECS  = 1.
            ETOT  = SwanIntgratSpc(OUTPAR(3)-1., FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 3   )
            EKTOT = SwanIntgratSpc(OUTPAR(3)   , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 3   )
            IF (EKTOT.GT.0.) THEN
               WLMEAN = PI2 * ETOT / EKTOT
               VOQ(IP,VOQR(IVTYPE)) = WLMEAN
            ELSE
               VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            ENDIF
         ENDIF
      ENDIF

!       steepness

      IVTYPE = 18
      IF (OQPROC(IVTYPE)) THEN
         IF (OUTPAR(12).EQ.0) THEN
!             integration over [0,inf]
            ETOT  = 0.
            EKTOT = 0.
!             new integration method involving FRINTF
            DO IS=1, MSC
               SIG2 = (SPCSIG(IS))**2
               SKK  = SIG2 * (WK(IS))**OUTPAR(3)
               DO ID=1,MDC
                  ETOT  = ETOT + SIG2 * ACLOC(ID,IS)
                  EKTOT = EKTOT + SKK * ACLOC(ID,IS)
               ENDDO
            ENDDO
            ETOT  = FRINTF * ETOT
            EKTOT = FRINTF * EKTOT
            IF (MSC .GT. 3) THEN
!                contribution of tail to total energy density
               PPTAIL = PWTAIL(1) - 1.
               CETAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
               PPTAIL = PWTAIL(1) - 1. - 2.*OUTPAR(3)
               IF (PPTAIL.LE.0.) THEN
                  CALL MSGERR (2,'error tail computation')
               ELSE
                  CKTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
                  DO ID=1,MDC
                     ETOT  = ETOT + CETAIL * SIG2 * ACLOC(ID,MSC)
                     EKTOT = EKTOT + CKTAIL * SKK * ACLOC(ID,MSC)
                  ENDDO
               ENDIF
            ENDIF
            IF (EKTOT.GT.0.) THEN
               WLMEAN = PI2 * (ETOT / EKTOT) ** (1./OUTPAR(3))
               VOQ(IP,VOQR(IVTYPE)) = 4.* SQRT(ETOT*DDIR) / WLMEAN
            ELSE
               VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            ENDIF
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(27)
            FMAX = PI2*OUTPAR(42)
            ECS  = 1.
            ETOT  = SwanIntgratSpc(0.          , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 1   )
            EPTOT = SwanIntgratSpc(OUTPAR(3)-1., FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 3   )
            EKTOT = SwanIntgratSpc(OUTPAR(3)   , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , 0.    ,&
            &0.          , ACLOC, 3   )
            IF (EKTOT.GT.0.) THEN
               WLMEAN = PI2 * EPTOT / EKTOT
               VOQ(IP,VOQR(IVTYPE)) = 4.* SQRT(ETOT) / WLMEAN
            ELSE
               VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            ENDIF
         ENDIF
      ENDIF

!       average absolute period Tm01

      IVTYPE = 11
      IF (ICUR.GT.0 .AND. OQPROC(IVTYPE)) THEN
         UXLOC = VOQ(IP,VOQR(5))
         UYLOC = VOQ(IP,VOQR(5)+1)
         IF (OUTPAR(7).EQ.0) THEN
!             integration over [0,inf]
            ETOT = 0.
            EFTOT = 0.
            PPTAIL = PWTAIL(1) - 1.
            ETAIL  = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            PPTAIL = PWTAIL(1) - 2.
            EFTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            DO ID=1, MDC
               THETA = SPCDIR(ID,1) + ALCQ
               UXD = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               DO IS = 1, MSC
                  OMEG = SPCSIG(IS) + WK(IS) * UXD
                  EADD = FRINTF * SPCSIG(IS)**2 * ACLOC(ID,IS)
                  ETOT = ETOT + EADD
                  EFTOT = EFTOT + EADD * OMEG
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EADD = SPCSIG(MSC)**2 * ACLOC(ID,MSC)
                  ETOT = ETOT + ETAIL * EADD
                  EFTOT = EFTOT + EFTAIL * OMEG * EADD
               ENDIF
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(22)
            FMAX = PI2*OUTPAR(37)
            ECS  = 1.
            ETOT =SwanIntgratSpc(0. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , UXLOC, UYLOC, ACLOC      ,&
            &2  )
            EFTOT=SwanIntgratSpc(1. , FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , UXLOC, UYLOC, ACLOC      ,&
            &2  )
         ENDIF
         IF (EFTOT.GT.0.) THEN
            TPER = 2.*PI * ETOT / EFTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       average absolute period (case with current)

      IVTYPE = 42
      IF (ICUR.GT.0 .AND. OQPROC(IVTYPE)) THEN
         UXLOC = VOQ(IP,VOQR(5))
         UYLOC = VOQ(IP,VOQR(5)+1)
         IF (OUTPAR(17).EQ.0) THEN
!             integration over [0,inf]
            ETOT = 0.
            EFTOT = 0.
            PPTAIL = PWTAIL(1) - OUTPAR(2)
            ETAIL  = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            PPTAIL = PWTAIL(1) - OUTPAR(2) - 1.
            EFTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            DO ID=1, MDC
               THETA = SPCDIR(ID,1) + ALCQ
               UXD = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               DO IS = 1, MSC
                  OMEG = SPCSIG(IS) + WK(IS) * UXD
                  OMEG1P = OMEG ** (OUTPAR(2)-1.)
                  EADD = OMEG1P * FRINTF * SPCSIG(IS)**2 * ACLOC(ID,IS)
                  ETOT = ETOT + EADD
                  EFTOT = EFTOT + EADD * OMEG
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EADD = OMEG1P * SPCSIG(MSC)**2 * ACLOC(ID,MSC)
                  ETOT = ETOT + ETAIL * EADD
                  EFTOT = EFTOT + EFTAIL * OMEG * EADD
               ENDIF
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(32)
            FMAX = PI2*OUTPAR(47)
            ECS  = 1.
            ETOT  = SwanIntgratSpc(OUTPAR(2)-1., FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , UXLOC ,&
            &UYLOC       , ACLOC, 2   )
            EFTOT = SwanIntgratSpc(OUTPAR(2)   , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , UXLOC ,&
            &UYLOC       , ACLOC, 2   )
         ENDIF
         IF (EFTOT.GT.0.) THEN
            TPER = 2.*PI * ETOT / EFTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       zero-crossing period Tm02

      IVTYPE = 32
      IF (OQPROC(IVTYPE)) THEN
         IF (ICUR.GT.0) THEN
            UXLOC = VOQ(IP,VOQR(5))
            UYLOC = VOQ(IP,VOQR(5)+1)
         ENDIF
         IF (OUTPAR(15).EQ.0) THEN
!             integration over [0,inf]
            ETOT  = 0.
            EFTOT = 0.
            PPTAIL = PWTAIL(1) - 1.
            ETAIL  = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            PPTAIL = PWTAIL(1) - 3.
            EFTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            DO ID=1, MDC
               IF (ICUR.GT.0) THEN
                  THETA = SPCDIR(ID,1) + ALCQ
                  UXD   = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               ENDIF
               DO IS=1,MSC
                  EADD  = SPCSIG(IS)**2 * ACLOC(ID,IS) * FRINTF
                  IF (ICUR.GT.0) THEN
                     OMEG  = SPCSIG(IS) + WK(IS) * UXD
                     OMEG2 = OMEG**2
                  ELSE
                     OMEG2 = SPCSIG(IS)**2
                  ENDIF
                  ETOT  = ETOT + EADD
                  EFTOT = EFTOT + EADD * OMEG2
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EADD  = SPCSIG(MSC)**2 * ACLOC(ID,MSC)
                  ETOT  = ETOT  + ETAIL * EADD
                  EFTOT = EFTOT + EFTAIL * OMEG2 * EADD
               ENDIF
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(30)
            FMAX = PI2*OUTPAR(45)
            ECS  = 1.
            IF (ICUR.GT.0) THEN
               ITP = 2
            ELSE
               ITP = 1
            ENDIF
            ETOT  = SwanIntgratSpc(0.          , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , UXLOC ,&
            &UYLOC       , ACLOC, ITP )
            EFTOT = SwanIntgratSpc(2.          , FMIN , FMAX, SPCSIG,&
            &SPCDIR(1,1) , WK   , ECS , UXLOC ,&
            &UYLOC       , ACLOC, ITP )
         ENDIF
         IF (EFTOT.GT.0.) THEN
            VOQ(IP,VOQR(IVTYPE)) = 2.*PI * SQRT(ETOT/EFTOT)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       frequency spectral width (kappa)

      IVTYPE = 33
      IF (OQPROC(IVTYPE)) THEN
         TM02 = VOQ(IP,VOQR(32))
         IF (ICUR.GT.0) THEN
            UXLOC = VOQ(IP,VOQR(5))
            UYLOC = VOQ(IP,VOQR(5)+1)
         ENDIF
         ETOT  = 0.
         ECTOT = 0.
         ESTOT = 0.
         IF (OUTPAR(16).EQ.0) THEN
!             integration over [0,inf]
            PPTAIL = PWTAIL(1) - 1.
            ECTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            DO  ID=1, MDC
               IF (ICUR.GT.0) THEN
                  THETA = SPCDIR(ID,1) + ALCQ
                  UXD   = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               ENDIF
               DO  IS = 1, MSC
                  SIG = SPCSIG(IS)
                  IF (ICUR.GT.0) THEN
                     OMEG = SIG + WK(IS) * UXD
                  ELSE
                     OMEG = SIG
                  ENDIF
                  FND = OMEG * TM02
                  COSFND = COS(FND)
                  SINFND = SIN(FND)
                  EADD   = SIG**2 * ACLOC(ID,IS) * FRINTF
                  ETOT  = ETOT  + EADD
                  ECTOT = ECTOT + COSFND * EADD
                  ESTOT = ESTOT + SINFND * EADD
               ENDDO
               IF (MSC .GT. 3) THEN
!                 contribution of tail to total energy density
                  EADD  = ECTAIL * SIG**2 * ACLOC(ID,MSC)
                  ETOT  = ETOT  + EADD
                  ECTOT = ECTOT + COSFND * EADD
                  ESTOT = ESTOT + SINFND * EADD
               ENDIF
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(31)
            FMAX = PI2*OUTPAR(46)
            DO ID = 1, MDC
               IF (ICUR.GT.0) THEN
                  THETA = SPCDIR(ID,1) + ALCQ
                  UXD = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               ENDIF
               DO IS = 2, MSC
                  SIG1 = SPCSIG(IS-1)
                  SIG2 = SPCSIG(IS  )
                  IF (ICUR.GT.0) THEN
                     OMEG1 = SIG1 + WK(IS-1) * UXD
                     OMEG2 = SIG2 + WK(IS  ) * UXD
                  ELSE
                     OMEG1 = SIG1
                     OMEG2 = SIG2
                  ENDIF
                  COSFN1 = COS(OMEG1 * TM02)
                  SINFN1 = SIN(OMEG1 * TM02)
                  COSFN2 = COS(OMEG2 * TM02)
                  SINFN2 = SIN(OMEG2 * TM02)
                  DS = SIG2 - SIG1
                  IF ( SIG1.GE.FMIN .AND. SIG2.LE.FMAX ) THEN
                     CIA  = 0.5*SIG1 * DS
                     CIB  = 0.5*SIG2 * DS
                     CIAC = 0.5*COSFN1 * SIG1 * DS
                     CIBC = 0.5*COSFN2 * SIG2 * DS
                     CIAS = 0.5*SINFN1 * SIG1 * DS
                     CIBS = 0.5*SINFN2 * SIG2 * DS
                  ELSEIF ( SIG2.GT.FMAX ) THEN
                     DSSW = FMAX - SIG1
                     IF (ICUR.GT.0) THEN
                        OMEG2=FMAX+(WK(IS)*DSSW+WK(IS-1)*(DS-DSSW))*UXD/DS
                     ELSE
                        OMEG2=FMAX
                     ENDIF
                     COSFN2 = COS(OMEG2 * TM02)
                     SINFN2 = SIN(OMEG2 * TM02)
                     CIBC   = 0.5*COSFN2 * FMAX * DSSW**2 / DS
                     CIAC   = 0.5*(COSFN1*SIG1 + COSFN2*FMAX)*DSSW - CIBC
                     CIBS   = 0.5*SINFN2 * FMAX * DSSW**2 / DS
                     CIAS   = 0.5*(SINFN1*SIG1 + SINFN2*FMAX)*DSSW - CIBS
                     CIB    = 0.5*FMAX * DSSW**2 / DS
                     CIA    = 0.5*(SIG1 + FMAX)*DSSW - CIB
                  ELSEIF ( SIG2.GT.FMIN ) THEN
                     DSSW = SIG2 - FMIN
                     IF (ICUR.GT.0) THEN
                        OMEG1=FMIN+(WK(IS-1)*DSSW+WK(IS)*(DS-DSSW))*UXD/DS
                     ELSE
                        OMEG1=FMIN
                     ENDIF
                     COSFN1 = COS(OMEG1 * TM02)
                     SINFN1 = SIN(OMEG1 * TM02)
                     CIAC   = 0.5*COSFN1 * FMIN * DSSW**2 / DS
                     CIBC   = 0.5*(COSFN2*SIG2 + COSFN1*FMIN)*DSSW - CIAC
                     CIAS   = 0.5*SINFN1 * FMIN * DSSW**2 / DS
                     CIBS   = 0.5*(SINFN2*SIG2 + SINFN1*FMIN)*DSSW - CIAS
                     CIA    = 0.5*FMIN * DSSW**2 / DS
                     CIB    = 0.5*(SIG2 + FMIN)*DSSW - CIA
                  ELSE
                     CIA  = 0.
                     CIB  = 0.
                     CIAC = 0.
                     CIBC = 0.
                     CIAS = 0.
                     CIBS = 0.
                  ENDIF
                  ETOT = ETOT  + CIA *ACLOC(ID,IS-1) + CIB *ACLOC(ID,IS)
                  ECTOT= ECTOT + CIAC*ACLOC(ID,IS-1) + CIBC*ACLOC(ID,IS)
                  ESTOT= ESTOT + CIAS*ACLOC(ID,IS-1) + CIBS*ACLOC(ID,IS)
                  IF ( SIG2.GT.FMAX ) EXIT
               ENDDO
            ENDDO
!             --- add tail contribution, if appropriate
            IF ( FMAX.GT.SPCSIG(MSC) ) THEN
               IF ( MSC.GT.3 ) THEN
                  ECTAIL = 1. / (PWTAIL(1) - 1.)
                  IF ( FMAX.GT.100. ) THEN
                     CTAIL = 0.
                  ELSE
                     CTAIL = FMAX * (SPCSIG(MSC)/FMAX)**PWTAIL(1)
                  ENDIF
                  DO ID = 1, MDC
                     IF (ICUR.GT.0) THEN
                        THETA = SPCDIR(ID,1) + ALCQ
                        UXD   = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
                        OMEG2 = SPCSIG(MSC) + WK(MSC) * UXD
                     ELSE
                        OMEG2 = SPCSIG(MSC)
                     ENDIF
                     COSFN2 = COS(OMEG2 * TM02)
                     SINFN2 = SIN(OMEG2 * TM02)
                     EHFR = ACLOC(ID,MSC) * SPCSIG(MSC)
                     ETOT  = ETOT  + EHFR*(SPCSIG(MSC)-CTAIL) * ECTAIL
                     ECTOT = ECTOT + EHFR*(COSFN2*SPCSIG(MSC) -&
                     &COS(FMAX*TM02)*CTAIL)*ECTAIL
                     ESTOT = ESTOT + EHFR*(SINFN2*SPCSIG(MSC) -&
                     &SIN(FMAX*TM02)*CTAIL)*ECTAIL
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
         IF (ETOT.GT.0.) THEN
            VOQ(IP,VOQR(IVTYPE)) =&
            &SQRT(ECTOT*ECTOT+ESTOT*ESTOT) / ETOT
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       average absolute period Tm-10 (case with current)

      IVTYPE = 47
      IF (ICUR.GT.0 .AND. OQPROC(IVTYPE)) THEN
         UXLOC = VOQ(IP,VOQR(5))
         UYLOC = VOQ(IP,VOQR(5)+1)
         IF (OUTPAR(19).EQ.0.) THEN
!             integration over [0,inf]
            ETOT = 0.
            EFTOT = 0.
            PPTAIL = PWTAIL(1)
            ETAIL  = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            PPTAIL = PWTAIL(1) - 1.
            EFTAIL = 1. / (PPTAIL * (1. + PPTAIL * (FRINTH-1.)))
            DO ID=1, MDC
               THETA = SPCDIR(ID,1) + ALCQ
               UXD = UXLOC*COS(THETA) + UYLOC*SIN(THETA)
               DO IS = 1, MSC
                  OMEG = SPCSIG(IS) + WK(IS) * UXD
                  OMEG1P = OMEG ** (-1.)
                  EADD = OMEG1P * FRINTF * SPCSIG(IS)**2 * ACLOC(ID,IS)
                  ETOT = ETOT + EADD
                  EFTOT = EFTOT + EADD * OMEG
               ENDDO
               IF (MSC .GT. 3) THEN
!                  contribution of tail to total energy density
                  EADD = OMEG1P * SPCSIG(MSC)**2 * ACLOC(ID,MSC)
                  ETOT = ETOT + ETAIL * EADD
                  EFTOT = EFTOT + EFTAIL * OMEG * EADD
               ENDIF
            ENDDO
         ELSE
!             integration over [fmin,fmax]
            FMIN = PI2*OUTPAR(34)
            FMAX = PI2*OUTPAR(49)
            ECS  = 1.
            ETOT =SwanIntgratSpc(-1., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , UXLOC, UYLOC, ACLOC      ,&
            &2  )
            EFTOT=SwanIntgratSpc( 0., FMIN, FMAX, SPCSIG, SPCDIR(1,1),&
            &WK , ECS , UXLOC, UYLOC, ACLOC      ,&
            &2  )
         ENDIF
         IF (EFTOT.GT.0.) THEN
            TPER = 2.*PI * ETOT / EFTOT
            VOQ(IP,VOQR(IVTYPE)) = TPER
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       Benjamin-Feir Index (BFI)

      IVTYPE = 59
      IF (OQPROC(IVTYPE)) THEN
         STPNS = VOQ(IP,VOQR(18))
         QP    = VOQ(IP,VOQR(58))
         IF ( STPNS.NE.OVEXCV(18) .AND. QP.NE.OVEXCV(58) ) THEN
            VOQ(IP,VOQR(IVTYPE)) = SQRT(PI2)*STPNS*QP
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       peak wave length

      IVTYPE = 71
      IF (OQPROC(IVTYPE)) THEN
         EMAX = 0.
         ISIGM = -1
         DO IS = 1, MSC
            ETD = 0.
            DO ID = 1, MDC
               ETD = ETD + WK(IS)*ACLOC(ID,IS)*DDIR
            ENDDO
            IF (ETD.GT.EMAX) THEN
               EMAX  = ETD
               ISIGM = IS
            ENDIF
         ENDDO
         IF (ISIGM.GT.0) THEN
            VOQ(IP,VOQR(IVTYPE)) = 2.*PI/WK(ISIGM)
         ELSE
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
         ENDIF
      ENDIF

!       partitioning output according to Hanson and Phillips (2001)

      IF ( OQPROC(100).OR.OQPROC(110).OR.OQPROC(120).OR.&
      &OQPROC(130).OR.OQPROC(140).OR.OQPROC(150).OR.&
      &OQPROC(160) ) THEN
!         to achieve minimum storage but guaranteed storage of all
!         partitions DIMXPT = ((MSC+1)/2) * ((MDC-1)/2)
         DIMXPT = ((MSC+1)/2) * ((MDC-1)/2)
         ALLOCATE (XPT(7,0:DIMXPT))
         XPT = 0.
!         compute wind parameters, for partitioning routine
         UABS = SQRT(VOQ(IP,VOQR(26))**2+VOQ(IP,VOQR(26)+1)**2)
         UDIR = ATAN2(VOQ(IP,VOQR(26)+1),VOQ(IP,VOQR(26)))*180./PI
!          IF (UDIR.LT.360.) UDIR = UDIR + 360.
!
!         compute partitioning and integral parameters per partition
         CALL SWPART (TRANSPOSE(ACLOC), UABS, UDIR, VOQ(IP,VOQR(4)),&
         &WK, SPCSIG, SPCDIR, NP, XPT, DIMXPT)

!         requested number of swells
         NOSWLL = INT(OUTPAR(51))
         IF (OQPROC(100)) VOQ(IP,VOQR(100:100+NOSWLL)) = 0.
         IF (OQPROC(110)) VOQ(IP,VOQR(110:110+NOSWLL)) = 0.
         IF (OQPROC(120)) VOQ(IP,VOQR(120:120+NOSWLL)) = 0.
         IF (OQPROC(130)) VOQ(IP,VOQR(130:130+NOSWLL)) = 0.
         IF (OQPROC(140)) VOQ(IP,VOQR(140:140+NOSWLL)) = 0.
         IF (OQPROC(150)) VOQ(IP,VOQR(150:150+NOSWLL)) = 0.
         IF (OQPROC(160)) VOQ(IP,VOQR(160:160+NOSWLL)) = 0.

!         limit number of partitions in output to 10
         NP = MIN(NP,10)
         IF (NP.GT.0) THEN
            IF (OQPROC(171)) VOQ(IP,VOQR(171)) = NINT(REAL(NP))
            IF (OQPROC(100)) THEN
!              XPT(:,0) are values for the total wave field, not required 41.62
               ! wind sea partition
               IF ( (XPT(6,1).GE.WSCUT).AND.(XPT(1,1).GE.0.) ) THEN
                  VOQ(IP,VOQR(100)) = XPT(1,1)
               ELSE
                  VOQ(IP,VOQR(100)) = 0.
               ENDIF
               ! swell partitions
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  ! swell index starts at 2 if there is wind sea
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  IF ( XPT(1,IPTSW).GE.0. ) THEN
                     VOQ(IP,VOQR(100+IPT)) = XPT(1,IPTSW)
                  ELSE
                     VOQ(IP,VOQR(100+IPT)) = 0.
                  ENDIF
               ENDDO
            ENDIF
            IF (OQPROC(110)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(110)) = XPT(2,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(110+IPT)) = XPT(2,IPTSW)
               ENDDO
            ENDIF
            IF (OQPROC(120)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(120)) = XPT(3,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(120+IPT)) = XPT(3,IPTSW)
               ENDDO
            ENDIF
            IF (OQPROC(130)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(130)) = XPT(4,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(130+IPT)) = XPT(4,IPTSW)
               ENDDO
            ENDIF
            IF (OQPROC(140)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(140)) = XPT(5,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(140+IPT)) = XPT(5,IPTSW)
               ENDDO
            ENDIF
            IF (OQPROC(150)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(150)) = XPT(6,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(150+IPT)) = XPT(6,IPTSW)
               ENDDO
            ENDIF
            IF (OQPROC(160)) THEN
               IF ( XPT(6,1).GE.WSCUT ) THEN
                  VOQ(IP,VOQR(160)) = XPT(7,1)
               ENDIF
               DO IPT = 1, NOSWLL
                  IPTSW = IPT
                  IF (XPT(6,1) .GE. WSCUT) IPTSW = IPT+1
                  VOQ(IP,VOQR(160+IPT)) = XPT(7,IPTSW)
               ENDDO
            ENDIF
         ENDIF
         DEALLOCATE (XPT)
      ENDIF

      VALID_OUTPUT = .TRUE.
      END BLOCK point_output

!       points on land: assign exception value

      IF (.NOT. VALID_OUTPUT) THEN
      do II = 1, NVOTP
         IVTYPE = IVOTP(II)
         IF (OQPROC(IVTYPE)) THEN
            VOQ(IP,VOQR(IVTYPE)) = OVEXCV(IVTYPE)
            IF (OVSVTY(IVTYPE).EQ.3) THEN
               VOQ(IP,VOQR(IVTYPE)+1) = OVEXCV(IVTYPE)
            ENDIF
         ENDIF
      end do
      END IF
   end do

   IF (ALLOCATED(FLUX)) DEALLOCATE(FLUX)
   IF (ALLOCATED(FLOC)) DEALLOCATE(FLOC)

   IF (ALLOCATED(EBLOC)) DEALLOCATE(EBLOC)

   RETURN
!     end of subroutine SWOEXA
end subroutine SWOEXA
!************************************************************************
!                                                                      *
SUBROUTINE SWOEXF (MIP      ,XC       ,YC       ,VOQR     ,&
&VOQ      ,AC2      ,DEP2     ,SPCSIG   ,&
&WK       ,CG       ,SPCDIR   ,NE       ,&
&NED      ,KGRPNT   ,XCGRID   ,YCGRID   ,&
&HS       ,IONOD&
&)
   USE swan_service_interfaces, ONLY: STRACE, EQREAL
   USE swan_wave_physics, ONLY: KSCIP1
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE M_PARALL

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
!     30.72: IJsbrand Haagsma
!     30.80, 40.13: Nico Booij
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     40.31: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.55, Mar. 97: Procedure updated for curvilinear coordinates basics is
!                     described in SWANDOC.WP5 comp. grid point coordinates are
!                     new arguments
!     30.72, Oct. 97: Logical function EQREAL introduced for floating point
!                     comparisons
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.80, Apr. 98: Provision for 1D computation
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     30.81, Dec. 98: Implicit none added, force for point surrounded by
!                     dry points set to 0.
!     40.13, Aug. 01: provision for repeating grid (KREPTX>0)
!                     spherical coordinates taken into account
!                     swcomm2.inc reactivated
!     40.31, Jan. 04: adapted for parallelisation with MPI
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Calculates wave-driven force (output quantity IVTYPE=20)
!
!  3. Method
!
!     Radiation stresses are defined as:
!                     /
!     Sxx = rho grav | ((N cos^2(theta) + N - 1/2) sig Ac) d sig d theta
!                   /
!                     /
!     Sxy = rho grav | (N sin(theta) cos(theta) sig Ac) d sig d theta
!                   /
!                     /
!     Syy = rho grav | ((N sin^2(theta) + N - 1/2) sig Ac) d sig d theta
!                   /
!
!     The force in x-direction and y-direction are:
!
!     Fx = - (@Sxx/@x + @Sxy/@y)
!     Fy = - (@Sxy/@x + @Syy/@y)
!
!     where @ denotes the partial derivative.
!
!     The value of N and its derivative w.rt. the depth are calculated
!     in the KSCIP1 subroutine.
!
!     First the gradients with respect to i and j (comp. grid counters)
!     are computed, then these are transformed into gradients in (x,y)
!
!  4. Argument variables
!
!     AC2     input  action density
!     CG      local  group velocity in output point
!     DEP2    input  depth at comp. grid points
!     KGRPNT  input  index for indirect adressing
!     IONOD   input  array indicating in which subdomain output
!                    points are located
!     MIP     input  number of output points
!     NE      local  ratio of group and phase velocity
!     NED     local  derivative of NE with respect to depth
!     SPCDIR  input  (*,1); spectral directions (radians)
!                    (*,2); cosine of spectral directions
!                    (*,3); sine of spectral directions
!                    (*,4); cosine^2 of spectral directions
!                    (*,5); cosine*sine of spectral directions
!                    (*,6); sine^2 of spectral directions
!     SPCSIG  input  relative frequencies in computational domain in
!                    sigma-space
!     XC, YC  input  comp. grid coordinates of output point
!     XCGRID  input  coordinates of computational grid in x-direction
!     YCGRID  input  coordinates of computational grid in y-direction
!     VOQR    input  location in VOQ of a certain outp quant.
!     VOQ     output values of output quantities
!     WK      local  wavenumber in output point

   INTEGER MIP, VOQR(*) ,KGRPNT(MXC,MYC)
   INTEGER IONOD(*)
   REAL    AC2(MDC,MSC,MCGRD), CG(*), DEP2(MCGRD), NE(*), NED(*)
   REAL    HS(MCGRD)
   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    XC(MIP), YC(MIP)
   REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)
   REAL    VOQ(MIP,*), WK(*)

!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     AC2LOC       Local action density
!     ACWAVE       Action density in output point
!     ACWI, ACWJ   dAC/dI and dAC/dJ
!     ACWX         X-gradient of local action density
!     ACWY         Y-gradient of local action density
!     DDET         determinant
!     DDI, DDJ     dDEP/dI and dDEP/dJ
!     DDX, DDY     spatial depth gradients
!     DIX, DIY     coefficients for transformation from I-gradient
!                  to (X,Y)-gradients
!     DJX, DJY     coefficients for transformation from J-gradient
!                  to (X,Y)-gradients
!     DS2
!     DXI, DXJ     dX/dI and dX/dJ
!     DYI, DYJ     dY/dI and dY/dJ
!     DEP          depth
!     DEPLOC       local depth
!     FX, FY       (preliminary) forces in X-, and Y-direction
!     FXADD, FYADD Cumulated forces/(RHO*GRAV) per frequency and directi-
!                  onal step in X-, and Y-direction
!     ID           counter for steps in direction
!     IENT         number of entries
!     IND1, IND2,
!     IND3, IND4,
!     IND5, IND6,
!     IND7, IND8,
!     IND9         indirect adresses
!     IP           counter
!     IS           counter for sigma
!     IVTYPE
!     JX           counter in X-direction
!     JXLO, JXUP   lower resp. upper gridpoint number of point under consideration
!                  in X-direction
!     JY           counter in Y-direction
!     JYLO, JYUP   lower resp. upper gridpoint number of point under consideration
!                  in Y-direction
!     NAX, NAY     derivative of N * Ac.dens. = N * E / Sigma, w.r.t. X
!                  or Y, respectively.
!     ONX, ONY     Indicates whether or not an output point lies on a
!                  computational point or not
!     RRDI,RRDJ    multiplication factor: 0.5 in case of two-sided or 1
!                  of one-sided differential
!     SIG          dummy variable
!     SXLO, SXUP   weight coefficients for the lower and upper x-level of the
!                  point under consideration, respectively.
!     SYLO, SYUP   weight coefficients for the lower and upper y-level of the
!                  point under consideration, respectively.

   REAL        ACWAV, ACWI, ACWJ, ACWX, ACWY, DDET, DDI, DDJ, DDX,&
   &DDY, DIX, DIY,DJX, DJY, DS2, DXI, DXJ, DYI, DYJ, DEP,&
   &DEPLOC, FX,FY, FXADD, FYADD, NAX, NAY, RRDI, RRDJ,&
   &SIG, SXLO, SXUP, SYLO, SYUP, CSLAT
   INTEGER, SAVE :: IENT = 0
   INTEGER     ID, IND1, IND2, IND3, IND4, IND5, IND6, IND7,&
   &IND8, IND9, IP, IS, IVTYPE, JX, JXLO, JXUP, JY,&
   &JYLO, JYUP
   LOGICAL     ONX, ONY, VALID_FORCE

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     KSCIP1           calculates WK, CG, N and ND
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWEXCHG          exchanges AC2 at subdomain boundaries
!TIMG!     SWTSTA           Start timing for a section of code
!TIMG!     SWTSTO           Stop timing for a section of code


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
!     -In determining derivatives one-sided differences are used at
!      border meshes; for output points inside a mesh derivative
!      over one step is taken; for output points on a computational
!      grid point a central derivative is taken
!     -A marigin of 0.01 m is taken outside the computational grid.
!     -The range of the counter runs from 1 to MXC; the range of XC(IP)
!      from 0 to MXC-1!
!
!  Counter: 1          2          JX        JX+1      JX+2       MXC-1
!
!         |=|----------|-- -- -- -|--------|=|=|--------|-- -- -- -|----------|=|
!
!  XC:      0          1                     JX                  MXC-2
!
!
!     -Order in which they are treated:
!
!         |=|----------|          |--------|=|=|--------|          |----------|=|
! Order:         A                     B     C      D
!
!
! 12. Structure
!
!     ----------------------------------------------------------------
!     For all output points do
!         Initialize both force components as 0
!         Determine neighbouring points to be used for gradients in
!         X and Y
!         Call KSCIP1 (determine derivative of N with respect to depth)
!         For all spectral components do
!             determine derivative of nE with respect to X
!             determine derivative of nE with respect to Y
!             calculate contribution to force components
!     ----------------------------------------------------------------
!
! 13. Source text

   CALL STRACE (IENT, 'SWOEXF')

   IVTYPE = 20

!     loop over all output points

   do IP=1,MIP
      VALID_FORCE = .FALSE.
      force_point: BLOCK
      DEP = VOQ(IP,VOQR(4))
      IF (DEP.LE.0.)                  EXIT force_point
      IF (EQREAL(DEP,OVEXCV(4)))      EXIT force_point
      IF (KREPTX .EQ. 0) THEN
         IF (XC(IP).LT.-0.01)            EXIT force_point
         IF (XC(IP).GT.REAL(MXC-1)+0.01) EXIT force_point
      ENDIF
      IF (YC(IP).LT.-0.01)            EXIT force_point
      IF (YC(IP).GT.REAL(MYC-1)+0.01) EXIT force_point
      IF (PARLL .AND. IONOD(IP).NE.INODE) EXIT force_point

!       first the action density spectrum is interpolated

      FX  = 0.
      FY  = 0.
      JX  = NINT(XC(IP))
      RRDI = 1.
      ONX = .FALSE.
      IF (KREPTX.EQ.0 .AND. JX.EQ.0) THEN
         JXLO = 1
         JXUP = 2
         JX   = 1
         SXUP = XC(IP)
         SXLO = 1.-SXUP
      ELSE IF (KREPTX.EQ.0 .AND. JX.EQ.MXC-1) THEN
         JXLO = MXC-1
         JXUP = MXC
         SXLO = REAL(MXC-1)-XC(IP)
         SXUP = 1.-SXLO
         JX   = JX+1
      ELSE IF (XC(IP).LT.REAL(JX)-0.01) THEN
         JXLO = JX
         JXUP = JX+1
         SXLO = REAL(JX)-XC(IP)
         SXUP = 1.-SXLO
         JX   = JX+1
      ELSE IF (XC(IP).GT.REAL(JX)+0.01) THEN
         JXLO = JX+1
         JXUP = JX+2
         SXUP = XC(IP)-REAL(JX)
         SXLO = 1.-SXUP
         JX   = JX+1
      ELSE
         JXLO = JX
         JXUP = JX+2
         RRDI = 0.5
         JX   = JX+1
         ONX  = .TRUE.
      ENDIF
      IF (KREPTX .GT. 0) THEN
         JX   = 1 + MODULO (JX-1, MXC)
         JXLO = 1 + MODULO (JXLO-1, MXC)
         JXUP = 1 + MODULO (JXUP-1, MXC)
      ENDIF
      IF (ONED) THEN
         JYLO = 1
         JYUP = 1
         JY   = 1
         RRDJ = 0.
         ONY  = .TRUE.
      ELSE
         JY   = NINT(YC(IP))
         RRDJ = 1.
         ONY  = .FALSE.
         IF (JY.EQ.0) THEN
            JYLO = 1
            JYUP = 2
            JY   = 1
            SYUP = YC(IP)
            SYLO = 1.-SYUP
         ELSE IF (JY.EQ.MYC-1) THEN
            JYLO = MYC-1
            JYUP = MYC
            SYLO = REAL(MYC-1)-YC(IP)
            SYUP = 1.-SYLO
            JY   = JY+1
         ELSE IF (YC(IP).LT.REAL(JY)-0.01) THEN
            JYLO = JY
            JYUP = JY+1
            SYLO = REAL(JY)-YC(IP)
            SYUP = 1.-SYLO
            JY   = JY+1
         ELSE IF (YC(IP).GT.REAL(JY)+0.01) THEN
            JYLO = JY+1
            JYUP = JY+2
            SYUP = YC(IP)-REAL(JY)
            SYLO = 1.-SYUP
            JY   = JY+1
         ELSE
            JYLO = JY
            JYUP = JY+2
            RRDJ = 0.5
            JY  = JY+1
            ONY = .TRUE.
         ENDIF
      ENDIF

!       *** Using indirect addressing for arrays AC2 and DEP2 ***
      IND1 = KGRPNT(JXLO,JYLO)
      IND2 = KGRPNT(JXUP,JYLO)
      IND3 = KGRPNT(JXUP,JYUP)
      IND4 = KGRPNT(JXLO,JYUP)
      IND5 = KGRPNT(JXLO,JY  )
      IND6 = KGRPNT(JXUP,JY  )
      IND7 = KGRPNT(JX  ,JYLO)
      IND8 = KGRPNT(JX  ,JYUP)
      IND9 = KGRPNT(JX  ,JY  )
      IF (ONY) THEN
         IF (DEP2(IND5).LE.DEPMIN .OR. .NOT. HS(IND5).NE.0.) EXIT force_point
         IF (DEP2(IND6).LE.DEPMIN .OR. .NOT. HS(IND6).NE.0.) EXIT force_point
      ELSE
         IF (DEP2(IND1).LE.DEPMIN .OR. .NOT. HS(IND1).NE.0.) EXIT force_point
         IF (DEP2(IND2).LE.DEPMIN .OR. .NOT. HS(IND2).NE.0.) EXIT force_point
         IF (DEP2(IND3).LE.DEPMIN .OR. .NOT. HS(IND3).NE.0.) EXIT force_point
         IF (DEP2(IND4).LE.DEPMIN .OR. .NOT. HS(IND4).NE.0.) EXIT force_point
      ENDIF
      IF (ONX) THEN
         IF (DEP2(IND7).LE.DEPMIN .OR. .NOT. HS(IND7).NE.0.) EXIT force_point
         IF (DEP2(IND8).LE.DEPMIN .OR. .NOT. HS(IND8).NE.0.) EXIT force_point
      ELSE
         IF (DEP2(IND1).LE.DEPMIN .OR. .NOT. HS(IND1).NE.0.) EXIT force_point
         IF (DEP2(IND2).LE.DEPMIN .OR. .NOT. HS(IND2).NE.0.) EXIT force_point
         IF (DEP2(IND3).LE.DEPMIN .OR. .NOT. HS(IND3).NE.0.) EXIT force_point
         IF (DEP2(IND4).LE.DEPMIN .OR. .NOT. HS(IND4).NE.0.) EXIT force_point
      ENDIF

!       determine depth and (x,y) derivatives w.r.t. i and j

      IF (ONY) THEN
         DDI = RRDI * (DEP2(IND6)-DEP2(IND5))
         DXI = RRDI * (XCGRID(JXUP,JY)-XCGRID(JXLO,JY))
         DYI = RRDI * (YCGRID(JXUP,JY)-YCGRID(JXLO,JY))
      ELSE
         DDI = RRDI * (SYUP*(DEP2(IND3)-DEP2(IND4)) +&
         &SYLO*(DEP2(IND2)-DEP2(IND1)))
         DXI = RRDI * (SYUP*(XCGRID(JXUP,JYUP)-XCGRID(JXLO,JYUP)) +&
         &SYLO*(XCGRID(JXUP,JYLO)-XCGRID(JXLO,JYLO)))
         DYI = RRDI * (SYUP*(YCGRID(JXUP,JYUP)-YCGRID(JXLO,JYUP)) +&
         &SYLO*(YCGRID(JXUP,JYLO)-YCGRID(JXLO,JYLO)))
      ENDIF
      IF (ONX) THEN
         DDJ = RRDJ * (DEP2(IND8)-DEP2(IND7))
         DXJ = RRDJ * (XCGRID(JX,JYUP)-XCGRID(JX,JYLO))
         DYJ = RRDJ * (YCGRID(JX,JYUP)-YCGRID(JX,JYLO))
      ELSE
         DDJ = RRDJ * (SXUP*(DEP2(IND3)-DEP2(IND2)) +&
         &SXLO*(DEP2(IND4)-DEP2(IND1)))
         DXJ = RRDJ * (SXUP*(XCGRID(JXUP,JYUP)-XCGRID(JXUP,JYLO)) +&
         &SXLO*(XCGRID(JXLO,JYUP)-XCGRID(JXLO,JYLO)))
         DYJ = RRDJ * (SXUP*(YCGRID(JXUP,JYUP)-YCGRID(JXUP,JYLO)) +&
         &SXLO*(YCGRID(JXLO,JYUP)-YCGRID(JXLO,JYLO)))
      ENDIF
      IF (KSPHER.GT.0) THEN
!         spherical coordinates are used; first compute cos(latitude)
         CSLAT = COS(DEGRAD*(YOFFS+YCGRID(JX,JY)))
!         LENDEG is the length of one degree of the sphere
         DXI = DXI * LENDEG * CSLAT
         DYI = DYI * LENDEG
         DXJ = DXJ * LENDEG * CSLAT
         DYJ = DYJ * LENDEG
      ENDIF

!       coefficients from transformation from (i,j)-gradients to (x,y)-gradients

      IF (JXUP.EQ.JXLO .AND. JYUP.EQ.JYLO) THEN
!         point surrounded by dry points
         DIX  = 0.
         DIY  = 0.
         DJX  = 0.
         DJY  = 0.
      ELSE IF (JXUP.EQ.JXLO) THEN
!         no forces in i-direction
         DS2  = DXJ**2 + DYJ**2
         DIX  = 0.
         DIY  = 0.
         DJX  = DXJ/DS2
         DJY  = DYJ/DS2
      ELSE IF (JYUP.EQ.JYLO) THEN
!         no forces in j-direction
         DS2  = DXI**2 + DYI**2
         DIX  = DXI/DS2
         DIY  = DYI/DS2
         DJX  = 0.
         DJY  = 0.
      ELSE
!         coefficients for transformation from
!         (i,j)-gradients to (x,y)-gradients
         DDET = DXI*DYJ - DXJ*DYI
         DIX  =  DYJ / DDET
         DIY  = -DXJ / DDET
         DJX  = -DYI / DDET
         DJY  =  DXI / DDET
      ENDIF
!       spatial depth gradients:
      DDX  = DDI*DIX + DDJ*DJX
      DDY  = DDI*DIY + DDJ*DJY

      IF (ITEST.GE.80 .OR. IOUTES .GE. 20) WRITE (PRTEST, "(' SWOEXF ', 5I6, 2X, 4F7.4, 2X, 4E12.4)") IP,&
      &JXLO, JXUP, JYLO, JYUP ,SXLO, SXUP, SYLO, SYUP,&
      &DIX, DIY, DJX, DJY

!       compute NE and NED

      DEPLOC = VOQ(IP,VOQR(4))
      CALL KSCIP1 (MSC, SPCSIG, DEPLOC, WK, CG, NE, NED)
      IF (ITEST.GE.100 .OR. IOUTES .GE. 20) THEN
         WRITE (PRTEST, "(' depth gradient ', 4(1X,F9.4))")  DEPLOC, DDX, DDY
         do IS = 1, MIN(MSC,20)
            WRITE (PRTEST, "(' i, SPCSIG, N, Nd ', I2, 3(1X, E12.4))") IS, SPCSIG(IS), NE(IS),&
            &NED(IS)
         end do
      ENDIF

      do ID  = 1, MDC
         do IS = 1, MSC
            SIG = SPCSIG(IS)

!           ACWAV is local action density

            IF (ONX.AND.ONY) THEN
               ACWAV = AC2(ID,IS,IND9)
            ELSE IF (ONX) THEN
               ACWAV = SYLO * AC2(ID,IS,IND7) +&
               &SYUP * AC2(ID,IS,IND8)
            ELSE IF (ONY) THEN
               ACWAV = SXLO * AC2(ID,IS,IND5) +&
               &SXUP * AC2(ID,IS,IND6)
            ELSE
               ACWAV = SXLO * (SYLO * AC2(ID,IS,IND1) +&
               &SYUP * AC2(ID,IS,IND4)) +&
               &SXUP * (SYLO * AC2(ID,IS,IND2) +&
               &SYUP * AC2(ID,IS,IND3))
            ENDIF

!           ACWX is X-gradient of local action density, ACWY is Y-gradient

            IF (ONY) THEN
               ACWI = RRDI * (AC2(ID,IS,IND6) -&
               &AC2(ID,IS,IND5))
            ELSE
               ACWI = RRDI * (SYLO * (AC2(ID,IS,IND2) -&
               &AC2(ID,IS,IND1)) +&
               &SYUP * (AC2(ID,IS,IND3) -&
               &AC2(ID,IS,IND4)))
            ENDIF
            IF (ONX) THEN
               ACWJ = RRDJ * (AC2(ID,IS,IND8) -&
               &AC2(ID,IS,IND7))
            ELSE
               ACWJ = RRDJ * (SXLO * (AC2(ID,IS,IND4) -&
               &AC2(ID,IS,IND1)) +&
               &SXUP * (AC2(ID,IS,IND3) -&
               &AC2(ID,IS,IND2)))
            ENDIF

!           spatial action density gradients:
            ACWX = ACWI*DIX + ACWJ*DJX
            ACWY = ACWI*DIY + ACWJ*DJY

!           NAX is the derivative of N * Ac.dens.  w.r.t. X
!           So NAX = @(N*Ac)/@X =Ac*@N/@X +N*@Ac/@X
!
!           where @ denotes the partial derivative.
!
!           Further note that that @N/@X = @N/@Depth * @Depth/@X
!                                        = NED * DDX
!
!           Anologously for NAY.

            NAX = NE(IS) * ACWX + NED(IS) * DDX * ACWAV
            NAY = NE(IS) * ACWY + NED(IS) * DDY * ACWAV
            FXADD = - ( (SPCDIR(ID,4) + 1.) * NAX - 0.5 * ACWX +&
            &SPCDIR(ID,5) * NAY ) * SIG
            FYADD = - ( (SPCDIR(ID,6) + 1.) * NAY - 0.5 * ACWY +&
            &SPCDIR(ID,5) * NAX ) * SIG

!           integration

            FX = FX + SIG * FXADD
            FY = FY + SIG * FYADD
         end do
      end do

      FX = RHO * GRAV * FX * DDIR * FRINTF
      FY = RHO * GRAV * FY * DDIR * FRINTF
      VOQ(IP,VOQR(IVTYPE))   = (COSCQ*FX - SINCQ*FY)
      VOQ(IP,VOQR(IVTYPE)+1) = (SINCQ*FX + COSCQ*FY)
      VALID_FORCE = .TRUE.
      END BLOCK force_point

!       points on land: assign exception value

      IF (.NOT. VALID_FORCE) THEN
         VOQ(IP,VOQR(IVTYPE))   = OVEXCV(IVTYPE)
         VOQ(IP,VOQR(IVTYPE)+1) = OVEXCV(IVTYPE)
      END IF

   end do

   RETURN
!     end of subroutine SWOEXF
end subroutine SWOEXF

end module swan_output_orchestration
