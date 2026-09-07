
!     SWAN - routines for distributed-memory approach based on MPI
!
!  Contents of this file
!
!     SWINITMPI
!     SWEXITMPI
!     SWSYNC
!     SWSENDNB
!     SWRECVNB
!     SWBROADC
!     SWGATHER
!     SWREDUCE
!     SWREDUCI
!     SWREDUCR
!     SWSTRIP
!     SWORB
!     SWPARTIT
!     SWBLADM
!     SWDECOMP
!     SWEXCHG_JAC
!     SWEXCHG_WFR
!     SWRECVAC
!     SWSENDAC
!     SWCOLLECT
!     SWCOLOUT
!     SWCOLTAB
!     SWCOLSPC
!     SWCOLBLK
!     SWBLKCOL
!
!****************************************************************

module swan_parallel
   use swan_build_config, only: jacobi_sweep_enabled, timing_enabled
   use swan_matlab_output_backend, only: matlab_direct_record_length
   use swan_io_limits, only: LENFNM
   use swan_output_variables, only: NMOVAR, OVEXCV, OVHEXP, OVLNAM, OVSNAM, OVSVTY, OVUNIT
   use swan_time, only: CHTIME
   implicit none(type, external)
   private
!  Both historical exchange algorithms are ordinary module procedures. A
!  selected facade presents one fixed contract to their callers.
   public :: SWINITMPI, SWEXITMPI, SWSYNC, SWDECOMP, SWCOLLECT, SWCOLOUT
   public :: SWPARTIT
   public :: SWEXCHG_JAC, SWEXCHG_WFR
   public :: SWRECVAC, SWSENDAC
   public :: SWBLKCOL

contains

SUBROUTINE SWINITMPI
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_mpi_backend, ONLY: mpi_backend_communication_constants, swan_mpi_success
   USE swan_mpi_lifecycle_backend, ONLY: lifecycle_initialize, &
      lifecycle_rank, lifecycle_size
   USE swan_service_interfaces, ONLY: MSGERR, SWTSTA, SWTSTO

!****************************************************************
!
   USE swan_diagnostics_level
   USE M_PARALL

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Jan. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Join parallel application
!
!  3. Method
!
!     Start MPI and initialize some variables
!
!  4. Argument variables
!
!     ---
!
!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     CHARS :     array to pass character info to MSGERR
!     IERR  :     error value of MPI call
!     MSGSTR:     string to pass message to call MSGERR

   INTEGER      IERR
   CHARACTER(LEN=20) CHARS(2)
   CHARACTER(LEN=80) MSGSTR

!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     MPI_COMM_RANK    Get rank of processes in MPI communication contex
!     MPI_COMM_SIZE    Get number of processes in MPI communication cont
!     MPI_INIT         Enroll in MPI
!     MSGERR           Writes error message
!
!  9. Subroutines calling
!
!     Main program SWAN
!
! 10. Error messages
!
!     ---
!
! 12. Structure
!
!     Start MPI and initialize some common variables in module M_PARALL
!
! 13. Source text

   LEVERR = 0
   MAXERR = 1
   ITRACE = 0
   IERR = swan_mpi_success

!     --- enroll in MPI

   CALL lifecycle_initialize(IERR)
   IF (IERR.NE.swan_mpi_success) THEN
      CHARS(1) = INTSTR(IERR)
      MSGSTR = 'MPI produces some internal error - '//&
      &'return code is '//TRIM(ADJUSTL(CHARS(1)))
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF
!
!     --- initialize common variables

   INODE = 0
   NPROC = 1

!     --- get node number INODE

   CALL lifecycle_rank(INODE, IERR)
   INODE = INODE + 1
   IF (IERR.NE.swan_mpi_success) THEN
      CHARS(1) = INTSTR(IERR)
      CHARS(2) = INTSTR(INODE)
      MSGSTR = 'MPI produces some internal error - '//&
      &'return code is '//TRIM(ADJUSTL(CHARS(1)))//&
      &' and node number is '//TRIM(ADJUSTL(CHARS(2)))
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF

!     --- determine total number of processes

   CALL lifecycle_size(NPROC, IERR)
   IF (IERR.NE.swan_mpi_success) THEN
      CHARS(1) = INTSTR(IERR)
      CHARS(2) = INTSTR(INODE)
      MSGSTR = 'MPI produces some internal error - '//&
      &'return code is '//TRIM(ADJUSTL(CHARS(1)))//&
      &' and node number is '//TRIM(ADJUSTL(CHARS(2)))
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF
!
!     --- determine whether this is a parallel run or not

   IF ( NPROC.GT.1 ) THEN
      PARLL = .TRUE.
   ELSE
      PARLL = .FALSE.
   END IF

!     --- am I master?

   IAMMASTER = INODE.EQ.MASTER

!     --- define MPI constants for communication within SWAN

   CALL mpi_backend_communication_constants(SWCHAR, SWINT, SWREAL, SWMAX, &
      SWMIN, SWSUM)

   RETURN
end subroutine SWINITMPI
!****************************************************************

SUBROUTINE SWEXITMPI
   USE swan_mpi_backend, ONLY: mpi_backend_initialized
   USE swan_mpi_lifecycle_backend, ONLY: lifecycle_abort, &
      lifecycle_barrier, lifecycle_finalize

!****************************************************************
!
   USE swan_diagnostics_level

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Jan. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Exit parallel application
!
!  3. Method
!
!     Wrapper for MPI_FINALIZE
!
!  4. Argument variables
!
!     ---
!
!  6. Local variables
!
!     IERR    :   error value of MPI call
!     PARALMPI:   if true, parallel process is carried out with MPI

   INTEGER IERR
   LOGICAL PARALMPI

!  8. Subroutines used
!
!     MPI_ABORT        Abort MPI if severe error occurs
!     MPI_BARRIER      Blocks until all nodes have called this routine
!     MPI_INITIALIZED  Indicates whether MPI_Init has been called
!     MPI_FINALIZE     Cleans up the MPI state and exits
!
!  9. Subroutines calling
!
!     Main program SWAN
!
! 10. Error messages
!
!     ---
!
! 12. Structure
!
!     if MPI has been initialized
!        synchronize nodes
!        if severe error
!           abort MPI
!        else
!           close MPI
!
! 13. Source text
!
   CALL mpi_backend_initialized(PARALMPI, IERR)
   IF ( PARALMPI ) THEN

      CALL lifecycle_barrier(IERR)

      IF ( LEVERR.GE.4 ) THEN

!        --- in case of a severe error abort all MPI processes

         CALL lifecycle_abort(LEVERR, IERR)

      ELSE

!        --- otherwise stop MPI operations on this computer

         CALL lifecycle_finalize(IERR)

      END IF

   END IF

   RETURN
end subroutine SWEXITMPI
!****************************************************************

SUBROUTINE SWSYNC
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_mpi_backend, ONLY: swan_mpi_success
   USE swan_mpi_lifecycle_backend, ONLY: lifecycle_barrier
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************
!
   USE swan_diagnostics_level
   USE M_PARALL

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Jan. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Synchronize nodes
!
!  3. Method
!
!     Wrapper for MPI_BARRIER
!
!  4. Argument variables
!
!     ---
!
!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     CHARS :     array to pass character info to MSGERR
!     IENT  :     number of entries
!     IERR  :     error value of MPI call
!     MSGSTR:     string to pass message to call MSGERR

   INTEGER, SAVE :: IENT = 0
   INTEGER      IERR
   CHARACTER(LEN=20) CHARS(2)
   CHARACTER(LEN=80) MSGSTR

!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     MPI_BARRIER      Blocks until all nodes have called this routine
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWMAIN
!
! 10. Error messages
!
!     ---
!
! 12. Structure
!
!     Blocks until all nodes have called MPI_BARRIER routine.
!     In this way, all nodes are synchronized
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWSYNC')
   IERR = swan_mpi_success

!     --- blocks until all nodes have called this routine

   CALL lifecycle_barrier(IERR)
   IF (IERR.NE.swan_mpi_success) THEN
      CHARS(1) = INTSTR(IERR)
      CHARS(2) = INTSTR(INODE)
      MSGSTR = 'MPI produces some internal error - '//&
      &'return code is '//TRIM(ADJUSTL(CHARS(1)))//&
      &' and node number is '//TRIM(ADJUSTL(CHARS(2)))
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF

   RETURN
end subroutine SWSYNC
!****************************************************************

!****************************************************************
!****************************************************************
!
SUBROUTINE SWSTRIP_WFR ( IPOWN, IDIR, NPART, IWORK, MXC, MYC )
   USE swan_service_interfaces, ONLY: STRACE
!
!****************************************************************
!
   USE swan_diagnostics_level
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Performs a stripwise partitioning with straight interfaces
!
!  3. Method
!
!     Each active point in a row/column will be assign to a part
!     according to its number and size (stored in IWORK).
!     The remaining points in the row/column will be assign
!     to the same part.
!
!  4. Argument variables
!
!     IDIR        direction of cutting
!                 1 = row
!                 2 = column
!     IPOWN       array giving the subdomain number of each gridpoint
!     IWORK       work array with the following meaning:
!                    IWORK(1,i) = number of i-th part to be created
!                    IWORK(2,i) = size of i-th part to be created
!     MXC         maximum counter of gridpoints in x-direction
!     MYC         maximum counter of gridpoints in y-direction
!     NPART       number of parts to be created
!
   INTEGER   IDIR, MXC, MYC, NPART
   INTEGER   IPOWN(*)
   INTEGER(KIND=SELECTED_INT_KIND(18)) IWORK(2,*)
!
!  6. Local variables
!
!     IC    :     index of (IX,IY)-point
!     ICC   :     index of (IX,IY)-point
!     IENT  :     number of entries
!     INCX  :     increment for adressing: 1 for x-dir, MXC for y-dir
!     INCY  :     increment for adressing: MXC for x-dir, 1 for y-dir
!     IX    :     index in x-direction
!     IY    :     index in y-direction
!     IYY   :     index in y-direction
!     IPART :     a part counter
!     MXCI  :     maximum counter of gridpoints in x/y-direction
!     MYCI  :     maximum counter of gridpoints in y/x-direction
!     NCURPT:     number of currently assigned points to a created part
!     NPREM :     number of remaining points in a row/column
!
   INTEGER IC, ICC, INCX, INCY, IX, IY, IYY, IPART,&
   &MXCI, MYCI, NCURPT, NPREM
   INTEGER, SAVE :: IENT = 0
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWPARTIT
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
!     depending on cutting direction, determine indirect addressing
!     create first empty part
!     for all active points do
!         assign this point to the created part
!         if size of created part has been reached
!            determine remaining active points in the current column
!            if no remaining points, create next empty part
!            else remaining points belong to the current part
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWSTRIP')

!     --- depending on cutting direction, determine indirect addressing
!         for array IPOWN

   IF ( IDIR.EQ.1 ) THEN
      MXCI = MYC
      MYCI = MXC
      INCX = MXC
      INCY = 1
   ELSE IF ( IDIR.EQ.2 ) THEN
      MXCI = MXC
      MYCI = MYC
      INCX = 1
      INCY = MXC
   END IF

!     --- create first empty part

   IPART  = 1
   NCURPT = 0

!     --- for all active points do

   DO IX = 1, MXCI
      DO IY = 1, MYCI

         IC = IX*INCX + IY*INCY - MXC

         IF ( IPOWN(IC).EQ.1 ) THEN

!              --- assign this point to the created part

            IPOWN(IC) = IWORK(1,IPART)
            NCURPT    = NCURPT + 1

!              --- if size of created part has been reached

            IF ( NCURPT.GE.IWORK(2,IPART) ) THEN

!                 --- determine remaining active points in the
!                     current column

               NPREM = 0
               DO IYY = IY+1, MYCI
                  ICC = IX*INCX + IYY*INCY - MXC
                  IF (IPOWN(ICC).EQ.1) NPREM = NPREM +1
               END DO

               IF ( NPREM.EQ.0 ) THEN

!                    --- if no remaining points, create next empty part

                  IPART  = IPART + 1
                  NCURPT = 0

               ELSE

!                    --- else remaining points belong to the current par

                  IWORK(2,IPART  ) = IWORK(2,IPART  ) + NPREM
                  IWORK(2,IPART+1) = IWORK(2,IPART+1) - NPREM

               END IF

            END IF

         END IF

      END DO
   END DO

   RETURN
end subroutine SWSTRIP_WFR
!****************************************************************
!
SUBROUTINE SWSTRIP_JAC ( IPOWN, IDIR, IPART, LPARTS,&
&MXC  , MYC )
   USE swan_service_interfaces, ONLY: STRACE
!
!****************************************************************
!
   USE swan_diagnostics_level
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Performs a stripwise partitioning with straight interfaces
!
!  3. Method
!
!     This method is described in Ph.D. Thesis of M. Roest
!     entitled:
!     Partitioning for parallel finite difference computations
!     in coastal water simulation, DUT, 1997
!
!  4. Argument variables
!
!     IDIR        direction of cutting
!                 1 = row
!                 2 = column
!     IPART       part number that must be partitioned
!     IPOWN       array giving the subdomain number of each gridpoint
!     LPARTS      list of parts to be created
!                    lparts(1,i) = number of i-th part to be created
!                    lparts(2,i) = size of i-th part to be created
!     MXC         maximum counter of gridpoints in x-direction
!     MYC         maximum counter of gridpoints in y-direction
!
   INTEGER   IDIR, IPART, MXC, MYC
   INTEGER   IPOWN(*)
   INTEGER, PARAMETER :: PART_KIND = SELECTED_INT_KIND(18)
   INTEGER(KIND=PART_KIND) LPARTS(2,*)
!
!  6. Local variables
!
!     IC    :     index of (IX,IY)-point
!     ICC   :     index of (IX,IY)-point
!     IENT  :     number of entries
!     INCX  :     increment for adressing: 1 for x-dir, MXC for y-dir
!     INCY  :     increment for adressing: MXC for x-dir, 1 for y-dir
!     IX    :     index in x-direction
!     IY    :     index in y-direction
!     IYY   :     index in y-direction
!     JPART :     a part counter
!     MXCI  :     maximum counter of gridpoints in x/y-direction
!     MYCI  :     maximum counter of gridpoints in y/x-direction
!     NBACK :     number of points in a row already assigned to a part
!     NFORW :     number of points in a row remaining to be assigned
!     NINPRT:     number of points currently assigned to a new part
!
   INTEGER IC, ICC, INCX, INCY, IX, IY, IYY, JPART,&
   &MXCI, MYCI, NBACK, NFORW, NINPRT
   INTEGER, SAVE :: IENT = 0
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWORB
!     SWPARTIT
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
!     depending on IDIR, determine indirect addressing in IPOWN
!     start by creating the first part, which is currently empty
!     for all points in IPOWN do
!        if point belongs to the part that must be partitioned then
!           when current part has reached its planned size
!              see how many points in this row remain to be assigned
!              if no more points to be assigned, go on to next part
!              else, if majority of row has been assigned take the rest
!              else, leave this row to next part
!           assign point IC to part that is currently being created
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWSTRIP')

!     --- depending on IDIR, determine indirect addressing in IPOWN

   IF ( IDIR.EQ.1 ) THEN
      MXCI = MYC
      MYCI = MXC
      INCX = MXC
      INCY = 1
   ELSE IF ( IDIR.EQ.2 ) THEN
      MXCI = MXC
      MYCI = MYC
      INCX = 1
      INCY = MXC
   END IF

!     --- start by creating the first part, which is currently empty

   JPART  = 1
   NINPRT = 0

!     --- for all points in IPOWN do

   DO IX = 1, MXCI
      DO IY = 1, MYCI

         IC = IX*INCX + IY*INCY - MXC

!           --- if this point belongs to the part that must be partition

         IF ( IPOWN(IC).EQ.IPART ) THEN

!              --- when current part has reached its planned size

            IF ( INT(NINPRT,PART_KIND).GE.LPARTS(2,JPART) ) THEN

!                 --- see how many points in this row have been assigned

               NBACK = 0
               DO IYY = 1, IY-1

                  ICC = IX*INCX + IYY*INCY - MXC
                  IF (INT(IPOWN(ICC),PART_KIND).EQ.LPARTS(1,JPART)) &
                     NBACK = NBACK +1

               END DO

!                 --- see how many points in this row remain to be assig

               NFORW = 0
               DO IYY = IY, MYCI

                  ICC = IX*INCX + IYY*INCY - MXC
                  IF (IPOWN(ICC).EQ.IPART) NFORW = NFORW +1

               END DO

!                 --- if no more points to be assigned, go on to next pa

               IF ( NFORW.EQ.0 ) THEN

                  JPART  = JPART + 1
                  NINPRT = 0

               ELSE IF ( (NBACK-NFORW).GT.0 ) THEN

!                    --- if majority of row has been assigned take the r

                  LPARTS(2,JPART  ) = LPARTS(2,JPART  ) + &
                     INT(NFORW,PART_KIND)
                  LPARTS(2,JPART+1) = LPARTS(2,JPART+1) - &
                     INT(NFORW,PART_KIND)

               ELSE
!                    --- else, leave this row to next part

                  LPARTS(2,JPART  ) = LPARTS(2,JPART  ) - &
                     INT(NBACK,PART_KIND)
                  LPARTS(2,JPART+1) = LPARTS(2,JPART+1) + &
                     INT(NBACK,PART_KIND)

                  DO IYY = 1, IY-1

                     ICC = IX*INCX + IYY*INCY - MXC
                     IF ( INT(IPOWN(ICC),PART_KIND).EQ.LPARTS(1,JPART) ) THEN
                        IPOWN(ICC) = INT(LPARTS(1,JPART+1),KIND(IPOWN))
                     END IF

                  END DO

                  JPART  = JPART + 1
                  NINPRT = NBACK

               END IF

            END IF

!              --- assign point IC to part that is currently being creat

            IPOWN(IC) = INT(LPARTS(1,JPART),KIND(IPOWN))
            NINPRT    = NINPRT + 1

         END IF

      END DO
   END DO

   RETURN
end subroutine SWSTRIP_JAC
!****************************************************************
!
SUBROUTINE SWORB ( IPOWN, IDIR, NPART, LPARTS,&
&MXC  , MYC )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
!
!****************************************************************
!
   USE swan_diagnostics_level
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Performs an Orthogonal Recursive Bisection partitioning
!
!  3. Method
!
!     Starting with a single part (the entire domain), each part is
!     recursively partitioned by bisecting it, until all parts have been
!     created. The bisection direction is swapped in each direction.
!
!     This method is described in Ph.D. Thesis of M. Roest
!     entitled:
!     Partitioning for parallel finite difference computations
!     in coastal water simulation, DUT, 1997
!
!  4. Argument variables
!
!     IDIR        direction of cutting
!                 1 = row
!                 2 = column
!     IPOWN       array giving the subdomain number of each gridpoint
!     LPARTS      list of parts to be created
!                    lparts(1,i) = number of i-th part to be created
!                    lparts(2,i) = size of i-th part to be created
!     MXC         maximum counter of gridpoints in x-direction
!     MYC         maximum counter of gridpoints in y-direction
!     NPART       number of parts to be created
!
   INTEGER   IDIR, MXC, MYC, NPART
   INTEGER   IPOWN(*)
   INTEGER, PARAMETER :: PART_KIND = SELECTED_INT_KIND(18)
   INTEGER(KIND=PART_KIND) LPARTS(2,*)
!
!  6. Local variables
!
!     IDIFF :     the difference to be applied to a subdomain-size
!     IENT  :     number of entries
!     IP    :     counter of parts to be splitted
!     ISPLIT:     counter of parts to be created in splitting
!     ISSUCC:     flag indicating success in reducing a difference in si
!                 0=no
!                 1=yes
!     IWORK :     see description LPARTS
!     J     :     loop counter
!     JEND  :     number of last new part to be created by splitting
!     JPARTE:     number of last part in 1..npart belonging to jpart
!     JPARTS:     number of first part in 1..npart belonging to jpart
!     JSTART:     number of first new part to be created by splitting
!     KSPLIT:     number of parts to be created in a particular splittin
!     NP    :     number of parts to be created in a particular recursio
!     NSPLIT:     maximum number of parts to be created in one splitting
!                 (NB: 2 = bisection, 4 = quadrisection)

   INTEGER IDIFF, IP, ISPLIT, ISSUCC, J, JEND, JPARTE, JPARTS,&
   &JSTART, KSPLIT, NP
   INTEGER, PARAMETER :: NSPLIT = 2
   INTEGER, SAVE :: IENT = 0
   INTEGER(KIND=PART_KIND) IWORK(2,NPART)
!
!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!     SWSTRIP          Performs a stripwise partitioning with straight
!                      interfaces
!
!  9. Subroutines calling
!
!     SWPARTIT
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
!     while not enough parts have been created, do another recursion
!
!        for each part that currently exists
!
!          determine which final parts belong to this part
!          if the number of such parts > 1, do further splitting
!
!            determine into how many parts this part must be split
!            determine the sizes and numbers of parts to be created
!
!            do splitting
!
!            determine whether objective partsizes have been modified
!            and distribute the difference over the constituent parts
!
!        swap cutting direction
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWORB')

!     --- while not enough parts have been created, do another recursion

   NP = 1
   DO WHILE (NP.LE.NPART)

!        --- for each part that currently exists

      DO IP = 1, NP

!           --- determine which final parts belong to this part

         JPARTS = (IP-1)*NPART/NP+1
         JPARTE = (IP  )*NPART/NP

!           --- if the number of such parts > 1, do further splitting

         IF ( (JPARTE-JPARTS+1).GT.1 ) THEN

!              --- determine into how many parts this part must be split

            KSPLIT = MIN(NSPLIT,JPARTE-JPARTS+1)

!              --- determine the sizes and numbers of parts to be create

            DO ISPLIT = 1, KSPLIT

               JSTART = JPARTS+(ISPLIT-1)*(JPARTE-JPARTS+1)/KSPLIT
               JEND   = JPARTS+    ISPLIT*(JPARTE-JPARTS+1)/KSPLIT-1

               IWORK(1,ISPLIT) = INT(JSTART,PART_KIND)

               IWORK(2,ISPLIT) = 0
               DO J = JSTART, JEND
                  IWORK(2,ISPLIT) = IWORK(2,ISPLIT) + LPARTS(2,J)
               END DO

            END DO

!              --- do splitting

            CALL SWSTRIP_JAC (IPOWN,IDIR,JPARTS,IWORK,MXC,MYC)

!              --- determine whether objective partsizes have been modif
!                  in SWSTRIP in order to make straight interfaces

            DO ISPLIT = 1, KSPLIT

               JSTART = JPARTS+(ISPLIT-1)*(JPARTE-JPARTS+1)/KSPLIT
               JEND   = JPARTS+    ISPLIT*(JPARTE-JPARTS+1)/KSPLIT-1

               DO J = JSTART, JEND
                  IWORK(2,ISPLIT) = IWORK(2,ISPLIT) - LPARTS(2,J)
               END DO

!                 --- and distribute the difference over the contiguous
!                     parts making sure not to cause negative subdomain-

               J      = JSTART
               ISSUCC = 0
               IF ( IWORK(2,ISPLIT).LT.0 ) THEN
                  IDIFF = -1
               ELSE
                  IDIFF =  1
               END IF

!                 --- reduce the difference until nothing is left

            DO WHILE (IWORK(2,ISPLIT).NE.0)

!                    --- only adjust parts if their size remains valid

                  IF ( LPARTS(2,J).GT.0 .AND.&
                  &((LPARTS(2,J)+INT(IDIFF,PART_KIND)).GT.0) ) THEN
                     ISSUCC          = 1
                     LPARTS(2,J)     = LPARTS(2,J) + INT(IDIFF,PART_KIND)
                     IWORK(2,ISPLIT) = IWORK(2,ISPLIT) - &
                        INT(IDIFF,PART_KIND)
                  END IF

!                    --- go on to the next part

                  J = J + 1

!                    --- when all parts have been visited, go back to fi

                  IF ( J.GT.JEND ) THEN

!                       --- check whether any reduction of the differenc
!                           was done in last pass over all parts

                     IF ( ISSUCC.EQ.0 ) THEN
                        CALL MSGERR (4,'Internal problem in SWORB')
                        RETURN
                     END IF
                     J = JSTART
                  END IF

               END DO

            END DO

         END IF

      END DO

!        --- swap cutting direction

      IDIR = MOD(IDIR,2) + 1

      NP = NSPLIT*NP
   END DO

   RETURN
end subroutine SWORB
!****************************************************************

SUBROUTINE SWPARTIT ( IPOWN, MXC, MYC )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW

!****************************************************************

   USE swan_diagnostics_level
   USE M_PARALL
   USE swan_global_grid

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Sep. 04: determines load per processor based on speed
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Carries out the partitioning of the SWAN computational grid
!
!  3. Method
!
!     Wavefront uses stripwise partitioning; Jacobi uses Orthogonal
!     Recursive Bisection.
!
!  4. Argument variables
!
!     IPOWN       array giving the subdomain number of each gridpoint
!     MXC         maximum counter of gridpoints in x-direction
!     MYC         maximum counter of gridpoints in y-direction

   INTEGER MXC, MYC
   INTEGER IPOWN(MXC,MYC)

!  6. Local variables
!
!     I     :     loop counter
!     ICNT  :     auxiliary integer to count weights
!     IDIR  :     direction of cutting
!                 1 = row
!                 2 = column
!     IENT  :     number of entries
!     IX    :     index in x-direction
!     IY    :     index in y-direction
!     IWORK :     work array with the following meaning:
!                    IWORK(1,i) = number of i-th part to be created
!                    IWORK(2,i) = size of i-th part to be created
!     NACTP :     total number of active gridpoints
!     NPCUM :     cumulative number of gridpoints

   INTEGER, SAVE :: IENT = 0
   INTEGER   I, IDIR, IX, IY
   INTEGER(KIND=SELECTED_INT_KIND(18)) ICNT, NACTP, NPCUM
   INTEGER(KIND=SELECTED_INT_KIND(18)) IWORK(2,NPROC)

!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWORB            Performs an Orthogonal Recursive Bisection partition
!     SWSTRIP          Performs a stripwise partitioning with straight
!                      interfaces


!  9. Subroutines calling
!
!     SWDECOMP
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
!     if not parallel, return
!     determine direction of cutting
!     determine number of active points
!     determine numbers and sizes of parts to be created
!     partition grid
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWPARTIT')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

!     --- determine direction of cutting

   IF ( MXC.GT.MYC ) THEN
      IDIR = 2
   ELSE
      IDIR = 1
   END IF

!     --- determine number of active points and
!         set IPOWN to 1 in these points

   NACTP = 0
   DO IX = 1, MXC
      DO IY = 1, MYC
         IF ( KGRPGL(IX,IY).NE.1 ) THEN
            IPOWN(IX,IY) = 1
            NACTP        = NACTP + 1
         END IF
      END DO
   END DO

!     --- determine numbers and sizes of parts to be created

   NPCUM = 0
   ICNT  = 0
   DO I = 1, NPROC
      ICNT       = ICNT + IWEIG(I)
      IWORK(1,I) = I
      IWORK(2,I) = (NACTP*ICNT)/SUM(IWEIG) - NPCUM
      NPCUM      = (NACTP*ICNT)/SUM(IWEIG)
   END DO
   DEALLOCATE(IWEIG)

!     --- partition grid
!
   IF (jacobi_sweep_enabled) THEN
      CALL SWORB ( IPOWN, IDIR, NPROC, IWORK, MXC, MYC )
      IF (STPNOW()) RETURN
   ELSE
      CALL SWSTRIP_WFR ( IPOWN, IDIR, NPROC, IWORK, MXC, MYC )
   END IF

   RETURN
end subroutine SWPARTIT
!****************************************************************

SUBROUTINE SWBLADM ( IPOWN, MXC, MYC )
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA

!****************************************************************

   USE swan_diagnostics_level
   USE M_PARALL
   USE swan_global_grid

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Jul. 04: determine global bounds in subdomains
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     For the present node, carries out the block administration
!     and determines array bounds with respect to global grid
!
!  3. Method
!
!     Based on domain decomposition, the interface sizes are
!     determined that is needed for the setup of block
!     administration stored as IBLKAD
!
!  4. Argument variables
!
!     IPOWN       array giving the subdomain number of each gridpoint
!     MXC         maximum counter of gridpoints in x-direction
!     MYC         maximum counter of gridpoints in y-direction

   INTEGER MXC, MYC
   INTEGER IPOWN(MXC,MYC)

!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     CHARS :     character for passing info to MSGERR
!     I     :     loop counter
!     IC    :     index of (IX,IY)-point
!     ICOFF :     offset of IC-index
!     ICRECV:     array containing positions of unknowns
!                 to be received from neighbour
!     ICSEND:     array containing positions of unknowns
!                 to be sent to neighbour
!     IDOM  :     subdomain number
!     IENT  :     number of entries
!     IF    :     first non-character in string
!     IL    :     last non-character in string
!     INB   :     neighbour counter
!     ISTART:     startaddress for each size interface in array IBLKAD
!     IX    :     index in x-direction
!     IXOFF :     offset in x-direction
!     IY    :     index in y-direction
!     IYOFF :     offset in y-direction
!     IWORK :     array used to determine interface sizes
!                   IWORK(1,i) = number of the i-th neighbour
!                   IWORK(2,i) = position of the i-th neighbour with
!                                respect to present subdomain
!                                (resp. top, bottom, right, left)
!                   IWORK(3,i) = size of interface to i-th neighbour
!     JOFFS :     offsets at which a point of a neigbhour domain can be
!     MSGSTR:     string to pass message to call MSGERR
!     MXSIZ :     size of present subdomain in x-direction
!     MYSIZ :     size of present subdomain in y-direction
!     NNEIGH:     number of neighbouring subdomains
!     NOVLU :     number of overlapping unknowns

   INTEGER, SAVE :: IENT = 0
   INTEGER      I, IC, ICOFF, IDOM, IF, IL, INB, ISTART,&
   &IX, IXOFF, IY, IYOFF, JOFFS(2,4), MXSIZ, MYSIZ,&
   &NNEIGH, NOVLU
   INTEGER      IWORK(3,NPROC),&
   &ICRECV(NPROC,MAX(MXC,MYC)),&
   &ICSEND(NPROC,MAX(MXC,MYC))
   CHARACTER(LEN=20) CHARS
   CHARACTER(LEN=80) MSGSTR

!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!     TXPBLA           Removes leading and trailing blanks in string
!
!  9. Subroutines calling
!
!     SWDECOMP
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
!     intialize offsets to be used in searching for interfaces
!     determine enclosing box of present subdomain
!     if subdomain appears to be empty
!        give warning and set empty bounding box
!     else
!        extend enclosing box to include halo area
!     localize global bounds in present subdomain
!     determine size of enclosing box
!     determine interface sizes:
!
!        loop over global grid
!           if point belongs to this part
!              for each of the four sizes
!                  if a neighbouring subdomain is found there
!                     find it in the list of neighbours
!                     if not yet in the list, add it
!                     store position of neighbour
!                     update number of overlapping unknowns
!
!     store block administration
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWBLADM')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

!     --- intialize offsets to be used in searching for interfaces

   JOFFS = RESHAPE((/0,1,0,-1,1,0,-1,0/), (/2,4/))

!     --- determine enclosing box of present subdomain
!
   IF (jacobi_sweep_enabled) THEN
      MXF = MXC+1
      MYF = MYC+1
      MXL = 0
      MYL = 0
   ELSE IF ( MXC.GT.MYC ) THEN
      MXF = MXC+1
      MXL = 0
   ELSE
      MYF = MYC+1
      MYL = 0
   END IF

   DO IX = 1, MXC
      DO IY = 1, MYC

         IF( IPOWN(IX,IY).EQ.INODE ) THEN

            MXF = MIN(IX,MXF)
            MYF = MIN(IY,MYF)
            MXL = MAX(IX,MXL)
            MYL = MAX(IY,MYL)

         END IF

      END DO
   END DO

!     --- if subdomain appears to be empty

   IF ( MXF.GT.MXL .OR. MYF.GT.MYL ) THEN

!        --- give warning and set empty bounding box

      CHARS = INTSTR(INODE)
      CALL TXPBLA(CHARS,IF,IL)
      MSGSTR = 'Empty subdomain is detected - '//&
      &' node number is '//CHARS(IF:IL)
      CALL MSGERR ( 1, MSGSTR )

      MXF = 1
      MYF = 1
      MXL = 0
      MYL = 0

   ELSE

!        --- extend enclosing box to include halo area

      MXF = MAX(1  ,MXF-IHALOX)
      MYF = MAX(1  ,MYF-IHALOY)
      MXL = MIN(MXC,MXL+IHALOX)
      MYL = MIN(MYC,MYL+IHALOY)

   END IF

!     --- localize global bounds in present subdomain

   IF ( MXCGL.GT.MYCGL ) THEN
      LMXF = MXF.EQ.1     .AND. INODE.EQ.1
      LMXL = MXL.EQ.MXCGL .AND. INODE.EQ.NPROC
      LMYF = MYF.EQ.1
      LMYL = MYL.EQ.MYCGL
   ELSE
      LMXF = MXF.EQ.1
      LMXL = MXL.EQ.MXCGL
      LMYF = MYF.EQ.1     .AND. INODE.EQ.1
      LMYL = MYL.EQ.MYCGL .AND. INODE.EQ.NPROC
   END IF

!     --- determine size of enclosing box

   MXSIZ = MXL - MXF + 1
   MYSIZ = MYL - MYF + 1

   IWORK  = 0
   ICRECV = 0
   ICSEND = 0

!     --- determine interface sizes

   DO IX = 1, MXC
      DO IY = 1, MYC

!           --- if point belongs to this part

         IF ( IPOWN(IX,IY).EQ.INODE ) THEN

!              --- for each of the four sizes

            DO I = 1, 4

               IXOFF = JOFFS(1,I)
               IYOFF = JOFFS(2,I)

!                 --- if a neighbouring subdomain is found there

               IF ( (IX+IXOFF).GT.0.AND.(IX+IXOFF).LE.MXC.AND.&
               &(IY+IYOFF).GT.0.AND.(IY+IYOFF).LE.MYC ) THEN

                  IF ( IPOWN(IX+IXOFF,IY+IYOFF).NE.0.AND.&
                  &IPOWN(IX+IXOFF,IY+IYOFF).NE.INODE ) THEN

                     IC    = (IY      -MYF)*MXSIZ + (IX      -MXF+1)
                     ICOFF = (IY+IYOFF-MYF)*MXSIZ + (IX+IXOFF-MXF+1)

!                       --- find it in the list of neighbours

                     IDOM = IPOWN(IX+IXOFF,IY+IYOFF)

                     INB = 1
                     DO WHILE ( INB.LE.NPROC .AND. &
                     &IWORK(1,INB).NE.IDOM .AND. &
                     &IWORK(1,INB).NE.0 )
                        INB = INB + 1
                     END DO

                     IF ( INB.GT.NPROC ) THEN
                        CALL MSGERR (4,'Found more neighbours than '//&
                        &'subdomains in the partitioning')
                        RETURN
                     END IF

!                       --- if not yet in the list, add it

                     IF ( IWORK(1,INB).EQ.0 ) IWORK(1,INB) = IDOM

!                       --- store position of neighbour with respect to
!                           present subdomain

                     IWORK(2,INB) = I

!                       --- update number of overlapping unknowns

                     IWORK(3,INB) = IWORK(3,INB) + 1

                     ICSEND(INB,IWORK(3,INB)) = IC
                     ICRECV(INB,IWORK(3,INB)) = ICOFF

                  END IF

               END IF

            END DO

         END IF

      END DO
   END DO

!     --- store block administration

   NNEIGH    = COUNT(IWORK(1,:)>0)
   IBLKAD(1) = NNEIGH
   ISTART    = 3*NNEIGH+2
   DO INB = 1, NNEIGH
      IBLKAD(3*INB-1) = IWORK(1,INB)
      IBLKAD(3*INB  ) = IWORK(2,INB)
      IBLKAD(3*INB+1) = ISTART
      NOVLU           = IWORK(3,INB)
      IBLKAD(ISTART)  = NOVLU
      DO I = 1, NOVLU
         IBLKAD(ISTART      +I) = ICSEND(INB,I)
         IBLKAD(ISTART+NOVLU+I) = ICRECV(INB,I)
      END DO
      ISTART = ISTART + 2*NOVLU+1
   END DO

   RETURN
end subroutine SWBLADM
!****************************************************************

SUBROUTINE SWDECOMP
   USE swan_service_interfaces, ONLY: STRACE, STPNOW

!****************************************************************

   USE swan_diagnostics_level
   USE swan_computational_grid
   USE M_PARALL
   USE swan_global_grid

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Carries out domain decomposition meant for
!     distributed-memory approach
!
!  3. Method
!
!     First, carry out the partitioning of the
!     SWAN computational grid and then do the
!     block administration
!
!  4. Argument variables
!
!     ---
!
!  6. Local variables
!
!     IENT  :     number of entries
!     IX    :     loop counter
!     IY    :     loop counter
!     IPOWN :     array giving the subdomain number of each gridpoint

   INTEGER, SAVE :: IENT = 0
   INTEGER IX, IY
   INTEGER, ALLOCATABLE :: IPOWN(:,:)

!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWBLADM          Carries out the block administration
!     SWPARTIT         Carries out the partitioning of the SWAN
!                      computational grid


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
!     allocate and initialize array for block administration
!     store the original values of MXC, MYC and MCGRD
!     if not parallel, return
!     carry out the partitioning of computational grid
!     carry out the block administration
!     compute MXC, MYC and MCGRD for each subdomain
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWDECOMP')

!     --- allocate and initialize array for block administration

   IF (.NOT.ALLOCATED(IBLKAD)) ALLOCATE(IBLKAD(41+20*MAX(MXC,MYC)))
   IBLKAD = 0

!     --- store the original values of MXC, MYC and MCGRD of
!         global computational grid

   MXCGL   = MXC
   MYCGL   = MYC
   MCGRDGL = MCGRD
   MXF     = 1
   MXL     = MXC
   MYF     = 1
   MYL     = MYC
   LMXF    = MXF.EQ.1
   LMXL    = MXL.EQ.MXCGL
   LMYF    = MYF.EQ.1
   LMYL    = MYL.EQ.MYCGL

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

!     --- carry out the partitioning of the SWAN
!         computational grid

   ALLOCATE(IPOWN(MXC,MYC))
   IPOWN = 0
   CALL SWPARTIT( IPOWN, MXC, MYC )
   IF (STPNOW()) RETURN

!     --- carry out the block administration

   CALL SWBLADM( IPOWN, MXC, MYC )
   IF (STPNOW()) RETURN

!     --- compute MXC, MYC and MCGRD for each subdomain

   MXC = MXL - MXF + 1
   MYC = MYL - MYF + 1

   MCGRD = 1
   DO IX = MXF, MXL
      DO IY = MYF, MYL
         IF ( KGRPGL(IX,IY).NE.1 ) MCGRD = MCGRD + 1
      END DO
   END DO

   DEALLOCATE(IPOWN)

   RETURN
end subroutine SWDECOMP
!****************************************************************
!
SUBROUTINE SWEXCHG_JAC ( FIELD, SWPDIR, KGRPNT )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW, SWTSTA, SWTSTO
!
!****************************************************************
!
   USE swan_diagnostics_level
   USE swan_computational_grid
   USE M_PARALL
   USE swan_global_grid
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Updates geographical field array through exchanging
!     values between neighbouring subdomains depending on
!     sweep direction
!
!  3. Method
!
!     Made use of MPI by means of SWSENDNB and SWRECVNB
!     and also block administration (stored in IBLKAD)
!
!  4. Argument variables
!
!     FIELD       geographical field array for which 'halo' values must
!                 be copied from neighbouring subdomains
!     KGRPNT      indirect addressing for grid points
!     SWPDIR      sweep direction (0=all directions together)
!
   INTEGER SWPDIR
   INTEGER KGRPNT(MXC*MYC)
   REAL    FIELD(MCGRD)
!
!  6. Local variables
!
!     IDOM  :     subdomain number
!     IENT  :     number of entries
!     INB   :     neighbour counter
!     IPNB  :     position of neighbour (=top, bottom, right, left)
!     IPR   :     array containing positions of neighbours from
!                 which data is to be received
!     IPS   :     array containing positions of neighbours to
!                 which data is to be sent
!     ISTART:     pointer in array IBLKAD
!     ISWP  :     sweep direction
!     ITAG  :     message tag for sending and receiving
!     K     :     loop counter
!     NNEIGH:     number of neighbouring subdomains
!     NOVLU :     number of overlapping unknowns
!     WORK  :     work array to store data to be sent to or
!                 received from neighbour
!
   INTEGER IDOM, INB, IPNB, ISTART, ISWP, ITAG,&
   &K, NNEIGH, NOVLU
   INTEGER, SAVE :: IENT = 0
   INTEGER IPR(2,4), IPS(2,4)
   REAL    WORK(MAX(MXC,MYC))
!
!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWRECVNB         Data is received from a neighbour
!     SWSENDNB         Data is sent to a neighbour
!     SWTSTA           Start timing for a section of code
!     SWTSTO           Stop timing for a section of code
!
!
!  9. Subroutines calling
!
!     SWCOMP
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
!     if not parallel, return
!
!     for all neighbouring subdomains do
!        get position
!        if position corresponds to sweep selection
!           get subdomain number, pointer and size
!           store data to be sent in array WORK
!           send array WORK
!
!     for all neighbouring subdomains do
!        get position
!        if position corresponds to sweep selection
!           get subdomain number, pointer and size
!           receive next array and store in WORK
!           store the received data
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWEXCHG')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

   IPR = RESHAPE((/2,4,2,3,1,3,1,4/), (/2,4/))
   IPS = RESHAPE((/1,3,1,4,2,4,2,3/), (/2,4/))

   IF (timing_enabled) CALL SWTSTA(203)

   ISWP = MAX(1,SWPDIR)

   NNEIGH = IBLKAD(1)

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get position

      IPNB = IBLKAD(3*INB)

!        --- if position corresponds to sweep selection

      IF ( SWPDIR.EQ.0 .OR.&
      &IPNB.EQ.IPS(1,ISWP) .OR. IPNB.EQ.IPS(2,ISWP) ) THEN

!           --- get subdomain number, pointer and size

         IDOM   = IBLKAD(3*INB-1)
         ISTART = IBLKAD(3*INB+1)
         NOVLU  = IBLKAD(ISTART)

!           --- store data to be sent in array WORK

         DO K = 1, NOVLU
            WORK(K) = FIELD(KGRPNT(IBLKAD(ISTART+K)))
         END DO

!           --- send array WORK

         ITAG = 2
         CALL SWSENDNB ( WORK, NOVLU, IDOM, ITAG )
         IF (STPNOW()) RETURN

      END IF

   END DO

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get position

      IPNB = IBLKAD(3*INB)

!        --- if position corresponds to sweep selection

      IF ( SWPDIR.EQ.0 .OR.&
      &IPNB.EQ.IPR(1,ISWP) .OR. IPNB.EQ.IPR(2,ISWP) ) THEN

!           --- get subdomain number, pointer and size

         IDOM   = IBLKAD(3*INB-1)
         ISTART = IBLKAD(3*INB+1)
         NOVLU  = IBLKAD(ISTART)

!           --- receive next array and store in WORK

         ITAG  = 2
         CALL SWRECVNB ( WORK, NOVLU, IDOM, ITAG )
         IF (STPNOW()) RETURN

!           --- store the received data

         DO K = 1, NOVLU
            FIELD(KGRPNT(IBLKAD(ISTART+NOVLU+K))) = WORK(K)
         END DO

      END IF

   END DO

   IF (timing_enabled) CALL SWTSTO(203)

   RETURN
end subroutine SWEXCHG_JAC
!****************************************************************
!
SUBROUTINE SWEXCHG_WFR ( FIELD, KGRPNT )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW, SWTSTA, SWTSTO
!
!****************************************************************
!
   USE swan_diagnostics_level
   USE swan_computational_grid
   USE M_PARALL
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Updates geographical field array through exchanging
!     values between neighbouring subdomains
!
!  3. Method
!
!     Made use of MPI by means of SWSENDNB and SWRECVNB
!     and also block administration (stored in IBLKAD)
!
!  4. Argument variables
!
!     FIELD       geographical field array for which 'halo' values must
!                 be copied from neighbouring subdomains
!     KGRPNT      indirect addressing for grid points
!
   INTEGER KGRPNT(MXC*MYC)
   REAL    FIELD(MCGRD)
!
!  6. Local variables
!
!     IDOM  :     subdomain number
!     IENT  :     number of entries
!     INB   :     neighbour counter
!     ISTART:     pointer in array IBLKAD
!     ITAG  :     message tag for sending and receiving
!     K     :     loop counter
!     NNEIGH:     number of neighbouring subdomains
!     NOVLU :     number of overlapping unknowns
!     WORK  :     work array to store data to be sent to or
!                 received from neighbour
!
   INTEGER IDOM, INB, ISTART, ITAG, K, NNEIGH, NOVLU
   INTEGER, SAVE :: IENT = 0
   REAL    WORK(MAX(MXC,MYC))
!
!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWRECVNB         Data is received from a neighbour
!     SWSENDNB         Data is sent to a neighbour
!     SWTSTA           Start timing for a section of code
!     SWTSTO           Stop timing for a section of code
!
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
!     if not parallel, return
!
!     for all neighbouring subdomains do
!        get subdomain number, pointer and size
!        store data to be sent in array WORK
!        send array WORK
!
!     for all neighbouring subdomains do
!        get subdomain number, pointer and size
!        receive next array and store in WORK
!        store the received data
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWEXCHG')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

   IF (timing_enabled) CALL SWTSTA(203)

   NNEIGH = IBLKAD(1)

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get subdomain number, pointer and size

      IDOM   = IBLKAD(3*INB-1)
      ISTART = IBLKAD(3*INB+1)
      NOVLU  = IBLKAD(ISTART)

!        --- store data to be sent in array WORK

      DO K = 1, NOVLU
         WORK(K) = FIELD(KGRPNT(IBLKAD(ISTART+K)))
      END DO

!        --- send array WORK

      ITAG = 2
      CALL SWSENDNB ( WORK, NOVLU, IDOM, ITAG )
      IF (STPNOW()) RETURN

   END DO

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get subdomain number, pointer and size

      IDOM   = IBLKAD(3*INB-1)
      ISTART = IBLKAD(3*INB+1)
      NOVLU  = IBLKAD(ISTART)

!        --- receive next array and store in WORK

      ITAG  = 2
      CALL SWRECVNB ( WORK, NOVLU, IDOM, ITAG )
      IF (STPNOW()) RETURN

!        --- store the received data

      DO K = 1, NOVLU
         FIELD(KGRPNT(IBLKAD(ISTART+NOVLU+K))) = WORK(K)
      END DO

   END DO

   IF (timing_enabled) CALL SWTSTO(203)

   RETURN
end subroutine SWEXCHG_WFR
!****************************************************************
!
!****************************************************************
!
SUBROUTINE SWRECVAC ( AC2, IS, J, SWPDIR, KGRPNT )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW
!
!****************************************************************
!
   USE swan_diagnostics_level
   USE swan_computational_grid
   USE swan_spectral_grid
   USE M_PARALL
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Receives action density from neighbouring subdomains
!     depending on sweep direction
!
!  3. Method
!
!     Use of SWRECVNB and block administration (stored in IBLKAD)
!
!  4. Argument variables
!
!     AC2         action density
!     IS          start index of J-th row
!     J           J-th row
!     KGRPNT      indirect addressing for grid points
!     SWPDIR      sweep direction
!
   INTEGER IS, J, SWPDIR
   INTEGER KGRPNT(MXC,MYC)
   REAL    AC2(MDC,MSC,MCGRD)
!
!  6. Local variables
!
!     IDOM  :     subdomain number
!     IENT  :     number of entries
!     INB   :     neighbour counter
!     IPNB  :     position of neighbour (=top, bottom, right, left)
!     IPR   :     array containing positions of neighbours from
!                 which data is to be received
!     ITAG  :     message tag for sending and receiving
!     NNEIGH:     number of neighbouring subdomains
!     WORK  :     work array to store data to received from neighbour
!
   INTEGER IDOM, INB, IPNB, ITAG, NNEIGH
   INTEGER, SAVE :: IENT = 0
   INTEGER IPR(2,4)
   REAL    WORK(MDC,MSC)
!
!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWRECVNB         Data is received from a neighbour
!
!
!  9. Subroutines calling
!
!     SWCOMP
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
!     if not parallel, return
!
!     for all neighbouring subdomains do
!        get position
!        if position corresponds to sweep selection
!           get subdomain number
!           receive next array and store in WORK
!           store the received data
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWRECVAC')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

   IPR = RESHAPE((/2,4,2,3,1,3,1,4/), (/2,4/))

   NNEIGH = IBLKAD(1)

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get position

      IPNB = IBLKAD(3*INB)

!        --- if position corresponds to sweep selection

      IF ( IPNB.EQ.IPR(1,SWPDIR) .OR. IPNB.EQ.IPR(2,SWPDIR) ) THEN

!           --- get subdomain number

         IDOM   = IBLKAD(3*INB-1)

!           --- receive next array and store in WORK

         ITAG  = 2
         CALL SWRECVNB ( WORK, MDC*MSC, IDOM, ITAG )
         IF (STPNOW()) RETURN

!           --- store the received data

         AC2(:,:,KGRPNT(IS,J)) = WORK(:,:)

      END IF

   END DO

   RETURN
end subroutine SWRECVAC
!****************************************************************
!
SUBROUTINE SWSENDAC ( AC2, IE, J, SWPDIR, KGRPNT )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW
!
!****************************************************************
!
   USE swan_diagnostics_level
   USE swan_computational_grid
   USE swan_spectral_grid
   USE M_PARALL
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Feb. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Sends action density to neighbouring subdomains
!     depending on sweep direction
!
!  3. Method
!
!     Use of SWSENDNB and block administration (stored in IBLKAD)
!
!  4. Argument variables
!
!     AC2         action density
!     IE          end index of J-th row
!     J           J-th row
!     KGRPNT      indirect addressing for grid points
!     SWPDIR      sweep direction
!
   INTEGER IE, J, SWPDIR
   INTEGER KGRPNT(MXC,MYC)
   REAL    AC2(MDC,MSC,MCGRD)
!
!  6. Local variables
!
!     IDOM  :     subdomain number
!     IENT  :     number of entries
!     INB   :     neighbour counter
!     IPNB  :     position of neighbour (=top, bottom, right, left)
!     IPS   :     array containing positions of neighbours to
!                 which data is to be sent
!     ITAG  :     message tag for sending and receiving
!     NNEIGH:     number of neighbouring subdomains
!     WORK  :     work array to store data to be sent to neighbour
!
   INTEGER IDOM, INB, IPNB, ITAG, NNEIGH
   INTEGER, SAVE :: IENT = 0
   INTEGER IPS(2,4)
   REAL    WORK(MDC,MSC)
!
!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWSENDNB         Data is sent to a neighbour
!
!
!  9. Subroutines calling
!
!     SWCOMP
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
!     if not parallel, return
!
!     for all neighbouring subdomains do
!        get position
!        if position corresponds to sweep selection
!           get subdomain number
!           store data to be sent in array WORK
!           send array WORK
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWSENDAC')

!     --- if not parallel, return
   IF (.NOT.PARLL) RETURN

   IPS = RESHAPE((/1,3,1,4,2,4,2,3/), (/2,4/))

   NNEIGH = IBLKAD(1)

!     --- for all neighbouring subdomains do

   DO INB = 1, NNEIGH

!        --- get position

      IPNB = IBLKAD(3*INB)

!        --- if position corresponds to sweep selection

      IF ( IPNB.EQ.IPS(1,SWPDIR) .OR. IPNB.EQ.IPS(2,SWPDIR) ) THEN

!           --- get subdomain number

         IDOM   = IBLKAD(3*INB-1)

!           --- store data to be sent in array WORK

         WORK(:,:) = AC2(:,:,KGRPNT(IE,J))

!           --- send array WORK

         ITAG = 2
         CALL SWSENDNB ( WORK, MDC*MSC, IDOM, ITAG )
         IF (STPNOW()) RETURN

      END IF

   END DO

   RETURN
end subroutine SWSENDAC
!****************************************************************

SUBROUTINE SWCOLLECT ( FIELDGL, FIELD, FULL )
   USE swan_service_interfaces, ONLY: STRACE, STPNOW

!****************************************************************

   USE swan_diagnostics_level
   USE swan_computational_grid
   USE M_GENARR
   USE M_PARALL
   USE swan_global_grid

   IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Mar. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: extended to full arrays
!
!  2. Purpose
!
!     Collects geographical FIELD arrays from all nodes
!
!  3. Method
!
!     Made use of MPI by means of SWGATHER
!
!  4. Argument variables
!
!     FIELD       geographical field array in own subdomain
!     FIELDGL     global geographical field array gathered from all nodes
!     FULL        if true, full arrays are handled otherwise 1-D compact
!                 arrays are handled

   REAL    FIELD(*), FIELDGL(*)
   LOGICAL FULL

!  6. Local variables
!
!     FLDC  :     auxiliary array for collecting data
!     IARRC :     auxiliary array for collecting grid indices and counter
!     IARRL :     auxiliary array containing grid indices and counter
!     IENT  :     number of entries
!     ILEN  :     integer indicating length of an array
!     ILEN2 :     integer indicating length of another array
!     INDX  :     pointer in array
!     INDXC :     pointer in collected array
!     IOFF1 :     offset
!     IOFF2 :     another offset
!     IP    :     node number
!     IX    :     loop counter
!     IY    :     loop counter
!     KGRPTC:     auxiliary array for collecting indirect
!                 addressing for grid points
!     MXFGL :     first index w.r.t. global grid in x-direction
!     MXLGL :     last index w.r.t. global grid in x-direction
!     MYFGL :     first index w.r.t. global grid in y-direction
!     MYLGL :     last index w.r.t. global grid in y-direction

   INTEGER, SAVE :: IENT = 0
   INTEGER ILEN, ILEN2, INDX, INDXC, IOFF1, IOFF2, IP, IX, IY,&
   &MXFGL, MXLGL, MYFGL, MYLGL
   INTEGER IARRL(5), IARRC(5,0:NPROC-1)

   INTEGER, ALLOCATABLE :: KGRPTC(:)
   REAL,    ALLOCATABLE :: FLDC(:)

!  8. Subroutines used
!
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWGATHER         Gathers different amounts of data from all nodes


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
!     if sequential run
!        just make a copy
!     else
!        gather necessary arrays
!        copy gathered data to global array in appropriate manner
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOLLECT')

   IF (.NOT.PARLL) THEN

!        --- in case of sequential run, just make a copy

      IF (FULL) THEN
         DO INDX = 1, MXC*MYC
            FIELDGL(INDX) = FIELD(INDX)
         END DO
      ELSE
         DO INDX = 1, MCGRD
            FIELDGL(INDX) = FIELD(INDX)
         END DO
      END IF

   ELSE

!        --- gather necessary arrays

      IARRL(1) = MXF
      IARRL(2) = MXL
      IARRL(3) = MYF
      IARRL(4) = MYL
      IARRL(5) = MCGRD
      CALL SWGATHER (IARRC, 5*NPROC, IARRL, 5 )
      IF (STPNOW()) RETURN

      IF (.NOT.FULL) THEN
         IF (IAMMASTER) THEN
            ILEN = MXCGL*MYCGL +&
            &4*MAX(IHALOX,IHALOY)*NPROC*MAX(MXCGL,MYCGL)
            ALLOCATE(KGRPTC(ILEN))
         END IF
         CALL SWGATHER ( KGRPTC, ILEN, KGRPNT, MXC*MYC )
         IF (STPNOW()) RETURN
      ELSE
         IF (IAMMASTER) ALLOCATE(KGRPTC(0))
      END IF

      IF (IAMMASTER) THEN
         IF (FULL) THEN
            ILEN = MXCGL*MYCGL +&
            &4*MAX(IHALOX,IHALOY)*NPROC*MAX(MXCGL,MYCGL)
         ELSE
            ILEN = MCGRDGL +&
            &4*MAX(IHALOX,IHALOY)*NPROC*MAX(MXCGL,MYCGL)
         END IF
         ALLOCATE(FLDC(ILEN))
      END IF
      IF (FULL) THEN
         ILEN2 = MXC*MYC
      ELSE
         ILEN2 = MCGRD
      END IF
      CALL SWGATHER ( FLDC, ILEN, FIELD, ILEN2 )
      IF (STPNOW()) RETURN

!        --- copy gathered data to global array in appropriate manner

      IF (IAMMASTER) THEN

         IOFF1 = 0
         IOFF2 = 0

         DO IP = 0, NPROC-1
            IF ( MXCGL.GT.MYCGL ) THEN
               IF ( IARRC(1,IP).EQ.1 .AND. IP.EQ.0 ) THEN
                  MXFGL = 1
               ELSE
                  MXFGL = IARRC(1,IP) + IHALOX
               END IF
               IF ( IARRC(3,IP).EQ.1 ) THEN
                  MYFGL = 1
               ELSE
                  MYFGL = IARRC(3,IP) + IHALOY
               END IF
               IF ( IARRC(2,IP).EQ.MXCGL .AND. IP.EQ.NPROC-1 ) THEN
                  MXLGL = MXCGL
               ELSE
                  MXLGL = IARRC(2,IP) - IHALOX
               END IF
               IF ( IARRC(4,IP).EQ.MYCGL ) THEN
                  MYLGL = MYCGL
               ELSE
                  MYLGL = IARRC(4,IP) - IHALOY
               END IF
            ELSE
               IF ( IARRC(1,IP).EQ.1 ) THEN
                  MXFGL = 1
               ELSE
                  MXFGL = IARRC(1,IP) + IHALOX
               END IF
               IF ( IARRC(3,IP).EQ.1 .AND. IP.EQ.0 ) THEN
                  MYFGL = 1
               ELSE
                  MYFGL = IARRC(3,IP) + IHALOY
               END IF
               IF ( IARRC(2,IP).EQ.MXCGL ) THEN
                  MXLGL = MXCGL
               ELSE
                  MXLGL = IARRC(2,IP) - IHALOX
               END IF
               IF ( IARRC(4,IP).EQ.MYCGL .AND. IP.EQ.NPROC-1 ) THEN
                  MYLGL = MYCGL
               ELSE
                  MYLGL = IARRC(4,IP) - IHALOY
               END IF
            END IF

            ILEN = IARRC(2,IP)-IARRC(1,IP)+1

            IF (FULL) THEN
               DO IX = MXFGL, MXLGL
                  DO IY = MYFGL, MYLGL
                     INDX  = (IY-1)*MXCGL+IX
                     INDXC = (IY-IARRC(3,IP))*ILEN+IX&
                     &-IARRC(1,IP)+1+IOFF2
                     FIELDGL(INDX)= FLDC(INDXC)
                  END DO
               END DO
            ELSE
               DO IX = MXFGL, MXLGL
                  DO IY = MYFGL, MYLGL
                     INDX  = KGRPGL(IX,IY)
                     INDXC = KGRPTC((IY-IARRC(3,IP))*ILEN+IX&
                     &-IARRC(1,IP)+1+IOFF2)
                     FIELDGL(INDX) = FLDC(INDXC+IOFF1)
                  END DO
               END DO
            END IF

            IOFF1 = IOFF1 + IARRC(5,IP)
            IOFF2 = IOFF2 + (IARRC(2,IP)-IARRC(1,IP)+1)*&
            &(IARRC(4,IP)-IARRC(3,IP)+1)

         END DO

      END IF

      IF (IAMMASTER) DEALLOCATE(KGRPTC,FLDC)

   END IF

   RETURN
end subroutine SWCOLLECT
!****************************************************************

SUBROUTINE SWCOLOUT ( OURQT, BLKND )
   USE swan_time, ONLY: DTTIME, DTINTI, DTRETI, DTTIWR
   USE swan_output_orchestration, ONLY: SWOEXC
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, EQREAL, STPNOW

!****************************************************************

   USE swan_time, ONLY: default_time_context
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_time
   USE swan_coordinate_offset
   USE swan_computational_grid_kind
   USE swan_numerics
   USE swan_computational_grid
   USE OUTP_DATA
   USE M_PARALL
   USE swan_global_grid
   USE SwanGriddata, ONLY: nvertsg

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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     41.36: Marcel Zijlema
!
!  1. Updates
!
!     40.30, May  03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: optimization output with respect to COMPGRID
!     40.51, Feb. 05: re-design output process in parallel mode
!     41.36, Jun. 06: collecting data for PunSWAN
!
!  2. Purpose
!
!     Collects output results
!
!  3. Method
!
!     Read individual process output files containing tables,
!     spectral outputs and block data and write them to
!     generic output files in appropriate manner
!
!  4. Argument variables
!
!     BLKND       collected array giving node number per subdomain
!     OURQT       array indicating at what time requested output
!                 is processed

   REAL    BLKND(MXCGL,MYCGL)
   REAL(KIND=KIND(0.0D0))  OURQT(MAX_OUTP_REQ)

!  6. Local variables
!
!     CORQ  :     current item in list of request outputs
!     CROSS :     auxiliary logical array
!     CUOPS :     current item in list of point sets
!     EXIST :     logical whether a file exist or not
!     DIF   :     difference between end and actual times
!     DTTIWR:     to write time string
!     IC    :     loop variable
!     IENT  :     number of entries
!     ILPOS :     actual length of filename
!     IP    :     loop variable
!     IPROC :     loop counter
!     IRQ   :     request number
!     IT    :     time step counter
!     IT0   :     integer indicating first step of simulation
!     IT1   :     integer indicating last step of simulation
!     ITMP1 :     auxiliary integer
!     ITMP2 :     auxiliary integer
!     ITMP3 :     auxiliary integer
!     ITMP4 :     auxiliary integer
!     ITMP5 :     auxiliary integer
!     ITMP6 :     auxiliary integer
!     IUNIT :     counter for file unit numbers
!     MIP   :     total number of output points
!     MXK   :     number of points in x-direction of output frame
!     MYK   :     number of points in y-direction of output frame
!     OPENED:     logical whether a file is open or not
!     PSTYPE:     type of point set
!     RTMP1 :     auxiliary real
!     RTMP2 :     auxiliary real
!     RTMP3 :     auxiliary real
!     RTMP4 :     auxiliary real
!     RTYPE :     type of request
!     SNAMPF:     name of plot frame
!     TNEXT :     time of next requested output
!     XC    :     computational grid x-coordinate of output point
!     XP    :     user x-coordinate of output point
!     YC    :     computational grid y-coordinate of output point
!     YP    :     user y-coordinate of output point

   INTEGER, SAVE :: IENT = 0
   INTEGER   IC, IP, IRQ, IT, IT0, IT1, IUNIT, MIP, MXK, MYK
   INTEGER   ITMP1, ITMP2, ITMP3, ITMP4, ITMP5, ITMP6
   INTEGER   ILPOS, IPROC
   REAL(KIND=KIND(0.0D0))    DIF, TNEXT
   REAL      RTMP1, RTMP2, RTMP3, RTMP4
   REAL, ALLOCATABLE :: XC(:), YC(:), XP(:), YP(:)
   LOGICAL   OPENED
   LOGICAL   EXIST
   LOGICAL, ALLOCATABLE :: CROSS(:,:)
   CHARACTER(LEN=1)  :: PSTYPE
   CHARACTER(LEN=4)  :: RTYPE
   CHARACTER(LEN=8)  :: SNAMPF
   TYPE(ORQDAT), POINTER :: CORQ
   TYPE(OPSDAT), POINTER :: CUOPS

!  8. Subroutines used
!
!     EQREAL           Logical comparing two reals
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     STRACE           Tracing routine for debugging
!     SWCOLBLK         Collects block output
!     SWCOLSPC         Collects spectral output
!     SWCOLTAB         Collects table ouput
!     SWOEXC           Computes coordinates of output points


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
!     do for all COMPUTE commands
!        do for all time steps
!           do for all output requests
!              processing of output instructions necessary for collection
!              check time of output action
!              compute coordinates of output points
!              correct problem coordinates with offset values
!              rewrite table output by means of collection of output
!              rewrite spectral output by means of collection of output
!              rewrite block output by means of collection of output
!     close all files and delete process files
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOLOUT')

   IF ( NREOQ.EQ.0 ) RETURN

!     --- do for all COMPUTE commands

   DO IC = 1, NCOMPT

      NSTATC = NINT(RCOMPT(IC,1))
      IF ( NSTATC.EQ.1 ) THEN
         IT0 = 0
      ELSE
         IT0 = 1
      END IF
      IT1   = NINT(RCOMPT(IC,2))
      default_time_context%TFINC = RCOMPT(IC,3)
      default_time_context%TINIC = RCOMPT(IC,4)
      default_time_context%DT    = RCOMPT(IC,5)
      default_time_context%TIMCO = default_time_context%TINIC

!        --- do for all time steps

      DO IT = IT0, IT1

         IF (NSTATM.GT.0) CHTIME = DTTIWR(ITMOPT, default_time_context%TIMCO)

!           --- do for all output requests

         CORQ => FORQ
         output_request_loop: do IRQ = 1, NREOQ
            request_processing: do

!              --- processing of output instructions necessary
!                  for collection
!
!              --- check time of output action

            DIF = default_time_context%TFINC - default_time_context%TIMCO
            IF ( IT.EQ.IT0 .AND. IC.EQ.1 ) THEN
               CORQ%OQR(1) = OURQT(IRQ)
            END IF
            IF (CORQ%OQR(1).LT.default_time_context%TINIC) THEN
               TNEXT = default_time_context%TINIC
            ELSE
               TNEXT = CORQ%OQR(1)
            ENDIF
            IF ( ABS(DIF).LT.0.5*default_time_context%DT .AND. CORQ%OQR(2).LT.0. ) THEN
               CORQ%OQR(1) = default_time_context%TIMCO
            ELSE IF ( CORQ%OQR(2).GT.0. .AND. default_time_context%TIMCO.GE.TNEXT ) THEN
               CORQ%OQR(1) = TNEXT + CORQ%OQR(2)
            ELSE
               EXIT request_processing
            END IF

            RTYPE  = CORQ%RQTYPE
            IF (RTYPE.EQ.'BLKV') EXIT request_processing ! no collection for VTK files
            SNAMPF = CORQ%PSNAME

            IF (SNAMPF.EQ.'COMPGRID') THEN
               LCOMPGRD=.TRUE.
            ELSE
               LCOMPGRD=.FALSE.
            ENDIF

            CUOPS => FOPS
            DO
               IF (CUOPS%PSNAME.EQ.SNAMPF) EXIT
               IF (.NOT.ASSOCIATED(CUOPS%NEXTOPS)) EXIT request_processing
               CUOPS => CUOPS%NEXTOPS
            END DO
            PSTYPE = CUOPS%PSTYPE

            IF ( PSTYPE.EQ.'F' .OR. PSTYPE.EQ.'H' ) THEN
               MXK = CUOPS%OPI(1)
               MYK = CUOPS%OPI(2)
               MIP = MXK * MYK
            ELSE IF ( PSTYPE.EQ.'C' .OR. PSTYPE.EQ.'P' .OR.&
            &PSTYPE.EQ.'N' ) THEN
               MXK = 0
               MYK = 0
               MIP = CUOPS%MIP
            ELSE IF ( PSTYPE.EQ.'U' ) THEN
               MIP = CUOPS%MIP
               IF ( LCOMPGRD ) MIP = nvertsg
               MXK = MIP
               MYK = 1
            END IF

            IF (.NOT.ALLOCATED(XC)) ALLOCATE(XC(MIP))
            IF (.NOT.ALLOCATED(YC)) ALLOCATE(YC(MIP))
            IF (.NOT.ALLOCATED(XP)) ALLOCATE(XP(MIP))
            IF (.NOT.ALLOCATED(YP)) ALLOCATE(YP(MIP))
            IF (.NOT.ALLOCATED(CROSS)) ALLOCATE(CROSS(4,MIP))

!              --- compute coordinates of output points

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
            CALL SWOEXC (PSTYPE              ,&
            &CUOPS%OPI           ,CUOPS%OPR           ,&
            &CUOPS%XP            ,CUOPS%YP            ,&
            &MIP                 ,XP                  ,&
            &YP                  ,XC                  ,&
            &YC                  ,KGRPGL              ,&
            &XGRDGL              ,YGRDGL              ,&
            &CROSS               )
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

!              --- find global vertex index for output locations
!                  in unstructured mesh and stored in array XC

            IF ( OPTG.EQ.5 .AND. .NOT.LCOMPGRD ) THEN
               XC = -1.
               YC =  0.
               FILENM = 'output.set'
               ILPOS = INDEX ( FILENM, ' ' )-1
               DO IPROC = 1, NPROC
!                    append node number to FILENM
                  WRITE(FILENM(ILPOS+1:ILPOS+4),"('-',I3.3)") IPROC
                  IUNIT = HIOPEN+NREOQ*(NPROC+1)+IPROC
                  INQUIRE ( FILE=FILENM, EXIST=EXIST, OPENED=OPENED )
                  IF ( .NOT.OPENED ) THEN
                     IF (EXIST) THEN
                        OPEN ( UNIT=IUNIT, FILE=FILENM,&
                        &FORM='UNFORMATTED' )
                     ELSE
                        CALL MSGERR( 4,&
                        &'file '//trim(FILENM)//' does not exist' )
                        RETURN
                     ENDIF
                  ENDIF
                  READ(IUNIT) ITMP1, ITMP2
                  IF (ITMP1.EQ.IRQ) THEN
                     DO IP = 1, ITMP2
                        READ (IUNIT) ITMP3, ITMP4
                        XC(ITMP3) = REAL(ITMP4) - 1.
                     ENDDO
                  ELSE
                     CALL MSGERR( 4,&
                     &'inconsistency found in SWCOLOUT: wrong request number ' )
                     RETURN
                  ENDIF
               ENDDO
            ELSE IF ( OPTG.EQ.5 ) THEN
               DO IP = 1, MIP
                  XC(IP) = REAL(IP) - 1.
                  YC(IP) = 0.
               ENDDO
            ENDIF

!              --- correct problem coordinates with offset values

            DO IP = 1, MIP
               RTMP1 = XP(IP)
               RTMP2 = YP(IP)
               IF (.NOT.EQREAL(RTMP1,OVEXCV(1))) XP(IP)=RTMP1+XOFFS
               IF (.NOT.EQREAL(RTMP2,OVEXCV(2))) YP(IP)=RTMP2+YOFFS
            END DO

!              --- rewrite table output by means of collection of
!                  output locations

            IF ( RTYPE(1:3).EQ.'TAB' ) THEN
               IF ( RTYPE.EQ.'TABC') THEN
!                 --- use "block" intermediate file facility to pass dat
                  CALL SWCOLBLK ( RTYPE, CORQ%OQI, CORQ%OQR, CORQ%IVTYP,&
                  &CORQ%FAC, SNAMPF, MIP, 1, IRQ,&
                  &BLKND, XC, YC )
                  IF (STPNOW()) RETURN
               ELSE
                  CALL SWCOLTAB ( RTYPE, CORQ%OQI, CORQ%IVTYP, MIP, IRQ,&
                  &BLKND, XC, YC, XP, YP )
                  IF (STPNOW()) RETURN
               ENDIF
            END IF

!              --- rewrite spectral output by means of collection of
!                  output locations

            IF ( RTYPE(1:2).EQ.'SP' ) THEN
               CALL SWCOLSPC ( RTYPE, CORQ%OQI, CORQ%OQR, MIP,&
               &IRQ  , BLKND   , XC      , YC )
               IF (STPNOW()) RETURN
            END IF

!              --- rewrite block output by means of collection of process
!                  output data

            IF ( RTYPE(1:3).EQ.'BLK' ) THEN
               CALL SWCOLBLK ( RTYPE, CORQ%OQI, CORQ%OQR, CORQ%IVTYP,&
               &CORQ%FAC, SNAMPF, MXK, MYK, IRQ,&
               &BLKND, XC, YC )
               IF (STPNOW()) RETURN
            END IF

            IF (ALLOCATED(XP)) DEALLOCATE(XP)
            IF (ALLOCATED(YP)) DEALLOCATE(YP)
            IF (ALLOCATED(XC)) DEALLOCATE(XC)
            IF (ALLOCATED(YC)) DEALLOCATE(YC)

            IF (ALLOCATED(CROSS)) DEALLOCATE(CROSS)

            EXIT request_processing
            end do request_processing
            CORQ => CORQ%NEXTORQ

         end do output_request_loop

         IF ( NSTATC.EQ.1.AND.IT.LT.IT1 ) default_time_context%TIMCO = default_time_context%TIMCO + default_time_context%DT

      END DO

   END DO

!     --- close all files and delete process files

   DO IUNIT = HIOPEN+1, HIOPEN+NREOQ
      INQUIRE ( UNIT=IUNIT, OPENED=OPENED )
      IF (OPENED) CLOSE(IUNIT)
   END DO
   DO IUNIT = HIOPEN+NREOQ+1, HIOPEN+NREOQ*(NPROC+1)+NPROC
      INQUIRE ( UNIT=IUNIT, EXIST=EXIST, OPENED=OPENED )
      IF (EXIST.AND.OPENED) CLOSE ( UNIT=IUNIT, STATUS='delete' )
   END DO

   RETURN
end subroutine SWCOLOUT
!****************************************************************

SUBROUTINE SWCOLTAB ( RTYPE, OQI, IVTYP, MIP, IRQ, BLKND,&
&XC   , YC , XP   , YP )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA, EQREAL

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind, ONLY: OPTG
   USE swan_numerics
   USE OUTP_DATA
   USE M_PARALL
   USE swan_global_grid

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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Agnieszka Herman
!     40.51: Marcel Zijlema
!     41.36: Marcel Zijlema
!
!  1. Updates
!
!     40.30, May  03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: optimization output with respect to COMPGRID
!     40.51, Feb. 05: further optimization
!     40.51, Feb. 05: re-design output process in parallel mode
!     41.36, Jun. 12: collecting data for PunSWAN
!
!  2. Purpose
!
!     Printing of table output based on point set by means of
!     collecting individual process output files
!
!  4. Argument variables
!
!     BLKND       collected array giving node number per subdomain
!     IRQ         request number
!     IVTYP       type of variable output
!     MIP         total number of output points
!     OQI         array containing output request data
!     RTYPE       type of output request
!     XC          computational grid x-coordinate of output point
!     XP          user x-coordinate of output point
!     YC          computational grid y-coordinate of output point
!     YP          user y-coordinate of output point

   INTEGER   IRQ, MIP, OQI(4), IVTYP(OQI(3))
   REAL      BLKND(MXCGL,MYCGL), XC(MIP), YC(MIP), XP(MIP), YP(MIP)
   CHARACTER(LEN=4) :: RTYPE

!  6. Local variables
!
!     EQREAL:     logical comparing two reals
!     EXIST :     logical whether a file exist or not
!     FSTR  :     an auxiliary string
!     I     :     integer
!     IENT  :     number of entries
!     IF    :     first non-character in string
!     IL    :     last non-character in string
!     ILPOS :     actual length of filename
!     IP    :     loop counter
!     IPROC :     loop counter
!     IUNIT :     counter for file unit numbers
!     IUT   :     auxiliary integer representing reference number
!     IVTYPE:     type of output quantity
!     IXK   :     loop counter
!     IYK   :     loop counter
!     JVAR  :     loop counter
!     LFIELD:     actual length of a part of field OUTLIN
!     MSGSTR:     string to pass message to call MSGERR
!     NLINES:     number of lines in heading
!     NREF  :     unit reference number
!     NUMDEC:     number of decimals in the table
!     NVAR  :     number of output variables
!     OPENED:     logical whether a file is open or not
!     OUTLIN:     output line
!     RVAL1 :     a value
!     RVAL2 :     another value

   INTEGER, SAVE :: IENT = 0
   INTEGER       I, IF, IL, ILPOS, IP, IPROC, IUNIT,&
   &IUT, IVTYPE, IXK, IYK, JVAR, LFIELD, NLINES,&
   &NREF, NUMDEC, NVAR
   REAL          RVAL1, RVAL2
      LOGICAL :: EXIST, OPENED
   CHARACTER(LEN=80)  MSGSTR
   CHARACTER(LEN=18)  FSTR
   CHARACTER(LEN=512) OUTLIN

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!     TXPBLA           Removes leading and trailing blanks in string
!
!  9. Subroutines calling
!
!     SWCOLOUT
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
!     if generic output file exist
!
!        if it is not open or already open thru another request
!
!           open generic output file or reset reference number
!
!           count lines of heading
!
!           open individual process output files and
!           write heading to generic output file
!
!     read output data from the proper process file and write
!     it in appropriate manner to generic output file
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOLTAB')

   NREF   = OQI(1)
   NVAR   = OQI(3)
   NLINES = 0

!     --- if generic output file exist

   IF ( NREF.NE.0 .AND. NREF.NE.PRINTF ) THEN

      FILENM = OUTP_FILES(OQI(2))
      ILPOS  = INDEX(FILENM, '-0')
      IF (ILPOS.NE.0) FILENM = FILENM(1:ILPOS-1)
      INQUIRE ( FILE=FILENM, OPENED=OPENED, NUMBER=IUT )

!        --- if it is not open or already open thru another request

      IF ( .NOT.OPENED .OR. NREF.LE.HIOPEN ) THEN

!           --- open generic output file or reset reference number

         IF ( .NOT.OPENED ) THEN
            NREF = HIOPEN + IRQ
            OPEN ( UNIT=NREF, FILE=FILENM )
         ELSE
            NREF = IUT
         END IF
         OQI(1) = NREF

!           --- count lines of heading

         IF ( RTYPE.NE.'TABD' ) THEN
            IF ( RTYPE.EQ.'TABP' .OR. RTYPE.EQ.'TABI' ) THEN
               NLINES = NLINES + 7
            ELSE IF ( RTYPE.EQ.'TABT' .OR. RTYPE.EQ.'TABS' ) THEN
               NLINES = NLINES + 5
               IF ( RTYPE.EQ.'TABT' ) THEN
                  NLINES = NLINES + 1
               ELSE
                  IF ( NSTATM.EQ.1 ) NLINES = NLINES + 2
                  NLINES = NLINES + 2 + MIP
               END IF
               DO JVAR = 1, NVAR
                  IVTYPE = IVTYP(JVAR)
                  IF ( OVSVTY(IVTYPE).LE.2 ) THEN
                     NLINES = NLINES + 3
                  ELSE
                     NLINES = NLINES + 6
                  END IF
               END DO
            END IF
         END IF

!           --- open individual process output files and
!               write heading to generic output file

         FILENM = OUTP_FILES(OQI(2))
         ILPOS  = INDEX ( FILENM, ' ' )-1
         DO IPROC = 1, NPROC
            I = IPROC
            WRITE(FILENM(ILPOS-3:ILPOS),"('-',I3.3)") I
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            INQUIRE ( FILE=FILENM, EXIST=EXIST, OPENED=OPENED )
            IF ( .NOT.OPENED ) THEN
               IF (EXIST) THEN
                  OPEN ( UNIT=IUNIT, FILE=FILENM )
               ELSE
                  MSGSTR= 'file '//FILENM(1:ILPOS)//' does not exist'
                  CALL MSGERR( 4, MSGSTR )
                  RETURN
               END IF
            END IF
            DO IP = 1, NLINES
               READ (IUNIT,'(A)') OUTLIN
               CALL TXPBLA(OUTLIN,IF,IL)
               IF (IPROC.EQ.1) WRITE (NREF, '(A)') OUTLIN(1:IL)
            END DO
         END DO

      END IF

   END IF

!     --- read output data from the proper process file and write
!         it in appropriate manner to generic output file

   IF ( NREF.NE.PRINTF ) THEN
      IF ( RTYPE.EQ.'TABS' .AND. NSTATM.EQ.1 ) THEN
         DO IPROC = 1, NPROC
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            READ (IUNIT,'(A)') OUTLIN
            CALL TXPBLA(OUTLIN,IF,IL)
            IF (IPROC.EQ.1) WRITE (NREF, '(A)') OUTLIN(1:IL)
         END DO
      END IF
      IPLOOP : DO IP = 1, MIP
         IXK = INT(XC(IP))
         IYK = INT(YC(IP))
         IF (OPTG.NE.5) THEN
            RVAL1 = FLOAT(IXK)
            RVAL2 = FLOAT(IYK)
            IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR.&
            &EQREAL(XC(IP),0.   ) ) IXK = IXK + 1
            IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR.&
            &EQREAL(YC(IP),0.   ) ) IYK = IYK + 1
         ELSE
            IXK = IXK + 1
            IYK = IYK + 1
         ENDIF
         IF ( IXK.LT.1 .OR. IYK.LT.1 .OR. IXK.GT.MXCGL .OR.&
         &IYK.GT.MYCGL ) THEN
            CALL WREXCV
            CYCLE IPLOOP
         END IF
         IPROC = 1
         PROCLOOP : DO
            IF ( NINT(BLKND(IXK,IYK)).EQ.IPROC ) THEN
               IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
               READ (IUNIT,'(A)') OUTLIN
               CALL TXPBLA(OUTLIN,IF,IL)
               WRITE (NREF, '(A)') OUTLIN(1:IL)
               EXIT PROCLOOP
            ELSE
               IPROC = IPROC + 1
               IF ( IPROC.LE.NPROC ) THEN
                  CYCLE PROCLOOP
               ELSE
                  CALL WREXCV
                  EXIT PROCLOOP
               END IF
            END IF
         END DO PROCLOOP
      END DO IPLOOP
   END IF
   RETURN

CONTAINS
   SUBROUTINE WREXCV
      USE swan_output_formats, ONLY: FLT_TABLE, FLD_TABLE
      IL = 1
      OUTLIN = '    '
      IF (RTYPE.EQ.'TABI') THEN
!        --- write point sequence number as first column
         WRITE (OUTLIN(1:8), '(I8)') IP
         IL = 9
         OUTLIN(IL:IL) = ' '
      END IF
      DO JVAR = 1, NVAR
         IVTYPE = IVTYP(JVAR)
         IF (IVTYPE.EQ.40) THEN
!           --- for time, 18 characters are needed
            FSTR   = '(A18)'
            LFIELD = 18
            OUTLIN(IL:IL+LFIELD-1) = CHTIME
         ELSE
            IF (RTYPE.EQ.'TABD') THEN
               FSTR   = FLT_TABLE
               LFIELD = FLD_TABLE
            ELSE
               FSTR   = '(F11.X)'
               LFIELD = 11
               NUMDEC = MAX (0,6-NINT(LOG10(ABS(OVHEXP(IVTYPE)))))
               IF (NUMDEC.GT.9) NUMDEC = 9
               WRITE (FSTR(6:6), '(I1)') NUMDEC
            END IF
!          --- write value into OUTLIN
            IF (IVTYPE.EQ.1) THEN
               WRITE (OUTLIN(IL:IL+LFIELD-1), FMT=FSTR) XP(IP)
            ELSE IF (IVTYPE.EQ.2) THEN
               WRITE (OUTLIN(IL:IL+LFIELD-1), FMT=FSTR) YP(IP)
            ELSE
               WRITE (OUTLIN(IL:IL+LFIELD-1), FMT=FSTR) OVEXCV(IVTYPE)
            END IF
            IF (OVSVTY(IVTYPE).EQ.3) THEN
               IL = IL + LFIELD + 1
!             --- write second component of a vectorial quantity
               WRITE (OUTLIN(IL:IL+LFIELD-1), FMT=FSTR) OVEXCV(IVTYPE)
            END IF
         END IF
         IL = IL + LFIELD + 1
         OUTLIN(IL-1:IL) = '  '
      END DO
      CALL TXPBLA(OUTLIN,IF,IL)
      WRITE (NREF, '(A)') OUTLIN(1:IL)
      RETURN
   end subroutine WREXCV

end subroutine SWCOLTAB
!****************************************************************

SUBROUTINE SWCOLSPC ( RTYPE, OQI, OQR, MIP, IRQ, BLKND, XC, YC )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA, EQREAL

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind, ONLY: OPTG
   USE swan_coordinate_offset, ONLY: XOFFS, YOFFS
   USE swan_numerics
   USE swan_spectral_grid
   USE OUTP_DATA
   USE M_PARALL
   USE swan_global_grid
   USE swan_netcdf_output_backend, ONLY: is_netcdf_filename, &
   &swn_outnc_colspc
   USE SwanGriddata, ONLY: xcugrdgl, ycugrdgl

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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.51: Agnieszka Herman
!     40.51: Marcel Zijlema
!     41.36: Marcel Zijlema
!
!  1. Updates
!
!     40.30, May  03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: optimization output with respect to COMPGRID
!     40.51, Feb. 05: further optimization
!     40.51, Feb. 05: re-design output process in parallel mode
!     41.36, Jun. 12: collecting data for PunSWAN
!
!  2. Purpose
!
!     Printing of spectral output based on point set by means of
!     collecting individual process output files
!
!  4. Argument variables
!
!     BLKND       collected array giving node number per subdomain
!     IRQ         request number
!     MIP         total number of output points
!     OQI         integer array containing output request data
!     OQR         real array containing output request data
!     RTYPE       type of output request
!     XC          computational grid x-coordinate of output point
!     YC          computational grid y-coordinate of output point

   INTEGER   IRQ, MIP, OQI(4)
   REAL(KIND=KIND(0.0D0))    OQR(2)
   REAL      BLKND(MXCGL,MYCGL), XC(MIP), YC(MIP)
   CHARACTER(LEN=4) :: RTYPE

!  6. Local variables
!
!     EMPTY :     logical whether a line is empty or not
!     EQREAL:     logical comparing two reals
!     EXIST :     logical whether a file exist or not
!     I     :     integer
!     IBLKN :     integer giving node number per subdomain
!     IENT  :     number of entries
!     IF    :     first non-character in string
!     IL    :     last non-character in string
!     ILPOS :     actual length of filename
!     IP    :     loop counter
!     IPROC :     loop counter
!     IS    :     loop counter
!     IUNIT :     counter for file unit numbers
!     IUT   :     auxiliary integer representing reference number
!     IXK   :     loop counter
!     IYK   :     loop counter
!     MSGSTR:     string to pass message to call MSGERR
!     NCF   :     true if netCDF file
!     NLINES:     number of lines in heading
!     NREF  :     unit reference number
!     OPENED:     logical whether a file is open or not
!     OTYPE :     integer indicating dimension of spectrum
!     OUTLIN:     output line
!     RVAL1 :     a value
!     RVAL2 :     another value

   INTEGER, SAVE :: IENT = 0
   INTEGER       I, IBLKN, IF, IL, ILPOS, IP, IPROC, IS,&
   &IUNIT, IUT, IXK, IYK, NLINES, NREF, OTYPE
   REAL          RVAL1, RVAL2
      LOGICAL :: EMPTY, EXIST, OPENED
   LOGICAL, SAVE :: NCF = .FALSE.
   CHARACTER(LEN=80)  MSGSTR
   CHARACTER (LEN=LENSPO) OUTLIN
   CHARACTER (LEN=8) :: CRFORM = '(2F14.4)'

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!     STPNOW           Logical indicating whether program must
!     swn_outnc_colspc Collect spectral output for netcdf
!     TXPBLA           Removes leading and trailing blanks in string
!
!
!  9. Subroutines calling
!
!     SWCOLOUT
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
!     if generic output file exist
!
!        if it is not open or already open thru another request
!
!           open generic output file or reset reference number
!
!           count lines of heading
!
!           open individual process output files and
!           write heading to generic output file
!
!     read output data from the proper process file and write
!     it in appropriate manner to generic output file
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOLSPC')

   FILENM = OUTP_FILES(OQI(2))
   NCF = is_netcdf_filename(FILENM)
!
   IF ( NCF ) THEN
      CALL swn_outnc_colspc( RTYPE, OQI, OQR, MIP, KGRPGL )
      RETURN
   ENDIF

   NREF   = OQI(1)
   NLINES = 0

!     --- if generic output file exist

   IF ( NREF.NE.0 ) THEN

      FILENM = OUTP_FILES(OQI(2))
      ILPOS  = INDEX(FILENM, '-0')
      IF (ILPOS.NE.0) FILENM = FILENM(1:ILPOS-1)
      INQUIRE ( FILE=FILENM, OPENED=OPENED, NUMBER=IUT )

!        --- if it is not open or already open thru another request

      IF ( .NOT.OPENED .OR. NREF.LE.HIOPEN ) THEN

!           --- open generic output file or reset reference number

         IF ( .NOT.OPENED ) THEN
            NREF = HIOPEN + IRQ
            OPEN ( UNIT=NREF, FILE=FILENM )
         ELSE
            NREF = IUT
         END IF
         OQI(1) = NREF

!           --- count lines of first part of heading

         NLINES = NLINES + 5
         IF ( LCOMPGRD    ) NLINES = NLINES - 1
         IF ( NSTATM.EQ.1 ) NLINES = NLINES + 2

!           --- open individual process output files and
!               write heading to generic output file

         FILENM = OUTP_FILES(OQI(2))
         ILPOS  = INDEX ( FILENM, ' ' )-1
         DO IPROC = 1, NPROC
            I = IPROC
            WRITE(FILENM(ILPOS-3:ILPOS),"('-',I3.3)") I
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            INQUIRE ( FILE=FILENM, EXIST=EXIST, OPENED=OPENED )
            IF ( .NOT.OPENED ) THEN
               IF (EXIST) THEN
                  OPEN ( UNIT=IUNIT, FILE=FILENM )
               ELSE
                  MSGSTR= 'file '//FILENM(1:ILPOS)//' does not exist'
                  CALL MSGERR( 4, MSGSTR )
                  RETURN
               END IF
            END IF
            DO IP = 1, NLINES
               READ (IUNIT,'(A)') OUTLIN
               CALL TXPBLA(OUTLIN,IF,IL)
               IF (IPROC.EQ.1) WRITE (NREF, '(A)') OUTLIN(1:IL)
            END DO
         END DO

!           --- write number of locations in case of whole unstructured
         IF ( OPTG.EQ.5 .AND. LCOMPGRD ) THEN
            WRITE (NREF,'(I6,T41,A)') MIP, 'number of locations'
         ENDIF

!           --- write coordinates of output points to generic
!               output file

         IF ( OPTG.NE.5 .OR. .NOT.LCOMPGRD ) THEN
            DO IP = 1, MIP
               IF ( XC(IP).LT.-0.01 .OR. YC(IP).LT.-0.01 .OR.&
               &XC(IP).GT.REAL(MXCGL-1)+0.01 .OR.&
               &YC(IP).GT.REAL(MYCGL-1)+0.01 ) THEN
                  IBLKN = NPROC+1
               ELSE
                  IXK = INT(XC(IP))
                  IYK = INT(YC(IP))
                  IF (OPTG.NE.5) THEN
                     RVAL1 = FLOAT(IXK)
                     RVAL2 = FLOAT(IYK)
                     IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR.&
                     &EQREAL(XC(IP),0.   ) ) IXK = IXK + 1
                     IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR.&
                     &EQREAL(YC(IP),0.   ) ) IYK = IYK + 1
                  ELSE
                     IXK = IXK + 1
                     IYK = IYK + 1
                  ENDIF
                  IBLKN = NINT(BLKND(IXK,IYK))
               END IF
               EMPTY = .TRUE.
               DO IPROC = 1, NPROC
                  IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
                  READ (IUNIT,'(A)') OUTLIN
                  CALL TXPBLA(OUTLIN,IF,IL)
                  IF (EMPTY.AND.IBLKN.EQ.IPROC) THEN
                     WRITE (NREF, '(A)') OUTLIN(1:IL)
                     EMPTY = .FALSE.
                  END IF
               END DO
               IF (EMPTY) WRITE (NREF, '(A)') OUTLIN(1:IL)
            END DO
         ELSE
            DO IP = 1, MIP
               WRITE (NREF, FMT=CRFORM) DBLE(xcugrdgl(IP)+XOFFS),&
               &DBLE(ycugrdgl(IP)+YOFFS)
            ENDDO
         ENDIF

!           --- count lines of rest of heading and write heading
!               to generic output file

         NLINES = 2 + MSC
         IF (RTYPE(4:4).EQ.'C') THEN
            NLINES = NLINES + 7 + MDC
         ELSE
            NLINES = NLINES + 11
         END IF
         DO IPROC = 1, NPROC
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            DO IP = 1, NLINES
               READ (IUNIT,'(A)') OUTLIN
               CALL TXPBLA(OUTLIN,IF,IL)
               IF (IPROC.EQ.1) WRITE (NREF, '(A)') OUTLIN(1:IL)
            END DO
         END DO

      END IF

   END IF

   IF (RTYPE(4:4).EQ.'C') THEN
      IF (RTYPE.EQ.'SPEC') THEN
         OTYPE = -2
      ELSE
         OTYPE =  2
      END IF
   ELSE
      IF (RTYPE.EQ.'SPE1') THEN
         OTYPE = -1
      ELSE
         OTYPE =  1
      END IF
   END IF

!     --- read output data from the proper process file and write
!         it in appropriate manner to generic output file

   IF ( NSTATM.EQ.1 ) THEN
      DO IPROC = 1, NPROC
         IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
         READ (IUNIT,'(A)') OUTLIN
         CALL TXPBLA(OUTLIN,IF,IL)
         IF (IPROC.EQ.1) WRITE (NREF, '(A)') OUTLIN(1:IL)
      END DO
   END IF

   IPLOOP : DO IP = 1, MIP
      IXK = INT(XC(IP))
      IYK = INT(YC(IP))
      IF (OPTG.NE.5) THEN
         RVAL1 = FLOAT(IXK)
         RVAL2 = FLOAT(IYK)
         IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR.&
         &EQREAL(XC(IP),0.   ) ) IXK = IXK + 1
         IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR.&
         &EQREAL(YC(IP),0.   ) ) IYK = IYK + 1
      ELSE
         IXK = IXK + 1
         IYK = IYK + 1
      ENDIF
      IF ( IXK.LT.1 .OR. IYK.LT.1 .OR. IXK.GT.MXCGL .OR.&
      &IYK.GT.MYCGL ) THEN
         WRITE (NREF, '(A)') 'NODATA'
         CYCLE IPLOOP
      END IF
      IPROC = 1
      PROCLOOP : DO
         IF ( NINT(BLKND(IXK,IYK)).EQ.IPROC ) THEN
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            READ (IUNIT,'(A)') OUTLIN
            CALL TXPBLA(OUTLIN,IF,IL)
            WRITE (NREF, '(A)') OUTLIN(1:IL)
            IF ( OUTLIN(IF:IL).NE.'NODATA' ) THEN
               IF ( ABS(OTYPE).EQ.1 ) THEN
                  DO IS = 1, MSC
                     READ (IUNIT,'(A)') OUTLIN
                     CALL TXPBLA(OUTLIN,IF,IL)
                     WRITE (NREF, '(A)') OUTLIN(1:IL)
                  END DO
               ELSE
                  IF ( OUTLIN(IF:IL).NE.'ZERO' ) THEN
                     DO IS = 1, 1+MSC
                        READ (IUNIT,'(A)') OUTLIN
                        CALL TXPBLA(OUTLIN,IF,IL)
                        WRITE (NREF, '(A)') OUTLIN(1:IL)
                     END DO
                  END IF
               END IF
            END IF
            EXIT PROCLOOP
         ELSE
            IPROC = IPROC + 1
            IF ( IPROC.LE.NPROC ) THEN
               CYCLE PROCLOOP
            ELSE
               WRITE (NREF, '(A)') 'NODATA'
               EXIT PROCLOOP
            END IF
         END IF
      END DO PROCLOOP
   END DO IPLOOP

   RETURN
end subroutine SWCOLSPC
!****************************************************************

SUBROUTINE SWCOLBLK ( RTYPE , OQI, OQR, IVTYP, FAC  ,&
&PSNAME, MXK, MYK, IRQ  , BLKND,&
&XC    , YC )
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_output_writers, ONLY: SBLKPT, SRAWPT, SWRMAT
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA, EQREAL, TABHED

!****************************************************************

   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid_kind, ONLY: OPTG
   USE swan_numerics, ONLY: NSTATM
   USE swan_computational_grid
   USE swan_spherical_geometry, ONLY: KSPHER
   USE OUTP_DATA
   USE M_PARALL
   USE swan_global_grid
   USE SwanGridData, ONLY: XCUGRDGL, YCUGRDGL
   USE swan_netcdf_output_backend

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
!     40.51: Marcel Zijlema
!     41.36: Marcel Zijlema
!     41.62: Andre van der Westhuysen
!
!  1. Updates
!
!     40.31, Dec. 03: New subroutine
!     40.41, Jun. 04: some improvements with respect to MATLAB
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: optimization output with respect to COMPGRID
!     40.51, Feb. 05: further optimization
!     40.51, Feb. 05: re-design output process in parallel mode
!     41.36, Jun. 12: collecting data for PunSWAN
!     41.62, Nov. 15: included output fields for wave partitioning
!
!  2. Purpose
!
!     Writing of block output by means of collecting
!     individual process output files
!
!  4. Argument variables
!
!     BLKND       collected array giving node number per subdomain
!     FAC         factors of multiplication of block output
!     IRQ         request number
!     IVTYP       type of variable output
!     MXK         number of points in x-direction of output frame
!     MYK         number of points in y-direction of output frame
!     OQI         integer array containing output request data
!     OQR         real array containing output request data
!     PSNAME      name of output locations
!     RTYPE       type of output request
!     XC          computational grid x-coordinate of output point
!     YC          computational grid y-coordinate of output point

   INTEGER   MXK, MYK, IRQ, OQI(4), IVTYP(OQI(3))
   REAL      BLKND(MXCGL,MYCGL), XC(MXK*MYK), YC(MXK*MYK)
   REAL(KIND=KIND(0.0D0))    OQR(2)
   REAL      FAC(OQI(3))
   CHARACTER(LEN=4) :: RTYPE
   CHARACTER(LEN=8) :: PSNAME

!  6. Local variables
!
!     CTIM  :     string representing date of computation
!     DFAC  :     multiplication factor of block output
!     EQREAL:     logical comparing two reals
!     EXIST :     logical whether a file exist or not
!     FMAX  :     auxiliary real
!     FTIP  :     auxiliary real
!     FTIP1 :     auxiliary real
!     FTIP2 :     auxiliary real
!     I     :     integer
!     IBLKN :     integer giving node number per subdomain
!     IDLA  :     lay-out indicator
!     IF    :     first non-character in string
!     IFAC  :     auxiliary integer
!     IENT  :     number of entries
!     IL    :     last non-character in string
!     ILPOS :     actual length of filename
!     IP    :     loop counter
!     IPD   :     switch for printing on paper or writing to file
!     IPROC :     loop counter
!     IREC  :     direct access file record counter
!     IUNIT :     counter for file unit numbers
!     IUT   :     auxiliary integer representing reference number
!     IVTYPE:     type of output quantity
!     IXK   :     loop counter
!     IYK   :     loop counter
!     JVAR  :     loop counter
!     MATLAB:     indicates whether binary Matlab files are used
!     MSGSTR:     string to pass message to call MSGERR
!     NAMVAR:     name of MATLAB variable
!     NREF  :     unit reference number
!     NVAR  :     number of output variables
!     OPENED:     logical whether a file is open or not
!     RVAL1 :     a value
!     RVAL2 :     another value
!     VOQ   :     collected output variables

   INTEGER, SAVE :: IENT = 0
   INTEGER      I, IBLKN, IDLA, IF, IFAC, IL, ILPOS, IP, IPD,&
   &IPROC, IUNIT, IUT, IXK, IYK, IVTYPE, J, JVAR, NREF,&
   &NVAR
   REAL         DFAC, FMAX, FTIP, FTIP1, FTIP2
   REAL         RVAL1, RVAL2
      LOGICAL :: EXIST, OPENED
   INTEGER, SAVE :: IREC(MAX_OUTP_REQ)=0
   LOGICAL, SAVE :: MATLAB=.FALSE.
   LOGICAL, SAVE :: NCF   =.FALSE.
   LOGICAL, SAVE :: RAWPRT=.FALSE.
   CHARACTER(LEN=80) MSGSTR
   CHARACTER (LEN=20) :: CTIM
   CHARACTER (LEN=30) :: NAMVAR
   CHARACTER (LEN=80) :: HTXT(3)
   REAL, ALLOCATABLE :: VOQ(:,:)
   INTEGER VOQR(NMOVAR)

!  8. Subroutines used
!
!     MSGERR           Writes error message
!     SBLKPT           Writes block output to an ASCII file
!     STRACE           Tracing routine for debugging
!     SWRMAT           Writes block output to a binary Matlab file
!     TABHED           Prints heading
!     TXPBLA           Removes leading and trailing blanks in string
!
!  9. Subroutines calling
!
!     SWCOLOUT
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
!     if generic output file exist
!
!        if it is not open or already open thru another request
!
!           open generic output file or reset reference number
!
!           open individual process output files
!
!     read data from the proper process file and write
!     it in appropriate manner to generic output file
!
! 13. Source text

   IF (LTRACE) CALL STRACE (IENT,'SWCOLBLK')

   NREF = OQI(1)
   NVAR = OQI(3)
   IDLA = OQI(4)

   IF ( RTYPE.EQ.'BLKP' ) THEN
      IPD = 1
      IF (NREF.EQ.PRINTF) CALL TABHED ('SWAN', PRINTF)
   ELSE IF ( RTYPE.EQ.'BLKD' ) THEN
      IPD = 2
   ELSE
      IPD = 3
   ENDIF

!     --- if generic output file exist

   IF ( NREF.NE.0 .AND. NREF.NE.PRINTF ) THEN

      FILENM = OUTP_FILES(OQI(2))
      ILPOS  = INDEX(FILENM, '-0')
      IF (ILPOS.NE.0) FILENM = FILENM(1:ILPOS-1)
      INQUIRE ( FILE=FILENM, OPENED=OPENED, NUMBER=IUT )
      MATLAB = INDEX( FILENM, '.MAT' ).NE.0 .OR.&
      &INDEX (FILENM, '.mat' ).NE.0
      NCF = is_netcdf_filename(FILENM)
      RAWPRT = INDEX( FILENM, '.RAW' ).NE.0 .OR.&
      &INDEX (FILENM, '.raw' ).NE.0

!        --- if it is not open or already open thru another request

      IF ( .NOT.OPENED .OR. NREF.LE.HIOPEN ) THEN

!           --- open generic output file or reset reference number
!
         IF (NCF .AND. .NOT.OPENED) THEN
            IUT    = HIOPEN + IRQ
            OQI(1) = IUT
            IF (OPTG.NE.5) THEN
               CALL swn_outnc_openblockfile(FILENM, MYK, MXK,&
               &OVLNAM, XGRDGL, YGRDGL,&
               &OQI, OQR, IVTYP, IRQ)
            ELSE
               CALL swn_outnc_openblockfile(FILENM, MYK, MXK,&
               &OVLNAM, XCUGRDGL, YCUGRDGL,&
               &OQI, OQR, IVTYP, IRQ)
            ENDIF
            OPENED = .TRUE.
         END IF

         IF ( .NOT.OPENED ) THEN
            NREF = HIOPEN + IRQ
            OPEN ( UNIT=NREF, FILE=FILENM )
         ELSE
            NREF = IUT
         END IF
         OQI(1) = NREF

         IF (MATLAB .AND. .NOT.OPENED) THEN
            CLOSE(NREF)
            OPEN(UNIT=NREF, FILE=FILENM, FORM='UNFORMATTED',&
            &STATUS='REPLACE',&
            &ACCESS='DIRECT', RECL=matlab_direct_record_length)
            IREC(IRQ) = 1
         END IF

         IF (RAWPRT .AND. IPD.EQ.1 .AND. .NOT.OPENED) THEN
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
            WRITE(NREF,'(A16,A24,A50)') TRIM(HTXT(1)), TRIM(HTXT(2)),&
            &TRIM(HTXT(3))
            WRITE(NREF,'(A31,A20)') '        hs     tp     lp       ',&
            &'theta     sp      wf'
         END IF

!           --- open individual process output files

         FILENM = OUTP_FILES(OQI(2))
         ILPOS  = INDEX ( FILENM, ' ' )-1
         DO IPROC = 1, NPROC
            I = IPROC
            WRITE(FILENM(ILPOS-3:ILPOS),"('-',I3.3)") I
            IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
            INQUIRE ( FILE=FILENM, EXIST=EXIST, OPENED=OPENED )
            IF ( .NOT.OPENED ) THEN
               IF (EXIST) THEN
                  OPEN ( UNIT=IUNIT, FILE=FILENM, FORM='UNFORMATTED')
               ELSE
                  MSGSTR= 'file '//FILENM(1:ILPOS)//' does not exist'
                  CALL MSGERR( 4, MSGSTR )
                  RETURN
               END IF
            END IF
         END DO

      END IF

   END IF

!     --- read data from the proper process file and write
!         it in appropriate manner to generic output file

   CTIM = CHTIME
   CALL TXPBLA(CTIM,IF,IL)
   CTIM(9:9)='_'

   IF (.NOT.RAWPRT) THEN
      ALLOCATE(VOQ(MXK*MYK,2))
   ELSE
      ALLOCATE(VOQ(MXK*MYK,NMOVAR))
      VOQR=(/ (J, J=1, NMOVAR) /)
   ENDIF

   JLOOP: DO JVAR = 1, NVAR

      IVTYPE = IVTYP(JVAR)
      DFAC   = FAC(JVAR)

      IF (.NOT.RAWPRT) THEN
         VOQ = OVEXCV(IVTYPE)
         J   = 1
      ELSE
         J   = IVTYPE
      ENDIF

      DO IP = 1, MXK*MYK
         IXK = INT(XC(IP))
         IYK = INT(YC(IP))
         IF (OPTG.NE.5) THEN
            RVAL1 = FLOAT(IXK)
            RVAL2 = FLOAT(IYK)
            IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR.&
            &EQREAL(XC(IP),0.   ) ) IXK = IXK + 1
            IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR.&
            &EQREAL(YC(IP),0.   ) ) IYK = IYK + 1
         ELSE
            IXK = IXK + 1
            IYK = IYK + 1
         ENDIF
         IF ( IXK.LT.1 .OR. IYK.LT.1 .OR. IXK.GT.MXCGL .OR.&
         &IYK.GT.MYCGL ) THEN
            IBLKN = 0
            IPROC = NPROC+1
         ELSE
            IBLKN = NINT(BLKND(IXK,IYK))
            IPROC = 1
         END IF
         PROCLOOP1 : DO
            IF ( IBLKN.EQ.IPROC ) THEN
               IUNIT = HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
               READ (IUNIT) VOQ(IP,J)
               EXIT PROCLOOP1
            ELSE
               IPROC = IPROC + 1
               IF ( IPROC.LE.NPROC ) THEN
                  CYCLE PROCLOOP1
               ELSE
                  EXIT PROCLOOP1
               END IF
            END IF
         END DO PROCLOOP1
      END DO

      IF ( OVSVTY(IVTYPE).GE.3 ) THEN

         DO IP = 1, MXK*MYK
            IXK = INT(XC(IP))
            IYK = INT(YC(IP))
            IF (OPTG.NE.5) THEN
               RVAL1 = FLOAT(IXK)
               RVAL2 = FLOAT(IYK)
               IF ( .NOT.EQREAL(XC(IP),RVAL1) .OR.&
               &EQREAL(XC(IP),0.   ) ) IXK = IXK + 1
               IF ( .NOT.EQREAL(YC(IP),RVAL2) .OR.&
               &EQREAL(YC(IP),0.   ) ) IYK = IYK + 1
            ELSE
               IXK = IXK + 1
               IYK = IYK + 1
            ENDIF
            IF ( IXK.LT.1 .OR. IYK.LT.1 .OR. IXK.GT.MXCGL .OR.&
            &IYK.GT.MYCGL ) THEN
               IBLKN = 0
               IPROC = NPROC+1
            ELSE
               IBLKN = NINT(BLKND(IXK,IYK))
               IPROC = 1
            END IF
            PROCLOOP2 : DO
               IF ( IBLKN.EQ.IPROC ) THEN
                  IUNIT=HIOPEN+NREOQ+(NREF-HIOPEN-1)*NPROC+IPROC
                  READ (IUNIT) VOQ(IP,J+1)
                  EXIT PROCLOOP2
               ELSE
                  IPROC = IPROC + 1
                  IF ( IPROC.LE.NPROC ) THEN
                     CYCLE PROCLOOP2
                  ELSE
                     EXIT PROCLOOP2
                  END IF
               END IF
            END DO PROCLOOP2
         END DO
      END IF

      IF (RAWPRT) CYCLE JLOOP

      IF ( IPD.EQ.1 ) THEN
         IF ( DFAC.LE.0. ) THEN
            IF ( OVHEXP(IVTYPE).LT.0.5E10 ) THEN
               IFAC = INT (10.+LOG10(OVHEXP(IVTYPE))) - 13
            ELSE
               IF ( OVSVTY(IVTYPE).EQ.1 ) THEN
                  FMAX = 1.E-8
                  DO IP = 1, MXK*MYK
                     FTIP = ABS(VOQ(IP,1))
                     FMAX = MAX (FMAX, FTIP)
                  END DO
               ELSE IF ( OVSVTY(IVTYPE).EQ.2 ) THEN
                  FMAX = 1000.
               ELSE IF ( OVSVTY(IVTYPE).EQ.3 ) THEN
                  FMAX = 1.E-8
                  DO IP = 1, MXK*MYK
                     FTIP1 = ABS(VOQ(IP,1))
                     FTIP2 = ABS(VOQ(IP,2))
                     FMAX  = MAX (FMAX, FTIP1, FTIP2)
                  END DO
               END IF
               IFAC = INT (10.+LOG10(FMAX)) - 13
            END IF
            DFAC = 10.**IFAC
         END IF
      ELSE
         IF ( DFAC.LE.0. ) DFAC = 1.
      END IF

      IF (OVSVTY(IVTYPE) .LT. 3) THEN
         IF (MATLAB) THEN
            IF (IL.EQ.1 .OR. IVTYPE.LT.3 .OR. IVTYPE.EQ.52) THEN
               NAMVAR = OVSNAM(IVTYPE)
            ELSE
               NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
               &'_'//CTIM
            END IF
            CALL SWRMAT( MYK, MXK, NAMVAR, VOQ(1,1), NREF,&
            &IREC(IRQ), IDLA, OVEXCV(IVTYPE) )
         ELSE IF (NCF) THEN
            IF ( IVTYPE.GT.2.AND.IVTYPE.NE.40 ) THEN
               CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
               &IRQ, VOQ(1,1),&
               &OVEXCV(IVTYPE), 1)
            END IF
         ELSE
            CALL SBLKPT( IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
            &MXK, MYK, IDLA, OVLNAM(IVTYPE), VOQ(1,1) )
         END IF
      ELSE
         IF (MATLAB) THEN
            IF (IL.EQ.1) THEN
               NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
               &'_x'
            ELSE
               NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
               &'_x_'//CTIM
            END IF
            CALL SWRMAT( MYK, MXK, NAMVAR,&
            &VOQ(1,1), NREF, IREC(IRQ), IDLA, OVEXCV(IVTYPE) )
            IF (IL.EQ.1) THEN
               NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
               &'_y'
            ELSE
               NAMVAR = OVSNAM(IVTYPE)(1:LEN_TRIM(OVSNAM(IVTYPE)))//&
               &'_y_'//CTIM
            END IF
            CALL SWRMAT( MYK, MXK, NAMVAR,&
            &VOQ(1,2), NREF, IREC(IRQ), IDLA, OVEXCV(IVTYPE) )
         ELSE IF (NCF) THEN
            IF ( IVTYPE.GT.3 ) THEN
               CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
               &IRQ, VOQ(1,1),&
               &OVEXCV(IVTYPE), 1)
               CALL swn_outnc_appendblock(MYK, MXK, IVTYPE, OQI(1),&
               &IRQ, VOQ(1,2),&
               &OVEXCV(IVTYPE), 2)
            END IF
         ELSE
            CALL SBLKPT( IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
            &MXK, MYK, IDLA, OVLNAM(IVTYPE)//'X-comp',&
            &VOQ(1,1) )
            CALL SBLKPT( IPD, NREF, DFAC, PSNAME, OVUNIT(IVTYPE),&
            &MXK, MYK, IDLA, OVLNAM(IVTYPE)//'Y-comp',&
            &VOQ(1,2) )
         END IF
      END IF

   END DO JLOOP

!     generate a dump of the raw partition data, if appropriate
   IF (RAWPRT) CALL SRAWPT ( NREF, VOQR, VOQ, MXK, MYK )

   IF ( NCF ) CALL swn_outnc_close_on_end(OQI(1), IRQ)

   IF (IPD.EQ.1 .AND. NREF.EQ.PRINTF) WRITE (PRINTF, "(///)")

   DEALLOCATE(VOQ)


   RETURN
end subroutine SWCOLBLK
!****************************************************************
!
SUBROUTINE SWBLKCOL ( MCOLR, KGRPNT )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA
   USE swan_number_formatting, ONLY: INTSTR
!
!****************************************************************
!
   USE swan_diagnostics_level
   USE swan_io_units
   USE swan_computational_grid
   USE M_PARALL
   USE swan_global_grid
!
   IMPLICIT NONE
!
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
!
!  0. Authors
!
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.30, Mar. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Colours the subdomains with red, yellow, green and black
!     in order to determine the sequence of sweeps during the
!     iteration process
!
!  3. Method
!
!     The four-colour ordering scheme is based on colouring
!     subdomains in each direction in alternating way with
!     red and black (similar to a chessboard colouring),
!     whereafter the product of resulting color directions
!     is taken.
!     Initially, the most left and under blocks are coloured
!     red, whereas other are uncoloured. Then, the colors
!     are propagated from left to right and from bottom to
!     top. Finally, when both directions of all subdomains
!     have been coloured, the subdomains are coloured by
!     taking the product of two color directions.
!
!  4. Argument variables
!
!     KGRPNT      indirect addressing for grid points
!     MCOLR       flag to indicate multi-colouring
!                 of subdomains (.TRUE.) or not (.FALSE.)
!
   INTEGER KGRPNT(MXC,MYC)
   LOGICAL MCOLR
!
!  5. Parameter variables
!
!     ITERMAX:    maximum number of iterations
!     IWHITE:     integer used to colour subdomains 'white'
!
   INTEGER, PARAMETER :: IWHITE=0, ITERMAX=100
!
!  6. Local variables
!
!     CHARS :     character for passing info to MSGERR
!     ICOLNB:     color of neighbouring subdomain
!     ICONV :     indicator for convergence (0=yes, 1=no)
!     IENT  :     number of entries
!     IF    :     first non-character in string
!     IL    :     last non-character in string
!     ITER  :     iteration count
!     IXCOL :     color in x-direction of own subdomain
!     IYCOL :     color in y-direction of own subdomain
!     MSGSTR:     string to pass message to call MSGERR
!     XCOL  :     field array containing present color in x-direction
!     YCOL  :     field array containing present color in y-direction
!
   INTEGER      ICOLNB, ICONV, IF, IL, ITER, IXCOL, IYCOL
   INTEGER, SAVE :: IENT = 0
   CHARACTER(LEN=20) CHARS
   CHARACTER(LEN=80) MSGSTR
   REAL, ALLOCATABLE :: XCOL(:), YCOL(:)
!
!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!     SWEXCHG          Updates geographical field array through
!                      exchanging values between subdomains
!     SWREDUCE         Performs a global reduction
!     TXPBLA           Removes leading and trailing blanks in string
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
!     if not parallel or no colouring, return
!
!     initially, both x- and y-direction of the most left
!     and under subdomains are coloured red and that of all
!     other subdomains are marked white, i.e. not being
!     coloured
!
!     while not all subdomains are coloured do
!        exchange colors of both directions between subdomains
!        adjust color in x-direction of own subdomain
!        adjust color in y-direction of own subdomain
!        check whether all subdomains have been coloured
!
!     if not all subdomains have been coloured gives message and stops
!
!     finally, subdomains are coloured by taking the product of two
!     color directions
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWBLKCOL')

   IBCOL = IRED

!     --- if not parallel or no colouring, return
   IF (.NOT.PARLL .OR. .NOT.MCOLR) RETURN

   ALLOCATE(XCOL(MCGRD))
   ALLOCATE(YCOL(MCGRD))

!     --- initially, both x- and y-direction of the most left
!         and under subdomains are coloured red and that of all
!         other subdomains are marked white, i.e. not being
!         coloured

   IF ( MXF.EQ.1 ) THEN
      IXCOL = IRED
   ELSE
      IXCOL = IWHITE
   END IF
   IF ( MYF.EQ.1 ) THEN
      IYCOL = IRED
   ELSE
      IYCOL = IWHITE
   END IF

!     --- while not all subdomains are coloured, adjust the
!         color of each direction of own subdomain depending
!         on the color directions of left and under neighbours

   DO ITER = 1, ITERMAX

      ICONV = 0

!        --- exchange colors of both directions between subdomains

      XCOL = REAL(IXCOL)
      YCOL = REAL(IYCOL)
      CALL SWEXCHG_JAC ( XCOL, 0, KGRPNT )
      CALL SWEXCHG_JAC ( YCOL, 0, KGRPNT )

!        --- adjust color in x-direction of own subdomain

      IF ( IXCOL.EQ.IWHITE ) THEN

         ICOLNB = NINT(XCOL(KGRPNT(1,2)))
         IF ( ICOLNB.EQ.IRED ) THEN
            IXCOL = IBLACK
         ELSE IF ( ICOLNB.EQ.IBLACK ) THEN
            IXCOL = IRED
         ELSE
            ICONV = 1
         END IF

      END IF

!        --- adjust color in y-direction of own subdomain

      IF ( IYCOL.EQ.IWHITE ) THEN

         ICOLNB = NINT(YCOL(KGRPNT(2,1)))
         IF ( ICOLNB.EQ.IRED ) THEN
            IYCOL = IBLACK
         ELSE IF ( ICOLNB.EQ.IBLACK ) THEN
            IYCOL = IRED
         ELSE
            ICONV = 1
         END IF

      END IF

!        --- check whether all subdomains have been coloured

      CALL SWREDUCE( ICONV, 1, SWMAX )
      IF ( ICONV.EQ.0 ) EXIT

   END DO

!     --- if not all subdomains have been coloured
!         gives message and stops

   IF (ICONV.NE.0 .AND. (IXCOL.EQ.IWHITE .OR. IYCOL.EQ.IWHITE)) THEN
      CHARS = INTSTR(INODE)
      CALL TXPBLA(CHARS,IF,IL)
      MSGSTR = 'Subdomain '//CHARS(IF:IL)//&
      &'has not been coloured'
      CALL MSGERR ( 4, MSGSTR )
      RETURN
   END IF

!     --- finally, subdomains are coloured by taking the product
!         of two color directions

   IF ( IXCOL.EQ.IRED .AND. IYCOL.EQ.IRED ) THEN
      IBCOL = IRED
   ELSE IF ( IXCOL.EQ.IBLACK .AND. IYCOL.EQ.IRED ) THEN
      IBCOL = IYELOW
   ELSE IF ( IXCOL.EQ.IBLACK .AND. IYCOL.EQ.IBLACK ) THEN
      IBCOL = IGREEN
   ELSE IF ( IXCOL.EQ.IRED .AND. IYCOL.EQ.IBLACK ) THEN
      IBCOL = IBLACK
   END IF

   DEALLOCATE(XCOL,YCOL)

   RETURN
end subroutine SWBLKCOL

end module swan_parallel
