!  The main program, kept apart from the driver it calls.
!
!  SWMAIN and everything under it live in swanmain.f90, which is part of the
!  library. That is what lets a test call the driver twice in one process --
!  the thing this whole migration is aimed at. As long as PROGRAM SWAN sat in
!  the same file, that file could only be linked into an executable and no test
!  could reach SWMAIN at all.
!
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
