module swan_read_adc_grid
   use swan_io_limits, only: LENFNM
   implicit none(type, external)
   private
   public :: SwanReadADCGrid
contains

subroutine SwanReadADCGrid
   USE swan_file_opening, ONLY: FOR
   USE swan_service_interfaces, ONLY: MSGERR, STPNOW, STRACE

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
!     it under the terms of the GNU General Public License as published by
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
!   Authors
!
!   40.80: Marcel Zijlema
!   41.07: Casey Dietrich
!   42.08: Casey Dietrich
!
!   Updates
!
!   40.80, December 2007: New subroutine
!   41.07,   August 2009: use ADCIRC boundary info to mark all boundary vertices
!   42.08, December 2023: use global-to-local tables from ADCIRC
!
!   Purpose
!
!   Reads ADCIRC grid described in fort.14
!
!   Method
!
!   Grid coordinates of vertices are read from file fort.14 and stored in Swan data structure
!   Vertices of triangles are read from file fort.14 and stored in Swan data structure
!
!   Bottom topography from file fort.14 will also be stored
!
!   Modules used

    use ocpcomm2
    use ocpcomm4
    use m_genarr
    use SwanGriddata

    implicit none(type, external)

!   Local variables

    character(lenfnm)       :: grdfil   ! name of grid file including path
    integer, save           :: ient = 0 ! number of entries in this subroutine
    integer                 :: idum     ! dummy integer
    integer                 :: ii       ! auxiliary integer
    integer                 :: iostat   ! I/O status in call FOR
    integer                 :: istat    ! indicate status of allocation
    integer                 :: itype    ! ADCIRC boundary type
    integer                 :: ivert    ! vertex index
    integer                 :: ivert1   ! another vertex index
    integer                 :: j        ! loop counter
    integer                 :: k        ! loop counter
    integer                 :: n1       ! auxiliary integer
    integer                 :: n2       ! another auxiliary integer
    integer                 :: ndsd     ! unit reference number of file
    integer                 :: nopbc    ! number of open boundaries in ADCIRC
    integer                 :: vm       ! boundary marker
    character(80)           :: line     ! auxiliary textline

!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanReadADCGrid')

    ! open file fort.14

    ndsd   = 0
    iostat = 0
    grdfil = 'fort.14'
    call for (ndsd, grdfil, 'OF', iostat)
    if (stpnow()) return

    ! skip first line

    read(ndsd,'(a80)', iostat=iostat) line
    if (read_failed(iostat)) return

    ! read number of elements and number of vertices

    read(ndsd, *, iostat=iostat) ncells, nverts
    if (read_failed(iostat)) return
    istat = 0
    if(.not.allocated(xcugrd)) allocate (xcugrd(nverts), stat = istat)
    if ( istat == 0 ) then
       if(.not.allocated(ycugrd)) allocate (ycugrd(nverts), stat = istat)
    endif
    if ( istat == 0 ) then
       if(.not.allocated(DEPTH)) allocate (DEPTH(nverts), stat = istat)
    endif
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadADCGrid: array xcugrd, ycugrd or depth ' )
       return
    endif

    ! read coordinates of vertices and bottom topography

    do j = 1, nverts
       read(ndsd, *, iostat=iostat) ii, xcugrd(ii), ycugrd(ii), DEPTH(ii)
       if (read_failed(iostat)) return
       if ( ii/=j ) call msgerr ( 1, 'numbering of vertices is not sequential in grid file fort.14 ' )
    enddo

    if(.not.allocated(kvertc)) allocate (kvertc(3,ncells), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadADCGrid: array kvertc ' )
       return
    endif

    ! read vertices of triangles

    do j = 1, ncells
       read(ndsd, *, iostat=iostat) ii, idum, kvertc(1,ii), kvertc(2,ii), kvertc(3,ii)
       if (read_failed(iostat)) return
       if ( ii/=j ) call msgerr ( 1, 'numbering of triangles is not sequential in grid file fort.14 ' )
    enddo

    if(.not.allocated(vmark)) allocate (vmark(nverts), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadADCGrid: array vmark ' )
       return
    endif
    vmark = 0

    ! read ADCIRC boundary information and store boundary markers

    read(ndsd, *, iostat=iostat) nopbc
    if (read_failed(iostat)) return
    read(ndsd, *, iostat=iostat) idum
    if (read_failed(iostat)) return
    do j = 1, nopbc
       vm = j
       read(ndsd, *, iostat=iostat) n2
       if (read_failed(iostat)) return
       do k = 1, n2
           read(ndsd, *, iostat=iostat) ivert
           if (read_failed(iostat)) return
           vmark(ivert) = vm
       enddo
    enddo

    read(ndsd, *, iostat=iostat) n1
    if (read_failed(iostat)) return
    read(ndsd, *, iostat=iostat) idum
    if (read_failed(iostat)) return
    do j = 1, n1
       vm = nopbc + j
       read(ndsd, *, iostat=iostat) n2, itype
       if (read_failed(iostat)) return
       if ( itype /= 4 .and. itype /= 24 ) then
          do k = 1, n2
             read(ndsd, *, iostat=iostat) ivert
             if (read_failed(iostat)) return
             vmark(ivert) = vm
          enddo
       else
          do k = 1, n2
             read(ndsd, *, iostat=iostat) ivert, ivert1
             if (read_failed(iostat)) return
             vmark(ivert ) = vm
             vmark(ivert1) = vm
          enddo
       endif
    enddo

    ! close file fort.14

    close(ndsd)

contains

    logical function read_failed(status)
       integer, intent(in) :: status

       read_failed = status /= 0
       if (.not.read_failed) return
       if (is_iostat_end(status)) then
          call msgerr (4, 'unexpected end of file in grid file fort.14')
       else
          call msgerr (4, 'error reading data from grid file fort.14')
       endif
    end function read_failed

end subroutine SwanReadADCGrid

end module swan_read_adc_grid
