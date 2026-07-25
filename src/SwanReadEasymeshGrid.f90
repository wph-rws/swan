module swan_read_easymesh_grid
   use swan_io_limits, only: LENFNM
   implicit none(type, external)
   private
   public :: SwanReadEasymeshGrid
contains

subroutine SwanReadEasymeshGrid ( basenm, lenfnm )
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
!
!   Updates
!
!   40.80, July 2007: New subroutine
!
!   Purpose
!
!   Reads Easymesh grid described in <name>.n and <name>.e
!
!   Method
!
!   Grid coordinates of vertices are read from file <name>.n and stored in Swan data structure
!   Vertices of triangles are read from file <name>.e and stored in Swan data structure
!
!   Modules used

    USE swan_diagnostics_level
    use SwanGriddata

    implicit none(type, external)

!   Argument variables

    integer, intent(in)           :: lenfnm ! length of file names
    character(lenfnm), intent(in) :: basenm ! base name of Easymesh files

!   Local variables

    character(lenfnm) :: filenm   ! file name
    integer, save     :: ient = 0 ! number of entries in this subroutine
    integer           :: iostat   ! I/O status in call FOR
    integer           :: istat    ! indicate status of allocation
    integer           :: j        ! loop counter
    integer           :: ndsd     ! unit reference number of file
    character(80)     :: line     ! auxiliary textline

!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanReadEasymeshGrid')

    ! open file <name>.n containing the coordinates of vertices

    filenm = trim(basenm)//'.n'
    ndsd   = 0
    iostat = 0
    call for (ndsd, filenm, 'OF', iostat)
    if (stpnow()) return

    ! read first line to determine number of vertices

    read(ndsd, *, iostat=iostat) nverts
    if (read_failed(iostat)) return
    istat = 0
    if(.not.allocated(xcugrd)) allocate (xcugrd(nverts), stat = istat)
    if ( istat == 0 ) then
       if(.not.allocated(ycugrd)) allocate (ycugrd(nverts), stat = istat)
    endif
    if ( istat == 0 ) then
       if(.not.allocated(vmark)) allocate (vmark(nverts), stat = istat)
    endif
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadEasymeshGrid: array xcugrd, ycugrd or vmark ' )
       return
    endif

    ! read coordinates of vertices and boundary marker

    do j = 1, nverts
       read(ndsd, "((6x,2e22.15,i3))", iostat=iostat) xcugrd(j), ycugrd(j), vmark(j)
       if (read_failed(iostat)) return
    enddo

    ! close file <name>.n

    close(ndsd)

    ! open file <name>.e containing the (Delaunay) triangles

    filenm = trim(basenm)//'.e'
    ndsd   = 0
    iostat = 0
    call for (ndsd, filenm, 'OF', iostat)
    if (stpnow()) return

    ! read first line to determine number of triangles

    read(ndsd, *, iostat=iostat) ncells
    if (read_failed(iostat)) return
    if(.not.allocated(kvertc)) allocate (kvertc(3,ncells), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadEasymeshGrid: array kvertc ' )
       return
    endif

    ! read vertices of triangles

    do j = 1, ncells
       read(ndsd, "((5x,3i5,a))", iostat=iostat) kvertc(1,j), kvertc(2,j), kvertc(3,j), line
       if (read_failed(iostat)) return
    enddo

    ! close file <name>.e

    close(ndsd)

    ! Easymesh counters vertices starting from 0 (C style), therefore add 1

    kvertc = kvertc + 1

contains

    logical function read_failed(status)
       integer, intent(in) :: status

       read_failed = status /= 0
       if (.not.read_failed) return
       inquire (unit=ndsd, name=filenm)
       if (is_iostat_end(status)) then
          call msgerr (4, 'unexpected end of file in Easymesh file '//filenm)
       else
          call msgerr (4, 'error reading data from Easymesh file '//filenm)
       endif
    end function read_failed

end subroutine SwanReadEasymeshGrid

end module swan_read_easymesh_grid
