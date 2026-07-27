module swan_read_triangle_grid
   use swan_io_limits, only: LENFNM
   implicit none(type, external)
   private
   public :: SwanReadTriangleGrid
contains

subroutine SwanReadTriangleGrid ( basenm, lenfnm )
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
!   Reads Triangle grid described in <name>.node and <name>.ele
!
!   Method
!
!   Grid coordinates of vertices are read from file <name>.node and stored in Swan data structure
!   Vertices of triangles are read from file <name>.ele and stored in Swan data structure
!
!   Modules used

    use ocpcomm4
    use SwanGriddata

    implicit none(type, external)

!   Argument variables

    integer, intent(in)           :: lenfnm ! length of file names
    character(lenfnm), intent(in) :: basenm ! base name of Triangle files

!   Local variables

    character(lenfnm) :: filenm   ! file name
    integer, save     :: ient = 0 ! number of entries in this subroutine
    integer           :: idum     ! dummy integer
    integer           :: ii       ! auxiliary integer
    integer           :: iostat   ! I/O status in call FOR
    integer           :: istat    ! indicate status of allocation
    integer           :: j        ! loop counter
    integer           :: nattr    ! number of attributes
    integer           :: nbmark   ! number of boundary markers (0 or 1)
    integer           :: ndim     ! dimension of Triangle (must be 2)
    integer           :: ndsd     ! unit reference number of file
    integer           :: nnodes   ! number of nodes per triangle
    real              :: rdum     ! dummy value
    character(80)     :: line     ! auxiliary textline

!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanReadTriangleGrid')

    ! open file <name>.node containing the coordinates of vertices

    filenm = trim(basenm)//'.node'
    ndsd   = 0
    iostat = 0
    call for (ndsd, filenm, 'OF', iostat)
    if (stpnow()) return

    ! read first line to determine number of vertices

    read(ndsd, *, iostat=iostat) nverts, ndim, nattr, nbmark
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
       call msgerr ( 4, 'Allocation problem in SwanReadTriangleGrid: array xcugrd, ycugrd or vmark ' )
       return
    endif

    ! check if boundary marker has been specified

    if ( nbmark == 0 ) then
       call msgerr ( 4, 'boundary marker for vertices/faces must be specified ' )
       return
    endif

    ! read coordinates of vertices and boundary marker

    if ( nattr == 0 ) then
       do j = 1, nverts
          read(ndsd, *, iostat=iostat) ii, xcugrd(ii), ycugrd(ii), vmark(ii)
          if (read_failed(iostat)) return
          if ( ii/=j ) call msgerr ( 1, 'numbering of vertices is not sequential in Triangle file '//filenm )
       enddo
    else
       do j = 1, nverts
          read(ndsd, *, iostat=iostat) ii, xcugrd(ii), ycugrd(ii), rdum, vmark(ii)
          if (read_failed(iostat)) return
          if ( ii/=j ) call msgerr ( 1, 'numbering of vertices is not sequential in Triangle file '//filenm )
       enddo
    endif

    ! close file <name>.node

    close(ndsd)

    ! open file <name>.ele containing the (Delaunay) triangles

    filenm = trim(basenm)//'.ele'
    ndsd   = 0
    iostat = 0
    call for (ndsd, filenm, 'OF', iostat)
    if (stpnow()) return

    ! read first line to determine number of triangles

    read(ndsd, *, iostat=iostat) ncells, nnodes, nattr
    if (read_failed(iostat)) return
    if(.not.allocated(kvertc)) allocate (kvertc(3,ncells), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanReadTriangleGrid: array kvertc ' )
       return
    endif

    ! read vertices of triangles

    if ( nnodes == 3 .and. nattr == 0 ) then
       do j = 1, ncells
          read(ndsd, *, iostat=iostat) ii, kvertc(1,ii), kvertc(2,ii), kvertc(3,ii)
          if (read_failed(iostat)) return
          if ( ii/=j ) call msgerr ( 1, 'numbering of triangles is not sequential in Triangle file '//filenm )
       enddo
    else
       do j = 1, ncells
          read(ndsd, *, iostat=iostat) ii, kvertc(1,ii), kvertc(2,ii), kvertc(3,ii), line
          if (read_failed(iostat)) return
          if ( ii/=j ) call msgerr ( 1, 'numbering of triangles is not sequential in Triangle file '//filenm )
       enddo
    endif

    ! close file <name>.ele

    close(ndsd)

contains

    logical function read_failed(status)
       integer, intent(in) :: status

       read_failed = status /= 0
       if (.not.read_failed) return
       inquire (unit=ndsd, name=filenm)
       if (is_iostat_end(status)) then
          call msgerr (4, 'unexpected end of file in Triangle file '//filenm)
       else
          call msgerr (4, 'error reading data from Triangle file '//filenm)
       endif
    end function read_failed

end subroutine SwanReadTriangleGrid

end module swan_read_triangle_grid
