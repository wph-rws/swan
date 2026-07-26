module swan_grid_topology
   use swan_grid_cell, only: SwanGridCell
   use swan_grid_face, only: SwanGridFace
   use swan_grid_vert, only: SwanGridVert
   use swan_print_grid_info, only: SwanPrintGridInfo
   implicit none(type, external)
   private
   public :: SwanGridTopology
contains

subroutine SwanGridTopology
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

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
!   Setups the SWAN grid topology
!
!   Method
!
!   Returns information about the grid and the topology of the
!   region in a structure useful to the rest of the program
!
!   Modules used

    use ocpcomm4
    use SwanGriddata
    use SwanGridobjects
    use SwanSpatialIndex, only: SwanSpatialIndexReset

    implicit none(type, external)

!   Local variables

    integer, save :: ient = 0 ! number of entries in this subroutine
    integer       :: istat    ! indicate status of allocation

!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanGridTopology')

    ! cached coordinates and boundary faces belong to the previous topology;
    ! invalidate them even if the new grid happens to have the same size

    call SwanSpatialIndexReset

    ! allocate arrays vert, cell and face

    allocate(gridobject%vert_grid(nverts), stat = istat)
    if ( istat == 0 ) allocate(gridobject%cell_grid(ncells), stat = istat)
    if ( istat == 0 ) allocate(gridobject%face_grid(nfaces), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanGridTopology: array vert, cell or face' )
       return
    endif

    ! setup the vertices

    call SwanGridVert ( nverts, xcugrd, ycugrd, vmark )

    ! setup the cells

    call SwanGridCell ( ncells, nverts, xcugrd, ycugrd, kvertc )

    ! setup the faces

    call SwanGridFace ( nfaces, ncells, nverts, xcugrd, ycugrd, kvertf )

    ! print some info about the grid

    call SwanPrintGridInfo

end subroutine SwanGridTopology

end module swan_grid_topology
