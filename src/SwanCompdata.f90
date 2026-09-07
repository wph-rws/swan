module SwanCompdata

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
!   40.92: Marcel Zijlema
!   43.05: Marcel Zijlema
!
!   Updates
!
!   40.80,    July 2007: New Module
!   40.92,    June 2008: changes with respect to boundary polygons
!   43.05, January 2023: wavefront and graph-level scheduling
!
!   Purpose
!
!   Module containing data for computation with unstructured grid
!
!   Method
!
!   Data based on unstructured grid
!
!   Modules used

    ! MICMAX is all this module needs: it is the extent of the stencil array
    ! vs below. Importing swan_stencil wholesale re-exported the whole of it,
    ! because a module without PRIVATE passes on everything it imports.
    use swan_stencil, only: MICMAX
    use swan_front_scheduling_backend, only: flist, fptr, fronte, fronts, nfront

    implicit none(type, external)

!   Module parameters
!
!
!   Module variables

    integer                                    :: nbpol  ! total number of boundary polygons
    integer, dimension(10000)                  :: nbpt   ! number of boundary vertices for each boundary polygon
    integer, dimension(:,:), save, allocatable :: blist  ! list of boundary vertices in ascending order for each boundary polygon
    integer, dimension(:,:), save, allocatable :: bmark  ! list of corresponding boundary markers for each boundary polygon
    integer, dimension(:,:), save, allocatable :: bvertg ! global index of boundary vertex in own subdomain
    integer, dimension(:,:), save, allocatable, target :: vlist  ! vertex list

!   Source text

contains

   subroutine CLEAR_COMPUTATION_DATA ()
      if (allocated(vlist )) deallocate(vlist )
      if (allocated(blist )) deallocate(blist )
      if (allocated(bvertg)) deallocate(bvertg)
      if (allocated(bmark )) deallocate(bmark )
   end subroutine CLEAR_COMPUTATION_DATA

   logical function COMPUTATION_DATA_IS_CLEAR ()
      COMPUTATION_DATA_IS_CLEAR = .not.allocated(vlist) .and. &
         .not.allocated(blist) .and. .not.allocated(bvertg) .and. &
         .not.allocated(bmark)
   end function COMPUTATION_DATA_IS_CLEAR

end module SwanCompdata
