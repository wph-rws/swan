module swan_find_point
   use swan_pointin_mesh, only: SwanPointinMesh
   implicit none
   private
   public :: SwanFindPoint
contains

subroutine SwanFindPoint ( x, y, kvert )
   USE swan_service_interfaces, ONLY: STRACE

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
!   40.80, June 2007: New subroutine
!
!   Purpose
!
!   Finds the closest vertex index of the given point
!
!   Modules used

    use ocpcomm4
    use swcomm3
    use SwanGriddata
    use SwanGridobjects
    use SwanSpatialIndex

    implicit none

!   Argument variables

    integer, intent(out) :: kvert ! closest vertex index of given point
                                  ! Note: kvert = -1 indicate point is not found
    real, intent(in)     :: x     ! x-coordinate of given point
    real, intent(in)     :: y     ! y-coordinate of given point

!   Local variables

    integer, save                         :: ient = 0        ! number of entries in this subroutine
    integer                               :: iface           ! loop counter over faces
    integer                               :: nscan           ! number of boundary/full faces to scan
    integer                               :: v1              ! first vertex of present face
    integer                               :: v2              ! second vertex of present face

    real                                  :: dxb             ! x-component of length of boundary face
    real                                  :: dyb             ! y-component of length of boundary face
    real                                  :: r               ! relative distance of point to begin of boundary face
    real                                  :: reldis          ! relative distance of point to boundary face
    real                                  :: x1              ! x-coordinate of begin of boundary face
    real                                  :: x2              ! x-coordinate of end of boundary face
    real                                  :: y1              ! y-coordinate of begin of boundary face
    real                                  :: y2              ! y-coordinate of end of boundary face

!  (local LOGICAL declaration removed: SwanPointinMesh is now a module function)
    logical                               :: usecache        ! boundary-face cache is available

    type(facetype), dimension(:), pointer :: face            ! datastructure for faces with their attributes

!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanFindPoint')

    ! point to face object

    face => gridobject%face_grid

    ! check whether point is outside the grid

    if ( x < XCGMIN .or. x > XCGMAX .or. y < YCGMIN .or. y > YCGMAX ) then

       kvert = -1
       return

    endif

    if ( SwanPointinMesh( x, y ) ) then

       ! if point is inside the mesh then compute closest index
       ! (bucket index; returns the same vertex as a linear scan)

       call SwanNearestVertex ( x, y, kvert )

    else

       ! scan the boundary to look for the given point

       call SwanBndFaceCache
       usecache = nbfac >= 0
       if ( usecache ) then
          nscan = nbfac
       else
          ! allocation-failure fallback: retain the former full-face scan
          nscan = nfaces
       endif
       kvert = -1

       ! loop over cached boundary faces (or all faces on cache failure)

       faceloop: do iface = 1, nscan

          if ( usecache ) then
             v1 = bfv1(iface)
             v2 = bfv2(iface)
          else
             if ( face(iface)%atti(FMARKER) /= 1 ) cycle faceloop
             v1 = face(iface)%atti(FACEV1)
             v2 = face(iface)%atti(FACEV2)
          endif

             x1 = xcugrd(v1)
             y1 = ycugrd(v1)
             x2 = xcugrd(v2)
             y2 = ycugrd(v2)

             dxb = x2 - x1
             dyb = y2 - y1

             reldis = abs( dyb*(x-x1) - dxb*(y-y1) ) / ( dxb*dxb + dyb*dyb )

             if ( reldis < 0.01 ) then

                r   = ( dxb*(x-x1) + dyb*(y-y1) ) / ( dxb*dxb + dyb*dyb )

                if ( r < -0.01 .or. r > 1.01 ) then      ! 41.13
                   kvert = -1
                else

                   if ( r < 0.5 ) then
                      kvert = v1
                   else
                      kvert = v2
                   endif
                   exit faceloop

                endif

             else
                kvert = -1
             endif

       enddo faceloop

    endif

end subroutine SwanFindPoint

end module swan_find_point
