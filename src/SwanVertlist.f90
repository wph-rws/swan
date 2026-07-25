module swan_vertlist
   implicit none(type, external)
   private
   public :: SwanVertlist
contains

subroutine SwanVertlist ( compda )
   USE swan_field_file_update, ONLY: FLFILE
   USE swan_front_scheduling_backend, ONLY: build_front_schedule
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
!   41.07: Casey Dietrich
!   41.48: Marcel Zijlema
!   41.68: Marcel Zijlema
!   43.05: Marcel Zijlema
!
!   Updates
!
!   40.80,    July 2007: New subroutine
!   41.07,    July 2009: small fix (assign ref.point to deepest point in case of no b.c.)
!   41.48,   March 2013: including order along a user-given direction
!   41.68,  August 2015: introduction of a fixed number of sweeps per iteration
!   41.68,   April 2018: removal of the wavefront approach (based on reference point)
!   43.05, January 2023: graph-level and fixed-front scheduling
!
!   Purpose
!
!   Creates vertex list in line with sweep direction
!   Note: first sweep direction always equals user-given/wave/wind direction
!   Creates wavefronts using the configured scheduling backend
!
!   Method
!
!   Sorting based on increasing distance along sweep direction
!
!   Modules used

    use swan_diagnostics_level
    use swan_io_units
    use swan_input_grids, only: COSWC, SINWC, VARWI
    use swan_compda_layout, only: MCMVAR, JWX2, JWY2, JWX3, JWY3
    use swan_numerics, only: NSTATM
    use swan_math_constants
    use m_genarr
    use swan_input_fields
    use m_parall
    use SwanGriddata
    use SwanGridobjects
    use SwanCompdata

    implicit none(type, external)

!   Argument variables

    real, dimension(nverts,MCMVAR), intent(in) :: compda ! array containing space-dependent info (e.g. wind)

!   Local variables
    integer, save                        :: ient = 0 ! number of entries in this subroutine
    integer                              :: ierr     ! error indicator: ierr=0: no error, otherwise error
    integer                              :: istat    ! indicate status of allocation
    integer                              :: j        ! loop counter over vertices
    integer                              :: k        ! counter
    integer                              :: swpdir   ! sweep counter

    real                                 :: sdir     ! sweep direction
    real                                 :: wdsum    ! total sum of wind direction
    real                                 :: wx       ! wind velocity in x-direction
    real                                 :: wy       ! wind velocity in y-direction
    real, dimension(:,:), allocatable    :: dist     ! distance of each point with respect to reference point

    type(verttype), dimension(:), pointer :: vert    ! datastructure for vertices with their attributes
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanVertlist')

    ! point to vertex objects

    vert => gridobject%vert_grid

    ! create vertex list

    istat = 0
    if(.not.allocated(vlist)) allocate (vlist(nverts,nsweep), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanVertlist: array vlist ' )
       return
    endif

    allocate (dist(nverts,nsweep))

    ! check first sweep direction

    if ( .not. asort > -999. ) then
!      if asort still does not have a value, try space-varying wind and take the mean of wind direction
       if ( VARWI ) then
!         retrieve current wind field
          if ( NSTATM == 1 ) call FLFILE ( 5, 6, WXI, WYI, 0, JWX2, JWX3, 0, JWY2, JWY3, COSWC, SINWC, compda, XCGRID, YCGRID, KGRPNT, ierr )
          k     = 0
          wdsum = 0.
          do j = 1, nverts
!            internal vertices, exception vertices and ghost vertices only
             if ( vmark(j) == 0 .or. vmark(j) >= excmark ) then
                wx = compda(j,JWX2)
                wy = compda(j,JWY2)
                if ( wx /= 0. .or. wy /= 0. ) then
                   k = k + 1
                   wdsum = wdsum + atan2(wy,wx)
                endif
             endif
          enddo
          asort = wdsum / real(k)
          call SWREDUCE( asort, 1, SWSUM )
          asort = asort / real(NPROC)
       else
!         final attempt: set sweep direction to zero
          asort = 0.
       endif
    endif
    if ( ITEST >= 40 ) write (PRINTF,"(' Number of sweeps = ',i2,'; chosen wave direction for sweeping: ',f7.2,' degrees')") nsweep, 180.*asort/PI
    asort = asort - PI/real(nsweep)

    ! order vertices according to sweep direction; base vector is user-given/wave/wind direction

    sdir = asort + PI/real(nsweep)
    do swpdir = 1, nsweep
       do j = 1, nverts
          dist(j,swpdir) = vert(j)%attr(VERTX) * cos(sdir) + vert(j)%attr(VERTY) * sin(sdir)
       enddo
       sdir = sdir + PI2/real(nsweep)
    enddo

    ! sort vertex list in order of increasing distance

    do swpdir = 1, nsweep

       do j = 1, nverts
          vlist(j,swpdir) = j
       enddo

       call SwanTreeSort ( dist(:,swpdir), vlist(:,swpdir) )

    enddo

    call build_front_schedule(vlist)
    deallocate(dist)

contains

    subroutine SwanTreeSort ( key, order )

    ! Sort vertex indices by increasing projected distance in O(N log N).
    ! The segment tree tracks the first minimum at its current position, thus
    ! preserving the tie behaviour of the former MINLOC selection sort.

    real   , dimension(:), intent(in)    :: key
    integer, dimension(:), intent(inout) :: order

    integer                            :: base
    integer                            :: ierr
    integer                            :: itmp
    integer                            :: j
    integer                            :: k
    integer                            :: n
    integer                            :: node
    integer, dimension(:), allocatable :: tree

    n = size(order)
    if ( n < 2 ) return

    base = 1
    do while ( base < n )
       base = 2*base
    enddo

    allocate(tree(2*base),stat=ierr)
    if ( ierr /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanTreeSort: array tree ' )
       return
    endif
    tree = 0

    do j = 1, n
       tree(base+j-1) = j
    enddo
    do node = base-1, 1, -1
       tree(node) = SwanTreeWinner ( tree(2*node), tree(2*node+1), key, order )
    enddo

    do j = 1, n-1

       k = tree(1)
       if ( k /= j ) then
          itmp     = order(j)
          order(j) = order(k)
          order(k) = itmp
       endif

       tree(base+j-1) = 0
       call SwanTreeRefresh ( tree, base, j, key, order )
       if ( k /= j ) call SwanTreeRefresh ( tree, base, k, key, order )

    enddo

    deallocate(tree)

    end subroutine SwanTreeSort

    subroutine SwanTreeRefresh ( tree, base, position, key, order )

    integer, dimension(:), intent(inout) :: tree
    integer              , intent(in)    :: base
    integer              , intent(in)    :: position
    real   , dimension(:), intent(in)    :: key
    integer, dimension(:), intent(in)    :: order

    integer :: node

    node = (base+position-1)/2
    do while ( node > 0 )
       tree(node) = SwanTreeWinner ( tree(2*node), tree(2*node+1), key, order )
       node = node/2
    enddo

    end subroutine SwanTreeRefresh

    integer function SwanTreeWinner ( left, right, key, order )

    integer              , intent(in) :: left
    integer              , intent(in) :: right
    real   , dimension(:), intent(in) :: key
    integer, dimension(:), intent(in) :: order

    if ( left == 0 ) then
       SwanTreeWinner = right
    elseif ( right == 0 ) then
       SwanTreeWinner = left
    elseif ( key(order(left)) < key(order(right)) ) then
       SwanTreeWinner = left
    elseif ( key(order(left)) > key(order(right)) ) then
       SwanTreeWinner = right
    else
       SwanTreeWinner = min(left,right)
    endif

    end function SwanTreeWinner

end subroutine SwanVertlist

end module swan_vertlist
