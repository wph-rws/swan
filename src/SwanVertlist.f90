subroutine SwanVertlist ( compda )
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
!GRAPH!   43.05, January 2023: graph-level scheduling
!FXFRO!   43.05, January 2023: wavefront scheduling
!
!   Purpose
!
!   Creates vertex list in line with sweep direction
!   Note: first sweep direction always equals user-given/wave/wind direction
!GRAPH!   Creates wavefronts based on graph-level scheduling
!FXFRO!   Creates wavefronts
!
!   Method
!
!   Sorting based on increasing distance along sweep direction
!
!   Modules used
!
    use ocpcomm4
    use swcomm2, only: COSWC, SINWC, VARWI
    use swcomm3, only: MCMVAR, JWX2, JWY2, JWX3, JWY3
    use m_genarr
    use m_parall
    use SwanGriddata
    use SwanGridobjects
    use SwanCompdata
!
    implicit none
!
!   Argument variables
!
    real, dimension(nverts,MCMVAR), intent(in) :: compda ! array containing space-dependent info (e.g. wind)
!
!   Parameter variables
!
!GRAPH    integer, parameter :: nlpf = 1 ! number of levels per wavefront
!FXFRO    integer, parameter :: nvth = 10 ! target number of vertices per thread
!
!   Local variables
!
!GRAPH    integer                              :: icell    ! cell index
    integer, save                        :: ient = 0 ! number of entries in this subroutine
    integer                              :: ierr     ! error indicator: ierr=0: no error, otherwise error
!GRAPH    integer                              :: ifront   ! front id / loop counter
!FXFRO    integer                              :: ifront   ! loop counter over wavefronts
    integer                              :: istat    ! indicate status of allocation
    integer                              :: itmp     ! temporary stored integer for swapping
    integer                              :: j        ! loop counter over vertices
!GRAPH    integer                              :: jc       ! loop counter over cells
    integer                              :: k        ! counter
    integer, dimension(1)                :: kd       ! location of minimum value in array dist
!GRAPH    integer                              :: l        ! counter
!GRAPH    integer                              :: lmax     ! indicate maximum level of upstream neighbours
!GRAPH    integer                              :: m        ! neighbour vertex
!GRAPH    integer                              :: maxfr    ! maximum number of wavefronts over all sweeps
!GRAPH    integer                              :: nlevel   ! number of created graph levels per sweep
!FXFRO    integer                              :: nth      ! number of threads
!FXFRO    integer                              :: nvf      ! number of vertices per wavefront
    integer                              :: swpdir   ! sweep counter
!GRAPH    integer, dimension(3)                :: v        ! vertices in present cell
!GRAPH    integer, dimension(2)                :: vu       ! upwave vertices in present cell
    !
    real                                 :: rtmp     ! temporary stored real for swapping
    real                                 :: sdir     ! sweep direction
    real                                 :: wdsum    ! total sum of wind direction
    real                                 :: wx       ! wind velocity in x-direction
    real                                 :: wy       ! wind velocity in y-direction
!GRAPH    !
!GRAPH    integer, dimension(:,:), allocatable :: fid      ! front identifier
!GRAPH    integer, dimension(:)  , allocatable :: fcount   ! vertex counts per front
!GRAPH    integer, dimension(:)  , allocatable :: fill     ! auxiliary ptr array
!GRAPH    integer, dimension(:)  , allocatable :: level    ! graph levels
!GRAPH    integer, dimension(:)  , allocatable :: pos      ! vertex position in vlist
    !
    real, dimension(:,:), allocatable    :: dist     ! distance of each point with respect to reference point
    !
!GRAPH    type(celltype), dimension(:), pointer :: cell    ! datastructure for cells with their attributes
    type(verttype), dimension(:), pointer :: vert    ! datastructure for vertices with their attributes
!FXFRO    !
!FXFRO!$  integer, external :: omp_get_max_threads         ! get maximum number of threads
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
    if (ltrace) call strace (ient,'SwanVertlist')
    !
!GRAPH    ! point to vertex and cell objects
!FXFRO    ! point to vertex object
    !
    vert => gridobject%vert_grid
!GRAPH    cell => gridobject%cell_grid
    !
    ! create vertex list
    !
    istat = 0
    if(.not.allocated(vlist)) allocate (vlist(nverts,nsweep), stat = istat)
    if ( istat /= 0 ) then
       call msgerr ( 4, 'Allocation problem in SwanVertlist: array vlist ' )
       return
    endif
    !
    allocate (dist(nverts,nsweep))
    !
    ! check first sweep direction
    !
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
          call SWREDUCE( asort, 1, SWREAL, SWSUM )
          asort = asort / real(NPROC)
       else
!         final attempt: set sweep direction to zero
          asort = 0.
       endif
    endif
    if ( ITEST >= 40 ) write (PRINTF,10) nsweep, 180.*asort/PI
    asort = asort - PI/real(nsweep)
    !
    ! order vertices according to sweep direction; base vector is user-given/wave/wind direction
    !
    sdir = asort + PI/real(nsweep)
    do swpdir = 1, nsweep
       do j = 1, nverts
          dist(j,swpdir) = vert(j)%attr(VERTX) * cos(sdir) + vert(j)%attr(VERTY) * sin(sdir)
       enddo
       sdir = sdir + PI2/real(nsweep)
    enddo
    !
    ! sort vertex list in order of increasing distance
    !
    do swpdir = 1, nsweep
       !
       do j = 1, nverts
          vlist(j,swpdir) = j
       enddo
       !
       do j = 1, nverts-1
          !
          kd = minloc(dist(j:nverts,swpdir))
          k  = kd(1) + j-1
          !
          if ( k /= j ) then
             !
             rtmp            = dist(j,swpdir)
             dist(j,swpdir)  = dist(k,swpdir)
             dist(k,swpdir)  = rtmp
             !
             itmp            = vlist(j,swpdir)
             vlist(j,swpdir) = vlist(k,swpdir)
             vlist(k,swpdir) = itmp
             !
          endif
          !
       enddo
       !
    enddo
    !
!GRAPH    ! create wavefronts based on graph levels
!FXFRO    ! create wavefronts
    !
!GRAPH    allocate (nfront(nsweep))
!GRAPH    !
!GRAPH    allocate (pos  (nverts))
!GRAPH    allocate (level(nverts))
!GRAPH    !
!GRAPH    allocate (fid(nverts,nsweep))
!GRAPH    !
!GRAPH    do swpdir = 1, nsweep
!GRAPH       !
!GRAPH       ! determine position of each vertex of vlist
!GRAPH       !
!GRAPH       pos = 0
!GRAPH       !
!GRAPH       do j = 1, nverts
!GRAPH          !
!GRAPH          k = vlist(j,swpdir)
!GRAPH          !
!GRAPH          pos(k) = j
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH       ! construct levels from a directed dependency graph
!GRAPH       ! note: each level contains vertices that are independent by construction
!GRAPH       !
!GRAPH       level = 0
!GRAPH       !
!GRAPH       do j = 1, nverts
!GRAPH          !
!GRAPH          k = vlist(j,swpdir)
!GRAPH          !
!GRAPH          lmax = 0
!GRAPH          !
!GRAPH          do jc = 1, vert(k)%noc
!GRAPH             !
!GRAPH             icell = vert(k)%cell(jc)%atti(CELLID)
!GRAPH             !
!GRAPH             v(1) = cell(icell)%atti(CELLV1)
!GRAPH             v(2) = cell(icell)%atti(CELLV2)
!GRAPH             v(3) = cell(icell)%atti(CELLV3)
!GRAPH             !
!GRAPH             ! pick up two upwave vertices
!GRAPH             !
!GRAPH             do l = 1, 3
!GRAPH                if ( v(l) == k ) then
!GRAPH                   vu(1) = v(mod(l  ,3)+1)
!GRAPH                   vu(2) = v(mod(l+1,3)+1)
!GRAPH                   exit
!GRAPH                endif
!GRAPH             enddo
!GRAPH             !
!GRAPH             ! pick first neighbour vertex
!GRAPH             !
!GRAPH             m = vu(1)
!GRAPH             !
!GRAPH             ! is vertex m upstream from current vertex k?
!GRAPH             ! if so, they do not belong to the same level
!GRAPH             !
!GRAPH             if ( pos(m) < pos(k) ) then
!GRAPH                !
!GRAPH                lmax = max(lmax,level(m))
!GRAPH                !
!GRAPH             endif
!GRAPH             !
!GRAPH          enddo
!GRAPH          !
!GRAPH          ! make sure that vertex k is assigned to new level
!GRAPH          !
!GRAPH          level(k) = lmax + 1
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH       nlevel = maxval(level)
!GRAPH       !
!GRAPH       nfront(swpdir) = ceiling( real(nlevel) / real(nlpf) )
!GRAPH       !
!GRAPH       if ( ITEST >= 40 ) then
!GRAPH          if ( nlpf == 1 ) then
!GRAPH             write(PRINTF,20) swpdir, nlevel
!GRAPH          else
!GRAPH             write(PRINTF,30) swpdir, nlevel, nfront(swpdir)
!GRAPH          endif
!GRAPH       endif
!GRAPH       !
!GRAPH       !  aggregate a number of levels into a front
!GRAPH       !
!GRAPH       do j = 1, nverts
!GRAPH          !
!GRAPH          fid(j,swpdir) = (level(j)-1) / nlpf + 1
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH    enddo
!GRAPH    !
!GRAPH    maxfr = maxval(nfront)
!GRAPH    !
!GRAPH    allocate (fptr(maxfr+1,nsweep))
!GRAPH    !
!GRAPH    do swpdir = 1, nsweep
!GRAPH       !
!GRAPH       ! next, count vertices per level / front
!GRAPH       !
!GRAPH       allocate (fcount(nfront(swpdir)))
!GRAPH       !
!GRAPH       fcount = 0
!GRAPH       !
!GRAPH       do j = 1, nverts
!GRAPH          !
!GRAPH          ifront = fid(j,swpdir)
!GRAPH          !
!GRAPH          fcount(ifront) = fcount(ifront) + 1
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH       fptr(1,swpdir) = 1
!GRAPH       !
!GRAPH       do ifront = 1, nfront(swpdir)
!GRAPH          !
!GRAPH          fptr(ifront+1,swpdir) = fptr(ifront,swpdir) + fcount(ifront)
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH       deallocate(fcount)
!GRAPH       !
!GRAPH    enddo
!GRAPH    !
!GRAPH    ! create front list
!GRAPH    !
!GRAPH    istat = 0
!GRAPH    if(.not.allocated(flist)) allocate (flist(nverts,nsweep), stat = istat)
!GRAPH    if ( istat /= 0 ) then
!GRAPH       call msgerr ( 4, 'Allocation problem in SwanVertlist: array flist ' )
!GRAPH       return
!GRAPH    endif
!GRAPH    flist = 0
!GRAPH    !
!GRAPH    do swpdir = 1, nsweep
!GRAPH       !
!GRAPH       allocate (fill(nfront(swpdir)))
!GRAPH       !
!GRAPH       fill(1:nfront(swpdir))=fptr(1:nfront(swpdir),swpdir)
!GRAPH       !
!GRAPH       do j = 1, nverts
!GRAPH          !
!GRAPH          k = vlist(j,swpdir)
!GRAPH          !
!GRAPH          ifront = fid(k,swpdir)
!GRAPH          !
!GRAPH          l = fill(ifront)
!GRAPH          !
!GRAPH          flist(l,swpdir) = k
!GRAPH          !
!GRAPH          fill(ifront) = fill(ifront) + 1
!GRAPH          !
!GRAPH       enddo
!GRAPH       !
!GRAPH       deallocate(fill)
!GRAPH       !
!GRAPH    enddo
!FXFRO    ! first, determine number of threads
!FXFRO    !
!FXFRO    nth = 1
!FXFRO    !$ nth = omp_get_max_threads()
!FXFRO    !
!FXFRO    ! next, compute target number of vertices per wavefront ...
!FXFRO    !
!FXFRO    nvf = nvth * nth
!FXFRO    !
!FXFRO    ! ... and number of wavefronts
!FXFRO    !
!FXFRO    nfront = min(nverts,max(100,ceiling(real(nverts)/real(nvf))))
!FXFRO    !
!FXFRO    if(.not.allocated(fronts)) allocate (fronts(nfront))
!FXFRO    if(.not.allocated(fronte)) allocate (fronte(nfront))
!FXFRO    !
!FXFRO    ! compute actual number of vertices per front
!FXFRO    !
!FXFRO    nvf = int( (nverts+nfront-1)/nfront )
!FXFRO    if ( ITEST >= 40 ) write (PRINTF,20) nfront, nvf
!FXFRO    !
!FXFRO    ! per wavefront, determine start and end vertex indices
!FXFRO    !
!FXFRO    do ifront = 1, nfront
!FXFRO       !
!FXFRO       fronts(ifront) = 1 + (ifront-1)*nvf
!FXFRO       fronte(ifront) = min(nverts, ifront*nvf)
!FXFRO       !
!FXFRO    enddo
    !
!GRAPH    deallocate(dist,fid,level,pos)
!FXFRO    deallocate(dist)
    !
 10 format (' Number of sweeps = ',i2,'; chosen wave direction for sweeping: ',f7.2,' degrees')
!GRAPH 20 format (' sweepnr= ',i2,': number of graph levels = ',i8)
!GRAPH 30 format (' sweepnr= ',i2,': number of graph levels = ',i8,' and number of fronts = ',i8)
!FXFRO 20 format (' Number of fronts = ',i4,' and number of vertices per front = ',i6)
    !
end subroutine SwanVertlist
