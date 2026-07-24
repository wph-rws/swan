module SwanSpatialIndex
   USE swan_service_interfaces, ONLY: MSGERR

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
!   43.05: Wilbert Huibregtse
!
!   Updates
!
!   43.05, July 2026: New module
!
!   Purpose
!
!   Provides a uniform bucket index over the unstructured grid so that
!   nearest-vertex queries (SwanFindPoint) run in O(1) expected time
!   instead of scanning all vertices, and caches the list of boundary
!   faces so that point-in-mesh tests (SwanPointinMesh) do not have to
!   filter the full face list on every call
!
!   Method
!
!   Vertices are binned into a uniform grid of buckets covering the mesh
!   bounding box (about one vertex per bucket). A nearest-vertex query
!   scans buckets in rings of increasing Chebyshev radius around the
!   query point and stops as soon as no unscanned bucket can contain a
!   vertex closer than the best one found. The distance formula and the
!   tie behaviour (lowest vertex index wins on equal distance) are
!   identical to the former linear scan, so the returned vertex is
!   exactly the same
!
!   The index is built on first use. SwanGridTopology explicitly resets
!   it whenever a new grid is installed, including when the new grid has
!   the same vertex count. Queries are intended for serial input/output
!   code paths
!
!   Modules used

    use SwanGriddata, only: nverts, nfaces, xcugrd, ycugrd

    implicit none

    public

    integer, save                              :: nbfac = -1 ! number of cached boundary faces (<0: cache not built)
    integer, dimension(:), allocatable, save   :: bfv1       ! first vertex of each cached boundary face
    integer, dimension(:), allocatable, save   :: bfv2       ! second vertex of each cached boundary face

    integer, save, private                     :: nbx  = 0   ! number of buckets in x-direction
    integer, save, private                     :: nby  = 0   ! number of buckets in y-direction
    integer, save, private                     :: nvidx = -1 ! number of vertices the index was built for (<0: no index)
    integer, save, private                     :: nfidx = -1 ! number of faces the boundary cache was built for
    real, save, private                        :: bhx        ! bucket size in x-direction
    real, save, private                        :: bhy        ! bucket size in y-direction
    real, save, private                        :: bx0        ! x-coordinate of index origin
    real, save, private                        :: by0        ! y-coordinate of index origin
    integer, dimension(:), allocatable, save, private :: bptr  ! per bucket the start position in blist (CSR layout)
    integer, dimension(:), allocatable, save, private :: blist ! vertex indices grouped per bucket

contains

subroutine SwanSpatialIndexReset

!   Invalidates all cached grid data. Called whenever grid topology is rebuilt

    implicit none

    if ( allocated(bptr)  ) deallocate(bptr)
    if ( allocated(blist) ) deallocate(blist)
    if ( allocated(bfv1)  ) deallocate(bfv1)
    if ( allocated(bfv2)  ) deallocate(bfv2)

    nbx   = 0
    nby   = 0
    nvidx = -1
    nfidx = -1
    nbfac = -1

end subroutine SwanSpatialIndexReset

subroutine SwanSpatialIndexBuild

!   Bins all vertices of the unstructured grid into a uniform bucket grid

    implicit none

    integer :: ibkt   ! flattened bucket index
    integer :: istat  ! status of allocation
    integer :: j      ! loop counter over vertices
    integer :: k      ! counter

    real    :: siz    ! target bucket size
    real    :: w      ! width of bounding box
    real    :: h      ! height of bounding box
    real    :: xmax   ! largest x-coordinate
    real    :: ymax   ! largest y-coordinate

    nvidx = -1
    if ( nverts <= 0 ) return

    bx0  = minval(xcugrd(1:nverts))
    by0  = minval(ycugrd(1:nverts))
    xmax = maxval(xcugrd(1:nverts))
    ymax = maxval(ycugrd(1:nverts))

    w = xmax - bx0
    h = ymax - by0

    ! about one vertex per bucket; degenerate (collinear) grids get a single row or column

    siz = sqrt( w*h/real(nverts) )
    if ( siz > 0. ) then
       nbx = max( 1, min( int(w/siz) + 1, nverts ) )
       nby = max( 1, min( int(h/siz) + 1, nverts ) )
    else
       nbx = 1
       nby = 1
    endif

    bhx = w/real(nbx)
    if ( .not. bhx > 0. ) bhx = 1.
    bhy = h/real(nby)
    if ( .not. bhy > 0. ) bhy = 1.

    if ( allocated(bptr)  ) deallocate(bptr)
    if ( allocated(blist) ) deallocate(blist)
    istat = 0
    allocate (bptr(nbx*nby+1), blist(nverts), stat = istat)
    if ( istat /= 0 ) then
       if ( allocated(bptr)  ) deallocate(bptr)
       if ( allocated(blist) ) deallocate(blist)
       call msgerr ( 4, 'Allocation problem in SwanSpatialIndexBuild: bucket arrays ' )
       return
    endif

    ! count vertices per bucket, turn counts into start positions and fill
    ! the bucket lists (in increasing vertex order within each bucket)

    bptr = 0
    do j = 1, nverts
       ibkt = SwanBucketNr(xcugrd(j),ycugrd(j))
       bptr(ibkt+1) = bptr(ibkt+1) + 1
    enddo
    bptr(1) = 1
    do k = 2, nbx*nby+1
       bptr(k) = bptr(k) + bptr(k-1)
    enddo
    do j = 1, nverts
       ibkt = SwanBucketNr(xcugrd(j),ycugrd(j))
       blist(bptr(ibkt)) = j
       bptr(ibkt) = bptr(ibkt) + 1
    enddo
    do k = nbx*nby+1, 2, -1
       bptr(k) = bptr(k-1)
    enddo
    bptr(1) = 1

    nvidx = nverts

end subroutine SwanSpatialIndexBuild

integer function SwanBucketNr ( x, y )

!   Returns the flattened bucket number for the given point (clamped to the bucket grid)

    implicit none

    real, intent(in) :: x ! x-coordinate of given point
    real, intent(in) :: y ! y-coordinate of given point

    integer :: ib ! bucket index in x-direction
    integer :: jb ! bucket index in y-direction

    ib = min( nbx, max( 1, int((x-bx0)/bhx) + 1 ) )
    jb = min( nby, max( 1, int((y-by0)/bhy) + 1 ) )

    SwanBucketNr = (jb-1)*nbx + ib

end function SwanBucketNr

subroutine SwanNearestVertex ( x, y, kvert )

!   Finds the vertex closest to the given point; result is identical to a
!   linear first-minimum scan over all vertices

    implicit none

    integer, intent(out) :: kvert ! closest vertex index of given point
    real, intent(in)     :: x     ! x-coordinate of given point
    real, intent(in)     :: y     ! y-coordinate of given point

    integer :: ib     ! loop counter over buckets in x-direction
    integer :: ib0    ! bucket in x-direction containing the given point
    integer :: ibkt   ! flattened bucket index
    integer :: idx    ! loop counter over bucket contents
    integer :: ivert  ! vertex index
    integer :: jb     ! loop counter over buckets in y-direction
    integer :: jb0    ! bucket in y-direction containing the given point
    integer :: r      ! Chebyshev ring radius

    real    :: dismin ! minimal distance found
    real    :: dist   ! computed distance
    real    :: rminh  ! smallest bucket size
    real    :: xc     ! x-coordinate of vertex
    real    :: yc     ! y-coordinate of vertex

    logical :: inrange ! indicate whether present ring contains buckets

    if ( nvidx /= nverts ) call SwanSpatialIndexBuild

    ! MSGERR records the allocation failure but does not necessarily stop at
    ! once. Preserve the old exact result instead of touching absent arrays.

    if ( nvidx /= nverts .or. .not.allocated(bptr) .or. .not.allocated(blist) ) then
       call SwanNearestVertexLinear ( x, y, kvert )
       return
    endif

    ib0 = min( nbx, max( 1, int((x-bx0)/bhx) + 1 ) )
    jb0 = min( nby, max( 1, int((y-by0)/bhy) + 1 ) )

    rminh  = min(bhx,bhy)
    kvert  = -1
    dismin = 1.e20

    do r = 0, nbx+nby

       ! vertices in rings beyond r-1 are at least (r-1)*rminh away, so the
       ! best match cannot be improved (nor its tie broken) anymore

       if ( kvert > 0 .and. real(r-1)*rminh > dismin ) exit

       inrange = .false.

       do jb = max(1,jb0-r), min(nby,jb0+r)

          do ib = max(1,ib0-r), min(nbx,ib0+r)

             ! ring cells only: skip the interior scanned in previous rings

             if ( max(abs(ib-ib0),abs(jb-jb0)) /= r ) cycle

             inrange = .true.
             ibkt    = (jb-1)*nbx + ib

             do idx = bptr(ibkt), bptr(ibkt+1)-1

                ivert = blist(idx)
                xc    = xcugrd(ivert)
                yc    = ycugrd(ivert)

                ! same distance formula as the former linear scan; on equal
                ! distance the lowest vertex index wins, as MINLOC-like
                ! first-minimum scanning did

                dist = sqrt( (x-xc)**2 + (y-yc)**2 )
                if ( dist < dismin ) then
                   kvert  = ivert
                   dismin = dist
                elseif ( dist == dismin .and. ivert < kvert ) then
                   kvert  = ivert
                endif

             enddo

          enddo

       enddo

       ! once a ring lies fully outside the bucket grid all larger rings do too

       if ( .not.inrange .and. r > 0 ) exit

    enddo

end subroutine SwanNearestVertex

subroutine SwanNearestVertexLinear ( x, y, kvert )

!   Allocation-failure fallback with the exact former scan semantics

    implicit none

    integer, intent(out) :: kvert
    real, intent(in)     :: x
    real, intent(in)     :: y

    integer :: ivert
    real    :: dismin
    real    :: dist

    kvert  = -1
    dismin = 1.e20
    do ivert = 1, nverts
       dist = sqrt( (x-xcugrd(ivert))**2 + (y-ycugrd(ivert))**2 )
       if ( dist < dismin ) then
          kvert  = ivert
          dismin = dist
       endif
    enddo

end subroutine SwanNearestVertexLinear

subroutine SwanBndFaceCache

!   Caches the vertex pairs of all boundary faces so that repeated
!   point-in-mesh tests need not filter the full face list

    use SwanGridobjects

    implicit none

    integer :: iface ! loop counter over faces
    integer :: istat ! status of allocation
    integer :: k     ! counter

    type(facetype), dimension(:), pointer :: face ! datastructure for faces with their attributes

    if ( nbfac >= 0 .and. nfidx == nfaces ) return
    nbfac = -1
    nfidx = -1

    face => gridobject%face_grid

    k = 0
    do iface = 1, nfaces
       if ( face(iface)%atti(FMARKER) == 1 ) k = k + 1
    enddo

    if ( allocated(bfv1) ) deallocate(bfv1)
    if ( allocated(bfv2) ) deallocate(bfv2)
    istat = 0
    allocate (bfv1(k), bfv2(k), stat = istat)
    if ( istat /= 0 ) then
       if ( allocated(bfv1) ) deallocate(bfv1)
       if ( allocated(bfv2) ) deallocate(bfv2)
       call msgerr ( 4, 'Allocation problem in SwanBndFaceCache: face arrays ' )
       return
    endif

    k = 0
    do iface = 1, nfaces
       if ( face(iface)%atti(FMARKER) == 1 ) then
          k = k + 1
          bfv1(k) = face(iface)%atti(FACEV1)
          bfv2(k) = face(iface)%atti(FACEV2)
       endif
    enddo

    nbfac = k
    nfidx = nfaces

end subroutine SwanBndFaceCache

end module SwanSpatialIndex
