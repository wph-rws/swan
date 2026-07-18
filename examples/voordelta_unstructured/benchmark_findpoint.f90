program benchmark_findpoint
!
!   Verifies that SwanNearestVertex (bucket index) returns exactly the
!   same vertex as the former linear scan in SwanFindPoint, for every
!   vertex position, perturbed positions and random positions, and
!   compares the run time of both. Links against the production module
!   (no copy of the implementation).
!
    use iso_fortran_env, only: int64
    use SwanGriddata, only: nverts, xcugrd, ycugrd
    use SwanSpatialIndex, only: SwanNearestVertex, SwanSpatialIndexReset

    implicit none

    character(512) :: filename
    integer :: id, ios, j, k, kexp, kgot, marker, nattr, nbmark, ndim, nq, unit
    integer :: nmis, nmis_reset
    integer(int64) :: clock0, clock1, rate
    real :: elapsed_linear, elapsed_tree, x, y
    real, allocatable :: qx(:), qy(:)
    integer, allocatable :: kres(:)
    real :: xmin, xmax, ymin, ymax
    real, allocatable :: rnd(:)
    real, allocatable :: xsave(:), ysave(:)

    call get_command_argument(1,filename)
    if ( len_trim(filename) == 0 ) error stop 'mesh .node path required'
    open(newunit=unit,file=trim(filename),status='old',action='read',iostat=ios)
    if ( ios /= 0 ) error stop 'cannot open mesh'
    read(unit,*) nverts, ndim, nattr, nbmark
    allocate(xcugrd(nverts),ycugrd(nverts))
    do j = 1, nverts
       read(unit,*) id, xcugrd(id), ycugrd(id), marker
    enddo
    close(unit)

    xmin = minval(xcugrd); xmax = maxval(xcugrd)
    ymin = minval(ycugrd); ymax = maxval(ycugrd)

    ! query set: all vertices, perturbed vertices, uniform random points
    ! (also slightly outside the bounding box)
    nq = 3*nverts
    allocate(qx(nq), qy(nq), kres(nq), rnd(2*nverts))
    call random_seed()
    call random_number(rnd)
    do j = 1, nverts
       qx(j) = xcugrd(j)
       qy(j) = ycugrd(j)
       qx(nverts+j) = xcugrd(j) + (rnd(j)-0.5)*(xmax-xmin)*1.e-4
       qy(nverts+j) = ycugrd(j) + (rnd(nverts+j)-0.5)*(ymax-ymin)*1.e-4
       qx(2*nverts+j) = xmin + rnd(nverts+j)*1.04*(xmax-xmin) - 0.02*(xmax-xmin)
       qy(2*nverts+j) = ymin + rnd(j)*1.04*(ymax-ymin) - 0.02*(ymax-ymin)
    enddo

    call system_clock(clock0,rate)
    do j = 1, nq
       call SwanNearestVertex ( qx(j), qy(j), kres(j) )
    enddo
    call system_clock(clock1)
    elapsed_tree = real(clock1-clock0)/real(rate)

    call system_clock(clock0)
    nmis = 0
    do j = 1, nq
       call linear_scan ( qx(j), qy(j), kexp )
       if ( kexp /= kres(j) ) then
          nmis = nmis + 1
          if ( nmis <= 10 ) print '(a,2e16.8,2i8)', 'MISMATCH x,y,lin,idx: ', qx(j), qy(j), kexp, kres(j)
       endif
    enddo
    call system_clock(clock1)
    elapsed_linear = real(clock1-clock0)/real(rate)

    print '(a,i0)', 'vertices: ', nverts
    print '(a,i0)', 'queries: ', nq
    print '(a,f12.6)', 'linear_seconds: ', elapsed_linear
    print '(a,f12.6)', 'index_seconds: ', elapsed_tree
    print '(a,f12.3)', 'speedup: ', elapsed_linear/elapsed_tree
    print '(a,i0)', 'mismatches: ', nmis
    if ( nmis == 0 ) then
       print '(a)', 'RESULT: IDENTICAL'
    else
       error stop 'RESULT: MISMATCH'
    endif

    ! Regression for cache invalidation: install different coordinates while
    ! retaining exactly the same vertex count. A count-only cache key would
    ! keep stale bucket membership here.
    allocate(xsave(nverts),ysave(nverts))
    xsave = xcugrd
    ysave = ycugrd
    xcugrd = 12345.0 - 2.5*ysave
    ycugrd = -54321.0 + 0.75*xsave
    call SwanSpatialIndexReset
    nmis_reset = 0
    do j = 1, nverts
       call SwanNearestVertex ( xcugrd(j), ycugrd(j), kgot )
       call linear_scan ( xcugrd(j), ycugrd(j), kexp )
       if ( kgot /= kexp ) nmis_reset = nmis_reset + 1
    enddo
    print '(a,i0)', 'same_count_reset_mismatches: ', nmis_reset
    if ( nmis_reset /= 0 ) error stop 'RESULT: STALE CACHE AFTER RESET'

contains

    subroutine linear_scan ( x, y, kvert )
    ! the former loop from SwanFindPoint, verbatim semantics
        real, intent(in) :: x, y
        integer, intent(out) :: kvert
        integer :: ivert
        real :: dismin, dist, xc, yc
        dismin = 1.e20
        kvert = -1
        do ivert = 1, nverts
           xc = xcugrd(ivert)
           yc = ycugrd(ivert)
           dist = sqrt( (x-xc)**2 + (y-yc)**2 )
           if ( dist < dismin ) then
              kvert  = ivert
              dismin = dist
           endif
        enddo
    end subroutine linear_scan

end program benchmark_findpoint
