program benchmark_sort
    use iso_fortran_env, only: int64
    implicit none
    integer, parameter :: nsweep = 4
    real, parameter :: pi = 3.14159265358979323846
    character(512) :: filename
    integer :: base, id, ios, j, k, marker, nattr, nbmark, ndim
    integer :: nverts, s, unit
    integer, dimension(1) :: kd
    integer(int64) :: clock0, clock1, rate
    real :: angle, elapsed_selection, elapsed_tree, rtmp
    real, allocatable :: dist(:,:), key(:), x(:), y(:)
    integer, allocatable :: reference(:,:), result(:,:)

    call get_command_argument(1,filename)
    if ( len_trim(filename) == 0 ) error stop 'mesh .node path required'
    open(newunit=unit,file=trim(filename),status='old',action='read',iostat=ios)
    if ( ios /= 0 ) error stop 'cannot open mesh'
    read(unit,*) nverts, ndim, nattr, nbmark
    allocate(x(nverts),y(nverts),dist(nverts,nsweep),key(nverts))
    allocate(reference(nverts,nsweep),result(nverts,nsweep))
    do j = 1, nverts
       read(unit,*) id, x(id), y(id), marker
    enddo
    close(unit)

    do s = 1, nsweep
       angle = real(s-1)*pi/real(nsweep/2)
       dist(:,s) = x*cos(angle) + y*sin(angle)
    enddo

    call system_clock(clock0,rate)
    do s = 1, nsweep
       key = dist(:,s)
       do j = 1, nverts
          reference(j,s) = j
       enddo
       do j = 1, nverts-1
          kd = minloc(key(j:nverts))
          k = kd(1)+j-1
          if ( k /= j ) then
             rtmp = key(j)
             key(j) = key(k)
             key(k) = rtmp
             id = reference(j,s)
             reference(j,s) = reference(k,s)
             reference(k,s) = id
          endif
       enddo
    enddo
    call system_clock(clock1)
    elapsed_selection = real(clock1-clock0)/real(rate)

    call system_clock(clock0)
    do s = 1, nsweep
       do j = 1, nverts
          result(j,s) = j
       enddo
       call tree_sort(dist(:,s),result(:,s))
    enddo
    call system_clock(clock1)
    elapsed_tree = real(clock1-clock0)/real(rate)

    print '(a,i0)', 'vertices: ', nverts
    print '(a,f12.6)', 'selection_seconds: ', elapsed_selection
    print '(a,f12.6)', 'tree_seconds: ', elapsed_tree
    print '(a,f12.3)', 'sort_speedup: ', elapsed_selection/elapsed_tree
    print '(a,l1)', 'identical_order: ', all(reference == result)

contains

    subroutine tree_sort(keys,order)
        real, intent(in) :: keys(:)
        integer, intent(inout) :: order(:)
        integer :: ierr, itmp, left, n, node, position
        integer, allocatable :: tree(:)
        n = size(order)
        base = 1
        do while ( base < n )
           base = 2*base
        enddo
        allocate(tree(2*base),stat=ierr)
        if ( ierr /= 0 ) error stop 'tree allocation failed'
        tree = 0
        do position = 1, n
           tree(base+position-1) = position
        enddo
        do node = base-1, 1, -1
           tree(node) = winner(tree(2*node),tree(2*node+1),keys,order)
        enddo
        do position = 1, n-1
           left = tree(1)
           if ( left /= position ) then
              itmp = order(position)
              order(position) = order(left)
              order(left) = itmp
           endif
           tree(base+position-1) = 0
           call refresh(tree,base,position,keys,order)
           if ( left /= position ) call refresh(tree,base,left,keys,order)
        enddo
        deallocate(tree)
    end subroutine tree_sort

    subroutine refresh(tree,tree_base,position,keys,order)
        integer, intent(inout) :: tree(:)
        integer, intent(in) :: tree_base, position, order(:)
        real, intent(in) :: keys(:)
        integer :: node
        node = (tree_base+position-1)/2
        do while ( node > 0 )
           tree(node) = winner(tree(2*node),tree(2*node+1),keys,order)
           node = node/2
        enddo
    end subroutine refresh

    integer function winner(left,right,keys,order)
        integer, intent(in) :: left, right, order(:)
        real, intent(in) :: keys(:)
        if ( left == 0 ) then
           winner = right
        elseif ( right == 0 ) then
           winner = left
        elseif ( keys(order(left)) < keys(order(right)) ) then
           winner = left
        elseif ( keys(order(left)) > keys(order(right)) ) then
           winner = right
        else
           winner = min(left,right)
        endif
    end function winner
end program benchmark_sort
