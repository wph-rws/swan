program test_front_dependency_completeness
!  Regression test for the unstructured OpenMP determinism.
!
!  The graph backend must not place a neighbour that is read in the same front as
!  its reader: the solver reads ac2 of BOTH neighbours vu(1),vu(2)
!  (SwanCompUnstruc st_kc + SwanTranspX) and writes ac2 of the vertex itself,
!  with only a barrier BETWEEN fronts. A neighbour in the same front is
!  an unsynchronised concurrent read/write.
!
!  Mesh 1 (open fan, boundary): a square in 2 CCW triangles; vertex 2 only
!  ever sees neighbour 1 as vu(2) -> catches the missing vu(2) edge. This mesh
!  gives two such pairs (2 reads 1, and 4 reads 3), both through vu(2).
!  Mesh 2 (closed fan, interior): centre with 3 CCW triangles; every neighbour
!  comes by as vu(1) -> control, no false alarm.
!
!  As long as the vu(2) edge is missing, mesh 1 should FAIL; CMakeLists.txt
!  therefore registers the test as WILL_FAIL until the missing edge is restored.
   use swan_front_scheduling_backend, only: fixed_front_scheduling_enabled, &
      build_front_schedule, clear_front_schedule, front_count, front_bounds, &
      scheduled_vertices
   use SwanGridobjects, only: CELLID, CELLV1, CELLV2, CELLV3, gridobject
   implicit none(type, external)

   integer :: fails

   fails = 0
   !  The fixed front order (FFRO) does not promise this ordering; there the
   !  test is not registered. A silent passing run here would suggest coverage
   !  that does not exist.
   if (fixed_front_scheduling_enabled) &
      error stop 'not applicable to the fixed front order (FFRO)'

   call check_open_fan(fails)
   call check_closed_fan(fails)

   if (fails /= 0) error stop 'same-front read neighbor found'
   print *, 'front dependency completeness passes'

contains

   subroutine setup_mesh(nverts, ncells, cells)
      integer, intent(in) :: nverts, ncells, cells(3, ncells)
      integer :: ic, iv, k, n
      integer, allocatable :: cnt(:)
      allocate(gridobject%vert_grid(nverts))
      allocate(gridobject%cell_grid(ncells))
      do ic = 1, ncells
         gridobject%cell_grid(ic)%atti(CELLID) = ic
         gridobject%cell_grid(ic)%atti(CELLV1) = cells(1, ic)
         gridobject%cell_grid(ic)%atti(CELLV2) = cells(2, ic)
         gridobject%cell_grid(ic)%atti(CELLV3) = cells(3, ic)
      end do
      allocate(cnt(nverts))
      cnt = 0
      do iv = 1, nverts
         gridobject%vert_grid(iv)%noc = 0
      end do
      do ic = 1, ncells
         do k = 1, 3
            iv = cells(k, ic)
            n = cnt(iv) + 1
            cnt(iv) = n
            gridobject%vert_grid(iv)%cell(n)%atti(CELLID) = ic
         end do
      end do
      do iv = 1, nverts
         gridobject%vert_grid(iv)%noc = cnt(iv)
      end do
      deallocate(cnt)
   end subroutine setup_mesh

   subroutine teardown_mesh()
      if (associated(gridobject%vert_grid)) deallocate(gridobject%vert_grid)
      if (associated(gridobject%cell_grid)) deallocate(gridobject%cell_grid)
      call clear_front_schedule()
   end subroutine teardown_mesh

   subroutine assert_no_same_front(nverts, cells, ncells, vlist, label, fails, only)
      integer, intent(in) :: nverts, ncells, vlist(nverts)
      integer, intent(in) :: cells(3, ncells)
      character(len=*), intent(in) :: label
      integer, intent(inout) :: fails
      integer, intent(in), optional :: only(:)
      integer :: vl(nverts, 1)
      integer :: j, k, l, f, first, last, m, mm, nbad
      integer :: pos(nverts), fro(nverts)
      integer :: vv(3), vu(2)

      vl(:, 1) = vlist
      call build_front_schedule(vl)
      do j = 1, nverts
         pos(vlist(j)) = j
      end do
      do f = 1, front_count(1)
         call front_bounds(f, 1, first, last)
         do j = first, last
            fro(scheduled_vertices(j, 1)) = f
         end do
      end do
      nbad = 0
      do k = 1, nverts
         if (present(only)) then
            if (.not.any(only == k)) cycle
         end if
         do j = 1, ncells
            vv = cells(:, j)
            do l = 1, 3
               if (vv(l) == k) then
                  vu(1) = vv(mod(l, 3) + 1)
                  vu(2) = vv(mod(l + 1, 3) + 1)
                  do mm = 1, 2
                     m = vu(mm)
                     if (pos(m) < pos(k) .and. fro(m) == fro(k)) then
                        nbad = nbad + 1
!                       Each pair separately, including the vu slot: which slot it
!                       concerns is the core of the diagnosis. Only vu(2) pairs
!                       should show up here; a vu(1) pair would mean
!                       that something else is wrong than this diagnosis describes.
                        print *, '   ', trim(label), ': k=', k, ' reads neighbour', m, &
                           ' via vu(', mm, ') in front', fro(k)
                     end if
                  end do
               end if
            end do
         end do
      end do
      if (nbad /= 0) then
         fails = fails + 1
         print *, trim(label), ': FAIL, same-front pairs =', nbad
      else
         print *, trim(label), ': ok'
      end if
      call teardown_mesh()
   end subroutine assert_no_same_front

   subroutine check_open_fan(fails)
      integer, intent(inout) :: fails
      integer :: cells(3, 2)
      integer :: vlist(4)
      ! T1=(1,2,3), T2=(1,3,4), both CCW. Vertex 2 sees neighbour 1 only as vu(2).
      cells(:, 1) = [1, 2, 3]
      cells(:, 2) = [1, 3, 4]
      vlist = [1, 2, 3, 4]
      call setup_mesh(4, 2, cells)
      call assert_no_same_front(4, cells, 2, vlist, 'open fan', fails)
   end subroutine check_open_fan

   subroutine check_closed_fan(fails)
      integer, intent(inout) :: fails
      integer :: cells(3, 3)
      integer :: vlist(4)
      ! Centre 4 with ring 1,2,3 CCW: only 4 has a closed fan and
      ! sees every neighbour as vu(1). The ring vertices are boundary themselves (open
      ! partial fans) and fall outside this check.
      cells(:, 1) = [4, 1, 2]
      cells(:, 2) = [4, 2, 3]
      cells(:, 3) = [4, 3, 1]
      vlist = [1, 2, 3, 4]
      call setup_mesh(4, 3, cells)
      call assert_no_same_front(4, cells, 3, vlist, 'closed fan', fails, only=[4])
   end subroutine check_closed_fan

end program test_front_dependency_completeness
