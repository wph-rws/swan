program test_front_dependency_completeness
!  Regressietest bij het ongestructureerde OpenMP-determinisme.
!
!  De graph-achterkant mag geen gelezen buur in hetzelfde front plaatsen als
!  zijn lezer: de solver leest ac2 van BEIDE buren vu(1),vu(2)
!  (SwanCompUnstruc st_kc + SwanTranspX) en schrijft ac2 van de vertex zelf,
!  met alleen een barriere TUSSEN fronten. Een buur in hetzelfde front is
!  een ongesynchroniseerde gelijktijdige lees/schrijf.
!
!  Mesh 1 (open waaier, rand): vierkant in 2 CCW-driehoeken; vertex 2 ziet
!  buur 1 alleen ooit als vu(2) -> vangt de ontbrekende vu(2)-rand. Deze mesh
!  levert twee zulke paren op (2 leest 1, en 4 leest 3), beide via vu(2).
!  Mesh 2 (gesloten waaier, binnen): center met 3 CCW-driehoeken; elke buur
!  komt als vu(1) langs -> controle, geen valse alarm.
!
!  Zolang de vu(2)-rand ontbreekt hoort mesh 1 te FALEN; CMakeLists.txt draagt
!  de test daarom als WILL_FAIL totdat de ontbrekende rand is hersteld.
   use swan_front_scheduling_backend, only: fixed_front_scheduling_enabled, &
      build_front_schedule, clear_front_schedule, front_count, front_bounds, &
      scheduled_vertices
   use SwanGridobjects, only: CELLID, CELLV1, CELLV2, CELLV3, gridobject
   implicit none(type, external)

   integer :: fails

   fails = 0
   if (fixed_front_scheduling_enabled) then
      print *, 'fixed backend: dependency ordering not applicable, skip'
      stop 0
   end if

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
!                       Elk paar apart, inclusief de vu-slot: welke slot het
!                       betreft is de kern van de diagnose. Alleen vu(2)-paren
!                       horen hier op te duiken; een vu(1)-paar zou betekenen
!                       dat er iets anders mis is dan deze diagnose beschrijft.
                        print *, '   ', trim(label), ': k=', k, ' leest buur', m, &
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
      ! T1=(1,2,3), T2=(1,3,4), beide CCW. Vertex 2 ziet buur 1 alleen als vu(2).
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
      ! Center 4 met ring 1,2,3 CCW: alleen 4 heeft een gesloten waaier en
      ! ziet elke buur als vu(1). De ringvertices zijn zelf rand (open
      ! deelwaaiers) en vallen buiten deze controle.
      cells(:, 1) = [4, 1, 2]
      cells(:, 2) = [4, 2, 3]
      cells(:, 3) = [4, 3, 1]
      vlist = [1, 2, 3, 4]
      call setup_mesh(4, 3, cells)
      call assert_no_same_front(4, cells, 3, vlist, 'closed fan', fails, only=[4])
   end subroutine check_closed_fan

end program test_front_dependency_completeness
