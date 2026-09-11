program test_sweep_sector_budget
!
!  The source-term budgets may be cleared over the sweep sector only when that
!  sector holds every active bin.  SwanSweepSel keeps just the last pair of
!  sector boundaries, so a sweep whose active directions fall apart into more
!  than one group -- reachable on an unstructured grid with a current -- leaves
!  active bins outside IDCMIN..IDCMAX.  ADDDIS reads those bins and no source
!  routine writes them, so clearing only the sector would let the previous
!  point's values survive into the budget output.
!
!  Guard both halves of the contract: the selector really does produce such a
!  sweep, and SWEEP_WINDOW_COVERS_ACTIVE_BINS rejects it.
!
   use swan_spectral_grid, only: MDC, MSC, FULCIR, spectral_window_t, &
      sweep_window_covers_active_bins
   use swan_physics_selection, only: ICUR
   use swan_sweep_sel, only: SwanSweepSel
   implicit none(type, external)

   integer, parameter :: NDIR = 36, NFRQ = 4
   integer, target    :: idcmin(NFRQ), idcmax(NFRQ), iscmin(NDIR), iscmax(NDIR)
   type(spectral_window_t) :: WINDOW
   logical :: anybin(NDIR,NFRQ)
   real    :: cax(NDIR,NFRQ,1), cay(NDIR,NFRQ,1), rdx(2), rdy(2), spcsig(NFRQ)
   integer :: idtot, isslow, istot, is, id, uncleared, active
   logical :: cleared(NDIR,NFRQ)

   MDC = NDIR
   MSC = NFRQ
   FULCIR = .true.
   ICUR = 1
   WINDOW%idcmin => idcmin
   WINDOW%idcmax => idcmax
   WINDOW%iscmin => iscmin
   WINDOW%iscmax => iscmax
   spcsig = [(0.1*real(is), is = 1, NFRQ)]

!  A sweep keeps the bins with cax >= 0 and cay >= 0 after projection.  With a
!  current the transport velocity is no longer monotonic in the direction bin,
!  so make two separated groups of directions satisfy the test.
   rdx = [1.0, 0.0]
   rdy = [0.0, 1.0]
   cax = -1.0
   cay = -1.0
   do is = 1, NFRQ
      do id = 2, 5
         cax(id,is,1) = 1.0
         cay(id,is,1) = 1.0
      end do
      do id = 20, 24
         cax(id,is,1) = 1.0
         cay(id,is,1) = 1.0
      end do
   end do

   call SwanSweepSel (WINDOW, anybin, idtot, isslow, istot, cax, cay, rdx, rdy, spcsig, 1)

   active = count(anybin)
   call require(active == NFRQ*9, 'selector did not activate the two direction groups')

!  Reproduce the sector-scoped clearing of SOURCE and count what it misses.
   cleared = .false.
   do is = 1, WINDOW%isstop
      if ( WINDOW%idcmax(is) < WINDOW%idcmin(is) ) cycle
      do id = WINDOW%idcmin(is), WINDOW%idcmax(is)
         cleared(mod(id - 1 + MDC, MDC) + 1, is) = .true.
      end do
   end do
   uncleared = count(anybin .and. .not.cleared)
   print '(a,i0,a,i0)', ' split sweep: active bins ', active, &
      ', left uncleared by sector-only clearing ', uncleared

   call require(uncleared > 0, &
      'split sweep no longer leaves active bins outside the sector; revisit this test')
   call require(.not.sweep_window_covers_active_bins(WINDOW, anybin), &
      'coverage check accepted a sweep whose sector misses active bins')

!  With the coverage check in front of it, the clearing SOURCE performs leaves
!  no active bin behind: the fallback clears the whole spectrum.
   if ( .not.sweep_window_covers_active_bins(WINDOW, anybin) ) cleared = .true.
   call require(count(anybin .and. .not.cleared) == 0, &
      'guarded clearing still left an active bin with stale budget values')

!  A single contiguous group, wrapped across the end of the circle, must stay
!  on the fast path: that is the case every structured sweep produces.
   cax = -1.0
   cay = -1.0
   do is = 1, NFRQ
      do id = 1, 3
         cax(id,is,1) = 1.0
         cay(id,is,1) = 1.0
      end do
      do id = 34, NDIR
         cax(id,is,1) = 1.0
         cay(id,is,1) = 1.0
      end do
   end do
   call SwanSweepSel (WINDOW, anybin, idtot, isslow, istot, cax, cay, rdx, rdy, spcsig, 1)
   call require(count(anybin) == NFRQ*6, 'wrapped group was not activated as expected')
   call require(sweep_window_covers_active_bins(WINDOW, anybin), &
      'coverage check rejected a single wrapped sector')

!  An active bin above ISSTOP is never reached by the sector loop either.
   anybin = .false.
   anybin(1,1) = .true.
   idcmin(1) = 1
   idcmax(1) = 1
   WINDOW%isstop = 1
   call require(sweep_window_covers_active_bins(WINDOW, anybin), &
      'coverage check rejected a sweep it does cover')
   anybin(1,NFRQ) = .true.
   call require(.not.sweep_window_covers_active_bins(WINDOW, anybin), &
      'coverage check missed an active bin above ISSTOP')

   print *, 'sweep sector budget clearing contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_sweep_sector_budget
