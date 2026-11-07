program test_extrapolation_history
   use swan_parallel, only: SWINITMPI, SWEXITMPI
   use swan_stop_extrapolation, only: hs_extrapolation_t
   use swan_computational_grid, only: MXC, MYC
   use swan_spectral_grid, only: MSC, MDC, FRINTF, DDIR
   use swan_physical_settings, only: DEPMIN
   use swan_numerics, only: PNUMS, PNUMS_DABS, PNUMS_DREL, PNUMS_NPNTS
   use M_PARALL, only: LMXF, LMXL, LMYF, LMYL
   implicit none(type, external)
   type(hs_extrapolation_t) :: history
   real :: ac(1,1,2), sigma(1), depth(2)
   integer :: grid(1,1)
   logical :: checked
   call SWINITMPI
   MXC = 1
   MYC = 1
   MSC = 1
   MDC = 1
   FRINTF = 1.
   DDIR = 1.
   DEPMIN = .01
   LMXF = .true.
   LMXL = .true.
   LMYF = .true.
   LMYL = .true.
   PNUMS(PNUMS_DABS) = .01
   PNUMS(PNUMS_DREL) = 0.
   PNUMS(PNUMS_NPNTS) = 100.
   sigma = 1.
   grid = 2
   depth = 20.
   ac = 1./16.
   checked = history%check(2, ac, sigma, depth, grid)
   if (checked .or. allocated(history%h1)) error stop 'disabled EXTRAP allocated history'
   call history%start(2, 2)
   call checkpoint(2, 1.)
   call checkpoint(4, 1.)
   call checkpoint(6, 1.)
   if (.not.history%reached) error stop 'constant wet point did not pass'
   depth(2) = 0.
   call checkpoint(8, 1.)
   depth(2) = 20.
   call checkpoint(10, 1.)
   if (history%reached) error stop 'newly wet point used its old dry-period history'
   call checkpoint(12, 1.)
   if (history%reached) error stop 'two wet checkpoints are insufficient'
   call checkpoint(14, 1.)
   if (.not.history%reached) error stop 'three constant wet checkpoints did not pass'
   call checkpoint(16, 1.1)
   if (history%reached) error stop 'new growing change passed'
   call checkpoint(18, 1.)
   if (history%reached) error stop 'non-shrinking swing passed'
   call SWEXITMPI
contains
subroutine checkpoint(iter, hs)
   integer, intent(in) :: iter
   real, intent(in) :: hs
   ac = hs**2/16.
   checked = history%check(iter, ac, sigma, depth, grid)
   if (.not.checked) error stop 'checkpoint was skipped'
end subroutine checkpoint
end program test_extrapolation_history
