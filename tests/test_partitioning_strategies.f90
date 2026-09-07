program test_partitioning_strategies
   use M_PARALL, only: IWEIG, NPROC, PARLL
   use swan_build_config, only: jacobi_sweep_enabled
   use swan_global_grid, only: KGRPGL
   use swan_parallel, only: SWPARTIT
   implicit none(type, external)

   integer, parameter :: MXC = 6, MYC = 5
   integer :: expected, index, status
   integer :: owned(MXC,MYC), part_counts(3)
   logical :: active(MXC,MYC)
   character(len=8) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_partitioning_strategies expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected partitioning strategy'
   if (jacobi_sweep_enabled .neqv. (expected == 1)) &
      error stop 'unexpected partitioning strategy selected'

   active = .true.
   active(1,1) = .false.
   active(2,1) = .false.
   active(6,2) = .false.
   active(3,3) = .false.
   active(1,5) = .false.

   allocate(KGRPGL(MXC,MYC), IWEIG(3))
   KGRPGL = merge(2, 1, active)
   IWEIG = [1, 2, 1]
   NPROC = size(IWEIG)
   PARLL = .true.
   owned = 0

   call SWPARTIT(owned, MXC, MYC)

   if (any(owned == 0 .neqv. .not.active)) &
      error stop 'partitioning changed the active-point mask'
   if (any(owned < 0) .or. any(owned > NPROC)) &
      error stop 'partitioning produced an invalid owner'
   do index = 1, NPROC
      part_counts(index) = count(owned == index)
   end do
   if (any(part_counts == 0)) &
      error stop 'partitioning produced an empty part'
   if (sum(part_counts) /= count(active)) &
      error stop 'partitioning lost active points'
   if (part_counts(2) <= part_counts(1) .or. &
       part_counts(2) <= part_counts(3)) &
      error stop 'partitioning ignored the configured process weights'

   deallocate(KGRPGL)
   PARLL = .false.
end program test_partitioning_strategies
