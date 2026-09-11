program test_mpi_lifecycle_backend
   use swan_mpi_lifecycle_backend, only: coherent_coupling_enabled, &
      lifecycle_abort, lifecycle_barrier, lifecycle_finalize, &
      lifecycle_initialize, lifecycle_rank, lifecycle_size
   implicit none(type, external)

   integer :: expected, ierr, process_count, rank, status
   character(len=32) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_mpi_lifecycle_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected lifecycle selection'
   if (coherent_coupling_enabled .neqv. (expected == 1)) &
      error stop 'unexpected MPI lifecycle backend selection'

   if (coherent_coupling_enabled) then
      ierr = 23
      rank = 7
      process_count = 11
      call lifecycle_initialize(ierr)
      call lifecycle_rank(rank, ierr)
      call lifecycle_size(process_count, ierr)
      call lifecycle_barrier(ierr)
      call lifecycle_abort(4, ierr)
      call lifecycle_finalize(ierr)
      if (ierr /= 23 .or. rank /= 7 .or. process_count /= 11) &
         error stop 'coherent lifecycle must leave externally owned state unchanged'
   end if
end program test_mpi_lifecycle_backend
