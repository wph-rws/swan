program test_metis_backend
   use swan_metis_partition_backend
   implicit none(type, external)

   integer :: expected, status
   logical :: logcom(7), original_logcom(7)
   real :: field(2), original_field(2), original_ownership(2)
   real, allocatable :: ownership(:)
   character(len=8) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_metis_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected METIS capability value'
   if (metis_enabled .neqv. (expected == 1)) &
      error stop 'unexpected METIS backend selected'

   if (.not.metis_enabled) then
      logcom = [.true., .false., .true., .false., .true., .false., .true.]
      original_logcom = logcom
      field = [1.25, -2.5]
      original_field = field

      call metis_copy_ownership(ownership)
      if (allocated(ownership)) &
         error stop 'disabled METIS ownership allocated data'
      ownership = [3.0, 4.0]
      original_ownership = ownership

      call metis_decompose(logcom)
      call metis_collect_boundary_points()
      call metis_exchange_real(field)
      call metis_copy_ownership(ownership)
      if (metis_vertex_is_resident(1)) &
         error stop 'disabled METIS backend reports a resident vertex'
      call metis_release()

      if (any(logcom .neqv. original_logcom)) &
         error stop 'disabled METIS decomposition changed state'
      if (any(abs(field - original_field) > 0.0)) &
         error stop 'disabled METIS exchange changed data'
      if (any(abs(ownership - original_ownership) > 0.0)) &
         error stop 'disabled METIS ownership changed data'
   end if
end program test_metis_backend
