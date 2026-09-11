program test_esmf_coupling_backend
   use M_GENARR, only: SAVE_SINBAC, SINBAC
   use swan_esmf_coupling_backend, only: &
      accumulate_exponential_wind_input, esmf_coupling_enabled, &
      reset_exponential_wind_input
   implicit none(type, external)

   integer :: expected, status
   character(len=32) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_esmf_coupling_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected ESMF selection'
   if (esmf_coupling_enabled .neqv. (expected == 1)) &
      error stop 'unexpected ESMF coupling backend selection'

   allocate(SINBAC(2,2,2))
   SINBAC = 4.0
   SAVE_SINBAC = .true.
   call reset_exponential_wind_input(1)
   call accumulate_exponential_wind_input(2, 1, 1, 1.5)
   if (esmf_coupling_enabled) then
      if (abs(SINBAC(1,1,1)) > epsilon(1.0) .or. &
          abs(SINBAC(2,1,1)-1.5) > epsilon(1.0)) &
         error stop 'enabled ESMF coupling did not record wind input'
   else
      if (any(abs(SINBAC-4.0) > epsilon(1.0))) &
         error stop 'disabled ESMF coupling changed wind-input state'
   end if
   SAVE_SINBAC = .false.
   deallocate(SINBAC)
end program test_esmf_coupling_backend
