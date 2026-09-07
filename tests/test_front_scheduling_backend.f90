program test_front_scheduling_backend
   use swan_build_config, only: fixed_front_enabled
   use swan_front_scheduling_backend, only: fixed_front_scheduling_enabled
   implicit none(type, external)

   integer :: expected, status
   character(len=8) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_front_scheduling_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected front scheduler value'
   if (fixed_front_enabled .neqv. (expected == 1)) &
      error stop 'unexpected configured front scheduler'
   if (fixed_front_scheduling_enabled .neqv. fixed_front_enabled) &
      error stop 'front scheduler and build configuration disagree'
end program test_front_scheduling_backend
