program test_build_config
   use swan_build_config
   implicit none

   integer :: expected(4), index, status
   character(len=32) :: argument

   if (command_argument_count() /= size(expected)) then
      error stop 'test_build_config expects four integer values'
   end if
   do index = 1, size(expected)
      call get_command_argument(index, argument)
      read (argument, *, iostat=status) expected(index)
      if (status /= 0) error stop 'invalid expected build-config value'
   end do

   if (sequential_record_length /= expected(1)) &
      error stop 'unexpected sequential record length'
   if (print_record_length /= expected(2)) &
      error stop 'unexpected print record length'
   if (default_maximum_unit /= expected(3)) &
      error stop 'unexpected maximum unit'
   if (default_free_unit_start /= expected(4)) &
      error stop 'unexpected free-unit start'
end program test_build_config
