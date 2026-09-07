program test_build_config
   use swan_build_config
   implicit none

   integer :: expected(4), iarg, status
   character(len=32) :: argument

   if (command_argument_count() /= size(expected)) then
      error stop 'test_build_config expects four integer values'
   end if
   do iarg = 1, size(expected)
      call get_command_argument(iarg, argument)
      read (argument, *, iostat=status) expected(iarg)
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
   if (len_trim(fork_identity) == 0) &
      error stop 'missing fork identity'
   if (index(fork_identity, 'wph-rws/swan') == 0) &
      error stop 'fork identity does not name wph-rws/swan'
   if (index(upstream_reference, 'tudelft') == 0) &
      error stop 'upstream reference does not name TU Delft'
end program test_build_config
