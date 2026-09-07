program test_computation_data
   use SwanCompdata, only: vlist, blist, bvertg, bmark, &
      CLEAR_COMPUTATION_DATA, COMPUTATION_DATA_IS_CLEAR
   implicit none(type, external)

   call CLEAR_COMPUTATION_DATA()
   call require(COMPUTATION_DATA_IS_CLEAR(), 'initial state is not clear')

   allocate(vlist(2,2), blist(2,2), bvertg(2,2), bmark(2,2))

   call require(.not.COMPUTATION_DATA_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_COMPUTATION_DATA()
   call require(COMPUTATION_DATA_IS_CLEAR(), 'clear left computation data behind')
   call CLEAR_COMPUTATION_DATA()
   call require(COMPUTATION_DATA_IS_CLEAR(), 'second clear changed empty state')

   print *, 'computation data clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_computation_data
