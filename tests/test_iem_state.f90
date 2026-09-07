program test_iem_state
   use SwanIEM, only: iss, iwt, itt, freq, E0, Ebig, &
      CLEAR_IEM_STATE, IEM_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_IEM_STATE()
   call require(IEM_STATE_IS_CLEAR(), 'initial state is not clear')

   allocate(iss(2), iwt(2), itt(2), freq(2), E0(2,2), Ebig(2,2,2))

   call require(.not.IEM_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_IEM_STATE()
   call require(IEM_STATE_IS_CLEAR(), 'clear left IEM state behind')
   call CLEAR_IEM_STATE()
   call require(IEM_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'IEM state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_iem_state
