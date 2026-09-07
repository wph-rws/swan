program test_bragg_state
   use SwanBraggScat, only: fb, fbdxy, &
      CLEAR_BRAGG_STATE, BRAGG_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_BRAGG_STATE()
   call require(BRAGG_STATE_IS_CLEAR(), 'initial state is not clear')

   allocate(fb(2,2,2), fbdxy(2,2,2,2))

   call require(.not.BRAGG_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_BRAGG_STATE()
   call require(BRAGG_STATE_IS_CLEAR(), 'clear left Bragg state behind')
   call CLEAR_BRAGG_STATE()
   call require(BRAGG_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'Bragg state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_bragg_state
