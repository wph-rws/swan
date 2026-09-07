program test_obstacle_state
   use M_OBSTA, only: OBSTDAT, FOBSTAC, OBSTDONE, &
      CLEAR_OBSTACLE_STATE, OBSTACLE_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_OBSTACLE_STATE()
   call require(OBSTACLE_STATE_IS_CLEAR(), 'initial state is not clear')

   OBSTDONE = .TRUE.
   allocate(FOBSTAC%TRCF1D(3), FOBSTAC%TRCF2D(2,2))
   allocate(FOBSTAC%IGFRQD(4), FOBSTAC%XCRP(2), FOBSTAC%YCRP(2))
   allocate(FOBSTAC%NEXTOBST)
   allocate(FOBSTAC%NEXTOBST%TRCF1D(1))
   allocate(FOBSTAC%NEXTOBST%XCRP(2), FOBSTAC%NEXTOBST%YCRP(2))
   allocate(FOBSTAC%NEXTOBST%NEXTOBST)
   allocate(FOBSTAC%NEXTOBST%NEXTOBST%IGFRQD(2))

   call require(.not.OBSTACLE_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_OBSTACLE_STATE()
   call require(OBSTACLE_STATE_IS_CLEAR(), 'clear left obstacle state behind')

   ! The owner contract is deliberately idempotent: SWCLME may be called after
   ! an input or setup error that already released part of the state.
   call CLEAR_OBSTACLE_STATE()
   call require(OBSTACLE_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'obstacle state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_obstacle_state
