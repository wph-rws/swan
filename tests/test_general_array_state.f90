program test_general_array_state
   use M_GENARR, only: KGRPNT, KGRBND, XYTST, AC2, XCGRID, YCGRID, &
      SPCSIG, SPCDIR, SINBAC, SAVE_SINBAC, &
      CLEAR_GENERAL_ARRAY_STATE, GENERAL_ARRAY_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_GENERAL_ARRAY_STATE()
   call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'initial state is not clear')

   allocate(KGRPNT(2,2), KGRBND(2), XYTST(2))
   allocate(AC2(2,2,2), XCGRID(2,2), YCGRID(2,2))
   allocate(SPCSIG(2), SPCDIR(2,2))
   allocate(SINBAC(2,2,2))
   SAVE_SINBAC = .TRUE.

   call require(.not.GENERAL_ARRAY_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_GENERAL_ARRAY_STATE()
   call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'clear left array state behind')
   call CLEAR_GENERAL_ARRAY_STATE()
   call require(GENERAL_ARRAY_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'general array state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_general_array_state
