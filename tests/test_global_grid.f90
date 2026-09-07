program test_global_grid
   use swan_global_grid, only: KGRPGL, KGRBGL, XGRDGL, YGRDGL, &
      CLEAR_GLOBAL_GRID, GLOBAL_GRID_IS_CLEAR
   implicit none(type, external)

   call CLEAR_GLOBAL_GRID()
   call require(GLOBAL_GRID_IS_CLEAR(), 'initial state is not clear')

   allocate(KGRPGL(2,2), KGRBGL(2), XGRDGL(2,2), YGRDGL(2,2))

   call require(.not.GLOBAL_GRID_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_GLOBAL_GRID()
   call require(GLOBAL_GRID_IS_CLEAR(), 'clear left global grid behind')
   call CLEAR_GLOBAL_GRID()
   call require(GLOBAL_GRID_IS_CLEAR(), 'second clear changed empty state')

   print *, 'global grid clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_global_grid
