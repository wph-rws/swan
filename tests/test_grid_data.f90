program test_grid_data
   use SwanGriddata, only: xcugrd, ycugrd, xcugrdgl, ycugrdgl, ivertg, &
      vmark, CLEAR_GRID_DATA, GRID_DATA_IS_CLEAR
   implicit none(type, external)

   call CLEAR_GRID_DATA()
   call require(GRID_DATA_IS_CLEAR(), 'initial state is not clear')

   allocate(xcugrd(2), ycugrd(2), xcugrdgl(2), ycugrdgl(2))
   allocate(ivertg(2), vmark(2))

   call require(.not.GRID_DATA_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_GRID_DATA()
   call require(GRID_DATA_IS_CLEAR(), 'clear left grid data behind')
   call CLEAR_GRID_DATA()
   call require(GRID_DATA_IS_CLEAR(), 'second clear changed empty state')

   print *, 'grid data clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_grid_data
