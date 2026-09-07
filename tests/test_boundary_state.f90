program test_boundary_state
   use M_BNDSPEC, only: ALOBND, FBNDFIL, FBS, FBGP, CUBGP, &
      LBFILS, LBS, LBGP, CLEAR_BOUNDARY_STATE, &
      BOUNDARY_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_BOUNDARY_STATE()
   call require(BOUNDARY_STATE_IS_CLEAR(), 'initial state is not clear')

   ALOBND = .TRUE.
   LBFILS = .TRUE.
   LBS = .TRUE.
   LBGP = .TRUE.
   allocate(FBNDFIL%BSPLOC(2), FBNDFIL%BSPDIR(2), FBNDFIL%BSPFRQ(2))
   allocate(FBNDFIL%NEXTBSPC)
   allocate(FBNDFIL%NEXTBSPC%BSPLOC(1))
   allocate(FBS%NEXTBS)
   allocate(FBS%NEXTBS%NEXTBS)
   allocate(FBGP%NEXTBGP)
   allocate(FBGP%NEXTBGP%NEXTBGP)
   CUBGP => FBGP%NEXTBGP%NEXTBGP

   call require(.not.BOUNDARY_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_BOUNDARY_STATE()
   call require(BOUNDARY_STATE_IS_CLEAR(), 'clear left boundary state behind')
   call CLEAR_BOUNDARY_STATE()
   call require(BOUNDARY_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'boundary state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_boundary_state
