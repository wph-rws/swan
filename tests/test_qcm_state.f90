program test_qcm_state
   use SwanQCM, only: kx, ky, xpsc, xpd, ypd, kxd, kyd, disbk0, disbk1, &
      CLEAR_QCM_STATE, QCM_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_QCM_STATE()
   call require(QCM_STATE_IS_CLEAR(), 'initial state is not clear')

   allocate(kx(2), ky(2), xpsc(2), xpd(2), ypd(2), kxd(2), kyd(2))
   allocate(disbk0(2), disbk1(2))

   call require(.not.QCM_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_QCM_STATE()
   call require(QCM_STATE_IS_CLEAR(), 'clear left QCM state behind')
   call CLEAR_QCM_STATE()
   call require(QCM_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'QCM state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_qcm_state
