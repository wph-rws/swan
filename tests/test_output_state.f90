program test_output_state
   use OUTP_DATA, only: FOPS, FORQ, COPS, LOPS, LORQ, LCOMPGRD, NREOQ, &
      OUTP_FILES, CLEAR_OUTPUT_STATE, OUTPUT_STATE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_OUTPUT_STATE()
   call require(OUTPUT_STATE_IS_CLEAR(), 'initial state is not clear')

   LOPS = .TRUE.
   LORQ = .TRUE.
   LCOMPGRD = .TRUE.
   NREOQ = 1
   OUTP_FILES(1) = 'output.tbl'
   allocate(FOPS%XP(2), FOPS%YP(2), FOPS%XQ(2), FOPS%YQ(2))
   allocate(FOPS%NEXTOPS)
   allocate(FOPS%NEXTOPS%XP(1))
   allocate(FORQ%IVTYP(2), FORQ%FAC(2))
   allocate(FORQ%NEXTORQ)
   allocate(FORQ%NEXTORQ%IVTYP(1))
   COPS => FOPS

   call require(.not.OUTPUT_STATE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_OUTPUT_STATE()
   call require(OUTPUT_STATE_IS_CLEAR(), 'clear left output state behind')
   call CLEAR_OUTPUT_STATE()
   call require(OUTPUT_STATE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'output state clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_output_state
