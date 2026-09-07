program test_input_fields
   use swan_input_fields, only: DEPTH, WLEVL, FRIC, UXB, UYB, WXI, WYI, &
      ASTDF, MUDLF, NPLAF, TURBF, AICEF, HICEF, HSSF, TSSF, DSSF, &
      CLEAR_INPUT_FIELDS, INPUT_FIELDS_ARE_CLEAR
   implicit none(type, external)

   call CLEAR_INPUT_FIELDS()
   call require(INPUT_FIELDS_ARE_CLEAR(), 'initial state is not clear')

   allocate(DEPTH(2), WLEVL(2), FRIC(2), UXB(2), UYB(2), WXI(2), WYI(2))
   allocate(ASTDF(2), MUDLF(2), NPLAF(2), TURBF(2))
   allocate(AICEF(2), HICEF(2), HSSF(2), TSSF(2), DSSF(2))

   call require(.not.INPUT_FIELDS_ARE_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_INPUT_FIELDS()
   call require(INPUT_FIELDS_ARE_CLEAR(), 'clear left input fields behind')
   call CLEAR_INPUT_FIELDS()
   call require(INPUT_FIELDS_ARE_CLEAR(), 'second clear changed empty state')

   print *, 'input fields clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_input_fields
