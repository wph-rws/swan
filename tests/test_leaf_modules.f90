program test_leaf_modules
!  Unit fixtures for the small, pure modules that came out of the subsystem
!  split. These are deterministic conversions and formatters: they need no grid
!  and no run, so a regression shows up here rather than as a shifted figure in
!  an end-to-end case.
   use swan_angle_conversions, only: DEGCNV, ANGRAD, ANGDEG
   use swan_number_formatting, only: INTSTR, NUMSTR
   use swan_text_utilities, only: UPCASE
   use swcomm3, only: BNAUT, DNORTH, PI
   use ocpcomm4, only: INAN, RNAN
   implicit none

   call test_nautical_conversion
   call test_radian_conversion
   call test_integer_formatting
   call test_number_formatting
   call test_uppercase

contains

   subroutine test_nautical_conversion
      real :: saved_dnorth
      logical :: saved_bnaut
      !  Range checks hold the result rather than calling DEGCNV twice in one
      !  expression, which would make the compiler report an eliminated
      !  duplicate call and so raise the diagnostic count for a test artefact.
      real :: wrapped

      saved_bnaut = BNAUT
      saved_dnorth = DNORTH

      ! Cartesian mode passes the angle through untouched.
      BNAUT = .false.
      DNORTH = 90.0
      call require(close(DEGCNV(45.0), 45.0), &
         "DEGCNV changed the angle outside nautical mode")

      ! Nautical mode mirrors around 180 + DNORTH and wraps into [0,360).
      BNAUT = .true.
      DNORTH = 90.0
      call require(close(DEGCNV(0.0), 270.0), &
         "DEGCNV converted north incorrectly")
      wrapped = DEGCNV(-350.0)
      call require(wrapped >= 0.0 .and. wrapped < 360.0, &
         "DEGCNV did not wrap a negative result into [0,360)")
      wrapped = DEGCNV(300.0)
      call require(wrapped >= 0.0 .and. wrapped < 360.0, &
         "DEGCNV did not wrap a result at or above 360")

      ! The conversion is its own inverse: applying it twice restores the angle.
      call require(close(DEGCNV(DEGCNV(37.5)), 37.5), &
         "DEGCNV is not self-inverse")

      BNAUT = saved_bnaut
      DNORTH = saved_dnorth
   end subroutine test_nautical_conversion

   subroutine test_radian_conversion
      real :: saved_pi

      ! ANGRAD and ANGDEG read PI from SWCOMM3, which SWINIT fills at runtime;
      ! in a bare unit test it is still zero. Setting it here keeps the fixture
      ! honest about that dependency instead of hiding it.
      saved_pi = PI
      PI = 4.0 * atan(1.0)

      call require(close(ANGRAD(180.0), 3.14159265), &
         "ANGRAD did not convert 180 degrees to pi")
      call require(close(ANGDEG(ANGRAD(123.75)), 123.75), &
         "degree/radian conversion did not round-trip")

      PI = saved_pi
   end subroutine test_radian_conversion

   subroutine test_integer_formatting
      integer :: value

      call require(trim(adjustl(INTSTR(42))) == "42", &
         "INTSTR formatted a positive integer incorrectly")
      call require(trim(adjustl(INTSTR(0))) == "0", &
         "INTSTR formatted zero incorrectly")

      ! INTSTR used to consume its argument while formatting it. Guard that:
      ! the caller's value has to survive the call.
      value = 12345
      call require(trim(adjustl(INTSTR(value))) == "12345", &
         "INTSTR formatted a variable incorrectly")
      call require(value == 12345, &
         "INTSTR modified the integer supplied by its caller")
   end subroutine test_integer_formatting

   subroutine test_number_formatting
      ! NUMSTR selects on sentinels rather than on the format: it writes IVAL
      ! unless IVAL is INAN, and only then falls through to RVAL. Passing a real
      ! format with a live integer therefore writes the integer with an F edit
      ! descriptor and aborts at runtime, so the sentinels are the contract.
      call require(trim(adjustl(NUMSTR(7, RNAN, '(I4)'))) == "7", &
         "NUMSTR did not format the integer branch")
      call require(index(NUMSTR(INAN, 1.5, '(F6.2)'), "1.50") > 0, &
         "NUMSTR did not format the real branch")
   end subroutine test_number_formatting

   subroutine test_uppercase
      character(len=12) :: text

      text = 'CoMPute  x9'
      call UPCASE(text)
      call require(text == 'COMPUTE  X9 ', &
         "UPCASE did not uppercase letters while leaving the rest alone")
   end subroutine test_uppercase

   logical function close(actual, expected)
      real, intent(in) :: actual, expected
      close = abs(actual - expected) < 1.0e-4
   end function close

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) error stop message
   end subroutine require

end program test_leaf_modules
