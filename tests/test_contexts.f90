program test_contexts
   use swan_input_parser, only: command_reader_t, default_command_reader, &
      keywis, rdinit
   use swan_kinds, only: swan_double
   use swan_time, only: dttime, dtinti, time_context_t
   implicit none

   call test_time_contexts
   call test_command_readers

contains

   subroutine test_time_contexts
      type(time_context_t) :: first, second
      integer :: first_date(6), first_next_day(6), second_date(6)
      integer :: round_trip(6)
      real :: first_seconds, second_seconds

      first_date = [2020, 1, 1, 12, 0, 0]
      first_next_day = [2020, 1, 2, 12, 0, 0]
      second_date = [2021, 1, 1, 6, 0, 0]

      first_seconds = dttime(first_date, first)
      second_seconds = dttime(second_date, second)

      call require(abs(first_seconds - 43200.0) < 0.1, &
         "first context has the wrong local time")
      call require(abs(second_seconds - 21600.0) < 0.1, &
         "second context has the wrong local time")
      call require(first%reference_set .and. second%reference_set, &
         "time contexts were not initialized")
      call require(first%reference_day /= second%reference_day, &
         "time contexts unexpectedly share a reference day")

      first_seconds = dttime(first_next_day, first)
      call require(abs(first_seconds - 129600.0) < 0.1, &
         "first context lost its reference day")
      call require(abs(dttime(second_date, second) - second_seconds) < 0.1, &
         "using the first context changed the second context")

      call dtinti(real(first_seconds, swan_double), round_trip, first)
      call require(all(round_trip == first_next_day), &
         "time conversion did not round-trip in its own context")

      call first%reset()
      call require(.not. first%reference_set .and. first%reference_day == 0, &
         "time context reset did not clear the reference")
      call require(second%reference_set, &
         "resetting one time context changed another context")
   end subroutine test_time_contexts

   subroutine test_command_readers
      type(command_reader_t) :: first, second

      default_command_reader%KEYWRD = "DEFAULT"
      default_command_reader%ELTYPE = "KEY"

      call rdinit(first)
      call rdinit(second)
      first%KEYWRD = "ALPHA"
      first%ELTYPE = "KEY"
      second%KEYWRD = "BETA"
      second%ELTYPE = "KEY"

      call require(keywis(first, "ALPHA"), &
         "the first reader did not recognize its keyword")
      call require(first%ELTYPE == "USED", &
         "the first reader did not consume its keyword")
      call require(second%ELTYPE == "KEY", &
         "the first reader changed the second reader")
      call require(default_command_reader%ELTYPE == "KEY", &
         "an explicit reader changed the default reader")

      call require(keywis(second, "BETA"), &
         "the second reader did not recognize its keyword")
      call second%reset()
      call require(second%ELTYPE == "USED" .and. second%KEYWRD == "", &
         "command reader reset did not restore its initial state")
      call require(first%KEYWRD == "ALPHA", &
         "resetting the second reader changed the first reader")
   end subroutine test_command_readers

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) error stop message
   end subroutine require

end program test_contexts
