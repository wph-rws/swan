program test_contexts
   use swan_input_parser, only: command_reader_t, default_command_reader, &
      keywis, rdinit, getkar
   use swan_kinds, only: swan_double
   use swan_time, only: dttime, dtinti, time_context_t
   use swan_io_context, only: io_context_t, diagnostics_context_t, &
      capture_io_context, apply_io_context, &
      capture_diagnostics_context, apply_diagnostics_context
   use ocpcomm4, only: PRINTF, LEVERR, MAXERR, INPUTF, ITEST, ITRACE, HIOPEN
   implicit none

   call test_time_contexts
   call test_command_readers
   call test_io_contexts
   call test_reader_input_streams
   call test_diagnostics_stop
   call test_trace_context
   call test_error_reporting
   call test_file_opening_context
   call test_reader_own_log

contains

   subroutine test_reader_own_log
      use swan_input_parser, only: inkeyw
      type(command_reader_t) :: reader
      integer :: ulog, saved_leverr
      character(len=120) :: line

      saved_leverr = LEVERR
      LEVERR = 0

      ! Bind the reader to its own log and error status, then provoke a parser
      ! error by asking for a required keyword the (empty) input cannot supply.
      open (newunit=ulog, status='scratch')
      call rdinit(reader)
      reader%io%PRINTF = ulog
      reader%io%PRTEST = ulog
      reader%io%SCREEN = ulog
      reader%io_bound = .true.
      reader%diag%LEVERR = 0
      reader%diag%MAXERR = 4
      reader%diag_bound = .true.

      reader%ELTYPE = 'ERR'
      call inkeyw(reader, 'REQ', '    ')

      call require(reader%diag%LEVERR > 0, &
         "a bound reader did not record its error in its own context")
      call require(LEVERR == 0, &
         "a bound reader raised the shared global error level")
      rewind (ulog)
      read (ulog, '(A)', end=100) line
      call require(len_trim(line) > 0, "the reader's own log stayed empty")
100   close (ulog)

      LEVERR = saved_leverr
   end subroutine test_reader_own_log

   subroutine test_file_opening_context
      use swan_file_opening, only: FOR
      use ocpcomm2, only: LENFNM
      type(io_context_t) :: io
      integer :: unit1, iostat1, saved_hiopen
      !  FOR declares its filename dummy with a fixed length, so the actual
      !  argument has to carry that same length; a shorter one is a mismatch
      !  that -fcheck=all traps at the call.
      character(len=LENFNM) :: name1

      saved_hiopen = HIOPEN

      ! Give the context its own free-unit window and let FOR pick from it.
      call io%reset()
      io%FUNLO = 61
      io%FUNHI = 79
      io%PRINTF = PRINTF
      io%PRTEST = PRINTF

      unit1  = 0
      iostat1 = -2                     ! suppress messages
      name1  = 'swan_context_probe.tmp'
      call FOR (unit1, name1, 'UU', iostat1, io)

      call require(unit1 >= io%FUNLO .and. unit1 <= io%FUNHI, &
         "FOR did not draw a unit from the context's free-unit window")
      call require(io%HIOPEN == unit1, &
         "FOR did not record the opened unit in the context")

      close (unit1, status='delete')
      HIOPEN = saved_hiopen
   end subroutine test_file_opening_context

   subroutine test_error_reporting
      use swan_service_interfaces, only: msgerr
      type(diagnostics_context_t) :: diag
      type(io_context_t) :: io
      integer :: ulog, saved_leverr
      character(len=80) :: line

      saved_leverr = LEVERR
      LEVERR = 0

      open (newunit=ulog, status='scratch')
      io%PRINTF = ulog
      diag%LEVERR = 0
      diag%MAXERR = 4        ! high threshold: LEV=2 opens no error file

      call msgerr(2, 'context test message', diag, io)

      call require(diag%LEVERR == 2, &
         "msgerr did not raise the context error level")
      call require(LEVERR == 0, &
         "msgerr changed the global error level on a context call")
      rewind (ulog)
      read (ulog, '(A)') line
      call require(index(line, 'context test message') > 0, &
         "msgerr wrote no message to the io context stream")
      call require(index(line, 'Error') > 0, &
         "msgerr wrote the wrong severity prefix")
      close (ulog)

      LEVERR = saved_leverr
   end subroutine test_error_reporting

   subroutine test_trace_context
      use swan_service_interfaces, only: strace
      type(diagnostics_context_t) :: quiet, loud
      type(io_context_t) :: io
      integer :: ient, ulog
      character(len=64) :: line

      ! A quiet context (ITRACE == 0) suppresses tracing: IENT stays put.
      quiet%ITRACE = 0
      ient = 0
      call strace(ient, 'TESTQUIET', quiet)
      call require(ient == 0, "strace traced despite a quiet context")

      ! A loud context traces to its own stream: IENT advances and the trace
      ! line lands in the io context's unit, not the globals.
      open (newunit=ulog, status='scratch')
      loud%ITRACE = 5
      io%PRTEST = ulog; io%SCREEN = ulog; io%PRINTF = ulog
      ient = 0
      call strace(ient, 'TESTLOUD', loud, io)
      call require(ient == 1, "strace did not trace with a loud context")
      rewind (ulog)
      read (ulog, '(A)') line
      call require(index(line, 'TESTLOUD') > 0, &
         "strace wrote no trace line to the io context stream")
      close (ulog)
   end subroutine test_trace_context

   subroutine test_diagnostics_stop
      use swan_service_interfaces, only: stpnow
      type(diagnostics_context_t) :: calm, fatal
      integer :: saved_leverr, saved_maxerr

      saved_leverr = LEVERR; saved_maxerr = MAXERR
      LEVERR = 0; MAXERR = 1      ! global state: do not stop

      fatal%LEVERR = 4; fatal%MAXERR = 1
      calm%LEVERR = 0;  calm%MAXERR = 1

      call require(stpnow(fatal), &
         "stpnow ignored a fatal error level in its context")
      call require(.not. stpnow(calm), &
         "stpnow stopped on a calm context")
      call require(.not. stpnow(), &
         "stpnow stopped on a calm global state")

      ! MAXERR == -1 disables stopping even at a fatal level.
      fatal%MAXERR = -1
      call require(.not. stpnow(fatal), &
         "stpnow did not honor MAXERR == -1 in the context")

      LEVERR = saved_leverr; MAXERR = saved_maxerr
   end subroutine test_diagnostics_stop

   subroutine test_reader_input_streams
      type(command_reader_t) :: first, second
      integer :: u1, u2, ulog
      integer :: saved_printf, saved_itest, saved_itrace, saved_inputf

      ! The parser reads ITEST/ITRACE/PRINTF; SWANINIT does not run here, so
      ! pin them to sane values and route the line echo to a scratch log.
      saved_printf = PRINTF; saved_itest = ITEST
      saved_itrace = ITRACE; saved_inputf = INPUTF
      ITEST = 0; ITRACE = 0
      open (newunit=ulog, status='scratch')
      PRINTF = ulog

      open (newunit=u1, status='scratch'); write (u1, '(A)') 'ALPHA'; rewind (u1)
      open (newunit=u2, status='scratch'); write (u2, '(A)') 'BETA';  rewind (u2)

      call rdinit(first)
      call rdinit(second)
      first%io%INPUTF = u1;  first%io_bound = .true.
      second%io%INPUTF = u2; second%io_bound = .true.

      ! Force a fresh line read; each reader must draw from its own file.
      first%KARNR = 0;  call getkar(first)
      second%KARNR = 0; call getkar(second)

      call require(first%KAART(1:5) == 'ALPHA', &
         "a bound reader did not read from its own input file")
      call require(second%KAART(1:4) == 'BETA', &
         "the second bound reader read the wrong file")
      call require(first%KAR == 'A' .and. second%KAR == 'B', &
         "bound readers returned the wrong first character")

      PRINTF = saved_printf; ITEST = saved_itest
      ITRACE = saved_itrace; INPUTF = saved_inputf
      close (u1); close (u2); close (ulog)
   end subroutine test_reader_input_streams

   subroutine test_io_contexts
      type(io_context_t) :: first, second
      type(diagnostics_context_t) :: diag
      integer :: saved_printf, saved_leverr, saved_maxerr

      ! Two contexts hold independent stream units.
      first%PRINTF = 41
      second%PRINTF = 42
      call require(first%PRINTF == 41 .and. second%PRINTF == 42, &
         "io contexts unexpectedly share their print unit")
      call first%reset()
      call require(first%PRINTF == 4 .and. second%PRINTF == 42, &
         "resetting one io context changed another context")

      ! apply/capture round-trips through the OCPCOMM4 globals without loss.
      saved_printf = PRINTF
      saved_leverr = LEVERR
      saved_maxerr = MAXERR

      second%PRINTF = 77
      call apply_io_context(second)
      call require(PRINTF == 77, "apply_io_context did not publish the print unit")
      call capture_io_context(first)
      call require(first%PRINTF == 77, "capture_io_context did not read the print unit")

      LEVERR = 3
      MAXERR = 2
      call capture_diagnostics_context(diag)
      call require(diag%LEVERR == 3 .and. diag%MAXERR == 2, &
         "capture_diagnostics_context did not read the error status")
      diag%LEVERR = 0
      call apply_diagnostics_context(diag)
      call require(LEVERR == 0 .and. MAXERR == 2, &
         "apply_diagnostics_context did not publish the error status")

      ! Leave the globals as we found them.
      PRINTF = saved_printf
      LEVERR = saved_leverr
      MAXERR = saved_maxerr
   end subroutine test_io_contexts

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
