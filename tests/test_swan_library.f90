program test_swan_library
! Contract tests: required API order, error result instead of STOP,
! idempotent finalize also after a failed initialisation. No second instance or
! overlapping execution (explicitly not claimed yet).
   use swan_library, only: swan_compute, swan_config_t, swan_finalize, &
      swan_initialize, swan_result_t, swan_state_t, swan_write_output, &
      SWAN_LIBRARY_OK
   implicit none

   type(swan_config_t) :: config
   type(swan_state_t) :: state
   type(swan_result_t) :: res

   ! compute without initialize is an error for the host, not a STOP
   res = swan_compute(state)
   if (res%code == SWAN_LIBRARY_OK) error stop "compute without init must fail"

   ! an empty input name is an error
   config%input_file = ""
   res = swan_initialize(config, state)
   if (res%code == SWAN_LIBRARY_OK) error stop "empty input must fail"
   ! finalize also after a failed initialisation, idempotent when called twice
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "finalize after failure must succeed"
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "double finalize must succeed"

   ! a missing file is an error
   config%input_file = "no-such-input-file.swn"
   res = swan_initialize(config, state)
   if (res%code == SWAN_LIBRARY_OK) error stop "missing input must fail"
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "finalize after failure must succeed"

   print *, "swan_library contract fails cleanly and finalizes idempotently"
end program test_swan_library
