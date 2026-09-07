program test_swan_library
! Contracttests: verplichte API-volgorde, foutresultaat i.p.v. STOP,
! idempotent finalize ook na mislukte initialisatie. Geen tweede instantie of
! overlappende uitvoering (expliciet nog niet geclaimd).
   use swan_library, only: swan_compute, swan_config_t, swan_finalize, &
      swan_initialize, swan_result_t, swan_state_t, swan_write_output, &
      SWAN_LIBRARY_OK
   implicit none

   type(swan_config_t) :: config
   type(swan_state_t) :: state
   type(swan_result_t) :: res

   ! compute zonder initialize is een fout aan de host, geen STOP
   res = swan_compute(state)
   if (res%code == SWAN_LIBRARY_OK) error stop "compute zonder init moet falen"

   ! lege invoernaam is een fout
   config%input_file = ""
   res = swan_initialize(config, state)
   if (res%code == SWAN_LIBRARY_OK) error stop "lege invoer moet falen"
   ! finalize ook na mislukte initialisatie, tweemaal idempotent
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "finalize na falen moet slagen"
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "dubbele finalize moet slagen"

   ! ontbrekend bestand is een fout
   config%input_file = "bestaat-niet-als-invoer.swn"
   res = swan_initialize(config, state)
   if (res%code == SWAN_LIBRARY_OK) error stop "ontbrekende invoer moet falen"
   res = swan_finalize(state)
   if (res%code /= SWAN_LIBRARY_OK) error stop "finalize na falen moet slagen"

   print *, "swan_library contract faalt-ordelijk en finaliseert idempotent"
end program test_swan_library
