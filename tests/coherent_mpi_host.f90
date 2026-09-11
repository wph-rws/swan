program coherent_mpi_host
!  A host program that manages MPI itself and calls SWAN as a library,
!  as a coupler does in a COH build.
!
!  With COH, SWAN leaves starting and stopping MPI to the host. That
!  is the only thing this variant does differently, and no other test reaches it:
!  swan.exe itself does not start MPI, so a standalone run shows nothing. Here
!  the host starts MPI, SWAN runs the quick test, and afterwards MPI must still
!  be open and usable. A SWAN that starts or shuts down MPI itself makes this
!  program hang in the double MPI_INIT or in the MPI_BARRIER afterwards.
!
!  The host writes to error_unit after the run: SWAN has already closed
!  unit 6 by then.
   use mpi
   use swan_library, only: swan_compute, swan_config_t, swan_finalize, &
      swan_initialize, swan_result_t, swan_state_t, swan_write_output, &
      SWAN_LIBRARY_OK
   use swan_parallel, only: SWINITMPI, SWEXITMPI
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none(type, external)

   type(swan_config_t) :: config
   type(swan_state_t) :: state
   type(swan_result_t) :: res, fin
   integer :: ierr
   logical :: finalized, initialized

   call MPI_INIT(ierr)
   if (ierr /= MPI_SUCCESS) error stop 'the host could not start MPI'

   call SWINITMPI

   config%input_file = 'INPUT'
   res = swan_initialize(config, state)
   if (res%code == SWAN_LIBRARY_OK) res = swan_compute(state)
   if (res%code == SWAN_LIBRARY_OK) res = swan_write_output(state)
   fin = swan_finalize(state)

   call SWEXITMPI

   if (res%code /= SWAN_LIBRARY_OK) then
      write (error_unit, '(A)') 'SWAN run failed: '//trim(res%message)
      error stop 1
   end if
   if (fin%code /= SWAN_LIBRARY_OK) error stop 'SWAN did not clean up'

   call MPI_INITIALIZED(initialized, ierr)
   call MPI_FINALIZED(finalized, ierr)
   if (.not.initialized .or. finalized) &
      error stop 'SWAN shut down the MPI environment of the host'
   call MPI_BARRIER(MPI_COMM_WORLD, ierr)
   if (ierr /= MPI_SUCCESS) error stop 'MPI is no longer usable after the SWAN run'

   call MPI_FINALIZE(ierr)
end program coherent_mpi_host
