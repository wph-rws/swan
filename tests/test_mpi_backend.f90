program test_mpi_backend
   use swan_mpi_backend, only: mpi_backend_allgather_integer, &
      mpi_backend_allgatherv_integer, mpi_backend_allreduce_integer, &
      mpi_backend_enabled, mpi_backend_irecv_integer, &
      mpi_backend_irecv_real, mpi_backend_isend_integer, &
      mpi_backend_isend_real, mpi_backend_wait_all, swan_mpi_success
   implicit none(type, external)

   integer :: expected, ierr, request, status
   integer :: integer_buffer(2), integer_send(3), integer_receive(3)
   integer :: receive_counts(0:0), displacements(0:0), requests(2)
   integer :: gathered(0:0), reduced
   real :: real_buffer(2)
   character(len=8) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_mpi_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected MPI capability value'
   if (mpi_backend_enabled .neqv. (expected == 1)) &
      error stop 'unexpected MPI backend selected'

   if (.not.mpi_backend_enabled) then
      integer_buffer = [11, 12]
      call mpi_backend_irecv_integer(integer_buffer, 2, 101, 1, 2, &
         request, ierr)
      if (ierr /= swan_mpi_success .or. request /= 0) &
         error stop 'disabled integer receive returned an error or request'
      if (any(integer_buffer /= [11, 12])) &
         error stop 'disabled integer receive changed its buffer'

      call mpi_backend_isend_integer(integer_buffer, 2, 101, 1, 2, &
         request, ierr)
      if (ierr /= swan_mpi_success .or. request /= 0) &
         error stop 'disabled integer send returned an error or request'

      real_buffer = [1.25, -2.5]
      call mpi_backend_irecv_real(real_buffer, 2, 102, 1, 2, request, ierr)
      if (ierr /= swan_mpi_success .or. request /= 0) &
         error stop 'disabled real receive returned an error or request'
      if (any(transfer(real_buffer, [0, 0]) /= &
              transfer([1.25, -2.5], [0, 0]))) &
         error stop 'disabled real receive changed its buffer'

      call mpi_backend_isend_real(real_buffer, 2, 102, 1, 2, request, ierr)
      if (ierr /= swan_mpi_success .or. request /= 0) &
         error stop 'disabled real send returned an error or request'

      requests = [17, 18]
      call mpi_backend_wait_all(size(requests), requests, ierr)
      if (ierr /= swan_mpi_success .or. any(requests /= [17, 18])) &
         error stop 'disabled wait changed requests or returned an error'

      call mpi_backend_allreduce_integer(23, reduced, 101, 201, ierr)
      if (ierr /= swan_mpi_success .or. reduced /= 23) &
         error stop 'disabled allreduce lacks serial identity semantics'

      call mpi_backend_allgather_integer(29, gathered, 101, ierr)
      if (ierr /= swan_mpi_success .or. gathered(0) /= 29) &
         error stop 'disabled allgather lacks serial identity semantics'

      integer_send = [31, 37, 41]
      integer_receive = 0
      receive_counts(0) = size(integer_send)
      displacements(0) = 0
      call mpi_backend_allgatherv_integer(integer_send, size(integer_send), &
         integer_receive, receive_counts, displacements, 101, ierr)
      if (ierr /= swan_mpi_success .or. &
          any(integer_receive /= integer_send)) &
         error stop 'disabled allgatherv lacks serial identity semantics'
   end if
end program test_mpi_backend
