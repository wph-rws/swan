module swan_mpi_backend
   use mpi
   implicit none(type, external)
   private

   logical, parameter, public :: mpi_backend_enabled = .true.
   integer, parameter, public :: swan_mpi_error_count = MPI_ERR_COUNT
   integer, parameter, public :: swan_mpi_success = MPI_SUCCESS
   public :: mpi_backend_broadcast_character
   public :: mpi_backend_broadcast_integer_array
   public :: mpi_backend_broadcast_integer_matrix
   public :: mpi_backend_broadcast_integer_scalar
   public :: mpi_backend_broadcast_real_array
   public :: mpi_backend_broadcast_real_matrix
   public :: mpi_backend_broadcast_real_scalar
   public :: mpi_backend_broadcast_real_tensor3
   public :: mpi_backend_broadcast_real_tensor4
   public :: mpi_backend_gather_counts
   public :: mpi_backend_gatherv_integer_i12
   public :: mpi_backend_gatherv_integer_i21
   public :: mpi_backend_gatherv_real
   public :: mpi_backend_receive_real_matrix, mpi_backend_receive_real_vector
   public :: mpi_backend_reduce_integer_array
   public :: mpi_backend_reduce_integer_scalar
   public :: mpi_backend_reduce_real_array, mpi_backend_reduce_real_scalar
   public :: mpi_backend_send_real_matrix, mpi_backend_send_real_vector
   public :: mpi_backend_abort, mpi_backend_barrier
   public :: mpi_backend_communication_constants
   public :: mpi_backend_finalize, mpi_backend_initialize
   public :: mpi_backend_initialized, mpi_backend_rank, mpi_backend_size
   public :: mpi_backend_allgather_integer
   public :: mpi_backend_allgatherv_integer
   public :: mpi_backend_allreduce_integer
   public :: mpi_backend_irecv_integer, mpi_backend_irecv_real
   public :: mpi_backend_isend_integer, mpi_backend_isend_real
   public :: mpi_backend_wait_all

contains

   subroutine mpi_backend_initialize(ierr)
      integer, intent(out) :: ierr

      call MPI_INIT(ierr)
   end subroutine mpi_backend_initialize

   subroutine mpi_backend_rank(rank, ierr)
      integer, intent(out) :: rank, ierr

      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
   end subroutine mpi_backend_rank

   subroutine mpi_backend_size(size, ierr)
      integer, intent(out) :: size, ierr

      call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
   end subroutine mpi_backend_size

   subroutine mpi_backend_communication_constants(character_type, &
      integer_type, real_type, maximum_operation, minimum_operation, &
      sum_operation)
      integer, intent(out) :: character_type, integer_type, real_type
      integer, intent(out) :: maximum_operation, minimum_operation
      integer, intent(out) :: sum_operation

      character_type = MPI_CHARACTER
      integer_type = MPI_INTEGER
      real_type = MPI_REAL
      maximum_operation = MPI_MAX
      minimum_operation = MPI_MIN
      sum_operation = MPI_SUM
   end subroutine mpi_backend_communication_constants

   subroutine mpi_backend_initialized(initialized, ierr)
      logical, intent(out) :: initialized
      integer, intent(out) :: ierr

      call MPI_INITIALIZED(initialized, ierr)
   end subroutine mpi_backend_initialized

   subroutine mpi_backend_barrier(ierr)
      integer, intent(out) :: ierr

      call MPI_BARRIER(MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_barrier

   subroutine mpi_backend_abort(error_code, ierr)
      integer, intent(in) :: error_code
      integer, intent(out) :: ierr

      call MPI_ABORT(MPI_COMM_WORLD, error_code, ierr)
   end subroutine mpi_backend_abort

   subroutine mpi_backend_finalize(ierr)
      integer, intent(out) :: ierr

      call MPI_FINALIZE(ierr)
   end subroutine mpi_backend_finalize

   subroutine mpi_backend_reduce_integer_scalar(value, count, datatype, &
      operation, ierr)
      integer, intent(inout) :: value
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      call MPI_ALLREDUCE(MPI_IN_PLACE, value, count, datatype, operation, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_reduce_integer_scalar

   subroutine mpi_backend_reduce_integer_array(values, count, datatype, &
      operation, ierr)
      integer, intent(inout) :: values(:)
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      call MPI_ALLREDUCE(MPI_IN_PLACE, values, count, datatype, operation, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_reduce_integer_array

   subroutine mpi_backend_reduce_real_scalar(value, count, datatype, &
      operation, ierr)
      real, intent(inout) :: value
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      call MPI_ALLREDUCE(MPI_IN_PLACE, value, count, datatype, operation, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_reduce_real_scalar

   subroutine mpi_backend_reduce_real_array(values, count, datatype, &
      operation, ierr)
      real, intent(inout) :: values(:)
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      call MPI_ALLREDUCE(MPI_IN_PLACE, values, count, datatype, operation, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_reduce_real_array

   subroutine mpi_backend_broadcast_integer_scalar(value, count, datatype, &
      root, ierr)
      integer, intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(value, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_integer_scalar

   subroutine mpi_backend_broadcast_integer_array(values, count, datatype, &
      root, ierr)
      integer, intent(inout) :: values(*)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_integer_array

   subroutine mpi_backend_broadcast_integer_matrix(values, count, datatype, &
      root, ierr)
      integer, contiguous, intent(inout) :: values(:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_integer_matrix

   subroutine mpi_backend_broadcast_real_scalar(value, count, datatype, &
      root, ierr)
      real, intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(value, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_real_scalar

   subroutine mpi_backend_broadcast_real_array(values, count, datatype, &
      root, ierr)
      real, intent(inout) :: values(*)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_real_array

   subroutine mpi_backend_broadcast_real_matrix(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_real_matrix

   subroutine mpi_backend_broadcast_real_tensor3(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_real_tensor3

   subroutine mpi_backend_broadcast_real_tensor4(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:,:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(values, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_real_tensor4

   subroutine mpi_backend_broadcast_character(value, count, datatype, root, &
      ierr)
      character(len=*), intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      call MPI_BCAST(value, count, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_broadcast_character

   subroutine mpi_backend_send_real_vector(values, count, datatype, &
      destination, tag, ierr)
      real, contiguous, intent(in) :: values(:)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: ierr

      call MPI_SEND(values, count, datatype, destination, tag, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_send_real_vector

   subroutine mpi_backend_send_real_matrix(values, count, datatype, &
      destination, tag, ierr)
      real, contiguous, intent(in) :: values(:,:)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: ierr

      call MPI_SEND(values, count, datatype, destination, tag, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_send_real_matrix

   subroutine mpi_backend_receive_real_vector(values, count, datatype, &
      source, tag, ierr)
      real, contiguous, intent(out) :: values(:)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: ierr
      integer :: status(MPI_STATUS_SIZE)

      call MPI_RECV(values, count, datatype, source, tag, MPI_COMM_WORLD, &
         status, ierr)
   end subroutine mpi_backend_receive_real_vector

   subroutine mpi_backend_receive_real_matrix(values, count, datatype, &
      source, tag, ierr)
      real, contiguous, intent(out) :: values(:,:)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: ierr
      integer :: status(MPI_STATUS_SIZE)

      call MPI_RECV(values, count, datatype, source, tag, MPI_COMM_WORLD, &
         status, ierr)
   end subroutine mpi_backend_receive_real_matrix

   subroutine mpi_backend_gatherv_integer_i21(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      integer, intent(in) :: input(:)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      integer, intent(out) :: output(:,:)
      integer, intent(out) :: ierr

      call MPI_GATHERV(input, input_count, datatype, output, receive_counts, &
         displacements, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_gatherv_integer_i21

   subroutine mpi_backend_gatherv_integer_i12(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      integer, intent(in) :: input(:,:)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      integer, intent(out) :: output(:)
      integer, intent(out) :: ierr

      call MPI_GATHERV(input, input_count, datatype, output, receive_counts, &
         displacements, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_gatherv_integer_i12

   subroutine mpi_backend_gatherv_real(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      real, intent(in) :: input(*)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      real, intent(out) :: output(*)
      integer, intent(out) :: ierr

      call MPI_GATHERV(input, input_count, datatype, output, receive_counts, &
         displacements, datatype, root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_gatherv_real

   subroutine mpi_backend_gather_counts(input_count, receive_counts, &
      datatype, root, ierr)
      integer, intent(in) :: input_count, datatype, root
      integer, intent(out) :: receive_counts(0:*)
      integer, intent(out) :: ierr

      call MPI_GATHER(input_count, 1, datatype, receive_counts, 1, datatype, &
         root, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_gather_counts

   subroutine mpi_backend_irecv_integer(buffer, count, datatype, source, &
      tag, request, ierr)
      integer, intent(inout) :: buffer(*)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: request, ierr

      call MPI_IRECV(buffer, count, datatype, source, tag, MPI_COMM_WORLD, &
         request, ierr)
   end subroutine mpi_backend_irecv_integer

   subroutine mpi_backend_isend_integer(buffer, count, datatype, destination, &
      tag, request, ierr)
      integer, intent(in) :: buffer(*)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: request, ierr

      call MPI_ISEND(buffer, count, datatype, destination, tag, &
         MPI_COMM_WORLD, request, ierr)
   end subroutine mpi_backend_isend_integer

   subroutine mpi_backend_irecv_real(buffer, count, datatype, source, tag, &
      request, ierr)
      real, intent(inout) :: buffer(*)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: request, ierr

      call MPI_IRECV(buffer, count, datatype, source, tag, MPI_COMM_WORLD, &
         request, ierr)
   end subroutine mpi_backend_irecv_real

   subroutine mpi_backend_isend_real(buffer, count, datatype, destination, &
      tag, request, ierr)
      real, intent(in) :: buffer(*)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: request, ierr

      call MPI_ISEND(buffer, count, datatype, destination, tag, &
         MPI_COMM_WORLD, request, ierr)
   end subroutine mpi_backend_isend_real

   subroutine mpi_backend_wait_all(count, requests, ierr)
      integer, intent(in) :: count
      integer, intent(inout) :: requests(*)
      integer, intent(out) :: ierr

      call MPI_WAITALL(count, requests, MPI_STATUSES_IGNORE, ierr)
   end subroutine mpi_backend_wait_all

   subroutine mpi_backend_allreduce_integer(send_value, receive_value, &
      datatype, operation, ierr)
      integer, intent(in) :: send_value
      integer, intent(out) :: receive_value
      integer, intent(in) :: datatype, operation
      integer, intent(out) :: ierr

      call MPI_ALLREDUCE(send_value, receive_value, 1, datatype, operation, &
         MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_allreduce_integer

   subroutine mpi_backend_allgather_integer(send_value, receive_values, &
      datatype, ierr)
      integer, intent(in) :: send_value
      integer, intent(out) :: receive_values(0:*)
      integer, intent(in) :: datatype
      integer, intent(out) :: ierr

      call MPI_ALLGATHER(send_value, 1, datatype, receive_values, 1, &
         datatype, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_allgather_integer

   subroutine mpi_backend_allgatherv_integer(send_values, send_count, &
      receive_values, receive_counts, displacements, datatype, ierr)
      integer, intent(in) :: send_values(*)
      integer, intent(in) :: send_count, receive_counts(0:*), &
         displacements(0:*), datatype
      integer, intent(inout) :: receive_values(*)
      integer, intent(out) :: ierr

      call MPI_ALLGATHERV(send_values, send_count, datatype, receive_values, &
         receive_counts, displacements, datatype, MPI_COMM_WORLD, ierr)
   end subroutine mpi_backend_allgatherv_integer

end module swan_mpi_backend
