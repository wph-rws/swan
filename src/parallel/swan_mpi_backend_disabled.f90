module swan_mpi_backend
   implicit none(type, external)
   private

   logical, parameter, public :: mpi_backend_enabled = .false.
   integer, parameter, public :: swan_mpi_error_count = 1
   integer, parameter, public :: swan_mpi_success = 0
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

      ierr = swan_mpi_success
   end subroutine mpi_backend_initialize

   subroutine mpi_backend_rank(rank, ierr)
      integer, intent(out) :: rank, ierr

      rank = 0
      ierr = swan_mpi_success
   end subroutine mpi_backend_rank

   subroutine mpi_backend_size(size, ierr)
      integer, intent(out) :: size, ierr

      size = 1
      ierr = swan_mpi_success
   end subroutine mpi_backend_size

   subroutine mpi_backend_communication_constants(character_type, &
      integer_type, real_type, maximum_operation, minimum_operation, &
      sum_operation)
      integer, intent(out) :: character_type, integer_type, real_type
      integer, intent(out) :: maximum_operation, minimum_operation
      integer, intent(out) :: sum_operation

      character_type = 0
      integer_type = 0
      real_type = 0
      maximum_operation = 0
      minimum_operation = 0
      sum_operation = 0
   end subroutine mpi_backend_communication_constants

   subroutine mpi_backend_initialized(initialized, ierr)
      logical, intent(out) :: initialized
      integer, intent(out) :: ierr

      initialized = .false.
      ierr = swan_mpi_success
   end subroutine mpi_backend_initialized

   subroutine mpi_backend_barrier(ierr)
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
   end subroutine mpi_backend_barrier

   subroutine mpi_backend_abort(error_code, ierr)
      integer, intent(in) :: error_code
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled) ierr = error_code
   end subroutine mpi_backend_abort

   subroutine mpi_backend_finalize(ierr)
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
   end subroutine mpi_backend_finalize

   subroutine mpi_backend_reduce_integer_scalar(value, count, datatype, &
      operation, ierr)
      integer, intent(inout) :: value
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled) value = count + datatype + operation
   end subroutine mpi_backend_reduce_integer_scalar

   subroutine mpi_backend_reduce_integer_array(values, count, datatype, &
      operation, ierr)
      integer, intent(inout) :: values(:)
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1) = count + datatype + operation
   end subroutine mpi_backend_reduce_integer_array

   subroutine mpi_backend_reduce_real_scalar(value, count, datatype, &
      operation, ierr)
      real, intent(inout) :: value
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled) value = real(count + datatype + operation)
   end subroutine mpi_backend_reduce_real_scalar

   subroutine mpi_backend_reduce_real_array(values, count, datatype, &
      operation, ierr)
      real, intent(inout) :: values(:)
      integer, intent(in) :: count, datatype, operation
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1) = real(count + datatype + operation)
   end subroutine mpi_backend_reduce_real_array

   subroutine mpi_backend_broadcast_integer_scalar(value, count, datatype, &
      root, ierr)
      integer, intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled) value = count + datatype + root
   end subroutine mpi_backend_broadcast_integer_scalar

   subroutine mpi_backend_broadcast_integer_array(values, count, datatype, &
      root, ierr)
      integer, intent(inout) :: values(*)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) &
         values(1) = datatype + root
   end subroutine mpi_backend_broadcast_integer_array

   subroutine mpi_backend_broadcast_integer_matrix(values, count, datatype, &
      root, ierr)
      integer, contiguous, intent(inout) :: values(:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1,1) = count + datatype + root
   end subroutine mpi_backend_broadcast_integer_matrix

   subroutine mpi_backend_broadcast_real_scalar(value, count, datatype, &
      root, ierr)
      real, intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled) value = real(count + datatype + root)
   end subroutine mpi_backend_broadcast_real_scalar

   subroutine mpi_backend_broadcast_real_array(values, count, datatype, &
      root, ierr)
      real, intent(inout) :: values(*)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) &
         values(1) = real(datatype + root)
   end subroutine mpi_backend_broadcast_real_array

   subroutine mpi_backend_broadcast_real_matrix(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1,1) = real(count + datatype + root)
   end subroutine mpi_backend_broadcast_real_matrix

   subroutine mpi_backend_broadcast_real_tensor3(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1,1,1) = real(count + datatype + root)
   end subroutine mpi_backend_broadcast_real_tensor3

   subroutine mpi_backend_broadcast_real_tensor4(values, count, datatype, &
      root, ierr)
      real, contiguous, intent(inout) :: values(:,:,:,:)
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1,1,1,1) = real(count + datatype + root)
   end subroutine mpi_backend_broadcast_real_tensor4

   subroutine mpi_backend_broadcast_character(value, count, datatype, root, &
      ierr)
      character(len=*), intent(inout) :: value
      integer, intent(in) :: count, datatype, root
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. len(value) > 0) &
         value(1:1) = achar(mod(count + datatype + root, 127))
   end subroutine mpi_backend_broadcast_character

   subroutine mpi_backend_send_real_vector(values, count, datatype, &
      destination, tag, ierr)
      real, contiguous, intent(in) :: values(:)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         ierr = int(values(1)) + count + datatype + destination + tag
   end subroutine mpi_backend_send_real_vector

   subroutine mpi_backend_send_real_matrix(values, count, datatype, &
      destination, tag, ierr)
      real, contiguous, intent(in) :: values(:,:)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         ierr = int(values(1,1)) + count + datatype + destination + tag
   end subroutine mpi_backend_send_real_matrix

   subroutine mpi_backend_receive_real_vector(values, count, datatype, &
      source, tag, ierr)
      real, contiguous, intent(out) :: values(:)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: ierr

      values = 0.0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1) = real(count + datatype + source + tag)
   end subroutine mpi_backend_receive_real_vector

   subroutine mpi_backend_receive_real_matrix(values, count, datatype, &
      source, tag, ierr)
      real, contiguous, intent(out) :: values(:,:)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: ierr

      values = 0.0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(values) > 0) &
         values(1,1) = real(count + datatype + source + tag)
   end subroutine mpi_backend_receive_real_matrix

   subroutine mpi_backend_gatherv_integer_i21(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      integer, intent(in) :: input(:)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      integer, intent(out) :: output(:,:)
      integer, intent(out) :: ierr

      output = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(output) > 0 .and. size(input) > 0) &
         output(1,1) = input(1) + input_count + receive_counts(0) + &
            displacements(0) + datatype + root
   end subroutine mpi_backend_gatherv_integer_i21

   subroutine mpi_backend_gatherv_integer_i12(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      integer, intent(in) :: input(:,:)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      integer, intent(out) :: output(:)
      integer, intent(out) :: ierr

      output = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. size(output) > 0 .and. size(input) > 0) &
         output(1) = input(1,1) + input_count + receive_counts(0) + &
            displacements(0) + datatype + root
   end subroutine mpi_backend_gatherv_integer_i12

   subroutine mpi_backend_gatherv_real(input, input_count, output, &
      receive_counts, displacements, datatype, root, ierr)
      real, intent(in) :: input(*)
      integer, intent(in) :: input_count, receive_counts(0:*), &
         displacements(0:*), datatype, root
      real, intent(out) :: output(*)
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. input_count > 0) &
         output(1) = input(1) + real(receive_counts(0) + displacements(0) + &
            datatype + root)
   end subroutine mpi_backend_gatherv_real

   subroutine mpi_backend_gather_counts(input_count, receive_counts, &
      datatype, root, ierr)
      integer, intent(in) :: input_count, datatype, root
      integer, intent(out) :: receive_counts(0:*)
      integer, intent(out) :: ierr

      receive_counts(0) = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled) &
         receive_counts(0) = input_count + datatype + root
   end subroutine mpi_backend_gather_counts

   subroutine mpi_backend_irecv_integer(buffer, count, datatype, source, &
      tag, request, ierr)
      integer, intent(inout) :: buffer(*)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: request, ierr

      request = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) &
         buffer(1) = datatype + source + tag
   end subroutine mpi_backend_irecv_integer

   subroutine mpi_backend_isend_integer(buffer, count, datatype, destination, &
      tag, request, ierr)
      integer, intent(in) :: buffer(*)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: request, ierr

      request = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) &
         request = buffer(1) + datatype + destination + tag
   end subroutine mpi_backend_isend_integer

   subroutine mpi_backend_irecv_real(buffer, count, datatype, source, tag, &
      request, ierr)
      real, intent(inout) :: buffer(*)
      integer, intent(in) :: count, datatype, source, tag
      integer, intent(out) :: request, ierr

      request = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) then
         buffer(1) = real(datatype + source + tag)
      end if
   end subroutine mpi_backend_irecv_real

   subroutine mpi_backend_isend_real(buffer, count, datatype, destination, &
      tag, request, ierr)
      real, intent(in) :: buffer(*)
      integer, intent(in) :: count, datatype, destination, tag
      integer, intent(out) :: request, ierr

      request = 0
      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) &
         request = int(buffer(1)) + datatype + destination + tag
   end subroutine mpi_backend_isend_real

   subroutine mpi_backend_wait_all(count, requests, ierr)
      integer, intent(in) :: count
      integer, intent(inout) :: requests(*)
      integer, intent(out) :: ierr

      ierr = swan_mpi_success
      if (mpi_backend_enabled .and. count > 0) requests(1) = requests(1)
   end subroutine mpi_backend_wait_all

   subroutine mpi_backend_allreduce_integer(send_value, receive_value, &
      datatype, operation, ierr)
      integer, intent(in) :: send_value
      integer, intent(out) :: receive_value
      integer, intent(in) :: datatype, operation
      integer, intent(out) :: ierr

      receive_value = send_value
      ierr = swan_mpi_success
      if (mpi_backend_enabled) receive_value = datatype + operation
   end subroutine mpi_backend_allreduce_integer

   subroutine mpi_backend_allgather_integer(send_value, receive_values, &
      datatype, ierr)
      integer, intent(in) :: send_value
      integer, intent(out) :: receive_values(0:*)
      integer, intent(in) :: datatype
      integer, intent(out) :: ierr

      receive_values(0) = send_value
      ierr = swan_mpi_success
      if (mpi_backend_enabled) receive_values(0) = datatype
   end subroutine mpi_backend_allgather_integer

   subroutine mpi_backend_allgatherv_integer(send_values, send_count, &
      receive_values, receive_counts, displacements, datatype, ierr)
      integer, intent(in) :: send_values(*)
      integer, intent(in) :: send_count, receive_counts(0:*), &
         displacements(0:*), datatype
      integer, intent(inout) :: receive_values(*)
      integer, intent(out) :: ierr

      integer :: copy_count, first

      ierr = swan_mpi_success
      copy_count = min(send_count, receive_counts(0))
      first = displacements(0) + 1
      if (copy_count > 0) &
         receive_values(first:first + copy_count - 1) = &
            send_values(1:copy_count)
      if (mpi_backend_enabled .and. datatype < 0) ierr = datatype
   end subroutine mpi_backend_allgatherv_integer

end module swan_mpi_backend
