module swan_mpi_lifecycle_backend
   use swan_mpi_backend, only: mpi_backend_abort, mpi_backend_barrier, &
      mpi_backend_finalize, mpi_backend_initialize, mpi_backend_rank, &
      mpi_backend_size
   implicit none(type, external)
   private

   logical, parameter, public :: coherent_coupling_enabled = .false.
   public :: lifecycle_abort, lifecycle_barrier, lifecycle_finalize
   public :: lifecycle_initialize, lifecycle_rank, lifecycle_size

contains

   subroutine lifecycle_initialize(ierr)
      integer, intent(inout) :: ierr

      call mpi_backend_initialize(ierr)
   end subroutine lifecycle_initialize

   subroutine lifecycle_rank(rank, ierr)
      integer, intent(inout) :: rank, ierr

      call mpi_backend_rank(rank, ierr)
   end subroutine lifecycle_rank

   subroutine lifecycle_size(process_count, ierr)
      integer, intent(inout) :: process_count, ierr

      call mpi_backend_size(process_count, ierr)
   end subroutine lifecycle_size

   subroutine lifecycle_barrier(ierr)
      integer, intent(inout) :: ierr

      call mpi_backend_barrier(ierr)
   end subroutine lifecycle_barrier

   subroutine lifecycle_abort(error_code, ierr)
      integer, intent(in) :: error_code
      integer, intent(inout) :: ierr

      call mpi_backend_abort(error_code, ierr)
   end subroutine lifecycle_abort

   subroutine lifecycle_finalize(ierr)
      integer, intent(inout) :: ierr

      call mpi_backend_finalize(ierr)
   end subroutine lifecycle_finalize
end module swan_mpi_lifecycle_backend
