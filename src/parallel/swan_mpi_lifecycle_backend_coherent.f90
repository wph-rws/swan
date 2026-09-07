module swan_mpi_lifecycle_backend
   implicit none(type, external)
   private

   logical, parameter, public :: coherent_coupling_enabled = .true.
   public :: lifecycle_abort, lifecycle_barrier, lifecycle_finalize
   public :: lifecycle_initialize, lifecycle_rank, lifecycle_size

contains

   subroutine lifecycle_initialize(ierr)
      integer, intent(inout) :: ierr

      ierr = ierr
   end subroutine lifecycle_initialize

   subroutine lifecycle_rank(rank, ierr)
      integer, intent(inout) :: rank, ierr

      rank = rank
      ierr = ierr
   end subroutine lifecycle_rank

   subroutine lifecycle_size(process_count, ierr)
      integer, intent(inout) :: process_count, ierr

      process_count = process_count
      ierr = ierr
   end subroutine lifecycle_size

   subroutine lifecycle_barrier(ierr)
      integer, intent(inout) :: ierr

      ierr = ierr
   end subroutine lifecycle_barrier

   subroutine lifecycle_abort(error_code, ierr)
      integer, intent(in) :: error_code
      integer, intent(inout) :: ierr

      ierr = ierr + 0*error_code
   end subroutine lifecycle_abort

   subroutine lifecycle_finalize(ierr)
      integer, intent(inout) :: ierr

      ierr = ierr
   end subroutine lifecycle_finalize
end module swan_mpi_lifecycle_backend
