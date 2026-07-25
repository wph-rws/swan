module swan_metis_partition_backend
   implicit none(type, external)
   private

   logical, parameter, public :: metis_enabled = .false.
   public :: metis_collect_boundary_points, metis_copy_ownership
   public :: metis_decompose, metis_exchange_real, metis_release
   public :: metis_vertex_is_resident

contains

   subroutine metis_decompose(logcom)
      logical, intent(inout) :: logcom(7)

      if (metis_enabled) logcom(1) = logcom(1)
   end subroutine metis_decompose

   subroutine metis_collect_boundary_points()
   end subroutine metis_collect_boundary_points

   subroutine metis_exchange_real(field)
      real, intent(inout) :: field(:)

      if (metis_enabled) field = field
   end subroutine metis_exchange_real

   subroutine metis_copy_ownership(ownership)
      real, allocatable, intent(inout) :: ownership(:)

      if (metis_enabled .and. allocated(ownership)) ownership = ownership
   end subroutine metis_copy_ownership

   logical function metis_vertex_is_resident(vertex)
      integer, intent(in) :: vertex

      metis_vertex_is_resident = metis_enabled .and. vertex > 0
   end function metis_vertex_is_resident

   subroutine metis_release()
   end subroutine metis_release

end module swan_metis_partition_backend
