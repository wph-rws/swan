module swan_block_coloring_backend
   use swan_parallel, only: SWBLKCOL
   implicit none(type, external)
   private

   logical, parameter, public :: block_coloring_enabled = .true.
   public :: color_swan_subdomains

contains

   subroutine color_swan_subdomains(multi_coloring, group_points)
      logical, intent(inout) :: multi_coloring
      integer, intent(in) :: group_points(:,:)

      call SWBLKCOL(multi_coloring, group_points)
   end subroutine color_swan_subdomains

end module swan_block_coloring_backend
