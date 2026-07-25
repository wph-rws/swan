module swan_block_coloring_backend
   implicit none(type, external)
   private

   logical, parameter, public :: block_coloring_enabled = .false.
   public :: color_swan_subdomains

contains

   subroutine color_swan_subdomains(multi_coloring, group_points)
      logical, intent(inout) :: multi_coloring
      integer, intent(in) :: group_points(:,:)

      if (block_coloring_enabled .and. size(group_points) > 0) &
         multi_coloring = .not.multi_coloring
   end subroutine color_swan_subdomains

end module swan_block_coloring_backend
