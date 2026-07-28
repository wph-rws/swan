module swan_global_grid
!
!     The computational grid as it was before the domain was split.
!
!     Under MPI each process holds a piece of the grid, but output has to be
!     written for the whole of it and boundary conditions are given on the whole
!     of it. These nine describe that undivided grid: its size, its addressing
!     and its coordinates.
!
!     They are the counterpart of swan_computational_grid, which describes what
!     the calling process actually holds. In a serial run the two agree.
!
!     They were in M_PARALL, which is really the MPI service layer: the halo
!     widths, the communicator handles and the reduction wrappers. Eleven files
!     wanted this grid description and got the message passing with it.
!     M_PARALL's own procedures do not touch any of these.
!
   implicit none(type, external)
   private

   public :: MXCGL, MYCGL, MCGRDGL, NGRBGL, NBGGL
   public :: KGRPGL, KGRBGL, XGRDGL, YGRDGL

!     MXCGL, MYCGL : size of the global grid in each direction
!     MCGRDGL      : number of wet points in the global grid
!     NGRBGL       : number of points on the global grid boundary
!     NBGGL        : number of boundary grid points in the global grid
   integer :: MXCGL, MYCGL, MCGRDGL
   integer :: NGRBGL, NBGGL

!     KGRPGL : indirect addresses of the global grid points
!     KGRBGL : the global boundary points
   integer, save, allocatable :: KGRPGL(:,:), KGRBGL(:)

!     XGRDGL, YGRDGL : coordinates of the global grid points
   real, save, allocatable :: XGRDGL(:,:), YGRDGL(:,:)
end module swan_global_grid
