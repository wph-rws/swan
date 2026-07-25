module swan_computational_grid_kind
!
!     What kind of computational grid the run is on.
!
!     OPTG is the switch nearly every grid-dependent routine dispatches on, and
!     sixteen files wanted it from SWCOMM2. CVLEFT and CCURV qualify the
!     curvilinear case, so they belong with it rather than with the input grids
!     that SWCOMM2 was mostly about.
!
!     Set by the CGRID command and read everywhere after that.
!
   implicit none(type, external)
   private

   public :: OPTG, CVLEFT, CCURV

!     OPTG : 1=regular, 2=irregular but rectangular (unused), 3=curvilinear,
!            5=unstructured
   integer :: OPTG

!     CVLEFT : whether the curvilinear computational grid is left-oriented
!     CCURV  : whether the curvilinear grid needs the correction
   logical :: CVLEFT, CCURV
end module swan_computational_grid_kind
