module swan_input_fields
!
!     The input fields as they stand on the computational grid.
!
!     Each of these is read from a file or set to a constant, interpolated onto
!     the computational grid, and then read by the source terms. They are the
!     values behind the eighteen input grids that swan_input_grids describes:
!     that module says where a field is defined, this one holds it.
!
!     Sixteen arrays used by two or three files each. They were in M_GENARR
!     together with AC2, SPCSIG and KGRPNT, which almost every importer needs,
!     so everyone carried the ice thickness and the mud layer along.
!
   implicit none(type, external)
   private

   public :: DEPTH, WLEVL, FRIC
   public :: UXB, UYB, WXI, WYI
   public :: ASTDF, MUDLF, NPLAF, TURBF
   public :: AICEF, HICEF
   public :: HSSF, TSSF, DSSF

!     DEPTH : depth
!     WLEVL : water level
!     FRIC  : bottom friction coefficient
   real, save, allocatable :: DEPTH(:), WLEVL(:), FRIC(:)

!     UXB, UYB : current velocity, contravariant components
!     WXI, WYI : wind velocity, contravariant components
   real, save, allocatable :: UXB(:), UYB(:)
   real, save, allocatable :: WXI(:), WYI(:)

!     ASTDF : air-sea temperature difference
!     MUDLF : thickness of the fluid mud layer
!     NPLAF : number of plants per square meter
!     TURBF : turbulent viscosity
   real, save, allocatable :: ASTDF(:), MUDLF(:)
   real, save, allocatable :: NPLAF(:), TURBF(:)

!     AICEF : ice concentration as a fraction
!     HICEF : ice thickness in meters
   real, save, allocatable :: AICEF(:), HICEF(:)

!     The sea-swell parameters, which arrive as fields rather than being
!     computed: significant height, mean period and mean direction.
   real, save, allocatable :: HSSF(:), TSSF(:), DSSF(:)
end module swan_input_fields
