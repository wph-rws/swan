module swan_computational_grid
!
!     The geographic grid the computation runs on: its size, its origin, its
!     orientation and the extent it covers.
!
!     MXC, MYC and MCGRD are wanted by twenty-one importers each -- anything
!     that walks the grid or dimensions an array over it. In SWCOMM3 that meant
!     importing the spectral discretisation and the physics with them.
!
!     Established by the CGRID command and, for an unstructured run, by the
!     mesh reader. ALOCMP is the one mutable member: it records that COMPDA has
!     to be reallocated because the layout changed after a command was parsed.
!
   implicit none(type, external)
   private

   public :: MXC, MYC, MCGRD, NX, NY, NGRBND
   public :: XPC, YPC, XCLEN, YCLEN, DX, DY
   public :: ALPC, COSPC, SINPC
   public :: XCGMIN, XCGMAX, YCGMIN, YCGMAX, XCP, YCP
   public :: ONED, ALOCMP

!     MXC, MYC : number of grid points in each direction
!     MCGRD    : number of wet grid points, the extent of the COMPDA rows
!     NX, NY   : number of meshes in each direction
!     NGRBND   : number of grid points on the boundary
   integer :: MXC, MYC, MCGRD
   integer :: NX, NY, NGRBND

!     XPC, YPC     : origin of the computational grid in user coordinates
!     XCLEN, YCLEN : length of the grid in each direction
!     DX, DY       : mesh size, XCLEN/NX and YCLEN/NY
   real :: XPC, YPC
   real :: XCLEN, YCLEN
   real :: DX, DY

!     ALPC         : direction of the grid's x-axis in user coordinates
!     COSPC, SINPC : cosine and sine of ALPC
   real :: ALPC, COSPC, SINPC

!     Bounding box of the grid, used to reject output points that fall outside
!     it, and XCP/YCP as the point currently being located.
   real :: XCGMIN, XCGMAX, YCGMIN, YCGMAX
   real :: XCP, YCP

!     ONED : whether the run is one-dimensional
   logical :: ONED

!     ALOCMP : COMPDA must be reallocated because its layout changed
   logical :: ALOCMP = .FALSE.
end module swan_computational_grid
