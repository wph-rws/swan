module swan_spherical_geometry
!
!     Whether the computation is on a sphere, and how big that sphere is.
!
!     Set by the COORD command and then read wherever a distance, a gradient or
!     a propagation velocity has to be converted between degrees and metres.
!     Eighteen files need KSPHER; they had to import SWCOMM4 with the test
!     output and the propagation scheme to get it.
!
   implicit none(type, external)
   private

   public :: KSPHER, KREPTX, PROJ_METHOD
   public :: REARTH, LENDEG

!     KSPHER : 0=Cartesian coordinates, >0=spherical coordinates
!     KREPTX : if >0 the domain repeats itself in x-direction, which is what
!              lets propagation run around the globe
   integer :: KSPHER, KREPTX

!     PROJ_METHOD : 0=(quasi-)Cartesian, 1=uniform Mercator (spherical only)
   integer :: PROJ_METHOD

!     REARTH : radius of the earth
!     LENDEG : length of one degree of the sphere, REARTH*PI/180
   real :: REARTH, LENDEG
end module swan_spherical_geometry
