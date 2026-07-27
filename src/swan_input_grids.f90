module swan_input_grids
!
!     The eighteen grids SWAN reads its input fields on.
!
!     Depth, current, friction, wind, water level, coordinates, air-sea
!     temperature difference, plants, turbulence, mud, ice and the sea-swell
!     parameters each arrive on their own grid, which may differ from the
!     computational grid in origin, orientation, mesh and staggering. Every
!     array here is indexed by the grid number in the list below, so they
!     describe one table with eighteen rows.
!
!     The VAR* flags belong to the same table: each says whether its field
!     actually varies over space, which is what decides whether the grid is
!     consulted at all. COSVC/SINVC and COSWC/SINWC are the current and wind
!     grid's rotation precomputed, so they are derived rows of the same table.
!
!     A later step can make this an array of one derived type; the arrays are
!     already parallel and the indices already agree. That is a large diff
!     against upstream, so it waits for a reason beyond tidiness.
!
   implicit none(type, external)
   private

   public :: NUMGRD
   public :: IGTYPE, LEDS, MXG, MYG
   public :: ALPG, COSPG, SINPG, DXG, DYG, XPG, YPG
   public :: STAGX, STAGY, EXCFLD
   public :: COSVC, SINVC, COSWC, SINWC
   public :: VARFR, VARWI, VARWLV, VARAST, VARNPL, VARTUR, VARMUD
   public :: VARAICE, VARHICE, VARHSS, VARTSS, VARDSS

!     NUMGRD : number of input grids
!
!      1 depth                      10 air-sea temperature difference
!      2 current velocity, x         11 number of plants per square meter
!      3 current velocity, y         12 turbulent viscosity
!      4 friction coefficient        13 fluid mud layer
!      5 wind velocity, x            14 ice concentration (fraction)
!      6 wind velocity, y            15 ice thickness
!      7 water level                 16 sea-swell significant wave height
!      8 y-coordinate                17 sea-swell mean wave period
!      9 x-coordinate                18 sea-swell mean wave direction
   integer, parameter :: NUMGRD = 18

!     IGTYPE : 0=constant values, 1=regular, 2=curvilinear
!     LEDS   : 0=values not read yet, 1=values were read
   integer :: IGTYPE(NUMGRD), LEDS(NUMGRD)

!     MXG, MYG : number of meshes in each direction
   integer :: MXG(NUMGRD), MYG(NUMGRD)

!     ALPG         : direction of the grid's x-axis in user coordinates
!     COSPG, SINPG : cosine and sine of ALPG
   real :: ALPG(NUMGRD), COSPG(NUMGRD), SINPG(NUMGRD)

!     DXG, DYG : mesh size in each direction
!     XPG, YPG : origin
   real :: DXG(NUMGRD), DYG(NUMGRD)
   real :: XPG(NUMGRD), YPG(NUMGRD)

!     STAGX, STAGY : staggering of a curvilinear input grid with respect to
!                    the computational grid
   real :: STAGX(NUMGRD), STAGY(NUMGRD)

!     EXCFLD : exception value marking a missing value in the input field
   real :: EXCFLD(NUMGRD)

!     COSVC, SINVC : cosine and sine of -ALPG(2), the current grid's rotation
!     COSWC, SINWC : cosine and sine of -ALPG(5), the wind grid's rotation
   real :: COSVC, SINVC, COSWC, SINWC

!     Whether each field varies over space at all. When it does not, the field
!     is a single value and its input grid is never consulted.
   logical :: VARFR, VARWI, VARWLV, VARAST
   logical :: VARNPL, VARTUR, VARMUD
   logical :: VARAICE, VARHICE
   logical :: VARHSS, VARTSS, VARDSS
end module swan_input_grids
