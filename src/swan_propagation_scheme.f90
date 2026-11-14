module swan_propagation_scheme
!
!     Which numerical scheme propagates action through geographic space.
!
!     The choice is made once, from the PROP command and the stationary or
!     nonstationary mode, and then read in the propagation routines. It sat in
!     SWCOMM4 between the test-output settings and the shape of the earth.
!
!     The scheme that is used at one grid point (upstream PROPSL) is not
!     here: near a boundary, dry land or an obstacle SWOMPU falls back to a
!     lower order for that point only. It is a local variable of SWOMPU,
!     which passes it to ACTION and QCSOURCE.
!
   implicit none(type, external)
   private

   public :: PROPSC, PROPSS, PROPSN, PROPFL
   public :: WAVAGE

!     PROPSC : scheme selected for spatial propagation
!              1=first order (BSBT), 2=SORDUP, 3=third order (S&L)
!     PROPSS : scheme used in stationary mode; 1=BSBT, 2=SORDUP
!     PROPSN : scheme used in nonstationary mode; 1=BSBT, 3=S&L
   integer :: PROPSC, PROPSS, PROPSN

!     PROPFL : whether flux limiting in spectral space is applied
   integer :: PROPFL

!     WAVAGE : wave-age parameter counteracting the garden-sprinkler effect
   real :: WAVAGE
end module swan_propagation_scheme
