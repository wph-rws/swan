module swan_stencil
!
!     The size of the computational stencil, and the reduction factor of
!     the ST6 wind input as it is kept between two computations.
!
!     The stencil itself (which grid points the running thread is working
!     on) is not in this module. Every solver thread keeps it in variables
!     of its own (st_ix, st_iy, st_kc, st_co and st_nm in SWOMPU and
!     SwanCompUnstruc) and passes each routine what it needs as arguments.
!     Upstream keeps the stencil in SWCOMM3 (IXCGRD, IYCGRD, KCGRD, COSLAT
!     and ICMAX), where every routine reads it without it being visible in
!     the call. No module variable of the fork is thread-private.
!
   implicit none(type, external)
   private

   public :: MICMAX
   public :: RDFSIN_MASTER

!     MICMAX : the largest stencil SWAN uses, and so the extent of the
!              stencil arrays in the solvers.
   integer, parameter :: MICMAX = 13

!     RDFSIN_MASTER : reduction factor per frequency for the ST6 wind input
!                     (upstream RDFSIN), as the part of the program that
!                     runs on one thread holds it. SWIND_DBYB computes the
!                     factor for a grid point and SURF_ROUGH_ECMWF reads it
!                     at the next one, so it lives on from point to point.
!                     In the solvers every thread starts with this value in
!                     an array of its own, which the routines get as the
!                     argument RDFSIN; when the solver is done, the first
!                     thread puts its last value back here. This is how
!                     upstream behaves, where RDFSIN is one array per
!                     thread for the whole program.
   real :: RDFSIN_MASTER(100)
end module swan_stencil
