module swan_spectral_grid
!
!     The discretisation of the spectrum: how many frequencies and directions
!     the action density is resolved on, and how they are spaced.
!
!     MSC and MDC are the two most-used symbols in the whole of SWCOMM3 -- 38
!     and 35 of its 56 importers -- because every routine that touches a
!     spectrum needs its shape. They came with the grid geometry, the physics
!     settings and the source-term coefficients attached.
!
!     Fixed by the CGRID command before the computation starts and read-only
!     from then on.
!
   implicit none(type, external)
   private

   public :: MSC, MDC, MTC
   public :: DDIR, FRINTF, FRINTH
   public :: SPDIR1, SPDIR2, FULCIR, SLOW, SHIG
   public :: MSC4MI, MSC4MA, MDC4MI, MDC4MA

!     MSC : number of frequencies
!     MDC : number of directions
!     MTC : number of time steps in a nonstationary run
   integer :: MSC, MDC, MTC

!     DDIR   : mesh size in theta-direction, (SPDIR2-SPDIR1)/MDC
!     FRINTF : frequency integration factor, ln(sigma(i+1)/sigma(i))
!     FRINTH : sqrt of the frequency ratio between neighbouring bins
   real :: DDIR, FRINTF, FRINTH

!     SPDIR1, SPDIR2 : first and last direction of the spectral grid
!     FULCIR : whether the directional grid covers the full circle
   real :: SPDIR1, SPDIR2
   logical :: FULCIR

!     SLOW, SHIG : lowest and highest sigma of the spectral grid, 2*PI times
!                  the lowest and highest frequency. FRINTF is derived from
!                  their ratio, so the three belong together.
   real :: SLOW, SHIG

!     Bounds of the extended frequency and direction range the quadruplet
!     routines work on: the interactions reach outside the computed grid, so
!     their arrays run from MSC4MI to MSC4MA rather than from 1 to MSC.
   integer :: MSC4MI, MSC4MA, MDC4MI, MDC4MA
end module swan_spectral_grid
