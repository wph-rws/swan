module swan_stencil
!
!     The computational stencil: which grid points the thread that is running
!     is currently working on, and the few per-thread quantities that go with
!     it.
!
!     Everything here is `THREADPRIVATE`. Each solver thread walks its own part
!     of the grid, so each needs its own copy of where it is. That is what made
!     the rest of SWCOMM3 hard to reason about: fifty-odd files imported a
!     module in which most symbols were run configuration and a handful were
!     per-thread state, with nothing in the source saying which was which.
!
!     This module is the last of SWCOMM3. It is also the obstacle to making the
!     physics kernels `pure`: the remaining consumers still reference one of
!     these symbols, and a pure procedure may read module state but the intent
!     here is to pass the stencil as an argument instead. The thread-state
!     manifest records the proposed owner for each symbol.
!
   implicit none(type, external)
   private

   public :: MICMAX
   public :: IXCGRD, IYCGRD, KCGRD, COSLAT, ICMAX
   public :: ILMAX, RDFSIN

!     MICMAX : the largest stencil SWAN uses, and so the extent of the arrays
!              below. ICMAX says how many of those points are in use.
   integer, parameter :: MICMAX = 13

!     IXCGRD, IYCGRD : the x and y index of each point of the stencil
!     KCGRD          : the grid address of each point; KCGRD(1) is the point
!                      being computed and the rest are its upwind neighbours
!     COSLAT         : cosine of the latitude at each stencil point; 1 in
!                      Cartesian coordinates
   integer :: IXCGRD(MICMAX), IYCGRD(MICMAX), KCGRD(MICMAX)
   real :: COSLAT(MICMAX)

!     RDFSIN : reduction factor per frequency for the wind input, handed to
!              LFACTOR in SdsBabanin
   real :: RDFSIN(100)
!$OMP THREADPRIVATE(IXCGRD,IYCGRD,KCGRD,COSLAT)
!$OMP THREADPRIVATE(RDFSIN)

!     ICMAX  : number of points actually in the stencil, 3 or more
   integer :: ICMAX
!$OMP THREADPRIVATE(ICMAX)

!     ILMAX : maximum number of layers used in the vegetation model. Not
!             thread state; it stayed here because it was declared between two
!             stencil arrays and nothing else in SWCOMM3 claimed it.
   integer :: ILMAX
end module swan_stencil
