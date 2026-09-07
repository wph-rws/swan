module swan_stencil
!
!     The computational stencil: which grid points the thread that is running
!     is currently working on, and the few per-thread quantities that go with
!     it.
!
!     Only RDFSIN is still `THREADPRIVATE` (seeded by COPYIN): each solver
!     thread scales the wind input with its own copy. The stencil itself
!     lives in thread-local scratch inside the solvers, which pass every
!     kernel its context explicitly. That is what made the rest of SWCOMM3
!     hard to reason about: fifty-odd files imported a module in which most
!     symbols were run configuration and a handful were per-thread state,
!     with nothing in the source saying which was which.
!
!     The stencil arrays below are the last of SWCOMM3. Solver kernels take
!     them as arguments instead, so no kernel depends on hidden per-thread
!     module state for them anymore. The thread-state manifest records the
!     remaining threadprivate inventory.
!
   implicit none(type, external)
   private

   public :: MICMAX
   public :: IXCGRD, IYCGRD, KCGRD, COSLAT, ICMAX
   public :: RDFSIN

!     MICMAX : the largest stencil SWAN uses, and so the extent of the arrays
!              below. ICMAX says how many of those points are in use.
   integer, parameter :: MICMAX = 13

!     IXCGRD, IYCGRD : the x and y index of each point of the stencil
!     KCGRD          : the grid address of each point; KCGRD(1) is the point
!                      being computed and the rest are its upwind neighbours
!     COSLAT         : cosine of the latitude at each stencil point; 1 in
!                      Cartesian coordinates
!     These five are ordinary shared module data now. The solvers keep the
!     per-thread point in thread-local scratch (SWOMPU/SwanCompUnstruc) and
!     pass every kernel its context explicitly, so no threadprivate copy or
!     COPYIN seeding remains for them.
   integer :: IXCGRD(MICMAX), IYCGRD(MICMAX), KCGRD(MICMAX)
   real :: COSLAT(MICMAX)

!     RDFSIN : reduction factor per frequency for the wind input, handed to
!              LFACTOR in SdsBabanin
   real :: RDFSIN(100)
!$OMP THREADPRIVATE(RDFSIN)
!  Only RDFSIN remains threadprivate: every solver thread scales the wind
!  input with its own copy, seeded by COPYIN at region entry.

!     ICMAX  : number of points actually in the stencil, 3 or more. Shared;
!              the per-thread width travels with the explicit stencil context.
   integer :: ICMAX
end module swan_stencil
