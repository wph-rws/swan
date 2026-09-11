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
!     The former shared stencil arrays were the last of SWCOMM3; they are
!     removed now that solver kernels take their context as arguments, so no
!     kernel depends on hidden per-thread module state for them anymore.
!     The thread-state manifest records the remaining threadprivate inventory.
!
   implicit none(type, external)
   private

   public :: MICMAX
   public :: RDFSIN

!     MICMAX : the largest stencil SWAN uses, and so the extent of the
!              thread-local scratch arrays in the solvers.
   integer, parameter :: MICMAX = 13

!     The former shared stencil arrays (IXCGRD, IYCGRD, KCGRD, COSLAT, ICMAX)
!     are removed: no kernel reads them implicitly anymore, and shared module
!     data picked up inside a parallel region would silently race where the
!     old code was threadprivate-safe.

!     RDFSIN : reduction factor per frequency for the wind input, handed to
!              LFACTOR in SdsBabanin
   real :: RDFSIN(100)
!$OMP THREADPRIVATE(RDFSIN)
!  Only RDFSIN remains threadprivate: every solver thread scales the wind
!  input with its own copy, seeded by COPYIN at region entry.
end module swan_stencil
