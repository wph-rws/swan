module swan_boundary_counters
!
!     How much boundary data the run has.
!
!     Three counters filled while the BOUND commands are read and afterwards
!     used to size and drive the boundary loops. They are the counterpart of
!     the specifications in M_BNDSPEC, which is where they will end up once
!     that module gets a type; until then they at least no longer travel with
!     the input grids.
!
   implicit none(type, external)
   private

   public :: NBFILS, NBSPEC, NBGRPT

!     NBFILS : number of boundary condition files
!     NBSPEC : number of boundary spectra
!     NBGRPT : number of computational grid points a boundary condition holds
!              for
   integer :: NBFILS, NBSPEC, NBGRPT
end module swan_boundary_counters
