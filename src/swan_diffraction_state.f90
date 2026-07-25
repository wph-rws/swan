module swan_diffraction_state
!
!     The diffraction parameter and its two spatial derivatives, owned by one
!     object instead of living as three module variables in M_DIFFR.
!
!     Lifetime is the run: SWPREP sizes the arrays once the computational grid
!     is known, the diffraction routines fill them before each sweep, the
!     propagation and output routines read them, and SWCLME releases them.
!
!     Passing this as an explicit argument is what makes that dependency
!     visible at every call site. Under OpenMP the object is shared: DIFPAR and
!     SwanDiffPar write it single threaded, and the workers only read it, with
!     the existing barriers in SWCOMP separating the two.
!
!     IDIFFR = 0 leaves the arrays sized zero rather than unallocated, so a
!     consumer never has to test allocation status before referring to them.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: diffraction_state_t

   type :: diffraction_state_t
!     PARAM  : diffraction parameter per computational grid point
!     DPARDX : its derivative in x
!     DPARDY : its derivative in y
      real(swan_real), allocatable :: param(:)
      real(swan_real), allocatable :: dpardx(:)
      real(swan_real), allocatable :: dpardy(:)
   contains
      procedure :: resize
      procedure :: clear
      procedure :: is_sized
   end type diffraction_state_t

contains

subroutine resize (self, npoints)
!
!     Size the three arrays for NPOINTS computational grid points. NPOINTS = 0
!     is the empty state used when diffraction is switched off.
!
   class(diffraction_state_t), intent(inout) :: self
   integer, intent(in) :: npoints

   if (allocated(self%param)) then
      if (size(self%param) == npoints) return
      call self%clear()
   end if

   allocate(self%param(npoints))
   allocate(self%dpardx(npoints))
   allocate(self%dpardy(npoints))
end subroutine resize

subroutine clear (self)
   class(diffraction_state_t), intent(inout) :: self

   if (allocated(self%param))  deallocate(self%param)
   if (allocated(self%dpardx)) deallocate(self%dpardx)
   if (allocated(self%dpardy)) deallocate(self%dpardy)
end subroutine clear

logical function is_sized (self)
!
!     True once RESIZE has run, whatever the size. Consumers that only read the
!     arrays need no such test; this exists for the preparation path.
!
   class(diffraction_state_t), intent(in) :: self

   is_sized = allocated(self%param)
end function is_sized

end module swan_diffraction_state
