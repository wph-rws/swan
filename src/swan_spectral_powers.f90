module swan_spectral_powers
!
!     Rebuildable powers of the relative-frequency grid.  The table is
!     run-shared and is only rebuilt by the command/preparation path, outside
!     active computational regions.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: spectral_powers_t

   type :: spectral_powers_t
      real(swan_real), allocatable :: value(:,:)
   contains
      procedure :: rebuild => rebuild_spectral_powers
      procedure :: clear => clear_spectral_powers
   end type spectral_powers_t

contains

subroutine rebuild_spectral_powers(self, frequencies)
   class(spectral_powers_t), intent(inout) :: self
   real(swan_real), intent(in) :: frequencies(:)
   integer :: power

   if (allocated(self%value)) then
      if (size(self%value,1) /= size(frequencies) .or.&
          size(self%value,2) /= 6) deallocate(self%value)
   end if
   if (.not. allocated(self%value)) allocate(self%value(size(frequencies),6))

   self%value(:,1) = frequencies
   do power = 2, 6
      self%value(:,power) = frequencies * self%value(:,power-1)
   end do
end subroutine rebuild_spectral_powers

subroutine clear_spectral_powers(self)
   class(spectral_powers_t), intent(inout) :: self

   if (allocated(self%value)) deallocate(self%value)
end subroutine clear_spectral_powers

end module swan_spectral_powers
