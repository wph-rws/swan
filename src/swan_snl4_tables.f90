module swan_snl4_tables
!
!     Run-shared precomputed data for quadruplet interactions.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: snl4_tables_t

   type :: snl4_tables_t
      integer :: quadruplet_count = 1
      real(swan_real), allocatable :: frequency_power_11(:)
      real(swan_real), allocatable :: coefficient_1(:)
      real(swan_real), allocatable :: coefficient_2(:)
      real(swan_real), allocatable :: lambda(:)
      real(swan_real), allocatable :: cached_dal1(:)
      real(swan_real), allocatable :: cached_dal2(:)
      real(swan_real), allocatable :: cached_dal3(:)
      real(swan_real), allocatable :: cached_angular_weights(:,:)
      integer, allocatable :: cached_indices(:,:)
      real(swan_real), allocatable :: cached_spectral_weights(:,:)
   contains
      procedure :: clear => clear_snl4_tables
   end type snl4_tables_t

contains

subroutine clear_snl4_tables(self)
   class(snl4_tables_t), intent(inout) :: self

   if (allocated(self%frequency_power_11)) deallocate(self%frequency_power_11)
   if (allocated(self%coefficient_1)) deallocate(self%coefficient_1)
   if (allocated(self%coefficient_2)) deallocate(self%coefficient_2)
   if (allocated(self%lambda)) deallocate(self%lambda)
   if (allocated(self%cached_dal1)) deallocate(self%cached_dal1)
   if (allocated(self%cached_dal2)) deallocate(self%cached_dal2)
   if (allocated(self%cached_dal3)) deallocate(self%cached_dal3)
   if (allocated(self%cached_angular_weights)) deallocate(self%cached_angular_weights)
   if (allocated(self%cached_indices)) deallocate(self%cached_indices)
   if (allocated(self%cached_spectral_weights)) deallocate(self%cached_spectral_weights)
   self%quadruplet_count = 1
end subroutine clear_snl4_tables

end module swan_snl4_tables
