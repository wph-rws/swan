module m_propcache
   implicit none(type, external)
   private

   logical, public, save :: prop_cache_valid = .false.
   real, public, allocatable, save :: prop_kwave(:,:)
   real, public, allocatable, save :: prop_cgo(:,:)
   real, public, allocatable, save :: prop_dmw(:,:)
   real, public, allocatable, save :: prop_sinh2(:,:), prop_tanh2(:,:)

   public :: prop_cache_reset, bottom_motion_factors

contains

subroutine prop_cache_reset()
   prop_cache_valid = .false.
   if (allocated(prop_kwave)) deallocate(prop_kwave)
   if (allocated(prop_cgo))   deallocate(prop_cgo)
   if (allocated(prop_dmw))   deallocate(prop_dmw)
   if (allocated(prop_sinh2)) deallocate(prop_sinh2)
   if (allocated(prop_tanh2)) deallocate(prop_tanh2)
end subroutine prop_cache_reset

pure subroutine bottom_motion_factors(n, kwave, depth, sinh2, tanh2)
   integer, intent(in) :: n
   real, intent(in) :: kwave(n), depth
   real, intent(out) :: sinh2(n), tanh2(n)

   sinh2(:) = sinh(min(30., kwave(:)*depth))**2
   tanh2(:) = tanh(min(30., kwave(:)*depth))**2
end subroutine bottom_motion_factors

end module m_propcache
