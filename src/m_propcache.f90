module m_propcache
  implicit none(type, external)
  private

  logical, public, save :: prop_cache_valid = .false.
  real, public, allocatable, save :: prop_kwave(:,:)
  real, public, allocatable, save :: prop_cgo(:,:)
  real, public, allocatable, save :: prop_dmw(:,:)

  public :: prop_cache_reset

contains

  subroutine prop_cache_reset()
    prop_cache_valid = .false.
    if (allocated(prop_kwave)) deallocate(prop_kwave)
    if (allocated(prop_cgo))   deallocate(prop_cgo)
    if (allocated(prop_dmw))   deallocate(prop_dmw)
  end subroutine prop_cache_reset

end module m_propcache
