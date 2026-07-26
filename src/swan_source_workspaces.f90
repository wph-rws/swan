module swan_source_workspaces
!
!     Per-thread source-term state.  The two solver workspaces stay distinct
!     because their remaining scratch and seed state have different lifetimes;
!     only the source workspace is shared structurally.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: wcap_workspace_t, source_workspace_t
   public :: structured_thread_workspace_t, unstructured_thread_workspace_t
   public :: thread_workspaces_t

   type :: wcap_workspace_t
      real(swan_real) :: total_action
      real(swan_real) :: energy_over_root_wavenumber
      real(swan_real) :: energy_times_wavenumber
      real(swan_real) :: first_energy_moment
      real(swan_real) :: second_energy_moment
      real(swan_real) :: fourth_energy_moment
      real(swan_real) :: mean_wavenumber_wam
      real(swan_real) :: mean_wavenumber_01
      real(swan_real) :: mean_frequency_wam
      real(swan_real) :: mean_frequency_10
      real(swan_real) :: mean_frequency_01
   contains
      procedure :: begin_point => begin_wcap_point
   end type wcap_workspace_t

   type :: source_workspace_t
      type(wcap_workspace_t) :: wcap
   end type source_workspace_t

   type :: structured_thread_workspace_t
      type(source_workspace_t) :: source
   end type structured_thread_workspace_t

   type :: unstructured_thread_workspace_t
      type(source_workspace_t) :: source
   end type unstructured_thread_workspace_t

   type :: thread_workspaces_t
      type(structured_thread_workspace_t), allocatable :: structured(:)
      type(unstructured_thread_workspace_t), allocatable :: unstructured(:)
   contains
      procedure :: ensure_structured
      procedure :: ensure_unstructured
      procedure :: clear => clear_thread_workspaces
   end type thread_workspaces_t

contains

subroutine begin_wcap_point(self)
!
!     Preserve the legacy dry-point contract exactly.  Four quantities had an
!     unconditional entry value; the other seven carried their thread's prior
!     value when ETOT <= 0 and therefore must not be initialized here.
!
   class(wcap_workspace_t), intent(inout) :: self

   self%mean_wavenumber_wam = 10.0_swan_real
   self%mean_wavenumber_01 = 10.0_swan_real
   self%mean_frequency_01 = 10.0_swan_real
   self%mean_frequency_10 = 10.0_swan_real
end subroutine begin_wcap_point

subroutine ensure_structured(self, count)
   class(thread_workspaces_t), intent(inout) :: self
   integer, intent(in) :: count

   if (allocated(self%structured)) then
      if (size(self%structured) == count) return
      deallocate(self%structured)
   end if
   allocate(self%structured(count))
end subroutine ensure_structured

subroutine ensure_unstructured(self, count)
   class(thread_workspaces_t), intent(inout) :: self
   integer, intent(in) :: count

   if (allocated(self%unstructured)) then
      if (size(self%unstructured) == count) return
      deallocate(self%unstructured)
   end if
   allocate(self%unstructured(count))
end subroutine ensure_unstructured

subroutine clear_thread_workspaces(self)
   class(thread_workspaces_t), intent(inout) :: self

   if (allocated(self%structured)) deallocate(self%structured)
   if (allocated(self%unstructured)) deallocate(self%unstructured)
end subroutine clear_thread_workspaces

end module swan_source_workspaces
