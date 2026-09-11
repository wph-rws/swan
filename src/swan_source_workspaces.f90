module swan_source_workspaces
!
!     Per-thread source-term state.  The two solver workspaces stay distinct
!     because their remaining scratch and seed state have different lifetimes;
!     only the source workspace is shared structurally.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: wcap_workspace_t, dia_workspace_t, fft_workspace_t
   public :: point_integrals_t, test_output_t, source_budget_t
   public :: iteration_cache_t, system_matrix_t
   public :: source_workspace_t
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

   type :: dia_workspace_t
!     Scratch of the DIA quadruplet calculation plus the coefficient tables
!     it shares with the explicit quadruplet routines.  The allocatable
!     members keep the caller-chosen bounds (MSC4MI:MSC4MA, MDC4MI:MDC4MA);
!     the fixed-size members carry the FAC4WW/SWPRE4W output.  Deliberately
!     without default initialization: FAC4WW/SWPRE4W define every member
!     that is ever read (they are unused when IQUAD < 1, exactly as the
!     stack locals they replace were).
      real, dimension(:,:), allocatable :: ue, sa1, sa2, sfnl, dsnl
      real, dimension(:,:), allocatable :: da1c, da1p, da1m, da2c, da2p, da2m
      integer :: wwint(24)
      real :: wwawg(8), wwswg(8)
      real :: snlc1, dal1, dal2, dal3
   contains
      procedure :: release => release_dia_workspace
   end type dia_workspace_t

   type :: fft_workspace_t
!     Fourier workspace of the quasi-coherent framework: transformed
!     modulations, coefficients and the FFTPACK work arrays.  Kinds mirror
!     the previous explicit declarations of the per-thread allocatables.
      real, dimension(:,:,:), allocatable :: cgft, sigft
      complex, dimension(:,:), allocatable :: uxft, uyft
      complex(kind=8), dimension(:,:), allocatable :: cft
      real(kind=8), dimension(:,:), allocatable :: rft, sft
      real(kind=8), dimension(:), allocatable :: wft, wsave
      complex(kind=8), dimension(:,:), allocatable :: cfd
      real(kind=8), dimension(:), allocatable :: wfd, wsavd
   contains
      procedure :: release => release_fft_workspace
   end type fft_workspace_t

!  Values produced once per geographical point and consumed together by the
!  source-term dispatcher.  Scalar members intentionally have no default
!  initialization: several legacy paths preserve the preceding thread-local
!  value, and bundling must not change that numerical contract.
   type :: point_integrals_t
      real, pointer :: abrbot => null(), kmespc => null(), smespc => null()
      real, pointer :: hs => null(), etot => null(), qbloc => null(), hm => null()
      real, pointer :: fpm => null(), wind10 => null(), etotw => null()
      real, pointer :: smebrk => null(), kteta => null()
      real, pointer :: ubot(:) => null()
      real, pointer :: ustar(:) => null()
      real, pointer :: zelen(:) => null()
      real, pointer :: ursell(:) => null()
      real, pointer :: tauwv(:) => null()
      real, pointer :: biphas(:) => null()
   end type point_integrals_t

!  Non-owning views on the source-term test arrays.  They only travel across
!  the solver-to-SOURCE hop; individual kernels continue to receive exactly
!  the one output array they used before.
   type :: test_output_t
      real, pointer :: plwnds(:,:,:) => null()
      real, pointer :: plwndd(:,:,:) => null()
      real, pointer :: plwcap(:,:,:) => null()
      real, pointer :: plbtfr(:,:,:) => null()
      real, pointer :: plswel(:,:,:) => null()
      real, pointer :: plwbrk(:,:,:) => null()
      real, pointer :: plnl4s(:,:,:) => null()
      real, pointer :: plnl4d(:,:,:) => null()
      real, pointer :: plvegt(:,:,:) => null()
      real, pointer :: plturb(:,:,:) => null()
      real, pointer :: plmud(:,:,:) => null()
      real, pointer :: plice(:,:,:) => null()
      real, pointer :: plbrag(:,:,:) => null()
      real, pointer :: pltri(:,:,:) => null()
      logical :: testfl
      integer :: iptst
   end type test_output_t

!  Explicit/implicit source accounting.  SOURCE and QCSOURCE receive the
!  complete set, then unpack only the member required by each leaf kernel.
   type :: source_budget_t
      real, pointer :: dissc0(:,:,:) => null()
      real, pointer :: dissc1(:,:,:) => null()
      real, pointer :: genc0(:,:,:) => null()
      real, pointer :: genc1(:,:,:) => null()
      real, pointer :: redc0(:,:,:) => null()
      real, pointer :: redc1(:,:,:) => null()
   end type source_budget_t

!  Full-spectrum iteration caches owned by the solvers.  Only SWOMPU receives
!  the complete view; SOURCE/QCSOURCE and leaf routines get their former
!  subsets unpacked at the call site (the no-broadening rule).
   type :: iteration_cache_t
      real, pointer :: memnl4(:,:,:) => null()
      real, pointer :: membrg(:,:,:) => null()
      real, pointer :: memqcm(:,:,:) => null()
      real, pointer :: memqcb(:,:,:) => null()
      real, pointer :: memsina(:,:,:) => null()
      real, pointer :: memsinb(:,:,:) => null()
   end type iteration_cache_t

!  The directional/frequency system assembled for one point.  SOURCE keeps
!  its diagonal and right-hand-side outputs separate; this complete view is
!  restricted to ACTION and the solver family.
   type :: system_matrix_t
      real, pointer :: imatla(:,:) => null()
      real, pointer :: imatda(:,:) => null()
      real, pointer :: imatua(:,:) => null()
      real, pointer :: imatra(:,:) => null()
      real, pointer :: imat5l(:,:) => null()
      real, pointer :: imat6u(:,:) => null()
   end type system_matrix_t

   type :: source_workspace_t
      type(wcap_workspace_t) :: wcap
      type(dia_workspace_t) :: dia
      type(fft_workspace_t) :: fft
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
!     Preserve the legacy zero-energy contract exactly.  Four quantities had
!     an unconditional entry value; the other seven carried their thread's
!     prior value when ETOT <= 0 and therefore must not be initialized here.
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

subroutine release_dia_workspace(self)
!     Mirror of the per-thread DEALLOCATE block that SWCOMP and
!     SwanCompUnstruc used to spell out member by member.
   class(dia_workspace_t), intent(inout) :: self

   if (allocated(self%ue)) deallocate(self%ue)
   if (allocated(self%sa1)) deallocate(self%sa1)
   if (allocated(self%sa2)) deallocate(self%sa2)
   if (allocated(self%sfnl)) deallocate(self%sfnl)
   if (allocated(self%dsnl)) deallocate(self%dsnl)
   if (allocated(self%da1c)) deallocate(self%da1c)
   if (allocated(self%da1p)) deallocate(self%da1p)
   if (allocated(self%da1m)) deallocate(self%da1m)
   if (allocated(self%da2c)) deallocate(self%da2c)
   if (allocated(self%da2p)) deallocate(self%da2p)
   if (allocated(self%da2m)) deallocate(self%da2m)
end subroutine release_dia_workspace

subroutine release_fft_workspace(self)
   class(fft_workspace_t), intent(inout) :: self

   if (allocated(self%sigft)) deallocate(self%sigft)
   if (allocated(self%cgft)) deallocate(self%cgft)
   if (allocated(self%uxft)) deallocate(self%uxft)
   if (allocated(self%uyft)) deallocate(self%uyft)
   if (allocated(self%cft)) deallocate(self%cft)
   if (allocated(self%rft)) deallocate(self%rft)
   if (allocated(self%sft)) deallocate(self%sft)
   if (allocated(self%wft)) deallocate(self%wft)
   if (allocated(self%wsave)) deallocate(self%wsave)
   if (allocated(self%cfd)) deallocate(self%cfd)
   if (allocated(self%wfd)) deallocate(self%wfd)
   if (allocated(self%wsavd)) deallocate(self%wsavd)
end subroutine release_fft_workspace

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
