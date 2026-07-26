module swan_triad_state
!
!     Run-shared data used by the triad interaction models.  The driver owns
!     one instance and passes it to parsing, preparation and computation.
!
   use swan_kinds, only: swan_real
   implicit none(type, external)
   private

   public :: triad_state_t

   type :: triad_state_t
      integer :: frequency_dimension = 0
      integer :: lower_index(3,2) = 0
      integer :: lower_index_next(3,2) = 0
      integer :: upper_index(3) = 0
      integer :: upper_index_next(3) = 0
      real(swan_real) :: lower_weight(3,2) = 0.0_swan_real
      real(swan_real) :: lower_weight_next(3,2) = 0.0_swan_real
      real(swan_real) :: upper_weight(3) = 0.0_swan_real
      real(swan_real) :: upper_weight_next(3) = 0.0_swan_real
      real(swan_real), allocatable :: biphase_unfiltered(:)
      real(swan_real), allocatable :: interpolation(:,:)
      real(swan_real), allocatable :: scaling(:,:,:)
      logical :: collinear = .true.
   contains
      procedure :: resize => resize_triad_state
      procedure :: clear => clear_triad_state
   end type triad_state_t

contains

subroutine resize_triad_state(self, model, frequency_count, grid_point_count, &
                              biphase_model, allocation_status)
   class(triad_state_t), intent(inout) :: self
   integer, intent(in) :: model, frequency_count, grid_point_count
   integer, intent(in) :: biphase_model
   integer, intent(out) :: allocation_status

   call self%clear()
   allocation_status = 0

   select case (model)
   case (1, 11)
      self%frequency_dimension = frequency_count
      allocate(self%interpolation(0,0))
      allocate(self%scaling(frequency_count, grid_point_count, 2), &
               stat=allocation_status)
   case (2, 3)
      self%frequency_dimension = frequency_count * frequency_count + &
         frequency_count * (frequency_count - 1) / 2
      allocate(self%interpolation(self%frequency_dimension, 2))
      allocate(self%scaling(self%frequency_dimension, grid_point_count, 4), &
               stat=allocation_status)
   case (5)
      if (self%collinear) then
         self%frequency_dimension = frequency_count * (frequency_count - 1) / 2
      else
         self%frequency_dimension = frequency_count * frequency_count
      end if
      allocate(self%interpolation(self%frequency_dimension, 2))
      allocate(self%scaling(self%frequency_dimension, grid_point_count, 2), &
               stat=allocation_status)
   case default
      allocate(self%interpolation(0,0))
      allocate(self%scaling(0,0,0))
   end select

   if (allocation_status /= 0) return
   if (biphase_model == 3) then
      allocate(self%biphase_unfiltered(grid_point_count), stat=allocation_status)
      if (allocation_status == 0) self%biphase_unfiltered = 0.0_swan_real
   end if
end subroutine resize_triad_state

subroutine clear_triad_state(self)
   class(triad_state_t), intent(inout) :: self

   if (allocated(self%biphase_unfiltered)) deallocate(self%biphase_unfiltered)
   if (allocated(self%interpolation)) deallocate(self%interpolation)
   if (allocated(self%scaling)) deallocate(self%scaling)
   self%frequency_dimension = 0
end subroutine clear_triad_state

end module swan_triad_state
