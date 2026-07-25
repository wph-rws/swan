module swan_sweep_exchange_backend
   use swan_build_config, only: timing_enabled
   use swan_computational_grid, only: MCGRD, MXC, MYC
   use swan_mpi_backend, only: mpi_backend_enabled
   use swan_parallel, only: SWEXCHG_WFR, SWRECVAC, SWSENDAC
   use swan_service_interfaces, only: STPNOW, SWTSTA, SWTSTO
   use swan_spectral_grid, only: MDC, MSC
   implicit none(type, external)
   private

   logical, parameter, public :: sweep_exchange_is_jacobi = .false.

   type, public :: sweep_layout_t
      integer :: outer_first = 0
      integer :: outer_last = -1
      integer :: outer_step = 1
      integer :: inner_first = 0
      integer :: inner_last = -1
      integer :: inner_step = 1
      integer :: node = 0
      integer :: start_boundary = 0
      integer :: end_boundary = 0
      logical :: transpose = .false.
   end type sweep_layout_t

   public :: complete_sweep, complete_sweep_iteration
   public :: configure_sweep_layout, configured_sweep_direction
   public :: exchange_swan_field, map_sweep_point
   public :: propagation_stencil_length
   public :: receive_sweep_row, send_sweep_row

contains

   integer function configured_sweep_direction(sweep_index, block_color)
      integer, intent(in) :: sweep_index, block_color

      configured_sweep_direction = sweep_index + 1 + 0*block_color
   end function configured_sweep_direction

   integer function propagation_stencil_length(propagation_scheme)
      integer, intent(in) :: propagation_scheme

      select case (propagation_scheme)
      case (3)
         propagation_stencil_length = 3
      case (2)
         propagation_stencil_length = 2
      case default
         propagation_stencil_length = 1
      end select
   end function propagation_stencil_length

   subroutine configure_sweep_layout(layout, sweep_direction, ix_first, &
      ix_last, iy_first, iy_last, x_step, y_step, global_x, global_y, &
      parallel, node, process_count)
      type(sweep_layout_t), intent(out) :: layout
      integer, intent(in) :: sweep_direction
      integer, intent(in) :: ix_first, ix_last, iy_first, iy_last
      integer, intent(in) :: x_step, y_step, global_x, global_y
      logical, intent(in) :: parallel
      integer, intent(in) :: node, process_count

      integer :: row_first, row_last

      layout%transpose = global_x <= global_y .and. parallel
      if (.not.layout%transpose) then
         row_first = iy_first
         row_last = iy_last
         layout%outer_step = -y_step
         layout%inner_first = ix_first
         layout%inner_last = ix_last
         layout%inner_step = -x_step
         select case (sweep_direction)
         case (1)
            layout%start_boundary = row_last - 1
            layout%end_boundary = row_first - 1
            layout%node = node
            layout%outer_first = row_first
            layout%outer_last = row_last + process_count - 1
         case (2)
            layout%start_boundary = row_last - 1
            layout%end_boundary = row_first - 1
            layout%node = process_count + 1 - node
            layout%outer_first = row_first
            layout%outer_last = row_last + process_count - 1
         case (3)
            layout%start_boundary = row_first - 1
            layout%end_boundary = row_last - 1
            layout%node = process_count + 1 - node
            layout%outer_first = row_first + process_count - 1
            layout%outer_last = row_last
         case default
            layout%start_boundary = row_first - 1
            layout%end_boundary = row_last - 1
            layout%node = node
            layout%outer_first = row_first + process_count - 1
            layout%outer_last = row_last
         end select
      else
         row_first = ix_first
         row_last = ix_last
         layout%outer_step = -x_step
         layout%inner_first = iy_first
         layout%inner_last = iy_last
         layout%inner_step = -y_step
         select case (sweep_direction)
         case (1)
            layout%start_boundary = row_last - 1
            layout%end_boundary = row_first - 1
            layout%node = node
            layout%outer_first = row_first
            layout%outer_last = row_last + process_count - 1
         case (2)
            layout%start_boundary = row_first - 1
            layout%end_boundary = row_last - 1
            layout%node = node
            layout%outer_first = row_first + process_count - 1
            layout%outer_last = row_last
         case (3)
            layout%start_boundary = row_first - 1
            layout%end_boundary = row_last - 1
            layout%node = process_count + 1 - node
            layout%outer_first = row_first + process_count - 1
            layout%outer_last = row_last
         case default
            layout%start_boundary = row_last - 1
            layout%end_boundary = row_first - 1
            layout%node = process_count + 1 - node
            layout%outer_first = row_first
            layout%outer_last = row_last + process_count - 1
         end select
      end if
   end subroutine configure_sweep_layout

   subroutine map_sweep_point(layout, row_token, point_token, active, ix, iy)
      type(sweep_layout_t), intent(in) :: layout
      integer, intent(in) :: row_token, point_token
      logical, intent(out) :: active
      integer, intent(out) :: ix, iy

      integer :: row

      row = row_token - layout%node + 1
      active = layout%node >= row_token - layout%start_boundary .and. &
         layout%node <= row_token - layout%end_boundary
      if (layout%transpose) then
         ix = row
         iy = point_token
      else
         ix = point_token
         iy = row
      end if
   end subroutine map_sweep_point

   subroutine receive_sweep_row(layout, row_token, sweep_direction, stencil, &
      action_density, group_points, stop_requested)
      type(sweep_layout_t), intent(in) :: layout
      integer, intent(in) :: row_token, sweep_direction, stencil
      real, intent(inout) :: action_density(*)
      integer, intent(in) :: group_points(*)
      logical, intent(out) :: stop_requested

      integer :: offset, row

      stop_requested = .false.
      if (.not.mpi_backend_enabled) return
      row = row_token - layout%node + 1
      if (timing_enabled) call SWTSTA(213)
      if (.not.layout%transpose) then
         do offset = stencil, 1, -1
            call SWRECVAC(action_density, &
               layout%inner_first-offset*layout%inner_step, row, &
               sweep_direction, group_points)
         end do
      else
         do offset = stencil, 1, -1
            call SWRECVAC(action_density, row, &
               layout%inner_first-offset*layout%inner_step, &
               sweep_direction, group_points)
         end do
      end if
      if (timing_enabled) call SWTSTO(213)
      stop_requested = STPNOW()
   end subroutine receive_sweep_row

   subroutine send_sweep_row(layout, row_token, sweep_direction, stencil, &
      action_density, group_points, stop_requested)
      type(sweep_layout_t), intent(in) :: layout
      integer, intent(in) :: row_token, sweep_direction, stencil
      real, intent(inout) :: action_density(*)
      integer, intent(in) :: group_points(*)
      logical, intent(out) :: stop_requested

      integer :: offset, row

      stop_requested = .false.
      if (.not.mpi_backend_enabled) return
      row = row_token - layout%node + 1
      if (timing_enabled) call SWTSTA(213)
      if (.not.layout%transpose) then
         do offset = stencil-1, 0, -1
            call SWSENDAC(action_density, &
               layout%inner_last-offset*layout%inner_step, row, &
               sweep_direction, group_points)
         end do
      else
         do offset = stencil-1, 0, -1
            call SWSENDAC(action_density, row, &
               layout%inner_last-offset*layout%inner_step, &
               sweep_direction, group_points)
         end do
      end if
      if (timing_enabled) call SWTSTO(213)
      stop_requested = STPNOW()
   end subroutine send_sweep_row

   subroutine exchange_swan_field(field, group_points)
      real, intent(inout) :: field(*)
      integer, intent(in) :: group_points(*)

      call SWEXCHG_WFR(field, group_points)
   end subroutine exchange_swan_field

   subroutine complete_sweep(sweep_index, action_density, work, &
      group_points, stop_requested)
      integer, intent(in) :: sweep_index
      real, intent(inout) :: action_density(MDC,MSC,MCGRD)
      real, allocatable, intent(inout) :: work(:)
      integer, intent(in) :: group_points(MXC*MYC)
      logical, intent(out) :: stop_requested

      stop_requested = .false.
      if (sweep_index < 0 .and. allocated(work)) then
         stop_requested = size(action_density) == 0 .and. group_points(1) == 0
      end if
   end subroutine complete_sweep

   subroutine complete_sweep_iteration(action_density, work, group_points, &
      stop_requested)
      real, intent(inout) :: action_density(MDC,MSC,MCGRD)
      real, allocatable, intent(inout) :: work(:)
      integer, intent(in) :: group_points(MXC*MYC)
      logical, intent(out) :: stop_requested

      integer :: direction, frequency

      stop_requested = .false.
      if (.not.mpi_backend_enabled) return
      if (timing_enabled) call SWTSTA(213)
      do direction = 1, MDC
         do frequency = 1, MSC
            work(:) = action_density(direction,frequency,:)
            call exchange_swan_field(work, group_points)
            action_density(direction,frequency,:) = work(:)
         end do
      end do
      if (timing_enabled) call SWTSTO(213)
      stop_requested = STPNOW()
   end subroutine complete_sweep_iteration

end module swan_sweep_exchange_backend
