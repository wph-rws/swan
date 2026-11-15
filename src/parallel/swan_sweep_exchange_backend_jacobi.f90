module swan_sweep_exchange_backend
   use swan_build_config, only: timing_enabled
   use swan_computational_grid, only: MCGRD, MXC, MYC
   use M_PARALL, only: IBLKAD, PARLL, SWMPI_CHECK, SWREAL
   use swan_mpi_backend, only: mpi_backend_enabled, mpi_backend_irecv_real, &
      mpi_backend_isend_real, mpi_backend_wait_all
   use swan_parallel, only: SWEXCHG_JAC, SWSYNC
   use swan_service_interfaces, only: STPNOW, SWTSTA, SWTSTO
   use swan_spectral_grid, only: MDC, MSC
   implicit none(type, external)
   private

   logical, parameter, public :: sweep_exchange_is_jacobi = .true.

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

   select case (block_color)
   case (1)
      configured_sweep_direction = mod(sweep_index,4) + 1
   case (2)
      configured_sweep_direction = mod(sweep_index+1,4) + 1
   case (3)
      configured_sweep_direction = mod(sweep_index+2,4) + 1
   case default
      configured_sweep_direction = mod(sweep_index+3,4) + 1
   end select
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

   layout%outer_first = iy_first
   layout%outer_last = iy_last
   layout%outer_step = -y_step
   layout%inner_first = ix_first
   layout%inner_last = ix_last
   layout%inner_step = -x_step
   layout%node = 0*sweep_direction + 0*global_x + 0*global_y + 0*node + &
      0*process_count
   layout%start_boundary = merge(1,0,parallel)
   layout%end_boundary = layout%start_boundary
   layout%transpose = .false.
end subroutine configure_sweep_layout

subroutine map_sweep_point(layout, row_token, point_token, active, ix, iy)
   type(sweep_layout_t), intent(in) :: layout
   integer, intent(in) :: row_token, point_token
   logical, intent(out) :: active
   integer, intent(out) :: ix, iy

   active = .not.layout%transpose
   ix = point_token
   iy = row_token
end subroutine map_sweep_point

subroutine receive_sweep_row(layout, row_token, sweep_direction, stencil, &
   action_density, group_points, stop_requested)
   type(sweep_layout_t), intent(in) :: layout
   integer, intent(in) :: row_token, sweep_direction, stencil
   real, intent(inout) :: action_density(*)
   integer, intent(in) :: group_points(*)
   logical, intent(out) :: stop_requested

   stop_requested = .false.
   if (layout%node < 0 .and. row_token + sweep_direction + stencil < 0) &
      action_density(1) = real(group_points(1))
end subroutine receive_sweep_row

subroutine send_sweep_row(layout, row_token, sweep_direction, stencil, &
   action_density, group_points, stop_requested)
   type(sweep_layout_t), intent(in) :: layout
   integer, intent(in) :: row_token, sweep_direction, stencil
   real, intent(inout) :: action_density(*)
   integer, intent(in) :: group_points(*)
   logical, intent(out) :: stop_requested

   stop_requested = .false.
   if (layout%node < 0 .and. row_token + sweep_direction + stencil < 0) &
      action_density(1) = real(group_points(1))
end subroutine send_sweep_row

subroutine exchange_swan_field(field, group_points)
   real, intent(inout) :: field(*)
   integer, intent(in) :: group_points(*)

   call SWEXCHG_JAC(field, 0, group_points)
end subroutine exchange_swan_field

subroutine complete_sweep(sweep_index, action_density, group_points, &
   stop_requested)
   integer, intent(in) :: sweep_index
   real, intent(inout) :: action_density(MDC,MSC,MCGRD)
   integer, intent(in) :: group_points(MXC*MYC)
   logical, intent(out) :: stop_requested

   stop_requested = .false.
   if (sweep_index /= 3 .or. .not.mpi_backend_enabled) return
   if (timing_enabled) call SWTSTA(213)
   !  With one process there is no neighbour and nothing to exchange.
   if (PARLL) then
      call exchange_action_density(action_density, group_points)
      if (STPNOW()) then
         stop_requested = .true.
         return
      end if
   end if
   call SWSYNC
   stop_requested = STPNOW()
   if (timing_enabled) call SWTSTO(213)
end subroutine complete_sweep

!  Exchange the action density on the edges of the subdomain with the
!  neighbouring subdomains: for every neighbour one message with all
!  spectral bins of the points it needs, and one back. SWEXCHG_JAC does
!  the same for one field at a time, which took MDC*MSC small messages per
!  neighbour; the values and the order in which they are stored are the
!  same. The receives are posted first and the sends do not block: two
!  neighbours send to each other at the same time, and a blocking send of a
!  message this large waits until the other side receives.
subroutine exchange_action_density(action_density, group_points)
   real, intent(inout) :: action_density(MDC,MSC,MCGRD)
   integer, intent(in) :: group_points(MXC*MYC)

   integer, parameter :: tag = 2
   integer :: neighbour, neighbours, domain, start, count, k, ierr
   integer, allocatable :: first(:), requests(:)
   real, allocatable :: incoming(:,:,:), outgoing(:,:,:)

   neighbours = IBLKAD(1)
   allocate(first(neighbours+1), requests(2*neighbours))
   first(1) = 1
   do neighbour = 1, neighbours
      start = IBLKAD(3*neighbour+1)
      first(neighbour+1) = first(neighbour) + IBLKAD(start)
   end do
   !  one element more than is used, so that the buffer of a neighbour
   !  without points still has an address
   allocate(incoming(MDC,MSC,first(neighbours+1)))
   allocate(outgoing(MDC,MSC,first(neighbours+1)))

   do neighbour = 1, neighbours
      domain = IBLKAD(3*neighbour-1)
      start = IBLKAD(3*neighbour+1)
      count = IBLKAD(start)
      call mpi_backend_irecv_real(incoming(1,1,first(neighbour)), &
         MDC*MSC*count, SWREAL, domain-1, tag, requests(neighbour), ierr)
      call SWMPI_CHECK(ierr, 'MPI_IRECV')
   end do
   do neighbour = 1, neighbours
      domain = IBLKAD(3*neighbour-1)
      start = IBLKAD(3*neighbour+1)
      count = IBLKAD(start)
      do k = 1, count
         outgoing(:,:,first(neighbour)+k-1) = &
            action_density(:,:,group_points(IBLKAD(start+k)))
      end do
      call mpi_backend_isend_real(outgoing(1,1,first(neighbour)), &
         MDC*MSC*count, SWREAL, domain-1, tag, &
         requests(neighbours+neighbour), ierr)
      call SWMPI_CHECK(ierr, 'MPI_ISEND')
   end do
   call mpi_backend_wait_all(2*neighbours, requests, ierr)
   call SWMPI_CHECK(ierr, 'MPI_WAITALL')

   do neighbour = 1, neighbours
      start = IBLKAD(3*neighbour+1)
      count = IBLKAD(start)
      do k = 1, count
         action_density(:,:,group_points(IBLKAD(start+count+k))) = &
            incoming(:,:,first(neighbour)+k-1)
      end do
   end do
end subroutine exchange_action_density

subroutine complete_sweep_iteration(action_density, group_points, &
   stop_requested)
   real, intent(inout) :: action_density(MDC,MSC,MCGRD)
   integer, intent(in) :: group_points(MXC*MYC)
   logical, intent(out) :: stop_requested

   stop_requested = .false.
   if (.not.mpi_backend_enabled) then
      stop_requested = size(action_density) == 0 .and. group_points(1) == 0
   end if
end subroutine complete_sweep_iteration

end module swan_sweep_exchange_backend
