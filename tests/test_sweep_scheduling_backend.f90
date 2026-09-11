program test_sweep_scheduling_backend
   use swan_sweep_exchange_backend, only: configure_sweep_layout, &
      configured_sweep_direction, map_sweep_point, &
      propagation_stencil_length, sweep_exchange_is_jacobi, sweep_layout_t
   implicit none(type, external)

   type(sweep_layout_t) :: layout
   integer :: expected, ix, iy, status
   logical :: active
   character(len=32) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_sweep_scheduling_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected sweep selection'
   if (sweep_exchange_is_jacobi .neqv. (expected == 1)) &
      error stop 'unexpected sweep scheduling backend selection'
   if (propagation_stencil_length(3) /= 3 .or. &
       propagation_stencil_length(2) /= 2 .or. &
       propagation_stencil_length(1) /= 1) &
      error stop 'unexpected propagation stencil length'

   call configure_sweep_layout(layout, 3, 10, 2, 8, 1, 1, 1, &
      20, 10, .true., 2, 4)
   if (sweep_exchange_is_jacobi) then
      if (configured_sweep_direction(0,1) /= 1 .or. &
          configured_sweep_direction(0,2) /= 2 .or. &
          configured_sweep_direction(0,3) /= 3 .or. &
          configured_sweep_direction(0,4) /= 4) &
         error stop 'Jacobi colour-dependent sweep order changed'
      if (layout%outer_first /= 8 .or. layout%outer_last /= 1 .or. &
          layout%outer_step /= -1 .or. layout%inner_first /= 10 .or. &
          layout%inner_last /= 2 .or. layout%inner_step /= -1) &
         error stop 'Jacobi rectangular layout changed'
      call map_sweep_point(layout, 8, 10, active, ix, iy)
   else
      if (configured_sweep_direction(0,4) /= 1 .or. &
          configured_sweep_direction(3,1) /= 4) &
         error stop 'wavefront sweep order changed'
      if (layout%outer_first /= 11 .or. layout%outer_last /= 1 .or. &
          layout%outer_step /= -1 .or. layout%inner_first /= 10 .or. &
          layout%inner_last /= 2 .or. layout%inner_step /= -1) &
         error stop 'wavefront non-transposed layout changed'
      call map_sweep_point(layout, 10, 10, active, ix, iy)
   end if
   if (.not.active .or. ix /= 10 .or. iy /= 8) &
      error stop 'sweep point mapping changed'
end program test_sweep_scheduling_backend
