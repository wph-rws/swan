program test_block_coloring_backend
   use M_PARALL, only: IBLACK, IGREEN, IHALOX, IHALOY, IRED, IYELOW
   use swan_block_coloring_backend, only: block_coloring_enabled, &
      color_swan_subdomains
   use swan_build_config, only: jacobi_sweep_enabled
   use swan_sweep_exchange_backend, only: configure_sweep_layout, &
      configured_sweep_direction, map_sweep_point, &
      propagation_stencil_length, sweep_exchange_is_jacobi, sweep_layout_t
   implicit none(type, external)

   integer :: expected, ix, iy, status, sweep_index
   integer :: group_points(1,1)
   logical :: active, multi_coloring
   type(sweep_layout_t) :: layout
   character(len=8) :: argument

   if (command_argument_count() /= 1) &
      error stop 'test_block_coloring_backend expects one integer value'
   call get_command_argument(1, argument)
   read (argument, *, iostat=status) expected
   if (status /= 0) error stop 'invalid expected sweep capability value'

   if (jacobi_sweep_enabled .neqv. (expected == 1)) &
      error stop 'unexpected configured sweep capability'
   if (block_coloring_enabled .neqv. jacobi_sweep_enabled) &
      error stop 'block-coloring backend and sweep configuration disagree'
   if (sweep_exchange_is_jacobi .neqv. jacobi_sweep_enabled) &
      error stop 'field-exchange backend and sweep configuration disagree'
   if (any([(configured_sweep_direction(sweep_index,2), sweep_index=0,3)] /= &
      merge([2,3,4,1], [1,2,3,4], jacobi_sweep_enabled))) &
      error stop 'unexpected configured sweep order'
   if (any([(propagation_stencil_length(sweep_index), sweep_index=1,3)] /= &
      [1,2,3])) error stop 'unexpected propagation stencil length'

   call configure_sweep_layout(layout, 1, 2, 8, 2, 7, -1, -1, &
      20, 10, .false., 1, 1)
   if (any([layout%outer_first, layout%outer_last, layout%outer_step, &
      layout%inner_first, layout%inner_last, layout%inner_step] /= &
      [2,7,1,2,8,1])) error stop 'unexpected serial sweep layout'
   call map_sweep_point(layout, 3, 4, active, ix, iy)
   if (.not.active .or. ix /= 4 .or. iy /= 3) &
      error stop 'unexpected serial sweep point mapping'

   if (jacobi_sweep_enabled) then
      if (IHALOX /= 1 .or. IHALOY /= 1) &
         error stop 'unexpected Jacobi halo width'
      if (any([IRED, IYELOW, IGREEN, IBLACK] /= [1, 2, 3, 4])) &
         error stop 'unexpected Jacobi color constants'
   else
      if (IHALOX /= 3 .or. IHALOY /= 3) &
         error stop 'unexpected wavefront halo width'
      group_points = 7
      multi_coloring = .false.
      call color_swan_subdomains(multi_coloring, group_points)
      if (multi_coloring .or. group_points(1,1) /= 7) &
         error stop 'disabled block-coloring backend changed its inputs'
   end if
end program test_block_coloring_backend
