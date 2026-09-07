program test_vegetation_layers
   use swan_vegetation_layers, only: LAYH, VEGDIL, VEGDRL, VEGNSL, &
      CLEAR_VEGETATION_LAYERS, VEGETATION_LAYERS_ARE_CLEAR
   implicit none(type, external)

   call CLEAR_VEGETATION_LAYERS()
   call require(VEGETATION_LAYERS_ARE_CLEAR(), 'initial state is not clear')

   allocate(LAYH(2), VEGDIL(2), VEGDRL(2), VEGNSL(2))

   call require(.not.VEGETATION_LAYERS_ARE_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_VEGETATION_LAYERS()
   call require(VEGETATION_LAYERS_ARE_CLEAR(), 'clear left vegetation behind')
   call CLEAR_VEGETATION_LAYERS()
   call require(VEGETATION_LAYERS_ARE_CLEAR(), 'second clear changed empty state')

   print *, 'vegetation layers clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_vegetation_layers
