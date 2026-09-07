module swan_vegetation_layers
!
!     The vegetation model's per-layer properties.
!
!     Vegetation is described as a stack of layers, each with its own thickness,
!     stem diameter, drag coefficient and plant density. Unlike the input fields
!     these are indexed by layer as well as by grid point, and only the
!     vegetation dissipation term reads them.
!
!     Three files use these four arrays. ILMAX below says how many
!     layers there are.
!
   implicit none(type, external)
   private

   public :: LAYH, VEGDIL, VEGDRL, VEGNSL
   public :: ILMAX
   public :: CLEAR_VEGETATION_LAYERS, VEGETATION_LAYERS_ARE_CLEAR

!     ILMAX  : maximum number of layers used in the vegetation model. Not
!              thread state; it lived in swan_stencil only because it was
!              declared between two stencil arrays.
   integer :: ILMAX = 0

!     LAYH   : thickness of each layer
!     VEGDIL : stem diameter per layer and grid point
!     VEGDRL : drag coefficient per layer and grid point
!     VEGNSL : number of plants per square meter, per layer and grid point
   real, save, allocatable :: LAYH(:)
   real, save, allocatable :: VEGDIL(:), VEGDRL(:), VEGNSL(:)

contains

   subroutine CLEAR_VEGETATION_LAYERS ()
      if (allocated(LAYH  )) deallocate(LAYH  )
      if (allocated(VEGDIL)) deallocate(VEGDIL)
      if (allocated(VEGDRL)) deallocate(VEGDRL)
      if (allocated(VEGNSL)) deallocate(VEGNSL)
   end subroutine CLEAR_VEGETATION_LAYERS

   logical function VEGETATION_LAYERS_ARE_CLEAR ()
      VEGETATION_LAYERS_ARE_CLEAR = .not.allocated(LAYH) .and. &
         .not.allocated(VEGDIL) .and. .not.allocated(VEGDRL) .and. &
         .not.allocated(VEGNSL)
   end function VEGETATION_LAYERS_ARE_CLEAR
end module swan_vegetation_layers
