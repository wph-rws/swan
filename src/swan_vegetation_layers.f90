module swan_vegetation_layers
!
!     The vegetation model's per-layer properties.
!
!     Vegetation is described as a stack of layers, each with its own thickness,
!     stem diameter, drag coefficient and plant density. Unlike the input fields
!     these are indexed by layer as well as by grid point, and only the
!     vegetation dissipation term reads them.
!
!     Three files use these four arrays. ILMAX in swan_stencil says how many
!     layers there are.
!
   implicit none(type, external)
   private

   public :: LAYH, VEGDIL, VEGDRL, VEGNSL

!     LAYH   : thickness of each layer
!     VEGDIL : stem diameter per layer and grid point
!     VEGDRL : drag coefficient per layer and grid point
!     VEGNSL : number of plants per square meter, per layer and grid point
   real, save, allocatable :: LAYH(:)
   real, save, allocatable :: VEGDIL(:), VEGDRL(:), VEGNSL(:)
end module swan_vegetation_layers
