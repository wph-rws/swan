module swan_input_fields
!
!     The input fields as they stand on the computational grid.
!
!     Each of these is read from a file or set to a constant, interpolated onto
!     the computational grid, and then read by the source terms. They are the
!     values behind the eighteen input grids that swan_input_grids describes:
!     that module says where a field is defined, this one holds it.
!
!     Sixteen arrays used by two or three files each. They were in M_GENARR
!     together with AC2, SPCSIG and KGRPNT, which almost every importer needs,
!     so everyone carried the ice thickness and the mud layer along.
!
   implicit none(type, external)
   private

   public :: DEPTH, WLEVL, FRIC
   public :: UXB, UYB, WXI, WYI
   public :: ASTDF, MUDLF, NPLAF, TURBF
   public :: AICEF, HICEF
   public :: HSSF, TSSF, DSSF
   public :: CLEAR_INPUT_FIELDS, INPUT_FIELDS_ARE_CLEAR

!     DEPTH : depth
!     WLEVL : water level
!     FRIC  : bottom friction coefficient
   real, save, allocatable :: DEPTH(:), WLEVL(:), FRIC(:)

!     UXB, UYB : current velocity, contravariant components
!     WXI, WYI : wind velocity, contravariant components
   real, save, allocatable :: UXB(:), UYB(:)
   real, save, allocatable :: WXI(:), WYI(:)

!     ASTDF : air-sea temperature difference
!     MUDLF : thickness of the fluid mud layer
!     NPLAF : number of plants per square meter
!     TURBF : turbulent viscosity
   real, save, allocatable :: ASTDF(:), MUDLF(:)
   real, save, allocatable :: NPLAF(:), TURBF(:)

!     AICEF : ice concentration as a fraction
!     HICEF : ice thickness in meters
   real, save, allocatable :: AICEF(:), HICEF(:)

!     The sea-swell parameters, which arrive as fields rather than being
!     computed: significant height, mean period and mean direction.
   real, save, allocatable :: HSSF(:), TSSF(:), DSSF(:)

contains

   subroutine CLEAR_INPUT_FIELDS ()
      if (allocated(DEPTH)) deallocate(DEPTH)
      if (allocated(WLEVL)) deallocate(WLEVL)
      if (allocated(FRIC )) deallocate(FRIC )
      if (allocated(UXB  )) deallocate(UXB  )
      if (allocated(UYB  )) deallocate(UYB  )
      if (allocated(WXI  )) deallocate(WXI  )
      if (allocated(WYI  )) deallocate(WYI  )
      if (allocated(ASTDF)) deallocate(ASTDF)
      if (allocated(MUDLF)) deallocate(MUDLF)
      if (allocated(NPLAF)) deallocate(NPLAF)
      if (allocated(TURBF)) deallocate(TURBF)
      if (allocated(AICEF)) deallocate(AICEF)
      if (allocated(HICEF)) deallocate(HICEF)
      if (allocated(HSSF )) deallocate(HSSF )
      if (allocated(TSSF )) deallocate(TSSF )
      if (allocated(DSSF )) deallocate(DSSF )
   end subroutine CLEAR_INPUT_FIELDS

   logical function INPUT_FIELDS_ARE_CLEAR ()
      INPUT_FIELDS_ARE_CLEAR = .not.allocated(DEPTH) .and. &
         .not.allocated(WLEVL) .and. .not.allocated(FRIC) .and. &
         .not.allocated(UXB) .and. .not.allocated(UYB) .and. &
         .not.allocated(WXI) .and. .not.allocated(WYI) .and. &
         .not.allocated(ASTDF) .and. .not.allocated(MUDLF) .and. &
         .not.allocated(NPLAF) .and. .not.allocated(TURBF) .and. &
         .not.allocated(AICEF) .and. .not.allocated(HICEF) .and. &
         .not.allocated(HSSF) .and. .not.allocated(TSSF) .and. &
         .not.allocated(DSSF)
   end function INPUT_FIELDS_ARE_CLEAR
end module swan_input_fields
