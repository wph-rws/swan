module swan_output_variables
!
!     The table describing every quantity SWAN can output: its command keyword,
!     names, unit, type, limits and exception value.
!
!     These ten arrays were spread over two declaration blocks of SWCOMM1, a
!     module that also holds physics settings, grid parameters and output
!     quadrature geometry. They form one coherent table with one meaning, so
!     they belong together and apart.
!
!     The table is filled once in SWINIT and refined by a few parsed commands;
!     everything else only reads it. Moving it here does not change that
!     lifetime -- it names the concern and takes ten arrays out of the shared
!     grab bag, so a consumer that needs the table no longer imports the
!     physics settings with it.
!
   implicit none(type, external)
   private

   public :: NMOVAR
   public :: OVKEYW, OVSNAM, OVLNAM, OVUNIT, OVSVTY
   public :: OVLLIM, OVULIM, OVLEXP, OVHEXP, OVEXCV

!     NMOVAR : number of output variables the table describes
   integer, parameter :: NMOVAR = 171

!     OVKEYW : keyword used in the SWAN command that selects the quantity
!     OVSNAM : short name, used as a column heading
!     OVLNAM : long name
!     OVUNIT : unit name
   character(len=8)  :: OVKEYW(NMOVAR)
   character(len=6)  :: OVSNAM(NMOVAR)
   character(len=40) :: OVLNAM(NMOVAR)
   character(len=16) :: OVUNIT(NMOVAR)

!     OVSVTY : type of the quantity (scalar, vector, ...)
   integer :: OVSVTY(NMOVAR)

!     OVLLIM, OVULIM : lower and upper limit of an acceptable value
!     OVLEXP, OVHEXP : lowest and highest expected value
!     OVEXCV         : exception value written where the quantity is undefined
   real :: OVLLIM(NMOVAR), OVULIM(NMOVAR)
   real :: OVLEXP(NMOVAR), OVHEXP(NMOVAR)
   real :: OVEXCV(NMOVAR)
end module swan_output_variables
