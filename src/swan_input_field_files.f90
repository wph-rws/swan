module swan_input_field_files
!
!     Where each input field is being read from, and how far the reading has
!     got.
!
!     One row per input grid, same numbering as swan_input_grids: the unit the
!     field is read from, how the file is laid out, and the time window it
!     covers. Only three files touch this bookkeeping, and until now all
!     thirty-eight importers of SWCOMM2 carried it.
!
!     This is the one part of the old module that is written during a run
!     rather than only at input time: IFLTIM advances as a nonstationary field
!     is read forward.
!
!     NUMGRD comes from swan_input_grids because these arrays are indexed by
!     the same grid number; a second copy of the bound would be a second thing
!     to keep in step.
   use swan_input_grids, only: NUMGRD

   implicit none(type, external)
   private

   public :: IFLDYN, IFLIDL, IFLIFM, IFLNHF, IFLNHD, IFLNDS, IFLNDF
   public :: IFLBEG, IFLINT, IFLEND, IFLTIM
   public :: IFLFAC, IFLFRM
   public :: LWDATE

!     IFLDYN : 0=data is stationary, 1=nonstationary
   integer :: IFLDYN(NUMGRD)

!     IFLIDL : lay-out of the values in the file
!     IFLIFM : format identifier
!     IFLNHF : number of heading lines per file
!     IFLNHD : number of heading lines per input field
!     IFLNDS : unit number of the data file
!     IFLNDF : unit number of the namelist file
   integer :: IFLIDL(NUMGRD), IFLIFM(NUMGRD), IFLNHF(NUMGRD)
   integer :: IFLNHD(NUMGRD), IFLNDS(NUMGRD), IFLNDF(NUMGRD)

!     IFLBEG, IFLEND : first and last time the file holds data for
!     IFLINT         : time interval between fields on the file
!     IFLTIM         : time of the field read last
   real(kind=kind(0.0d0)) :: IFLBEG(NUMGRD), IFLINT(NUMGRD), IFLEND(NUMGRD)
   real(kind=kind(0.0d0)) :: IFLTIM(NUMGRD)

!     IFLFAC : factor every value read is multiplied by
!     IFLFRM : format string
   real :: IFLFAC(NUMGRD)
   character(len=40) :: IFLFRM(NUMGRD)

!     LWDATE : length of the date-time string in a WAM file
   integer :: LWDATE
end module swan_input_field_files
