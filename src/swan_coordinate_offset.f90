module swan_coordinate_offset
!
!     The translation between the user's coordinate system and SWAN's internal
!     one.
!
!     SWAN subtracts a fixed offset from every x and y it reads so that the
!     internal coordinates stay small enough for single precision, and adds it
!     back on output. Twenty-four of the thirty-eight files that imported
!     SWCOMM2 needed nothing from it but this offset -- and got the eighteen
!     input grids and the field-file bookkeeping with it.
!
!     Set once, from the first coordinates read; LXOFFS records that it has
!     been.
!
   implicit none(type, external)
   private

   public :: XOFFS, YOFFS, LXOFFS

!     XOFFS, YOFFS : offset from user to internal coordinates
   real :: XOFFS, YOFFS

!     LXOFFS : whether the offset has been established yet
   logical :: LXOFFS
end module swan_coordinate_offset
