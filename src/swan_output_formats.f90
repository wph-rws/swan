module swan_output_formats
!
!     How numbers and headings are written in block, table and spectral output.
!
!     Seven edit descriptors and field widths, all with a default that the
!     OUTPUT OPTIONS command may override. They are read by the writers and by
!     nothing else, but they lived in OUTP_DATA next to the request
!     administration that ten files need.
!
   implicit none(type, external)
   private

   public :: OUT_COMMENT
   public :: FLT_BLOCK, FLT_TABLE, FIX_SPEC
   public :: FLD_TABLE, DEC_BLOCK, DEC_SPEC

!     OUT_COMMENT : the character a heading line starts with
   character(len=1) :: OUT_COMMENT = '%'

!     FLT_BLOCK : floating-point format for block output
!     FLT_TABLE : floating-point format for table output
!     FIX_SPEC  : fixed-point format for spectral output
   character(len=40) :: FLT_BLOCK = '(6E12.4)'
   character(len=40) :: FLT_TABLE = '(E11.4)'
   character(len=40) :: FIX_SPEC = '(200(1X,I4))'

!     FLD_TABLE : field width for fixed-point table output
!     DEC_BLOCK : number of decimals for fixed-point block output
!     DEC_SPEC  : number of decimals for spectral output
   integer :: FLD_TABLE = 12
   integer :: DEC_BLOCK = 4
   integer :: DEC_SPEC = 4
end module swan_output_formats
