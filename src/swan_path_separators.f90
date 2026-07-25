module swan_path_separators
!
!     The two directory separation characters, read from the initialisation file
!     at startup so a deck written on one platform can name files for another.
!
!     File opening replaces every DIRCH1 in a name by DIRCH2. They sat with the
!     project identification in OCPCOMM2, but they describe file naming, not the
!     run.
!
   implicit none(type, external)
   private

   public :: DIRCH1, DIRCH2

!     DIRCH1 : separator as it appears in the input file
!     DIRCH2 : separator it is replaced by
   character(len=1) :: DIRCH1, DIRCH2
end module swan_path_separators
