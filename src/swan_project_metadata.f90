module swan_project_metadata
!
!     Identification of the run, as entered with the PROJECT command and printed
!     in every output heading.
!
!     Filled once: SWINIT sets blanks, the parsed PROJECT command supplies the
!     real values, and SWINIT renders the version string. Everything else only
!     reads it, so this is run-scope read-only data.
!
   implicit none(type, external)
   private

   public :: PROJID, PROJNR, PROJT1, PROJT2, PROJT3, INST, VERTXT

!     PROJID : project name
!     PROJNR : project number
   character(len=16) :: PROJID
   character(len=4)  :: PROJNR

!     PROJT1, PROJT2, PROJT3 : three lines of project title
   character(len=72) :: PROJT1, PROJT2, PROJT3

!     INST   : name of the institute, read from the initialisation file
!     VERTXT : SWAN version as text, rendered once from the version number
   character(len=40) :: INST
   character(len=20) :: VERTXT
end module swan_project_metadata
