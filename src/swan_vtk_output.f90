module swan_vtk_output
!
!     The bookkeeping and boilerplate for VTK and PVD output.
!
!     A VTK series is a directory of files, one per time step, tied together by
!     a PVD collection file. That needs a unit number and a step counter per
!     output request, a directory name, and the fixed XML lines the files open
!     and close with.
!
!     Eleven symbols that only the VTK writers care about, sitting in OUTP_DATA
!     among the output-request structures that every output path uses.
!
   use swan_io_limits, only: LENFNM
   use OUTP_DATA, only: MAX_OUTP_REQ

   implicit none(type, external)
   private

   public :: UPVDF, NTVTK, VTKDIR, VTKLINE
   public :: XMLLIN1, XMLLIN2, XMLLIN3
   public :: PVDLIN1, PVDLIN2, PVDLIN3, PVDLIN4

!     These are indexed by output request, so the bound is the one OUTP_DATA
!     already defines rather than a second copy of 250.
!     UPVDF  : unit number of the PVD file per output request
!     NTVTK  : time-step counter per request; -1 until the first step is written
!     VTKDIR : directory holding the series of time-varying VTK files
   integer :: UPVDF(1:MAX_OUTP_REQ)
   integer, save :: NTVTK(1:MAX_OUTP_REQ) = -1
   character(len=LENFNM) :: VTKDIR(1:MAX_OUTP_REQ)

!     VTKLINE : buffer holding one line of VTK XML while it is assembled
   character(len=1024) :: VTKLINE

!     The XML header lines every VTK file opens with.
   character(len=25) :: XMLLIN1 = '<?xml version="1.0"?>'
   character(len= 5) :: XMLLIN2 = '<!--'
   character(len= 5) :: XMLLIN3 = '-->'

!     The fixed lines of a PVD collection file.
   character(len=80) :: PVDLIN1 = '<VTKFile type="Collection" '//&
   &'version="0.1" byte_order="LittleEndian">'
   character(len=15) :: PVDLIN2 = '  <Collection>'
   character(len=15) :: PVDLIN3 = '  </Collection>'
   character(len=10) :: PVDLIN4 = '</VTKFile>'
end module swan_vtk_output
