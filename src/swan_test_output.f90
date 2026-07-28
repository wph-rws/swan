module swan_test_output
!
!     Which test output SWAN writes, where it writes it, and whether the point
!     currently being computed is one of the test points.
!
!     These twelve came out of SWCOMM4, which also held the propagation scheme
!     and the shape of the earth. Test output is a concern of its own: it is
!     configured by the TEST, INTE, OUTE and COTES commands and read by the
!     computational routines only to decide whether to print.
!
!     Two of them are per-thread state. IPTST and TESTFL say which test point
!     the calling thread is on, so every thread needs its own copy; they are in
!     the thread-state manifest and the OpenMP gate covers them.
!
   implicit none(type, external)
   private

   public :: ICOTES, INTES, IOUTES
   public :: IPTST, TESTFL
   public :: IFPAR, IFS1D, IFS2D
   public :: LXDMP, LYDMP
   public :: NPTST, NPTSTA
   public :: MTSVAR

!     ICOTES : minimum value for ITEST, set by the undocumented COTES command
!     INTES  : testing parameter, set by the undocumented INTE command
!     IOUTES : minimum value for ITEST, set by the undocumented OUTE command
   integer :: ICOTES, INTES, IOUTES

!     IPTST  : sequence number of the test point being computed
!     TESTFL : whether test output must be written for the current point
   integer :: IPTST
   logical :: TESTFL
!$OMP THREADPRIVATE(IPTST,TESTFL)

!     IFPAR : unit number for test output of parameters in test points
!     IFS1D : unit number for test output of 1D source-term spectra
!     IFS2D : unit number for test output of 2D source-term spectra
!             all three are made non-zero by FOR when the file is opened
   integer :: IFPAR, IFS1D, IFS2D

!     LXDMP, LYDMP : grid counters of a test point, set by TEST ... POI
   integer :: LXDMP, LYDMP

!     NPTST  : number of test points, set by the TEST command
!     NPTSTA : MAX(1,NPTST); the first dimension the test arrays are sized on,
!              so that they stay allocatable when there are no test points
   integer :: NPTST, NPTSTA

!     MTSVAR : number of quantities held per test point in the array TESTDA,
!              which is dimensioned (MDC,MSC,NPTSTA,MTSVAR). It documented the
!              TESTDA layout from inside SWCOMM3, next to the COMPDA layout it
!              has nothing to do with.
   integer :: MTSVAR

!     XYTST, the test-point indices, is deliberately not here. It lives in
!     M_GENARR with the other arrays the run owns, and the routines that use it
!     take it as a dummy argument -- which is the pattern this migration is
!     working towards. Exporting it here as well would make the name ambiguous
!     in every routine that has both that argument and NPTST.
end module swan_test_output
