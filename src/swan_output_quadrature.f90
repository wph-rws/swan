module swan_output_quadrature
!
!     Geometry of the output frame: the rotated, translated grid that BLOCK and
!     TABLE requests are evaluated on.
!
!     Ten quantities describing one frame sat between the physics settings in
!     SWCOMM1. Four companions -- the frame's mesh counts and spacings -- were
!     in OCPCOMM3 and are already local to the routine that builds them, so this
!     module completes that separation for the parts that genuinely cross file
!     boundaries.
!
!     Written when an output frame is defined and read while writing the
!     requested quantities; never touched during the computation itself.
!
   implicit none(type, external)
   private

   public :: XPQ, YPQ, ALPQ, COSPQ, SINPQ
   public :: XQLEN, YQLEN, ALCQ, COSCQ, SINCQ

!     XPQ, YPQ   : origin of the output frame in problem coordinates
!     ALPQ       : direction of the frame's x-axis
!     COSPQ,SINPQ: cosine and sine of ALPQ
   real :: XPQ, YPQ
   real :: ALPQ, COSPQ, SINPQ

!     XQLEN,YQLEN: length of the frame in each direction
!     ALCQ       : direction of the frame relative to the computational grid
!     COSCQ,SINCQ: cosine and sine of ALCQ
   real :: XQLEN, YQLEN
   real :: ALCQ, COSCQ, SINCQ
end module swan_output_quadrature
