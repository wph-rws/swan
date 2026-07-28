module swan_physics_selection
!
!     Which physical processes the run includes, and how each of them is tuned.
!
!     Every process SWAN can model has a selector saying which formulation to
!     use -- or zero for "leave it out" -- and an array of coefficients for
!     that formulation. The two always travel together: PWIND means nothing
!     without IWIND to say which of its entries apply.
!
!     The GEN, WCAP, BREAK, FRICTION, TRIAD, QUAD, DIFFRAC, VEGETATION, MUD,
!     ICE, BRAGG and SCAT commands write these; the source-term routines read
!     them. Nothing here changes during the computation.
!
!     This is what is left of SWCOMM3 once the grids, the COMPDA layout, the
!     physical constants and the numerical control have their own modules --
!     and it is the part that genuinely is one subject.
!
   implicit none(type, external)
   private

   public :: IGEN, IWIND, IWCAP, IWCCUR, IDRAG
   public :: IBOT, ISURF, IFRSRF, IDISRF
   public :: ITRIAD, IBIPH, IQUAD, ITFRE
   public :: IREFR, IDIFFR, ICUR
   public :: IVEG, ITURBV, IMUD, IICE, IBRAG, IQCM
   public :: PWIND, PWCAP, PBOT, PSURF, PTRIAD, PQUAD
   public :: PDIFFR, PTURBV, PMUD, PICE, PSICE, PBRAG, PSCAT
   public :: PSETUP, PSHAPE
   public :: MWIND, MWCAP, MBOT, MSURF, MTRIAD, MQUAD
   public :: MDIFFR, MTURBV, MMUD, MICE, MSICE, MBRAG, MSCAT
   public :: MSETUP, MSHAPE, MDISP, MGENR, MREDS, MTRNP
   public :: U10, WDIC, WDIP, WNDSCL, TRUE_U10, SIGMAG, FPI
   public :: A1SDS, A2SDS, P1SDS, P2SDS, CDSV, CDFAC, B1Z
   public :: UPWARDS, VECTOR_TAU, FESWELL, ROGERS, ZIEGER, ARDHUIN
   public :: MODGAM, OFFSRC

!     Dimensions of the coefficient arrays below.
   integer, parameter :: MWIND = 40, MWCAP = 15, MBOT = 10, MSURF = 20
   integer, parameter :: MTRIAD = 10, MQUAD = 10, MDIFFR = 10
   integer, parameter :: MTURBV = 5, MMUD = 10, MICE = 2, MSICE = 8
   integer, parameter :: MBRAG = 5, MSCAT = 10
   integer, parameter :: MSETUP = 2, MSHAPE = 5

!     Dimensions of the per-process output arrays the computation fills.
   integer, parameter :: MDISP = 8, MGENR = 1, MREDS = 4, MTRNP = 3

!     Which formulation is used for each process; 0 means the process is off.
!     IGEN is the generation package as a whole (GEN1/GEN2/GEN3), the rest
!     select within it.
   integer :: IGEN, IWIND, IWCAP, IWCCUR, IDRAG
   integer :: IBOT, ISURF, IFRSRF, IDISRF
   integer :: ITRIAD, IBIPH, IQUAD, ITFRE
   integer :: IREFR, IDIFFR, ICUR
   integer :: IVEG, ITURBV, IMUD, IICE, IBRAG, IQCM

!     The coefficients belonging to those selectors.
   real :: PWIND(MWIND), PWCAP(MWCAP), PBOT(MBOT), PSURF(MSURF)
   real :: PTRIAD(MTRIAD), PQUAD(MQUAD), PDIFFR(MDIFFR)
   real :: PTURBV(MTURBV), PMUD(MMUD), PICE(MICE), PSICE(MSICE)
   real :: PBRAG(MBRAG), PSCAT(MSCAT)
   real :: PSETUP(MSETUP), PSHAPE(MSHAPE)

!     The wind as the source terms see it: speed, direction and the scaling
!     and spreading applied to it.
   real :: U10, WDIC, WDIP, WNDSCL, SIGMAG, FPI
   logical :: TRUE_U10

!     Coefficients and switches of the Babanin/Rogers/Zieger source terms,
!     which were added later and never got an array of their own.
   real :: A1SDS, A2SDS, P1SDS, P2SDS, CDSV, CDFAC, B1Z
   real :: FESWELL
   logical :: UPWARDS, VECTOR_TAU
   logical :: ROGERS, ZIEGER, ARDHUIN

!     MODGAM : whether the breaker index is varied rather than held constant
!     OFFSRC : whether the source terms are switched off entirely
   logical, save :: MODGAM = .TRUE.
   logical :: OFFSRC
end module swan_physics_selection
