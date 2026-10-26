module swan_numerics
!
!     How the computation is driven: stationary or not, how many iterations it
!     may take, when it is deemed converged, and what it is allowed to update.
!
!     This is the run's control panel rather than its physics. It sat in
!     SWCOMM3 among the source-term coefficients, so a routine that only wanted
!     to know whether the run is nonstationary imported the whitecapping
!     tuning with it.
!
!     PNUMS is the array the numerical settings are actually stored in; the
!     named entries around it select which of them apply.
!
   implicit none(type, external)
   private

!     Dimensions of the two arrays below. They were parameters in SWCOMM3 and
!     are used nowhere else, so they travel with the arrays they size rather
!     than being written out as literals here.
   integer, parameter :: MNUMS = 40, MSPPAR = 5

!     PNUMS : the numerical parameters, set by the NUMERIC command
   real :: PNUMS(MNUMS)
!
!     Named indices for PNUMS, instead of magic numbers.
!     The meaning follows the consumer; two input keywords (STOPC CURVAT
!     and ACCUR DHOVAL) write the same cell (15), likewise 16. The values and
!     defaults are unchanged; this only names the index.
   integer, parameter :: &
       PNUMS_DREL     = 1,  &  ! relative accuracy criterion Hs/Tm01 (DREL)
       PNUMS_DABS     = 2,  &  ! absolute accuracy criterion Hs (DABS)
       PNUMS_DTABS    = 3,  &  ! absolute accuracy criterion Tm01 (DTABS)
       PNUMS_NPNTS    = 4,  &  ! % wet points where accuracy is reached (NPNTS)
       PNUMS_NOTUSED  = 5,  &  ! unused (set to zero in the defaults)
       PNUMS_CDD      = 6,  &  ! numerical diffusion in theta (CDD)
       PNUMS_CSS      = 7,  &  ! numerical diffusion in sigma (CSS)
       PNUMS_SCHEMEFR = 8,  &  ! explicit/implicit in frequency space
       PNUMS_CEXPL    = 9,  &  ! diffusion coefficient of the explicit scheme
       PNUMS_EPS1     = 11, &  ! SIP solver: epsilon-1
       PNUMS_EPS2     = 12, &  ! SIP solver: stopping tolerance eps2
       PNUMS_SIPPRN   = 13, &  ! SIP solver: output level
       PNUMS_SIPMAX   = 14, &  ! SIP solver: maximum number of iterations
       PNUMS_TOLHS    = 15, &  ! allowed error Hs (STOPC CURVAT / ACCUR DHOVAL)
       PNUMS_TOLTM    = 16, &  ! allowed error Tm01 (STOPC CURVT / ACCUR DTOVAL)
       PNUMS_CDLIM    = 17, &  ! refraction limit (CDLIM; <0 = none)
       PNUMS_FROUDE   = 18, &  ! Froude number limit on current velocity
       PNUMS_CFLFR    = 19, &  ! CFL criterion, explicit in frequency space
       PNUMS_LIMGRW   = 20, &  ! limiter: max growth per spectral bin (LIMITER)
       PNUMS_STOPTY   = 21, &  ! type of stopping counter (0/1/2)
       PNUMS_SUPEPS   = 23, &  ! setup solver: stopping tolerance
       PNUMS_SUPPRN   = 24, &  ! setup solver: output level
       PNUMS_SUPMAX   = 25, &  ! setup solver: maximum number of iterations
       PNUMS_LCTFRQ   = 26, &  ! Ctheta limit: frequency bound
       PNUMS_LCTPP    = 27, &  ! Ctheta limit: peak period
       PNUMS_QBCOEF   = 28, &  ! limiter on action: extra Qb coefficient
       PNUMS_LCTON    = 29, &  ! Ctheta limit in frequency band on/off
       PNUMS_ALFA     = 30, &  ! under-relaxation factor (ALFA)
       PNUMS_GRADK    = 32, &  ! grad-Ctheta from wave number (1) or depth (0)
       PNUMS_LCSON    = 33, &  ! Csigma Courant limit on/off
       PNUMS_LCSAL    = 34, &  ! Csigma Courant limit: alpha
       PNUMS_LCTCON   = 35, &  ! Ctheta Courant limit on/off
       PNUMS_LCTAL    = 36, &  ! Ctheta Courant limit: alpha
       PNUMS_BKDACC   = 37, &  ! BKD: accuracy bound above which MODGAM is switched off
       PNUMS_RESTOL   = 38     ! balance residual: <0 off, 0 measure only, >0 stopping threshold

   public :: PNUMS, NSTATC, NSTATM
   public :: ITERMX, MXITST, MXITNS
   public :: ACUPDA, BRESCL, BNDCHK
   public :: NCOMPT, RCOMPT, RUNMADE
   public :: LADDS, LSETUP, LSRFB, LSPNAR
   public :: DSHAPE, FSHAPE, SPPARM, RDCOEF
   public :: NUMOBS
   public :: MNUMS, MSPPAR
   public :: PNUMS_DREL, PNUMS_DABS, PNUMS_DTABS, PNUMS_NPNTS
   public :: PNUMS_NOTUSED, PNUMS_CDD, PNUMS_CSS, PNUMS_SCHEMEFR
   public :: PNUMS_CEXPL, PNUMS_EPS1, PNUMS_EPS2, PNUMS_SIPPRN
   public :: PNUMS_SIPMAX, PNUMS_TOLHS, PNUMS_TOLTM, PNUMS_CDLIM
   public :: PNUMS_FROUDE, PNUMS_CFLFR, PNUMS_LIMGRW, PNUMS_STOPTY
   public :: PNUMS_SUPEPS, PNUMS_SUPPRN, PNUMS_SUPMAX, PNUMS_LCTFRQ
   public :: PNUMS_LCTPP, PNUMS_QBCOEF, PNUMS_LCTON, PNUMS_ALFA
   public :: PNUMS_GRADK
   public :: PNUMS_LCSON, PNUMS_LCSAL, PNUMS_LCTCON, PNUMS_LCTAL
   public :: PNUMS_BKDACC, PNUMS_RESTOL

!     NSTATC : whether the computation itself is nonstationary
!     NSTATM : whether the run has a time frame at all
   integer :: NSTATC, NSTATM

!     ITERMX : number of iterations made
!     MXITST : maximum number of iterations in stationary mode
!     MXITNS : maximum number of iterations in nonstationary mode
   integer :: ITERMX, MXITST, MXITNS

!     ACUPDA : whether action densities are updated during the computation
!     BRESCL : whether rescaling is applied
!     BNDCHK : whether the computed Hs on a boundary is compared with the
!              value the boundary condition asked for
   logical :: ACUPDA, BRESCL, BNDCHK

!     NCOMPT : number of COMPUTE commands stored
!     RCOMPT : their arguments, kept until the run executes them
!     RUNMADE: whether a computation has been carried out already
   integer :: NCOMPT
   real(kind=kind(0.0d0)) :: RCOMPT(300,5)
   logical, save :: RUNMADE = .FALSE.

!     LADDS  : whether the source terms are added rather than replaced
!     LSETUP : whether wave-induced setup is computed
!     LSRFB  : whether surf breaking is limited by the bottom slope
!     LSPNAR : whether the directional space is narrow
   logical :: LADDS
   integer :: LSETUP
   logical :: LSRFB, LSPNAR

!     DSHAPE, FSHAPE : which directional and frequency distribution a
!              parametric boundary spectrum is built from
!     SPPARM : the parameters of that spectrum
!     RDCOEF : reflection coefficient used at obstacles
   integer :: DSHAPE, FSHAPE
   real :: SPPARM(MSPPAR), RDCOEF

!     NUMOBS : number of obstacles
   integer :: NUMOBS
end module swan_numerics
