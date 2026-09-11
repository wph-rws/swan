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
!     Benaamde indexen voor PNUMS, in plaats van magic numbers.
!     De betekenis volgt de consument; twee invoerzoekwoorden (STOPC CURVAT
!     en ACCUR DHOVAL) schrijven dezelfde cel (15), idem 16. De waarden en
!     defaults zijn ongewijzigd; dit benoemt alleen de index.
   integer, parameter :: &
       PNUMS_DREL     = 1,  &  ! relatief accuraatheidscriterium Hs/Tm01 (DREL)
       PNUMS_DABS     = 2,  &  ! absoluut accuraatheidscriterium Hs (DABS)
       PNUMS_DTABS    = 3,  &  ! absoluut accuraatheidscriterium Tm01 (DTABS)
       PNUMS_NPNTS    = 4,  &  ! % natte punten waar accuraat bereikt (NPNTS)
       PNUMS_NOTUSED  = 5,  &  ! ongebruikt (op nul gezet in de defaults)
       PNUMS_CDD      = 6,  &  ! numerieke diffusie in theta (CDD)
       PNUMS_CSS      = 7,  &  ! numerieke diffusie in sigma (CSS)
       PNUMS_SCHEMEFR = 8,  &  ! expliciet/impliciet in frequentieruimte
       PNUMS_CEXPL    = 9,  &  ! diffusiecoefficient expliciete scheme
       PNUMS_EPS1     = 11, &  ! SIP-solver: epsilon-1
       PNUMS_EPS2     = 12, &  ! SIP-solver: stop-tolerantie eps2
       PNUMS_SIPPRN   = 13, &  ! SIP-solver: uitvoerniveau
       PNUMS_SIPMAX   = 14, &  ! SIP-solver: maximaal aantal iteraties
       PNUMS_TOLHS    = 15, &  ! toegestane fout Hs (STOPC CURVAT / ACCUR DHOVAL)
       PNUMS_TOLTM    = 16, &  ! toegestane fout Tm01 (STOPC CURVT / ACCUR DTOVAL)
       PNUMS_CDLIM    = 17, &  ! brekingsbeperking (CDLIM; <0 = geen)
       PNUMS_FROUDE   = 18, &  ! Froudegetal-limiet op stroomsnelheid
       PNUMS_CFLFR    = 19, &  ! CFL-criterium expliciet in frequentieruimte
       PNUMS_LIMGRW   = 20, &  ! limiter: max groei per spectrale bin (LIMITER)
       PNUMS_STOPTY   = 21, &  ! type stopteller (0/1/2)
       PNUMS_SUPEPS   = 23, &  ! setup-solver: stop-tolerantie
       PNUMS_SUPPRN   = 24, &  ! setup-solver: uitvoerniveau
       PNUMS_SUPMAX   = 25, &  ! setup-solver: maximaal aantal iteraties
       PNUMS_LCTFRQ   = 26, &  ! Ctheta-limiet: frequentiegrens
       PNUMS_LCTPP    = 27, &  ! Ctheta-limiet: piekperiode
       PNUMS_QBCOEF   = 28, &  ! limiter op actie: extra Qb-coefficient
       PNUMS_LCTON    = 29, &  ! Ctheta-limiet in frequentieband aan/uit
       PNUMS_ALFA     = 30, &  ! onder-relaxatiefactor (ALFA)
       PNUMS_GRADK    = 32, &  ! grad-Ctheta via golvengetal (1) of diepte (0)
       PNUMS_LCSON    = 33, &  ! Csigma-Courantlimiet aan/uit
       PNUMS_LCSAL    = 34, &  ! Csigma-Courantlimiet: alpha
       PNUMS_LCTCON   = 35, &  ! Ctheta-Courantlimiet aan/uit
       PNUMS_LCTAL    = 36, &  ! Ctheta-Courantlimiet: alpha
       PNUMS_BKDACC   = 37     ! BKD: accuraatheidsgrens waarboven MODGAM uit gaat

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
   public :: PNUMS_BKDACC

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
