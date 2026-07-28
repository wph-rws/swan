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

   public :: PNUMS, NSTATC, NSTATM
   public :: ITERMX, MXITST, MXITNS
   public :: ACUPDA, BRESCL, BNDCHK
   public :: NCOMPT, RCOMPT, RUNMADE
   public :: LADDS, LSETUP, LSRFB, LSPNAR
   public :: DSHAPE, FSHAPE, SPPARM, RDCOEF
   public :: NUMOBS
   public :: MNUMS, MSPPAR

!     Dimensions of the two arrays below. They were parameters in SWCOMM3 and
!     are used nowhere else, so they travel with the arrays they size rather
!     than being written out as literals here.
   integer, parameter :: MNUMS = 40, MSPPAR = 5

!     PNUMS : the numerical parameters, set by the NUMERIC command
   real :: PNUMS(MNUMS)

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
