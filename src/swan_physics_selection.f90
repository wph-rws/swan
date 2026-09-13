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
   public :: PHYSICS_OFF
   public :: IGEN_GEN1, IGEN_GEN2, IGEN_GEN3, IGEN_GEN4
   public :: IWIND_OFF, IWIND_GEN1, IWIND_GEN2, IWIND_KOMEN
   public :: IWIND_JANSSEN, IWIND_YAN, IWIND_BABANIN
   public :: IWCAP_OFF, IWCAP_KOMEN, IWCAP_JANSSEN, IWCAP_LHIG
   public :: IWCAP_BJ, IWCAP_KBJ, IWCAP_AB, IWCAP_BABANIN
   public :: IDRAG_WU, IDRAG_FIT, IDRAG_SWELL
   public :: IDRAG_HWANG, IDRAG_FAN, IDRAG_ECMWF
   public :: IQUAD_OFF, IQUAD_DIA, IQUAD_DIA_WAM, IQUAD_EXACT, IQUAD_MDIA
   public :: IQUAD_FULL, IQUAD_XNL_DEEP, IQUAD_XNL_DEEP_WAM, IQUAD_XNL_FINITE
   public :: IQUAD_VALUES
   public :: ITRIAD_OFF, ITRIAD_LTA, ITRIAD_SPB, ITRIAD_FTIM, ITRIAD_DCTA
   public :: ITRIAD_LTA_ORIGINAL, ITRIAD_VALUES
   public :: IBIPH_OFF, IBIPH_ELDEBERKY, IBIPH_SAPR, IBIPH_DEWIT
   public :: IBOT_OFF, IBOT_JONSWAP, IBOT_COLLINS, IBOT_MADSEN
   public :: IBOT_JONSWAP_VAR, IBOT_RIPPLES
   public :: ISURF_OFF, ISURF_CON, ISURF_VAR, ISURF_RUE
   public :: ISURF_TG, ISURF_BKD, ISURF_ASYM
   public :: IREFR_OFF, IREFR_NO_LIMITER, IREFR_LIMITER
   public :: ITFRE_OFF, ITFRE_ON, ICUR_OFF, ICUR_ON
   public :: IDIFFR_OFF, IDIFFR_ON, IQCM_OFF, IQCM_ON
   public :: IFRSRF_OFF, IFRSRF_ON, IDISRF_OFF, IDISRF_ON
   public :: IWCCUR_OFF, IWCCUR_ON, IMUD_OFF, IMUD_ON
   public :: IVEG_OFF, IVEG_ON, ITURBV_OFF, ITURBV_ON
   public :: IBRAG_OFF, IBRAG_ON
   public :: IICE_OFF, IICE_CICE, IICE_ADCICE, IICE_IC4M2
   public :: IICE_D15, IICE_M18, IICE_R21B

!     Dimensions of the coefficient arrays below.
   integer, parameter :: MWIND = 40, MWCAP = 15, MBOT = 10, MSURF = 20
   integer, parameter :: MTRIAD = 10, MQUAD = 10, MDIFFR = 10
   integer, parameter :: MTURBV = 5, MMUD = 10, MICE = 2, MSICE = 8
   integer, parameter :: MBRAG = 5, MSCAT = 10
   integer, parameter :: MSETUP = 2, MSHAPE = 5

!     Dimensions of the per-process output arrays the computation fills.
   integer, parameter :: MDISP = 8, MGENR = 1, MREDS = 4, MTRNP = 3

!     Named formulation choices. Every value below is the long-standing
!     selector encoding; the names come from the parser keywords in swanpre1
!     and the defaults in swanmain, so each writer site can state which
!     physics it selects instead of a bare number. Values and defaults are
!     unchanged: replacing a literal by its parameter cannot alter behaviour.
!     Off is uniformly zero for every process selector.
   integer, parameter :: PHYSICS_OFF = 0

!     Generation package as a whole (GEN1/GEN2/GEN3/GEN4 commands).
   integer, parameter :: IGEN_GEN1 = 1, IGEN_GEN2 = 2
   integer, parameter :: IGEN_GEN3 = 3, IGEN_GEN4 = 4

!     Wind growth formulation (WIND command and GEN expansion).
   integer, parameter :: IWIND_OFF = 0
   integer, parameter :: IWIND_GEN1 = 1, IWIND_GEN2 = 2
   integer, parameter :: IWIND_KOMEN = 3      ! GEN3 KOMEN (Komen et al. 1984)
   integer, parameter :: IWIND_JANSSEN = 4    ! GEN3 JANSsen
   integer, parameter :: IWIND_YAN = 5        ! GEN3 YAN / WESTHuysen (Yan 1987)
   integer, parameter :: IWIND_BABANIN = 8    ! GEN3 BABanin / ST6 (Rogers et al.)

!     Whitecapping formulation (WCAP command and GEN expansion).
   integer, parameter :: IWCAP_OFF = 0
   integer, parameter :: IWCAP_KOMEN = 1      ! KOMen (Komen et al. 1984)
   integer, parameter :: IWCAP_JANSSEN = 2    ! JANSsen (1989, 1991)
   integer, parameter :: IWCAP_LHIG = 3       ! Longuett-HIGgins
   integer, parameter :: IWCAP_BJ = 4         ! Battjes/Janssen
   integer, parameter :: IWCAP_KBJ = 5        ! Komen + BJ combination
   integer, parameter :: IWCAP_AB = 7         ! Alves and Banner (2003)
   integer, parameter :: IWCAP_BABANIN = 8    ! Rogers/Babanin ST6

!     Wind drag formulation (DRAG suffix of GEN3, HWANG/FAN/ECMWF of ST6).
   integer, parameter :: IDRAG_WU = 1         ! WU (Wu 1982)
   integer, parameter :: IDRAG_FIT = 2        ! FIT (Zijlema et al. 2012)
   integer, parameter :: IDRAG_SWELL = 3      ! SWELL
   integer, parameter :: IDRAG_HWANG = 4      ! HWANG
   integer, parameter :: IDRAG_FAN = 5        ! FAN
   integer, parameter :: IDRAG_ECMWF = 6      ! ECMWF

!     Quadruplet interactions (QUAD command: user integer, see manual).
   integer, parameter :: IQUAD_OFF = 0
   integer, parameter :: IQUAD_DIA = 1        ! DIA deep water
   integer, parameter :: IQUAD_DIA_WAM = 2    ! DIA with WAM depth scaling
   integer, parameter :: IQUAD_EXACT = 3      ! direct finite depth
   integer, parameter :: IQUAD_MDIA = 4       ! MDIA
   integer, parameter :: IQUAD_FULL = 8       ! fully explicit, full circle
!     The XNL suite of Van Vledder; SWINTFXNL maps these onto its own iq_quad
!     by subtracting 50.
   integer, parameter :: IQUAD_XNL_DEEP = 51     ! deep water transfer
   integer, parameter :: IQUAD_XNL_DEEP_WAM = 52 ! deep water, WAM depth scaling
   integer, parameter :: IQUAD_XNL_FINITE = 53   ! finite depth transfer

!     Every value the source-term dispatcher in SWCOMP actually handles. The
!     QUAD command takes a bare integer, so an unlisted one is neither a
!     formulation nor an error unless the parser says so: IQUAD .GE. 1 still
!     opens the quadruplet bookkeeping (MEMNL4, the limiter, the convergence
!     bookkeeping) while no branch computes a source term, and the run reports
!     a quadruplet formulation it never applied.
   integer, parameter :: IQUAD_VALUES(*) = &
      [IQUAD_OFF, IQUAD_DIA, IQUAD_DIA_WAM, IQUAD_EXACT, IQUAD_MDIA, &
       IQUAD_FULL, IQUAD_XNL_DEEP, IQUAD_XNL_DEEP_WAM, IQUAD_XNL_FINITE]

!     Triad interactions (TRIAD command).
   integer, parameter :: ITRIAD_OFF = 0
   integer, parameter :: ITRIAD_LTA = 1       ! LTA
   integer, parameter :: ITRIAD_SPB = 2       ! SPB
   integer, parameter :: ITRIAD_FTIM = 3      ! FTIM
   integer, parameter :: ITRIAD_DCTA = 5      ! DCTA (default)
!     The LTA as it stood before release 41.01, reachable only as the bare
!     integer 11: it drops the shoaling factor FT and takes the group velocity
!     from the local spectrum rather than the offshore one (see SWLTA). It is
!     the operational so-rp choice, so it is a formulation in its own right
!     and not a spelling variant of ITRIAD_LTA.
   integer, parameter :: ITRIAD_LTA_ORIGINAL = 11

!     Every value the triad dispatcher handles; see IQUAD_VALUES above for why
!     an unlisted one has to be rejected rather than ignored.
   integer, parameter :: ITRIAD_VALUES(*) = &
      [ITRIAD_OFF, ITRIAD_LTA, ITRIAD_SPB, ITRIAD_FTIM, ITRIAD_DCTA, &
       ITRIAD_LTA_ORIGINAL]

!     Biphase formulation (BIPHASE suffix of TRIAD).
   integer, parameter :: IBIPH_OFF = 0
   integer, parameter :: IBIPH_ELDEBERKY = 1  ! ELDeberky (default, URCRIT)
   integer, parameter :: IBIPH_SAPR = 2       ! SAPRykina et al. (2017)
   integer, parameter :: IBIPH_DEWIT = 3      ! WIT / DEWIT

!     Bottom friction (FRICTION command).
   integer, parameter :: IBOT_OFF = 0
   integer, parameter :: IBOT_JONSWAP = 1     ! JONswap, constant
   integer, parameter :: IBOT_COLLINS = 2     ! COLLins
   integer, parameter :: IBOT_MADSEN = 3      ! MADsen
   integer, parameter :: IBOT_JONSWAP_VAR = 4 ! JONswap, VARiable
   integer, parameter :: IBOT_RIPPLES = 5     ! RIPples

!     Depth-induced breaking (BREAK command).
   integer, parameter :: ISURF_OFF = 0
   integer, parameter :: ISURF_CON = 1        ! CONstant (gamma 0.73)
   integer, parameter :: ISURF_VAR = 2        ! VARiable / NELder
   integer, parameter :: ISURF_RUE = 3        ! RUEssink
   integer, parameter :: ISURF_TG = 4         ! Thornton-Guza
   integer, parameter :: ISURF_BKD = 6        ! Beta-kd
   integer, parameter :: ISURF_ASYM = 7       ! ASYMetric

!     Refraction treatment: -1 limits Ctheta, 1 no limiter (default), 0 off.
   integer, parameter :: IREFR_OFF = 0
   integer, parameter :: IREFR_NO_LIMITER = 1
   integer, parameter :: IREFR_LIMITER = -1

!     Binary process switches (0 off, 1 on).
   integer, parameter :: ITFRE_OFF = 0, ITFRE_ON = 1
   integer, parameter :: ICUR_OFF = 0, ICUR_ON = 1
   integer, parameter :: IDIFFR_OFF = 0, IDIFFR_ON = 1
   integer, parameter :: IQCM_OFF = 0, IQCM_ON = 1
   integer, parameter :: IFRSRF_OFF = 0, IFRSRF_ON = 1
   integer, parameter :: IDISRF_OFF = 0, IDISRF_ON = 1
   integer, parameter :: IWCCUR_OFF = 0, IWCCUR_ON = 1
   integer, parameter :: IMUD_OFF = 0, IMUD_ON = 1
   integer, parameter :: IVEG_OFF = 0, IVEG_ON = 1
   integer, parameter :: ITURBV_OFF = 0, ITURBV_ON = 1
   integer, parameter :: IBRAG_OFF = 0, IBRAG_ON = 1

!     Ice formulations (CICE / IC4M2 / D15 / M18 / R21B commands).
   integer, parameter :: IICE_OFF = 0
   integer, parameter :: IICE_CICE = 1
   integer, parameter :: IICE_ADCICE = 2
   integer, parameter :: IICE_IC4M2 = 3       ! R19 Rogers (2019)
   integer, parameter :: IICE_D15 = 4         ! D15 Doble et al. (2015)
   integer, parameter :: IICE_M18 = 5         ! M18 Meylan et al. (2018)
   integer, parameter :: IICE_R21B = 6        ! R21B Rogers et al. (2021)

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
