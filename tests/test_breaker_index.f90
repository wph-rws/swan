program test_breaker_index
!  BRKPAR moet voor elke brekingsformulering een eindige, fysisch zinnige
!  brekingsindex opleveren -- ook voor een roosterpunt zonder energie.
!
!  Zo'n punt is niet exotisch: bij de eerste iteratie is het hele veld nul,
!  en in de luwte van een obstakel of in een afgesloten hoek van het domein
!  blijft het dat. De Saprykina-tak (ISURF_ASYM) zoekt daar een piekfrequentie
!  die niet bestaat. De "geen piek gevonden"-sentinel is ISIGM = -1, dus een
!  toets op ISIGM .EQ. 0 vuurt nooit en de tak viel door naar een vergelijking
!  van E1 en E2 die op dat pad nooit zijn toegekend.
!
!  De testen hieronder controleren de invariant, niet een opgeslagen getal:
!  zonder golven valt er niets te breken, dus de index moet op de
!  spilling-constante PSURF(4) blijven staan. Nul zou hier fataal zijn -- dat
!  is exact de val van BF-17: HM = GAMBR*DEP2 wordt dan nul, BB in SSURF
!  oneindig, en de verzadigde dissipatietak dissipeert juist alles.
   use swan_dissipation, only: BRKPAR
   use swan_physics_selection, only: ISURF, PSURF, IDISRF, &
                                     ISURF_CON, ISURF_VAR, ISURF_RUE, &
                                     ISURF_TG, ISURF_BKD, ISURF_ASYM
   use swan_spectral_grid, only: MSC, MDC, DDIR, FRINTF, spectral_window_t
   use swan_computational_grid, only: MCGRD
   use swan_stencil, only: MICMAX
   use swan_math_constants, only: PI
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
   implicit none

   integer, parameter :: NS = 20, ND = 8, NP = 3

   real :: spcsig(NS), ecos(ND), esin(ND), dep2(NP), botlv(NP)
   real :: ac2(ND, NS, NP), kwave(NS, MICMAX)
   real :: rdx(2), rdy(2)
   type(spectral_window_t) :: window
   integer :: id, is

   call configure_grid

   call test_empty_spectrum_keeps_a_finite_index
   call test_empty_spectrum_matches_the_spilling_constant
   call test_energy_bearing_spectrum_still_selects_a_branch

   print *, 'breaker index stays finite and physical without energy'

contains

   subroutine configure_grid
      MSC = NS
      MDC = ND
      MCGRD = NP
      DDIR = 2.0 * PI / real(ND)
      !  Logaritmisch frequentierooster met de gebruikelijke aangroeifactor
      !  1.1; FRINTF is de logaritmische stap die de tweede harmonische in
      !  BRKPAR lokaliseert.
      FRINTF = log(1.1)
      do is = 1, NS
         spcsig(is) = 0.3 * exp(FRINTF * real(is - 1))
         kwave(is, :) = spcsig(is)**2 / 9.81
      end do
      do id = 1, ND
         ecos(id) = cos(DDIR * real(id - 1))
         esin(id) = sin(DDIR * real(id - 1))
      end do

      dep2 = [5.0, 5.0, 5.0]
      botlv = [5.0, 5.2, 5.1]
      rdx = [1.0, 0.0]
      rdy = [0.0, 1.0]

      !  PSURF(4) is de spilling-constante van Saprykina; de overige posities
      !  krijgen hun gebruikelijke defaults zodat de andere takken ook een
      !  zinnige waarde opleveren.
      PSURF = 0.0
      PSURF(1) = 1.0
      PSURF(2) = 0.73
      PSURF(4) = 0.6
      PSURF(5) = 0.3
      PSURF(6) = 0.4
      PSURF(7) = 0.35
      PSURF(8) = 1.0
      PSURF(15) = 10.0

      IDISRF = 0
   end subroutine configure_grid

!  Zonder energie moet elke formulering een eindige index geven. Dit vangt
!  zowel het lezen van niet-toegekende lokalen als een index die op nul of
!  oneindig uitkomt.
   subroutine test_empty_spectrum_keeps_a_finite_index
      integer, parameter :: branches(6) = &
         [ISURF_CON, ISURF_VAR, ISURF_RUE, ISURF_TG, ISURF_BKD, ISURF_ASYM]
      real :: brcoef, kteta
      integer :: branch

      ac2 = 0.0
      do branch = 1, size(branches)
         ISURF = branches(branch)
         brcoef = huge(brcoef)
         kteta = huge(kteta)
         call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                     kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
         call require(ieee_is_finite(brcoef), &
                      'brekingsindex is NaN of oneindig zonder energie')
         !  Nul is de val van BF-17: HM wordt dan nul en SSURF dissipeert alles.
         call require(abs(brcoef) > 0.0, 'brekingsindex nul zonder energie')
         call require(kteta >= 1.0, 'richtingsfactor kleiner dan een')
      end do
   end subroutine test_empty_spectrum_keeps_a_finite_index

!  De Saprykina-tak in het bijzonder: zonder piek is er geen eerste of tweede
!  harmonische, dus blijft de spilling-constante over.
   subroutine test_empty_spectrum_matches_the_spilling_constant
      real :: brcoef, kteta

      ISURF = ISURF_ASYM
      ac2 = 0.0
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, PSURF(4)), &
                   'lege Saprykina-tak geeft niet de spilling-constante')
   end subroutine test_empty_spectrum_matches_the_spilling_constant

!  Tegenproef: met energie moet de tak wél door de harmonischen-vergelijking
!  lopen, anders zou de bovenstaande test ook slagen op een tak die altijd de
!  constante teruggeeft.
   subroutine test_energy_bearing_spectrum_still_selects_a_branch
      real :: brcoef, kteta

      ISURF = ISURF_ASYM

      !  Smalle piek laag in het spectrum: de tweede harmonische valt binnen
      !  het rooster en draagt vrijwel niets, dus de biphase-tak (-1) wint.
      call set_energy_density([3], [1.0], [10, 11], [0.0, 0.0])
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, -1.0), &
                   'smalle piek kiest niet de biphase-tak')

      !  Piek laag in het spectrum met een zware tweede harmonische: die
      !  draagt meer dan 35 procent, dus de spilling-constante wint -- via
      !  een ander pad dan de lege tak.
      !
      !  De tweede harmonische ligt IS2 = INT(LOG(2)/FRINTF) bins hoger; met
      !  FRINTF = LOG(1.1) is dat bin 7 en 8 boven de piek, dus de piek moet
      !  laag genoeg liggen om beide binnen het rooster te houden.
      call set_energy_density([3], [1.0], [10, 11], [0.5, 0.5])
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, PSURF(4)), &
                   'zware tweede harmonische kiest niet de spilling-constante')
   end subroutine test_energy_bearing_spectrum_still_selects_a_branch

!  Zet de energiedichtheid ED per bin op een gevraagde waarde. BRKPAR rekent
!  ED = SUM(AC2,DIM=1)*SPCSIG*DDIR, dus de actiedichtheid wordt teruggerekend;
!  zo staat in de test wat de fysica ziet in plaats van een omgerekend getal.
   subroutine set_energy_density(peak_bins, peak_values, tail_bins, tail_values)
      integer, intent(in) :: peak_bins(:), tail_bins(:)
      real, intent(in) :: peak_values(:), tail_values(:)
      integer :: k

      ac2 = 0.0
      do k = 1, size(peak_bins)
         ac2(:, peak_bins(k), 1) = &
            peak_values(k) / (spcsig(peak_bins(k)) * DDIR * real(ND))
      end do
      do k = 1, size(tail_bins)
         ac2(:, tail_bins(k), 1) = &
            tail_values(k) / (spcsig(tail_bins(k)) * DDIR * real(ND))
      end do
   end subroutine set_energy_density

!  Exacte gelijkheid zonder de -Wcompare-reals-melding: de invariant hier is
!  wel degelijk bitgelijkheid, want beide takken geven een kopie van PSURF(4)
!  of de literal -1 door. Zelfde idioom als test_physics_kernels.
   logical function same_bits(actual, expected)
      real, intent(in) :: actual, expected

      same_bits = transfer(actual, 0) == transfer(expected, 0)
   end function same_bits

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) error stop message
   end subroutine require

end program test_breaker_index
