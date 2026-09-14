program test_breaker_index
!  BRKPAR must give a finite, physically sensible breaker index for every
!  breaking formulation -- also for a grid point without energy.
!
!  Such a point is not exotic: at the first iteration the whole field is zero,
!  and in the lee of an obstacle or in a closed-off corner of the domain
!  it stays that way. The Saprykina branch (ISURF_ASYM) looks there for a peak frequency
!  that does not exist. The "no peak found" sentinel is ISIGM = -1, so a
!  test on ISIGM .EQ. 0 never fires and the branch fell through to a comparison
!  of E1 and E2, which are never assigned on that path.
!
!  The tests below check the invariant, not a stored number:
!  without waves there is nothing to break, so the index must stay at the
!  spilling constant PSURF(4). Zero would be fatal here -- that
!  is exactly the trap of BF-34: HM = GAMBR*DEP2 then becomes zero, BB in SSURF
!  infinite, and the saturated dissipation branch then dissipates everything.
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
      !  Logarithmic frequency grid with the usual growth factor
      !  1.1; FRINTF is the logarithmic step that locates the second harmonic in
      !  BRKPAR.
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

      !  PSURF(4) is Saprykina's spilling constant; the other positions
      !  get their usual defaults so that the other branches also give a
      !  sensible value.
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

!  Without energy, every formulation must give a finite index. This catches
!  both reading unassigned locals and an index that comes out as zero or
!  infinite.
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
                      'breaker index is NaN or infinite without energy')
         !  Zero is the trap of BF-34: HM then becomes zero and SSURF dissipates everything.
         call require(abs(brcoef) > 0.0, 'breaker index zero without energy')
         call require(kteta >= 1.0, 'directional factor smaller than one')
      end do
   end subroutine test_empty_spectrum_keeps_a_finite_index

!  The Saprykina branch in particular: without a peak there is no first or second
!  harmonic, so the spilling constant is what remains.
   subroutine test_empty_spectrum_matches_the_spilling_constant
      real :: brcoef, kteta

      ISURF = ISURF_ASYM
      ac2 = 0.0
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, PSURF(4)), &
                   'empty Saprykina branch does not give the spilling constant')
   end subroutine test_empty_spectrum_matches_the_spilling_constant

!  Counter-check: with energy the branch must go through the harmonics comparison,
!  otherwise the test above would also pass for a branch that always returns
!  the constant.
   subroutine test_energy_bearing_spectrum_still_selects_a_branch
      real :: brcoef, kteta

      ISURF = ISURF_ASYM

      !  Narrow peak low in the spectrum: the second harmonic falls inside
      !  the grid and carries almost nothing, so the biphase branch (-1) wins.
      call set_energy_density([3], [1.0], [10, 11], [0.0, 0.0])
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, -1.0), &
                   'narrow peak does not select the biphase branch')

      !  Peak low in the spectrum with a heavy second harmonic: it
      !  carries more than 35 per cent, so the spilling constant wins -- by
      !  a different path than the empty branch.
      !
      !  The second harmonic lies IS2 = INT(LOG(2)/FRINTF) bins higher; with
      !  FRINTF = LOG(1.1) that is bins 7 and 8 above the peak, so the peak must
      !  lie low enough to keep both inside the grid.
      call set_energy_density([3], [1.0], [10, 11], [0.5, 0.5])
      brcoef = huge(brcoef)
      call BRKPAR(brcoef, ecos, esin, ac2, spcsig, dep2, botlv, rdx, rdy, &
                  kwave, window, 0.0, kteta, 0.1, 1, 2, 3)
      call require(same_bits(brcoef, PSURF(4)), &
                   'heavy second harmonic does not select the spilling constant')
   end subroutine test_energy_bearing_spectrum_still_selects_a_branch

!  Set the energy density ED per bin to a requested value. BRKPAR computes
!  ED = SUM(AC2,DIM=1)*SPCSIG*DDIR, so the action density is computed back;
!  that way the test states what the physics sees instead of a converted number.
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

!  Exact equality without the -Wcompare-reals warning: the invariant here
!  really is bit equality, because both branches pass on a copy of PSURF(4)
!  or the literal -1. Same idiom as test_physics_kernels.
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
