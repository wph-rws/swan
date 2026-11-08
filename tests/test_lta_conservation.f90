program test_lta_conservation
!  The triads of the LTA (TRIAD ITRIAD=1) only move energy between
!  frequencies. Apart from the group-velocity factor, which SWLTA applies
!  afterwards, they must not create or destroy energy. This test calls SWLTA
!  directly on one grid point with a smooth spectrum and the coefficients
!  and interpolation weights that FAC3WW computes for a real depth. The group
!  velocity is set to 1, so that only the exchange itself is weighed. It then
!  integrates the net source over frequency and direction.
!
!  With the fork's form of the third-harmonic term (BF-63, the default) the
!  net source is zero up to the interpolation between the log-spaced bins.
!  With TRIAD ... UPSTREAM (the form of TU Delft 41.51) the loss at f is
!  2 SA3(3f) and nothing is taken at 2f, so a third of the energy that
!  arrives at the third harmonic is created; the net source is then clearly
!  positive. The self interaction (p + p -> 2p) conserves energy in both
!  forms, so this difference is the third harmonic alone.
   use swan_nonlinear_interactions, only: FAC3WW, SWLTA
   use swan_triad_state, only: triad_state_t
   use swan_spectral_grid, only: MSC, MDC, DDIR, FRINTF, FULCIR, &
                                 spectral_window_t
   use swan_computational_grid, only: MCGRD
   use swan_physics_selection, only: ITRIAD, PTRIAD, LTA_CONSERVE, MREDS
   use swan_physical_settings, only: GRAV, DEPMIN
   use swan_diagnostics_level, only: ITEST, LTRACE
   use swan_test_output, only: TESTFL, NPTST, IPTST
   use swan_stencil, only: MICMAX
   use swan_math_constants, only: PI
   implicit none(type, external)

   integer, parameter :: nfreq = 40, ndir = 36, point = 2
   real, parameter :: fmin = 0.04, fmax = 1.2, fpeak = 0.1, depth = 5.0

   type(triad_state_t) :: triads
   type(spectral_window_t) :: window
   integer, target :: idcmin(nfreq), idcmax(nfreq), iscmin(ndir), iscmax(ndir)
   real :: spcsig(nfreq), dep2(point), cgo(nfreq, MICMAX)
   real :: ac2(ndir, nfreq, point), ursell(point), biphas(point)
   real :: smebrk, net, gain, loss, net_upstream, gain_upstream, loss_upstream
   integer :: status

   call set_up
   call net_source(.true., net, gain, loss)
   call net_source(.false., net_upstream, gain_upstream, loss_upstream)

   write (*, '(a, es11.3, a, es11.3)') 'BF-63:    net ', net, &
      '  gross gain ', gain
   write (*, '(a, es11.3, a, es11.3)') 'UPSTREAM: net ', net_upstream, &
      '  gross gain ', gain_upstream

!  A run that moved nothing would conserve trivially; make sure the spectrum
!  really drives a transfer.
   call require(gain > 0.0 .and. loss < 0.0, &
      'the LTA moved no energy, so the test proves nothing')
!  Measured on 9 November 2026, with the net source as a fraction of the
!  gross gain (the sum over the bins that gain energy):
!
!     frequencies   BF-63      UPSTREAM
!          20      -4.2e-3     +4.4e-2
!          40      -6.1e-4     +4.6e-2
!          80      -2.1e-4     +4.4e-2
!         160      -1.0e-4     +4.4e-2
!
!  With BF-63 the rest shrinks as the grid gets finer, so it is the
!  interpolation to 2f, 3f and 3f/2 between log-spaced bins. In the upstream
!  form it stays the same at every resolution: the source itself creates
!  energy. The thresholds lie a factor of eight and of three from the
!  values at 40 frequencies.
   call require(abs(net) <= 5.0e-3 * gain, &
      'the LTA with the third-harmonic fix (BF-63) creates or destroys energy')
   call require(net_upstream >= 1.5e-2 * gain_upstream, &
      'the upstream LTA no longer creates energy at the third harmonic; ' // &
      'BF-63 and its test need another look')

   write (*, '(a)') 'LTA conservation: OK'

contains

subroutine set_up
   integer :: is, id
   real :: ratio, f, theta, spread

   GRAV = 9.81
   DEPMIN = 0.05
   ITEST = 0
   LTRACE = .false.
   TESTFL = .false.
   NPTST = 0
   IPTST = 1

   MSC = nfreq
   MDC = ndir
   MCGRD = point
   FULCIR = .true.
   DDIR = 2.0 * PI / real(ndir)

!     Logarithmic frequency grid, as SWAN builds it.
   ratio = (fmax / fmin)**(1.0 / real(nfreq - 1))
   FRINTF = log(ratio)
   do is = 1, nfreq
      spcsig(is) = 2.0 * PI * fmin * ratio**(is - 1)
   end do

!     The operational so-rp settings: TRIAD ITRIAD=1 TRFAC=0.8 CUTFR=2.5,
!     with the 41.51 defaults for what that deck leaves open: [urcrit] and
!     [urslim], the 60-degree window around each direction, and the coupling
!     coefficient of Akrish et al. (2024), which SWAN chooses for the LTA
!     when IC is not given (PTRIAD(10) = 4, see SWAN's input checks).
   ITRIAD = 1
   PTRIAD = 0.0
   PTRIAD(1) = 0.8
   PTRIAD(2) = 2.5
   PTRIAD(4) = 0.63
   PTRIAD(5) = 0.1
   PTRIAD(8) = 60.0
   PTRIAD(10) = 4.0

   dep2 = depth
   call triads%resize(ITRIAD, nfreq, point, 0, status)
   call require(status == 0, 'could not allocate the triad state')
   call FAC3WW(dep2, spcsig, triads)

!     A smooth single-peaked spectrum, cos^2 spread around 0 degrees. It
!     decays to round-off well inside both ends of the grid, so no transfer
!     runs off the grid.
   ac2 = 0.0
   do is = 1, nfreq
      f = spcsig(is) / (2.0 * PI)
      do id = 1, ndir
         theta = (real(id) - 0.5) * DDIR - PI
         spread = max(0.0, cos(theta))**2
         ac2(id, is, point) = 0.5 * exp(-((log(f / fpeak)) / 0.35)**2) &
                              * spread / spcsig(is)
      end do
   end do
   smebrk = 2.0 * PI * fpeak * 1.1

!     Above the Ursell limit, with a biphase as SWAN parametrises it.
   ursell = 0.5
   biphas = -0.5 * PI + 0.5 * PI * tanh(PTRIAD(4) / ursell)

!     The group velocity is 1: the exchange alone is weighed.
   cgo = 1.0

   idcmin = 1
   idcmax = ndir
   iscmin = 1
   iscmax = nfreq
   window%idcmin => idcmin
   window%idcmax => idcmax
   window%iscmin => iscmin
   window%iscmax => iscmax
   window%isstop = nfreq
   window%iddlow = 1
   window%iddtop = ndir
   window%idwmin = 1
   window%idwmax = ndir
end subroutine set_up

!  The rate of change of energy, integrated over the spectrum. SWLTA puts a
!  positive source on the right-hand side and a negative one, divided by the
!  action density, on the main diagonal (Patankar), so the source in each
!  bin is IMATRA - IMATDA * AC2 in action. Times sigma it is energy, and the
!  bin width in sigma is sigma * FRINTF.
subroutine net_source(conserve, net, gain, loss)
   logical, intent(in) :: conserve
   real, intent(out) :: net, gain, loss
   real :: imatra(ndir, nfreq), imatda(ndir, nfreq)
   real :: redc0(ndir, nfreq, MREDS), redc1(ndir, nfreq, MREDS)
   real :: pltri(ndir, nfreq, 1), qtl2(nfreq, 2), rate
   integer :: is, id

   LTA_CONSERVE = conserve
   imatra = 0.0
   imatda = 0.0
   redc0 = 0.0
   redc1 = 0.0
   pltri = 0.0
   qtl2 = triads%scaling(:, point, :)

   call SWLTA(ac2, dep2, cgo, spcsig, imatra, imatda, redc0, redc1, &
              window, smebrk, pltri, ursell, biphas, qtl2, triads, point)

   net = 0.0
   gain = 0.0
   loss = 0.0
   do is = 1, nfreq
      do id = 1, ndir
         rate = (imatra(id, is) - imatda(id, is) * ac2(id, is, point)) &
                * spcsig(is) * spcsig(is) * FRINTF * DDIR
         net = net + rate
         if (rate > 0.0) then
            gain = gain + rate
         else
            loss = loss + rate
         end if
      end do
   end do
end subroutine net_source

subroutine require(condition, message)
   logical, intent(in) :: condition
   character(*), intent(in) :: message
   if (.not. condition) then
      write (*, '(a)') 'FAIL: ' // message
      error stop 1
   end if
end subroutine require

end program test_lta_conservation
