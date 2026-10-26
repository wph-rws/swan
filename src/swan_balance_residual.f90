module swan_balance_residual
!
!     Defect of the discrete action balance, reported next to SWAN's
!     change-based stopping criterion.
!
!     SWAN stops an iteration when Hs and Tm01 stop changing. Under-relaxation
!     (ALFA), the action limiter and a limit cycle all make that change small
!     without the balance being satisfied, and a change-based test cannot tell
!     those apart from convergence. This module supplies the second, independent
!     measure: how far the current field is from satisfying the equations the
!     solver is trying to satisfy.
!
!     Every grid point assembles its own spectral system before solving it.
!     SOURCE and ACTION leave the diagonal, the two directional neighbours and
!     the right-hand side in the system matrix; the defect is that system
!     applied to the current AC2:
!
!        d = IMATLA N(id-1) + IMATDA N(id) + IMATUA N(id+1) - IMATRA
!
!     with the neighbour terms dropped exactly where the solver drops them. It
!     is taken before SOLPRE, so under-relaxation does not enter (SOLPRE adds
!     ALFA*SIGMA*N to both sides, which cancels at the current N), and before
!     PHILIM and RESCALE, so neither does the limiter. Upwind grid points carry
!     their update of the running sweep, so this is the defect of the nonlinear
!     Gauss-Seidel iteration SWAN performs; it vanishes when, and only when, an
!     iteration leaves the field unchanged without help from relaxation or
!     limiting.
!
!     Two ratios are formed per grid point, each summed over the four sweeps of
!     an iteration and divided by the matching sum of absolute terms, so both
!     are dimensionless and bounded by one:
!
!     - the spectral residual sums |d| bin by bin. It is the strict measure,
!       and in practice it does not go to zero: with the DIA the individual
!       bins of the energetic part of the spectrum keep moving by five to
!       fifteen per cent from one iteration to the next long after Hs has
!       settled, because those movements cancel in the integral.
!
!     - the energy residual sums SIGMA*d with its sign, which is the defect of
!       the energy balance of the whole spectrum. Bin-to-bin noise cancels in
!       it just as it does in Hs, while a spectrum that is still gaining or
!       losing energy -- the signature of an iteration stopped too early --
!       leaves it standing. It measures the quantity Hs is made of, so it is
!       the one NUMERIC RESIDUAL puts a threshold on.
!
!     Every solver path is covered with the system it solves: the explicit
!     update without refraction or frequency shift, SOLMAT's tridiagonal
!     directional system, SIP's five-point system that also couples the
!     neighbouring frequencies through IMAT5L/IMAT6U (currents or a
!     time-varying depth with the implicit scheme in frequency space), and
!     SOLMT1's directional system with the bins blocked by a counter current
!     left out (the explicit scheme in frequency space, whose frequency terms
!     are already in the right-hand side).
!
   use swan_spectral_grid, only: MDC, MSC, spectral_window_t
   use swan_physics_selection, only: ICUR, IREFR, ITFRE
   use swan_run_mode, only: DYNDEP
   use swan_numerics, only: PNUMS, PNUMS_SCHEMEFR
   use M_PARALL, only: SWREDUCE, SWMAX, SWSUM

   implicit none(type, external)
   private

   public :: balance_residual_t, balance_residual_summary_t
   public :: accumulate_balance_defect, summarize_balance_residual
   public :: write_balance_residual, balance_residual_reached
   public :: balance_defect_system
   public :: DEFECT_NONE, DEFECT_DIAGONAL, DEFECT_DIRECTIONAL
   public :: DEFECT_SIP, DEFECT_BLOCKED

!     Which system the solver of a grid point solves, as far as the defect is
!     concerned: none it can form, the diagonal of the explicit update, the
!     tridiagonal directional system of SOLMAT, the five-point system of SIP,
!     or the directional system of SOLMT1 without its blocked bins.
   integer, parameter :: DEFECT_NONE = 0
   integer, parameter :: DEFECT_DIAGONAL = 1
   integer, parameter :: DEFECT_DIRECTIONAL = 2
   integer, parameter :: DEFECT_SIP = 3
   integer, parameter :: DEFECT_BLOCKED = 4

!     Per-grid-point accumulators for one iteration. A grid point is updated
!     by exactly one thread per sweep and the sweeps are separated by
!     barriers, so the accumulation needs no synchronisation.
   type :: balance_residual_t
      real(kind=8), allocatable :: defect(:)
      real(kind=8), allocatable :: scale(:)
      real(kind=8), allocatable :: energy_defect(:)
      real(kind=8), allocatable :: energy_scale(:)
      logical :: enabled = .false. ! only computed after an explicit RESIDUAL
      integer :: system = DEFECT_NONE
   contains
      procedure :: start => start_balance_residual
      procedure :: finish => finish_balance_residual
   end type balance_residual_t

!     What one iteration reports. POINTS counts the wet grid points that took
!     part: those with any term at all in their balance.
   type :: balance_residual_summary_t
      logical :: available = .false.
      integer :: points = 0
      real :: energy_rms = 0.
      real :: energy_max = 0.
      real :: spectral_rms = 0.
   end type balance_residual_summary_t

contains

   subroutine start_balance_residual(this, npoints)
      class(balance_residual_t), intent(inout) :: this
      integer, intent(in) :: npoints

      call this%finish()
      allocate(this%defect(npoints), this%scale(npoints))
      allocate(this%energy_defect(npoints), this%energy_scale(npoints))
      this%system = balance_defect_system()
      this%enabled = .true.
      call clear(this)
   end subroutine start_balance_residual

   subroutine finish_balance_residual(this)
      class(balance_residual_t), intent(inout) :: this

      this%enabled = .false.
      if (allocated(this%defect)) deallocate(this%defect)
      if (allocated(this%scale)) deallocate(this%scale)
      if (allocated(this%energy_defect)) deallocate(this%energy_defect)
      if (allocated(this%energy_scale)) deallocate(this%energy_scale)
   end subroutine finish_balance_residual

   subroutine clear(this)
      type(balance_residual_t), intent(inout) :: this

      this%defect = 0.d0
      this%scale = 0.d0
      this%energy_defect = 0.d0
      this%energy_scale = 0.d0
   end subroutine clear

!     The branch SWOMPU and SwanCompUnstruc take between the explicit update,
!     SOLMAT, and the SIP/SOLMT1 family; the three conditions are theirs.
   integer function balance_defect_system()
      if (IREFR == 0 .and. ITFRE == 0) then
         balance_defect_system = DEFECT_DIAGONAL
      else if ((DYNDEP .or. ICUR == 1) .and. int(PNUMS(PNUMS_SCHEMEFR)) == 1) then
         balance_defect_system = DEFECT_SIP
      else if ((DYNDEP .or. ICUR == 1) .and. int(PNUMS(PNUMS_SCHEMEFR)) == 2) then
         balance_defect_system = DEFECT_BLOCKED
      else if ((DYNDEP .or. ICUR == 1) .and. int(PNUMS(PNUMS_SCHEMEFR)) /= 0) then
         balance_defect_system = DEFECT_NONE
      else
         balance_defect_system = DEFECT_DIRECTIONAL
      end if
   end function balance_defect_system

!     Adds the defect of the system about to be solved at grid point IGP,
!     from the four coefficient arrays SOURCE and ACTION leave behind (read
!     only; the solver itself keeps the system matrix). Which system that is
!     was fixed when the accumulators were started: the explicit update SWAN
!     uses when neither refraction nor frequency shift couples the bins, the
!     tridiagonal system SOLMAT solves per frequency over IDCMIN..IDCMAX, the
!     same system with the neighbouring frequencies added for SIP (up to
!     ISSTOP, as SWSIP solves it), or the directional system without the
!     blocked bins for SOLMT1. FREQ_LOWER, FREQ_UPPER and BLOCKED are the
!     IMAT5L/IMAT6U coefficients and the blocked-bin mask of those two paths.
!
!     This runs for every grid point in every sweep, so it must be cheap.
!     That is why each system has its own loop over the directions of a
!     frequency: a deck without currents then does not test per bin for
!     frequency coupling or blocked bins, and in the directionally coupled
!     system the first and the last bin, which have only one neighbour, sit
!     outside the loop. Further: no MOD in the inner loop, single precision per
!     bin (the smallest residual that matters is about 1e-4 of the terms, far
!     above rounding), and SIGMA once per frequency. The totals per
!     point are kept in double precision.
!
!     All variants add the terms in the same order as one
!     general loop with all five terms and leave out only terms that are
!     zero there. Adding zero is exact, so the numbers are bit for bit those
!     of that general loop.
   subroutine accumulate_balance_defect(this, igp, window, ac2, spcsig, &
      lower, diagonal, upper, rhs, freq_lower, freq_upper, blocked)
      type(balance_residual_t), intent(inout) :: this
      integer, intent(in) :: igp
      type(spectral_window_t), intent(in) :: window
      real, intent(in) :: ac2(MDC,MSC)
      real, intent(in) :: spcsig(MSC)
      real, intent(in) :: lower(MDC,MSC), diagonal(MDC,MSC)
      real, intent(in) :: upper(MDC,MSC), rhs(MDC,MSC)
      real, intent(in) :: freq_lower(MDC,MSC), freq_upper(MDC,MSC)
      logical, intent(in) :: blocked(MDC,MSC)

      integer :: is, idmin, idmax, isstop
      real :: sum_abs, sum_terms, sum_signed
      real(kind=8) :: defect, scale, energy_defect, energy_scale

      if (this%system == DEFECT_NONE) return
      isstop = MSC
      if (this%system == DEFECT_SIP) isstop = window%isstop
      defect = 0.d0
      scale = 0.d0
      energy_defect = 0.d0
      energy_scale = 0.d0
      do is = 1, isstop
         idmin = window%idcmin(is)
         idmax = window%idcmax(is)
         if (idmax < idmin) cycle
         select case (this%system)
         case (DEFECT_DIAGONAL)
            call diagonal_row(idmin, idmax, diagonal(:,is), ac2(:,is), &
               rhs(:,is), sum_abs, sum_terms, sum_signed)
         case (DEFECT_DIRECTIONAL)
            call directional_row(idmin, idmax, lower(:,is), diagonal(:,is), &
               upper(:,is), ac2(:,is), rhs(:,is), sum_abs, sum_terms, sum_signed)
         case (DEFECT_SIP)
            call sip_row(is, isstop, idmin, idmax, lower, diagonal, upper, &
               freq_lower, freq_upper, ac2, rhs, sum_abs, sum_terms, sum_signed)
         case default
            call blocked_row(idmin, idmax, lower(:,is), diagonal(:,is), &
               upper(:,is), ac2(:,is), rhs(:,is), blocked(:,is), &
               sum_abs, sum_terms, sum_signed)
         end select
         defect = defect + real(sum_abs, 8)
         scale = scale + real(sum_terms, 8)
         energy_defect = energy_defect + real(spcsig(is), 8) * real(sum_signed, 8)
         energy_scale = energy_scale + real(spcsig(is), 8) * real(sum_terms, 8)
      end do
      this%defect(igp) = this%defect(igp) + defect
      this%scale(igp) = this%scale(igp) + scale
      this%energy_defect(igp) = this%energy_defect(igp) + energy_defect
      this%energy_scale(igp) = this%energy_scale(igp) + energy_scale
   end subroutine accumulate_balance_defect

!     The explicit system: only the diagonal.
   pure subroutine diagonal_row(idmin, idmax, diagonal, ac2, rhs, &
      sum_abs, sum_terms, sum_signed)
      integer, intent(in) :: idmin, idmax
      real, intent(in) :: diagonal(MDC), ac2(MDC), rhs(MDC)
      real, intent(out) :: sum_abs, sum_terms, sum_signed

      integer :: iddum, id
      real :: t_diag, t_rhs, d

      sum_abs = 0.
      sum_terms = 0.
      sum_signed = 0.
      id = mod(idmin - 1 + MDC, MDC) + 1
      do iddum = idmin, idmax
         t_diag = diagonal(id) * ac2(id)
         t_rhs = rhs(id)
         d = t_diag - t_rhs
         sum_abs = sum_abs + abs(d)
         sum_terms = sum_terms + abs(t_diag) + abs(t_rhs)
         sum_signed = sum_signed + d
         id = id + 1
         if (id > MDC) id = 1
      end do
   end subroutine diagonal_row

!     The tridiagonal system of SOLMAT. Within IDMIN..IDMAX each bin
!     is coupled to its two neighbours, except the first (no lower neighbour) and the
!     last (no upper neighbour).
   pure subroutine directional_row(idmin, idmax, lower, diagonal, upper, ac2, &
      rhs, sum_abs, sum_terms, sum_signed)
      integer, intent(in) :: idmin, idmax
      real, intent(in) :: lower(MDC), diagonal(MDC), upper(MDC)
      real, intent(in) :: ac2(MDC), rhs(MDC)
      real, intent(out) :: sum_abs, sum_terms, sum_signed

      integer :: iddum, id, idm1, idp1
      real :: t_diag, t_lower, t_upper, t_rhs, d

      sum_abs = 0.
      sum_terms = 0.
      sum_signed = 0.
      id = mod(idmin - 1 + MDC, MDC) + 1
      t_diag = diagonal(id) * ac2(id)
      t_rhs = rhs(id)
      if (idmax == idmin) then
         d = t_diag - t_rhs
         sum_abs = sum_abs + abs(d)
         sum_terms = sum_terms + abs(t_diag) + abs(t_rhs)
         sum_signed = sum_signed + d
         return
      end if

      idp1 = id + 1
      if (idp1 > MDC) idp1 = 1
      t_upper = upper(id) * ac2(idp1)
      d = t_diag + t_upper - t_rhs
      sum_abs = sum_abs + abs(d)
      sum_terms = sum_terms + abs(t_diag) + abs(t_upper) + abs(t_rhs)
      sum_signed = sum_signed + d
      idm1 = id
      id = idp1

      do iddum = idmin + 1, idmax - 1
         idp1 = id + 1
         if (idp1 > MDC) idp1 = 1
         t_lower = lower(id) * ac2(idm1)
         t_diag = diagonal(id) * ac2(id)
         t_upper = upper(id) * ac2(idp1)
         t_rhs = rhs(id)
         d = t_lower + t_diag + t_upper - t_rhs
         sum_abs = sum_abs + abs(d)
         sum_terms = sum_terms + abs(t_lower) + abs(t_diag) + abs(t_upper) &
            + abs(t_rhs)
         sum_signed = sum_signed + d
         idm1 = id
         id = idp1
      end do

      t_lower = lower(id) * ac2(idm1)
      t_diag = diagonal(id) * ac2(id)
      t_rhs = rhs(id)
      d = t_lower + t_diag - t_rhs
      sum_abs = sum_abs + abs(d)
      sum_terms = sum_terms + abs(t_lower) + abs(t_diag) + abs(t_rhs)
      sum_signed = sum_signed + d
   end subroutine directional_row

!     The five-point system of SIP: the directional system plus the neighbours in
!     frequency, up to and including ISSTOP.
   pure subroutine sip_row(is, isstop, idmin, idmax, lower, diagonal, upper, &
      freq_lower, freq_upper, ac2, rhs, sum_abs, sum_terms, sum_signed)
      integer, intent(in) :: is, isstop, idmin, idmax
      real, intent(in) :: lower(MDC,MSC), diagonal(MDC,MSC), upper(MDC,MSC)
      real, intent(in) :: freq_lower(MDC,MSC), freq_upper(MDC,MSC)
      real, intent(in) :: ac2(MDC,MSC), rhs(MDC,MSC)
      real, intent(out) :: sum_abs, sum_terms, sum_signed

      integer :: iddum, id, idm1, idp1
      real :: t_diag, t_lower, t_upper, t_freq, t_rhs, d

      sum_abs = 0.
      sum_terms = 0.
      sum_signed = 0.
      id = mod(idmin - 1 + MDC, MDC) + 1
      idm1 = id
      do iddum = idmin, idmax
         idp1 = id + 1
         if (idp1 > MDC) idp1 = 1
         t_diag = diagonal(id,is) * ac2(id,is)
         t_rhs = rhs(id,is)
         t_lower = 0.
         t_upper = 0.
         t_freq = 0.
         if (iddum > idmin) t_lower = lower(id,is) * ac2(idm1,is)
         if (iddum < idmax) t_upper = upper(id,is) * ac2(idp1,is)
         if (is > 1) t_freq = freq_lower(id,is) * ac2(id,is-1)
         if (is < isstop) t_freq = t_freq + freq_upper(id,is) * ac2(id,is+1)
         d = t_lower + t_diag + t_upper + t_freq - t_rhs
         sum_abs = sum_abs + abs(d)
         sum_terms = sum_terms + abs(t_lower) + abs(t_diag) + abs(t_upper) &
            + abs(t_freq) + abs(t_rhs)
         sum_signed = sum_signed + d
         idm1 = id
         id = idp1
      end do
   end subroutine sip_row

!     The directional system of SOLMT1 without the bins that the current
!     blocks. A blocked bin does not count itself, but it remains a
!     neighbour of the bins next to it.
   pure subroutine blocked_row(idmin, idmax, lower, diagonal, upper, ac2, rhs, &
      blocked, sum_abs, sum_terms, sum_signed)
      integer, intent(in) :: idmin, idmax
      real, intent(in) :: lower(MDC), diagonal(MDC), upper(MDC)
      real, intent(in) :: ac2(MDC), rhs(MDC)
      logical, intent(in) :: blocked(MDC)
      real, intent(out) :: sum_abs, sum_terms, sum_signed

      integer :: iddum, id, idm1, idp1
      real :: t_diag, t_lower, t_upper, t_rhs, d

      sum_abs = 0.
      sum_terms = 0.
      sum_signed = 0.
      id = mod(idmin - 1 + MDC, MDC) + 1
      idm1 = id
      do iddum = idmin, idmax
         idp1 = id + 1
         if (idp1 > MDC) idp1 = 1
         if (.not.blocked(id)) then
            t_diag = diagonal(id) * ac2(id)
            t_rhs = rhs(id)
            t_lower = 0.
            t_upper = 0.
            if (iddum > idmin) t_lower = lower(id) * ac2(idm1)
            if (iddum < idmax) t_upper = upper(id) * ac2(idp1)
            d = t_lower + t_diag + t_upper - t_rhs
            sum_abs = sum_abs + abs(d)
            sum_terms = sum_terms + abs(t_lower) + abs(t_diag) + abs(t_upper) &
               + abs(t_rhs)
            sum_signed = sum_signed + d
         end if
         idm1 = id
         id = idp1
      end do
   end subroutine blocked_row

!     Reduces one iteration's accumulators over the points in COUNTED and
!     clears them for the next iteration. COUNTED marks the wet points this
!     process owns, so MPI halo points are not counted twice. The local pass
!     runs in index order on one thread, so the numbers do not depend on the
!     thread count; across MPI processes the partial sums are combined in
!     single precision and may differ from a serial run in the last digits.
!     Call it on one thread only (it performs MPI collectives).
   subroutine summarize_balance_residual(this, counted, supported, summary)
      type(balance_residual_t), intent(inout) :: this
      logical, intent(in) :: counted(:)
      logical, intent(in) :: supported
      type(balance_residual_summary_t), intent(out) :: summary

      integer :: ip, npts(1)
      real(kind=8) :: energy, spectral, sum_energy, sum_spectral, largest
      real(kind=8) :: sums(2), peak(1)

      npts = 0
      sum_energy = 0.d0
      sum_spectral = 0.d0
      largest = 0.d0
      do ip = 1, size(counted)
         if (.not.counted(ip)) cycle
         if (this%scale(ip) <= 0.d0 .or. this%energy_scale(ip) <= 0.d0) cycle
         energy = abs(this%energy_defect(ip)) / this%energy_scale(ip)
         spectral = this%defect(ip) / this%scale(ip)
         npts(1) = npts(1) + 1
         sum_energy = sum_energy + energy**2
         sum_spectral = sum_spectral + spectral**2
         largest = max(largest, energy)
      end do
      call clear(this)

      ! summed in double precision over the MPI processes as well, so a
      ! parallel run gives the serial numbers up to the order of summation
      sums = [sum_energy, sum_spectral]
      peak = largest
      call SWREDUCE(npts, 1, SWSUM)
      call SWREDUCE(sums, 2, SWSUM)
      call SWREDUCE(peak, 1, SWMAX)

      summary%available = supported .and. npts(1) > 0
      summary%points = npts(1)
      if (.not.summary%available) return
      summary%energy_rms = real(sqrt(sums(1) / real(npts(1), kind=8)))
      summary%energy_max = real(peak(1))
      summary%spectral_rms = real(sqrt(sums(2) / real(npts(1), kind=8)))
   end subroutine summarize_balance_residual

!     One line per iteration, next to the "accuracy OK" line and worded so
!     that parsers looking for that line are not affected. THRESHOLD is the
!     NUMERIC RESIDUAL value; zero or less means none was asked for.
   subroutine write_balance_residual(unit, summary, threshold)
      integer, intent(in) :: unit
      type(balance_residual_summary_t), intent(in) :: summary
      real, intent(in) :: threshold

      if (.not.summary%available) then
         write (unit, "(' balance residual not available for this solver',/)")
      else if (threshold > 0.) then
         write (unit, "(' balance residual energy rms ',1PE9.2,', max ',1PE9.2, &
            &', spectral rms ',1PE9.2,' in ',I9,' wet grid points (',1PE9.2, &
            &' required)',/)") summary%energy_rms, summary%energy_max, &
            summary%spectral_rms, summary%points, threshold
      else
         write (unit, "(' balance residual energy rms ',1PE9.2,', max ',1PE9.2, &
            &', spectral rms ',1PE9.2,' in ',I9,' wet grid points',/)") &
            summary%energy_rms, summary%energy_max, summary%spectral_rms, &
            summary%points
      end if
   end subroutine write_balance_residual

!     Whether the residual lets the iteration stop. Without a threshold it
!     always does, so a run without NUMERIC RESIDUAL stops exactly where it
!     did before.
   logical function balance_residual_reached(summary, threshold)
      type(balance_residual_summary_t), intent(in) :: summary
      real, intent(in) :: threshold

      if (threshold <= 0.) then
         balance_residual_reached = .true.
      else
         balance_residual_reached = summary%available .and. &
            summary%energy_rms <= threshold
      end if
   end function balance_residual_reached

end module swan_balance_residual
