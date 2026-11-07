module swan_stop_extrapolation
!
!     An estimate of how much Hs still has to change, next to SWAN's
!     change-based stopping criterion (NUMERIC ... EXTRAP [nstep], a check of
!     this fork).
!
!     STOPC and ACCUR judge the change of Hs from one iteration to the next.
!     Under-relaxation (ALFA) makes that change small by design, so the test
!     passes while Hs is still moving steadily. STOPC also asks for a small
!     curvature of the iteration curve, but a steady drift has no curvature
!     at all. On examples/nonlinear_interactions/quad/quad_dia1 with
!     ALFA=0.01, STOPC stops after about 40 iterations with Hs at 25 km
!     still 0.52 m below the converged value, while the change per
!     iteration is about 0.007 m and is not getting smaller.
!
!     When an iteration converges, the change shrinks by about the same
!     factor r each step. The remaining change is then the sum of a
!     geometric series, |D| r / (1 - r), with D the last change (Aitken).
!     One iteration is too short to measure r: from one iteration to the
!     next the change of Hs carries noise of the same size as the change
!     itself, so r jumps between -1 and 5. Over a block of [nstep]
!     iterations the noise averages out. So every [nstep] iterations this
!     module stores Hs per grid point, and from three such checkpoints it
!     takes D2 = H(k) - H(k-n), D1 = H(k-n) - H(k-2n) and r = D2/D1:
!
!     - 0 < r < 1: Hs converges; the estimated remaining change is
!       |D2| r / (1 - r), at most |D2| * HORIZON (below);
!     - r >= 1: the change is steady or growing, so Hs is not converging
!       yet and the series gives no estimate. Such a change is counted as
!       going on for HORIZON more blocks, |D2| * HORIZON. A real drift, as
!       in quad_dia1 (0.07 m per block of 10), then fails by far, while
!       the noise of an iteration that has reached its floor (well below
!       a millimetre per block, with a ratio that is random) passes;
!     - r <= 0: Hs moves back and forth around its value; the remaining
!       change is about the size of the swing, |D2|.
!
!     A grid point passes when that estimate is within the accuracy of the
!     stopping criterion itself: [dabs] or [drel] times Hs. Under NUMERIC
!     ACCUR the deck sets no [dabs]; the default [dabs] of STOPC is then
!     used, not [dhoval] times the mean Hs. When the mean Hs is larger than
!     [dabs]/[dhoval] (0.25 m with the defaults) that is the stricter of the
!     two. The iteration
!     only stops when the share of passing wet points reaches [npnts] as
!     well, so the criterion of the deck still has to be met, and it can
!     no longer be met by an iteration that only moves slowly. If the block
!     test is never met, the run ends at [mxitst], as before, and PRINT says
!     how many points were still drifting.
!
!     On quad_dia1 the estimate lies within a factor 0.4 to 1.4 of the true
!     remaining change once the iteration converges. That is an estimate,
!     not a bound: an iteration that slows down by itself, or a slow mode
!     under a fast one, can still fool it. Only grid points that this
!     process owns are counted, so MPI halo points are not counted twice.
!
   use swan_computational_grid, only: MXC, MYC
   use swan_spectral_grid, only: MSC, MDC, FRINTF, DDIR
   use swan_physical_settings, only: DEPMIN
   use swan_numerics, only: PNUMS, PNUMS_DABS, PNUMS_DREL, PNUMS_NPNTS
   use M_PARALL, only: SWREDUCE, SWSUM, SWMAX, IHALOX, IHALOY, LMXF, LMXL, LMYF, LMYL
   implicit none(type, external)
   private

   public :: hs_extrapolation_t

!  number of blocks a steady change is counted as going on for (see above)
   real, parameter :: HORIZON = 100.

   type :: hs_extrapolation_t
      logical :: enabled = .false.
      integer :: nstep = 10
      integer :: nheld = 0 ! checkpoints stored so far (0, 1, 2)
      logical :: reached = .false. ! the block test was met at the last checkpoint
      integer, allocatable :: held(:) ! consecutive wet checkpoints per point
      real, allocatable :: h1(:), h2(:) ! Hs at the last and the one-before-last checkpoint
      ! numbers of the last checkpoint, for PRINT
      real :: percent = 0.
      real :: largest = 0.
      integer :: wet = 0
      integer :: steady = 0
   contains
   procedure :: start => extrapolation_start
   procedure :: check => extrapolation_check
   procedure :: write => extrapolation_write
   end type hs_extrapolation_t

contains

subroutine extrapolation_start(self, nstep, mcgrd)
   class(hs_extrapolation_t), intent(inout) :: self
   integer, intent(in) :: nstep, mcgrd
   self%enabled = .true.
   self%nstep = max(2, nstep)
   self%nheld = 0
   self%reached = .false.
   if (allocated(self%h1)) deallocate(self%h1)
   if (allocated(self%h2)) deallocate(self%h2)
   if (allocated(self%held)) deallocate(self%held)
   allocate(self%h1(mcgrd), self%h2(mcgrd), self%held(mcgrd))
   self%held = 0
   self%h1 = 0.
   self%h2 = 0.
end subroutine extrapolation_start

!  Called once per iteration, by one thread. Returns .true. when this
!  iteration is a checkpoint, so that the caller can print the numbers.
logical function extrapolation_check(self, iter, ac2, spcsig, depth, kgrpnt) result(checked)
   class(hs_extrapolation_t), intent(inout) :: self
   integer, intent(in) :: iter
   real, intent(in) :: ac2(:,:,:) ! action density (MDC,MSC,MCGRD)
   real, intent(in) :: spcsig(:) ! relative frequencies (MSC)
   real, intent(in) :: depth(:) ! water depth per grid point (MCGRD)
   integer, intent(in) :: kgrpnt(:,:) ! grid point index (MXC,MYC)

   integer :: ix, iy, ip, is, ix1, ix2, iy1, iy2
   integer :: counts(3) ! wet points, passing points, steady or growing change
   real :: largest(1), m0, hs, d1, d2, r, estimate, tolerance, tiny

   checked = .false.
   if (.not. self%enabled) return
   if (mod(iter, self%nstep) /= 0) return
   checked = .true.

   ix1 = 1
   if (.not.LMXF) ix1 = 1 + IHALOX
   ix2 = MXC
   if (.not.LMXL) ix2 = MXC - IHALOX
   iy1 = 1
   if (.not.LMYF) iy1 = 1 + IHALOY
   iy2 = MYC
   if (.not.LMYL) iy2 = MYC - IHALOY

   counts = 0
   largest = 0.
   do iy = iy1, iy2
      do ix = ix1, ix2
         ip = kgrpnt(ix,iy)
         if (ip <= 1) cycle
         if (.not. depth(ip) > DEPMIN) then
            self%held(ip) = 0
            cycle
         end if
         m0 = 0.
         do is = 1, MSC
            m0 = m0 + spcsig(is)**2 * sum(ac2(1:MDC,is,ip))
         end do
         hs = 4. * sqrt(max(0., m0 * FRINTF * DDIR))
         counts(1) = counts(1) + 1
         if (self%held(ip) >= 2) then
            d2 = hs - self%h1(ip)
            d1 = self%h1(ip) - self%h2(ip)
            tolerance = max(PNUMS(PNUMS_DABS), PNUMS(PNUMS_DREL) * hs)
!              a change at rounding level counts as no change
            tiny = 1.e-6 * max(hs, 1.e-3)
            r = 0.
            if (abs(d2) <= tiny) then
               estimate = 0.
            else if (d2 * d1 <= 0.) then
               estimate = abs(d2)
            else
               r = d2 / d1
               if (r >= 1.) then
                  estimate = HORIZON * abs(d2)
                  if (estimate > tolerance) counts(3) = counts(3) + 1
               else
!                    with r just below 1 the series is no better than a
!                    steady change, so the estimate is not taken larger
                  estimate = abs(d2) * min(r / (1. - r), HORIZON)
               end if
            end if
            if (estimate <= tolerance) counts(2) = counts(2) + 1
!              the largest estimate is that of the points that converge or
!              swing; the steady ones are counted apart
            if (r < 1. .or. d2 * d1 <= 0.) largest(1) = max(largest(1), estimate)
         end if
         self%held(ip) = min(2, self%held(ip) + 1)
         self%h2(ip) = self%h1(ip)
         self%h1(ip) = hs
      end do
   end do
   call SWREDUCE(counts, 3, SWSUM)
   call SWREDUCE(largest, 1, SWMAX)

   if (self%nheld < 2) then
      self%nheld = self%nheld + 1
      self%reached = .false.
      self%wet = 0
      return
   end if
   self%wet = counts(1)
   self%steady = counts(3)
   self%largest = largest(1)
   if (counts(1) > 0) then
      self%percent = 100. * real(counts(2)) / real(counts(1))
   else
      self%percent = 100.
   end if
   self%reached = self%percent >= PNUMS(PNUMS_NPNTS)
end function extrapolation_check

subroutine extrapolation_write(self, unit)
   class(hs_extrapolation_t), intent(in) :: self
   integer, intent(in) :: unit
   if (self%wet == 0) then
      write (unit, '(a,i0,a)') ' extrapolated change of Hs: first estimate after ', &
         3 * self%nstep, ' iterations'
   else
      write (unit, '(a,f6.2,a,f6.2,a,es10.3,a,i0,a)') ' extrapolated change of Hs OK in ', &
         self%percent, ' % of wet grid points (', PNUMS(PNUMS_NPNTS), &
         ' % required); largest estimate where Hs converges ', self%largest, ' m; ', &
         self%steady, ' points fail with a steady or growing change'
   end if
end subroutine extrapolation_write

end module swan_stop_extrapolation
