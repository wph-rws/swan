module swan_frequency_range
!
!     Checks after a stationary computation whether the frequency grid of
!     CGRID reaches far enough above the spectral peak. The SWAN user manual
!     (CGRID, [fhigh]) asks for a highest frequency of at least 2.5 to 3
!     times the highest peak frequency expected; below that, the computation
!     cuts off part of the spectrum, and in low wind the peak itself can lie
!     at the highest frequency. Upstream SWAN does not check this.
!
!     The peak is the frequency bin with the largest energy density
!     E(sigma) = sigma * sum over directions of N(sigma,theta); the energy
!     density per frequency has its maximum in the same bin. Only wet grid
!     points with energy that this process owns are counted, so MPI halo
!     points are not counted twice. If at least WARN_PERCENT of them have a
!     highest frequency below REQUIRED_RATIO times their peak frequency,
!     SWAN gives a warning (level 1) with the numbers. The threshold of one
!     percent is a choice of this fork, so that a few points next to land
!     do not give a warning for an otherwise fine grid.
!
!     This reads AC2 after the computation and changes nothing.
!
   use swan_computational_grid, only: MXC, MYC
   use swan_spectral_grid, only: MSC
   use swan_physical_settings, only: DEPMIN
   use M_PARALL, only: SWREDUCE, SWSUM, IHALOX, IHALOY, LMXF, LMXL, LMYF, LMYL
   use swan_service_interfaces, only: MSGERR
   implicit none(type, external)
   private

   public :: check_frequency_range

   real, parameter :: REQUIRED_RATIO = 2.5
   integer, parameter :: WARN_PERCENT = 1

contains

subroutine check_frequency_range(ac2, spcsig, depth, kgrpnt)
   real, intent(in) :: ac2(:,:,:) ! action density (MDC,MSC,MCGRD)
   real, intent(in) :: spcsig(:) ! relative frequencies (MSC)
   real, intent(in) :: depth(:) ! water depth per grid point (MCGRD)
   integer, intent(in) :: kgrpnt(:,:) ! grid point index (MXC,MYC)

   integer :: ix, iy, ip, is, ipeak, ix1, ix2, iy1, iy2
   integer :: counts(3) ! points with energy, too short a grid, peak at the top
   real :: energy, largest
   character(len=240) :: message

   ix1 = 1
   if (.not.LMXF) ix1 = 1 + IHALOX
   ix2 = MXC
   if (.not.LMXL) ix2 = MXC - IHALOX
   iy1 = 1
   if (.not.LMYF) iy1 = 1 + IHALOY
   iy2 = MYC
   if (.not.LMYL) iy2 = MYC - IHALOY

   counts = 0
   do iy = iy1, iy2
      do ix = ix1, ix2
         ip = kgrpnt(ix,iy)
         if (ip <= 1) cycle
         if (.not. depth(ip) > DEPMIN) cycle
         ipeak = 0
         largest = 0.
         do is = 1, MSC
            energy = spcsig(is) * sum(ac2(:,is,ip))
            if (energy > largest) then
               largest = energy
               ipeak = is
            end if
         end do
         if (ipeak == 0) cycle
         counts(1) = counts(1) + 1
         if (spcsig(MSC) < REQUIRED_RATIO * spcsig(ipeak)) counts(2) = counts(2) + 1
         if (ipeak == MSC) counts(3) = counts(3) + 1
      end do
   end do
   call SWREDUCE(counts, 3, SWSUM)

   if (counts(1) == 0) return
   if (100 * counts(2) < WARN_PERCENT * counts(1)) return
   write (message, '(a,i0,a,i0,a,i0,a)') &
      'frequency grid too short: at ', counts(2), ' of ', counts(1), &
      ' wet grid points [fhigh] is less than 2.5 times the peak frequency, and in ', &
      counts(3), ' the peak lies at [fhigh] itself; the manual asks for 2.5 to 3 times'
   call MSGERR(1, trim(message))
end subroutine check_frequency_range

end module swan_frequency_range
