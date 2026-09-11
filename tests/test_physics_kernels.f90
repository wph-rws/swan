program test_physics_kernels
!  Targeted fixtures for numerical kernels that sit underneath every result.
!  These are checked against physics and mathematics rather than against stored
!  numbers, so they stay meaningful if the implementation is ever rewritten:
!  a run that shifts by a fraction of a percent shows up here as a broken
!  identity instead of as a moved figure at the end of a full simulation.
   use swan_wave_physics, only: kscip1, kscip1_kernel
   use swan_spectrum_transform, only: gammaf, gammaf_kernel
   use swan_geometry, only: tcross, tcross_kernel
   use swan_physical_settings, only: GRAV
   implicit none

   call test_deep_water_limit
   call test_shallow_water_limit
   call test_dispersion_relation
   call test_group_velocity_identity
   call test_gamma_function
   call test_line_crossing
   call test_pure_kernel_equivalence

contains

!  In deep water the dispersion relation collapses to k = sigma^2/g, the group
!  velocity to half the phase speed and the group number to one half.
   subroutine test_deep_water_limit
      integer, parameter :: n = 3
      real :: sigma(n), k(n), cg(n), ratio(n), depth, saved_grav

      saved_grav = GRAV
      GRAV = 9.81

      sigma = [1.5, 2.0, 2.5]
      depth = 5000.0                       ! effectively infinite
      call kscip1(n, sigma, depth, k, cg, ratio)

      call require(close(k(1), sigma(1)**2 / GRAV, 1.0e-4), &
         "deep-water wave number is not sigma^2/g")
      call require(close(cg(1), 0.5 * GRAV / sigma(1), 1.0e-4), &
         "deep-water group velocity is not g/(2 sigma)")
      call require(all(abs(ratio - 0.5) < 1.0e-5), &
         "deep-water group number is not one half")

      GRAV = saved_grav
   end subroutine test_deep_water_limit

!  In shallow water waves are non-dispersive: every component travels at
!  sqrt(g d) and the group number is one.
   subroutine test_shallow_water_limit
      integer, parameter :: n = 2
      real :: sigma(n), k(n), cg(n), ratio(n), depth, saved_grav

      saved_grav = GRAV
      GRAV = 9.81

      sigma = [1.0e-8, 2.0e-8]             ! long waves on a shallow bottom
      depth = 2.0
      call kscip1(n, sigma, depth, k, cg, ratio)

      call require(all(abs(cg - sqrt(GRAV * depth)) < 1.0e-4), &
         "shallow-water group velocity is not sqrt(g d)")
      call require(all(abs(ratio - 1.0) < 1.0e-5), &
         "shallow-water group number is not one")

      GRAV = saved_grav
   end subroutine test_shallow_water_limit

!  Across the whole intermediate range the result has to satisfy
!  sigma^2 = g k tanh(k d). KSCIP1 does not solve that implicitly; it uses an
!  explicit approximation, which is why the tolerance is 0.3% rather than
!  round-off. The measured worst case over this range is 0.18%, so a real
!  regression in the fit shows up while the approximation itself passes.
   subroutine test_dispersion_relation
      integer, parameter :: n = 60
      real :: sigma(n), k(n), depth, residual, saved_grav
      integer :: i, j

      saved_grav = GRAV
      GRAV = 9.81

      do i = 1, n
         sigma(i) = 0.05 + 0.05 * real(i)
      end do

      do j = 1, 40
         depth = 0.5 * real(j)
         call kscip1(n, sigma, depth, k)
         do i = 1, n
            residual = abs(sigma(i)**2 - GRAV * k(i) * tanh(k(i) * depth)) &
                       / sigma(i)**2
            call require(residual < 3.0e-3, &
               "wave number does not satisfy the dispersion relation")
         end do
      end do

      GRAV = saved_grav
   end subroutine test_dispersion_relation

!  The group velocity, wave number and group number are not independent:
!  cg = n c and c = sigma/k, so cg k = n sigma must hold exactly in every
!  branch. This catches a branch that updates one of the three without the
!  others, which the dispersion check above would not notice.
   subroutine test_group_velocity_identity
      integer, parameter :: n = 40
      real :: sigma(n), k(n), cg(n), ratio(n), depth, saved_grav
      integer :: i, j

      saved_grav = GRAV
      GRAV = 9.81

      do i = 1, n
         sigma(i) = 0.05 + 0.1 * real(i)
      end do

      do j = 1, 30
         depth = 0.4 * real(j)
         call kscip1(n, sigma, depth, k, cg, ratio)
         do i = 1, n
            call require(abs(cg(i) * k(i) - ratio(i) * sigma(i)) &
                         <= 1.0e-5 * abs(ratio(i) * sigma(i)), &
               "cg, k and n are mutually inconsistent")
         end do
      end do

      GRAV = saved_grav
   end subroutine test_group_velocity_identity

!  Gamma is pinned on values that follow from its definition rather than on
!  output of the routine itself: Gamma(n) = (n-1)! and Gamma(1/2) = sqrt(pi).
   subroutine test_gamma_function
      call require(close(gammaf(1.0), 1.0, 1.0e-4), "Gamma(1) is not 1")
      call require(close(gammaf(4.0), 6.0, 1.0e-3), "Gamma(4) is not 3!")
      call require(close(gammaf(5.0), 24.0, 1.0e-2), "Gamma(5) is not 4!")
      call require(close(gammaf(0.5), sqrt(4.0 * atan(1.0)), 1.0e-4), &
         "Gamma(1/2) is not sqrt(pi)")
   end subroutine test_gamma_function

!  TCROSS decides whether a grid link crosses an obstacle side. Getting this
!  wrong makes obstacles leak or block too much, which is invisible in a
!  gross wave height but wrong locally.
   subroutine test_line_crossing
      logical :: on_obstacle

      ! Clearly crossing: the link runs through the middle of the side.
      call require(tcross(0.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.0, 2.0, on_obstacle), &
         "TCROSS missed two segments that plainly cross")

      ! Clearly apart: the link stops well before the side.
      call require(.not. tcross(0.0, 1.0, 5.0, 5.0, 0.0, 0.0, 0.0, 2.0, &
                                on_obstacle), &
         "TCROSS reported a crossing between separated segments")

      ! Parallel and offset: no crossing however far they run.
      call require(.not. tcross(0.0, 4.0, 0.0, 4.0, 0.0, 0.0, 1.0, 1.0, &
                                on_obstacle), &
         "TCROSS reported a crossing between parallel segments")
   end subroutine test_line_crossing

!  The public legacy entries retain tracing and shared compatibility state.
!  Their mathematical results must remain identical to the pure kernels.
   subroutine test_pure_kernel_equivalence
      integer, parameter :: n = 5
      real :: sigma(n), wrapper_k(n), kernel_k(n)
      real :: wrapper_cg(n), kernel_cg(n), wrapper_n(n), kernel_n(n)
      real :: saved_grav
      logical :: wrapper_on_obstacle, kernel_on_obstacle
      logical :: wrapper_crossing, kernel_crossing

      saved_grav = GRAV
      GRAV = 9.81
      sigma = [0.1, 0.5, 1.0, 2.0, 4.0]

      call kscip1(n, sigma, 7.5, wrapper_k, wrapper_cg, wrapper_n)
      call kscip1_kernel(n, sigma, 7.5, GRAV, kernel_k, kernel_cg, kernel_n)
      call require(all(same_bits(wrapper_k, kernel_k)), &
         "KSCIP1 wrapper differs from its pure kernel")
      call require(all(same_bits(wrapper_cg, kernel_cg)), &
         "KSCIP1 group velocity differs from its pure kernel")
      call require(all(same_bits(wrapper_n, kernel_n)), &
         "KSCIP1 group number differs from its pure kernel")

      call require(same_bits(gammaf(0.75), gammaf_kernel(0.75)), &
         "GAMMAF wrapper differs from its pure kernel")

      wrapper_crossing = tcross(0.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.0, 2.0, &
                                wrapper_on_obstacle)
      call tcross_kernel(0.0, 2.0, 1.0, 1.0, 1.0, 1.0, 0.0, 2.0, &
                         kernel_crossing, kernel_on_obstacle)
      call require(wrapper_crossing .eqv. kernel_crossing, &
         "TCROSS wrapper differs from its pure kernel")
      call require(wrapper_on_obstacle .eqv. kernel_on_obstacle, &
         "TCROSS endpoint classification differs from its pure kernel")

      GRAV = saved_grav
   end subroutine test_pure_kernel_equivalence

   logical function close(actual, expected, tolerance)
      real, intent(in) :: actual, expected, tolerance
      close = abs(actual - expected) <= tolerance * max(abs(expected), 1.0)
   end function close

   elemental logical function same_bits(actual, expected)
      real, intent(in) :: actual, expected

      same_bits = transfer(actual, 0) == transfer(expected, 0)
   end function same_bits

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not. condition) error stop message
   end subroutine require

end program test_physics_kernels
