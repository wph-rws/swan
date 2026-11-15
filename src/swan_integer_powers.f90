module swan_integer_powers
!
!     Integer powers above four, written out as products in the order GCC's
!     optimising expansion of X**N uses.
!
!     For a constant N the optimiser expands X**N into an addition chain of
!     multiplications. Unoptimised code (-O0) does not; it calls libgcc's
!     __powisf2, which multiplies by repeated squaring. For N = 5, 6 and 11
!     those are different sequences of products and they round differently in
!     the last bit: over a million single-precision arguments 35, 55 and 65 per
!     cent of the results differ, while -O1, -O2 and -O3 agree with one another
!     on every one. Spelled out, every optimisation level evaluates the same
!     products, so a -O0 build computes what the optimised builds compute and
!     the optimised builds keep their results bit for bit.
!
!     That last bit is not academic. In the nonstationary regular example the
!     fifth power in the JONSWAP shape of the boundary spectrum grew into a
!     Tm01 difference of 1.2e-3 by time step 5, which kept the Debug route from
!     reproducing the reference.
!
!     X**2 and X**3 need nothing: both methods form X*X and X*X2. For X**4
!     gfortran forms X2*X2 at every level, but another compiler need not:
!     LLVM flang 22 multiplies three times, ((X*X)*X)*X, and that rounds
!     differently. So a fourth power is written as (X*X)**2 where it stands,
!     which every compiler squares twice. With that, flang gives the output
!     of a gfortran build without the vector maths library on every example
!     deck (15 November 2026); for gfortran nothing changes. The Intel
!     compiler expands X**5 and X**11 by repeated squaring, as -O0 does, so
!     with it this module is also what makes the result one and the same.
!
   implicit none(type, external)
   private

   public :: power5, power6, power9, power11

contains

elemental real function power5(x)
   real, intent(in) :: x
   real :: x2

   x2 = x * x
   power5 = (x * x2) * x2
end function power5

elemental real function power6(x)
   real, intent(in) :: x
   real :: x3

   x3 = x * (x * x)
   power6 = x3 * x3
end function power6

 ! The MDIA coefficients are scaled by (2*PI)**9. Upstream evaluates that at
 ! run time because its PI is a variable; with PI a named constant here the
 ! front end would fold it exactly rounded instead, one bit above the run-time
 ! product. A stationary MDIA iteration amplifies that bit into a different
 ! number of iterations, so the factor is formed the way the optimiser forms
 ! it: x**9 = x**6 * x**3.
elemental real function power9(x)
   real, intent(in) :: x
   real :: x3

   x3 = x * (x * x)
   power9 = (x3 * x3) * x3
end function power9

elemental real function power11(x)
   real, intent(in) :: x
   real :: x2, x3

   x2 = x * x
   x3 = x * x2
   power11 = (x3 * x2) * (x3 * x3)
end function power11

end module swan_integer_powers
