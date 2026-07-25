module swan_integer_powers
!
!     Integer powers above four, written out as products in the order GCC's
!     optimising expansion of X**N uses.
!
   implicit none(type, external)
   private

   public :: power9

contains

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

end module swan_integer_powers
