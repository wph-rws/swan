module swan_math_constants
!
!     The circular constant and what SWAN derives from it.
!
!     These three were assigned once in SWINIT and never again, but they were
!     declared as mutable module variables in SWCOMM3 among the physical
!     settings that the SET command really does change. Sixty-two references
!     across the sources read them.
!
!     As parameters they are what they always were -- constants -- and the
!     compiler can fold them. It also means one fewer piece of run state a
!     reader has to establish the lifetime of.
!
   implicit none(type, external)
   private

   public :: PI, PI2, DEGRAD

!     PI     : the circular constant
!     PI2    : 2*PI, the length of a full circle in radians
!     DEGRAD : PI/180, the factor converting degrees to radians
   real, parameter :: PI = 4.0 * atan(1.0)
   real, parameter :: PI2 = 2.0 * PI
   real, parameter :: DEGRAD = PI / 180.0
end module swan_math_constants
