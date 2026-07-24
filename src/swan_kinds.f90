module swan_kinds
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none
   private

   ! Preserve SWAN's established single-precision numerical model while
   ! giving declarations and literals an explicit, shared kind.
   integer, parameter, public :: swan_real = real32
   integer, parameter, public :: swan_double = real64
   integer, parameter, public :: swan_int = int32
   integer, parameter, public :: swan_long_int = int64
end module swan_kinds
