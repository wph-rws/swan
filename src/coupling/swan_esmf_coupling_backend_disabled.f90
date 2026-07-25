module swan_esmf_coupling_backend
   implicit none(type, external)
   private

   logical, parameter, public :: esmf_coupling_enabled = .false.
   public :: accumulate_exponential_wind_input
   public :: reset_exponential_wind_input

contains

   subroutine accumulate_exponential_wind_input(direction, frequency, &
      point, value)
      integer, intent(in) :: direction, frequency, point
      real, intent(in) :: value

      if (esmf_coupling_enabled .and. direction + frequency + point > 0 &
         .and. value > huge(value)) error stop
   end subroutine accumulate_exponential_wind_input

   subroutine reset_exponential_wind_input(point)
      integer, intent(in) :: point

      if (esmf_coupling_enabled .and. point > 0) error stop
   end subroutine reset_exponential_wind_input
end module swan_esmf_coupling_backend
