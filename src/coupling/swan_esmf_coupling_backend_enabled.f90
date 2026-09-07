module swan_esmf_coupling_backend
   use M_GENARR, only: SAVE_SINBAC, SINBAC
   implicit none(type, external)
   private

   logical, parameter, public :: esmf_coupling_enabled = .true.
   public :: accumulate_exponential_wind_input
   public :: reset_exponential_wind_input

contains

   subroutine accumulate_exponential_wind_input(direction, frequency, &
      point, value)
      integer, intent(in) :: direction, frequency, point
      real, intent(in) :: value

      if (SAVE_SINBAC) SINBAC(direction,frequency,point) = &
         SINBAC(direction,frequency,point) + value
   end subroutine accumulate_exponential_wind_input

   subroutine reset_exponential_wind_input(point)
      integer, intent(in) :: point

      if (SAVE_SINBAC) SINBAC(:,:,point) = 0.0
   end subroutine reset_exponential_wind_input
end module swan_esmf_coupling_backend
