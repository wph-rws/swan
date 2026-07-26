module swan_wave_physics
   use swan_kinds, only: swan_real
   use swan_service_interfaces, only: strace
   use OCPCOMM4, only: LTRACE
   use SWCOMM3, only: GRAV, PMUD
   implicit none(type, external)
   private

   public :: kscip1, kscip2

contains

   subroutine kscip1(frequency_count, frequencies, depth, wave_number, &
                     group_velocity, group_number, group_number_depth_derivative)
      integer, intent(in) :: frequency_count
      real(swan_real), intent(in) :: frequencies(frequency_count), depth
      real(swan_real), intent(out) :: wave_number(frequency_count)
      real(swan_real), intent(out), optional :: group_velocity(frequency_count)
      real(swan_real), intent(out), optional :: group_number(frequency_count)
      real(swan_real), intent(out), optional :: &
         group_number_depth_derivative(frequency_count)

      integer, save :: entry_count = 0
      integer :: frequency_index
      real(swan_real) :: c, fac1, fac2, fac3, group_number_value
      real(swan_real) :: group_velocity_value, knd, nd_value
      real(swan_real) :: root_depth_over_gravity, snd, snd2
      real(swan_real) :: sqrt_gravity_depth

      if (LTRACE) call strace(entry_count, 'KSCIP1')

      root_depth_over_gravity = sqrt(depth / GRAV)
      sqrt_gravity_depth = root_depth_over_gravity * GRAV

      do frequency_index = 1, frequency_count
         snd = frequencies(frequency_index) * root_depth_over_gravity
         if (snd >= 2.5_swan_real) then
            wave_number(frequency_index) = &
               frequencies(frequency_index)**2 / GRAV
            group_velocity_value = &
               0.5_swan_real * GRAV / frequencies(frequency_index)
            group_number_value = 0.5_swan_real
            nd_value = 0.0_swan_real
         else if (snd < 1.0e-6_swan_real) then
            wave_number(frequency_index) = snd / depth
            group_velocity_value = sqrt_gravity_depth
            group_number_value = 1.0_swan_real
            nd_value = 0.0_swan_real
         else
            snd2 = snd * snd
            c = sqrt(GRAV * depth / &
               (snd2 + 1.0_swan_real / &
               (1.0_swan_real + 0.666_swan_real * snd2 + &
                0.445_swan_real * snd2**2 - 0.105_swan_real * snd2**3 + &
                0.272_swan_real * snd2**4)))
            wave_number(frequency_index) = frequencies(frequency_index) / c
            knd = wave_number(frequency_index) * depth
            fac1 = 2.0_swan_real * knd / sinh(2.0_swan_real * knd)
            group_number_value = 0.5_swan_real * (1.0_swan_real + fac1)
            group_velocity_value = group_number_value * c
            fac2 = snd2 / knd
            fac3 = 2.0_swan_real * fac2 / (1.0_swan_real + fac2 * fac2)
            fac2 = -wave_number(frequency_index) * &
               (2.0_swan_real * group_number_value - 1.0_swan_real) / &
               (2.0_swan_real * depth * group_number_value)
            nd_value = fac1 * &
               (0.5_swan_real / depth - wave_number(frequency_index) / fac3 + &
                fac2 * (0.5_swan_real / wave_number(frequency_index) - &
                        depth / fac3))
         end if

         if (present(group_velocity)) then
            group_velocity(frequency_index) = group_velocity_value
         end if
         if (present(group_number)) then
            group_number(frequency_index) = group_number_value
         end if
         if (present(group_number_depth_derivative)) then
            group_number_depth_derivative(frequency_index) = nd_value
         end if
      end do
   end subroutine kscip1

   subroutine kscip2(frequency_count, frequencies, depth, wave_number, &
                     group_velocity, group_number, group_number_depth_derivative, &
                     mud_dissipation, mud_depth)
      integer, intent(in) :: frequency_count
      real(swan_real), intent(in) :: frequencies(frequency_count), depth, mud_depth
      real(swan_real), intent(inout) :: wave_number(frequency_count)
      real(swan_real), intent(inout), optional :: group_velocity(frequency_count)
      real(swan_real), intent(inout), optional :: group_number(frequency_count)
      real(swan_real), intent(inout), optional :: &
         group_number_depth_derivative(frequency_count)
      real(swan_real), intent(inout), optional :: mud_dissipation(frequency_count)

      integer, save :: entry_count = 0
      integer :: frequency_index
      real(swan_real) :: c, density_ratio, dissipation_value, dtilde
      real(swan_real) :: fac1, fac2, fac3, group_number_value
      real(swan_real) :: group_velocity_value, kinematic_mud_viscosity
      real(swan_real) :: kinematic_water_viscosity, muddy_wave_number, knd
      real(swan_real) :: mud_boundary_layer, nd_value, viscosity_ratio

      if (LTRACE) call strace(entry_count, 'KSCIP2')

      kinematic_mud_viscosity = PMUD(3)
      kinematic_water_viscosity = PMUD(5)
      viscosity_ratio = sqrt(kinematic_mud_viscosity / kinematic_water_viscosity)
      density_ratio = PMUD(4) / PMUD(2)

      do frequency_index = 1, frequency_count
         knd = wave_number(frequency_index) * depth
         if (knd < 10.0_swan_real .and. mud_depth > 1.0e-5_swan_real) then
            mud_boundary_layer = &
               sqrt(2.0_swan_real * kinematic_mud_viscosity / &
                    frequencies(frequency_index))
            dtilde = mud_depth / mud_boundary_layer
            call muddy_dispersion( &
               depth, dtilde, viscosity_ratio, &
               mud_boundary_layer, density_ratio, wave_number(frequency_index), &
               muddy_wave_number, dissipation_value)
            wave_number(frequency_index) = muddy_wave_number

            knd = wave_number(frequency_index) * depth
            if (knd < 35.0_swan_real) then
               fac1 = 2.0_swan_real * knd / sinh(2.0_swan_real * knd)
            else
               fac1 = 2.0e-30_swan_real * knd
            end if
            group_number_value = 0.5_swan_real * (1.0_swan_real + fac1)
            c = frequencies(frequency_index) / wave_number(frequency_index)
            group_velocity_value = group_number_value * c
            fac2 = frequencies(frequency_index) * c / GRAV
            fac3 = 2.0_swan_real * fac2 / (1.0_swan_real + fac2 * fac2)
            fac2 = -wave_number(frequency_index) * &
               (2.0_swan_real * group_number_value - 1.0_swan_real) / &
               (2.0_swan_real * depth * group_number_value)
            nd_value = fac1 * &
               (0.5_swan_real / depth - wave_number(frequency_index) / fac3 + &
                fac2 * (0.5_swan_real / wave_number(frequency_index) - &
                        depth / fac3))

            if (present(group_velocity)) then
               group_velocity(frequency_index) = group_velocity_value
            end if
            if (present(group_number)) then
               group_number(frequency_index) = group_number_value
            end if
            if (present(group_number_depth_derivative)) then
               group_number_depth_derivative(frequency_index) = nd_value
            end if
            if (present(mud_dissipation)) then
               mud_dissipation(frequency_index) = dissipation_value
            end if
         else if (present(mud_dissipation)) then
            mud_dissipation(frequency_index) = 0.0_swan_real
         end if
      end do
   end subroutine kscip2

   subroutine muddy_dispersion(water_depth, normalized_mud_depth, &
                               viscosity_ratio, mud_boundary_layer, density_ratio, &
                               wave_number, muddy_wave_number, dissipation)
      real(swan_real), intent(in) :: water_depth, normalized_mud_depth
      real(swan_real), intent(in) :: viscosity_ratio, mud_boundary_layer
      real(swan_real), intent(in) :: density_ratio, wave_number
      real(swan_real), intent(out) :: muddy_wave_number, dissipation

      real(swan_real) :: b1, b2, b3, bip, br, brp, mud_depth

      mud_depth = normalized_mud_depth * mud_boundary_layer
      b1 = density_ratio * &
         (-2.0_swan_real * density_ratio**2 + 2.0_swan_real * density_ratio - &
          1.0_swan_real - viscosity_ratio**2) * sinh(normalized_mud_depth) * &
          cosh(normalized_mud_depth) - density_ratio**2 * viscosity_ratio * &
         (cosh(normalized_mud_depth)**2 + sinh(normalized_mud_depth)**2) - &
         (density_ratio - 1.0_swan_real)**2 * viscosity_ratio * &
         (cosh(normalized_mud_depth)**2 * cos(normalized_mud_depth)**2 + &
          sinh(normalized_mud_depth)**2 * sin(normalized_mud_depth)**2) - &
         2.0_swan_real * density_ratio * (1.0_swan_real - density_ratio) * &
         (viscosity_ratio * cosh(normalized_mud_depth) + &
          density_ratio * sinh(normalized_mud_depth)) * cos(normalized_mud_depth)
      b2 = density_ratio * &
         (-2.0_swan_real * density_ratio**2 + 2.0_swan_real * density_ratio - &
          1.0_swan_real + viscosity_ratio**2) * sin(normalized_mud_depth) * &
          cos(normalized_mud_depth) - 2.0_swan_real * density_ratio * &
         (1.0_swan_real - density_ratio) * &
         (viscosity_ratio * sinh(normalized_mud_depth) + &
          density_ratio * cosh(normalized_mud_depth)) * sin(normalized_mud_depth)
      b3 = (viscosity_ratio * cosh(normalized_mud_depth) + &
            density_ratio * sinh(normalized_mud_depth))**2 * &
           cos(normalized_mud_depth)**2 + &
           (viscosity_ratio * sinh(normalized_mud_depth) + &
            density_ratio * cosh(normalized_mud_depth))**2 * &
           sin(normalized_mud_depth)**2

      br = wave_number * mud_boundary_layer * (b1 - b2) / &
           (2.0_swan_real * b3) + density_ratio * wave_number * mud_depth
      brp = b1 / b3
      bip = b2 / b3

      dissipation = -mud_boundary_layer * (brp + bip) * wave_number**2 / &
         (sinh(2.0_swan_real * wave_number * water_depth) + &
          2.0_swan_real * wave_number * water_depth)
      muddy_wave_number = wave_number - br * wave_number / &
         (sinh(wave_number * water_depth) * cosh(wave_number * water_depth) + &
          wave_number * water_depth)

   end subroutine muddy_dispersion

end module swan_wave_physics
