module swan_netcdf_output_backend
   use swn_outnc, only: NCOFFSET, STNAMES, stnames_init, &
      swn_outnc_appendblock, &
      swn_outnc_close_on_end, swn_outnc_colspc, swn_outnc_openblockfile, &
      swn_outnc_spec
   implicit none(type, external)
   private

   logical, parameter, public :: netcdf_enabled = .true.
   public :: STNAMES, stnames_init
   public :: swn_outnc_appendblock, swn_outnc_close_on_end
   public :: swn_outnc_colspc, swn_outnc_openblockfile, swn_outnc_spec
   public :: is_netcdf_filename, netcdf_block_file_needs_open

contains

   pure logical function is_netcdf_filename(filename)
      character(len=*), intent(in) :: filename

      is_netcdf_filename = index(filename, '.NC') /= 0 .or. &
         index(filename, '.nc') /= 0
   end function is_netcdf_filename

   pure logical function netcdf_block_file_needs_open(irq)
      integer, intent(in) :: irq

      netcdf_block_file_needs_open = NCOFFSET(irq) == 0
   end function netcdf_block_file_needs_open

end module swan_netcdf_output_backend
