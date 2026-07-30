program check_netcdf_output
   use netcdf
   implicit none

   character(len=1024) :: filename
   character(len=64) :: expected_argument
   character(len=16) :: grid_kind
   character(len=32) :: attribute
   integer :: ncid, variable, dimension, input_status
   integer :: nx, ny, raw_hsig(1)
   real :: scale_factor, add_offset, center_hsig, expected_hsig

   if (command_argument_count() /= 3) then
      error stop 'usage: check_netcdf_output FILE EXPECTED_HSIG map|point'
   end if
   call get_command_argument(1, filename)
   call get_command_argument(2, expected_argument)
   call get_command_argument(3, grid_kind)
   read (expected_argument, *, iostat=input_status) expected_hsig
   if (input_status /= 0) error stop 'invalid expected Hsig'

   call check(nf90_open(trim(filename), NF90_NOWRITE, ncid), 'open')
   call check(nf90_get_att(ncid, NF90_GLOBAL, 'Conventions', attribute), &
      'read Conventions')
   if (trim(attribute) /= 'CF-1.5') error stop 'unexpected Conventions attribute'
   attribute = ''
   call check(nf90_get_att(ncid, NF90_GLOBAL, 'project', attribute), &
      'read project')
   if (trim(attribute) /= 'QUICK') error stop 'unexpected project attribute'

   call check(nf90_inq_varid(ncid, 'hs', variable), 'find hs variable')
   call check(nf90_get_att(ncid, variable, 'scale_factor', scale_factor), &
      'read hs scale_factor')
   call check(nf90_get_att(ncid, variable, 'add_offset', add_offset), &
      'read hs add_offset')
   if (trim(grid_kind) == 'map') then
      call check(nf90_inq_dimid(ncid, 'x', dimension), 'find x dimension')
      call check(nf90_inquire_dimension(ncid, dimension, len=nx), 'read x dimension')
      call check(nf90_inq_dimid(ncid, 'y', dimension), 'find y dimension')
      call check(nf90_inquire_dimension(ncid, dimension, len=ny), 'read y dimension')
      if (nx /= 21 .or. ny /= 11) error stop 'unexpected netCDF grid dimensions'
      call check(nf90_get_var(ncid, variable, raw_hsig, &
         start=(/11, 6, 1/), count=(/1, 1, 1/)), 'read centre hs')
   else if (trim(grid_kind) == 'point') then
      call check(nf90_inq_dimid(ncid, 'points', dimension), 'find points dimension')
      call check(nf90_inquire_dimension(ncid, dimension, len=nx), 'read points dimension')
      if (nx /= 1) error stop 'unexpected netCDF point count'
      ny = 1
      call check(nf90_get_var(ncid, variable, raw_hsig, &
         start=(/1, 1/), count=(/1, 1/)), 'read point hs')
   else
      error stop 'grid kind must be map or point'
   end if
   center_hsig = real(raw_hsig(1)) * scale_factor + add_offset
   if (abs(center_hsig - expected_hsig) > 5.e-4) then
      write (*, '(a,f10.6)') 'unexpected centre Hsig: ', center_hsig
      error stop 'netCDF data differs from the quick-test reference'
   end if
   call check(nf90_close(ncid), 'close')
   write (*, '(a,i0,a,i0,a,f8.5,a)') &
      'netCDF hs grid is ', nx, 'x', ny, '; centre=', center_hsig, ' m'

contains

   subroutine check(status, operation)
      integer, intent(in) :: status
      character(len=*), intent(in) :: operation

      if (status /= NF90_NOERR) then
         write (*, '(a,2a)') trim(operation), ': ', trim(nf90_strerror(status))
         error stop 'netCDF validation failed'
      end if
   end subroutine check

end program check_netcdf_output
