module swan_file_open_backend
   implicit none(type, external)
   private
   public :: open_swan_file

contains

   subroutine open_swan_file(unit, iostat, filename, status, access, form, &
                             record_length)
      integer, intent(in) :: unit
      integer, intent(out) :: iostat
      character(len=*), optional, intent(in) :: filename
      character(len=*), optional, intent(in) :: status, access, form
      integer, optional, intent(in) :: record_length
      character(len=11) :: open_status, open_access, open_form
      integer :: recl

      open_status = 'UNKNOWN'
      open_access = 'SEQUENTIAL'
      open_form = 'FORMATTED'
      recl = 0
      if (present(status)) open_status = status
      if (present(access)) open_access = access
      if (present(form)) open_form = form
      if (present(record_length)) recl = record_length

      if (present(filename)) then
         if (recl > 0) then
            open(unit=unit, iostat=iostat, file=filename, status=open_status, &
                 access=open_access, form=open_form, recl=recl)
         else
            open(unit=unit, iostat=iostat, file=filename, status=open_status, &
                 access=open_access, form=open_form)
         end if
      else
         if (recl > 0) then
            open(unit=unit, iostat=iostat, status=open_status, &
                 access=open_access, form=open_form, recl=recl)
         else
            open(unit=unit, iostat=iostat, status=open_status, &
                 access=open_access, form=open_form)
         end if
      end if
   end subroutine open_swan_file

end module swan_file_open_backend
