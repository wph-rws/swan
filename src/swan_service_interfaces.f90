module swan_service_interfaces
   implicit none
   private

   public :: eqreal
   public :: msgerr, stpnow, strace, txpbla

   interface
      subroutine msgerr(level, message)
         integer, intent(in)          :: level
         character(len=*), intent(in) :: message
      end subroutine msgerr

      subroutine strace(entry_count, routine_name)
         integer, intent(inout)       :: entry_count
         character(len=*), intent(in) :: routine_name
      end subroutine strace

      subroutine txpbla(text, first, last)
         character(len=*), intent(inout) :: text
         integer, intent(out)             :: first, last
      end subroutine txpbla

      logical function eqreal(first, second)
         real, intent(in) :: first, second
      end function eqreal

      logical function stpnow()
      end function stpnow

   end interface
end module swan_service_interfaces
