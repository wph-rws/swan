module swan_service_interfaces
   implicit none
   private

   public :: eqreal
   public :: msgerr, stpnow, strace, txpbla

   interface
      subroutine msgerr(level, message, diag, io)
         use swan_io_context, only: diagnostics_context_t, io_context_t
         integer, intent(in)          :: level
         character(len=*), intent(in) :: message
         type(diagnostics_context_t), optional, intent(inout) :: diag
         type(io_context_t), optional, intent(in) :: io
      end subroutine msgerr

      subroutine strace(entry_count, routine_name, diag, io)
         use swan_io_context, only: diagnostics_context_t, io_context_t
         integer, intent(inout)       :: entry_count
         character(len=*), intent(in) :: routine_name
         type(diagnostics_context_t), optional, intent(in) :: diag
         type(io_context_t), optional, intent(in) :: io
      end subroutine strace

      subroutine txpbla(text, first, last)
         character(len=*), intent(inout) :: text
         integer, intent(out)             :: first, last
      end subroutine txpbla

      logical function eqreal(first, second)
         real, intent(in) :: first, second
      end function eqreal

      logical function stpnow(diag)
         use swan_io_context, only: diagnostics_context_t
         type(diagnostics_context_t), optional, intent(in) :: diag
      end function stpnow

   end interface
end module swan_service_interfaces
