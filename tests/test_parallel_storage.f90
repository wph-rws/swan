program test_parallel_storage
   use M_PARALL, only: IBLKAD, CLEAR_PARALLEL_STORAGE, &
      PARALLEL_STORAGE_IS_CLEAR
   implicit none(type, external)

   call CLEAR_PARALLEL_STORAGE()
   call require(PARALLEL_STORAGE_IS_CLEAR(), 'initial state is not clear')

   allocate(IBLKAD(4))

   call require(.not.PARALLEL_STORAGE_IS_CLEAR(), &
      'filled state was reported as clear')
   call CLEAR_PARALLEL_STORAGE()
   call require(PARALLEL_STORAGE_IS_CLEAR(), 'clear left storage behind')
   call CLEAR_PARALLEL_STORAGE()
   call require(PARALLEL_STORAGE_IS_CLEAR(), 'second clear changed empty state')

   print *, 'parallel storage clear contract passes'

contains

   subroutine require(condition, message)
      logical, intent(in) :: condition
      character(len=*), intent(in) :: message

      if (.not.condition) error stop message
   end subroutine require
end program test_parallel_storage
