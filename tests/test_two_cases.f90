program test_two_cases
!  Run the same case twice in one process and check that the second run gives
!  the same answer as the first.
!
!  This is the acceptance test the modernisation is aimed at. SWAN was written
!  as a program that runs once and exits, so every module variable is allowed
!  to keep whatever it was left holding. A second run in the same process
!  exposes exactly the state that was never designed to be reset: counters that
!  only grow, allocatables that are already allocated, SAVE flags that latch,
!  units that are still open.
!
!  A difference here is not a rounding question. Both runs read the same deck
!  and do the same arithmetic, so any difference in the answer is state that
!  survived from the first run.
   use swan_driver, only: SWMAIN
   use swan_parallel, only: SWINITMPI, SWEXITMPI
   implicit none

   character(len=*), parameter :: DECK = 'INPUT'
   real :: first(4), second(4)
   integer :: status

   call write_bottom
   call write_deck

   ! Match the production driver's lifetime: the parallel runtime surrounds
   ! both SWAN runs and is initialized only once.
   call SWINITMPI

   call run_once(first, status)
   if (status /= 0) error stop "the first run did not produce a result table"

   call run_once(second, status)
   if (status /= 0) error stop "the second run did not produce a result table"

   call compare('depth', first(1), second(1))
   call compare('Hsig',  first(2), second(2))
   call compare('Tm01',  first(3), second(3))
   call compare('Dir',   first(4), second(4))

   call SWEXITMPI
   print *, 'two runs in one process agree'

contains

   subroutine run_once(values, ierr)
      real, intent(out)    :: values(4)
      integer, intent(out) :: ierr
      integer :: unit, dummy

      !  Remove the table first. Without this the reader happily returns the
      !  previous run's numbers when the second run fails to write its own,
      !  and the test passes by comparing the first run with itself.
      open(newunit=unit, file='two_cases.tbl', status='old', iostat=dummy)
      if (dummy == 0) close(unit, status='delete')

      call SWMAIN
      call read_table(values, ierr)
   end subroutine run_once

   !  The table has a three-line header and one row: XP YP DEPTH HSIGN TM01 DIR.
   subroutine read_table(values, ierr)
      real, intent(out)    :: values(4)
      integer, intent(out) :: ierr
      integer :: unit, i
      real    :: xp, yp
      character(len=200) :: line

      values = 0.0
      open(newunit=unit, file='two_cases.tbl', status='old', action='read', &
           iostat=ierr)
      if (ierr /= 0) return
      !  A TABLE with HEADER opens with seven comment lines; the data follows.
      do i = 1, 64
         read(unit, '(A)', iostat=ierr) line
         if (ierr /= 0) exit
         if (line(1:1) == '%' .or. len_trim(line) == 0) cycle
         read(line, *, iostat=ierr) xp, yp, values(1), values(2), values(3), &
                                    values(4)
         close(unit)
         return
      end do
      ierr = 1
      close(unit)
   end subroutine read_table

   subroutine compare(name, a, b)
      character(len=*), intent(in) :: name
      real, intent(in) :: a, b

      !  Bit patterns, not values: both runs do the same arithmetic on the same
      !  input, so anything but an identical result is state that survived.
      !  Comparing the bits also says that plainly and avoids an inexact-equality
      !  warning on a comparison that is meant to be exact.
      if (transfer(a, 1) /= transfer(b, 1)) then
         write(*, '(A,A,A,G16.8,A,G16.8)') &
            'second run differs in ', name, ': ', a, ' then ', b
         error stop "state survived from the first run"
      end if
   end subroutine compare

   subroutine write_bottom
      integer :: unit

      open(newunit=unit, file='bottom.bot', status='replace', action='write')
      write(unit, '(A)') '20.0 20.0'
      write(unit, '(A)') '20.0 20.0'
      close(unit)
   end subroutine write_bottom

   subroutine write_deck
      integer :: unit

      open(newunit=unit, file=DECK, status='replace', action='write')
      write(unit, '(A)') "PROJECT 'TWICE' '001'"
      write(unit, '(A)') 'SET NAUTICAL'
      write(unit, '(A)') 'MODE STATIONARY TWODIMENSIONAL'
      write(unit, '(A)') 'COORDINATES CARTESIAN'
      write(unit, '(A)') 'CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10 ' // &
                         'CIRCLE 24 0.05 1.0 16'
      write(unit, '(A)') 'INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0'
      write(unit, '(A)') "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
      write(unit, '(A)') 'WIND 8.0 270.0'
      write(unit, '(A)') 'BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES'
      write(unit, '(A)') 'BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0'
      write(unit, '(A)') 'GEN3 KOMEN'
      write(unit, '(A)') 'BREAKING CONSTANT 1.0 0.73'
      write(unit, '(A)') 'FRICTION JONSWAP CONSTANT 0.038'
      write(unit, '(A)') "POINTS 'CENTER' 1000.0 500.0"
      write(unit, '(A)') "TABLE 'CENTER' HEADER 'two_cases.tbl' XP YP " // &
                         'DEPTH HSIGN TM01 DIR'
      write(unit, '(A)') 'COMPUTE'
      write(unit, '(A)') 'STOP'
      close(unit)
   end subroutine write_deck

end program test_two_cases
