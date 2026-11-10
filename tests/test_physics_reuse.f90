program test_physics_reuse
!  Serial reusability with changing physics.
!  B uses the 41.51 default (no GEN line), A uses GEN3 KOMEN. B-A-B-A
!  must reproduce both fresh choices, including explicit whitecapping,
!  boundary output and reading wind before bottom, while B
!  demonstrably computed different physics from A. Every SWMAIN return must
!  also have left all owners empty, as in
!  test_two_cases.
!
!  Two decks then repeat on their own. A nonstationary deck whose time step is
!  too large for the higher-order scheme raises a level-2 error and must stop
!  at the same time in every run; a stationary deck with BREAKING BKD, which
!  switches off its varying breaker index once the iteration is accurate
!  enough, must give the same answer in every run. Both decisions were
!  remembered by the process, so a second run computed on, or used a constant
!  breaker index. Each nonstationary run must also write its error to
!  Errfile; SWMAIN closes that file at the end of a run, and a second run
!  wrote the error to fort.17 instead.
!
!  This explicitly does not yet prove alternating use of two instances
!  existing at once, nor thread safety, nor changing
!  grids/spectral dimensions (next step).
   use swan_driver, only: SWMAIN
   use swan_parallel, only: SWINITMPI, SWEXITMPI
   use M_OBSTA, only: OBSTACLE_STATE_IS_CLEAR
   use M_BNDSPEC, only: BOUNDARY_STATE_IS_CLEAR
   use OUTP_DATA, only: OUTPUT_STATE_IS_CLEAR
   use M_GENARR, only: GENERAL_ARRAY_STATE_IS_CLEAR
   use swan_input_fields, only: INPUT_FIELDS_ARE_CLEAR
   use swan_vegetation_layers, only: VEGETATION_LAYERS_ARE_CLEAR
   use swan_global_grid, only: GLOBAL_GRID_IS_CLEAR
   use M_PARALL, only: PARALLEL_STORAGE_IS_CLEAR
   use SwanGriddata, only: GRID_DATA_IS_CLEAR
   use SwanCompdata, only: COMPUTATION_DATA_IS_CLEAR
   use SwanIEM, only: IEM_STATE_IS_CLEAR
   use SwanBraggScat, only: BRAGG_STATE_IS_CLEAR
   use SwanQCM, only: QCM_STATE_IS_CLEAR
   use, intrinsic :: iso_fortran_env, only: error_unit
   implicit none

   character(len=*), parameter :: DECK = 'INPUT'
   real :: first(4), middle(4), third(4), fourth(4)
   integer :: status, scenario, rows(3), run

   call write_bottom

   ! Match the production driver's lifetime: the parallel runtime surrounds
   ! all SWAN runs and is initialized only once.
   call SWINITMPI

   do scenario = 0, 3
      ! B starts with the default physics. It must also give that answer
      ! after A explicitly selects Komen. A-B-A alone cannot show this leak.
      call run_once(.false., first, status)
      if (status /= 0) error stop "the first B did not produce a table"
      call run_once(.true., middle, status)
      if (status /= 0) error stop "the first A did not produce a table"
      call run_once(.false., third, status)
      if (status /= 0) error stop "the second B did not produce a table"
      call run_once(.true., fourth, status)
      if (status /= 0) error stop "the second A did not produce a table"
      call compare('B depth', first(1), third(1))
      call compare('B Hsig', first(2), third(2))
      call compare('B Tm01', first(3), third(3))
      call compare('B Dir', first(4), third(4))
      call compare('A Hsig', middle(2), fourth(2))
      call compare('A Tm01', middle(3), fourth(3))
      if (all(transfer(first, [1, 1, 1, 1]) == &
              transfer(middle, [1, 1, 1, 1]))) then
         error stop "the contrasting physics did not change the answer"
      end if
   end do

   ! The nonstationary deck runs to 00:30 in 10-minute steps, so a run that
   ! does not stop writes four times. With the default [maxerr] the CFL error
   ! must stop every run at the same time, before the end.
   do run = 1, 3
      call run_nonstationary(rows(run))
   end do
   if (any(rows /= rows(1))) then
      ! SWAN closes unit 6 at the end of a run, so report on error_unit.
      write(error_unit, '(A,3I3)') ' times written by the three nonstationary runs:', rows
      error stop "the CFL error did not stop every nonstationary run alike"
   end if
   if (rows(1) >= 4) error stop "the CFL error did not stop the nonstationary run"

   ! The BKD deck: the second run must repeat the first.
   scenario = 4
   call run_once(.false., first, status)
   if (status /= 0) error stop "the first BKD run did not produce a table"
   call run_once(.false., third, status)
   if (status /= 0) error stop "the second BKD run did not produce a table"
   call compare('BKD Hsig', first(2), third(2))
   call compare('BKD Tm01', first(3), third(3))

   call SWEXITMPI
   print *, 'B-A-B-A physics runs agree in all four scenarios, the ', &
            'nonstationary and BKD runs repeat, and state is clear'

contains

subroutine run_once(with_komen, values, ierr)
   logical, intent(in)     :: with_komen
   real, intent(out)    :: values(4)
   integer, intent(out) :: ierr
   integer :: unit, dummy

   !  Remove the table first. Without this the reader happily returns the
   !  previous run's numbers when a run fails to write its own, and the
   !  test passes by comparing a run with itself.
   open(newunit=unit, file='physics_reuse.tbl', status='old', iostat=dummy)
   if (dummy == 0) close(unit, status='delete')

   call write_deck(with_komen)
   call SWMAIN
   call check_clear
   call read_table(values, ierr)
end subroutine run_once

 !  Run the nonstationary deck and count the times its table holds.
subroutine run_nonstationary(times)
   integer, intent(out) :: times
   integer :: unit, ierr
   logical :: found
   character(len=200) :: line

   call remove('nonstat_reuse.tbl')
   call remove('Errfile')
   call remove('fort.17')
   call write_nonstationary_deck
   call SWMAIN
   call check_clear

   ! The CFL error is above [maxerr], so it must be in this run's Errfile.
   inquire(file='fort.17', exist=found)
   if (found) error stop "the error message went to fort.17"
   found = .false.
   open(newunit=unit, file='Errfile', status='old', action='read', iostat=ierr)
   if (ierr == 0) then
      do
         read(unit, '(A)', iostat=ierr) line
         if (ierr /= 0) exit
         if (index(line, 'CFL greater than 10') > 0) found = .true.
      end do
      close(unit)
   end if
   if (.not.found) error stop "the CFL error is missing from this run's Errfile"

   times = 0
   open(newunit=unit, file='nonstat_reuse.tbl', status='old', action='read', &
        iostat=ierr)
   if (ierr /= 0) return
   do
      read(unit, '(A)', iostat=ierr) line
      if (ierr /= 0) exit
      if (line(1:1) == '%' .or. len_trim(line) == 0) cycle
      times = times + 1
   end do
   close(unit)
end subroutine run_nonstationary

subroutine remove(name)
   character(len=*), intent(in) :: name
   integer :: unit, ierr

   open(newunit=unit, file=name, status='old', iostat=ierr)
   if (ierr == 0) close(unit, status='delete')
end subroutine remove

 !  Every SWMAIN return must have left all owners empty.
subroutine check_clear
   if (.not.OBSTACLE_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live obstacle state"
   end if
   if (.not.BOUNDARY_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live boundary state"
   end if
   if (.not.OUTPUT_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live output state"
   end if
   if (.not.GENERAL_ARRAY_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live general array state"
   end if
   if (.not.INPUT_FIELDS_ARE_CLEAR()) then
      error stop "SWMAIN returned with live input fields"
   end if
   if (.not.VEGETATION_LAYERS_ARE_CLEAR()) then
      error stop "SWMAIN returned with live vegetation state"
   end if
   if (.not.GLOBAL_GRID_IS_CLEAR()) then
      error stop "SWMAIN returned with live global grid"
   end if
   if (.not.PARALLEL_STORAGE_IS_CLEAR()) then
      error stop "SWMAIN returned with live parallel storage"
   end if
   if (.not.GRID_DATA_IS_CLEAR()) then
      error stop "SWMAIN returned with live grid data"
   end if
   if (.not.COMPUTATION_DATA_IS_CLEAR()) then
      error stop "SWMAIN returned with live computation data"
   end if
   if (.not.IEM_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live IEM state"
   end if
   if (.not.BRAGG_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live Bragg state"
   end if
   if (.not.QCM_STATE_IS_CLEAR()) then
      error stop "SWMAIN returned with live QCM state"
   end if
end subroutine check_clear

 !  The table has a header and one row: XP YP DEPTH HSIGN TM01 DIR.
subroutine read_table(values, ierr)
   real, intent(out)    :: values(4)
   integer, intent(out) :: ierr
   integer :: unit, i
   real    :: xp, yp
   character(len=200) :: line

   values = 0.0
   open(newunit=unit, file='physics_reuse.tbl', status='old', action='read', &
        iostat=ierr)
   if (ierr /= 0) return
   !  A TABLE with HEADER opens with comment lines; the data follows.
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

   !  Bit patterns, not values: all runs do the same arithmetic on the same
   !  input, so anything but an identical result is state that survived.
   if (transfer(a, 1) /= transfer(b, 1)) then
      write(error_unit, '(A,A,A,G16.8,A,G16.8)') &
         'second run differs in ', name, ': ', a, ' then ', b
      error stop "state survived from the contrasting run"
   end if
end subroutine compare

subroutine write_bottom
   integer :: unit

   open(newunit=unit, file='bottom.bot', status='replace', action='write')
   write(unit, '(A)') '20.0 20.0'
   write(unit, '(A)') '20.0 20.0'
   close(unit)
   open(newunit=unit, file='wind.wnd', status='replace', action='write')
   write(unit, '(A)') '8.0 8.0'
   write(unit, '(A)') '8.0 8.0'
   write(unit, '(A)') '0.0 0.0'
   write(unit, '(A)') '0.0 0.0'
   close(unit)
   ! From 20 m in the west to 1.5 m in the east, so that the waves break.
   open(newunit=unit, file='slope.bot', status='replace', action='write')
   write(unit, '(A)') '20.0 1.5'
   write(unit, '(A)') '20.0 1.5'
   close(unit)
end subroutine write_bottom

 !  The 10-minute step gives a CFL number far above 10 for the S&L scheme.
subroutine write_nonstationary_deck
   integer :: unit

   open(newunit=unit, file=DECK, status='replace', action='write')
   write(unit, '(A)') "PROJECT 'NONSTAT' '001'"
   write(unit, '(A)') 'SET NAUTICAL'
   write(unit, '(A)') 'MODE NONSTATIONARY TWODIMENSIONAL'
   write(unit, '(A)') 'COORDINATES CARTESIAN'
   write(unit, '(A)') 'CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 20 10 ' // &
                      'CIRCLE 12 0.05 1.0 8'
   write(unit, '(A)') 'INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0'
   write(unit, '(A)') "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
   write(unit, '(A)') 'WIND 8.0 270.0'
   write(unit, '(A)') 'BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES'
   write(unit, '(A)') 'BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0'
   write(unit, '(A)') 'GEN3 KOMEN'
   write(unit, '(A)') 'BREAKING CONSTANT 1.0 0.73'
   write(unit, '(A)') 'FRICTION JONSWAP CONSTANT 0.038'
   write(unit, '(A)') "POINTS 'CENTER' 1000.0 500.0"
   write(unit, '(A)') "TABLE 'CENTER' HEADER 'nonstat_reuse.tbl' TIME HSIGN " // &
                      'OUTPUT 20260101.000000 10 MIN'
   write(unit, '(A)') 'COMPUTE NONSTAT 20260101.000000 10 MIN 20260101.003000'
   write(unit, '(A)') 'STOP'
   close(unit)
end subroutine write_nonstationary_deck

subroutine write_deck(with_komen)
   logical, intent(in) :: with_komen
   integer :: unit

   open(newunit=unit, file=DECK, status='replace', action='write')
   write(unit, '(A)') "PROJECT 'TWICE' '001'"
   write(unit, '(A)') 'SET NAUTICAL'
   write(unit, '(A)') 'MODE STATIONARY TWODIMENSIONAL'
   write(unit, '(A)') 'COORDINATES CARTESIAN'
   write(unit, '(A)') 'CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 4 2 ' // &
                      'CIRCLE 12 0.05 1.0 8'
   write(unit, '(A)') 'INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0'
   if (scenario == 3) then
      write(unit, '(A)') 'INPGRID WIND REGULAR 0 0 0 1 1 2000 1000'
      write(unit, '(A)') "READINP WIND 1.0 'wind.wnd' 1 0 FREE"
   end if
   if (scenario == 4) then
      write(unit, '(A)') "READINP BOTTOM 1.0 'slope.bot' 3 0 FREE"
   else
      write(unit, '(A)') "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
   end if
   if (scenario /= 3) write(unit, '(A)') 'WIND 8.0 270.0'
   write(unit, '(A)') 'BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES'
   write(unit, '(A)') 'BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0'
   if (with_komen) then
      write(unit, '(A)') 'GEN3 KOMEN'
   end if
   if (scenario == 1) write(unit, '(A)') 'WCAP KOMEN'
   if (scenario == 2) then
      write(unit, '(A)') "TABLE 'BOUNDARY' NOHEADER 'boundary.tbl' HSIGN"
   end if
   if (scenario == 4) then
      write(unit, '(A)') 'NUMERIC STOPC STAT 50'
      write(unit, '(A)') 'BREAKING BKD'
   else
      write(unit, '(A)') 'NUMERIC STOPC STAT 2'
      write(unit, '(A)') 'BREAKING CONSTANT 1.0 0.73'
   end if
   write(unit, '(A)') 'FRICTION JONSWAP CONSTANT 0.038'
   write(unit, '(A)') "POINTS 'CENTER' 1000.0 500.0"
   write(unit, '(A)') "TABLE 'CENTER' HEADER 'physics_reuse.tbl' XP YP " // &
                      'DEPTH HSIGN TM01 DIR'
   write(unit, '(A)') 'COMPUTE'
   write(unit, '(A)') 'STOP'
   close(unit)
end subroutine write_deck

end program test_physics_reuse
