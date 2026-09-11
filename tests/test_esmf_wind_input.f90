program test_esmf_wind_input
!  The ESMF build's own path in a real computation.
!
!  With ESMF, SWAN adds up the exponential wind growth per grid point in SINBAC,
!  so that a coupler can fetch it. That only happens when the coupler
!  switches on SAVE_SINBAC and allocates SINBAC itself; swan.exe never does, so
!  none of the ordinary tests gets there. This test plays the coupler: it
!  runs a small deck with KOMEN wind (SWIND3) first without and then with
!  SAVE_SINBAC and requires that
!
!  - the computation with storage does not change (bit-identical table),
!  - SINBAC and SAVE_SINBAC remain in place afterwards: the coupler allocated
!    them and owns them, and reads SINBAC out after SWMAIN, just as with
!    upstream, and
!  - SINBAC then holds summed wind growth: nothing negative, the initial value -1
!    overwritten everywhere and a positive contribution somewhere.
   use swan_driver, only: SWMAIN
   use swan_parallel, only: SWINITMPI, SWEXITMPI
   use swan_esmf_coupling_backend, only: esmf_coupling_enabled
   use M_GENARR, only: GENERAL_ARRAY_STATE_IS_CLEAR, SAVE_SINBAC, SINBAC
   implicit none(type, external)

   !  CIRCLE 12 ... 8 gives 12 directions and 9 frequencies; CGRID 4 x 2 gives
   !  15 grid points plus the one auxiliary point that SWAN puts in front.
   integer, parameter :: NDIR = 12, NFREQ = 9, NPOINT = 16
   real :: plain(4), coupled(4)
   integer :: status

   if (.not.esmf_coupling_enabled) error stop 'this test belongs to the ESMF build'

   call write_bottom
   call write_deck
   call SWINITMPI

   call run_once(plain, status)
   if (status /= 0) error stop 'the run without coupling gave no table'

   allocate(SINBAC(NDIR,NFREQ,NPOINT))
   SINBAC = -1.0
   SAVE_SINBAC = .true.
   call run_once(coupled, status)
   if (status /= 0) error stop 'the run with coupling gave no table'
   if (.not.allocated(SINBAC) .or. .not.SAVE_SINBAC) &
      error stop 'SWMAIN released the coupling space of the coupler'
   if (.not.GENERAL_ARRAY_STATE_IS_CLEAR()) &
      error stop 'SWMAIN left general state behind'
   if (any(SINBAC < 0.0)) error stop 'SINBAC contains negative wind growth'
   if (.not.any(SINBAC > 0.0)) error stop 'SINBAC contains no wind growth'

   if (any(transfer(plain, [1, 1, 1, 1]) /= transfer(coupled, [1, 1, 1, 1]))) then
      write (*, '(A,4G16.8)') 'without coupling:', plain
      write (*, '(A,4G16.8)') 'with coupling:   ', coupled
      error stop 'storing the wind growth changed the computation'
   end if

   SAVE_SINBAC = .false.
   deallocate(SINBAC)
   call SWEXITMPI
   print *, 'wind growth stored for the coupling without touching the computation'

contains

   subroutine run_once(values, ierr)
      real, intent(out) :: values(4)
      integer, intent(out) :: ierr
      integer :: unit, dummy

      open(newunit=unit, file='esmf.tbl', status='old', iostat=dummy)
      if (dummy == 0) close(unit, status='delete')
      call SWMAIN
      call read_table(values, ierr)
   end subroutine run_once

   subroutine read_table(values, ierr)
      real, intent(out) :: values(4)
      integer, intent(out) :: ierr
      integer :: unit, i
      real :: xp, yp
      character(len=200) :: line

      values = 0.0
      open(newunit=unit, file='esmf.tbl', status='old', action='read', &
           iostat=ierr)
      if (ierr /= 0) return
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

   subroutine write_bottom
      integer :: unit

      open(newunit=unit, file='bottom.bot', status='replace', action='write')
      write(unit, '(A)') '20.0 20.0'
      write(unit, '(A)') '20.0 20.0'
      close(unit)
   end subroutine write_bottom

   subroutine write_deck
      integer :: unit

      open(newunit=unit, file='INPUT', status='replace', action='write')
      write(unit, '(A)') "PROJECT 'ESMF' '001'"
      write(unit, '(A)') 'SET NAUTICAL'
      write(unit, '(A)') 'MODE STATIONARY TWODIMENSIONAL'
      write(unit, '(A)') 'COORDINATES CARTESIAN'
      write(unit, '(A)') 'CGRID REGULAR 0.0 0.0 0.0 2000.0 1000.0 4 2 ' // &
                         'CIRCLE 12 0.05 1.0 8'
      write(unit, '(A)') 'INPGRID BOTTOM REGULAR 0.0 0.0 0.0 1 1 2000.0 1000.0'
      write(unit, '(A)') "READINP BOTTOM 1.0 'bottom.bot' 3 0 FREE"
      write(unit, '(A)') 'WIND 15.0 270.0'
      write(unit, '(A)') 'BOUND SHAPESPEC JONSWAP 3.3 PEAK DSPR DEGREES'
      write(unit, '(A)') 'BOUNDSPEC SIDE WEST CONSTANT PAR 1.0 6.0 270.0 20.0'
      write(unit, '(A)') 'GEN3 KOMEN'
      write(unit, '(A)') 'NUMERIC STOPC STAT 2'
      write(unit, '(A)') "POINTS 'CENTER' 1000.0 500.0"
      write(unit, '(A)') "TABLE 'CENTER' HEADER 'esmf.tbl' XP YP " // &
                         'DEPTH HSIGN TM01 DIR'
      write(unit, '(A)') 'COMPUTE'
      write(unit, '(A)') 'STOP'
      close(unit)
   end subroutine write_deck
end program test_esmf_wind_input
