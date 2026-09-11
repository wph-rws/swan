program benchmark_fft

   use, intrinsic :: iso_fortran_env, only: int64, real64
   use swan_fftw_compat, only: cfft2b, cfft2f, cfft2i

   implicit none

   character(len=32) :: argument
   complex(real64), allocatable :: values(:,:)
   integer(int64) :: finished, rate, started
   integer :: i, ierr, l, lensav, lenwrk, repetitions
   real(real64), allocatable :: work(:), wsave(:)

   if (command_argument_count() /= 2) then
      error stop 'usage: benchmark_fft SIZE REPETITIONS'
   end if
   call get_command_argument(1, argument)
   read(argument, *) l
   call get_command_argument(2, argument)
   read(argument, *) repetitions
   if (l < 1 .or. repetitions < 1) error stop 'arguments must be positive'

   lensav = 4*l + 2*int(log(real(l,real64))/log(2.0_real64)) + 8
   lenwrk = 2*l*l
   allocate(values(l,l), work(lenwrk), wsave(lensav))
   values = cmplx(1.0_real64, -0.5_real64, kind=real64)

   ! Plan creation is deliberately excluded from the timed region.
   call cfft2i(l, l, wsave, lensav, ierr)
   if (ierr /= 0) error stop 'CFFT2I failed'

   call system_clock(started, rate)
   do i = 1, repetitions
      call cfft2f(l, l, l, values, wsave, lensav, work, lenwrk, ierr)
      if (ierr /= 0) error stop 'CFFT2F failed'
      call cfft2b(l, l, l, values, wsave, lensav, work, lenwrk, ierr)
      if (ierr /= 0) error stop 'CFFT2B failed'
   end do
   call system_clock(finished)

   write(*, '(f0.9,1x,es24.16)') &
      real(finished-started,real64)/real(rate,real64), sum(abs(values))

end program benchmark_fft
