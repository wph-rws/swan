! Every OpenMP thread transforms its own SIZE x SIZE array forward and back
! REPETITIONS times through the fork's cfft2f/cfft2b, all at once. This
! measures what a plan lookup costs when many threads ask for it together.
! Usage: benchmark_fft_omp SIZE REPETITIONS; prints the thread count, the
! time in seconds and a checksum. See benchmarks/README.md.
program benchmark_fft_omp
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use omp_lib, only: omp_get_max_threads
   use swan_fftw_compat, only: cfft2b, cfft2f, cfft2i
   implicit none
   character(len=32) :: argument
   complex(real64), allocatable :: values(:,:)
   integer(int64) :: finished, rate, started
   integer :: i, ierr, l, lensav, lenwrk, repetitions
   real(real64), allocatable :: work(:), wsave(:)
   real(real64) :: total
   call get_command_argument(1, argument); read(argument, *) l
   call get_command_argument(2, argument); read(argument, *) repetitions
   lensav = 4*l + 2*int(log(real(l,real64))/log(2.0_real64)) + 8
   lenwrk = 2*l*l
   allocate(wsave(lensav))
   call cfft2i(l, l, wsave, lensav, ierr)
   if (ierr /= 0) error stop 'CFFT2I failed'
   total = 0
   call system_clock(started, rate)
   !$omp parallel private(values, work, i, ierr) reduction(+:total)
   allocate(values(l,l), work(lenwrk))
   values = cmplx(1.0_real64, -0.5_real64, kind=real64)
   do i = 1, repetitions
      call cfft2f(l, l, l, values, wsave, lensav, work, lenwrk, ierr)
      call cfft2b(l, l, l, values, wsave, lensav, work, lenwrk, ierr)
   end do
   total = total + sum(abs(values))
   !$omp end parallel
   call system_clock(finished)
   write(*, '(i3,1x,f0.9,1x,es24.16)') omp_get_max_threads(), &
      real(finished-started,real64)/real(rate,real64), total
end program benchmark_fft_omp
