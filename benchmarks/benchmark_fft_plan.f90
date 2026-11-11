! Make the FFTW plan for SIZE x SIZE once, print it, transform once and print
! a checksum. Run it many times to see whether the plan, and with it the last
! bits of the transform, change from run to run.
! Usage: benchmark_fft_plan SIZE. See benchmarks/README.md.
program benchmark_fft_plan
   use, intrinsic :: iso_fortran_env, only: real64
   use, intrinsic :: iso_c_binding
   use swan_fftw_compat
   implicit none
   character(len=32) :: argument
   complex(real64), allocatable :: values(:,:)
   integer :: i, j, ierr, l, lensav, lenwrk
   real(real64), allocatable :: work(:), wsave(:)
   call get_command_argument(1, argument); read(argument, *) l
   lensav = 4*l + 2*int(log(real(l,real64))/log(2.0_real64)) + 8
   lenwrk = 2*l*l
   allocate(values(l,l), work(lenwrk), wsave(lensav))
   do j = 1, l
      do i = 1, l
         values(i,j) = cmplx(sin(0.37_real64*i*j), cos(0.11_real64*(i+2*j)), kind=real64)
      end do
   end do
   call cfft2i(l, l, wsave, lensav, ierr)
   call fftw_print_plan(plan_cache(1)%aligned_forward)
   write(*,*)
   call cfft2f(l, l, l, values, wsave, lensav, work, lenwrk, ierr)
   write(*, '(a,es24.16,1x,es24.16)') 'checksum ', sum(real(values)), sum(aimag(values))
end program benchmark_fft_plan
