program test_fftw_compat

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none

   integer, parameter :: l = 3
   integer, parameter :: m = 4
   integer, parameter :: lensav = 2*l + int(log(real(l,real64))/log(2.0_real64)) + &
                                           2*m + int(log(real(m,real64))/log(2.0_real64)) + 8
   integer, parameter :: lenwrk = 2*l*m

   complex(real64) :: actual(l,m)
   complex(real64) :: expected(l,m)
   complex(real64) :: original(l,m)
   real(real64) :: work(lenwrk)
   real(real64) :: wsave(lensav)
   real(real64), parameter :: tolerance = 5.0e-13_real64
   real(real64) :: angle
   real(real64) :: pi
   integer :: failures
   integer :: ierr
   integer :: j1, j2, k1, k2, trial

   pi = acos(-1.0_real64)
   do j2 = 1, m
      do j1 = 1, l
         original(j1,j2) = cmplx(real(2*j1-j2, real64), &
                                  real(j1+3*j2, real64), kind=real64)
      end do
   end do

   call cfft2i(l, m, wsave, lensav, ierr)
   if (ierr /= 0) error stop 'CFFT2I failed'

   expected = cmplx(0.0_real64, 0.0_real64, kind=real64)
   do k2 = 0, m-1
      do k1 = 0, l-1
         do j2 = 0, m-1
            do j1 = 0, l-1
               angle = -2.0_real64*pi * (real(j1*k1,real64)/real(l,real64) + &
                                           real(j2*k2,real64)/real(m,real64))
               expected(k1+1,k2+1) = expected(k1+1,k2+1) + original(j1+1,j2+1) * &
                  cmplx(cos(angle), sin(angle), kind=real64)
            end do
         end do
      end do
   end do
   expected = expected / real(l*m, real64)

   actual = original
   call cfft2f(l, l, m, actual, wsave, lensav, work, lenwrk, ierr)
   if (ierr /= 0) error stop 'CFFT2F failed'
   if (maxval(abs(actual-expected)) > tolerance) error stop 'forward DFT mismatch'

   call cfft2b(l, l, m, actual, wsave, lensav, work, lenwrk, ierr)
   if (ierr /= 0) error stop 'CFFT2B failed'
   if (maxval(abs(actual-original)) > tolerance) error stop 'FFT round-trip mismatch'

   ! Exercise concurrent initialisation and execution of the shared FFTW plan.
   failures = 0
   !$omp parallel do private(actual,original,work,wsave,ierr,j1,j2) reduction(+:failures)
   do trial = 1, 64
      do j2 = 1, m
         do j1 = 1, l
            original(j1,j2) = cmplx(real(trial+j1-j2, real64), &
                                     real(j1+2*j2, real64), kind=real64)
         end do
      end do
      actual = original
      call cfft2i(l, m, wsave, lensav, ierr)
      if (ierr == 0) call cfft2f(l, l, m, actual, wsave, lensav, work, lenwrk, ierr)
      if (ierr == 0) call cfft2b(l, l, m, actual, wsave, lensav, work, lenwrk, ierr)
      if (ierr /= 0 .or. maxval(abs(actual-original)) > tolerance) failures = failures + 1
   end do
   !$omp end parallel do
   if (failures /= 0) error stop 'concurrent FFT round-trip mismatch'

end program test_fftw_compat
