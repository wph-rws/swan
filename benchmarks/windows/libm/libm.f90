program libm
!  Prints the bits of the single-precision elementary functions for 100,000 arguments each, so that
!  the results of two run-time libraries can be compared line by line (glibc against Windows).
   implicit none
   integer, parameter :: n = 100000
   integer :: i
   real :: x, y
   real, volatile :: r
   open (11, file='libm_out.txt', status='replace')
   do i = 1, n
      x = 1.0e-3 * real(i) * 0.3137
      y = 0.5 + 4.0e-5 * real(i)
      r = exp(-x);        write (11,'(a,1x,z8.8)') 'exp  ', transfer(r, 1)
      r = sinh(0.3*x);    write (11,'(a,1x,z8.8)') 'sinh ', transfer(r, 1)
      r = tanh(0.3*x);    write (11,'(a,1x,z8.8)') 'tanh ', transfer(r, 1)
      r = cos(x);         write (11,'(a,1x,z8.8)') 'cos  ', transfer(r, 1)
      r = sin(x);         write (11,'(a,1x,z8.8)') 'sin  ', transfer(r, 1)
      r = (1.0+x)**y;     write (11,'(a,1x,z8.8)') 'pow  ', transfer(r, 1)
      r = log(1.0+x);     write (11,'(a,1x,z8.8)') 'log  ', transfer(r, 1)
      r = sqrt(1.0+x);    write (11,'(a,1x,z8.8)') 'sqrt ', transfer(r, 1)
      r = atan2(x, y);    write (11,'(a,1x,z8.8)') 'atan2', transfer(r, 1)
      r = acos(cos(x));   write (11,'(a,1x,z8.8)') 'acos ', transfer(r, 1)
   end do
   close (11)
end program libm
