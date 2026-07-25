program cvspec1d

!     PURPOSE
!
!        This program converts a pre-SWAN-40.00
!        1D-spectrum file to a file suitable for
!        SWAN version 40.51.
!
!     USAGE OF THIS PROGRAM
!
!     the program will work interactively
!     it will ask the user to provide the following data:
!     1: name of the input file (file containing spectra in old format)
!     2: number of spectral frequencies
!     3: average direction, or -999. if average direction is in the file
!     4: directional spread (in degr), or -999. if spreading is in the file
!     5: number of columns in the input file (excluding the first column
!        which always contains the frequencies)

   implicit none

   integer, parameter :: mxfreq = 500, mxcols = 1000
   integer :: ifreq, iloc, inrhog, ip, iread, iverf, jj, kcols, kk
   integer :: ncols, nfreq, nlocs
   real :: aa, avdir, bb, dd, ee
   real :: freqs(mxfreq), values(mxfreq,mxcols)
   character(len=40) :: infile, outfil

   WRITE(6,*) '      -----------------------------------------------'
   WRITE(6,*) '      |   This program converts a pre-SWAN-40.00    |'
   WRITE(6,*) '      |   1D-spectrum file to a file suitable for   |'
   WRITE(6,*) '      |   SWAN version 40.51                        |'
   WRITE(6,*) '      -----------------------------------------------'
   WRITE(6,*) ' '

   inrhog = 0
   kcols = 3

   write (6,*) 'Give the name of the input file: '
   read  (5, '(a)') infile
   write (6,*) 'Number of frequencies: '
   read  (5,*) nfreq
   write (6,*) 'Average direction (-999. if in file): '
   read  (5,*) avdir
   write (6,*) 'Directional spreading (-999. if in file): '
   read  (5,*) dd
   write (6,*) 'Number of columns in the input file (excl freq): '
   read  (5, *) ncols

   if (avdir.gt.-990.) kcols = kcols-1
   if (dd.gt.0.) kcols = kcols-1

   write (6,*) 'Give the name of the output file: '
   read  (5, '(a)') outfil

   if (infile.ne.'    ') then
      iread = 8
      open (iread, file=infile, form='formatted', status='old')
   else
      iread = 5
   endif
   open (9, file=outfil, form='formatted', status='unknown')

   iverf = 1
   WRITE (9, "('SWAN', I4, T41, 'Swan standard spectral file, version')") IVERF
   WRITE (9,'(a35)') '$   Data produced by pre-SWAN-40.00'

   nlocs = (ncols+kcols-1) / kcols
   if (nlocs.gt.1) then
      WRITE (9, "(A, T41, A)") 'LOCATIONS', 'locations in x-y-space'
      WRITE (9, "(I6, T41, A)") nlocs, 'number of locations'
      do IP = 1, nlocs
         WRITE (9, *) 0., 0.
      end do
   endif

   WRITE (9, "(A, T41, A)") 'AFREQ', 'absolute frequencies in Hz'
   WRITE (9, "(I6, T41, A)") nfreq, 'number of frequencies'
   do ifreq=1, nfreq
      read (iread, *) freqs(ifreq), (values(ifreq,jj), jj=1,ncols)
      write (9, "(F10.4)") FREQS(IFREQ)
   enddo

   WRITE (9, "(A, T41, A)") 'QUANT', ' '
   WRITE (9, "(I6, T41, A)") 3, 'number of quantities'

   IF (INRHOG.EQ.1) THEN
      WRITE (9, "(A, T41, A)") 'EnDens', 'energy densities'
      write (9, "(A, T41, A)") 'J/m2/Hz/rad', 'unit'
      write (9, "(e13.4, T41, A)") -99., 'exception value'
   ELSE
      WRITE (9, "(A, T41, A)") 'VaDens', 'variance densities'
      write (9, "(A, T41, A)") 'm2/Hz', 'unit'
      write (9, "(e13.4, T41, A)") -99., 'exception value'
   ENDIF
   WRITE (9, "(A, T41, A)") 'CDIR','average Cartesian direction in degr'
   write (9, "(A, T41, A)") 'degr', 'unit'
   write (9, "(e13.4, T41, A)") -999., 'exception value'
   WRITE (9, "(A, T41, A)") 'DSPRDEGR', 'directional spread in degr'
   write (9, "(A, T41, A)") 'degr', 'unit'
   write (9, "(e13.4, T41, A)") -9., 'exception value'

   do kk = 1, ncols, kcols
      iloc = (kk+kcols-1) / kcols
      write (9,"('LOCATION ',i3)") iloc
      do ifreq = 1, nfreq
         ee = values(ifreq,kk)
         if (avdir.gt.-990.) then
            aa = avdir
            jj = kk
         else
            aa = values(ifreq,kk+1)
            jj = kk+1
         endif
         if (dd.gt.0.) then
            bb = dd
         else
            bb = values(ifreq,jj+1)
         endif
         write (9,"(e12.5, 2f9.2)") ee, aa, bb
      enddo
   enddo
   close (8)
   close (9)
   write (*,*) 'conversion finished'
   stop
end program cvspec1d
