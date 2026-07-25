program convrt2d

!     Update: Corrected the count in one of the do-loops (3-11-99)
!
!     PURPOSE
!
!        This program converts a pre-SWAN-40.00
!        2D-spectrum file to a file suitable for
!        SWAN version 40.51.
!
!     USAGE OF THIS PROGRAM
!
!     the program will work interactively
!     it will ask the user to provide the following data:
!     1: name of the input file (file containing spectra in old format)
!     2: type of this file, i.e. nest/spec2d, stationary/nonstationary
!     3: name of the output file (file containing spectra in new format)
!
!     In the case of a nesting file the program will write computational
!     grid data to the screen.

   IMPLICIT NONE

   integer, parameter :: maxloc=5000, maxfrq=100, maxdir=360
   integer   id, ifr, iver, intype, outtyp, itim, iloc, time, iostat,&
   &ndirs, nfreqs, numl, numq, mxn, myn
   real      dir(maxdir), fact, freqhz(maxfrq), pi, tt, efac,&
   &xnlen, ynlen, alpcn, slow, shig, sector, exc
   REAL      vadens(maxdir,maxfrq)
   REAL(KIND=KIND(0.0D0)) dvadens(maxdir,maxfrq), xpcn, ypcn,&
   &locx(maxloc), locy(maxloc)
   character(len=40) :: infile, outfil
   character(len=20) :: chtime
   logical   first

   iver = 1
   exc  = -99.
   pi   = 4.*atan(1.)
   numl = 1
   numq = 1
   itim = 0

!     Old SWAN data files had the Variance denisity calculated per radian
!     The new files store it per degree. The factor pi/180 is the conversion
!     factor.

   fact = pi/180

   WRITE(6,*) '      -----------------------------------------------'
   WRITE(6,*) '      |   This program converts a pre-SWAN-40.00    |'
   WRITE(6,*) '      |   2D-spectrum file to a file suitable for   |'
   WRITE(6,*) '      |   SWAN version 40.51                        |'
   WRITE(6,*) '      -----------------------------------------------'
   WRITE(6,*) ' '

   write (6,*) 'Give the name of the input file: '
   read  (5, '(a)') infile
   write (6,*) 'Give type: 1=old Swan stat, 2=old Swan nonstat,'
   write (6,*) '           3=old Nest stat, 4=old Nest nonstat: '
   read  (5, *) intype
   write (6,*) 'Give the name of the output file: '
   read  (5, '(a)') outfil
   outtyp = 5

   open (8, file=infile, status='old', form='formatted')
   open (9, file=outfil, status='unknown', form='formatted')

!     write heading into the file
!     write keyword SWAN and version number

   write (9, "('SWAN', I4, T41, 'Swan standard spectral file, version')") iver
   WRITE (9,'(a35)') '$   Data produced by pre-SWAN-40.00'
   if (intype.eq.2 .or. intype.eq.4) then
      itim = 1
   endif
   if (itim.gt.0) then
      write (9, "(a, t41, a)") 'TIME', 'time-dependent data'
      write (9, "(i6, t41, a)") itim, 'time-coding option'
   endif

!     first read to obtain general data

   if (intype.eq.1 .or. intype.eq.2) then
!       first read to find all locations
      do iloc = 1, maxloc
         read (8, *, iostat=iostat) locx(iloc), locy(iloc)
         if (is_iostat_end(iostat)) exit
         if (iostat /= 0) error stop 'Error reading input spectrum locations'
         read (8, *) ndirs, nfreqs
         read (8, *) (dir(id), id=1,ndirs)
         read (8, *) (freqhz(ifr), ifr=1,nfreqs)
         do ifr = 1,nfreqs
            read  (8, *) (dvadens(id,ifr), id=1,ndirs)
         end do
         numl = iloc
      enddo
      if (iostat == 0) then
         read (8, *, iostat=iostat) xpcn, ypcn
         if (.not.is_iostat_end(iostat)) error stop 'Too many locations on input file'
      endif
      rewind(8)
   else if (intype.eq.3 .or. intype.eq.4) then
      first = .true.
      header_pass: do
      if (intype.eq.4) then
         read (8,'(1X,A)') CHTIME
         read (8,'(1X,F8.0)') tt
      endif
      read (8, *) XNLEN, YNLEN, SLOW, SHIG
      read (8, *) MXN, MYN
      read (8, *) XPCN, YPCN, ALPCN
      write (6, *) 'CGRID Regular ', xpcn, ypcn, alpcn*180./pi,&
      &'    &', xnlen, ynlen, mxn, myn, '    &'
      read (8, *) ndirs, nfreqs
      read (8, *) (dir(id), id=1,ndirs)
      read (8, *) (freqhz(ifr), ifr=1,nfreqs)
      sector = real(ndirs) * (dir(2)-dir(1))
      if (abs(sector-360.).lt.10.) then
         write (6, *) 'circle ', ndirs,&
         &freqhz(1), freqhz(nfreqs), nfreqs
      else
         write (6, *) 'sector ', dir(1), dir(ndirs), ndirs,&
         &freqhz(1), freqhz(nfreqs), nfreqs
      endif
      if (.not.first) exit header_pass
!       first read to find all locations
      do iloc = 1, maxloc
         read (8, *, iostat=iostat) locx(iloc), locy(iloc)
         if (is_iostat_end(iostat)) exit
         if (iostat /= 0) error stop 'Error reading nested spectrum locations'
         do ifr = 1,nfreqs
            read  (8, *) (dvadens(id,ifr), id=1,ndirs)
         end do
         numl = iloc
      enddo
      if (iostat == 0) then
         read (8, *, iostat=iostat) xpcn, ypcn
         if (.not.is_iostat_end(iostat)) error stop 'Too many locations on input file'
      endif
      rewind(8)
      first = .false.
      end do header_pass
   endif

   if (outtyp.eq.5) then
      write (9, "(a, t41, a)") 'LOCATIONS', 'locations in x-y-space'
      write (9, "(i6, t41, a)") numl, 'number of locations'
      do iloc = 1, numl
         write (9, "(f11.2,f11.2)") locx(iloc), locy(iloc)
      enddo

      write (9, "(a, t41, a)") 'AFREQ', 'absolute frequency in Hz'
      write (9, "(i6, t41, a)") nfreqs,  'number of frequencies'
      write (9, "(f11.4)") (freqhz(ifr), ifr=1,nfreqs)

      write (9, "(a, t41, a)") 'CDIR', 'spectral Cartesian directions in degr'
      write (9, "(i6, t41, a)") ndirs,  'number of directions'
      write (9, "(f11.4)") (dir(id), id=1,ndirs)

      WRITE (9, "(a, t41, a)") 'QUANT'
      WRITE (9, "(i6, t41, a)") numq,         'number of quantities in table'
      WRITE (9, "(a, t41, a)") 'VaDens',     'variance densities in m2/Hz/degr'
      write (9, "(a, t41, a)") 'm2/Hz/degr', 'unit'
      write (9, "(e13.4, T41, A)") exc,   'exception value'

      time_loop: do time= 1, 999
         if (intype.eq.4) then
            read (8,'(1X,A)', iostat=iostat) CHTIME
            if (is_iostat_end(iostat)) exit time_loop
            if (iostat /= 0) error stop 'Error reading spectrum time stamp'
            read (8,'(1X,F8.0)') tt
            write (9, '(A)') chtime
         endif
         do iloc = 1, numl
            if (intype.gt.0 .and. intype.le.4) then
               read (8, *) locx(iloc), locy(iloc)
            endif
            if (intype.eq.1 .or. intype.eq.2) then
!             ignore frequencies and directions
               read (8, *) ndirs, nfreqs
               read (8, *) (dir(id), id=1,ndirs)
               read (8, *) (freqhz(ifr), ifr=1,nfreqs)
            endif

            efac=0.
            do ifr = 1,nfreqs
               read (8, *) (dvadens(id,ifr), id=1,ndirs)
               do id=1,ndirs
                  if (intype.eq.1 .or. intype.eq.2) then
                     vadens(id,ifr) = REAL(dvadens(id,ifr))
                  else if (intype.eq.3 .or. intype.eq.4) then
!                 convert action density into energy density
                     vadens(id,ifr) = REAL(dvadens(id,ifr)) *&
                     &(2.*pi*freqhz(ifr))
                  endif
                  if (vadens(id,ifr).ge.0.) then
                     efac = max (efac, vadens(id,ifr))
                  else
                     efac = max (efac, 10.*abs(vadens(id,ifr)))
                  endif
               end do
            end do
            if (efac .le. 1.e-10) then
               WRITE (9, '(a4)') 'ZERO'
            else
               efac = 1.01 * efac * 10.**(-4)
!             factor pi/180 introduced to account for change from rad to degr
!             factor 2*pi to account for transition from rad/s to hz
               write (9, "('FACTOR', /, e18.8)") efac * 2. * pi**2 / 180.
               do ifr = 1, nfreqs
!                write spectral energy densities to file
                  write (9, "(200(1x,i4))") (nint(vadens(id,ifr)/efac), id=1,ndirs)
               enddo
            endif
         enddo
         if (itim.eq.0) exit time_loop
      enddo time_loop
   endif

   write (6, *) ' Conversion finished'

end program convrt2d
