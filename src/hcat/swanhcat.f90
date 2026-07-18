!     HotConcat
!     Author: Ben Payment 07.17.2006
!
!     Program take hotfiles created from SWAN parallel runs and compiles
!     into a single file.
!
!     HotConcat [-v] [-s] [-h halosize] <basefile>
!     <basefile> refers to the base file name used for hotfiles
!     [-v]           verbose mode: program reports non-error information
!     [-h halosize]  optional argument which changes the overlap halo fr
!     [-s]           stomp over existing basefile
!     [help] [-help] display above information
!
!     SWAN (Simulating WAves Nearshore); a third generation wave model
!     Copyright (C) 1993-2024  Delft University of Technology
!
!     This program is free software: you can redistribute it and/or modi
!     it under the terms of the GNU General Public License as published
!     the Free Software Foundation, either version 3 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program. If not, see <http://www.gnu.org/licenses/


program HotConcat

   implicit none
   character(1024) RLINE
   CHARACTER(LEN=128) arg_str, basefile, tmp
   integer arg_count, ios, i, j, k, l, numfiles, sum, halo, haloindex, mxc, myc
   integer prevsource,freq,cdir, version, ival, ival2, indx
   real    rval
   character(len=16) :: rprojid
   character(len=4)  :: rprojnr
   character(len=20) :: rvertxt, rchtime
   logical ydivide, verbose, haloflag, exists, stomp, nonstat, kspher
   logical free
   real, allocatable :: x(:,:),y(:,:), ac2(:,:)
   integer, allocatable :: locations(:),index(:), source(:)
   integer, allocatable :: ownlocat(:)
!.....Set command-line argument defaults
   basefile = 'NULL'
   free = .TRUE.
   halo = 3
   verbose = .FALSE.
   stomp = .FALSE.

!.....Process command-line arguments
   haloflag = .false.
   arg_count = command_argument_count()
   if (arg_count < 1) then
      print *, 'HotConcat [-v] [-s] [-h halosize] <basefile>'
      stop 1
   end if

   do i = 1, arg_count
      call get_command_argument(number=i, value=arg_str)
      if (arg_str == 'help' .or. arg_str == '-help' .or. arg_str == '--help') then
         print *, 'HotConcat [-v] [-s] [-h halosize] <basefile>'
         print *, '[-v]           report non-error information'
         print *, '[-h halosize]  change the overlap halo from its default of 3'
         print *, '[-s]           overwrite an existing basefile'
         stop
      else if (haloflag) then
         read (arg_str, *, iostat=ios) halo
         if (ios /= 0 .or. halo < 0) error stop 'Invalid halo size'
         haloflag = .false.
      else if (arg_str == '-h') then
         haloflag = .true.
      else if (arg_str == '-v') then
         verbose = .true.
      else if (arg_str == '-s') then
         stomp = .true.
      else if (arg_str == '-sv' .or. arg_str == '-vs') then
         stomp = .true.
         verbose = .true.
      else if (arg_str == '-sh' .or. arg_str == '-hs') then
         stomp = .true.
         haloflag = .true.
      else if (arg_str == '-vh' .or. arg_str == '-hv') then
         verbose = .true.
         haloflag = .true.
      else if (arg_str == '-svh' .or. arg_str == '-shv' .or. &
               arg_str == '-vsh' .or. arg_str == '-vhs' .or. &
               arg_str == '-hsv' .or. arg_str == '-hvs') then
         stomp = .true.
         verbose = .true.
         haloflag = .true.
      else if (basefile == 'NULL') then
         basefile = arg_str
      else
         error stop 'More than one basefile specified'
      end if
   end do

   if (haloflag) error stop 'Missing halo size after -h'

!.....Error trap
   IF(basefile=='NULL') THEN
      PRINT *,'Invalid basefile or basefile argument missing'
      STOP
   ENDIF

!.....Report halo size
   IF(verbose) PRINT *,'Halosize is',halo

!.....Open basefile
   IF (stomp) THEN
      IF (free) THEN
         OPEN(unit=10, file=TRIM(basefile), iostat=ios)
      ELSE
         OPEN(unit=10, file=TRIM(basefile), form='UNFORMATTED',&
         &iostat=ios)
      ENDIF
   ELSE
      IF (free) THEN
         OPEN(unit=10, file=TRIM(basefile), iostat=ios, STATUS='NEW')
      ELSE
         OPEN(unit=10, file=TRIM(basefile), form='UNFORMATTED',&
         &iostat=ios, STATUS='NEW')
      ENDIF
   ENDIF
   IF ( ios /= 0 ) then
      PRINT*,'Could not open ',trim(basefile),' or it already exists'
      STOP
   ENDIF

!.....Identify subdomain files
   numfiles = 0
   DO i=1,1000
      tmp = trim(basefile)
      WRITE(tmp(LEN_TRIM(tmp)+1:LEN_TRIM(tmp)+4),"('-',I3.3,A)") i
      INQUIRE(FILE=tmp,EXIST=exists)
      IF(exists) THEN
         numfiles = i
      ELSE
         EXIT
      ENDIF
   ENDDO

!.....Error trap
   IF(numfiles<2) THEN
      PRINT *,'Unable to find more than 1 file'
      STOP
   ENDIF

!.....Report number of files
   IF(verbose) PRINT *,'Trying to open',numfiles,'files'

!.....Open subdomain files
   DO i=1,numfiles
      tmp = trim(basefile)
      WRITE(tmp(LEN_TRIM(tmp)+1:LEN_TRIM(tmp)+4),"('-',I3.3,A)") i
      IF (free) THEN
         OPEN(unit=10+i, file=tmp, status='old', iostat=ios)
      ELSE
         OPEN(unit=10+i, file=tmp, form='unformatted', status='old',&
         &iostat=ios)
      ENDIF
      IF ( ios /= 0 ) THEN
         PRINT *,'Unable to open',TRIM(tmp)
      ELSE
         IF(verbose) PRINT *,TRIM(tmp),' opened succesfully'
      ENDIF
   ENDDO

!.....Specify the number of location for each hotfile including
!.....the one being created at location(0)
   nonstat = .FALSE. !nonstationary unless time is found
   allocate(locations(0:numfiles))
   IF (free) THEN
      DO i=1,numfiles
         READ (10+i,"(A)") RLINE
         IF(i==1) WRITE(10,"(A)") TRIM(RLINE)
         READ(RLINE,'(A4,I5)') tmp,version
         IF(tmp /= 'SWAN') THEN
            tmp = trim(basefile)
            WRITE(tmp(LEN_TRIM(tmp)+1:LEN_TRIM(tmp)+4),"('-',I3.3,A)") i
            PRINT *,trim(tmp),&
            &'is not a proper hotfile missing header SWAN'
            STOP
         ENDIF
         IF (version /= 1) THEN
            tmp = trim(basefile)
            WRITE(tmp(LEN_TRIM(tmp)+1:LEN_TRIM(tmp)+4),"('-',I3.3,A)") i
            PRINT *,trim(tmp),'is verion',version,&
            &'only version 1 is supported'
            STOP
         ENDIF
         DO
            READ (10+i,"(A)") RLINE
            IF(i==1) WRITE(10,"(A)") TRIM(RLINE)
            IF (RLINE(1:4)=='TIME') nonstat=.TRUE.
            IF (RLINE(1:6)=='LONLAT' .OR. RLINE(1:4)=='LOCA') EXIT
         END DO
         IF (RLINE(1:6).EQ.'LONLAT') THEN
            kspher = .TRUE.
         ELSE IF (RLINE(1:4).EQ.'LOCA') THEN
            kspher = .FALSE.
         ENDIF
         READ (10+i,"(I8,2I6,A)") locations(i),mxc,myc,RLINE
         IF(verbose) PRINT *,'File',i,'has',locations(i),'locations'
      ENDDO
   ELSE
      DO i=1,numfiles
         READ (10+i) RVERTXT
         READ (10+i) RPROJID, RPROJNR
         IF(i==1) WRITE(10) RVERTXT
         IF(i==1) WRITE(10) RPROJID, RPROJNR
         READ (10+i) IVAL
         IF(i==1) WRITE(10) IVAL
         IF (IVAL==1) THEN
            READ (10+i) IVAL2
            IF(i==1) WRITE(10) IVAL2
            nonstat=.TRUE.
         ENDIF
         READ (10+i) IVAL
         IF(i==1) WRITE(10) IVAL
         READ (10+i) locations(i), mxc, myc
         IF(verbose) PRINT *,'File',i,'has',locations(i),'locations'
      ENDDO
   ENDIF

!.....Find sum of location length
   sum=0
   DO i=1,numfiles
      sum=sum+locations(i)
   ENDDO

!.....Allocate 2D array for x,y data
   allocate(x(0:numfiles,1:sum))
   allocate(y(0:numfiles,1:sum))

!.....Allocate array to refer to which hotfile a grid point info is stor
   allocate(source(1:sum))

!.....Read in locations
   IF (free) THEN
      IF (.not.kspher) THEN
         DO i=1,numfiles
            DO j=1,locations(i)
               READ (10+i,"(F14.4,F14.4)") x(i,j),y(i,j)
            ENDDO
         ENDDO
      ELSE
         DO i=1,numfiles
            DO j=1,locations(i)
               READ (10+i,"(F12.6,F12.6)") x(i,j),y(i,j)
            ENDDO
         ENDDO
      ENDIF
   ELSE
      DO i=1,numfiles
         DO j=1,locations(i)
            READ (10+i) x(i,j),y(i,j)
         ENDDO
      ENDDO
   ENDIF

!.....Determine how data is divided for parallel run either on x or y
   IF(x(1,1)==x(2,1)) THEN
      ydivide=.TRUE.
      IF(verbose) PRINT *,'Processes divided along y'
   ELSE
      ydivide=.FALSE.
      IF(verbose) PRINT *,'Processes divided along x'
   ENDIF

!.....Index array keep track of the index number of the grid point being
   allocate(index(0:numfiles))
   DO i=0,numfiles
      index(i)=1
   ENDDO

!.....Sort grid points
   DO WHILE (index(numfiles)<=locations(numfiles))
!........IF divided along Y
      IF(ydivide) THEN
         DO i=1,numfiles
            IF(i==numfiles) THEN
               DO WHILE(x(i,index(i))<=x(1,index(1)-1))
                  x(0,index(0))=x(i,index(i))
                  y(0,index(0))=y(i,index(i))
                  source(index(0))=i
                  index(i)=index(i)+1
                  index(0)=index(0)+1
                  IF(index(i)>locations(i)) EXIT
               ENDDO
            ELSEIF (index(i)<=locations(i)) THEN
               haloindex=0
               DO WHILE(x(i,index(i))<=x(i+1,index(i+1)) .AND.&
               &haloindex<halo)
                  x(0,index(0))=x(i,index(i))
                  y(0,index(0))=y(i,index(i))
                  source(index(0))=i
                  IF( y(i,index(i)) == y(i+1,index(i+1)) ) THEN
                     haloindex=haloindex+1
                     index(i+1)=index(i+1)+1
                  ENDIF
                  index(i)=index(i)+1
                  index(0)=index(0)+1
               ENDDO
               index(i)=index(i)+halo
            ELSE
               PRINT *,'Error'
               STOP
            ENDIF
         ENDDO
!........IF divided along X
      ELSE
         DO i=1,numfiles
            IF(i==numfiles) THEN
               DO WHILE(index(i)<=locations(i))
                  x(0,index(0))=x(i,index(i))
                  y(0,index(0))=y(i,index(i))
                  source(index(0))=i
                  index(i)=index(i)+1
                  index(0)=index(0)+1
               ENDDO
            ELSE
               haloindex=0
               DO WHILE(x(i,index(i))<=x(i+1,index(i+1)) .AND.&
               &haloindex<halo)
                  x(0,index(0))=x(i,index(i))
                  y(0,index(0))=y(i,index(i))
                  source(index(0))=i
                  IF( x(i,index(i)) == x(i+1,index(i+1)) .AND.&
                  &y(i,index(i)) == y(i+1,index(i+1)) ) THEN
                     IF (y(i+1,index(i+1))>y(i+1,index(i+1)+1)) THEN
                        haloindex=haloindex+1
                     ENDIF
                     index(i+1)=index(i+1)+1
                  ENDIF
                  index(i)=index(i)+1
                  index(0)=index(0)+1
               ENDDO
            ENDIF
         ENDDO
      ENDIF
   ENDDO

!.....Index shift, set total number of locations
   DO i=0,numfiles
      index(i)=index(i)-1
   ENDDO
   locations(0)=index(0)

!.....Specify the number of own location for each hotfile
   allocate(ownlocat(0:numfiles))
   ownlocat=0
   DO j=1,numfiles
      index(0)=1
      DO WHILE (index(0)<=locations(0))
         IF (source(index(0))==j) ownlocat(j)=ownlocat(j)+1
         index(0)=index(0)+1
      END DO
   END DO

   IF (ydivide) THEN
      myc = locations(0)/mxc
   ELSE
      mxc = locations(0)/myc
   ENDIF

!.....Write total number of locations to basefile
   IF (free) THEN
      WRITE (10,"(I8,2I6,A)") locations(0),mxc,myc,TRIM(RLINE)
   ELSE
      WRITE (10) locations(0),mxc,myc
   ENDIF

!.....Write grid points to basefile
   IF (free) THEN
      IF (.not.kspher) THEN
         DO i=1,locations(0)
            WRITE (10,"(F14.4,F14.4)") x(0,i),y(0,i)
         ENDDO
      ELSE
         DO i=1,locations(0)
            WRITE (10,"(F12.6,F12.6)") x(0,i),y(0,i)
         ENDDO
      ENDIF
   ELSE
      DO i=1,locations(0)
         WRITE (10) x(0,i),y(0,i)
      ENDDO
   ENDIF

!.....Process FREQ
   IF (free) THEN
      DO i=1,numfiles
         READ (10+i,"(A)") RLINE
         IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         READ (10+i,"(A)") RLINE
         READ (RLINE,*) freq
         IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         IF(verbose) PRINT *,'File',i,'has',freq,'frequencies'
         DO j=1,freq
            READ (10+i,"(A)") RLINE
            IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         ENDDO
      ENDDO
   ELSE
      DO i=1,numfiles
         READ (10+i) freq
         IF (i==1) WRITE(10) freq
         IF(verbose) PRINT *,'File',i,'has',freq,'frequencies'
         DO j=1,freq
            READ (10+i) RVAL
            IF (i==1) WRITE(10) RVAL
         ENDDO
      ENDDO
   ENDIF

!.....Process DIR
   IF (free) THEN
      DO i=1,numfiles
         READ (10+i,"(A)") RLINE
         IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         READ (10+i,"(A)") RLINE
         READ (RLINE,*) cdir
         IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         IF(verbose) PRINT *,'File',i,'has',cdir,'directions'
         DO j=1,cdir
            READ (10+i,"(A)") RLINE
            IF (i==1) WRITE(10,"(A)") TRIM(RLINE)
         ENDDO
      ENDDO
   ELSE
      DO i=1,numfiles
         READ (10+i) cdir
         IF (i==1) WRITE(10) cdir
         IF(verbose) PRINT *,'File',i,'has',cdir,'directions'
         DO j=1,cdir
            READ (10+i) RVAL
            IF (i==1) WRITE(10) RVAL
         ENDDO
      ENDDO
   ENDIF

!.....Allocate action density in case of binary format for reading/writi
   if (.not.free) allocate(ac2(cdir,freq))

!.....Process QUANT (must be single quantity) in case of free format
   IF (free) THEN
      DO i=1,numfiles
         DO j=1,5
            READ (10+i,"(A)") RLINE
            IF (i==numfiles) WRITE(10,"(A)") TRIM(RLINE)
         ENDDO
      ENDDO
   ENDIF

!.....Process date & time -- in case of nonstationary run
   IF (nonstat) THEN
      IF (free) THEN
         DO i=1,numfiles
            READ (10+i,"(A)") RLINE
            IF (i==numfiles) WRITE(10,"(A)") TRIM(RLINE)
         ENDDO
      ELSE
         DO i=1,numfiles
            READ (10+i) RCHTIME
            IF (i==numfiles) WRITE(10) RCHTIME
         ENDDO
      ENDIF
   ENDIF

!.....Reset index
   DO i=0,numfiles
      index(i)=1
   ENDDO

!.....Change in source represent hotfile boundary and the need to proces
   prevsource=1

!.....Process action densities
   DO WHILE (index(0)<=locations(0))
      DO j=1,numfiles
!...........IF divided along Y
         IF(ydivide) THEN
            IF(source(index(0))==j) THEN
!.................IF source equals 1 there are no halo points at the beg
               IF(j .NE. 1) THEN
                  IF (free) THEN
                     DO k=1,halo
                        READ (10+j,"(A)",IOSTAT=ios) RLINE
                        IF (RLINE(1:6).EQ.'FACTOR' .and. ios==0) THEN
                           DO l=1,freq+1
                              READ (10+j,"(A)",IOSTAT=ios) RLINE
                           ENDDO
                           index(j)=index(j)+1
                        ELSE IF(RLINE(1:4).EQ.'ZERO'.and.ios==0) THEN
                           index(j)=index(j)+1
                        ELSE IF(RLINE(1:6).EQ.'NODATA'.and.ios==0)THEN
                           index(j)=index(j)+1
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,RLINE
                           EXIT
                        ENDIF
                     ENDDO
                  ELSE
                     DO k=1,halo
                        READ (10+j,IOSTAT=ios) INDX
                        IF (INDX.EQ.1 .and. ios==0) THEN
                           index(j)=index(j)+1
                        ELSE IF (INDX.GT.1 .and. ios==0) THEN
                           READ (10+j,IOSTAT=ios) ac2(:,:)
                           index(j)=index(j)+1
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,INDX
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
!.................Write out grid point data as long as the source doesn'
               DO WHILE(source(index(0))==j)
                  IF (free) THEN
                     READ (10+j,"(A)",IOSTAT=ios) RLINE
                     IF (RLINE(1:6).EQ.'FACTOR' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        DO l=1,freq+1
                           READ (10+j,"(A)",IOSTAT=ios) RLINE
                           WRITE(10,"(A)") TRIM(RLINE)
                        ENDDO
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF(RLINE(1:4).EQ.'ZERO' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF(RLINE(1:6).EQ.'NODATA' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE
                        PRINT *,'Unable to process'
                        PRINT *,RLINE
                        EXIT
                     ENDIF
                  ELSE
                     READ (10+j,IOSTAT=ios) INDX
                     IF (INDX.EQ.1 .and. ios==0) THEN
                        WRITE(10) INDX
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF (INDX.GT.1 .and. ios==0) THEN
                        WRITE(10) INDX
                        READ (10+j,IOSTAT=ios) ac2(:,:)
                        WRITE(10) ac2(:,:)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE
                        PRINT *,'Unable to process'
                        PRINT *,INDX
                        EXIT
                     ENDIF
                  ENDIF
                  prevsource=j
               ENDDO
!.................IF source equals last source there are no halo points
               IF(j .NE. numfiles) THEN
                  IF (free) THEN
                     DO k=1,halo
                        READ (10+j,"(A)",IOSTAT=ios) RLINE
                        IF (RLINE(1:6).EQ.'FACTOR'.and.ios==0) THEN
                           DO l=1,freq+1
                              READ (10+j,"(A)",IOSTAT=ios) RLINE
                           ENDDO
                           index(j)=index(j)+1
                        ELSE IF(RLINE(1:4).EQ.'ZERO'.and.ios==0) THEN
                           index(j)=index(j)+1
                        ELSE IF(RLINE(1:6).EQ.'NODATA'.and.ios==0)THEN
                           index(j)=index(j)+1
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,RLINE
                           EXIT
                        ENDIF
                     ENDDO
                  ELSE
                     DO k=1,halo
                        READ (10+j,IOSTAT=ios) INDX
                        IF (INDX.EQ.1 .and. ios==0) THEN
                           index(j)=index(j)+1
                        ELSE IF (INDX.GT.1 .and. ios==0) THEN
                           READ (10+j,IOSTAT=ios) ac2(:,:)
                           index(j)=index(j)+1
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,INDX
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
!...........IF divided along X
         ELSE
            IF(source(index(0))==j) THEN
!.................IF source equals 1 there are no halo points at the beg
               IF(j .NE. 1) THEN
!                     DO WHILE (index(j-1)<=ownlocat(j-1))
                  DO WHILE (index(j-1)<=locations(j-1))
                     IF (free) THEN
                        READ (10+j,"(A)",IOSTAT=ios) RLINE
                        IF (RLINE(1:6).EQ.'FACTOR' .and. ios==0)THEN
                           DO l=1,freq+1
                              READ (10+j,"(A)",IOSTAT=ios) RLINE
                           ENDDO
                           index(j-1)=index(j-1)+1
                           index(j)=index(j)+1
                        ELSEIF(RLINE(1:4).EQ.'ZERO'.and.ios==0) THEN
                           index(j-1)=index(j-1)+1
                           index(j)=index(j)+1
                        ELSEIF(RLINE(1:6).EQ.'NODATA'.and.ios==0)THEN
                           index(j-1)=index(j-1)+1
                           index(j)=index(j)+1
                        ELSE IF (ios/=0) THEN
                           PRINT *,'End of file error'
                           PRINT *,'ios:',ios
                           STOP
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,'RLINE: ',RLINE
                           STOP
                        ENDIF
                     ELSE
                        READ (10+j,IOSTAT=ios) INDX
                        IF (INDX.EQ.1 .and. ios==0) THEN
                           index(j-1)=index(j-1)+1
                           index(j)=index(j)+1
                        ELSE IF (INDX.GT.1 .and. ios==0) THEN
                           READ (10+j,IOSTAT=ios) ac2(:,:)
                           index(j-1)=index(j-1)+1
                           index(j)=index(j)+1
                        ELSE IF (ios/=0) THEN
                           PRINT *,'End of file error'
                           PRINT *,'ios:',ios
                           STOP
                        ELSE
                           PRINT *,'Unable to process'
                           PRINT *,'INDX: ',INDX
                           STOP
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
!.................Write out grid point data as long as the source doesn'
               DO WHILE(source(index(0))==j)
                  IF (free) THEN
                     READ (10+j,"(A)",IOSTAT=ios) RLINE
                     IF (RLINE(1:6).EQ.'FACTOR' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        DO l=1,freq+1
                           READ (10+j,"(A)",IOSTAT=ios) RLINE
                           WRITE(10,"(A)") TRIM(RLINE)
                        ENDDO
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF(RLINE(1:4).EQ.'ZERO' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF(RLINE(1:6).EQ.'NODATA' .and. ios==0) THEN
                        WRITE(10,"(A)") TRIM(RLINE)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF (ios/=0) THEN
                        PRINT *,'End of file error'
                        PRINT *,'ios:',ios
                        STOP
                     ELSE
                        PRINT *,'Unable to process'
                        PRINT *,'RLINE: ',RLINE
                        STOP
                     ENDIF
                  ELSE
                     READ (10+j,IOSTAT=ios) INDX
                     IF (INDX.EQ.1 .and. ios==0) THEN
                        WRITE(10) INDX
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF (INDX.GT.1 .and. ios==0) THEN
                        WRITE(10) INDX
                        READ (10+j,IOSTAT=ios) ac2(:,:)
                        WRITE(10) ac2(:,:)
                        index(j)=index(j)+1
                        index(0)=index(0)+1
                     ELSE IF (ios/=0) THEN
                        PRINT *,'End of file error'
                        PRINT *,'ios:',ios
                        STOP
                     ELSE
                        PRINT *,'Unable to process'
                        PRINT *,'INDX: ',INDX
                        STOP
                     ENDIF
                  ENDIF
                  prevsource=j
               ENDDO
            ENDIF
         ENDIF
      ENDDO
   ENDDO

!.....Close all files
   CLOSE ( unit = 10 );
   DO i=1,numfiles
      CLOSE (unit=10+i)
   ENDDO

end program HotConcat
