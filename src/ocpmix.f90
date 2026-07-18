!     Ocean Pack miscellaneous routines
!
!     real function DTTIME
!     subroutine    DTINTI
!     subroutine    DTRETI
!     char function DTTIWR
!     REPARM
!     INAR2D
!     STRACE
!     MSGERR
!     TABHED
!     FOR
!     logical function EQREAL  ( Checks whether REAL1 is appr.
!                                equal to REAL2                 )
!     logical function EQDBLE
!     LSPLIT             ( splits an input line into data items )
!     BUGFIX
!     COPYCH (copied from file OCPDPN)
!
!*******************************************************************
!                                                                  *
REAL FUNCTION DTTIME (INTTIM)
!                                                                  *
!*******************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.74: IJsbrand Haagsma (Include version)
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!      9705, May  97: month number is checked
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     DTTIME gives time in seconds from a reference day
!            it also initialises the reference day
!
!  3. Method
!
!     every fourth year is a leap-year, but not the century-years, howev
!     also leap-years are: year 0, 1000, 2000 etc.
!     1 jan of year 0 is daynumber 1.
!
!  4. Argument variables
!
!     INTTIM(1): year
!           (2): month
!           (3): day
!           (4): hour
!           (5): minute
!           (6): second

   INTEGER INTTIM(6)

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IDYMON : number of days of each month (February counts as 28 days)
!     IYEAR  : number of years after substacking the centuries
!     IYRM1  : ??
!     IDNOW  : ??
!     I      : ??
!     II     : ??

   INTEGER, SAVE :: IDYMON(12) = &
      [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   INTEGER IYEAR, IYRM1, IDNOW, I, II

!     LEAPYR : Whether year in INTTIM(1) is a leapyear
!     LOGREF : ??

   LOGICAL LEAPYR
   LOGICAL, SAVE :: LOGREF = .FALSE.

!     REFDAY  day number of the reference day; the reference time is 0:0
!            of the reference day; the first day entered is used as
!             reference day.
!
!
!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   IYEAR = INTTIM(1)
   IYRM1 = IYEAR-1
   LEAPYR=(MOD(IYEAR,4).EQ.0.AND.MOD(IYEAR,100).NE.0).OR.&
   &MOD(IYEAR,400).EQ.0
   IDNOW=0
   IF (INTTIM(2).GT.12) THEN
      WRITE (PRINTF, "(' erroneous month ', I2, ' in date/time ', 6I4)") INTTIM(2), (INTTIM(II), II=1,6)
   ELSE IF (INTTIM(2).GT.1) THEN
      do I = 1,INTTIM(2)-1
         IDNOW=IDNOW+IDYMON(I)
      end do
   ENDIF
   IDNOW=IDNOW+INTTIM(3)
   IF (LEAPYR.AND.INTTIM(2).GT.2) IDNOW=IDNOW+1
   IDNOW = IDNOW + IYEAR*365 + IYRM1/4 - IYRM1/100 + IYRM1/400 + 1
   IF (IYEAR.EQ.0) IDNOW=IDNOW-1
   IF (.NOT.LOGREF) THEN
      REFDAY = IDNOW
      LOGREF = .TRUE.
      DTTIME = 0.
   ELSE
      DTTIME = REAL(IDNOW-REFDAY) * 24.*3600.
   ENDIF
   DTTIME = DTTIME + 3600.*REAL(INTTIM(4)) + 60.*REAL(INTTIM(5)) +&
   &REAL(INTTIM(6))
   RETURN
end function DTTIME
!*******************************************************************
!                                                                  *
SUBROUTINE DTINTI (TIMESC, INTTIM)
!                                                                  *
!*******************************************************************

   USE OCPCOMM1

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.74: IJsbrand Haagsma (Include version)
!     30.70: Nico Booij (small change)
!
!  1. Updates
!
!      9705, May  97: month number is checked
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.70, Jan. 98: small change in interpretation of time in sec
!
!  2. Purpose
!
!     DTINTI calculates integer time array INTTIM from time in seconds
!            from given reference day REFDAY
!
!  3. Method
!
!     every fourth year is a leap-year, but not the century-years, howev
!     also leap-years are: year 0, 1000, 2000 etc.
!     1 jan of year 0 is daynumber 1.
!
!  4. Argument variables
!
!     INTTIM(1): year
!           (2): month
!           (3): day
!           (4): hour
!           (5): minute
!           (6): second

   INTEGER INTTIM(6)

!     TIMESC : input  time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0)) TIMESC

!  5. PARAMETER VARIABLES
!
!     IDAYYR : number of days in a year (not leapyear)
!     IFOUR  : number of days in 4 years (leapyear)
!     IDYCEN : number of days in a century (not leapyear)
!     IDYMIL : number of days in 4 centuries (leapyear)

   INTEGER, PARAMETER :: IDAYYR=365
   INTEGER, PARAMETER :: IFOUR=4*IDAYYR+1
   INTEGER, PARAMETER :: IDYCEN=25*IFOUR-1
   INTEGER, PARAMETER :: IDYMIL=4*IDYCEN+1

!  6. LOCAL VARIABLES
!
!     I4     : number of blocks of four years after subtraction of the
!              millenia and the centuries
!     ICEN   : number of centuries after subtracking the millenia
!     ICNT   : count no remainder in day of year
!     IDYMN  : day of the month
!     IDYMON : number of days of each month
!     IDYNOW : local daynumber
!     IMIL   : number of millenia in julday-1 days
!     IMN    : month counter
!     IYR    : remaining number of years
!     IYEAR  : number of years after subtracking the centuries
!     NDAY   : number of days since reference day
!     NOWDAY : reference day

   INTEGER, PARAMETER :: IDYMON(12) = &
      [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   INTEGER I4, ICEN, ICNT, IDYMN, IDYNOW, IMIL, IMN, IYR, IYEAR, &
      NDAY, NOWDAY

!     TT     : time in seconds since begin of the same day

   REAL(KIND=KIND(0.0D0))  TT

!     LEAPYR : logical for yes or no leap-year

   LOGICAL LEAPYR

!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   NDAY = INT((TIMESC+0.4)/(24*3600))
   DO
      TT = TIMESC - DBLE(NDAY)*24.*3600.
      IF (TT.GE.-0.4) EXIT
      NDAY = NDAY - 1
   END DO
   NOWDAY = REFDAY + NDAY

!        get year

   ICNT   = 0
   IDYNOW = NOWDAY-1

   IMIL   = IDYNOW/IDYMIL
   IDYNOW = IDYNOW-IMIL*IDYMIL
   IF (IDYNOW.EQ.0) ICNT = ICNT + 1

   ICEN   = IDYNOW/IDYCEN
   IDYNOW = IDYNOW-ICEN*IDYCEN
   IF (IDYNOW.EQ.0) ICNT = ICNT + 1

   I4     = IDYNOW/IFOUR
   IDYNOW = IDYNOW-I4*IFOUR
   IF (IDYNOW.EQ.0) ICNT = ICNT + 1

   IF (IDYNOW.GE.366) THEN
      IDYNOW = IDYNOW-366
      IYR    = IDYNOW/IDAYYR
      IDYNOW = IDYNOW-IYR*IDAYYR
      IYR    = IYR+1
   ELSE
      IYR = 0
   ENDIF

   IYEAR = 400*IMIL + 100*ICEN + 4*I4 + IYR

   IF (MOD(IYEAR,100).NE.0.OR.MOD(IYEAR,400).EQ.0) IDYNOW = IDYNOW+1

!        get month and day

   LEAPYR = (MOD(IYEAR,4).EQ.0.AND.MOD(IYEAR,100).NE.0).OR.&
   &MOD(IYEAR,400).EQ.0

   do IMN = 1, 12
      IDYMN=IDYMON(IMN)
      IF(LEAPYR.AND.IMN.EQ.2) IDYMN=IDYMN+1
      IF(IDYNOW.LE.IDYMN) EXIT
      IDYNOW=IDYNOW-IDYMN
   end do
   IF (ICNT==2) THEN
      IYEAR=IYEAR-1
      IMN=12
      IDYNOW=31
   ENDIF

   INTTIM(2) = IMN
   INTTIM(3) = IDYNOW
   INTTIM(1) = IYEAR

!        get time of day

   INTTIM(4) = INT(TT/3600.)
   TT        = TT - 3600.*DBLE(INTTIM(4))
   INTTIM(5) = INT(TT/60.)
   TT        = TT - 60.*DBLE(INTTIM(5))
   INTTIM(6) = INT(TT)
   RETURN
end subroutine DTINTI
!*****************************************************************
!                                                                *
SUBROUTINE DTRETI (TSTRNG, IOPT, TIMESC)
!                                                                *
!*****************************************************************

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!  1. UPDATES
!
!  2. PURPOSE
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IOPT   : input    option number

   INTEGER IOPT

!     TIMESC : output   time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0))  TIMESC

!     TSTRNG : input    time string

   CHARACTER(LEN=*) :: TSTRNG

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     ITIME  : ??

   INTEGER ITIME(6)

   REAL    RTMP

!     DTTIME : Gives time in seconds from a reference day it also initia
!              reference day

   REAL    DTTIME

!  8. SUBROUTINE USED
!
!     DTSTTI   (installation dependent subroutines)
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL DTSTTI (IOPT, TSTRNG, ITIME)
   RTMP   = DTTIME (ITIME)
   TIMESC = DBLE(RTMP)
   RETURN
end subroutine DTRETI
!*****************************************************************
!                                                                *
CHARACTER(LEN=18) FUNCTION DTTIWR (IOPT, TIMESC)
!                                                                *
!*****************************************************************

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     40.02: IJsbrand Haagsma
!
!  1. UPDATES
!
!     30.05: New subroutine
!     40.02, Oct. 00: Made length of TSTRNG equal to TIMESTR
!
!  2. PURPOSE
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IOPT   : input    time coding option number

   INTEGER    IOPT

!     TIMESC : output   time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0))     TIMESC

!     TSTRNG : input    time string

   CHARACTER (LEN=24) :: TSTRNG

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES

   INTEGER    ITIME(6)

!  8. SUBROUTINE USED
!
!     DTTIST   (installation dependent subroutines)
!     DTINTI   (misc. routines)
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL DTINTI (TIMESC, ITIME)
   CALL DTTIST (IOPT, TSTRNG, ITIME)
   DTTIWR = TSTRNG(1:18)
   RETURN
end function DTTIWR
!*****************************************************************
!                                                                *
SUBROUTINE REPARM (NDSL, NDSD, IDLA, IDFM, RFORM,&
&NHEDF, IDYN, NHEDT, LOGC, NHEDC)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.74: IJsbrand Haagsma (Include version)
!     30.80: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.00: Nico Booij (modifications)
!     40.02: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.10, Dec. 95: [fac] is read before 'fname' in view of later
!                     reading of tables; argument VFAC removed
!                     arguments LOGT, NHEDT, LOGC, NHEDC added
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.00, Jan. 98: SWAN specific statements modified; argument list
!                     changed
!     30.82, Sep. 98: Added type specification for HEDLIN
!     30.80, Dec. 98: Initialisation of NHEDT and NHEDC
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Sep. 00: Replaced computed GOTO by CASE construct
!     40.03, Jul. 00: TRIM used to improve readability of message
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     reads parameters for reading an array from users input
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IDFM   : output   format index
!     IDLA   : output   lay-out indicator
!     IDYN   : input    indicate whether grid is dynamic or not
!     NDSD   : ??       unit number of the file from which to read the d
!     NDSL   : ??       unit number of the file containing the list of f
!     NHEDF  : output   number of heading lines in the file (once in eac
!     NHEDT  : output   number of heading lines in the file before readi
!                       each time level
!     NHEDC  : output   number of heading lines in the file before each
!                       or vector component

   INTEGER   IDFM, IDLA,  NDSL, NDSD, NHEDF, NHEDT, NHEDC, IDYN

!     LOGC   : input    if True more than one component is read from fil

   LOGICAL   LOGC

!     RFORM  : output   reading format

   CHARACTER(LEN=*) :: RFORM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!
!     IENT   : Number of entries into this subroutine
!     IH     : ??
!     IOSTAT : input   0 : Full messages printed
!                      -1: Only error messages printed
!                      -2: No messages printed
!              output  error indicator

   INTEGER, SAVE :: IENT = 0
   INTEGER   IH, IOSTAT

!     HEDLIN : Content of a header line
!     KEYWIS : ??

   LOGICAL   KEYWIS, BNEW

!     OLDFIL : ??

   CHARACTER(LEN=80) :: HEDLIN
   CHARACTER(LEN=36), SAVE :: OLDFIL = ' '

!  8. SUBROUTINE USED

   LOGICAL   STPNOW

!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'REPARM')

   CALL INKEYW ('STA', '   ')
   IF (KEYWIS('SERI')) THEN
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
!       open namelist file and read first datafile name
      CALL FOR (NDSL, FILENM, 'OF', IOSTAT)
      IF (STPNOW()) RETURN
      READ(NDSL, '(A36)') FILENM
   ELSE
      CALL INCSTR ('FNAME', FILENM, 'REQ', ' ')
   ENDIF

   IF (FILENM.NE.OLDFIL) THEN
      BNEW = .TRUE.
      NDSD = 0
      IDLA = 1
      IDFM = 0
      RFORM = ' '
      NHEDF = 0
      OLDFIL = FILENM
   ELSE
      BNEW = .FALSE.
   ENDIF

   CALL INKEYW ('STA', ' ')
   IF (BNEW) THEN
!       read lay-out indicator
      CALL ININTG ('IDLA', IDLA, 'UNC', 1)
!       names changed and order changed, ver 30.20 (Swan)
      CALL ININTG ('NHEDF', NHEDF, 'UNC', 0)
      NHEDT = 0
      IF (IDYN.GT.0) THEN
         CALL ININTG ('NHEDT', NHEDT, 'UNC', 0)
      ENDIF
      NHEDC = 0
      IF (LOGC) THEN
         CALL ININTG ('NHEDVEC', NHEDC, 'UNC', 0)
      ENDIF
      CALL INKEYW ('STA', 'FREE')
      IDFM = 2
      IF (KEYWIS('FRE')) THEN
         IDFM = 0
      ELSE IF (KEYWIS('UNF')) THEN
         IDFM = -1
      ELSE IF (KEYWIS('FOR')) THEN
!         formatted read
         CALL ININTG ('IDFM', IDFM, 'NSKP', 2)
         SELECT CASE(IDFM)
          CASE(1)
            RFORM = '(10X,12F5.0)'
          CASE(2)
            CALL INCSTR ('FORM', RFORM, 'REQ', ' ')
          CASE(5)
            RFORM = '(16F5.0)'
          CASE(6)
            RFORM = '(12F6.0)'
          CASE(8)
            RFORM = '(10F8.0)'
          CASE DEFAULT
            CALL MSGERR (2, 'illegal format number')
            WRITE (PRINTF, "(' -> ', I6)") IDFM
         END SELECT
      ELSE
         CALL WRNKEY
         IDFM = 0
      ENDIF
!       --------------------------------------------------------
!                          open the file
!       --------------------------------------------------------
      IF (IDFM.NE.-1) THEN
         IOSTAT = 0
         CALL FOR (NDSD, FILENM, 'OF', IOSTAT)
         IF (STPNOW()) RETURN
         IF (NHEDF.GT.0) THEN
            WRITE (PRINTF, '(A,A,A)') ' **  Heading lines file ',&
            &TRIM(FILENM), ' **'
            DO IH=1, NHEDF
               READ (NDSD, '(A80)') HEDLIN
               WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
            ENDDO
         ENDIF
      ELSE
         IOSTAT = 0
         CALL FOR (NDSD, FILENM, 'OU', IOSTAT)
         IF (STPNOW()) RETURN
         DO IH=1, NHEDF
            READ (NDSD)
         ENDDO
      ENDIF
   ENDIF
   RETURN
end subroutine REPARM
!*****************************************************************
!                                                                *
SUBROUTINE INAR2D (ARR, MXA, MYA, NDSL, NDSD, IDFM, RFORM,&
&IDLA, VFAC, NHED, NHEDF)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.82: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.08: Erick Rogers
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     01.05, Feb. 90: Before reading values in the array are divided by
!                     in order to retain correct values for points where
!                     value was given
!     01.06, Apr. 91: i/o status is printed if read error occurs
!     30.72, Sept 97: Changed DO-block with one CONTINUE to DO-block wit
!                     two CONTINUE's
!     30.72, Sept 97: Corrected reading of heading lines for SERIES of f
!                     in dynamic mode
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     40.00, July 98: SWAN specific statements modified
!                     unformatted read: heading lines also read unformat
!                     distinction between NDSD (data file) and NDSL (fil
!     30.82, Sep. 98: Added INQUIRE statement to produce correct file na
!                     case of a read error
!     34.01, Feb. 99: Introducing STPNOW
!     40.02, Sep. 00: Replaced computed GOTO with CASE construct
!     40.02, Sep. 00: Replaced reserved words IOSTAT with IOERR and STAT
!     40.03, Jul. 00: END= added to READ statement for correct reading o
!                     of files
!     40.03, Jul. 00: TRIM used to improve readability of message
!     40.13, Apr. 01: END=930 added in READ statement; corresponding err
!     40.08, Mar. 03: Changed an INQUIRE statement so that it does not p
!                     misleading results.
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Reads a 2d array from dataset
!     is used to read e.g. bathymetry, one component of wind velocity
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     IDFM   : input    format index
!     IDLAM  : input    lay-out indicator
!     MXA    : input    number of points along x-side of grid
!     MYA    : input    number of points along y-side of grid
!     NDSD   : input    unit number of the file from which to read the d
!     NDSL   : input    unit number of the file containing the list of f
!     NHEDF  : input    number of heading lines in the file (first lines
!     NHEDL  : input    number of heading lines in the file
!                       before each array

   INTEGER   IDFM, IDLA, MXA, MYA, NDSD, NDSL, NHED, NHEDF

!     ARR    : input    results appear in this array
!     RFORM  : input    format used in reading data (char. string)
!     VFAC   : input    factor by which data must be multiplied.

   REAL      ARR(MXA,MYA), VFAC

   CHARACTER(LEN=*) :: RFORM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IERR   : ??
!     IENT   : number of entries into this subroutine
!     IOERR  : input   0 : Full messages printed
!                      -1: Only error messages printed
!                      -2: No messages printed
!              output  error indicator
!     IH     : ??
!     IX     : ??
!     IY     : ??
!     NUMFIL : ??

   INTEGER, SAVE :: IENT = 0
   INTEGER   IERR, IOERR, IH, IX, IY, NUMFIL

!     HEDLIN : Content of a header line

   CHARACTER(LEN=80) :: HEDLIN

!  8. SUBROUTINE USED

   LOGICAL STPNOW

!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'INAR2D')

IF (NDSD.LT.0) RETURN
!     no reading from file due to open error
!
!     *** NUMFIL is the number of files that is open in one time step  *
   NUMFIL = 0
   IF (ITEST.GE.100) THEN
      WRITE (PRINTF, "(' * TEST INAR2D *', 4I4, 1X, A16, I3, 1X, E12.4, I3)") MXA, MYA, NDSD, IDFM, RFORM,&
      &IDLA, VFAC, NHED
   ENDIF

!     Read heading lines, and print the same:

   input_files: DO
   IERR = 0
   IF (NHED.GT.0) THEN
      IF (IDFM.LT.0) THEN
         IF (ITEST.GE.30)&
         &WRITE (PRINTF, '(I3,A)') NHED, ' Heading lines'
         do IH=1, NHED
            READ (NDSD, IOSTAT=IERR)
            IF (IERR /= 0) EXIT
         end do
      ELSE
         do IH=1, NHED
            READ (NDSD, '(A80)', IOSTAT=IERR) HEDLIN
            IF (IERR /= 0) EXIT
            IF (IH.EQ.1) WRITE (PRINTF, '(A)') ' **  Heading lines  **'
            WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
         end do
      ENDIF
   ENDIF

!     divide existing values in the array by VFAC

   IF (IERR == 0) THEN
      do IY = 1, MYA
         do IX = 1, MXA
            ARR(IX,IY) = ARR(IX,IY) / VFAC
         end do
      end do

!     start reading of 2D-array

      IF (IDFM.EQ.0) THEN
!       free format read
         SELECT CASE(IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, *, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, *, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ELSE IF (IDFM.GT.0) THEN
!       read with fixed format
         SELECT CASE (IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, RFORM, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, RFORM, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ELSE
!       unformatted read
         SELECT CASE(IDLA)
          CASE(1)
            DO IY=MYA, 1, -1
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(2)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=MYA,1,-1)
          CASE(3)
            DO IY=1, MYA
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IX=1,MXA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(4)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IX=1,MXA), IY=1,MYA)
          CASE(5)
            DO IX=1, MXA
               READ (NDSD, IOSTAT=IERR) (ARR(IX,IY), IY=1,MYA)
               IF (IERR /= 0) EXIT
            ENDDO
          CASE(6)
            READ (NDSD, IOSTAT=IERR) &
               ((ARR(IX,IY), IY=1,MYA), IX=1,MXA)
         END SELECT
      ENDIF
   ENDIF

   IF (IERR == 0) EXIT input_files

!     *** End of data file, in case SERIES next file is opened
!     *** unit = NDSD is closed before the next one is opened

   IF (IS_IOSTAT_END(IERR)) THEN
      CLOSE(NDSD)
      NUMFIL = NUMFIL + 1
      IF (NUMFIL < 2 .AND. NDSL > 0) THEN
         READ (NDSL, '(A)', IOSTAT=IOERR) FILENM
         IF (IOERR /= 0) THEN
            FILENM='UNKNOWN_FILE'
            INQUIRE (UNIT=NDSL, NAME=FILENM)
            CALL MSGERR (2, 'Series of input files ended in '//TRIM(FILENM))
            RETURN
         END IF
      IF (IDFM.NE.-1) THEN
         IOERR = 0
         CALL FOR (NDSD, FILENM, 'OF', IOERR)
         IF (STPNOW()) RETURN
      ELSE
         IOERR = 0
         CALL FOR (NDSD, FILENM, 'OU', IOERR)
         IF (STPNOW()) RETURN
      ENDIF

!       Read heading lines, and print these:

      IF (NHEDF.GT.0) THEN
         IF (IDFM.LT.0) THEN
            IF (ITEST.GE.30) WRITE (PRINTF, '(I3,A,A)') NHEDF,&
            &' Heading lines at begin of file ', TRIM(FILENM)
            do IH=1, NHEDF
               READ (NDSD, IOSTAT=IERR)
               IF (IERR /= 0) EXIT
            end do
         ELSE
            WRITE (PRINTF, '(A,A,A)') ' **  Heading lines file ',&
            &TRIM(FILENM), ' **'
            do IH=1, NHEDF
               READ (NDSD, '(A80)', IOSTAT=IERR) HEDLIN
               IF (IERR /= 0) EXIT
               WRITE (PRINTF, '(A4,A80)') ' -> ', HEDLIN
            end do
         ENDIF
      ENDIF
         IF (IERR == 0) CYCLE input_files
      ENDIF

!     error message when end of file is encountered
!
!     --- initialize FILENM so that previous value is not used
!         in case unit NDSD does not exist
      FILENM='UNKNOWN_FILE'
!     ------------------------------------------------------------------
!     THIS INQUIRE STATEMENT IS PROBLEMATIC, SINCE (AT LEAST
!     SOMETIMES) NDSD HAS ALREADY BEEN CLOSED, SO THE INQUIRE
!     STATEMENT SHOULD NOT WORK.
!     ------------------------------------------------------------------
   INQUIRE (UNIT=NDSD, NAME=FILENM)
   CALL MSGERR (2, 'Unexpected end of file while reading '//&
   &TRIM(FILENM))
   NDSD = 0
      IDLA = -1
!     Value of IDLA=-1 signals end of file to calling program

   ELSE

!     --- initialize FILENM
      FILENM='UNKNOWN_FILE'
      INQUIRE (UNIT=NDSD, NAME=FILENM)
      CALL MSGERR (2, 'Error while reading file '//TRIM(FILENM))
      WRITE (PRINTF, "(' i/o status ', I6)") IERR
      IDLA = -2
!     Value of IDLA=-2 signals read error to calling program
   END IF
   EXIT input_files
   END DO input_files

!     Multiply all values in the array by VFAC

do IY = 1, MYA
      do IX = 1, MXA
         ARR(IX,IY) = ARR(IX,IY) * VFAC
      end do
end do

IF (ITEST.GE.100 .OR. IDLA.LT.0) THEN
      do IY=MYA, 1, -1
         WRITE (PRINTF, "((1X, 10E12.4))") (ARR(IX,IY), IX=1,MXA)
      end do
   ENDIF
   RETURN

end subroutine INAR2D
!*****************************************************************
!                                                                *
SUBROUTINE STRACE (IENT, SUBNAM)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4
   USE M_PARALL

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. PURPOSE
!
!     This subroutine produces depending on the value of 'ITRACE'
!     a message containing the name 'SUBNAM'. the purpose of this
!     action is to detect the entry of a subroutine.
!
!  3. METHOD
!
!     the first executable statement of subroutine 'AAA' has to
!     be : CALL STRACE(IENT,'AAA')
!     further is necessary : DATA IENT/0/
!     IF ITRACE=0, no message
!     IF ITRACE>0, a message is printed up to ITRACE times
!
!  4. ARGUMENT VARIABLES
!
!     IENT   :  i/o    Number of entries into the calling subroutine

   INTEGER IENT

!     SUBNAM :  inp    name of the calling subroutine.

   CHARACTER(LEN=*) :: SUBNAM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!$ LOGICAL,EXTERNAL :: OMP_IN_PARALLEL
!
!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   IF (ITRACE.EQ.0) RETURN
   IF (IENT.GT.ITRACE) RETURN
!$ IF (OMP_IN_PARALLEL()) THEN
!$OMP MASTER
!$    IENT=IENT+1
!$    WRITE (PRTEST, "(' ++ trace subr: ',A)") SUBNAM
!$    IF (SCREEN.NE.PRINTF) WRITE (SCREEN, "(' ++ trace subr: ',A)") SUBNAM
!$OMP END MASTER
!$ ELSE
      IENT=IENT+1
      WRITE (PRTEST, "(' ++ trace subr: ',A)") SUBNAM
      IF ( SCREEN.NE.PRINTF .AND. IAMMASTER )&
      &WRITE (SCREEN, "(' ++ trace subr: ',A)") SUBNAM
!$ ENDIF
   RETURN
!  *  END OF SUBR. STRACE  *
end subroutine STRACE
!*****************************************************************
!                                                                *
SUBROUTINE MSGERR (LEV,STRING)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4
   USE M_PARALL

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.03, Aug. 00: variable ERRFNM introduced in order to get correct
!                     message on UNIX system
!     40.02, Sep. 00: Removed STOP statement
!     40.13, Nov. 01: OPEN statement instead of CALL FOR
!                     to prevent recursive subroutines calling
!     40.30, Jan. 03: introduction distributed-memory approach using MPI
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. PURPOSE
!
!     Error messages are produced by subroutine MSGERR. if necessary
!     the value of LEVERR is increased.
!     In case of a high error level an error message file is opened
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     LEV    : indicates how severe the present error is
!     STRING : contents of the present error message

   INTEGER   LEV

   CHARACTER(LEN=*) :: STRING

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IERR   : if non-zero error message file was already opened unsucce
!     IERRF  : unit reference number of the error message file
!     ILPOS  : actual length of error message filename

   INTEGER, SAVE :: IERR=0, IERRF=0
   INTEGER ILPOS

!     ERRM   : error message prefix

   CHARACTER (LEN=17) :: ERRM

!     ERRFNM : name of error message file

   CHARACTER (LEN=LENFNM), SAVE :: ERRFNM = 'Errfile'

!  8. SUBROUTINE USED
!
!     ---
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT


   IF (LEV.GT.LEVERR) LEVERR=LEV
   IF (LEV.EQ.0) THEN
      ERRM = 'Message          '
   ELSE IF (LEV.EQ.1) THEN
      ERRM = 'Warning          '
   ELSE IF (LEV.EQ.2) THEN
      ERRM = 'Error            '
   ELSE IF (LEV.EQ.3) THEN
      ERRM = 'Severe error     '
   ELSE
      ERRM = 'Terminating error'
   ENDIF
   WRITE (PRINTF,"(' ** ', A, ': ',A)") ERRM, STRING
   IF (LEV.GT.MAXERR) THEN
      IF (IERRF.EQ.0) THEN
         IF (IERR.NE.0) RETURN

!         append node number to ERRFNM in case of
!         parallel computing

         IF (PARLL) THEN
            ILPOS = INDEX ( ERRFNM, ' ' )-1
            WRITE(ERRFNM(ILPOS+1:ILPOS+4),"('-',I3.3)") INODE
         END IF

         IERRF = 17
         OPEN (UNIT=IERRF, FILE=ERRFNM, FORM='FORMATTED')
      ENDIF
      WRITE (IERRF,"(A, ': ',A)") ERRM, STRING
   ENDIF

   RETURN

end subroutine MSGERR

!*****************************************************************
!                                                                *
LOGICAL FUNCTION STPNOW()
!                                                                *
!*****************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.82, Feb. 99: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.82: New function
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     Function determines wheter the SWAN program should be stopped
!     due to a terminating error
!
!  3. Method
!
!     Compares two common variables (the maximum allowable error-level,
!     MAXERR and the actual error-level: LEVERR).
!
!  4. ARGUMENT VARIABLES
!
!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IENT  : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

!  8. SUBROUTINE USED
!
!$ LOGICAL,EXTERNAL :: OMP_IN_PARALLEL
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL  STRACE (IENT,'STPNOW')

   IF (LEVERR .GE. 4) THEN
      STPNOW = .TRUE.
   ELSE
      STPNOW = .FALSE.
   END IF
   IF (MAXERR.EQ.-1) STPNOW = .FALSE.
!$ IF (OMP_IN_PARALLEL()) STPNOW = .FALSE.

   RETURN
end function STPNOW
!*****************************************************************
!                                                                *
SUBROUTINE TABHED (PROGNM, LPR)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.13, Jan. 01: VERTXT replaces VERNUM
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. PURPOSE
!
!     prints the table heading, containing:
!     run description, 3 lines
!     name of institute, program name,
!     project name, run id.
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     LPR    : input    unit ref. nr. for output

   INTEGER LPR

!     PROGNM : input    program name

   CHARACTER(LEN=*) :: PROGNM

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!  8. SUBROUTINE USED
!
!  9. SUBROUTINES CALLING
!
! 10. ERROR MESSAGES
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   WRITE (LPR, "('1', A72, ' | ', A40)") PROJT1, INST
   WRITE (LPR, "(1X, A72, ' | ', A, ' version: ', A)") PROJT2, PROGNM, VERTXT
   WRITE (LPR, "(1X, A72, ' | ', A16, 1X, A4)") PROJT3, PROJID, PROJNR
   WRITE (LPR, "(' --------------------------------------------------', '---------------------------------------------------------')")
   RETURN
end subroutine TABHED
!*****************************************************************
!                                                                *
SUBROUTINE FOR (IUNIT, DDNAME, SF, IOSTAT)
!                                                                *
!*****************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.13: Nico Booij
!     30.70: Nico Booij
!     30.82: IJsbrand Haagsma
!     34.01: IJsbrand Haagsma
!     40.00, 40.03: Nico Booij
!     40.41: Marcel Zijlema
!     41.20: Casey Dietrich
!
!  1. Updates
!
!     30.13, Jan. 96: new structure
!     30.70, Feb. 98: terminating error if input file does not exist
!     30.82, Nov. 98: Introduced recordlength of 1000 for new files to
!                     avoid errors on the Cray-J90
!     34.01, Feb. 99: STOP statement removed
!     40.00, Feb. 99: DIRCH2 replaces DIRCH1 in filenames
!     40.03, May  00: modification for Linux: local copy of filename
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!     41.20, Mar. 10: extension to tightly coupled ADCIRC+SWAN model
!
!  1. PURPOSE
!
!     General open file routine.
!
!  2. METHOD
!
!     FORTRAN 77 OPEN option.
!                INQUIRE
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!       IUNIT   int     input   =0 : get free unit number
!                               >0 : fixed unit number
!                       output  allocated unit number
!       DDNAME  char    input   ddname/filename string (empty if IUNIT>0
!       SF      char*2  input   file qualifiers
!                               1st char: O(ld),N(ew),S(cratch),U(nknown
!                               2nd char: F(ormatted),U(nformatted)
!       IOSTAT  int     input   0 : Full messages printed
!                               -1: Only error messages printed
!                               -2: No messages printed
!                       output  error indicator

   INTEGER   IUNIT, IOSTAT
   CHARACTER(LEN=LENFNM) :: DDNAME
   CHARACTER(LEN=2) :: SF

!  5. PARAMETER VAR. (CONSTANTS)
!
!     Error codes:
!
!       IOSTAT = IESUCC No errors
!       IOSTAT > 0      I/O error
!       IOSTAT = IENUNF No free unit number found
!       IOSTAT = IEUNBD Specified unit number out of bounds
!       IOSTAT = IENODD No filename supplied with IUNIT=0
!       IOSTAT = IEDDNM Incorrect filename supplied with IUNIT>0
!       IOSTAT = IEEXST Specified unit number does not exist
!       IOSTAT = IEOPEN Specified unit number already opened
!       IOSTAT = IESTAT Error in file qualifiers
!       IOSTAT = IENSCR Named scratch file
!       IOSTAT = IENSIO No specified I/O error

   INTEGER, PARAMETER :: IESUCC=0, IENUNF=-1, IEUNBD=-2, IENODD=-3
   INTEGER, PARAMETER :: IEDDNM=-4, IEEXST=-5, IEOPEN=-6, IESTAT=-7
   INTEGER, PARAMETER :: IENSCR=-12

!     EMPTY    blank string

   CHARACTER(LEN=*), PARAMETER :: EMPTY='        '

!  6. LOCAL VARIABLES
!
!     IENT      number of entries into this subroutine
!     IFO       format index
!     IFUN      free unit number
!     II        counter
!     IOSTTM    aux. error index
!     IS        file status index
!     IUTTM     aux. unit number

   INTEGER, SAVE :: IENT = 0
   INTEGER, SAVE :: IFUN = 0
   INTEGER   IFO, II, IOSTTM, IS, IUTTM

!     EXIST     if true, file exists
!     OPENED    if true, file is opened

   LOGICAL   EXIST, OPENED

!     S
!     F
!     FILTTM   auxiliary
!     FISTAT   file status, values: OLD, NEW, UNKNOWN
!     FORM     formatting, values: FORMATTED, UNFORMATTED
!     DDNAME_L local copy of DDNAME

   CHARACTER :: S, F
   CHARACTER(LEN=LENFNM) :: FILTTM, DDNAME_L
   CHARACTER(LEN=11), PARAMETER :: FISTAT(4) = &
      [CHARACTER(LEN=11) :: 'OLD', 'NEW', 'SCRATCH', 'UNKNOWN']
   CHARACTER(LEN=11), PARAMETER :: FORM(2) = &
      [CHARACTER(LEN=11) :: 'FORMATTED', 'UNFORMATTED']

!  4. SUBROUTINES USED
!
!
!  5. ERROR MESSAGES
!
!       and error messages added using MSGERR
!
!
!  6. REMARKS
!
!       Free unit number search interval: FUNLO<=IUNIT<=FUNHI
!       FUNLO, FUNHI, IUNMIN and IUNMAX were initialized by OCPINI,
!       they are transmitted via module OCPCOMM4
!
!  7. STRUCTURE
!
!       ----------------------------------------------------------------
!       Check file qualifiers
!       ----------------------------------------------------------------
!       If IUNIT = 0
!       Then If DDNAME = ' '
!            Then error message
!            Else Inquire to find if file exists and is opened,
!                 and if so, to find correct unit number
!                 If file is not opened
!                 Then get a free unit number, assign value to IUNIT
!                      open the file
!                 Else assign correct unit number to IUNIT
!       Else Inquire to find if file exists and is opened,
!                   and if so, to find correct filename
!            If file with unit nr IUNIT is already open
!            Then If filename does not correspond to DDNAME
!                 Then Close file with old filename and unit IUNIT
!                      Open file with new filename DDNAME and unit IUNIT
!            Else If DDNAME is not empty
!                 Then Open file with new filename DDNAME and unit IUNIT
!                 Else Open file with unit IUNIT
!       ----------------------------------------------------------------
!
!  8. SOURCE TEXT

   CALL STRACE (IENT, 'FOR')

   IF (ITEST.GE.80) WRITE (PRTEST, "(' Entry FOR: ', I3, 1X, A36, A2, I7)") IUNIT, DDNAME, SF, IOSTAT
   DDNAME_L = DDNAME

!     check file qualifiers

   IF ((IUNIT.NE.0) .AND.&
   &((IUNIT .LT. IUNMIN) .OR. (IUNIT .GT. IUNMAX))) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3, 'Unit number out of range')
      IOSTAT= IEUNBD
      RETURN
   END IF

   S   = SF(1:1)
   F   = SF(2:2)
   IS  = INDEX('ONSU',S)
   IFO = INDEX('FU',F)
   IF ((IS .EQ. 0) .OR. (IFO .EQ. 0)) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3,'Error in file qualifiers')
      IOSTAT= IESTAT
      RETURN
   END IF

   IF ((S.EQ.'S').AND.(DDNAME.NE.EMPTY)) THEN
      IF (IOSTAT.GT.-2) CALL MSGERR (3, 'Named scratch file')
      IOSTAT= IENSCR
      RETURN
   END IF

   IF (DDNAME.NE.EMPTY) THEN
!       directory separation character is replaced in filenames
      DO II = 1, LEN(DDNAME)
         IF (DDNAME(II:II).EQ.DIRCH1) DDNAME(II:II) = DIRCH2
      ENDDO
   ENDIF

   IF (IUNIT .EQ. 0) THEN
      IF (DDNAME.EQ.EMPTY) THEN
         IF (IOSTAT.GT.-1) CALL MSGERR (3, 'No filename given')
         IOSTAT= IENODD
         RETURN
      ELSE
!           Was the file opened already ?
         INQUIRE (FILE=DDNAME, IOSTAT=IOSTTM, EXIST=EXIST,&
         &OPENED=OPENED, NUMBER=IUTTM)
         IF (IOSTTM .NE. IESUCC) THEN
            IF (IOSTAT.GT.-1) CALL MSGERR (2,&
            &'Inquire failed, filename: '//DDNAME_L)
            IOSTAT = IOSTTM
            RETURN
         ENDIF
!           If file does not exist, print term. error
         IF (IS.EQ.1 .AND. .NOT. EXIST) THEN
            CALL MSGERR (4,&
            &'File cannot be opened/does not exist: '//DDNAME_L)
            IOSTAT = IEEXST
            RETURN
         END IF
         IF (OPENED) THEN
            IF (IOSTAT.GT.-1)&
            &CALL MSGERR (2, 'File is already opened: '//DDNAME_L)
            IOSTAT = IEOPEN
            IUNIT = IUTTM
            RETURN
         ENDIF
!           Assign free unit number
         DO
            IF (IFUN.EQ.0) THEN
               IFUN = FUNLO
            ELSE
               IFUN = IFUN + 1
            ENDIF
!Casey 160728: Merging the changes from Jason in an earlier version of S
            IF (IFUN.LT.411 .OR. IFUN.GT.417) EXIT
         END DO
         IUNIT = IFUN
         IF (IUNIT .GT. FUNHI) THEN
            IF (IOSTAT.GT.-2) CALL MSGERR (3, 'All free units used')
            IOSTAT= IENUNF
         ENDIF
      END IF
      OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,FILE=DDNAME,&
!/Cray      &RECL=1000,&
!/SGI      &RECL=1000,&
!CVIS      &SHARED,&
      &STATUS=FISTAT(IS),ACCESS='SEQUENTIAL',FORM=FORM(IFO))
      IF (open_failed()) RETURN
   ELSE
      INQUIRE (UNIT=IUNIT, NAME=FILTTM, IOSTAT=IOSTTM,&
      &EXIST=EXIST, OPENED=OPENED)
      IF (IOSTTM .NE. IESUCC) THEN
         IF (IOSTAT.GT.-1) CALL MSGERR (2,&
         &'Inquire failed, filename: '//FILTTM)
         IOSTAT = IOSTTM
         RETURN
      ENDIF
      IF (OPENED) THEN
         IF (IOSTAT.GT.-1) THEN
            CALL MSGERR (1,&
            &'File is already opened, filename: '//FILTTM)
         ENDIF
         IF (FILTTM.NE.DDNAME .AND. FILTTM.NE.EMPTY) THEN
            IF (IOSTAT.GT.-2) THEN
               WRITE (PRINTF, '(A, I4, 6A)') ' unit', IUNIT,&
               &' filenames: ', FILTTM, ' and: ', DDNAME
               CALL MSGERR (2, 'filename and unit number inconsistent')
            ENDIF
            IOSTAT = IEDDNM
!             close old file and open new one with given filename
            CLOSE (IUNIT)
            OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray            &RECL=1000,&
!/SGI            &RECL=1000,&
!CVIS            &SHARED,&
            &FILE=DDNAME,ACCESS='SEQUENTIAL',FORM=FORM(IFO))
            IF (open_failed()) RETURN
            IF (IOSTTM.NE.IESUCC) IOSTAT = IOSTTM
            HIOPEN = IFUN
            IF (ITEST.GE.30) WRITE (PRINTF, "(' File opened: ', I6, 2X, A36, 2X, A2)") IUNIT, DDNAME, SF
            RETURN
         ENDIF
         IOSTAT = IEOPEN
         RETURN
      END IF
      IF (DDNAME.NE.EMPTY) THEN
         OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray         &RECL=1000,&
!/SGI         &RECL=1000,&
!CVIS         &SHARED,&
         &FILE=DDNAME,ACCESS='SEQUENTIAL',FORM=FORM(IFO))
         IF (open_failed()) RETURN
      ELSE
         OPEN (UNIT=IUNIT,IOSTAT=IOSTTM,STATUS=FISTAT(IS),&
!/Cray         &RECL=1000,&
!/SGI         &RECL=1000,&
!CVIS         &SHARED,&
         &ACCESS='SEQUENTIAL',FORM=FORM(IFO))
         IF (open_failed()) RETURN
      END IF
   END IF
   HIOPEN = IFUN
IF (ITEST.GE.30) WRITE (PRINTF, "(' File opened: ', I6, 2X, A36, 2X, A2)") IUNIT, DDNAME, SF
   RETURN

CONTAINS

   LOGICAL FUNCTION open_failed()
      open_failed = IOSTTM.NE.IESUCC
      IF (.NOT.open_failed) RETURN
      IF (IOSTAT.GT.-2) THEN
         CALL MSGERR (3, 'File open failed, filename: '//DDNAME_L)
         WRITE (PRINTF,"(' File -> ', A36, 2X, ' IOSTAT=', I6, 4X, A2)") DDNAME, IOSTTM, SF
      ENDIF
      IUNIT = -1
      IOSTAT = IOSTTM
   END FUNCTION open_failed

!  *  end of subroutine  FOR  *
end subroutine FOR
!***********************************************************************
!                                                                      *
LOGICAL FUNCTION EQREAL (REAL1, REAL2 )
!                                                                      *
!***********************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     30.72 IJsbrand Haagsma
!     30.60 Nico Booij
!     40.04 Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Oct. 97: Changed from EXCYES to make floating point point c
!     30.60, July 97: new subroutine (EXCYES)
!     40.04, Aug. 00: introduced EPSILON and TINY
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. Purpose
!
!     to determine whether a value (usually a value read from file)
!     is an exception value or not
!     Later (30.72) used to make comparisons of floating points within r
!
!  3. Method (updated...)
!
!     Checks whether ABS(REAL1-REAL2) .LE. TINY(REAL1) or whether this
!     difference is .LE. then EPS (= EPSILON(REAL1)*ABS(REAL1-REAL2) )
!
!  4. Argument variables
!
!     REAL1  : input    value that is to be tested
!     REAL2  : input    given exception value

   REAL      REAL1, REAL2

!  5. Parameter variables
!
!  6. Local variables
!
!     EPS    : Small number (related to REAL1 and its difference with RE
!     IENT   : Number of entries into this subroutine

   REAL      EPS
   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWREAD
!     SWDIM
!     SIRAY
!     SWBOUN
!     SWODDC
!     SWOEXD
!     SWOEXA
!     SWOEXF
!     SWPLOT
!     SWSPEC
!     ISOLIN
!     SNYPT2
!     INCTIM
!     INDBLE
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
! 13. Source text

   CALL STRACE(IENT,'EQREAL')
   EQREAL = .FALSE.

   EPS = EPSILON(REAL1)*ABS(REAL1-REAL2)
   IF (EPS ==0) EPS = TINY(REAL1)
   IF (ABS(REAL1-REAL2) .GT. TINY(REAL1)) THEN
      IF (ABS(REAL1-REAL2) .LT. EPS) EQREAL = .TRUE.
   ELSE
      EQREAL = .TRUE.
   ENDIF
   RETURN
!     end of subroutine EQREAL
end function EQREAL
!***********************************************************************
!                                                                      *
LOGICAL FUNCTION EQDBLE (DBLE1, DBLE2)
!                                                                      *
!***********************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     Marcel Zijlema
!
!  1. Updates
!
!     July 2015: copied from EQREAL, adapted to REAL(KIND=KIND(0.0D0))
!
!  2. Purpose
!
!     to determine whether a value is an exception value or not
!
!  4. Argument variables
!
!     DBLE1  : input    value that is to be tested
!     DBLE2  : input    given exception value

   REAL(KIND=KIND(0.0D0))    DBLE1, DBLE2

!  5. Parameter variables
!
!  6. Local variables
!
!     IENT   : Number of entries into this subroutine

   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!  9. Subroutines calling
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
! 13. Source text

   CALL STRACE(IENT,'EQDBLE')
   EQDBLE = .FALSE.

   IF ( .NOT. DBLE1 /= DBLE2 ) EQDBLE = .TRUE.
   RETURN
!     end of subroutine EQDBLE
end function EQDBLE
!*******************************************************************
!                                                                  *
SUBROUTINE LSPLIT(RELINE, DATITM, NUMITM)
!                                                                  *
!*******************************************************************

   USE OCPCOMM1
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     40.00, 40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.00, Jan. 98: New subroutine for SWAN
!     40.03, Jun. 00: declaration updated, TRIM added for readability
!                     test output added
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. PURPOSE
!
!     a line read from a file is separated into single data items
!     each data item is found in a string DATITM
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     NUMITM : input    max number of data items in array

   INTEGER, INTENT(IN) :: NUMITM

!     DATITM : output   array of data items
!     RELINE : input    string (read from an input file)

   CHARACTER (LEN=*), INTENT(OUT) :: DATITM(NUMITM)
   CHARACTER (LEN=*), INTENT(IN) ::  RELINE

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     CRL    : a character of the input line RELINE
!     QUOTE  : ' i.e. string delimiter

   CHARACTER(LEN=1), PARAMETER :: QUOTE = "'"
   CHARACTER(LEN=1) :: CRL

!     ICR1   : ??
!     IENT   : Number of entries into this subroutine
!     ILL    : sequence number of character being processed
!     IITM   : counter of data items
!     LENLIN : lenght of an input line
!     RITM   : type of data, 0: empty string, 2: string enclosed
!              in quotes, 1: other

   INTEGER, SAVE :: IENT = 0
   INTEGER   ICR1, ILL, IITM, LENLIN, RITM

!     LCHSTR : if True, program is reading a string (enclosed in quotes)

   LOGICAL   LCHSTR

!  8. SUBROUTINE USED
!
!     ------
!
!  9. SUBROUTINES CALLING
!
!     SWBOUN
!
! 10. ERROR MESSAGES
!
!     Too many data items on input line
!
! 11. REMARKS
!
! 12. STRUCTURE
!
! 13. SOURCE TEXT

   CALL STRACE (IENT, 'LSPLIT')

   LENLIN = LEN(RELINE)
   LCHSTR = .FALSE.
   RITM   = 1
   ICR1   = 1
   DO IITM = 1, NUMITM
      DATITM(IITM) = '    '
   ENDDO
   IF (ITEST.GE.150) WRITE (PRTEST,*) ' test LSPLIT ', RELINE

!     free format: separate the line into data items
!     blanks and commas serve as separation between data items
!     DATITM is string containing one data item

   IITM = 1
   do ILL = 1, LENLIN
      CRL = RELINE(ILL:ILL)
      IF (LCHSTR) THEN
!           reading a character string enclosed in quotes
         IF (CRL.EQ.QUOTE) THEN
!              closing quote
            LCHSTR = .FALSE.
            RITM   = 2
            IF (IITM.GT.NUMITM) THEN
               CALL MSGERR (2, 'too many items on input line')
               WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
            ENDIF
            DATITM(IITM) = RELINE (ICR1:ILL-1)
         ENDIF
      ELSE
         IF (CRL.EQ.',') THEN
            IF (RITM.EQ.0) THEN
!                 empty item
               IITM = IITM + 1
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               DATITM(IITM) = '    '
            ELSE
               IF (RITM.EQ.1) DATITM(IITM) = RELINE(ICR1:ILL)
               RITM = 0
            ENDIF
         ELSE IF (CRL.EQ.' ' .OR. CRL.EQ.TABC) THEN
            IF (RITM.EQ.1) THEN
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               DATITM(IITM) = RELINE(ICR1:ILL)
               RITM = 2
            ENDIF
         ELSE
            IF (RITM.NE.1) THEN
               IITM = IITM + 1
               IF (IITM.GT.NUMITM) THEN
                  CALL MSGERR (2, 'too many items on input line')
                  WRITE (PRINTF, *) ' -> ', TRIM(RELINE)
               ENDIF
               IF (CRL.EQ.QUOTE) THEN
                  ICR1 = ILL+1
                  LCHSTR = .TRUE.
               ELSE
                  ICR1 = ILL
                  RITM = 1
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF (ITEST.GE.250) WRITE (PRTEST, "(' test LSPLIT ', A1, 3I3, 2X, A20)") CRL, RITM,&
      &IITM, ICR1
   end do
   IF (ITEST.GE.130) THEN
      DO IITM = 1, NUMITM
         WRITE (PRTEST, "(' LSPLIT data item ', I2, ' is: ', A)") IITM, DATITM(IITM)
      ENDDO
   ENDIF
   RETURN
end subroutine LSPLIT
!***********************************************************************
!                                                                      *
SUBROUTINE BUGFIX (FIXABC)
!                                                                      *
!***********************************************************************

   USE OCPCOMM2


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. Authors
!
!     40.03  Nico Booij
!
!  1. UPDATE
!
!     40.03, May  00: new subroutine
!
!  2. PURPOSE
!
!     Adding one character to the version character string
!
!  3. METHOD
!
!
!  4. Argument variables
!
!       FIXABC  char   input    character indicating a bugfix

   CHARACTER (LEN=1), INTENT(IN) :: FIXABC

!  5. Parameter variables
!
!  6. Local variables
!
!       IC      counter of characters

   INTEGER   IC

!  8. Subroutines used
!
!  9. Subroutines calling
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       for characters in VERTXT starting at end, do
!           if character is not blank
!           then replace previous character by FIXABC
!       ----------------------------------------------------------------
!
! 13. Source text

   DO IC = LEN(VERTXT), 1, -1
      IF (VERTXT(IC:IC) .NE. ' ') THEN
         VERTXT(IC+1:IC+1) = FIXABC
         EXIT
      ENDIF
   ENDDO
   RETURN
!     end of subroutine BUGFIX
end subroutine BUGFIX
!***********************************************************************
!                                                                      *
SUBROUTINE COPYCH (STRING, MOVE, IARRAY, LENARR, IERR)
!                                                                      *
!***********************************************************************

   USE OCPCOMM4

   IMPLICIT NONE


!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmers: The SWAN team                                |
!   --|-----------------------------------------------------------|--
!
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
!
!
!  0. AUTHORS
!
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     40.03: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     30.72, Sept 97: INTEGER(KIND=SELECTED_INT_KIND(9)) replaced by INTEGER
!     ver 30.01
!     30.81, Nov. 98: Replaced variable STATUS by IERR (because STATUS i
!                     reserved word)
!     30.81, Jan. 99: Replaced variable FROM by FROM_ and TO by TO_ (bec
!                     FROM and TO are reserved words)
!     40.03, Nov. 99: LENS2 removed from WRITE statement (value not yet
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!
!  2. PURPOSE
!
!     copy a string into an integer array or vice-versa
!     MOVE (TO_ or FROM_) indicates copying direction
!
!  3. METHOD
!
!     ---
!
!  4. ARGUMENT VARIABLES
!
!     IARRAY : output   an integer array
!     LENARR : input    length of array IARRAY
!     IERR   : output   error status: 0=no error, 9=end-of-file

   INTEGER, SAVE :: IENT = 0
   INTEGER   IARRAY(*), LENARR, &
   &IERR

!     STRING : i/o      a character string
!     MOVE   : input    if MOVE=TO_, STRING is copied to IARRAY
!                       if MOVE=FROM_, STRING is copied from IARRAY

   CHARACTER(LEN=1) :: MOVE
   CHARACTER(LEN=*) :: STRING

!  5. PARAMETER VARIABLES
!
!     OPMLFC : largest allowed integer character (ASCII) code + 1
!     OPMNLI : number of characters that can be stored in one integer nu

   INTEGER, PARAMETER :: OPMNLI=4, OPMLFC=128

!  6. LOCAL VARIABLES
!
!     IC     : counter
!     IENT   : number of entries into this subroutine
!     II     : counter
!     LENS1  : length of a string
!     LENS2  : length of a string
!     LL     : integer representation of a character
!     MC1    : integer converted to/from character
!     MCHAR  : integer converted to/from character
!     MM     : aux. number
!     NSL    : position of character in string

   INTEGER   IC, II, LENS1, LENS2, LL, MC1, MCHAR, MM, NSL

!     CC     : a single character
!     CHAR   : intrinsic character function, translates integer to chara
!     FROM_  : 'F'
!     TO_    : 'T'

   CHARACTER(LEN=1) :: CC
   CHARACTER(LEN=1), PARAMETER :: TO_ = 'T', FROM_ = 'F'

!  8. SUBROUTINE USED
!
!     CHAR, ICHAR (intrinsic functions)
!
!  9. SUBROUTINES CALLING
!
!     ---
!
! 10. ERROR MESSAGES
!
!     If PINDEX or PPLACE is out of range an error message is printed
!
! 11. REMARKS
!
!     ---
!
! 12. STRUCTURE
!
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
! 13. SOURCE TEXT

   CALL STRACE (IENT,'COPYCH')

   LENS1 = LEN(STRING)
   IF (LENARR.GT.360) THEN
      CALL MSGERR (2, 'extremely long string in COPYCH')
      WRITE (PRTEST, *) ' test COPYCH  ',&
      &MOVE, LENARR, LENS1, ' ', STRING(1:80)
      LENARR = 360
   ENDIF
   LENS2 = LENARR*OPMNLI

   IF (MOVE .EQ. TO_) THEN
      NSL = 0
      do II = 1, LENARR
         MCHAR = 0
         do IC = 1, OPMNLI
            NSL = NSL + 1
            IF (NSL .LE. LENS1) THEN
               CC = STRING(NSL:NSL)
            ELSE
               CC = ' '
            ENDIF
            LL = ICHAR(CC)
            IF (LL.GE.OPMLFC) THEN
               IERR = 803
               WRITE (PRTEST, "(' character cannot be copied: ', A1)") CC
               LL = ICHAR ('?')
            ENDIF
            MCHAR = OPMLFC*MCHAR + LL
!            IF (ITEST.GE.250) WRITE (PRTEST, *) NSL, CC, LL
         end do
         IARRAY(II) = MCHAR
      end do
      IF (LENS1.GT.LENS2) THEN
         check_capacity: do II = LENS1+1, LENS2
            IF (STRING(II:II) .NE. ' ') THEN
               IERR = 801
               CALL MSGERR(1, 'string longer than capacity of array')
               IF (ITEST.GE.50) WRITE (PRTEST, *) ' test COPYCH  ',&
               &MOVE, LENARR, LENS1, LENS2, ' ', STRING(1:80)
               EXIT check_capacity
            ENDIF
         end do check_capacity
      ENDIF
   ELSE IF (MOVE .EQ. FROM_) THEN

!       character string copied from an array
!
!       first the string is filled with blanks
      STRING = '    '
      NSL = 0
      unpack_words: do II = 1, LENARR
         MC1 = IARRAY(II)
         do IC = 1, OPMNLI
            MM  = OPMLFC ** (OPMNLI-IC)
            LL  = MC1 / MM
            NSL = NSL + 1
            IF (NSL .LE. LENS1) THEN
               STRING(NSL:NSL) = CHAR(LL)
            ELSE
               IF (CHAR(LL) .NE. ' ') THEN
                  IF (IERR.NE.802)&
                  &CALL MSGERR(1, 'string shorter than capacity of array')
                  IF (ITEST.GE.50) WRITE (PRTEST, *) ' test COPYCH  ',&
                  &MOVE, LENARR, LENS1, LENS2, ' ', STRING
                  IERR = 802
                  EXIT unpack_words
               ENDIF
            ENDIF
            MC1 = MC1 - LL * MM
!           IF (ITEST.GE.250) WRITE (PRTEST, *) NSL, LL, STRING(NSL:NSL)
         end do
         IF (MC1.NE.0) WRITE (PRINTF, *) ' Error COPYCH'
      end do unpack_words
   ELSE
      CALL MSGERR (2, 'error COPYCH, argument MOVE')
   ENDIF
   IF (ITEST.GE.230) WRITE (PRTEST, "(' exit COPYCH ', I3, 1X, A20, 1X, A1, 4(1X,I12))") LENS1, STRING, MOVE,&
   &(IARRAY(II), II=1,LENARR)
   RETURN
!*    end of subroutine COPYCH   **
end subroutine COPYCH
