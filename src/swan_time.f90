MODULE swan_time
   USE swan_kinds, ONLY: swan_double
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: time_context_t, default_time_context
   PUBLIC :: DTTIME, DTINTI, DTRETI, DTTIWR

   TYPE :: time_context_t
      REAL(swan_double) :: TINIC = 0.0_swan_double
      REAL(swan_double) :: DT    = 0.0_swan_double
      REAL(swan_double) :: TFINC = 0.0_swan_double
      REAL(swan_double) :: TIMCO = 0.0_swan_double
      INTEGER :: IFACMX = 0
      INTEGER :: IFACMY = 0
      REAL :: BEGBOU = 0.0
      REAL :: TIMERB = 0.0
      REAL :: TINTBO = 0.0
      INTEGER :: reference_day = 0
      LOGICAL :: reference_set = .FALSE.
   CONTAINS
      PROCEDURE :: reset => reset_time_context
   END TYPE time_context_t

   TYPE(time_context_t), SAVE, TARGET :: default_time_context

! Timing instrumentation remains module state because it measures nested
! process/thread sections rather than simulation-clock state.
!TIMG   INTEGER, PARAMETER, PUBLIC :: NSECTM = 300, MXTIMR = 10
!TIMG   INTEGER, SAVE, PUBLIC :: NCUMTM(NSECTM), LISTTM(MXTIMR), LASTTM
!TIMG   REAL(swan_double), SAVE, PUBLIC :: DCUMTM(NSECTM,2), TIMERS(MXTIMR,2)
!TIMG!$OMP THREADPRIVATE(DCUMTM,TIMERS,NCUMTM,LISTTM,LASTTM)

   INTERFACE
      SUBROUTINE DTSTTI(IOPT, TIMSTR, INTTIME)
         INTEGER, INTENT(IN) :: IOPT
         CHARACTER(LEN=*), INTENT(IN) :: TIMSTR
         INTEGER, INTENT(OUT) :: INTTIME(6)
      END SUBROUTINE DTSTTI
      SUBROUTINE DTTIST(IOPT, TIMSTR, INTTIME)
         INTEGER, INTENT(IN) :: IOPT
         CHARACTER(LEN=*), INTENT(OUT) :: TIMSTR
         INTEGER, INTENT(INOUT) :: INTTIME(6)
      END SUBROUTINE DTTIST
   END INTERFACE

CONTAINS

SUBROUTINE reset_time_context(context)
   CLASS(time_context_t), INTENT(INOUT) :: context

   context%TINIC = 0.0_swan_double
   context%DT = 0.0_swan_double
   context%TFINC = 0.0_swan_double
   context%TIMCO = 0.0_swan_double
   context%IFACMX = 0
   context%IFACMY = 0
   context%BEGBOU = 0.0
   context%TIMERB = 0.0
   context%TINTBO = 0.0
   context%reference_day = 0
   context%reference_set = .FALSE.
END SUBROUTINE reset_time_context

REAL FUNCTION DTTIME (INTTIM, CONTEXT)
!                                                                  *
!*******************************************************************

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
!     This program is free software: you can redistribute it and/or modify
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
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
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
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     DTTIME gives time in seconds from a reference day
!            it also initialises the reference day
!
!  3. Method
!
!     every fourth year is a leap-year, but not the century-years, however
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

   INTEGER, INTENT(IN) :: INTTIM(6)
   TYPE(time_context_t), OPTIONAL, TARGET, INTENT(INOUT) :: CONTEXT
   TYPE(time_context_t), POINTER :: CLOCK

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

!     REFDAY  day number of the reference day; the reference time is 0:00
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

   CLOCK => default_time_context
   IF (PRESENT(CONTEXT)) CLOCK => CONTEXT

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
   IF (.NOT.CLOCK%reference_set) THEN
      CLOCK%reference_day = IDNOW
      CLOCK%reference_set = .TRUE.
      DTTIME = 0.
   ELSE
      DTTIME = REAL(IDNOW-CLOCK%reference_day) * 24.*3600.
   ENDIF
   DTTIME = DTTIME + 3600.*REAL(INTTIM(4)) + 60.*REAL(INTTIM(5)) +&
   &REAL(INTTIM(6))
   RETURN
end function DTTIME

SUBROUTINE DTINTI (TIMESC, INTTIM, CONTEXT)
!                                                                  *
!*******************************************************************

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
!     This program is free software: you can redistribute it and/or modify
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
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
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
!     every fourth year is a leap-year, but not the century-years, however
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

   INTEGER, INTENT(OUT) :: INTTIM(6)
   TYPE(time_context_t), OPTIONAL, TARGET, INTENT(INOUT) :: CONTEXT
   TYPE(time_context_t), POINTER :: CLOCK

!     TIMESC : input  time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0)), INTENT(IN) :: TIMESC

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

   CLOCK => default_time_context
   IF (PRESENT(CONTEXT)) CLOCK => CONTEXT
   IF (.NOT.CLOCK%reference_set) THEN
      ERROR STOP 'DTINTI requires an initialized time context'
   END IF

   NDAY = INT((TIMESC+0.4)/(24*3600))
   DO
      TT = TIMESC - DBLE(NDAY)*24.*3600.
      IF (TT.GE.-0.4) EXIT
      NDAY = NDAY - 1
   END DO
   NOWDAY = CLOCK%reference_day + NDAY

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

SUBROUTINE DTRETI (TSTRNG, IOPT, TIMESC, CONTEXT)
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
!     This program is free software: you can redistribute it and/or modify
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
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
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

   INTEGER, INTENT(IN) :: IOPT

!     TIMESC : output   time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0)), INTENT(OUT) :: TIMESC

!     TSTRNG : input    time string

   CHARACTER(LEN=*), INTENT(IN) :: TSTRNG
   TYPE(time_context_t), OPTIONAL, TARGET, INTENT(INOUT) :: CONTEXT

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     ITIME  : ??

   INTEGER ITIME(6)

   REAL    RTMP

!     DTTIME : Gives time in seconds from a reference day it also initialises the
!              reference day

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
   IF (PRESENT(CONTEXT)) THEN
      RTMP = DTTIME(ITIME, CONTEXT)
   ELSE
      RTMP = DTTIME(ITIME)
   END IF
   TIMESC = DBLE(RTMP)
   RETURN
end subroutine DTRETI

CHARACTER(LEN=18) FUNCTION DTTIWR (IOPT, TIMESC, CONTEXT)
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
!     This program is free software: you can redistribute it and/or modify
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
!     along with this program. If not, see <http://www.gnu.org/licenses/>.
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

   INTEGER, INTENT(IN) :: IOPT

!     TIMESC : output   time in seconds from given reference day REFDAY

   REAL(KIND=KIND(0.0D0)), INTENT(IN) :: TIMESC
   TYPE(time_context_t), OPTIONAL, TARGET, INTENT(INOUT) :: CONTEXT

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

   IF (PRESENT(CONTEXT)) THEN
      CALL DTINTI(TIMESC, ITIME, CONTEXT)
   ELSE
      CALL DTINTI(TIMESC, ITIME)
   END IF
   CALL DTTIST (IOPT, TSTRNG, ITIME)
   DTTIWR = TSTRNG(1:18)
   RETURN
end function DTTIWR

END MODULE swan_time
