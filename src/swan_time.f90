MODULE swan_time
   USE swan_kinds, ONLY: swan_double
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: time_context_t, default_time_context
   PUBLIC :: DTTIME, DTINTI, DTRETI, DTTIWR, DTSTTI, DTTIST
   PUBLIC :: CHTIME, ITMOPT

!     CHTIME : current simulation time rendered as text for headings
   CHARACTER(LEN=20) :: CHTIME

!     ITMOPT : how a date-time is coded in input and output. Every caller
!     passes it straight on to DTTIWR, INCTIM or DTTIST, so it belongs with
!     them rather than with the unit numbers it sat next to in OCPCOMM4.
   INTEGER :: ITMOPT

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

!  DTSTTI and DTTIST used to be external procedures declared here; they are
!  module procedures now. That became possible once UPCASE moved to
!  swan_text_utilities, which removed DTSTTI's dependency on the command parser
!  (the parser uses this module, so the pair formed a cycle).

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

   USE swan_io_units

   IMPLICIT NONE(TYPE, EXTERNAL)


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

   IMPLICIT NONE(TYPE, EXTERNAL)


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

   IMPLICIT NONE(TYPE, EXTERNAL)


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

   IMPLICIT NONE(TYPE, EXTERNAL)


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

!*****************************************************************
!                                                                *
SUBROUTINE DTSTTI (IOPT, TIMSTR, DTTIME)
   USE swan_text_utilities, ONLY: UPCASE
   USE swan_service_interfaces, ONLY: MSGERR
   IMPLICIT NONE(TYPE, EXTERNAL)
!                                                                *
!*****************************************************************
!
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
!     Updates
!
!       ver 30.70, Sep 1997 by N.Booij: adaptation in view year 2000
!       40.89, Nico Booij: integer Jumpyear introduced to prevent problems
!                          with 2-digit year code
!
!     Function:
!
!       transform time string into integer time array
!
!     Argument list:
!
!       IOPT    input  int   option number
!                            1: ISO notation   19870530.153000
!                            2: (HP compiler): 30-May-87 15:30:00
!                            3: (old Lahey)    05/30/87 15:30:00
!                            4:                         15:30:00
!                            5:                87/05/30 15:30:00
!                            6: WAM            8705301530
!
!       TIMSTR  input  char  time string
!       DTTIME  outp   int   time array: elements: year, month, day,
!                            hour, minute, second
!
!    Remarks:
!     Options can be added by the user
!     existing options should not be changed
!
!     Source:

   INTEGER, PARAMETER :: JUMPYEAR = 30 ! Used in interpretation of 2-digit years
   INTEGER :: IOPT, DTTIME(6), II, IMM, IOSTAT
   CHARACTER(LEN=3), PARAMETER :: MONC(12) = [&
      'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
      'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
   CHARACTER(LEN=3) :: MONCI
   CHARACTER(LEN=*) :: TIMSTR

   IF (IOPT.EQ.1) THEN
      READ (TIMSTR, '(I4,I2,I2,1X,3I2)', IOSTAT=IOSTAT) (DTTIME(II), II=1,6)
      IF (time_read_failed(IOSTAT)) RETURN
   ELSE IF (IOPT.EQ.2) THEN
      READ (TIMSTR, '(I2,1X,A3,1X,I2,3(1X,I2))', IOSTAT=IOSTAT)&
      &DTTIME(3), MONCI, DTTIME(1), (DTTIME(II), II=4,6)
      IF (time_read_failed(IOSTAT)) RETURN
      IF (DTTIME(1).LT.JUMPYEAR) THEN
         DTTIME(1) = 2000 + DTTIME(1)
      ELSE
         DTTIME(1) = 1900 + DTTIME(1)
      ENDIF
      DTTIME(2) = 0
      do IMM = 1, 12
         CALL UPCASE (MONCI)
         IF (MONCI == MONC(IMM)) THEN
            DTTIME(2) = IMM
            EXIT
         END IF
      end do
      IF (DTTIME(2) == 0) CALL MSGERR (2, 'incorrect month string: '//MONCI)
   ELSE IF (IOPT.EQ.3) THEN
      READ (TIMSTR, '(I2,5(1X,I2))', IOSTAT=IOSTAT)&
      &DTTIME(2), DTTIME(3), DTTIME(1), (DTTIME(II), II=4,6)
      IF (time_read_failed(IOSTAT)) RETURN
      IF (DTTIME(1).LT.JUMPYEAR) THEN
         DTTIME(1) = 2000 + DTTIME(1)
      ELSE
         DTTIME(1) = 1900 + DTTIME(1)
      ENDIF
   ELSE IF (IOPT.EQ.4) THEN
      READ (TIMSTR, '(I2,2(1X,I2))', IOSTAT=IOSTAT) (DTTIME(II), II=4,6)
      IF (time_read_failed(IOSTAT)) RETURN
      do II = 1, 3
         DTTIME(II) = 0
      end do
   ELSE IF (IOPT.EQ.5) THEN
      READ (TIMSTR, '(I2,5(1X,I2))', IOSTAT=IOSTAT) (DTTIME(II), II=1,6)
      IF (time_read_failed(IOSTAT)) RETURN
      IF (DTTIME(1).LT.JUMPYEAR) THEN
         DTTIME(1) = 2000 + DTTIME(1)
      ELSE
         DTTIME(1) = 1900 + DTTIME(1)
      ENDIF
   ELSE IF (IOPT.EQ.6) THEN
      READ (TIMSTR, '(5I2)', IOSTAT=IOSTAT) (DTTIME(II), II=1,5)
      IF (time_read_failed(IOSTAT)) RETURN
      DTTIME(6) = 0.
      IF (DTTIME(1).LT.JUMPYEAR) THEN
         DTTIME(1) = 2000 + DTTIME(1)
      ELSE
         DTTIME(1) = 1900 + DTTIME(1)
      ENDIF
   ELSE
      CALL MSGERR (2, 'wrong time coding option in subroutine DTSTTI')
   ENDIF
   RETURN
CONTAINS

   LOGICAL FUNCTION time_read_failed(status)
      INTEGER, INTENT(IN) :: status

      time_read_failed = status.NE.0
      IF (time_read_failed) CALL MSGERR (2, 'time string unreadable: '//TIMSTR)
   END FUNCTION time_read_failed

end subroutine DTSTTI
!*****************************************************************
!                                                                *
SUBROUTINE DTTIST (IOPT, TIMSTR, DTTIME)
   USE swan_service_interfaces, ONLY: MSGERR
   IMPLICIT NONE(TYPE, EXTERNAL)
!                                                                *
!*****************************************************************
!
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
!     Updates
!
!       ver 30.70, Sep 1997 by N.Booij: adaptation in view year 2000
!
!     Function:
!
!       transform integer time array into time string
!
!     Argument list:
!
!       IOPT    input  int   option number (see subr. DTSTTI)
!       TIMSTR  outp   char  time string
!       DTTIME  input  int   time array: elements: year, month, day,
!                            hour, minute, second
!
!     Source:

   INTEGER :: IOPT, DTTIME(6), IC, II, LTS
   CHARACTER(LEN=24) :: TIMSTR
   CHARACTER(LEN=3), PARAMETER :: MONC(12) = [&
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', &
      'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

   TIMSTR = '    '
   IF (IOPT.EQ.1) THEN
      WRITE (TIMSTR, "(I4,I2,I2,'.',3I2)") (DTTIME(II), II=1,6)
      LTS = 15
   ELSE IF (IOPT.EQ.2) THEN
      IF (DTTIME(1).GE.2000) THEN
         DTTIME(1) = DTTIME(1) - 2000
      ELSE
         DTTIME(1) = DTTIME(1) - 1900
      ENDIF
      WRITE (TIMSTR, "(I2,'-',A3,'-',I2,'.',I2,':',I2,':',I2)") DTTIME(3), MONC(DTTIME(2)), DTTIME(1),&
      &(DTTIME(II), II=4,6)
      LTS = 18
   ELSE IF (IOPT.EQ.3) THEN
      IF (DTTIME(1).GE.2000) THEN
         DTTIME(1) = DTTIME(1) - 2000
      ELSE
         DTTIME(1) = DTTIME(1) - 1900
      ENDIF
      WRITE (TIMSTR, "(I2,'/',I2,'/',I2,'.',I2,':',I2,':',I2)")&
      &DTTIME(2), DTTIME(3), DTTIME(1), (DTTIME(II), II=4,6)
      LTS = 17
   ELSE IF (IOPT.EQ.4) THEN
      WRITE (TIMSTR, "(I2,':',I2,':',I2)") (DTTIME(II), II=4,6)
      LTS = 8
   ELSE IF (IOPT.EQ.5) THEN
      IF (DTTIME(1).GE.2000) THEN
         DTTIME(1) = DTTIME(1) - 2000
      ELSE
         DTTIME(1) = DTTIME(1) - 1900
      ENDIF
      WRITE (TIMSTR, "(I2,'/',I2,'/',I2,'.',I2,':',I2,':',I2)") (DTTIME(II), II= 1,6)
      LTS = 17
   ELSE IF (IOPT.EQ.6) THEN
      IF (DTTIME(1).GE.2000) THEN
         DTTIME(1) = DTTIME(1) - 2000
      ELSE
         DTTIME(1) = DTTIME(1) - 1900
      ENDIF
      WRITE (TIMSTR, "(5I2)") (DTTIME(II), II=1,5)
      LTS = 10
   ELSE
      CALL MSGERR (2, 'wrong time coding option in subroutine DTTIST')
   ENDIF

   do IC = 1, LTS
      IF (TIMSTR(IC:IC).EQ.' ') TIMSTR(IC:IC) = '0'
   end do

   RETURN
end subroutine DTTIST

END MODULE swan_time
