module swan_matlab_output_backend
   implicit none(type, external)
   private
   integer, parameter, public :: matlab_direct_record_length = 4
   public :: write_matlab_matrix
contains
      SUBROUTINE write_matlab_matrix ( MROWS , NCOLS, MATNAM, RDATA,&
      &IOUTMA, IREC , IDLA  , DUMVAL )
         USE swan_service_interfaces, ONLY: MSGERR, STRACE
         USE swan_project_metadata, ONLY: PROJID, PROJNR, VERTXT
!
!****************************************************************
!
         USE swan_diagnostics_level
!
         IMPLICIT NONE
!
!
!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering and Geosciences              |
!     | Environmental Fluid Mechanics Section                     |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer: Marcel Zijlema                                |
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
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     41.08: Pieter Smit
!
!  1. Updates
!
!     40.30, May  03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
!     41.08, Aug. 09: adapted to write binary file in the Level 5 MAT-fi
!
!  2. Purpose
!
!     Writes block output to a binary file in the MAT-format
!     to be used in MATLAB
!
!  3. Method
!
!     1) The binary file BINFIL must be opened with the following
!        statement:
!
!        OPEN(UNIT=IOUTMA, FILE=BINFIL, FORM='UNFORMATTED',
!             ACCESS='DIRECT', RECL=4)
!
!        Furthermore, initialize record counter to IREC = 1
!
!     2) Be sure to close the binary file when there are no more
!        matrices to be saved
!
!     3) The matrix may contain signed infinity and/or Not a Numbers.
!        According to the IEEE 754 standard, on a 32-bit machine, the re
!        format has an 8-bit biased exponent (=actual exponent increased
!        by bias=127) and a 23-bit fraction or mantissa. The leftmost
!        bit is the sign bit. Let a fraction, biased exponent and sign
!        bit be denoted as F, E and S, respectively. The following
!        formats adhere to IEEE standard:
!
!          S = 0, E = 11111111 and F  = 00 ... 0 : X = +Inf
!          S = 1, E = 11111111 and F  = 00 ... 0 : X = -Inf
!          S = 0, E = 11111111 and F <> 00 ... 0 : X = NaN
!
!        Hence, the representation of +Inf equals 2**31 - 2**23. A
!        representation of a NaN equals the representation of +Inf
!        plus 1.
!
!     4) The NaN's or Inf's are indicated by a dummy value as given
!        by dumval
!
!     For more information on the Level 5 MAT-file format consult
!     document "MAT-File Format" of MathWorks
!
!  4. Argument variables
!
!     DUMVAL      a dummy value meant for indicating NaN
!     IDLA        controls lay-out of output (see user manual)
!     IOUTMA      unit number of binary MAT-file
!     IREC        direct access file record counter
!     MATNAM      character array holding the matrix name
!     MROWS       a 4-byte integer representing the number of
!                 rows in matrix
!     NCOLS       a 4-byte integer representing the number of
!                 columns in matrix
!     RDATA       real array consists of MROWS * NCOLS real
!                 elements stored column wise
!
         INTEGER       MROWS, NCOLS, IDLA, IOUTMA, IREC
         REAL          RDATA(*), DUMVAL
         CHARACTER(LEN=*) :: MATNAM
!
!  5. Parameter variables
!
!     BlockSize   size of matlab data segment
!     DataSize    number of bytes written per write statement
!     HeaderSize  size of the header in bytes
!     mChar       character data
!     mInt32      signed   INTEGER(KIND=SELECTED_INT_KIND(9))
!     mSingle     real
!
!     --- standard sizes
!
         INTEGER, PARAMETER :: DataSize   = 4
         INTEGER, PARAMETER :: HeaderSize = 128
         INTEGER, PARAMETER :: BlockSize  = 8
!
!     --- Matlab data types
!
         INTEGER, PARAMETER :: mChar      = 1
         INTEGER, PARAMETER :: mInt32     = 5
         INTEGER, PARAMETER :: mSingle    = 7
!
!  6. Local variables
!
!     CTMP  :     a temporary character array
!     HEADER:     header of binary MAT-file
!     I     :     loop variable
!     IENT  :     number of entries
!     IOS   :     auxiliary integer with iostat-number
!     IRECS :     size of array including tags and flags
!     J     :     index
!     M     :     loop variable
!     MSGSTR:     string to pass message to call MSGERR
!     N     :     loop variable
!     NAMLEN:     a 4-byte integer representing the number of
!                 characters in matrix name
!     NANVAL:     an integer representing Not a Number
!     NTOT  :     size of data array
!
         INTEGER I, J, IOS, M, N, NTOT
         INTEGER, SAVE :: IENT = 0
         INTEGER NAMLEN, NANVAL
         INTEGER, SAVE :: IRECS
         CHARACTER(LEN=80) MSGSTR
         CHARACTER(LEN=HeaderSize) HEADER
         CHARACTER(LEN=BlockSize) CTMP

!
!  8. Subroutines used
!
!     MSGERR           Writes error message
!
!  9. Subroutines calling
!
!     ---
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     ---
!
! 12. Structure
!
!     set Not a Number
!     length of name matrix
!     size of data array
!     write header once
!     array name
!     write matrix
!     write the size of the array
!     if necessary, give message that error occurred while writing file
!
! 13. Source text
!
         IF (LTRACE) CALL STRACE (IENT,'SWRMAT')

         IOS = 0

!     --- set Not a Number

         NANVAL = 255 * 2**23 + 1

!     --- length of name matrix

         NAMLEN = LEN_TRIM(MATNAM)

!     --- size of data array

         NTOT = MROWS * NCOLS

!     --- descriptive header

         WRITE (HEADER, '(6A)') 'Data produced by SWAN version ',&
         &TRIM(VERTXT),'; project: ',TRIM(PROJID),&
         &'; run number: ',PROJNR

!     --- data offset

         HEADER(117:124) = CHAR(ICHAR(' '))

!     --- version

         HEADER(125:126) = CHAR(0) // CHAR(1)

!     --- endian indicator

         WRITE(HEADER(127:128),'(A)') INT(19785,KIND=2)

!     --- write header once

         IF ( IREC.EQ.1 ) THEN
            DO I = 1, HeaderSize/DataSize
               J = DataSize*(I-1) + 1
               IF ( IOS.EQ.0 )&
               &WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) HEADER(J:J+DataSize-1)
               IREC = IREC + 1
            END DO
         END IF

!     --- array tag

         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 14
         IREC = IREC + 1
         IRECS = IREC
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
         IREC = IREC + 1

!     --- array flags

         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mInt32
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 2*DataSize
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 7
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
         IREC = IREC + 1

         IF ( MOD(2,BlockSize/DataSize).NE.0 ) THEN
            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
            IREC = IREC + 1
         END IF

!     --- dimensions array

         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mInt32
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 2*DataSize
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) MROWS
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) NCOLS
         IREC = IREC + 1

         IF ( MOD(2,BlockSize/DataSize).NE.0 ) THEN
            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) 0
            IREC = IREC + 1
         END IF

!     --- array name

         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) mChar
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) NAMLEN
         IREC = IREC + 1

         I = 1
         DO
            CTMP(1:8) = CHAR(ICHAR(' '))
            IF ( I.GT.NAMLEN ) THEN
               EXIT
            ELSE IF ( I+BlockSize.LE.NAMLEN ) THEN
               CTMP(1:8) = MATNAM(I:I+BlockSize-1)
            ELSE
               CTMP(1:NAMLEN-I+1) = MATNAM(I:NAMLEN)
            END IF

            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CTMP(1:4)
            IREC = IREC + 1
            IF ( IOS.EQ.0 ) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CTMP(5:8)
            I = I + BlockSize
            IREC = IREC + 1
         END DO

!     --- write matrix

         IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) mSingle
         IREC = IREC + 1
         IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) NTOT*DataSize
         IREC = IREC + 1

         DO M = 1, NCOLS
            DO N = 1, MROWS
               IF ( IDLA.EQ.1 ) THEN
                  J = (MROWS-N)*NCOLS + M
               ELSE
                  J = (N-1)*NCOLS + M
               END IF
               IF (RDATA(J).NE.DUMVAL ) THEN
                  WRITE (IOUTMA,REC=IREC) RDATA(J)
               ELSE IF (.NOT. DUMVAL.NE.0. ) THEN
                  WRITE (IOUTMA,REC=IREC) RDATA(J)
               ELSE
                  WRITE (IOUTMA,REC=IREC) NANVAL
               END IF
               IREC = IREC + 1
            END DO
         END DO

         IF ( MOD(NTOT,BlockSize/DataSize).NE.0 ) THEN
            IF ( IOS.EQ.0 ) WRITE (IOUTMA,REC=IREC,IOSTAT=IOS) 0.
            IREC = IREC + 1
         END IF

!     --- write the size of the array

         IF ( IOS.EQ.0 )&
         &WRITE (IOUTMA,REC=IRECS,IOSTAT=IOS) (IREC-IRECS-1)*DataSize

!     --- if necessary, give message that error occurred while writing f

         IF ( IOS.NE.0 ) THEN
            WRITE (MSGSTR, '(A,I5)')&
            &'Error while writing binary MAT-file - '//&
            &'IOSTAT number is ', IOS
            CALL MSGERR( 4, TRIM(MSGSTR) )
            RETURN
         END IF

         RETURN
      end subroutine write_matlab_matrix

end module swan_matlab_output_backend
