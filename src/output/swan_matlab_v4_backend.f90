module swan_matlab_output_backend
   implicit none(type, external)
   private
   integer, parameter, public :: matlab_direct_record_length = 1
   public :: write_matlab_matrix
contains
      SUBROUTINE write_matlab_matrix ( MROWS , NCOLS, MATNAM, RDATA,&
      &IOUTMA, IREC , IDLA  , DUMVAL )
         USE swan_service_interfaces, ONLY: MSGERR, STRACE, SWI2B, SWR2B, TXPBLA
         USE swan_number_formatting, ONLY: INTSTR
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
!
!  1. Updates
!
!     40.30, May 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files r
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
!             ACCESS='DIRECT', RECL=1)
!
!        Furthermore, initialize record counter to IREC = 1
!
!     2) Be sure to close the binary file when there are no more
!        matrices to be saved
!
!     3) The matrix may contain signed infinity and/or Not a Numbers.
!        According to the IEEE standard, on a 32-bit machine, the real
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
!        The Cray machine C916 at SARA Information Centre does not
!        support the IEEE standard.
!
!     4) The NaN's or Inf's are indicated by a dummy value as given
!        by dumval
!
!     For more information consult "Appendix - MAT-File Structure"
!     of the MATLAB External Data Reference guide (Version 4.2)
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
!     ---
!
!  6. Local variables
!
!     BVAL  :     a byte value
!     CHARS :     array to pass character info to MSGERR
!     I     :     loop variable
!     IENT  :     number of entries
!     IF    :     first non-character in string
!     IL    :     last non-character in string
!     IMAGF :     a 4-byte imaginary flag. Possible values are:
!                 0: there is only real data
!                 1: the data has also an imaginary part
!     IOS   :     auxiliary integer with iostat-number
!     ITYPE :     the type flag containing a 4-byte integer whose
!                 decimal digits encode storage information.
!                 If the integer is represented as ABCD then:
!                 "A" indicates the format to write the binary
!                 data to a file on the machine. Possible values are:
!                   0: Intel based machines (PC 386/486, Pentium)
!                   1: Motorola 68000 based machines (Macintosh,
!                      HP 9000, SPARC, Apollo, SGI)
!                   2: VAX-D format
!                   3: VAX-G format
!                   4: Cray
!                 "B" is always zero
!                 "C" indicates which format the data is stored.
!                  Possible values are:
!                   0: REAL(KIND=KIND(0.0D0)) (64 bit) floating point numbers
!                   1: single precision (32 bit) floating point numbers
!                   2: 32-bit signed integers
!                   3: 16-bit signed integers
!                   4: 16-bit unsigned integers
!                   5: 8-bit unsigned integers
!                 "D" indicates the type of data (matrix).
!                  Possible values:
!                   0: numeric matrix
!                   1: textual matrix
!                   2: sparse  matrix
!     J     :     index
!     M     :     loop variable
!     MSGSTR:     string to pass message to call MSGERR
!     N     :     loop variable
!     NAMLEN:     a 4-byte integer representing the number of
!                 characters in matrix name plus 1
!     NANVAL:     an integer representing Not a Number
!
         INTEGER I, J, IF, IL, IOS, M, N
         INTEGER, SAVE :: IENT = 0
         INTEGER BVAL(4), IMAGF, ITYPE, NAMLEN, NANVAL
         CHARACTER(LEN=20) CHARS
         CHARACTER(LEN=80) MSGSTR
!
!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     MSGERR           Writes error message
!     TXPBLA           Removes leading and trailing blanks in string
!     SWI2B            Calculates 32-bit representation of an
!                      integer number
!     SWR2B            Calculates 32-bit representation of a
!                      floating-point number
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
!
!     set some flags
!
!     write header consisting of ITYPE, MROWS, NCOLS, IMAGF, NAMLEN and
!     name of matrix MATNAM
!
!     write matrix
!
!     if necessary, give message that error occurred while writing file
!
! 13. Source text
!
         IF (LTRACE) CALL STRACE (IENT,'SWRMAT')

!     --- set Not a Number

         NANVAL = 255 * 2**23 + 1

!     --- set some flags

         ITYPE = 1010
         IMAGF = 0
         IOS   = 0

!     --- write header consisting of ITYPE, MROWS, NCOLS, IMAGF,
!         NAMLEN and name of matrix MATNAM
!         the name should be ended by zero-byte terminator

         CALL SWI2B ( ITYPE, BVAL )
         DO I = 1, 4
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
            IREC = IREC + 1
         END DO

         CALL SWI2B ( MROWS, BVAL )
         DO I = 1, 4
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
            IREC = IREC + 1
         END DO

         CALL SWI2B ( NCOLS, BVAL )
         DO I = 1, 4
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
            IREC = IREC + 1
         END DO

         CALL SWI2B ( IMAGF, BVAL )
         DO I = 1, 4
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
            IREC = IREC + 1
         END DO

         CALL TXPBLA(MATNAM,IF,IL)
         NAMLEN = IL - IF + 2
         CALL SWI2B ( NAMLEN, BVAL )
         DO I = 1, 4
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
            IREC = IREC + 1
         END DO

         DO I = IF, IL
            IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) MATNAM(I:I)
            IREC = IREC + 1
         END DO
         IF (IOS.EQ.0) WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(0)
         IREC = IREC + 1

!     --- write matrix

         DO M = 1, NCOLS
            DO N = 1, MROWS
               IF ( IDLA.EQ.1 ) THEN
                  J = (MROWS-N)*NCOLS + M
               ELSE
                  J = (N-1)*NCOLS + M
               END IF
               IF (RDATA(J).NE.DUMVAL ) THEN
                  CALL SWR2B ( RDATA(J), BVAL )
               ELSE IF (.NOT. DUMVAL.NE.0. ) THEN
                  CALL SWR2B ( RDATA(J), BVAL )
               ELSE
                  CALL SWI2B ( NANVAL, BVAL )
               END IF
               DO I = 1, 4
                  IF (IOS.EQ.0)&
                  &WRITE(IOUTMA,REC=IREC,IOSTAT=IOS) CHAR(BVAL(I))
                  IREC = IREC + 1
               END DO
            END DO
         END DO

!     --- if necessary, give message that error occurred while writing f

         IF ( IOS.NE.0 ) THEN
            CHARS = INTSTR(IOS)
            CALL TXPBLA(CHARS,IF,IL)
            MSGSTR = 'Error while writing binary MAT-file - '//&
            &'IOSTAT number is '//CHARS(IF:IL)
            CALL MSGERR ( 4, MSGSTR )
            RETURN
         END IF

         RETURN
      end subroutine write_matlab_matrix

end module swan_matlab_output_backend
