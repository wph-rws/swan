SUBROUTINE SWI2B ( IVAL, BVAL )
   USE swan_service_interfaces, ONLY: STRACE
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
!     Calculates 32-bit representation of an integer number
!
!  3. Method
!
!     The representation of an integer number is divided into 4 parts
!     of 8 bits each, resulting in 32-bit word in memory. Generally,
!     storage words are represented with bits counted from the right,
!     making bit 0 the lower-order bit and bit 31 the high-order bit,
!     which is also the sign bit.
!
!     The integer number is always an exact representation of an
!     integer of value positive, negative, or zero. Each bit, except
!     the leftmost bit, corresponds to the actual exponent as power
!     of two.
!
!     For representing negative numbers, the method called
!     "excess 2**(m - 1)" is used, which represents an m-bit number by
!     storing it as the sum of itself and 2**(m - 1). For a 32-bit
!     machine, m = 32. This results in a positive number, so the
!     leftmost bit need to be reversed. This method is identical to the
!     two's complement method.
!
!     An example:
!
!        the 32-bit representation of 5693 is
!
!        decimal    :     0        0       22       61
!        hexidecimal:     0        0       16       3D
!        binary     : 00000000 00000000 00010110 00111101
!
!        since,
!
!        5693 = 2^12 + 2^10 + 2^9 + 2^5 + 2^4 + 2^3 + 2^2 + 2^0
!
!  4. Argument variables
!
!     BVAL        a byte value as a part of the representation of
!                 integer number
!     IVAL        integer number
!
   INTEGER BVAL(4), IVAL
!
!  6. Local variables
!
!     I     :     loop counter
!     IENT  :     number of entries
!     IQUOT :     auxiliary integer with quotient
!     M     :     maximal exponent number possible (for 32-bit machine,
!
   INTEGER I, IQUOT
   INTEGER, PARAMETER :: M = 32
   INTEGER, SAVE :: IENT = 0
!
! 12. Structure
!
!     initialise 4 parts of the representation
!     if integer < 0, increased it by 2**(m-1)
!     compute the actual part of the representation
!     determine the sign bit
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWI2B')

!     --- initialise 4 parts of the representation

   DO I = 1, 4
      BVAL(I) = 0
   END DO

   IQUOT = IVAL

!     --- clear the sign bit before splitting a negative integer

   IF ( IVAL < 0 ) IQUOT = IBCLR(IQUOT, M - 1)

!     --- compute the actual part of the representation

   DO I = 4, 1, -1
      BVAL(I) = MOD(IQUOT,256)
      IQUOT = INT(IQUOT/256)
   END DO

!     --- determine the sign bit

   IF ( IVAL.LT.0 ) BVAL(1) = BVAL(1) + 128

   RETURN
end subroutine SWI2B
!****************************************************************
!
SUBROUTINE SWR2B ( RVAL, BVAL )
   USE swan_service_interfaces, ONLY: STRACE
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
!     Calculates 32-bit representation of a floating-point number
!
!  3. Method
!
!     The representation of a floating-point number is divided into 4
!     parts of 8 bits each, resulting in 32-bit word in memory.
!     Generally, storage words are represented with bits counted from
!     the right, making bit 0 the lower-order bit and bit 31 the
!     high-order bit, which is also the sign bit.
!
!     The floating-point number is a processor approximation. Its format
!     has an 8-bit biased exponent and a 23-bit fraction or mantissa. Th
!     leftmost bit is the sign bit which is zero for plus and 1 for
!     minus. The biased exponent equals the bias and the actual exponent
!     (power of two) of the number. For a 32-bit machine, bias=127.
!
!     Furthermore, the floating-point number is usually stored in the
!     normalized form, i.e. it has a binary point to the left of the
!     mantissa and an implied leading 1 to the left of the binary point.
!     Thus, if X is a floating-point number, then it is calculated as
!     follows:
!
!         X = (-1)**sign bit 1.fraction * 2**(biased exponent-bias)
!
!     There are several exceptions. Let a fraction, biased exponent
!     and sign bit be denoted as F, E and S, respectively. The following
!     formats adhere to IEEE standard:
!
!     S = 0, E = 00000000 and F  = 00 ... 0 : X = 0
!     S = 0, E = 00000000 and F <> 00 ... 0 : X = +0.fraction * 2**(1-bi
!     S = 1, E = 00000000 and F <> 00 ... 0 : X = -0.fraction * 2**(1-bi
!     S = 0, E = 11111111 and F  = 00 ... 0 : X = +Inf
!     S = 1, E = 11111111 and F  = 00 ... 0 : X = -Inf
!     S = 0, E = 11111111 and F <> 00 ... 0 : X = NaN
!
!     A NaN (Not a Number) is a value reserved for signalling an
!     attempted invalid operation, like 0/0. Its representation
!     equals the representation of +Inf plus 1, i.e. 2**31 - 2**23 + 1
!
!     An example:
!
!        the 32-bit representation of 23.1 is
!
!        decimal    :    65      184      204      205
!        hexidecimal:    41       B8       CC       CD
!        binary     : 01000001 10111000 11001100 11001101
!
!        since,
!
!        23.1 = 2^4 + 2^2 + 2^1 + 2^0 + 2^-4 + 2^-5 + 2^-8 + 2^-9 +
!               2^-12 + 2^-13 + 2^-16 + 2^-17 + 2^-19
!
!        so that the biased exponent = 4 + 127 = 131 = 10000011 = E
!        and the sign bit = 0 = S. The remaining of the 32-bit word is
!        the fraction, which is
!
!    3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16 -17
!
! F= 0 1 1 1  0  0  0  1  1  0  0  1  1   0   0   1   1   0   0   1   1
!
!  4. Argument variables
!
!     BVAL        a byte value as a part of the representation of
!                 floating-point number
!     RVAL        floating-point number
!
   INTEGER BVAL(4)
   REAL    RVAL
!
!  6. Local variables
!
!     ACTEXP:     actual exponent in the representation
!     BEXPO :     biased exponent
!     BIAS  :     bias (for 32-bit machine, bias=127)
!     EXPO  :     calculated exponent of floating-point number
!     FRAC  :     fraction of floating-point number
!     I     :     loop counter
!     IENT  :     number of entries
!     IPART :     i-the part of the representation
!     IQUOT :     auxiliary integer with quotient
!     LEADNR:     leading number of floating-point number
!     LFRAC :     length of fraction in representation
!           :     (for 32-bit machine, lfrac=23)
!     RFRAC :     auxiliary real with fraction
!
   INTEGER ACTEXP, EXPO, I, IPART, BEXPO
   INTEGER, PARAMETER :: LFRAC = 23, BIAS = 127
   INTEGER, SAVE :: IENT = 0
   INTEGER(KIND=SELECTED_INT_KIND(18)) LEADNR, IQUOT
   REAL FRAC, RFRAC, MAXREAL
!
! 12. Structure
!
!     initialise 4 parts of the representation and biased exponent
!     determine leading number and fraction
!     do while leading number >= 1
!        calculate positive exponent as power of two
!     or while fraction > 0
!        calculate negative exponent as power of two
!     end do
!     compute the actual part of the representation
!
! 13. Source text
!
   IF (LTRACE) CALL STRACE (IENT,'SWR2B')

!     --- initialise 4 parts of the representation and biased exponent

   DO I = 1, 4
      BVAL(I) = 0
   END DO
   BEXPO  = -1

!     --- clamp reals to the max INTEGER(KIND=SELECTED_INT_KIND(18)), to prevent overflow

   MAXREAL=9.0E+18
   IF ( RVAL .GT. MAXREAL ) THEN
      RVAL=MAXREAL
   ELSEIF ( RVAL .LT. -MAXREAL ) THEN
      RVAL=-MAXREAL
   END IF

!     --- determine leading number and fraction

   IF ( ABS(RVAL).LT.1.E-7 ) THEN
      LEADNR = 0
      FRAC   = 0.
   ELSE
      LEADNR = INT(ABS(RVAL),KIND=8)
      FRAC   = ABS(RVAL) - REAL(LEADNR)
   END IF

   conversion_loop: DO
   IF ( LEADNR.GE.1 ) THEN

!        --- calculate positive exponent as power of two

      EXPO  = 0
      IQUOT = LEADNR
      DO WHILE (IQUOT.GE.2)

         IQUOT = INT(IQUOT/2,KIND=8)
         EXPO  = EXPO + 1

      END DO

   ELSE IF ( FRAC.GT.0. ) THEN

!        --- calculate negative exponent as power of two

      EXPO = 0
      RFRAC = FRAC
      DO WHILE (RFRAC.LT.1.)

         RFRAC = RFRAC * 2.
         EXPO  = EXPO - 1

      END DO

   ELSE

      EXIT conversion_loop

   END IF

!     --- compute the actual part of the representation

   IF ( BEXPO.EQ.-1 ) THEN

!        --- determine biased exponent

      BEXPO = EXPO + BIAS

!        --- the first seven bits of biased exponent belong
!            to first part of the representation

      BVAL(1) = INT(BEXPO/2)

!        --- determine the sign bit

      IF ( RVAL.LT.0. ) BVAL(1) = BVAL(1) + 128

!        --- the eighth bit of biased component is the leftmost
!            bit of second part of the representation

      BVAL(2) = MOD(BEXPO,2)*2**7
      IPART = 2

   ELSE

!        --- compute the actual exponent of bit 1 in i-th part of
!            the representation

      ACTEXP = (IPART-2)*8 + 7 - BEXPO + BIAS + EXPO
      IF ( ACTEXP.LT.0 ) THEN
         ACTEXP = ACTEXP + 8
         IPART = IPART + 1
         IF ( IPART.GT.4 ) EXIT conversion_loop
      END IF
      BVAL(IPART) = BVAL(IPART) + 2**ACTEXP

   END IF

   IF ( EXPO.LT.(BEXPO-BIAS-LFRAC) ) EXIT conversion_loop
   LEADNR = LEADNR - 2.**EXPO
   IF ( EXPO.LT.0 ) FRAC = FRAC - 2.**EXPO

   END DO conversion_loop

   RETURN
end subroutine SWR2B
