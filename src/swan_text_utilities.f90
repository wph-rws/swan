MODULE swan_text_utilities
!
!     Small text helpers shared by the command parser and the time conversion
!     routines. Kept in its own module because DTSTTI needs UPCASE while the
!     parser needs swan_time: holding UPCASE here breaks that cycle.
!
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: UPCASE

CONTAINS

PURE SUBROUTINE UPCASE (CHARST)
!     Zuiver: de enige bijwerking was
!     STRACE-diagnostiek; vereist voor zuivere EQCSTR hierboven.
!                                                               *
!****************************************************************

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
!     40.41: Marcel Zijlema
!
!  1. UPDATES
!
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     changes all characters of the string CHARST from lower to
!     upper case
!
!  3. METHOD
!
!  4. ARGUMENT VARIABLES
!
!     CHARST : a character string

   CHARACTER(LEN=*), INTENT(INOUT) :: CHARST

!  5. PARAMETER VARIABLES
!
!  6. LOCAL VARIABLES
!
!     IC     : sequence number of a character in the string CHARST
!     KK     : position of a character in a given string
!     LLCC   : length of the given character string

   INTEGER   IC, KK, LLCC

!     ABCUP  : A to Z upper case characters
!     ABCLO  : a to z lower case characters
!     CC     : a character

   CHARACTER(LEN=*), PARAMETER :: ABCUP = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   CHARACTER(LEN=*), PARAMETER :: ABCLO = 'abcdefghijklmnopqrstuvwxyz'
   CHARACTER(LEN=1) :: CC

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

   LLCC = LEN (CHARST)
   do IC = 1, LLCC
      CC = CHARST(IC:IC)
      KK = INDEX (ABCLO, CC)
      IF (KK.NE.0) CHARST(IC:IC) = ABCUP(KK:KK)
   end do
   RETURN
!     end of subroutine UPCASE
end subroutine UPCASE

END MODULE swan_text_utilities
