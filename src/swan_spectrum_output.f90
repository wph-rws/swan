MODULE swan_spectrum_output
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: WRSPEC

CONTAINS

SUBROUTINE WRSPEC (NREF, ACLOC)
   USE swan_service_interfaces, ONLY: STRACE
!                                                                      *
!************************************************************************

   USE OCPCOMM4
   USE SWCOMM3
   USE OUTP_DATA

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
!     40.00, 40.13: Nico Booij
!     40.41: Marcel Zijlema
!
!  1. UPDATE
!
!     new subroutine, update 40.00
!     40.03, Mar. 00: precision increased; 2 decimals more in output table
!     40.13, July 01: variable format using module OUTP_DATA
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. PURPOSE
!
!     Writing of action density spectrum in Swan standard format
!
!  3. METHOD
!
!
!  4. Argument variables
!
!       NREF    int    input    unit ref. number of output file
!       ACLOC   real   local    2-D spectrum or source term at one
!                               output location

   INTEGER, INTENT(IN) :: NREF
   REAL, INTENT(IN)    :: ACLOC(1:MDC,1:MSC)

!  5. Parameter variables
!
!  6. Local variables
!
!       ID      counter of spectral directions
!       IS      counter of spectral frequencies

   INTEGER :: ID, IS

!       EFAC    multiplication factor written to file

   REAL    :: EFAC

!  8. Subroutines used
!
!  9. Subroutines calling
!
!     SWOUTP (SWAN/OUTP)
!
! 10. Error messages
!
! 11. Remarks
!
! 12. Structure
!
!       ----------------------------------------------------------------
!       determine maximum value of ACLOC
!       if maximum = 0
!       then write 'ZERO' to file
!       else write 'FACTOR'
!            determine multiplication factor, write this to file
!            write values of ACLOC/factor to file
!       ----------------------------------------------------------------
!
! 13. Source text

   INTEGER, SAVE :: IENT = 0
   IF (LTRACE) CALL STRACE (IENT, 'WRSPEC')

!     first determine maximum energy density
   EFAC = 0.
   DO ID = 1, MDC
      DO IS = 1, MSC
         IF (ACLOC(ID,IS).GE.0.) THEN
            EFAC = MAX (EFAC, ACLOC(ID,IS))
         ELSE
            EFAC = MAX (EFAC, 10.*ABS(ACLOC(ID,IS)))
         ENDIF
      ENDDO
   ENDDO
   IF (EFAC .LE. 1.E-10) THEN
      WRITE (NREF, "(A4)") 'ZERO'
   ELSE
      EFAC = 1.01 * EFAC * 10.**(-DEC_SPEC)
!       factor PI/180 introduced to account for change from rad to degr
!       factor 2*PI to account for transition from rad/s to Hz
      WRITE (NREF, "('FACTOR', /, E18.8)") EFAC * 2. * PI**2 / 180.
      DO IS = 1, MSC
!         write spectral energy densities to file
         WRITE (NREF, FIX_SPEC) (NINT(ACLOC(ID,IS)/EFAC), ID=1,MDC)
      ENDDO
   ENDIF
   RETURN
!     end of subroutine WRSPEC
end subroutine WRSPEC

END MODULE swan_spectrum_output
