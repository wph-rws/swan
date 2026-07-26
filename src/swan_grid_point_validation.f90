module swan_grid_point_validation
   use swan_service_interfaces, only: strace
   use SWCOMM3, only: MXC, MYC
   implicit none(type, external)
   private

   public :: pvalid, validbp

contains

   logical function pvalid(x_index, y_index, grid_point)
      integer, intent(in) :: x_index, y_index
      integer, intent(in) :: grid_point(MXC, MYC)

      integer, save :: entry_count = 0

      call strace(entry_count, 'PVALID')

      pvalid = x_index >= 1 .and. x_index <= MXC .and. &
               y_index >= 1 .and. y_index <= MYC
      if (pvalid) pvalid = grid_point(x_index, y_index) > 1
   end function pvalid

LOGICAL FUNCTION VALIDBP (IX, IY, KGRPNT,WNP)
!                                                                      *
!************************************************************************

   USE SWCOMM3
   USE OCPCOMM4

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
!     40.04  Annette Kieftenburg
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     August 2000 new function
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Check whether point with index (IX,IY) can be a valid boundary point
!
!  3. Method
!
!     The number of wet neighbouring points WNP is determined
!     Depending on this number certain configurations with wet and dry
!     points surrounding (IX,IY) are excluded
!
!  4. Argument variables
!
!     IX, IY    input    x- and y-index of point under consideration
!     KGRPNT    input    indirect addresses for grid points
!     WNP       output   number of wet neighbouring points

   INTEGER, INTENT(IN) :: IX, IY
   INTEGER, INTENT(IN) :: KGRPNT(MXC,MYC)
   INTEGER, INTENT(OUT) :: WNP

!  5. Parameter variables
!
!  6. Local variables
!
!     IENT    number of entries of this subroutine

   INTEGER, SAVE :: IENT = 0

!  8. Subroutines used
!
!     Function PVALID


!  9. Subroutines calling
!
!     CGBOUN
!
! 10. Error messages
!
! 11. Remarks
!
!     This function prevends all projections of one cell width to
!     be a boundary point
!
! 12. Structure
!
!     Determine amount of Wet Neighbouring Points WNP
!     IF WNP =0  (isolated cell)     VALIDBP = .FALSE.
!     IF WNP =1  (one wet neighbour) VALIDBP = .FALSE.
!     IF WNP =2  (neighbouring points on straight line)
!                                    VALIDBP = .FALSE.
!
!      . W-d     (neighbouring points make angle with no wet point
!        | |      'between' them)    VALIDBP = .FALSE.
!      D-X-W
!        |
!      . D .     (X: point under consideration (assumed to be wet)
!                 W: wet neighbour
!                 D: dry neighbour  d: dry non-neighbour)
!                 .: either wet or dry point
!
!     IF WNP =3
!      . W-d     (point is 1D connection between areas or centre point
!        | |      of isolated 'half plus')
!      D-X-W                         VALIDBP = .FALSE.
!        | |
!      . W-d
!
!
! 13. Source text
!
!************************************************************************

   CALL STRACE (IENT, 'VALIDBP')

   VALIDBP = .TRUE.
!      IF (PVALID(IX,IY,KGRPNT)) THEN
   WNP = 0
   IF (PVALID(IX-1,IY,KGRPNT)) WNP = WNP +1
   IF (PVALID(IX,IY-1,KGRPNT)) WNP = WNP +1
   IF (PVALID(IX+1,IY,KGRPNT)) WNP = WNP +1
   IF (PVALID(IX,IY+1,KGRPNT)) WNP = WNP +1
!         isolated point
   IF (WNP.EQ.0) VALIDBP = .FALSE.
!         point with one valid (wet) neighbouring grid point
   IF (WNP.EQ.1) VALIDBP = .FALSE.

!         neighbouring points on straight line (i.e. in fact 1D)
   IF ((WNP.EQ.2) .AND.(&
   &(PVALID(IX,IY-1,KGRPNT).AND.PVALID(IX,IY+1,KGRPNT)) .OR.&
   &(PVALID(IX-1,IY,KGRPNT).AND.PVALID(IX+1,IY,KGRPNT)) .OR.&
!         neighbouring points make angle but no wet point 'between' them
   &(PVALID(IX-1,IY,KGRPNT).AND.PVALID(IX,IY+1,KGRPNT) .AND.&
   &.NOT. PVALID(IX-1,IY+1,KGRPNT)) .OR.&
   &(PVALID(IX-1,IY,KGRPNT).AND.PVALID(IX,IY-1,KGRPNT) .AND.&
   &.NOT. PVALID(IX-1,IY-1,KGRPNT)) .OR.&
   &(PVALID(IX+1,IY,KGRPNT).AND.PVALID(IX,IY-1,KGRPNT) .AND.&
   &.NOT. PVALID(IX+1,IY-1,KGRPNT)) .OR.&
   &(PVALID(IX+1,IY,KGRPNT).AND.PVALID(IX,IY+1,KGRPNT) .AND.&
   &.NOT. PVALID(IX+1,IY+1,KGRPNT)) )  )  VALIDBP = .FALSE.

!        point (IX,IY) is 1D connection between areas or isolated
!        centre point of 'half plus'
   IF ((WNP.EQ.3) .AND.(&
   &(.NOT.PVALID(IX-1,IY,KGRPNT) .AND.&
   &.NOT.PVALID(IX+1,IY-1,KGRPNT).AND.&
   &.NOT.PVALID(IX+1,IY+1,KGRPNT)).OR.&
   &(.NOT.PVALID(IX+1,IY,KGRPNT) .AND.&
   &.NOT.PVALID(IX-1,IY-1,KGRPNT).AND.&
   &.NOT.PVALID(IX-1,IY+1,KGRPNT)).OR.&
   &(.NOT.PVALID(IX,IY-1,KGRPNT) .AND.&
   &.NOT.PVALID(IX-1,IY+1,KGRPNT).AND.&
   &.NOT.PVALID(IX+1,IY+1,KGRPNT)).OR.&
   &(.NOT.PVALID(IX,IY+1,KGRPNT) .AND.&
   &.NOT.PVALID(IX-1,IY-1,KGRPNT).AND.&
   &.NOT.PVALID(IX+1,IY-1,KGRPNT)) )  )  VALIDBP = .FALSE.

   RETURN
end function VALIDBP

end module swan_grid_point_validation
