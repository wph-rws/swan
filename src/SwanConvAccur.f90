subroutine SwanConvAccur ( accur, hscurr, tmcurr, delhs, deltm, xytst, spcsig, ac2 )
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
!     it under the terms of the GNU General Public License as published by
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
!   Authors
!
!   40.80: Marcel Zijlema
!   41.10: Marcel Zijlema
!
!   Updates
!
!   40.80, October 2007: New subroutine
!   41.10,  August 2009: parallelization using OpenMP directives
!
!   Purpose
!
!   Determine accuracy of some integral parameters for convergence check
!
!   Modules used
!
    use ocpcomm4
    use swcomm3
    use swcomm4
    use m_parall
    use SwanGriddata
    use SwanGridobjects
!
    implicit none
!
!   Argument variables
!
    integer, dimension(NPTST), intent(in)       :: xytst  ! test points for output purposes
    !
    real, intent(out)                           :: accur  ! percentage of active vertices in which required accuracy has been reached
    real, dimension(MDC,MSC,nverts), intent(in) :: ac2    ! action density at current time level
    real, dimension(nverts), intent(out)        :: delhs  ! difference in wave height between last 2 iterations in all vertices
    real, dimension(nverts), intent(out)        :: deltm  ! difference in mean period between last 2 iterations in all vertices
    real, dimension(nverts), intent(inout)      :: hscurr ! wave height at current iteration level
    real, dimension(nverts), intent(inout)      :: tmcurr ! mean period at current iteration level
    real, dimension(MSC), intent(in)            :: spcsig ! relative frequency bins
!
!   Local variables
!
    integer                               :: id       ! loop counter over direction bins
    integer, save                         :: ient = 0 ! number of entries in this subroutine
    integer                               :: is       ! loop counter over frequency bins
    integer                               :: ivert    ! loop counter over vertices
    integer                               :: j        ! loop counter
    !
    real                                  :: fact     ! auxiliary factor
    real                                  :: hsabs    ! absolute difference in wave height between last 2 iterations
    real                                  :: hsoval   ! required accuracy with respect to overall error in wave height
    real                                  :: hsprev   ! wave height at previous iteration level
    real                                  :: hsrel    ! required accuracy with respect to relative error in wave height
    real                                  :: hsmean   ! space-averaged wave height at previous iteration level
    real                                  :: m0       ! moment of zeroth order
    real                                  :: m1       ! moment of first order
    real                                  :: npacc    ! number of vertices in which required accuracy has been reached
    real                                  :: nwetp    ! total number of active vertices
    real                                  :: tmmean   ! space-averaged mean period at previous iteration level
    real                                  :: tmabs    ! absolute difference in mean period between last 2 iterations
    real                                  :: tmoval   ! required accuracy with respect to overall error in mean period
    real                                  :: tmprev   ! mean period at previous iteration level
    real                                  :: tmrel    ! required accuracy with respect to relative error in mean period
    !
    logical                               :: lhead    ! logical indicating to write header
    logical                               :: tstfl    ! indicates whether vertex is a test point
    !
    type(verttype), dimension(:), pointer :: vert     ! datastructure for vertices with their attributes
!   Common variables
!
    common/convaccur/npacc,nwetp,hsmean,tmmean
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text
!
    if (ltrace) call strace (ient,'SwanConvAccur')
    !
    ! point to vertex object
    !
    vert => gridobject%vert_grid
    !
    npacc = 0.
    nwetp = 0.
    !
    hsmean = 0.
    tmmean = 0.
    !
    deltm = 0.
    delhs = 0.
    !
    lhead = .true.
    !
    ! calculate space-averaged wave height and mean period
    !
    hsmean = 0.
    tmmean = 0.
    !
    do ivert = 1, nverts
       !
       if ( vert(ivert)%active ) then
          !
          nwetp = nwetp + 1.
          !
          hsmean = hsmean + hscurr(ivert)
          tmmean = tmmean + tmcurr(ivert)
          !
       endif
       !
    enddo
    !
    ! perform global reductions in parallel run
    !
    call SWREDUCE(  nwetp, 1, SWREAL, SWSUM )
    call SWREDUCE( hsmean, 1, SWREAL, SWSUM )
    call SWREDUCE( tmmean, 1, SWREAL, SWSUM )
    !
    hsmean = hsmean/nwetp
    tmmean = tmmean/nwetp
    !
    ! calculate a set of accuracy parameters based on relative and overall accuracy measures for Hs and Tm
    !
    do ivert = 1, nverts
       !
       if ( vert(ivert)%active ) then
          !
          ! determine whether the present vertex is a test point
          !
          tstfl = .false.
          if ( NPTST > 0 ) then
             do j = 1, NPTST
                if ( ivert /= xytst(j) ) cycle
                tstfl = .true.
             enddo
          endif
          !
          ! store wave height and mean period of previous iteration level
          !
          hsprev = max( 1.e-20, hscurr(ivert) )
          tmprev = max( 1.e-20, tmcurr(ivert) )
          !
          ! compute wave height and mean period for present vertex
          !
          m0 = 0.
          m1 = 0.
          do is = 1, MSC
             do id = 1, MDC
                fact = spcsig(is)**2 * ac2(id,is,ivert)
                m0 = m0 + fact
                m1 = m1 + fact * spcsig(is)
             enddo
          enddo
          m0 = m0 * FRINTF * DDIR
          m1 = m1 * FRINTF * DDIR
          !
          if ( m0 > 0. ) then
             hscurr(ivert) = max ( 1.e-20, 4.*sqrt(m0) )
          else
             hscurr(ivert) = 1.e-20
          endif
          if ( m1 > 0. ) then
             tmcurr(ivert) = max ( 1.e-20, PI2*(m0/m1) )
          else
             tmcurr(ivert) = 1.e-20
          endif
          !
          ! compute absolute differences in wave height and mean period between last 2 iterations
          !
          hsabs = abs ( hscurr(ivert) - hsprev )
          tmabs = abs ( tmcurr(ivert) - tmprev )
          !
          delhs(ivert) = hsabs
          deltm(ivert) = tmabs
          !
          ! compute required accuracies for wave height and mean period
          !
          hsrel  = PNUMS( 1) * hsprev
          hsoval = PNUMS(15) * hsmean
          !
          tmrel  = PNUMS( 1) * tmprev
          tmoval = PNUMS(16) * tmmean
          !
          ! count vertices where wave height and mean period have reached required accuracies
          !
          if ( hsabs <= max(hsrel,hsoval) .and. tmabs <= max(tmrel,tmoval) ) npacc = npacc + 1.
          !
          if (tstfl) then
             if (lhead) write(PRINTF,11)
             write (PRINTF,12) ivert, hsabs/hsprev, hsabs/hsmean, tmabs/tmprev, tmabs/tmmean
             lhead = .false.
          endif
          !
       else
          !
          hscurr(ivert) = 1.e-20
          tmcurr(ivert) = 1.e-20
          !
       endif
       !
    enddo
    !
    ! perform global reduction in parallel run
    !
    call SWREDUCE( npacc, 1, SWREAL, SWSUM )
    !
    ! compute percentage of active vertices where required accuracy has been reached
    !
    if ( nwetp > 0. ) accur = ceiling(npacc*10000./nwetp)/100.
    !
 11 format(13x,'dHrel          ','dHoval         ','dTm01rel       ','dTm01oval ')
 12 format(1x,ss,'k=',i7,'  ',1pe13.6e2,'  ',1pe13.6e2,'  ',1pe13.6e2,'  ',1pe13.6e2)
    !
end subroutine SwanConvAccur
