module swan_run_mode
!
!     Whether the run is stationary, and what follows from that.
!
!     These three answer one question between them: does anything change with
!     time? RDTIM is zero in stationary mode and 1/DT otherwise, DYNDEP says
!     the depth is time-varying, and ICOND says an initial condition has to be
!     computed because the run is nonstationary.
!
   implicit none(type, external)
   private

   public :: RDTIM, DYNDEP, ICOND

!     RDTIM : 0 in stationary mode, 1/DT in nonstationary mode
   real(kind=kind(0.0d0)) :: RDTIM

!     DYNDEP : whether the depth varies with time
   logical :: DYNDEP

!     ICOND : 0 when stationary or when no initial condition is needed,
!             1 when a nonstationary run has to compute one
   integer :: ICOND
end module swan_run_mode
