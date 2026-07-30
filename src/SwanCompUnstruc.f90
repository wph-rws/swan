module swan_comp_unstruc
   use swan_build_config, only: timing_enabled
   use swan_triad_state, only: triad_state_t
   use swan_snl4_tables, only: snl4_tables_t
   use swan_spectral_powers, only: spectral_powers_t
   use swan_source_workspaces, only: thread_workspaces_t
   use swan_conv_accur, only: SwanConvAccur
   use swan_conv_stopc, only: SwanConvStopc
   use swan_diff_par, only: SwanDiffPar
   use swan_disp_parm, only: SwanDispParm
   use swan_grad_depthor_k, only: SwanGradDepthorK
   use swan_grad_vel, only: SwanGradVel
   use swan_propvel_s, only: SwanPropvelS
   use swan_propvel_x, only: SwanPropvelX
   use swan_sweep_sel, only: SwanSweepSel
   use swan_transp_ac, only: SwanTranspAc
   use swan_time, only: CHTIME
   implicit none(type, external)
   private
   public :: SwanCompUnstruc
contains

subroutine SwanCompUnstruc ( ac2, ac1, compda, spcsig, spcdir, xytst, cross, it, &
                             diffr, triads, snl4, spectral_powers, thread_workspaces )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, SWTSTA, SWTSTO
   use swan_diffraction_state, only: diffraction_state_t
   use swan_computation, only: SWPRSET, SINTGRL, SOLPRE, SOLMAT, SOLMT1, SOURCE, PHILIM, RESCALE, SWSIP
   use swan_services, only: SWTRCF, SWACC

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
!   40.85: Marcel Zijlema
!   40.95: Marcel Zijlema
!   41.02: Marcel Zijlema
!   41.07: Marcel Zijlema
!   41.10: Marcel Zijlema
!   41.20: Casey Dietrich
!   41.60: Marcel Zijlema
!   41.63: Marcel Zijlema
!   41.67: Marcel Zijlema
!   41.68: Marcel Zijlema
!   41.80: Dirk Rijnsdorp and Ad Reniers
!   41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!   41.91: Marcel Zijlema
!   43.05: Marcel Zijlema
!
!   Updates
!
!   40.80,     July 2007: New subroutine
!   40.85,   August 2008: add propagation, generation and redistribution terms for output purposes
!   40.95,     June 2008: parallelization of unSWAN
!   41.02, February 2009: implementation of diffraction
!   41.07,   August 2009: bug fix: never-ending sweep is prevented
!   41.10,   August 2009: parallelization using OpenMP directives
!   41.20,     June 2010: extension to tightly coupled ADCIRC+SWAN model
!   41.60,     July 2015: more accurate computation of gradients of depth or wave number for turning rate
!   41.63,   August 2015: efficiency UnSWAN algorithm improved; one sweep per iteration instead of undetermined number of sweeps
!   41.68,   August 2015: introduction of a fixed number of sweeps per iteration
!   41.67,   August 2017: more accurate computation of gradients of ambient currents for transport velocities
!   41.80,September 2021: adding Bragg scattering
!   41.90,  October 2021: adding QC scattering
!   41.91, February 2022: adding QC surf breaking
!   43.05,  January 2023: wavefront scheduling in OpenMP
!
!   Purpose
!
!   Performs one time step for solution of wave action equation on unstructured grid
!
!   Method
!
!   A vertex-based algorithm is employed in which the variables are stored at the vertices of the mesh
!   The equation is solved in each vertex assuming a constant spectral grid resolution in all vertices
!   The propagation terms in both geographic and spectral spaces are integrated implicitly
!   Sources are treated explicitly and sinks implicitly
!   The calculation of the source terms is carried out in the original SWAN routines, e.g., SOURCE
!
!   The wave action equation is solved iteratively
!   A number of iterations are carried out until convergence is reached
!   In each iteration, a fixed number of sweeps are carried out
!   Per sweep direction, a loop over ordered vertices is executed
!   The solution of each vertex must be updated geographically before proceeding to the next one
!   Per vertex, a loop over cells is executed
!   The solution of each cell (partly) enclosed by the present sweep must be updated
!   The two upwave faces connecting the vertex to be updated enclose those wave directions that can be processed in the spectral space
!
!   Modules used

    use swan_diagnostics_level
    use swan_io_units
    use swan_wind_source, only: WINDP1, WINDP3
    use swan_dissipation, only: PLTSRC
    use swan_nonlinear_interactions, only: FAC3WW, FAC4WW, SWBIPM, SWPRE4W
    use swan_propagation, only: SPREDT, ADDDIS
    use swan_coordinate_offset
    use swan_run_mode
    USE swan_stencil
    use swan_physics_selection
    use swan_numerics
    use swan_physical_settings
    use swan_spectral_grid
    use swan_compda_layout
    use swan_math_constants
    use swan_test_output
    use swan_propagation_scheme
    use swan_spherical_geometry
    use SwanGriddata
    use SwanGridobjects
    use SwanCompdata
    use SwanQCM
    use m_parall
    use swan_fftw_compat, only: cfft2i
!METIS    use SwanParallel

    implicit none(type, external)

!   Argument variables

    integer, dimension(nfaces), intent(in)         :: cross  ! contains sequence number of obstacles for each face
                                                             ! where they crossing or zero if no crossing
    integer, intent(in)                            :: it     ! counter in time space
    integer, dimension(NPTST), intent(in)          :: xytst  ! test points for output purposes

    real, dimension(MDC,MSC,nverts), intent(in)    :: ac1    ! action density at previous time level
    real, dimension(MDC,MSC,nverts), intent(inout) :: ac2    ! action density at current time level
    real, dimension(nverts,MCMVAR), intent(inout)  :: compda ! array containing space-dependent info (e.g. depth)
    real, dimension(MDC,6), intent(in)             :: spcdir ! (*,1): spectral direction bins (radians)
                                                             ! (*,2): cosine of spectral directions
                                                             ! (*,3): sine of spectral directions
                                                             ! (*,4): cosine^2 of spectral directions
                                                             ! (*,5): cosine*sine of spectral directions
                                                             ! (*,6): sine^2 of spectral directions
    real, dimension(MSC), intent(in)               :: spcsig ! relative frequency bins
    type(diffraction_state_t), intent(inout)       :: diffr  ! diffraction parameter and its derivatives
    type(triad_state_t), intent(inout)              :: triads
    type(snl4_tables_t), intent(inout)              :: snl4
    type(spectral_powers_t), intent(in)              :: spectral_powers
    type(thread_workspaces_t), intent(inout)         :: thread_workspaces

!   Parameter variables

    integer, parameter                    :: itswp = 4 ! the number of iterations needed prior to switch to a lower number of sweeps

!   Local variables

    integer                               :: icell     ! cell index / loop counter
    integer                               :: id        ! loop counter over direction bins
    integer                               :: iddlow    ! minimum direction bin that is propagated within a sweep
    integer                               :: iddtop    ! maximum direction bin that is propagated within a sweep
    integer                               :: iddum     ! counter in directional space for considered sweep
    integer, parameter                    :: idebug=0  ! level of debug output:
                                                       ! 0 = no output
                                                       ! 1 = print extra output for debug purposes
    integer                               :: idtot     ! maximum number of bins in directional space for considered sweep
    integer                               :: idwmax    ! maximum counter for spectral wind direction
    integer                               :: idwmin    ! minimum counter for spectral wind direction
    integer, save                         :: ient = 0  ! number of entries in this subroutine
    integer                               :: ierror    ! error indicator
    integer                               :: iface     ! face index
    integer                               :: ifront    ! loop counter over wavefronts
    integer                               :: inocnt    ! inocnv counter for calling thread
    integer                               :: inocnv    ! integer indicating number of vertices in which solver does not converged
    integer                               :: is        ! loop counter over frequency bins
    integer                               :: isslow    ! minimum frequency that is propagated within a sweep
    integer                               :: isstop    ! maximum frequency that is propagated within a sweep
    integer                               :: istat     ! indicate status of allocation
    integer                               :: istot     ! maximum number of bins in frequency space for considered sweep
    integer                               :: idummy    ! integer dummy for structured-grid arguments
    integer                               :: iter      ! iteration counter
    integer                               :: ivert     ! vertex index
    integer                               :: j         ! loop counter
    integer                               :: jc        ! loop counter
    integer                               :: k         ! loop counter
    integer                               :: kvert     ! loop counter over vertices
    integer, dimension(2)                 :: link      ! local face connected to considered vertex where an obstacle crossed
    integer                               :: mnisl     ! minimum sigma-index occured in applying limiter
    integer                               :: mxnfl     ! maximum number of use of limiter in spectral space
    integer                               :: mxnfr     ! maximum number of use of rescaling in spectral space
    integer                               :: n         ! actual number of sweeps (during the iteration process)
    integer                               :: npfl      ! number of vertices in which limiter is used
    integer                               :: npfr      ! number of vertices in which rescaling is used
    integer                               :: swpdir    ! sweep counter
    integer                               :: swpnr     ! sweep number
    integer                               :: tid       ! thread number
    integer                               :: thread_count
    integer, dimension(3)                 :: v         ! vertices in present cell
    integer                               :: vb        ! vertex of begin of present face
    integer                               :: ve        ! vertex of end of present face
    integer, dimension(2)                 :: vu        ! upwave vertices in present cell
    integer, dimension(24)                :: wwint     ! counters for 4 wave-wave interactions (see routine FAC4WW)

    integer, dimension(:), allocatable    :: idcmax    ! maximum frequency-dependent counter in directional space
    integer, dimension(:), allocatable    :: idcmin    ! minimum frequency-dependent counter in directional space
    integer, dimension(:), allocatable    :: iscmax    ! maximum direction-dependent counter in frequency space
    integer, dimension(:), allocatable    :: iscmin    ! minimum direction-dependent counter in frequency space
    integer, dimension(:), allocatable    :: islmin    ! lowest sigma-index occured in applying limiter
    integer, dimension(:), allocatable    :: nflim     ! number of frequency use of limiter in each vertex
    integer, dimension(:), allocatable    :: nrscal    ! number of frequency use of rescaling in each vertex

    real                                  :: abrbot    ! near bottom excursion
    real                                  :: accur     ! percentage of active vertices in which required accuracy has been reached
    real                                  :: acnrmo    ! norm of difference of previous iteration
    real, dimension(2)                    :: acnrms    ! array containing infinity norms
    real                                  :: dal1      ! a coefficent for the 4 wave-wave interactions
    real                                  :: dal2      ! another coefficent for the 4 wave-wave interactions
    real                                  :: dal3      ! just another coefficent for the 4 wave-wave interactions
    real                                  :: dhdx      ! derivative of depth in x-direction
    real                                  :: dhdy      ! derivative of depth in y-direction
    real                                  :: dummy     ! dummy variable (to be used in existing SWAN routine call)
    real                                  :: duxdx     ! derivative of ux2 to x
    real                                  :: duxdy     ! derivative of ux2 to y
    real                                  :: duydx     ! derivative of uy2 to x
    real                                  :: duydy     ! derivative of uy2 to y
    real                                  :: etot      ! total wave energy density
    real                                  :: fpm       ! Pierson Moskowitz frequency
    real                                  :: frac      ! fraction of total active vertices
    real                                  :: hm        ! maximum wave height
    real                                  :: hs        ! significant wave height
    real                                  :: kmespc    ! mean average wavenumber based on the WAM formulation
    real                                  :: kteta     ! number of directional partitions
    real                                  :: nwetp     ! total number of active vertices
    real                                  :: qbloc     ! fraction of breaking waves
    real, dimension(2)                    :: rdx       ! first component of contravariant base vector rdx(b) = a^(b)_1
    real, dimension(2)                    :: rdy       ! second component of contravariant base vector rdy(b) = a^(b)_2
    real                                  :: rhof      ! asymptotic convergence factor
    real                                  :: rval1     ! a dummy value
    real                                  :: rval2     ! a dummy value
    real                                  :: sdir      ! sweep direction
    real                                  :: smebrk    ! mean frequency based on the first order moment
    real                                  :: snlc1     ! a coefficent for the 4 wave-wave interactions
    real                                  :: stopcr    ! stopping criterion for stationary solution
    real                                  :: th1       ! direction of one face pointing to present vertex
    real                                  :: th2       ! direction of another face pointing to present vertex
    real                                  :: thmax     ! maximum direction of present sweep
    real                                  :: thmin     ! minimum direction of present sweep
    real                                  :: thetaw    ! mean direction of the wind speed vector with respect to ambient current
    real                                  :: ufric     ! wind friction velocity
    real, dimension(nverts)               :: urmstop
    real, dimension(5)                    :: usrset    ! auxiliary array to store user-defined settings of 3rd generation mode
    real                                  :: wind10    ! magnitude of the wind speed vector with respect to ambient current
    real, dimension(8)                    :: wwawg     ! weight coefficients for the 4 wave-wave interactions (see routine FAC4WW)
    real, dimension(8)                    :: wwswg     ! weight coefficients for the 4 wave-wave interactions semi-implicitly (see routine FAC4WW)
    real                                  :: xis       ! difference between succeeding frequencies for computing 4 wave-wave interactions

    real, dimension(:,:), allocatable     :: ac2old    ! array to store action density before solving system of equations
    real, dimension(:,:), allocatable     :: alimw     ! maximum energy by wind growth
                                                       ! this auxiliary array is used because the maximum value has to be checked
                                                       ! direct after solving the action balance equation
    real, dimension(:,:,:), allocatable   :: amat      ! coefficient matrix of system of equations in spectral space
    real, dimension(:,:), allocatable     :: rhs       ! right-hand side of system of equations in spectral space
    real, dimension(:,:), allocatable     :: cad       ! wave transport velocity in theta-direction
    real, dimension(:,:), allocatable     :: cas       ! wave transport velocity in sigma-direction
    real, dimension(:,:,:), allocatable   :: cax       ! wave transport velocity in x-direction
    real, dimension(:,:,:), allocatable   :: cay       ! wave transport velocity in y-direction
    real, dimension(:,:,:), allocatable   :: cgft      ! Fourier-transformed modulation of group velocity
    real, dimension(:,:), allocatable     :: cgo       ! group velocity
    real, dimension(:,:), allocatable     :: da1c      ! implicit interaction contribution of first quadruplet, current bin (unfolded space)
    real, dimension(:,:), allocatable     :: da1m      ! implicit interaction contribution of first quadruplet, current bin -1 (unfolded space)
    real, dimension(:,:), allocatable     :: da1p      ! implicit interaction contribution of first quadruplet, current bin +1 (unfolded space)
    real, dimension(:,:), allocatable     :: da2c      ! implicit interaction contribution of second quadruplet, current bin (unfolded space)
    real, dimension(:,:), allocatable     :: da2m      ! implicit interaction contribution of second quadruplet, current bin -1 (unfolded space)
    real, dimension(:,:), allocatable     :: da2p      ! implicit interaction contribution of second quadruplet, current bin +1 (unfolded space)
    real, dimension(:,:,:), allocatable   :: disc0     ! explicit part of dissipation in present vertex for output purposes
    real, dimension(:,:,:), allocatable   :: disc1     ! implicit part of dissipation in present vertex for output purposes
    real, dimension(:), allocatable       :: dkdx      ! derivative of wave number in x-direction
    real, dimension(:), allocatable       :: dkdy      ! derivative of wave number in y-direction
    real, dimension(:,:), allocatable     :: dmw       ! mud dissipation rate
    real, dimension(:,:), allocatable     :: dsnl      ! total interaction contribution of quadruplets to the main diagonal matrix
    real, dimension(:,:,:), allocatable   :: fbd       ! bottom spectrum for Bragg scattering
    real, dimension(:,:,:), allocatable   :: genc0     ! explicit part of generation in present vertex for output purposes
    real, dimension(:,:,:), allocatable   :: genc1     ! implicit part of generation in present vertex for output purposes
    real, dimension(:), allocatable       :: hscurr    ! wave height at current iteration level
    real, dimension(:), allocatable       :: hsdifc    ! difference in wave height of current and one before previous iteration
    real, dimension(:), allocatable       :: hsprev    ! wave height at previous iteration level
    real, dimension(:,:), allocatable     :: kwave     ! wave number
    real, dimension(:,:), allocatable     :: leakcf    ! leak coefficient in present vertex for output purposes
    real, dimension(:,:,:), allocatable   :: membrg    ! auxiliary array to store results of Bragg scattering in full spectral space
    real, dimension(:,:,:), allocatable   :: memnl4    ! auxiliary array to store results of 4 wave-wave interactions in full spectral space
    real, dimension(:,:,:), allocatable   :: memqcb    ! auxiliary array to store results of QC surf breaking in full spectral space
    real, dimension(:,:,:), allocatable   :: memqcm    ! auxiliary array to store results of QC scattering in full spectral space
    real, dimension(:,:,:), allocatable   :: memsina   ! auxiliary array to store results of linear wind input in full spectral space
    real, dimension(:,:,:), allocatable   :: memsinb   ! auxiliary array to store results of exponential wind input in full spectral space
    real, dimension(:,:,:), allocatable   :: obredf    ! action reduction coefficient based on transmission
    real, dimension(:,:), allocatable     :: qtl1      ! local interpolation factors for triads
    real, dimension(:,:), allocatable     :: qtl2      ! local scaling factors for triads
    real, dimension(:,:,:), allocatable   :: redc0     ! explicit part of redistribution in present vertex for output purposes
    real, dimension(:,:,:), allocatable   :: redc1     ! implicit part of redistribution in present vertex for output purposes
    real, dimension(:,:), allocatable     :: reflso    ! contribution to the source term due to reflection
    real, dimension(:,:), allocatable     :: sa1       ! explicit interaction contribution of first quadruplet (unfolded space)
    real, dimension(:,:), allocatable     :: sa2       ! explicit interaction contribution of second quadruplet (unfolded space)
    real, dimension(:,:), allocatable     :: sfnl      ! total interaction contribution of quadruplets to the right-hand side
    real, dimension(:,:,:), allocatable   :: sigft     ! Fourier-transformed modulation of intrinsic frequency
    real, dimension(:,:,:,:), allocatable :: swtsda    ! several source terms computed at test points
    real, dimension(:), allocatable       :: temp      ! temporary array to store data for MPI communication
    real, dimension(:), allocatable       :: tmcurr    ! mean period at current iteration level
    real, dimension(:), allocatable       :: tmdifc    ! difference in mean period of current and one before previous iteration
    real, dimension(:), allocatable       :: tmprev    ! mean period at previous iteration level
    real, dimension(:,:,:), allocatable   :: trac0     ! explicit part of propagation in present vertex for output purposes
    real, dimension(:,:,:), allocatable   :: trac1     ! implicit part of propagation in present vertex for output purposes
    real, dimension(:,:), allocatable     :: ue        ! energy density for computing 4 wave-wave interactions (unfolded space)
    complex, dimension(:,:), allocatable  :: uxft      ! u-component of Fourier-transformed modulation of ambient current
    complex, dimension(:,:), allocatable  :: uyft      ! v-component of Fourier-transformed modulation of ambient current

    logical                               :: fguess    ! indicate whether first guess need to be applied or not
    logical                               :: lpredt    ! indicate whether action density in first iteration need to be estimated or not
    logical                               :: initial_prediction_pass

    logical, dimension(:,:), allocatable  :: anybin    ! true if bin is active in considered sweep
    logical, dimension(:,:), allocatable  :: anyblk    ! true if bin is blocked by a counter current based on a CFL criterion
    logical, dimension(:), allocatable    :: anywnd    ! true if wind input is active in considered bin
    logical, dimension(:,:), allocatable  :: groww     ! check for each frequency whether the waves are growing or not
                                                       ! in a spectral direction

    character(80)                         :: msgstr    ! string to pass message

    type(celltype), dimension(:), pointer :: cell      ! datastructure for cells with their attributes
    type(verttype), dimension(:), pointer :: vert      ! datastructure for vertices with their attributes

    ! help arrays for FFT in relation to QC scattering
    complex(kind=8), dimension(:,:), allocatable :: cft   ! Fourier coefficients (FFT)
    real   (kind=8), dimension(:,:), allocatable :: rft   ! input data (FFT)
    real   (kind=8), dimension(:,:), allocatable :: sft   ! input data (FFT)
    real   (kind=8), dimension(:)  , allocatable :: wft   ! work array (FFT)
    real   (kind=8), dimension(:)  , allocatable :: wsave ! work array (FFT)

    ! help arrays for FFT in relation to QC surf breaking
    complex(kind=8), dimension(:,:), allocatable :: cfd   ! Fourier coefficients (FFT)
    real   (kind=8), dimension(:)  , allocatable :: wfd   ! work array (FFT)
    real   (kind=8), dimension(:)  , allocatable :: wsavd ! work array (FFT)

!$  integer, external :: omp_get_num_threads ! number of OpenMP threads being used
!$  integer, external :: omp_get_thread_num  ! get thread number
!$  integer, external :: omp_get_max_threads
!
!   Structure
!
!   Description of the pseudo code
!
!   Source text

    if (ltrace) call strace (ient,'SwanCompUnstruc')

    ! point to vertex and cell objects

    vert => gridobject%vert_grid
    cell => gridobject%cell_grid

    ! some initializations

    ICMAX  = 3         ! stencil size
    PROPSL = PROPSC
    idummy = 0         ! structured-grid arguments are unused for OPTG=5

    IXCGRD(1) = -9999  ! to be used in routines SINTGRL and SOURCE so that Ursell number and
    IYCGRD(1) = -9999  ! quadruplets are calculated once in each vertex during an iteration

    tid   = 0
    thread_count = 1
!$  thread_count = omp_get_max_threads()
    call thread_workspaces%ensure_unstructured(thread_count)

    ! print all the settings used in SWAN run

    if ( it == 1 .and. ITEST > 0 ) call SWPRSET (spcsig, spcdir,&
                                                  triads%collinear)

    ! print test points

    if ( NPTST > 0 ) then
       do j = 1, NPTST
          write (PRINTF,"(' Test points :',2i7)") j, xytst(j)
       enddo
    endif

    ! allocation of shared arrays

    IF (timing_enabled) CALL SWTSTA(101)
    allocate(islmin(nverts))
    allocate( nflim(nverts))
    allocate(nrscal(nverts))

    allocate(hscurr(nverts))
    allocate(hsprev(nverts))
    allocate(hsdifc(nverts))
    allocate(tmcurr(nverts))
    allocate(tmprev(nverts))
    allocate(tmdifc(nverts))
    allocate(temp  (nverts))

    allocate(swtsda(MDC,MSC,NPTSTA,MTSVAR))

    if ( IQUAD > 2 ) then
       allocate(memnl4(MDC,MSC,nverts), stat = istat)
       if ( istat /= 0 ) then
          call msgerr ( 4, 'Allocation problem in SwanCompUnstruc: array memnl4 ' )
          return
       endif
    else
       allocate(memnl4(0,0,0))
    endif

    if ( IBRAG == 3 ) then
       allocate(membrg(MDC,MSC,nverts), stat = istat)
       if ( istat /= 0 ) then
          call msgerr ( 4, 'Allocation problem in SwanCompUnstruc: array membrg ' )
          return
       endif
    else
       allocate(membrg(0,0,0))
    endif

    if ( IQCM > 0 ) then
       allocate(memqcm(MDC,MSC,nverts), stat = istat)
       if ( istat /= 0 ) then
          call msgerr ( 4, 'Allocation problem in SwanCompUnstruc: array memqcm ' )
          return
       endif
    else
       allocate(memqcm(0,0,0))
    endif

    if ( ISURF > 0 .and. IGEN == 4 ) then
       allocate(memqcb(MDC,MSC,nverts), stat = istat)
       if ( istat /= 0 ) then
          call msgerr ( 4, 'Allocation problem in SwanCompUnstruc: array memqcb ' )
          return
       endif
    else
       allocate(memqcb(0,0,0))
    endif

    if ( IWIND == 8 ) then
       istat = 0
       allocate(memsina(MDC,MSC,nverts), stat = istat)
       if ( istat == 0 ) allocate(memsinb(MDC,MSC,nverts), stat = istat)
       if ( istat /= 0 ) then
          call msgerr ( 4, 'Allocation problem in SwanCompUnstruc: array memsin ' )
          return
       endif
    else
       allocate(memsina(0,0,0))
       allocate(memsinb(0,0,0))
    endif

    IF (timing_enabled) CALL SWTSTO(101)

    ! initialization of shared arrays

    hscurr = 0.
    hsprev = 99999.
    hsdifc = 0.
    tmcurr = 0.
    tmprev = 0.
    tmdifc = 0.

    swtsda = 0.

    ! spawn a parallel region
    !
    !$omp parallel default(shared) &
    !$omp private(cad, cas, cax, cay, cgo, kwave, dmw) &
    !$omp private(idcmin, idcmax, iscmin, iscmax, anybin) &
    !$omp private(amat, rhs, ac2old) &
    !$omp private(anywnd, obredf, reflso, alimw, groww, anyblk, fbd) &
    !$omp private(disc0, disc1, genc0, genc1, redc0, redc1, trac0, trac1, leakcf) &
    !$omp private(sigft, cgft, uxft, uyft, cft, rft, sft, wft, wsave, cfd, wfd, wsavd) &
    !$omp private(ue, sa1, sa2, sfnl) &
    !$omp private(qtl1, qtl2) &
    !$omp private(da1c, da1p, da1m, da2c, da2p, da2m, dsnl) &
    !$omp private(tid, iter, kvert, ifront) &
    !$omp private(ivert, jc, k, j, icell, n, v, vu, swpnr, swpdir, sdir, rdx, rdy, lpredt, initial_prediction_pass, vb, ve, iface, link, inocnt, thmin, thmax, th1, th2) &
    !$omp private(iddlow, iddtop, idtot, isslow, isstop, istot) &
    !$omp private(abrbot, kmespc, idwmin, idwmax, hs, etot, qbloc, ufric, fpm, thetaw, hm, wind10, smebrk, kteta) &
    !$omp private(dhdx, dhdy, dkdx, dkdy, duxdx, duxdy, duydx, duydy) &
    !$omp copyin(ICMAX, COSLAT, IPTST, TESTFL, RDFSIN)
    !
    ! print number of threads set by environment
    !
    !$omp master
    !$    if ( it == 1 .and. ITEST >= 10 ) &
    !$    write (SCREEN,'(a,i2/)') ' Number of threads during execution of parallel region = ', omp_get_num_threads()
    !$omp end master
    !
    ! get thread number
    !
    !$ tid = omp_get_thread_num()
    tid = tid + 1

    ! allocation of private arrays

    IF (timing_enabled) CALL SWTSTA(101)
    allocate(   cad(MDC,MSC      ))
    allocate(   cas(MDC,MSC      ))
    allocate(   cax(MDC,MSC,ICMAX))
    allocate(   cay(MDC,MSC,ICMAX))
    allocate (  cgo(    MSC,ICMAX))
    allocate (kwave(    MSC,ICMAX))
    allocate (  dmw(    MSC,ICMAX))

    allocate(idcmax(    MSC))
    allocate(idcmin(    MSC))
    allocate(iscmax(MDC    ))
    allocate(iscmin(MDC    ))
    allocate(anybin(MDC,MSC))

    allocate(  amat(MDC,MSC,5))
    allocate(   rhs(MDC,MSC  ))
    allocate(ac2old(MDC,MSC  ))

    allocate(anywnd(MDC))
    allocate(obredf(MDC,MSC,2))
    allocate(reflso(MDC,MSC))
    allocate( alimw(MDC,MSC))
    allocate( groww(MDC,MSC))
    allocate(anyblk(MDC,MSC))

    allocate( disc0(MDC,MSC,MDISP))
    allocate( disc1(MDC,MSC,MDISP))
    allocate( genc0(MDC,MSC,MGENR))
    allocate( genc1(MDC,MSC,MGENR))
    allocate( redc0(MDC,MSC,MREDS))
    allocate( redc1(MDC,MSC,MREDS))
    allocate( trac0(MDC,MSC,MTRNP))
    allocate( trac1(MDC,MSC,MTRNP))
    allocate(leakcf(MDC,MSC      ))

    allocate(dkdx(MSC))
    allocate(dkdy(MSC))

    if ( IBRAG > 1 ) then
       allocate(fbd(MDC,MDC,MSC))
    else
       allocate(fbd(0,0,0))
    endif

    if ( IQCM > 0 ) then

       ! allocate work arrays for FFT

       allocate(rft(ncoz,ncoz))
       allocate(sft(ncoz,ncoz))
       allocate(cft(ncoz,ncoz))

       allocate(wft  (lenwft))
       allocate(wsave(lensav))

       ! initialization FFT

       call cfft2i ( ncoz, ncoz, wsave, lensav, ierror )
       if ( ierror /= 0 ) then
          write (msgstr, '(a,i6)') 'something went wrong with the FFT initialization - return code is ',ierror
          call msgerr ( 4, trim(msgstr) )
       endif

       ! allocate and initialize Fourier-transformed modulations of relative frequency, group velocity and ambient current

       allocate(sigft(ncoz,ncoz,MSC))
       allocate(cgft (ncoz,ncoz,MSC))
       allocate(uxft (ncoz,ncoz)    )
       allocate(uyft (ncoz,ncoz)    )

       sigft = 0.
       cgft  = 0.
       uxft  = (0.,0.)
       uyft  = (0.,0.)

    else
       allocate(rft  (0,0))
       allocate(sft  (0,0))
       allocate(cft  (0,0))
       allocate(wft  (0))
       allocate(wsave(0))
       allocate(sigft(0,0,0))
       allocate(cgft (0,0,0))
       allocate(uxft (0,0))
       allocate(uyft (0,0))
    endif

    if ( ISURF > 0 .and. IGEN == 4 ) then

       ! allocate work arrays for FFT

       allocate(cfd(myd,mxd))

       allocate(wfd  (lenwfd))
       allocate(wsavd(lensvd))

       ! initialization FFT

       call cfft2i ( myd, mxd, wsavd, lensvd, ierror )
       if ( ierror /= 0 ) then
          write (msgstr, '(a,i6)') 'something went wrong with the FFT initialization - return code is ',ierror
          call msgerr ( 4, trim(msgstr) )
       endif

    else
       allocate(cfd  (0,0))
       allocate(wfd  (0))
       allocate(wsavd(0))
    endif

    ! calculate ranges of spectral space for arrays related to 4 wave-wave interactions
    !
    !$omp single
    IF (timing_enabled) CALL SWTSTA(135)
    if ( IQUAD == 4 ) then
       ! cache the MDIA coefficients once and set the widest spectral range over all quadruplets
       call SWPRE4W (xis, snlc1, dal1, dal2, dal3, spcsig, wwint, wwawg,&
                     wwswg, snl4)
    elseif ( IQUAD > 0 ) then
       call FAC4WW (xis, snlc1, dal1, dal2, dal3, spcsig, wwint, wwawg,&
                    wwswg, snl4)
    endif
    IF (timing_enabled) CALL SWTSTO(135)
    !$omp end single
    !
    ! store frequency- and space-dependent data for triads
    !
    !$omp single
    IF (timing_enabled) CALL SWTSTA(134)
    if ( ITRIAD > 0 ) then
       if ( it == 1 .or. DYNDEP ) call FAC3WW (compda(1,JDP2), spcsig,&
                                                triads)
    endif
    IF (timing_enabled) CALL SWTSTO(134)
    !$omp end single

    if ( IQUAD > 0 ) then
       allocate(  ue(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
       allocate( sa1(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
       allocate( sa2(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
       allocate(sfnl(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
       if ( IQUAD == 1 ) then
          allocate(da1c(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(da1p(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(da1m(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(da2c(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(da2p(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(da2m(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
          allocate(dsnl(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
       else
          allocate(da1c(0,0))
          allocate(da1p(0,0))
          allocate(da1m(0,0))
          allocate(da2c(0,0))
          allocate(da2p(0,0))
          allocate(da2m(0,0))
          allocate(dsnl(0,0))
       endif
    else
       allocate(  ue(0,0))
       allocate( sa1(0,0))
       allocate( sa2(0,0))
       allocate(sfnl(0,0))
       allocate(da1c(0,0))
       allocate(da1p(0,0))
       allocate(da1m(0,0))
       allocate(da2c(0,0))
       allocate(da2p(0,0))
       allocate(da2m(0,0))
       allocate(dsnl(0,0))
    endif

    if ( ITRIAD > 0 ) then
       if (ITRIAD == 1 .or. ITRIAD == 11) then
          allocate(qtl1(  0,0))
          allocate(qtl2(MSC,2))
       else if (ITRIAD == 2 .or. ITRIAD == 3) then
          allocate(qtl1(triads%frequency_dimension,2))
          allocate(qtl2(triads%frequency_dimension,4))
       else if (ITRIAD == 5) then
          allocate(qtl1(triads%frequency_dimension,2))
          allocate(qtl2(triads%frequency_dimension,2))
       endif
    else
       allocate(qtl1(0,0))
       allocate(qtl2(0,0))
    endif
    IF (timing_enabled) CALL SWTSTO(101)

    ! marks vertices active and non-active
    !
    !$omp single
    nwetp = 0.
    do kvert = 1, nverts
       if ( compda(kvert,JDP2) > DEPMIN ) then
          vert(kvert)%active = .true.
          nwetp = nwetp +1.
       else
          vert(kvert)%active = .false.
       endif
    enddo
    if ( it == 1 .and. ITEST > 0 ) write (PRINTF,"(' Number of active points = ',i6,' (fillings-degree: ',f6.2,' %)')") nint(nwetp), nwetp*100./real(nverts)

    ! First guess of action density will be applied if 3rd generation mode is employed and wind is active (IWIND > 2)
    ! Note: this first guess is not used in nonstationary run (NSTATC > 0) or hotstart (ICOND = 4)

    if ( IWIND > 2 .and. NSTATC == 0 .and. ICOND /= 4 ) then
       fguess = .true.
    else
       fguess = .false.
    endif
    !$omp end single

    IF (timing_enabled) CALL SWTSTA(103)
    iterloop: do iter = 1, ITERMX

       ! some initializations

       if ( IQUAD  > 2 ) memnl4(1:MDC,1:MSC,1:nverts) = 0.
       if ( IBRAG == 3 ) membrg(1:MDC,1:MSC,1:nverts) = 0.
       if ( IQCM   > 0 ) memqcm(1:MDC,1:MSC,1:nverts) = 0.
       if ( IWIND == 8 ) memsina(1:MDC,1:MSC,1:nverts) = 0.
       if ( IWIND == 8 ) memsinb(1:MDC,1:MSC,1:nverts) = 0.
       if ( ISURF > 0 .and. IGEN == 4 ) memqcb(1:MDC,1:MSC,1:nverts) = 0.
       if ( ITRIAD > 0 &
            .or. ISURF == 7 &
                       ) then
          compda(1:nverts,JURSEL) = 0.
          compda(1:nverts,JBIPH ) = 0.
       endif

       compda(1:nverts,JDISS) = 0.
       compda(1:nverts,JLEAK) = 0.
       compda(1:nverts,JDSXB) = 0.
       compda(1:nverts,JDSXS) = 0.
       compda(1:nverts,JDSXW) = 0.
       compda(1:nverts,JDSXM) = 0.
       compda(1:nverts,JDSXI) = 0.
       compda(1:nverts,JDSXV) = 0.
       compda(1:nverts,JDSXT) = 0.
       compda(1:nverts,JDSXL) = 0.
       compda(1:nverts,JGENR) = 0.
       compda(1:nverts,JGSXW) = 0.
       compda(1:nverts,JREDS) = 0.
       compda(1:nverts,JRSXQ) = 0.
       compda(1:nverts,JRSXT) = 0.
       compda(1:nverts,JRSXB) = 0.
       compda(1:nverts,JRSXC) = 0.
       compda(1:nverts,JTRAN) = 0.
       compda(1:nverts,JTSXG) = 0.
       compda(1:nverts,JTSXT) = 0.
       compda(1:nverts,JTSXS) = 0.
       compda(1:nverts,JRADS) = 0.
       compda(1:nverts,JQB  ) = 0.

       inocnt = 0

       ! synchronize threads
       !$omp barrier
       !
       !$omp single
       inocnv = 0

       islmin = 9999
       nflim  = 0
       nrscal = 0

       acnrms = -9999.

       ! During first iteration, first guess of action density is based on 2nd generation mode
       ! After first iteration, user-defined settings are re-used

       if ( fguess ) then

          if ( iter == 1 )then

             ! save user-defined settings of 3rd generation mode
             ! Note: surf breaking, bottom friction and triads may be still active

             usrset(1) = IWIND
             usrset(2) = IWCAP
             usrset(3) = IQUAD
             usrset(4) = PNUMS(20)
             usrset(5) = PNUMS(30)

             ! first guess settings

             IWIND     = 2               ! if first guess should be based on 1st generation mode, set IWIND = 1
             IWCAP     = 0
             IQUAD     = 0
             PNUMS(20) = 1.E22           ! no limiter
             PNUMS(30) = 0.              ! no under-relaxation

             write (PRINTF,"(// ' Settings of 2nd generation mode as first guess are used:')")

          elseif ( iter == 2 ) then

             ! re-activate user-defined settings of 3rd generation mode

             IWIND     = usrset(1)
             IWCAP     = usrset(2)
             IQUAD     = usrset(3)
             PNUMS(20) = usrset(4)
             PNUMS(30) = usrset(5)

             write (PRINTF,"(// ' User-defined settings of 3rd generation mode is re-used:')")

          endif

          ! print info

          if ( iter < 3 ) then
             write (PRINTF,"(' ITER ',i4,' GRWMX ',e12.4,' ALFA ',e12.4)") iter, PNUMS(20), PNUMS(30)
             write (PRINTF,"(' IWIND ',i4,' IWCAP ',i4 ,' IQUAD ',i4)") IWIND, IWCAP, IQUAD
             write (PRINTF,"(' ISURF ',i4,' IBOT ',i4 ,' ITRIAD ',i4)") ISURF, IBOT , ITRIAD
             write (PRINTF,"(' IVEG ',i4,' ITURBV',i4 ,' IMUD ',i4/ ' IICE ',i4,' IBRAG ',i4 )") IVEG , ITURBV, IMUD, IICE, IBRAG
          endif

       endif

       ! calculate diffraction parameter and its derivatives

       if ( IDIFFR /= 0 ) call SwanDiffPar ( ac2, compda(1,JDP2), spcsig, diffr )

       ! spatially filter the De Wit's biphase to prevent abrupt changes
       ! note: just copy the unfiltered one to array BIPHAS

       if ( IBIPH == 3 ) call SWBIPM (compda(1,JBIPH), compda(1,JDP2),&
                                      compda(1,JHSIBC),&
                                      triads%biphase_unfiltered)

       ! all vertices are set untagged except non-active ones

       do kvert = 1, nverts
          do jc = 1, vert(kvert)%noc         ! all cells around vertex
             vert(kvert)%updated(jc) = 0
          enddo
       enddo

       do kvert = 1, nverts

          ! in case of non-active vertex set action density equal to zero

          if ( .not.vert(kvert)%active ) then

             do jc = 1, vert(kvert)%noc
                icell = vert(kvert)%cell(jc)%atti(CELLID)
                vert(kvert)%updated(jc) = icell
             enddo

             ac2(:,:,kvert) = 0.

          endif

       enddo
       !$omp end single
       !
       !$omp master
       if ( SCREEN /= PRINTF ) then
          if ( NSTATC == 1 ) then
             if ( IAMMASTER ) write (SCREEN,"('+time ', a18, ', step ',i6, '; iteration ',i4)") CHTIME, it, iter
          else
             write (PRINTF,"(' iteration ', i4)") iter
             if ( IAMMASTER ) write (SCREEN,"(' iteration ', i4)") iter
          endif
       endif
       !$omp end master
       !
       ! loop over a fixed number of sweeps through grid

       if ( iter < itswp ) then
          n = nsweep
       elseif ( .not.FULCIR ) then
          n = nsweep
       else
          n = 1
       endif

       sdir = asort + PI/real(n)

       sweeploop: do swpdir = 1, n

          ! loop over wavefronts

!GRAPH          frontloop: do ifront = 1, nfront(swpdir)
!FXFRO          frontloop: do ifront = 1, nfront

             ! loop over vertices in the grid
             !
             !$omp do schedule(static)
!GRAPH             vertloop: do kvert = fptr(ifront,swpdir), fptr(ifront+1,swpdir)-1
!FXFRO             vertloop: do kvert = fronts(ifront), fronte(ifront)
!
!GRAPH                ivert = flist(kvert,swpdir)
!FXFRO                ivert = vlist(kvert,swpdir)

                if ( vert(ivert)%active ) then   ! this active vertex needs to be updated

                   ! determine whether the present vertex is a test point

                   IPTST  = 0
                   TESTFL = .false.
                   if ( NPTST > 0 ) then
                      do j = 1, NPTST
                         if ( ivert /= xytst(j) ) cycle
                         IPTST  = j
                         TESTFL = .true.
                      enddo
                   endif

                   ! compute gradients of depth or wave number in present vertex meant for computing turning rate

                   call SwanGradDepthorK ( compda(1,JDP2), compda(1,JMUDL2), spcsig, dhdx, dhdy, dkdx, dkdy, ivert )

                   ! compute the derivatives of the ambient current for computing transport velocities in spectral space

                   if ( ICUR /= 0 ) then

                      call SwanGradVel ( compda(1,JDP2), compda(1,JVX2), compda(1,JVY2), duxdx, duxdy, duydx, duydy, ivert )

                   endif

                   celloop: do jc = 1, vert(ivert)%noc

                      icell = vert(ivert)%cell(jc)%atti(CELLID)

                      v(1) = cell(icell)%atti(CELLV1)
                      v(2) = cell(icell)%atti(CELLV2)
                      v(3) = cell(icell)%atti(CELLV3)

                      ! pick up two upwave vertices

                      do k = 1, 3
                         if ( v(k) == ivert ) then
                            vu(1) = v(mod(k  ,3)+1)
                            vu(2) = v(mod(k+1,3)+1)
                            exit
                         endif
                      enddo

                      ! stores vertices of computational stencil

                      KCGRD(1) = ivert
                      KCGRD(2) = vu(1)
                      KCGRD(3) = vu(2)

                      swpnr = 0                                              ! this trick assures to calculate Ursell number and
                      if ( all(mask=vert(ivert)%updated(:)==0) ) swpnr = 1   ! quadruplets only once in each vertex during an iteration

                      ! compute wavenumber and group velocity in points of stencil

                      IF (timing_enabled) CALL SWTSTA(110)
                      call SwanDispParm ( kwave, cgo, dmw, compda(1,JDP2), compda(1,JMUDL2), spcsig, KCGRD, ICMAX )
                      IF (timing_enabled) CALL SWTSTO(110)

                      ! compute wave transport velocities in points of stencil for all directions

                      IF (timing_enabled) CALL SWTSTA(111)
                      call SwanPropvelX ( cax, cay, compda(1,JVX2), compda(1,JVY2), cgo, spcdir(1,2), spcdir(1,3), &
                                          diffr, KCGRD, ICMAX )
                      IF (timing_enabled) CALL SWTSTO(111)

                      ! get local contravariant base vectors and their directions at present vertex

                      do k = 1, 3
                         if ( v(k) == ivert ) then
                            rdx(1) = cell(icell)%geom(k)%rdx1
                            rdx(2) = cell(icell)%geom(k)%rdx2
                            rdy(1) = cell(icell)%geom(k)%rdy1
                            rdy(2) = cell(icell)%geom(k)%rdy2
                            th1    = cell(icell)%geom(k)%th1
                            th2    = cell(icell)%geom(k)%th2
                            exit
                         endif
                      enddo

                      ! in case of spherical coordinates determine cosine of latitude (in degrees)
                      ! and recalculate local contravariant base vectors

                      if ( KSPHER > 0 ) then
                         do k = 1, ICMAX
                            COSLAT(k) = cos(DEGRAD*(vert(KCGRD(k))%attr(VERTY) + YOFFS))
                         enddo
                         do j = 1, 2
                            rdx(j) = rdx(j) / (COSLAT(1) * LENDEG)
                            rdy(j) = rdy(j) / LENDEG
                         enddo
                      endif

                      ! check out of bounds of face directions of present cell

                      if ( th1 < asort .and. th2 < asort ) then
                         th1 = th1 + PI2
                         th2 = th2 + PI2
                      endif

                      if ( th1 > asort+PI2 .and. th2 > asort+PI2 ) then
                         th1 = th1 - PI2
                         th2 = th2 - PI2
                      endif

                      ! compute bounds of present sweep

                      thmin = sdir - PI/real(n)
                      thmax = sdir + PI/real(n)

                      if ( th1 < asort .and. swpdir == n ) then
                         thmin = thmin - PI2
                         thmax = thmax - PI2
                      endif

                      if ( th2 > asort+PI2 .and. swpdir == 1 ) then
                         thmin = thmin + PI2
                         thmax = thmax + PI2
                      endif

                      ! in case no intersection of present cell and sweep, cycle to next cell

                      if ( ( thmin < th1 .and. thmax < th1 ) .or. &
                           ( thmin > th2 .and. thmax > th2 )      ) cycle celloop

                      ! compute spectral directions for the considered sweep in present vertex

                      IF (timing_enabled) CALL SWTSTA(112)
                      call SwanSweepSel ( idcmin, idcmax, anybin, iscmin, iscmax, &
                                          iddlow, iddtop, idtot , isslow, isstop, &
                                          istot , cax   , cay   , rdx   , rdy   , &
                                          spcsig, ICMAX)
                      IF (timing_enabled) CALL SWTSTO(112)

                      if ( idtot > 0 ) then

                         ! compute propagation velocities in spectral space for the considered sweep in present vertex

                         IF (timing_enabled) CALL SWTSTA(113)
                         call SwanPropvelS ( cad             , cas           , compda(1,JVX2), compda(1,JVY2), &
                                             compda(1,JDP1)  , compda(1,JDP2), cax           , cay           , &
                                             kwave           , cgo           , spcsig        , iddlow        , &
                                             iddtop          , spcdir(1,2)   , spcdir(1,3)   , spcdir(1,4)   , &
                                             spcdir(1,5)     , spcdir(1,6)   , rdx           , rdy           , &
                                             dhdx            , dhdy          , dkdx          , dkdy          , &
                                             duxdx           , duxdy         , duydx         , duydy         , &
                                             diffr           , KCGRD         , ICMAX         )
                         IF (timing_enabled) CALL SWTSTO(113)

                         ! estimate action density in case of first iteration at cold start in stationary mode
                         ! (since it is zero in first stationary run)

                         lpredt = .false.
                         if ( iter == 1 .and. ICOND /= 4 .and. NSTATC == 0 ) then
                            lpredt = .true.
                         endif
                         initial_prediction_pass = lpredt
                         prediction_loop: do
                         if ( initial_prediction_pass ) then
                            compda(ivert,JHS) = 0.
                            initial_prediction_pass = .false.
                         else
                         if ( lpredt ) then
                            ! XCGRID/YCGRID omitted: only used on the OPTG==3 branch, never for unstructured.
                            call SPREDT (swpnr , ac2   , cax  , cay  , idcmin, idcmax,        &
                                         isstop, anybin, rdx=rdx, rdy=rdy, obredf=obredf, IGP=KCGRD(1))
                            lpredt = .false.
                         endif

                         ! calculate various integral parameters

                         IF (timing_enabled) CALL SWTSTA(116)
                         call SINTGRL (spcdir          , kwave           , ac2  , compda(1,JDP2), qbloc , compda(1,JURSEL), &
                                       compda(1,JBIPH) ,                                                                    &
                                       rdx             , rdy             , dummy, etot          , abrbot, compda(1,JUBOT) , &
                                       hs              , compda(1,JQB)   , hm   , kmespc        , smebrk, kteta           , &
                                       compda(1,JPBOT) , compda(1,JBOTLV), compda(1,JGAMMA)                       , swpnr , &
                                       urmstop         ,                                                                    &
                                       iddlow          , iddtop          , triads, spectral_powers%value,&
                                       thread_workspaces%unstructured(tid)%source%wcap, KCGRD(1),&
                                       KCGRD(2), KCGRD(3), IXCGRD(1), IYCGRD(1))
                         IF (timing_enabled) CALL SWTSTO(116)

                         compda(ivert,JHS) = hs
                         endif

                         ! compute transmission and/or reflection if obstacle is present in computational stencil

                         obredf = 1.
                         reflso = 0.

                         IF (timing_enabled) CALL SWTSTA(136)
                         if ( NUMOBS > 0 ) then

                            ! determine obstacle for the link(s) in the computational stencil

                            do j = 1, cell(icell)%nof

                               ! determine vertices of the local face

                               vb = cell(icell)%face(j)%atti(FACEV1)
                               ve = cell(icell)%face(j)%atti(FACEV2)

                               if ( vb==ivert .or. ve==ivert ) then

                                  if ( vb==vu(1) .or. ve==vu(1) ) then

                                     iface = cell(icell)%face(j)%atti(FACEID)
                                     link(1) = cross(iface)

                                  elseif ( vb==vu(2) .or. ve==vu(2) ) then

                                     iface = cell(icell)%face(j)%atti(FACEID)
                                     link(2) = cross(iface)

                                  endif

                               endif

                            enddo

                            if ( link(1)/=0 .or. link(2)/=0 ) then

                               ! KGRPNT/XCGRID/YCGRID omitted: only used on the OPTG/=5 branch, never for unstructured.
                               call SWTRCF ( compda(1,JDP2), compda(1,JWLV2), compda(1,JHS)  , link, obredf    , ac2, reflso,         &
                                             cax=cax       , cay=cay        , rdx=rdx        , rdy=rdy         , anybin=anybin,       &
                                             spcsig=spcsig , spcdir=spcdir  , cgo=cgo        , kwave=kwave     ,                      &
                                             hss2=compda(1,JHSS2), tss2=compda(1,JTSS2), dss2=compda(1,JDSS2),                      &
                                             kcgrd=KCGRD, ixcgrd=IXCGRD, iycgrd=IYCGRD )

                            endif

                         endif
                         IF (timing_enabled) CALL SWTSTO(136)
                         if (.not.lpredt) exit prediction_loop
                         enddo prediction_loop

                         ! compute the transport part of the action balance equation

                         IF (timing_enabled) CALL SWTSTA(118)
                         call SwanTranspAc ( amat  , rhs   , leakcf, ac2   , ac1   , &
                                             cgo   , cax   , cay   , cad   , cas   , &
                                             anybin, rdx   , rdy   , spcsig, spcdir, &
                                             obredf, idcmin, idcmax, iscmin, iscmax, &
                                             iddlow, iddtop, isslow, isstop, anyblk, &
                                             trac0 , trac1 , KCGRD, COSLAT, ICMAX )
                         IF (timing_enabled) CALL SWTSTO(118)

                         ! compute the source part of the action balance equation

                         if ( .not.OFFSRC ) then

                            ! initialize wind friction velocity and Pierson Moskowitz frequency

                            ufric = 1.e-15
                            fpm   = 1.e-15

                            ! compute the wind speed, mean wind direction, the PM frequency,
                            ! wind friction velocity and the minimum and maximum counters for
                            ! active wind input

                            IF (timing_enabled) CALL SWTSTA(115)
                            if ( IWIND > 0 ) call WINDP1 ( wind10, thetaw, idwmin        , idwmax        , &
                                                           fpm   , ufric , compda(1,JWX2), compda(1,JWY2), &
                                                           anywnd, spcdir, compda(1,JVX2), compda(1,JVY2), &
                                                           spcsig, ac2                                     &
                                                          ,genc0 , kwave                                   &
                                                         , KCGRD(1))
                            IF (timing_enabled) CALL SWTSTO(115)
                            if ( IWIND > 0 .and. IWIND /= 4 ) compda(ivert,JUSTAR) = ufric

                            ! fill in the triad interpolation and scaling factors for current grid point

                            if (ITRIAD == 1 .or. ITRIAD == 11) then
                               qtl2(:,1) = triads%scaling(:,KCGRD(1),1)
                               qtl2(:,2) = triads%scaling(:,KCGRD(1),2)
                            else if (ITRIAD == 2 .or. ITRIAD == 3) then
                               qtl1(:,1) = triads%interpolation(:,1)
                               qtl1(:,2) = triads%interpolation(:,2)
                               qtl2(:,1) = triads%scaling(:,KCGRD(1),1)
                               qtl2(:,2) = triads%scaling(:,KCGRD(1),2)
                               qtl2(:,3) = triads%scaling(:,KCGRD(1),3)
                               qtl2(:,4) = triads%scaling(:,KCGRD(1),4)
                            else if (ITRIAD == 5) then
                               qtl1(:,1) = triads%interpolation(:,1)
                               qtl1(:,2) = triads%interpolation(:,2)
                               qtl2(:,1) = triads%scaling(:,KCGRD(1),1)
                               qtl2(:,2) = triads%scaling(:,KCGRD(1),2)
                            endif

                            ! compute the source terms

                            IF (timing_enabled) CALL SWTSTA(117)
                            if ( IGEN /= 4 ) then
                            call SOURCE ( iter                , IXCGRD(1)           , IYCGRD(1)           , swpnr               , &
                                          kwave               , spcsig              , spcdir(1,2)         , spcdir(1,3)         , &
                                          ac2                 , compda(1,JDP2)      , amat(1,1,1)         , rhs                 , &
                                          abrbot              , kmespc              , dummy               , compda(1,JUBOT)     , &
                                          ufric               , compda(1,JVX2)      , compda(1,JVY2)      , idcmin              , &
                                          idcmax              , iddlow              , iddtop              , idwmin              , &
                                          idwmax              , isstop              ,                                             &
                                          swtsda(1,1,1,JPWNDS), swtsda(1,1,1,JPWNDD), swtsda(1,1,1,JPWCAP), swtsda(1,1,1,JPBTFR), &
                                          swtsda(1,1,1,JPSWEL),                                                                   &
                                          swtsda(1,1,1,JPWBRK), swtsda(1,1,1,JP4S)  , swtsda(1,1,1,JP4D)  ,                       &
                                          swtsda(1,1,1,JPVEGT), swtsda(1,1,1,JPTURB), swtsda(1,1,1,JPMUD) , swtsda(1,1,1,JPICE) , &
                                          swtsda(1,1,1,JPBRAG), swtsda(1,1,1,JPTRI) ,                                             &
                                          hs                  , etot                , qbloc               , thetaw              , &
                                          hm                  , fpm                 , wind10              , dummy               , &
                                          groww               , alimw               , smebrk              , kteta               , &
                                          snlc1               ,                                                                   &
                                          dal1                , dal2                , dal3                , ue                  , &
                                          sa1                 , sa2                 , da1c                , da1p                , &
                                          da1m                , da2c                , da2p                , da2m                , &
                                          sfnl                , dsnl                , memnl4              , wwint               , &
                                          wwawg               , wwswg               , cgo                 , compda(1,JUSTAR)    , &
                                          compda(1,JZEL)      , spcdir              , anywnd              , dmw                 , &
                                          fbd                 , membrg              ,                                             &
                                          cas                 , qtl1                , qtl2                ,                       &
                                          memsina             , memsinb             ,                                             &
                                          disc0               ,                                                                   &
                                          disc1               , genc0               , genc1               , redc0               , &
                                          redc1               , xis                 , compda(1,JFRC2)     , it                  , &
                                          compda(1,JNPLA2)    , compda(1,JTURB2)    , compda(1,JMUDL2)    ,                       &
                                          compda(1,JAICE2)    , compda(1,JHICE2)    ,                                             &
                                          compda(1,JURSEL)    , anybin              , reflso              , compda(1,JTAUW)     , &
                                         compda(1,JBIPH)                                                                         &
                                         ,urmstop              ,triads              ,snl4               ,spectral_powers,&
                                         thread_workspaces%unstructured(tid)%source%wcap, KCGRD(1)&
                                                                                                                                )
                            endif
                            if ( IQCM > 0 .or. IGEN == 4 ) then
                            call QCSOURCE ( rhs                , amat(1,1,1)         , iter                , ac2                , &
                                            compda(1,JDP2)     , compda(1,JVX2)      , compda(1,JVY2)      , swpnr              , &
                                            IXCGRD(1)          , IYCGRD(1)           , rdx                 , rdy                , &
                                            kwave              , cgo                 , sigft               , cgft               , &
                                            uxft               , uyft                , memqcm              , memqcb             , &
                                            swtsda(1,1,1,JPQCS), swtsda(1,1,1,JPWBRK),                                            &
                                            disc0              , disc1               , genc0               , genc1              , &
                                            redc0              , redc1               , spcsig              , spcdir             , &
                                            idcmin             , idcmax              , isstop              ,                      &
                                            spcdir(1,2)        , spcdir(1,3)         , etot                , hm                 , &
                                            qbloc              , smebrk              , kteta               , kmespc             , &
                                            cft                , rft                 , sft                 , wft                , &
                                            wsave              , cfd                 , wfd                 , wsavd              , &
                                            thread_workspaces%unstructured(tid)%source%wcap%mean_frequency_wam                    &
                                                                                                                                 , KCGRD(1))
                            endif
                            IF (timing_enabled) CALL SWTSTO(117)

                         endif

                         ! update action density by means of solving the action balance equation

                         if ( ACUPDA ) then

                            ! preparatory steps before solving system of equations

                            IF (timing_enabled) CALL SWTSTA(119)
                            call SOLPRE(ac2        , ac2old     , rhs        , amat(1,1,4), &
                                        amat(1,1,1), amat(1,1,5), amat(1,1,2), amat(1,1,3), &
                                        idcmin     , idcmax     , anybin     , idtot      , &
                                        istot      , iddlow     , iddtop     , isstop     , &
                                        spcsig     , KCGRD(1))
                            IF (timing_enabled) CALL SWTSTO(119)

                            if ( IREFR == 0 .and. ITFRE == 0 ) then

                               ! no refraction and no frequency shift
                               ! no need to solve a system, just update solution

                               IF (timing_enabled) CALL SWTSTA(120)
                               do is = 1, MSC
                                  do iddum = idcmin(is), idcmax(is)
                                     id = mod(iddum-1+MDC, MDC) + 1
                                     rval1 = amat(id,is,1)
                                     if ( abs(rval1) > 1.e-20 ) then
                                        rval2 = rval1
                                     else
                                        rval2 = sign(1.e-20,rval1)
                                     endif
                                     ac2(id,is,ivert) = rhs(id,is) / rval2
                                  enddo
                               enddo

                               ! set system to zero

                               amat(:,:,1) = 0.
                               rhs         = 0.
                               IF (timing_enabled) CALL SWTSTO(120)

                            elseif ( .not.DYNDEP .and. ICUR == 0 .or. int(PNUMS(8)) == 0 ) then

                               ! propagation in theta space only
                               ! solve tridiagonal system of equations using Thomas' algorithm

                               IF (timing_enabled) CALL SWTSTA(120)
                               call SOLMAT ( idcmin     , idcmax     , ac2        , rhs, &
                                             amat(1,1,1), amat(1,1,5), amat(1,1,4), KCGRD(1))
                               IF (timing_enabled) CALL SWTSTO(120)

                            else

                               ! propagation in both sigma and theta spaces

                               if ( int(PNUMS(8)) == 1 ) then

                                  ! implicit scheme in sigma space
                                  ! solve pentadiagonal system of equations using SIP solver

                                  IF (timing_enabled) CALL SWTSTA(120)
                                  call SWSIP ( ac2        , amat(1,1,1)    , rhs            , amat(1,1,4), &
                                               amat(1,1,5), amat(1,1,2)    , amat(1,1,3)    , ac2old     , &
                                               PNUMS(12)  , nint(PNUMS(14)), nint(PNUMS(13)), inocnt     , &
                                               iddlow     , iddtop         , isstop         , idcmin     , &
                                               idcmax     , KCGRD(1))
                                  IF (timing_enabled) CALL SWTSTO(120)

                               elseif (int(PNUMS(8)) == 2 ) then

                                  ! explicit scheme in sigma space
                                  ! solve tridiagonal system of equations using Thomas' algorithm

                                  IF (timing_enabled) CALL SWTSTA(120)
                                  call SOLMT1  ( idcmin     , idcmax     , ac2        , rhs    , &
                                                 amat(1,1,1), amat(1,1,5), amat(1,1,4),          &
                                                 isstop     , anyblk     , iddlow     , iddtop , KCGRD(1))
                                  IF (timing_enabled) CALL SWTSTO(120)

                               endif

                            endif

                            ! if negative action density occur rescale with a factor

                            IF (timing_enabled) CALL SWTSTA(121)
                            if ( BRESCL ) call RESCALE ( ac2, isstop, idcmin, idcmax, nrscal, KCGRD(1))
                            IF (timing_enabled) CALL SWTSTO(121)

                            ! store propagation, generation, dissipation, redistribution, leak and radiation stress in present vertex

                            IF (timing_enabled) CALL SWTSTA(124)
                            if ( LADDS )                                                        &
                               call ADDDIS ( compda(1,JDISS), compda(1,JLEAK), ac2   , anybin , &
                                             disc0          , disc1          ,                  &
                                             genc0          , genc1          ,                  &
                                             redc0          , redc1          ,                  &
                                             trac0          , trac1          ,                  &
                                             amat(1,1,4)    , amat(1,1,5)    ,                  &
                                             amat(1,1,2)    , amat(1,1,3)    ,                  &
                                             compda(1,JDSXB), compda(1,JDSXS), compda(1,JDSXW), &
                                             compda(1,JDSXV), compda(1,JDSXT), compda(1,JDSXM), &
                                             compda(1,JDSXI),                                   &
                                             compda(1,JDSXL),                                   &
                                             compda(1,JGSXW), compda(1,JGENR),                  &
                                             compda(1,JRSXQ), compda(1,JRSXT), compda(1,JRSXB), &
                                             compda(1,JRSXC), compda(1,JREDS),                  &
                                             compda(1,JTSXG), compda(1,JTSXT),                  &
                                             compda(1,JTSXS), compda(1,JTRAN),                  &
                                             leakcf         , compda(1,JRADS), spcsig           , KCGRD(1))
                            IF (timing_enabled) CALL SWTSTO(124)

                            ! limit the change of the spectrum

                            IF (timing_enabled) CALL SWTSTA(122)
                            if ( PNUMS(20) < 100. ) call PHILIM ( ac2, ac2old, cgo, kwave, spcsig, anybin, islmin, nflim, qbloc, KCGRD(1) )
                            IF (timing_enabled) CALL SWTSTO(122)

                            ! reduce the computed energy density if the value is larger then the limit value
                            ! as computed in SOURCE in case of first or second generation mode

                            IF (timing_enabled) CALL SWTSTA(123)
                            if ( IWIND == 1 .or. IWIND == 2 ) call WINDP3 ( isstop, alimw, ac2, groww, idcmin, idcmax , KCGRD(1))
                            IF (timing_enabled) CALL SWTSTO(123)

                            ! store some infinity norms meant for convergence check

                            if ( PNUMS(21) == 2. ) call SWACC ( ac2, ac2old, acnrms, isstop, idcmin, idcmax , KCGRD(1))

                         endif

                      endif

                      ! tag updated vertex in present cell

                      vert(ivert)%updated(jc) = icell

                   enddo celloop

                endif

             enddo vertloop
             !$omp end do

          enddo frontloop

          sdir = sdir + PI2/real(n)

          ! synchronize threads before starting next sweep
          !$omp barrier

       enddo sweeploop

       ! global sum to inocnv counter
       !
       !$omp atomic
       inocnv = inocnv + inocnt

       ! synchronize threads before checking stop criterion
       !$omp barrier
       !
       ! exchange action densities with neighbours in parallel run

       if ( ACUPDA .and. PARLL ) then
          IF (timing_enabled) CALL SWTSTA(213)
          do id = 1, MDC
             do is = 1, MSC
                temp(:) = ac2(id,is,:)
!METIS                call SwanUvExchgR ( temp )
                ac2(id,is,:) = temp(:)
             enddo
          enddo
          IF (timing_enabled) CALL SWTSTO(213)
       endif

       ! store the source terms assembled in test points per iteration in the files IFPAR, IFS1D and IFS2D
       !
       !$omp master
       IF (timing_enabled) CALL SWTSTA(105)
       if ( NPTST > 0 .and. NSTATM == 0 ) then
          if ( IFPAR > 0 ) write (IFPAR,"(i4, t41, 'iteration')") iter
          if ( IFS1D > 0 ) write (IFS1D,"(i4, t41, 'iteration')") iter
          if ( IFS2D > 0 ) write (IFS2D,"(i4, t41, 'iteration')") iter
          ! KGRPNT is omitted: unstructured (OPTG=5) never uses it (see PLTSRC decl).
          call PLTSRC ( swtsda(1,1,1,JPWNDS), swtsda(1,1,1,JPWNDD), swtsda(1,1,1,JPWCAP), swtsda(1,1,1,JPBTFR), &
                        swtsda(1,1,1,JPWBRK), swtsda(1,1,1,JP4S)  , swtsda(1,1,1,JP4D)  , swtsda(1,1,1,JPTRI) , &
                        swtsda(1,1,1,JPVEGT), swtsda(1,1,1,JPTURB), swtsda(1,1,1,JPMUD) , swtsda(1,1,1,JPICE) , &
                        swtsda(1,1,1,JPBRAG), swtsda(1,1,1,JPQCS) ,                                             &
                        swtsda(1,1,1,JPSWEL),                                                                   &
                        ac2                 , spcsig              , compda(1,JDP2)      , xytst               )
       endif
       IF (timing_enabled) CALL SWTSTO(105)

       ! indicate number of vertices in which rescaling has taken place
       ! (only for debug purposes)

       if ( ITEST >= 30 .or. idebug == 1 ) then

          nwetp = 0.
          do ivert = 1, nverts
             if ( vert(ivert)%active ) nwetp = nwetp +1.
          enddo
          call SWREDUCE( nwetp, 1, SWSUM )

          mxnfr = maxval(nrscal)
          npfr  = count(mask=nrscal>0)

          ! perform global reductions in parallel run

          call SWREDUCE( npfr , 1, SWSUM )
          call SWREDUCE( mxnfr, 1, SWMAX )

          frac = real(npfr)*100./nwetp
          if ( npfr > 0 ) write (PRINTF,"(1x,'use of ',a9,' in ',f6.2,' % of active vertices with maximum in spectral space = ',i4)") 'rescaling', frac, mxnfr
          if ( NSTATC == 0 .and. npfr > 0 .and. IAMMASTER ) write (SCREEN,"(1x,'use of ',a9,' in ',f6.2,' % of active vertices with maximum in spectral space = ',i4)") 'rescaling', frac, mxnfr

       endif

       ! indicate number of vertices in which limiter has taken place
       ! (only for debug purposes)

       if ( ITEST >= 30 .or. idebug == 1 ) then

          nwetp = 0.
          do ivert = 1, nverts
             if ( vert(ivert)%active ) nwetp = nwetp +1.
          enddo
          call SWREDUCE( nwetp, 1, SWSUM )

          mxnfl = maxval(nflim)
          npfl  = count(mask=nflim>0)

          ! perform global reductions in parallel run

          call SWREDUCE( npfl , 1, SWSUM )
          call SWREDUCE( mxnfl, 1, SWMAX )

          frac = real(npfl)*100./nwetp
          if ( npfl > 0 ) write (PRINTF,"(1x,'use of ',a9,' in ',f6.2,' % of active vertices with maximum in spectral space = ',i4)") 'limiter', frac, mxnfl
          if ( NSTATC == 0 .and. npfl > 0 .and. IAMMASTER ) write (SCREEN,"(1x,'use of ',a9,' in ',f6.2,' % of active vertices with maximum in spectral space = ',i4)") 'limiter', frac, mxnfl

          mnisl = minval(islmin)

          call SWREDUCE( mnisl, 1, SWMIN )

          if ( npfl > 0 ) write (PRINTF,"(1x,'lowest frequency occured above which limiter is applied = ',f7.4,' Hz')") spcsig(mnisl)/PI2
          if ( NSTATC == 0 .and. npfl > 0 .and. IAMMASTER ) write (SCREEN,"(1x,'lowest frequency occured above which limiter is applied = ',f7.4,' Hz')") spcsig(mnisl)/PI2

       endif

       ! indicate number of vertices in which the SIP solver did not converged
       ! (only for debug purposes)

       if ( ITEST >= 30 .or. idebug == 1 ) then
          call SWREDUCE( inocnv , 1, SWSUM )
          if ( (DYNDEP .or. ICUR /= 0) .and. inocnv /= 0 ) then
             write (PRINTF,"(2x,'SIP solver: no convergence in ',i4,' vertices')") inocnv
          endif
       endif
       !$omp end master
       !
       ! exchange array COMPDA with neighbours in parallel run

       if ( PARLL ) then
          IF (timing_enabled) CALL SWTSTA(213)
          do j = 1, MCMVAR
             temp(:) = compda(:,j)
!METIS             call SwanUvExchgR ( temp )
             compda(:,j) = temp(:)
          enddo
          IF (timing_enabled) CALL SWTSTO(213)
       endif

       ! store QC bulk dissipation to previous iteration

       if ( ISURF > 0 .and. IGEN == 4 ) then
          disbk0 = disbk1
!METIS          if ( PARLL ) call SwanUvExchgR ( disbk0 )
       endif

       ! info regarding the iteration process and the accuracy

       if ( PNUMS(21) <= 1. ) then

          IF (timing_enabled) CALL SWTSTA(102)
          if ( PNUMS(21) == 0. ) then

             !$omp single
             call SwanConvAccur ( accur, hscurr, tmcurr, compda(1,JDHS), compda(1,JDTM), xytst, spcsig, ac2 )
             !$omp end single

          else

             !$omp single
             call SwanConvStopc ( accur, hscurr, hsprev, hsdifc, tmcurr, tmprev, tmdifc, compda(1,JDHS), compda(1,JDTM), xytst, spcsig, ac2 )
             !$omp end single

          endif

          !$omp master
          if ( iter == 1 ) then
             accur = -9999.
             write (PRINTF,"(' not possible to compute accuracy, first iteration',/)")
             if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(' not possible to compute accuracy, first iteration',/)")
          else
             write (PRINTF,"(' accuracy OK in ',f6.2,' % of wet grid points (',f6.2,' % required)',/ )") accur, PNUMS(4)
             if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(' accuracy OK in ',f6.2,' % of wet grid points (',f6.2,' % required)',/ )") accur, PNUMS(4)
          endif
          !$omp end master
          IF (timing_enabled) CALL SWTSTO(102)

          ! if accuracy has been reached then terminates iteration process

          if ( accur >= PNUMS(4) ) exit iterloop

       elseif ( PNUMS(21) == 2. ) then

         !$omp master
          IF (timing_enabled) CALL SWTSTA(102)
          write (PRINTF,"(7x,'Rho ','Maxnorm ',' Stoppingcrit.')")
          if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(7x,'Rho ','Maxnorm ',' Stoppingcrit.')")
          if ( iter == 1 ) then
             stopcr = -9999.
             write (PRINTF,"(' not possible to compute accuracy, first iteration',/)")
             if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(' not possible to compute accuracy, first iteration',/)")
          else
             if ( acnrms(1) < 1.e-20 .or. acnrmo < 1.e-20 ) then
                write (PRINTF,"(' norm less then 1e-20, no stopping criterion',/)")
                if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(' norm less then 1e-20, no stopping criterion',/)")
             else
                rhof   = acnrms(1)/acnrmo
                stopcr = PNUMS(1)*acnrms(2)*(1.-rhof)/rhof
                write (PRINTF,"(1x,ss,' ',1pe13.6e2,' ',1pe13.6e2,' ',1pe13.6e2,/)") rhof, acnrms(1), stopcr
                if ( NSTATC == 0 .and. IAMMASTER ) write (SCREEN,"(1x,ss,' ',1pe13.6e2,' ',1pe13.6e2,' ',1pe13.6e2,/)") rhof, acnrms(1), stopcr
             endif
          endif
          acnrmo = acnrms(1)
          IF (timing_enabled) CALL SWTSTO(102)
          !$omp end master
          !
          ! if accuracy has been reached then terminates iteration process

          if ( acnrms(1) < stopcr ) exit iterloop

       endif

    enddo iterloop
    IF (timing_enabled) CALL SWTSTO(103)

    ! deallocation of private arrays

    IF (timing_enabled) CALL SWTSTA(101)
    deallocate(  cad)
    deallocate(  cas)
    deallocate(  cax)
    deallocate(  cay)
    deallocate(  cgo)
    deallocate(kwave)
    deallocate(  dmw)

    deallocate(idcmax)
    deallocate(idcmin)
    deallocate(iscmax)
    deallocate(iscmin)
    deallocate(anybin)

    deallocate(  amat)
    deallocate(   rhs)
    deallocate(ac2old)

    deallocate(anywnd)
    deallocate(obredf)
    deallocate(reflso)
    deallocate( alimw)
    deallocate( groww)
    deallocate(anyblk)

    deallocate( disc0)
    deallocate( disc1)
    deallocate( genc0)
    deallocate( genc1)
    deallocate( redc0)
    deallocate( redc1)
    deallocate( trac0)
    deallocate( trac1)
    deallocate(leakcf)

    deallocate(dkdx)
    deallocate(dkdy)

    deallocate(fbd)

    deallocate(sigft)
    deallocate(cgft)
    deallocate(uxft)
    deallocate(uyft)
    deallocate(cft)
    deallocate(rft)
    deallocate(sft)
    deallocate(wft)
    deallocate(wsave)

    deallocate(cfd)
    deallocate(wfd)
    deallocate(wsavd)

    deallocate(  ue)
    deallocate( sa1)
    deallocate( sa2)
    deallocate(sfnl)
    deallocate(da1c)
    deallocate(da1p)
    deallocate(da1m)
    deallocate(da2c)
    deallocate(da2p)
    deallocate(da2m)
    deallocate(dsnl)

    deallocate(qtl1)
    deallocate(qtl2)
    IF (timing_enabled) CALL SWTSTO(101)

    ! end of parallel region
    !
    !$omp end parallel
    !
    ! store the source terms assembled in test points per time step in the files IFPAR, IFS1D and IFS2D

    IF (timing_enabled) CALL SWTSTA(105)
    if ( NPTST > 0 .and. NSTATM == 1 ) then
       if ( IFPAR > 0 ) write (IFPAR,"(a , t41, 'date-time')") CHTIME
       if ( IFS1D > 0 ) write (IFS1D,"(a , t41, 'date-time')") CHTIME
       if ( IFS2D > 0 ) write (IFS2D,"(a , t41, 'date-time')") CHTIME
       ! KGRPNT is omitted: unstructured (OPTG=5) never uses it (see PLTSRC decl).
       call PLTSRC ( swtsda(1,1,1,JPWNDS), swtsda(1,1,1,JPWNDD), swtsda(1,1,1,JPWCAP), swtsda(1,1,1,JPBTFR), &
                     swtsda(1,1,1,JPWBRK), swtsda(1,1,1,JP4S)  , swtsda(1,1,1,JP4D)  , swtsda(1,1,1,JPTRI) , &
                     swtsda(1,1,1,JPVEGT), swtsda(1,1,1,JPTURB), swtsda(1,1,1,JPMUD) , swtsda(1,1,1,JPICE) , &
                     swtsda(1,1,1,JPBRAG), swtsda(1,1,1,JPQCS) ,                                             &
                     swtsda(1,1,1,JPSWEL),                                                                   &
                     ac2                 , spcsig              , compda(1,JDP2)      , xytst               )
    endif
    IF (timing_enabled) CALL SWTSTO(105)

    ! deallocation of shared arrays

    IF (timing_enabled) CALL SWTSTA(101)
    deallocate(islmin)
    deallocate( nflim)
    deallocate(nrscal)

    deallocate(hscurr)
    deallocate(hsprev)
    deallocate(hsdifc)
    deallocate(tmcurr)
    deallocate(tmprev)
    deallocate(tmdifc)
    deallocate(temp  )

    deallocate(swtsda)

    deallocate(memnl4)
    deallocate(membrg)
    deallocate(memqcm)
    deallocate(memsina)
    deallocate(memsinb)
    deallocate(memqcb)

    IF (timing_enabled) CALL SWTSTO(101)


end subroutine SwanCompUnstruc

end module swan_comp_unstruc
