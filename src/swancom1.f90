
!     SWAN/COMPU    file 1 of 5
!
!
!     PROGRAM SWANCOM1.FOR
!
!     This subroutine SWANCOM1 of the main program SWAN
!     includes the next subroutines :
!
!     SWCOMP  (main subroutine for the computational module)
!     SWOMPU  (carries out computation for one grid point)
!     SWPRSET (print all the settings used in SWAN run)
!     SACCUR  (calculate the accuracy and check if the iteration process
!              can be terminated)
!     INSAC   (initialize the values for the calculation of accuracy)
!     ACTION  (fill the arrays with the derivatives of the action eq.)
!     SINTGRL (calculate some general wave (integral) parameters)
!     SOLPRE  (preparation before solving the system)
!     SOLMAT  (solve tri-diagonal system in absence of a current)
!     SOLMT1  (solve tri-diagonal system in presence of a current;
!              almost identical to SOLMAT, however, space in theta can
!              be periodic)
!     SOURCE  (fill the array with the source terms)
!     PHILIM  (limit the change in action density between two iterations)
!     HJLIM   (limit the change in action density between two iterations  40.61
!              based on Hersbach and Janssen limiter)
!     RESCALE (remove negative values from action density)
!     SWSIP   (solve penta-diagonal system in spectral space by means
!              of Stone's SIP solver)
!     SWSOR   (solve penta-diagonal system in spectral space by means
!              of point SOR method)
!     SWMTLB  (compute bounds of thread loop)
!     SWSTPC  (calculate the accuracy and check if the iteration process  40.41
!              can be terminated based on curvature of Hs)
!     SETUPP  (compute the wave-induced setup for a one-dimensional and
!              two-dimensional run. Note that the one-dimensional mode of 32.01
!              SWAN has been coded in this project (H3268))
!     SETUP2D (computation of the change of waterlevel by waves,
!              a 2D Poisson equation in general coordinates is solved)
!
!******************************************************************

MODULE M_CONVERGENCE_SHARED
   IMPLICIT NONE(TYPE, EXTERNAL)
   PRIVATE
   PUBLIC :: SACCUR_HSMN2, SACCUR_SMN2, SACCUR_NINDX
   PUBLIC :: SACCUR_WETGRD, SACCUR_IACCUR
   PUBLIC :: SWSTPC_WETGRD, SWSTPC_IACCUR

   REAL    :: SACCUR_HSMN2 = 0.0
   REAL    :: SACCUR_SMN2  = 0.0
   INTEGER :: SACCUR_NINDX = 0
   INTEGER :: SACCUR_WETGRD = 0
   INTEGER :: SACCUR_IACCUR = 0
   INTEGER :: SWSTPC_WETGRD = 0
   INTEGER :: SWSTPC_IACCUR = 0
END MODULE M_CONVERGENCE_SHARED

module swan_computation
!  De !TIMG-timers worden uit meerdere procedures van deze module
!  aangeroepen, dus hun interface hoort op moduleniveau zichtbaar te zijn.
   use swan_service_interfaces, only: SWTSTA, SWTSTO
   use swan_diffraction_state, only: diffraction_state_t
   use swan_triad_state, only: triad_state_t
   use swan_snl4_tables, only: snl4_tables_t
   use swan_spectral_powers, only: spectral_powers_t
   use swan_source_workspaces, only: thread_workspaces_t, wcap_workspace_t
   implicit none(type, external)
   private
!  Entry points used by the driver (SWCOMP) and by the unstructured solver,
!  which reuses the structured sweep building blocks.
   public :: SWCOMP, SWPRSET, SINTGRL, SOLPRE, SOLMAT, SOLMT1, SOURCE
   public :: PHILIM, RESCALE, SWSIP
contains

SUBROUTINE SWCOMP (AC1        ,AC2        ,&
&COMPDA     ,&
&SPCDIR     ,SPCSIG     ,&
&XYTST      ,&
&IT         ,KGRPNT     ,&
&XCGRID     ,YCGRID     ,&
&CROSS      ,DIFFR      ,TRIADS, SNL4, SPECTRAL_POWERS, THREAD_WORKSPACES)
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_parallel, ONLY: SWCOLLECT, SWEXCHG
!  The remaining imports are used only from switch-hidden call sites, so each
!  carries the prefix of the variant that calls it. Importing them
!  unconditionally instead breaks the other variant: SWRECVAC and SWSENDAC do
!  not exist in a !JAC build at all.
!WFR   USE swan_parallel, ONLY: SWRECVAC, SWSENDAC
!JAC   USE swan_parallel, ONLY: SWSYNC
   USE swan_propagation, ONLY: DIFPAR, SWAPRE
   USE swan_nonlinear_interactions, ONLY: FAC3WW, FAC4WW, SWBIPM, SWPRE4W
   USE swan_dissipation, ONLY: PLTSRC
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA, STPNOW

!******************************************************************

   USE swan_time, ONLY: default_time_context
   USE OCPCOMM2
   USE OCPCOMM3
   USE OCPCOMM4
   USE SWCOMM1
   USE SWCOMM2
   USE SWCOMM3
   USE SWCOMM4
   USE SwanQCM
   USE M_PARALL
   USE m_constants, ONLY: init_constants
   USE m_xnldata
   USE m_fileio
   USE m_propcache, ONLY: prop_cache_reset
   USE swan_fftw_compat, ONLY: cfft2i
!ESMF   USE M_GENARR, ONLY: SAVE_SINBAC, SINBAC

   IMPLICIT NONE(TYPE, EXTERNAL)

    TYPE(diffraction_state_t), INTENT(INOUT) :: DIFFR
    TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
    TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4
    TYPE(spectral_powers_t), INTENT(IN) :: SPECTRAL_POWERS
    TYPE(thread_workspaces_t), INTENT(INOUT) :: THREAD_WORKSPACES


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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     30.75: IJsbrand Haagsma (Bug fix)
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     31.03: Annette Kieftenburg
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     33.08: W. Erick Rogers (some S&L scheme-related changes)
!     33.10: Nico Booij and Erick Rogers
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.17: IJsbrand Haagsma
!     40.21: Agnieszka Herman
!     40.22: John Cazes and Tim Campbell
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.31: Tim Campbell and John Cazes
!     40.31: Andre van der Westhuysen
!     40.41: Andre van der Westhuysen
!     40.41: Marcel Zijlema
!     40.41: Andre van der Westhuysen
!     40.59: W. Erick Rogers
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!
!  1. Updates
!
!     30.72, Nov. 97: Declaration of MSC4MI, MSC4MA, MDC4MI, MDC4MA and
!                     ISTAT removed because they are common and already
!                     declared in the INCLUDE file
!     30.72, Nov. 97: ITERMX can be chosen freely with NUM ACCUR also in dynamic
!                     mode. Default ITERMX=6. Needs extensive testing
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     32.02, Jan. 98: Introduced 1D-version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: call WINDP0 removed, function taken over by WINDP1
!     30.72, Mar. 98: Current switched off for the first iteration, when
!                     preconditining is required
!     30.72, Mar. 98: Writes the result of the iteration step to the PRINT
!                     file
!     30.75, Mar. 98: Renamed SLOW to SIGLOW, because SLOW was used only locally
!     31.03, Feb. 98: Call SETUPP added, initialisation of array SETPDA
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Jan. 99: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     33.08, July 98: some S&L scheme-related changes
!     30.82, July 99: Corrected argumentlist SETUPP
!     40.00, July 99: argument KQUAD removed from call PLTSRC
!     30.82, Sep. 99: Modified messages in case of non-convergence
!     33.10, Jan. 00: minor changes re: the SORDUP scheme
!     40.03, Mar. 00: Ursell number is now array (value for each grid point)
!     40.02, Oct. 00: Avoided real/int conflict by introducing replacing
!                     RWAREA for WAREA in FAC4WW and SETUPP
!     40.13, Mar. 01: comments changed;
!                     order of calling SWAPAR and SPROXY changed
!                     message concerning lack of convergence only to print file
!                     in nonstationary cases
!     40.21, Aug. 01: implementation of diffraction
!     40.22, Sep. 01: WAREA, LWAREA, and RWAREA structures removed
!                     and replaced with allocated arrays to ease
!                     OpenMP implementation.
!     40.22, Sep. 01: OpenMP directives were added to parallelize the
!                     outer Y loop for the call to SWOMPU in the sweep
!                     across the computational grid.
!     40.22, Sep. 01: Added logical array LLOCK for thread management
!                     during parallel operation.  It will not affect
!                     serial execution.
!     40.22, Sep. 01: Changed array definitions to use the parameter
!                     MICMAX instead of ICMAX.
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.23, Aug. 02: Print of CPU times added
!     40.23, Aug. 02: Print of use of limiter and rescaling
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Jul. 03: some improvements and corrections w.r.t. OpenMP
!     40.31, Sep. 03: Under-relaxation parameter is set to zero in first
!                     iteration because of the first guess
!     40.41, May  04: Implemented XNL (Webb-Resio-Tracy) method for
!                     quadruplet interactions
!     40.41, Jun. 04: Implementation of curvature-based convergence check
!     40.41, Aug. 04: some code optimization
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.59, Aug. 07: stencil modification
!     41.90, Oct. 21: adding QC scattering
!
!  2. Purpose
!
!     The aim of this model is to simulate the wave energy in
!     shallow water areas. In the subroutine SWCOMP the main processes
!     taking place in the shallow water zone are determined in
!     several subroutines.
!     The input for this subroutine comes from SWANPRE1 and SWANPRE2.
!     The output is send to the subroutines SWANOUT1 and SWANOUT2.
!     The output consist of some characteristic
!     wave parameters and the wave action density. The equations are
!     all based on the action density N which is a function of the
!     spatial position (x,y), the relative frequency (s) and the
!     spectral direction (d).
!
!  3. Method
!
!     Keywords:
!     Action density, propagation terms, refraction, reflection,
!     white capping, wave breaking, bottom friction, nonlinear
!     and nonhomogeneous wind- and current-fields, wave blocking,
!     fully spectral description, nonlinear wave-wave interaction,
!     higher order upwind schemes, flux limiting, SIP solver
!
!  4. Argument variables
!
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

   REAL    SPCDIR(MDC,6)
   REAL    SPCSIG(MSC)
   REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)


!     INTEGERS:
!     --------------------------------------------------------------
!     IC                Dummy variable: ICode gridpoint:
!                       IC = 1  Top or Bottom gridpoint
!                       IC = 2  Left or Right gridpoint
!                       IC = 3  Central gridpoint
!                       Whether which value IC has, depends of the sweep
!                       If necessary, IC can be enlarged by increasing
!                       the array size of ICMAX
!     ITER              Counter of iterations per 4 sweeps for accuracy
!     ITERMX            Maximum number of iterations in model
!     IX                Counter of gridpoints in x-direction
!     IY                Counter of gridpoints in y-direction
!     IS                Counter of relative frequency band
!     IT                Counter in time space
!     ID                Counter of directional distribution
!     IBOT              Indicator for bottom friction
!                       IBOT = 0  no bottom friction dissipation
!                       IBOT = 1  Jonswap bottom dissipation model
!                       IBOT = 2  Dingemans bottom dissipation model
!                       IBOT = 3  Madsen bottom dissipation model
!                       IBOT = 5  bottom dissipation due to ripples
!     ICUR              Indicator for current
!     ISURF             Indicator for wave breaking
!     ITRIAD            Indicator for nonlinear triad interactions
!     IQUAD             Indicator for nonlinear quadruplet interactions
!     IWCAP             Indicator for white capping
!     IWIND             Indicator for which wind generation model is used
!                       IWIND = 1 first generation wind growth model
!                       IWIND = 2 second generation wind growth model
!                       IWIND = 3 third generation wind growth model
!     IREFR             indicator for refraction (can be tuned off)
!     ITFRE             indicator for transport of action in frequency
!                       space
!     ICMAX             Maximum array size for the points in the molecule
!     KSX      input    Dummy variable to get the right sign in the
!                       numerical difference scheme in X-direction
!                       depending on the sweep direction
!     KSY      input    Dummy variable to get the right sign in the
!                       numerical difference scheme in Y-direction
!                       depending on the sweep direction
!     MXC               Maximum counter of gridppoints in x-direction in
!                       computational model: (XLEN/DX + 1 )
!     MYC               Maximum counter of gridppoints in y-direction in
!                       computational model: (YLEN/DY + 1 )
!     MSC               Maximum counter of relative frequency in
!                       computational model
!     MDC               Maximum counter of directional distribution in
!                       computational model (2PI / DDIR + 1)
!     MTC               Maximum counter of the time, i.e.:
!                       (total time in proto type) / (time step)
!     MBOT              Maximum array size for PBOT
!     MSURF             Maximum array size for PSSURF
!     MTRIAD            Maximum array size for PTRIAD
!     MWCAP             Maximum array size for PWCAP
!     MWIND             Maximum array size for PWIND
!     MNISL             Minimum sigma-index occured in applying limiter
!     MXNFL             Maximum number of use of limiter in a spectral
!                       space
!     MXNFR             Maximum number of use of rescaling in a spectral
!                       space
!     NPFL              Number of geographical points in which limiter
!                       is used
!     NPFR              Number of geographical points in which rescaling
!                       is used
!     NVARW             Number of geographical points in which variance
!                       of the Wigner distribution is negative
!     NWETP             Total number of wet gridpoints
!     IDEBUG            Level of debug output:
!                       0 = no output
!                       1 = print of statistics w.r.t. use of limiter
!                           and rescaling
!     I1GRD             Lower index for thread loop over spatial grid
!     I2GRD             Upper index for thread loop over spatial grid
!     I1MYC             Lower index for thread loop over y-grid row
!     I2MYC             Upper index for thread loop over y-grid row
!
!     REALS:
!     --------------------------------------------------------------
!
!     ALEN              Part of side length of an angle side
!     BETA              Angle between DX end DY
!     BLEN              Part of side length of an angle side
!     DIR               Spectral direction (i.e., ID*DDIR)
!     DX       input    Length of spatial cell in X-direction
!     DY       input    Length of spatial cell in Y-direction
!     DS       input    Width of frequency band (is not constant because
!                       of the logarithmic distribution of the frequency
!     DDIR     input    Width of directional band
!     DT       input    Time step
!     DDX      input    Same as DX but with correct sign depending of the
!                       direction of the sweep (+1. OR -1. ) no input
!     DDY      input    Same as DY but with correct sign depending of the
!                       direction of the sweep (+1. OR -1. ) no input
!     FAC_A             Factor representing the influence of the action-
!                       density depening of the propagation velocity
!     FAC_B             Factor representing the influence of the action-
!                       density depending of the propagation velocity
!     GAMMA             PI - alpha - beta
!     HM                Maximum waveheight (breaking source term)
!     GRAV     input    Gravitational acceleration
!     FRAC              Fraction of total wet points
!
!     one and more dimensional arrays:
!     ---------------------------------
!
!     AC1       4D    Action density as function of D,S,X,Y at time T
!     AC2       4D    (Nonstationary case) action density as function
!                     of D,S,X,Y at time T+DT
!     CGO       2D    Group velocity as function of IC and IS in the
!                     direction of wave propagation in absence of currents
!     CAX       3D    Wave transport velocity in X-direction, function of
!                     (ID,IS,IC)
!     CAY       3D    Wave transport velocity in Y-direction, function of
!                     (ID,IS,IC)
!     CAS       3D    Wave transport velocity in S-direction, function of
!                     (ID,IS,IC)
!     CAD       3D    Wave transport velocity in D-dirction, function of
!                     (ID,IS,IC)
!     COMPDA    3D    array containing depth and other arrays of (IX,IY)  20.39
!                     JDP1    Depth as function of X and Y at time T
!                     JDP2    (Nonstationary case) depth as function of
!                             at time T+DT
!                     JVX1    X-component of current velocity of X and Y
!                             at time T
!                     JVX2    (Nonstationary case) X-component of current
!                             velocity in (X,Y) at time T+DT
!                     JVY1    Y-component of current velocity in (X,Y)
!                             at time T
!                     JVY2    (Nonstationary case) Y-component of current
!                             velocity in (X,Y) at time T+DT
!                     JWX2    X-component of wind velocity in (X,Y)
!                             at time T+DT (nonstationary case)
!                     JWY2    Y-component of wind velocity in (X,Y)
!                             at time T+DT (nonstationary case)
!                     JUBOT   Absolute orbital velocity in a gridpoint (IX,IY)
!     SWTSDA    4D    intermediate data computed for the test points;
!                     there are MTSVAR subarrays:
!                     JPWNDD   wind input term (implicit part)
!                     JPWNDS   wind input term (explicit part)
!                     JPWCAP   whitecapping source term
!                     JPBTFR   bottom friction
!                     JPVEGT   vegetation dissipation
!                     JPTURB   turbulent dissipation
!                     JPMUD    fluid mud-induced wave dissipation
!                     JPICE    dissipation by sea ice
!                     JPWBRK   surf breaking
!                     JPSWEL   swell dissipation
!                     JP4S     quadruplet interactions
!                     JP4D     quadruplet interactions
!                     JPTRI    triad interactions
!                     JPBRAG   Bragg scattering
!                     JPQCS    QC scattering
!     ALIMW     1D    Maximum energy by wind growth. This dummy array is
!                     used because the maximum value has to be checked
!                     direct after the solver of the tri-diagonal matrix
!                     see the subroutine SOLMAT
!     GROWW     1D    Check for a certain frequency if the waves are
!                     growing or not in a spectral direction (LOGICAL)
!     HSAC0     2D    Represent the significant wave height at iter-2
!     HSAC1     2D    Represent the significant wave height at iter-1
!     HSAC2     2D    Represent the significant wave height at iter
!     HSDIFC    2D    Represent Hs(iter) - Hs(iter-2) meant for
!                     computation of curvature of Hs
!     IMATDA    2D    Coefficients of main diagonal of matrix
!     IMATLA    2D    Coefficients of lower diagonal of matrix in theta-space
!     IMATUA    2D    Coefficients of upper diagonal of matrix in theta-space
!     IMAT5L    2D    Coefficients of lower diagonal of matrix in sigma-space
!     IMAT6U    2D    Coefficients of upper diagonal of matrix in sigma-space
!     IMATRA    2D    Coefficients of right hand side
!     KWAVE     2D    wavenumber as function of the relative frequency S
!                     and position IC(ix,iy)
!     DMW       2D    mud dissipation rate as function of IC and IS
!     PBOT      1D    Coefficient for the bottom friction models
!     PSURF     1D    Coefficient for the wave breaking model
!     PTRIAD    1D    Coefficient for the triad interaction model
!     PWCAP     1D    Coefficient for the white capping model
!     PWIND     1D    Coefficient for the wind growth model
!     SACC0     2D    Represents the mean wave frequency at iter-2
!     SACC1     2D    Represents the mean wave frequency at iter-1
!     SACC2     2D    Represents the mean wave frequency at iter
!     TMDIFC    2D    Represent Tm(iter) - Tm(iter-2) meant for
!                     computation of curvature of Tm
!     PWTAIL    1D    coefficients for tail of spectrum
!     QTL1      2D    local interpolation factors for triads
!     QTL2      2D    local scaling factors for triads
!     IDCMIN    1D    frequency dependent counter in directional space
!                     no current <---> current
!     IDCMAX    1D    frequency dependent counter in directional space
!                     no current <---> current
!     ISCMIN    1D    frequency dependent counter in frequency space
!                     no current <---> current
!     ISCMAX    1D    frequency dependent counter in frequency space
!                     no current <---> current
!     ANYBIN    2D    Set for a particular bin TRUE or FALSE depending on
!                     propagation velocities within a sweep
!     WWINT     1D    Counters for 4 wave-wave interactions
!     WWAWG     1D    Weight coefficients for the 4 wave-wave interactions
!     WWSWG     1D    Weights coefficients for the 4 wave-wave interactions
!                     for the semi-implicit computation
!     ISLMIN    1D    Lowest sigma-index occured in applying limiter
!     NFLIM     1D    Number of frequency use of limiter in each
!                     geographical point
!     NRSCAL    1D    Number of frequency use of rescaling in each
!                     geographical point
!     AC2LOC    2D    help array containing action density for
!                     sending/receiving with MPI
!     IARR      1D    help array of type integer for MPI communication
!     ARR       1D    help array of type real for MPI communication
!     REFLSO    2D    contribution to the source term due to reflection
!     FBD       3D    bottom spectrum for Bragg scattering
!
!     arrays for QC scattering:
!     ---------------
!
!     CGFT            Fourier-transformed modulation of group velocity
!     SIGFT           Fourier-transformed modulation of intrinsic frequency
!     UXFT            u-component of Fourier-transformed modulation of ambient current
!     UYFT            v-component of Fourier-transformed modulation of ambient current
!
!     CFT             Fourier coefficients (FFT)
!     RFT             input data (FFT)
!     SFT             input data (FFT)
!     WFT             work array (FFT)
!     WSAVE           work array (FFT)
!
!     arrays for QC surf breaking:
!     ---------------
!
!     CFD             Fourier coefficients (FFT)
!     WFD             work array (FFT)
!     WSAVD           work array (FFT)
!
!     Coefficients for the arrays:
!     -----------------------------
!                         default
!                         value:
!
!     PBOT(1)   = CFC      0.005    (Collins equation)
!     PBOT(2)   = CFW      0.01     (Collins equation)
!     PBOT(3)   = GAMJNS   0.038    (Jonswap formulation)
!     note: this lower friction value combined with second order polynomial wind drag
!     PBOT(4)   = MF      -0.08     (Madsen equation)
!     PBOT(5)   = KN       0.05     (bottom roughness)
!
!     ISURF                1        (Constant breaking coefficient)
!                          2        (variable breaking coefficient
!                                    according to Nelson (1994))
!     PSURF(1)  = ALFA     1.0      (Battjes Janssen)
!     PSURF(2)  = GAMMA    0.73     (breaking criterium)
!
!     PWCAP(1)  = ALFAWC   2.36e-5  (Empirical coefficient)
!     PWCAP(2)  = ALFAPM   3.02E-3  (Alpha of Pierson Moskowitz frequency)
!
!     PWIND(1)  = CF10     188.0    (second generation wind growth model)
!     PWIND(2)  = CF20     0.59     (second generation wind growth model)
!     PWIND(3)  = CF30     0.12     (second generation wind growth model)
!     PWIND(4)  = CF40     250.0    (second generation wind growth model)
!     PWIND(5)  = CF50     0.0023   (second generation wind growth model)
!     PWIND(6)  = CF60    -0.2233   (second generation wind growth model)
!     PWIND(7)  = CF70     0.       (second generation wind growth model)
!     PWIND(8)  = CF80    -0.56     (second generation wind growth model)
!     PWIND(9)  = RHOAW    0.00125  (density air / density water)
!     PWIND(10) = EDMLPM   0.0036   (limit energy Pierson Moskowitz)
!     PWIND(11) = CDRAG    0.0012   (drag coefficient)
!     PWIND(12) = UMIN     1.0      (minimum wind velocity)
!     PWIND(13) = PMLM     0.13     (  )
!
!     PNUMS(1)  = DREL     relative error in Hs and Tm
!     PNUMS(2)  = DHABS    absolute error in Hs
!     PNUMS(3)  = DTABS    absolute error in Tm
!     PNUMS(4)  = NPNTS    number of points were accuracy is reached
!
!     PNUMS(5)  = NOT USED
!     PNUMS(6)  = CDD      blending parameter for finite differences
!                          in theta space
!     PNUMS(7)  = CSS      blending parameter for finite differences
!                          in sigma space
!     PNUMS(8)  = NUMFRE   numerical scheme in frequency space :
!                          1) implicit scheme
!                          2) explicit scheme CFL limited
!                          3) explicit scheme filter after iteration
!     PNUMS(9)  = DIFFC    if explicit scheme is used, then numerical
!                          diffusion coefficient can be chosen
!     PNUMS(12) = EPS2     termination criterion in relative sense for a
!                          penta-diagonal solver
!     PNUMS(13) = OUTP     request for output for a penta-diagonal solver
!     PNUMS(14) = NITER    maximum number of iterations for a penta-diagonal
!                          solver
!     PNUMS(15) = DHOVAL   global error in Hs
!               = CURVAT   curvature of Hs meant for convergence check
!     PNUMS(16) = DTOVAL   global error in Tm01
!     PNUMS(17) = CDLIM    coefficient of limitation of Ctheta
!     PNUMS(18) = FROUDMAX maximum Froude number for reduction of currents
!     PNUMS(19) = CFL      CFL criterion for option explicit scheme
!                          in frequency space (see PNUMS(8))
!     PNUMS(20) = GRWMX    maximum growth in spectral bin
!
!     PNUMS(21) = STOPC    type of stopping criterion:
!                          0: standard SWAN based on relative and global
!                             errors of Hs and Tm01,
!                          1: based on absolute, relative and curvature
!                             errors of Hs
!
!     PNUMS(30) = ALFA     relaxation parameter for under-relaxation method
!
!     arrays for the 4-wave interactions:
!
!     WWINT ( 1 = IDP    WWAWG ( = AGW1    WWSWG ( = SWG1
!             2 = IDP1           = AWG2            = SWG2
!             3 = IDM            = AWG3            = SWG3
!             4 = IDM1           = AWG4            = SWG4
!             5 = ISP            = AWG5            = SWG5
!             6 = ISP1           = AWG6            = SWG6
!             7 = ISM            = AWG7            = SWG7
!             8 = ISM1           = AWG8 )          = SWG8  )
!             9 = ISLOW
!             10= ISHGH
!             11= ISCLW
!             12= ISCHG
!             13= IDLOW
!             14= IDHGH
!             15= MSC4MI
!             16= MSC4MA
!             17= MDC4MI
!             18= MDC4MA
!             19= MSCMAX
!             20= MDCMAX
!             21= IDPP
!             22= IDMM
!             23= ISPP
!             24= ISMM )
!
!
!  6. Local variables
!
!     SIGLOW: recommended lowest frequency when TRIADS are activated

   REAL    SIGLOW

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     INSAC
!     SWOMPU
!     PLTSRC
!     SETUPP
!     SACCUR
!     SWSTPC
!TIMG!     SWTSTA
!TIMG!     SWTSTO
!     SWREDUCE
!JAC!     SWEXCHG
!WFR!     SWRECVAC
!WFR!     SWSENDAC
!JAC!     SWSYNC
!     MSGERR : Handles error messages according to severity
!     NUMSTR : Converts integer/real to string
!     TXPBLA : Removes leading and trailing blanks in string
!MPI!     STPNOW : Logical indicating whether program must
!MPI!              terminated or not
!
!
!  9. Subroutines calling
!
!     SWANPREn, SWANOUTn
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     SWCOMP is the main subroutine is and called of the main program SWAN.
!     The main program SWAN is build of three main subroutines:
!
!     1. SWREAD    (preparation of the computation (reading parameters))
!     2. SWCOMP    (computation of the action densities (discussed below))
!     3. SWOUTP    (output of the computation)
!
!     In this part the subroutine SWCOMP is discussed:
!
!     SWCOMP
!     ======
!      |
!      |------+  INSAC                       determine initial values for
!      |                                     accuracy check
!      |
!      |
!      Begin parallel region over threads
!      |
!      |-------+ SWOMPU
!      |       |
!      |       |--------+ SWAPAR             determ. of waveparameters
!      |       |
!      |       |--------+ SPROXY             comp. of propagation
!      |       |                             velocities of energy:
!      |       |                             CAX, CAY
!      |       |
!      |       |--------+ SPROSD             comp. of propagation
!      |       |                             velocities of energy:
!      |       |                             CAS, CAD
!      |       |
!      |       |--------+ WINDP1             compute absolute wind, FPM
!      |       |                             mean wind direction, min. and
!      |       |                             max. counters for the wind,
!      |       |                             wind friction velocity
!      |       |
!      |       |--------+ CNTAIL             Compute contributions to
!      |       |                             spectrum due to high frequency
!      |       |                             tail
!      |       |
!      |       |--------+ SPREDT             predict energy density in
!      |       |                             gridpoints for first
!      |       |                             iteration
!      |       |
!      |       |--------+ SINTGRL            comp. Ub, Etot, Hmax, Qb,
!      |       |        |                    SME, SMA, SMESPC , SMASPC
!      |       |        |
!      |       |        +-------+ FRABRE     comp. of fraction of
!      |       |                             breaking waves
!      |       |
!      |       |--------+ SOURCE             comp. of source terms
!      |       |        |
!      |       |        +-------+ SBOT       bottom friction
!      |       |        |
!      |       |        +-------+ SVEG       dissipation due to vegetation
!      |       |        |
!      |       |        +-------+ STURBV     dissipation due to turbulence
!      |       |        |
!      |       |        +-------+ SMUD       fluid mud-induced wave dissipation
!      |       |        |
!      |       |        +-------+ SICE       dissipation by sea ice
!      |       |        |
!      |       |        +-------+ SWCAP      white capping
!      |       |        |
!      |       |        +-------+ SSURF      wave breaking
!      |       |        |
!      |       |        +-------+ SWLTA      nonlinear triad interactions based on LTA
!      |       |        |
!      |       |        +-------+ SWSNL?     nonlinear quadruplet interactions
!      |       |        |
!      |       |        +-------+ SWIND1     first generation wind model
!      |       |                |
!      |       |                + -- WINDP2  compute total wind sea energy o
!      |       |                |    SWIND2  second generation wind model
!      |       |                |
!      |       |                + SWIND3     third generation wind model
!      |       |
!      |       |--------+ ACTION             comp. of ACTION balance eq.
!      |       |        |                    (  @ CAn/@n )
!      |       |        |
!      |       |        +-------+ STIME      comp. of (@AC2/@t)
!      |       |        |
!      |       |        +-------+ STRSX      @[CAX AC2]/@X
!      |       |        |
!      |       |        +-------+ STRSY      @[CAY AC2]/@Y
!      |       |        |
!      |       |        +-------+ STRSS      @[CAS AC2]/@S
!      |       |        |
!      |       |        +-------+ STRSD      @[CAD AC2]/@D
!      |       |
!      |       |--------+ SOLMAT             solve the matrix which is
!      |       |                             filled in SOURCE and ACTION
!      |       |
!      |       |--------+ FILIMP             filter the frequency spectrum
!      |       |   |                         in presence of a current using
!      |       |   |                         a diffusion model (important for
!      |       |   |                         wave blocking) -->IMPLICIT
!      |       |   |
!      |       |   |----+ DIFSOL             The matrix filled in FILIMP is
!      |       |                             solved for each direction separately
!      |       |
!      |       |--------+ WINDP3             Limit the energy spectrum
!      |                                     for first and second
!      |                                     generation wind model
!      |
!      |
!      End parallel region over threads
!      |
!      |-------+ PLTSRC                      write sourceterm after an
!      |                                     iteration to a file SOURCE
!      |
!      |-------+ SACCUR / SWSTPC             check accuracy of the comp.
!      |
!     END SWCOMP
!
!
! 12. Structure
!
!     The numerical procedure in SWAN is based on the four-direction
!     point Gauss-Seidel technique with the following
!
!       {**************************************************************}
!       {           definition of the sweep directions                 }
!       {                                                              }
!       {                   \         N         /                      }
!       {     swp_NW = 4     _\|      |      |/_   swp_EN = 3          }
!       {                             |                                }
!       {                             |                                }
!       {             W --------------------------- E  (0 degrees,id=1)}
!       {                             |                                }
!       {                   __.       |      .__                       }
!       {     swp_WS = 1     /|       |      |\    swp_SE = 2          }
!       {                  /          S         \                      }
!       {                                                              }
!       {**************************************************************}
!       {                                                              }
!       { swp_NW:         *  ksy=+1   swp_EN:        *  ksy=+1         }
!       {                 |                          |                 }
!       {                 |  -dy                -dy  |                 }
!       {            dx   |                          |  -dx            }
!       {       *---------o  IX,IY            IX,IY  o--------*        }
!       {     ksx=-1                                    ksx=+1         }
!       {                                                              }
!       {                                                              }
!       { swp_WS:    dx               swp_SE:           -dx            }
!       {       *---------o  IX,IY            IX,IY  o--------*        }
!       {       ksx=-1    |                          |     ksx=+1      }
!       {                 |  dy                  dy  |                 }
!       {                 |                          |                 }
!       {                 *  ksy=-1                  *  ksy=-1         }
!       {**************************************************************}
!
!     ----------------------------------------------------------
!     Call INSAC to give values to HSACC and SACC meant for accuracy check
!     ----------------------------------------------------------
!     For IT = 1 to end of computation time (MTC), do,
!
!       If accuracy <= given tolerance, then do iteration,
!
!         -----------------------------------------------------
!         give argument for sweep : swpdir = 1
!         KSX = -1         DDX = +DX.
!         KSY = -1         DDY = +DY.
!         give number of direction a start and an end value:
!         For IY=2 to MYC and IX=2 to MXC, do,
!            Call SWOMPU to compute the wave field
!         -----------------------------------------------------
!         give argument for sweep : swpdir = 2
!         KSX = +1         DDX = -DX.
!         KSY = -1         DDY = +DY.
!         give number of direction a start and an end value:
!         For IX=MXC-1 to 1 and IY=2 to MYC, do,
!            Call SWOMPU to compute the wave field
!         -----------------------------------------------------
!         give argument for sweep : swpdir = 3
!         KSX = +1.         DDX = -DX.
!         KSY = +1.         DDY = -DY.
!         give number of direction a start and an end value:
!         For IY=MYC-1 to 1 and IX=MXC-1 to 1, do,
!            Call SWOMPU to compute the wave field
!         -----------------------------------------------------
!         give argument for sweep : swpdir = 4
!         KSX = -1         DDX = +DX.
!         KSY = +1         DDY = -DY.
!         give number of direction a start and an end value:
!         For IX=2 to MXC and IY=MYC-1 to 1, do,
!            Call SWOMPU to compute the wave field
!         ----------------------------------------------------
!     CALL PLTSRC to write the source term to a file
!     ----------------------------------------------------
!     CALL SACCUR / SWSTPC to check the accuracy of the computation
!     --------------------------------------------------------
!     End of SWCOMP
!     --------------------------------------------------------
!
! 13. Source text
!
!     ************************************************************************
!     *
!     *                  MAIN SUBROUTINE OF COMPUTATIONAL PART
!     *
!     *                               -- SWCOMP --
!     *
!     *                Definition of variables in main program
!     *
!     ************************************************************************

   INTEGER :: ITER  ,IX    ,IY    ,IS    ,IT
   INTEGER :: IP, IDC, ISC
   INTEGER :: KSX   ,KSY   ,SWPDIR
   INTEGER :: INOCNV
   INTEGER :: INOCNT

   REAL ::  DDX   ,DDY   ,ACCUR ,XIS   ,SNLC1 ,DAL1  ,DAL2  ,DAL3

   LOGICAL :: PRECOR

   INTEGER :: MNISL, MXNFL, MXNFR, NPFL, NPFR, NVARW, NWETP
   INTEGER, PARAMETER :: IDEBUG=0
   REAL    :: FRAC

   INTEGER   ISTAT, IF1, IL1
   CHARACTER(LEN=20) CHARS(1)
   CHARACTER(LEN=80) MSGSTR

   INTEGER IARR(10)
   REAL     ARR(10)
   REAL     VARW

!WFR   INTEGER JDUM, JS, JE, JWFRS, JWFRE, INCJ, JJ, JNODE,&
!WFR   &JSD, JED, IE, INCI, III, LSTCP
!WFR
!JAC   INTEGER ISWP
!JAC
   INTEGER II, IX1, IX2, IY1, IY2

!     Add variables for the XNL interface (quadruplet interaction)
   INTEGER :: IXGRID, IXQUAD, IQERR

   INTEGER :: XYTST(2*NPTST)
   INTEGER :: KGRPNT(MXC,MYC)
   INTEGER :: CROSS(2,MCGRD)

   REAL     AC2(MDC,MSC,MCGRD)     ,&
   &AC1(MDC,MSC,MCGRD)     ,&
   &COMPDA(MCGRD,MCMVAR)

   REAL WWAWG(8), WWSWG(8)

   INTEGER, DIMENSION(:), ALLOCATABLE :: IDCMIN, IDCMAX,&
   &ISCMIN, ISCMAX
   INTEGER WWINT(24)

   REAL, DIMENSION(:,:,:), ALLOCATABLE :: CAX,CAY,CAX1,CAY1,&
   &CAS,CAD

   REAL, DIMENSION(:,:), ALLOCATABLE :: CGO,KWAVE,DMW

   REAL, DIMENSION(:,:), ALLOCATABLE :: ALIMW

   REAL, DIMENSION(:,:), ALLOCATABLE :: UE,SA1,SA2,SFNL

   REAL, DIMENSION(:,:), ALLOCATABLE :: DA1C,DA1P,DA1M,&
   &DA2C,DA2P,DA2M,DSNL

   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMNL4
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMBRG
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMQCM
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMQCB
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMSINA
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: MEMSINB

   REAL, DIMENSION(:,:,:), ALLOCATABLE :: OBREDF
   REAL, DIMENSION(:,:), ALLOCATABLE :: REFLSO
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: FBD

   REAL, DIMENSION(:), ALLOCATABLE :: HSAC1,HSAC2,SACC1,SACC2
   REAL, DIMENSION(:), ALLOCATABLE :: HSAC0,HSDIFC
   REAL, DIMENSION(:), ALLOCATABLE :: SACC0,TMDIFC

   REAL, DIMENSION(:,:), ALLOCATABLE :: SETPDA

   LOGICAL, DIMENSION(:), ALLOCATABLE :: GROWW

   LOGICAL, DIMENSION(:), ALLOCATABLE :: ANYWND

   REAL, ALLOCATABLE :: SWTSDA(:,:,:,:)

!     SWMATR and LSWMAT replace the single array SWMATR
!     that is equivalenced to the logical array LSWMATR
!     in the subroutine SWOMPU.
   REAL, DIMENSION(:,:,:), ALLOCATABLE :: SWMATR

   LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: LSWMAT

   REAL, DIMENSION(:,:), ALLOCATABLE :: QTL1, QTL2

!$ LOGICAL, ALLOCATABLE :: LLOCK(:,:)
!$ LOGICAL LLOCKED

   INTEGER, ALLOCATABLE :: ISLMIN(:), NFLIM(:), NRSCAL(:)

!MPI   REAL, ALLOCATABLE :: AC2LOC(:)
!
!     Add variables for OMP thread parameters.
!$ INTEGER, EXTERNAL :: OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!$ INTEGER, EXTERNAL :: OMP_GET_MAX_THREADS
   INTEGER :: THREAD_COUNT, THREAD_INDEX
   INTEGER I1GRD,I2GRD,I1MYC,I2MYC

   INTEGER, SAVE :: IENT = 0
   INTEGER MSTPDA, KWIND, KWCAP, KQUAD, IYSTEP, J, IJ, IK, ID
   REAL ALFAT, GRWOLD

   INTEGER IERR

!     arrays for QC scattering
   COMPLEX(KIND=8), DIMENSION(:,:)  , ALLOCATABLE :: CFT
   REAL   (KIND=8), DIMENSION(:,:)  , ALLOCATABLE :: RFT
   REAL   (KIND=8), DIMENSION(:,:)  , ALLOCATABLE :: SFT
   REAL   (KIND=8), DIMENSION(:)    , ALLOCATABLE :: WFT
   REAL   (KIND=8), DIMENSION(:)    , ALLOCATABLE :: WSAVE

   REAL           , DIMENSION(:,:,:), ALLOCATABLE :: CGFT
   REAL           , DIMENSION(:,:,:), ALLOCATABLE :: SIGFT
   COMPLEX        , DIMENSION(:,:)  , ALLOCATABLE :: UXFT
   COMPLEX        , DIMENSION(:,:)  , ALLOCATABLE :: UYFT

!     arrays for QC surf breaking
   COMPLEX(KIND=8), DIMENSION(:,:)  , ALLOCATABLE :: CFD
   REAL   (KIND=8), DIMENSION(:)    , ALLOCATABLE :: WFD
   REAL   (KIND=8), DIMENSION(:)    , ALLOCATABLE :: WSAVD

!-----------------------------------------------------------------------
!                      End of variable definition
!-----------------------------------------------------------------------

   IF (LTRACE) CALL STRACE (IENT,'SWCOMP')

   THREAD_COUNT = 1
!$ THREAD_COUNT = OMP_GET_MAX_THREADS()
   CALL THREAD_WORKSPACES%ENSURE_STRUCTURED(THREAD_COUNT)

   IF (IT .EQ. 1 .AND. ITEST.GE.1) CALL SWPRSET (SPCSIG,SPCDIR,&
   &TRIADS%collinear)

!     *** print test points ***

   IF (NPTST.GT.0) THEN
      do II = 1, NPTST
         WRITE(PRINTF,"(' Test points :',3I5)") II, XYTST(2*II-1)+MXF-2, XYTST(2*II)+MYF-2
      end do
   ENDIF

!     *** prepare ranges of spectral space, constants and      ***
!     *** weight factors for nonlinear 4 wave interactions     ***
!
!TIMG   CALL SWTSTA(135)
   IF ( IQUAD.EQ.4 ) THEN
!        --- cache the MDIA coefficients once and set the widest
!            spectral range over all quadruplets
      CALL SWPRE4W (XIS   ,SNLC1 ,&
      &DAL1  ,DAL2  ,DAL3  ,SPCSIG,&
      &WWINT ,WWAWG ,WWSWG, SNL4 )
   ELSE IF ( IQUAD.GE.1 ) THEN
      CALL FAC4WW (XIS   ,SNLC1 ,&
      &DAL1  ,DAL2  ,DAL3  ,SPCSIG,&
      &WWINT ,WWAWG ,WWSWG, SNL4 )
   ENDIF
!TIMG   CALL SWTSTO(135)
!
!     --- store frequency- and space-dependent data for triads
!TIMG   CALL SWTSTA(134)
   IF ( ITRIAD.GT.0 ) THEN
      IF (IT.EQ.1 .OR. DYNDEP) CALL FAC3WW (COMPDA(1,JDP2), SPCSIG,&
      &TRIADS)
   ENDIF
!TIMG   CALL SWTSTO(134)
!
! *** Indexing and bounds for SWMAT arrays

   JMATD = 1
   JMATR = 2
   JMATL = 3
   JMATU = 4
   JMAT5 = 5
   JMAT6 = 6
   JDIS0 = 7
   JDIS1 = JDIS0+MDISP
   JGEN0 = JDIS1+MDISP
   JGEN1 = JGEN0+MGENR
   JRED0 = JGEN1+MGENR
   JRED1 = JRED0+MREDS
   JTRA0 = JRED1+MREDS
   JTRA1 = JTRA0+MTRNP
   JAOLD = JTRA1+MTRNP
   JLEK1 = JAOLD+1
   MSWMATR = JLEK1
   JABIN = 1
   JABLK = 2
   MLSWMAT = 2

! *** Stencil size

   IF (PROPSC.EQ.3) THEN
      ICMAX  = 13
!WFR      LSTCP  = 3
   ELSE IF (PROPSC.EQ.2) THEN
      ICMAX  = 7
!WFR      LSTCP  = 2
   ELSE
      ICMAX  = 5
!WFR      LSTCP  = 1
   ENDIF

!TIMG   CALL SWTSTA(101)
!
!----------------------------------------------------------------------
!     Begin allocate shared arrays.
!----------------------------------------------------------------------

   ALLOCATE(HSAC1(MCGRD))
   ALLOCATE(HSAC2(MCGRD))
   ALLOCATE(SACC1(MCGRD))
   ALLOCATE(SACC2(MCGRD))
   ALLOCATE(HSAC0(MCGRD))
   ALLOCATE(HSDIFC(MCGRD))
   ALLOCATE(SACC0(MCGRD))
   ALLOCATE(TMDIFC(MCGRD))

   ALLOCATE(ISLMIN(MCGRD))
   ALLOCATE(NFLIM(MCGRD))
   ALLOCATE(NRSCAL(MCGRD))

   IF ( IQUAD .GE. 1) THEN
!       *** quadruplets ***
      IF ( IQUAD .GE. 3 ) THEN
!         *** prior to every iteration full directional domain ***
         ALLOCATE(MEMNL4(MDC,MSC,MCGRD),STAT=ISTAT)
         IF ( ISTAT.NE.0 ) THEN
            CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
            CALL TXPBLA(CHARS(1),IF1,IL1)
            MSGSTR =&
            &'Allocation problem: array MEMNL4 and return code is '//&
            &CHARS(1)(IF1:IL1)
            CALL MSGERR ( 4, MSGSTR )
            RETURN
         END IF
      ELSE
!       *** iquad < 3 ***
         ALLOCATE(MEMNL4(0,0,0))
      END IF
   ELSE
!       *** no quadruplets ***
      ALLOCATE(MEMNL4(0,0,0))
   ENDIF

   IF ( IBRAG.EQ.3 ) THEN
!        *** prior to every iteration full directional domain ***
      ISTAT=0
      ALLOCATE(MEMBRG(MDC,MSC,MCGRD),STAT=ISTAT)
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array MEMBRG and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF
   ELSE
      ALLOCATE(MEMBRG(0,0,0))
   END IF

   IF ( IQCM.GT.0 ) THEN
!        *** prior to every iteration full directional domain ***
      ISTAT=0
      ALLOCATE(MEMQCM(MDC,MSC,MCGRD),STAT=ISTAT)
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array MEMQCM and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF
   ELSE
      ALLOCATE(MEMQCM(0,0,0))
   END IF

   IF ( ISURF.GT.0 .AND. IGEN.EQ.4 ) THEN
!        *** prior to every iteration full directional domain ***
      ISTAT=0
      ALLOCATE(MEMQCB(MDC,MSC,MCGRD),STAT=ISTAT)
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array MEMQCB and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF
   ELSE
      ALLOCATE(MEMQCB(0,0,0))
   END IF

   IF ( IWIND.EQ.8 ) THEN
!        *** prior to every iteration full directional domain ***
      ISTAT=0
      ALLOCATE(MEMSINA(MDC,MSC,MCGRD),STAT=ISTAT)
      IF (ISTAT.EQ.0) ALLOCATE(MEMSINB(MDC,MSC,MCGRD),STAT=ISTAT)
      IF ( ISTAT.NE.0 ) THEN
         CHARS(1) = NUMSTR(ISTAT,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR =&
         &'Allocation problem: array MEMSIN and return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
         RETURN
      END IF
   ELSE
      ALLOCATE(MEMSINA(0,0,0))
      ALLOCATE(MEMSINB(0,0,0))
   END IF

!     *** Lock array for thread management
!$ ALLOCATE(LLOCK(MXC,MYC))
!
!MPI   ALLOCATE(AC2LOC(MCGRD))
!MPI
   ALLOCATE(SWTSDA(MDC,MSC,NPTSTA,MTSVAR))
   SWTSDA = 0.

!     *** In case of SETUP expand array for setup data ***

   IF (LSETUP.GT.0) THEN
      MSTPDA = 23
      ALLOCATE(SETPDA(MCGRD,MSTPDA))
   ELSE
      ALLOCATE(SETPDA(0,0))
   END IF

!----------------------------------------------------------------------
!     End allocate shared arrays.
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!     Begin initialization of shared arrays.
!----------------------------------------------------------------------

   HSAC1 = 99999.
   HSAC2 = 0.
   SACC1 = 0.
   SACC2 = 0.
   HSAC0 = 0.
   HSDIFC= 0.
   SACC0 = 0.
   TMDIFC= 0.
   IF ( IQUAD.GE.3 ) MEMNL4 = 0.
   IF ( IBRAG.EQ.3 ) MEMBRG = 0.
   IF ( IQCM .GT.0 ) MEMQCM = 0.
   IF ( ISURF.GT.0 .AND. IGEN.EQ.4 ) MEMQCB = 0.
   IF ( IWIND.EQ.8 ) MEMSINA = 0.
   IF ( IWIND.EQ.8 ) MEMSINB = 0.
   IF ( LSETUP.GT.0 ) SETPDA = 0.

!     Wave number and group velocity only depend on stationary input
!     fields.  Build a read-only grid cache before entering the OpenMP
!     region; non-stationary computations keep the original code path.

   CALL prop_cache_reset()
   IF ( NSTATC.EQ.0 .AND. .NOT.DYNDEP ) THEN
      CALL SWAPRE ( COMPDA(1,JDP2), COMPDA(1,JMUDL2), SPCSIG )
   ENDIF

!----------------------------------------------------------------------
!     End initialization shared arrays.
!----------------------------------------------------------------------
!TIMG
!TIMG   CALL SWTSTO(101)
!
!----------------------------------------------------------------------
!     Begin parallel region.
!----------------------------------------------------------------------
!
!$OMP PARALLEL DEFAULT(SHARED) &
!$OMP& PRIVATE(ITER, SWPDIR, IX, IY, II, IJ, IK, THREAD_INDEX) &
!$OMP& PRIVATE(CAX, CAY, CAX1, CAY1, CAS, CAD, CGO, KWAVE, DMW) &
!$OMP& PRIVATE(SIGFT, CGFT, UXFT, UYFT, CFT, RFT, SFT, WFT, WSAVE) &
!$OMP& PRIVATE(CFD, WFD, WSAVD) &
!$OMP& PRIVATE(SWMATR, LSWMAT, ALIMW, GROWW, IDCMIN, IDCMAX) &
!$OMP& PRIVATE(ISCMIN, ISCMAX, UE, SA1, SA2, SFNL) &
!$OMP& PRIVATE(DA1C, DA1P, DA1M, DA2C, DA2P, DA2M, DSNL) &
!$OMP& PRIVATE(ANYWND, OBREDF) &
!$OMP& PRIVATE(REFLSO, INOCNT, FBD) &
!$OMP& PRIVATE(IP,IDC,ISC) &
!$OMP& PRIVATE(I1GRD,I2GRD,I1MYC,I2MYC) &
!$OMP& PRIVATE(JDUM,JJ,III) &
!$OMP& PRIVATE(IS,IE,INCI,JS,JE,INCJ,JSD,JED,JNODE,JWFRS,JWFRE) &
!$OMP& PRIVATE(QTL1,QTL2) &
!$OMP& PRIVATE(LLOCKED) &
!$OMP& COPYIN(ICMAX,CSETUP) &
!$OMP& COPYIN(COSLAT,PROPSL) &
!$OMP& COPYIN(IPTST,TESTFL) &
!$OMP& COPYIN(RDFSIN)
!
!$OMP MASTER
!  Print number of threads set by environment
!$ IF ( IT.EQ.1.AND.ITEST.GE.10 )&
!$ &WRITE(SCREEN,'(a,i2/)')&
!$ &' Number of threads during execution of parallel region = ',&
!$ &OMP_GET_NUM_THREADS()
!$OMP END MASTER
!TIMG
!TIMG   CALL SWTSTA(101)
!
   THREAD_INDEX = 1
!$ THREAD_INDEX = OMP_GET_THREAD_NUM() + 1
!
!----------------------------------------------------------------------
!     Begin allocate private arrays.
!----------------------------------------------------------------------

   ALLOCATE(CAX(MDC,MSC,MICMAX))
   ALLOCATE(CAY(MDC,MSC,MICMAX))
   ALLOCATE(CAX1(MDC,MSC,MICMAX))
   ALLOCATE(CAY1(MDC,MSC,MICMAX))
   ALLOCATE(CAS(MDC,MSC,MICMAX))
   ALLOCATE(CAD(MDC,MSC,MICMAX))
   ALLOCATE(CGO(MSC,MICMAX))
   ALLOCATE(KWAVE(MSC,MICMAX))
   ALLOCATE(DMW(MSC,MICMAX))
!     Since SWMATR has been broken up into a real array(SWMATR) and a
!     logical array(LSWMAT), the size of each array has been adjusted
!     to MSWMATR(x-2) and MLSWMAT(2) instead of the original equivalenced 40.22
!     array with a size of MSWMAT(x).
   ALLOCATE(SWMATR(MDC,MSC,MSWMATR))
   ALLOCATE(LSWMAT(MDC,MSC,MLSWMAT))
   ALLOCATE(ALIMW(MDC,MSC))
   ALLOCATE(GROWW(MDC*MSC))
   ALLOCATE(IDCMIN(MSC))
   ALLOCATE(IDCMAX(MSC))
   ALLOCATE(ISCMIN(MDC))
   ALLOCATE(ISCMAX(MDC))
!     *** quadruplets ***
   IF ( IQUAD .GE. 1) THEN
      ALLOCATE(UE(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
      ALLOCATE(SA1(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
      ALLOCATE(SA2(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
      ALLOCATE(SFNL(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
      IF ( IQUAD .EQ. 1 ) THEN
!         *** semi-implicit calculation ***
         ALLOCATE(DA1C(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DA1P(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DA1M(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DA2C(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DA2P(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DA2M(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
         ALLOCATE(DSNL(MSC4MI:MSC4MA,MDC4MI:MDC4MA))
      ELSE
!       *** iquad > 1 ***
         ALLOCATE(DA1C(0,0))
         ALLOCATE(DA1P(0,0))
         ALLOCATE(DA1M(0,0))
         ALLOCATE(DA2C(0,0))
         ALLOCATE(DA2P(0,0))
         ALLOCATE(DA2M(0,0))
         ALLOCATE(DSNL(0,0))
      END IF
   ELSE
!       *** no quadruplets ***
      ALLOCATE(UE(0,0))
      ALLOCATE(SA1(0,0))
      ALLOCATE(SA2(0,0))
      ALLOCATE(SFNL(0,0))
      ALLOCATE(DA1C(0,0))
      ALLOCATE(DA1P(0,0))
      ALLOCATE(DA1M(0,0))
      ALLOCATE(DA2C(0,0))
      ALLOCATE(DA2P(0,0))
      ALLOCATE(DA2M(0,0))
      ALLOCATE(DSNL(0,0))
   END IF
!     *** triads ***
   IF ( ITRIAD.GT.0 ) THEN
      IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
         ALLOCATE(QTL1(  0,0))
         ALLOCATE(QTL2(MSC,2))
      ELSE IF (ITRIAD.EQ.2 .OR. ITRIAD.EQ.3) THEN
         ALLOCATE(QTL1(TRIADS%frequency_dimension,2))
         ALLOCATE(QTL2(TRIADS%frequency_dimension,4))
      ELSE IF (ITRIAD.EQ.5) THEN
         ALLOCATE(QTL1(TRIADS%frequency_dimension,2))
         ALLOCATE(QTL2(TRIADS%frequency_dimension,2))
      ENDIF
   ELSE
!        *** no triads ***
      ALLOCATE(QTL1(0,0))
      ALLOCATE(QTL2(0,0))
   ENDIF

!     *** for wind, indicating a bin inside the wind region     ***

   ALLOCATE(ANYWND(MDC))

!     *** for obstacles, to store the transmission coefficients ***
!     *** and contribution to the source terms                  ***

   ALLOCATE(OBREDF(MDC,MSC,2))
   ALLOCATE(REFLSO(MDC,MSC))

!     *** for Bragg scattering, bottom spectrum needs to be stored

   IF (IBRAG.GT.1) THEN
      ALLOCATE(FBD(MDC,MDC,MSC))
   ELSE
      ALLOCATE(FBD(0,0,0))
   ENDIF

!     *** arrays for QC scattering

   IF ( IQCM.GT.0 ) THEN

      ! allocate work arrays for FFT

      ALLOCATE(RFT(NCOZ,NCOZ))
      ALLOCATE(SFT(NCOZ,NCOZ))
      ALLOCATE(CFT(NCOZ,NCOZ))

      ALLOCATE(WFT  (LENWFT))
      ALLOCATE(WSAVE(LENSAV))

      ! initialization FFT

      CALL CFFT2I ( NCOZ, NCOZ, WSAVE, LENSAV, IERR )
      IF ( IERR /= 0 ) THEN
         CHARS(1) = NUMSTR(IERR,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR = 'something went wrong with the FFT'//&
         &' initialization - return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
      ENDIF

      ! allocate Fourier-transformed modulations of
      ! relative frequency, group velocity and ambient current

      ALLOCATE(SIGFT(NCOZ,NCOZ,MSC))
      ALLOCATE(CGFT (NCOZ,NCOZ,MSC))
      ALLOCATE(UXFT (NCOZ,NCOZ)    )
      ALLOCATE(UYFT (NCOZ,NCOZ)    )
   ELSE
      ALLOCATE(RFT  (0,0))
      ALLOCATE(SFT  (0,0))
      ALLOCATE(CFT  (0,0))
      ALLOCATE(WFT  (0))
      ALLOCATE(WSAVE(0))
      ALLOCATE(SIGFT(0,0,0))
      ALLOCATE(CGFT (0,0,0))
      ALLOCATE(UXFT (0,0))
      ALLOCATE(UYFT (0,0))
   ENDIF

!     *** arrays for QC surf breaking

   IF ( ISURF.GT.0 .AND. IGEN.EQ.4 ) THEN

      ! allocate work arrays for FFT

      ALLOCATE(CFD(MYD,MXD))

      ALLOCATE(WFD  (LENWFD))
      ALLOCATE(WSAVD(LENSVD))

      ! initialization FFT

      CALL CFFT2I ( MYD, MXD, WSAVD, LENSVD, IERR )
      IF ( IERR /= 0 ) THEN
         CHARS(1) = NUMSTR(IERR,RNAN,'(I6)')
         CALL TXPBLA(CHARS(1),IF1,IL1)
         MSGSTR = 'something went wrong with the FFT'//&
         &' initialization - return code is '//&
         &CHARS(1)(IF1:IL1)
         CALL MSGERR ( 4, MSGSTR )
      ENDIF

   ELSE
      ALLOCATE(CFD  (0,0))
      ALLOCATE(WFD  (0))
      ALLOCATE(WSAVD(0))
   ENDIF

!----------------------------------------------------------------------
!     End allocate private arrays.
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!     Begin initialization of private arrays.
!----------------------------------------------------------------------

   CAX    = 0.
   CAY    = 0.
   CAX1   = 0.
   CAY1   = 0.
   CAS    = 0.
   CAD    = 0.
   CGO    = 0.
   KWAVE  = 0.
   DMW    = 0.
   SWMATR = 0.
   ALIMW  = 0.
   IF ( IQUAD.GE.1 ) THEN
      UE   = 0.
      SA1  = 0.
      SA2  = 0.
      SFNL = 0.
      IF ( IQUAD.EQ.1 ) THEN
         DA1C = 0.
         DA1P = 0.
         DA1M = 0.
         DA2C = 0.
         DA2P = 0.
         DA2M = 0.
         DSNL = 0.
      END IF
   END IF
   IF ( IQCM.GT.0 ) THEN
      SIGFT = 0.
      CGFT  = 0.
      UXFT  = (0.,0.)
      UYFT  = (0.,0.)
   ENDIF
!     *** triads ***
   IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
      QTL2 = 0.
   ELSE IF (ITRIAD.EQ.2 .OR. ITRIAD.EQ.3 .OR. ITRIAD.EQ.5) THEN
      QTL1 = 0.
      QTL2 = 0.
   ENDIF

!----------------------------------------------------------------------
!     End initialization private arrays.
!----------------------------------------------------------------------
!$OMP BARRIER
!TIMG
!TIMG   CALL SWTSTO(101)
!
! Each thread compute its own spatial grid loop bounds for MCGRD
   CALL SWMTLB(1,MCGRD,I1GRD,I2GRD)
! Each thread compute its own spatial grid loop bounds for MYC
   CALL SWMTLB(1,MYC,I1MYC,I2MYC)

!     *** initialise values for determining the accuracy that ***
!     *** has been reached                                    ***
!     *** This is done in parallel within OpenMP environment  ***
!
!TIMG   CALL SWTSTA(102)
   CALL INSAC (AC2               ,SPCSIG          ,COMPDA(1,JDP2)  ,&
   &HSAC2             ,SACC2           ,KGRPNT          ,&
   &I1MYC             ,I2MYC                            )
!TIMG   CALL SWTSTO(102)
!
!     *** To obtain a first estimate of energy density in a    ***
!     *** gridpoint considered we run the SWAN model (in case  ***
!     *** of active wind) in a second generation mode first.   ***
!     *** After 1st iteration, the options, as defined by the  ***
!     *** user, are re-activated.                              ***
!     *** This first guess is not used in nonstationary        ***
!     *** computations (NSTATC>0), or if a restart file was    ***
!     *** used (ICOND=4)                                       ***
!
!$OMP MASTER
   IF ( IWIND.GE.3 .AND. NSTATC.EQ.0 .AND. ICOND.NE.4 ) THEN
!     --- first guess will be used
      PRECOR = .TRUE.
   ELSE
      PRECOR = .FALSE.
   END IF
!$OMP END MASTER
!
!     *** call initialization procedure of XNL to create *.BQF ***
!     *** interaction files                                    ***
!
!TIMG   CALL SWTSTA(135)
!$OMP MASTER
   IF (IQUAD.EQ.51.OR.IQUAD.EQ.52.OR.IQUAD.EQ.53) THEN
      CALL init_constants
      IXQUAD = IQUAD - 50
      IF (IAMMASTER) THEN
         WRITE(SCREEN,*) 'GurboQuad initialization'
         WRITE(SCREEN,*) 'gravity               :', GRAV
         WRITE(SCREEN,*) 'pftail                :', PWTAIL(1)
         WRITE(SCREEN,*) 'number of sigma values:', MSC
         WRITE(SCREEN,*) 'number of directions  :', MDC
         WRITE(SCREEN,*) 'IQ_QUAD               :', IXQUAD
      END IF

      IXGRID = 3
      CALL xnl_init(SPCSIG  , SPCDIR(:,1)*180./PI , MSC , MDC ,&
      &-PWTAIL(1), GRAV   , COMPDA(2,JDP2) ,&
      &MCGRD-1   , IXQUAD , IXGRID   ,INODE,IQERR )
   END IF
!$OMP END MASTER
!TIMG   CALL SWTSTO(135)
!
!TIMG   CALL SWTSTA(103)
   iteration_loop: do ITER = 1, ITERMX

!       initialise local (thread private) counter for SIP solver
      INOCNT = 0

!       initialise propagation, generation, dissipation, redistribution,  40.85
!       leak and radiation stress for each iteration
!       this is done in parallel within OpenMP environment

      DO IP = I1GRD,I2GRD
         COMPDA(IP,JDISS) = 0.
         COMPDA(IP,JLEAK) = 0.
         COMPDA(IP,JDSXB) = 0.
         COMPDA(IP,JDSXS) = 0.
         COMPDA(IP,JDSXW) = 0.
         COMPDA(IP,JDSXM) = 0.
         COMPDA(IP,JDSXV) = 0.
         COMPDA(IP,JDSXI) = 0.
         COMPDA(IP,JDSXT) = 0.
         COMPDA(IP,JDSXL) = 0.
         COMPDA(IP,JGENR) = 0.
         COMPDA(IP,JGSXW) = 0.
         COMPDA(IP,JREDS) = 0.
         COMPDA(IP,JRSXQ) = 0.
         COMPDA(IP,JRSXT) = 0.
         COMPDA(IP,JRSXB) = 0.
         COMPDA(IP,JRSXC) = 0.
         COMPDA(IP,JTRAN) = 0.
         COMPDA(IP,JTSXG) = 0.
         COMPDA(IP,JTSXT) = 0.
         COMPDA(IP,JTSXS) = 0.
         COMPDA(IP,JRADS) = 0.
         COMPDA(IP,JQB  ) = 0.
!ESMF         IF (SAVE_SINBAC) SINBAC(:,:,IP) = 0.
      ENDDO

!       initialise Ursell number and biphase to 0 for each iteration
!       this is done in parallel within OpenMP environment

      IF (ITRIAD.GT.0&
      &.OR. ISURF.EQ.7&
      &) THEN
         DO IP = I1GRD,I2GRD
            COMPDA(IP,JURSEL) = 0.
            COMPDA(IP,JBIPH ) = 0.
         ENDDO
      ENDIF

!       *** IQUAD = 3: the nonlinear wave interactions are     ***
!       *** calculated just once for an iteration. First,      ***
!       *** set the auxiliary array equal zero before a        ***
!       *** new iteration                                      ***
!       *** This is done in parallel within OpenMP environment ***

      IF ( IQUAD .GE. 3 ) THEN
         DO IP = I1GRD,I2GRD
            DO ISC = 1,MSC
               DO IDC = 1,MDC
                  MEMNL4(IDC,ISC,IP)=0.
               END DO
            END DO
         END DO
      END IF

      IF ( IBRAG .EQ. 3 ) THEN
         DO IP = I1GRD,I2GRD
            DO ISC = 1,MSC
               DO IDC = 1,MDC
                  MEMBRG(IDC,ISC,IP)=0.
               END DO
            END DO
         END DO
      END IF

      IF ( IQCM .GT. 0 ) THEN
         DO IP = I1GRD,I2GRD
            DO ISC = 1,MSC
               DO IDC = 1,MDC
                  MEMQCM(IDC,ISC,IP)=0.
               END DO
            END DO
         END DO
      END IF

      IF ( ISURF.GT.0 .AND. IGEN.EQ.4 ) THEN
         DO IP = I1GRD,I2GRD
            DO ISC = 1,MSC
               DO IDC = 1,MDC
                  MEMQCB(IDC,ISC,IP)=0.
               END DO
            END DO
         END DO
      END IF

      IF ( IWIND .EQ. 8 ) THEN
         DO IP = I1GRD,I2GRD
            DO ISC = 1,MSC
               DO IDC = 1,MDC
                  MEMSINA(IDC,ISC,IP)=0.
                  MEMSINB(IDC,ISC,IP)=0.
               END DO
            END DO
         END DO
      END IF

!----------------------------------------------------------------------
!     Begin master thread region.
!----------------------------------------------------------------------
!$OMP MASTER
!
!       *** If a current is present and a penta-diagonal solver    ***
!       *** is employed, it is possible that the solver does       ***
!       *** not converged. For this, the counter INOCNV represents ***
!       *** the number of geographical points in which the solver  ***
!       *** did not converged                                      ***

      INOCNV = 0

      IF ( ITEST.GE.30 .OR. IDEBUG.EQ.1 ) THEN
         ISLMIN(:) = 9999
         NFLIM (:) = 0
         NRSCAL(:) = 0
         IARR = 0
         ARR  = 0.
      END IF

      IF ( PRECOR ) THEN
!           *** third generation wave input ***
         IF ( ITER .EQ. 1 )THEN
!             *** save settings of 3rd generation model             ***
!             *** bottom friction, surf breaking and triads may     ***
!             *** still active                                      ***
            KWIND  = IWIND
            KWCAP  = IWCAP
            KQUAD  = IQUAD
!             ***  save maximum change per bin and under-relaxation ***
            GRWOLD = PNUMS(20)
            ALFAT  = PNUMS(30)

!             first guess settings
!
!             if 1st generation is to be used as first guess, replace
!             the next statement by IWIND = 1
            IWIND  = 2
            IWCAP  = 0
            IQUAD  = 0
            PNUMS(20) = 1.E22
!             ***  under-relaxation parameter is PNUMS(30) and
!                  temporarily set to zero
            PNUMS(30) = 0.
         ELSE IF ( ITER .EQ. 2 ) THEN
            IWIND  = KWIND
            IWCAP  = KWCAP
            IQUAD  = KQUAD
            PNUMS(20) = GRWOLD
            PNUMS(30) = ALFAT
         ENDIF

      ENDIF

      IF ( PRECOR .AND. ITER .LE. 2 ) THEN
         WRITE(PRINTF,*)' -----------------------------------------',&
         &'----------------------'
         IF ( ITER .EQ. 1 ) THEN
            WRITE(PRINTF,*) ' First guess by 2nd generation model',&
            &' flags for first iteration:'
         ELSE IF ( ITER .EQ. 2 ) THEN
            WRITE(PRINTF,*) ' Options given by user are activated',&
            &' for proceeding calculation:'
         ENDIF
         WRITE(PRINTF,"(' ITER ',I4,' GRWMX ',E12.4,' ALFA ', E12.4)") ITER, PNUMS(20), PNUMS(30)
         WRITE(PRINTF,"(' IWIND ',I4,' IWCAP ',I4,' IQUAD ',I4)") IWIND, IWCAP, IQUAD
         WRITE(PRINTF,"(' ITRIAD ',I4,' IBOT ',I4,' ISURF ',I4)") ITRIAD, IBOT , ISURF
         WRITE(PRINTF,"(' IVEG ',I4,' ITURBV ',I4,' IMUD ',I4, ' IBRAG ',I4)") IVEG, ITURBV, IMUD, IBRAG
         WRITE(PRINTF,"(' IICE ',I4,' ICEWIND ',F5.2)") IICE, ICEWIND
         WRITE(PRINTF,*)' -----------------------------------------',&
         &'----------------------'
      ENDIF

!       --- calculate diffraction parameter and its derivatives
!TIMG      CALL SWTSTA(137)
      IF ( IDIFFR.GT.0 )&
      &CALL DIFPAR( AC2   , SPCSIG, KGRPNT, COMPDA(1,JDP2), DIFFR ,&
      &CROSS , XCGRID, YCGRID, XYTST  )
!TIMG      CALL SWTSTO(137)
!
!       --- spatially filter the De Wit's biphase to prevent abrupt changes
      IF (IBIPH.EQ.3) CALL SWBIPM ( COMPDA(1,JBIPH ), COMPDA(1,JDP2),&
      &COMPDA(1,JHSIBC), TRIADS%biphase_unfiltered )

!----------------------------------------------------------------------
!     End master thread region.
!----------------------------------------------------------------------
!$OMP END MASTER
!
!----------------------------------------------------------------------
!     Synchronize threads before loop over sweep directions.
!----------------------------------------------------------------------
!$OMP BARRIER
!
!               *** START ITERATION PROCESS WITH 4 SWEEPS ***
!
!       *** loop over sweep directions ***
!
!WFR      DO SWPDIR = 1, 4
!JAC         DO ISWP = 0, 3
!JAC
!JAC! ======================================================================
!JAC!
!JAC!           Determine sequence of sweeps depending on
!JAC!           color of subdomain
!JAC!
!JAC! ======================================================================
!JAC
!JAC            IF ( IBCOL.EQ.IRED ) THEN
!JAC               SWPDIR = MOD(ISWP  ,4)+1
!JAC            ELSE IF ( IBCOL.EQ.IYELOW ) THEN
!JAC               SWPDIR = MOD(ISWP+1,4)+1
!JAC            ELSE IF ( IBCOL.EQ.IGREEN ) THEN
!JAC               SWPDIR = MOD(ISWP+2,4)+1
!JAC            ELSE
!JAC               SWPDIR = MOD(ISWP+3,4)+1
!JAC            END IF
!
!           Initialize LLOCK in parallel
!           Make .FALSE. at grid points where depth is negative
!$          DO IY = I1MYC,I2MYC
!$             DO IX = 1, MXC
!$                IF (COMPDA(KGRPNT(IX,IY),JDP2).GE.DEPMIN) THEN
!$                   LLOCK(IX,IY) = .TRUE.
!$                ELSE
!$                   LLOCK(IX,IY) = .FALSE.
!$                ENDIF
!$             ENDDO
!$          ENDDO
!
!----------------------------------------------------------------------
!     Synchronize threads before setting LLOCK for boundary.
!----------------------------------------------------------------------
!$OMP BARRIER
!
!----------------------------------------------------------------------
!     Begin master thread region.
!----------------------------------------------------------------------
!$OMP MASTER
!
!           make LLOCK False for points on boundary
            IF (SWPDIR.EQ.1) THEN
               KSX = -1
               KSY = -1
               DDX = +DX
               DDY = +DY
               IF (KREPTX.EQ.0) THEN
                  IX1 = 2
                  IF (.NOT.LMXF) IX1 = IX1-1+IHALOX
!$                LLOCK(IX1-1,:) = .FALSE.
               ELSE
                  IX1 = 1
               ENDIF
               IX2 = MXC
               IY1 = 2
               IY2 = MYC
               IF (.NOT.LMXL) IX2 = IX2-IHALOX
               IF (.NOT.LMYF) IY1 = IY1-1+IHALOY
               IF (.NOT.LMYL) IY2 = IY2-IHALOY
!$             LLOCK(:,IY1-1) = .FALSE.
            ELSE IF (SWPDIR.EQ.2) THEN
               KSX = +1
               KSY = -1
               DDX = -DX
               DDY = +DY
               IF (KREPTX.EQ.0) THEN
                  IX1 = MXC-1
                  IF (.NOT.LMXL) IX1 = IX1+1-IHALOX
!$                LLOCK(IX1+1,:) = .FALSE.
               ELSE
                  IX1 = MXC
               ENDIF
               IX2 = 1
               IY1 = 2
               IY2 = MYC
               IF (.NOT.LMXF) IX2 = IX2+IHALOX
               IF (.NOT.LMYF) IY1 = IY1-1+IHALOY
               IF (.NOT.LMYL) IY2 = IY2-IHALOY
!$             LLOCK(:,IY1-1) = .FALSE.
            ELSE IF (SWPDIR.EQ.3) THEN
               KSX = +1
               KSY = +1
               DDX = -DX
               DDY = -DY
               IF (KREPTX.EQ.0) THEN
                  IX1 = MXC-1
                  IF (.NOT.LMXL) IX1 = IX1+1-IHALOX
!$                LLOCK(IX1+1,:) = .FALSE.
               ELSE
                  IX1 = MXC
               ENDIF
               IX2 = 1
               IY1 = MYC-1
               IY2 = 1
               IF (.NOT.LMXF) IX2 = IX2+IHALOX
               IF (.NOT.LMYL) IY1 = IY1+1-IHALOY
               IF (.NOT.LMYF) IY2 = IY2+IHALOY
!$             LLOCK(:,IY1+1) = .FALSE.
            ELSE IF (SWPDIR.EQ.4) THEN
               KSX = -1
               KSY = +1
               DDX = +DX
               DDY = -DY
               IF (KREPTX.EQ.0) THEN
                  IX1 = 2
                  IF (.NOT.LMXF) IX1 = IX1-1+IHALOX
!$                LLOCK(IX1-1,:) = .FALSE.
               ELSE
                  IX1 = 1
               ENDIF
               IX2 = MXC
               IY1 = MYC-1
               IY2 = 1
               IF (.NOT.LMXL) IX2 = IX2-IHALOX
               IF (.NOT.LMYL) IY1 = IY1+1-IHALOY
               IF (.NOT.LMYF) IY2 = IY2+IHALOY
!$             LLOCK(:,IY1+1) = .FALSE.
            ENDIF

            IYSTEP = KSY

!           *** change values of variables for one-dimensional run ***

            IF ( ONED ) THEN
               IY1    = 1
               IY2    = 1
               KSY    = 0
               IYSTEP = 1
            ENDIF

            IF (SCREEN.NE.PRINTF) THEN
               IF (NSTATC.EQ.1) THEN
                  IF (IAMMASTER) WRITE(SCREEN,"('+time ', A18, ', step ',I6, '; iteration ' ,I4, '; sweep ',I1)") CHTIME, IT,&
                  &ITER, SWPDIR
               ELSE
                  WRITE(PRINTF,"(' iteration ', I4, '; sweep ', I1)") ITER, SWPDIR
                  IF (IAMMASTER) THEN
                     IF (SWPDIR.EQ.1) THEN
                        WRITE(SCREEN,"(' iteration ', I4, '; sweep ', I1)") ITER, SWPDIR
                     ELSE
                        WRITE(SCREEN,"('+iteration ', I4, '; sweep ', I1)") ITER, SWPDIR
                     END IF
                  END IF
               ENDIF
            ENDIF

!----------------------------------------------------------------------
!     End master thread region and synchronize threads
!----------------------------------------------------------------------
!$OMP END MASTER
!$OMP BARRIER
!$OMP FLUSH
!
!WFR! ======================================================================
!WFR!
!WFR!           Set up start and end indices for respectively
!WFR!           IX- and IY-loops appropriated for block
!WFR!           wavefront approach within distributed-memory
!WFR!           environment
!WFR!
!WFR! ======================================================================
!WFR
!WFR            IF ( MXCGL.GT.MYCGL .OR. .NOT.PARLL ) THEN
!WFR               JS   =  IY1
!WFR               JE   =  IY2
!WFR               INCJ = -IYSTEP
!WFR               IS   =  IX1
!WFR               IE   =  IX2
!WFR               INCI = -KSX
!WFR               IF (SWPDIR.EQ.1) THEN
!WFR                  JSD   = JE - 1
!WFR                  JED   = JS - 1
!WFR                  JNODE = INODE
!WFR                  JWFRS = 0
!WFR                  JWFRE = NPROC-1
!WFR               ELSE IF (SWPDIR.EQ.2) THEN
!WFR                  JSD   = JE - 1
!WFR                  JED   = JS - 1
!WFR                  JNODE = NPROC+1-INODE
!WFR                  JWFRS = 0
!WFR                  JWFRE = NPROC-1
!WFR               ELSE IF (SWPDIR.EQ.3) THEN
!WFR                  JSD   = JS - 1
!WFR                  JED   = JE - 1
!WFR                  JNODE = NPROC+1-INODE
!WFR                  JWFRS = NPROC-1
!WFR                  JWFRE = 0
!WFR               ELSE IF (SWPDIR.EQ.4) THEN
!WFR                  JSD   = JS - 1
!WFR                  JED   = JE - 1
!WFR                  JNODE = INODE
!WFR                  JWFRS = NPROC-1
!WFR                  JWFRE = 0
!WFR               END IF
!WFR            ELSE
!WFR               JS   =  IX1
!WFR               JE   =  IX2
!WFR               INCJ = -KSX
!WFR               IS   =  IY1
!WFR               IE   =  IY2
!WFR               INCI = -IYSTEP
!WFR               IF (SWPDIR.EQ.1) THEN
!WFR                  JSD   = JE - 1
!WFR                  JED   = JS - 1
!WFR                  JNODE = INODE
!WFR                  JWFRS = 0
!WFR                  JWFRE = NPROC-1
!WFR               ELSE IF (SWPDIR.EQ.2) THEN
!WFR                  JSD   = JS - 1
!WFR                  JED   = JE - 1
!WFR                  JNODE = INODE
!WFR                  JWFRS = NPROC-1
!WFR                  JWFRE = 0
!WFR               ELSE IF (SWPDIR.EQ.3) THEN
!WFR                  JSD   = JS - 1
!WFR                  JED   = JE - 1
!WFR                  JNODE = NPROC+1-INODE
!WFR                  JWFRS = NPROC-1
!WFR                  JWFRE = 0
!WFR               ELSE IF (SWPDIR.EQ.4) THEN
!WFR                  JSD   = JE - 1
!WFR                  JED   = JS - 1
!WFR                  JNODE = NPROC+1-INODE
!WFR                  JWFRS = 0
!WFR                  JWFRE = NPROC-1
!WFR               END IF
!WFR            END IF
!
!----------------------------------------------------------------------
!     Execute loop over rows of spatial grid in a
!     pipelined parallel manner within OpenMP environment
!----------------------------------------------------------------------
!$OMP DO SCHEDULE(STATIC,1) &
!$OMP& FIRSTPRIVATE(WWINT) &
!$OMP& LASTPRIVATE(WWINT)
!
!JAC            DO IY = IY1, IY2, -IYSTEP
!JAC               DO IX = IX1, IX2, -KSX
!WFR! ======================================================================
!WFR!
!WFR!           Within distributed-memory environment, current
!WFR!           sweep is carry out in a block wavefront manner
!WFR!
!WFR! ======================================================================
!WFR
!WFR                  DO JDUM = JS+JWFRS, JE+JWFRE, INCJ
!WFR
!WFR                     JJ = JDUM-JNODE+1
!WFR                     IF (JNODE.GE.JDUM-JSD .AND. JNODE.LE.JDUM-JED) THEN
!WFR
!WFR!MPI! ======================================================================
!WFR!MPI!
!WFR!MPI!             Receive action density from previous updated
!WFR!MPI!             row within distributed-memory environment
!WFR!MPI!
!WFR!MPI! ======================================================================
!WFR!MPI
!WFR!TIMG!MPI                        CALL SWTSTA(213)
!WFR!MPI                        IF ( MXCGL.GT.MYCGL ) THEN
!WFR!MPI                           DO III = LSTCP, 1, -1
!WFR!MPI                              CALL SWRECVAC(AC2,IS-III*INCI,JJ,SWPDIR,KGRPNT)
!WFR!MPI                           END DO
!WFR!MPI                        ELSE
!WFR!MPI                           DO III = LSTCP, 1, -1
!WFR!MPI                              CALL SWRECVAC(AC2,JJ,IS-III*INCI,SWPDIR,KGRPNT)
!WFR!MPI                           END DO
!WFR!MPI                        END IF
!WFR!TIMG!MPI                        CALL SWTSTO(213)
!WFR!MPI                        IF (STPNOW()) RETURN
!WFR
!WFR                        DO II = IS, IE, INCI
!WFR                           IF ( MXCGL.GT.MYCGL .OR. .NOT.PARLL ) THEN
!WFR                              IX = II
!WFR                              IY = JJ
!WFR                           ELSE
!WFR                              IX = JJ
!WFR                              IY = II
!WFR                           END IF
!
!----------------------------------------------------------------------
!               Wait until the upwind row is available.  A scalar atomic
!               read/write pair publishes LLOCK without flushing the
!               complete lock array; TASKYIELD lets another ready thread
!               progress while this pipeline stage is blocked.
!----------------------------------------------------------------------
!$                         IF ( .NOT.ONED ) THEN
!$                            LLOCKED = .TRUE.
!$                            DO WHILE(LLOCKED)
!$OMP ATOMIC READ
!$                               LLOCKED = LLOCK(IX,IY+IYSTEP)
!$                               IF (LLOCKED) THEN
!$OMP TASKYIELD
!$                               ENDIF
!$                            END DO
!$                         END IF
!
!TIMG                           CALL SWTSTA(104)
                           CALL SWOMPU (SWPDIR,KSX              ,KSY              ,&
                           &IX               ,IY               ,DDX              ,&
                           &DDY              ,default_time_context%DT               ,SNLC1            ,&
                           &DAL1             ,DAL2             ,DAL3             ,&
                           &XIS              ,SWTSDA           ,INOCNT           ,&
                           &AC2              ,COMPDA           ,SPCDIR           ,&
                           &SPCSIG           ,XYTST            ,ITER             ,&
                           &CGO              ,&
                           &CAX              ,CAY              ,CAS              ,&
                           &CAD              ,SWMATR           ,LSWMAT           ,&
                           &KWAVE            ,DMW              ,&
                           &ALIMW            ,GROWW                              ,&
                           &UE               ,SA1              ,SA2              ,&
                           &DA1C             ,DA1P             ,DA1M             ,&
                           &DA2C             ,DA2P             ,DA2M             ,&
                           &SFNL             ,DSNL             ,MEMNL4           ,&
                           &QTL1             ,QTL2             ,&
                           &IDCMIN           ,IDCMAX           ,&
                           &WWINT            ,WWAWG            ,WWSWG            ,&
                           &ISCMIN           ,ISCMAX           ,&
                           &ANYWND           ,AC1              ,IT               ,&
                           &XCGRID           ,YCGRID           ,&
                           &KGRPNT           ,CROSS            ,&
                           &OBREDF           ,REFLSO           ,&
                           &FBD              ,MEMBRG           ,&
                           &SIGFT            ,CGFT             ,UXFT             ,&
                           &UYFT             ,CFT              ,RFT              ,&
                           &SFT              ,WFT              ,WSAVE            ,&
                           &CFD              ,WFD              ,WSAVD            ,&
                           &MEMQCM           ,MEMQCB           ,&
                           &ISLMIN           ,NFLIM            ,NRSCAL           ,&
                           &MEMSINA          ,MEMSINB          ,&
                           &CAX1             ,CAY1             ,DIFFR,&
                           &TRIADS           ,SNL4             ,SPECTRAL_POWERS,&
                           &THREAD_WORKSPACES%STRUCTURED(THREAD_INDEX)%SOURCE%WCAP&
                           &)
!TIMG                           CALL SWTSTO(104)
!MPI                           IF (STPNOW()) RETURN
!
!----------------------------------------------------------------------
!               Once the computation is done for grid point (IX,IY) the
!               thread signals that the data is available by changing
!               LLOCK(IX,IY).
!----------------------------------------------------------------------
!$OMP ATOMIC WRITE
!$                         LLOCK(IX,IY) = .FALSE.

!JAC               END DO
!WFR                        END DO

!WFR!MPI! ======================================================================
!WFR!MPI!
!WFR!MPI!             Send action density to next row
!WFR!MPI!             within distributed-memory environment
!WFR!MPI!
!WFR!MPI! ======================================================================
!WFR!MPI
!WFR!TIMG!MPI                        CALL SWTSTA(213)
!WFR!MPI                        IF ( MXCGL.GT.MYCGL ) THEN
!WFR!MPI                           DO III = LSTCP-1, 0, -1
!WFR!MPI                              CALL SWSENDAC(AC2,IE-III*INCI,JJ,SWPDIR,KGRPNT)
!WFR!MPI                           END DO
!WFR!MPI                        ELSE
!WFR!MPI                           DO III = LSTCP-1, 0, -1
!WFR!MPI                              CALL SWSENDAC(AC2,JJ,IE-III*INCI,SWPDIR,KGRPNT)
!WFR!MPI                           END DO
!WFR!MPI                        END IF
!WFR!TIMG!MPI                        CALL SWTSTO(213)
!WFR!MPI                        IF (STPNOW()) RETURN
!WFR                     END IF
!WFR
!JAC            END DO
!WFR                  END DO
!$OMP ENDDO NOWAIT
!JAC
!JAC! ======================================================================
!JAC!
!JAC!           Exchange action densities at subdomain interfaces
!JAC!           within distributed-memory environment
!JAC!
!JAC! ======================================================================
!JAC
!JAC                  IF ( ISWP.EQ.3 ) THEN
!JAC!TIMG                     CALL SWTSTA(213)
!JAC                     DO ID = 1, MDC
!JAC                        DO IS = 1, MSC
!JAC                           AC2LOC(:) = AC2(ID,IS,:)
!JAC                           CALL SWEXCHG( AC2LOC, 0, KGRPNT )
!JAC!MPI                           IF (STPNOW()) RETURN
!JAC                           AC2(ID,IS,:) = AC2LOC(:)
!JAC                        END DO
!JAC                     END DO
!JAC                     CALL SWSYNC
!JAC!MPI                     IF (STPNOW()) RETURN
!JAC!TIMG                     CALL SWTSTO(213)
!JAC                  END IF
!
!----------------------------------------------------------------------
!     Synchronize threads before checking stop condition and
!     before starting next sweep direction.
!----------------------------------------------------------------------
!$OMP BARRIER
!WFR      END DO
!JAC         END DO
!WFR!MPI!
!WFR!MPI!       --- exchange action densities at subdomain interfaces
!WFR!MPI!
!WFR!TIMG!MPI                  CALL SWTSTA(213)
!WFR!MPI                  DO ID = 1, MDC
!WFR!MPI                     DO IS = 1, MSC
!WFR!MPI                        AC2LOC(:) = AC2(ID,IS,:)
!WFR!MPI                        CALL SWEXCHG( AC2LOC, KGRPNT )
!WFR!MPI                        AC2(ID,IS,:) = AC2LOC(:)
!WFR!MPI                     END DO
!WFR!MPI                  END DO
!WFR!TIMG!MPI                  CALL SWTSTO(213)
!WFR!MPI                  IF (STPNOW()) RETURN
!
!----------------------------------------------------------------------
!     Each thread sum contributions to the global INOCNV counter
!     which counts the number of grid points over the four sweeps
!     in which the SIP solver did not converge.
!----------------------------------------------------------------------
!$OMP ATOMIC
                  INOCNV = INOCNV + INOCNT

!----------------------------------------------------------------------
!     Synchronize threads before master thread stores source terms
!     and computes wave induced setup.
!----------------------------------------------------------------------
!$OMP BARRIER
!
!----------------------------------------------------------------------
!     Begin master thread region.
!----------------------------------------------------------------------
!$OMP MASTER

                  IF ( ITEST.GE.30 .OR. IDEBUG.EQ.1 ) THEN
                     NWETP = 0
                     do IP = 2, MCGRD
                        IF (COMPDA(IP,JDP2).GT.DEPMIN) NWETP = NWETP + 1
                     end do

                     MXNFL = MAXVAL(NFLIM)
                     NPFL  = COUNT(MASK=NFLIM>0)

                     MNISL = MINVAL(ISLMIN)

                     MXNFR = MAXVAL(NRSCAL)
                     NPFR  = COUNT(MASK=NRSCAL>0)

                     NVARW = 0
                     IF (IQCM.NE.0) THEN
!             --- compute Wigner variance and count negative ones
                        DO IP = 2, MCGRD
                           IF (COMPDA(IP,JDP2).GT.DEPMIN) THEN
                              VARW = 0.
                              DO IS = 1, MSC
                                 DO ID = 1, MDC
                                    VARW = VARW + AC2(ID,IS,IP)
                                 ENDDO
                              ENDDO
                              IF (VARW.LT.0.) NVARW = NVARW + 1
                           ENDIF
                        ENDDO
                     ENDIF

!MPI! ======================================================================
!MPI!
!MPI!          Gather data meant for global reductions in arrays
!MPI!          IARR and ARR within distributed-memory environment
!MPI!
!MPI! ======================================================================
!MPI!
!MPI                     IARR(1) = NWETP
!MPI                     IARR(2) = NPFL
!MPI                     IARR(3) = NPFR
!MPI                     IARR(4) = INOCNV
!MPI                     IARR(5) = NVARW
!MPI!
!MPI                     ARR(1)  = REAL(MXNFL)
!MPI                     ARR(2)  = REAL(MXNFR)
!MPI!
!MPI!          --- carry out reductions across all nodes
!MPI!
!MPI                     CALL SWREDUCE(   ARR, 4, SWMAX )
!MPI                     IF (STPNOW()) RETURN
!MPI                     CALL SWREDUCE(  IARR, 5, SWSUM )
!MPI                     IF (STPNOW()) RETURN
!MPI                     CALL SWREDUCE( MNISL, 1, SWMIN )
!MPI                     IF (STPNOW()) RETURN
!MPI!
!MPI                     NWETP     = IARR(1)
!MPI                     NPFL      = IARR(2)
!MPI                     NPFR      = IARR(3)
!MPI                     INOCNV    = IARR(4)
!MPI                     NVARW     = IARR(5)
!MPI!
!MPI                     MXNFL     = NINT(ARR(1))
!MPI                     MXNFR     = NINT(ARR(2))
!MPI!
                     FRAC = REAL(NPFL)*100./REAL(NWETP)
                     IF(NPFL.GT.0) WRITE(PRINTF,"(1X,'use of ',A9,' in ',F6.2, ' % of wet points with maximum in spectral space = ', I4)") 'limiter',FRAC,MXNFL
                     IF (NSTATC.EQ.0 .AND. NPFL.GT.0 .AND. IAMMASTER)&
                     &WRITE(SCREEN,"(1X,'use of ',A9,' in ',F6.2, ' % of wet points with maximum in spectral space = ', I4)") 'limiter',FRAC,MXNFL

                     IF (NPFL.GT.0) WRITE(PRINTF,"(1X, 'lowest frequency occured above which limiter is applied = ', F7.4,' Hz')") SPCSIG(MNISL)/PI2
                     IF (NSTATC.EQ.0 .AND. NPFL.GT.0 .AND. IAMMASTER)&
                     &WRITE(SCREEN,"(1X, 'lowest frequency occured above which limiter is applied = ', F7.4,' Hz')") SPCSIG(MNISL)/PI2

                     FRAC = REAL(NPFR)*100./REAL(NWETP)
                     IF(NPFR.GT.0) WRITE(PRINTF,"(1X,'use of ',A9,' in ',F6.2, ' % of wet points with maximum in spectral space = ', I4)") 'rescaling',FRAC,MXNFR
                     IF (NSTATC.EQ.0 .AND. NPFR.GT.0 .AND. IAMMASTER)&
                     &WRITE(SCREEN,"(1X,'use of ',A9,' in ',F6.2, ' % of wet points with maximum in spectral space = ', I4)") 'rescaling',FRAC,MXNFR

                     IF (IQCM.NE.0) THEN
                        FRAC = REAL(NVARW)*100./REAL(NWETP)
                        IF(NVARW.GT.0) WRITE(PRINTF,"(1X,'occurence of ',A17,' in ',F6.2, ' % of wet points')") 'negative variance',FRAC
                        IF (NSTATC.EQ.0 .AND. NVARW.GT.0 .AND. IAMMASTER)&
                        &WRITE(SCREEN,"(1X,'occurence of ',A17,' in ',F6.2, ' % of wet points')") 'negative variance',FRAC
                     ENDIF

                  END IF

!       *** store the source terms for test gridpoints  ***
!       *** in the files IFPAR, IFS1D and IFS2D         ***
!
!TIMG                  CALL SWTSTA(105)
                  IF (NPTST.GT.0 .AND. NSTATM.EQ.0&
                  &) THEN
                     IF (IFPAR.GT.0) WRITE (IFPAR, "(I4, T41, 'iteration')") ITER
                     IF (IFS1D.GT.0) WRITE (IFS1D, "(I4, T41, 'iteration')") ITER
                     IF (IFS2D.GT.0) WRITE (IFS2D, "(I4, T41, 'iteration')") ITER
                     CALL PLTSRC (SWTSDA(1,1,1,JPWNDS)  ,SWTSDA(1,1,1,JPWNDD)  ,&
                     &SWTSDA(1,1,1,JPWCAP)  ,SWTSDA(1,1,1,JPBTFR)  ,&
                     &SWTSDA(1,1,1,JPWBRK)  ,SWTSDA(1,1,1,JP4S)    ,&
                     &SWTSDA(1,1,1,JP4D)    ,SWTSDA(1,1,1,JPTRI)   ,&
                     &SWTSDA(1,1,1,JPVEGT)  ,SWTSDA(1,1,1,JPTURB)  ,&
                     &SWTSDA(1,1,1,JPMUD)   ,SWTSDA(1,1,1,JPICE)   ,&
                     &SWTSDA(1,1,1,JPBRAG)  ,SWTSDA(1,1,1,JPQCS)   ,&
                     &SWTSDA(1,1,1,JPSWEL)  ,&
                     &AC2                   ,SPCSIG                ,&
                     &COMPDA(1,JDP2)        ,XYTST                 ,&
                     &KGRPNT                )
                  END IF
!TIMG                  CALL SWTSTO(105)
!
!       *** compute wave-induced setup ***
!
!TIMG                  CALL SWTSTA(106)
                  IF (LSETUP.GT.0)&
                  &CALL SETUPP ( KGRPNT, MSTPDA, SETPDA, AC2, COMPDA(1,JDP2),&
                  &COMPDA(1,JDPSAV), COMPDA(1,JSETUP),&
                  &XCGRID, YCGRID, SPCSIG, SPCDIR )
!TIMG                  CALL SWTSTO(106)
!
!----------------------------------------------------------------------
!     End master thread region.
!----------------------------------------------------------------------
!$OMP END MASTER
!
!       *** check if numerical accuracy has been reached       ***
!       *** this is done in parallel within OpenMP environment ***
!
!TIMG                  CALL SWTSTA(102)
                  IF (PNUMS(21).EQ.0.) THEN
                     CALL SACCUR (COMPDA(1,JDP2),KGRPNT          ,&
                     &XYTST           ,&
                     &AC2             ,SPCSIG          ,ACCUR           ,&
                     &HSAC1           ,HSAC2           ,SACC1           ,&
                     &SACC2           ,COMPDA(1,JDHS)  ,COMPDA(1,JDTM)  ,&
                     &I1MYC           ,I2MYC                            )
                  ELSE IF (PNUMS(21).EQ.1.) THEN
                     CALL SWSTPC ( HSAC0         ,HSAC1           ,HSAC2 ,&
                     &SACC0         ,SACC1           ,SACC2 ,&
                     &HSDIFC        ,TMDIFC          ,&
                     &COMPDA(1,JDHS),COMPDA(1,JDTM)  ,&
                     &COMPDA(1,JDP2),ACCUR           ,&
                     &I1MYC         ,I2MYC           )
                  END IF
!TIMG                  CALL SWTSTO(102)
!
!----------------------------------------------------------------------
!     Begin master thread region.
!----------------------------------------------------------------------
!$OMP MASTER
!
!       *** info regarding the iteration process and the accuracy ***

                  IF (PNUMS(21).EQ.1. .AND. ITER.EQ.1) THEN
                     WRITE(PRINTF,"(' not possible to compute, first iteration',/)")
                     IF (NSTATC.EQ.0.AND.IAMMASTER) WRITE(SCREEN,"(' not possible to compute, first iteration',/)")
                  ELSE
                     WRITE(PRINTF,"(' accuracy OK in ',F6.2, ' % of wet grid points (',F6.2,' % required)',/ )") ACCUR,PNUMS(4)
                     IF (NSTATC.EQ.0.AND.IAMMASTER)&
                     &WRITE(SCREEN,"(' accuracy OK in ',F6.2, ' % of wet grid points (',F6.2,' % required)',/ )") ACCUR,PNUMS(4)
                  END IF

!       *** number of points in which the penta-diagonal solver ***
!       *** did not converged                                   ***

                  IF ( ITEST.GE.30 .OR. IDEBUG.EQ.1 ) THEN
                     IF ((DYNDEP .OR. ICUR.EQ.1) .AND. INOCNV .NE. 0) THEN
                        WRITE(PRINTF,"(2X,'SIP solver: no convergence in ',I4,' gridpoints')") INOCNV
                     END IF
                  END IF

!----------------------------------------------------------------------
!     End master thread region.
!----------------------------------------------------------------------
!$OMP END MASTER
!
!MPI!       --- exchange COMPDA at subdomain interfaces
!MPI!           within distributed-memory environment
!MPI!
!TIMG!MPI                  CALL SWTSTA(213)
!MPI                  DO J = 1, MCMVAR
!WFR!MPI                     CALL SWEXCHG( COMPDA(1,J), KGRPNT )
!JAC!MPI                     CALL SWEXCHG( COMPDA(1,J), 0, KGRPNT )
!MPI                  ENDDO
!TIMG!MPI                  CALL SWTSTO(213)
!MPI                  IF (STPNOW()) RETURN
!MPI!
!       --- store QC bulk dissipation for the next iteration
                  IF ( ISURF.GT.0 .AND. IGEN.EQ.4 ) THEN
!          --- make it global first and then scatter it to all nodes
                     CALL SWCOLLECT ( disbk0, disbk1, .FALSE. )
                     CALL SWBROADC  ( disbk0, MCGRDGL )
!MPI                     IF (STPNOW()) RETURN
                  ENDIF

!----------------------------------------------------------------------
!     Synchronize threads before checking accuracy and before
!     starting next iteration.
!----------------------------------------------------------------------
!$OMP BARRIER
!
!       --- check if breaker index for BKD needs to be modified
                  IF (MODGAM .AND. ACCUR.GE.PNUMS(37)) MODGAM = .FALSE.

!       *** if accuracy has been reached then the iteration ***
!       *** can be terminated ---> EXIT iteration_loop      ***

                  IF ( (ITER.NE.1 .OR. PNUMS(21).EQ.0.) .AND.&
                  &ACCUR.GE.PNUMS(4) ) EXIT iteration_loop

   end do iteration_loop
!TIMG                  CALL SWTSTO(103)
!
!----------------------------------------------------------------------
!     Begin deallocate private arrays.
!----------------------------------------------------------------------
!TIMG                  CALL SWTSTA(101)
                  DEALLOCATE(IDCMIN)
                  DEALLOCATE(IDCMAX)
                  DEALLOCATE(ISCMIN)
                  DEALLOCATE(ISCMAX)
                  DEALLOCATE(CGO)
                  DEALLOCATE(KWAVE)
                  DEALLOCATE(DMW)
                  DEALLOCATE(CAX)
                  DEALLOCATE(CAY)
                  DEALLOCATE(CAS)
                  DEALLOCATE(CAD)
                  DEALLOCATE(CAX1)
                  DEALLOCATE(CAY1)
                  DEALLOCATE(ALIMW)
                  DEALLOCATE(UE)
                  DEALLOCATE(SA1)
                  DEALLOCATE(SA2)
                  DEALLOCATE(SFNL)
                  DEALLOCATE(DA1C)
                  DEALLOCATE(DA1P)
                  DEALLOCATE(DA1M)
                  DEALLOCATE(DA2C)
                  DEALLOCATE(DA2P)
                  DEALLOCATE(DA2M)
                  DEALLOCATE(DSNL)
                  DEALLOCATE(QTL1)
                  DEALLOCATE(QTL2)
                  DEALLOCATE(OBREDF)
                  DEALLOCATE(REFLSO)
                  DEALLOCATE(GROWW)
                  DEALLOCATE(ANYWND)
                  DEALLOCATE(FBD)
                  DEALLOCATE(SWMATR)
                  DEALLOCATE(LSWMAT)
                  DEALLOCATE(SIGFT)
                  DEALLOCATE(CGFT)
                  DEALLOCATE(UXFT)
                  DEALLOCATE(UYFT)
                  DEALLOCATE(CFT)
                  DEALLOCATE(RFT)
                  DEALLOCATE(SFT)
                  DEALLOCATE(WFT)
                  DEALLOCATE(WSAVE)
                  DEALLOCATE(CFD)
                  DEALLOCATE(WFD)
                  DEALLOCATE(WSAVD)
!TIMG                  CALL SWTSTO(101)
!----------------------------------------------------------------------
!     End deallocate private arrays.
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!     End parallel region.
!----------------------------------------------------------------------
!$OMP END PARALLEL
                  CALL prop_cache_reset()

!     Print message when the solver did not converge in setup calculation

                  IF (.NOT.CSETUP) THEN
                     WRITE(PRINTF,"(1X,'no convergence in set-up calculation')")
                     IF (SCREEN.NE.PRINTF.AND.NSTATC.EQ.0.AND.IAMMASTER)&
                     &WRITE(SCREEN,"(1X,'no convergence in set-up calculation')")
                  END IF

!TIMG                  CALL SWTSTA(105)
                  IF (NPTST.GT.0 .AND. NSTATM.EQ.1&
                  &) THEN
                     IF (IFPAR.GT.0) WRITE (IFPAR, "(A, T41, 'date-time')") CHTIME
                     IF (IFS1D.GT.0) WRITE (IFS1D, "(A, T41, 'date-time')") CHTIME
                     IF (IFS2D.GT.0) WRITE (IFS2D, "(A, T41, 'date-time')") CHTIME

                     CALL PLTSRC (SWTSDA(1,1,1,JPWNDS)  ,SWTSDA(1,1,1,JPWNDD)  ,&
                     &SWTSDA(1,1,1,JPWCAP)  ,SWTSDA(1,1,1,JPBTFR)  ,&
                     &SWTSDA(1,1,1,JPWBRK)  ,SWTSDA(1,1,1,JP4S)    ,&
                     &SWTSDA(1,1,1,JP4D)    ,SWTSDA(1,1,1,JPTRI)   ,&
                     &SWTSDA(1,1,1,JPVEGT)  ,SWTSDA(1,1,1,JPTURB)  ,&
                     &SWTSDA(1,1,1,JPMUD)   ,SWTSDA(1,1,1,JPICE)   ,&
                     &SWTSDA(1,1,1,JPBRAG)  ,SWTSDA(1,1,1,JPQCS)   ,&
                     &SWTSDA(1,1,1,JPSWEL)  ,&
                     &AC2                   ,SPCSIG                ,&
                     &COMPDA(1,JDP2)        ,XYTST                 ,&
                     &KGRPNT                )
                  END IF
!TIMG                  CALL SWTSTO(105)
!
!----------------------------------------------------------------------
!     Begin deallocate shared arrays.
!----------------------------------------------------------------------
!TIMG                  CALL SWTSTA(101)
                  DEALLOCATE(SETPDA)
                  DEALLOCATE(HSAC1)
                  DEALLOCATE(HSAC2)
                  DEALLOCATE(SACC1)
                  DEALLOCATE(SACC2)
                  DEALLOCATE(HSAC0)
                  DEALLOCATE(HSDIFC)
                  DEALLOCATE(SACC0)
                  DEALLOCATE(TMDIFC)
                  DEALLOCATE(ISLMIN)
                  DEALLOCATE(NFLIM)
                  DEALLOCATE(NRSCAL)
                  DEALLOCATE(MEMNL4)
                  DEALLOCATE(MEMBRG)
                  DEALLOCATE(MEMQCM)
                  DEALLOCATE(MEMQCB)
                  DEALLOCATE(MEMSINA)
                  DEALLOCATE(MEMSINB)
!$                DEALLOCATE(LLOCK)
!MPI                  DEALLOCATE(AC2LOC)
                  DEALLOCATE(SWTSDA)
!TIMG                  CALL SWTSTO(101)
!----------------------------------------------------------------------
!     End deallocate shared arrays.
!----------------------------------------------------------------------

                  RETURN
               end subroutine SWCOMP

!************************************************************************

               SUBROUTINE SWOMPU (SWPDIR   ,KSX      ,KSY      ,&
               &IX       ,IY       ,DDX      ,&
               &DDY      ,DT       ,SNLC1    ,&
               &DAL1     ,DAL2     ,DAL3     ,&
               &XIS      ,SWTSDA   ,INOCNV   ,&
               &AC2      ,COMPDA   ,SPCDIR   ,&
               &SPCSIG   ,XYTST    ,ITER     ,&
               &CGO      ,&
               &CAX      ,CAY      ,CAS      ,&
               &CAD      ,SWMATR   ,LSWMAT   ,&
               &KWAVE    ,DMW      ,&
               &ALIMW    ,GROWW              ,&
               &UE       ,SA1      ,SA2      ,&
               &DA1C     ,DA1P     ,DA1M     ,&
               &DA2C     ,DA2P     ,DA2M     ,&
               &SFNL     ,DSNL     ,MEMNL4   ,&
               &QTL1     ,QTL2     ,&
               &IDCMIN   ,IDCMAX   ,&
               &WWINT    ,WWAWG    ,WWSWG    ,&
               &ISCMIN   ,ISCMAX   ,&
               &ANYWND   ,AC1      ,IT       ,&
               &XCGRID   ,YCGRID   ,&
               &KGRPNT   ,CROSS    ,&
               &OBREDF   ,REFLSO   ,&
               &FBD      ,MEMBRG   ,&
               &SIGFT    ,CGFT     ,UXFT     ,&
               &UYFT     ,CFT      ,RFT      ,&
               &SFT      ,WFT      ,WSAVE    ,&
               &CFD      ,WFD      ,WSAVD    ,&
               &MEMQCM   ,MEMQCB   ,&
               &ISLMIN   ,NFLIM    ,NRSCAL   ,&
               &MEMSINA  ,MEMSINB  ,&
               &CAX1,CAY1,DIFFR,TRIADS,SNL4,SPECTRAL_POWERS,WCAP_WORKSPACE&
               &)
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_services, ONLY: SWTRCF
   USE swan_propagation, ONLY: ADDDIS, DSPHER, SPREDT, SPROSD, SPROXY, SWAPAR, SWGEOM, SWPSEL
   USE swan_wind_source, ONLY: WINDP1, WINDP3

!************************************************************************

                  USE OCPCOMM2
                  USE OCPCOMM3
                  USE OCPCOMM4
                  USE SWCOMM1
                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE SwanQCM
                  USE M_PARALL

                  IMPLICIT NONE(TYPE, EXTERNAL)

    TYPE(diffraction_state_t), INTENT(IN) :: DIFFR
    TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
    TYPE(snl4_tables_t), INTENT(INOUT) :: SNL4
    TYPE(spectral_powers_t), INTENT(IN) :: SPECTRAL_POWERS
    TYPE(wcap_workspace_t), INTENT(INOUT) :: WCAP_WORKSPACE


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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.80: Nico Booij
!     30,81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     30.90: IJsbrand Haagsma (Equivalence version)
!     32.02: Roeland Ris & Cor van der Schelde (1D-version)
!     33.08: W. Erick Rogers (some S&L scheme-related changes)
!     33.09: Nico Booij (spherical coord.)
!     33.10: Nico Booij and Erick Rogers (2nd order upwind)
!     34.01: Jeroen Adema
!     40.00: Nico Booij
!     40.02: IJsbrand Haagsma
!     40.03, 40.13: Nico Booij
!     40.08: Erick Rogers
!     40.09: Annette Kieftenburg
!     40.16: IJsbrand Haagsma
!     40.17: IJsbrand Haagsma
!     40.22: John Cazes and Tim Campbell
!     40.23: Marcel Zijlema
!     40.28: Annette Kieftenburg
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!     40.59: W. Erick Rogers
!     40.61: Roop Lalbeharry
!     41.75: W. Erick Rogers
!     41.90: Gal Akrish, Pieter Smit and Marcel Zijlema
!
!  1. Updates
!
!     30.72, Nov. 97: Declaration of ISTAT, ITFRE, DDIR, DX, DY, GRAV,
!                     PI, U10 and WDIC removed because they are
!                     common and already declared in the INCLUDE file
!     32.02, Jan. 98: Introduced 1D-version
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.72, Feb. 98: Modified argument list for update CGSTAB solver
!     30.70, Feb. 98: argument list of WINDP1 changed, current vel. added
!     40.00, July 98: KCGRD removed from Call WINDP1
!     40.00, Aug. 98: argument OBREDF added in call SPREDT
!                     subr SWTRCF called to calculate obstacle reduction factors
!     30.90, Oct. 98: Introduced EQUIVALENCE POOL-arrays
!     30.82, Oct. 98: Updated description several variables
!     30.80, Nov. 98: Provision for limitation on Ctheta (refraction)
!     30.81, Jan. 99: Replaced variable STATUS by IERR (because STATUS is a
!                     reserved word)
!     34.01, Feb. 99: Introducing STPNOW
!     33.08  July 98: some S&L scheme-related changes
!     30.80, Aug. 99: Argument list SPROSD modified
!     40.10, Nov. 99: two arguments in call SWTRCF changed;
!                     CHS -> COMPDA(1,JHS) and WLEV2 -> COMPDA(1,JWLV2)
!     33.09, Nov. 99: call DSPHER added (ray curvature due to spherical
!     33.10, Jan. 00: changes re: the SORDUP scheme
!     40.09, May  00: Argument list SWTRCF modified
!     40.03, Jun. 00: new version of SPROSD has new argument list
!                     Ursell array added to argument list of SDISPA and
!                     readability of test output improved
!     40.10, Sep. 00: Replaced SDISPA with SINTGRL
!     40.02, Sep. 00: Replaced SWMATR(1,1,JABIN) with LSWMAT (logical equivalence)
!     40.13, Mar. 01: if point was dry at previous time level, fall back to BSBT scheme
!                     order of calling SWAPAR and SPROXY changed, in order
!                     to get correct values of CGO as input to SPROXY
!     40.22, Sep. 01: Removed WAREA constructs and split SWMATR into
!                     SWMATR(real) and LSWMAT(logical).
!     40.22, Sep. 01: Changed array definitions to use the parameter
!                     MICMAX instead of ICMAX.
!     40.13, Oct. 01: loop over IC moved to subroutines SWAPAR and SPROXY
!     40.16, Dec. 01: Implementation of limiter switches
!     40.17, Dec. 01: Implementation of Multiple DIA
!     40.28, Dec. 01: Argument list SWTRCF modified
!     40.23, Aug. 02: Print of CPU times added
!     40.23, Aug. 02: Introducing arrays NFLIM and NRSCAL
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.08, Mar. 03: Dimensioning of RDX, RDX changed to be consistent
!                     with other subroutines
!     40.41, Aug. 04: code optimization
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.59, Aug. 07: stencil numbering made consistent, so that if used for
!                     varied purposes (e.g. SPROSD), if-then not required
!     40.61, Nov. 06: Hersbach and Janssen (1999) limiter option added
!     41.75, Jan. 19: adding sea ice
!     41.90, Oct. 21: adding QC scattering
!
!  2. Purpose
!
!     This subroutine computes the wave spectrum for one sweep
!     direction, and is called four times per iteration.
!
!  3. Method
!
!
!    THIS IS THE STENCIL USED WITH THE S&L SCHEME:
!
!      IY+1                 o 11 o 4  o 12
!                           |    |    |
!                 8    6    | 2  | 1  | 5
!      IY         O----O----O----*----O
!                           |    |    |
!                           10   | 3  | 13
!      IY-1                 o----o----o
!                                |
!                                |
!      IY-2                      O 7
!                                |
!                                |
!      IY-3                      O 9
!
!                 ^    ^    ^    ^    ^
!                 |    |    |    |    |
!               IX-3 IX-2 IX-1  IX  IX+1
!
!     1: IX  , IY
!     2: IX-1, IY
!     3: IX  , IY-1
!     4: IX  , IY+1
!     5: IX+1, IY
!     6: IX-2, IY
!     7: IX  , IY-2
!     8: IX-3, IY
!     9: IX  , IY-3
!    10: IX-1, IY-1
!    11: IX-1, IY+1
!    12: IX+1, IY+1
!    13: IX+1, IY-1
!
!    THIS IS THE STENCIL USED WITH THE *OLD* SORDUP SCHEME:
!
!                      4    2
!      IY              O----O----* 1
!                                |
!                                |
!      IY-1                      O 3
!                                |
!                                |
!      IY-2                      O 5
!
!                      ^    ^    ^
!                      |    |    |
!                    IX-2 IX-1  IX
!     1: IX  , IY
!     2: IX-1, IY
!     3: IX  , IY-1
!     4: IX-2, IY
!     5: IX  , IY-2
!
!    THIS IS THE STENCIL USED WITH THE *NEW* SORDUP SCHEME:
!    (NUMBERING HAS CHANGED)
!
!                                O 4(7)
!                                |
!                                |
!                  6(4)   2(2)   | 1(1) 5(6)
!      IY          O------O------*------O
!                                |
!                                |
!                                |
!      IY-1                      O 3(3)
!                                |
!                                |
!                                |
!      IY-2                      O 7(5)
!
!                      ^    ^    ^
!                      |    |    |
!                    IX-2 IX-1  IX
!     1: IX  , IY
!     2: IX-1, IY
!     3: IX  , IY-1
!     4: IX  , IY+1        7==>4
!     5: IX+1, IY          6==>5
!     6: IX-2, IY          4==>6
!     7: IX  , IY-2        5==>7
!
!  AND THE BSBT SCHEME:
!
!                          O 4
!                          |
!                   2      | 1     5
!                   O------*------O
!                          |
!                          |
!                        3 O
!     1: IX  , IY
!     2: IX-1, IY
!     3: IX  , IY-1
!     4: IX  , IY+1
!     5: IX+1, IY
!
!  4. Argument variables
!
!     ITER  : input Iteration counter for SWAN
!     IT    : input Time step counter for SWAN

                  INTEGER ITER,   IT

! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space
! i   XCGRID: Coordinates of computational grid in x-direction
! i   YCGRID: Coordinates of computational grid in y-direction

                  REAL :: SPCDIR(MDC,6)
                  REAL :: SPCSIG(MSC)
                  REAL :: XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     Since the real piece of LSWMAT was removed, the dimensions of
!     LSWMAT are now MLSWMAT not MSWMAT.
                  LOGICAL :: LSWMAT(MDC,MSC,MLSWMAT)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     SPREDT
!     SWAPAR
!     SPROXY
!     SINTGRL
!     SOURCE
!     ACTION
!     SOLMAT
!     SOLMT1
!     SPROSD
!     SWPSEL
!
!  9. Subroutines calling
!
!     SWCOMP
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
!     ---------------------------------------------------------
!     Call WINDP2 to compute some wave parameters necessarry for
!                 the wind subroutines. The wind sea energy spectrum
!                 is computed before every iteration
!     Compute for the two nearby points:
!       {to reduce the size of the arrays K, CPX, CPY, CAX, CAY, CAS, CAD
!       and CGO, CP use a FUNCTION ICODE(_,_) in were the information
!       of the nearby gridpoints is stored.
!       The size of the arrays of the wave parameters are reduced significantly,
!       par example: CAX(ID,IS,IX,IY) --> CAX(ID,IS,ICMAX)  with ICMAX = 3
!       If a higher order scheme is used ICMAX can be increased so that
!       at locations ksx = -2,+2 and ksy = -2,+2 can be used:
!
!                                 o ksy=+2
!                                 |
!                                 |
!                                 o ksy=+1     with:  * = (0,0)
!                                 |
!                                 |
!                   o------o------*------o------o
!                ksx=-2   ksx=-1  |   ksx=+1   ksx=+2
!                                 |
!                                 o ksy=-1
!                                 |
!                                 |
!                                 o ksy=-2
!
!          Molecule:
!
!          (4)     2
!            o------o------* 1           Central grid point     : IC = 1              30.70(?), 33.10
!                          |             Point in X-direction   : IC = 2              30.70(?), 33.10
!                          |             Point in Y-direction   : IC = 3              30.70(?), 33.10
!                        3 o             Point in X-direction   : IC = (4)            30.70(?), 33.10
!                          |             Point in Y-diretion    : IC = (5)            30.70(?), 33.10
!                          |             5 gridpoints --> ICC = 5
!                      (5) o             ( ) = is not used by default (BSBT) scheme   30.70(?), 33.10
!
!          Notice that IX and IY are still in the argument list because
!          the counter of DEP2(IX,IY) and UX2(IX,IY) and UY2(IX,IY) !
!
!     For every ICC = 1 to ICMAX do
!       If (ICC = 1) then
!         IC  = ICODE(0,0)                {central gridpoint}
!         ICX = IX
!         ICY = IY
!         --------------------------------------------------
!       Else if (ICC = 2) then
!         IC = ICODE(KSX,0)               {left or right gridpoint}
!         ICX = IX+KSX
!         ICY = IY
!         --------------------------------------------------
!       Else if (ICC = 3 ) then
!         IC = ICODE(0,KSY)               {top or bottom gridpoint)
!         ICX = IX
!         ICY = IY+KSY
!         --------------------------------------------------
!     End if
!
!     -----------------------------------------------------------------
!     For each gridpoint (IC=1,2,3)  do:
!       Call SWAPAR  to compute the wavenumber K and the group velocity
!       Call SPROXY  to compute propagation velocities CAX, CAY of
!                    energy propagates
!       If central gridpoint (IC=1) do
!         Call SWGEOM  to compute geometric quantities due to
!                      curvilinear grid
!         Call SWPSEL  to compute the bins that fall within a sweep
!                      and which are propagated within a sweep
!         Call SPROSD  to calculate the propagation velocities in
!                      spectral space (CAS and CAD)
!     -------------------------------------------------------
!     If depth > 1.e-4 then do
!       If wind is present :
!         Call WINDP1 to compute the wind speed, PM frequency, mean
!                     wind direction, wind friction velocity and counters
!       ----------------------------------------------------------------
!       Call CNTAIL to compute the contribution of high frequency
!                   tail to the spectrum
!       ----------------------------------------------------------------
!       For the first iteration (ITER = 1), do:
!         Call SPREDT to estimate the action density in stationary mode
!       ----------------------------------------------------------------
!       Call SINTGRL to compute some wave parameters (mean frequency
!                    mean wave number, near bottom velocity and signi-
!                    ficant wave height, fraction of breaking waves
!       ----------------------------------------------------------------
!       Call SOURCE  to compute the source terms for each bin which fall
!                    within a sweep:
!                   1. Dissipation by wave-bottom effects
!                   2. Dissipation due to surf breaking
!                   3. Dissipation due to whitecapping
!                   4. Generation of wave energy by wind effects
!                   5. Nonlinear wave-wave interactions (quadruplets)
!                   6. Nonlinear wave-wave interactions (triads)
!                   7. Dissipation due to vegetation
!                   8. Dissipation due to turbulence
!                   9. Fluid mud-induced wave dissipation
!                  10. Dissipation by sea ice
!                  11. Bragg scattering
!       ----------------------------------------------------------------
!       Call ACTION  calculate the derivatives in x,y,s,d space and store
!                    the results in the corresponding arrays
!       ----------------------------------------------------------------
!       If a current is present do
!         If implicit scheme in frequency space do
!         -----
!           Call SWSIP    to solve penta-diagonal system by means
!                         of Stone's SIP solver
!         -----
!         endif
!       else if no current is present do
!         Call SOLMAT   to solve tri-diagonal system
!       end if
!     ------------------------------------------------------------------
!     CALL WINDP3   for a first or second generation model: limit the
!                   computed action density in a gridpoint according to
!                   the saturation spectra.
!     ---------------------------------------------------------
!     End of SWOMPU
!     ---------------------------------------------------------
!
! 13. Source text

                  INTEGER  IC    ,IX    ,IY    ,IS    ,SWPDIR,&
                  &KSX   ,KSY   ,&
                  &IDWMIN,IDWMAX,IDTOT ,ISTOT ,IDDLOW,IDDTOP,&
                  &ISSTOP,INOCNV
                  INTEGER  LINK(MICMAX)

                  REAL     DDX   ,DDY   ,&
                  &ETOT  ,AC2TOT,ABRBOT,HM    ,HS    ,QBLOC ,&
                  &SMESPC,KMESPC,ETOTW ,WIND10,FPM   ,&
                  &THETAW,SNLC1 ,DAL1  ,DAL2  ,DAL3  ,XIS   ,&
                  &UFRIC ,SMEBRK,KTETA
                  REAL(KIND=8) DT

                  LOGICAL  INSIDE
                  LOGICAL  LPREDT

                  INTEGER :: XYTST(2*NPTST) ,IDCMIN(MSC)
                  INTEGER :: IDCMAX(MSC)    ,ISCMIN(MDC)    ,ISCMAX(MDC)
                  INTEGER :: WWINT(*)
                  INTEGER :: KGRPNT(MXC,MYC)
                  INTEGER :: CROSS(2,MCGRD)

!     *** number of arrays for SWAN ***

                  REAL  :: AC2(MDC,MSC,MCGRD)
                  REAL  :: AC1(MDC,MSC,MCGRD)
                  REAL  :: COMPDA(MCGRD,MCMVAR)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
                  REAL  :: CGO(MSC,MICMAX)          ,&
                  &CAX(MDC,MSC,MICMAX)      ,&
                  &CAY(MDC,MSC,MICMAX)      ,&
                  &CAX1(MDC,MSC,MICMAX)     ,&
                  &CAY1(MDC,MSC,MICMAX)     ,&
                  &CAS(MDC,MSC,MICMAX)      ,&
                  &CAD(MDC,MSC,MICMAX)
                  REAL  :: ALIMW(MDC,MSC)
!              Since the logical piece of SWMATR was removed, the
!              dimensions of SWMATR are now MSWMATR not MSWMAT
                  REAL  :: SWMATR(MDC,MSC,MSWMATR)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
                  REAL  :: KWAVE(MSC,MICMAX), DMW(MSC,MICMAX)    ,&
                  &UE(MSC4MI:MSC4MA , MDC4MI:MDC4MA )    ,&
                  &SA1(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
                  &SA2(MSC4MI:MSC4MA , MDC4MI:MDC4MA )   ,&
                  &DA1C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DA1P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DA1M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DA2C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DA2P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DA2M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &SFNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &DSNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )  ,&
                  &MEMNL4(MDC,MSC,MCGRD)                 ,&
                  &MEMBRG(MDC,MSC,MCGRD)                 ,&
                  &MEMQCM(MDC,MSC,MCGRD)                 ,&
                  &MEMQCB(MDC,MSC,MCGRD)                 ,&
                  &MEMSINA(MDC,MSC,MCGRD)                ,&
                  &MEMSINB(MDC,MSC,MCGRD)                ,&
                  &SWTSDA(MDC,MSC,NPTSTA,MTSVAR)         ,&
                  &WWAWG(*)                              ,&
                  &WWSWG(*)                              ,&
                  &QTL1(:,:) ,QTL2(:,:)                  ,&
                  &RDX(MICMAX)   ,RDY(MICMAX)            ,&
                  &OBREDF(MDC,MSC,2)                     ,&
                  &REFLSO(MDC,MSC)
                  REAL  :: FBD(MDC,MDC,MSC)
                  REAL  :: URMSTOP(MCGRD)

                  LOGICAL  GROWW(MDC,MSC)    ,&
                  &ANYWND(MDC)

                  INTEGER :: ISLMIN(MCGRD), NFLIM(MCGRD), NRSCAL(MCGRD)

!     *** arrays for QC scattering ***
!
!     CGFT : Fourier-transformed modulation of group velocity
!     SIGFT: Fourier-transformed modulation of intrinsic frequency
!     UXFT : u-component of Fourier-transformed modulation of ambient current
!     UYFT : v-component of Fourier-transformed modulation of ambient current
!
!     CFT  : Fourier coefficients (FFT)
!     RFT  : input data (FFT)
!     SFT  : input data (FFT)
!     WFT  : work array (FFT)
!     WSAVE: work array (FFT)

                  REAL            :: CGFT(NCOZ,NCOZ,MSC), SIGFT(NCOZ,NCOZ,MSC)
                  COMPLEX         :: UXFT(NCOZ,NCOZ), UYFT(NCOZ,NCOZ)
                  COMPLEX(KIND=8) :: CFT(NCOZ,NCOZ)
                  REAL   (KIND=8) :: RFT(NCOZ,NCOZ), SFT(NCOZ,NCOZ),&
                  &WFT(LENWFT), WSAVE(LENSAV)

!     *** arrays for QC surf breaking ***
!
!     CFD  : Fourier coefficients (FFT)
!     WFD  : work array (FFT)
!     WSAVD: work array (FFT)

                  COMPLEX(KIND=8) :: CFD(MYD,MXD)
                  REAL   (KIND=8) :: WFD(LENWFD), WSAVD(LENSVD)

                  INTEGER, SAVE :: IENT = 0
                  INTEGER NLINK, ILINK, II, INDEX, ID, IDC, ISC,&
                  &ID_MIN, ID_MAX, IDDUM
                  REAL SP, TEMP

                  IF (LTRACE) CALL STRACE (IENT,'SWOMPU')
!     *** Get grid point numbers for points in computational stencil ***
                  IXCGRD(1) = IX
                  IYCGRD(1) = IY
                  KCGRD(1)  = KGRPNT(IX,IY)
                  IF (KCGRD(1).GT.1) THEN
                     IXCGRD(2) = IX+KSX
                     IYCGRD(2) = IY
                     IXCGRD(3) = IX
                     IYCGRD(3) = IY+KSY
                     IXCGRD(4) = IX
                     IYCGRD(4) = IY-KSY
                     IXCGRD(5) = IX-KSX
                     IYCGRD(5) = IY
                     PROPSL = PROPSC
                     IF (PROPSC.EQ.1) THEN
                        ICMAX = 5
                     ELSE
!         add more points for higher order schemes
                        ICMAX = 7
                        IXCGRD(6) = IX+2*KSX
                        IYCGRD(6) = IY
                        IXCGRD(7) = IX
                        IYCGRD(7) = IY+2*KSY
                     ENDIF
                     IF (PROPSC.EQ.3) THEN
!         add more points for S&L scheme
                        ICMAX = 13
                        IXCGRD(8)  = IX+3*KSX
                        IYCGRD(8)  = IY
                        IXCGRD(9)  = IX
                        IYCGRD(9)  = IY+3*KSY
                        IXCGRD(10) = IX+KSX
                        IYCGRD(10) = IY+KSY
                        IXCGRD(11) = IX+KSX
                        IYCGRD(11) = IY-KSY
                        IXCGRD(12) = IX-KSX
                        IYCGRD(12) = IY-KSY
                        IXCGRD(13) = IX-KSX
                        IYCGRD(13) = IY+KSY
                     ENDIF
                     DO IC = 2, ICMAX
!         if one of the points of a stencil is outside the computational  33.09
!         domain, fall back to first order scheme
!
! Note that stencil of first order scheme has been increased to 5
! Also note that points 4 and 5 are used only for SPROSD.
! In cases where user has chosen the first order scheme (PROPSC=1) , and    40.59
! points 4 and/or 5 fall outside the grid, we do not want to set PROPSL=0.  40.59
! However, we do want to set INSIDE=FALSE, so that KCGRD(IC) = 1, so that   40.59
! SPROSD will know that the point is not available. As the code is written  40.59
! now, no change is required. But also be aware that refraction will no
! longer be calculated at the last grid point (i.e. SPROSD is not coded
! fall back to the first order scheme, it just set C_theta=0).

                        INSIDE = .TRUE.
                        IF (IXCGRD(IC).LT.1) THEN
                           IF (KREPTX.GT.0) THEN
!             domain is repeating in x-direction
                              IXCGRD(IC) = IXCGRD(IC) + MXC
                           ELSE
                              IF (IC.LE.3) THEN
                                 PROPSL = 0
                              ELSE
                                 IF (PROPSC.GT.1) PROPSL = 1
                              ENDIF
                              INSIDE = .FALSE.
                           ENDIF
                        ENDIF
                        IF (IXCGRD(IC).GT.MXC) THEN
                           IF (KREPTX.GT.0) THEN
                              IXCGRD(IC) = IXCGRD(IC) - MXC
                           ELSE
                              IF (IC.LE.3) THEN
                                 PROPSL = 0
                              ELSE
                                 IF (PROPSC.GT.1) PROPSL = 1
                              ENDIF
                              INSIDE = .FALSE.
                           ENDIF
                        ENDIF
                        IF (IYCGRD(IC).LT.1 .OR. IYCGRD(IC).GT.MYC) THEN
                           IF (.NOT.ONED) THEN
                              IF (IC.LE.3) THEN
                                 PROPSL = 0
                              ELSE
                                 IF (PROPSC.GT.1) PROPSL = 1
                              ENDIF
                           ENDIF
                           INSIDE = .FALSE.
                        ENDIF
                        IF (INSIDE) THEN
                           KCGRD(IC) = KGRPNT(IXCGRD(IC),IYCGRD(IC))
                        ELSE
                           KCGRD(IC) = 1
                        ENDIF
!         if point in stencil is dry, fall back to BSBT scheme.
                        IF (PROPSC.GT.1) THEN
                           IF (COMPDA(KCGRD(IC),JDP2).LE.DEPMIN) PROPSL = 1
                           IF (NSTATC.GT.0) THEN
!             if nonstationary, check also previous time level
                              IF (COMPDA(KCGRD(IC),JDP1).LE.DEPMIN) PROPSL = 1
                           ENDIF
                        END IF
                     ENDDO
                     IF (PROPSL.EQ.0) THEN
                        ICMAX = 1
                     ELSEIF (PROPSL.EQ.1) THEN
                        ICMAX = 5
                     ENDIF
                  ELSE
                     PROPSL = 0
                     ICMAX = 1
                  ENDIF

!     *** If there are obstacles crossing the points in the stencil ***
!     *** then fall back to first order scheme                      ***
!
!TIMG                  CALL SWTSTA(136)
                  IF (NUMOBS.NE.0 .AND. PROPSL.NE.1) THEN
                     IF (PROPSL.EQ.3) THEN
                        NLINK  = 10
                        IF (SWPDIR .EQ. 1 ) THEN
                           LINK(1)  = CROSS(1,KCGRD(1))
                           LINK(2)  = CROSS(2,KCGRD(1))
                           LINK(3)  = CROSS(1,KCGRD(2))
                           LINK(4)  = CROSS(2,KCGRD(2))
                           LINK(5)  = CROSS(1,KCGRD(3))
                           LINK(6)  = CROSS(2,KCGRD(3))
                           LINK(7)  = CROSS(2,KCGRD(4))
                           LINK(8)  = CROSS(1,KCGRD(5))
                           LINK(9)  = CROSS(1,KCGRD(6))
                           LINK(10) = CROSS(2,KCGRD(7))
                        ELSE IF (SWPDIR .EQ. 2) THEN
                           LINK(1)  = CROSS(1,KCGRD(2))
                           LINK(2)  = CROSS(2,KCGRD(1))
                           LINK(3)  = CROSS(1,KCGRD(6))
                           LINK(4)  = CROSS(2,KCGRD(2))
                           LINK(5)  = CROSS(1,KCGRD(10))
                           LINK(6)  = CROSS(2,KCGRD(3))
                           LINK(7)  = CROSS(2,KCGRD(4))
                           LINK(8)  = CROSS(1,KCGRD(1))
                           LINK(9)  = CROSS(1,KCGRD(8))
                           LINK(10) = CROSS(2,KCGRD(7))
                        ELSE IF (SWPDIR .EQ. 3) THEN
                           LINK(1)  = CROSS(1,KCGRD(2))
                           LINK(2)  = CROSS(2,KCGRD(3))
                           LINK(3)  = CROSS(1,KCGRD(6))
                           LINK(4)  = CROSS(2,KCGRD(10))
                           LINK(5)  = CROSS(1,KCGRD(10))
                           LINK(6)  = CROSS(2,KCGRD(7))
                           LINK(7)  = CROSS(2,KCGRD(1))
                           LINK(8)  = CROSS(1,KCGRD(1))
                           LINK(9)  = CROSS(1,KCGRD(8))
                           LINK(10) = CROSS(2,KCGRD(9))
                        ELSE IF (SWPDIR .EQ. 4) THEN
                           LINK(1)  = CROSS(1,KCGRD(1))
                           LINK(2)  = CROSS(2,KCGRD(3))
                           LINK(3)  = CROSS(1,KCGRD(2))
                           LINK(4)  = CROSS(2,KCGRD(10))
                           LINK(5)  = CROSS(1,KCGRD(3))
                           LINK(6)  = CROSS(2,KCGRD(7))
                           LINK(7)  = CROSS(2,KCGRD(1))
                           LINK(8)  = CROSS(1,KCGRD(5))
                           LINK(9)  = CROSS(1,KCGRD(6))
                           LINK(10) = CROSS(2,KCGRD(9))
                        ENDIF
                     ELSE IF (PROPSL.EQ.2) THEN
                        NLINK  = 4
                        IF (SWPDIR .EQ. 1 ) THEN
                           LINK(1)  = CROSS(1,KCGRD(1))
                           LINK(2)  = CROSS(2,KCGRD(1))
                           LINK(3)  = CROSS(1,KCGRD(2))
                           LINK(4)  = CROSS(2,KCGRD(3))
                        ELSE IF (SWPDIR .EQ. 2) THEN
                           LINK(1)  = CROSS(1,KCGRD(2))
                           LINK(2)  = CROSS(2,KCGRD(1))
                           LINK(3)  = CROSS(1,KCGRD(6))
                           LINK(4)  = CROSS(2,KCGRD(3))
                        ELSE IF (SWPDIR .EQ. 3) THEN
                           LINK(1)  = CROSS(1,KCGRD(2))
                           LINK(2)  = CROSS(2,KCGRD(3))
                           LINK(3)  = CROSS(1,KCGRD(6))
                           LINK(4)  = CROSS(2,KCGRD(7))
                        ELSE IF (SWPDIR .EQ. 4) THEN
                           LINK(1)  = CROSS(1,KCGRD(1))
                           LINK(2)  = CROSS(2,KCGRD(3))
                           LINK(3)  = CROSS(1,KCGRD(2))
                           LINK(4)  = CROSS(2,KCGRD(7))
                        ENDIF
                     ENDIF
                     IF (PROPSL.GT.1) THEN
!           if there is an obstacle crossing, fall back to 1st order
                        DO ILINK = 1, NLINK
                           IF (LINK(ILINK).GT.0) PROPSL = 1
                        ENDDO
                     ENDIF
                  ENDIF
!TIMG                  CALL SWTSTO(136)

                  IF (ITEST .GE. 180 ) THEN
                     WRITE(PRINTF,"(' Points in stencil in subr SWOMPU, sweep : ',I1, /,'POINT( IX, IY), INDEX, COORDX, COORDY')") SWPDIR

                     DO IC = 1, ICMAX
                        IF ( KCGRD(IC).EQ.1 ) CYCLE   ! if point is not valid, then cy
                        WRITE(PRINTF,"(4X,I4,1X,I4,3X,I5,5X,F10.2,4X,F10.2)") IXCGRD(IC), IYCGRD(IC), KCGRD(IC),&
                        &XCGRID(IXCGRD(IC),IYCGRD(IC)), YCGRID(IXCGRD(IC),IYCGRD(IC))
                     ENDDO
                  ENDIF

!     *** determine whether the point is a test point ***

                  IPTST  = 0
                  TESTFL = .FALSE.
                  IF (NPTST.GT.0) THEN
                     do II = 1, NPTST
                        IF (IX.EQ.XYTST(2*II-1) .AND. IY.EQ.XYTST(2*II)) THEN
                           IPTST = II
                           TESTFL = .TRUE.
                           IF (ITEST .GE. 10)&
                           &WRITE(PRINTF, "(' Test point ', I2, ', (ix,iy)', 2I5, ', point index ',I5, ', iter ', I2, ', sweep ', I1)") IPTST, IX+MXF-2, IY+MYF-2, KCGRD(1), ITER,&
                           &SWPDIR
                        END IF
                     end do
                  ENDIF

                  IF (TESTFL .AND. ITEST .GE. 220) THEN
                     INDEX = KGRPNT(IX,IY)
                     WRITE(PRINTF,"(' Action densities for IX IY IDNX: ', 2I4, I7)") IX+MXF-1,IY+MYF-1,INDEX
                     DO IS = 1, MSC
                        WRITE(PRINTF, "(' frequency ', I4)") IS
                        WRITE(PRINTF, "(100E12.4)") (AC2(ID,IS,INDEX), ID=1, MDC)
                     ENDDO
                  ENDIF

                  IF(TESTFL.AND.ITEST.GE.10) WRITE(PRINTF,"(//,' sweep direction and node IX, IY ',3I4)") SWPDIR,IX+MXF-1,&
                  &IY+MYF-1

!     --- in case of non-active point set action density in land points
!         equal to zero and go back to main program
                  IF (KCGRD(1).LE.1 .OR. COMPDA(KCGRD(1),JDP2).LE.DEPMIN) THEN
                     DO IS = 1, MSC
                        DO ID = 1, MDC
                           AC2(ID,IS,KCGRD(1)) = 0.
                        ENDDO
                     ENDDO
                     COMPDA(KCGRD(1),JHS) = 0.
                     RETURN
                  END IF

                  IF (KCGRD(2) .LE. 1) RETURN
                  IF (.NOT.ONED .AND. (KCGRD(3).LE.1)) RETURN

                  IF (PROPSL.EQ.3 .AND. NSTATC.GT.0) THEN

!         calculate propagation velocities for old time level
!         (needed for S&L scheme nonstationary)
!
!         COMPDA(1,JDP2) is dep2 ;change to..dep1 which is COMPDA(1,JDP1) 33.08
!         COMPDA(1,JVX2) is ux2  ;change to..ux1 which is COMPDA(1,JVX1)  33.08
!         COMPDA(1,JVY2) is uy2  ;change to..uy1 which is COMPDA(1,JVY1)  33.08
!
!         we could save CPU time by calculating the CAX values only once  33.08
!         when CAX is constant, but this would require SWAN to
!         save CAX over the entire grid (more memory).
!
!         if nonstationary, then CAX1 is not necessarily equal to CAX
!         also, if we are using the BSBT scheme only,
!         then CAX1, CAY1 are not needed.
!
!TIMG                     CALL SWTSTA(110)
                     CALL SWAPAR ( COMPDA(1,JDP1), COMPDA(1,JMUDL1),&
                     &KWAVE, CGO, DMW, SPCSIG )
!TIMG                     CALL SWTSTO(110)
!
!         *** compute the propagation velocities CAX1 and CAY1       ***
!         *** for all directions for the gridpoints IC = 1 to ICMAX  ***
!
!TIMG                     CALL SWTSTA(111)
                     CALL SPROXY (CAX1            ,&
                     &CAY1           ,CGO            ,SPCDIR(1,2)    ,&
                     &SPCDIR(1,3)    ,COMPDA(1,JVX1) ,COMPDA(1,JVY1) ,&
                     &SWPDIR         ,DIFFR&
                     &)
!TIMG                     CALL SWTSTO(111)

                  END IF

!     calculate propagation velocities for new time level
!
!     *** Compute wavenumber KWAVE and group velocity CGO   ***
!     *** in the gridpoints of the stencil                  ***
!
!TIMG                  CALL SWTSTA(110)
                  CALL SWAPAR ( COMPDA(1,JDP2), COMPDA(1,JMUDL2),&
                  &KWAVE, CGO, DMW, SPCSIG )
!TIMG                  CALL SWTSTO(110)
!
!     *** compute the propagation velocities CAX and CAY        ***
!     *** for all directions for the gridpoints IC = 1 to ICMAX ***
!
!TIMG                  CALL SWTSTA(111)
                  CALL SPROXY (CAX            ,&
                  &CAY            ,CGO            ,SPCDIR(1,2)    ,&
                  &SPCDIR(1,3)    ,COMPDA(1,JVX2) ,COMPDA(1,JVY2) ,&
                  &SWPDIR         ,DIFFR&
                  &)
!TIMG                  CALL SWTSTO(111)
!
!     --- compute geometric quantities due to curvilinear grid
!
!TIMG                  CALL SWTSTA(112)
                  CALL SWGEOM ( RDX, RDY, XCGRID, YCGRID, SWPDIR )
!TIMG                  CALL SWTSTO(112)
!
!     *** compute minimum and maximum counter (IDCMIN and ***
!     *** IDCMAX) and fill the array ANYBIN to determine  ***
!     *** if a bin lies within the sweep considered       ***
!
!TIMG                  CALL SWTSTA(112)
                  CALL SWPSEL (SWPDIR                            ,IDCMIN        ,&
                  &IDCMAX          ,CAX              ,&
                  &CAY             ,LSWMAT(1,1,JABIN),&
                  &ISCMIN           ,&
                  &ISCMAX          ,IDTOT            ,ISTOT         ,&
                  &IDDLOW          ,IDDTOP           ,ISSTOP        ,&
                  &COMPDA(1,JDP2)  ,COMPDA(1,JVX2)   ,COMPDA(1,JVY2),&
                  &SPCDIR          ,RDX              ,RDY           ,&
                  &KGRPNT&
                  &)
!TIMG                  CALL SWTSTO(112)
!
!     *** compute the propagation velocities CAS and CAD   ***
!
!TIMG                  CALL SWTSTA(113)
                  CALL SPROSD (SPCSIG         ,KWAVE          ,CAS            ,&
                  &CAD            ,CGO            ,&
                  &COMPDA(1,JDP2) ,COMPDA(1,JDP1) ,SPCDIR(1,2)    ,&
                  &SPCDIR(1,3)    ,COMPDA(1,JVX2) ,COMPDA(1,JVY2) ,&
                  &SPCDIR(1,4)    ,SPCDIR(1,6)    ,SPCDIR(1,5)    ,&
                  &RDX            ,RDY            ,&
                  &CAX            ,CAY            ,&
                  &XCGRID         ,YCGRID         ,&
                  &IDDLOW         ,IDDTOP         ,DIFFR&
                  &)
!TIMG                  CALL SWTSTO(113)
!
!TIMG                  CALL SWTSTA(114)
                  IF (KSPHER.GT.0 .AND. IREFR.NE.0) THEN

!        *** compute the change of propagation velocity CAD   ***
!        *** due to the use of spherical coordinates          ***

                     CALL DSPHER (CAD                ,CAX            ,&
                     &CAY                ,&
                     &LSWMAT(1,1,JABIN)  ,YCGRID         ,&
                     &SPCDIR(1,2)        ,SPCDIR(1,3)    )
                  ENDIF
!TIMG                  CALL SWTSTO(114)

                  IF ( IDTOT.GT.0 ) THEN

!       *** initialize friction velocity and Fpm frequency even ***
!       *** when there is no wind input                         ***

                     UFRIC = 1.E-15
                     FPM   = 1.E-15
                     IF ( IWIND .GE. 1 ) THEN

!         *** compute the wind speed, mean wind direction, the    ***
!         *** PM frequency, wind friction velocity U*  and the    ***
!         *** minimum and maximum counters for active wind input  ***
!
!TIMG                        CALL SWTSTA(115)
                        CALL WINDP1 (WIND10     ,THETAW     ,&
                        &IDWMIN     ,IDWMAX     ,&
                        &FPM        ,UFRIC      ,&
                        &COMPDA(1,JWX2) ,COMPDA(1,JWY2) ,&
                        &ANYWND     ,SPCDIR     ,&
                        &COMPDA(1,JVX2) ,COMPDA(1,JVY2) ,SPCSIG ,AC2&
                        &,SWMATR(1,1,JGEN0), KWAVE&
                        &)
!TIMG                        CALL SWTSTO(115)
                        IF (IWIND.NE.4) COMPDA(KCGRD(1),JUSTAR) = UFRIC
                     END IF

!       *** fill in the triad interpolation and scaling factors
!           for current grid point

                     IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
                        QTL2(:,1) = TRIADS%scaling(:,KCGRD(1),1)
                        QTL2(:,2) = TRIADS%scaling(:,KCGRD(1),2)
                     ELSE IF (ITRIAD.EQ.2 .OR. ITRIAD.EQ.3) THEN
                        QTL1(:,1) = TRIADS%interpolation(:,1)
                        QTL1(:,2) = TRIADS%interpolation(:,2)
                        QTL2(:,1) = TRIADS%scaling(:,KCGRD(1),1)
                        QTL2(:,2) = TRIADS%scaling(:,KCGRD(1),2)
                        QTL2(:,3) = TRIADS%scaling(:,KCGRD(1),3)
                        QTL2(:,4) = TRIADS%scaling(:,KCGRD(1),4)
                     ELSE IF (ITRIAD.EQ.5) THEN
                        QTL1(:,1) = TRIADS%interpolation(:,1)
                        QTL1(:,2) = TRIADS%interpolation(:,2)
                        QTL2(:,1) = TRIADS%scaling(:,KCGRD(1),1)
                        QTL2(:,2) = TRIADS%scaling(:,KCGRD(1),2)
                     ENDIF

!       *** estimate action density in case of first iteration ***
!       *** in stationary mode (since it is zero in first      ***
!           stationary run)                                    ***

                     prediction_flow: BLOCK
                     LOGICAL :: postpone_prediction

                     postpone_prediction = ICOND.NE.4 .AND. ITER.EQ.1 .AND. NSTATC.EQ.0
                     LPREDT = postpone_prediction
                     IF (ICOND.NE.4 .AND. ITER.EQ.1 .AND. NSTATC.EQ.0) THEN
                        COMPDA(KCGRD(1),JHS) = 0.
                     END IF
                     prediction_pass: DO
                     IF (.NOT.postpone_prediction .AND. LPREDT) THEN
                        CALL SPREDT (SWPDIR           ,AC2               ,CAX       ,&
                        &CAY              ,IDCMIN            ,IDCMAX    ,&
                        &ISSTOP           ,LSWMAT(1,1,JABIN) ,&
                        &XCGRID           ,YCGRID            ,&
                        &RDX              ,RDY               ,OBREDF    )
                        LPREDT=.FALSE.
                     END IF

!       Calculate various integral parameters for use in the source terms
!
!TIMG                     CALL SWTSTA(116)
                     IF (.NOT.postpone_prediction) THEN
                     CALL SINTGRL  (SPCDIR  ,KWAVE   ,AC2     ,&
                     &COMPDA(1,JDP2)   ,QBLOC   ,COMPDA(1,JURSEL),&
                     &COMPDA(1,JBIPH)  ,RDX     ,RDY     ,&
                     &AC2TOT  ,ETOT    ,&
                     &ABRBOT  ,COMPDA(1,JUBOT)  ,HS      ,&
                     &COMPDA(1,JQB)    ,&
                     &HM      ,KMESPC  ,SMEBRK  ,KTETA   ,&
                     &COMPDA(1,JPBOT)  ,&
                     &COMPDA(1,JBOTLV) ,COMPDA(1,JGAMMA) ,&
                     &SWPDIR           ,&
                     &URMSTOP          ,&
                     &IDDLOW           ,IDDTOP, TRIADS,&
                     &SPECTRAL_POWERS%value, WCAP_WORKSPACE )
!TIMG                     CALL SWTSTO(116)

                     COMPDA(KCGRD(1),JHS) = HS
                     END IF
                     postpone_prediction = .FALSE.

!       *** If there are obstacles crossing the points in the stencil ***
!       *** then the transmission and reflection coeff. are computed  ***
!       *** and also the contribution to the source term              ***

!TIMG                     CALL SWTSTA(136)
                     IF (NUMOBS .NE. 0) THEN

!         *** OBREDF(:,:,2) are the transmission coeff for the two links ***
!         *** in the stencil (between the three point on the stencil)
!         *** REFLSO(:,:) contains the contribution to the source term

                        DO IDC = 1, MDC
                           DO ISC = 1, MSC
                              OBREDF(IDC,ISC,1) = 1.
                              OBREDF(IDC,ISC,2) = 1.
                              REFLSO(IDC,ISC  ) = 0.
                           ENDDO
                        ENDDO

                        IF (SWPDIR .EQ. 1 ) THEN
                           LINK(1) = CROSS(1,KCGRD(1))
                           LINK(2) = CROSS(2,KCGRD(1))
                        ELSE IF (SWPDIR .EQ. 2) THEN
                           LINK(1) = CROSS(1,KCGRD(2))
                           LINK(2) = CROSS(2,KCGRD(1))
                        ELSE IF (SWPDIR .EQ. 3) THEN
                           LINK(1) = CROSS(1,KCGRD(2))
                           LINK(2) = CROSS(2,KCGRD(3))
                        ELSE IF (SWPDIR .EQ. 4) THEN
                           LINK(1) = CROSS(1,KCGRD(1))
                           LINK(2) = CROSS(2,KCGRD(3))
                        ENDIF

                        IF (LINK(1) .NE. 0 .OR. LINK(2) .NE. 0) THEN
                           IF (ITEST .GE. 120) WRITE(PRINTF,"(' SWOMPU: SWPDIR POINT LINK1 LINK2 = ',4(1X,I5))")&
                           &SWPDIR,KCGRD(1),LINK(1),LINK(2)

                           CALL SWTRCF (COMPDA(1,JDP2),COMPDA(1,JWLV2),&
                           &COMPDA(1,JHS), LINK, OBREDF,&
                           &AC2, REFLSO, KGRPNT, XCGRID,&
                           &YCGRID, CAX, CAY, RDX, RDY, LSWMAT(1,1,JABIN),&
                           &SPCSIG, SPCDIR, CGO, KWAVE,&
                           &COMPDA(1,JHSS2), COMPDA(1,JTSS2), COMPDA(1,JDSS2))
                        ENDIF

                     ENDIF
!TIMG                     CALL SWTSTO(136)
                     IF (.NOT.LPREDT) EXIT prediction_pass
                     END DO prediction_pass
                     END BLOCK prediction_flow

!       *** compute source terms and fill the matrix ***
!
!TIMG                     CALL SWTSTA(117)
                     IF ( IGEN.NE.4 ) THEN
                        CALL SOURCE (ITER   ,IX                  ,IY                  ,&
                        &SWPDIR              ,KWAVE               ,SPCSIG              ,&
                        &SPCDIR(1,2)         ,SPCDIR(1,3)         ,AC2                 ,&
                        &COMPDA(1,JDP2)      ,SWMATR(1,1,JMATD)   ,SWMATR(1,1,JMATR)   ,&
                        &ABRBOT              ,KMESPC              ,SMESPC              ,&
                        &COMPDA(1,JUBOT)     ,UFRIC               ,COMPDA(1,JVX2)      ,&
                        &COMPDA(1,JVY2)      ,IDCMIN              ,IDCMAX              ,&
                        &IDDLOW              ,IDDTOP              ,IDWMIN              ,&
                        &IDWMAX              ,ISSTOP              ,SWTSDA(1,1,1,JPWNDS),&
                        &SWTSDA(1,1,1,JPWNDD),SWTSDA(1,1,1,JPWCAP),SWTSDA(1,1,1,JPBTFR),&
                        &SWTSDA(1,1,1,JPSWEL),&
                        &SWTSDA(1,1,1,JPWBRK),SWTSDA(1,1,1,JP4S)  ,SWTSDA(1,1,1,JP4D)  ,&
                        &SWTSDA(1,1,1,JPVEGT),SWTSDA(1,1,1,JPTURB),SWTSDA(1,1,1,JPMUD) ,&
                        &SWTSDA(1,1,1,JPICE) ,SWTSDA(1,1,1,JPBRAG),&
                        &SWTSDA(1,1,1,JPTRI) ,                     HS                  ,&
                        &ETOT                ,QBLOC               ,THETAW              ,&
                        &HM                  ,FPM                 ,WIND10              ,&
                        &ETOTW               ,GROWW               ,ALIMW               ,&
                        &SMEBRK              ,KTETA               ,SNLC1               ,&
                        &DAL1                ,DAL2                ,DAL3                ,&
                        &UE                  ,SA1                 ,&
                        &SA2                 ,DA1C                ,DA1P                ,&
                        &DA1M                ,DA2C                ,DA2P                ,&
                        &DA2M                ,SFNL                ,DSNL                ,&
                        &MEMNL4              ,WWINT               ,WWAWG               ,&
                        &WWSWG               ,CGO                 ,COMPDA(1,JUSTAR)    ,&
                        &COMPDA(1,JZEL)      ,SPCDIR              ,ANYWND              ,&
                        &DMW                 ,FBD                 ,MEMBRG              ,&
                        &CAS                 ,QTL1                ,QTL2                ,&
                        &MEMSINA             ,MEMSINB             ,&
                        &SWMATR(1,1,JDIS0)   ,SWMATR(1,1,JDIS1)   ,&
                        &SWMATR(1,1,JGEN0)   ,SWMATR(1,1,JGEN1)   ,&
                        &SWMATR(1,1,JRED0)   ,SWMATR(1,1,JRED1)   ,&
                        &XIS                 ,COMPDA(1,JFRC2)     ,IT                  ,&
                        &COMPDA(1,JNPLA2)    ,COMPDA(1,JTURB2)    ,COMPDA(1,JMUDL2)    ,&
                        &COMPDA(1,JAICE2)    ,COMPDA(1,JHICE2)    ,&
                        &COMPDA(1,JURSEL)    ,LSWMAT(1,1,JABIN)   ,REFLSO              ,&
                        &COMPDA(1,JTAUW)     ,COMPDA(1,JBIPH)&
                        &,URMSTOP            ,TRIADS, SNL4, SPECTRAL_POWERS,&
                        &WCAP_WORKSPACE&
                        &)
                     ENDIF
                     IF ( IQCM.GT.0 .OR. IGEN.EQ.4 ) THEN
                        CALL QCSOURCE ( SWMATR(1,1,JMATR)         , SWMATR(1,1,JMATD) ,&
                        &ITER                , AC2                 , COMPDA(1,JDP2)    ,&
                        &COMPDA(1,JVX2)      , COMPDA(1,JVY2)      , SWPDIR            ,&
                        &IX                  , IY                  , RDX               ,&
                        &RDY                 , KWAVE               , CGO               ,&
                        &SIGFT               , CGFT                , UXFT              ,&
                        &UYFT                , MEMQCM              , MEMQCB            ,&
                        &SWTSDA(1,1,1,JPQCS) , SWTSDA(1,1,1,JPWBRK), SWMATR(1,1,JDIS0) ,&
                        &SWMATR(1,1,JDIS1)   , SWMATR(1,1,JGEN0)   , SWMATR(1,1,JGEN1) ,&
                        &SWMATR(1,1,JRED0)   , SWMATR(1,1,JRED1)   , SPCSIG            ,&
                        &SPCDIR              , IDCMIN              , IDCMAX            ,&
                        &ISSTOP              , SPCDIR(1,2)         , SPCDIR(1,3)       ,&
                        &ETOT                , HM                  , QBLOC             ,&
                        &SMEBRK              , KTETA               , KMESPC            ,&
                        &CFT                 , RFT                 , SFT               ,&
                        &WFT                 , WSAVE               , CFD               ,&
                        &WFD                 , WSAVD               ,&
                        &WCAP_WORKSPACE%mean_frequency_wam&
                        &)
                     ENDIF
!TIMG                     CALL SWTSTO(117)
!
!       *** compute transport of action and fill the matrix ***
!
!TIMG                     CALL SWTSTA(118)
                     CALL ACTION (IDCMIN      ,IDCMAX            ,SPCSIG            ,&
                     &AC2               ,CAX               ,CAY               ,&
                     &CAS               ,CAD               ,SWMATR(1,1,JMATL) ,&
                     &SWMATR(1,1,JMATD) ,SWMATR(1,1,JMATU) ,SWMATR(1,1,JMATR) ,&
                     &SWMATR(1,1,JMAT5) ,&
                     &SWMATR(1,1,JMAT6) ,ISCMIN            ,ISCMAX            ,&
                     &IDDLOW            ,IDDTOP            ,ISSTOP            ,&
                     &LSWMAT(1,1,JABLK) ,LSWMAT(1,1,JABIN) ,&
                     &SWMATR(1,1,JLEK1) ,AC1               ,&
                     &DYNDEP            ,RDX               ,RDY               ,&
                     &SWPDIR            ,IX                ,IY                ,&
                     &KSX               ,KSY               ,&
                     &XCGRID            ,YCGRID            ,&
                     &ITER              ,KGRPNT            ,OBREDF            ,&
                     &CAX1              ,CAY1              ,SPCDIR            ,&
                     &CGO               ,SWMATR(1,1,JTRA0) ,SWMATR(1,1,JTRA1)&
                     &)
!TIMG                     CALL SWTSTO(118)
!
!       matrix is computed now; updating action densities starts
!       provided ACUPDA is true

                     IF (.NOT.ACUPDA) THEN
                        IF (TESTFL .AND. ITEST.GE.30) WRITE (PRINTF, *) ' No update'
                     ENDIF

                     IF (ACUPDA) THEN

!       preparatory steps before solution of linear system
!
!TIMG                     CALL SWTSTA(119)
                     CALL SOLPRE(AC2                ,SWMATR(1,1,JAOLD)  ,&
                     &SWMATR(1,1,JMATR)  ,SWMATR(1,1,JMATL)  ,&
                     &SWMATR(1,1,JMATD)  ,SWMATR(1,1,JMATU)  ,&
                     &SWMATR(1,1,JMAT5)  ,SWMATR(1,1,JMAT6)  ,&
                     &IDCMIN             ,IDCMAX             ,&
                     &LSWMAT(1,1,JABIN)  ,&
                     &IDTOT              ,ISTOT              ,&
                     &IDDLOW             ,IDDTOP             ,&
                     &ISSTOP             ,&
                     &SPCSIG                                 )
!TIMG                     CALL SWTSTO(119)

                     IF ( IREFR.EQ.0 .AND. ITFRE.EQ.0 ) THEN

!          *** No refraction and no frequency shift   ***
!          *** no need to solve a system, just update ***
!
!TIMG                        CALL SWTSTA(120)
                        DO IS = 1, MSC
                           DO IDDUM = IDCMIN(IS), IDCMAX(IS)
                              ID = MOD(IDDUM-1+MDC, MDC) + 1
                              SP = SWMATR(ID,IS,JMATD)
                              IF ( ABS(SP) .LE. 1.E-20 ) THEN
                                 TEMP = SIGN (1.E-20,SP)
                              ELSE
                                 TEMP = SP
                              ENDIF
                              AC2(ID,IS,KCGRD(1)) = SWMATR(ID,IS,JMATR) / TEMP
                           ENDDO
                        ENDDO

!          *** set matrix elements to zero ***

                        SWMATR(:,:,JMATR) = 0.
                        SWMATR(:,:,JMATD) = 0.
!TIMG                        CALL SWTSTO(120)

                     ELSEIF ( (DYNDEP .OR. ICUR .EQ. 1) .AND.&
                     &PNUMS(8).NE.0.                 ) THEN

!         *** Implicit or explicit scheme in frequency space and ***
!         *** implicit scheme in directional space               ***

                        IF ( INT(PNUMS(8)) .EQ. 1 ) THEN

!           *** Implicit scheme in frequency space. Solve penta- ***
!           *** diagonal system with the SIP solver              ***
!
!TIMG                           CALL SWTSTA(120)
                           CALL SWSIP ( AC2, SWMATR(1,1,JMATD), SWMATR(1,1,JMATR),&
                           &SWMATR(1,1,JMATL), SWMATR(1,1,JMATU),&
                           &SWMATR(1,1,JMAT5), SWMATR(1,1,JMAT6),&
                           &SWMATR(1,1,JAOLD),&
                           &PNUMS(12), NINT(PNUMS(14)), NINT(PNUMS(13)),&
                           &INOCNV, IDDLOW, IDDTOP, ISSTOP, IDCMIN,&
                           &IDCMAX )
!TIMG                           CALL SWTSTO(120)

                        ELSE IF (INT(PNUMS(8)).EQ.2 .OR. INT(PNUMS(8)).EQ.3) THEN

!           *** Explicit scheme in frequency space. Energy near the ***
!           *** blocking point is removed from the spectrum based   ***
!           *** on CFL criterion                                    ***
!
!TIMG                           CALL SWTSTA(120)
                           CALL SOLMT1  (IDCMIN             ,IDCMAX             ,&
                           &AC2                ,SWMATR(1,1,JMATR)  ,&
                           &SWMATR(1,1,JMATD)  ,SWMATR(1,1,JMATU)  ,&
                           &SWMATR(1,1,JMATL)  ,&
                           &ISSTOP             ,&
                           &LSWMAT(1,1,JABLK)  ,IDDLOW             ,&
                           &IDDTOP                                 )
!TIMG                           CALL SWTSTO(120)

                        END IF

                     ELSE

!         *** No current. Only implicit scheme in directional space  ***
!         *** Solve the tri-diagonal matrix with Thomas algorithm    ***
!
!TIMG                        CALL SWTSTA(120)
                        CALL SOLMAT (IDCMIN            ,IDCMAX             ,&
                        &AC2                ,SWMATR(1,1,JMATR)  ,&
                        &SWMATR(1,1,JMATD)  ,SWMATR(1,1,JMATU)  ,&
                        &SWMATR(1,1,JMATL)&
                        &)
!TIMG                        CALL SWTSTO(120)

                     END IF

!       *** test output ***

                     IF ( TESTFL .AND. ITEST .GE. 90 ) THEN
                        WRITE (PRTEST, *) ' solution vector'
                        WRITE (PRTEST, *) ' IS ID1 ID2     action densities'
                        DO IS = 1, MSC
                           ID_MIN = IDCMIN(IS)
                           ID_MAX = IDCMAX(IS)
                           WRITE(PRINTF,"(3I4,600(1X,E12.4))") IS, ID_MIN, ID_MAX,&
                           &(AC2(MOD(IDDUM-1+MDC,MDC)+1, IS, KCGRD(1)),&
                           &IDDUM = ID_MIN, ID_MAX)
                        ENDDO
                     END IF

!       *** if negative action density occur rescale with a factor ***
!       *** only the sector computed is rescaled !!                ***
!
!TIMG                     CALL SWTSTA(121)
                     IF (BRESCL) CALL RESCALE(AC2, ISSTOP, IDCMIN, IDCMAX, NRSCAL)
!TIMG                     CALL SWTSTO(121)
!
!       calculate propagation, generation, dissipation, redistribution
!       leak and radiation stress in present grid point
!
!TIMG                     CALL SWTSTA(124)
                     IF ( LADDS )&
                     &CALL ADDDIS (COMPDA(1,JDISS)    ,COMPDA(1,JLEAK)    ,&
                     &AC2                ,LSWMAT(1,1,JABIN)  ,&
                     &SWMATR(1,1,JDIS0)  ,SWMATR(1,1,JDIS1)  ,&
                     &SWMATR(1,1,JGEN0)  ,SWMATR(1,1,JGEN1)  ,&
                     &SWMATR(1,1,JRED0)  ,SWMATR(1,1,JRED1)  ,&
                     &SWMATR(1,1,JTRA0)  ,SWMATR(1,1,JTRA1)  ,&
                     &SWMATR(1,1,JMATL)  ,SWMATR(1,1,JMATU)  ,&
                     &SWMATR(1,1,JMAT5)  ,SWMATR(1,1,JMAT6)  ,&
                     &COMPDA(1,JDSXB)    ,&
                     &COMPDA(1,JDSXS)    ,&
                     &COMPDA(1,JDSXW)    ,&
                     &COMPDA(1,JDSXV)    ,COMPDA(1,JDSXT)    ,&
                     &COMPDA(1,JDSXM)    ,&
                     &COMPDA(1,JDSXI)    ,&
                     &COMPDA(1,JDSXL)    ,&
                     &COMPDA(1,JGSXW)    ,COMPDA(1,JGENR)    ,&
                     &COMPDA(1,JRSXQ)    ,COMPDA(1,JRSXT)    ,&
                     &COMPDA(1,JRSXB)    ,COMPDA(1,JRSXC)    ,&
                     &COMPDA(1,JREDS)    ,&
                     &COMPDA(1,JTSXG)    ,COMPDA(1,JTSXT)    ,&
                     &COMPDA(1,JTSXS)    ,COMPDA(1,JTRAN)    ,&
                     &SWMATR(1,1,JLEK1)  ,COMPDA(1,JRADS)    ,&
                     &SPCSIG&
                     &)
!TIMG                     CALL SWTSTO(124)
!
!       limit the change of the spectrum
!
!TIMG                     CALL SWTSTA(122)
                     IF (PNUMS(20).LT.100.) THEN
                        IF (IWIND.NE.4 .OR. NSTATC.NE.1) THEN
!             default limiter
                           CALL PHILIM (AC2, SWMATR(1,1,JAOLD),&
                           &CGO, KWAVE,&
                           &SPCSIG, LSWMAT(1,1,JABIN),&
                           &ISLMIN, NFLIM,&
                           &QBLOC)
                        ELSE
!             Hersbach and Janssen (1999) limiter
                           CALL HJLIM (AC2, SWMATR(1,1,JAOLD),&
                           &CGO, KWAVE,&
                           &SPCSIG, LSWMAT(1,1,JABIN),&
                           &ISLMIN, NFLIM,&
                           &QBLOC, COMPDA(1,JUSTAR))
                        END IF
                     END IF
!TIMG                     CALL SWTSTO(122)
!
!       *** reduce the computed energy density if the value is  ***
!       *** larger then the limit value as computed in SWIND    ***
!       *** in case of first or second generation mode          ***
!
!TIMG                     CALL SWTSTA(123)
                     IF ( IWIND .EQ. 1 .OR. IWIND .EQ. 2 )&
                     &CALL WINDP3 (ISSTOP, ALIMW, AC2, GROWW, IDCMIN, IDCMAX )
!TIMG                     CALL SWTSTO(123)
!
!       *** test output ***

                     IF ( TESTFL .AND. ITEST .GE. 70 ) THEN
                        WRITE (PRINTF, *) ' action densities after adaptations'
                        WRITE (PRTEST, *) ' IS ID1 ID2     action densities'
                        DO IS = 1, MSC
                           ID_MIN = IDCMIN(IS)
                           ID_MAX = IDCMAX(IS)
                           WRITE(PRINTF,"(3I4,600(1X,E12.4))") IS, ID_MIN, ID_MAX,&
                           &(AC2(MOD(IDDUM-1+MDC,MDC)+1, IS, KCGRD(1)),&
                           &IDDUM = ID_MIN, ID_MAX)
                        ENDDO
                     END IF

                     END IF

                  END IF

!     End of the subroutine SWOMPU
                  RETURN
               end subroutine SWOMPU
!****************************************************************

               SUBROUTINE SWPRSET (SPCSIG,SPCDIR,TCOLL)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE OCPCOMM4
                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE SwanIEM, only: ntf, dfiem, sflog

                  IMPLICIT NONE(TYPE, EXTERNAL)
                  LOGICAL, INTENT(IN) :: TCOLL


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
!     40.80: Marcel Zijlema
!
!  1. Updates
!
!     40.80, Oct. 07: New subroutine
!
!  2. Purpose
!
!     Print all the settings used in SWAN run
!
!  4. Argument variables
!
!     SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
!     SPCSIG: Relative frequencies in computational domain in sigma-space

                  REAL SPCSIG(MSC), SPCDIR(MDC,6)

!  6. Local variables
!
!     ICMX  :     stencil size
!     IENT  :     number of entries in this subroutine
!     IS    :     loop counter

                  INTEGER, SAVE :: IENT = 0
                  INTEGER ICMX, IS
                  INTEGER II ! dummy variable

                  REAL QMAX

!  9. Subroutines calling
!
!     SWCOMP
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SWPRSET')

                  WRITE(PRINTF,"(/, '----------------------------------------------------------------' ,/, ' COMPUTATIONAL PART OF ', A ,/, '----------------------------------------------------------------' ,/)") 'SWAN'

                  IF ( ONED ) THEN
                     WRITE(PRINTF,*) 'One-dimensional mode of SWAN is activated'
                  ENDIF

                  IF (OPTG.NE.5) THEN
                     WRITE(PRINTF,"(' Gridresolution : MXC ',I12 ,' MYC ',I12)") MXC,MYC
                     WRITE(PRINTF,"(' : MCGRD ',I12)") MCGRD
                     WRITE(PRINTF,"(' : MSC ',I12 ,' MDC ',I12)") MSC,MDC
                  ELSE
                     WRITE(PRINTF,"(' Gridresolution : MSC ',I12 ,' MDC ',I12)") MSC,MDC
                  ENDIF
                  WRITE(PRINTF,"(' : MTC ',I12)") MTC
                  WRITE(PRINTF,"(' : NSTATC ',I12 ,' ITERMX',I12)") NSTATC, ITERMX
                  WRITE(PRINTF,"(' Propagation flags : ITFRE ',I12 ,' IREFR ',I12)") ITFRE,IREFR
                  WRITE(PRINTF,"(' Source term flags : IBOT ',I12 ,' ISURF ',I12)") IBOT,ISURF
                  WRITE(PRINTF,"(' : IWCAP ',I12 ,' IWIND ',I12)") IWCAP,IWIND
                  WRITE(PRINTF,"(' : ITRIAD ',I12 ,' IQUAD ',I12)") ITRIAD,IQUAD
                  WRITE(PRINTF,"(' : IBRAG ',I12 ,' IQCM ',I12)") IBRAG,IQCM
                  WRITE(PRINTF,"(' : IVEG ',I12 ,' ITURBV',I12)") IVEG, ITURBV
                  WRITE(PRINTF,"(' : IMUD ',I12 ,' IICE ',I12)") IMUD, IICE
                  IF (OPTG.NE.5) WRITE(PRINTF,"(' Spatial step : DX ',E12.4,' DY ',E12.4)") DX,DY
                  IF ( LSRFB .AND. ntf.GT.0 .AND. .NOT.sflog ) THEN
                     WRITE(PRINTF,"(' Spectral bin : df ',E12.4,' DDIR ',E12.4)") dfiem, DDIR/DEGRAD
                  ELSE
                     WRITE(PRINTF,"(' Spectral bin : df/f ',E12.4,' DDIR ',E12.4)") EXP(ALOG(SHIG/SLOW)/REAL(MSC-1))-1.,&
                     &DDIR/DEGRAD
                  ENDIF
                  WRITE(PRINTF,"(' Physical constants : GRAV ',E12.4,' RHO ',E12.4)") GRAV, RHO
                  WRITE(PRINTF,"(' Wind input : WSPEED ',E12.4,' DIR ',E12.4)") U10 , WDIC/DEGRAD
                  WRITE(PRINTF,'(A,F5.2)')'                      : ICEWIND ',ICEWIND
                  WRITE(PRINTF,"(' Tail parameters : E(f) ',E12.4,' E(k) ',E12.4)") PWTAIL(1),PWTAIL(2)
                  WRITE(PRINTF,"(' : A(f) ',E12.4,' A(k) ',E12.4)") PWTAIL(3),PWTAIL(4)
                  WRITE(PRINTF,"(' Accuracy parameters : DREL ',E12.4,' NPNTS ',E12.4)") PNUMS(1), PNUMS(4)
                  IF (PNUMS(21).EQ.0.) THEN
                     WRITE(PRINTF,"(' : DHOVAL ',E12.4,' DTOVAL',E12.4)") PNUMS(15),PNUMS(16)
                  ELSE IF (PNUMS(21).EQ.1.) THEN
                     WRITE(PRINTF,"(' : DHABS ',E12.4,' CURVAT',E12.4)") PNUMS(2),PNUMS(15)
                  END IF
                  WRITE(PRINTF,"(' : GRWMX ',E12.4)") PNUMS(20)
                  WRITE(PRINTF,"(' Drying/flooding : LEVEL ',E12.4,' DEPMIN',E12.4)") WLEV, DEPMIN
                  IF (BNAUT) THEN
                     WRITE (PRINTF,"(' The ',A9, ' convention for wind and wave directions is used')") 'nautical '
                  ELSE
                     WRITE (PRINTF,"(' The ',A9, ' convention for wind and wave directions is used')") 'Cartesian'
                  ENDIF
                  IF (OPTG.EQ.5) THEN
                     WRITE(PRINTF,"(' Scheme for geographic propagation is ',A6)") 'BSBT  '
                  ELSEIF (PROPSC.EQ.3) THEN
                     WRITE(PRINTF,"(' Scheme for geographic propagation is ',A6)") 'S&L   '
                     ICMX = 13
                  ELSEIF (PROPSC.EQ.2) THEN
                     WRITE(PRINTF,"(' Scheme for geographic propagation is ',A6)") 'SORDUP'
                     ICMX = 7
                  ELSE
                     WRITE(PRINTF,"(' Scheme for geographic propagation is ',A6)") 'BSBT  '
                     ICMX = 5
                  ENDIF
                  IF (OPTG.NE.5) WRITE(PRINTF,"(' Scheme geogr. space : PROPSC ',I12 ,' ICMAX ',I12)") PROPSC, ICMX
                  WRITE(PRINTF,"(' Scheme spectral space: CSS ',E12.4,' CDD ',E12.4)") PNUMS(7), PNUMS(6)

                  IF ( (DYNDEP .OR. ICUR.EQ.1) .AND. INT(PNUMS(8)).EQ.1 ) THEN
                     WRITE(PRINTF,*) 'Solver is SIP'
                     WRITE(PRINTF,"(' : EPS2 ',E12.4,' OUTPUT',I12)") PNUMS(12), INT(PNUMS(13))
                     WRITE(PRINTF,"(' : NITER ',I12)") INT(PNUMS(14))
                  ENDIF

                  IF (ICUR.GT.0) THEN
                     WRITE (PRINTF,"(' Current is ', A3)") 'on'
                  ELSE
                     WRITE (PRINTF,"(' Current is ', A3)") 'off'
                  ENDIF

                  IF (IQUAD.GT.0) THEN
                     WRITE(PRINTF,"(' Quadruplets : IQUAD ',I12)") IQUAD
                     IF (IQUAD.LE.3 .OR. IQUAD.EQ.8) THEN
                        WRITE(PRINTF,"(' : LAMBDA ',E12.4, ' CNL4 ',E12.4)") PQUAD(1), PQUAD(2)
                        WRITE(PRINTF,"(' : CSH1 ',E12.4, ' CSH2 ',E12.4)") PQUAD(3), PQUAD(4)
                        WRITE(PRINTF,"(' : CSH3 ',E12.4)") PQUAD(5)
                     ENDIF
                     WRITE(PRINTF,"(' Maximum Ursell nr for Snl4 : ',E12.4)") PTRIAD(3)
                  ELSEIF (IGEN.LT.4) THEN
                     WRITE (PRINTF, *) 'Quadruplets is off'
                  ENDIF
                  IF (ITRIAD.GT.0) THEN
                     WRITE(PRINTF,"(' Triads : ITRIAD ',I12 , ' TRFAC ',E12.4)") ITRIAD, PTRIAD(1)
                     IF (IBIPH.EQ.1) THEN
                        IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
                           WRITE(PRINTF,"(' : CUTFR ',E12.4, ' URCRI ',E12.4)") PTRIAD(2), PTRIAD(4)
                        ELSEIF (ITRIAD.EQ.3) THEN
                           WRITE(PRINTF,"(' : URCRI ',E12.4)") PTRIAD(4)
                        ELSEIF (ITRIAD.EQ.5) THEN
                           WRITE(PRINTF,"(' : POWER ',E12.4, ' URCRI ',E12.4)") PTRIAD(2), PTRIAD(4)
                        ENDIF
                     ELSE IF (IBIPH.EQ.3) THEN
                        IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
                           WRITE(PRINTF,"(' : CUTFR ',E12.4, ' LPAR ',E12.4)") PTRIAD(2), PTRIAD(9)
                        ELSEIF (ITRIAD.EQ.3) THEN
                           WRITE(PRINTF,"(' : LPAR ',E12.4)") PTRIAD(9)
                        ELSEIF (ITRIAD.EQ.5) THEN
                           WRITE(PRINTF,"(' : POWER ',E12.4, ' LPAR ',E12.4)") PTRIAD(2), PTRIAD(9)
                        ENDIF
                     ELSE
                        IF (ITRIAD.EQ.1 .OR. ITRIAD.EQ.11) THEN
                           WRITE(PRINTF,"(' : CUTFR ',E12.4)") PTRIAD(2)
                        ELSEIF (ITRIAD.EQ.5) THEN
                           WRITE(PRINTF,"(' : POWER ',E12.4)") PTRIAD(2)
                        ENDIF
                     ENDIF
                     WRITE(PRINTF,"(' Minimum Ursell nr for Snl3 : ',E12.4)") PTRIAD(5)
                     IF (ITRIAD.EQ.1) THEN
                        WRITE (PRINTF,*) 'extended LTA is employed'
                     ELSE IF (ITRIAD.EQ.2) THEN
                        WRITE (PRINTF,*) 'SPB (Becq-Girard et al, 1999) is employed'
                     ELSE IF (ITRIAD.EQ.3) THEN
                        WRITE (PRINTF,*) 'FTIM is employed with parametrized biphase'
                     ELSE IF (ITRIAD.EQ.5) THEN
                        WRITE (PRINTF,*) 'DCTA (Booij et al, 2009) is employed'
                     ELSE IF (ITRIAD.EQ.11) THEN
                        WRITE (PRINTF,*) 'LTA (Eldeberky, 1996) is employed'
                     ENDIF
                     IF (ITRIAD.NE.5) THEN
                        IF (.NOT.PTRIAD(8).NE.-1.) THEN
                           IF (FULCIR) THEN
                              WRITE(PRINTF,"(' Directional integration width:',E12.4)") 360.
                           ELSE
                              WRITE(PRINTF,"(' Directional integration width:',E12.4)") (SPCDIR(MDC,1)-SPCDIR(1,1))/DEGRAD
                           ENDIF
                        ELSEIF (PTRIAD(8).NE.0.) THEN
                           WRITE(PRINTF,"(' Directional integration width:',E12.4)") PTRIAD(8)
                        ELSE
                           WRITE(PRINTF,*)'Original collinear approximation is used'
                        ENDIF
                     ELSE IF (.NOT.TCOLL) THEN
                        WRITE(PRINTF,*) 'Noncollinear effects are included'
                     ENDIF
                  ELSE
                     WRITE (PRINTF, *) 'Triads is off'
                  ENDIF
                  IF (ITRIAD.NE.2) THEN
                     IF (IBIPH.EQ.1) THEN
                        WRITE (PRINTF,*) 'Biphase based on Eldeberky (1996)'
                     ELSEIF (IBIPH.EQ.2) THEN
                        WRITE (PRINTF,*) 'Biphase based on Saprykina et al. (2017)'
                     ELSEIF (IBIPH.EQ.3) THEN
                        WRITE (PRINTF,*) 'Biphase based on De Wit (2022)'
                     ENDIF
                  ENDIF
                  IF (ITRIAD.NE.5) THEN
                     IF (PTRIAD(10).EQ.1.) THEN
                        WRITE(PRINTF,"(' Transfer function based on Freilich and Guza (1984)')")
                     ELSEIF (PTRIAD(10).EQ.2.) THEN
                        WRITE(PRINTF,"(' Transfer function based on Madsen and Sorensen (1993)')")
                     ELSEIF (PTRIAD(10).EQ.3.) THEN
                        WRITE(PRINTF,"(' Transfer function based on Bredmose et al (2005)')")
                     ELSEIF (PTRIAD(10).EQ.4.) THEN
                        WRITE(PRINTF,"(' Transfer function based on Akrish et al (2024)')")
                     ENDIF
                  ENDIF
                  IF (IBRAG.GT.0) THEN
                     WRITE(PRINTF,"(' Bragg scattering : IBRAG ',I12 , ' CUTOFF',E12.4)") IBRAG, PBRAG(2)
                  ELSE
                     WRITE (PRINTF, *) 'Bragg scattering is off'
                  ENDIF
                  IF (IQCM.GT.0) THEN
                     IF (IQCM.EQ.1) THEN
                        WRITE(PRINTF,*) 'QC scattering due to medium variations'
                     ELSEIF (IQCM.EQ.2) THEN
                        WRITE(PRINTF,*) 'Wave-current interaction'
                     ENDIF
                     QMAX = MIN(PSCAT(1),PSCAT(2))
                     WRITE(PRINTF,"(' QC source term : QMAX ',E12.4)") QMAX
                  ELSE
                     WRITE (PRINTF, *) 'QC scattering is off'
                  ENDIF
                  IF (LSRFB) THEN
                     IF (ntf.LT.0) THEN
                        WRITE(PRINTF,"(' Surfbeat (bound) : DF ',E12.4)") dfiem
                     ELSE
                        WRITE(PRINTF,"(' Surfbeat (reflected) : DF ',E12.4, ' NTF ',I12)") dfiem, ntf
                     ENDIF
                  ELSE
                     WRITE (PRINTF, *) 'Surfbeat (IEM) is off'
                  ENDIF
                  IF (IBOT.EQ.2) THEN
                     WRITE(PRINTF,"(' Collins (`72) : CFW ',E12.4,' CFC ',E12.4)") PBOT(2), PBOT(1)
                  ELSEIF (IBOT.EQ.3) THEN
                     WRITE(PRINTF,"(' Madsen et al. (`84) : MF ',E12.4,' KN ',E12.4)") PBOT(4), PBOT(5)
                  ELSEIF (IBOT.EQ.1) THEN
                     WRITE(PRINTF,"(' JONSWAP (`73) : GAMMA ',E12.4)") PBOT(3)
                  ELSEIF (IBOT.EQ.4) THEN
                     WRITE(PRINTF,"(' JONSWAP (`73) : GAMMA1 ',E12.4,' GAMMA2',E12.4)") PBOT(6), PBOT(7)
                     WRITE(PRINTF,"(' : DSPR1 ',E12.4,' DSPR2 ',E12.4)") PBOT(8), PBOT(9)
                  ELSEIF (IBOT.EQ.5) THEN
                     WRITE(PRINTF,"(' RIPPLES (`07) : CF ',E12.4)") PBOT(6)
                  ELSE
                     WRITE (PRINTF, *) 'Bottom friction is off'
                  ENDIF

                  IF (IVEG.EQ.1) THEN
                     WRITE(PRINTF,"(' Vegetation due to Dalrymple (1984)')")
                  ELSEIF (IVEG.EQ.2) THEN
                     WRITE(PRINTF,"(' Vegetation due to Jacobsen et al. (2019)')")
                  ELSE
                     WRITE (PRINTF, *) 'Vegetation is off'
                  ENDIF

                  IF (ITURBV.EQ.1) THEN
                     WRITE(PRINTF,"(' Turbulent viscosity : CTB ',E12.4,' TBCUR ',E12.4)") PTURBV(1), PTURBV(2)
                  ELSE
                     WRITE (PRINTF, *) 'Turbulence is off'
                  ENDIF

                  IF (IMUD.EQ.1) THEN
                     WRITE(PRINTF,"(' Mud Ng (2000) : RHOM ',E12.4,' KINVIS',E12.4)") PMUD(2), PMUD(3)
                  ELSE
                     WRITE (PRINTF, *) 'Fluid mud is off'
                  ENDIF

                  IF (IICE.EQ.1) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: CTGA with CICE'
                  ELSEIF (IICE.EQ.2) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: CTGA with CICE ADCICE'
                  ELSEIF (IICE.EQ.3) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: R19 with parameters:'
                     DO II=2,8 ! C0 is PSICE(2) etc.
                        WRITE(PRINTF,'(A,I1,A,E12.4)')'     C',II-2,' :: ',PSICE(II)
                     END DO
                  ELSEIF (IICE.EQ.4) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: D15 with parameter:'
                     WRITE(PRINTF,'(A,E12.4)')' Chf=    ',PSICE(1)
                  ELSEIF (IICE.EQ.5) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: M18 with parameter:'
                     WRITE(PRINTF,'(A,E12.4)')' Chf=    ',PSICE(1)
                  ELSEIF (IICE.EQ.6) THEN
                     WRITE(PRINTF,'(A)')'Sea ice: R21B with parameters:'
                     WRITE(PRINTF,'(A,E12.4)')' Chf=    ',PSICE(1)
                     WRITE(PRINTF,'(A,E12.4)')' npf=    ',PSICE(2)
                  ELSE
                     WRITE (PRINTF, *) 'Dissipation by sea ice is off'
                  ENDIF

                  IF (IWCAP.EQ.1) THEN
                     WRITE(PRINTF,'(2A,E12.4)')' W-cap Komen (`84)    : ',&
                     &'EMPCOF (CDS2): ',PWCAP(1)
                     WRITE(PRINTF,'(2A,E12.4)')' W-cap Komen (`84)    : ',&
                     &'APM (STPM)   : ',PWCAP(2)
                     WRITE(PRINTF,'(2A,E12.4)')' W-cap Komen (`84)    : ',&
                     &'POWST        : ',PWCAP(9)
                     WRITE(PRINTF,'(2A,E12.4)')' W-cap Komen (`84)    : ',&
                     &'DELTA        : ',PWCAP(10)
                     WRITE(PRINTF,'(2A,E12.4)')' W-cap Komen (`84)    : ',&
                     &'POWK         : ',PWCAP(11)
                  ELSEIF (IWCAP.EQ.2) THEN
                     WRITE(PRINTF,"(' W-cap Janssen (`90) : CFJANS ',E12.4, ' DELTA ',E12.4)") PWCAP(3),PWCAP(4)
                  ELSEIF (IWCAP.EQ.3) THEN
                     WRITE(PRINTF,"(' W-cap Longuet-Higgins: CFLHIG ',E12.4)") PWCAP(5)
                  ELSEIF (IWCAP.EQ.4) THEN
                     WRITE(PRINTF,"(' W-cap Battjes/Janssen: BJSTP ',E12.4, ' BJALF ',E12.4)") PWCAP(6), PWCAP(7)
                  ELSEIF (IWCAP.EQ.5) THEN
                     WRITE(PRINTF,"(' W-cap Battjes/Janssen: BJSTP ',E12.4, ' BJALF ',E12.4)") PWCAP(6), PWCAP(7)
                     WRITE(PRINTF,"(' : KCONV ',E12.4)") PWCAP(8)
                  ELSEIF (IWCAP.EQ.7) THEN
                     WRITE(PRINTF,"(' W-cap Alves-Banner : CDS2 ', E12.4,' BR ',E12.4)") PWCAP(1), PWCAP(12)
                     IF (IWCCUR.EQ.1) THEN
                        WRITE(PRINTF,"(' current-induced W-cap: CDS3 ',E12.4)") PWCAP(14)
                     ENDIF
                  ELSEIF (IWCAP.EQ.8) THEN
                     WRITE(PRINTF,"(' W-cap Babanin : A1 ', E12.4,' A2 ',E12.4)") A1SDS    , A2SDS
                     WRITE(PRINTF,"(' : P1 ', E12.4,' P2 ',E12.4)") P1SDS    , P2SDS
                     WRITE(PRINTF,"(' : CDSV ', E12.4)") CDSV
                     WRITE(PRINTF,"(' Babanin Sds: concave up behavior : ', L3 )") UPWARDS
                     WRITE(PRINTF,"(' DBYB Sin: tau calculated from vector: ', L3 )") VECTOR_TAU
                     WRITE(PRINTF,"(' DBYB Sin: true U10 used : ', L3 )") TRUE_U10
                     WRITE(PRINTF,"(' DBYB Sin: U10PROXY = ',F6.1,' * Ustar' )") WNDSCL
                     WRITE(PRINTF,"(' DBYB Sin: factor on Cdrag to counter wind bias:',F6.3)") CDFAC
                  ELSEIF (IGEN.LT.4) THEN
                     WRITE (PRINTF, *) 'Whitecapping is off'
                  ENDIF

                  IF (IWIND.GT.0) THEN
                     IF (IDRAG.EQ.1) THEN
                        WRITE(PRINTF, *) 'Wind drag is Wu'
                     ELSE IF (IDRAG.EQ.2) THEN
                        WRITE(PRINTF, *) 'Wind drag is fit'
                     ELSE IF (IDRAG.EQ.3) THEN
                        WRITE(PRINTF, *) 'Wind drag is swell'
                     ELSE IF (IDRAG.EQ.4) THEN
                        WRITE(PRINTF, *) 'Wind drag is Hwang'
                     ELSE IF (IDRAG.EQ.5) THEN
                        WRITE(PRINTF, *) 'Wind drag is Fan'
                     ELSE IF (IDRAG.EQ.6) THEN
                        WRITE(PRINTF, *) 'Wind drag is ECMWF'
                     ENDIF
                  ENDIF

                  IF (IWIND.EQ.3) THEN
                     WRITE (PRINTF, *) 'Snyder/Komen wind input'
                  ELSEIF (IWIND.EQ.4) THEN
                     WRITE (PRINTF, *) 'Janssen wind input'
                  ELSEIF (IWIND.EQ.5) THEN
                     WRITE (PRINTF, *) 'Yan/Westhuysen wind input'
                  ELSEIF (IWIND.EQ.8) THEN
                     WRITE (PRINTF, *) 'DBYB/Tsagareli/Rogers wind input'
                  ENDIF

                  IF (ISURF.EQ.1) THEN
                     WRITE(PRINTF,"(' Battjes Janssen (`78): ALPHA ',E12.4, ' GAMMA ',E12.4)") PSURF(1),PSURF(2)
                  ELSEIF (ISURF.EQ.2) THEN
                     WRITE(PRINTF,"(' Nelson (`94) : ALPHA ',E12.4, ' GAMmin',E12.4)") PSURF(1), PSURF(4)
                     WRITE(PRINTF,"(' GAMmax ',E12.4)") PSURF(5)
                  ELSEIF (ISURF.EQ.3) THEN
                     WRITE(PRINTF,"(' Ruessink et al (2003): ALPHA ',E12.4, ' A ',E12.4)") PSURF(1), PSURF(4)
                     WRITE(PRINTF,"(' B ',E12.4)") PSURF(5)
                  ELSEIF (ISURF.EQ.4) THEN
                     WRITE(PRINTF,"(' Thornton Guza (`83) : ALPHA ',E12.4, ' GAMMA ',E12.4)") PSURF(1), PSURF(4)
                     WRITE(PRINTF,"(' N ',E12.4)") PSURF(5)
                  ELSEIF (ISURF.EQ.6) THEN
                     WRITE(PRINTF,"(' beta-kd (2013) : ALPHA ',E12.4, ' GAMMA0',E12.4)") PSURF(1), PSURF(4)
                     WRITE(PRINTF,"(' A1 ',E12.4, ' A2 ',E12.4)") PSURF(5), PSURF(6)
                     WRITE(PRINTF,"(' A3 ',E12.4)") PSURF(7)
                  ELSEIF (ISURF.EQ.7) THEN
                     WRITE(PRINTF,"(' Surf asymmetry : ALPHA ',E12.4, ' GAMMA0',E12.4)") PSURF(1), PSURF(4)
                     WRITE(PRINTF,"(' A ',E12.4)") PSURF(5)
                  ELSE
                     WRITE (PRINTF, *) 'Surf breaking is off'
                  ENDIF

                  IF (LSETUP.GT.0) THEN
                     WRITE(PRINTF,"(' Set-up : SUPCOR ',E12.4)") PSETUP(2)
                  ELSEIF (IGEN.LT.4) THEN
                     WRITE (PRINTF, *) 'Set-up is off'
                  ENDIF

                  IF (IDIFFR.EQ.1) THEN
                     WRITE(PRINTF,"(' Diffraction : SMPAR ',E12.4, ' SMNUM ',I12)") PDIFFR(1), NINT(PDIFFR(2))
                  ELSEIF (IGEN.LT.4) THEN
                     WRITE (PRINTF, *) 'Diffraction is off'
                  ENDIF

                  IF ( IGEN.LT.4 ) THEN
                     WRITE(PRINTF,"(' Janssen (`89,`90) : ALPHA ',E12.4, ' KAPPA ',E12.4)") PWIND(14), PWIND(15)
                     WRITE(PRINTF,"(' Janssen (`89,`90) : RHOA ',E12.4, ' RHOW ',E12.4)") PWIND(16), PWIND(17)
                     WRITE(PRINTF,*)
                     WRITE(PRINTF,"(' 1st and 2nd gen. wind: CF10 ',E12.4, ' CF20 ',E12.4)") PWIND(1), PWIND(2)
                     WRITE(PRINTF,"(' : CF30 ',E12.4, ' CF40 ',E12.4)") PWIND(3), PWIND(4)
                     WRITE(PRINTF,"(' : CF50 ',E12.4, ' CF60 ',E12.4)") PWIND(5), PWIND(6)
                     WRITE(PRINTF,"(' : CF70 ',E12.4, ' CF80 ',E12.4)") PWIND(7), PWIND(8)
                     WRITE(PRINTF,"(' : RHOAW ',E12.4, ' EDMLPM',E12.4)") PWIND(9), PWIND(10)
                     WRITE(PRINTF,"(' : CDRAG ',E12.4, ' UMIN ',E12.4)") PWIND(11), PWIND(12)
                     WRITE(PRINTF,"(' : LIM_PM ',E12.4)") PWIND(13)
                  ENDIF

                  WRITE(PRINTF,*)
                  IF ( ITEST .GT. 2 )  THEN
                     DO IS = 1, MSC
                        WRITE(PRINTF,*)' IS and SPCSIG(IS)    :',IS,SPCSIG(IS)
                     ENDDO
                     WRITE(PRINTF,*)
                  ENDIF

                  RETURN
               end subroutine SWPRSET
!****************************************************************

               SUBROUTINE SACCUR (DEP2       ,KGRPNT     ,&
               &XYTST      ,&
               &AC2        ,SPCSIG     ,ACCUR      ,&
               &HSACC1     ,HSACC2     ,SACC1      ,&
               &SACC2      ,DELHS      ,DELTM      ,&
               &I1MYC      ,I2MYC                  )
   USE swan_service_interfaces, ONLY: STRACE, EQREAL, STPNOW

!****************************************************************

                  USE OCPCOMM2
                  USE OCPCOMM3
                  USE OCPCOMM4
                  USE SWCOMM1
                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE M_PARALL
                  USE M_CONVERGENCE_SHARED, ONLY: &
                     HSMN2 => SACCUR_HSMN2, SMN2 => SACCUR_SMN2, &
                     NINDX => SACCUR_NINDX, WETGRD => SACCUR_WETGRD, &
                     IACCUR => SACCUR_IACCUR

                  IMPLICIT NONE(TYPE, EXTERNAL)

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer(s): R.C. Ris                                   |
!     |                Modified by R. Padilla and N. Booij        |
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
!     30.72: IJsbrand Haagsma
!     30.82: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.22: John Cazes and Tim Campbell
!     40.30: Marcel Zijlema
!     40.31: Tim Campbell and John Cazes
!     40.41: Marcel Zijlema
!
!  1. Update
!
!     30.72, Nov. 97: Declaration of DDIR, PI and PI2 removed because
!                     they are common and already declared in the
!                     INCLUDE file
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Aug. 99: Introduced a new overall measure for checking accuracy
!     30.82, Aug. 99: Changed all variables INDEX to INDX, since INDEX is reserved
!     40.03, Feb. 00: test level of message changed
!     40.22, Sep. 01: Added initialization of SACC1 and HSACC1 elements
!                     that are not wet points.
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Jul. 03: some improvements and corrections w.r.t. OpenMP
!     40.41, Aug. 04: add some test output for checking accuracy
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     To check the accuracy of the final computation. If a certain
!     accuracy has been reached then terminate the iteration process
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     AC2         action density
!     ACCUR       indicates percentage of grid points in
!                 which accuracy is reached
!     DELHS       difference in Hs between last 2 iterations
!     DELTM       difference in Tm01 between last 2 iterations
!     DEP2        depth
!     HSACC1      significant wave height at iter-1
!     HSACC2      significant wave height at iter
!     I1MYC       lower index for thread loop over y-grid row
!     I2MYC       upper index for thread loop over y-grid row
!     KGRPNT      indirect addressing
!     SACC1       mean wave frequency at iter-1
!     SACC2       mean wave frequency at iter
!     SPCSIG      relative frequencies in computational domain
!                 in sigma-space
!     XYTST       coordinates of test points

                  INTEGER I1MYC, I2MYC
                  REAL    ACCUR
                  INTEGER KGRPNT(MXC,MYC)
                  INTEGER XYTST(2*NPTST)
                  REAL    AC2(MDC,MSC,MCGRD)   ,&
                  &DEP2(MCGRD)          ,&
                  &HSACC1(MCGRD)        ,&
                  &HSACC2(MCGRD)        ,&
                  &SACC1(MCGRD)         ,&
                  &SACC2(MCGRD)         ,&
                  &DELHS(MCGRD)         ,&
                  &DELTM(MCGRD)
                  REAL    SPCSIG(MSC)

!  6. Local variables

                  INTEGER  IS    ,ID    ,IX,IY,IX1,IX2,IY1,IY2
                  INTEGER  WETGRDt, IACCURt

!     INDX  : counter
!     NINDX : number of gridpoints to average over

                  INTEGER INDX
                  INTEGER NINDXt

                  REAL    SME_T ,SME_B ,&
                  &TMREL ,HSREL ,TMABS ,HSABS
                  REAL    ARR(2)

!     HSMN2 : mean Hs over space at current iteration level
!     HSOVAL: Overall accuracy measure for Hs
!     SMN2  : mean Tm over space at current iteration level
!     TMOVAL: Overall accuracy measure for Tm

                  REAL    HSOVAL, TMOVAL
                  REAL    HSMN2t, SMN2t

!     LHEAD : logical indicating to write header
!     TSTFL : indicates whether grid point is a test point

                  LOGICAL LHEAD, TSTFL

                  INTEGER, SAVE :: IENT = 0
                  INTEGER II
                  REAL  ACS2, ACS3

!  7. Common blocks used
!
!     Module variables above are shared by all OpenMP threads.
!
!  8. Subroutines used
!
!     EQREAL           Boolean function which compares two REAL values
!     STRACE           Tracing routine for debugging
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     SWREDUCE         Performs a global reduction

!
!  9. Subroutines calling
!
!     SWCOMP (in SWANCOM1)
!
! 12. Structure
!
!   ---------------------------------------------------------------
!   If not the first iteration, the do
!     Set old values in dummy array
!   ---------------------------------------------------------------
!   Do for every x and y
!     Compute the mean action density frequency SACC1 and the
!     and the significant waveheight HSACC1
!   ---------------------------------------------------------------
!   If relative error for mean frequency or significant wave height
!      > certain given value then increase variable with one and
!      compute the relative number of gridpoints in where the accuracy
!      has not been reached
!   ---------------------------------------------------------------
!   End of the subroutine SACCUR
!   ----------------------------------------------------------------
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SACCUR')

!$OMP MASTER
!     Master thread initialize the shared variables
                  HSMN2  = 0.
                  SMN2  = 0.
                  NINDX  = 0
                  WETGRD = 0
                  IACCUR = 0
!$OMP END MASTER
!$OMP BARRIER

                  IF ( LMXF ) THEN
                     IX1 = 1
                  ELSE
                     IX1 = 1+IHALOX
                  END IF
                  IF ( LMXL ) THEN
                     IX2 = MXC
                  ELSE
                     IX2 = MXC-IHALOX
                  END IF
                  IF ( LMYF ) THEN
                     IY1 = I1MYC
                  ELSE
                     IY1 = 1+IHALOY
                  END IF
                  IF ( LMYL ) THEN
                     IY2 = I2MYC
                  ELSE
                     IY2 = MYC-IHALOY
                  END IF

!     *** If the computation is non steady : check the gridpoints ***
!     *** at the different "timesteps" if they are still the same ***
!     *** then WETGRD is the same. If not: change this subroutine ***
!     ***                                                         ***
!     ***   +++++++++++++++++++     +++++++++++++++++++++         ***
!     ***   +++++++++++++++++++     +++++++++++++++++++++         ***
!     ***   ++++++++     ++++++     +++++++++      ++++++         ***
!     ***   +                 +     ++++                +         ***
!     ***   +                 +     +++                 +         ***
!     ***   +                 +     ++                  +         ***
!     ***   +                 +     +                   +         ***
!     ***   +       t=0       +     +      t=t+1        +         ***
!     ***   +                 +     +                   +         ***
!     ***   +++++++++++++++++++     +++++++++++++++++++++         ***
!     ***                                                         ***

                  WETGRDt = 0
                  do IX = IX1, IX2
                     do IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)
                        IF (DEP2(INDX) .GT. DEPMIN) THEN
                           HSACC1(INDX) = MAX( 1.E-20 , HSACC2(INDX) )
                           SACC1(INDX)  = MAX( 1.E-20 , SACC2(INDX)  )
                           WETGRDt = WETGRDt + 1
!       Added to initialize HSACC1 and SACC1 values.
                        ELSE
                           HSACC1(INDX) = 0.
                           SACC1(INDX)  = 0.
                        END IF
                     end do
                  end do

!     *** first criterion to terminate the iteration process ***
!     ***                                                    ***
!     *** RELATIVE error :                                   ***
!     ***               Hs2  - Hs1      Tm2 - Tm1            ***
!     ***      DREL  =  ----------  and ----------           ***
!     ***                   Hs1            Tm1               ***
!     ***                                                    ***
!     *** ABSOLUTE error :                                   ***
!     ***                                                    ***
!     ***      DHABS  =  Hs2  - Hs1 < PNUMS(2)               ***
!     ***      DTABS  =  Tm2  - Tm1 < PNUMS(3)               ***
!     ***                                                    ***

                  do IX = IX1, IX2
                     do IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)

!       *** Compute the mean ENERGY DENSITY frequency and    ***
!       *** significant waveheight over the full spectrum    ***
!       *** per gridpoint                                    ***

                        IF (DEP2(INDX) .GT. DEPMIN) THEN
                           SME_T  = 0.
                           SME_B  = 0.
                           do IS = 1, MSC
                              do ID = 1, MDC
                                 ACS2  = SPCSIG(IS)**2 * AC2(ID,IS,INDX)
                                 ACS3  = SPCSIG(IS) * ACS2
                                 SME_B = SME_B + ACS2
                                 SME_T = SME_T + ACS3
                              end do
                           end do
                           SME_B = SME_B * FRINTF * DDIR
                           SME_T = SME_T * FRINTF * DDIR

!         *** mean frequency and significant wave height per gridpoint ***

                           IF ( SME_B .LE. 0. ) THEN
                              SME_B = 1.E-20
                              SACC2(INDX) = 1.E-20
                              HSACC2(INDX) = 1.E-20
                           ELSE
                              SACC2(INDX) = MAX ( 1.E-20 , (SME_T / SME_B) )
                              HSACC2(INDX) = MAX ( 1.E-20 , (4. * SQRT(SME_B)) )
                           END IF
                        END IF
                     end do
                  end do

!     *** the mean significant waveheight and the mean  ***
!     *** relative frequency over the gridpoints which  ***
!     *** depth is larger than 0 m.                     ***
!     *** The amount of gridpoints is denoted with the  ***
!     *** variable : NINDX                              ***
!     *** These values are used to compute the SRELF    ***
!     *** and the HSRELF instead of SACC2 and HSACC2    ***
!
!     Note that initialization of ACCUR is not needed since it is
!     not being summed upon
                  IACCURt = 0

!     Calculate the mean Hs and Tm over all wet gridpoints. These means
!     are then used as an overall accuracy measure.

                  HSMN2t= 0.
                  SMN2t= 0.
                  NINDXt= 0

                  do IX = IX1, IX2
                     do IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)
                        IF (DEP2(INDX).GT.DEPMIN) THEN
                           HSMN2t = HSMN2t + HSACC2(INDX)
                           SMN2t  =  SMN2t +  SACC2(INDX)
                           NINDXt = NINDXt + 1
                        END IF
                     end do
                  end do

!     Global sum of NINDX
!$OMP ATOMIC
                  NINDX = NINDX + NINDXt
!$OMP ATOMIC
                  HSMN2 = HSMN2 + HSMN2t
!$OMP ATOMIC
                  SMN2 = SMN2 + SMN2t

!$OMP BARRIER
!$OMP MASTER
                  ARR(1) = HSMN2
                  ARR(2) = SMN2
                  CALL SWREDUCE( ARR, 2, SWSUM )
                  CALL SWREDUCE( NINDX, 1, SWSUM )
!MPI                  IF (STPNOW()) RETURN
                  HSMN2 = ARR(1) / REAL(NINDX)
                  SMN2 = ARR(2) / REAL(NINDX)
!$OMP END MASTER
!$OMP BARRIER
!
!     Calculate a set of accuracy parameters based on relative, absolute
!     and overall accuracy measures for Hs and Tm

                  LHEAD=.TRUE.
                  do IX = IX1, IX2
                     do IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)

!       --- determine whether the point is a test point

                        TSTFL = .FALSE.
                        IF (NPTST.GT.0) THEN
                           do II = 1, NPTST
                              IF (IX.EQ.XYTST(2*II-1) .AND. &
                              &IY.EQ.XYTST(2*II)) TSTFL = .TRUE.
                           end do
                        END IF

                        IF ( DEP2(INDX) .GT. DEPMIN ) THEN
                           TMREL  = ABS ( SACC2(INDX) - SACC1(INDX) ) /&
                           &SACC1(INDX)
                           TMABS  = ABS ( ( PI2/SACC2(INDX)) - (PI2/SACC1(INDX)) )
                           TMOVAL = ABS ( SACC2(INDX) - SACC1(INDX) ) / SMN2

                           HSREL  = ABS ( HSACC2(INDX) - HSACC1(INDX) ) /&
                           &HSACC1(INDX)
                           HSABS  = ABS ( HSACC2(INDX) - HSACC1(INDX) )
                           HSOVAL = ABS ( HSACC2(INDX) - HSACC1(INDX) ) / HSMN2

                           IF (EQREAL(SACC1(INDX),1.E-20) .OR.&
                           &EQREAL(SACC2(INDX),1.E-20) ) THEN
                              DELTM(INDX) = 0.
                           ELSE
                              DELTM(INDX) = TMABS
                           END IF
                           DELHS(INDX) = HSABS

!         *** gridpoint in which mean period and wave height ***
!         *** have reached required accuracy                 ***

                           IF ( ITEST .GE. 30 .AND. TESTFL) THEN
                              WRITE(PRINTF,"(' SACCUR: SA2 SA1 HSA2 HSA1 :',4E12.4)") SACC2(INDX), SACC1(INDX),&
                              &HSACC2(INDX), HSACC1(INDX)
                              WRITE(PRINTF,"(' SACCUR: TMREL HSREL TMABS HSABS :',4E12.4)") TMREL, HSREL, TMABS, HSABS
                           ENDIF

                           IF ( (TMREL .LE. PNUMS(1) .OR. TMOVAL .LE. PNUMS(16)) .AND.&
                           &(HSREL .LE. PNUMS(1) .OR. HSOVAL .LE. PNUMS(15)) ) THEN
                              IACCURt = IACCURt + 1
                           END IF

                           IF (TSTFL) THEN
                              IF (LHEAD) WRITE(PRINTF,"(25X,'dHrel ','dHoval ', 'dTm01rel ','dTm01oval ')")
                              WRITE(PRINTF,"(1X,SS,'(IX,IY)=(',I5,',',I5,')',' ', 1PE13.6E2,' ',1PE13.6E2,' ',1PE13.6E2,' ', 1PE13.6E2)") IX+MXF-2, IY+MYF-2, HSREL, HSOVAL,&
                              &TMREL, TMOVAL
                              LHEAD=.FALSE.
                           END IF
                        ELSE
!         *** otherwise set arrays equal 0 ***
                           DELTM(INDX) = 0.0
                           DELHS(INDX) = 0.0
                        END IF

!       Test output at test points

                        IF ( ITEST .GE. 30 .AND. TESTFL) THEN
                           WRITE(PRINTF,"(' SACCUR: TMREL, TMABS, TMOVAL :',3E12.4)") TMREL, TMABS, TMOVAL
                           WRITE(PRINTF,"(' SACCUR: HSREL, HSABS, HSOVAL :',3E12.4)") HSREL, HSABS, HSOVAL
                        END IF

                     end do
                  end do

!     Global sum of IACCUR and WETGRD
!$OMP ATOMIC
                  IACCUR = IACCUR + IACCURt
!$OMP ATOMIC
                  WETGRD = WETGRD + WETGRDt

!$OMP BARRIER
!$OMP MASTER

                  CALL SWREDUCE ( IACCUR, 1, SWSUM )
!MPI                  IF (STPNOW()) RETURN
                  ACCUR  = REAL(IACCUR) * 100. / REAL(NINDX)
!$OMP END MASTER
!$OMP BARRIER
!
!     *** test output ***
!
!$OMP MASTER
                  IF ( ITEST .GE. 30 ) THEN
                     WRITE(PRINTF,"(' SACCUR: PNUMS(1) DHABS DTABS :',3E12.4)") PNUMS(1), PNUMS(2), PNUMS(3)
                     WRITE(PRINTF,"(' SACCUR: WETGRD IACCUR ACCUR :',2I8,E12.4)") NINDX,IACCUR,ACCUR
                  END IF
!$OMP END MASTER
!
!     End of the subroutine SACCUR
                  RETURN
               end subroutine SACCUR

!****************************************************************

               SUBROUTINE INSAC (AC2      ,SPCSIG   ,DEP2     ,&
               &HSACC2   ,SACC2    ,KGRPNT   ,&
               &I1MYC    ,I2MYC              )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE OCPCOMM2
                  USE OCPCOMM3
                  USE OCPCOMM4
                  USE SWCOMM1
                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE M_PARALL

!   --|-----------------------------------------------------------|--
!     | Delft University of Technology                            |
!     | Faculty of Civil Engineering                              |
!     | Fluid Mechanics Section                                   |
!     | P.O. Box 5048, 2600 GA  Delft, The Netherlands            |
!     |                                                           |
!     | Programmer(s): R.C. Ris                                   |
!     |                Modified by R. Padilla and N. Booij        |
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
!     30.72: IJsbrand Haagsma
!     40.30: Marcel Zijlema
!     40.31: Tim Campbell and John Cazes
!     40.41: Marcel Zijlema
!
!  1. Update
!
!     30.72, Nov. 97: Declartion of DDIR removed because it is a common
!                     and already declared in the INCLUDE file
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.31, Jul. 03: some improvements and corrections w.r.t. OpenMP
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     To check the accuracy of the final computation. If a certain
!     accuracy has been reached then quit the iteration process
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space

                  REAL    SPCSIG(MSC)

!        IX          Counter of gridpoints in x-direction
!        IY          Counter of gridpoints in y-direction
!        IS          Counter of relative frequency band
!        ID          Counter of the spectral direction
!        ICMAX       Maximum counter for the points of the molecul
!        ITER        Number of iteration i.e. number of full sweeps
!        MXC         Maximum counter of gridppoints in x-direction
!        MYC         Maximum counter of gridppoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!
!        REALS:
!        ---------
!
!        DDIR        Spectral direction band width
!        DS          Width of the frequency band
!
!        one and more dimensional arrays:
!        ---------------------------------
!
!        AC2       4D    Action density as function of D,S,X,Y at time T
!        DEP2      2D    Depth
!        HSACC2    2D    Dummy array for the significant wave height
!                        (old value)
!        SACC2     2D    Dummy array for the mean frequency (old value)
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        ---
!
!     7. ERROR MESSAGES
!
!        ---
!
!     8. REMARKS
!
!     9. STRUCTURE
!
!   ---------------------------------------------------------------
!   If the first iteration, the do
!     Set old values in dummy array
!   ---------------------------------------------------------------
!   End of the subroutine INSAC
!   ----------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

                  INTEGER  IS    ,ID, IND, IX, IY, IX1, IX2, IY1, IY2
                  INTEGER  I1MYC, I2MYC

                  INTEGER  KGRPNT(MXC,MYC)
                  REAL     ACS2, ACS3, SME_T, SME_B

                  REAL     AC2(MDC,MSC,MCGRD)   ,&
                  &DEP2(MCGRD)          ,&
                  &HSACC2(MCGRD)        ,&
                  &SACC2(MCGRD)

                  INTEGER, SAVE :: IENT = 0

                  IF (LTRACE) CALL STRACE (IENT,'INSAC')

                  IF ( LMXF ) THEN
                     IX1 = 1
                  ELSE
                     IX1 = 1+IHALOX
                  END IF
                  IF ( LMXL ) THEN
                     IX2 = MXC
                  ELSE
                     IX2 = MXC-IHALOX
                  END IF
                  IF ( LMYF ) THEN
                     IY1 = I1MYC
                  ELSE
                     IY1 = 1+IHALOY
                  END IF
                  IF ( LMYL ) THEN
                     IY2 = I2MYC
                  ELSE
                     IY2 = MYC-IHALOY
                  END IF

                  do IX = IX1, IX2
                     do IY = IY1, IY2

!       *** Compute the mean ENERGY DENSITY frequency SACC2  ***
!       *** and the wavenumber HSACC2 average over the full  ***
!       *** spectrum per gridpoint                           ***

                        IND = KGRPNT(IX,IY)
                        IF (DEP2(IND) .GT. DEPMIN ) THEN
                           SME_T  = 0.
                           SME_B  = 0.
                           do IS = 1, MSC
                              do ID = 1, MDC
                                 ACS2 = SPCSIG(IS)**2 * AC2(ID,IS,IND)
                                 ACS3 = SPCSIG(IS) * ACS2
                                 SME_B = SME_B + ACS2
                                 SME_T = SME_T + ACS3
                              end do
                           end do
                           SME_B = SME_B * FRINTF * DDIR
                           SME_T = SME_T * FRINTF * DDIR

!         *** mean frequency and significant wave height ***
!         *** per gridpoint                               ***

                           IF ( SME_B .LE. 0. ) THEN
                              SACC2(IND)  = 1.E-20
                              HSACC2(IND) = 1.E-20
                           ELSE
                              SACC2(IND)  = MAX ( 1.E-20 , (SME_T / SME_B) )
                              HSACC2(IND) = MAX ( 1.E-20 , (4. * SQRT(SME_B)) )
                           END IF
                        ELSE
                           SACC2(IND)  = 0.
                           HSACC2(IND) = 0.
                        END IF
                     end do
                  end do

!     End of the subroutine INSAC
                  RETURN
               end subroutine INSAC

!****************************************************************

               SUBROUTINE ACTION (IDCMIN     ,IDCMAX     ,SPCSIG     ,&
               &AC2        ,CAX        ,CAY        ,&
               &CAS        ,CAD        ,IMATLA     ,&
               &IMATDA     ,IMATUA     ,IMATRA     ,&
               &IMAT5L     ,&
               &IMAT6U     ,ISCMIN     ,ISCMAX     ,&
               &IDDLOW     ,IDDTOP     ,ISSTOP     ,&
               &ANYBLK     ,ANYBIN     ,&
               &LEAKC1     ,AC1        ,&
               &DYNDEP     ,RDX        ,RDY        ,&
               &SWPDIR     ,IX         ,IY         ,&
               &KSX        ,KSY        ,&
               &XCGRID     ,YCGRID     ,&
               &ITER       ,KGRPNT     ,OBREDF     ,&
               &CAX1       ,CAY1       ,SPCDIR     ,&
               &CGO        ,TRAC0      ,TRAC1&
               &)
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_propagation, ONLY: SANDL, SORDUP, STRSD, STRSSB, STRSSI, STRSXY, SWFLXD

!****************************************************************

                  USE swan_time, ONLY: default_time_context
                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4


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
!     30.72: IJsbrand Haagsma
!     30.74: IJsbrand Haagsma (Include version)
!     33.08: W. Erick Rogers (some S&L scheme-related changes)
!     33.09: Nico Booij and Erick Rogers
!     33.10: Nico Booij and Erick Rogers
!     40.03: Nico Booij
!     40.08: Erick Rogers
!     40.09: Annette Kieftenburg
!     40.22: John Cazes and Tim Campbell
!     40.23: Marcel Zijlema
!     40.28: Annette Kieftenburg
!     40.41: Marcel Zijlema
!     40.85: Marcel Zijlema
!
!  1. Updates
!
!     30.74, Nov. 97: Prepared for version with INCLUDE statements
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Mar. 98: water level (WLEV2) and wave height (CHS) in comp. grid
!                     added as arguments (needed for SWTRCF)
!                     Call SWTRCF modified
!     33.08, July 98: some S&L scheme-related changes
!     33.09, Sept 99: changes re: the spherical coordinates
!     33.10, Jan. 00: changes re: the SORDUP scheme
!     40.09, May  00: Argument list SWTRCF modified
!     40.03, Apr. 00: integers LINK1 and LINK2 replaced by array LINK(1:MICMAX)
!     40.22, Sep. 01: Removed WAREA array.
!     40.22, Sep. 01: Changed array definitions to use the parameter
!                     MICMAX instead of ICMAX.
!     40.28, Dec. 01: Argument list SWTRCF modified
!     40.23, Aug. 02: Print of CPU times added
!     40.23, Nov. 02: call to SWFLXD added
!     40.08, Mar. 03: Dimensioning of RDX, RDX changed to be consistent
!                     with other subroutines
!     40.41, Aug. 04: call to SWTRCF removed because superfluous
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.85, Aug. 08: add transport for output purposes
!
!  2. Purpose
!
!     to determine the transport, refraction and the source terms
!     of the action balance equation
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!     SPCSIG: Relative frequencies in computational domain in sigma-space
!     XCGRID: Coordinates of computational grid in x-direction
!     YCGRID: Coordinates of computational grid in y-direction

                  REAL    SPCSIG(MSC)
                  REAL    XCGRID(MXC,MYC),    YCGRID(MXC,MYC)

!     IX          Counter of gridpoints in x-direction
!     IY          Counter of gridpoints in y-direction
!     ICMAX       Maximum counter for the points of the molecul
!     MXC         Maximum counter of gridppoints in x-direction
!     MYC         Maximum counter of gridppoints in y-direction
!     MSC         Maximum counter of relative frequency
!     MDC         Maximum counter of directional distribution
!     KSX         Dummy variable to get the right sign in the
!                 numerical difference scheme in X-direction
!                 depending on the sweep direction, KSX = -1 or +1
!     KSY         Dummy variable to get the right sign in the
!                 numerical difference scheme in Y-direction
!                 depending on the sweep direction, KSY = -1 or +1
!     IDTOT,ISTOT Maximum range between the counters in directional
!                 space and frequency space respectively
!     IDDLOW      Minimum counter per sweep taken over all
!                 frequencies
!     IDDTOP      Maximum counter per sweep taken over all
!                 frequencies
!     ISSTOP      Maximum counter per sweep taken over all
!                 frequencies
!
!
!     REALS:
!     ---------
!
!     DX,DY       Step size in x-direction and y-direction
!     DDX         Same as DX but with correct sign depending of the
!                 direction of the sweep
!     DDY         Same as DY but with correct sign depending of the
!                 direction of the sweep
!
!     one and more dimensional arrays:
!     ---------------------------------
!
!     AC2       4D    Action density as function of D,S,X,Y at time T
!     CAD       3D    Wave transport velocity in spectral direction as
!                     function of (ID,IS,IC)
!     CAS       3D    Wave transport velocity in frequency-direction as
!                     as function of (ID,IS,IC)
!     CAX       3D    Wave transport velocity in X-dirction as function
!                     (ID,IS,IC)
!     CAY       3D    Wave transport velocity in Y-dirction as function
!                     (ID,IS,IC)
!     IMATDA    2D    Coefficients of main diagonal of matrix
!     IMATLA    2D    Coefficients of lower diagonal of matrix
!     IMATUA    2D    Coefficients of upper diagonal of matrix
!     IMATRA    2D    Coefficients of right hand side
!     IMAT5L    2D    coefficient of lower diagonal in presence of
!                     a current (see routine SWSIP)
!     IMAT6U    2D    coefficient of upper diagonal in presence of
!                     a current (see routine SWSIP)
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRSXY
!     SORDUP
!     SANDL
!     STRSSI
!     STRSSB
!     STRSD
!     SWFLXD
!
!  9. Subroutines calling
!
!     SWOMPU
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
!     ------------------------------------------------------------------
!     *** transport in geographical space ***
!     Call STRSX to compute the propagation terms in X-direction
!     Call STRSY to compute the propagation terms in Y-direction
!     ------------------------------------------------------------------
!     *** transport in frequency space ***
!     If implicit scheme in frequency space then
!     ---
!       Call STRSSI  to compute the propagation terms in S-direction
!     ---
!     else if explicit scheme in frequency space and the energy near
!          the blocking point is removed from the spectrum then
!     ---
!       Call STRSSB to compute the propagation terms in S-direction
!     ---
!     endif
!     ------------------------------------------------------------------
!     IF no flux-limiting DO
!       Call STRSD to compute the propagation terms in directional domain
!     ELSE IF flux-limiting DO
!       Call SWFLXD
!     ---------------------------------------------------------
!     End of subroutine ACTION
!     ---------------------------------------------------------
!
!  13. Source text

                  INTEGER  ID, IDC, IDDLOW, IDDTOP, IDDUM, IS, ISC
                  INTEGER  ISSTOP, SWPDIR, ITER, IX, IY, KSX, KSY

                  LOGICAL           DYNDEP

                  INTEGER :: IDCMIN(MSC), IDCMAX(MSC)
                  INTEGER :: ISCMIN(MDC), ISCMAX(MDC)
                  INTEGER :: KGRPNT(MXC,MYC)

                  REAL  :: AC2(MDC,MSC,MCGRD)  ,AC1(MDC,MSC,MCGRD)
                  REAL  :: CAX(MDC,MSC,MICMAX)  ,CAY(MDC,MSC,MICMAX)
                  REAL  :: CAX1(MDC,MSC,MICMAX) ,CAY1(MDC,MSC,MICMAX)
                  REAL  :: CGO(MSC,MICMAX)
                  REAL  :: CAS(MDC,MSC,MICMAX) ,CAD(MDC,MSC,MICMAX)
                  REAL  :: IMATLA(MDC,MSC)     ,IMATDA(MDC,MSC)     ,&
                  &IMATUA(MDC,MSC)     ,IMATRA(MDC,MSC)     ,&
                  &IMAT5L(MDC,MSC)     ,IMAT6U(MDC,MSC)     ,&
                  &LEAKC1(MDC,MSC)
                  REAL  :: RDX(MICMAX)         ,RDY(MICMAX)         ,&
                  &OBREDF(MDC,MSC,2)   ,&
                  &SPCDIR(MDC,6)
                  REAL  :: TRAC0 (1:MDC,1:MSC,1:MTRNP)
                  REAL  :: TRAC1 (1:MDC,1:MSC,1:MTRNP)

                  LOGICAL  ANYBLK(MDC,MSC)     ,ANYBIN(MDC,MSC)

                  INTEGER, SAVE :: IENT = 0

                  IF (LTRACE) CALL STRACE (IENT,'ACTION')

!     *** set the coefficients in the arrays 0 ***

                  DO IS = 1, MSC
                     DO ID = 1, MDC
                        IMATLA(ID,IS) = 0.
                        IMATUA(ID,IS) = 0.
                        IMAT5L(ID,IS) = 0.
                        IMAT6U(ID,IS) = 0.
                     ENDDO
                  ENDDO

!     set leak coefficient at 0

                  DO ISC = 1, MSC
                     DO IDC = 1, MDC
                        LEAKC1(IDC,ISC) = 0.
                     ENDDO
                  ENDDO

!     *** set all transport coeff at 0 ***

                  TRAC0(1:MDC,1:MSC,1:MTRNP) = 0.
                  TRAC1(1:MDC,1:MSC,1:MTRNP) = 0.

!TIMG                  CALL SWTSTA(140)
!
!     *** Call propagation module in X-Y space  ***
!
!     --- depending on PROPSL, call STRSXY or other scheme
                  IF (PROPSL.EQ.3) THEN    ! use S&L scheme
                     CALL SANDL(ISSTOP   ,IDCMIN   ,IDCMAX   ,CGO     ,CAX    ,&
                     &CAY      ,AC2      ,AC1      ,IMATRA  ,IMATDA ,&
                     &RDX      ,RDY      ,CAX1     ,CAY1    ,SPCDIR ,&
                     &TRAC0    ,TRAC1    )
                  ELSE IF (PROPSL.EQ.2) THEN ! use SORDUP scheme
                     CALL SORDUP(ISSTOP   ,IDCMIN   ,IDCMAX   ,CAX      ,&
                     &CAY      ,AC2      ,IMATRA   ,IMATDA   ,&
                     &RDX      ,RDY      ,TRAC0    ,TRAC1    )
                  ELSE                     ! use BSBT scheme
                     CALL STRSXY(ISSTOP   ,IDCMIN   ,IDCMAX   ,CAX      ,&
                     &CAY      ,AC2      ,AC1      ,IMATRA   ,IMATDA   ,&
                     &RDX      ,RDY      ,&
                     &OBREDF   ,TRAC0    ,TRAC1    )

                  END IF
!TIMG                  CALL SWTSTO(140)
!
!     *** test output ***

                  IF ( TESTFL .AND. ITEST .GE. 120 ) THEN
                     WRITE(PRINTF,"(' ACTION POINT :',I5)") KCGRD(1)
                     WRITE(PRINTF,*)
                     WRITE(PRINTF,*) ' matrix coefficients in action after strs(x-y)'
                     WRITE(PRINTF,*)
                     WRITE(PRINTF,*)&
                     &'   IS   ID    IMAT5L       IMATDA       IMAT6U    IMATRA    CAS'
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IF (ISCMIN(ID).GT.0) THEN
                           DO IS = ISCMIN(ID), ISCMAX(ID)
                              WRITE(PRINTF,"(1X,2I4,4X,4E12.4,E10.2)") IS, ID, IMAT5L(ID,IS),IMATDA(ID,IS),&
                              &IMAT6U(ID,IS),IMATRA(ID,IS),CAS(ID,IS,1)
                           ENDDO
                        ENDIF
                     ENDDO
                  END IF

!TIMG                  CALL SWTSTA(141)
                  IF ( (DYNDEP .OR. ICUR.EQ.1) .AND. ITFRE.NE.0 ) THEN

!       *** call propagation module in S-direction ***

                     IF ( INT(PNUMS(8)) .EQ. 1 ) THEN

!         *** use implicit scheme for the integration in frequency ***
!         *** space (no a priori assumptions)                      ***

                        CALL STRSSI (SPCSIG  ,&
                        &CAS     ,IMAT5L  ,IMATDA  ,IMAT6U  ,ANYBIN  ,&
                        &IMATRA  ,AC2     ,ISCMIN  ,ISCMAX  ,IDDLOW  ,&
                        &IDDTOP  ,TRAC0   ,TRAC1                     )

                     ELSE IF ( INT(PNUMS(8)) .EQ. 2 ) THEN

!         *** Explicit numerical scheme in frequency space    ***
!         *** based on flux transport of action across        ***
!         *** boundaries. Energy is removed from the spectrum ***
!         *** based on a CFL criterion                        ***

                        CALL STRSSB (IDDLOW  ,IDDTOP  ,&
                        &IDCMIN  ,IDCMAX  ,ISSTOP  ,CAX     ,CAY     ,&
                        &CAS     ,AC2     ,SPCSIG  ,IMATRA  ,&
                        &ANYBLK  ,RDX     ,RDY     ,TRAC0            )

                     END IF
                  END IF
!TIMG                  CALL SWTSTO(141)
!
!     *** call propagation module in D-direction ***
!
!TIMG                  CALL SWTSTA(142)
                  IF ( IREFR.NE.0 ) THEN
                     IF ( PROPFL.EQ.0 ) THEN
                        CALL STRSD (DDIR    ,IDCMIN  ,&
                        &IDCMAX  ,CAD     ,IMATLA  ,IMATDA  ,IMATUA  ,&
                        &IMATRA  ,AC2     ,ISSTOP  ,&
                        &ANYBIN  ,LEAKC1  ,TRAC0   ,TRAC1            )
                     ELSE IF ( PROPFL.EQ.1 ) THEN
                        CALL SWFLXD (CAD, IMATLA, IMATDA, IMATUA, IMATRA,&
                        &AC2, DDIR, ANYBIN, LEAKC1, IDCMIN,&
                        &IDCMAX, ISSTOP)
                     END IF
                  END IF
!TIMG                  CALL SWTSTO(142)
!
!     *** test; remove on vector computer ***

                  IF ( TESTFL .AND. ITEST .GE. 70 ) THEN
                     WRITE(PRINTF,*) ' *** Values at end of subroutine action ***'
                     WRITE (PRINTF,"(' ACTION: POINT MCGRD MSC MDC : ',4I5)") KCGRD(1), MCGRD, MSC, MDC
                     WRITE (PRINTF,"(' ACTION: IDLW IDTP ISTOP ICMAX : ',4I4)") IDDLOW,IDDTOP,ISSTOP, ICMAX
                     WRITE (PRINTF,"(' ACTION: KCGRD(1), KCGRD(2), KCGRD(3) : ',3I4)") KCGRD(1), KCGRD(2), KCGRD(3)
                     WRITE (PRINTF,"(' ACTION:RDX(1) RDX(2) RDY(1) RDY(2) : ',4E12.4)") RDX(1), RDX(2), RDY(1), RDY(2)
                     IF (ITEST.GE.210) THEN
                        DO IS = 1, MSC
                           WRITE(PRINTF,"(' ACTION: SPCSIG IDCMIN IDCMAX : ',F8.4,2I6)") SPCSIG(IS),IDCMIN(IS),IDCMAX(IS)
                        ENDDO
                        DO IS = 1, MSC
                           WRITE(PRINTF,*) 'IS ',IS
                           DO ID = 1, MDC
                              WRITE(PRINTF,"(' ACTION: ID CAX CAY CAS CAD AC2:',I3,5E12.4)") ID, CAX(ID,IS,1), CAY(ID,IS,1),&
                              &CAS(ID,IS,1), CAD(ID,IS,1), AC2(ID,IS,KCGRD(1))
                           ENDDO
                        ENDDO
                     ENDIF
                     WRITE(PRINTF,*) ' *** end of subr ACTION *** '
                  END IF
!     End of subroutine ACTION
                  RETURN
               end subroutine ACTION
!****************************************************************

               SUBROUTINE SINTGRL(SPCDIR  ,KWAVE   ,AC2     ,&
               &DEP2    ,QB_LOC  ,URSELL  ,BIPHAS  ,&
               &RDX     ,RDY     ,&
               &AC2TOT  ,ETOT    ,&
               &ABRBOT  ,UBOT    ,HS      ,QB      ,&
               &HM      ,KMESPC  ,SMEBRK  ,KTETA   ,&
               &TMBOT   ,BOTLV   ,GAMBR   ,&
               &SWPDIR  ,&
               &URMSTOP ,&
&IDDLOW  ,IDDTOP, TRIADS, SIGPOW, WCAP_WORKSPACE )
   USE swan_service_interfaces, ONLY: STRACE
   USE swan_nonlinear_interactions, ONLY: PEREXC, SWBIDW
   USE swan_dissipation, ONLY: BRKPAR, FRABRE

!****************************************************************

                  USE OCPCOMM4
                  USE SWCOMM1
                  USE SWCOMM3
                  USE SWCOMM4

                  IMPLICIT NONE(TYPE, EXTERNAL)
                  TYPE(triad_state_t), INTENT(INOUT) :: TRIADS
                  REAL, INTENT(IN) :: SIGPOW(:,:)
                  TYPE(wcap_workspace_t), INTENT(INOUT) :: WCAP_WORKSPACE



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
!     40.02: IJsbrand Haagsma
!     40.08: Erick Rogers
!     40.13: Nico Booij
!     40.16: IJsbrand Haagsma
!     40.22: John Cazes and Tim Campbell
!     40.41: Marcel Zijlema
!     40.51: Marcel Zijlema
!     41.38: James Salmon
!     41.96: Marcel Zijlema
!     41.97: Marcel Zijlema
!
!  1. Updates
!
!     40.02, Jan. 00: New, based on the old SDISPA subroutine
!     40.02, Oct. 00: KWAVE removed in call BRKPAR
!     40.13, Aug. 01: reduction of spectrum not for Mode Noupdate
!     40.22, Sep. 01: Changed array definitions to use the parameter
!                     MICMAX instead of ICMAX.
!     40.22, Sep. 01: Changed allocated arrays to static arrays to fix
!                     OpenMP problems with arrays allocated in parallel
!                     regions.
!     40.22, Oct. 01: PSURF(2) is no longer used as a variable
!     40.16, Dec. 01: Implemented limiter switches
!     40.08, Mar. 03: Dimensioning of RDX, RDX changed to be consistent
!                     with other subroutines
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.51, Feb. 05: near bottom wave period added
!     41.38, Apr. 12: extension to nkd scaling
!     41.96, Aug. 22: store BKD-computed breaker index (e.g. for output)
!     41.97, Sep. 22: add alternative biphase formulations
!
!  2. Purpose
!
!     To compute several integrals used in SWAN and some general parameters
!
!  3. Method
!
!     The total energy ETOT is calculate as the following integral
!
!     ETOT = Integrate [ AC2(theta,sigma) sigma dsigma dtheta ]
!
!     To avoid too high dissipation by breaking, ETOT is maximised by a
!     total energy EMAX based on the maximum wave height HM:
!
!     HM    = PSURF(2) * depth
!
!                      2
!     EMAX  = 0.25 * HM
!
!     When EMAX > ETOT, then the action density AC2 is reduced by EMAX/ETOT
!
!     In the physicaly unrealistic case that ETOT <= 0, the integrals and other parameters
!     get values that represent a steady sea-state with wind close to zero.
!
!     The following integrals are calculated:
!
!                                                       2
!     AB2   = Integrate [ (AC2(theta,sigma) sigma / Sinh [ K(sigma) depth ]) dsigma dtheta ]
!
!     ACTOT = Integrate [ AC2(theta,sigma) dsigma dtheta ]
!
!     EDRKTOT=Integrate [ (AC2(theta,sigma) sigma / Sqrt [ K(sigma) ]) dsigma dtheta ]
!
!     EKTOT = Integrate [ AC2(theta,sigma) K(sigma) sigma dsigma dtheta
!
!                                               2
!     ETOT1 = Integrate [ AC2(theta,sigma) sigma dsigma dtheta ]
!
!                                               3
!     ETOT2 = Integrate [ AC2(theta,sigma) sigma dsigma dtheta ]
!
!                                               5
!     ETOT4 = Integrate [ AC2(theta,sigma) sigma dsigma dtheta ]
!
!                                                3      2
!     UB2   = Integrate [ (AC2(theta,sigma) sigma / Sinh [ K(sigma) depth ]) dsigma dtheta ]
!
!     For reasons of ??, in the calculation of UB2, AB2, ETOTM2, ETOTM4, the high frequency
!     tail is ignored.
!
!     Based on these integrals the following parameters are calculated:
!
!     ABRBOT  = Sqrt [ 2 AB2 ]
!     HS      = 4 Sqrt [ ETOT ]
!                               2
!     KM_WAM  = (ETOT / EDRKTOT)
!     QB      : computed in the subroutine FRABRE
!     SIGM01  = ETOT1 / ETOT
!     SIGM_10 = ETOT  / ACTOT
!     UBOT    = Sqrt [ UB2 ]             NOTE: THIS IS THE ROOT MEAN SQUARE OF THE ORBITAL MOTION NEAR THE BOTTOM!!!
!     TMBOT   = 2 PI Sqrt [ AB2 / UB2 ]
!
!  4. Argument variables
!
!     ABRBOT: Near bottom excursion
!     AC2   : Action density as function of ID, IS, IX and IY
!     AC2TOT: Total action density per gridpoint
!     BIPHAS: biphase as function of IX and IY
!     BOTLV : Bottom depth
!     DEP2  : Water depth
!     ETOT  : Total wave energy density
!     GAMBR : breaker parameter as function of IX and IY
!     HM    : Maximum wave height
!     HS    : Significant wave height
!     KMESPC: Mean average wavenumber according to the WAM-formulation
!     KTETA : number of directional partitions
!     KWAVE : Wavenumber
!     QB    : Fraction of breaking waves
!     QB_LOC: Fraction of breaking waves at current grid-point
!     SMEBRK: Mean frequency according to first order moment
!     SWPDIR: Number of current sweep direction
!     TMBOT : near bottom wave period
!     UBOT  : Near bottom velocity as function of IX and IY
!     URSELL: Ursell number as function of IX and IY

                  INTEGER, INTENT(IN)  :: SWPDIR
                  INTEGER, INTENT(IN)  :: IDDLOW, IDDTOP

                  REAL, INTENT(IN)     :: DEP2(MCGRD), BOTLV(MCGRD)
                  REAL, INTENT(INOUT)  :: GAMBR(MCGRD)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
                  REAL, INTENT(IN)     :: KWAVE(MSC,MICMAX)
!  RDX/RDY assumed-size: SINTGRL only forwards them; the unstructured caller
!  passes a 2-element array and the callees read RDX(1:2). See swanser.
                  REAL, INTENT(IN)     :: RDX(*), RDY(*)
                  REAL, INTENT(IN)     :: SPCDIR(MDC,6)

                  REAL, INTENT(IN OUT) :: AC2(MDC,MSC,MCGRD)
                  REAL, INTENT(IN OUT) :: QB(MCGRD)
                  REAL, INTENT(IN OUT) :: UBOT(MCGRD)
                  REAL, INTENT(IN OUT) :: URMSTOP(MCGRD)
                  REAL, INTENT(IN OUT) :: URSELL(MCGRD)
                  REAL, INTENT(IN OUT) :: BIPHAS(MCGRD)
                  REAL, INTENT(IN OUT) :: TMBOT(MCGRD)

                  REAL, INTENT(OUT)    :: ABRBOT, ETOT, HM, HS, QB_LOC
                  REAL, INTENT(OUT)    :: AC2TOT, KMESPC, SMEBRK, KTETA

!  6. Local variables
!
!     IENT  : Number of entries in this subroutine
!     IS    : Counter for the relative frequency band

                  INTEGER, SAVE :: IENT = 0

!     AB2                : Sum of E_DSHKD2_DS_DD
!     ACTOT_DSIG         : Integration term for calculating ACTOT
!     ARR                : auxiliary array
!     BIPH               : parameterized biphase of the spectrum
!     EDRKTOT_SWELL      : Swell-part of EDRKTOT
!     EMAX               : Maximum energy according calculated HM
!     ETOT_DRK_DSIG      : Integration term for calculating EDRKTOT
!     ETOT_K_DSIG        : Integration term for calculating EKTOT
!     ETOT_DSHKD2_DSIG   : Integration term for calculating AB2
!     ETOT_DSIG          : Integration term for calculating ETOT
!     ETOT_SIG_DSIG      : Integration term for calculating ETOT1
!     ETOT_SIG2_DSIG     : Integration term for calculating ETOT2
!     ETOT_SIG2_DSHKD2_DSIG: Integration term for calculating UB2
!     ETOT_SIG4_DSIG     : Integration term for calculating ETOT4
!     ETOT_SIG_2_DSIG    : Integration term for calculating ETOT_2
!     ETOT_2             : (negative) second moment of the energy density
!     FRINT_X_DDIR       : FRINTF * DDIR
!     KLOC               : help variable to compute wave number based on sigma0,-2
!     SINH_K_X_DEP_2     : SINH(KWAVE*DEP2)**2
!     UB2                : Sum of ETOT_SIG2_DSHKD2_DSIG
!     WH                 : fraction of breaking waves

                  REAL              :: AB2, EMAX, UB2, ETOT_2, KLOC(1), ARR(1)
                  REAL              :: WH
                  REAL              :: BIPH
                  REAL              :: FRINTF_X_DDIR
                  REAL              :: BRCOEF   ! variable breaking coefficient (cal
                  REAL              :: UT2
                  REAL              :: DELL ! delta l for estimating biphase

                  REAL              :: ETOT_DSIG(MSC)
                  REAL              :: ACTOT_DSIG(MSC), ETOT_DSHKD2_DSIG(MSC)
                  REAL              :: ETOT_DRK_DSIG(MSC), ETOT_SIG2_DSIG(MSC)
                  REAL              :: ETOT_K_DSIG(MSC), ETOT_SIG_DSIG(MSC)
                  REAL              :: ETOT_SIG2_DSHKD2_DSIG(MSC)
                  REAL              :: ETOT_SIG2_DTHKD2_DSIG(MSC)
                  REAL              :: ETOT_SIG4_DSIG(MSC)
                  REAL              :: SINH_K_X_DEP_2(MSC)
                  REAL              :: TANH_K_X_DEP_2(MSC)
                  REAL              :: ETOT_SIG_2_DSIG(MSC)

!     9. STRUCTURE
!
!   ----------------------------------------------------------
!   Determine ETOT
!   If energy level is too large compared with depth
!   Then reduce action densities
!   ------------------------------------------------------
!   For all spectral frequencies do
!       determine wavenumber K and K*depth
!       For every spectral direction do
!           add AC2 to sum of action densities
!       --------------------------------------------------
!       add contributions to various moments of energy density
!   ---------------------------------------------------------
!   add tail contributions
!   determine average frequency and wavenumber
!   ----------------------------------------------------------
!   If B&J surf breaking is used
!   Then call FRABRE to compute fraction of breaking waves
!   ----------------------------------------------------------
!   determine orbital motion near the bottom
!   ----------------------------------------------------------
!
! 13. Source text:

                  ASSOCIATE(ACTOT => WCAP_WORKSPACE%total_action,&
                  &EDRKTOT => WCAP_WORKSPACE%energy_over_root_wavenumber,&
                  &EKTOT => WCAP_WORKSPACE%energy_times_wavenumber,&
                  &ETOT1 => WCAP_WORKSPACE%first_energy_moment,&
                  &ETOT2 => WCAP_WORKSPACE%second_energy_moment,&
                  &ETOT4 => WCAP_WORKSPACE%fourth_energy_moment,&
                  &KM_WAM => WCAP_WORKSPACE%mean_wavenumber_wam,&
                  &KM01 => WCAP_WORKSPACE%mean_wavenumber_01,&
                  &SIGM_WAM => WCAP_WORKSPACE%mean_frequency_wam,&
                  &SIGM_10 => WCAP_WORKSPACE%mean_frequency_10,&
                  &SIGM01 => WCAP_WORKSPACE%mean_frequency_01)

                  IF (LTRACE) CALL STRACE (IENT,'SINTGRL')

!     --- initialisation

                  CALL WCAP_WORKSPACE%BEGIN_POINT()

                  HS      = 0.
                  HM      = 0.1

                  QB(KCGRD(1))   = 0.
                  ABRBOT         = 0.001
                  UBOT(KCGRD(1)) = 0.
                  URMSTOP(KCGRD(1)) = 0.
                  TMBOT(KCGRD(1))= 0.

!     --- calculate total spectral energy

                  FRINTF_X_DDIR = FRINTF * DDIR
                  ETOT_DSIG(:)  = SUM(AC2(:,:,KCGRD(1)),DIM=1) * SIGPOW(:,2) *&
                  &FRINTF_X_DDIR
                  ETOT          = SUM(ETOT_DSIG)

!     --- add high frequency tail

                  ETOT = ETOT + ETOT_DSIG(MSC) * PWTAIL(6) / FRINTF

!     --- compute maximum energy based on maximum wave height

                  EMAX = 0.25 * ( PSURF(2) * DEP2(KCGRD(1)) )**2

!     --- reduce action density if necessary

                  IF (ACUPDA .AND. ETOT .GT. EMAX .AND. ISURF .GE. 1&
                  &.AND. IQCM .EQ. 0) THEN
                     AC2(:,:,KCGRD(1)) = MAX(0.,(EMAX/ETOT)*AC2(:,:,KCGRD(1)))

                     IF (TESTFL.AND.ITEST.GE.80)&
                     &WRITE (PRTEST,"(' energy is reduced in SINTGRL', 4(1x, e12.4))") DEP2(KCGRD(1)), EMAX, ETOT

!        --- correct value for ETOT

                     ETOT = EMAX

                  ENDIF

                  IF ( ETOT .GT. 0. ) THEN

!       --- calculate all other integrals

                     SINH_K_X_DEP_2(:)   = SINH(MIN(30.,&
                     &KWAVE(:,1)*DEP2(KCGRD(1)))&
                     &)**2
                     TANH_K_X_DEP_2(:)   = TANH(MIN(30.,&
                     &KWAVE(:,1)*DEP2(KCGRD(1)))&
                     &)**2
                     ACTOT_DSIG(:)       = SUM(AC2(:,:,KCGRD(1)),DIM=1) *&
                     &SIGPOW(:,1) * FRINTF_X_DDIR
                     ETOT_SIG_DSIG(:)    = ACTOT_DSIG(:) * SIGPOW(:,2)
                     ETOT_SIG2_DSIG(:)   = ACTOT_DSIG(:) * SIGPOW(:,3)
                     ETOT_SIG4_DSIG(:)   = ACTOT_DSIG(:) * SIGPOW(:,5)
                     ETOT_SIG_2_DSIG(:)  = ACTOT_DSIG(:) / SIGPOW(:,1)
                     ETOT_DRK_DSIG(:)    = ETOT_DSIG(:) / SQRT(KWAVE(:,1))
                     ETOT_K_DSIG(:)      = ETOT_DSIG(:) * KWAVE(:,1)
                     ETOT_DSHKD2_DSIG(:) = ETOT_DSIG(:) / SINH_K_X_DEP_2(:)
                     ETOT_SIG2_DSHKD2_DSIG(:) = ETOT_SIG2_DSIG(:) / SINH_K_X_DEP_2(:)
                     ETOT_SIG2_DTHKD2_DSIG(:) = ETOT_SIG2_DSIG(:) / TANH_K_X_DEP_2(:)

                     ACTOT         = SUM(ACTOT_DSIG)
                     ETOT1         = SUM(ETOT_SIG_DSIG)
                     ETOT2         = SUM(ETOT_SIG2_DSIG)
                     ETOT4         = SUM(ETOT_SIG4_DSIG)
                     ETOT_2        = SUM(ETOT_SIG_2_DSIG)
                     EDRKTOT       = SUM(ETOT_DRK_DSIG)
                     EKTOT         = SUM(ETOT_K_DSIG)
                     UB2           = SUM(ETOT_SIG2_DSHKD2_DSIG)
                     UT2           = SUM(ETOT_SIG2_DTHKD2_DSIG)
                     AB2           = SUM(ETOT_DSHKD2_DSIG)

!       --- add high frequency tails

                     ACTOT       = ACTOT + PWTAIL(5) * ACTOT_DSIG(MSC) / FRINTF
                     ETOT1       = ETOT1 + PWTAIL(7) * ETOT_DSIG(MSC) *&
                     &SIGPOW(MSC,1) / FRINTF
                     ETOT_2      = ETOT_2 + PWTAIL(4) * ACTOT_DSIG(MSC) /&
                     &SIGPOW(MSC,1) / FRINTF
                     EDRKTOT     = EDRKTOT + PWTAIL(5) * ETOT_DSIG(MSC) /&
                     &(SQRT(KWAVE(MSC,1)) * FRINTF)
                     EKTOT       = EKTOT + PWTAIL(8) * ETOT_DSIG(MSC) *&
                     &KWAVE(MSC,1) / FRINTF

!       --- calculate the mean frequencies SIGM01 and SIGM_10,
!           mean wavenumbers KM_WAM, KM01 and significant waveheight HS

                     IF (ETOT1  .GT. 0.) SIGM01  = ETOT1 / ETOT
                     IF (EKTOT  .GT. 0.) KM01    = EKTOT / ETOT
                     IF (ACTOT  .GT. 0.) SIGM_10 = ETOT / ACTOT
                     IF (EDRKTOT .GT. 0. ) THEN
                        KM_WAM  = ( ETOT / EDRKTOT )**2.
                        SIGM_WAM = SQRT(GRAV*KM_WAM*TANH(KM_WAM*DEP2(KCGRD(1))))
                     ENDIF
                     IF ( ETOT .GT. 1.E-20 ) THEN
                        HS       = 4. * SQRT (ETOT)
                     END IF

!       --- calculate the orbital velocity UBOT, orbital excursion ABRBOT
!           and near bottom wave period TMBOT

                     IF ( UB2 .GT. 0.) UBOT(KCGRD(1)) = SQRT ( UB2 )
                     IF ( AB2 .GT. 0.) ABRBOT = SQRT (2. *  AB2)
                     IF ( UB2 .GT. 0. .AND. AB2 .GT. 0. )&
                     &TMBOT(KCGRD(1)) = PI2*SQRT(AB2/UB2)
                     IF ( UT2 .GT. 0.) URMSTOP(KCGRD(1)) = SQRT ( 2. * UT2 )

                  ENDIF

!     --- calculate Ursell number and biphase
!     --- update only for first encounter in a sweep

                  IF ( ITRIAD.GT.0&
                  &.OR. ISURF.EQ.7&
                  &) THEN
                     IF (( SWPDIR .EQ. 1) .OR.&
                     &( SWPDIR .EQ. 2 .AND. IXCGRD(1) .EQ. 1) .OR.&
                     &( SWPDIR .EQ. 3 .AND. IYCGRD(1) .EQ. 1) .OR.&
                     &( SWPDIR .EQ. 4 .AND.&
                     &(IXCGRD(1).EQ.MXC .AND. IYCGRD(1).EQ.1) )) THEN
!          --- Ursell number
                        URSELL(KCGRD(1)) = (GRAV*HS) /&
                        &(2.*SQRT(2.)*SIGM01**2*DEP2(KCGRD(1))**2)
!          --- biphase
                        IF ( IBIPH.EQ.1 ) THEN
!             Eldeberky (1996)
                           BIPHAS(KCGRD(1)) = 0.5*PI *&
                           &(TANH(PTRIAD(4)/URSELL(KCGRD(1)))-1.)
                        ELSEIF ( IBIPH.EQ.2 ) THEN
!             Saprykina et al. (2017)
                           CALL PEREXC ( DELL, DEP2, AC2, SIGPOW(:,1), RDX, RDY,&
                           &BOTLV )
                           BIPHAS(KCGRD(1)) = 0.5*PI * (MIN(1.,DELL/PTRIAD(9)) - 1.)
                        ELSEIF ( IBIPH.EQ.3 ) THEN
!             De Wit (2022)
                           CALL SWBIDW ( BIPH, AC2, SIGPOW(:,1), RDX, RDY, BOTLV,&
                           &SPCDIR(1,2), SPCDIR(1,3) )
!             --- scale biphase
                           BIPH = SQRT(TANH(URSELL(KCGRD(1)))) * BIPH
!             --- in between -90 and 90 deg
                           TRIADS%biphase_unfiltered(KCGRD(1)) =&
                           &MIN(0.5*PI, MAX(-0.5*PI, BIPH))
!             note: this biphase will be spatially averaged at the
!                   start of next iteration (see routine SWCOMP)
                        ENDIF
                     ENDIF
                  ELSE
                     URSELL(KCGRD(1)) = 0.
                     BIPHAS(KCGRD(1)) = 0.
                  END IF

!     --- compute actual maximum wave height based on breaking model

                  IF ( ISURF.GT.0 ) THEN
!        compute some parameters for breaker models
                     CALL BRKPAR (BRCOEF, SPCDIR(1,2), SPCDIR(1,3), AC2,&
                     &SIGPOW(:,1), DEP2, BOTLV,&
                     &RDX, RDY, KWAVE, IDDLOW, IDDTOP, SPCDIR(1,1),&
                     &KTETA, WCAP_WORKSPACE%mean_wavenumber_wam )

                     IF (ISURF.EQ.6) THEN
!           in case of BKD store breaker index
                        IF (MODGAM) THEN
                           IF (( SWPDIR .EQ. 1) .OR.&
                           &( SWPDIR .EQ. 2 .AND. IXCGRD(1) .EQ. 1) .OR.&
                           &( SWPDIR .EQ. 3 .AND. IYCGRD(1) .EQ. 1) .OR.&
                           &( SWPDIR .EQ. 4 .AND.&
                           &(IXCGRD(1).EQ.MXC .AND. IYCGRD(1).EQ.1) )) THEN
                              GAMBR(KCGRD(1)) = BRCOEF
                           ENDIF
                        ENDIF
                        HM = GAMBR(KCGRD(1)) * DEP2(KCGRD(1))

                     ELSEIF ( ISURF.EQ.7 ) THEN
!           compute breaker index based on the asymmetry
!           of breaking waves (Saprykina et al., 2017)
                        IF (( SWPDIR .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 2 .AND. IXCGRD(1) .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 3 .AND. IYCGRD(1) .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 4 .AND.&
                        &(IXCGRD(1).EQ.MXC .AND. IYCGRD(1).EQ.1) )) THEN
!              see also routine BRKPAR
                           IF ( BRCOEF.LT.0. ) THEN
                              BIPH = BIPHAS(KCGRD(1))
                              IF ( BIPH.LT.0. ) THEN
                                 BRCOEF = PSURF(4) - 0.3 * PSURF(5) * BIPH
                              ELSE
!                    local bed slope negative, no surf breaking
                                 BRCOEF = 0.
                              ENDIF
                           ENDIF
                           GAMBR(KCGRD(1)) = BRCOEF
                        ENDIF
                        HM = GAMBR(KCGRD(1)) * DEP2(KCGRD(1))
                     ELSE
                        HM = BRCOEF * DEP2(KCGRD(1))
                     ENDIF
                  ELSE
!        breaking disabled, assign very high value to Hmax
                     HM = 100.
                  ENDIF

!     --- calculate fraction of breakers

                  IF ( ETOT .GT. 0. ) THEN

!       --- calculate Qb when BJ78 breaker is activated

                     IF ( ISURF.NE.4&
                     &) THEN
                        CALL FRABRE (HM, ETOT, QB(KCGRD(1)), KTETA)

!       --- calculate Qb when TG83 breaker is activated

                     ELSEIF (ISURF.EQ.4) THEN
                        WH = (2.*SQRT(2.*ETOT)/HM)**PSURF(5)
                        WH = MIN(1.,WH)
                        QB(KCGRD(1)) = WH
                     ENDIF

                  ENDIF

                  QB_LOC = QB(KCGRD(1))

!     *** test output ***

                  IF (TESTFL .AND. ITEST.GE.60) THEN
                     WRITE(PRTEST, "(' SINTGRL: ETOT Hs Sigma K Aorb', 5(1X, E11.4))") ETOT, HS, SIGM_10, KM_WAM, ABRBOT
                  END IF

!     Set variables used outside the whitecapping scope

                  AC2TOT = ACTOT
                  KMESPC = KM_WAM
                  SMEBRK = SIGM01

                  END ASSOCIATE
                  RETURN

               end subroutine SINTGRL

!****************************************************************

               SUBROUTINE SOLPRE (AC2         ,AC2OLD      ,&
               &IMATRA      ,IMATLA      ,&
               &IMATDA      ,IMATUA      ,&
               &IMAT5L      ,IMAT6U      ,&
               &IDCMIN      ,IDCMAX      ,&
               &ANYBIN      ,&
               &IDTOT       ,ISTOT       ,&
               &IDDLOW      ,IDDTOP      ,&
               &ISSTOP      ,&
               &SPCSIG                   )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4
                  USE M_PARALL


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
!     40.00: Nico Booij
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.00, Feb. 99: New subroutine common tasks before solution of linear
!                     system (software moved from SOLBAND, SOLMAT and SOLMT1)
!     40.23, Aug. 02: implementation of under-relaxation technique
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.41, Aug. 04: code optimized
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Copy local spectrum to array AC2OLD before solving system,
!     apply under-relaxation approach in active bins, fill matrix
!     arrays for non-active bins and write test output
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
!        one and more dimensional arrays:
!        ---------------------------------
!        AC2       4D    Action density as function of D,S,X,Y and T
!        AC2OLD    2D    Values of action density at previous iteration
!        IMATDA    2D    Coefficients of diagonal of matrix
!        IMATLA    2D    Coefficients of lower diagonal of matrix
!        IMATUA    2D    Coefficients of upper diagonal of matrix
!        IMATRA    2D    Coefficients of right hand side of matrix
!        IMAT5L    2D    Coefficients for implicit calculation in
!                        frequency space (lower diagonal)
!        IMAT6U    2D    Coefficients for implicit calculation in
!                        frequency space (upper diagonal)
!        SPCSIG    1D    Relative frequencies in sigma-space

                  REAL     AC2(MDC,MSC,MCGRD)           ,&
                  &IMATRA(MDC,MSC)              ,&
                  &IMATLA(MDC,MSC)              ,&
                  &IMATDA(MDC,MSC)              ,&
                  &IMATUA(MDC,MSC)              ,&
                  &IMAT5L(MDC,MSC)              ,&
                  &IMAT6U(MDC,MSC)              ,&
                  &AC2OLD(MDC,MSC)              ,&
                  &SPCSIG(MSC)

!        IDCMIN    1D    Integer array containing minimum counter
!        IDCMAX    1D    Integer array containing maximum counter

                  INTEGER  IDCMIN(MSC)                  ,&
                  &IDCMAX(MSC)

!        ANYBIN    2D    Logical array. if a certain bin is enclosed
!                        in a sweep then ANYBIN is TRUE . array is
!                        used to determine whether some coefficients
!                        in the array have to be changed

                  LOGICAL  ANYBIN(MDC,MSC)

!  7. Common blocks used
!
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        NONE
!
!     7. ERROR MESSAGES
!
!        ---
!
!     8. REMARKS
!
!
!     9. STRUCTURE
!
!
!     10. SOURCE
!
!************************************************************************

                  INTEGER, SAVE :: IENT = 0
                  INTEGER  IS, ID, IDDUM, ID_MIN, ID_MAX, &
                  &IDDLOW  ,&
                  &IDDTOP  ,IDTOT   ,ISTOT   ,ISSTOP
                  REAL     ALFA

                  IF (LTRACE) CALL STRACE (IENT,'SOLPRE')

!     --- apply under-relaxation approach, if requested

                  ALFA = PNUMS(30)
                  IF (ALFA.GT.0. .AND. NSTATC.EQ.0) THEN
                     DO IS = 1, ISSTOP
                        DO IDDUM = IDCMIN(IS), IDCMAX(IS)
                           ID = MOD(IDDUM-1 + MDC, MDC) + 1
                           IMATDA(ID,IS) = IMATDA(ID,IS) + ALFA*SPCSIG(IS)
                           IMATRA(ID,IS) = IMATRA(ID,IS) + ALFA*SPCSIG(IS)*&
                           &AC2(ID,IS,KCGRD(1))
                        END DO
                     END DO
                  END IF

!     --- when ambient currents are involved or when the spectral space
!         is not a circular one (use SECTOR instead of CIRCLE in command
!         CGRID), some bins do not fall within the current sweep (in
!         particular, when SECTOR = 0 or 4 i.c. ICUR=1 or SECTOR = 4 i.c.
!         FULCIR=.FALSE., see routine SWPSEL for meaning of SECTOR). For
!         such bins, the corresponding rows in the matrix are reset such
!         that the solution AC2 does not change: the main diagonal is set
!         to 1, the off-diagonals are set to 0 and the righ-hand side is
!         set to AC2.

                  IF ( ICUR.EQ.1 .OR. .NOT.FULCIR ) THEN
                     DO IS = 1, MSC
                        DO ID = 1, MDC
                           IF ( .NOT. ANYBIN(ID,IS) ) THEN
                              IMATLA(ID,IS) = 0.
                              IMATDA(ID,IS) = 1.
                              IMATUA(ID,IS) = 0.
                              IMATRA(ID,IS) = AC2(ID,IS,KCGRD(1))
                              IMAT5L(ID,IS) = 0.
                              IMAT6U(ID,IS) = 0.
                           END IF
                        ENDDO
                     ENDDO
                  END IF

!     *** the action density is stored in an auxiliary array AC2OLD ***

                  DO IS = 1, MSC
                     DO ID = 1, MDC
                        AC2OLD(ID,IS) = AC2(ID,IS,KCGRD(1))
                     ENDDO
                  ENDDO

!     *** test output ***

                  IF ( TESTFL .AND. ITEST .GE. 70 ) THEN
                     WRITE (PRINTF,"(' SOLPRE: Matrix values for point:', 2I5)") IXCGRD(1)+MXF-2, IYCGRD(1)+MYF-2
                     WRITE (PRINTF,"(' bin diagonal r.h.s. ID-1 ID+1', ' IS-1 IS+1')")

                     DO IS = 1, ISSTOP
                        ID_MIN = IDCMIN(IS)
                        ID_MAX = IDCMAX(IS)
                        DO IDDUM = ID_MIN, ID_MAX
                           ID = MOD(IDDUM-1 + MDC, MDC) + 1
                           IF ( DYNDEP .OR. ICUR .EQ. 1 ) THEN
                              WRITE(PRINTF,"(2I3,6(1X,E12.4))") ID, IS, IMATDA(ID,IS), IMATRA(ID,IS),&
                              &IMATLA(ID,IS), IMATUA(ID,IS), IMAT5L(ID,IS), IMAT6U(ID,IS)
                           ELSE
                              WRITE(PRINTF,"(2I3,6(1X,E12.4))") ID, IS, IMATDA(ID,IS), IMATRA(ID,IS),&
                              &IMATLA(ID,IS), IMATUA(ID,IS)
                           ENDIF
                        ENDDO
                     ENDDO
                  END IF

                  RETURN
               end subroutine SOLPRE

!****************************************************************

               SUBROUTINE SOLMAT (IDCMIN     ,IDCMAX     ,&
               &AC2        ,IMATRA     ,&
               &IMATDA     ,IMATUA     ,&
               &IMATLA&
               &)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4
                  USE M_PARALL


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
!     30.72: IJsbrand Haagsma
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, Feb. 99: swcomm3 introduced
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     SUBROUTINE to solve the linear system which is filled in the
!     subroutine ACTION. The solution give the values for the
!     wave action for every frequency and every direction.
!     The system is solved by means of the Thomas sweep algorithm
!     in the spectral direction only.
!
!  3. Method
!
!     Solver for tri-diagonal matrix:
!
!
!        / 2  3          \ /   \
!        | 1  2  3       | |   |
!        |    1  2  3    | | N | =  RHS
!        |       1  2  3 | |   |
!        \          1  2 / \   /
!
!
!     This method consists of forward and backward sweeps.
!
!  4. Argument variables
!
!
!     IX          Counter of gridpoints in x-direction
!     IY          Counter of gridpoints in y-direction
!     IS          Counter of relative frequency band
!     ID          Counter of directional distribution
!     J           Dummy counter
!     MXC         Maximum counter of gridppoints in x-direction
!     MYC         Maximum counter of gridppoints in y-direction
!     MSC         Maximum counter of relative frequency
!     MDC         Maximum counter of directional distribution
!
!     REALS:
!     ---------
!
!     SP          Dummy variable
!     TEMP        Dummy variable
!
!     one and more dimensional arrays:
!     ---------------------------------
!     AC2       4D    Action density as function of D,S,X,Y and T
!     IMATDA    2D    Coefficients of diagonal of matrix
!     IMATLA    2D    Coefficients of lower diagonal of matrix
!     IMATUA    2D    Coefficients of upper diagonal of matrix
!     IMATRA    2D    Coefficients of right hand side of matrix
!     IDCMIN    1D    Integer array containing minimum counter
!     IDCMAX    1D    Integer array containing maximum counter
!
!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     ---
!
!  9. Subroutines calling
!
!     SWOMPU
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
!     -------------------------------------------------------------
!     For every D-direction within the sector do
!       Eliminate the lower diagonal
!     -------------------------------------------------------------
!     For every D-direction within the sector do
!       Solve the linear equation to get the wave action for every
!       direction (ID)
!     -------------------------------------------------------------
!     Set all the values in the arrays to zero:
!     IMATRA(MDC,MSC),IMATLA(MDC,MSC),IMATDA(MDC,MSC),IMATUA(MDC,MSC)
!     ------------------------------------------------------------
!
! 13. Source text


                  INTEGER, SAVE :: IENT = 0
                  INTEGER  IS, ID, J, ID_MIN, ID_MAX
                  INTEGER  IDDUM, IDM1, IDP1

                  REAL     SP      ,TEMP

                  REAL     AC2(MDC,MSC,MCGRD)           ,&
                  &IMATRA(MDC,MSC)              ,&
                  &IMATLA(MDC,MSC)              ,&
                  &IMATDA(MDC,MSC)              ,&
                  &IMATUA(MDC,MSC)

                  INTEGER  IDCMIN(MSC)        ,&
                  &IDCMAX(MSC)

                  IF (LTRACE) CALL STRACE (IENT,'SOLMAT')

!     **** 17/JAN   IN MOD (  , ) ;   + MDC WAS ADDED ****

                  do IS = 1, MSC
                     ID_MIN = IDCMIN(IS)
                     ID_MAX = IDCMAX(IS)

!       *** elimination of the lower diagonal of the first matrix ***

                     do IDDUM = (ID_MIN+1), ID_MAX
                        ID   = MOD(IDDUM-1+MDC, MDC) + 1
                        IDM1 = MOD(IDDUM-2+MDC, MDC) + 1
                        SP   = IMATDA(IDM1,IS)
                        IF ( ABS(SP) .LE. 1.E-20 ) THEN
                           TEMP = IMATLA(ID,IS) / SIGN( 1.E-20 , SP)
                        ELSE
                           TEMP = IMATLA(ID,IS) / SP
                        END IF
                        IMATDA(ID,IS) = IMATDA(ID,IS) - TEMP * IMATUA(IDM1,IS)
                        IMATRA(ID,IS) = IMATRA(ID,IS) - TEMP * IMATRA(IDM1,IS)
                     end do

!       *** solving of the linear equations for the wave action ***
!
!       *** first for ID_MAX, then for the others ***

                     ID   = MOD(ID_MAX-1+MDC, MDC) + 1
                     SP = IMATDA(ID,IS)
                     IF ( ABS(SP) .LE. 1.E-20 ) THEN
                        TEMP = SIGN (1.E-20 , SP)
                     ELSE
                        TEMP = SP
                     END IF

!       *** wave action for ID_MAX ***

                     AC2(ID,IS,KCGRD(1)) = IMATRA(ID,IS) / TEMP

                     do J = 1, (ID_MAX-ID_MIN)
                        ID   = MOD(ID_MAX-J-1+MDC, MDC) +1
                        IDP1 = MOD(ID_MAX-J+MDC, MDC) +1
                        SP = IMATDA(ID,IS)
                        IF ( ABS(SP) .LE. 1.E-20 ) THEN
                           TEMP = SIGN (1.E-20 , SP)
                        ELSE
                           TEMP = SP
                        END IF
                        AC2(ID,IS,KCGRD(1)) = ( IMATRA(ID,IS) - IMATUA(ID,IS) *&
                        &AC2(IDP1,IS,KCGRD(1)) ) / TEMP

                     end do

                     IF ( ITEST .GE. 120 .AND. TESTFL ) THEN
                        WRITE(PRINTF,"(' SOLMAT: POINT ID_MIN ID_MAX :',3I5)") KCGRD(1),ID_MIN,ID_MAX
                        do IDDUM = ID_MIN, ID_MAX
                           ID = MOD(IDDUM-1+MDC, MDC) + 1
                           WRITE (PRINTF,"(' IS ID AC2() :',2I5,2X,E12.4)") IS,ID,AC2(ID,IS,KCGRD(1))
                        end do
                     END IF

                  end do

!     *** set all the coefficients in the arrays 0 ***

                  DO IS = 1, MSC
                     DO ID = 1, MDC
                        IMATRA(ID,IS) = 0.
                        IMATDA(ID,IS) = 0.
                     ENDDO
                  ENDDO

                  IF ( TESTFL .AND. ITEST.GE. 40 ) THEN
                     WRITE (PRINTF,"(' SOLMAT: point :',2I5)") IXCGRD(1)+MXF-2, IYCGRD(1)+MYF-2
                     WRITE(PRINTF,*)
                  END IF

!     End of the subroutine SOLMAT

                  RETURN
               end subroutine SOLMAT

!****************************************************************

               SUBROUTINE SOLMT1 (IDCMIN     ,IDCMAX     ,&
               &AC2        ,IMATRA     ,&
               &IMATDA     ,IMATUA     ,&
               &IMATLA     ,&
               &ISSTOP     ,&
               &ANYBLK     ,IDDLOW     ,&
               &IDDTOP              )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4


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
!     30.72: IJsbrand Haagsma
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     40.00, Feb. 99: swcomm3 introduced
!     40.41, Aug. 04: array SECTOR removed and some corrections if SECTOR=0
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     subroutine to solve the linear system which is filled in the
!     subroutines ACTION and SOURCE. The solution give the values
!     for the wave action for every frequency and every direction.
!     Ambient currents are involved and the propagation term in
!     frequency space is discretized explicitly.
!     The system is solved by means of the Thomas sweep algorithm
!     in the spectral direction only.
!
!  3. Method
!
!     Solver for tridiagonal matrix with a possible coefficient
!     at the bottom left position and top right position due
!     to periodicity in theta-space:
!
!
!        / 2  3        1 \ /   \
!        | 1  2  3       | |   |
!        |    1  2  3    | | N | =  RHS
!        |       1  2  3 | |   |
!        \ 3        1  2 / \   /
!
!
!     This method consists of forward and backward sweeps.
!
!  4. Argument variables
!
!
!        IX          Counter of gridpoints in x-direction
!        IY          Counter of gridpoints in y-direction
!        IS          Counter of relative frequency band
!        ID          Counter of directional distribution
!        J           Dummy counter
!        MXC         Maximum counter of gridppoints in x-direction
!        MYC         Maximum counter of gridppoints in y-direction
!        MSC         Maximum counter of relative frequency
!        MDC         Maximum counter of directional distribution
!
!        REALS:
!        ---------
!
!        SP          Dummy variable
!        TEMP        Dummy variable
!
!        one and more dimensional arrays:
!        ---------------------------------
!        AC2       4D    Action density as function of D,S,X,Y and T
!        IMATDA    2D    Coefficients of diagonal of matrix
!        IMATLA    2D    Coefficients of lower diagonal of matrix
!        IMATUA    2D    Coefficients of upper diagonal of matrix
!        IMATRA    2D    Coefficients of right hand side of matrix
!        IDCMIN    1D    Integer array containing minimum counter
!        IDCMAX    1D    Integer array containing maximum counter
!        ICOLU2    1D    In presence of a current the spectral direction can
!                        be circular and closed. Matrix coefficients appear in
!                        the top right and bottom left corner of the matrix
!                        After pivoting --> coefficients are stored in ICOLU2
!                        space
!
!  7. Common blocks used
!
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        NONE
!
!     7. ERROR MESSAGES
!
!        ---
!
!     8. REMARKS
!
!
!     9. STRUCTURE
!
!   -------------------------------------------------------------
!   For every D-direction within the sector do
!     Eliminate the lower diagonal
!   -------------------------------------------------------------
!   For every D-direction within the sector do
!     Solve the linear equation to get the wave action for every
!     direction (ID)
!   -------------------------------------------------------------
!   Set all the values in the arrays to zero:
!   IMATRA(MDC,MSC),IMATLA(MDC,MSC),IMATDA(MDC,MSC),IMATUA(MDC,MSC)
!   ------------------------------------------------------------
!   End of SOLMT1
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

                  INTEGER, SAVE :: IENT = 0
                  INTEGER  IS, ID, J, IDDUM, IIDM, IIDP
                  INTEGER  ISSTOP, IDDTOP, IDDLOW, IDLOW, IDTOP

                  REAL     SP     ,TEMP   ,CORMAT ,TEMP1

                  REAL     AC2(MDC,MSC,MCGRD)           ,&
                  &IMATRA(MDC,MSC)              ,&
                  &IMATLA(MDC,MSC)              ,&
                  &IMATDA(MDC,MSC)              ,&
                  &IMATUA(MDC,MSC)              ,&
                  &ICOLU2(MDC)

                  INTEGER  IDCMIN(MSC)                  ,&
                  &IDCMAX(MSC)

                  LOGICAL  ANYBLK(MDC,MSC)

                  IF (LTRACE) CALL STRACE (IENT,'SOLMT1')

!     *** since explicit scheme is used and when CFL exceeds CFL max ***
!     *** then bin should not be propagated within the current sweep ***

                  DO IS = 1, ISSTOP
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IF ( ANYBLK(ID,IS) ) THEN
                           IMATLA(ID,IS) = 0.
                           IMATDA(ID,IS) = 1.
                           IMATUA(ID,IS) = 0.
                           IMATRA(ID,IS) = 0.
                        END IF
                     ENDDO
                  ENDDO

!     *** start process of elimination ***

                  do IS = 1, MSC

!         *** set values in auxiliary array for last ***
!         *** column equal zero                      ***

                     DO ID = 1, MDC
                        ICOLU2(ID) = 0.
                     ENDDO

                     IF ( IDCMIN(IS).LE.IDCMAX(IS) ) THEN
                        IDLOW = IDCMIN(IS)
                        IDTOP = IDCMAX(IS)
                     ELSE
                        IDLOW = 1
                        IDTOP = MDC
                     END IF

!         *** set values in coefficients in left bottom element ***
!         *** and right top element. Situation only occurs if   ***
!         *** the matrix is solved for all directions           ***

                     IF ( IDLOW .EQ. 1  .AND.  IDTOP .EQ. MDC ) THEN
                        CORMAT    = IMATUA(MDC,IS)
                        ICOLU2(1) = IMATLA(1,IS)
                     ELSE
                        CORMAT    = 0.
                        ICOLU2(1) = 0.
                     END IF

!         *** elimination of the lower diagonal of the first matrix ***

                     do IDDUM = (IDLOW+1 ), IDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IIDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        SP = IMATDA(IIDM,IS)
                        IF ( ABS(SP) .LE. 1.E-20 ) THEN
                           TEMP = IMATLA(ID,IS) / SIGN( 1.E-20 , SP)
                           TEMP1 = CORMAT / SIGN(1.E-20 , SP)
                        ELSE
                           TEMP = IMATLA(ID,IS) / SP
                           TEMP1 = CORMAT / SP
                        END IF
                        IMATDA(ID,IS)  = IMATDA(ID,IS)  - TEMP * IMATUA(IIDM,IS)
                        IMATRA(ID,IS)  = IMATRA(ID,IS)  - TEMP * IMATRA(IIDM,IS)
                        IMATRA(IDTOP,IS) = IMATRA(IDTOP,IS) - TEMP1 *&
                        &IMATRA(IIDM,IS)
                        CORMAT = 0. - TEMP1 * IMATUA(IIDM,IS)

                        IF ( IDDUM .LT. (IDTOP-1) ) THEN
                           ICOLU2(ID) =  - TEMP * ICOLU2(IIDM)
                        ELSE
                           IMATUA(ID,IS) = IMATUA(ID,IS) - TEMP * ICOLU2(IIDM)
                        END IF
                        IF ( IDDUM .LT. IDTOP ) THEN
                           IMATDA(IDTOP,IS) = IMATDA(IDTOP,IS) - TEMP1 *&
                           &ICOLU2(IIDM)
                        ELSE
                           IMATDA(IDTOP,IS) = IMATDA(IDTOP,IS) - TEMP1 *&
                           &IMATUA(IIDM,IS)
                        END IF

                     end do

!         *** solving of the linear equations for the wave action ***
!
!         *** first for IDTOP, then for the others ***

                     SP = IMATDA(IDTOP,IS)
                     IF ( ABS(SP) .LE. 1.E-20 ) THEN
                        TEMP = SIGN (1.E-20 , SP)
                     ELSE
                        TEMP = SP
                     END IF

!         *** wave action for IDCMAX ***

                     AC2(IDTOP,IS,KCGRD(1)) = IMATRA(IDTOP,IS) / TEMP

                     do J = 1, (IDTOP-IDLOW)
                        IDDUM = IDTOP - J
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IIDP = MOD ( IDDUM + MDC , MDC ) + 1
                        SP = IMATDA(ID,IS)
                        IF ( ABS(SP) .LE. 1.E-20 ) THEN
                           TEMP = SIGN (1.E-20 , SP)
                        ELSE
                           TEMP = SP
                        END IF
                        AC2(ID,IS,KCGRD(1)) = ( IMATRA(ID,IS) - IMATUA(ID,IS) *&
                        &AC2(IIDP,IS,KCGRD(1)) - ICOLU2(ID)  *&
                        &AC2(IDTOP,IS,KCGRD(1)) ) / TEMP
                     end do

!         *** extended info for SOLMT1 ***

                     IF ( ITEST .GE. 13 .AND. TESTFL ) THEN
                        WRITE(PRINTF,*) 'SOLMT1'
                        WRITE(PRINTF,*) ' matrix coefficients after pivoting '
                        WRITE(PRINTF,*)
                        WRITE(PRINTF,*)&
                        &'ID IDDUM IMATLA      IMATDA      IMATUA     ICOLU2    IMATRA'
                        do IDDUM = IDLOW, IDTOP
                           ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           WRITE(PRINTF,"(2I3,5E12.4)") ID, IDDUM,IMATLA(ID,IS),IMATDA(ID,IS),&
                           &IMATUA(ID,IS),ICOLU2(ID),IMATRA(ID,IS)
                        end do
                        WRITE(PRINTF,*)
                        do IDDUM = IDLOW, IDTOP
                           ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           WRITE (PRINTF,"(' IS ID and resolved vector :',2I5,2X,E12.4)") IS,ID,AC2(ID,IS,KCGRD(1))
                        end do
                        WRITE(PRINTF,*)
                     END IF
                  end do

!     *** set all the coefficients in the arrays 0 ***

                  DO IS = 1, MSC
                     DO ID = 1, MDC
                        IMATRA(ID,IS) = 0.
                        IMATDA(ID,IS) = 0.
                        ICOLU2(ID)    = 0.
                     ENDDO
                  ENDDO

!     End of the subroutine SOLMT1

                  RETURN
               end subroutine SOLMT1

!****************************************************************

               SUBROUTINE SOURCE (ITER       ,IX         ,IY         ,&
               &SWPDIR     ,KWAVE      ,SPCSIG     ,&
               &ECOS       ,ESIN       ,AC2        ,&
               &DEP2       ,IMATDA     ,IMATRA     ,&
               &ABRBOT     ,KMESPC     ,SMESPC     ,&
               &UBOT       ,UFRIC      ,UX2        ,&
               &UY2        ,IDCMIN     ,IDCMAX     ,&
               &IDDLOW     ,IDDTOP     ,IDWMIN     ,&
               &IDWMAX     ,ISSTOP     ,PLWNDS     ,&
               &PLWNDD     ,PLWCAP     ,PLBTFR     ,&
               &PLSWEL     ,&
               &PLWBRK     ,PLNL4S     ,PLNL4D     ,&
               &PLVEGT     ,PLTURB     ,PLMUD      ,&
               &PLICE      ,PLBRAG     ,&
               &PLTRI      ,            HS         ,&
               &ETOT       ,QBLOC      ,THETAW     ,&
               &HM         ,FPM        ,WIND10     ,&
               &ETOTW      ,GROWW      ,ALIMW      ,&
               &SMEBRK     ,KTETA      ,SNLC1      ,&
               &DAL1       ,DAL2       ,DAL3       ,&
               &UE         ,SA1        ,&
               &SA2        ,DA1C       ,DA1P       ,&
               &DA1M       ,DA2C       ,DA2P       ,&
               &DA2M       ,SFNL       ,DSNL       ,&
               &MEMNL4     ,WWINT      ,WWAWG      ,&
               &WWSWG      ,CGO        ,USTAR      ,&
               &ZELEN      ,SPCDIR     ,ANYWND     ,&
               &DMW        ,FBD        ,MEMBRG     ,&
               &CAS        ,QTL1       ,QTL2       ,&
               &MEMSINA    ,MEMSINB    ,&
               &DISSC0     ,DISSC1     ,GENC0      ,&
               &GENC1      ,REDC0      ,REDC1      ,&
               &XIS        ,FRCOEF     ,IT         ,&
               &NPLA2      ,TURBV2     ,MUDL2      ,&
               &AICE2      ,HICE2      ,&
               &URSELL     ,ANYBIN     ,REFLSO     ,&
               &TAUWV      ,BIPHAS&
               &,URMSTOP   ,TRIADS, SNL4, SPECTRAL_POWERS, WCAP_WORKSPACE&
               &)
   USE swan_service_interfaces, ONLY: MSGERR, STRACE
   USE swan_nonlinear_interactions, ONLY: FILNL3, RANGE4, SWDCTA, SWDNCTA, SWFTIM, SWINTFXNL, SWLTA, SWSNL1, SWSNL2, SWSNL3, SWSNL4, SWSNL8
   USE swan_dissipation, ONLY: SBOT, SICE, SMUD, SSURF, STURBV, SVEG, SWCAP, SWCAP8
   USE swan_wind_source, ONLY: WNDPAR, SWIND0, SWIND3, SWIND4, SWIND5

!****************************************************************

                  USE OCPCOMM2
                  USE OCPCOMM3
                  USE OCPCOMM4
                  USE SWCOMM1
                  USE SWCOMM2
                  USE SWCOMM3
                  USE SWCOMM4
                  USE SdsBabanin
                  USE SwanBraggScat

                  IMPLICIT NONE(TYPE, EXTERNAL)
                  TYPE(triad_state_t), INTENT(IN) :: TRIADS
                  TYPE(snl4_tables_t), INTENT(IN) :: SNL4
                  TYPE(spectral_powers_t), INTENT(IN) :: SPECTRAL_POWERS
                  TYPE(wcap_workspace_t), INTENT(INOUT) :: WCAP_WORKSPACE


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
!  0. Authors
!
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     32.06: Roeland Ris
!     40.02: IJsbrand Haagsma
!     40.03: Nico Booij
!     40.12: IJsbrand Haagsma
!     40.17: IJsbrand Haagsma
!     40.22: John Cazes and Tim Campbell
!     40.23: Marcel Zijlema
!     40.35: Nico Booij
!     40.41: Andre van der Westhuysen
!     40.41: Marcel Zijlema
!     40.55: Marcel Zijlema
!     40.59: Erick Rogers
!     40.61: Marcel Zijlema
!     40.85: Marcel Zijlema
!     41.75: Erick Rogers
!     41.80: Dirk Rijnsdorp and Ad Reniers
!
!  1. Updates
!
!     20.72, Jan. 96: Common introduced
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.82, Oct. 98: Updated description several variables
!     32.06, June 99: Updated argument list of WNDPAR
!     30.81, Sep. 99: Updated argument list of SSURF
!     40.03, Apr. 00: array Ursell added in argument list
!     40.02, Sep. 00: Replaced SWCAP1-5 by SWCAP
!     40.02, Oct. 00: References to CDRAGP and TAUWP removed
!     40.12, Nov. 00: Added WCAP to dissipation output (bug fix 40.11 A)
!     40.22, Sep. 01: Removed WAREA array.
!     40.22, Sep. 01: Changed array definitions to use the parameter
!                     MICMAX instead of ICMAX.
!     40.17, Dec. 01: Implemented Multiple DIA
!     40.23, Aug. 02: Print of CPU times added
!     40.23, Aug. 02: Parameter list of SWSNL2 and FILNL3 changed
!     40.35, Jun. 04: add turbulent viscosity model
!     40.41, May  04: Implementation of XNL (WRT) interface
!     40.41, Aug. 04: contribution due to reflection added to right-hand
!                     side of the system of equations
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.55, Dec. 05: introducing vegetation model
!     40.59, Aug. 07: introducing fluid mud-induced dissipation model
!     40.61, Sep. 06: introduction of all separate dissipation coefficients
!                     for output purposes
!     40.85, Aug. 08: add generation and redistribition for output purposes
!     41.75, Jan. 19: add dissipation by sea ice
!     41.80, Sep. 21: adding Bragg scattering
!
!  2. Purpose
!
!     to compute the source terms, i.e., bottom friction,
!     wave breaking, wind input, white capping and non linear
!     wave wave interactions
!
!  3. Method
!
!     ---
!
!  4. Argument variables
!
! i   ECOS  : =SPCDIR(*,2); cosine of spectral directions
! i   ESIN  : =SPCDIR(*,3); sine of spectral directions
! i   SPCDIR: (*,1); spectral directions (radians)
!             (*,2); cosine of spectral directions
!             (*,3); sine of spectral directions
!             (*,4); cosine^2 of spectral directions
!             (*,5); cosine*sine of spectral directions
!             (*,6); sine^2 of spectral directions
! i   SPCSIG: Relative frequencies in computational domain in sigma-space

                  REAL    ECOS(MDC)
                  REAL    ESIN(MDC)
                  REAL    SPCDIR(MDC,6)
                  REAL    SPCSIG(MSC)

!     INTEGERS:
!     --------------------------------------------------------------
!     IS       Counter of relative frequency band
!     IBOT     Indicator for bottom friction
!     ICUR     Indicator for current
!     ISURF    Indicator for wave breaking
!     ITRIAD   Indicator for nonlinear triad interactions
!     IQUAD    Indicator for nonlinear quadruplet interactions
!     IWCAP    Indicator for wave capping
!     IWIND    Indicator for which wind generation model is used
!     ICMAX    Maximum array size for the points of the molecul
!     MSC      Maximum counter of relative frequency in
!              computational model
!     MDC      Maximum counter of directional distribution in
!              computational model (2PI / DDIR + 1)
!     MTC      Maximum counter of the time, i.e.:
!              (total time in proto type) / (time step)
!     MBOT     Maximum array size for PBOT
!     MSURF    Maximum array size for PSSURF
!     MTRIAD   Maximum array size for PTRIAD
!     MWCAP    Maximum array size for PWCAP
!     MWIND    Maximum array size for PWIND
!     ISSTOP   Max frequency that is propagated within a sweep
!
!     REALS:
!     --------
!
!     DS          Width of frequency band (is not constant because
!                 of the logharitmic distribution of the frequency
!                 direction of the sweep (+1. OR -1. ) no input
!     GRAV        Gravitational acceleration
!     ABRBOT      Near bottom excursion amplitude
!     EMAX        Maximum energy according to the depth and the
!                 breaker parameter
!     ETOT        Total energy density per gridpoint
!     ETOTW       Total energy of the wind sea spectrum
!     GRAV        Gravitational acceleration
!     HM          Maximum wave height
!     KMESPC      Mean average wavenumber over full spectrum
!     SMESPC      Mean average frequency over full spectrum
!     QBLOC       Fraction of breaking waves
!
!     one and more dimensional arrays:
!     ---------------------------------
!
!     AC2       4D    (Nonstationary case) action density as function
!                     of D,S,X,Y at time T+DT
!     DEP2      2D    (Nonstationary case) depth as function of X and Y
!                     at time T+DIT
!     ECOS      1D    Represent the values of cos(d) of each spectral
!                     direction
!     ESIN      1D    Represent the values of sin(d) of each spectral
!                     direction
!     ALIMW     1D    Maximum energy by wind growth.
!     IMATDA    2D    coefficients of main diagonal
!     IMATRA    2D    right-hand side
!     KWAVE     2D    wavenumber as function of the relative frequency S
!                     and position IC(ix,iy)
!     PBOT      1D    Coefficient for the bottom friction models
!     PSURF     1D    Coefficient for the wave breaking model
!     PTRIAD    1D    Coefficient for the triad interaction model
!     PWCAP     1D    Coefficient for the white capping model
!     PWIND     1D    Coefficient for the wind growth model
!     UBOT      2D    Absolute orbital velocity in a gridpoint (IX,IY)
!     UX2       2D    (Nonstationary case) X-component of current velocity
!                     in (X,Y) at time T+DIT
!     UY2       2D    (Nonstationary case) Y-component of current velocity
!                     in (X,Y) at time T+DIT
!     USTAR     2D    Friction velocity at previous iteration for
!                     Janssen (1989,1990) wind input formulation
!     ZELEN     2D    Roughness length at previous iteration for
!                     Janssen (1989,1990) wind input formulation
!
!     Coefficients for the arrays:
!     -----------------------------
!                         default
!                         value:
!
!     PBOT(1)   = CFC      0.005    (Putnam and Collins equation)
!     PBOT(2)   = CFW      0.01     (Putnam and Collins equation)
!     PBOT(3)   = GAMJNS   0.038    (Jonswap formulation)
!     note: this lower friction value combined with second order polynomial wind drag
!     PBOT(4)   = MF      -0.08     (Madsen et al. equation)
!     PBOT(5)   = KN       0.05     (Madsen et al. bottom roughness)
!
!     PSURF(1)  = ALFA     1.0      (Battjes & Janssen, 1978)
!     PSURF(2)  = GAMMA    0.73     (breaking criterium)
!
!     PWCAP(1)  = ALFAWC   2.36e-5  (Emperical coefficient)
!     PWCAP(2)  = ALFAPM   3.02E-3  (Alpha of Pierson Moskowitz frequency)
!     PWCAP(3)  = CFJANS   4.5
!     PWCAP(4)  = DELTA    0.5
!     PWCAP(5)  = CFLHIG   1.
!     PWCAP(6)  = GAMBTJ   0.88     (Steepness limited wave breaking )
!
!     PWIND(1)  = CF10     188.0    (second generation wind growth model)
!     PWIND(2)  = CF20     0.59     (second generation wind growth model)
!     PWIND(3)  = CF30     0.12     (second generation wind growth model)
!     PWIND(4)  = CF40     250.0    (second generation wind growth model)
!     PWIND(5)  = CF50     0.0023   (second generation wind growth model)
!     PWIND(6)  = CF60    -0.2233   (second generation wind growth model)
!     PWIND(7)  = CF70     0.       (second generation wind growth model)
!     PWIND(8)  = CF80    -0.56     (second generation wind growth model)
!     PWIND(9)  = RHOAW    0.00125  (density air / density water)
!     PWIND(10) = EDMLPM   0.0036   (limit energy Pierson Moskowitz)
!     PWIND(11) = CDRAG    0.0012   (drag coefficient)
!     PWIND(12) = UMIN     1.0      (minimum wind velocity)
!     PWIND(13) = PMLM     0.13     (  )
!
!     arrays for Janssen (`89)
!     -----------
!     PWIND(14) 1D    alfa (which is tuned at 0.01)
!     PWIND(15) 1D    Kappa ( 0.41)
!     PWIND(16) 1D    Rho air (1.28)
!     PWIND(17) 1D    Rho water (1025)
!
!  6. Local variables
!
!     IT    : Number of the time-step
!
!     IQERR : Error indicator for SWINTFXNL interface
                  INTEGER IQERR

                  INTEGER, SAVE :: IENT = 0
                  INTEGER           :: ID, IDIA, IDC, IERR, IS, ISC, IT
                  INTEGER           :: N2, LMAX

                  REAL              :: DQ, DQ2, DT2

!     local copies of the interaction coefficients of one quadruplet
!     of the MDIA; the dummy arguments must not be written to here,
!     since they may be shared between threads
                  INTEGER           :: WWINT4(24)
                  INTEGER           :: WWINTL(24)
                  REAL              :: WWAWG4(8)
                  REAL              :: DAL14, DAL24, DAL34

!  7. Common blocks used
!
!     8. REMARKS
!
!   On setting the AICELOC and HICELOC variables:
!    * PICE(1) and PICE(2) are read in together, but their usage depends
!       on value of VARAICE, VARHICE:
!    * PICE(1) will be used for AICELOC *only* if VARAICE is false.
!    * PICE(2) will be used for HICELOC *only* if VARHICE is false.
!    * We check for case of IICE>6 as a reminder to update this subroutine
!      if/when new S_ice routines are added.
!
!     9. STRUCTURE
!
!   ------------------------------------------------------------
!   If SBOT is on (IBOT > 0 ) then,
!     Call SBOT  to compute the source term due to bottom friction
!      according to Hasselmann et al. (1974), Putnam and Jonsson (1949),
!      Madsen et al. (1991) or Smith et al. (2011)
!   ------------------------------------------------------------
!   If SVEG is on (IVEG > 0 ) then,
!     Call SVEG  to compute the source term due to vegetation
!      dissipation according to Dalrymple (1984) or Jacobsen et al. (2019)
!   ------------------------------------------------------------
!   If STURBV is on (ITURBV > 0 ) then,
!     Call STURBV  to compute the source term due to turbulence
!      dissipation according to Tolman (1961)
!   ------------------------------------------------------------
!   If SMUD is on (IMUD > 0 ) then,
!     Call SMUD  to compute the source term due to fluid mud
!      dissipation according to Ng (2000)
!   ------------------------------------------------------------
!   If SICE is on (IICE > 2 ) then,
!     Call SICE to compute the source term due to dissipation by sea ice.
!     For IICE=3, use method R19 : Rogers (2019)
!                                  - polynomial parametric function
!     For IICE=4, use method D15 : Doble et al. (2015)
!     For IICE=5, use method M18 : Meylan et al. (2018)
!                                  - model with O(3) power law
!     For IICE=6, use method R21B : Rogers et al. (2021B)
!   ------------------------------------------------------------
!   If SSURF is on (ISURF > 0 ) then,
!     Call SSURF to compute the source term due to wave breaking
!   ------------------------------------------------------------
!   IF IWIND =1 OR IWIND =2 THEN
!     Call WNDPAR (first or second generation mode of source terms
!                  using the DOLPHIN-B formulations)
!
!   else if IWIND = 3 then
!     input source term according to Snyder (1981)
!     Call SWIND3
!   else if IWIND = 4 then
!     input source term according to Janssen (1989,1991)
!     Call SWIND4
!   else if IWIND = 5 then
!     input source term according to Yan (1989) [reduces to Snyder form
!     for low frequencies and to Plant's (1982) form for high freq.
!     Call SWIND5
!   else if IWIND = 8 then
!     input source term according to Rogers et al. (JTECH 2012)
!     based on work of Donelan, Babanin, Tsagareli and others
!     Call SWIND_DBYB
!   ------------------------------------------------------------
!   If IWCAP > 1 then
!     Call SWCAP to compute the source term for white capping
!   --------------------------------------------------------------------
!   If ITRIAD > 0 then
!      Call SWLTA to compute the nonlinear 3 wave-wave interactions
!      based on the LTA technique (Eldeberky, 1996)
!   --------------------------------------------------------------------
!   If Ursell < Urmax
!   Then If IQUAD = 1
!        Then Call SWSNL1 to compute the nonlinear 4-wave interactions
!             semi-implicit per sweep direction
!        Else if IQUAD = 2
!        Then Call SWSNL2 to compute the nonlinear 4-wave interactions
!             fully explicit per sweep direction
!        Else if IQUAD = 3
!        Then Call SWSNL3 to compute the nonlinear 4-wave interactions
!             fully explicit per iteration
!             Call FILNL3 to get values for interactions from array
!             for full circle
!   --------------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************

                  INTEGER  ITER    ,IDWMIN  ,IDWMAX  ,SWPDIR  ,ISSTOP  ,&
                  &IDDTOP  ,IDDLOW  ,IX      ,IY

                  REAL     ABRBOT  ,ETOT    ,HM      ,QBLOC   ,ETOTW   ,&
                  &FPM     ,WIND10  ,THETAW  ,SMESPC  ,KMESPC  ,&
                  &SNLC1   ,FACHFR  ,DAL1    ,DAL2    ,DAL3    ,&
                  &UFRIC   ,SMEBRK  ,HS      ,XIS     ,KTETA   ,&
                  &AICELOC ,HICELOC ,DISBK

                  REAL  :: AC2(MDC,MSC,MCGRD)
                  REAL  :: DEP2(MCGRD)
                  REAL  :: ALIMW(MDC,MSC)
                  REAL  :: IMATDA(MDC,MSC)
                  REAL  :: IMATRA(MDC,MSC)
!     Changed ICMAX to MICMAX, since MICMAX doesn't vary over gridpoint
                  REAL  :: KWAVE(MSC,MICMAX)
                  REAL  :: DMW(MSC,MICMAX)
                  REAL  :: UBOT(MCGRD)
                  REAL  :: UX2(MCGRD)
                  REAL  :: UY2(MCGRD)
                  REAL  :: UE(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: SA1(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: SA2(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA1C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA1P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA1M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA2C(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA2P(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DA2M(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: SFNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: DSNL(MSC4MI:MSC4MA , MDC4MI:MDC4MA )
                  REAL  :: MEMNL4(MDC,MSC,MCGRD)
                  REAL  :: MEMBRG(MDC,MSC,MCGRD)
                  REAL  :: MEMSINA(MDC,MSC,MCGRD)
                  REAL  :: MEMSINB(MDC,MSC,MCGRD)
                  REAL  :: PLWNDS(MDC,MSC,NPTST)
                  REAL  :: PLWNDD(MDC,MSC,NPTST)
                  REAL  :: PLWCAP(MDC,MSC,NPTST)
                  REAL  :: PLBTFR(MDC,MSC,NPTST)
                  REAL  :: PLWBRK(MDC,MSC,NPTST)
                  REAL  :: PLNL4S(MDC,MSC,NPTST)
                  REAL  :: PLNL4D(MDC,MSC,NPTST)
                  REAL  :: PLVEGT(MDC,MSC,NPTST)
                  REAL  :: PLTURB(MDC,MSC,NPTST)
                  REAL  :: PLMUD (MDC,MSC,NPTST)
                  REAL  :: PLICE (MDC,MSC,NPTST)
                  REAL  :: PLSWEL(MDC,MSC,NPTST)
                  REAL  :: PLTRI (MDC,MSC,NPTST)
                  REAL  :: PLBRAG(MDC,MSC,NPTST)
                  REAL  :: WWAWG(*)
                  REAL  :: WWSWG(*)
                  REAL  :: CGO(MSC,MICMAX)
                  REAL  :: CAS(MDC,MSC,MICMAX)
                  REAL  :: USTAR(MCGRD)
                  REAL  :: ZELEN(MCGRD)
                  REAL  :: TAUWV(MCGRD)
                  REAL  :: DISSC0(1:MDC,1:MSC,1:MDISP)
                  REAL  :: DISSC1(1:MDC,1:MSC,1:MDISP)
                  REAL  :: GENC0 (1:MDC,1:MSC,1:MGENR)
                  REAL  :: GENC1 (1:MDC,1:MSC,1:MGENR)
                  REAL  :: REDC0 (1:MDC,1:MSC,1:MREDS)
                  REAL  :: REDC1 (1:MDC,1:MSC,1:MREDS)
                  REAL  :: URSELL(MCGRD)
                  REAL  :: BIPHAS(MCGRD)
                  REAL  :: FRCOEF(MCGRD)
                  REAL  :: NPLA2(MCGRD)
                  REAL  :: TURBV2(MCGRD)
                  REAL  :: MUDL2(MCGRD)
                  REAL  :: QTL1(:,:), QTL2(:,:)
                  REAL, INTENT(IN)  :: AICE2(MCGRD)
                  REAL, INTENT(IN)  :: HICE2(MCGRD)
                  REAL  :: REFLSO(MDC,MSC)
                  REAL  :: FBD(MDC,MDC,MSC)
                  REAL  :: URMSTOP(MCGRD)

                  INTEGER  IDCMIN(MSC)    ,&
                  &IDCMAX(MSC)    ,&
                  &WWINT(*)

                  LOGICAL  GROWW(MDC,MSC) ,&
                  &ANYBIN(MDC,MSC),&
                  &ANYWND(MDC)

                  IF (LTRACE) CALL STRACE (IENT,'SOURCE')

!     *** set relevant matrix elements to 0 ***

                  IF (OPTG.NE.5) THEN
                     IMATRA = 0.
                     IMATDA = 0.
                  ENDIF

!     *** set all dissipation coeff at 0 ***

                  DISSC0(1:MDC,1:MSC,1:MDISP) = 0.
                  DISSC1(1:MDC,1:MSC,1:MDISP) = 0.

!     *** set all generation coeff at 0 ***

                  GENC0(1:MDC,1:MSC,1:MGENR) = 0.
                  GENC1(1:MDC,1:MSC,1:MGENR) = 0.

!     *** set all redistribution coeff at 0 ***

                  REDC0(1:MDC,1:MSC,1:MREDS) = 0.
                  REDC1(1:MDC,1:MSC,1:MREDS) = 0.

!     *** set local ice concentration ***
!     (see Remarks)

                  AICELOC = 0.
                  IF ( VARAICE ) THEN
                     AICELOC = AICE2(KCGRD(1))
                  ELSEIF ( IICE.GE.3 ) THEN
                     AICELOC = PICE(1)
                  ELSEIF ( IICE.GT.6 ) THEN
                     CALL MSGERR (3,'invalid IICE option')
                  ENDIF

!     *** set local ice thickness ***
!     (see Remarks)

                  HICELOC = 0.
                  IF ( VARHICE ) THEN
                     HICELOC = HICE2(KCGRD(1))
                  ELSE
                     HICELOC = PICE(2)
                  ENDIF

!TIMG                  CALL SWTSTA(130)
                  IF (IBOT .GE. 1) THEN

!       *** wave-bottom interactions ***

                     CALL SBOT (ABRBOT   ,DEP2     ,ECOS     ,ESIN     ,AC2      ,&
                     &IMATDA   ,KWAVE    ,SPCSIG   ,UBOT     ,UX2      ,&
                     &UY2      ,IDCMIN   ,IDCMAX   ,IT       ,ITER     ,&
                     &SWPDIR   ,PLBTFR   ,ISSTOP   ,DISSC1   ,VARFR    ,&
                     &FRCOEF   )
                  END IF
!TIMG                  CALL SWTSTO(130)
!
!TIMG                  CALL SWTSTA(138)
                  IF ( IMUD.GE.1 ) THEN

!     *** wave-mud interactions ***
!
!        *** energy dissipation according to Ng (2000)

                     CALL SMUD ( DEP2   ,IMATDA  ,&
                     &KWAVE  ,CGO     ,DMW     ,&
                     &IDCMIN ,IDCMAX  ,ISSTOP  ,&
                     &DISSC1 ,PLMUD   )
                  END IF
!TIMG                  CALL SWTSTO(138)
!
!TIMG                  CALL SWTSTA(139)
                  IF ( IVEG.GE.1 ) THEN

!     *** wave-vegetation interactions ***

                     CALL SVEG (DEP2   ,IMATDA   ,ETOT   ,SMEBRK    ,&
                     &KWAVE  ,KMESPC   ,PLVEGT ,&
                     &IDCMIN ,IDCMAX   ,ISSTOP ,DISSC1    ,&
                     &NPLA2  )
                  END IF
!TIMG                  CALL SWTSTO(139)
!
!TIMG                  CALL SWTSTA(143)
                  IF ( ITURBV.GE.1 ) THEN

!        *** dissipation due to turbulent viscosity ***

                     CALL STURBV (TURBV2  ,DEP2    ,IMATDA  ,&
                     &IDCMIN  ,IDCMAX  ,ISSTOP  ,&
                     &KWAVE   ,DISSC1  ,PLTURB, SPECTRAL_POWERS%value)
                  END IF
!TIMG                  CALL SWTSTO(143)
!
!TIMG                  CALL SWTSTA(144)
                  IF ( IICE.GT.2 ) THEN

!        *** dissipation by sea ice ***

                     CALL SICE   (IMATDA , IDCMIN  , IDCMAX , ISSTOP&
                     &, DISSC1 , PLICE   , AICELOC, HICELOC&
                     &, SPCSIG  , CGO&
                     &)

                  END IF
!TIMG                  CALL SWTSTO(144)
!
!TIMG                  CALL SWTSTA(131)
                  IF (ISURF .GE. 1) THEN

!         *** calculate surf breaking source term (5 formulations) ***

                     CALL SSURF (ETOT    ,HM      ,QBLOC   ,SMEBRK  ,KTETA   ,&
                     &KMESPC  ,SPCSIG  ,AC2     ,IMATRA  ,&
                     &IMATDA  ,IDCMIN  ,IDCMAX  ,PLWBRK  ,&
                     &ISSTOP  ,DISSC0  ,DISSC1  ,DISBK   ,ITER,&
                     &WCAP_WORKSPACE%mean_frequency_wam )

                  END IF
!TIMG                  CALL SWTSTO(131)
!
!TIMG                  CALL SWTSTA(132)
                  IF ( IWIND .GE. 3&
                  &) THEN

!       *** linear wind input according to Cavaleri and Malanotte ***
!       *** Rizolli (1981) for a third generation mode of SWAN    ***

                     IF (PWIND(31) .GT. 1.E-20) THEN
                        IF ( IWIND.NE.8 ) THEN
                           CALL SWIND0 (IDCMIN  ,IDCMAX  ,ISSTOP  ,&
                           &SPCSIG  ,THETAW  ,ANYWND  ,&
                           &UFRIC   ,FPM     ,PLWNDS  ,&
                           &IMATRA  ,SPCDIR  ,GENC0   ,&
                           &KWAVE   ,AICELOC )
                        ELSE
                           IF ( SWPDIR .EQ. 1 .OR.&
                           &(SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                           &(SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                           &(SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1)) )&
                           &CALL SWIND0_NRL (SPCSIG  ,THETAW  ,ANYWND  ,&
                           &UFRIC   ,FPM     ,MEMSINA ,&
                           &SPCDIR  ,KWAVE   )

!       *** get source term value of array MEMSINA for the bin that  ***
!       *** falls within a sweep and store in right hand side IMATRA ***

                           CALL FILSIN( MEMSINA, IDCMIN, IDCMAX, IMATRA, ANYWND,&
                           &PLWNDS , ISSTOP, GENC0 , AICELOC )
                        ENDIF
                     ENDIF
                  ENDIF

                  IF ( IWIND .EQ. 1 .OR. IWIND .EQ. 2 ) THEN

                     CALL WNDPAR (ISSTOP,IDWMIN,IDWMAX,IDCMIN,IDCMAX,&
                     &DEP2  ,WIND10,GENC0,GENC1,&
                     &THETAW,AC2   ,KWAVE ,IMATRA,IMATDA,&
                     &SPCSIG,CGO   ,ALIMW ,GROWW ,ETOTW ,&
                     &PLWNDS,PLWNDD,SPCDIR,ITER,AICELOC    )


                  ELSE IF ( IWIND .EQ. 3 ) THEN

!       *** Wind input according to Snyder et al (1981) ***

                     CALL SWIND3 (SPCSIG  ,THETAW  ,&
                     &KWAVE   ,IMATRA  ,GENC0   ,&
                     &IDCMIN  ,IDCMAX  ,AC2     ,UFRIC   ,&
                     &FPM     ,PLWNDS  ,ISSTOP  ,SPCDIR  ,&
                     &ANYWND  ,AICELOC )

                  ELSE IF ( IWIND .EQ. 4 ) THEN

!       *** Wind input according to Janssen (1989,1991) ***

                     CALL SWIND4  (IDWMIN  ,IDWMAX  ,&
                     &SPCSIG  ,WIND10  ,THETAW  ,XIS     ,&
                     &DDIR    ,KWAVE   ,IMATRA  ,GENC0   ,&
                     &IDCMIN  ,IDCMAX  ,AC2     ,UFRIC   ,&
                     &PLWNDS  ,ISSTOP  ,ITER    ,USTAR   ,ZELEN   ,&
                     &SPCDIR  ,ANYWND  ,IT      ,TAUWV   ,AICELOC )

                  ELSE IF ( IWIND .EQ. 5 ) THEN

!       *** Wind input according to Yan (1989) ***

                     CALL SWIND5 (SPCSIG  ,THETAW  ,ISSTOP  ,&
                     &UFRIC   ,KWAVE   ,IMATRA  ,IDCMIN  ,&
                     &IDCMAX  ,AC2     ,ANYWND  ,PLWNDS  ,&
                     &SPCDIR  ,GENC0   ,AICELOC          )

                  ELSE IF ( IWIND .EQ. 8 ) THEN

!       *** wind according to Rogers et al. (JTECH 2012)
!           based on work of Donelan, Babanin, Tsagareli and others

                     IF ( SWPDIR .EQ. 1 .OR.&
                     &(SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                     &(SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                     &(SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1)) )&
                     &CALL SWIND_DBYB (SPCSIG  ,THETAW  ,KWAVE  ,MEMSINA ,&
                     &MEMSINB ,AC2     ,UFRIC  ,WIND10  ,&
                     &SPCDIR  ,ANYWND  ,CGO    ,ZELEN   )

!       *** get source term value of array MEMSINB for the bin that  ***
!       *** falls within a sweep and store in right hand side IMATRA ***

                     CALL FILSIN( MEMSINB, IDCMIN, IDCMAX, IMATRA, ANYWND,&
                     &PLWNDS , ISSTOP, GENC0 , AICELOC )

                  END IF
!TIMG                  CALL SWTSTO(132)
!
!     Calculate whitecapping source term (multiple formulations)
!
!TIMG                  CALL SWTSTA(133)
                  IF (IWCAP.GE.1) THEN
                     IF (IWCAP.LE.7) CALL SWCAP (SPCDIR  ,SPCSIG  ,KWAVE   ,AC2     ,&
                     &IDCMIN  ,IDCMAX  ,ISSTOP  ,&
                     &ETOT    ,IMATDA  ,IMATRA  ,PLWCAP  ,&
                     &CGO     ,UFRIC   ,CAS     ,&
                     &DEP2    ,DISSC1  ,DISSC0,&
                     &WCAP_WORKSPACE)
                     IF (IWCAP.EQ.8) CALL SWCAP8 (SPCDIR  ,SPCSIG  ,KWAVE   ,AC2 ,&
                     &IDCMIN  ,IDCMAX  ,ISSTOP  ,&
                     &ETOT    ,IMATDA  ,IMATRA  ,PLWCAP  ,&
                     &CGO     ,UFRIC   ,&
                     &DEP2    ,DISSC1  ,DISSC0, WCAP_WORKSPACE)
                  END IF

!     For now, we only call SSWELL if Babanin physics are in use
                  IF (IWCAP.EQ.8) THEN
                     IF (ZIEGER) THEN
                        CALL SSWELL_ZIEGER (SPCSIG, KWAVE, AC2, CGO, ISSTOP&
                        &,IDCMIN ,IDCMAX, MDC, DISSC1, IMATDA&
                        &,TESTFL,IPTST,PLSWEL)
                     ELSE IF (ROGERS) THEN
                        CALL SSWELL_ROGERS (SPCSIG, KWAVE, IDCMIN, IDCMAX&
                        &,ISSTOP , DISSC1 ,ETOT    ,IMATDA&
                        &,URMSTOP(KCGRD(1)), GRAV , PWIND(9), MDC&
                        &,TESTFL,IPTST,PLSWEL,CGO,CDSV,FESWELL)
                     ELSE IF (ARDHUIN) THEN
                        CALL SSWELL_ARDHUIN (SPCSIG, THETAW, KWAVE, IDCMIN,&
                        &IDCMAX, ISSTOP, DISSC1, ETOT, IMATDA,&
                        &SPCDIR, UFRIC ,&
                        &URMSTOP(KCGRD(1)), GRAV,PWIND(9),&
                        &TESTFL,IPTST,PLSWEL,MDC,CGO,CDSV)
                     ELSE
                        CALL MSGERR(4,' Sswell must be defined! ')
!           note that CGO is for diagnostic purposes only, may be omitted
!           excluded : SPCDIR AC2 DEP2 IMATRA
                     END IF
                  END IF
!TIMG                  CALL SWTSTO(133)
!
!     compute nonlinear interactions, starting with triads
!
!TIMG                  CALL SWTSTA(134)
                  IF (ITRIAD .GT. 0) THEN

!       *** compute the 3 wave-wave interactions if in each ***
!       *** geographical gridpoint a continuous spectrum    ***
!       *** is present, i.e., after first iteration         ***

                     IF ( ICUR .EQ. 0 .OR. ITER .GT. 1 ) THEN

                        IF (ITRIAD.EQ.1.OR.ITRIAD.EQ.11) THEN
!             LTA
                           CALL SWLTA ( AC2   , DEP2  , CGO   , SPCSIG,&
                           &IMATRA, IMATDA, REDC0 , REDC1 ,&
                           &IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
                           &SMEBRK, PLTRI , URSELL, BIPHAS, QTL2, TRIADS )
                        ELSEIF (ITRIAD.EQ.2 .OR. ITRIAD.EQ.3) THEN
!             SPB or FTIM
                           CALL SWFTIM ( AC2   , SPCSIG,&
                           &IMATRA, IMATDA, REDC0 , REDC1 ,&
                           &IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
                           &PLTRI , URSELL, BIPHAS,&
                           &QTL1  , QTL2  )
                        ELSEIF (ITRIAD.EQ.5) THEN
!             DCTA
                           IF ( TRIADS%collinear ) THEN
                              CALL SWDCTA ( AC2   , DEP2  , CGO   , SPCSIG,&
                              &IMATRA, IMATDA, REDC0 , REDC1 ,&
                              &IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
                              &SMEBRK, PLTRI , URSELL, BIPHAS,&
                              &QTL1  , QTL2  )
                           ELSE
                              CALL SWDNCTA ( AC2   , DEP2  , CGO   , SPCSIG, SPCDIR,&
                              &KWAVE , IMATRA, IMATDA, REDC0 , REDC1 ,&
                              &IDDLOW, IDDTOP, ISSTOP, IDCMIN, IDCMAX,&
                              &ETOT  , SMEBRK, PLTRI , URSELL, BIPHAS,&
                              &QTL1  , QTL2  )
                           ENDIF
                        ENDIF

                     ENDIF

                  ENDIF
!TIMG                  CALL SWTSTO(134)
!
!     --- compute quadruplet interactions if Ursell number < Urmax
!
!TIMG                  CALL SWTSTA(135)
                  IF (URSELL(KCGRD(1)).LT.PTRIAD(3)) THEN

!       *** compute the counters for the nonlinear four ***
!       *** wave-wave interactions in spectral space    ***
!       *** and high frequency factor                   ***

                     IF ( IQUAD .GE. 1 ) THEN
!          RANGE4 updates elements 13 and 14. Keep those per-gridpoint
!          values local because WWINT is shared by the unstructured
!          OpenMP path. IQUAD=4 uses its cached per-quadruplet copy belo
                        IF ( IQUAD .NE. 4 ) THEN
                           WWINTL(1:24) = WWINT(1:24)
                           CALL RANGE4 (WWINTL,IDDLOW,IDDTOP )
                        ENDIF
                        FACHFR = 1. / XIS ** PWTAIL(1)
                     ENDIF


                     IF (IQUAD .EQ. 1) THEN

!       *** semi-implicit calculation for all the bins that fall ***
!       *** within a sweep. No additional array is required      ***

                        CALL SWSNL1 (                  WWINTL  ,WWAWG   ,WWSWG   ,&
                        &IDCMIN  ,IDCMAX  ,UE      ,SA1     ,&
                        &SA2     ,DA1C    ,DA1P    ,DA1M    ,DA2C    ,&
                        &DA2P    ,DA2M    ,SPCSIG  ,SNLC1   ,KMESPC  ,&
                        &FACHFR  ,ISSTOP  ,DAL1    ,DAL2    ,DAL3    ,&
                        &SFNL    ,DSNL    ,DEP2    ,AC2     ,IMATDA  ,&
                        &IMATRA  ,PLNL4S  ,PLNL4D                    ,&
                        &IDDLOW  ,IDDTOP  ,REDC0   ,REDC1, SNL4 )

                     ELSE IF ( IQUAD .EQ. 2) THEN

!         *** fully explicit calculation for all the bins that fall ***
!         *** within a sweep. No additional array is required       ***

                        CALL SWSNL2 (                  IDDLOW  ,IDDTOP  ,WWINTL  ,&
                        &WWAWG   ,UE      ,SA1     ,ISSTOP  ,&
                        &SA2     ,SPCSIG  ,SNLC1   ,DAL1    ,DAL2    ,&
                        &DAL3    ,SFNL    ,DEP2    ,AC2     ,KMESPC  ,&
                        &REDC0   ,REDC1   ,IMATDA  ,IMATRA  ,&
                        &FACHFR  ,PLNL4S  ,         IDCMIN  ,IDCMAX, SNL4 )

                     ELSE IF ( IQUAD .EQ. 3) THEN

!         *** fully explicit calculation of the 4 wave-wave inter-  ***
!         *** actions for the full circle (1 -> MDC). An additional ***
!         *** array is required in which the values are stored prior***
!         *** to every iteration                                    ***

                        IF ( ITER .EQ. 1 ) THEN

!           *** calculate the interactions every sweep in each grid ***
!           *** point for the first iteration to ensure stable      ***
!           *** behaviour of the model                              ***

                           CALL SWSNL3 (                  WWINTL  ,WWAWG   ,&
                           &UE      ,SA1     ,SA2     ,SPCSIG  ,SNLC1   ,&
                           &DAL1    ,DAL2    ,DAL3    ,SFNL    ,DEP2    ,&
                           &AC2     ,KMESPC  ,MEMNL4  ,FACHFR, SNL4     )

                        ELSE IF ( ITER .GT. 1 .AND. ( SWPDIR .EQ. 1 .OR.&
                        &( SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1)) )) THEN

                           CALL SWSNL3 (                  WWINTL  ,WWAWG   ,&
                           &UE      ,SA1     ,SA2     ,SPCSIG  ,SNLC1   ,&
                           &DAL1    ,DAL2    ,DAL3    ,SFNL    ,DEP2    ,&
                           &AC2     ,KMESPC  ,MEMNL4  ,FACHFR, SNL4     )

                        ENDIF

!         *** Get source term value of additional array for the bin   ***
!         *** that fall within a sweep and store in right hand vector ***

                        CALL FILNL3 (IDCMIN  ,IDCMAX  ,IMATRA  ,IMATDA  ,AC2     ,&
                        &MEMNL4  ,PLNL4S  ,ISSTOP  ,REDC0   ,REDC1   )

                     ELSE IF (IQUAD .EQ. 4) THEN

!         Multiple DIA according to Hashimoto (1999)

                        IF ((ITER.EQ.1).OR.( ITER .GT. 1 .AND. ( SWPDIR .EQ. 1 .OR.&
                        &( SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1))))) THEN
                           DO IDIA=1,SNL4%quadruplet_count
!             --- restore the coefficients of this quadruplet from the
!                 cache filled by SWPRE4W into thread-local copies;
!                 FACHFR is set above and does not depend on the
!                 quadruplet
                              WWINT4(1:24) = SNL4%cached_indices(1:24,IDIA)
                              WWAWG4(1:8) = SNL4%cached_angular_weights(1:8,IDIA)
                              DAL14 = SNL4%cached_dal1(IDIA)
                              DAL24 = SNL4%cached_dal2(IDIA)
                              DAL34 = SNL4%cached_dal3(IDIA)
                              CALL RANGE4 (WWINT4,IDDLOW,IDDTOP )
                              CALL SWSNL4 (WWINT4  ,WWAWG4  ,&
                              &SPCSIG  ,SNLC1   ,&
                              &DAL14   ,DAL24   ,DAL34   ,DEP2    ,&
                              &AC2     ,KMESPC  ,MEMNL4  ,FACHFR  ,&
                              &IDIA    ,ITER    ,UE      ,SA1     ,&
                              &SA2     ,SFNL    ,SNL4)
                           END DO
                        ENDIF

!         Fill the matrix per sweep even though the quadruplets are calculated
!         only once per iteration

                        CALL FILNL3 (IDCMIN  ,IDCMAX  ,IMATRA  ,IMATDA  ,AC2     ,&
                        &MEMNL4  ,PLNL4S  ,ISSTOP  ,REDC0   ,REDC1   )

                     ELSE IF ( IQUAD .EQ. 8) THEN

!         --- fully explicit calculation of the 4 wave-wave
!             interactions for the full circle. The interactions
!             in neighbouring bins are interpolated in piecewise
!             constant manner. An additional array is required in
!             which the values are stored prior to every iteration

                        IF ( ITER .EQ. 1 ) THEN

!           *** calculate the interactions every sweep in each grid ***
!           *** point for the first iteration to ensure stable      ***
!           *** behaviour of the model                              ***

                           CALL SWSNL8 (WWINTL  ,UE      ,SA1     ,SA2     ,SPCSIG  ,&
                           &SNLC1   ,DAL1    ,DAL2    ,DAL3    ,SFNL    ,&
                           &DEP2    ,AC2     ,KMESPC  ,MEMNL4  ,FACHFR, SNL4 )

                        ELSE IF ( ITER .GT. 1 .AND. ( SWPDIR .EQ. 1 .OR.&
                        &( SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                        &( SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1)) )) THEN

                           CALL SWSNL8 (WWINTL  ,UE      ,SA1     ,SA2     ,SPCSIG  ,&
                           &SNLC1   ,DAL1    ,DAL2    ,DAL3    ,SFNL    ,&
                           &DEP2    ,AC2     ,KMESPC  ,MEMNL4  ,FACHFR, SNL4 )

                        ENDIF

!         *** get source term value of additional array for the bin   ***
!         *** that fall within a sweep and store in right hand vector ***

                        CALL FILNL3 (IDCMIN  ,IDCMAX  ,IMATRA  ,IMATDA  ,AC2     ,&
                        &MEMNL4  ,PLNL4S  ,ISSTOP  ,REDC0   ,REDC1   )

                     ELSEIF ((IQUAD.EQ.51).OR.(IQUAD.EQ.52).OR.(IQUAD.EQ.53)) THEN

!         Calculate the quadruplets using the XNL interface of
!         G. van Vledder
!
!         Avoid calculation of the quadruplets in more than one sweep

                        IF ((ITER .GE. 1) .AND.&
                        &( (SWPDIR.EQ.1)                                 .OR.&
                        &((SWPDIR.EQ.2).AND.(IX.EQ.1)                  ).OR.&
                        &((SWPDIR.EQ.3).AND.(IY.EQ.1)  .AND.(.NOT.ONED)).OR.&
                        &((SWPDIR.EQ.4).AND.(IX.EQ.MXC).AND.(IY.EQ.1)&
                        &.AND.(.NOT.ONED)) )&
                        &) THEN

                           IF (ITEST.GE.30) WRITE(PRINTF,'(A,4I6,F12.2)')&
                           &'SOURCE XNL: iter, swpdir, kcgrd(1), iquad, depth:',&
                           &ITER, SWPDIR, KCGRD(1), IQUAD, DEP2(KCGRD(1))

                           CALL SWINTFXNL(AC2,SPCSIG,SPCDIR,MDC,MSC,MCGRD,&
                           &DEP2,IQUAD,MEMNL4,KCGRD,ICMAX,IQERR)

                        ENDIF

                        IF (ITEST.GE.30) THEN
                           WRITE (PRTEST,*) '+SOURCE: IX, IY, SWPDIR: ',&
                           &IX, IY, SWPDIR
                        ENDIF
                        IF (TESTFL.AND.ITEST.GE.100) THEN
                           DO IS=1, MSC
                              DO ID = 1, MDC
                                 WRITE(PRINTF,"(' SOURCE: IS ID MEMNL(): ',2I6,E12.4)") IS,ID,MEMNL4(ID,IS,KCGRD(1))
                              ENDDO
                           ENDDO
                        ENDIF

                        CALL FILNL3 (IDCMIN  ,IDCMAX  ,IMATRA  ,IMATDA  ,AC2     ,&
                        &MEMNL4  ,PLNL4S  ,ISSTOP  ,REDC0   ,REDC1   )

                        IF (ITEST.GE.30) THEN
                           WRITE (PRTEST,*) '+SOURCE: ITER, IQUAD, SWPDIR, IQERR: ',&
                           &ITER, IQUAD, SWPDIR, IQERR
                        ENDIF

                     ENDIF
                  ENDIF
!TIMG                  CALL SWTSTO(135)
!
!TIMG                  CALL SWTSTA(145)
                  IF ( IBRAG.EQ.1 ) THEN

!        *** calculation for all the bins that fall within a sweep ***
!
!        *** Bragg scattering ***

                     CALL SWBRAGG1 ( IMATRA, AC2   , DEP2  , KWAVE , CGO   ,&
                     &SPCSIG, IDCMIN, IDCMAX, ISSTOP,&
                     &ECOS  , ESIN  , PLBRAG, REDC0 )

                  ELSE IF ( IBRAG.EQ.2 ) THEN

!        *** calculation for all the bins that fall within a sweep ***
!
!        *** bed elevation spectrum at wave number difference ***

                     CALL SWFB( FBD, DEP2, KWAVE, ECOS, ESIN )

!        *** Bragg scattering ***

                     CALL SWBRAGG2 ( IMATRA, AC2   , DEP2  , KWAVE , CGO   ,&
                     &FBD   , SPCSIG, IDCMIN, IDCMAX, ISSTOP,&
                     &ECOS  , ESIN  , PLBRAG, REDC0 )

                  ELSE IF ( IBRAG.EQ.3 ) THEN

!        *** calculation of the Bragg scattering for the full circle ***

                     IF ( SWPDIR .EQ. 1 .OR.&
                     &( SWPDIR .EQ. 2 .AND. IX .EQ. 1) .OR.&
                     &( SWPDIR .EQ. 3 .AND. IY .EQ. 1) .OR.&
                     &( SWPDIR .EQ. 4 .AND. (IX.EQ.MXC .AND. IY.EQ.1)) ) THEN

!           *** bed elevation spectrum at wave number difference ***

                        CALL SWFB( FBD, DEP2, KWAVE, ECOS, ESIN )

!           *** Bragg scattering ***

                        CALL SWBRAGG3 ( MEMBRG, AC2   , DEP2, KWAVE, CGO   ,&
                        &FBD   , SPCSIG, ECOS, ESIN )

                     ENDIF

!        *** get source term value for the bin that fall within ***
!        *** a sweep and store in right hand vector             ***

                     CALL FILBRG ( IMATRA, IDCMIN, IDCMAX, ISSTOP,&
                     &MEMBRG, PLBRAG, REDC0 )

                  ELSE IF ( IBRAG.EQ.4 ) THEN

!        *** implicit integration according to      ***
!            Ardhuin and Herbers (2002), see pg. 22 ***

                  ENDIF
!TIMG                  CALL SWTSTO(145)
!
!     --- add contribution due to reflection of obstacles
!
!TIMG                  CALL SWTSTA(136)
                  IF (NUMOBS.NE.0) THEN
                     DO IS = 1, MSC
                        DO ID = 1, MDC
                           IF (ANYBIN(ID,IS))&
                           &IMATRA(ID,IS) = IMATRA(ID,IS) + REFLSO(ID,IS)
                        END DO
                     END DO
                  END IF
!TIMG                  CALL SWTSTO(136)
!
!     End of the subroutine SOURCE
                  RETURN
               end subroutine SOURCE

!************************************************************************

               SUBROUTINE PHILIM(AC2,AC2OLD,CGO,KWAVE,SPCSIG,ANYBIN,ISLMIN,NFLIM,&
               &QB_LOC)

!************************************************************************

                  USE SWCOMM3

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
!     30.82: IJsbrand Haagsma
!     40.16: IJsbrand Haagsma
!     40.23: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     30.82, Feb. 99: New subroutine
!     40.16, Dec. 01: Implemented limiter switch
!     40.23, Aug. 02: Store number of frequency use of limiter
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Limits the change in action density between two iterations to a
!     certain percentage of the (directionally independent) Phillips
!     equilibrium level
!
!  3. Method
!
!     The maximum change of energy density per bin is related to
!     the (directionally independent) Phillips equilibrium level.
!     This change is estimated as:
!
!     |D E(s)| = factor * alpha_PM * g^2 / (s^5)
!
!     in which the Phillips' constant for a Pierson-Moskowitz
!     spectrum (alpha_PM) is taken to be 0.0081. Note that this
!     is a measure for a 1D spectrum. In SWAN, factor = 0.1
!     (stored in PNUMS(20)).
!
!     In terms of action density, we have
!
!     |D N(s)| = factor * alpha_PM * g^2 / (s^6)
!
!     Expressing in wave number k, this becomes with a deep
!     water approach of s^2 = gk:
!
!     |D N(s)| = factor * alpha_PM / (s^2 k^2)
!
!     Furthermore, with s = 2*k*c_g (deep water), we finally
!     have (Ris, 1997, p.36):
!
!                          alpha_PM
!     |D N(s,t)| = factor -----------
!                         2 s k^3 c_g
!
!     In cases where waves are breaking the dissipation of energy
!     is not limited. This is assumed to be the case when the
!     fraction of breaking waves Qb is more than 1.e-5.
!
!  4. Argument variables

                  LOGICAL ANYBIN(MDC,MSC)

!     ISLMIN: Lowest sigma-index occured in applying limiter
!     NFLIM : Number of frequency use of limiter
!     QB_LOC: Local value of Qb (fraction of breaking waves)

                  INTEGER ISLMIN(MCGRD), NFLIM(MCGRD)
                  REAL    QB_LOC
                  REAL    AC2(MDC,MSC,MCGRD)
                  REAL    AC2OLD(MDC,MSC)
                  REAL    CGO(MSC,MICMAX)
                  REAL    KWAVE(MSC,MICMAX)
                  REAL    SPCSIG(MSC)

!  6. Local variables
!
!     ID    : Counter for directional (theta) space
!     IS    : Counter for frequency (sigma) space

                  INTEGER ID,IS,NLIMIT

!     DAC2MX: Maximum deviation of action density AC2 between iterations

                  REAL    DAC2MX

! 13. Source text

                  IF (MSC.GT.3) THEN
                     DO IS=1,MSC
                        DAC2MX=ABS((PNUMS(20)*0.0081)/&
                        &(2.*SPCSIG(IS)*(KWAVE(IS,1)**3)*CGO(IS,1)))
                        NLIMIT=0
                        IF (QB_LOC.LT.PNUMS(28)) THEN
                           DO ID=1,MDC
                              IF (ANYBIN(ID,IS)) THEN
                                 IF (AC2(ID,IS,KCGRD(1)).GT.AC2OLD(ID,IS)+DAC2MX) THEN
                                    AC2(ID,IS,KCGRD(1))=AC2OLD(ID,IS)+DAC2MX
                                    NLIMIT=NLIMIT+1
                                 ELSE IF (AC2(ID,IS,KCGRD(1)).LT.AC2OLD(ID,IS)-DAC2MX) THEN
                                    AC2(ID,IS,KCGRD(1))=AC2OLD(ID,IS)-DAC2MX
                                    NLIMIT=NLIMIT+1
                                 END IF
                              END IF
                           END DO
                        ELSE
                           DO ID=1,MDC
                              IF (ANYBIN(ID,IS) .AND.&
                              &AC2(ID,IS,KCGRD(1)).GT.AC2OLD(ID,IS)+DAC2MX) THEN
                                 AC2(ID,IS,KCGRD(1))=AC2OLD(ID,IS)+DAC2MX
                                 NLIMIT=NLIMIT+1
                              END IF
                           END DO
                        END IF
                        IF (NLIMIT.GT.0) THEN
                           NFLIM(KCGRD(1)) = NFLIM(KCGRD(1)) + NLIMIT
                           ISLMIN(KCGRD(1)) = MIN(IS,ISLMIN(KCGRD(1)))
                        END IF
                     END DO
                  END IF
                  RETURN
               end subroutine PHILIM
!************************************************************************

               SUBROUTINE HJLIM(AC2,AC2OLD,CGO,KWAVE,SPCSIG,ANYBIN,ISLMIN,NFLIM,&
               &QB_LOC,USTAR)

!************************************************************************

                  USE SWCOMM3
                  USE swan_time, ONLY: default_time_context

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
!     40.61: Roop Lalbeharry
!
!  1. Updates
!
!     40.61, Nov. 06: New subroutine
!
!  2. Purpose
!
!     Limits the change in action density between two iterations to
!     the (directionally independent) Hersbach and Janssen (1999) limiter
!
!  3. Method
!
!     The maximum change of energy density per bin is related to
!     the (directionally independent) Hersbach and Janssen (1999) limiter
!     This change is estimated in terms of frequency and energy density
!
!     |D E(f,t)| = 3.0 * 1.0E-7 * g * u* * f_c * dt/ (f^4)
!
!     in which f_c is the model's cut off frequency and u* the
!     friction velocity and dt integration time step (sec)
!
!     In terms of action density and angular frequency and for deep water,
!     we have:
!
!     |D N(s,t)| = C_HJ * u* / (s^3 * k)
!
!     where C_HJ = (2.0 * PI)^2 * 3.0 * 1.0E-7 * s_c * dt and
!     u* = max(u*, g*s*_pm/s); s*_pm = 2.*PI*f*_pm; f*_pm = 5.6 * 1.0E-3
!
!     Compare with Ris's(1997, p.36) formulation for deep water:
!
!                          alpha_PM
!     |D N(s,t)| = factor ----------- ; factor = 0.1, alpha_PM = 0.0081
!                         2 s k^3 c_g
!
!     In cases where waves are breaking the dissipation of energy
!     is not limited. This is assumed to be the case when the
!     fraction of breaking waves Qb is more than 1.e-5.
!
!  4. Argument variables

                  LOGICAL ANYBIN(MDC,MSC)

!     ISLMIN: Lowest sigma-index occured in applying limiter
!     NFLIM : Number of frequency use of limiter
!     QB_LOC: Local value of Qb (fraction of breaking waves)

                  INTEGER ISLMIN(MCGRD), NFLIM(MCGRD)
                  REAL    QB_LOC
                  REAL    AC2(MDC,MSC,MCGRD)
                  REAL    AC2OLD(MDC,MSC)
                  REAL    CGO(MSC,MICMAX)
                  REAL    KWAVE(MSC,MICMAX)
                  REAL    SPCSIG(MSC)
                  REAL    USTAR(MCGRD)

!  6. Local variables
!
!     ID    : Counter for directional (theta) space
!     IS    : Counter for frequency (sigma) space

                  INTEGER ID,IS

!     DAC2MX: Maximum deviation of action density AC2 between iterations

                  REAL    DAC2MX, UFRIC_VEL, C_HJ, SPM_NOND

! 13. Source text

                  C_HJ = PI2**2*3.0*1.0E-7*default_time_context%DT*SPCSIG(MSC)
                  SPM_NOND = PI2 * 5.6 * 1.0E-3
                  IF (MSC.GT.3) THEN
                     DO IS=1,MSC
                        UFRIC_VEL = MAX(USTAR(KCGRD(1)), GRAV*SPM_NOND/SPCSIG(IS))
                        DAC2MX=ABS((C_HJ*UFRIC_VEL)/&
                        &(SPCSIG(IS)**3*KWAVE(IS,1)))
                        DO ID=1,MDC
                           IF (ANYBIN(ID,IS) .AND.&
                           &AC2(ID,IS,KCGRD(1)).GT.AC2OLD(ID,IS)+DAC2MX) THEN
                              AC2(ID,IS,KCGRD(1))=AC2OLD(ID,IS)+DAC2MX
                              NFLIM(KCGRD(1)) = NFLIM(KCGRD(1)) + 1
                              ISLMIN(KCGRD(1)) = MIN(IS,ISLMIN(KCGRD(1)))
                           END IF
                        END DO
                        IF (QB_LOC.LT.PNUMS(28)) THEN
                           DO ID=1,MDC
                              IF (ANYBIN(ID,IS) .AND.&
                              &AC2(ID,IS,KCGRD(1)).LT.AC2OLD(ID,IS)-DAC2MX) THEN
                                 AC2(ID,IS,KCGRD(1))=AC2OLD(ID,IS)-DAC2MX
                                 NFLIM(KCGRD(1)) = NFLIM(KCGRD(1)) + 1
                                 ISLMIN(KCGRD(1)) = MIN(IS,ISLMIN(KCGRD(1)))
                              END IF
                           END DO
                        END IF
                     END DO
                  END IF
                  RETURN
               end subroutine HJLIM
!****************************************************************

               SUBROUTINE RESCALE (AC2, ISSTOP, IDCMIN, IDCMAX, NRSCAL)
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4
                  USE M_PARALL

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
!  0. Authors
!
!     40.00: Nico Booij
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.00, Feb. 99: New subroutine (software moved from subroutines
!                     SOLBAND, SOLMT1 and SOLMAT
!     40.23, Aug. 02: Store number of frequency use of rescaling
!     40.30, Mar. 03: correcting indices of test point with offsets MXF, MYF
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Remove negative values from a computed action density spectrum
!
!  3. Method
!
!     Make negative action densities 0 at the expense of other action densities
!     for the frequency
!
!  4. Argument variables
!
!     AC2         action densities

                  REAL        AC2(MDC,MSC,MCGRD)

!     ISSTOP      maximum frequency counter in this sweep

                  INTEGER     ISSTOP

!     IDCMIN      Integer array containing minimum counter of directions
!     IDCMAX      Integer array containing maximum counter
!     NRSCAL      Number of frequency use of rescaling

                  INTEGER     IDCMIN(MSC), IDCMAX(MSC)
                  INTEGER     NRSCAL(MCGRD)

!  7. Common blocks used
!
!
!     5. SUBROUTINES CALLING
!
!        SWOMPU
!
!     6. SUBROUTINES USED
!
!        NONE
!
!     7. ERROR MESSAGES
!
!        ---
!
!     8. REMARKS
!
!        ---
!
!     9. STRUCTURE
!
!   -------------------------------------------------------------
!   For all frequencies do
!       Make ATOT equal to integral of action density over direction
!       Make ATOTP equal to integral of positive action density
!       Determine FACTOR
!       If negative values do occur
!       Then for all directions do
!            If action density is negative
!            Then make action density =0
!            Else multiply action density by FACTOR
!   ------------------------------------------------------------
!
!     10. SOURCE
!
!************************************************************************
!
!         local variables
!
!         IS         counter of frequency
!         ID         counter of direction
!         IDDUM      uncorrected counter of direction

                  INTEGER, SAVE :: IENT = 0
                  INTEGER  IS      ,ID      ,IDDUM

!         ATOT       integral of action density for one frequency
!         ATOTP      integral of positive action density for one frequency
!         FACTOR

                  REAL     ATOT    ,ATOTP   ,FACTOR

!         NEGVAL      if True, there are negative values in the spectrum

                  LOGICAL  NEGVAL

                  IF (LTRACE) CALL STRACE (IENT,'RESCALE')

!     *** if negative action density occur rescale with a factor ***
!     *** only the sector computed is rescaled !!                ***

                  do IS = 1 , ISSTOP
                     ATOT   = 0.
                     ATOTP  = 0.
                     FACTOR = 0.
                     NEGVAL = .FALSE.
                     do IDDUM = IDCMIN(IS), IDCMAX(IS)
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        ATOT = ATOT + AC2(ID,IS,KCGRD(1))
                        IF ( AC2(ID,IS,KCGRD(1)) .LT. 0. ) THEN
                           NRSCAL(KCGRD(1)) = NRSCAL(KCGRD(1)) + 1
                           NEGVAL = .TRUE.
                        ELSE
                           ATOTP = ATOTP + AC2(ID,IS,KCGRD(1))
                        END IF
                     end do
                     IF (NEGVAL) THEN
                        IF ( ATOTP .LT. 1.E-15 ) ATOTP = 1.E-15
                        FACTOR = ATOT / ATOTP

!         *** rescale ***

                        do IDDUM = IDCMIN(IS), IDCMAX(IS)
                           ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           IF ( AC2(ID,IS,KCGRD(1)) .LT. 0.) THEN
                              AC2(ID,IS,KCGRD(1)) = 0.
                           END IF
                           IF ( FACTOR .GE. 0. ) THEN
                              AC2(ID,IS,KCGRD(1)) = FACTOR * AC2(ID,IS,KCGRD(1))
                           ENDIF
                        end do

                        IF ( ITEST .GE. 120 .AND. TESTFL )&
                        &WRITE (PRINTF, "(' Rescale in Point, Isig, Factor, ATOT, ATOTP:', 3I4, 3(1X,E11.4))") IXCGRD(1)+MXF-2, IYCGRD(1)+MYF-2, IS,&
                        &FACTOR , ATOT, ATOTP
                     ENDIF
                  end do
                  RETURN
               end subroutine RESCALE
!****************************************************************

               SUBROUTINE SWSIP ( AC2   , IMATDA, IMATRA, IMATLA, IMATUA,&
               &IMAT5L, IMAT6U, AC2OLD, REPS  , MAXIT ,&
               &IAMOUT, INOCNV, IDDLOW, IDDTOP, ISSTOP,&
               &IDCMIN, IDCMAX )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE SWCOMM1
                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4
                  USE M_PARALL

                  IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.23: Marcel Zijlema
!     40.30: Marcel Zijlema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.23, Oct. 02: New subroutine
!     40.30, Mar. 03: introduction distributed-memory approach using MPI
!     40.41, Mar. 04: parameter ALFA set to 0.0, extra test output
!                     and some corrections if SECTOR=0
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Solves penta-diagonal system of equations in
!     spectral space by means of Stone's SIP solver
!
!  4. Argument variables
!
!     AC2         action density
!     AC2OLD      action density at previous iteration
!     IAMOUT      control parameter indicating the amount of
!                 output required
!                 0: no output
!                 1: only fatal errors will be printed
!                 2: gives output concerning the iteration process
!                 3: additional information about the iteration
!                    is printed
!     IDCMAX      maximum counter in directional space
!     IDCMIN      minimum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMAT5L      coefficients of lower diagonal in sigma-space
!     IMAT6U      coefficients of upper diagonal in sigma-space
!     IMATDA      coefficients of main diagonal
!     IMATLA      coefficients of lower diagonal in theta-space
!     IMATUA      coefficients of upper diagonal in theta-space
!     IMATRA      right-hand side
!     INOCNV      integer indicating number of grid points in which
!                 solver does not converged
!     ISSTOP      maximum frequency counter in a sweep
!     MAXIT       the maximum number of iterations to be performed in
!                 the linear solver
!     REPS        accuracy with respect to the right-hand side used
!                 in the following termination criterion:
!
!                 ||b-Ax || < reps*||b||
!                       k

                  INTEGER IAMOUT, INOCNV, IDDLOW, IDDTOP, ISSTOP, MAXIT
                  INTEGER IDCMIN(MSC), IDCMAX(MSC)
                  REAL    REPS
                  REAL    AC2(MDC,MSC,MCGRD),&
                  &IMATDA(MDC,MSC), IMATRA(MDC,MSC),&
                  &IMAT5L(MDC,MSC), IMAT6U(MDC,MSC),&
                  &IMATLA(MDC,MSC), IMATUA(MDC,MSC),&
                  &AC2OLD(MDC,MSC)

!  5. Parameter variables
!
!     ALFA        relaxation parameter used in the SIP solver
!     SMALL :     a small number

                  REAL, PARAMETER :: ALFA=0.0, SMALL=1.E-15

!  6. Local variables
!
!     BNORM :     2-norm of right-hand side vector
!     CMAT5L:     coefficients of lower diagonal in sigma-space
!                 obtained by an incomplete lower-upper factorization
!     CMAT6U:     coefficients of upper diagonal in sigma-space
!                 obtained by an incomplete lower-upper factorization
!     CMATDA:     coefficients of main diagonal obtained by an
!                 incomplete lower-upper factorization
!     CMATLA:     coefficients of lower diagonal in theta-space
!                 obtained by an incomplete lower-upper factorization
!     CMATUA:     coefficients of upper diagonal in theta-space
!                 obtained by an incomplete lower-upper factorization
!     EPSLIN:     required accuracy in the linear solver
!     ICONV :     indicator for convergence (1=yes, 0=no)
!     ID    :     loop counter
!     IDDL  :     minimum counter in theta-space of modulo MDC
!     IDDT  :     maximum counter in theta-space of modulo MDC
!     IDDUM :     loop counter
!     IDM   :     index of point ID-1
!     IDMAX :     local array of maximum counter in theta-space
!     IDMIN :     local array of minimum counter in theta-space
!     IDP   :     index of point ID+1
!     IENT  :     number of entries
!     IS    :     loop counter
!     ISM   :     index of point IS-1
!     ISP   :     index of point IS+1
!     IT    :     iteration count
!     LOPERI:     auxiliary vector meant for computation in
!                 periodic theta-space
!     P1    :     auxiliary factor
!     P2    :     auxiliary factor
!     P3    :     auxiliary factor
!     RES   :     the residual vector
!     RNORM :     2-norm of residual vector
!     RNRM0 :     2-norm of initial residual vector
!     UEPS  :     minimal accuracy based on machine precision
!     UPPERI:     auxiliary vector meant for computation in
!                 periodic theta-space

                  INTEGER, SAVE :: IENT = 0
                  INTEGER ICONV, ID, IDDL, IDDT, IDDUM, IDM, IDP, &
                  &IS, ISM, ISP, IT
                  INTEGER IDMIN(MSC), IDMAX(MSC)
                  REAL    BNORM, EPSLIN, P1, P2, P3, RNORM, RNRM0, UEPS
                  REAL    RES(MDC,MSC)   , CMATDA(MDC,MSC),&
                  &CMAT5L(MDC,MSC), CMAT6U(MDC,MSC),&
                  &CMATLA(MDC,MSC), CMATUA(MDC,MSC),&
                  &LOPERI(MSC)    , UPPERI(MSC)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWOMPU  (in SWANCOM1)
!
! 12. Structure
!
!     The system of equations is solved using an incomplete
!     factorization technique called Strongly Implicit Procedure
!     (SIP) as described in
!
!     H.L. Stone
!     Iterative solution of implicit approximations of
!     multidimensional partial differential equations
!     SIAM J. of Numer. Anal., vol. 5, 530-558, 1968
!
!     This method constructs an incomplete lower-upper factorization
!     that has the same sparsity as the original matrix. Hereby, a
!     parameter alfa is used, which should be 0.0 in case of SWAN
!     (when alfa > 0.95, the method may diverge).
!
!     Afterward, the resulting system is solved in an iterative manner
!     by forward and backward substitutions.
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SWSIP')

!     --- initialize arrays

                  RES    = 0.
                  CMATDA = 0.
                  CMATLA = 0.
                  CMATUA = 0.
                  CMAT5L = 0.
                  CMAT6U = 0.
                  LOPERI = 0.
                  UPPERI = 0.

!     --- in case of periodicity in theta-space, store values
!         of matrix coefficients corresponding to left bottom and
!         right top

                  DO IS = 1, ISSTOP
                     IF ( IDCMIN(IS).EQ.1 .AND. IDCMAX(IS).EQ.MDC ) THEN
                        UPPERI(IS) = IMATLA(  1,IS)
                        LOPERI(IS) = IMATUA(MDC,IS)
                     END IF
                  END DO

!     --- when no bins fall within the sweep, i.e. SECTOR = 0,
!         reset the bounds of sector as 1..MDC (routine SOLPRE
!         has clear the rows in the matrix that do not belong
!         to the sweep)

                  DO IS = 1, ISSTOP
                     IF ( IDCMIN(IS).LE.IDCMAX(IS) ) THEN
                        IDMIN(IS) = IDCMIN(IS)
                        IDMAX(IS) = IDCMAX(IS)
                     ELSE
                        IDMIN(IS) = 1
                        IDMAX(IS) = MDC
                     END IF
                  END DO

                  IT    = 0
                  ICONV = 0

!     --- construct L and U matrices (stored in CMAT[xx])

                  BNORM = 0.

                  IS    = 1
                  IDDUM = IDMIN(IS)
                  ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1

                  CMAT5L(ID,IS) = IMAT5L(ID,IS)
                  CMATLA(ID,IS) = IMATLA(ID,IS)
                  CMATDA(ID,IS) = 1./(IMATDA(ID,IS)+SMALL)
                  CMAT6U(ID,IS) = IMAT6U(ID,IS)*CMATDA(ID,IS)
                  CMATUA(ID,IS) = IMATUA(ID,IS)*CMATDA(ID,IS)
                  BNORM = BNORM + IMATRA(ID,IS)*IMATRA(ID,IS)

                  DO IDDUM = IDMIN(IS)+1, IDMAX(IS)
                     ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1

                     P2 = ALFA*CMAT6U(IDM,IS)
                     CMAT5L(ID,IS) = IMAT5L(ID,IS)
                     CMATLA(ID,IS) = IMATLA(ID,IS)/(1.+P2)

                     P2 = P2*CMATLA(ID,IS)
                     P3 = IMATDA(ID,IS) + P2&
                     &-CMATLA(ID,IS)*CMATUA(IDM,IS )&
                     &+SMALL
                     CMATDA(ID,IS) = 1./P3
                     CMAT6U(ID,IS) = (IMAT6U(ID,IS)-P2)*CMATDA(ID,IS)
                     CMATUA(ID,IS) =      IMATUA(ID,IS)*CMATDA(ID,IS)
                     BNORM = BNORM + IMATRA(ID,IS)*IMATRA(ID,IS)
                  END DO

                  DO IS = 2, ISSTOP
                     ISM = IS - 1

                     IDDUM = IDMIN(IS)
                     ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1

                     P1 = ALFA*CMATUA(ID,ISM)
                     CMAT5L(ID,IS) = IMAT5L(ID,IS)/(1.+P1)
                     CMATLA(ID,IS) = IMATLA(ID,IS)
                     P1 = P1*CMAT5L(ID,IS)
                     P3 = IMATDA(ID,IS) + P1&
                     &-CMAT5L(ID,IS)*CMAT6U(ID,ISM)&
                     &+SMALL
                     CMATDA(ID,IS) = 1./P3
                     CMAT6U(ID,IS) =      IMAT6U(ID,IS)*CMATDA(ID,IS)
                     CMATUA(ID,IS) = (IMATUA(ID,IS)-P1)*CMATDA(ID,IS)
                     BNORM = BNORM + IMATRA(ID,IS)*IMATRA(ID,IS)

                     DO IDDUM = IDMIN(IS)+1, IDMAX(IS)
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1

                        P1 = ALFA*CMATUA(ID ,ISM)
                        P2 = ALFA*CMAT6U(IDM,IS )
                        CMAT5L(ID,IS) = IMAT5L(ID,IS)/(1.+P1)
                        CMATLA(ID,IS) = IMATLA(ID,IS)/(1.+P2)
                        P1 = P1*CMAT5L(ID,IS)
                        P2 = P2*CMATLA(ID,IS)
                        P3 = IMATDA(ID,IS) + P1 + P2&
                        &-CMAT5L(ID,IS)*CMAT6U(ID ,ISM)&
                        &-CMATLA(ID,IS)*CMATUA(IDM,IS )&
                        &+SMALL
                        CMATDA(ID,IS) = 1./P3
                        CMAT6U(ID,IS) = (IMAT6U(ID,IS)-P2)*CMATDA(ID,IS)
                        CMATUA(ID,IS) = (IMATUA(ID,IS)-P1)*CMATDA(ID,IS)
                        BNORM = BNORM + IMATRA(ID,IS)*IMATRA(ID,IS)
                     END DO
                  END DO
                  BNORM = SQRT(BNORM)

                  EPSLIN = REPS*BNORM
                  UEPS   = 1000.*UNDFLW*BNORM
                  IF ( EPSLIN.LT.UEPS .AND. BNORM.GT.0. ) THEN
                     IF ( IAMOUT.GE.1 ) THEN
                        WRITE (PRINTF,'(A)')&
                        &' ++ SWSIP: the required accuracy is too small'
                        WRITE (PRINTF,*)&
                        &'           required accuracy    = ',EPSLIN
                        WRITE (PRINTF,*)&
                        &'           appropriate accuracy = ',UEPS
                     END IF
                     EPSLIN = UEPS
                  END IF

!     --- solve the system by forward and backward substitutions
!         in an iterative manner

                  sip_iterations: DO WHILE ( ICONV.EQ.0 .AND. IT.LT.MAXIT )

                     IT    = IT + 1
                     ICONV = 1
                     RNORM = 0.

                     IS  = 1
                     ISP = IS + 1

                     IDDUM = IDMIN(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                     IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                     RES(ID,IS) = IMATRA(ID,IS)&
                     &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                     &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                     &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                     &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                     RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                     RES(ID,IS) = RES(ID,IS)*CMATDA(ID,IS)

                     DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                        RES(ID,IS) = IMATRA(ID,IS)&
                        &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                        RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                        RES(ID,IS) = (RES(ID,IS) - CMATLA(ID,IS)*RES(IDM,IS))*&
                        &CMATDA(ID,IS)
                     END DO

                     IDDUM = IDMAX(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                     IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                     RES(ID,IS) = IMATRA(ID,IS)&
                     &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                     &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                     &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                     &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                     RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                     RES(ID,IS) = (RES(ID,IS) - CMATLA(ID,IS)*RES(IDM,IS))*&
                     &CMATDA(ID,IS)

                     DO IS = 2, ISSTOP-1
                        ISM = IS - 1
                        ISP = IS + 1

                        IDDUM = IDMIN(IS)
                        ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                        IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                        RES(ID,IS) = IMATRA(ID,IS)&
                        &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                        &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                        RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                        RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID,ISM))*&
                        &CMATDA(ID,IS)

                        DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                           ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                           IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                           RES(ID,IS) = IMATRA(ID,IS)&
                           &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                           &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                           &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                           &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                           &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                           RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                           RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID ,ISM)&
                           &- CMATLA(ID,IS)*RES(IDM,IS ))*&
                           &CMATDA(ID,IS)
                        END DO

                        IDDUM = IDMAX(IS)
                        ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                        RES(ID,IS) = IMATRA(ID,IS)&
                        &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                        RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                        RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID ,ISM)&
                        &- CMATLA(ID,IS)*RES(IDM,IS ))*&
                        &CMATDA(ID,IS)

                     END DO

                     IS  = ISSTOP
                     ISM = IS - 1

                     IDDUM = IDMIN(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                     IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                     RES(ID,IS) = IMATRA(ID,IS)&
                     &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                     &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                     &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                     &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                     RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                     RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID,ISM))*&
                     &CMATDA(ID,IS)

                     DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                        RES(ID,IS) = IMATRA(ID,IS)&
                        &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                        RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                        RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID ,ISM)&
                        &- CMATLA(ID,IS)*RES(IDM,IS ))*&
                        &CMATDA(ID,IS)
                     END DO

                     IDDUM = IDMAX(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                     IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                     RES(ID,IS) = IMATRA(ID,IS)&
                     &-IMATDA(ID,IS)*AC2(ID ,IS ,KCGRD(1))&
                     &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                     &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                     &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                     RNORM = RNORM + RES(ID,IS)*RES(ID,IS)
                     RES(ID,IS) = (RES(ID,IS) - CMAT5L(ID,IS)*RES(ID ,ISM)&
                     &- CMATLA(ID,IS)*RES(IDM,IS ))*&
                     &CMATDA(ID,IS)

                     IF ( RNORM.GT.1.E8 ) THEN
                        IT = MAXIT + 1
                        ICONV = 0
                        CYCLE sip_iterations
                     END IF
                     RNORM=SQRT(RNORM)
                     IF ( IAMOUT.EQ.3 .AND. IT.EQ.1 ) RNRM0 = RNORM

                     IF ( IAMOUT.EQ.2 ) THEN
                        WRITE (PRINTF,'(A,I3,A,E12.6)')&
                        &' ++ SWSIP: iter = ',IT,'    res = ',RNORM
                     END IF

                     IS    = ISSTOP
                     IDDUM = IDMAX(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1

                     AC2(ID,IS,KCGRD(1)) = AC2(ID,IS,KCGRD(1)) + RES(ID,IS)

                     DO IDDUM = IDMAX(IS)-1, IDMIN(IS), -1
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                        RES(ID,IS) = RES(ID,IS) - CMATUA(ID,IS)*RES(IDP,IS)
                        AC2(ID,IS,KCGRD(1)) = AC2(ID,IS,KCGRD(1)) + RES(ID,IS)
                     END DO

                     DO IS = ISSTOP-1, 1, -1
                        ISP = IS + 1

                        IDDUM = IDMAX(IS)
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1

                        RES(ID,IS) = RES(ID,IS) - CMAT6U(ID,IS)*RES(ID,ISP)
                        AC2(ID,IS,KCGRD(1)) = AC2(ID,IS,KCGRD(1)) + RES(ID,IS)

                        DO IDDUM = IDMAX(IS)-1, IDMIN(IS), -1
                           ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                           RES(ID,IS) = RES(ID,IS) - CMAT6U(ID,IS)*RES(ID ,ISP)&
                           &- CMATUA(ID,IS)*RES(IDP,IS )
                           AC2(ID,IS,KCGRD(1)) = AC2(ID,IS,KCGRD(1)) + RES(ID,IS)
                        END DO
                     END DO

                     IF ( RNORM.GT.UNDFLW**2 .AND. RNORM.GT.EPSLIN ) ICONV = 0
                  END DO sip_iterations

!     --- investigate the reason to stop

                  IF ( ICONV.EQ.0 ) THEN
                     AC2(:,:,KCGRD(1)) = AC2OLD(:,:)
                     INOCNV = INOCNV + 1
                  END IF
                  IF ( ICONV.EQ.0 .AND. IAMOUT.GE.1 ) THEN
                     IF (ERRPTS.GT.0.AND.IAMMASTER) THEN
                        WRITE(ERRPTS,"(I4,1X,I4,1X,I2)") IXCGRD(1)+MXF-1, IYCGRD(1)+MYF-1, 2
                     END IF
                     WRITE (PRINTF,'(A,I5,A,I5,A)')&
                     &' ++ SWSIP: no convergence in grid point (',&
                     &IXCGRD(1)+MXF-1,',',IYCGRD(1)+MYF-1,')'
                     WRITE (PRINTF,'(A,I3)')&
                     &'           total number of iterations     = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           2-norm of the residual         = ',RNORM
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           required accuracy              = ',EPSLIN
                  ELSE IF ( IAMOUT.EQ.3 ) THEN
                     WRITE (PRINTF,'(A,E12.6)')&
                     &' ++ SWSIP: 2-norm of the initial residual = ',RNRM0
                     WRITE (PRINTF,'(A,I3)')&
                     &'           total number of iterations     = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           2-norm of the residual         = ',RNORM
                  END IF

!     --- test output

                  IF ( TESTFL .AND. ITEST.GE.120 ) THEN
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*) '  Subroutine SWSIP'
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,"(' SWSIP : POINT MDC MSC :',3I5)") KCGRD(1), MDC, MSC
                     WRITE(PRTEST,"(' SWSIP : IDDLOW IDDTOP ISSTOP :',3I4)") IDDLOW, IDDTOP, ISSTOP
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*) ' coefficients of matrix and rhs  '
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,'(A111)')&
                     &' IS ID         IMATLA         IMATDA'//&
                     &'         IMATUA         IMATRA         IMAT5L'//&
                     &'         IMAT6U            AC2'
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        DO IS = 1, ISSTOP
                           WRITE(PRTEST,"(2I3,7E15.7)") IS, ID,&
                           &IMATLA(ID,IS), IMATDA(ID,IS),&
                           &IMATUA(ID,IS), IMATRA(ID,IS),&
                           &IMAT5L(ID,IS), IMAT6U(ID,IS),&
                           &AC2(ID,IS,KCGRD(1))
                        END DO
                     END DO
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*)'IS ID      LPER          UPER '
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IF ( ID.EQ.1 .OR. ID.EQ.MDC ) THEN
                           DO IS = 1, ISSTOP
                              WRITE(PRTEST,"(2I3,2E15.7)") IS, ID, LOPERI(IS), UPPERI(IS)
                           END DO
                        END IF
                     END DO
                  END IF

!     --- set matrix coefficients to zero

                  IMATDA = 0.
                  IMATRA = 0.

                  RETURN
               end subroutine SWSIP
!****************************************************************

               SUBROUTINE SWSOR ( AC2   , IMATDA, IMATRA, IMATLA, IMATUA,&
               &IMAT5L, IMAT6U, AC2OLD, REPS  , MAXIT ,&
               &IAMOUT, INOCNV, IDDLOW, IDDTOP, ISSTOP,&
               &IDCMIN, IDCMAX )
   USE swan_service_interfaces, ONLY: MSGERR, STRACE

!****************************************************************

                  USE SWCOMM1
                  USE SWCOMM3
                  USE SWCOMM4
                  USE OCPCOMM4
                  USE M_PARALL

                  IMPLICIT NONE(TYPE, EXTERNAL)


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
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Nov. 04: New subroutine
!
!  2. Purpose
!
!     Solves penta-diagonal system of equations in
!     spectral space with point SOR method
!
!  4. Argument variables
!
!     AC2         action density
!     AC2OLD      action density at previous iteration
!     IAMOUT      control parameter indicating the amount of
!                 output required
!                 0: no output
!                 1: only fatal errors will be printed
!                 2: gives output concerning the iteration process
!                 3: additional information about the iteration
!                    is printed
!     IDCMAX      maximum counter in directional space
!     IDCMIN      minimum counter in directional space
!     IDDLOW      minimum direction that is propagated within a sweep
!     IDDTOP      maximum direction that is propagated within a sweep
!     IMAT5L      coefficients of lower diagonal in sigma-space
!     IMAT6U      coefficients of upper diagonal in sigma-space
!     IMATDA      coefficients of main diagonal
!     IMATLA      coefficients of lower diagonal in theta-space
!     IMATUA      coefficients of upper diagonal in theta-space
!     IMATRA      right-hand side
!     INOCNV      integer indicating number of grid points in which
!                 solver does not converged
!     ISSTOP      maximum frequency counter in a sweep
!     MAXIT       the maximum number of iterations to be performed in
!                 the linear solver
!     REPS        relative accuracy of the final approximation

                  INTEGER IAMOUT, INOCNV, IDDLOW, IDDTOP, ISSTOP, MAXIT
                  INTEGER IDCMIN(MSC), IDCMAX(MSC)
                  REAL    REPS
                  REAL    AC2(MDC,MSC,MCGRD),&
                  &IMATDA(MDC,MSC), IMATRA(MDC,MSC),&
                  &IMAT5L(MDC,MSC), IMAT6U(MDC,MSC),&
                  &IMATLA(MDC,MSC), IMATUA(MDC,MSC),&
                  &AC2OLD(MDC,MSC)

!  5. Parameter variables
!
!     OMEG  :     relaxation parameter

                  REAL, PARAMETER :: OMEG=0.8

!  6. Local variables
!
!     AC2I  :     intermediate action density
!     ICONV :     indicator for convergence (1=yes, 0=no)
!     ID    :     loop counter in theta-space
!     IDDL  :     minimum counter in theta-space of modulo MDC
!     IDDT  :     maximum counter in theta-space of modulo MDC
!     IDDUM :     loop counter
!     IDINF :     index of point ID with largest error in solution
!     IDM   :     index of point ID-1
!     IDMAX :     local array of maximum counter in theta-space
!     IDMIN :     local array of minimum counter in theta-space
!     IDP   :     index of point ID+1
!     IENT  :     number of entries
!     INVMDA:     inverse of main diagonal
!     IS    :     loop counter in sigma-space
!     ISINF :     index of point IS with largest error in solution
!     ISM   :     index of point IS-1
!     ISP   :     index of point IS+1
!     IT    :     iteration count
!     LOPERI:     auxiliary vector meant for computation in
!                 periodic theta-space
!     RES   :     residual
!     RESM  :     inf-norm of residual vector
!     RESM0 :     inf-norm of initial residual vector
!     UPPERI:     auxiliary vector meant for computation in
!                 periodic theta-space

                  INTEGER, SAVE :: IENT = 0
                  INTEGER ICONV, ID, IDINF, IDDL, IDDT, IDDUM, IDM, IDP, &
                  &IS, ISINF, ISM, ISP, IT
                  INTEGER IDMIN(MSC), IDMAX(MSC)
                  REAL    AC2I, RES, RESM, RESM0
                  REAL    LOPERI(MSC), UPPERI(MSC), INVMDA(MDC,MSC)

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     MSGERR           Writes error message
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SWOMPU  (in SWANCOM1)
!
! 12. Structure
!
!     The system of equations is solved using the SOR technique in
!     pointwise manner
!     Note that with omeg=1, the Gauss-Seidel method is recovered
!
!     Convergence is reached, if the difference between two consecutive
!     iteration levels measured w.r.t. the maximum norm is smaller than
!     given tolerance
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SWSOR')

!     --- initialize arrays

                  LOPERI = 0.
                  UPPERI = 0.
                  INVMDA = 0.

!     --- in case of periodicity in theta-space, store values
!         of matrix coefficients corresponding to left bottom and
!         right top

                  DO IS = 1, ISSTOP
                     IF ( IDCMIN(IS).EQ.1 .AND. IDCMAX(IS).EQ.MDC ) THEN
                        UPPERI(IS) = IMATLA(  1,IS)
                        LOPERI(IS) = IMATUA(MDC,IS)
                     END IF
                  END DO

!     --- when no bins fall within the sweep, i.e. SECTOR = 0,
!         reset the bounds of sector as 1..MDC (routine SOLPRE
!         has clear the rows in the matrix that do not belong
!         to the sweep)

                  DO IS = 1, ISSTOP
                     IF ( IDCMIN(IS).LE.IDCMAX(IS) ) THEN
                        IDMIN(IS) = IDCMIN(IS)
                        IDMAX(IS) = IDCMAX(IS)
                     ELSE
                        IDMIN(IS) = 1
                        IDMAX(IS) = MDC
                     END IF
                  END DO

!     --- store inverse of main diagonal

                  DO IS = 1, ISSTOP
                     DO IDDUM = IDMIN(IS), IDMAX(IS)
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IF ( IMATDA(ID,IS).NE.0. ) THEN
                           INVMDA(ID,IS) = 1./IMATDA(ID,IS)
                        ELSE
                           CALL MSGERR ( 3,&
                           &'Main diagonal of spectral matrix is zero!' )
                        END IF
                     END DO
                  END DO

                  IT    = 0
                  ICONV = 0

!     --- start iteration process

                  sor_iterations: DO WHILE ( ICONV.EQ.0 .AND. IT.LT.MAXIT )

                     IT    = IT + 1
                     ICONV = 1
                     RESM  = 0.
                     IDINF = 0
                     ISINF = 0

                     IS  = 1
                     ISP = IS + 1

                     IDDUM = IDMIN(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                     IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                     AC2I = IMATRA(ID,IS)&
                     &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                     &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                     &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                     AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                     RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IDINF = ID
                        ISINF = IS
                     END IF
                     AC2(ID,IS,KCGRD(1)) = AC2I

                     DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                        AC2I = IMATRA(ID,IS)&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                        AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                        RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IDINF = ID
                           ISINF = IS
                        END IF
                        AC2(ID,IS,KCGRD(1)) = AC2I

                     END DO

                     IDDUM = IDMAX(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                     IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                     AC2I = IMATRA(ID,IS)&
                     &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                     &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                     &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                     AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                     RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IDINF = ID
                        ISINF = IS
                     END IF
                     AC2(ID,IS,KCGRD(1)) = AC2I

                     DO IS = 2, ISSTOP-1
                        ISM = IS - 1
                        ISP = IS + 1

                        IDDUM = IDMIN(IS)
                        ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                        IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                        AC2I = IMATRA(ID,IS)&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                        &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                        AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                        RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IDINF = ID
                           ISINF = IS
                        END IF
                        AC2(ID,IS,KCGRD(1)) = AC2I

                        DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                           ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                           IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                           IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                           AC2I = IMATRA(ID,IS)&
                           &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                           &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                           &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                           &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                           AC2I = AC2I*OMEG*INVMDA(ID,IS)+&
                           &(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                           RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                           IF ( RES.GT.RESM ) THEN
                              RESM  = RES
                              IDINF = ID
                              ISINF = IS
                           END IF
                           AC2(ID,IS,KCGRD(1)) = AC2I

                        END DO

                        IDDUM = IDMAX(IS)
                        ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                        AC2I = IMATRA(ID,IS)&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMAT6U(ID,IS)*AC2(ID ,ISP,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                        AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                        RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IDINF = ID
                           ISINF = IS
                        END IF
                        AC2(ID,IS,KCGRD(1)) = AC2I

                     END DO

                     IS  = ISSTOP
                     ISM = IS - 1

                     IDDUM = IDMIN(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDP   = MOD ( IDDUM     + MDC , MDC ) + 1
                     IDDT  = MOD ( IDMAX(IS) - 1 + MDC , MDC ) + 1

                     AC2I = IMATRA(ID,IS)&
                     &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                     &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))&
                     &-UPPERI(IS)*AC2(IDDT,IS,KCGRD(1))
                     AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                     RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IDINF = ID
                        ISINF = IS
                     END IF
                     AC2(ID,IS,KCGRD(1)) = AC2I

                     DO IDDUM = IDMIN(IS)+1, IDMAX(IS)-1
                        ID  = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IDM = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                        IDP = MOD ( IDDUM     + MDC , MDC ) + 1

                        AC2I = IMATRA(ID,IS)&
                        &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                        &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                        &-IMATUA(ID,IS)*AC2(IDP,IS ,KCGRD(1))
                        AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                        RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IDINF = ID
                           ISINF = IS
                        END IF
                        AC2(ID,IS,KCGRD(1)) = AC2I

                     END DO

                     IDDUM = IDMAX(IS)
                     ID    = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                     IDM   = MOD ( IDDUM - 2 + MDC , MDC ) + 1
                     IDDL  = MOD ( IDMIN(IS) - 1 + MDC , MDC ) + 1

                     AC2I = IMATRA(ID,IS)&
                     &-IMAT5L(ID,IS)*AC2(ID ,ISM,KCGRD(1))&
                     &-IMATLA(ID,IS)*AC2(IDM,IS ,KCGRD(1))&
                     &-LOPERI(IS)*AC2(IDDL,IS,KCGRD(1))
                     AC2I = AC2I*OMEG*INVMDA(ID,IS)+(1.-OMEG)*AC2(ID,IS,KCGRD(1))

                     RES = ABS(AC2(ID,IS,KCGRD(1)) - AC2I)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IDINF = ID
                        ISINF = IS
                     END IF
                     AC2(ID,IS,KCGRD(1)) = AC2I

                     IF ( RESM.GT.1.E8 ) THEN
                        IT = MAXIT + 1
                        ICONV = 0
                        CYCLE sor_iterations
                     END IF
                     IF ( IAMOUT.EQ.2 ) THEN
                        WRITE (PRINTF,'(A,I3,A,E12.6,A,I3,A,I3,A)')&
                        &' ++ SWSOR: iter = ',IT,'    res = ',RESM,&
                        &' in (ID,IS) = (',IDINF,',',ISINF,')'
                     END IF
                     IF ( IAMOUT.EQ.3 .AND. IT.EQ.1 ) RESM0 = RESM

                     IF ( RESM.GT.REPS ) ICONV = 0
                  END DO sor_iterations

!     --- investigate the reason to stop

                  IF ( ICONV.EQ.0 ) THEN
                     AC2(:,:,KCGRD(1)) = AC2OLD(:,:)
                     INOCNV = INOCNV + 1
                  END IF
                  IF ( ICONV.EQ.0 .AND. IAMOUT.GE.1 ) THEN
                     IF (ERRPTS.GT.0.AND.IAMMASTER) THEN
                        WRITE(ERRPTS,"(I4,1X,I4,1X,I2)") IXCGRD(1)+MXF-1, IYCGRD(1)+MYF-1, 2
                     END IF
                     WRITE (PRINTF,'(A,I5,A,I5,A)')&
                     &' ++ SWSOR: no convergence in grid point (',&
                     &IXCGRD(1)+MXF-1,',',IYCGRD(1)+MYF-1,')'
                     WRITE (PRINTF,'(A,I3)')&
                     &'           total number of iterations       = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           inf-norm of the residual         = ',RESM
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           required accuracy                = ',REPS
                  ELSE IF ( IAMOUT.EQ.3 ) THEN
                     WRITE (PRINTF,'(A,E12.6)')&
                     &' ++ SWSOR: inf-norm of the initial residual = ',RESM0
                     WRITE (PRINTF,'(A,I3)')&
                     &'           total number of iterations       = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'           inf-norm of the residual         = ',RESM
                  END IF

!     --- test output

                  IF ( TESTFL .AND. ITEST.GE.120 ) THEN
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*) '  Subroutine SWSOR'
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,"(' SWSOR : POINT MDC MSC :',3I5)") KCGRD(1), MDC, MSC
                     WRITE(PRTEST,"(' SWSOR : IDDLOW IDDTOP ISSTOP :',3I4)") IDDLOW, IDDTOP, ISSTOP
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*) ' coefficients of matrix and rhs  '
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,'(A111)')&
                     &' IS ID         IMATLA         IMATDA'//&
                     &'         IMATUA         IMATRA         IMAT5L'//&
                     &'         IMAT6U            AC2'
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        DO IS = 1, ISSTOP
                           WRITE(PRTEST,"(2I3,7E15.7)") IS, ID,&
                           &IMATLA(ID,IS), IMATDA(ID,IS),&
                           &IMATUA(ID,IS), IMATRA(ID,IS),&
                           &IMAT5L(ID,IS), IMAT6U(ID,IS),&
                           &AC2(ID,IS,KCGRD(1))
                        END DO
                     END DO
                     WRITE(PRTEST,*)
                     WRITE(PRTEST,*)'IS ID      LPER          UPER '
                     DO IDDUM = IDDLOW, IDDTOP
                        ID = MOD ( IDDUM - 1 + MDC , MDC ) + 1
                        IF ( ID.EQ.1 .OR. ID.EQ.MDC ) THEN
                           DO IS = 1, ISSTOP
                              WRITE(PRTEST,"(2I3,2E15.7)") IS, ID, LOPERI(IS), UPPERI(IS)
                           END DO
                        END IF
                     END DO
                  END IF

!     --- set matrix coefficients to zero

                  IMATDA = 0.
                  IMATRA = 0.
                  IMATLA = 0.
                  IMATUA = 0.
                  IMAT5L = 0.
                  IMAT6U = 0.

                  RETURN
               end subroutine SWSOR
!****************************************************************

               SUBROUTINE SWMTLB ( N1, N2, M1, M2 )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

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
!     40.31: Tim Campbell and John Cazes
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.31, Jul. 03: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!
!  2. Purpose
!
!     Given global loop bounds N1 and N2, compute loop bounds
!     M1 and M2 for calling thread
!
!  4. Argument variables
!
!     M1          lower index of thread loop
!     M2          upper index of thread loop
!     N1          lower index of global loop
!     N2          upper index of global loop

                  INTEGER N1, N2, M1, M2

!  6. Local variables
!
!     ID    :     thread number
!     IENT  :     number of entries
!     NCH   :     auxiliary integer
!     NTH   :     number of threads

                  INTEGER, SAVE :: IENT = 0
                  INTEGER ID, NTH, NCH
!$                INTEGER OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!$                EXTERNAL OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
!  8. Subroutines used
!
!     ---
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
!     Description of the pseudo code
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SWMTLB')

                  NTH = 1
                  ID  = 0
!$                NTH = OMP_GET_NUM_THREADS()
!$                ID  = OMP_GET_THREAD_NUM()
                  NCH = (N2-N1+1)/NTH
                  M1  = ID*NCH+N1
                  M2  = (ID+1)*NCH+N1-1
                  IF(ID.EQ.NTH-1) M2 = N2

                  RETURN
               end subroutine SWMTLB
!****************************************************************

               SUBROUTINE SWSTPC ( HSACC0, HSACC1, HSACC2, SACC0 , SACC1,&
               &SACC2 , HSDIFC, TMDIFC, DELHS , DELTM,&
               &DEP2  , ACCUR , I1MYC , I2MYC )
   USE swan_service_interfaces, ONLY: STRACE, EQREAL, STPNOW

!****************************************************************

                  USE OCPCOMM4
                  USE SWCOMM3
                  USE SWCOMM4
                  USE M_GENARR
                  USE M_PARALL
                  USE M_CONVERGENCE_SHARED, ONLY: &
                     WETGRD => SWSTPC_WETGRD, IACCUR => SWSTPC_IACCUR

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
!     40.41: Andre van der Westhuysen
!     40.41: Marcel Zijlema
!     40.93: Andre van der Westhuysen
!     41.90: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Jun. 04: New subroutine
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.93, Sep. 08: extended with curvature of Tm
!     41.90, Dec. 21: adapted to QCM
!
!  2. Purpose
!
!     Check convergence based on the relative, absolute
!     and curvature values of wave height and period
!
!  3. Method
!
!     In case of QC modelling, stopping criterion based on absolute and
!     relative changes in wave height only
!     Also note that the curvature criterion is risky because of inherent
!     rapid changes in Hs due to scattering and/or surf breaking
!
!  4. Argument variables
!
!     ACCUR       indicates percentage of grid points in
!                 which accuracy is reached
!     DELHS       difference in Hs between last 2 iterations
!     DELTM       difference in Tm between last 2 iterations
!     DEP2        depth
!     HSACC0      significant wave height at iter-2
!     HSACC1      significant wave height at iter-1
!     HSACC2      significant wave height at iter
!     HSDIFC      difference of Hs(i) - Hs(i-2) meant for
!                 computation of curvature of Hs
!     I1MYC       lower index for thread loop over y-grid row
!     I2MYC       upper index for thread loop over y-grid row
!     SACC0       mean wave frequency at iter-2
!     SACC1       mean wave frequency at iter-1
!     SACC2       mean wave frequency at iter
!     TMDIFC      difference of Tm(i) - Tm(i-2) meant for
!                 computation of curvature of Tm

                  INTEGER I1MYC, I2MYC
                  REAL    ACCUR
                  REAL    DEP2(MCGRD)          ,&
                  &HSACC0(MCGRD)        ,&
                  &HSACC1(MCGRD)        ,&
                  &HSACC2(MCGRD)        ,&
                  &SACC0(MCGRD)         ,&
                  &SACC1(MCGRD)         ,&
                  &SACC2(MCGRD)         ,&
                  &DELHS(MCGRD)         ,&
                  &DELTM(MCGRD)         ,&
                  &HSDIFC(MCGRD)        ,&
                  &TMDIFC(MCGRD)

!  6. Local variables
!
!     ACS2  :     auxiliary variable
!     ACS3  :     auxiliary variable
!     HSABS :     absolute value of Hs
!     HSCURV:     curvature value of Hs
!     HSDIFO:     previous value of HSDIFC
!     HSREL :     relative value of Hs
!     IACCUR:     indicates number of grid points in which
!                 accuracy is reached
!     IARR  :     auxiliary array meant for global reduction
!     ID    :     counter of direction
!     IENT  :     number of entries
!     II    :     loop variable
!     INDX  :     index for indirect address
!     IS    :     counter of frequency
!     IX    :     loop counter
!     IX1   :     lower index in x-direction
!     IX2   :     upper index in x-direction
!     IY    :     loop counter
!     IY1   :     lower index in y-direction
!     IY2   :     upper index in y-direction
!     LCONV :     logical indicating convergence
!     LHEAD :     logical indicating to write header
!     TMABS :     absolute value of Tm
!     TMCURV:     curvature value of Tm
!     TMDIFO:     previous value of TMDIFC
!     TMREL :     relative value of Tm
!     TSTFL :     indicates whether grid point is a test point
!     WETGRD:     number of wet grid points
!     XMOM0 :     zeroth moment
!     XMOM1 :     first moment

                  INTEGER, SAVE :: IENT = 0
                  INTEGER ID, IS, II, INDX, IX, IY, IX1, IX2, IY1, IY2
                  INTEGER IACCURt, WETGRDt, IARR(2)
                  REAL    ACS2, ACS3, HSREL ,HSABS, HSCURV, HSDIFO, TMABS,&
                  &TMREL, TMCURV, TMDIFO, XMOM0, XMOM1
                  LOGICAL LCONV, LHEAD, TSTFL

!  7. Common blocks used
!
!     Module variables WETGRD and IACCUR are shared by OpenMP threads.
!
!  8. Subroutines used
!
!     EQREAL           Boolean function which compares two REAL values
!     STRACE           Tracing routine for debugging
!     STPNOW           Logical indicating whether program must
!                      terminated or not
!     SWREDUCE         Performs a global reduction

!
!  9. Subroutines calling
!
!     SWCOMP (in SWANCOM1)
!
! 12. Structure
!
!     master thread initialize the shared variables
!     store Hs and Tm as old values and count number of wet grid points
!     compute new values of Hs and Tm
!     calculate a set of accuracy parameters based on relative,
!         absolute and curvature values of Hs, Tm and check accuracy
!     global sum of IACCUR and WETGRD
!     carry out reductions across all nodes
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SWSTPC')

!     --- master thread initialize the shared variables
!$OMP MASTER
                  WETGRD = 0
                  IACCUR = 0
!$OMP END MASTER
!$OMP BARRIER

                  IF ( LMXF ) THEN
                     IX1 = 1
                  ELSE
                     IX1 = 1+IHALOX
                  END IF
                  IF ( LMXL ) THEN
                     IX2 = MXC
                  ELSE
                     IX2 = MXC-IHALOX
                  END IF
                  IF ( LMYF ) THEN
                     IY1 = I1MYC
                  ELSE
                     IY1 = 1+IHALOY
                  END IF
                  IF ( LMYL ) THEN
                     IY2 = I2MYC
                  ELSE
                     IY2 = MYC-IHALOY
                  END IF

!     --- store Hs and Tm as old values and count number of wet grid points

                  WETGRDt = 0
                  DO IX = IX1, IX2
                     DO IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)
                        IF ( DEP2(INDX).GT.DEPMIN ) THEN
                           HSACC0(INDX) = MAX( 1.E-20 , HSACC1(INDX) )
                           HSACC1(INDX) = MAX( 1.E-20 , HSACC2(INDX) )
                           SACC0 (INDX) = MAX( 1.E-20 , SACC1 (INDX) )
                           SACC1 (INDX) = MAX( 1.E-20 , SACC2 (INDX) )
                           WETGRDt = WETGRDt + 1
                        ELSE
                           HSACC0(INDX) = 0.
                           HSACC1(INDX) = 0.
                           SACC0 (INDX) = 0.
                           SACC1 (INDX) = 0.
                        END IF
                     END DO
                  END DO

!     --- compute new values of Hs and Tm

                  DO IX = IX1, IX2
                     DO IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)

                        IF ( DEP2(INDX).GT.DEPMIN ) THEN

                           XMOM0 = 0.
                           XMOM1 = 0.
                           DO IS = 1, MSC
                              DO ID = 1, MDC
                                 ACS2  = SPCSIG(IS)**2 * AC2(ID,IS,INDX)
                                 ACS3  = SPCSIG(IS) * ACS2
                                 XMOM0 = XMOM0 + ACS2
                                 XMOM1 = XMOM1 + ACS3
                              END DO
                           END DO
                           XMOM0 = XMOM0 * FRINTF * DDIR
                           XMOM1 = XMOM1 * FRINTF * DDIR

                           IF ( XMOM0.GT.0. ) THEN
                              HSACC2(INDX) = MAX ( 1.E-20 , 4.*SQRT(XMOM0) )
                              SACC2 (INDX) = MAX ( 1.E-20 , (XMOM1/XMOM0) )
                           ELSEIF ( IQCM.NE.0 .AND. XMOM0.LT.0. ) THEN
                              HSACC2(INDX) = -1.
                              WETGRDt = WETGRDt - 1
                           ELSE
                              HSACC2(INDX) = 1.E-20
                              SACC2 (INDX) = 1.E-20
                           END IF

                        END IF

                     END DO
                  END DO

                  IACCURt = 0

!     --- calculate a set of accuracy parameters based on relative,
!         absolute and curvature values of Hs and check accuracy

                  LHEAD=.TRUE.
                  DO IX = IX1, IX2
                     DO IY = IY1, IY2
                        INDX = KGRPNT(IX,IY)

!           --- determine whether the point is a test point

                        TSTFL = .FALSE.
                        IF (NPTST.GT.0) THEN
                           do II = 1, NPTST
                              IF (IX.EQ.XYTST(2*II-1) .AND. &
                              &IY.EQ.XYTST(2*II)) TSTFL = .TRUE.
                           end do
                        END IF

                        DELHS(INDX) = 0.0
                        DELTM(INDX) = 0.0
                        IF ( DEP2(INDX).GT.DEPMIN ) THEN

                           HSABS = ABS ( HSACC2(INDX) - HSACC1(INDX) )
                           HSREL = HSABS / HSACC2(INDX)
                           TMABS = ABS ( (PI2/SACC2(INDX)) - (PI2/SACC1(INDX)) )
                           TMREL = TMABS / SACC2(INDX)

                           HSDIFO       = HSDIFC(INDX)
                           HSDIFC(INDX) = 0.5*( HSACC2(INDX) - HSACC0(INDX) )
                           HSCURV       = ABS(HSDIFC(INDX) - HSDIFO)/HSACC2(INDX)

                           TMDIFO       = TMDIFC(INDX)
                           TMDIFC(INDX) = 0.5*( SACC2(INDX) - SACC0(INDX) )
                           TMCURV       = ABS(TMDIFC(INDX) - TMDIFO)/SACC2(INDX)

                           DELHS(INDX) = HSABS
                           IF (EQREAL(SACC1(INDX),1.E-20) .OR.&
                           &EQREAL(SACC2(INDX),1.E-20) ) THEN
                              DELTM(INDX) = 0.
                           ELSE
                              DELTM(INDX) = TMABS
                           END IF

                           IF ( IQCM.EQ.0 ) THEN
                              LCONV = ( HSABS.LE.PNUMS(2) .OR.&
                              &(HSREL.LE.PNUMS(1).AND.HSCURV.LE.PNUMS(15)) )&
                              &.AND.&
                              &( TMCURV.LE.PNUMS(16) .AND.&
                              &(TMREL.LE.PNUMS(1) .OR. TMABS.LE.PNUMS(3)) )
                           ELSE
                              IF ( HSACC1(INDX).NE.1.E-20 .AND.&
                              &HSACC2(INDX).NE.-1.          ) THEN
                                 LCONV = HSREL.LE.PNUMS(1) .OR. HSABS.LE.PNUMS(2)
                              ELSE
                                 LCONV = .FALSE.
                              ENDIF
                           ENDIF

!              --- add gridpoint in which wave parameters have
!                  reached required accuracy

                           IF ( LCONV ) IACCURt = IACCURt + 1

                           IF (TSTFL.AND.IQCM.EQ.0) THEN
                              IF (LHEAD) WRITE(PRINTF,"(25X,'dHabs ','dHrel ', 'Curvature H ', 'dTabs ','dTrel ', 'Curvature T ')")
                              WRITE(PRINTF,"(1X,SS,'(IX,IY)=(',I5,',',I5,')',' ', 1PE13.6E2,' ',1PE13.6E2,' ',1PE13.6E2,' ', 1PE13.6E2,' ',1PE13.6E2,' ',1PE13.6E2)") IX+MXF-2, IY+MYF-2, HSABS, HSREL,&
                              &HSCURV, TMABS, TMREL, TMCURV
                              LHEAD=.FALSE.
                           END IF

                        END IF

                     END DO
                  END DO

!     --- global sum of IACCUR and WETGRD
!$OMP ATOMIC
                  IACCUR = IACCUR + IACCURt
!$OMP ATOMIC
                  WETGRD = WETGRD + WETGRDt

!     --- carry out reductions across all nodes
!
!$OMP BARRIER
!$OMP MASTER
                  IARR(1) = IACCUR
                  IARR(2) = WETGRD
                  CALL SWREDUCE ( IARR, 2, SWSUM )
!MPI                  IF (STPNOW()) RETURN
                  IACCUR = IARR(1)
                  WETGRD = IARR(2)
                  ACCUR  = CEILING(REAL(IACCUR) * 10000. / REAL(WETGRD))/100.
!$OMP END MASTER
!$OMP BARRIER
!
!     --- test output
!
!$OMP MASTER
                  IF ( ITEST.GE.30 ) THEN
                     WRITE(PRINTF,"(' SWSTPC: DHREL DHABS CURV :',3E12.4)") PNUMS(1), PNUMS(2), PNUMS(15)
                     WRITE(PRINTF,"(' SWSTPC: WETGRD IACCUR ACCUR :',2I8,E12.4)") WETGRD,IACCUR,ACCUR
                  END IF
!$OMP END MASTER

                  RETURN
               end subroutine SWSTPC
!********************************************************************
!                                                                   *
               SUBROUTINE SETUPP (KGRPNT, MSTPDA, SETPDA, AC2, DEP2, DEPSAV,&
               &SETUP2, XCGRID, YCGRID, SPCSIG, SPCDIR )
   USE swan_number_formatting, ONLY: INTSTR, NUMSTR
   USE swan_service_interfaces, ONLY: MSGERR, STRACE, TXPBLA
   USE swan_wave_physics, ONLY: KSCIP1
!                                                                   *
!********************************************************************

                  USE OCPCOMM4
                  USE SWCOMM3

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
!     30.70: Nico Booij
!     30.72: IJsbrand Haagsma
!     30.81: Annette Kieftenburg
!     30.82: IJsbrand Haagsma
!     31.03: Annette Kieftenburg
!     31.04: Nico Booij
!     32.01: Roeland Ris
!     32.03: IJsbrand Haagsma
!     34.01: Jeroen Adema
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     32.01, Sept 97: New Subroutine
!     32.03, Feb. 98: Comma added in FORMAT to prevent compilation error
!     30.72, Feb. 98: Introduced generic names XCGRID, YCGRID and SPCSIG for SWAN
!     30.70, Feb. 98: transformation of radiation stress in 1D case
!     30.82, Oct. 98: Updated description of several variables
!     30.81, Dec. 98: Argument list KSCIP1 adjusted
!     34.01, Feb. 99: Introducing STPNOW
!     30.82, July 99: Corrected argumentlist SETUPP and SETUP2D
!     30.82, July 99: Corrected argumentlist KSCIP1
!     40.41, Oct. 04: common blocks replaced by modules, include files removed
!     40.41, Dec. 04: this routine is reconsidered, cleaned up and moved to SWANCOM1
!
!  2. Purpose
!
!     computes the wave-induced forces and adds the set-up to the depth
!
!  3. Method
!
!     The wave-induced setup is calculated for the one-dimensional
!     mode of SWAN using the following equation:
!
!        d Sxx                d eta
!        ----- +  ( d + eta ) ----- = 0
!         d x                  d x
!
!     This equation is integrated using the forward Euler technique
!
!     For the two-dimensional case, a 2D Poisson equation in general coordinates
!     is solved by means of vertex-centered finite volume method
!
!  4. Argument variables
!
!     AC2       input    action density
!     DEPSAV    input    depth following from original bottom and water
!     DEP2      i/o      total depth including set-up
!                        on entry: includes previous estimate of set-up
!                        on exit : includes new estimate of set-up
!     KGRPNT    input    indirect addresses for grid points
!     MSTPDA    input    number of (aux.) data per grid point
!                        value is set at 23 in SWANCOM1
!     SETPDA    i/o      auxiliary data for computation of set-up
!                        1: x-comp of force, 2: y-comp of force,
!                        3: radiation stress component RSxx, 4: RSxy,
!                        5: RSyy
!                        SETPDA(*,6..MSTPDA) is used as work array
!     SETUP2    output   computed set-up
!     SPCDIR    input    (*,1); spectral directions (radians)
!                        (*,2); cosine of spectral directions
!                        (*,3); sine of spectral directions
!                        (*,4); cosine^2 of spectral directions
!                        (*,5); cosine*sine of spectral directions
!                        (*,6); sine^2 of spectral directions
!     SPCSIG    input    Relative frequencies in computational domain
!                        in sigma-space
!     XCGRID    input    Coordinates of computational grid in x-direction 30.72
!     YCGRID    input    Coordinates of computational grid in y-direction 30.72

                  INTEGER MSTPDA, KGRPNT(MXC,MYC)

                  REAL    AC2(MDC,MSC,MCGRD)
                  REAL    DEP2(MCGRD)
                  REAL    DEPSAV(MCGRD)
                  REAL    SETPDA(MCGRD,MSTPDA)
                  REAL    SETUP2(MCGRD)
                  REAL    SPCDIR(MDC,6)
                  REAL    SPCSIG(MSC)
                  REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)

!  5. Parameter variables
!
!     ---
!
!  6. Local variables
!
!     CG          group velocity
!     CHARS       array to pass character info to MSGERR
!     CK          CGO*KWAVE
!     DDET        determinant
!     DEPMAX      maximum depth
!     DEPLOC      local depth
!     DIX         di/dx
!     DIY         di/dy
!     DJX         dj/dx
!     DJY         dj/dy
!     DP1         depth in point i in 1-D case
!     DP2         depth in point i+1 in 1-D case
!     DS2         square of mesh length in x- or y-direction
!     DXI         dx/di
!     DXJ         dx/dj
!     DYI         dy/di
!     DYJ         dy/dj
!     ELOC        local energy
!     ETA1        setup in  point i in 1-D case
!     ETA2        setup in  point i+1 in 1-D case
!     ID          counter in directional space
!     IDXMAX      index of location with maximum depth
!     IENT        number of entries
!     IF1         first non-character in string1
!     IL1         last non-character in string1
!     INDX        address of current grid point (ix,iy)
!     INDXB       address of grid point (ix,iy-1)
!     INDXL       address of grid point (ix-1,iy)
!     INDXR       address of grid point (ix+1,iy)
!     INDXU       address of grid point (ix,iy+1)
!     IS          counter in frequency space
!     IX          counter in x-direction
!     IXLO        counter in x-direction for neighbouring grid point
!     IXUP        counter in x-direction for neighbouring grid point
!     IY          counter in y-direction
!     IYLO        counter in y-direction for neighbouring grid point
!     IYUP        counter in y-direction for neighbouring grid point
!     K           wavenumber
!     LINK        counter for neighbouring grid points
!     MSGSTR      string to pass message to call MSGERR
!     N           CGroup/CPhase
!     ND          derivative of N with respect to depth
!     NEIGHB      boolean variable indicating whether neighbouring point is wet
!     RRDI        1/number of steps in i-direction
!     RRDJ        1/number of steps in j-direction
!     RSXX        xx-component of the radiation stress
!     RSXXI       derivative of RSXX in i-direction
!     RSXXJ       derivative of RSXX in j-direction
!     RSXY        xy-component of the radiation stress
!     RSXYI       derivative of RSXY in i-direction
!     RSXYJ       derivative of RSXY in j-direction
!     RSYY        yy-component of the radiation stress
!     RSYYI       derivative of RSYY in i-direction
!     RSYYJ       derivative of RSYY in j-direction
!     S_UPCOR     total correction to setup (user defined and S_UPDP)
!     S_UPDP      setup at location with maximum depth, before correction
!     SIG         dummy variable for frequency
!     SXX1        radiation stress in  point i in 1-D case
!     SXX2        radiation stress in  point i+1 in 1-D case

                  INTEGER, SAVE :: IENT = 0
                  INTEGER  IDXMAX, INDXR, INDXU, INDXB,&
                  &ID, INDX, INDXL, IS, IX,&
                  &IXLO, IXUP, IY, IYLO, IYUP, LINK

                  REAL     CK, DDET, DEPLOC, DEPMAX, DIX, DIY, DJX, DJY,&
                  &DP1, DP2, DS2, DXI, DXJ, DYI, DYJ, ELOC, ETA1, ETA2,&
                  &RRDI, RRDJ, RSXX,  RSXXI, RSXXJ, RSXY,&
                  &RSXYI, RSXYJ, RSYY, RSYYI, RSYYJ,&
                  &S_UPCOR,&
                  &S_UPDP,&
                  &SXX1  ,SXX2
                  REAL     CG(1), K(1), N(1), ND(1), SIG(1)
                  INTEGER      IF1, IL1
                  CHARACTER(LEN=20) CHARS(1)
                  CHARACTER(LEN=80) MSGSTR

                  LOGICAL  NEIGHB

!  8. Subroutines used
!
!     INTSTR           Converts integer to string
!     KSCIP1           Calculates KWAVE, CGO
!     MSGERR           Writes error message
!     SETUP2D          Computation of the change of waterlevel by waves,
!                      a 2D Poisson equation in general coordinates is solved
!     STRACE           Tracing routine for debugging
!     TXPBLA           Removes leading and trailing blanks in string
!
!  9. Subroutines calling
!
!     SWCOMP
!
! 10. Error messages
!
!     setup in dry point is unequal to zero
!
! 11. Remarks
!
! 12. Structure
!
!     ---------------------------------------------------------
!     For all grid points do
!         If depth > DEPMIN
!         Then Integrate over spectrum to compute RSxx, RSxy, RSyy
!     ---------------------------------------------------------
!     If one-dimensional mode of SWAN
!     Then Calculate Setup in all grid points
!     Else Call SETUP2 to compute setup in all grid points
!     ---------------------------------------------------------
!     S_UPDP is setup in deepest point
!     Add user defined correction to setup
!     For all grid points do
!         If dep2 > DEPMIN
!            SETUP2 := SETUP2 - S_UPCOR
!         If dep2 > DEPMIN
!            compute new value for DEP2
!     ---------------------------------------------------------
!     For all grid points do
!         If depth < DEPMIN
!         Then If water level + setup in neighbouring point above
!                   bottom level in current point
!              Then make depth equal to neighbouring water level
!                   + SETUP - bottom level in current point
!     ---------------------------------------------------------
!
! 13. Source text
!
!************************************************************************

                  CALL STRACE (IENT, 'SETUPP')

                  DEPMAX = 0.
                  IDXMAX = 0

!     --- initializing SETPDA array

                  SETPDA = 0.

                  DO IY = 1, MYC
                     DO IX = 1, MXC
                        INDX = KGRPNT(IX,IY)
                        IF (INDX.GT.1) THEN
                           IF (DEP2(INDX).GT.DEPMIN) THEN

!             --- seek deepest point

                              IF (DEPSAV(INDX).GT.DEPMAX) THEN
                                 DEPMAX = DEPSAV(INDX)
                                 IDXMAX = INDX
                              ENDIF

!             --- compute radiation stress components RSXX, RSXY and RSYY

                              RSXX = 0.
                              RSXY = 0.
                              RSYY = 0.
                              DEPLOC = DEP2(INDX)
                              DO IS = 1, MSC
                                 SIG(1) = SPCSIG(IS)
                                 CALL KSCIP1 (1,SIG,DEPLOC,K,CG,N,ND)
                                 CK = CG(1) * K(1)
                                 DO ID = 1, MDC
                                    ELOC = SIG(1) * AC2(ID,IS,INDX)
!                                  -
!                                  |{cos(Theta)}^2         for i = 4
!                 SPCDIR(ID,i) is <| sin(Theta)cos(Theta)  for i = 5
!                                  |{sin(Theta)}^2         for i = 6
!                                  -

                                    RSXX = RSXX + (CK*SPCDIR(ID,4)+CK - SIG(1)/2.) * ELOC
                                    RSXY = RSXY + CK*SPCDIR(ID,5) * ELOC
                                    RSYY = RSYY + (CK*SPCDIR(ID,6)+CK - SIG(1)/2.) * ELOC
                                 ENDDO
                              ENDDO

!             --- store radiation stress components in array SETPDA
!
!             DDIR   is width of directional band
!             FRINTF is frequency integration factor df/f

                              IF (ONED) THEN
!               transform to computational direction
                                 SETPDA(INDX,3) = DDIR * FRINTF *&
                                 &((COSPC*RSXX + SINPC*RSXY) * COSPC +&
                                 &(COSPC*RSXY + SINPC*RSYY) * SINPC)
                              ELSE
                                 SETPDA(INDX,3) = RSXX * DDIR * FRINTF
                                 SETPDA(INDX,4) = RSXY * DDIR * FRINTF
                                 SETPDA(INDX,5) = RSYY * DDIR * FRINTF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO

                  IF ( ONED ) THEN

!       *** compute on the basis of the radiation stresses the setup ***

                     DO IY = 1, MYC
!          *** boundary condition ***
                        SETUP2(KGRPNT(1,IY)) = 0.
                        ETA2 = 0.
                        DO IX = 1, MXC-1
                           INDX  = KGRPNT(IX  ,IY)
                           INDXR = KGRPNT(IX+1,IY)
                           DP1   = DEP2(INDX )
                           DP2   = DEP2(INDXR)
                           IF ( INDX .GT.1 .AND. DP1.GT.DEPMIN .AND.&
                           &INDXR.GT.1 .AND. DP2.GT.DEPMIN ) THEN
                              ETA1 = SETUP2(INDX)
                              SXX1 = SETPDA(INDX ,3)
                              SXX2 = SETPDA(INDXR,3)
                              ETA2 = ETA1 + ( SXX1 - SXX2 ) / ( 0.5 * ( DP2 + DP1 ) )
                              SETUP2(INDXR) = ETA2
                           ELSE
                              SETUP2(INDXR) = 0.
                           END IF
                        END DO
                     END DO

                  ELSE

!       --- compute forces by taking derivative of radiation stress

                     DO IY = 1, MYC
                        DO IX = 1, MXC
                           INDX = KGRPNT(IX,IY)
                           DEPLOC = DEP2(INDX)
                           IF (INDX.GT.1 .AND. DEPLOC.GT.DEPMIN) THEN
                              IF (IX.EQ.1) THEN
                                 IXLO = 1
                                 IXUP = 2
                              ELSE IF (IX.EQ.MXC) THEN
                                 IXLO = MXC-1
                                 IXUP = MXC
                              ELSE
                                 IXLO = IX-1
                                 IXUP = IX+1
                              END IF
                              IF (DEP2(KGRPNT(IXLO,IY)).LE.DEPMIN) IXLO = IX
                              IF (DEP2(KGRPNT(IXUP,IY)).LE.DEPMIN) IXUP = IX
                              INDXL = KGRPNT(IXLO,IY)
                              INDXR = KGRPNT(IXUP,IY)
                              IF (IXLO.EQ.IXUP) THEN
                                 RRDI = 1.E-20
                              ELSE
                                 RRDI = 1. / REAL(IXUP-IXLO)
                              END IF
                              IF (IY.EQ.1) THEN
                                 IYLO = 1
                                 IYUP = 2
                              ELSE IF (IY.EQ.MYC) THEN
                                 IYLO = MYC-1
                                 IYUP = MYC
                              ELSE
                                 IYLO = IY-1
                                 IYUP = IY+1
                              ENDIF
                              IF (DEP2(KGRPNT(IX,IYLO)).LE.DEPMIN) IYLO = IY
                              IF (DEP2(KGRPNT(IX,IYUP)).LE.DEPMIN) IYUP = IY
                              INDXB = KGRPNT(IX,IYLO)
                              INDXU = KGRPNT(IX,IYUP)
                              IF (IYLO.EQ.IYUP) THEN
                                 RRDJ = 1.E-20
                              ELSE
                                 RRDJ = 1. / REAL(IYUP-IYLO)
                              END IF

!                --- determine (x,y) derivatives w.r.t. i and j

                              DXI = RRDI * (XCGRID(IXUP,IY)-XCGRID(IXLO,IY))
                              DYI = RRDI * (YCGRID(IXUP,IY)-YCGRID(IXLO,IY))
                              DXJ = RRDJ * (XCGRID(IX,IYUP)-XCGRID(IX,IYLO))
                              DYJ = RRDJ * (YCGRID(IX,IYUP)-YCGRID(IX,IYLO))

                              RSXXI = RRDI * (SETPDA(INDXR,3)-SETPDA(INDXL,3))
                              RSXXJ = RRDJ * (SETPDA(INDXU,3)-SETPDA(INDXB,3))
                              RSXYI = RRDI * (SETPDA(INDXR,4)-SETPDA(INDXL,4))
                              RSXYJ = RRDJ * (SETPDA(INDXU,4)-SETPDA(INDXB,4))
                              RSYYI = RRDI * (SETPDA(INDXR,5)-SETPDA(INDXL,5))
                              RSYYJ = RRDJ * (SETPDA(INDXU,5)-SETPDA(INDXB,5))

                              IF (IXLO.EQ.IXUP.AND.IYLO.EQ.IYUP) THEN
!                point surrounded by dry points
                                 DIX = 0.
                                 DIY = 0.
                                 DJX = 0.
                                 DJY = 0.
                              ELSE IF (IXLO.EQ.IXUP) THEN
!                no forces in i-direction
                                 DS2 = DXJ**2 + DYJ**2
                                 DIX = 0.
                                 DIY = 0.
                                 DJX = DXJ/DS2
                                 DJY = DYJ/DS2
                              ELSE IF (IYLO.EQ.IYUP) THEN
!                no forces in j-direction
                                 DS2 = DXI**2 + DYI**2
                                 DIX = DXI/DS2
                                 DIY = DYI/DS2
                                 DJX = 0.
                                 DJY = 0.
                              ELSE
!                coefficients for transformation from
!                (i,j)-gradients to (x,y)-gradients
                                 DDET = DXI*DYJ - DXJ*DYI
                                 DIX  =  DYJ / DDET
                                 DIY  = -DXJ / DDET
                                 DJX  = -DYI / DDET
                                 DJY  =  DXI / DDET
                              END IF

!                --- forces based on spatial gradients of radiation stresses
                              SETPDA(INDX,1) =&
                              &-(RSXXI*DIX + RSXXJ*DJX + RSXYI*DIY + RSXYJ*DJY)
                              SETPDA(INDX,2) =&
                              &-(RSXYI*DIX + RSXYJ*DJX + RSYYI*DIY + RSYYJ*DJY)

                           END IF
                        END DO
                     END DO

!       --- compute set-up in two dimensions

                     CALL SETUP2D( SETUP2, XCGRID, YCGRID, SETPDA(1,1), SETPDA(1,2),&
                     &KGRPNT, DEP2, SETPDA(1,6), SETPDA(1,15),&
                     &SETPDA(1,16) )

                  END IF

                  IF (LSETUP.EQ.1) THEN
!       set set-up to 0 for deepest point (This is allowed because the
!       solution of a Poisson equation + constant is again a solution of  31.03
!       the same Poisson equation)
                     S_UPDP = SETUP2(IDXMAX)
                     S_UPCOR = S_UPDP - PSETUP(2)
                     DO IY = 1, MYC
                        DO IX = 1, MXC
                           INDX = KGRPNT(IX,IY)
                           IF (INDX.GT.1) THEN
                              IF (DEP2(INDX).GT.DEPMIN) THEN
                                 SETUP2(INDX) = SETUP2(INDX) - S_UPCOR
                              ELSE
                                 IF (ABS(SETUP2(INDX)).GT.1.E-7) THEN
                                    CHARS(1) = INTSTR(INDX)
                                    CALL TXPBLA(CHARS(1),IF1,IL1)
                                    MSGSTR = 'Set-up in dry point with index '//&
                                    &CHARS(1)(IF1:IL1)
                                    CALL MSGERR ( 2, MSGSTR )
                                 END IF
                              END IF
                           END IF
                        END DO
                     END DO
                  END IF

!     --- include computed set-up to depth

                  DO IY = 1, MYC
                     DO IX = 1, MXC
                        INDX = KGRPNT(IX,IY)
                        IF (INDX.GT.1) THEN
                           DEP2(INDX) = DEPSAV(INDX) + SETUP2(INDX)
                        END IF
                     END DO
                  END DO

!     --- check whether dry points should be inundated

                  DO IY = 1, MYC
                     DO IX = 1, MXC
                        INDX = KGRPNT(IX,IY)
!         Note:    KGRPNT(.,.) = 1 means a permanently dry point!
                        IF (INDX.GT.1) THEN
                           IF (DEP2(INDX).LE.DEPMIN) THEN
                              DO LINK = 1, 4
                                 NEIGHB = .TRUE.
                                 IF (LINK.EQ.1) THEN
                                    IF (IX.EQ.1) THEN
                                       NEIGHB = .FALSE.
                                    ELSE
                                       INDXL = KGRPNT(IX-1,IY)
                                       IF (INDXL.LE.1) NEIGHB = .FALSE.
                                    ENDIF
                                 ELSE IF (LINK.EQ.2) THEN
                                    IF (IY.EQ.1) THEN
                                       NEIGHB = .FALSE.
                                    ELSE
                                       INDXL = KGRPNT(IX,IY-1)
                                       IF (INDXL.LE.1) NEIGHB = .FALSE.
                                    ENDIF
                                 ELSE IF (LINK.EQ.3) THEN
                                    IF (IX.EQ.MXC) THEN
                                       NEIGHB = .FALSE.
                                    ELSE
                                       INDXL = KGRPNT(IX+1,IY)
                                       IF (INDXL.LE.1) NEIGHB = .FALSE.
                                    ENDIF
                                 ELSE IF (LINK.EQ.4) THEN
                                    IF (IY.EQ.MYC) THEN
                                       NEIGHB = .FALSE.
                                    ELSE
                                       INDXL = KGRPNT(IX,IY+1)
                                       IF (INDXL.LE.1) NEIGHB = .FALSE.
                                    ENDIF
                                 ENDIF
                                 IF (NEIGHB) THEN
                                    IF (DEPSAV(INDX) + SETUP2(INDXL) .GT. DEPMIN) THEN
                                       SETUP2(INDX) = SETUP2(INDXL)
                                       DEP2(INDX) = DEPSAV(INDX) + SETUP2(INDXL)
                                    ENDIF
                                 ENDIF
                              ENDDO
                           ENDIF
                        ENDIF
                     ENDDO
                  ENDDO

                  RETURN
!     end of subroutine SETUPP
               end subroutine SETUPP
!****************************************************************

               SUBROUTINE SETUP2D ( SETUP , XCGRID, YCGRID, WFRCX, WFRCY,&
               &KGRPNT, DEPTH , AMAT  , RHS  , JCTA )
   USE swan_service_interfaces, ONLY: STRACE

!****************************************************************

                  USE OCPCOMM4
                  USE SWCOMM3

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
!     40.41: Marcel Zijlema
!
!  1. Updates
!
!     40.41, Dec. 04: New subroutine
!
!  2. Purpose
!
!     Computation of the change of waterlevel due to waves
!
!  3. Method
!
!     A 2D Poisson equation in general coordinates is solved
!     Vertex-centered finite volume method is employed
!     The system of equations is solved using a SOR method
!
!  4. Argument variables
!
!     AMAT        the coefficient matrix used in the linear system
!     DEPTH       water depth
!     JCTA        Jacobian times contravariant base vectors:
!                                          (K)
!                 JCTA(I,J,K,L)  contains a    in cell with index point
!                                          L
!                 and point type J
!     KGRPNT      indirect addressing for grid points
!     RHS         the right-hand side vector of the system of equations
!     SETUP       set-up
!     WFRCX       x-component of wave-induced force
!     WFRCY       y-component of wave-induced force
!     XCGRID      x-coordinates of computational grid
!     YCGRID      y-coordinates of computational grid

                  INTEGER KGRPNT(MXC,MYC)
                  REAL    XCGRID(MXC,MYC), YCGRID(MXC,MYC)
                  REAL    DEPTH(MCGRD), SETUP(MCGRD), WFRCX(MCGRD), WFRCY(MCGRD)
                  REAL    AMAT(MCGRD,9), JCTA(MCGRD,2,2,2), RHS(MCGRD)

!  5. Parameter variables
!
!     RELAX :     relaxation parameter
!                 =  -1: relaxation parameter based on gridsizes
!                 <> -1: a fixed (initial) relaxation parameter

                  REAL, PARAMETER :: RELAX=-1.

!  6. Local variables
!
!     CONTRB:     auxiliary variable containing contribution to the matrix
!     DEPF  :     water depth in flux point
!     FACT  :     a factor
!     IAMOUT:     control parameter indicating the amount of
!                 output required
!                 0: no output
!                 1: only fatal errors will be printed
!                 2: gives output concerning the iteration process
!                 3: additional information about the iteration
!                    is printed
!     ICONV :     indicator for convergence (1=yes, 0=no)
!     IENT  :     number of entries
!     II    :     iteration count in case of omega#1
!     INDX  :     index counter for point (ix  ,iy  ) in computational grid
!     INDXB :     index counter for point (ix  ,iy-1) in computational grid
!     INDXL :     index counter for point (ix-1,iy  ) in computational grid
!     INDXLB:     index counter for point (ix-1,iy-1) in computational grid
!     INDXLU:     index counter for point (ix-1,iy+1) in computational grid
!     INDXR :     index counter for point (ix+1,iy  ) in computational grid
!     INDXRB:     index counter for point (ix+1,iy-1) in computational grid
!     INDXRU:     index counter for point (ix+1,iy+1) in computational grid
!     INDXU :     index counter for point (ix  ,iy+1) in computational grid
!     IT    :     iteration count
!     IX    :     counter in x-direction
!     IXINF :     point in x-direction with largest error in solution
!     IY    :     counter in y-direction
!     IYINF :     point in y-direction with largest error in solution
!     JAC   :     Jacobian
!     MAXIT :     the maximum number of iterations to be performed in
!                 the linear solver
!     RES   :     residual
!     RESM  :     inf-norm of residual vector
!     RESMI :     intermediate inf-norm of residual vector
!     RESMO :     inf-norm of residual vector at previous iteration
!     RESM0 :     inf-norm of initial residual vector
!     REPS  :     relative accuracy of the final approximation
!     RHOV  :     estimated largest real eigenvalue
!     SETPI :     intermediate solution for set-up
!     XL    :     measure of convergence speed
!     XOM   :     actual relaxation parameter
!     XOMEG :     optimal overrelaxation parameter for SOR method

                  INTEGER, SAVE :: IENT = 0
                  INTEGER IAMOUT, ICONV, INDX, INDXB, INDXL, INDXLB, INDXLU,&
                  &INDXR, INDXRB, INDXRU, INDXU, IT, IX, IXINF, IY, IYINF,&
                  &II, MAXIT
                  REAL    CONTRB, DEPF, FACT, JAC, RES, RESM, RESMI, RESMO, RESM0,&
                  &REPS, RHOV, SETPI, XL, XOM, XOMEG

!  7. Common blocks used
!
!
!  8. Subroutines used
!
!     STRACE           Tracing routine for debugging
!
!  9. Subroutines calling
!
!     SETUPP
!
! 10. Error messages
!
!     ---
!
! 11. Remarks
!
!     1) point type (=1,2) defines the position of the point in a cell:
!
!        *-------*
!        |       |
!        2       |
!        |       |
!        *---1---*
!
!        * = cell corner, depth point
!
!     2) Neumann boundary condition is imposed on all the boundaries
!
!     3) The determination of the overrelaxation factor by means of
!        alternatively switching between 1 and optimal omega is based
!        on the method as described in
!
!        E.F.F. Botta and M.H.M. Ellenbroek
!        A modified SOR method for the Poisson equation in unsteady
!        free-surface flow calculations
!        J. Comput. Phys., vol. 60, 119-134, 1985
!
! 12. Structure
!
!     initialize some arrays
!     determine contravariant base vectors times Jacobian
!     build right-hand side of the system of equations
!     build the matrix of the linear system
!     in case of nesting put Dirichlet boundary condition
!     set parameters for the solver
!     determine relaxation factor
!     solve the system of equations
!     investigate the reason to stop
!
! 13. Source text

                  IF (LTRACE) CALL STRACE (IENT,'SETUP2D')

!     --- initialize some arrays

                  AMAT = 0.
                  JCTA = 0.
                  RHS  = 0.

!     --- determine contravariant base vector a^(1) times Jacobian
!         in point type 1

                  DO IX = 1, MXC-1
                     INDX = KGRPNT(IX,1)
                     IF ( INDX.GT.1 ) THEN
                        JCTA(INDX,1,1,1) = 0.5*( YCGRID(IX+1,2) +&
                        &YCGRID(IX  ,2) -&
                        &YCGRID(IX+1,1) -&
                        &YCGRID(IX  ,1) )
                        JCTA(INDX,1,1,2) = 0.5*( XCGRID(IX+1,1) +&
                        &XCGRID(IX  ,1) -&
                        &XCGRID(IX+1,2) -&
                        &XCGRID(IX  ,2) )
                     END IF
                  END DO
                  DO IY = 2, MYC-1
                     DO IX = 1, MXC-1
                        INDX = KGRPNT(IX,IY)
                        IF ( INDX.GT.1 ) THEN
                           JCTA(INDX,1,1,1) = 0.25*( YCGRID(IX+1,IY+1) +&
                           &YCGRID(IX  ,IY+1) -&
                           &YCGRID(IX+1,IY-1) -&
                           &YCGRID(IX  ,IY-1) )
                           JCTA(INDX,1,1,2) = 0.25*( XCGRID(IX+1,IY-1) +&
                           &XCGRID(IX  ,IY-1) -&
                           &XCGRID(IX+1,IY+1) -&
                           &XCGRID(IX  ,IY+1) )
                        END IF
                     END DO
                  END DO
                  DO IX = 1, MXC-1
                     INDX = KGRPNT(IX,MYC)
                     IF ( INDX.GT.1 ) THEN
                        JCTA(INDX,1,1,1) = 0.5*( YCGRID(IX+1,MYC  ) +&
                        &YCGRID(IX  ,MYC  ) -&
                        &YCGRID(IX+1,MYC-1) -&
                        &YCGRID(IX  ,MYC-1) )
                        JCTA(INDX,1,1,2) = 0.5*( XCGRID(IX+1,MYC-1) +&
                        &XCGRID(IX  ,MYC-1) -&
                        &XCGRID(IX+1,MYC  ) -&
                        &XCGRID(IX  ,MYC  ) )
                     END IF
                  END DO

!     --- determine contravariant base vector a^(1) times Jacobian
!         in point type 2

                  DO IY = 1, MYC-1
                     DO IX = 1, MXC
                        INDX = KGRPNT(IX,IY)
                        IF ( INDX.GT.1 ) THEN
                           JCTA(INDX,2,1,1) = YCGRID(IX,IY+1) - YCGRID(IX,IY  )
                           JCTA(INDX,2,1,2) = XCGRID(IX,IY  ) - XCGRID(IX,IY+1)
                        END IF
                     END DO
                  END DO

!     --- determine contravariant base vector a^(2) times Jacobian
!         in point type 1

                  DO IY = 1, MYC
                     DO IX = 1, MXC-1
                        INDX = KGRPNT(IX,IY)
                        IF ( INDX.GT.1 ) THEN
                           JCTA(INDX,1,2,1) = YCGRID(IX  ,IY) - YCGRID(IX+1,IY)
                           JCTA(INDX,1,2,2) = XCGRID(IX+1,IY) - XCGRID(IX  ,IY)
                        END IF
                     END DO
                  END DO

!     --- determine contravariant base vector a^(2) times Jacobian
!         in point type 2

                  DO IY = 1, MYC-1
                     INDX = KGRPNT(1,IY)
                     IF ( INDX.GT.1 ) THEN
                        JCTA(INDX,2,2,1) = 0.5*( YCGRID(1,IY+1) +&
                        &YCGRID(1,IY  ) -&
                        &YCGRID(2,IY+1) -&
                        &YCGRID(2,IY  ) )
                        JCTA(INDX,2,2,2) = 0.5*( XCGRID(2,IY+1) +&
                        &XCGRID(2,IY  ) -&
                        &XCGRID(1,IY+1) -&
                        &XCGRID(1,IY  ) )
                     END IF
                     DO IX = 2, MXC-1
                        INDX = KGRPNT(IX,IY)
                        IF ( INDX.GT.1 ) THEN
                           JCTA(INDX,2,2,1) = 0.25*( YCGRID(IX-1,IY+1) +&
                           &YCGRID(IX-1,IY  ) -&
                           &YCGRID(IX+1,IY+1) -&
                           &YCGRID(IX+1,IY  ) )
                           JCTA(INDX,2,2,2) = 0.25*( XCGRID(IX+1,IY+1) +&
                           &XCGRID(IX+1,IY  ) -&
                           &XCGRID(IX-1,IY+1) -&
                           &XCGRID(IX-1,IY  ) )
                        END IF
                     END DO
                     INDX = KGRPNT(MXC,IY)
                     IF ( INDX.GT.1 ) THEN
                        JCTA(INDX,2,2,1) = 0.5*( YCGRID(MXC-1,IY+1) +&
                        &YCGRID(MXC-1,IY  ) -&
                        &YCGRID(MXC  ,IY+1) -&
                        &YCGRID(MXC  ,IY  ) )
                        JCTA(INDX,2,2,2) = 0.5*( XCGRID(MXC  ,IY+1) +&
                        &XCGRID(MXC  ,IY  ) -&
                        &XCGRID(MXC-1,IY+1) -&
                        &XCGRID(MXC-1,IY  ) )
                     END IF
                  END DO

!     --- build right-hand side of the system of equations
!
!     --- interior domain
                  DO IY = 2, MYC-1
                     DO IX = 2, MXC-1
                        INDX  = KGRPNT(IX  ,IY  )
                        INDXL = KGRPNT(IX-1,IY  )
                        INDXR = KGRPNT(IX+1,IY  )
                        INDXB = KGRPNT(IX  ,IY-1)
                        INDXU = KGRPNT(IX  ,IY+1)
                        IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!              --- contribution of right flux
                           IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                              RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1)&
                              &*(WFRCX(INDX)+WFRCX(INDXR))&
                              &- 0.5*JCTA(INDX,1,1,2)&
                              &*(WFRCY(INDX)+WFRCY(INDXR))
                           END IF
!              --- contribution of left flux
                           IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                              RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1)&
                              &*(WFRCX(INDXL)+WFRCX(INDX))&
                              &+ 0.5*JCTA(INDXL,1,1,2)&
                              &*(WFRCY(INDXL)+WFRCY(INDX))
                           END IF
!              --- contribution of upper flux
                           IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                              RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1)&
                              &*(WFRCX(INDX)+WFRCX(INDXU))&
                              &- 0.5*JCTA(INDX,2,2,2)&
                              &*(WFRCY(INDX)+WFRCY(INDXU))
                           END IF
!              --- contribution of bottom flux
                           IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                              RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1)&
                              &*(WFRCX(INDXB)+WFRCX(INDX))&
                              &+ 0.5*JCTA(INDXB,2,2,2)&
                              &*(WFRCY(INDXB)+WFRCY(INDX))
                           END IF
                        END IF
                     END DO
                  END DO

!     --- lower boundary (IY=1)
                  DO IX = 2, MXC-1
                     INDX  = KGRPNT(IX  ,1)
                     INDXL = KGRPNT(IX-1,1)
                     INDXR = KGRPNT(IX+1,1)
                     INDXU = KGRPNT(IX  ,2)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXR))&
                           &- 0.5*JCTA(INDX,1,1,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXR))
                        END IF
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1)&
                           &*(WFRCX(INDXL)+WFRCX(INDX))&
                           &+ 0.5*JCTA(INDXL,1,1,2)&
                           &*(WFRCY(INDXL)+WFRCY(INDX))
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXU))&
                           &- JCTA(INDX,2,2,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXU))
                        END IF
                     END IF
                  END DO

!     --- upper boundary (IY=MYC)
                  DO IX = 2, MXC-1
                     INDX  = KGRPNT(IX  ,MYC  )
                     INDXL = KGRPNT(IX-1,MYC  )
                     INDXR = KGRPNT(IX+1,MYC  )
                     INDXB = KGRPNT(IX  ,MYC-1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,1,1,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXR))&
                           &- 0.5*JCTA(INDX,1,1,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXR))
                        END IF
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXL,1,1,1)&
                           &*(WFRCX(INDXL)+WFRCX(INDX))&
                           &+ 0.5*JCTA(INDXL,1,1,2)&
                           &*(WFRCY(INDXL)+WFRCY(INDX))
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1)&
                           &*(WFRCX(INDXB)+WFRCX(INDX))&
                           &+ JCTA(INDXB,2,2,2)&
                           &*(WFRCY(INDXB)+WFRCY(INDX))
                        END IF
                     END IF
                  END DO

!     --- left boundary (IX=1)
                  DO IY = 2, MYC-1
                     INDX  = KGRPNT(1,IY  )
                     INDXR = KGRPNT(2,IY  )
                     INDXB = KGRPNT(1,IY-1)
                     INDXU = KGRPNT(1,IY+1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXR))&
                           &- JCTA(INDX,1,1,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXR))
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXU))&
                           &- 0.5*JCTA(INDX,2,2,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXU))
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1)&
                           &*(WFRCX(INDXB)+WFRCX(INDX))&
                           &+ 0.5*JCTA(INDXB,2,2,2)&
                           &*(WFRCY(INDXB)+WFRCY(INDX))
                        END IF
                     END IF
                  END DO

!     --- right boundary (IX=MXC)
                  DO IY = 2, MYC-1
                     INDX  = KGRPNT(MXC  ,IY  )
                     INDXL = KGRPNT(MXC-1,IY  )
                     INDXB = KGRPNT(MXC  ,IY-1)
                     INDXU = KGRPNT(MXC  ,IY+1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1)&
                           &*(WFRCX(INDXL)+WFRCX(INDX))&
                           &+ JCTA(INDXL,1,1,2)&
                           &*(WFRCY(INDXL)+WFRCY(INDX))
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) - 0.5*JCTA(INDX,2,2,1)&
                           &*(WFRCX(INDX)+WFRCX(INDXU))&
                           &- 0.5*JCTA(INDX,2,2,2)&
                           &*(WFRCY(INDX)+WFRCY(INDXU))
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           RHS(INDX) = RHS(INDX) + 0.5*JCTA(INDXB,2,2,1)&
                           &*(WFRCX(INDXB)+WFRCX(INDX))&
                           &+ 0.5*JCTA(INDXB,2,2,2)&
                           &*(WFRCY(INDXB)+WFRCY(INDX))
                        END IF
                     END IF
                  END DO

!     --- left-lower corner (IX=1,IY=1)
                  INDX  = KGRPNT(1,1)
                  INDXR = KGRPNT(2,1)
                  INDXU = KGRPNT(1,2)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
                     IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1)&
                        &*(WFRCX(INDX)+WFRCX(INDXR))&
                        &- JCTA(INDX,1,1,2)&
                        &*(WFRCY(INDX)+WFRCY(INDXR))
                     END IF
!        --- contribution of upper flux
                     IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1)&
                        &*(WFRCX(INDX)+WFRCX(INDXU))&
                        &- JCTA(INDX,2,2,2)&
                        &*(WFRCY(INDX)+WFRCY(INDXU))
                     END IF
                  END IF

!     --- right-lower corner (IX=MXC,IY=1)
                  INDX  = KGRPNT(MXC  ,1)
                  INDXL = KGRPNT(MXC-1,1)
                  INDXU = KGRPNT(MXC  ,2)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
                     IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1)&
                        &*(WFRCX(INDXL)+WFRCX(INDX))&
                        &+ JCTA(INDXL,1,1,2)&
                        &*(WFRCY(INDXL)+WFRCY(INDX))
                     END IF
!        --- contribution of upper flux
                     IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) - JCTA(INDX,2,2,1)&
                        &*(WFRCX(INDX)+WFRCX(INDXU))&
                        &- JCTA(INDX,2,2,2)&
                        &*(WFRCY(INDX)+WFRCY(INDXU))
                     END IF
                  END IF

!     --- left-upper corner (IX=1,IY=MYC)
                  INDX  = KGRPNT(1,MYC  )
                  INDXR = KGRPNT(2,MYC  )
                  INDXB = KGRPNT(1,MYC-1)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
                     IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) - JCTA(INDX,1,1,1)&
                        &*(WFRCX(INDX)+WFRCX(INDXR))&
                        &- JCTA(INDX,1,1,2)&
                        &*(WFRCY(INDX)+WFRCY(INDXR))
                     END IF
!        --- contribution of bottom flux
                     IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1)&
                        &*(WFRCX(INDXB)+WFRCX(INDX))&
                        &+ JCTA(INDXB,2,2,2)&
                        &*(WFRCY(INDXB)+WFRCY(INDX))
                     END IF
                  END IF

!     --- right-upper corner (IX=MXC,IY=MYC)
                  INDX  = KGRPNT(MXC  ,MYC  )
                  INDXL = KGRPNT(MXC-1,MYC  )
                  INDXB = KGRPNT(MXC  ,MYC-1)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
                     IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) + JCTA(INDXL,1,1,1)&
                        &*(WFRCX(INDXL)+WFRCX(INDX))&
                        &+ JCTA(INDXL,1,1,2)&
                        &*(WFRCY(INDXL)+WFRCY(INDX))
                     END IF
!        --- contribution of bottom flux
                     IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                        RHS(INDX) = RHS(INDX) + JCTA(INDXB,2,2,1)&
                        &*(WFRCX(INDXB)+WFRCX(INDX))&
                        &+ JCTA(INDXB,2,2,2)&
                        &*(WFRCY(INDXB)+WFRCY(INDX))
                     END IF
                  END IF

!     --- build the matrix of the linear system
!
!     --- interior domain
                  DO IY = 2, MYC-1
                     DO IX = 2, MXC-1
                        INDX  = KGRPNT(IX  ,IY  )
                        INDXL = KGRPNT(IX-1,IY  )
                        INDXR = KGRPNT(IX+1,IY  )
                        INDXB = KGRPNT(IX  ,IY-1)
                        INDXU = KGRPNT(IX  ,IY+1)
                        IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!              --- contribution of right flux
                           IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                              DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
                              JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                              &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                              IF ( JAC.NE.0. ) THEN
                                 FACT = -DEPF/JAC
                              ELSE
                                 FACT = 0.
                              END IF
                              CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                              &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                              AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                              AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                              CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                              &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                              AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                              AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                              AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                              AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                           END IF
!              --- contribution of left flux
                           IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                              DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
                              JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                              &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                              IF ( JAC.NE.0. ) THEN
                                 FACT = -DEPF/JAC
                              ELSE
                                 FACT = 0.
                              END IF
                              CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                              &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                              AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                              AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                              CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                              &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                              AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                              AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                              AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                              AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                           END IF
!              --- contribution of upper flux
                           IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                              DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
                              JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                              &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                              IF ( JAC.NE.0. ) THEN
                                 FACT = -DEPF/JAC
                              ELSE
                                 FACT = 0.
                              END IF
                              CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                              &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                              AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                              AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                              CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                              &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                              AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                              AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                              AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                              AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                           END IF
!              --- contribution of bottom flux
                           IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                              DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
                              JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                              &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                              IF ( JAC.NE.0. ) THEN
                                 FACT = -DEPF/JAC
                              ELSE
                                 FACT = 0.
                              END IF
                              CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                              &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                              AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                              AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                              CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                              &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                              AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                              AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                              AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                              AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                           END IF
                        END IF
                        IF ( AMAT(INDX,1).EQ.0. ) THEN
                           AMAT(INDX,:) = 0.0
                           AMAT(INDX,1) = 1.0
                           RHS (INDX  ) = 0.0
                        END IF
                     END DO
                  END DO

!     --- lower boundary (IY=1)
                  DO IX = 2, MXC-1
                     INDX  = KGRPNT(IX  ,1)
                     INDXL = KGRPNT(IX-1,1)
                     INDXR = KGRPNT(IX+1,1)
                     INDXU = KGRPNT(IX  ,2)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
                           JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
                           JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           DEPF = DEPTH(INDX)+DEPTH(INDXU)
                           JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                           &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
                        AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,2)
                        AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,2)
                        AMAT(INDX,2) = 0.
                        AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
                        AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
                        AMAT(INDX,3) = 0.
                        AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,4)
                        AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,4)
                        AMAT(INDX,4) = 0.
                     END IF
                     IF ( AMAT(INDX,1).EQ.0. ) THEN
                        AMAT(INDX,:) = 0.0
                        AMAT(INDX,1) = 1.0
                        RHS (INDX  ) = 0.0
                     END IF
                  END DO

!     --- upper boundary (IY=MYC)
                  DO IX = 2, MXC-1
                     INDX  = KGRPNT(IX  ,MYC  )
                     INDXL = KGRPNT(IX-1,MYC  )
                     INDXR = KGRPNT(IX+1,MYC  )
                     INDXB = KGRPNT(IX  ,MYC-1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXR))
                           JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDXL)+DEPTH(INDX))
                           JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           DEPF = DEPTH(INDXB)+DEPTH(INDX)
                           JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                           &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                        END IF
                        AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,7)
                        AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,7)
                        AMAT(INDX,7) = 0.
                        AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
                        AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
                        AMAT(INDX,8) = 0.
                        AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,9)
                        AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,9)
                        AMAT(INDX,9) = 0.
                     END IF
                     IF ( AMAT(INDX,1).EQ.0. ) THEN
                        AMAT(INDX,:) = 0.0
                        AMAT(INDX,1) = 1.0
                        RHS (INDX  ) = 0.0
                     END IF
                  END DO

!     --- left boundary (IX=1)
                  DO IY = 2, MYC-1
                     INDX  = KGRPNT(1,IY  )
                     INDXR = KGRPNT(2,IY  )
                     INDXB = KGRPNT(1,IY-1)
                     INDXU = KGRPNT(1,IY+1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of right flux
                        IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                           DEPF = DEPTH(INDX)+DEPTH(INDXR)
                           JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                           &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
                           JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                           &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
                           JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                           &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                        END IF
                        AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,2)
                        AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,2)
                        AMAT(INDX,2) = 0.
                        AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
                        AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
                        AMAT(INDX,5) = 0.
                        AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,7)
                        AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,7)
                        AMAT(INDX,7) = 0.
                     END IF
                     IF ( AMAT(INDX,1).EQ.0. ) THEN
                        AMAT(INDX,:) = 0.0
                        AMAT(INDX,1) = 1.0
                        RHS (INDX  ) = 0.0
                     END IF
                  END DO

!     --- right boundary (IX=MXC)
                  DO IY = 2, MYC-1
                     INDX  = KGRPNT(MXC  ,IY  )
                     INDXL = KGRPNT(MXC-1,IY  )
                     INDXB = KGRPNT(MXC  ,IY-1)
                     INDXU = KGRPNT(MXC  ,IY+1)
                     IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!           --- contribution of left flux
                        IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                           DEPF = DEPTH(INDXL)+DEPTH(INDX)
                           JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                           &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                        END IF
!           --- contribution of upper flux
                        IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDX)+DEPTH(INDXU))
                           JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                           &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                           AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                           CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                           &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                           AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                           AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                        END IF
!           --- contribution of bottom flux
                        IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                           DEPF = 0.5*(DEPTH(INDXB)+DEPTH(INDX))
                           JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                           &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                           IF ( JAC.NE.0. ) THEN
                              FACT = -DEPF/JAC
                           ELSE
                              FACT = 0.
                           END IF
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                           AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                           AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                           CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                           &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                           AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                           AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                           AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                           AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                        END IF
                        AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,4)
                        AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,4)
                        AMAT(INDX,4) = 0.
                        AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
                        AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
                        AMAT(INDX,6) = 0.
                        AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,9)
                        AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,9)
                        AMAT(INDX,9) = 0.
                     END IF
                     IF ( AMAT(INDX,1).EQ.0. ) THEN
                        AMAT(INDX,:) = 0.0
                        AMAT(INDX,1) = 1.0
                        RHS (INDX  ) = 0.0
                     END IF
                  END DO

!     --- left-lower corner (IX=1,IY=1)
                  INDX  = KGRPNT(1,1)
                  INDXR = KGRPNT(2,1)
                  INDXU = KGRPNT(1,2)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
                     IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDX)+DEPTH(INDXR)
                        JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                        AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                        CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                        AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                        AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                        AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                     END IF
!        --- contribution of upper flux
                     IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDX)+DEPTH(INDXU)
                        JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                        &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                        &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                        AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                        CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                        &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                        AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                        AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                        AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                     END IF
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
                     AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
                     AMAT(INDX,3) = 0.
                     AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,4)
                     AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,4)
                     AMAT(INDX,4) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
                     AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
                     AMAT(INDX,5) = 0.
                     AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,7)
                     AMAT(INDX,9) = AMAT(INDX,9) - AMAT(INDX,7)
                     AMAT(INDX,7) = 0.
                  END IF
                  IF ( AMAT(INDX,1).EQ.0. ) THEN
                     AMAT(INDX,:) = 0.0
                     AMAT(INDX,1) = 1.0
                     RHS (INDX  ) = 0.0
                  END IF

!     --- right-lower corner (IX=MXC,IY=1)
                  INDX  = KGRPNT(MXC  ,1)
                  INDXL = KGRPNT(MXC-1,1)
                  INDXU = KGRPNT(MXC  ,2)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
                     IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDXL)+DEPTH(INDX)
                        JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                        AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                        AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                        CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                        AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                        AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                        AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                     END IF
!        --- contribution of upper flux
                     IF ( INDXU.GT.1 .AND. DEPTH(INDXU).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDX)+DEPTH(INDXU)
                        JAC  = JCTA(INDX,2,1,1)*JCTA(INDX,2,2,2) -&
                        &JCTA(INDX,2,1,2)*JCTA(INDX,2,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,2,1)+&
                        &JCTA(INDX,2,2,2)*JCTA(INDX,2,2,2))
                        AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + CONTRB
                        CONTRB = FACT*(JCTA(INDX,2,2,1)*JCTA(INDX,2,1,1)+&
                        &JCTA(INDX,2,2,2)*JCTA(INDX,2,1,2))
                        AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                        AMAT(INDX,7) = AMAT(INDX,7) - 0.25*CONTRB
                        AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                     END IF
                     AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,2)
                     AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,2)
                     AMAT(INDX,2) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,3)
                     AMAT(INDX,8) = AMAT(INDX,8) - AMAT(INDX,3)
                     AMAT(INDX,3) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
                     AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
                     AMAT(INDX,6) = 0.
                     AMAT(INDX,8) = AMAT(INDX,8) + 2.*AMAT(INDX,9)
                     AMAT(INDX,7) = AMAT(INDX,7) - AMAT(INDX,9)
                     AMAT(INDX,9) = 0.
                  END IF
                  IF ( AMAT(INDX,1).EQ.0. ) THEN
                     AMAT(INDX,:) = 0.0
                     AMAT(INDX,1) = 1.0
                     RHS (INDX  ) = 0.0
                  END IF

!     --- left-upper corner (IX=1,IY=MYC)
                  INDX  = KGRPNT(1,MYC  )
                  INDXR = KGRPNT(2,MYC  )
                  INDXB = KGRPNT(1,MYC-1)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of right flux
                     IF ( INDXR.GT.1 .AND. DEPTH(INDXR).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDX)+DEPTH(INDXR)
                        JAC  = JCTA(INDX,1,1,1)*JCTA(INDX,1,2,2) -&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,1,1)+&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,1,2))
                        AMAT(INDX,1) = AMAT(INDX,1) - CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + CONTRB
                        CONTRB = FACT*(JCTA(INDX,1,1,1)*JCTA(INDX,1,2,1)+&
                        &JCTA(INDX,1,1,2)*JCTA(INDX,1,2,2))
                        AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                        AMAT(INDX,4) = AMAT(INDX,4) - 0.25*CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                        AMAT(INDX,9) = AMAT(INDX,9) + 0.25*CONTRB
                     END IF
!        --- contribution of bottom flux
                     IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDXB)+DEPTH(INDX)
                        JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                        &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                        &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                        AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                        AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                        CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                        &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                        AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                        AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                        AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                     END IF
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
                     AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
                     AMAT(INDX,8) = 0.
                     AMAT(INDX,6) = AMAT(INDX,6) + 2.*AMAT(INDX,9)
                     AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,9)
                     AMAT(INDX,9) = 0.
                     AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,2)
                     AMAT(INDX,4) = AMAT(INDX,4) - AMAT(INDX,2)
                     AMAT(INDX,2) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,5)
                     AMAT(INDX,6) = AMAT(INDX,6) - AMAT(INDX,5)
                     AMAT(INDX,5) = 0.
                  END IF
                  IF ( AMAT(INDX,1).EQ.0. ) THEN
                     AMAT(INDX,:) = 0.0
                     AMAT(INDX,1) = 1.0
                     RHS (INDX  ) = 0.0
                  END IF

!     --- right-upper corner (IX=MXC,IY=MYC)
                  INDX  = KGRPNT(MXC  ,MYC  )
                  INDXL = KGRPNT(MXC-1,MYC  )
                  INDXB = KGRPNT(MXC  ,MYC-1)
                  IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
!        --- contribution of left flux
                     IF ( INDXL.GT.1 .AND. DEPTH(INDXL).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDXL)+DEPTH(INDX)
                        JAC  = JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,2) -&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,1,1)+&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,1,2))
                        AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                        AMAT(INDX,5) = AMAT(INDX,5) - CONTRB
                        CONTRB = -FACT*(JCTA(INDXL,1,1,1)*JCTA(INDXL,1,2,1)+&
                        &JCTA(INDXL,1,1,2)*JCTA(INDXL,1,2,2))
                        AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                        AMAT(INDX,3) = AMAT(INDX,3) - 0.25*CONTRB
                        AMAT(INDX,7) = AMAT(INDX,7) + 0.25*CONTRB
                        AMAT(INDX,8) = AMAT(INDX,8) + 0.25*CONTRB
                     END IF
!        --- contribution of bottom flux
                     IF ( INDXB.GT.1 .AND. DEPTH(INDXB).GT.DEPMIN ) THEN
                        DEPF = DEPTH(INDXB)+DEPTH(INDX)
                        JAC  = JCTA(INDXB,2,1,1)*JCTA(INDXB,2,2,2) -&
                        &JCTA(INDXB,2,1,2)*JCTA(INDXB,2,2,1)
                        IF ( JAC.NE.0. ) THEN
                           FACT = -DEPF/JAC
                        ELSE
                           FACT = 0.
                        END IF
                        CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,2,1)+&
                        &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,2,2))
                        AMAT(INDX,1) = AMAT(INDX,1) + CONTRB
                        AMAT(INDX,3) = AMAT(INDX,3) - CONTRB
                        CONTRB = -FACT*(JCTA(INDXB,2,2,1)*JCTA(INDXB,2,1,1)+&
                        &JCTA(INDXB,2,2,2)*JCTA(INDXB,2,1,2))
                        AMAT(INDX,2) = AMAT(INDX,2) - 0.25*CONTRB
                        AMAT(INDX,4) = AMAT(INDX,4) + 0.25*CONTRB
                        AMAT(INDX,5) = AMAT(INDX,5) - 0.25*CONTRB
                        AMAT(INDX,6) = AMAT(INDX,6) + 0.25*CONTRB
                     END IF
                     AMAT(INDX,5) = AMAT(INDX,5) + 2.*AMAT(INDX,7)
                     AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,7)
                     AMAT(INDX,7) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,8)
                     AMAT(INDX,3) = AMAT(INDX,3) - AMAT(INDX,8)
                     AMAT(INDX,8) = 0.
                     AMAT(INDX,3) = AMAT(INDX,3) + 2.*AMAT(INDX,4)
                     AMAT(INDX,2) = AMAT(INDX,2) - AMAT(INDX,4)
                     AMAT(INDX,4) = 0.
                     AMAT(INDX,1) = AMAT(INDX,1) + 2.*AMAT(INDX,6)
                     AMAT(INDX,5) = AMAT(INDX,5) - AMAT(INDX,6)
                     AMAT(INDX,6) = 0.
                  END IF
                  IF ( AMAT(INDX,1).EQ.0. ) THEN
                     AMAT(INDX,:) = 0.0
                     AMAT(INDX,1) = 1.0
                     RHS (INDX  ) = 0.0
                  END IF

!     --- in case of nesting put Dirichlet boundary condition

                  IF ( LSETUP.EQ.2 ) THEN
!        --- left and right boundaries
                     DO IY = 1, MYC
                        DO IX = 1, MXC, MXC-1
                           INDX  = KGRPNT(IX,IY)
                           AMAT(INDX,:) = 0.0
                           AMAT(INDX,1) = 1.0
                           IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
                              RHS(INDX) = SETUP(INDX)
                           ELSE
                              RHS(INDX) = 0.0
                           END IF
                        END DO
                     END DO
!        --- lower and upper boundaries
                     DO IY = 1, MYC, MYC-1
                        DO IX = 1, MXC
                           INDX  = KGRPNT(IX,IY)
                           AMAT(INDX,:) = 0.0
                           AMAT(INDX,1) = 1.0
                           IF ( INDX.GT.1 .AND. DEPTH(INDX).GT.DEPMIN ) THEN
                              RHS(INDX) = SETUP(INDX)
                           ELSE
                              RHS(INDX) = 0.0
                           END IF
                        END DO
                     END DO
                  END IF

!     --- set parameters for the solver

                  REPS   = PNUMS(23)
                  IAMOUT = INT(PNUMS(24))
                  MAXIT  = INT(PNUMS(25))

                  CSETUP = .TRUE.

!     --- determine relaxation factor

                  IF ( RELAX.EQ.-1. ) THEN
                     IF ( MCGRD.LT.100 ) THEN
                        RHOV = 1. - 1./REAL(MCGRD)
                     ELSE IF ( MCGRD.LT.1000 ) THEN
                        RHOV = 1. - 3./REAL(MCGRD)
                     ELSE
                        RHOV = 1. - 10./REAL(MCGRD)
                     END IF
                     XOMEG = 2./(1.+SQRT(1.-RHOV*RHOV))
                  ELSE
                     XOMEG = RELAX
                  END IF

                  IT    = 0
                  ICONV = 0
                  RESM  = 1.
                  XOM   = 1.

!     --- solve the system of equations

                  setup_iterations: DO WHILE ( ICONV.EQ.0 .AND. IT.LT.MAXIT )

                     IT    = IT + 1
                     ICONV = 1

                     RESMO = RESM
                     RESM  = 0.
                     IXINF = 0
                     IYINF = 0

!        --- interior domain
                     DO IY = 2, MYC-1
                        DO IX = 2, MXC-1
                           INDX   = KGRPNT(IX  ,IY  )
                           INDXL  = KGRPNT(IX-1,IY  )
                           INDXR  = KGRPNT(IX+1,IY  )
                           INDXB  = KGRPNT(IX  ,IY-1)
                           INDXU  = KGRPNT(IX  ,IY+1)
                           INDXLB = KGRPNT(IX-1,IY-1)
                           INDXRB = KGRPNT(IX+1,IY-1)
                           INDXLU = KGRPNT(IX-1,IY+1)
                           INDXRU = KGRPNT(IX+1,IY+1)

                           SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB)&
                           &- AMAT(INDX,3)*SETUP(INDXB )&
                           &- AMAT(INDX,4)*SETUP(INDXRB)&
                           &- AMAT(INDX,5)*SETUP(INDXL )&
                           &- AMAT(INDX,6)*SETUP(INDXR )&
                           &- AMAT(INDX,7)*SETUP(INDXLU)&
                           &- AMAT(INDX,8)*SETUP(INDXU )&
                           &- AMAT(INDX,9)*SETUP(INDXRU)
                           SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                           RES = ABS(SETUP(INDX) - SETPI)
                           IF ( RES.GT.RESM ) THEN
                              RESM  = RES
                              IXINF = IX
                              IYINF = IY
                           END IF
                           SETUP(INDX) = SETPI

                        END DO
                     END DO

!        --- lower boundary (IY=1)
                     DO IX = 2, MXC-1
                        INDX   = KGRPNT(IX  ,1)
                        INDXL  = KGRPNT(IX-1,1)
                        INDXR  = KGRPNT(IX+1,1)
                        INDXU  = KGRPNT(IX  ,2)
                        INDXLU = KGRPNT(IX-1,2)
                        INDXRU = KGRPNT(IX+1,2)

                        SETPI = RHS(INDX) - AMAT(INDX,5)*SETUP(INDXL )&
                        &- AMAT(INDX,6)*SETUP(INDXR )&
                        &- AMAT(INDX,7)*SETUP(INDXLU)&
                        &- AMAT(INDX,8)*SETUP(INDXU )&
                        &- AMAT(INDX,9)*SETUP(INDXRU)
                        SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                        RES = ABS(SETUP(INDX) - SETPI)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IXINF = IX
                           IYINF = 1
                        END IF
                        SETUP(INDX) = SETPI

                     END DO

!        --- upper boundary (IY=MYC)
                     DO IX = 2, MXC-1
                        INDX   = KGRPNT(IX  ,MYC  )
                        INDXL  = KGRPNT(IX-1,MYC  )
                        INDXR  = KGRPNT(IX+1,MYC  )
                        INDXB  = KGRPNT(IX  ,MYC-1)
                        INDXLB = KGRPNT(IX-1,MYC-1)
                        INDXRB = KGRPNT(IX+1,MYC-1)

                        SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB)&
                        &- AMAT(INDX,3)*SETUP(INDXB )&
                        &- AMAT(INDX,4)*SETUP(INDXRB)&
                        &- AMAT(INDX,5)*SETUP(INDXL )&
                        &- AMAT(INDX,6)*SETUP(INDXR )
                        SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                        RES = ABS(SETUP(INDX) - SETPI)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IXINF = IX
                           IYINF = MYC
                        END IF
                        SETUP(INDX) = SETPI

                     END DO

!        --- left boundary (IX=1)
                     DO IY = 2, MYC-1
                        INDX   = KGRPNT(1,IY  )
                        INDXR  = KGRPNT(2,IY  )
                        INDXB  = KGRPNT(1,IY-1)
                        INDXU  = KGRPNT(1,IY+1)
                        INDXRB = KGRPNT(2,IY-1)
                        INDXRU = KGRPNT(2,IY+1)

                        SETPI = RHS(INDX) - AMAT(INDX,3)*SETUP(INDXB )&
                        &- AMAT(INDX,4)*SETUP(INDXRB)&
                        &- AMAT(INDX,6)*SETUP(INDXR )&
                        &- AMAT(INDX,8)*SETUP(INDXU )&
                        &- AMAT(INDX,9)*SETUP(INDXRU)
                        SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                        RES = ABS(SETUP(INDX) - SETPI)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IXINF = 1
                           IYINF = IY
                        END IF
                        SETUP(INDX) = SETPI

                     END DO

!        --- right boundary (IX=MXC)
                     DO IY = 2, MYC-1
                        INDX   = KGRPNT(MXC  ,IY  )
                        INDXL  = KGRPNT(MXC-1,IY  )
                        INDXB  = KGRPNT(MXC  ,IY-1)
                        INDXU  = KGRPNT(MXC  ,IY+1)
                        INDXLB = KGRPNT(MXC-1,IY-1)
                        INDXLU = KGRPNT(MXC-1,IY+1)

                        SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB)&
                        &- AMAT(INDX,3)*SETUP(INDXB )&
                        &- AMAT(INDX,5)*SETUP(INDXL )&
                        &- AMAT(INDX,7)*SETUP(INDXLU)&
                        &- AMAT(INDX,8)*SETUP(INDXU )
                        SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                        RES = ABS(SETUP(INDX) - SETPI)
                        IF ( RES.GT.RESM ) THEN
                           RESM  = RES
                           IXINF = MXC
                           IYINF = IY
                        END IF
                        SETUP(INDX) = SETPI

                     END DO

!        --- left-lower corner (IX=1,IY=1)
                     INDX   = KGRPNT(1,1)
                     INDXR  = KGRPNT(2,1)
                     INDXU  = KGRPNT(1,2)
                     INDXRU = KGRPNT(2,2)

                     SETPI = RHS(INDX) - AMAT(INDX,6)*SETUP(INDXR )&
                     &- AMAT(INDX,8)*SETUP(INDXU )&
                     &- AMAT(INDX,9)*SETUP(INDXRU)
                     SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                     RES = ABS(SETUP(INDX) - SETPI)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IXINF = 1
                        IYINF = 1
                     END IF
                     SETUP(INDX) = SETPI

!        --- right-lower corner (IX=MXC,IY=1)
                     INDX   = KGRPNT(MXC  ,1)
                     INDXL  = KGRPNT(MXC-1,1)
                     INDXU  = KGRPNT(MXC  ,2)
                     INDXLU = KGRPNT(MXC-1,2)

                     SETPI = RHS(INDX) - AMAT(INDX,5)*SETUP(INDXL )&
                     &- AMAT(INDX,7)*SETUP(INDXLU)&
                     &- AMAT(INDX,8)*SETUP(INDXU )
                     SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                     RES = ABS(SETUP(INDX) - SETPI)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IXINF = MXC
                        IYINF = 1
                     END IF
                     SETUP(INDX) = SETPI

!        --- left-upper corner (IX=1,IY=MYC)
                     INDX   = KGRPNT(1,MYC  )
                     INDXR  = KGRPNT(2,MYC  )
                     INDXB  = KGRPNT(1,MYC-1)
                     INDXRB = KGRPNT(2,MYC-1)

                     SETPI = RHS(INDX) - AMAT(INDX,3)*SETUP(INDXB )&
                     &- AMAT(INDX,4)*SETUP(INDXRB)&
                     &- AMAT(INDX,6)*SETUP(INDXR )
                     SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                     RES = ABS(SETUP(INDX) - SETPI)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IXINF = 1
                        IYINF = MYC
                     END IF
                     SETUP(INDX) = SETPI

!        --- right-upper corner (IX=MXC,IY=MYC)
                     INDX   = KGRPNT(MXC  ,MYC  )
                     INDXL  = KGRPNT(MXC-1,MYC  )
                     INDXB  = KGRPNT(MXC  ,MYC-1)
                     INDXLB = KGRPNT(MXC-1,MYC-1)

                     SETPI = RHS(INDX) - AMAT(INDX,2)*SETUP(INDXLB)&
                     &- AMAT(INDX,3)*SETUP(INDXB )&
                     &- AMAT(INDX,5)*SETUP(INDXL )
                     SETPI = XOM*SETPI/AMAT(INDX,1) + (1.-XOM)*SETUP(INDX)

                     RES = ABS(SETUP(INDX) - SETPI)
                     IF ( RES.GT.RESM ) THEN
                        RESM  = RES
                        IXINF = MXC
                        IYINF = MYC
                     END IF
                     SETUP(INDX) = SETPI

                     IF ( RESM.GT.1.E8 ) THEN
                        IT = MAXIT + 1
                        ICONV = 0
                        CYCLE setup_iterations
                     END IF
                     IF ( IAMOUT.EQ.2 ) THEN
                        WRITE (PRINTF,'(A,I3,A,E12.6,A,I3,A,I3,A)')&
                        &' ++ SETUP2D: iter = ',IT,'    res = ',RESM,&
                        &' in (IX,IY) = (',IXINF,',',IYINF,')'
                     END IF
                     IF ( IAMOUT.EQ.3 .AND. IT.EQ.1 ) RESM0 = RESM

                     IF ( RESM.GT.REPS ) THEN
                        IF ( XOM.NE.1. ) THEN
                           II = II + 1
                           IF ( II.EQ.0 ) RESMI = RESM
                           IF ( II.GT.0 .AND.&
                           &RESM.GT.10.*RESMI*(XOMEG-1.)**II ) XOM = 1.
                        ELSE
                           XL = RESM/RESMO
                           IF ( XL.GT.0.9*RHOV*RHOV ) THEN
                              XOM = XOMEG
                              II  = -10
                           END IF
                        END IF
                        ICONV = 0
                     END IF

                  END DO setup_iterations

!     --- investigate the reason to stop

                  IF ( ICONV.EQ.0 ) THEN
                     CSETUP = .FALSE.
                     IF ( RESM.GT.1.E8 ) SETUP = 0.
                  END IF
                  IF ( ICONV.EQ.0 .AND. IAMOUT.GE.1 ) THEN
                     WRITE (PRINTF,'(A)')&
                     &' ++ SETUP2D: no convergence for 2D Poisson equation'
                     WRITE (PRINTF,'(A,I4)')&
                     &'             total number of iterations       = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'             inf-norm of the residual         = ',RESM
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'             required accuracy                = ',REPS
                  ELSE IF ( IAMOUT.EQ.3 ) THEN
                     WRITE (PRINTF,'(A,E12.6)')&
                     &' ++ SETUP2D: inf-norm of the initial residual = ',RESM0
                     WRITE (PRINTF,'(A,I4)')&
                     &'             total number of iterations       = ',IT
                     WRITE (PRINTF,'(A,E12.6)')&
                     &'             inf-norm of the residual         = ',RESM
                  END IF

                  RETURN
               end subroutine SETUP2D

end module swan_computation
