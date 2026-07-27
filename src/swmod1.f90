!                 COMMON VARIABLES RELATED MODULES, file 1 of 3
!
!     Contents of this file
!
!     OCPCOMM2 is gone: project identification and the path separators moved to
!     swan_project_metadata and swan_path_separators.
!     OCPCOMM3 is gone: its five variables were local to one output routine
!     and to the version string, so they became locals instead of shared state.
!     OCPCOMM4 is gone: the unit numbers became swan_io_units and the test,
!     trace and error dials swan_diagnostics_level. ITMOPT went to swan_time,
!     INAN and RNAN to swan_number_formatting next to the NUMSTR they are
!     arguments of, and IMPORT was dead.
!     SWCOMM1 is gone: the output variable table, output frame, unit names,
!     output settings and CHTIME each moved to a module of their own.
!     SWCOMM2 is gone: it was mostly the eighteen input grids, which are now
!     swan_input_grids and swan_input_field_files. The coordinate offset, the
!     computational grid kind, the boundary counters and the run mode each got
!     a module of their own, because that is what the importers wanted.
!     SWCOMM3            contains common variables for SWAN
!     SWCOMM4 is gone: the test-output status, the propagation scheme and the
!     shape of the earth became swan_test_output, swan_propagation_scheme and
!     swan_spherical_geometry; UNDFLW and MAXMES were eliminated.






MODULE SWCOMM3


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
!     41.75: Erick Rogers
!
!  1. Updates
!
!     40.41, Oct. 04: taken from the include file SWCOMM3.INC
!     41.75, Jan. 19: adding sea ice
!
!  2. Purpose
!
!     Common variables used by the subroutines in SWAN
!
!  3. Method
!
!     MODULE construct
!
!  4. Modules used
!
!     ---

   IMPLICIT NONE(TYPE, EXTERNAL)

!  5. Argument variables
!
!     ---
!
!  6. Parameter variables
!
!     MBOT   [  10] dimension of array PBOT
!     MDIFFR [  10] dimension of array PDIFFR
!     MDISP  [   8] dimension for dissipation arrays
!     MGENR  [   1] dimension for generation arrays
!     MICMAX [  13] max. number of points in comput. stencil
!     MMUD   [  10] dimension of array PMUD
!     MNUMS  [  40] dimension of array PNUMS
!     MQUAD  [  10] dimension of array PQUAD
!     MREDS  [   4] dimension for redistribution arrays
!     MSETUP [   2] dimension of array PSETUP
!     MSHAPE [   5] dimension of array PSHAPE
!     MSPPAR [   5] dimension of array SPPARM
!     MSURF  [  20] dimension of array PSURF
!     MTRNP  [   3] dimension for propagation arrays
!     MTURBV [   5] dimension of array PTURBV
!     MTRIAD [  10] dimension of array PTRIAD
!     MWCAP  [  15] dimension of array PWCAP
!     MWIND  [  40] dimension of array PWIND
!     MSICE  [   8] dimension of array PSICE
!     MICE   [   2] dimension of array PICE
!     MBRAG  [   5] dimension of array PBRAG
!     MSCAT  [  10] dimension of array PSCAT

   INTEGER, PARAMETER :: MBOT=10, MNUMS=40, MQUAD=10
   INTEGER, PARAMETER :: MSETUP=2, MSHAPE=5, MSPPAR=5, MSURF=20
   INTEGER, PARAMETER :: MTRIAD=10, MWCAP=15, MWIND=40, MTURBV=5
   INTEGER, PARAMETER :: MSICE=8, MICE=2, MICMAX=13, MDIFFR=10
   INTEGER, PARAMETER :: MMUD=10, MDISP=8, MGENR=1, MREDS=4, MTRNP=3
   INTEGER, PARAMETER :: MBRAG=5, MSCAT=10

!  7. Local variables
!
!     *** pointers for data arrays on computational grid ***
!
! JABIN  [  1] within array LSWMAT
! JABLK  [  2] within array LSWMAT
! JAOLD  [ 8+2*MDISP] within array SWMATR
! JASTD2 [  1] new air-sea temp. diff. within array COMPDA
! JASTD3 [  1] last read air-sea temp. diff. within array COMPDA
! JBIPH  [ 29] parametrized biphase
! JBOTLV [ 28] bottom level within array COMPDA
! JCDRAG [  1] drag coefficient within array COMPDA,
!              set by command WCAP JANS ...
! JDHS   [  6] wave height correction within array COMPDA
!              (difference in Hs between last two iterations)
! JDIS0  [  7] within array SWMATR
! JDIS1  [  7+MDISP] within array SWMATR
! JDISS  [  2] total dissipation within array COMPDA
! JDPSAV [  1] saved depth (for setup) within array COMPDA
! JDP1   [  7] old depth within array COMPDA
! JDP2   [  8] new depth within array COMPDA
! JDP3   [ 15] last read depth within array COMPDA
! JDSS2  [   ] new sea-swell Dir within array COMPDA
! JDSS3  [   ] last read sea-swell Dir within array COMPDA
! JDSXB  [  1] bottom friction dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXL  [  1] swell dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXM  [  1] fluid mud dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXS  [  1] surf dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXV  [  1] vegetation dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXT  [  1] turbulent dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDSXI  [  1] dissipation by sea ice within array COMPDA
!              set by command TABLE/BLOCK
! JDSXW  [  1] whitecapping dissipation within array COMPDA
!              set by command TABLE/BLOCK
! JDTM   [ 20] wave period correction within array COMPDA
!              (difference in average wave period between last two iterations)
! JFRC2  [  1] friction coefficient within array COMPDA
!              set by command READ FR ...
! JFRC3  [  1] friction coefficient within array COMPDA
!              set by command READ FR ...
! JGAMMA [  1] breaker index within array COMPDA
!              used with command BREAK BKD
! JGEN0  [   ] within array SWMATR
! JGEN1  [   ] within array SWMATR
! JGENR  [  1] total generation within array COMPDA
!              set by command TABLE/BLOCK
! JGSXW  [  1] wind input within array COMPDA
!              set by command TABLE/BLOCK
! JHS    [  1] significant wave height Hs within array COMPDA
! JHSIBC [ 25] significant wave height from boundary condition in array
! JHSS2  [   ] new sea-swell Hs within array COMPDA
! JHSS3  [   ] last read sea-swell Hs within array COMPDA
! JLEAK  [ 21] "leak" within array COMPDA
!              (refractive energy tranport over sector boundaries)
! JLEK1  [  7+2*MDISP] within array SWMATR
! JMAT5  [  5] within array SWMATR
! JMAT6  [  6] within array SWMATR
! JMATD  [  1] within array SWMATR
! JMATL  [  3] within array SWMATR
! JMATR  [  2] within array SWMATR
! JMATU  [  4] within array SWMATR
! JMUDL1 [   ] old fluid mud layer within array COMPDA
! JMUDL2 [   ] new fluid mud layer within array COMPDA
! JMUDL3 [   ] last read fluid mud layer within array COMPDA
! JAICE2 [   ] new ice fraction within array COMPDA
! JAICE3 [   ] last read ice fraction within array COMPDA
! JHICE2 [   ] new ice thickness within array COMPDA
! JHICE3 [   ] last read ice thickness within array COMPDA
! JNPLA2 [   ] new number of plants / m2 within array COMPDA
! JNPLA3 [   ] last read number of plants / m2 within array COMPDA
! JTSS2  [   ] new sea-swell Tm within array COMPDA
! JTSS3  [   ] last read sea-swell Tm within array COMPDA
! JTURB2 [   ] new turbulent viscosity within array COMPDA
! JTURB3 [   ] last read turbulent viscosity within array COMPDA
! JP4D   [  7] within array SWTSDA, quadruplet interactions (implicit part)
! JP4S   [  6] within array SWTSDA, quadruplet interactions (explicit part)
! JPBOT  [  1] bottom wave period within array COMPDA
!              set by command TABLE/BLOCK
! JPBTFR [  4] within array SWTSDA, bottom friction
! JPTRI  [  8] within array SWTSDA, triad interactions
! JPVEGT [  9] within array SWTSDA, vegetation dissipation
! JPTURB [ 10] within array SWTSDA, turbulent dissipation
! JPMUD  [ 11] within array SWTSDA, fluid mud dissipation
! JPICE  [ 13] within array SWTSDA, dissipation by sea ice
! JPBRAG [ 14] within array SWTSDA, Bragg scattering
! JPQCS  [ 15] within array SWTSDA, QC scattering
! JPWBRK [  5] within array SWTSDA, surf breaking
! JPWCAP [  3] within array SWTSDA, white capping
! JPWNDD [  2] within array SWTSDA, wind input term (implicit part)
! JPWNDS [  1] within array SWTSDA, wind input term (explicit part)
! JPSWEL [ 12] within array SWTSDA, swell dissipation
! JQB    [  4] fraction of breaking waves within array COMPDA
! JRADS  [  1] radiation stress within array COMPDA
!              set by command TABLE/BLOCK
! JRED0  [   ] within array SWMATR
! JRED1  [   ] within array SWMATR
! JREDS  [  1] total redistribution within array COMPDA
!              set by command TABLE/BLOCK
! JRSXQ  [  1] quadruplets within array COMPDA
!              set by command TABLE/BLOCK
! JRSXT  [  1] triads within array COMPDA
!              set by command TABLE/BLOCK
! JRSXB  [  1] Bragg scattering within array COMPDA
!              set by command TABLE/BLOCK
! JRSXC  [  1] QC scattering within array COMPDA
!              set by command TABLE/BLOCK
! JSETUP [  1] setup values within array COMPDA
! JSTP   [  5] steepness within array COMPDA
! JTAUW  [  1] TauW within array COMPDA,
!              set by command WCAP JANS ...
! JTRA0  [   ] within array SWMATR
! JTRA1  [   ] within array SWMATR
! JTRAN  [  1] total propagation within array COMPDA
!              set by command TABLE/BLOCK
! JTSXG  [  1] xy-propagation within array COMPDA
!              set by command TABLE/BLOCK
! JTSXT  [  1] theta-propagation within array COMPDA
!              set by command TABLE/BLOCK
! JTSXS  [  1] sigma-propagation within array COMPDA
!              set by command TABLE/BLOCK
! JUBOT  [  3] bottom orbital velocity within array COMPDA
! JURSEL [ 27] Ursell number as used in triad computation
! JUSTAR [  1] friction velocity within array COMPDA,
!              set by command WCAP JANS ...
! JVX1   [  9] x of old current velocity within array COMPDA
! JVX2   [ 11] x of new current velocity within array COMPDA
! JVX3   [ 13] x of last read current velocity within array COMPDA
! JVY1   [ 10] y of old current velocity within array COMPDA
! JVY2   [ 12] y of new current velocity within array COMPDA
! JVY3   [ 14] y of last read current velocity within array COMPDA
! JWLV1  [ 22] old water level within array COMPDA
! JWLV2  [ 24] new water level within array COMPDA
! JWLV3  [ 23] last read water level within array COMPDA
! JWX2   [ 16] x of new wind velocity within array COMPDA
! JWX3   [ 18] x of last read wind velocity within array COMPDA
! JWY2   [ 17] y of new wind velocity within array COMPDA
! JWY3   [ 19] y of last read wind velocity within array COMPDA
! JZEL   [  1] roughness within array COMPDA,
!              set by command WCAP JANS ...
! MCMVAR [ 29] within array COMPDA,
!              =MCMVAR+2, for command READ FR ... (add JFRC2, JFRC3)
!              =MCMVAR+4, for command WCAP JANS ... (add JCDRAG, JTAUW,
!              =MCMVAR+1, for command BREAKING BKD (add JGAMMA)
!              =MCMVAR+1, for command TABLE/BLOCK TMBOT (add JPBOT)
!              =MCMVAR+1, for command TABLE/BLOCK DISB (add JDSXB)
!              =MCMVAR+1, for command TABLE/BLOCK DISSU (add JDSXS)
!              =MCMVAR+1, for command TABLE/BLOCK DISW (add JDSXW)
!              =MCMVAR+1, for command TABLE/BLOCK DISM (add JDSXM)
!              =MCMVAR+1, for command TABLE/BLOCK DISV (add JDSXV)
!              =MCMVAR+1, for command TABLE/BLOCK DISTU (add JDSXT)
!              =MCMVAR+1, for command TABLE/BLOCK DISIC (add JDSXI)
!              =MCMVAR+1, for command TABLE/BLOCK DISSL (add JDSXL)
!              =MCMVAR+1, for command TABLE/BLOCK GENE (add JGENR)
!              =MCMVAR+1, for command TABLE/BLOCK GENW (add JGSXW)
!              =MCMVAR+1, for command TABLE/BLOCK REDI (add JREDS)
!              =MCMVAR+1, for command TABLE/BLOCK REDQ (add JRSXQ)
!              =MCMVAR+1, for command TABLE/BLOCK REDT (add JRSXT)
!              =MCMVAR+1, for command TABLE/BLOCK REDB (add JRSXB)
!              =MCMVAR+1, for command TABLE/BLOCK REDC (add JRSXC)
!              =MCMVAR+1, for command TABLE/BLOCK PROPA (add JTRAN)
!              =MCMVAR+1, for command TABLE/BLOCK PROPX (add JTSXG)
!              =MCMVAR+1, for command TABLE/BLOCK PROPT (add JTSXT)
!              =MCMVAR+1, for command TABLE/BLOCK PROPS (add JTSXS)
!              =MCMVAR+1, for command TABLE/BLOCK RADST (add JRADS)
! MLSWMAT [  2] within array LSWMAT
! MSWMATR [ 16] within array SWMATR
! MTSVAR  [ 15] within array TESTDA

   INTEGER             JASTD2,      JASTD3,      JGAMMA
   INTEGER             JCDRAG, JDHS
   INTEGER             JDISS, JDPSAV, JDP1
   INTEGER             JDP2
   INTEGER             JDTM, JFRC2, JFRC3
   INTEGER             JDSXB
   INTEGER             JDSXL
   INTEGER             JDSXS
   INTEGER             JDSXW
   INTEGER             JDSXM
   INTEGER             JDSXV,       JDSXT
   INTEGER             JGSXW,       JGENR
   INTEGER             JRSXQ,       JRSXT,       JREDS,       JRADS
   INTEGER             JTSXG,       JTSXT,       JTSXS,       JTRAN
   INTEGER             JHS,         JHSIBC,      JLEAK
   INTEGER             JP4D,        JP4S,        JPBTFR,      JPTRI
   INTEGER             JPWBRK,      JPWCAP,      JPWNDD,      JPWNDS
   INTEGER             JPMUD,       JMUDL1,      JMUDL2,      JMUDL3
   INTEGER             JPVEGT,      JNPLA2,      JNPLA3
   INTEGER             JPTURB,      JTURB2,      JTURB3
   INTEGER             JQB, JSETUP, JTAUW
   INTEGER             JUBOT,       JUSTAR,      JVX1,        JVX2
   INTEGER             JVY1, JVY2
   INTEGER             JWLV2
   INTEGER             JWX2,        JWX3
   INTEGER             JWY2,        JWY3,        JZEL  ,      JPBOT
   INTEGER             MCMVAR,                   MTSVAR,      JURSEL
   INTEGER             JBIPH
   INTEGER             JBOTLV
   INTEGER             JPSWEL
   INTEGER             JAICE2,      JAICE3,      JHICE2,      JHICE3
   INTEGER             JPICE,       JDSXI
   INTEGER             JHSS2,       JHSS3,       JTSS2,       JTSS3
   INTEGER             JDSS2,       JDSS3
   INTEGER             JPBRAG,      JRSXB,       JPQCS,       JRSXC

!     *** location and dimensions of computational grid ***
!
! ALCP   [CALCUL] =-ALPC;
!                  direction of user coordinates w.r.t. computational coordinates
! ALOCMP [ FALSE]  if True, array COMPDA must be re-allocated
! ALPC   [    0.]  direction of x-axis of computational grid w.r.t. user coordinates
! COSLAT [    10]  cos of latitude; =1 for Cartesian coordinates
! COSPC  [CALCUL] =COS(ALPC)
! DX     [CALCUL] =XCLEN/MXS; mesh size in x-direction of computational
! DY     [CALCUL] =YCLEN/MYS; mesh size in y-direction of computational
! DDIR   [CALCUL] =(SPDIR2-SPDIR1)/MDC;
!                  mesh size in theta-direction of computational grid
! DXRP   [      ]  unused
! DYRP   [      ]  unused
! FRINTF [CALCUL] =ALOG(SHIG/SLOW)/(MSC-1); frequency integration factor (df/f)
!                  (integral over frequency of G(f) = SUM_j (sigma_j*Gj*FRINTF) )
! FRINTH [CALCUL] =SQRT(SFAC); frequency mesh boundary factor
!                  (mesh in frequency space runs from sigma/FRINTH to sigma*FRINTH)
! FULCIR [  TRUE]  spectral directions cover the full/part of circle
! ICOMP  [     1]  unused
! ILMAX  [CALCUL]  maximum number of layers to be used in vegetation model
! IXCGRD [      ]  IX of points of computational stencil
! IYCGRD [      ]  IY of points of computational stencil
! KCGRD  [      ]  grid address of points of computational stencil
! RDFSIN [   100] reduction factor as function of frequency for wind input term
!                 (will be assigned to LFACTOR in SdsBabanin.f90)
! MCGRD  [     1]  number of wet grid points of the computational grid
! MDC    [     0]  grid points in theta-direction of computational grid
! MDC4MA [CALCUL] =IDHGH; some counter for quadruplet interactions. Stored in WWINT(18)
! MDC4MI [CALCUL] =IDLOW; some counter for quadruplet interactions. Stored in WWINT(17)
! MMCGR  [CALCUL] =MXC*MYC; grid points in computational grid
! MSC    [     0]  grid points in sigma-direction of computational grid
! MSC4MA [CALCUL] =ISHGH; some counter for quadruplet interactions. Stored in WWINT(16)
! MSC4MI [CALCUL] =ISLOW; some counter for quadruplet interactions. Stored in WWINT(15)
! MTC    [     1]  computational time steps
! MXC    [     0]  grid points in x-direction of computational grid
! MYC    [     0]  grid points in y-direction of computational grid
! NGRBND [CALCUL]  number of grid points on computational grid boundary
! NX     [CALCUL] =MXC-1; only used locally. Equal to MXS
! NY     [CALCUL] =MYC-1; only used locally. Equal to MYS
! SHIG   [CALCUL] =2*PI*FRHIG; highest spectral value of sigma
! SINPC  [CALCUL] =SIN(ALPC)
! SLOW   [CALCUL] =2*PI*FRLOW; lowest spectral value of sigma
! SPDIR1 [    0.]  represents first spectral direction (if FULCIR=.FALSE.)
! SPDIR2 [      ]  represents second spectral direction (if FULCIR=.FALSE.)
! XCGMAX [CALCUL]  maximum x-coordinate of computational grid points
! XCGMIN [CALCUL]  minimum x-coordinate of computational grid points
! XCLEN  [      ]  length of computational grid in x-direction
! XCP    [CALCUL] =-XPC*COSPC-YPC*SINPC;
!                  origin of user coordinates w.r.t. computational coordinates
! XPC    [    0.]  x coordinate of origin of computational grid
! YCGMAX [CALCUL]  maximum y-coordinate of computational grid points
! YCGMIN [CALCUL]  minimum y-coordinate of computational grid points
! YCLEN  [      ]  length of computational grid in y-direction
! YCP    [CALCUL] =XPC*SINPC-YPC*COSPC;
!                  origin of user coordinates w.r.t. computational coordinates
! YPC    [    0.]  y coordinate of origin of computational grid

   INTEGER             IXCGRD(MICMAX), IYCGRD(MICMAX), KCGRD(MICMAX)
   INTEGER             MCGRD
   INTEGER             MDC, MDC4MA, MDC4MI
   INTEGER             MSC,         MSC4MA,      MSC4MI,      MTC
   INTEGER             MXC,         MYC,         NX,          NY
   INTEGER             NGRBND
   INTEGER             ILMAX
   REAL                COSLAT(MICMAX)
   REAL                RDFSIN(100)
   REAL                ALPC, COSPC, DDIR
   REAL                DX, DY
   REAL                FRINTF,      FRINTH,      SHIG,        SINPC
   REAL                SLOW,        SPDIR1,      SPDIR2,      XCLEN
   REAL                XCP,         XPC,         YCLEN,       YCP
   REAL                YPC
   REAL                XCGMIN,      XCGMAX,      YCGMIN,      YCGMAX
   LOGICAL             FULCIR
   LOGICAL ::          ALOCMP = .FALSE.
!$OMP THREADPRIVATE(IXCGRD,IYCGRD,KCGRD,COSLAT)
!$OMP THREADPRIVATE(RDFSIN)
!
!     *** physical parameters ***
!
! CASTD  [    0.] (constant) air-sea temperature difference
! CDCAP  [99999.] maximum drag coefficient
! DEGRAD [CALCUL] =PI/180; constant to transform degrees to radians
! DNORTH [   90.] direction of the North w.r.t. x-axis of user coordinates
!                 =nor; set by command SET ... [nor] ...
! GRAV   [  9.81] acceleration due to gravity
!                 =grav; set by command SET ... [grav] ...
! PI     [3.1415] circular constant
! PI2    [CALCUL] =2*PI;
! PWTAIL(10)      coefficients to calculate tail of the spectrum
!    (1) [    4.] tail power of energy density spectrum as function of freq.
!                 =5.; for command GEN3 JANS ...
!                 =5.; for command WCAP JANS ...,
!                      not documented in manual
!                 =5.; for command GROWTH G3 JANS ...,
!                      not documented in manual
!                 =pwtail; set by command SET ... [pwtail]
!    (2) [   2.5] energy spectrum w.r.t. wave number, not used
!    (3) [CALCUL] =PWTAIL(1)+1; action density spectrum w.r.t. frequency
!    (4) [    3.] action density spectrum w.r.t. wave number, not used
!    (5) [CALCUL] =1./(PWTAIL(1)*(1.+PWTAIL(1)*(FRINTH-1.)));
!                 tail factor for the action integral
!    (6) [CALCUL] =1./((PWTAIL(1)-1.)*(1.+(PWTAIL(1)-1.)*(FRINTH-1.)));
!                 tail factor for the energy integral
!    (7) [CALCUL] =1./((PWTAIL(1)-2.)*(1.+(PWTAIL(1)-2.)*(FRINTH-1.)));
!                 tail factor for the first moment of energy
!    (8) [CALCUL] =1./((PWTAIL(1)-3.)*(1.+(PWTAIL(1)-3.)*(FRINTH-1.)));
!                 tail factor for the second moment of energy
! RHO    [ 1025.] density of the water
!                 =rho; set by command SET ... [rho] ...
! USCAP  [99999.] maximum ustar
! WLEV   [    0.] water level
!                 =level; set by command SET [level] ...
! ICEWIND [   0.] factor controlling wind input through ice cover
!                 ; set by command SET [ICEWIND] ...

   REAL CASTD,       CDCAP,       USCAP
   REAL DEGRAD,      DNORTH,      GRAV,        PI
   REAL PI2,         PWTAIL(10),  RHO,         WLEV
   REAL ICEWIND

!     *** information related to the numerical scheme ***
!
! ACUPDA [  true] indicates whether or not action densities are to be updated
!                 during computation
! BNAUT  [ false] indicates whether nautical or cartesian directions are used
! BNDCHK [  true] indicates whether computed Hs on boundary must be compared
!                 with value entered as boundary condition
! BRESCL [  true] rescaling on/off
! CSETUP [  true] indicates whether solver for setup has converged
! DEPMIN [  0.05] threshold depth (to prevent zero divisions)
!                 =depmin; set by command SET ... [depmin] ...
! DSHAPE [     2] indicates option for computation of directional distribution
!                 in the spectrum (boundary spectra etc.)
!                 =1: directional spread in degrees is given
!                 =2: power of COS is given
! FPI    [   ---] wind sea part of peak frequency
! FSHAPE [     2] indicates option for computation of frequency distribution
!                 in the spectrum (boundary spectra etc.)
!                 =2: Jonswap(default set by subr SWINIT),
!                 =1: Pierson-Moskowitz, =3: bin, =4: Gaussian, =5: TMA
!                 (set by command BOUNshape ..)
! HSRERR [   0.1] The error margin allowed between pre-scribed and calculated Hs
!                 at the up-wave boundary. If exceeded a warning is produced
! IBIPH  [     1] indicates estimating biphase:
!                 =1; based on Eldeberky (1996)
!                 =2; based on Saprykina et al. (2017)
!                 =3; based on De Wit (2022)
! IBOT   [     0] indicator bottom friction:
!                 =0; no bottom friction dissipation
!                 =1; set by command FRIC JON  ..., Jonswap bottom friction model
!                 =2; for command FRIC COLL ..., Collins bottom friction model
!                 =3; for command FRIC MAD  ..., Madsen bottom friction
!                 =5; for command FRIC RIP  .... roughness due to ripples and sediment
! IBRAG  [     0] indicates computation of Bragg scattering term:
!                 =0; source term is inactive
!                 =1; source term is calculated per sweep direction;
!                     bottom spectrum interpolated at k - k' a priori thus requiring storage
!                 =2; source term is calculated per sweep direction;
!                     bottom spectrum interpolated at k - k' per sweep (no storage)
!                 =3; source term is calculated per iteration
!                     bottom spectrum interpolated at k - k' per iteration (no storage)
!                 =4; implicit integration according to Ardhuin and Herbers (2002)
! ICMAX  [     3] number of points in computational stencil
! ICOR   [      ] not used
! ICUR   [     0] indicates presence of currents:
!                 =0; no currents
!                 =1; for command READ CUR ..., currents are present
! IDBR   [     1] not used
! IDIF   [     0] not used
! IDIFFR [     0] diffraction method
!                 0= no diffraction
!                 1= diffraction
! IDISRF [     0] indicates wave directionality in surf breaking
!                 0=no wave directionality
!                 1=wave directionality included
! IDRAG  [     2] indicates formulation for wind drag coefficient
!                 1=Wu (1982)
!                 2=2nd order polynomial fit (see CE publication, 2012)
!                 3=Cd based on cross swell
!                 4=Hwang (2011)
!                 5=Fan (2012)
!                 6=ECMWF
! IFRSRF [     0] indicates frequency dependent surf breaking
!                 0=no frequency dependency
!                 1=frequency dependency
! IGEN   [     3] indicates the generation mode
!                 =1; for command GEN1 ...,
!                 =2; for command GEN2 ...,
!                 =3; for command GEN3 ...,
!                 =4; for command GEN4 ...,
! IICE   [      ] indicates how ice is to be handled within the model
!                 =0; for no representation of sea ice (default)
!                 =1; activated with keyword "CICE" in "INPUT" file.
!                     Vestigial code: not in public release.
!                 =2; activated with keyword "ADCICE" in "INPUT" file.
!                     Vestigial code: not in public release.
!                 =3; v41.31: activated with keyword "IC4M2" in "INPUT"
!                 =3; v41.41+ : activated with keyword "R19" in "INPUT"
!                     Active code: dissipation by sea ice using method denoted
!                        as "IC4M2" in Rogers (2019) (R19), since it is
!                        (but not identical!) to IC4M2 in WW3
!                 =4; v41.41+: "D15" method. This uses a formula from
!                     Doble et al. (2015)
!                 =5; v41.41+: "M18" method. This uses a formula from
!                     Meylan et al. (2018) "model with order 3 power law"
!                     also known as "M2" model in Liu et al. (2020)
!                 =6; v41.41+: "R21B" method. This uses a formula from
!                     Rogers et al. (2021) Tech report, based on combination
!                     of Yu et al. (2019) normalization with monomial power
!                     law empirical fitting.
!CTGA 111003:  Full ice implementation.  Parameter read in swanpre1.ftn
!CTGA! IICE   [      ] indicates how ice is to be handled within the model
!CTGA!                 =1; for undefined command
!CTGA!                 =2; for command CICE ADCICE ...
! IINC   [     0] not used
! IMUD   [     0] indicates fluid mud dissipation term:
!                 =0; no dissipation due to fluid mud
!                 =1; dissipation due to fluid mud according to Ng (2000)
! IPRE   [      ] not used
! IQCM   [     0] indicates computation of QC scattering term:
!                 =0; source term is inactive
!                 =1; scattering due to depth and current variation
!                 =2; wave-current interaction; Fourier transforms per grid point (no storage)
!                 =3; wave-current interaction; Fourier transforms in pre-phase thus requiring storage
! IQUAD  [     2] indicates quadruplet interaction term:
!                 =0; for command OFF QUAD
!                 =0; for command GEN1 ...,
!                 =0; for command GEN2 ...,
!                 =0; for command GROWTH G1 ... (not documented in manual),
!                 =0; for command GROWTH G2 ... (not documented in manual),
!                 quadruplets are inactive
!                 =1; quadruplets are calculated semi implicit per sweep direction
!                 =2; for command GEN3 ...,
!                 =2; for command QUAD, not documented in the manual
!                 =2; set when IWIND=3 or 4 and ICUR=0 in SUBR ERRCHK,
!                 quadruplets are calculated fully explicit per sweep direction
!                 =3; set when IWIND=3 or 4 and ICUR=1 in SUBR ERRCHK,
!                 quadruplets are calculated fully explicit per iteration
!                 =8; quadruplets are calculated fully explicit per iteration and
!                     interactions are interpolated in piecewise constant manner
!                 =iquad; set by command GEN3 ... QUAD [iquad] ...,
! IREFR  [     1] indicates refraction effect:
!                 =0; for command OFF REF, refraction is inactive
!                 =1; refraction is active
! ISURF  [     1] indicates surf breaking (shallow water) term:
!                 =0; for command OFF BRE, surf breaking is inactive
!                 =1; for command BRE CON ..., surf breaking with constant parameter
!                 =2; for command BRE VAR ..., surf breaking according to Nelson
!                 =3; for command BRE RUE ..., surf breaking according to Ruessink
!                 =4; for command BRE TG ... , surf breaking according to Thornton and Guza
! ITERMX [      ] maximum number of iterations:
!                 is set equal to MXITST in case of stationary computations
!                 is set equal to MXITNS in case of nonstationary computations
! ITFRE  [     1] indicator for transport of action in frequency space
!                 =0; for command OFF FSH, frequency shifting inactive
!                 =1; frequency shifting active
! ITRIAD [     0] indicates triad interaction term:
!                 =0; triads are inactive
!                 =1; for command TRI DTA IMP ..., not documented in manual
!                 =2; for command TRI DTA EXP ..., not documented in manual
!                 =3; for command TRI [trfac] [cutfr], as in manual
!                 =3; for command TRI LTA IMP ..., not documented in manual
!                 =4; for command TRI LTA EXP ..., not documented in manual
! IVEG   [     0] indicates vegetation dissipation term:
!                 =0; no dissipation due to vegetation
!                 =1; dissipation due to vegetation according to Dalrymple (1984)
! ITURBV [     0] indicates if turbulent viscosity is activated
!                 =0; not activated
!                 =1; activated
! IWCAP  [     1] indicates whitecapping:
!                 =0; for command GEN1 ...,
!                 =0; for command GEN2 ...,
!                 =0; for command OFF WCAP, no whitecapping
!                 =1; for command GEN3 KOM ...,
!                 =1; for command WCAP KOM ..., not documented in manual,
!                 standard WAM formulation (Komen et al.; 1984)
!                 =2; for command GEN3 JANS ...,
!                 =2; for command WCAP JANS ..., not documented in manual,
!                 according to Janssen (1989, 1991)
!                 =3; for command WCAP LHIG ..., not documented in manual,
!                 according to Longuet-Higgins (1967), Yuan et al. (1986)
!                 =4; for command WCAP BJ ..., not documented in manual,
!                 according to Battjes & Janssen (1978)
!                 =5; for command WCAP KBJ ..., not documented in manual,
!                 combined formulation of Komen (1) and Battjes & Janssen (4)
! IWCCUR [     0] indicates enhanced whitecapping in counter current
! IWIND  [     0] indicates presence of wind, and type of source term used:
!                 =0; no wind
!                 =1; for command GEN1 ..., if wind is made active,
!                 =1; for command GROWTH G1 ..., not documented in manual,
!                 1st generation source term
!                 =2; for command GEN2 ..., if wind is made active,
!                 =2; for command GROWTH G2 ..., not documented in manual,
!                 2nd generation source term (as in Dolphin)
!                 =3; for command WIND ..., if IWIND still was 0, else unchanged,
!                 =3; for command GEN3 KOM ..., if wind is made active,
!                 =3; for command GROWTH G3 KOM ..., not documented in manual,
!                 3rd generation source term (Snyder)
!                 =4; for command GEN3 JANS ..., if wind is made active,
!                 =4; for command GROWTH G3 JANS ..., not documented in
!                 source term by P. Janssen (1989, 1991)
!                 =5; for command GEN3 YAN ..., if wind is made active,
!                 =5; for command GROWTH G3 YAN ..., not documented in manual,
!                 not documented in manual
! LADDS  [.fals.] indicates whether extra output is requested or not
! LSETUP [     0] =0; setup is not calculated
!                 =1; setup is calculated
!                 =2; setup is calculated with the boundary conditions from
!                     a nest file
! LSPNAR [.fals.] indicates whether directional spread is too narrow or
! LSRFB  [.fals.] indicates whether surfbeat computation should be performed
! MODGAM [.true.] if true modify breaker index for BKD during iteration
! MXITST [    50] max. number of iterations in stationary computations
! MXITNS [     1] max. number of iterations in nonstationary computations
! NCOR   [     1] not used
! NSTATC [     1] indicates stationarity of computation:
!                 =0; stationary computation
!                 =1; nonstationary computation
! NSTATM  [   -1] 0: stationary mode, 1: nonstationary mode, -1: unknown
! NUMOBS [      ] number of obstacles
! NCOMPT [      ] number of COMPUTE commands
! OFFSRC [.fals.] indicates whether source terms are included in
!                 action balance equation or not
! ONED   [.FALS.] Indicates whether the calculation should be performed
! PDIFFR(MDIFFR)  coefficients for diffraction
!  ( 1)           smoothing parameter
!  ( 2)           number of smoothing steps
!  ( 3)           if (Cx,Cy)-velocities are modified or not
! PBOT(MBOT)      coefficients for the bottom friction models
!  ( 1)  [    0.] =cfc; set by command FRIC COL [cfw] [cfc], (Collins equation),
!                 not documented in the manual
!  ( 2)  [ 0.015] =cfw; set by command FRIC COL [cfw], (Collins equation),
!                 also used as CFW in the source code
!  ( 3)  [ 0.038] =cfjon; set by command FRIC JON [cfjon], (Jonswap formulation)
!  ( 4)  [ -0.08] =mf; value cannot be changed, (Madsen equation)
!  ( 5)  [  0.05] bottom roughness length scale, (Madsen equation),
!                 =kn; set by command FRIC JON [kn],
!                 also used as AKN in the source code
!  ( 6)  [  2.65] specific gravity of sediment; set by command FRIC RIP
!  ( 7)  [0.0001] sediment diameter; set by command FRIC RIP [D]
! PMUD(MMUD)      coefficients for the fluid mud-induced dissipation model
!  ( 1)  [    0.] =layer; set by command MUD ... [layer] ...
!  ( 2)  [ 1300.] =rhom; set by command MUD ... [rhom] ...
!  ( 3)  [0.0027] =viscm; set by command MUD ... [viscm] ...
!  ( 4)  [  RHOW] =rhow; set by command MUD ... [rhow] ...
!  ( 5)  [1.3E-6] =viscw; set by command MUD ... [viscw] ...
! PSICE(MSICE)    coefficients for the dissipation by sea ice.
!                 The meaning of PSICE depends on IICE as follows:
!                 For case of IICE=3: simple polynomial with integer
!                 powers, ki= C0*f^0 + C1*f^1 ... C5*f^5 + C6*f^6
!  ( 1)  [    0.] = unused
!  ( 2)  [    0.] = coefficient C0, for f^0 term
!  ( 3)  [    0.] = coefficient C1, for f^1 term
!  ( 4)  [    0.] = coefficient C2, for f^2 term
!  ( 5)  [    0.] = coefficient C3, for f^3 term
!  ( 6)  [    0.] = coefficient C4, for f^4 term
!  ( 7)  [    0.] = coefficient C5, for f^5 term
!  ( 8)  [    0.] = coefficient C6, for f^6 term
!                 For case of IICE=4 :
!  ( 1)  [ 2.13 ] = coefficient Chf
!  ( 2 to 8 )     = unused
!                 For case of IICE= 5:
!  ( 1)  [ 0.059 ] = coefficient Chf
!  ( 2 to 8 )      = unused
!                 For case of IICE= 6:
!  ( 1)  [ 2.9 ]  = coefficient Chf
!  ( 2)  [ 4.5 ]  = coefficient npf
!  ( 3 to 8 )     = unused
! PICE(MICE)      value of uniform ice fraction and thickness,  used only
!                 if the user elects to treat ice fraction and thickness
!                 as constant and uniform.
! PNUMS(MNUMS)    numerical coefficients
!                 accuracy criterion:
!  ( 1)  [  0.02] relative error in Hs and Tm01
!                 =drel; set by command NUM ACCUR [drel] ...
!  ( 2)  [  0.03] absolute error in Hs (m)
!                 =dhabs; set by command NUM ACCUR ... [dhabs] ...
!  ( 3)  [   0.3] absolute error in Tm01 (s)
!                 =dtabs; set by command NUM ACCUR ... [dtabs] ...
!  ( 4)  [ 98.00] percentage of wet grid points were absolute and relative
!                 accuracy has been reached
!                 =npnts; set by command NUM ACCUR ... [npnts] ...
!  ( 5)  [    0.] not used
!                 diffusion schemes:
!  ( 6)  [   0.5] numerical diffusion over theta,
!                 =cdd; set by command NUM DIR [cdd]
!  ( 7)  [   0.5] numerical diffusion over sigma,
!                 =css; set by command NUM SIGIM [css] ...
!  ( 8)  [    1.] numerical scheme in frequency space
!                 =1.; for command NUM SIGIM, implicit scheme
!                 =2.; for command NUM SIGEX, explicit scheme,
!                 with CFL criterion
!                 =3.; for command NUM FIL, explicit scheme,
!                 without CFL criterion, not documented in the manual
!  ( 9)  [  0.01] diffusion coefficient for explicit scheme,
!                 =diffc; set by command NUM FIL [diffc],
!                 not documented in the manual
!                 iterative solver (SIP):
!  (12)  [ 1.E-4] termination criterion for iterative solver
!                 =eps2; set by command NUM SIGIM ... [eps2] ...
!                 (||Ax-b|| < eps2 * ||b||)
!  (13)  [    0.] output for SIP solver
!                 =outp; set by command NUM SIGIM ... [outp] ...
!                 <0. no output
!                 =0. only fatal errors are printed
!                 =1. additional information about the iteration is printed
!                 =2. maximal output regarding the iteration process
!  (14)  [   20.] maximum number of iterations in the solver
!                 =niter; set by command NUM SIGIM ... [niter]
!  (15)  [  0.02] global error in Hs
!  (16)  [  0.02] global error in Tm01
!  (17)  [   -1.] coefficient for limitation of Ctheta (not used currently)
!  (18)  [   0.8] limitation on Froude number (current velocity is reduced if
!                 larger than CGMAX=PNUMS(18)*SQRT(GRAV*DEPW)),
!                 =froudmax; set by command SET ... [froudmax] ...,
!                 not documented in the manual
!  (19)  [CALCUL] =0.5*SQRT(2.); CFL criterion for explicit scheme in
!                 frequency space, manual mentions default = 0.7,
!                 =cfl; set by command NUM SIGEX [cfl]
!  (20)  [   0.1] maximum growth in spectral bin,
!                 =0.1; for command WIND ..., if IWIND=3 or 4
!                 =0.1; for command GEN3
!                 =0.1; for command TRI
!                 =0.1; for command QUAD,
!                 not documented in the manual
!                 =1.E20; for command OFF QUAD
!                 =limiter; set by command GEN3 ... QUAD ... [limiter]
!                 =limiter; set by command NUM ACCUR ... [itermax] [limiter],
!                 not documented in the manual
!                 =limiter; set by command QUAD [iquad] [limiter],
!                 not documented in the manual
!  (21)  [    0.] =coefficient for type stopping criterion
!  (23)           termination criterion for iterative solver in set-up calculation
!                 =eps2, set by command NUM SETUP ... [eps2] ...
!  (24)           output for iterative solver in set-up calculation
!                 =outp, set by command NUM SETUP ... [outp] ...
!                 <0. no output
!                 =0. only fatal errors are printed
!                 =1. additional information about the iteration is printed
!                 =2. maximal output regarding the iteration process
!  (25)           maximum number of iterations for solver in set-up calculation
!                 =niter, set by command NUM SETUP ... [niter] ...
!  (28)  [    1.] Qb-value at which the limiter is not active in case lowering AC2
!  (30)  [    0.] =under-relaxation factor
!  (33)  [    0.] indicates use of Courant limiter for csigma
!                 =0. no limiter
!                 =1. csigma will be limited
!  (34)   [  0.5] =upper limit of CFL restriction for csigma
!  (35)  [    0.] indicates use of Courant limiter for ctheta
!                 =0. no limiter
!                 =1. ctheta will be limited
!  (36)   [  0.5] =upper limit of CFL restriction for ctheta
!  (37)   [ 95. ] = required fraction of wet grid points with which modification
!                   of BKD-computed breaker index stops
! PQUAD(MQUAD)    coefficients for quadruplet interaction
!  ( 1)  [  0.25] lambda in eq. B29 of user manual
!  ( 2)  [  3.E7] coefficient of interactions
!  ( 3)  [   5.5] coefficient for shallow water interactions
!  ( 4)  [ 0.833] coefficient for shallow water interactions
!  ( 5)  [ -1.25] coefficient for shallow water interactions
! PSETUP(MSETUP)
!  ( 1)  [   0.0] not used
!  ( 2)  [   0.0] user defined level for correction of the setup
! PSHAPE(MSHAPE)  coefficients for calculation of spectrum from integral
!                 parameters
!  ( 1)  [   3.3] peak enhancement factor of Jonswap spectrum
!                  =gamma; set by command BOUN SHAPE .. JON [gamma]
!  ( 2)  [   0.1] width of Gaussian spectrum (in Hz)
!                 =sigfr; set by command BOUN SHAPE .. GAU [sigfr]
!                 after reading the value is converted to radians/second
!                 by a 2 pi multiplication
!  ( 3)  [      ] reference depth for TMA spectrum
! PSURF(MSURF)    surf breaking coefficients
!  ( 1)  [   1.0] coef. for determining rate of dissipation, (Battjes Janssen),
!                 =1.5; for command BRE VAR
!                 =alpha, set by command BRE CON [alpha] ...
!                 =alpha, set by command BRE VAR [alpha] ...
!  ( 2)  [  0.73] breaker parameter
!                 =gamma, set by command BRE CON ... [gamma]
!  ( 4)  [      ] the min. value of the breaker parameter of Nelson
!                 =0.55; set by command BRE VAR
!                 =gammin; set by command BRE VAR ... [gammin] ...
!  ( 5)  [      ] the max. value of the breaker parameter of Nelson
!                 =0.81; set by command BRE VAR
!                 =gammax; set by command BRE VAR ... [gammax] ...
!  ( 6)  [      ] breaker parameter for negative bottom slopes
!                 =0.73; set by command BRE VAR
!                 =gamneg; set by command BRE VAR ... [gamneg] ...
!  ( 7)  [      ] proportionality coefficient in expression of Nelson
!                 =0.88; set by command BRE VAR
!                 =coeff1; set by command BRE VAR ... [coeff1] ...
!  ( 8)  [      ] coefficient in the exp in the expression of Nelson
!                 =0.012; set by command BRE VAR
!                 =coeff2; set by command BRE VAR ... [coeff2]
! PTRIAD(MTRIAD)
!  ( 1)  [  0.65] controls the proportionality coefficient,
!                 =trfac; set by command TRIAD ... [trfac] ...
!  ( 2)  [   2.5] controls the maximum frequency considered in the comp.,
!                 =cutfr; set by command TRIAD ... [cutfr]
!  ( 3)  [  10.0] controls above which Ursell number the quadruplets (and
!                 thus the limiter) are switched off
!                 =ursell; set by command LIM ... [ursell]
!  ( 4)  [  0.63] critical Ursell number appearing in the biphase expression
!                 =urcrit; set by command TRIAD ... [urcrit]
!  ( 5)  [  0.01] controls below which Ursell number the triads are switched off
!                 =urslim; set by command TRIAD ... [urslim]
! PWCAP(MWCAP)     whitecapping coefficients
!  ( 1)  [2.36E-5] coefficient for Komen et al. (1984),
!                 ALFAWC (Emperical coefficient)
!                 =cds2; set by command GEN3 KOM [cds2] ...,
!                =cds2; set by command WCAP KOM [cds2] ...,
!                 not documented in the user manual
!  ( 2)  [3.02E-3] coefficient for Komen et al. (1984),
!                 ALFAPM (Alpha of Pierson Moskowitz frequency)
!                 =stpm; set by command GEN3 KOM ... [stpm],
!                 =stpm; set by command WCAP KOM ... [stpm],
!                 not documented in the user manual
!  ( 3)  [   4.5] coeff. for Janssen (1989,1991), acc. to Komen et al. (1994),
!                 CFJANS (cds coefficient)
!                 =cds1; set by command GEN3 JANS [cds1] ...,
!                 =cds1; set by command WCAP JANS [cds1] ...,
!                 not documented in the user manual
!  ( 4)  [   0.5] (=DELTA)
!                 =delta; set by command GEN3 JANS ... [delta],
!                 =delta; set by command WCAP JANS ... [delta],
!                 not documented in the user manual
!  ( 5)  [    1.] coefficient of Longuet Higgins,
!                 =cflhig; set by command WCAP LHIG [cflhig],
!                 not documented in the user manual
!  ( 6)  [  0.88] GAMBTJ (Steepness limited wave breaking)
!                 =bjstp; set by command WCAP BJ [bjstp] ...
!                 =bjstp; set by command WCAP KBJ [bjstp] ...
!  ( 7)  [    1.] Alpha in Battjes/Janssen,
!                 =bjalf; set by command WCAP BJ ... [bjalf]
!                 =bjalf; set by command WCAP KBJ ... [bjalf] ...
!  ( 8)  [  0.75] numerical diffusion over sigma
!                 =kconv; set by command WCAP KBJ ... [kconv]
! PWIND(MWIND)    wind growth term coefficients
!  ( 1)  [  188.] controls linear wave growth,
!                 =cf10; set by command GEN1 [cf10] ...
!                 =cf10; set by command GEN2 [cf10] ...
!                 =cf10; set by command GROWTH G1 [cf10] ...,
!                 not documented in the user manual
!                 =cf10; set by command GROWTH G2 [cf10] ...,
!                 not documented in the user manual
!  ( 2)  [  0.59] controls the exponential wave growth,
!                 =cf20; set by command GEN1 ... [cf20] ...
!                 =cf20; set by command GEN2 ... [cf20] ...
!                 =cf20; set by command GROWTH G1 ... [cf20] ...,
!                 not documented in the user manual
!                 =cf20; set by command GROWTH G2 ... [cf20] ...,
!                 not documented in the user manual
!  ( 3)  [  0.12] controls the exponential wave growth,
!                 =cf30; set by command GEN1 ... [cf30] ...
!                 =cf30; set by command GEN2 ... [cf30] ...
!                 =cf30; set by command GROWTH G1 ... [cf30] ...,
!                 not documented in the user manual
!                 =cf30; set by command GROWTH G2 ... [cf30] ...,
!                 not documented in the user manual
!  ( 4)  [  250.] controls the dissipation rate,
!                 =cf40; set by command GEN1 ... [cf40] ...
!                 =cf40; set by command GEN2 ... [cf40] ...
!                 =cf40; set by command GROWTH G1 ... [cf40] ...,
!                 not documented in the user manual
!                 =cf40; set by command GROWTH G2 ... [cf40] ...,
!                 not documented in the user manual
!  ( 5)  [0.0023] controls the spectral energy of the limit spectrum
!                 =cf50; set by command GEN2 ... [cf50] ...
!                 =cf50; set by command GROWTH G2 ... [cf50] ...,
!                 not documented in the user manual
!  ( 6)  [-0.223] controls the spectral energy of the limit spectrum
!                 =cf60; set by command GEN2 ... [cf60] ...
!                 =cf60; set by command GROWTH G2 ... [cf60] ...,
!                 not documented in the user manual
!  ( 7)  [    0.] cf70, not used
!  ( 8)  [ -0.56] cf80, not used
!  ( 9)  [CALCUL] density air / density water (=RHOAW),
!                 =PWIND(16)/RHO
!                 =rhoaw; set by command GROWTH G1 ... [rhoaw] ...,
!                 not documented in the user manual
!                 =rhoaw; set by command GROWTH G2 ... [rhoaw] ...,
!                 not documented in the user manual
!  (10)  [0.0036] limit energy Pierson Moskowitz spectrum
!                 =edmlpm; set by command GEN1 ... [edmlpm] ...
!                 =edmlpm; set by command GEN2 ... [edmlpm] ...
!                 =edmlpm; set by command GROWTH G1 ... [edmlpm] ...,
!                 not documented in the user manual
!                 =edmlpm; set by command GROWTH G2 ... [edmlpm] ...,
!                 not documented in the user manual
!  (11)  [0.00123] drag coefficient
!                 =cdrag; set by command GEN1 ... [cdrag] ...
!                 =cdrag; set by command GEN2 ... [cdrag] ...
!                 =cdrag; set by command GROWTH G1 ... [cdrag] ...,
!                 not documented in the user manual
!                 =cdrag; set by command GROWTH G2 ... [cdrag] ...,
!                 not documented in the user manual
!  (12)  [   1.0] minimum wind velocity, relative to current at 10 m above msl
!                 =umin; set by command GEN1 ... [umin] ...
!                 =umin; set by command GEN2 ... [umin] ...
!                 =umin; set by command GROWTH G1 ... [umin] ...,
!                 not documented in the user manual
!                 =umin; set by command GROWTH G2 ... [umin] ...,
!                 not documented in the user manual
!  (13)  [  0.13] coefficient that determines the Pierson Moskowitz spectrum
!                 =cfpm; set by command GEN1 ... [cfpm]
!                 =cfpm; set by command GEN2 ... [cfpm]
!                 =cfpm; set by command GROWTH G1 ... [cfpm],
!                 not documented in the user manual
!                 =cfpm; set by command GROWTH G2 ... [cfpm],
!                 not documented in the user manual
!  (14)  [  0.01] (=ALPHA) Alpha, according to Janssen (1991) wave growth model
!  (15)  [  0.41] (=XKAPPA)carnock: Kappa
!  (16)  [  1.28] density of the air (=RHOA)
!  (17)  [CALCUL] =RHO, density of the water (=RHOW)
!  (31)  [    0.] proportionality coefficient in the wave growth term of
!                 Caveleri and Malanotte,
!                 =0.0015; for command GEN3 ... AGROW
!                 =a; set by command GEN3 ... AGROW [a]
! PBRAG(MBRAG)    Bragg scattering coefficients
!  ( 1)  [   ---] size of region of depth points around computational grid point
!                 for computing bottom spectrum; set by command BRAGG [nreg]
!  ( 2)  [    5.] =cutoff; set by command BRAGG [cutoff]
! PSCAT(MSCAT)    QC scattering coefficients
!  ( 1)  [    1.] multiple of mean wave number for truncating scattering wave number space
!                 =alpha; set by command SCAT TRU [alpha]
!  ( 2)  [   ---] maximum scattering wave number
!                 =qmax;  set by command SCAT TRU [qmax]
!  ( 3)  [   ---] origin of the wave number in x-direction;
!                 set by command SCAT GRI K [kxlow]
!  ( 4)  [   ---] origin of the wave number in y-direction;
!                 set by command SCAT GRI K [kylow]
!  ( 5)  [   ---] length of the wave number grid in x-direction;
!                 set by command SCAT GRI K [kxlen]
!  ( 6)  [   ---] length of the wave number grid in y-direction;
!                 set by command SCAT GRI K [kylen]
!  ( 7)  [    1.] resolution factor to determine the step size of the wave number grid;
!                 set by command SCAT GRI WID [rfac]
! SIGMAG [   0.1] width of the Gaussian frequency spectrum in Hz
!                 =0.01; for command BOU STAT ... GAU
!                 =sigfr; set by command BOU STAT ... GAU [sigfr]
!                 after reading the value is converted to radians/second
!                 by a 2 pi multiplication
! SPPARM          integral parameters used for computation of incident spectrum
!  ( 1)  [   ---] significant wave height
!  ( 2)  [   ---] wave period (peak or mean)
!  ( 3)  [   ---] average wave direction
!  ( 4)  [   ---] directional distribution coefficient
! SY0    [   3.3] peak enhancement parameter of the JONSWAP spectrum,
!                 =gamma; set by command BOU STAT ... JON [gamma]
! U10    [    0.] wind velocity
!                 =vel; set by command WIND [vel] ...
!CTGA 111003:  Full ice implementation.  Parameter read in swanpre1.ftn
! WBICETH[  70.0] Minimum ice concentration (percentage out of 100.0)
!                 necessary to trigger wave blocking.
! WDIC   [CALCUL] =PI2*((WDIP/PI2-NINT(WDIP/PI2)),
! WDIP   [    0.] wind direction with respect to problem coordinates
!                 =dir; set by command WIND ... [dir]

   INTEGER             DSHAPE,      FSHAPE
   INTEGER             ICMAX
   INTEGER             IBOT, ICUR
   INTEGER             IGEN
   INTEGER             IQUAD, IREFR, ISURF
   INTEGER             ITERMX,      ITFRE,       ITRIAD,      IBIPH
   INTEGER             IWCAP,       IWIND,       LSETUP,      IDRAG
   INTEGER             MXITST, MXITNS
   INTEGER             NSTATC,      NSTATM,      NUMOBS,      NCOMPT
   INTEGER             IDIFFR
   INTEGER             IWCCUR
   INTEGER             IMUD
   INTEGER             IVEG
   INTEGER             ITURBV
   INTEGER             IBRAG
   INTEGER             IQCM
   INTEGER             IFRSRF
   INTEGER             IDISRF
!CTGA 111003:  Full ice implementation.  Parameter read in swanpre1.ftn
   INTEGER             IICE
   REAL                DEPMIN,      PBOT(MBOT),  PNUMS(MNUMS)
   REAL                PSETUP(MSETUP),           PSHAPE(MSHAPE)
   REAL                PSURF(MSURF),             PTRIAD(MTRIAD)
   REAL                PWCAP(MWCAP),             PWIND(MWIND)
   REAL                SIGMAG
   REAL                SPPARM(MSPPAR), U10
   REAL                WDIC,          WDIP,      HSRERR
   REAL                PQUAD(MQUAD)
   REAL(KIND=KIND(0.0D0))              RCOMPT(300,5)
   REAL                PDIFFR(MDIFFR)
   REAL                PMUD(MMUD)
   REAL                PTURBV(MTURBV)
   REAL                PBRAG(MBRAG)
   REAL                PSICE(MSICE)
   REAL                PICE(MICE)
   REAL                PSCAT(MSCAT)
!CTGA 111003:  Full ice implementation.  Parameter read in swanpre1.ftn
   LOGICAL             ACUPDA
   LOGICAL             BNDCHK,      BNAUT,       ONED,        BRESCL
   LOGICAL             OFFSRC
   LOGICAL             LADDS
   LOGICAL             CSETUP
   LOGICAL             LSPNAR
   LOGICAL             LSRFB
   LOGICAL, SAVE ::    RUNMADE = .FALSE.
   LOGICAL, SAVE ::    MODGAM = .TRUE.
! parameters for Babanin physics and swell, see Rogers et al (JTECH, 2012)
   REAL                A1SDS, A2SDS, P1SDS, P2SDS, CDSV, FESWELL
   REAL                RDCOEF, B1Z, WNDSCL, CDFAC
   LOGICAL             UPWARDS, VECTOR_TAU, TRUE_U10
   LOGICAL             ROGERS, ZIEGER, ARDHUIN
   REAL                FPI
!$OMP THREADPRIVATE(ICMAX,CSETUP)
!
!  8. Subroutines and functions used
!
!     ---
!
!  9. Subroutines and functions calling
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
!     ---
!
! 13. Source text

end module SWCOMM3

