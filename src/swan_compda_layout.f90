module swan_compda_layout
!
!     Where each quantity lives in COMPDA, the array holding one row of data
!     per computational grid point.
!
!     COMPDA has no fixed layout: SWINIT assigns a column to every quantity the
!     run actually needs, and a TABLE or BLOCK request for a derived quantity
!     appends another. These indices are the result, and MCMVAR is how many
!     columns were handed out. Nothing here is meaningful without the array it
!     indexes.
!
!     Eighty-seven symbols for nine files. They sat in SWCOMM3, so the other
!     forty-eight importers -- everything that only wanted MSC, MDC or the grid
!     size -- carried the entire COMPDA layout with them.
!
!     The comments below are the original ones. A value in brackets is the
!     index the quantity gets in a default run; where it depends on the
!     commands given, the condition is named.
!
   implicit none(type, external)
   private

   PUBLIC :: JAICE2, JAICE3, JASTD2, JASTD3, JBIPH, JBOTLV, JCDRAG, JDHS
   PUBLIC :: JDISS, JDP1, JDP2, JDPSAV, JDSS2, JDSS3, JDSXB, JDSXI, JDSXL
   PUBLIC :: JDSXM, JDSXS, JDSXT, JDSXV, JDSXW, JDTM, JFRC2, JFRC3, JGAMMA
   PUBLIC :: JGENR, JGSXW, JHICE2, JHICE3, JHS, JHSIBC, JHSS2, JHSS3, JLEAK
   PUBLIC :: JMUDL1, JMUDL2, JMUDL3, JNPLA2, JNPLA3, JP4D, JP4S, JPBOT, JPBRAG
   PUBLIC :: JPBTFR, JPICE, JPMUD, JPQCS, JPSWEL, JPTRI, JPTURB, JPVEGT
   PUBLIC :: JPWBRK, JPWCAP, JPWNDD, JPWNDS, JQB, JRADS, JREDS, JRSXB, JRSXC
   PUBLIC :: JRSXQ, JRSXT, JSETUP, JTAUW, JTRAN, JTSS2, JTSS3, JTSXG, JTSXS
   PUBLIC :: JTSXT, JTURB2, JTURB3, JUBOT, JURSEL, JUSTAR, JVX1, JVX2, JVY1
   PUBLIC :: JVY2, JWLV2, JWX2, JWX3, JWY2, JWY3, JZEL, MCMVAR

! JASTD2 [  1] new air-sea temp. diff. within array COMPDA
! JASTD3 [  1] last read air-sea temp. diff. within array COMPDA
! JBIPH  [ 29] parametrized biphase
! JBOTLV [ 28] bottom level within array COMPDA
! JCDRAG [  1] drag coefficient within array COMPDA,
!              set by command WCAP JANS ...
! JDHS   [  6] wave height correction within array COMPDA
!              (difference in Hs between last two iterations)
! JDISS  [  2] total dissipation within array COMPDA
! JDPSAV [  1] saved depth (for setup) within array COMPDA
! JDP1   [  7] old depth within array COMPDA
! JDP2   [  8] new depth within array COMPDA
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
! JTAUW  [  1] TauW within array COMPDA,
!              set by command WCAP JANS ...
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
! JVY1   [ 10] y of old current velocity within array COMPDA
! JVY2   [ 12] y of new current velocity within array COMPDA
! JWLV2  [ 24] new water level within array COMPDA
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
   INTEGER             MCMVAR,      JURSEL
   INTEGER             JBIPH
   INTEGER             JBOTLV
   INTEGER             JPSWEL
   INTEGER             JAICE2,      JAICE3,      JHICE2,      JHICE3
   INTEGER             JPICE,       JDSXI
   INTEGER             JHSS2,       JHSS3,       JTSS2,       JTSS3
   INTEGER             JDSS2,       JDSS3
   INTEGER             JPBRAG,      JRSXB,       JPQCS,       JRSXC
end module swan_compda_layout
