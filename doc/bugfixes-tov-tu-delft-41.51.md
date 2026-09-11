# Bugfixes ten opzichte van TU Delft SWAN 41.51

## Vergelijkingsbasis

Deze lijst vergelijkt de RWS-fork op bronstand "Benoem de numerieke en fysische keuzes in plaats van indexen" met de op
11 september 2026 opnieuw opgehaalde `upstream/main` van TU Delft:

- repository: [`citg/wavemodels/swan`](https://gitlab.tudelft.nl/citg/wavemodels/swan);
- upstream-commit: [`43e9bbba393f2cf9eaff78bc92eaed118a3a5c8e`](https://gitlab.tudelft.nl/citg/wavemodels/swan/-/commit/43e9bbba393f2cf9eaff78bc92eaed118a3a5c8e), 13 juli 2026;
- laatste bronpatch daarin: `0dabfa4` (`updated src (patch B)`), eveneens
  13 juli 2026;
- versienummer aan beide kanten: **41.51** (in deze fork de parameter
  `SWAN_VERSION_NUMBER` in [src/swanmain.f90](../src/swanmain.f90)).

Het merge-base van beide lijnen is `43e9bbb`. De onderstaande punten zijn dus
lokale reparaties boven dezelfde 41.51-bronbasis, niet verschillen tussen twee
SWAN-releases. Bestandsnamen verwijzen naar de gemoderniseerde free-formbron in
deze fork; de TU Delft-bron is nog volledig op de `.ftn`-vorm gebaseerd
(19 `.ftn` + 57 `.ftn90` onder `src/`, geen enkel `.f90`) en wordt vóór het
compileren door `switch.pl` voorbewerkt.

De kwalificaties betekenen:

- **bewezen**: het defect is vóór de reparatie gereproduceerd en na de
  reparatie verdwenen, of met een geheugeninstrument aantoonbaar gemaakt;
- **bronmatig bewezen**: de fout volgt rechtstreeks uit de upstream-dataflow
  of een compilerdiagnose en de reparatie is gericht afgedekt, maar er is geen
  afzonderlijke vóór/na-meting van een gebruikerszichtbaar gevolg;
- **latent**: de TU Delft-code schendt een taal-, interface- of intern contract,
  maar het afgedekte normale pad derefereerde de foutieve waarde niet;
- **diagnostiek/build**: de berekening is niet veranderd, maar een ontbrekende
  waarschuwing, foutafhandeling of bouwroute is gerepareerd.

Samengestelde kwalificaties komen voor waar het defect zelf vaststaat maar het
gevolg op de beproefde decks uitbleef. De bewijskolom scheidt daarom steeds wat
**gemeten** is van wat uit de bron is **afgeleid**; waar geen vóór/na-meting van
het numerieke gevolg bestaat, wordt die ook niet geclaimd.

Twee reparaties (BF-01 en BF-13) bijten alleen wanneer één proces méér dan één
berekening uitvoert. De standalone 41.51-binary draait één case per proces; de
route erheen is een externe koppelaar — de upstreambron verwijst in het
commentaar bij `SWMAIN` naar het niet meegeleverde `couple2swan.F` — of de
bibliotheek-API van deze fork. Dat maakt ze niet minder echt, maar het bepaalt
wel wie er last van heeft.

## Geleverde bugfixes

| ID | Kwalificatie | Defect in de TU Delft-basis en lokale reparatie | Bewijs en commits |
|---|---|---|---|
| BF-01 | **bewezen** | **Een tweede berekening in hetzelfde proces kon crashen of invoer verkeerd interpreteren.** `LOGCOM(6)` wordt op `.TRUE.` gezet zodra `AC2` is gealloceerd (`swanpre1.ftn:4990`) en nergens teruggezet, terwijl `AC2` bij het afsluiten wordt vrijgegeven (`swanmain.ftn:9243`); een volgende run zag dus een array die niet meer bestond. Later bleef ook de door `REPARM` onthouden invoerbestandsnaam tussen runs staan. De dubbele allocatiestatus is vervangen door `ALLOCATED(AC2)` en de bestandsregistratie wordt bij iedere run gereset. De owner-`clear`-paden zijn bovendien idempotent gemaakt. Bereikbaar via een koppelaar of de bibliotheek-API, niet in de standalone binary. | De A–B–A-tests voor gelijke cases, wisselende fysica, roosters en spectra zijn bitgelijk. Zie `tests/test_two_cases.f90`, `tests/test_physics_reuse.f90`, `tests/test_grid_reuse.f90` en `tests/test_spectrum_reuse.f90`. Commits "Geef het roosteradres expliciet door en maak twee runs in een proces mogelijk", "Geef het roosteradres expliciet door en maak twee runs in een proces mogelijk" en de eigendomsstappen in "Leg de beoordeelde stand vast en maak de testbewaking eerlijk". |
| BF-02 | **bewezen met instrument, gevolg latent** | **25 optionele allocatable arrays werden ongealloceerd als argument doorgegeven.** Invoervelden worden pas gealloceerd zodra een `READINP`-commando ze aanlevert en de globale roosterarrays alleen bij een gestructureerd rooster, maar ze worden onvoorwaardelijk doorgegeven: of de data bestaat wordt bepaald door vlaggen als `LEDS`, nooit door `ALLOCATED`. Dat is ongedefinieerd Fortran-gedrag en stopt onder `-fcheck=all`. `SWINIT` geeft de 20 velden, de vier globale roosterarrays en `XYTST` nu een geldige lege toestand. | De volledige runtime-checksuite is groen en is negatief beproefd door één lege initialisatie tijdelijk weg te nemen. Het defect is dus met een instrument aantoonbaar; het gevolg bleef latent omdat het aangeroepen pad altijd eerst de vlag toetste en de array nooit uitlas. Zie `src/swanmain.f90` en `tests/test_input_fields.f90`. Commit "Maak de runtimecontroles bereikbaar en ruim de bouwhygiene op". |
| BF-03 | **bronmatig bewezen** | **Hetzelfde invoerveld opnieuw inlezen op een groter rooster schreef buiten de array.** Upstream alloceert eenmalig (`IF (.NOT.ALLOCATED(DEPTH)) ALLOCATE(DEPTH(MXG*MYG))`, `swanpre1.ftn:4484 e.v.`) en kopieert daarna met `CALL SWCOPR (TARR, DEPTH, MXG*MYG)`, waarin het doel als `REAL ARR2(LENGTH)` is gedeclareerd. Een tweede `READINP` voor hetzelfde veld op een grotere roostermaat behield stil de eerste maat en schreef het verschil voorbij het einde. `ENSURE_FIELD_SIZE` herschaalt het veld nu vóór het vullen. | De upstream-dataflow maakt de te kleine doelarray rechtstreeks aantoonbaar; een afzonderlijke vóór/na-deckmeting ontbreekt. De gerichte test vergroot `DEPTH` van twee naar vijf elementen en bewaakt de nieuwe extent. Die test pint de herschaalprimitieve zelf, niet de route erheen: zou `SREDEP` `ENSURE_FIELD_SIZE` ooit niet meer aanroepen, dan blijft dat onopgemerkt — een dekkingsgat op padniveau dat openstaat. Zie `src/swmod2.f90` (`ENSURE_FIELD_SIZE`), de aanroepen in `src/swanpre1.f90` en `tests/test_input_fields.f90`. Commit "Maak de runtimecontroles bereikbaar en ruim de bouwhygiene op". |
| BF-04 | **bronmatig bewezen** | **`MDIA LAMBDA` zonder niet-negatieve waarde rekende door met ongeïnitialiseerde spectrale grenzen.** Bij een lege lambdalijst werd `MDIA = 0`, waarna de lus over de quadruplets in `SWPRE4W` niet uitvoert en `MSC4MI`/`MSC4MA` en `MDC4MI`/`MDC4MA` uit ongeïnitialiseerde locals worden toegekend. De parser weigert nu een lege lambdalijst met een beëindigende invoerfout. | De statische dataflow en `-Wmaybe-uninitialized` tonen de fout; een afzonderlijke vóór/na-deckmeting ontbreekt. De tak zonder `LAMbda`-sleutelwoord loopt door op de default `MDIA = 6` (`swanmain.ftn:1155`), zodat de lege lijst de enige route naar `MDIA < 1` is. Zie `src/swanpre1.f90` en `doc/modern-fortran.md`. Commit "Breek de parsercyclus en geef de lezer zijn eigen diagnostiek". |
| BF-05 | **bewezen defect, numeriek latent op de beproefde decks** | **Een te kort `BOUNDSPEC ... SEGMENT` consumeerde het laatste grenspunt tweemaal.** Upstream bewaakt de puntenlus alleen in de tak zónder `[len]` (`IF (IKO.GT.KOUNTR) GOTO 360`, `swanpre2.ftn:3816`). Is `[len]` wél gegeven en is de keten uitgeput, dan waarschuwt de code `'Length of segment short, boundary values ignored'` en gaat vervolgens tóch de lus in, waar de laatste knoop van de al verbruikte keten opnieuw wordt gelezen: een dubbele entry voor hetzelfde grenspunt met een wegingsfactor groter dan één en een negatieve tegenweging. De lus wordt nu aan de kop bewaakt, zodat de code doet wat de melding zegt; de resterende invoer wordt nog wel gelezen om de invoerstroom uitgelijnd te houden. | Defect vastgesteld in de bron (de bewaking dekt maar één van beide takken); reparatie gemeten: gericht foutdeck eindigt schoon met behoud van de waarschuwing, nul severe fouten, nul Valgrind-meldingen, geldige V/C1-decks bitgelijk. Op het beproefde deck was de herschreven entry numeriek inert — er is dus **geen** vóór/na-verschil in Hsig aan te wijzen en dat wordt hier ook niet geclaimd. Zie `src/swanpre2.f90`. Commit "Isoleer de tests en sluit de geheugenlevensduur af". |
| BF-06 | **bewezen, latent voor huidige productiedecks** | **`ININTV` en `INITVD` konden op het `UNC`/`RQI`-geen-waarde-pad een ongeïnitialiseerde hulpwaarde naar de aanroeper schrijven.** Upstream roept `INREAL (NAME, RI, KONT, RSTA)` aan en berekent daarna onvoorwaardelijk `RVAR = FAC * RI`, terwijl `RI` op dat pad nooit is gezet. `RI` wordt nu met de bestaande `RVAR` gezaaid, precies conform het contract "waarde niet wijzigen". | De nieuwe contracttest was rood vóór de fix door echte stackrommel en is groen erna; de twee `-Wuninitialized`-meldingen zijn verdwenen. Alle huidige productieaanroepers gebruiken `STA` of `REQ` en blijven bitgelijk. Zie `src/ocpcre.f90`, `tests/test_contexts.f90`. Commit "Benoem de numerieke en fysische keuzes in plaats van indexen". |
| BF-07 | **bewezen** | **Gemengde netCDF-uitvoer kon na correcte uitvoer met een allocatorfout eindigen.** Twee gelijktijdige `BLOCK`-verzoeken voor hetzelfde grid, één tekst en één netCDF, gaven met `CALL FOR (NREF, TRIM(FILENM)//'.dum', 'UF', IOSTAT)` een kort stringresultaat aan een interface die het argument als `CHARACTER DDNAME*(LENFNM)` (140 tekens) declareert. Beide netCDF-aanroepen gebruiken nu een lokale buffer met de vereiste lengte; hetzelfde is voor de tabelroute gedaan. | `netcdf_mixed_block_output` reproduceert de voormalige crash; `netcdf_table_output` dekt de tweede aanroep. Valgrind rapporteert geen ongeldige leesactie meer. Zie `src/swanout2.f90`, `examples/output_backends/run.py` en `tests/check_netcdf_output.f90`. Commit "Bewaar en test de platformtakken en de uitvoerbackends". |
| BF-08 | **bewezen** | **Een ongestructureerde MPI-run zonder METIS segfaultte na een al fatale melding.** Na `MSGERR(4)` ontbrak een stopcontrole, waarna het ongepartitioneerde `ivertg` in diezelfde tak werd gederefereerd (`swanpre1.ftn:1532-1533` voor de `MSGERR(4)`, `1550` voor `I = ivertg(J)`). Een `IF (STPNOW()) RETURN` beëindigt het pad nu als normale SWAN-fout. | Op twee ranks veranderde de uitkomst van `SIGSEGV` naar exitcode 1 met de bedoelde melding en zonder `norm_end`; dezelfde case met METIS blijft groen. Zie `src/swanpre1.f90` en `tests/test_mpi_unstructured_partition.py`. Commit "Maak SWAN herbruikbaar binnen een proces en herstel de MPI-segfault zonder METIS". |
| BF-09 | **bewezen** | **De datum/tijdconversie las ongedefinieerde tekenposities.** `DATE_AND_TIME (TIMSTR(1:8), TIMSTR(10:20), ...)` vult niet heel `TIMSTR`, terwijl `DTSTTI` ook de scheider op positie 9 leest. De buffer wordt nu eerst met spaties gevuld. | Valgrind ging op dezelfde niet-stationaire case van één "conditional jump depends on uninitialised value" naar nul. Zie `src/ocpids.f90`. Commit "Leg de beoordeelde stand vast en maak de testbewaking eerlijk"; vóór/na-meting "Maak SWAN herbruikbaar binnen een proces en herstel de MPI-segfault zonder METIS". |
| BF-10 | **bewezen** | **`INTSTR` wijzigde tijdens formatteren het integerargument van de aanroeper.** De conversie rekent nu met een lokale kopie en het argument is `INTENT(IN)`. | Een leaf-modulefixture bewaakt dat het argument ongewijzigd blijft. Zie `src/swan_number_formatting.f90` en `tests/test_leaf_modules.f90`. Commit "Splits service- en fysicaroutines af in domeinmodules". |
| BF-11 | **bronmatig bewezen, diagnostiek** | **`SWPRTI` initialiseerde slechts 30 van zijn 33 timingrijen.** `TABLE` is als `(33,2)` gedeclareerd maar in een `DO K = 1, 30` genuld, terwijl de rijen 31 tot en met 33 — sea ice, Bragg scattering en QC scattering — verderop worden opgeteld en afgedrukt. Alle 33 rijen worden nu geïnitialiseerd. Het betreft de `!TIMG`-regels, die alleen actief zijn in een bouw met de timingschakelaar; zonder die schakelaar staat de hele routine uit. | De statische dataflow is eenduidig, zes `-Wmaybe-uninitialized`-meldingen verdwenen en de timingconfiguratie is groen; een vóór/na-meting van foutieve timinguitvoer ontbreekt. Zie `src/swanser.f90` en `doc/moderniseringsplan.md`. Opgenomen in de commit "Leg de beoordeelde stand vast en maak de testbewaking eerlijk". |
| BF-12 | **latent** | **Meerdere ongestructureerde aanroepen hadden ongeldige dummyargument-rangen of -vormen.** Voor `PLTSRC`, `SPREDT`, `SWTRCF`, `STRSSB`, `PEREXC`, `SWBIDW` en `BRKPAR` werden scalars als rank-2-placeholder doorgegeven, of arrays met twee elementen aan een dummy `RDX(MICMAX)`/`RDY(MICMAX)` — met `MICMAX = 13` als parameter, terwijl `SwanCompUnstruc` `real, dimension(2) :: rdx` doorgeeft. Niet gebruikte placeholders zijn `OPTIONAL` gemaakt en de werkelijk gebruikte tweeelementsvensters hebben een passende assumed-size-interface. | De routines lazen niet voorbij de tweede index, zodat geen foutieve waarde werd gederefereerd: **latent**. De expliciete module-interfaces maken de oude aanroepen compile-time ongeldig. De reguliere en ongestructureerde niet-stationaire referenties bleven bitgelijk. Commits "Geef de brontermen elk hun eigen module", "Geef propagatie en uitvoer elk hun eigen module", "Geef invoer, services, parallellisatie en driver elk hun eigen module" en "Geef invoer, services, parallellisatie en driver elk hun eigen module"; zie ook `doc/modern-fortran.md`. |
| BF-13 | **bewezen** | **Gekoppelde parser-, boundary-, obstakel- en uitvoerlijsten lekten geheugen.** Losse kopknooppunten werden nooit vrijgegeven (`OPSTMP` wordt in `swanpre2.ftn` achtmaal gealloceerd en nergens vrijgegeven) en van ketens alleen de laatste knoop (`IF (ASSOCIATED(TMP)) DEALLOCATE(TMP)`). De reparatie omvat `BFLTMP`/`BSTMP`/`BGPTMP`, `ORQTMP`/`OPSTMP`, `NEXTI`, meerdere `XYPT`-ketens en de obstakeltransmissie- en hoekpuntketens. Ook vijf foutpaden na `BCWAMN`/`BCWW3N`/`BCFILE` ruimen hun nog niet gekoppelde knoop en eventuele puntketen nu op. Relevant waar één proces meer dan één run doet; in een proces dat één case draait, ruimt het besturingssysteem bij afsluiten op. | Op de A–B–A-case daalde definitely/indirectly lost cumulatief van **2040 B naar 0 B** (216 + 792 + 936 + 96 B over de stappen). De afzonderlijke FILE-foutdecks daalden van 280 B, respectievelijk 328 B totaal verlies, naar 0 B. Het afsluitprotocol met 50 wisselende runs, een invoerfout en herstel slaagt zonder verloren blokken of open-unitgroei. Commits "Leg de beoordeelde stand vast en maak de testbewaking eerlijk", "Geef de gelekte lijstkoppen van de parser vrij", "Geef de gelekte lijstkoppen van de parser vrij", "Geef de gelekte lijstkoppen van de parser vrij", "Isoleer de tests en sluit de geheugenlevensduur af", "Isoleer de tests en sluit de geheugenlevensduur af" en "Isoleer de tests en sluit de geheugenlevensduur af". |
| BF-14 | **diagnostiek** | **De `DSPR DEGREES`-tak waarschuwde niet wanneer de richtingsspreiding te smal was voor het richtingsgrid.** Zo'n bundel kan vrijwel al zijn energie in de `SSHAPE`-discretisatie verliezen. Upstream heeft die bewaking alleen in de `POWER`-tak (`swanpre2.ftn:3639` en `3760`, criterium `SPPARM(4)*DDIR**2/2 > 1`). De `DEGREES`-tak heeft nu een eigen, empirisch gekalibreerd criterium (`DDIR > 2*SPPARM(4)*DEGRAD`) met dezelfde `LSPNAR`-latch. Omdat die latch gedeeld is, onderdrukt de eerste waarschuwing van één van beide takken die van de andere — zoals upstream het binnen de `POWER`-tak al deed. | Bij een spreiding van 2 graden op bins van 15 graden werd `Hs = 0,07` in plaats van circa 1,0 gemeten; bij 10 graden bleef `Hs = 0,999`. De numeriek bedoelde discretisatie is niet stil gewijzigd; geen groen deck geraakt. Zie `src/swanpre2.f90`. Commit "Valideer de fysica tegen analytische oplossingen". |
| BF-15 | **latent/interface** | **De METIS-C-aanroepen hadden geen expliciete interoperabele interface of controle op de gedeclareerde ABI-breedtes.** Upstream declareert ze als `integer(kind=kint), external :: METIS_PartGraphKway` en vertrouwt erop dat `kint` met `idx_t` uit de bibliotheek overeenkomt. Ze lopen nu via `ISO_C_BINDING`/`BIND(C)`. CMake controleert, wanneer `metis.h` op de gevonden include-locatie beschikbaar is, of die header 32-bit `idx_t` en 32-bit `real_t` declareert; andere gedeclareerde breedtes stoppen de configuratie. Dit bewijst niet zelfstandig dat een los gevonden bibliotheek met precies die header is gebouwd. | De daadwerkelijk gevonden 32/32-header/bibliotheekcombinatie, argumentdoorgifte, indexbereiken en echte tweeranks partitionering zijn getest. Een 64-bit-indexbibliotheek en een bewust niet-passende header/bibliotheekcombinatie waren niet beschikbaar en zijn daarom uitdrukkelijk niet gekwalificeerd. Zie `src/swan_metis_interface.f90`, `src/SwanParallel.f90` en `tests/test_metis_backend.f90`. Commit "Leg de beoordeelde stand vast en maak de testbewaking eerlijk". |
| BF-16 | **build** | **De Windows-CMake-route gaf bij GNU Fortran altijd de `CVIS`-schakelaar door** (`if( WIN32 ) set( SWITCHES -dos -cvis )` in `src/CMakeLists.txt`, zonder compilertoets). Die activeert de niet-standaard `SHARED`-specifier in de `OPEN`-statements en maakt een standaardconforme GNU-build onmogelijk, terwijl de GNU-runtime deze voor Windows-bestandsdeling niet nodig heeft. De standaardconforme bestandsbackend is nu de default; de bewaarde DEC-`SHARED`-backend wordt alleen via de expliciete capability-optie `SWAN_DEC_SHARED_IO` gekozen en niet meer automatisch op basis van Windows of “niet-GNU”. | Backendselectie en switchcompatibiliteit zijn statisch getest. Er was geen DEC-capabele Windows-compiler beschikbaar; een werkelijke bouw van de optionele `SHARED`-backend is daarom niet gekwalificeerd. Zie `CMakeLists.txt`, `src/platform/`, `tests/test_switch_compatibility.py` en `doc/switch-manifest.json`. Commit "Breid de voorbeelden uit en repareer de CVIS-schakelaar op Windows", later structureel opgenomen in de backendselectie. |

## Niet als bugfix meegeteld

De volgende verschillen met TU Delft zijn bewust niet in de tabel opgenomen:

- omzetting naar free-form Fortran, modules en expliciet toestandseigendom;
- vervanging van FFTPACK door FFTW3, de ruimtelijke index en andere
  prestatieverbeteringen;
- uitbreiding van CMake, CI, tests, voorbeelden en verificatietooling voor
  zover daarbij geen upstreamdefect is gerepareerd;
- verwijderde dode code en compilerwaarschuwingen zonder aangetoonde
  gedragsfout;
- **de ESMF-tak van `SWIND3`, `SWIND4` en `SWIND5`.** Die tak verwijst naar
  `KCGRD(1)`, en dat oogt als een verwijzing naar een argument dat sinds
  SWAN 40.00 uit deze routines is verdwenen. Dat is het niet: `KCGRD` is een
  modulevariabele van `SWCOMM3` (`swmod1.ftn:2078`, `THREADPRIVATE`), alle drie
  de routines doen `USE SWCOMM3`, en de waarde is het actuele roosterpunt. De
  tak was in de TU Delft-bron dus geldig en wees het juiste punt aan. Deze fork
  verving `KCGRD(1)` in deze routines door een expliciet `IGP`-argument,
  waardoor de dormante tak in de eigen boom moest meebewegen; daarbij is hij als
  gewone CMake-capability bouwbaar gemaakt en met een backendtest afgedekt
  (`src/coupling/`, `tests/test_esmf_coupling_backend.f90`). Dat is
  moderniseringsonderhoud, geen reparatie van upstream;
- de gewijzigde standaardfysica sinds SWAN 41.45. Dat is een bewuste
  upstreamwijziging. `GEN3 KOMEN DRAG FIT` is een compatibiliteitsinstelling om
  41.31-fysica te benaderen, geen bugfix in 41.51.

## Bekende open punten

Deze bevindingen zijn onderzocht maar nog niet opgelost en staan daarom niet
tussen de geleverde bugfixes:

- **kandidaat-upstreamdefect:** de niveaugrafiek die de wavefronts van de
  ongestructureerde OpenMP-solver bepaalt, ordent maar op één van de twee buren
  waarvan de solver `ac2` leest. De tweede buur (`vu(2)`) wordt wél berekend en
  nooit gebruikt, waardoor een vertex met open waaier — meshrand, kustlijn,
  obstakelsnede — in hetzelfde front kan belanden als een buur die hij leest
  terwijl die geschreven wordt. Dit is upstream-code
  (`0dabfa4:src/SwanVertlist.ftn90`, `!GRAPH`-blok, ongewijzigd door de
  modernisering). Het mechanisme is op een synthetische mesh aangetoond en met
  een test vastgelegd; de telling op de echte mesh, de correlatie met de
  waargenomen flips en het effect op de oplossing staan nog open, en daarmee ook
  of dit een reparatie of alleen een melding wordt. De operationele so-rp-suite
  is gestructureerd en wordt hier niet door geraakt. Zie
  `doc/moderniseringsplan.md`;
- een malafide `POINTS 'x' CURVE`-deck kan in Release incidenteel met
  `SIGABRT` eindigen in plaats van schoon te stoppen; bewezen pre-existing (de
  pre-repair-binary aborteert identiek) en heap-toestandsafhankelijk, zonder
  reproductie onder gdb, Valgrind of ASan;
- de operationele MPI-veldassemblage heeft 174 cellen die MPI droog en serieel
  nat rapporteert; de 132 gevraagde natte uitvoerpunten zijn wel bit-exact
  gelijk, maar het volledige veldcontract is nog niet afgesloten;
- de numerieke afwijking van de ongekwalificeerde Debug/`-O0`-route is
  vernauwd maar niet verklaard;
- resterende waarschuwingen uit `maybe-uninitialized`, `do-subscript` en
  `character-truncation` gelden als onderzoekskandidaten, niet als bewezen
  bugfixes.

Zie `doc/eindverantwoording.md` voor de actuele afbakening: wat wel en
niet geclaimd wordt.
