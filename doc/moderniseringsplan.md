# Moderniseringsplan — SWAN 41.51, RWS-fork

Dit plan beschrijft wat er in deze fork gaat gebeuren, in welke volgorde, en
waaraan elke stap moet voldoen voordat hij erin mag. Het is geschreven bij het
aftakken van TU Delft en het is het enige plan: alles wat daarna gecommit wordt,
voert dit plan uit.

Taal: Nederlands, net als de commitgeschiedenis. Broncodecommentaar en de
architectuurbeschrijving in [modern-fortran.md](modern-fortran.md) blijven
Engels, omdat die dicht tegen de upstream-code aan liggen en met TU Delft
gedeeld moeten kunnen worden.

---

## Inhoud

1. [Waarom deze fork bestaat](#1-waarom-deze-fork-bestaat)
2. [Wat we overnemen](#2-wat-we-overnemen)
3. [Doelen en niet-doelen](#3-doelen-en-niet-doelen)
4. [Harde randvoorwaarden](#4-harde-randvoorwaarden)
5. [Het fysicafeit dat alles kadert](#5-het-fysicafeit-dat-alles-kadert)
6. [Doelarchitectuur](#6-doelarchitectuur)
7. [Hoe we bewijzen dat er niets verandert](#7-hoe-we-bewijzen-dat-er-niets-verandert)
8. [Het werk in acht delen](#8-het-werk-in-acht-delen)
9. [Acceptatie per deel](#9-acceptatie-per-deel)
10. [Wat we bewust niet doen](#10-wat-we-bewust-niet-doen)
11. [Risico's en hoe we ze afdekken](#11-risicos-en-hoe-we-ze-afdekken)
12. [Release en upstream-import](#12-release-en-upstream-import)

---

## 1. Waarom deze fork bestaat

SWAN is het golfmodel waarmee Rijkswaterstaat onder andere de Voordelta- en
Oosterschelde-sommen draait. De code is dertig jaar oud, werkt, en is
wetenschappelijk goed onderhouden. Maar de vorm waarin hij staat maakt drie
dingen moeilijk die wij wél nodig hebben.

**Je kunt er niet met vertrouwen in wijzigen.** Grote delen staan in fixed-form
Fortran met impliciete typering, impliciete interfaces en gedeelde toestand in
moduleniveau-variabelen. De compiler kan bij een aanroep niet controleren of het
aantal en type van de argumenten klopt. Een verschrijving wordt dan geen
compileerfout maar een verkeerd getal, ergens, soms.

**Je kunt niet zien of je iets kapot hebt gemaakt.** Er is geen
regressietestsuite die de numerieke uitvoer vastpint. Dat een som draait zegt
niets; het gaat erom dat hij dezelfde golfhoogte uitrekent.

**Je kunt hem niet fatsoenlijk bouwen.** De bouw loopt via een Perl-script dat
compilerschakelaars als commentaarmarkeringen in de broncode zet en er per
variant een kopie uit genereert. Welke combinaties werkelijk werken is niet
vastgelegd en niet getest.

Het doel van deze fork is die drie dingen omdraaien, **zonder ook maar één
uitkomst te veranderen**. Aan het eind moet gelden: dezelfde invoer geeft
dezelfde uitvoer, de compiler controleert wat hij kan controleren, en een
testsuite laat het merken zodra dat niet meer zo is.

Dit is nadrukkelijk geen wetenschappelijk project. We verbeteren geen fysica, we
voegen geen formuleringen toe en we maken geen keuzes die het model anders laten
rekenen. We maken de code onderhoudbaar en de uitkomsten aantoonbaar.

## 2. Wat we overnemen

Vertrekpunt is TU Delft SWAN **41.51**, commit `43e9bbb` van 13 juli 2026,
remote `gitlab.tudelft.nl/citg/wavemodels/swan`. Die blijft als `upstream`
ingesteld staan; we willen latere releases kunnen binnenhalen.

Wat we erven:

| | |
|---|---|
| Bronbestanden | 76, waarvan 19 fixed-form (`.ftn`) en 57 vrije vorm (`.ftn90`) |
| Grootste bestanden | `swancom1.ftn` (576 KB), `swanmain.ftn` (436 KB), `swanpre1/2.ftn` (samen 675 KB) |
| Gedeelde toestand | Zestien moduleniveau-containers (`SWCOMM1`–`SWCOMM4`, `OCPCOMM1`–`OCPCOMM4`, `M_GENARR`, `M_PARALL`, `OUTP_DATA`, en verder), geïmporteerd door vrijwel elk bestand |
| `COMMON`-blokken | Vier, verdeeld over drie bestanden |
| Bouw | `Makefile` + `switch.pl`, die schakelaars als commentaar in de bron zet |
| FFT | Gebundelde FFTPACK 5.1 (`fftpack51.ftn90`, 425 KB) |
| Tests | Geen |

De gebundelde numerieke pakketten (`mod_xnl4v5`, `SdsBabanin`) zijn door derden
geschreven en horen daar; die raken we niet aan.

## 3. Doelen en niet-doelen

**Doelen.**

1. Alle broncode in vrije vorm, Fortran 2018, met `IMPLICIT NONE (TYPE, EXTERNAL)`
   zodat de compiler elke aanroep kan controleren.
2. Geen `COMMON`-blokken en geen gedeelde mutabele moduleniveau-toestand meer;
   toestand heeft een eigenaar en wordt expliciet doorgegeven.
3. Een CMake-bouw met echte opties in plaats van tekstsubstitutie in de bron, en
   een vastgelegde matrix van combinaties die daadwerkelijk gebouwd en getest
   worden.
4. Een regressiesuite die de numerieke uitvoer vastpint, draait in CI, en per
   bouwvariant groen is.
5. Een waarschuwingsratchet die terugval blokkeert: het aantal
   compilerwaarschuwingen mag alleen omlaag.
6. SWAN moet tweemaal in één proces kunnen draaien met verschillende invoer en
   beide keren het goede antwoord geven — de eis die volgt uit inbedding in een
   grotere keten.
7. Een operationele verificatiebasis op de so-rp/Voordelta-case, zodat we kunnen
   aantonen dat de gemoderniseerde binary doet wat de productiebinary doet.

**Niet-doelen.**

- Geen fysicawijzigingen, geen nieuwe formuleringen, geen gewijzigde defaults.
- Geen wijziging van invoersyntaxis of uitvoerformaten. Bestaande decks blijven
  werken, bestaande naverwerking blijft werken.
- Geen herschrijving van de gebundelde numerieke pakketten.
- Geen GPU-tak, geen alternatieve solver, geen nieuwe parallellisatiestrategie.
- Geen cosmetische herschrijving van code die verder met rust gelaten kan worden.
  Elke aangeraakte regel is een regel die bij de volgende upstream-merge
  conflicteert.

## 4. Harde randvoorwaarden

Deze gelden voor élke stap. Een stap die er niet aan voldoet gaat niet in.

1. **Geen wijziging van fysica, defaults, invoersyntaxis of uitvoerformaten.**
   Als een wijziging de uitkomst verandert, is hij fout — tenzij hij expliciet
   als correctheidsreparatie is aangekondigd, met bewijs waarom het oude gedrag
   fout was.

2. **Stop-anywhere.** Elke commit eindigt in een uitleverbare toestand. Geen
   half-gemigreerde module, geen twee gesynchroniseerde kopieën van dezelfde
   toestand, geen permanent dubbel aanroeppad. Als het werk morgen stopt, moet
   wat er staat bruikbaar zijn.

3. **Eén afgebakend subsysteem per stap**, met al zijn gebruikers in dezelfde
   stap. Een migratie die de helft van de consumers meeneemt is geen migratie.

4. **Upstream-mergekosten zijn een ontwerpcriterium.** De grote
   verzamelbestanden krijgen upstreamwijzigingen. Nieuwe infrastructuur komt
   daarom bij voorkeur in nieuwe bestanden; regelverplaatsingen worden
   geminimaliseerd. Een hernoeming die tien regels bespaart en duizend regels
   conflict oplevert, doen we niet.

5. **Referentievergelijking** voor serieel, OpenMP en MPI, plus de relevante
   schakelvarianten. Statistieken altijd over een expliciet natmasker; droge
   punten dragen SWAN's uitzonderingswaarde en horen niet in een gemiddelde.

6. **Beide fysicaconfiguraties bewaakt**: de 41.51-default en de
   41.31-compatibiliteit `GEN3 KOMEN DRAG FIT` (zie §5).

7. **Runtimecontroles, LTO en de waarschuwingsratchet blijven slagen.** Een stap
   die een van deze drie rood maakt, is niet af.

8. **Prestatie meten voor en na.** Een taalconstructie is geen optimalisatie
   zonder meetbare winst. Een claim zonder meting wordt ingetrokken, niet
   uitgelegd.

9. **Gebundeld numeriek werk** alleen wijzigen voor aantoonbare correctheid,
   nooit voor cosmetiek.

10. **Alle code blijft bouwen met GNU, Intel en waar mogelijk Flang** in
    F2018-modus. Takken die deze omgeving niet kan draaien worden bewaard en
    gedocumenteerd, niet stilzwijgend gesloopt.

## 5. Het fysicafeit dat alles kadert

Voordat er één regel verandert, moet dit vastliggen, omdat het anders elke
vergelijking met de operationele praktijk vergiftigt.

RWS draait operationeel op **SWAN 41.31** (binary BSS 41.31A.1). Deze fork staat
op **41.51**. Tussen die twee releases heeft TU Delft in release 41.45 de
**ingebouwde default-brontermen omgezet**. Een deck zonder expliciet
`GEN`-commando rekent in 41.51 dus met andere fysica dan in 41.31:

1. Diepwaterfysica: **Van der Westhuysen et al. (2007)** in plaats van Komen et
   al. (1984) — windinvoer van Yan, verzadigingsgebaseerde whitecapping van
   Alves-Banner.
2. Windsleep: **Wu (1982)** in plaats van de tweede-orde polynoomfit.
3. Triads: **DCTA** in plaats van LTA, met bifase-parameter 0,63 in plaats van 0,2.

Het gevolg is niet klein. Op de so-rp-conditie (20 m/s uit 310°, NAP +3,00 m),
gemiddeld over de 132 natte uitvoerpunten:

| Fysica | gemiddelde Hsig | iteraties |
|---|---|---|
| 41.51-default (Westhuysen) | 1,6453 m | 35 |
| `GEN3 KOMEN` | 1,3607 m | 32 |
| `GEN3 KOMEN DRAG FIT` | 1,2911 m | 30 |
| Echte 41.31 | 1,2901 m | 30 |

Dat is 28 % verschil in golfhoogte op dezelfde invoer. De regel die daaruit
volgt en die we overal aanhouden:

> **Wie 41.31-gedrag wil reproduceren, zet `GEN3 KOMEN DRAG FIT` in het deck.
> Alleen `GEN3 KOMEN` is niet genoeg** — dat laat ongeveer een vijfde van het
> verschil staan, want de default voor de windsleep veranderde óók.

Er zijn drie redenen waarom dit een randvoorwaarde is en geen voetnoot. Ten
eerste: elke vergelijking tussen deze fork en de operationele binary is
betekenisloos zonder dat commando. Ten tweede: de verleiding om een verschil van
28 % aan "de modernisering" toe te schrijven is groot, en zou ons maanden kosten.
Ten derde: de regressiesuite moet **beide** configuraties bewaken, want een
migratie mag geen van beide paden verstoren.

Dit feit wordt aangetoond, niet aangenomen: we bouwen upstream 41.31 vers, laten
zien dat die binary gelijk is aan de operationele BSS-binary (dus geen verborgen
RWS-patch), en doen daarna de decompositie van het gat.

## 6. Doelarchitectuur

### 6.1 Run en thread zijn twee dingen

De voor de hand liggende aanpak — alle gedeelde toestand in één groot
`swan_case_t` stoppen — is fout, en we voeren hem niet uit. Hij maakt per-thread
scratchgeheugen weer gedeeld en breekt daarmee precies de OpenMP-parallellisatie
die nu werkt. Het juiste model splitst run en thread:

```fortran
type :: swan_run_t
   type(case_state_t)                    :: case
   type(thread_workspace_t), allocatable :: thread(:)
end type swan_run_t
```

De twee solvers — gestructureerd en ongestructureerd — gebruiken niet dezelfde
thread-toestand. De workspace is daarom samengesteld, niet uniform:

```fortran
type :: common_thread_seed_t
   ! wat beide solvers delen: puntindex, breedtegraadcosinus, teststatus
end type

type :: source_workspace_t
   ! per-punt brontermtoestand
end type

type :: structured_thread_workspace_t
   type(common_thread_seed_t) :: seed
   type(source_workspace_t)   :: source
   ! propagatiescratch, alleen gestructureerd
end type

type :: unstructured_thread_workspace_t
   type(common_thread_seed_t) :: seed
   type(source_workspace_t)   :: source
   ! vertexscratch, alleen ongestructureerd
end type
```

De orkestratie kiest de workspace voor de actieve thread en geeft die expliciet
door. **Rekenkernen roepen nooit zelf `omp_get_thread_num()` diep in de
callstack aan.** Een procedure krijgt de kleinste deelcontext die hij nodig
heeft, niet het hele runobject.

### 6.2 Zes categorieën, één behandeling per categorie

Elke moduleniveau-variabele die we aanraken krijgt precies één van deze zes
labels. Zonder die classificatie is een migratie gokwerk.

| # | Categorie | Behandeling |
|---|---|---|
| 1 | Gedeelde, alleen-lezen rundata | Tabel in `case_state_t`, één eigenaar |
| 2 | Gedeelde, muteerbare rundata | Expliciete setter of rebuild, één mutatiepad |
| 3 | Thread-toestand gekopieerd vanuit de master | Veld in `common_thread_seed_t`, geseed bij regio-ingang |
| 4 | Bewezen write-before-read scratch | Oningevuld werkveld |
| 5 | Conditioneel geschreven of voortlevende thread-toestand | **Eerst invarianttest, dan pas migreren** |
| 6 | Solver- of schakelaarspecifieke thread-toestand | In de solverspecifieke workspace |

Categorie 5 is de gevaarlijke. Een migratie die zulke velden stilzwijgend
initialiseert kan tegelijk een latente fout repareren én bestaand gedrag
veranderen. Dan weet je van geen van beide meer of het klopt. Zulke gevallen
worden een aparte, expliciet aangekondigde correctheidswijziging — nooit een
bijvangst van een refactor.

### 6.3 Bestandsindeling

Nieuwe modules komen in nieuwe bestanden, gegroepeerd per subsysteem
(`src/output/`, `src/parallel/`, `src/propagation/`, `src/platform/`,
`src/coupling/`, `src/hcat/`). De grote upstream-verzamelbestanden blijven
bestaan en krimpen alleen waar dat echt moet, om de mergekosten te beheersen.

### 6.4 Schakelaars worden bouwopties

`switch.pl` zet schakelaars als commentaarmarkeringen in de bron. Dat vervangen
we door echte CMake-opties en echte Fortran. Waar een tak niet zomaar naar
gewone code om te zetten is — platformspecifieke I/O, een vreemde
MPI-koppeling — isoleren we hem achter een backendmodule met één interface en
twee implementaties, zodat beide takken gewoon compileren.

Welke combinaties ondersteund zijn, wordt vastgelegd in een manifest en getest.
Niet-ondersteunde combinaties falen luid bij het configureren, niet stil bij het
rekenen.

## 7. Hoe we bewijzen dat er niets verandert

Dit is het belangrijkste hoofdstuk van het plan. Zonder bewijsvoering is elke
refactor een gok.

### 7.1 Vastgepinde uitvoer

Voor elke voorbeeldcase leggen we de numerieke uitvoer vast als referentie in
versiebeheer. Een run die niet crasht bewijst niets; de tabel- en blokbestanden
zijn het model. Vergelijking gaat veldsgewijs met een relatieve tolerantie die
ruim genoeg is voor de laatste-cijferspreiding van de ongestructureerde solver
onder OpenMP, en strak genoeg dat elke wijziging met fysische betekenis faalt.

Drie regels die we niet overtreden:

- **Een referentie wordt nooit automatisch bijgewerkt om een poort groen te
  krijgen.** Bijwerken is een bewuste handeling met een eigen commit en een
  eigen verantwoording.
- **Een ontbrekende referentie is een fout, geen overslaan.** Als de suite een
  referentie niet vindt, faalt hij. Anders kun je dekking verliezen zonder dat
  iemand het merkt.
- **Referentiebestanden moeten daadwerkelijk in versiebeheer staan.** De
  negeerpatronen voor SWAN-uitvoer (`*.tbl`, `*.blk`, `PRINT`, `Errfile`) lijken
  sterk op de namen van de referenties zelf. Na het toevoegen van een referentie
  wordt gecontroleerd dat een verse checkout hem heeft.

Die laatste regel staat er omdat het anders precies fout gaat: het negeerpatroon
slikt het referentiebestand, de commit lijkt te slagen, de test blijft lokaal
groen op een bestand dat alleen op de eigen schijf staat, en pas iemand anders
ontdekt dat de poort leeg is.

### 7.2 De tolerantie moet bij het bestandstype passen

Een relatieve tolerantie werkt op tabeluitvoer, waar alle getallen ongeveer
dezelfde grootteorde hebben. Op spectrale uitvoer werkt hij niet: zo'n bestand
loopt van tienduizenden tot 10⁻⁶ in dezelfde kolom, en SWAN drukt met vier
significante cijfers af. Een verschil van één in het laatste afgedrukte cijfer
is dan geen resultaatverschil maar een afdrukverschil, terwijl het relatief
boven elke redelijke drempel uitkomt.

Een case die zulke uitvoer vergelijkt krijgt daarom een eigen tolerantie voor
het betreffende bestand, met de meting erbij: hoe groot de spreiding tussen twee
bouwen werkelijk is, en hoe groot de verschuiving is die de case moet vangen.
De gedeelde poort van 1e-3 blijft de regel; verruimen mag alleen per bestand,
alleen met een gemeten reden, en alleen als er een ruime marge blijft tot het
signaal. Een echte fysicawijziging verschuift getallen met procenten, dus zo'n
verruiming maskeert niets wat we willen zien.

Dat is iets anders dan een generieke tolerantie om een lastige bouwvariant groen
te krijgen. Die doen we niet — zie §10.

### 7.3 De waarschuwingsratchet

We bouwen met de strenge waarschuwingsset van GNU Fortran en tellen de
waarschuwingen per categorie. Het aantal mag alleen omlaag. Twee niveaus:

- **Budget per categorie.** Exact, niet royaal: elke categorie krijgt precies
  het huidige aantal. Een stijging is een fout.
- **Vingerafdrukken per bestand.** Een budget ziet geen omruil — één
  waarschuwing weg en één erbij in dezelfde categorie blijft onopgemerkt. Elke
  waarschuwing wordt daarom gereduceerd tot (categorie, bestand, boodschap),
  bewust zonder regelnummer, zodat verplaatsen en herformatteren onzichtbaar
  zijn en alleen een echt nieuwe waarschuwing opvalt.

Als het aantal daalt, wordt de basislijn in dezelfde stap opnieuw vastgelegd.
Slack laten staan is hetzelfde als de ratchet uitzetten.

### 7.4 Manifesten voor wat de compiler niet ziet

Drie eigenschappen kan geen compiler controleren, dus leggen we ze vast in een
manifest met een script dat de bron ertegen controleert:

- **Thread-private toestand** — welke variabelen `threadprivate` zijn, en waarom.
- **Eigendom van runtoestand** — wie een veld mag schrijven, en waar het wordt
  opgeruimd.
- **Schakelaarcombinaties** — welke bouwvarianten ondersteund zijn.

Daar komt een vierde bij zodra we argumentgroepen bundelen: de grens van zo'n
bundel mag niet stilletjes verbreden, dus een script bewaakt welke procedures
hem mogen ontvangen.

### 7.5 Poorten in CI

Zes verplichte poorten, met een afsluitende controle die alleen slaagt als alle
zes een geslaagd resultaat hebben afgeleverd:

| Poort | Wat hij afdekt |
|---|---|
| `serial` | Release-bouw, volledige CTest, installatieproef, case vanuit de installatie |
| `openmp` | OpenMP-bouw, volledige CTest, determinisme-smoke ongestructureerd |
| `runtime` | Bouw met compiler-runtimecontroles bovenop optimalisatie |
| `strict` | Schone strenge bouw, budget en vingerafdrukken |
| `mpi-2rank` | Echte tweerangs-MPI-regressies |
| `python-manifest` | Manifestcontroles en de Python-testsuite |

Een workflowbestand alleen dwingt niets af — een ontbrekende job valt niet op.
De afsluitende controle wél: die kent de lijst van vereiste resultaten en faalt
op een ontbrekend artefact.

### 7.6 Gerichte poortselectie

Niet elke wijziging hoeft de volledige breedte. Voor lokaal werk bepaalt een
script welke poorten een wijziging kán breken, op grond van de geraakte paden:

- MPI alleen bij MPI-rakende paden. Een deck- of fysicawijziging zonder
  codeverschil gedraagt zich onder MPI identiek aan serieel.
- Strict alleen bij bron-, toolchain- of budgetwijzigingen.
- Alleen documentatie: geen poorten.
- Onbekende paden kiezen veilig alles; nooit stil niets.

Overgeslagen poorten blijven staan op hun laatste groene commit, en die commit
wordt bij de stap vermeld. In CI draait altijd alles.

### 7.7 De operationele verificatiebasis

Naast de voorbeeldcases bouwen we een verificatiebasis op de echte
so-rp/Voordelta-case: een conditiematrix, een deckgenerator die per conditie een
deck schrijft, de randvoorwaardeketen, en een analyse die veld- en
puntstatistiek strikt gescheiden houdt met expliciete natmaskers. Dat is wat
uiteindelijk de vraag beantwoordt of deze fork operationeel bruikbaar is.

## 8. Het werk in acht delen

De volgorde is niet vrij. Je kunt geen toestand migreren voordat je kunt zien of
je iets kapotmaakt, en je kunt niets zien zonder bouw en tests. Vandaar deze
opbouw: eerst kunnen bouwen, dan kunnen meten, dan pas verbouwen.

### Deel I — Fundament

Van 76 upstream-bestanden naar een repository die met één commando bouwt.

- Negeerpatronen voor bouw- en runtime-artefacten, zodat uitvoer van een run
  niet per ongeluk in versiebeheer komt.
- Alle bronbestanden naar `.f90`, eerst als pure hernoeming zodat de
  geschiedenis van elk bestand intact blijft en de omzetting daarna leesbaar is.
- FFTPACK 5.1 vervangen door een FFTW3-backend achter een compatibiliteitslaag,
  zodat de 425 KB gebundelde FFT-code verdwijnt zonder dat aanroepers wijzigen.
- Fixed-form omzetten naar vrije vorm. Mechanisch, met zorg voor de
  valkuil die er is: een fixed-form regel die op kolom 72 wordt afgekapt
  verliest zijn staart, en bij commentaar merkt de compiler dat niet.
- Een CMake-bouw met echte opties.
- Voorbeeldcases met runners: een snelle rooktest, een niet-stationaire case en
  de Voordelta-case.

### Deel II — Meten en versnellen

Voordat we de structuur aanpakken, leggen we de prestatiebasis vast, want daarna
is elke vertraging een verdachte.

- Een benchmark tegen upstream, serieel en multicore, met opgeslagen meting.
- De quadruplet-brontermen versnellen door de nulstelling te beperken tot het
  deel van de array dat werkelijk gebruikt wordt, en de bronterm-lus te splitsen.
- De actielimiter in de rekenkern herstructureren.
- De FFT-compatlaag uitgelijnde plannen laten gebruiken.

Elke versnelling wordt gemeten, voor en na, en de meting wordt bewaard. Wat niet
meetbaar sneller is, gaat er weer uit.

### Deel III — Modulestructuur en contexten

Dit is de grootste structurele ingreep. Doel: elke procedure zit in een module,
zodat de compiler zijn interface kent.

- De diagnostiek eerst. `MSGERR`, `STRACE` en `STPNOW` worden door vrijwel alles
  aangeroepen en zitten daardoor in elke modulecyclus. Ze krijgen een optioneel
  contextargument, zodat een aanroeper zijn eigen foutkanaal en logstroom kan
  meegeven, en daarna kunnen ze moduleprocedures worden zonder cyclus.
- De bestandsopener gaat door een I/O-context; de commandolezer krijgt zijn
  eigen invoerstroom, diagnostiek en logstroom. Dat is wat straks twee runs in
  één proces mogelijk maakt.
- Subsystemen worden afgesplitst in modules: windbrontermen, dissipatie,
  niet-lineaire interacties, propagatie en transport, blok- en spectra-uitvoer,
  uitvoerorkestratie, invoerverwerking, services, parallelle synchronisatie, de
  top-level driver, de rekenorkestratie en de Ocean Pack-initialisatie.
- Elk los `Swan*`-bestand krijgt een eigen module.
- De laatste externe procedures worden moduleprocedures en de parsercyclus wordt
  gebroken.

Regel bij elke splitsing: de module krijgt alle consumers in dezelfde stap mee.
Geen tussentoestand met twee aanroeppaden.

### Deel IV — Uitvoer vastpinnen

Nu de structuur beweegt, moeten de getallen vastliggen.

- Referenties voor de rooktest, de niet-stationaire cases en de
  schakelaarvarianten. Na het toevoegen controleren dat een verse checkout ze
  heeft (§7.1).
- De veldsgewijze vergelijking met tolerantie, gedeeld door alle runners zodat
  ze niet uit elkaar groeien.
- De waarschuwingsratchet, eerst op totaal, daarna per categorie, daarna met
  vingerafdrukken tegen omruil.
- Unit-fixtures voor de pure bladmodules en gerichte tests voor de rekenkernen
  onder elk resultaat.
- Runtimecontroles als bouwoptie beschikbaar maken.

### Deel V — Operationele verificatiebasis

- De so-rp-tooling in versiebeheer: conditiematrix, deckgenerator,
  randvoorwaardeketen, analyse.
- Veld- en puntstatistiek scheiden met expliciete natmaskers. Dit is geen
  detail: middelen over alle uitvoerpunten inclusief droge punten geeft
  kunstmatig lage waarden, want een droog punt draagt SWAN's uitzonderingswaarde
  −9.
- De bytes van de operationele invoer bewaren, zodat de vergelijking
  reproduceerbaar is.
- Het GEN3-bewijs uit §5 leveren, met plots en een script dat het reproduceert.

### Deel VI — Gedeelde toestand ontmantelen

Het eigenlijke werk, en het gevaarlijkste. Volgorde: eerst zien, dan afdekken,
dan migreren.

- De thread-private toestand inventariseren en met een manifest bewaken.
- `IMPLICIT NONE (TYPE, EXTERNAL)` overal, zodat impliciete externals uitgesloten
  zijn.
- De F2018-contractlaag compleet maken: `intent` op elk argument, `pure` waar het
  kan.
- De afgebakende kleine containers migreren naar expliciete typen met een
  eigenaar, één voor één, elk met zijn eigen acceptatie.
- De dode variabelen verwijderen in plaats van ze mee te migreren. Wat niemand
  leest hoeft geen type.
- De grote containers opheffen: `OCPCOMM2`, `OCPCOMM3`, `OCPCOMM4`, `SWCOMM1`
  t/m `SWCOMM4`, `M_GENARR`, `M_PARALL`, `OUTP_DATA`. Elk wordt verdeeld over
  gerichte modules met een duidelijke inhoud, niet verplaatst naar één nieuwe
  verzamelbak.
- Het roosteradres expliciet als argument doorgeven in plaats van uit gedeelde
  stenciltoestand te lezen, voor alle rekenroutines.
- Timing en invariantcontroles worden gewone Fortran met compile-time
  constanten, in plaats van schakelaargestuurd commentaar.
- De acceptatietest die dit hele deel rechtvaardigt: **twee runs in één proces**,
  met verschillende invoer, beide correct.

### Deel VII — Schakelaars en backends

- Platformpaden vervangen door bouwconfiguratie.
- De Cray- en SGI-I/O-takken bewaren achter een optie én testen, zodat ze niet
  bitrotten.
- De METIS-partitionering, de netCDF-uitvoer en de Matlab-uitvoerbackends
  isoleren achter één interface, met echte end-to-end-tests.
- De parallelle hotstart-route afdekken.
- Het resterende schakellandschap vastleggen in het manifest, met een
  ondersteuningsmatrix die zegt wat wel en niet geclaimd wordt.

### Deel VIII — Releasekandidaat

Het laatste deel is geen refactor maar een beoordeling: is dit uit te leveren?

- De stand vastleggen en reproduceerbaar maken, met een herleidbare kandidaat.
- De testbewaking eerlijk maken: geen test die slaagt zonder te vergelijken.
- Mogelijke fouten onderzoeken, dode procedures en ongebruikte parameters
  verwijderen.
- Geheugenlevensduur op orde brengen: elk lek dat een Valgrind-run laat zien,
  met bewijs voor en na.
- De bibliotheekroute: SWAN moet als bibliotheek herbruikbaar zijn binnen één
  proces, bij wisselende fysica, wisselend rekenrooster en wisselende
  spectrumafmetingen.
- De fysicavalidatie verbreden met analytische proeven: shoaling tegen Green,
  refractie tegen Snellius, roosterconvergentie tegen eerste-ordetheorie,
  tijdstapgevoeligheid.
- Argumentgroepen bundelen waar procedures tientallen losse waarden
  doorgeven, bit-identiek, met een grensbewaker zodat de bundel niet verbreedt.
- De bugfixes ten opzichte van upstream inventariseren, zodat ze terug kunnen
  naar TU Delft.
- Dekkingsgaten dichten die de inventarisatie aan het licht brengt.

## 9. Acceptatie per deel

| Deel | Klaar als |
|---|---|
| I | De repository bouwt met CMake op GNU; de rooktest draait en produceert uitvoer |
| II | Benchmark tegen upstream vastgelegd; elke versnelling gemeten voor en na |
| III | Elke procedure zit in een module; geen impliciete interfaces meer behalve de expliciet vastgelegde uitzonderingen |
| IV | Elke voorbeeldcase heeft een referentie in versiebeheer; verse checkout is groen; ratchet actief met budget én vingerafdrukken |
| V | Het GEN3-verschil is gedecomponeerd en verklaard; de operationele binary is gelijk aan vers gebouwde upstream 41.31 |
| VI | Geen `COMMON`-blokken, geen gedeelde mutabele moduleniveau-toestand; twee runs in één proces geven beide het goede antwoord |
| VII | Elke ondersteunde schakelcombinatie bouwt en heeft een test; de ondersteuningsmatrix klopt met de werkelijkheid |
| VIII | Alle zes CI-poorten groen op één commit; open punten expliciet benoemd in plaats van weggelaten |

Voor alle delen geldt bovendien de generieke eis: **een verse `git clone`,
`cmake`, `ctest` is groen.** Niet "groen op mijn machine".

## 10. Wat we bewust niet doen

Deze opties zijn overwogen en afgewezen. Ze staan hier zodat ze niet opnieuw
opduiken.

- **Eén `swan_case_t` met één workspace per case.** Maakt per-thread scratch
  weer gedeeld en breekt de OpenMP-parallellisatie. Afgewezen ten gunste van het
  model in §6.1.
- **Alle gedeelde toestand naar één nieuwe verzamelmodule.** Dat verplaatst het
  probleem en levert niets op. Containers worden opgeheven en verdeeld over
  gerichte modules, of ze blijven staan.
- **De grote verzamelbestanden opsplitsen omdat ze groot zijn.** De omvang is
  vervelend, maar elke verplaatste regel is een mergeconflict bij de volgende
  upstream-release. Splitsen alleen waar een migratie het afdwingt.
- **Het gebundelde numerieke werk moderniseren.** Niet van ons, en er is niets
  mis mee.
- **Een generieke tolerantie om een lastige bouwvariant groen te krijgen.** Als
  een variant afwijkt, is dat een bevinding die verklaard moet worden, geen
  parameter die opgerekt moet worden.
- **Een GPU-tak.** Buiten scope.
- **Claims zonder meting.** Een verklaring voor een prestatieverschil die niet
  door een meting gedragen wordt, wordt ingetrokken en vervangen door de meting.

## 11. Risico's en hoe we ze afdekken

**Het grootste risico is een stille numerieke wijziging.** Een migratie die een
niet-geïnitialiseerd veld voortaan op nul zet, verandert de uitkomst zonder dat
iets faalt — behalve de referentievergelijking, mits die er is en mits ze in
versiebeheer staat. Afdekking: Deel IV komt vóór Deel VI, en de
categorie-5-regel uit §6.2 geldt onverkort.

**Het tweede risico is bewijs dat er niet is.** Een test die een referentie niet
vindt en daarom overslaat, een poort die niet draait omdat een job ontbreekt,
een budget dat slack heeft gehouden nadat het aantal daalde — alle drie zien
eruit als groen. Afdekking: ontbrekende referentie is een fout, de afsluitende
CI-controle kent de vereiste lijst, en basislijnen worden bij elke daling
opnieuw vastgelegd.

**Het derde risico zijn de takken die deze omgeving niet kan draaien** —
Intel, Flang, Cray, SGI, Windows. Afdekking: ze worden bewaard achter een optie,
ze blijven compileren waar dat kan, en de ondersteuningsmatrix zegt eerlijk wat
wel en niet beproefd is. Niet-beproefd is niet hetzelfde als ondersteund.

**Het vierde risico is de upstream-merge.** Elke aangeraakte regel kost bij de
volgende release. Afdekking: randvoorwaarde 4, en een inventaris van de
reparaties boven de upstream-bronbasis zodat ze terug kunnen.

**Het vijfde risico is de fork die onopgemerkt anders rekent dan productie.**
Afdekking: §5, en de operationele verificatiebasis uit Deel V.

## 12. Release en upstream-import

De fork houdt het versienummer van upstream aan: dit is SWAN 41.51, niet een
eigen versielijn. Wat wij toevoegen is bouw, structuur en bewijs — geen
modelgedrag.

Een releasekandidaat is pas een kandidaat als alle zes CI-poorten groen zijn op
**dezelfde commit**, en als expliciet is opgeschreven wat níét geclaimd wordt.
Een kandidaat met open punten is prima; een kandidaat die zijn open punten
verzwijgt niet.

Voor het binnenhalen van een nieuwe upstream-release geldt: `upstream` blijft
ingesteld, de reparaties boven de upstream-bronbasis staan geïnventariseerd, en
de regressiesuite is wat de merge beoordeelt. Als de suite na een merge groen
is, is de merge goed; als hij rood is, is dat een bevinding en geen reden om de
suite aan te passen.
