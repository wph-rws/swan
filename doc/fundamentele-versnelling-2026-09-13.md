Mijn eerste keuze voor forse versnelling is een betere stationaire solver,
gecombineerd met hergebruik tussen condities. De interessantste concrete
ontdekking in deze code is dat de Komen-whitecapping een veel eenvoudiger
volledige afgeleide heeft dan een algemene spectrale bronterm. De tweede is
dat de DIA exact schaalbaar is bij gelijkblijvende spectrale vorm.

Dit zijn voorstellen op basis van brononderzoek en enkele nieuwe proeven,
geen gerealiseerde versnellingen van de operationele suite. De afleidingen
hieronder zijn eigen analyse van deze broncode; de aangehaalde literatuur
onderbouwt de algemene numerieke methoden. Er is geen onderzoek gedaan naar
publiceerbare wetenschappelijke nieuwheid.

**Wat daadwerkelijk is gemeten.** De proef gebruikt het bestaande reguliere
Voordelta-demonstratiedeck: 18.471 roosterpunten, 36 richtingen en 25
frequenties. De uitgevoerde velden hebben 9.072 natte uitvoerpunten. Alle
vergelijkingen houden rooster, randvoorwaarden, richting, waterstand en
`GEN3 KOMEN` gelijk; alleen de genoemde numerieke keuze of windsnelheid
verandert. Dit is dus geen vergelijking tussen verschillende fysicapakketten.

| Proef | Tijd | Iteraties | Betekenis |
|---|---:|---:|---|
| U=15, DIA per sweep, `QUADRUPL 2` | 26,90 s | 25 | Referentie |
| U=15, DIA per iteratie, `QUADRUPL 3` | 23,59 s | 25 | 1,14× sneller in deze ene meting |
| U=16, gewone initialisatie | 25,53 s | 25 | Zelfde stopcriteria als hieronder |
| U=16, warmstart uit U=15 | 6,15 s | 6 | 4,15× sneller voor deze vervolgsom |
| U=16, strenge Hs- en periodecriteria, koud | 199,06 s | 200 | Niet geconvergeerd; 10,85% voldoet |
| U=16, dezelfde strenge criteria, warm | 200,42 s | 200 | Niet geconvergeerd; 10,91% voldoet |
| U=16, alleen aangescherpte Hs-criteria, koud | 99,21 s | 100 | Niet geconvergeerd; 85,17% voldoet |
| U=16, dezelfde Hs-criteria, warm | 104,45 s | 100 | Niet geconvergeerd; 93,64% voldoet |

De DIA-variant heeft tegenover de referentie een Hs-RMSE van 9,4 mm, een
maximale lokale afwijking van 11 cm en 2,90% verschil in de integraal van
de absolute spectrale afwijkingen, gewogen met energiegewichten. De
warmstart heeft tegenover de koude U=16-run een Hs-RMSE van 12,5 mm,
maximaal 3,9 cm verschil en 2,43% spectraal verschil volgens diezelfde norm.
De viermaal kortere tijd bewijst daarmee nog geen viermaal snellere oplossing
met dezelfde nauwkeurigheid.

De twee runs van 200 iteraties zijn geen geconvergeerde referenties. Hun
onderlinge Hs-RMSE is nog 8,1 mm en hun globale spectrale verschil 3,21%.
Meer iteraties alleen levert in deze proef geen overtuigend eenduidige
eindoplossing op. Zonder balansresidu is de oorzaak daarvan niet vastgesteld.
Ook een mildere aanscherping van uitsluitend Hs (`DABS=0.005`,
`DREL=0.001`, `CURVAT=0.001`, `NPNTS=99.5`) haalt binnen 100 iteraties
het criterium niet, voor beide initialisaties. Daarmee is de warmstartwinst
bij gelijke bereikte nauwkeurigheid nog niet vastgesteld.

De spectrale norm is
\(\sum_{x,i}\sigma_i^2|N_{test}-N_{ref}|/
\sum_{x,i}\sigma_i^2|N_{ref}|\), over de opgeslagen bins op het
logaritmische frequentierooster. Gemeenschappelijke quadratuurconstanten
vallen weg; de diagnostische staart buiten het rooster is in deze norm
niet toegevoegd. De Hs-statistieken gebruiken de niet-negatieve waarden
in de geschreven Hs-velden, zonder de land-exceptiewaarden. Geen van deze
verschilnormen is een balansresidu of een fout tegen de exacte oplossing.

De warmstarttijd omvat het lezen van de binaire hotfile. De tijd om de
U=15-oplossing te maken moet worden meegerekend wanneer die nog niet
beschikbaar is. Alle tijden omvatten processtart, model-I/O en het schrijven
van een binaire hotfile; voorbereiding en kopiëren van invoer vallen buiten
de stopwatch. Het zijn afzonderlijke verkennende metingen op één vaste CPU,
geen herhaalde prestatiekwalificatie.

Ook `QUADRUPL 1` is geprobeerd. De huidige binary meldt 25 iteraties en
99,75% nauwkeurige punten en breekt daarna af met `free(): invalid size`.
Er is geen geldige uitvoer en geen bruikbare snelheidsvergelijking. Een
concrete verdachte uit de code-inspectie: `SOURCE` actualiseert
`WWINTL(13:14)` via `RANGE4`, maar `SWSNL1` leest
`DIA_WORKSPACE%WWINT(13:14)`. Die laatste grenzen beschrijven de grotere
workspace; `SWSNL1` breidt ze nogmaals uit met `IIID`. Dat kan buiten de
gealloceerde array vallen. Zie [de aanroep](../src/swancom1.f90), rond regel
7060, en [de gelezen grenzen](../src/swancom4.f90), rond regels 1133 en 1188.
**Bevestigd en gerepareerd.** Die verdenking klopte precies. `SWPRE4W` zet
`WWINT(13:14)` gelijk aan `MDC4MI`/`MDC4MA`, dus aan de allocatiegrenzen zelf;
met de uitbreiding met `IIID` schrijft `SWSNL1` gegarandeerd buiten de array
zodra een sweep niet de volle cirkel beslaat. Onder `-fcheck=all` luidt de
melding `Index '-43' of dimension 2 of array 'dia_workspace%ue' below lower
bound of -39`. De reparatie geeft `SWSNL1` zijn `WWINT` weer als argument; zie
de commit "Geef SWSNL1 de sweepgrenzen van het eigen roosterpunt terug". Het
was een regressie van deze fork, ingevoerd bij de argumentbundeling, en de
uitvoer is na de reparatie bytegelijk aan de bouw van ervoor. De reden dat
niemand het merkte: geen enkel deck in de repo koos `QUADRUPL 1`. Dat deck
bestaat nu, en de dekkingspoort in `scripts/physics_coverage.py` houdt het zo.
De mislukte proef staat in het meetbestand.

**1. Los de Komen-whitecapping volledig impliciet op met een rang-één-correctie.**
In [SWCAP](../src/swancom2.f90), rond regels 2520 en 2649, staat voor de
gebruikte standaardwaarden `DELTA=1`, `POWK=1`, `POWST=2`:

\[
S_{wc,i}(N)=-a(N)k_i^2N_i,
\qquad
a(N)=\frac{C_{ds}}{s_{PM}^4}\,\bar\sigma\,\bar k^2 E^2.
\]

Hier is \(s_{PM}^2=\mathrm{PWCAP}(2)\). Voor vaste waterdiepte zijn de
golfgetallen \(k_i\) constant. Uit `SINTGRL`, rond regels 5410–5485, volgen
de drie gewogen sommen, inclusief de lineaire bijdragen van de spectrale
staart:

\[
E=w_E^TN,\quad A=w_A^TN,\quad B=w_B^TN,\qquad
\bar\sigma=E/A,\quad\bar k=(E/B)^2.
\]

Daarmee wordt

\[
a(N)=C\frac{E^7}{AB^4},\qquad
\nabla a=a\left(7\frac{w_E}{E}-\frac{w_A}{A}-4\frac{w_B}{B}\right),
\qquad C=\frac{\mathrm{PWCAP}(1)}{\mathrm{PWCAP}(2)^2}.
\]

De exacte afgeleide van de positieve sink \(-S_{wc}\) is dus

\[
J_{wc}=a\,\operatorname{diag}(k_i^2)
       +(k_i^2N_i)(\nabla a)^T.
\]

Alle koppelingen tussen de spectrale bins zitten in één buitenproduct.
Wanneer de overige termen in de lokale linearisatie een matrix \(D\)
opleveren, is een Newton-correctie voor \(D+uv^T\) te berekenen met

\[
z=D^{-1}r,\quad q=D^{-1}u,\quad
\delta N=z-q\frac{v^Tz}{1+v^Tq}.
\]

Er zijn twee oplossingen met dezelfde bestaande matrix nodig, met hergebruik
van haar factorisatie, plus enkele vectorbewerkingen. Een volle 900×900-matrix
opslaan is hiervoor niet nodig. Dit behoudt de spectrale koppeling die bij
het alleen vastzetten van de dempingscoëfficiënt ontbreekt. Voor een andere
vaste `DELTA` bestaat de Komen-demping uit twee vaste functies van \(k_i\),
met elk een scalaire coëfficiënt: dezelfde aanpak wordt dan hoogstens een
rang-twee-correctie.

Het doel is minder buiteniteraties door een betere linearisatie. `SWCAP`
zelf nam in het eerdere profiel maar ongeveer 1% van de tijd in; sneller
uitrekenen van alleen deze routine kan dus weinig opleveren. Of deze
koppeling de trage foutcomponent bepaalt, moet de proef uitwijzen.

Een nog kleinere lokale formulering is mogelijk wanneer de overige termen
diagonaal zijn: stel \(N_i(a)=b_i/(d_i+a k_i^2)\) en zoek de scalaire
consistentievoorwaarde \(a=a(N(a))\). Bij spectrale transportkoppeling
wordt elke evaluatie een bestaande bandmatrixoplossing. Positiviteit,
niet-singuliere correcties en residudaling moeten expliciet worden bewaakt.

Deze afleiding geldt op een vaste, positieve tak. De energiekap in
`SINTGRL`, droogvallen en andere schakelingen moeten apart in de
linearisatie worden behandeld. Westhuysen en ST6 hebben andere structuren;
een Komen-resultaat mag niet als bewijs voor die pakketten dienen. Voor de
operationele Komen-route verandert `DRAG FIT` deze afleiding van de
whitecapping niet. Een volledige SWAN-integratie is nog niet gemaakt.

De afleiding is afzonderlijk gecontroleerd met een synthetisch positief
spectrum van 900 bins, vaste diepte en de lineaire SWAN-staartbijdragen.
Het relatieve verschil tussen oorspronkelijke en gereduceerde formule is
\(6{,}2\cdot10^{-16}\); het Jacobiaan-vectorproduct wijkt
\(1{,}0\cdot10^{-11}\) af van centrale eindige verschillen. De
rang-één-oplossing verschilt \(7{,}7\cdot10^{-16}\) van een volle
matrixoplossing. Dit controleert de wiskundige bouwsteen, niet een aangepaste
Fortran-routine of de convergentie van een volledige SWAN-run.

**2. Bewaar de spectrale vorm en schaal de DIA analytisch.**
De rekenkern van [SWSNL2](../src/swancom4.f90), rond regels 1680–1728,
vormt producten van drie spectrale dichtheden. Interpolatie en het aanvullen
van de staart zijn lineair. De ondiepwaterfactor hangt af van diepte en een
gemiddeld golfgetal, dat bij uniforme amplitudeschaal gelijk blijft. Daarom
geldt voor de onbegrensde DIA-bronterm, met dezelfde actieve configuratie:

\[
S_{nl4}(\alpha N)=\alpha^3 S_{nl4}(N),\qquad\alpha>0.
\]

Een cel waarvan alleen het energieniveau verandert hoeft dus geen nieuwe
DIA-evaluatie: schaal de opgeslagen bronterm. Dit gaat verder dan cellen
alleen overslaan als ze vrijwel geconvergeerd zijn; ook een fors groeiend
spectrum kan dezelfde vorm houden.

De concrete proef bewaart per cel een genormaliseerde spectrale vorm, de
bijbehorende DIA-term en haar energie. Bepaal de amplitudeverhouding en
de energiegewogen afwijking van de vorm. Bij gelijke vorm is herschalen
algebraïsch exact. Bij kleine vormverandering is het een benadering waarvan
de fout moet worden gemeten. Herbereken bij grote vormverandering en
regelmatig over het hele domein; gebruik een vers berekende volledige
bronterm voor eindcontrole en gevraagde brontermbudgetten. Verandering van
de Ursell-schakeling, instellingen of limiteractiviteit maakt een cache
ongeldig. De schaalwet geldt voor de oorspronkelijke bron, niet automatisch
voor alle afgesplitste matrixcoëfficiënten of een begrensde update.

Een vervolg kan vormen delen tussen cellen of naburige condities. Op een
uniforme volledige richtingscirkel zijn bovendien verschuivingen met een
geheel aantal richtingsbins bruikbaar voor de isotrope DIA-kern. De
geldigheid daarvan moet op de volledige bron worden gecontroleerd,
los van sweepgrenzen en stromingsafhankelijke selectie.

Het [eerdere profiel](verbeterkansen-2026-09-12.json) schrijft 29,54% van de
tijd aan `SWSNL2` toe. Bij gelijkblijvend iteratieaantal is zelfs volledige
eliminatie daarvan begrensd tot \(1/(1-0{,}2954)=1{,}42\)× totale versnelling.
Caching kost bovendien geheugen en eigen rekentijd. De grote factorwinst
moet daarom mede uit minder sweeps komen.

**3. Gebruik de bestaande sweeps als bouwsteen voor een sterkere solver.**
Voor een vast stationair probleem vormen vier sweeps een afbeelding
\(N^{j+1}=G(N^j)\). Anderson-versnelling combineert enkele vorige updates
om langzaam uitdovende foutcomponenten te onderdrukken. Begin met drie
historische stappen, reguliere sweeps voor de start en een terugval wanneer
de kandidaat geen verbetering geeft. De algemene methode en haar relatie
met Krylov-methoden staan in
[Walker en Ni (2011)](https://epubs.siam.org/doi/10.1137/10078356X).
De toepassing op deze SWAN-code is een voorstel, geen gemeten resultaat.

De inbouwplaats ligt na `complete_sweep_iteration` en vóór de volgende
buiteniteratie in [SWCOMP](../src/swancom1.f90). De brontermcaches en
afgeleide grootheden moeten consistent met de gekozen kandidaat worden
bijgewerkt. Bij een afwijzing moet ook die toestand herstelbaar zijn.
Een combinatie van spectra zonder consistente `COMPDA` is geen geldige
implementatie. Extra geschiedenissen kosten voor dit demonstratierooster
circa 66,5 MB per enkelprecisieveld (18.471 x 36 x 25 x 4 byte); opslag van zowel stappen als residuen
maakt het totaal een veelvoud daarvan.

Controleer de onbegrensde, geschaalde balansfout op een gezamenlijk spectrum.
Alleen \(G(N)-N\) is onvoldoende als de limiter de update klein maakt.
Ook een kleine wijziging van Hs en Tm bepaalt niet de fout in het hele
spectrum. Positiviteit, randvoorwaarden en harde fysicaschakelingen moeten
bij elke extrapolatie behouden blijven. De SWAN-technische documentatie
bespreekt zelf de gevolgen van de groeilimiter voor de convergentie:
[convergence-enhancing measures](https://swanmodel.sourceforge.io/online_doc/swantech/node48.html).

Als de trage component vooral in de viergolfkoppeling zit, is een volgende
stap een lokaal Newton–Krylov-blok voor het hele spectrum: spectraal
transport, de spaarzame DIA-afgeleide en de bovenstaande correctie voor de
whitecapping samen. De DIA-kern geeft een product van de Jacobiaan met een
vector zonder een globale dichte matrix op te slaan. Meet dan het aantal
bron-, Jacobiaan- en preconditioner-evaluaties én totale tijd; een kleiner
aantal Newton-stappen alleen zegt niets over de winst.

**4. Versnel de langzaam veranderende momenten met een klein hulpmodel.**
Een fundamentelere route is een combinatie van het volledige spectrum met
een goedkoop stelsel voor enkele momenten: energie, gemiddelde frequentie
en richtingsfluxen. Dat stelsel kan een grove ruimtelijke correctie over
het hele domein snel doorgeven. Daarna corrigeert een volledige spectrale
sweep de vorm. Dit is verwant aan de high-order/low-order-methoden voor
transportproblemen; zie
[Chacón e.a. (2017)](https://www.sciencedirect.com/science/article/pii/S0021999116305770).
De voorgestelde overdracht naar SWAN is een afleiding naar analogie, geen
bestaand prestatiebewijs voor golven.

Cruciaal is een consistentiecorrectie: een oplossing van de fijne
SWAN-balans moet ook een nulpunt van de gekoppelde iteratie blijven.
Een grove SWAN-run als eerste gok is eenvoudiger, maar geeft die
eigenschap niet vanzelf. Bij geometrische coarsening moeten verbindingen
door geulen en de transmissie door obstakels behouden blijven. SWAN is
sterk richtingafhankelijk transport; een isotrope diffusiecorrectie kan
verkeerde foutcomponenten behandelen. Onderzoek daarom ook coarsening
langs voortplantingsrichtingen of in het momentstelsel.

Alleen het uiteindelijke frequentierooster grover kiezen verandert de
discretisatiefout én de DIA-eigenschappen. De
[SWAN-handleiding](https://swanmodel.sourceforge.io/online_doc/swanuse/node28.html)
waarschuwt voor DIA bij frequentieresoluties die sterk van circa 10%
afwijken. Een grof hulpstelsel moet de fijne oplossing versnellen; zijn
goedkope resultaat is op zichzelf geen vervanging voor die oplossing.

**5. Behandel de conditiematrix als een samenhangende reeks oplossingen.**
De gemeten warmstart is de eenvoudigste variant. Sorteer condities per
fysicaregime, keringstand en nat/droog-topologie en zoek daarbinnen korte
stappen in windsnelheid, richting en waterstand. Meerdere onafhankelijke
reeksen kunnen tegelijk lopen. Zo blijft batchparallelisme mogelijk zonder
voor elke conditie opnieuw een beginveld op te bouwen.

Een sterker beginveld volgt uit een tangentvoorspelling:
\[
F_N\,\frac{\partial N}{\partial p}=-F_p,
\qquad
N(p+\Delta p)\approx N(p)+\frac{\partial N}{\partial p}\Delta p.
\]
Hergebruik daarbij dezelfde solver of preconditioner als voor de
stationaire correctie. Een klein aantal referentiespectra kan de voorspeller
ook leveren. Toets de voorspelling met de oorspronkelijke balans en laat
SWAN corrigeren. Kosten van referenties, sensitiviteiten, opslag en deze
correctie horen allemaal in de batchtijd.

De bestaande [conditiematrix](../so-rp_swan/matrix/conditions.json) bevat
expliciet open/dichte kering, sterk uiteenlopende waterstanden en de
BG2-sectorovergang 315°/316°. Nabijheid in graden alleen maakt twee
condities dus niet noodzakelijk geschikt voor dezelfde voorspeller.

**Volgorde voor het vervolg.** Maak eerst een spectraal residu en
limiterdiagnose per buiteniteratie zichtbaar. Daarmee is te onderscheiden
of trage ruimtelijke overdracht, een lokale bronkoppeling of herhaald
begrenzen de iteraties bepaalt. Probeer vervolgens Anderson en de
Komen-correctie afzonderlijk; combineer ze pas als beide hun eigen kosten
terugverdienen. Meet tegelijk warmstarts op de operationele conditiematrix.
De DIA-vormcache is een aanvullende besparing. Een consistent moment- of
multigridstelsel verdient de grotere onderzoeksinspanning wanneer de
gemeten foutcomponenten daar aanleiding toe geven.

Een rekenvoorbeeld, geen voorspelling: van 30 gewone iteraties naar 10
versnelde iteraties die elk 20% duurder zijn geeft voor het iteratiewerk
\(30/(10\cdot1{,}2)=2{,}5\)× versnelling. Zo'n reductie treft transport,
bronberekening en lokale solver tegelijk. Dat is de schaal waarop ik de
grote winst zou zoeken.

**Herleidbaarheid.** De bronbasis is `4e8ac4b400500699d8095c389749bb2c6ec8eb6d`
met de bij aanvang al aanwezige sectorgebonden nulstelling in
`src/swancom1.f90`. Die productiebron is tijdens dit onderzoek niet gewijzigd.
De gebruikte executable had ten tijde van de runs SHA-256
`d0f61317d97dba3e93595891c4c2a55af925c13f1fae10d6cddd930dad8ec73f`. Die binary
bestaat niet meer: de boom is daarna herbouwd. De runs zijn dus niet opnieuw te
draaien tegen exact dezelfde executable.
De ruwe runs staan in `/tmp/swan-fundamental-sJMqQh/runs`.
De [meetgegevens](fundamentele-versnelling-2026-09-13.json) bewaren alle negen
runs, uitvoerhashes, invoerdecks, de begin-diff en de algebra-controle.
**Correctie achteraf.** Die bewering klopte niet. Tijdens en na deze runs is
de productiebron wel degelijk gewijzigd — de sectorgebonden nulstelling kreeg
een bewaking, `CMakeLists.txt` en een test kwamen erbij — en de binary is om
11:44 herbouwd. Bovendien liep die herbouw plus een ctest gelijktijdig met de
laatste twee runs (`u16_hs_cold` en `u16_hs_warm`), zodat hun tijden van 99,21 s
en 104,45 s vervuild zijn. Beide liepen tegen de iteratielimiet van 100, dus de
tijd zegt daar sowieso weinig; de nauwkeurigheidscijfers (85,17% koud tegen
93,64% warm) staan wel. Zie ook het beleid over meten op een stille machine in
`AGENTS.md`. De [SWAN-proef](fundamentele-versnelling-proef.py) en
[algebra-controle](komen-rang-een-controle.py) zijn afzonderlijk uitvoerbaar:

```sh
OPENBLAS_NUM_THREADS=1 python3 doc/fundamentele-versnelling-proef.py \
  /home/wilbert/swan /home/wilbert/swan/build/bin/swan.exe \
  /tmp/swan-fundamental-reproduction
OPENBLAS_NUM_THREADS=1 python3 doc/komen-rang-een-controle.py
```

Kies voor een nieuwe meting een nieuwe uitvoermap. Voor reproduceren van
deze meting is dezelfde bronbasis inclusief de beschreven begin-diff nodig;
dezelfde bestandsnaam van een executable garandeert dat niet. De proef
bewaart een crash als mislukte run en gaat verder. De verstreken tijd van
de oorspronkelijke `QUADRUPL 1`-crash is niet vastgelegd en staat als `null`
in het archief. De scripts hebben NumPy nodig; het binaire hotfileformaat
wordt hier gelezen voor de gebruikte GNU/Linux-build.

Dit document en het meetbewijs vernieuwen geen releaseclaim. De bestaande
gedocumenteerde poortbasis blijft commit `4e8ac4b`; deze verkenning heeft de
releasepoorten niet opnieuw uitgevoerd. De gerichte selector kiest voor
de vier toegevoegde documentatie- en bewijsbestanden geen poorten. De
SWAN-runs en de beschreven algebra-controle zijn daadwerkelijk uitgevoerd;
beide reproduceerscripts zijn ook op Python-syntax gecontroleerd.
