# Verbeterkansen: substantiële snelheidswinst

Onderzoek van 12 september 2026 op broncommit
`1219e06e6155f7642835a1fc27129f5eb7d6247a`.

De belangrijkste nieuwe bevinding is **21,0% minder rekentijd voor één
volledige Voordelta-som** door niet-aangevraagde brontermbudgetten niet te
berekenen. Dit is gemeten met een geïsoleerde proefimplementatie. Daarnaast
levert een andere verdeling van vier cores over vier sommen **35,6% minder
mediane batchtijd** op. Die twee percentages zijn afzonderlijke metingen en
mogen niet worden vermenigvuldigd.

De productiecode is ongewijzigd. De [proefpatch](bronbudget-op-aanvraag-proef.patch)
en [ruwe meetgegevens met reproduceerscripts](verbeterkansen-2026-09-12.json)
zijn bij dit rapport opgeslagen.

## 1. Brontermbudgetten alleen berekenen als ze nodig zijn

Dit idee komt uit het nieuwe CPU-profiel en de dataflow in de huidige code.
In `SOURCE` worden per punt en sweep zes budgetarrays genuld. De fysieke
brontermroutines vullen vervolgens deze aparte boekhouding naast de
coëfficiënten waarmee de golfvergelijking wordt opgelost. De uitlezing door
`ADDDIS` is al afhankelijk van `LADDS`; het nulstellen en invullen gebeuren
echter ook wanneer die uitvoer niet is gevraagd.

Het profiel wijst 600 van 2.752 samples toe aan de zes nulstellingen rond
regels 6812–6823 van `src/swancom1.f90`: **6,00 s, of 21,8% van de samples**.
Die regels schrijven in deze run logisch circa **77,1 GB**: 93.600 bytes per
aanroep × 823.450 aanroepen. Dit is een telling van schrijfbewerkingen, geen
meting van werkelijk DRAM-verkeer. Geoptimaliseerde bronregelattributie is
benaderend; de voor/na-proef onderbouwt het werkelijke effect.

De proef voegt aan zes brontermroutines een optionele `RECORD_BUDGET` toe.
Hun bestaande gedrag blijft de standaard voor losse aanroepers. `SOURCE`
kan de boekhouding uitschakelen wanneer `LADDS` en `TESTFL` beide uitstaan.
Dan worden zowel de nulstellingen als alle betreffende budgetupdates
overgeslagen. De updates van de fysieke systeemmatrix blijven uitgevoerd.

| Herhaling | Huidige code | Proef |
|---|---:|---:|
| 1 | 29,825 s | 23,561 s |
| 2 | 29,829 s | 27,083 s |
| 3 | 28,806 s | 22,400 s |
| **Mediaan** | **29,825 s** | **23,561 s** |

Dit is **21,0% minder tijd**, of **1,27× zo snel**. Alle drie proeftijden
liggen onder alle drie basistijden. Ook het tweede CPU-profiel ondersteunt
de verklaring: de self-tijd van `SOURCE` daalt van **6,14 naar 0,29 s**;
`SWSNL2` blijft ongeveer gelijk op **8,13 versus 8,20 s**. De versnelling
komt dus uit het wegnemen van boekhouding, niet uit een vereenvoudigde
quadrupletberekening of een aangepaste convergentiedrempel.

Controle van de proef:

- **63/63 seriële CTest-tests geslaagd**.
- Diepteveld, Hsig-veld en sitetabel zijn in alle voor/na-runs bytegelijk.
- De 25 iteraties met elk vier sweeps blijven gelijk.
- Twee aanvullende contractproeven zijn bytegelijk aan de baseline:
  expliciet aangevraagde `GENW DISW DISSU DISB`, met daadwerkelijk niet-nulle
  budgetuitvoer, en brontermspectra op een aangewezen testpunt.

De proef is bewust begrensd: de snelle route ondersteunt de gemeten
Komen-combinatie met DIA, bodemwrijving en breking, zonder triads, stroming,
ijs, vegetatie, modder of Bragg. Andere combinaties behouden de volledige
boekhouding. **De operationele so-rp-route met Westhuysen en triads is hiermee
nog niet versneld of gekwalificeerd.** Dat is de eerste uitbreiding: alle
schrijvers en lezers van de budgetkanalen controleren en de uitvoervraag
als expliciet contract doorgeven. Daarna ook de ongestructureerde route,
OpenMP en de operationele condities beproeven.

Dit is een proefimplementatie, geen gereed productievoorstel: onder meer de
fysicaselectie in de proef moet bij generalisatie een duidelijk contract
krijgen. Voor integratie selecteert `scripts/select_gates.py` op deze
bronpaden serieel, OpenMP, strict en pytest. Alleen de seriële proefpoort is
hier gedraaid; er wordt geen nieuwe OpenMP-, MPI- of strict-kwalificatie
geclaimd. De ongewijzigde hoofdboom blijft op bovengenoemde broncommit en
zijn bestaande kwalificatie staan.

## 2. Meerdere sommen: optimaliseer het aantal sommen per uur

De operationele `matrix_runner.py` voert de targets, condities en herhalingen
achter elkaar uit. Een begrensde procespool is een concrete verbeterstap.
Daarvoor zijn vier identieke demonstratiesommen doorgerekend met steeds
dezelfde vier cores, verdeeld over verschillende aantallen processen.

| Processen × threads | Drie batchtijden | Mediaan |
|---|---|---:|
| 1 × 4 | 41,538 / 65,065 / 60,630 s | 60,630 s |
| 2 × 2 | 45,867 / 46,221 / 45,392 s | 45,867 s |
| 4 × 1 | 39,679 / 39,075 / 37,485 s | 39,075 s |

Vier afzonderlijke processen met één thread geven **35,6% minder mediane
batchtijd**, of **1,55× zoveel sommen per tijdseenheid**, met bytegelijke
uitvoer. Dit versnelt het afhandelen van meerdere sommen; het is geen
versnelling van één individuele som. De 1×4-configuratie spreidt sterk.
Dit resultaat vraagt dus een nieuwe afweging op de operationele hardware
en grotere so-rp-condities, geen vaste universele instelling.

Voeg een `--jobs`-optie toe met afzonderlijke werkmappen, disjuncte
CPU-toewijzing, een geheugenlimiet en de bestaande hashes en publicatie bij
succes per run. Gelijktijdige SWAN-runs binnen één proces zijn hiermee niet
beproefd; de meting gebruikt onafhankelijke processen.

## 3. Groter algoritmisch onderzoek: alleen veranderende gebieden bijwerken

De huidige run heeft na iteratie 11 al **89,56%** voldoende nauwkeurige natte
punten, na iteratie 13 **93,92%** en na iteratie 24 **99,45%**. Toch wordt het
gebied tot iteratie 25 met **99,80%** vrijwel volledig doorgerekend.

Een nieuwe onderzoekslijn is daarom een actieve verzameling cellen, gestuurd
door lokale spectrale residuen en veranderingen in de bovenstroomse stencil.
Een punt mag tijdelijk worden overgeslagen als zijn oplossing en invoer uit
de buren stabiel zijn; een veranderde buur maakt het weer actief. Periodieke
volledige sweeps controleren het globale residu en voorkomen dat een foutief
inactief gebied buiten beeld blijft.

Dit grijpt aan op het aantal dure bronterm- en solveraanroepen. Het is een
grotere ingreep dan geheugenattributen of compilerflags. De bestaande
Hs/Tm-convergentievlag alleen is onvoldoende als oversla-criterium: een
spectrum kan nog veranderen en later veranderende buren kunnen een cel weer
beïnvloeden. Stroming, obstakels, droogvallen, limitering en de volgorde van
de Gauss–Seidel-sweeps moeten expliciet worden meegenomen.

**Hiervoor is nog geen snelheidswinst gemeten.** De volgende concrete proef
moet aantallen geëvalueerde cellen, totale tijd, spectrale residuen en
numerieke verschillen meten. Een factorwinst claimen op basis van alleen
het percentage geconvergeerde punten zou onjuist zijn.

## Afgevallen experimenten

Dezelfde bronbasis is ook met LTO en twee geheugenproeven gemeten. Ze vormen
geen aanbeveling:

- LTO: 27,659 → 27,286 s, slechts 1,35% lagere mediane tijd.
- Alleen `CONTIGUOUS` op de zes budgetpointers: 29,245 → 30,074 s.
- Daarbij volledige arraytoewijzingen gebruiken: 28,695 → 33,767 s.

Beide bronproeven waren numeriek goed en haalden 63 seriële tests, maar
waren langzamer. Hun patches en resultaten staan in het meetbestand.
De succesvolle derde proef begint opnieuw bij de oorspronkelijke bronbasis;
zij bevat deze twee wijzigingen niet.

## Methode en reproduceerbaarheid

- Intel Core Ultra 9 185H onder WSL2; GNU Fortran 13.3.0.
- De ongewijzigde `examples/voordelta/voordelta.swn`: 18.471 roosterpunten,
  36 richtingen en 25 frequenties, met `GEN3 KOMEN`.
- Release `-O3`, zonder native, LTO, fast-math of runtime-instrumentatie in
  de vergelijkende bronmetingen. Afzonderlijke profilerbuilds hebben `-g -pg`.
- Drie herhalingen, omgekeerde configuratievolgorde in de middelste ronde,
  verse werkmappen en vaste CPU-affiniteit. Voor de seriële proeven CPU 0;
  voor de batchproeven CPU's 0, 2, 4 en 6.
- OpenMP: `OMP_DYNAMIC=FALSE`, `OMP_PLACES=cores`, `OMP_PROC_BIND=close`,
  `OMP_WAIT_POLICY=PASSIVE`. De door WSL gerapporteerde topologie en variabele
  laptopklokken begrenzen de generaliseerbaarheid.
- De gewone timing omvat processtart en alle model-I/O, maar niet het vooraf
  kopiëren van de invoer. De batchtiming omvat ook de voorbereiding van de
  werkmappen. Builds, tests en verschillende meetconfiguraties overlappen niet.
- Het JSON-bestand bewaart iedere tijd, broncommit, compilerflags, invoer- en
  binaryhashes, uitvoerhashes, iteratiereeksen, profielen en de proefscripts.
  De oorspronkelijke werkmap is `/tmp/swan-improvement-3ga10cax`.

Bytegelijkheid betreft de daadwerkelijk geschreven velden en tabellen,
plus de aanvullende brontermspectra in de contractproef. Dit demonstratiedeck
schrijft niet alle interne spectra uit. De resultaten zijn geen formele
migratiekwalificatie van de operationele suite.

Een blijvende verbetering is een periodieke prestatiemeting op een stabiele
runner, gekoppeld aan uitvoercontrole en exacte buildidentiteit. De gevonden
overhead zou daarmee bij latere wijzigingen eerder zichtbaar worden.

## Overige concrete bevindingen

Deze zijn nuttig voor betrouwbaarheid, maar leveren geen modelversnelling:

- De CI-workflow schrijft `job.status`, terwijl `scripts/ci_gate.py` alleen
  `passed` accepteert. GitHub gebruikt `success`; zes succesvolle artefacten
  worden lokaal reproduceerbaar alle zes afgewezen.
  [GitHub-documentatie](https://docs.github.com/en/actions/reference/workflows-and-actions/contexts#job-context).
- De inhoudelijke CI-installatieproef gebruikt de buildbinary. Laat de case
  de geïnstalleerde binary uitvoeren; alleen `--help` met `|| true` bewijst
  geen werkende installatie.
- CMake-paden afzonderlijk door `select_gates` halen selecteert geen MPI,
  in strijd met de projectinstructie. De huidige test combineert CMake met
  een METIS-bestand en maskeert dat gat. Python-code in de operationele
  matrix krijgt bovendien een lege selectie doordat die hele map als
  validatiebewijs wordt behandeld.
- De opgeslagen operationele validatie vermeldt 174 cellen met een ander
  nat/droogmasker in de MPI-Matlabuitvoer. Sluit dat veldcontract voordat
  puntgelijkheid wordt uitgebreid tot een claim over het hele veld.
