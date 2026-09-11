# Eindverantwoording

Dit document sluit het [moderniseringsplan](moderniseringsplan.md) af. Het zegt
wat er is opgeleverd, waar het bewijs voor staat, en wat er open blijft. Het is
bedoeld om gelezen te worden door iemand die moet beslissen of hij deze fork
gebruikt — en die daarvoor moet weten wat er níét geclaimd wordt.

## 1. Wat er staat

Vertrekpunt was TU Delft SWAN 41.51: 76 bronbestanden, waarvan 19 in fixed-form
Fortran, vier `COMMON`-blokken, zestien gedeelde toestandscontainers, een
Perl-gestuurde bouw en geen enkele test.

Wat er nu staat:

| | Bij de fork | Nu |
|---|---|---|
| Bronbestanden | 76 | 159 |
| Fixed-form | 19 | 0 |
| `COMMON`-blokken | 4 | 0 |
| Modules | — | 152 |
| Gedeelde toestandscontainers | 16 | 0 |
| Geregistreerde tests | 0 | 62 (serieel), 66 (MPI) |
| Bouw | `Makefile` + `switch.pl` | CMake met 19 opties |
| Compilerwaarschuwingen (strikte set) | niet meetbaar | 1317, geratcheteerd |

Alle acht delen uit het plan zijn uitgevoerd. De doelarchitectuur uit §6 van het
plan staat: run-toestand en thread-toestand zijn gescheiden, rekenkernen krijgen
de kleinste deelcontext die ze nodig hebben, en geen enkele kern vraagt zelf naar
het threadnummer.

## 2. Wat er bewezen is

**Uitvoer is niet veranderd.** Elke voorbeeldcase vergelijkt zijn numerieke
uitvoer veldsgewijs met een referentie in versiebeheer. Waar een bouwvariant de
uitkomst werkelijk verandert — `JAC` en `FFRO` — heeft die variant een eigen
referentie in plaats van een opgerekte tolerantie.

**De compiler controleert wat hij kan controleren.** `IMPLICIT NONE (TYPE,
EXTERNAL)` staat overal, en de strikte bouw meldt geen enkele impliciete
interface meer -- bij de start waren het er 3229. De laatste twee, de aanroepen
naar de externe METIS-bibliotheek, hebben sinds de expliciete C-koppeling een
gecontroleerde `BIND(C)`-interface.

**Terugval wordt geblokkeerd.** De waarschuwingsratchet bewaakt zowel een budget
per categorie als een vingerafdruk per bestand, zodat ook een omruil — één
waarschuwing opgelost, één nieuwe in dezelfde categorie — opvalt.

**SWAN is herbruikbaar binnen één proces.** Dezelfde case draait tweemaal
bit-identiek, ook met wisselende fysica, een wisselend rekenrooster of wisselende
spectrumafmetingen ertussen, en de tussenliggende run wijkt aantoonbaar af.

**De fysica is niet alleen vastgepind maar ook getoetst.** Shoaling tegen Green,
refractie tegen Snellius, roosterconvergentie tegen eerste-ordetheorie — met de
poort vooraf vastgelegd.

**Het geheugen wordt vrijgegeven.** De gekoppelde lijsten van de parser lekten
cumulatief 2040 bytes per deck; dat is nu nul, gemeten met Valgrind op dezelfde
decks voor en na.

**Het verschil met de operationele praktijk is verklaard.** De 28 procent hogere
golfhoogte die 41.51 op de so-rp-conditie geeft ten opzichte van de operationele
41.31 komt volledig uit de gewijzigde default-brontermen, niet uit deze
modernisering: whitecapping en wind ongeveer 80 procent, de windsleep ongeveer
20 procent, triads verwaarloosbaar op deze conditie. Met `GEN3 KOMEN DRAG FIT`
resteert 0,0010 m verschil op een gemiddelde van 1,2901 m. De operationele
binary bleek gelijk aan vers gebouwde upstream 41.31, dus er zit geen verborgen
patch tussen.

## 3. Reparaties boven de upstream-bronbasis

Deze fork heeft een aantal echte defecten gerepareerd die ook in upstream 41.51
zitten. Ze staan met hun bewijskracht per item in
[bugfixes-tov-tu-delft-41.51.md](bugfixes-tov-tu-delft-41.51.md) — bewezen,
latent, of diagnostiek — zodat ze teruggegeven kunnen worden aan TU Delft.

De grootste: een ontbrekende stencilrand in de ongestructureerde
OpenMP-frontdecompositie, waardoor het resultaat van het threadaantal kon
afhangen; een ontbrekende stopcontrole na een fatale fout, waardoor MPI zonder
METIS crashte in plaats van netjes te stoppen; de lekkende lijstkoppen; en het
stil doorrekenen op een lege MDIA-lambdalijst.

## 4. Wat geclaimd wordt

GNU Fortran 13.3.0 op Linux x86-64, Release, met de zes verplichte poorten groen
op dezelfde commit: serieel, OpenMP, runtimecontroles, strict, MPI op twee ranks,
en de manifest- en Python-controles. Inclusief installatieproef en een case die
vanuit de installatie draait.

## 5. Wat níét geclaimd wordt

Dit is de belangrijkste paragraaf van dit document.

- **Nul waarschuwingen.** Er staan er 1317. Ze zijn geïnventariseerd en
  geratcheteerd, niet opgelost.
- **De Debug-route.** Op `-O0` wijkt de niet-stationaire reguliere case af van
  de referentie. Het verschil is scherp afgebakend: de golfhoogte blijft binnen
  2,1e-4 relatief — dezelfde orde als de OpenMP-spreiding waarop de tolerantie
  is geijkt — en precies één grootheid gaat over de poort van 1e-3 heen, de
  golfperiode Tm01 op tijdstap 5, met 1,18e-3. Het is deterministisch, het groeit
  tot stap 5 en krimpt daarna weer, en de iteratiestructuur is identiek aan die
  van de geoptimaliseerde bouw.

  Zes verklaringen zijn gefalsificeerd. FMA-contractie, de vectorizer, aliasing
  en float-store vielen eerder af. Daar kwamen twee bij: niet-geïnitialiseerd
  geheugen is het niet, want `-finit-real=zero` en `-finit-real=nan` geven
  bit-identieke uitvoer; en het is geen instelbare optimalisatie, want `-O0`
  plus alle 41 vlaggen die `-O1` aanzet wijkt nog steeds af, terwijl `-O1` met
  diezelfde 41 vlaggen uitgezet de referentie nog steeds exact reproduceert.
  `-O1`, `-O2`, `-O3` en gfortran 15 geven alle vier exact de referentie, dus
  dit is geen algemene bouwgevoeligheid maar iets dat eigen is aan
  ongeoptimaliseerde codegeneratie.

  Een bisect per bronbestand — alles op `-O1`, de helft op `-O0`, zeven rondes —
  wijst één bestand aan: `src/swan_spectrum_transform.f90`. Dat is waar een
  spectrum op een andere basis wordt herverdeeld, en waar Tm01 als verhouding
  van spectrale momenten uit voortkomt. De herverdeling in `CHGBAS` accumuleert
  in enkelvoudige precisie achter overlaptests op celgrenzen (`X1A < X2B`,
  `X1B > X2A`). Zulke vergelijkingen kunnen op een verschil in het laatste bit
  omklappen, waarna een hele cel wel of niet meetelt — wat past bij het abrupte
  karakter van de afwijking.

  Daarmee is de route van "onverklaard" naar "gelokaliseerd" gebracht. Wat nog
  ontbreekt is het exacte statement. Zolang dat er niet is wordt de route niet
  groen verklaard en niet met een ruimere tolerantie toegedekt: die zou juist de
  gevoeligste grootheid van de suite verblinden. De volgende stap is het
  statement aanwijzen en dan beslissen of de gelijkspel-regel in die
  overlaptests expliciet gemaakt moet worden — een wijziging die de uitkomst
  raakt en dus haar eigen verantwoording krijgt.

- **Een tweede compiler.** Intel, Flang en NAG zijn niet beproefd; hun
  afwezigheid in deze omgeving is geverifieerd, hun werking niet.
- **Native Windows, IBM, Fujitsu, Cray.** De takken zijn bewaard en compileren
  waar dat kan; beproefd zijn ze niet.
- **De volledige schone bouwmatrix van alle 28 configuraties op één kandidaat.**
  De registratietellingen zijn per configuratie gemeten, de gezamenlijke schone
  bouw niet.
- **Het MPI-veldcontract over decomposities.** Compact gemeten, geen oordeel.
- **Een afbreek-defect op een malafide deck.** Een deck met een ongeldige
  punt- en curvecombinatie kan afbreken op een manier die niet reproduceerbaar is
  onder instrumentatie. Het bestond al vóór deze fork. Wat we weten staat
  vastgelegd; een reparatie die we niet kunnen aantonen zou erger zijn.
- **Gelijktijdig of overlappend gebruik binnen één proces.** Seriële
  herbruikbaarheid is bewezen; threadveilig meerdere runs tegelijk is niet
  onderzocht.
- **De consolidatie van de twee testpuntvlaggen.** Geprobeerd en niet gehaald,
  en binnen de regels van het plan ook niet haalbaar: de vlaggen zijn per punt en
  veranderen binnen de lus, terwijl de bundel per lus gebonden wordt. Dat is een
  uitkomst, geen openstaand punt.

Een onderzoeksproef die bekend faalt kwalificeert haar route niet. Waar iets niet
beproefd is, staat "niet beproefd" — niet "ondersteund".

## 6. Hoe je het zelf controleert

```sh
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=ON
cmake --build build --parallel
ctest --test-dir build --output-on-failure
```

Dat moet 62 van de 62 groen geven vanuit een verse `git clone`, zonder
voorbereiding en zonder lokale data. De overige poorten staan in
`.github/workflows/ci.yml`; de strikte poort draait met
`python3 scripts/strict_diagnostics.py --require-baseline`.

Welke poorten een wijziging kán breken bepaalt `python3 scripts/select_gates.py`.
In CI draait altijd alles.

## 7. Onderhoud en upstream

`upstream` blijft ingesteld op TU Delft. De regressiesuite is wat een merge
beoordeelt: is hij na de merge groen, dan is de merge goed; is hij rood, dan is
dat een bevinding en geen reden om de suite aan te passen.

De ondersteunde combinaties staan in [support-matrix.md](support-matrix.md), de
architectuur in [modern-fortran.md](modern-fortran.md), en de werkafspraken in
[AGENTS.md](../AGENTS.md).
