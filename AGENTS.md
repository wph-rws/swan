# AGENTS.md

Gedeelde projectinstructies voor AI-coding-agents (Codex, Claude Code, e.a.) in deze SWAN-repo.

## Taal

Schrijf helder, gewoon Nederlands — in antwoorden, committeksten, commentaar en
documentatie. Geen Nederlands dat klinkt als een houterige vertaling van Engels
vakjargon, en geen Engelse termen waar een gewoon woord bestaat: uitvoer, niet
output; poort, niet gate; tijdslimiet, niet timeout. Namen die echt eigennaam
zijn blijven staan zoals ze heten: SWAN-commando's, routinenamen, bestandsnamen,
`ASLR`, `git bisect`. Leg een bevinding eerst uit in gewone woorden — wat er aan
de hand is, hoe je dat weet, wat het betekent — en pas daarna in termen van de
code.

## Repo-inrichting

- `origin` = `wph-rws/swan` (GitHub), `upstream` = TU Delft (`gitlab.tudelft.nl/citg/wavemodels/swan`).
- Huidige versie: SWAN **41.51** (zie `SWAN_VERSION_NUMBER` in `src/swanmain.f90`). De upstream-remote in deze repo gaat maar terug tot **41.41**; 41.31 zit er niet in.
- Broncode is gemoderniseerd van fixed-form (`.ftn`/`.f`) naar free-form Fortran (`.f90`).
- Lokaal aanwezig: `so-rp_swan/` met de operationele so-rp/Voordelta-case, de referentie-binary **BSS 41.31A.1**, en een verificatiematrix onder `so-rp_swan/runs/`.

## Belangrijk fysica-feit: gewijzigde default source terms (41.31 → 41.51)

**Symptoom:** een deck **zonder `GEN`-commando** geeft in 41.51 een duidelijk hogere Hsig en tragere convergentie dan in BSS 41.31, terwijl de invoer identiek is.

**Oorzaak:** zonder expliciet `GEN`-commando gebruikt SWAN de ingebouwde default 3e-generatie-package. TU Delft heeft die default in **release 41.45** omgezet (SWAN-release-notes; enige spoor in code = versietag `42.00` bij `INKEYW ('STA','WESTH')` in [src/swanpre1.f90](src/swanpre1.f90)):

1. Deep-water fysica: **Van der Westhuysen et al. (2007)** i.p.v. Komen et al. (1984) — wind IWIND 3→5 (Yan), whitecapping IWCAP 1→7 (Alves-Banner, verzadigingsgebaseerd).
2. Wind drag: **Wu (1982)** i.p.v. de 2e-orde polynoomfit (Zijlema et al., 2012).
3. Triads: **DCTA** i.p.v. LTA (ITRIAD 11→5); biphase `[urcrit]` 0.2 → 0.63.

**Fix om 41.31-fysica exact te reproduceren in 41.51:** voeg toe aan het deck:

```
GEN3 KOMEN DRAG FIT
```

> Let op: **alleen `GEN3 KOMEN` is niet genoeg** — dat laat ~20% van het verschil staan omdat de wind drag-default óók veranderde (WU vs. FIT). Voeg voor volledige 1-op-1-reproductie van de operationele suite eventueel ook `TRIAD ITRIAD=11 URCRIT=0.2` toe (verwaarloosbaar voor de so-rp-conditie, maar relevant voor triad-actievere/ondiepere sommen).

**Geverifieerd** (so-rp, 20 m/s uit 310°, NAP +3,00 m, MXITST=50; gem. Hsig over de **132 natte** uitvoerpunten):

| Fysica | gem. Hsig | iteraties |
|---|---|---|
| 41.51 default (Westhuysen) | 1,6453 m | 35 |
| `GEN3 KOMEN` | 1,3607 m | 32 |
| `GEN3 KOMEN DRAG FIT` | 1,2911 m | 30 |
| echte 41.31 (BSS-binary én vers gecompileerd upstream 41.31) | 1,2901 m | 30 |

> Let op de middeling: van de 135 uitvoerpunten zijn er **3 droge/landpunten met Hsig = −9** (SWAN-exceptiewaarde `EXCV`). Middelen over alle 135 (inclusief die −9) geeft kunstmatig lage waarden (resp. 1,4088 / 1,1305 / 1,0624 / 1,0615 m) — dat zijn domeingemiddelden, geen natte-punt-gemiddelden. Rapporteer altijd over de natte punten. De landpunten zijn in elke run identiek en vallen weg in de verschillen, dus de conclusie is onafhankelijk van deze keuze.

Attributie van het gat (0,355 m op natte punten): whitecapping+wind ~80%, wind drag ~20%, triads/biphase ~0% (deze conditie); restverschil restore vs. 41.31 = 0,0010 m. Controle: de BSS 41.31A.1-binary == vers gecompileerd stock upstream 41.31 → geen verborgen RWS-patch die Hsig beïnvloedt. Kanttekening: dit is één conditie; de grootte (~28%) is conditie-specifiek — herhaal de decompositie op de volledige conditiematrix voor een formele migratie-sign-off. Verificatieplots + reproduceerscript: `so-rp_swan/verificatie_plots/`.

## Verificatie: gerichte poortselectie

Niet elke wijziging hoeft de volledige breedte (serieel + OpenMP + MPI + strict). Bepaal per wijziging met `python3 scripts/select_gates.py [--base HEAD]` welke poorten de wijziging kán breken en draai alleen die, met de afgedrukte commando's:

- MPI alleen bij MPI-rakende paden (bron met mpi/parall/metis/coh/esmf/adcirc in de naam, MPI-tests, toolchain/CMake). Deck- of fysicawijzigingen zonder codeverschil gedragen zich onder MPI identiek aan serieel op dezelfde code.
- strict alleen bij bron-, toolchain- of budgetwijzigingen. Alleen docs/commentaar: geen poorten.
- upstream_delta bij alles wat de uitvoer van een deck kan verschuiven (bron, toolchain, decks, de inventaris zelf). Die poort haalt elk deck over de vastgezette TU Delft-binary én over de fork en toetst het resultaat tegen `doc/upstream-delta.json`: elk verschil moet een BF-nummer hebben. De inventaris zelf wordt bij elk commit in de pytest-poort gecontroleerd; de gemeten helft draait onder `SWAN_UPSTREAM_DELTA=1` en vraagt eenmalig een upstream-bouw in `.upstream/` (een pad zonder streepje erin, want upstream `switch.pl` loopt op een streepje vast in een oneindige lus).
- Onbekende paden kiezen veilig alles; nooit stil niets.
- Overgeslagen poorten blijven staan op hun laatste groene commit — vermeld die commit bij de wijziging, zodat de releaseclaim herleidbaar blijft.

## Prestatiemetingen: altijd ook multicore

SWAN wordt in de praktijk vrijwel altijd multicore gedraaid. Een snelheidsclaim
op één kern zegt daarom weinig over de winst die een gebruiker ziet, en kan die
zelfs verkeerd voorspellen: een ingreep die serieel wint kan multicore verliezen
zodra hij de belastingverdeling scheeftrekt, een synchronisatiepunt toevoegt, of
geheugenbandbreedte opsnoept die bij één thread nog ruim was.

Meet daarom elke prestatiewijziging op **minstens één seriële en één multicore
configuratie**, en vermeld beide. De seriële meting isoleert het rekenwerk, de
multicore meting laat zien wat de gebruiker overhoudt. Geef het threadaantal,
de pinning en de gebruikte kernen erbij; zonder die gegevens is een tijd niet
reproduceerbaar.

Twee dingen die daarbij horen:

- **Bewaak bytegelijkheid over threadaantallen.** De uitvoer hoort onafhankelijk
  te zijn van het aantal threads. Meet dat expliciet mee bij 1, 2, 4 en meer
  threads; een prestatiewinst die het determinisme breekt is geen winst.
- **Meet op een stille machine.** Draait er iets anders op dezelfde node — een
  tweede agent, een bouw, een andere som — dan is de tijd onbruikbaar. Controleer
  dat vooraf en vermeld het als het niet lukte.

Dit geldt ook voor een optie die standaard uitstaat: het uitgeschakelde pad moet
aantoonbaar gratis zijn, en dat is juist multicore niet vanzelfsprekend. Eén
onvoorwaardelijke atomaire teller in de roosterpuntlus kost serieel weinig en
multicore een contentiepunt.

## Convergentie: "geconvergeerd" is niet hetzelfde als "stopte met veranderen"

Het stationaire stopcriterium van SWAN meet uitsluitend de *verandering* van Hs
en Tm tussen iteraties. Onderrelaxatie — de `[alfa]` van false time stepping —
maakt die verandering kleiner, en dat is precies waar hij voor dient. De test
kan die twee oorzaken niet uit elkaar houden, en er is geen residu om het aan te
toetsen. SWAN's enige beveiliging is één iteratie breed: het criterium mag niet
op de eerste iteratie afgaan.

Dat is hier geen randgeval: het overgrote deel van de decks in deze repo zet
`ALFA`. Gemeten op `examples/nonlinear_interactions/quad/quad_dia1`: bij
`ALFA=0.01` convergeert de som in 40 iteraties, bij `ALFA=0.05` en `0.10` stopt
hij na twee iteraties met de melding "accuracy OK in 100.00 %" terwijl Hs op
25 km dan 1,278 m is in plaats van de 1,405 m van de geconvergeerde run.

Zolang er geen balansresidu is, is vergelijken de enige betrouwbare controle.
`so-rp_swan/matrix/convergence_check.py` doet dat per conditie: eerst de eigen
numeriek van het deck, dan een referentie met zwakkere onderrelaxatie en een
ruimere iteratielimiet, en daarna een oordeel — `converged`, `premature`,
`not-converged` of `inconclusive`. Draai hem bij elke wijziging die aan de
numeriek, de brontermen of de deckinstellingen raakt, en neem `inconclusive`
serieus: dat betekent dat ook de referentie niet convergeerde en er dus niets
is om op te steunen.
