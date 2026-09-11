# AGENTS.md

Gedeelde projectinstructies voor AI-coding-agents (Codex, Claude Code, e.a.) in deze SWAN-repo.

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
- Onbekende paden kiezen veilig alles; nooit stil niets.
- Overgeslagen poorten blijven staan op hun laatste groene commit — vermeld die commit bij de wijziging, zodat de releaseclaim herleidbaar blijft.
