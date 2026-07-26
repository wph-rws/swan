# Thread-state-manifest

Hoort bij [moderniseringsplan.md](moderniseringsplan.md). Het document begon als
inventaris voor de migratie en legt nu ook vast welk thread-state is
overgebleven en waar de gemigreerde whitecappingtoestand wordt beheerd.

`scripts/check_thread_state.py` vergelijkt de tabellen hieronder met de bron en
faalt als er een `THREADPRIVATE`-symbool bijkomt of verdwijnt.

## Methode

De analyse is **importbewust**: een symbool telt alleen mee in een bestand dat de
bijbehorende module daadwerkelijk `USE`t. Dat is geen formaliteit. Een
naamgebaseerde telling schrijft tien schrijvers van `EKTOT` toe aan
[swanout1.f90:4046](../src/swanout1.f90#L4046) e.v., maar dat is een lokale
`REAL` op [r.3151](../src/swanout1.f90#L3151) in een bestand dat `M_WCAP` niet
importeert — een naamcollisie, geen gedeelde toestand.

Levensduur is de kortste eenheid waarover de waarde geldig moet blijven:
`punt` (één roosterpunt of vertex), `sweep`, `iteratie`, `run`, `proces`.

## De zeven resterende directives

| # | Locatie | Module | Symbolen | Buildvariant |
|---|---|---|---|---|
| 1 | [swmod1.f90:1905](../src/swmod1.f90#L1905) | `SWCOMM3` | `IXCGRD, IYCGRD, KCGRD, COSLAT` | altijd |
| 2 | [swmod1.f90:1906](../src/swmod1.f90#L1906) | `SWCOMM3` | `RDFSIN` | altijd |
| 3 | [swmod1.f90:2542](../src/swmod1.f90#L2542) | `SWCOMM3` | `ICMAX, CSETUP` | altijd |
| 4 | [swmod1.f90:2666](../src/swmod1.f90#L2666) | `SWCOMM4` | `IPTST, TESTFL` | altijd |
| 5 | [swmod1.f90:2694](../src/swmod1.f90#L2694) | `SWCOMM4` | `PROPSL` | altijd |
| 6 | [SwanCompdata.f90:69](../src/SwanCompdata.f90#L69) | `SwanCompdata` | `vs` | altijd |
| 7 | [swan_time.f90:31](../src/swan_time.f90#L31) | `swan_time` | `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | **alleen `!TIMG`** |

Directive 7 staat achter de `!TIMG`-schakelaar en is in een standaardbuild
inactief. Een `THREADPRIVATE`-inventaris die alleen op actieve regels kijkt
mist hem; de driftcontrole leest daarom ook de geschakelde varianten.

De voormalige zesde groep, de elf scalars uit `M_WCAP`, staat niet meer in
deze tabel: de migratie heeft de module en haar `THREADPRIVATE`-directive verwijderd.
De toestand zit nu in een expliciete `wcap_workspace_t` per solverthread.

## COPYIN: welke threads geseed worden

De twee solvers seeden verschillende verzamelingen. Dat is ontwerpinput, niet
een detail: alleen deze symbolen moeten bij het betreden van de parallelle regio
de masterwaarde hebben.

| Symbool | [swancom1.f90:1238-1241](../src/swancom1.f90#L1238-L1241) (structured) | [SwanCompUnstruc.f90:475](../src/SwanCompUnstruc.f90#L475) (unstructured) |
|---|---|---|
| `ICMAX` | ✅ | ✅ |
| `COSLAT` | ✅ | ✅ |
| `IPTST` | ✅ | ✅ |
| `TESTFL` | ✅ | ✅ |
| `RDFSIN` | ✅ | ✅ |
| `CSETUP` | ✅ | — |
| `PROPSL` | ✅ | — |
| `IXCGRD, IYCGRD, KCGRD` | — | — |
| `vs` | — | — |
| `wcap_workspace_t` | expliciet per thread | expliciet per thread |

`CSETUP` en `PROPSL` ontbreken in de ongestructureerde regio omdat die solver
het gestructureerde propagatieschema en de setup-optie niet gebruikt.

## Manifest per symbool

Categorieën volgens §6.2 van het plan: **1** run-shared read-only, **2**
run-shared mutable, **3** thread-state geseed via COPYIN, **4** bewezen
write-before-read scratch, **5** conditioneel geschreven of voortlevend, **6**
solver- of switch-specifiek.

### `SWCOMM3` — stencil en propagatiekeuzes

| Symbool | Solver | COPYIN | Eerste definitie | Levensduur | Cat. | Voorgestelde eigenaar |
|---|---|---|---|---|---|---|
| `IXCGRD` | beide | — | [swancom1.f90:3073](../src/swancom1.f90#L3073) | punt | 4 | `structured_thread_workspace_t` |
| `IYCGRD` | beide | — | [swancom1.f90:3074](../src/swancom1.f90#L3074) | punt | 4 | `structured_thread_workspace_t` |
| `KCGRD` | beide | — | swancom1 (59×), [SwanCompUnstruc.f90:933](../src/SwanCompUnstruc.f90#L933) | punt | 4 | `common_thread_seed_t` (stencil) |
| `COSLAT` | beide | ✅ | [swanmain.f90](../src/swanmain.f90) via setup; swancom5 | sweep | 3 | `common_thread_seed_t` |
| `RDFSIN` | beide | ✅ | [swanmain.f90:3757](../src/swanmain.f90#L3757) | run | 3 | `common_thread_seed_t` |
| `ICMAX` | beide | ✅ | [swanmain.f90:1033](../src/swanmain.f90#L1033) | run | 3 | `common_thread_seed_t` |
| `CSETUP` | structured | ✅ | [swanmain.f90:1143](../src/swanmain.f90#L1143) | run | 3/6 | `structured_thread_workspace_t` |

`KCGRD` is in het ongestructureerde pad een spiegel van `vs`
(`KCGRD = vs`, met het commentaar "to be used in some original SWAN routines").
Na de migratie mag er maar één stencil-eigenaar zijn; de spiegel is precies het
soort dubbele opslag dat randvoorwaarde 2 verbiedt.

### `SWCOMM4` — teststatus en lokale propagatie

| Symbool | Solver | COPYIN | Eerste definitie | Levensduur | Cat. | Voorgestelde eigenaar |
|---|---|---|---|---|---|---|
| `IPTST` | beide | ✅ | [swanmain.f90:1567](../src/swanmain.f90#L1567) e.o. | punt | 3 | `common_thread_seed_t` |
| `TESTFL` | beide | ✅ | [swanmain.f90:1567](../src/swanmain.f90#L1567) | punt | 3 | `common_thread_seed_t` |
| `PROPSL` | structured | ✅ | [swanmain.f90:1130](../src/swanmain.f90#L1130) | sweep | 3/6 | `structured_thread_workspace_t` |

`IPTST` wordt in [swanmain.f90:6094](../src/swanmain.f90#L6094) e.v. ook als
`DO`-lusvariabele hergebruikt, buiten de parallelle regio. Dat is legaal maar
betekent dat het symbool twee betekenissen draagt; bij migratie moet de
lusvariabele lokaal worden en niet het contextveld.

### Voormalig `M_WCAP` — integraalparameters per roosterpunt

Historisch werden alle elf uitsluitend in `SINTGRL` geschreven
([swancom1.f90:5645-5764](../src/swancom1.f90#L5645-L5764)); slechts vier
bestanden importeerden de module en geen enkele scalar stond in COPYIN.

| Symbool | Default bij entry | Schrijfconditie | Cat. | Invariant |
|---|---|---|---|---|
| `KM_WAM` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L62) | verfijnd onder `EDRKTOT > 0.` | 4 | bewezen |
| `KM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L63) | verfijnd onder `EKTOT > 0.` | 4 | bewezen |
| `SIGM01` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L64) | verfijnd onder `ETOT1 > 0.` | 4 | bewezen |
| `SIGM_10` | `10.` [`begin_point`](../src/swan_source_workspaces.f90#L65) | verfijnd onder `ACTOT > 0.` | 4 | bewezen |
| `ACTOT` | **geen** | [`IF (ETOT > 0.)`](../src/swancom1.f90#L5739) | **5** | getest: waarde blijft staan |
| `ETOT1` | **geen** | idem | **5** | getest: waarde blijft staan |
| `ETOT2` | **geen** | idem | **5** | getest: waarde blijft staan |
| `ETOT4` | **geen** | idem | **5** | getest: waarde blijft staan |
| `EDRKTOT` | **geen** | idem | **5** | getest: waarde blijft staan |
| `EKTOT` | **geen** | idem | **5** | getest: waarde blijft staan |
| `SIGM_WAM` | **geen** | `IF (EDRKTOT > 0.)` binnen `IF (ETOT > 0.)` | **5** | getest: waarde blijft staan |

Eigenaar voor alle elf is nu
[`wcap_workspace_t`](../src/swan_source_workspaces.f90), opgenomen in
`source_workspace_t` en per thread opgeslagen in de gestructureerde of
ongestructureerde workspace. `begin_point` zet uitsluitend de vier historische
entry-defaults op 10; de zeven categorie-5-waarden worden bewust niet geraakt.

**De categorie-5-bevinding.** Bij `ETOT <= 0.` schrijft `SINTGRL` zeven van de
elf niet, terwijl [r.5932](../src/swancom1.f90#L5932) `AC2TOT = ACTOT`
onvoorwaardelijk uitvoert. Die thread draagt dan de waarde over van het vorige
punt dat hij behandelde.

**Effect op resultaten.** Voor geldige, niet-negatieve spectra verandert
initialisatie van deze zeven waarden de huidige numerieke solveruitkomst niet:

- [`SWCAP`](../src/swancom2.f90#L2478) en
  [`SWCAP8`](../src/swancom2.f90#L2844) keren bij `ETOT <= 0.` terug voordat
  een workspacewaarde een bronterm kan beïnvloeden;
- `SSURF` leest `SIGM_WAM` voor `ISURF = 6`, maar gebruikt de gekozen
  frequentie pas achter de [`BB > 0`](../src/swancom2.f90#L2111)-voorwaarde;
  bij `ETOT = 0.` blijft de surf-breakingbijdrage nul;
- `AC2TOT` heeft in de huidige gestructureerde en ongestructureerde solver geen
  lezer na de aanroep;
- zodra `ETOT > 0.`, berekent `SINTGRL` de integralen opnieuw. Voor een geldig
  positief spectrum is ook `EDRKTOT > 0.` en wordt `SIGM_WAM` opnieuw bepaald.

Een tijdelijke directe fixture heeft oude en nulgezette waarden door
`SWCAP`, `SWCAP8` en `SSURF` gestuurd en bevestigde bitgelijke
brontermuitgangen. Die fixture is na de analyse verwijderd: de codepaden
hierboven en de compacte toestandstest zijn voldoende blijvend bewijs.

Initialiseren kan in drie uitzonderingssituaties wel merkbaar zijn:

1. De eerste nul-energieaanroep van een thread heeft zonder initialisatie nog
   geen vorige waarde. `SSURF` kopieert `SIGM_WAM` vóór de `BB`-guard; een
   strenge runtime met signalling NaN- of floating-pointtraps kan daardoor
   stoppen. Dit verandert robuustheid, niet de normale fysische uitkomst.
2. Aangepaste externe code kan de publieke `SINTGRL`-uitparameter `AC2TOT`
   gebruiken, of toekomstige SWAN-code kan een workspacewaarde vóór een guard
   gaan gebruiken. Dan kan de oude waarde van een ander punt wel doorwerken.
3. Een ongeldig spectrum met negatieve waarden of NaN's breekt de
   positiviteitsrelatie tussen `ETOT` en de overige momenten. Daarvoor geldt de
   bovenstaande resultaatsneutraliteit niet.

De verwachte fix is dus resultaatneutraal voor de huidige normale solver, maar
kan een runtimefout voorkomen en beschermt externe of toekomstige gebruikers.
Het blijft daarom een afzonderlijke correctheidswijziging.

**Bereikbaarheid in de so-rp-case.** Een tijdelijke atomaire teller rond de
`ETOT == 0.`-tak is gedraaid op de representatieve operationele conditie
(20 m/s uit 310°, NAP +3,00 m, open kering) met acht OpenMP-threads. De invoer,
grid en fysica waren ongewijzigd; alleen het iteratielimiet en de omvang van de
uitvoer zijn voor de korte proef beperkt. Na één iteratie waren er 188
`SINTGRL`-aanroepen met exact nul energie. Een afzonderlijke proef met twee
iteraties telde er 192 in totaal, dus vier extra in de tweede iteratie. Dit zijn
aanroepen over de vier sweeps, niet noodzakelijk evenveel unieke roosterpunten.
Het pad is in deze case dus werkelijk bereikbaar en niet uitsluitend een
kunstmatige unit-testsituatie.

**Beperkte testvoetafdruk.** Eén compacte fixture in
[`test_contexts.f90`](../tests/test_contexts.f90) controleert de zeven
carry-overvelden en de vier historische defaults bitgewijs. Er is bewust geen
aparte executable of blijvende so-rp-regressierun voor alleen dit fenomeen. De
migratie initialiseert de waarden dus niet en bevat geen stilzwijgende
correctheidswijziging; een eventuele fix krijgt later zijn eigen, kleine
wijziging van deze bestaande fixture.

### `SwanCompdata` — ongestructureerde stencil

| Symbool | Solver | COPYIN | Schrijver | Levensduur | Cat. | Voorgestelde eigenaar |
|---|---|---|---|---|---|---|
| `vs` | unstructured | — | `SwanCompUnstruc` (3×) | punt | 4/6 | `unstructured_thread_workspace_t` |

`vs` is `integer, dimension(MICMAX)` en is de bron van waarheid voor de
ongestructureerde stencil; `KCGRD` is de spiegel ervoor.

### `swan_time` — TIMG-tellers

| Symbool | Buildvariant | Levensduur | Cat. | Voorgestelde eigenaar |
|---|---|---|---|---|
| `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | alleen `!TIMG` | proces | 6 | `timing_context_t`, buiten de migratie |

Timing is instrumentatie, geen modeltoestand. Deze groep blijft buiten de
context-migratie zolang `!TIMG` een schakelaar is; hij staat hier omdat een
inventaris die hem weglaat onvolledig is.

## Uitkomst van de workspace-migratie

1. Alle zeven categorie-5-invarianttests zijn aanwezig en het oude gedrag is
   behouden.
2. De stencil bestaat nog dubbel (`vs` en `KCGRD`). Deze groep viel buiten de
   afgebakende `M_WCAP`-migratie; die heeft er geen derde opslaglaag aan
   toegevoegd.
3. De vijf COPYIN-symbolen die beide solvers delen blijven in hun bestaande
   modules. Deze stap heeft uitsluitend de brontermworkspace
   ingevoerd en de overige COPYIN-toestand niet dubbel opgeslagen.
4. De OpenMP-gate moet `TESTFL`/`IPTST` meenemen: die bepalen de
   `TEST`-uitvoer, dus een migratiefout is daar zichtbaar in de PRINT-diff en
   niet in de Hsig-statistiek. De gestructureerde en ongestructureerde
   referentietests zijn daarom met 1, 2 en 4 threads gedraaid.
