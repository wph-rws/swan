# Thread-state-manifest

Fase 5 van [moderniseringsplan.md](moderniseringsplan.md). Dit is een
inventaris, geen codewijziging: het legt vast welke modulevariabelen per thread
bestaan, hoe ze aan een waarde komen en wie ze na de migratie zou moeten
bezitten. Zonder deze inventaris kan `M_WCAP` (fase 9) niet veilig worden
gemigreerd.

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

## De acht directives

| # | Locatie | Module | Symbolen | Buildvariant |
|---|---|---|---|---|
| 1 | [swmod1.f90:1905](../src/swmod1.f90#L1905) | `SWCOMM3` | `IXCGRD, IYCGRD, KCGRD, COSLAT` | altijd |
| 2 | [swmod1.f90:1906](../src/swmod1.f90#L1906) | `SWCOMM3` | `RDFSIN` | altijd |
| 3 | [swmod1.f90:2542](../src/swmod1.f90#L2542) | `SWCOMM3` | `ICMAX, CSETUP` | altijd |
| 4 | [swmod1.f90:2666](../src/swmod1.f90#L2666) | `SWCOMM4` | `IPTST, TESTFL` | altijd |
| 5 | [swmod1.f90:2694](../src/swmod1.f90#L2694) | `SWCOMM4` | `PROPSL` | altijd |
| 6 | [swmod2.f90:102](../src/swmod2.f90#L102) | `M_WCAP` | 11 integraalparameters | altijd |
| 7 | [SwanCompdata.f90:69](../src/SwanCompdata.f90#L69) | `SwanCompdata` | `vs` | altijd |
| 8 | [swan_time.f90:31](../src/swan_time.f90#L31) | `swan_time` | `DCUMTM, TIMERS, NCUMTM, LISTTM, LASTTM` | **alleen `!TIMG`** |

Directive 8 staat achter de `!TIMG`-schakelaar en is in een standaardbuild
inactief. Een `THREADPRIVATE`-inventaris die alleen op actieve regels kijkt
mist hem; de driftcontrole leest daarom ook de geschakelde varianten.

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
| 11 `M_WCAP`-parameters | — | — |

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

### `M_WCAP` — integraalparameters per roosterpunt

Alle elf worden uitsluitend in `SINTGRL` geschreven
([swancom1.f90:5645-5764](../src/swancom1.f90#L5645-L5764)); alleen vier
bestanden importeren de module. Geen enkele staat in COPYIN.

| Symbool | Default bij entry | Schrijfconditie | Cat. | Invariant |
|---|---|---|---|---|
| `KM_WAM` | `10.` [r.5651](../src/swancom1.f90#L5651) | verfijnd onder `EDRKTOT > 0.` | 4 | bewezen |
| `KM01` | `10.` [r.5652](../src/swancom1.f90#L5652) | verfijnd onder `EKTOT > 0.` | 4 | bewezen |
| `SIGM01` | `10.` [r.5654](../src/swancom1.f90#L5654) | verfijnd onder `ETOT1 > 0.` | 4 | bewezen |
| `SIGM_10` | `10.` [r.5655](../src/swancom1.f90#L5655) | verfijnd onder `ACTOT > 0.` | 4 | bewezen |
| `ACTOT` | **geen** | `IF (ETOT > 0.)` [r.5696](../src/swancom1.f90#L5696) | **5** | **ontbreekt** |
| `ETOT1` | **geen** | idem | **5** | **ontbreekt** |
| `ETOT2` | **geen** | idem | **5** | **ontbreekt** |
| `ETOT4` | **geen** | idem | **5** | **ontbreekt** |
| `EDRKTOT` | **geen** | idem | **5** | **ontbreekt** |
| `EKTOT` | **geen** | idem | **5** | **ontbreekt** |
| `SIGM_WAM` | **geen** | `IF (EDRKTOT > 0.)` binnen `IF (ETOT > 0.)` | **5** | **ontbreekt** |

Voorgestelde eigenaar voor alle elf: `wcap_workspace_t`, opgenomen in
`source_workspace_t`, levensduur punt.

**De blokkerende bevinding.** Bij `ETOT <= 0.` schrijft `SINTGRL` zeven van de
elf niet, terwijl [r.5888](../src/swancom1.f90#L5888) `AC2TOT = ACTOT`
onvoorwaardelijk uitvoert. Die thread draagt dan de waarde over van het vorige
punt dat hij behandelde. Voor `ACTOT` is de weg naar buiten direct aangetoond;
de overige zes worden door de whitecapping-routines in `swancom2` gelezen zonder
dat bewezen is dat die lezingen altijd achter `ETOT > 0.` liggen.

Dit is géén vrijbrief om ze te initialiseren. Nul zetten kan een latente fout
repareren én bestaand gedrag veranderen — en dat is precies wat randvoorwaarde 1
verbiedt. Vóór fase 9 moet er per symbool een invarianttest komen die vastlegt
wat de huidige code doet; een eventuele correctie is daarna een aparte,
expliciet aangekondigde wijziging.

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

## Wat hieruit volgt voor fase 9

1. Zeven `M_WCAP`-symbolen hebben een invarianttest nodig vóór migratie.
2. De stencil bestaat dubbel (`vs` en `KCGRD`); de migratie moet één eigenaar
   kiezen, niet beide velden overnemen.
3. `common_thread_seed_t` bevat exact de vijf COPYIN-symbolen die beide solvers
   delen; `CSETUP` en `PROPSL` horen in de gestructureerde workspace.
4. De OpenMP-gate moet `TESTFL`/`IPTST` meenemen: die bepalen de
   `TEST`-uitvoer, dus een migratiefout is daar zichtbaar in de PRINT-diff en
   niet in de Hsig-statistiek.
