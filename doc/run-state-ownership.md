# Run-state ownership

Hoort bij deel VI van [moderniseringsplan.md](moderniseringsplan.md):
gedeelde toestand ontmantelen.
Het machineleesbare contract staat in
[`run-state-ownership.json`](run-state-ownership.json) en wordt door
[`check_run_state_ownership.py`](../scripts/check_run_state_ownership.py)
tegen `SWCLME` gecontroleerd.

De nulmeting bevatte 60 losse `DEALLOCATE`-acties, vijf lijst-`DELETE`-acties en
vijf bijbehorende grendelresets. Alle zijn naar een idempotente owner-clear
gemigreerd: A1 obstakels, A2 boundarylijsten, A3 outputlijsten, A4 de algemene
arrays uit `M_GENARR` inclusief `SINBAC`/`SAVE_SINBAC`, A5 de invoervelden, de
vegetatielagen en het globale rooster, A6 de parallelle opslag, de
ongestructureerde grid- en rekendata en de IEM/Bragg/QCM-toestand. Daarbij is
een verwisseling in de A0-inventaris hersteld: `fb/fbdxy` staan in
`SwanBraggScat`, `kx…disbk1` in `SwanQCM` en `iss/iwt/itt/freq/E0/Ebig` in
`SwanIEM` — het manifest volgt nu de code. Tijdelijke toestand buiten `SWCLME`
(`kvertc/kvertf`, `IWEIG`, `botspc/dpmean`) blijft bij haar eigen
allocatie/deallocatiepaar en valt bewust buiten dit contract. Er resteren nul
losse `DEALLOCATE`-, lijst-`DELETE`- of grendelacties; `SWCLME` bevat alleen
nog owner-level `clear`-aanroepen (20 stuks).

Bij iedere eigendomsstap verhuizen de betreffende symbolen in het manifest van
`deallocate` of `delete` naar `owner_call`. De controle faalt wanneer een losse
actie verdwijnt zonder geregistreerde owner-call, wanneer een nieuw symbool in
`SWCLME` verschijnt, of wanneer twee owners hetzelfde symbool claimen.

De tabel is bewust geen algemeen `swan_run_t`: iedere bestaande module blijft
eigenaar van haar eigen levensduur. Alleen de opruimgrens wordt expliciet en
idempotent. Voor arrays die naar een typecomponent of alias in een hete lus
verhuizen blijft de prestatie-eis uit §4 van het plan verplicht.
