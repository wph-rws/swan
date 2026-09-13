# Ontwerp: een balansresidu voor de stationaire SWAN-oplossing

## Waarom

Op 13 september 2026 liep hetzelfde probleem drie keer tegen dezelfde muur aan.

Bij het versnellingsonderzoek: van geen enkele niet-bytegelijke ingreep viel aan
te tonen dat hij dezelfde oplossing oplevert, dus kon geen enkele versnelling
worden geland. Bij `QUADRUPL 1`: de som oscilleert eindeloos, maar of dat een
limietcyclus of trage convergentie is, was alleen met 599 iteraties te zien in
plaats van met één getal. En op de operationele conditie: twee runs die allebei
het 98%-criterium halen verschillen 45,9 mm RMS in Hs, veldbreed.

De oorzaak is telkens dezelfde. SWAN's stationaire stopcriterium meet de
*verandering* van Hs en Tm tussen iteraties. Dat is een maat voor "beweegt het
nog", niet voor "klopt het". De twee lopen uiteen zodra iets de verandering
klein maakt zonder de oplossing te bereiken — onderrelaxatie doet dat met opzet,
de actielimiter doet het als bijwerking, en een limietcyclus doet het vanzelf.

Zonder een tweede, onafhankelijke maat is dat onderscheid niet te maken. Dat is
geen tekortkoming van deze fork; het is zo in upstream 41.51.

## Wat het moet zijn

Het residu van de discrete actiebalans, geëvalueerd op het huidige veld, zonder
dat veld bij te werken:

```
r(x, sigma, theta) = d(c_x N)/dx + d(c_y N)/dy + d(c_sigma N)/dsigma
                   + d(c_theta N)/dtheta  -  S_tot / sigma
```

Cruciaal is *discreet* en niet continu: het residu moet met dezelfde
discretisatie worden gevormd die de solver gebruikt, anders meet je de
discretisatiefout in plaats van de convergentiefout. Voor de stationaire
reguliere route is dat het SORDUP/S&L-schema; voor de ongestructureerde route de
eigen stencil.

Dat maakt de implementatie eenvoudiger dan ze lijkt. SWAN stelt per roosterpunt
per sweep al een lokaal stelsel op in `IMATDA` en `IMATRA`. Het residu is wat
daarvan overblijft wanneer je het huidige `AC2` invult in plaats van ernaar op te
lossen. Één extra doorloop van de puntlus, zonder oplosstap en zonder limiter,
levert het.

## Aanpak

1. **Eén diagnostische doorloop na de laatste sweep van een iteratie.** Loop de
   roosterpunten in dezelfde volgorde af, bouw `IMATDA`/`IMATRA` zoals `SOURCE`
   en de propagatieroutines dat doen, maar vervang de aanroep van `SOLMAT` door
   het invullen van het huidige `AC2`. Het verschil is het residu per bin.
2. **Schaal per punt.** Een onbewerkt residu is niet vergelijkbaar tussen diep
   en ondiep water. Deel door een lokale schaal — de som van de absolute
   afzonderlijke termen is de gebruikelijke keuze, want die maakt het residu
   dimensieloos en begrensd door 1.
3. **Rapporteer twee getallen per iteratie**, naast het bestaande
   nauwkeurigheidspercentage: het maximum over de natte punten en de wortel uit
   het gemiddelde kwadraat. Beide in het PRINT-bestand, in dezelfde vorm als de
   bestaande `accuracy OK`-regel, zodat bestaande parsers er niet over vallen.
4. **Stopcriterium erbij, niet ervoor in de plaats.** Een nieuw sleutelwoord op
   `NUMERIC` dat een residudrempel zet. Blijft dat sleutelwoord weg, dan verandert
   er niets aan het gedrag — dat is de voorwaarde om dit überhaupt te kunnen
   landen naast een verificatiematrix.

## Wat de valkuilen zijn

**De limiter en de energiekap horen er niet in.** `PHILIM` en de kap in
`SINTGRL` begrenzen de *update*. Het residu moet de onbewerkte balans meten,
anders meet je opnieuw hoe hard er geremd wordt in plaats van hoe ver je van de
oplossing af staat. Dit is de fout die het makkelijkst ongemerkt binnensluipt.

**De sweepsector.** Een sweep werkt op een deel van de richtingen. Het residu
moet over de volle cirkel worden gevormd, na de vierde sweep, niet per sweep.

**Droge punten en de exceptiewaarde.** Punten met `DEP2 <= DEPMIN` tellen niet
mee, net als bij het bestaande criterium. De `-9`-exceptiewaarde in de
uitvoervelden mag nooit in een gemiddelde belanden — dezelfde valkuil als bij de
natte-punt-middeling in `AGENTS.md`.

**Kosten.** Eén extra doorloop is ruwweg één extra iteratie per keer dat het
residu wordt bepaald. Bij elke iteratie doen is daarmee grofweg 1/N duurder bij
N iteraties, dus circa 3% bij dertig iteraties — mits de doorloop de brontermen
hergebruikt en niet opnieuw de DIA uitrekent. Doet hij dat wel, dan is het eerder
een verdubbeling van de brontermkosten. De DIA is 34,6% van de rekentijd, dus dat
verschil is het overwegen waard: bewaar `IMATRA`/`IMATDA` van de laatste sweep
per punt, of aanvaard de extra kosten en bepaal het residu alleen elke k-de
iteratie.

**Verificatie.** Het residu hoort naar nul te gaan op een som die aantoonbaar
convergeert (`quad_dia2` haalt 100%), en hoort níét naar nul te gaan op
`quad_dia1` zonder `ALFA`, die in een limietcyclus blijft. Die twee decks staan
in de repo en vormen samen de test: een residu dat op beide hetzelfde doet, is
fout.

## Wat het oplevert

Drie dingen die vandaag geen van alle konden.

Een versnelling kan worden aangetoond: dezelfde oplossing binnen een
residudrempel in minder tijd is een claim die staat, ook als de uitvoer niet
bytegelijk is. Dat is de voorwaarde waaronder het bevriezingsprototype
(`doc/bevroren-punten-metingen.json`) alsnog geland kan worden.

Schijnconvergentie wordt zichtbaar zonder A/B-run. `so-rp_swan/matrix/convergence_check.py`
draait nu elke conditie twee keer om hetzelfde vast te stellen; met een residu is
één run genoeg.

En de operationele vraag wordt beantwoordbaar: hoe ver staat een
productieconfiguratie van de oplossing af. Vandaag gemeten als 0,18 m gemiddeld
onder een geconvergeerde run, maar alleen doordat die geconvergeerde run er
toevallig naast is gelegd.

## Status

Ontwerp, niet gebouwd. Het is met opzet als opdracht opgeschreven en niet als
open vraag: de aanpak ligt vast, de valkuilen staan erbij en de verificatie is
met bestaande decks te doen.
