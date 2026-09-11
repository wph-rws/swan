# Supportmatrix — 7 september 2026

Per combinatie: geslaagd / gefaald / niet uitgevoerd / n.v.t., met versie, OS,
architectuur, vlaggen, bibliotheken en bronstand. Niet-geïnstalleerd,
niet-gelicentieerd en ontbrekende hardware zijn verschillende redenen voor
"niet uitgevoerd". Alleen gemeten combinaties gelden als ondersteund.

Referentiebronstand voor deze tabel: de beoordeelde kandidaat van 7 september 2026
(pvalid zuiver, METIS-BIND(C), DTSTTI-init, bibliotheekcontract, CMake-herkenning).
Herhaal per release op dezelfde kandidaat.

## Compilers

| Compiler | Versie/OS/arch | Status | Bewijs |
|---|---|---|---|
| GNU 13 (referentie) | 13.3.0 / WSL2-Ubuntu x86-64 | **geslaagd** (serieel/OpenMP/strict/MPI-2rank/runtime) | schone strict-bouw groen (1361/504 op 7-9; 1360/503 op "Isoleer de tests en sluit de geheugenlevensduur af", 8-9; 1358/502 na het `RI`-herstel; 1324/488 na de fysicabenoeming, 10-9; **1317 tegen budget 1318** na argumentbundeling A-D, 11-9); CTest 58/61/62/63/67 per config; installatieproef + quick_test-regressie groen |
| GNU 15 (nieuwere GNU) | 15.2.0 / WSL2-Ubuntu x86-64 | **geslaagd** (configure + telling 44; volledige suite periodiek) | `ctest -N` 58; volledige groen-meting periodiek vóór release |
| Intel `ifx` (IntelLLVM, tweede implementatie) | niet geïnstalleerd hier | **niet uitgevoerd** (ontbrekende toolchain) | kandidaat; eerst taalproeven + serieel, daarna OpenMP/MPI/netCDF + operationele regressies; claim "tweede implementatie ondersteund" uitgesloten tot groen |
| Intel `ifort` (classic) | niet geïnstalleerd | **niet uitgevoerd** (historisch) | historische compatibiliteit, geen nieuwe basis |
| LLVM Flang / Flang / IBMFlang | niet geïnstalleerd | **niet uitgevoerd** (kandidaat) | aanvullende controle na taalproeven + serieel uitvoerbewijs |
| NAG | niet geïnstalleerd (licentie vereist) | **niet uitgevoerd** (niet-gelicentieerd) | waardevolle aanvullende taal-/runtimecontrole na licentie + proeven |
| IBM XL / Open XL, Fujitsu, HPE Cray | geen doelhardware hier | **niet uitgevoerd** (partner-runner vereist) | externe kwalificatie per toolchain/hardware/testlogs |
| Lahey, DEC/SGI, PGI/NVFortran | historisch / scope-uitgesloten | **n.v.t.** (historisch bewaard, niet ondersteund) | Lahey/DEC/SGI als ongetest gelabeld; PGI/NVFortran expliciet buiten scope |
| METIS 32-bit (libmetis-dev 5.1.0, IDX32/REAL32) | /usr/include+libmetis.so | **geslaagd** (METIS+MPI echte partitionering) | `metis_partitioning` + `quick_test_mpi` groen; header+bib uitzelfde installatie; breedtes 32/32 bij configure gecontroleerd |
| METIS 64-bit-index | niet geïnstalleerd | **niet uitgevoerd** (ontbrekende bib) | vereist afgeleide 64-bit-interface + test met 32- én 64-bit-bibs; zonder METIS geen afhankelijkheid |
| netCDF-Fortran / MPI (OpenMPI, GNU-wrapper) | systeemlibs | **geslaagd** waar getest (netCDF 61, MPI 62, MPI+netCDF 67) | CTest-tellingen; passende MPI/netCDF-modules per compiler vereist (geen `.mod`-hergebruik zonder compatibiliteitsbewijs) |
| Native Windows (MSYS2 UCRT64-GNU, ifx-Windows) | niet gemeten hier | **niet uitgevoerd** | aparte bouw-/uitvoerproeven, paden met spaties, I/O, Matlab/netCDF, DLL-vindbaarheid, geïnstalleerde executable; WSL groen certificeert geen Windows-binary |
| WSL2-Ubuntu x86-64 | kernel 6.6.87.2-microsoft-standard-WSL2 | **geslaagd** (Linux-ontwikkeling + kleine MPI) | clusterprestatie op beoogde productiehardware |

## Configuraties (CTest-registratie 2026-09-07)

58 voor standaard/OpenMP/TIMG(/OpenMP)/MATL4/METIS/FFRO/COH/ESMF/ADCIRC/LTO/
native/runtime/debug-invariants/legacy-I/O/GNU15/strict/debug; 61 voor
netCDF/MATL4+netCDF/MPI/JAC/TIMG+MPI/MATL4+MPI/FFRO+MPI; 63 METIS+MPI; 67
MPI+netCDF. `debug` is onderzoeksport (bekend rood op nonstationary).

Vereiste releasecombinaties en beschikbare runners liggen per release vast;
tweede implementatie vroeg beproefd (doorlopend). Een ontbrekende
kandidaat blokkeert geen gekwalificeerde GNU-route, maar sluit de bijbehorende
supportclaim wel uit.
