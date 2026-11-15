@echo off
rem Builds for the final measurement round, with MSYS2 ucrt64 gfortran 16.2, CMake and Ninja.
rem   build.cmd NAME      NAME = final_omp, final_serial, final_mpi, old_omp, old_serial, old_mpi,
rem                              final_native, final_v3, probe, pgo_gen, pgo_use, pgo_use_omp
rem "final" is the fork at commit 2466aa4 (src\final), "old" the fork at e68e9be (src\e68e9be),
rem "probe" is final with swancom1_probe.f90 (timers around waiting in the sweep).
set PATH=C:\msys64\ucrt64\bin;%PATH%;C:\msys64\usr\bin
set FC=C:/msys64/ucrt64/bin/gfortran.exe
set R=C:\swan-win-perf\final-round
set COMMON=-G Ninja -DCMAKE_Fortran_COMPILER=%FC% -DCMAKE_BUILD_TYPE=Release -DBUILD_TESTING=OFF
set N=%1
set SRC=
set OPT=
if "%N%"=="final_omp"    (set SRC=final&   set OPT=-DOPENMP=ON)
if "%N%"=="final_serial" (set SRC=final&   set OPT=-DOPENMP=OFF)
if "%N%"=="final_mpi"    (set SRC=final&   set OPT=-DMPI=ON)
if "%N%"=="old_omp"      (set SRC=e68e9be& set OPT=-DOPENMP=ON)
if "%N%"=="old_serial"   (set SRC=e68e9be& set OPT=-DOPENMP=OFF)
if "%N%"=="old_mpi"      (set SRC=e68e9be& set OPT=-DMPI=ON)
if "%N%"=="final_native" (set SRC=final&   set OPT=-DOPENMP=ON -DSWAN_NATIVE=ON)
if "%N%"=="final_v3"     (set SRC=final&   set OPT=-DOPENMP=ON "-DCMAKE_Fortran_FLAGS=-march=x86-64-v3 -ffp-contract=off")
if "%N%"=="probe"        (set SRC=probe&   set OPT=-DOPENMP=ON)
if "%N%"=="pgo_gen"      (set SRC=final&   set OPT=-DOPENMP=OFF "-DCMAKE_Fortran_FLAGS=-fprofile-generate -fprofile-dir=%R:\=/%/pgodata -fprofile-update=single" -DCMAKE_EXE_LINKER_FLAGS=-fprofile-generate)
if "%N%"=="pgo_use"      (set SRC=final&   set OPT=-DOPENMP=OFF "-DCMAKE_Fortran_FLAGS=-fprofile-use -fprofile-dir=%R:\=/%/pgodata -fprofile-correction -Wno-missing-profile")
if "%SRC%"=="" (echo unknown build %N% & exit /b 1)
rem the profile data are found by the path of the object files, so pgo_use builds in the directory of pgo_gen
set B=%R%\build-%N%
if "%N%"=="pgo_use" set B=%R%\build-pgo_gen
cmake -S %R%\src\%SRC% -B %B% %COMMON% %OPT% > %R%\logs\config-%N%.log 2>&1
cmake --build %B% -j 6 --target swan.exe > %R%\logs\build-%N%.log 2>&1
echo build %N% exit %ERRORLEVEL%
if "%N%"=="pgo_use" copy /y %B%\bin\swan.exe %R%\swan_pgo_use.exe > nul
if "%N%"=="pgo_gen" copy /y %B%\bin\swan.exe %R%\swan_pgo_gen.exe > nul
