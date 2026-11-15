@echo off
rem Section F: run the serial Windows build of the fork at 2466aa4 on the four decks of linux-ref,
rem and keep the output files in windows-out\<deck> for the comparison with the Linux builds.
setlocal enabledelayedexpansion
set PATH=C:\msys64\ucrt64\bin;%PATH%
set R=C:\swan-win-perf\final-round
set E=%R%\src\final\examples
if exist %R%\windows-out rmdir /s /q %R%\windows-out
call :one voordelta        %E%\voordelta
call :one sorp_physics     %R%\decks\sorp_physics
call :one quick_test       %E%\quick_test
call :one source_terms_st6 %E%\source_terms_st6
exit /b 0

:one
set W=%R%\windows-out\%1
mkdir %W%
for %%f in (%2\*.bot %2\*.dep %2\*.dat %2\*.wnd) do copy /y %%f %W%\ > nul
copy /y %R%\linux-ref\%1\INPUT.deck %W%\INPUT > nul
cd /d %W%
start /affinity 1000 /wait /b %R%\build-final_serial\bin\swan.exe > nul 2>&1
if exist norm_end (echo %1 ok) else (echo %1 FAILED)
cd /d %R%
exit /b 0
