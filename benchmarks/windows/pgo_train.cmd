@echo off
rem Training runs for profile-guided optimisation: the build with -fprofile-generate runs Voordelta and
rem the so-rp physics deck once, serially, on logical CPU 12. The profile goes to final-round\pgodata.
set PATH=C:\msys64\ucrt64\bin;%PATH%
set R=C:\swan-win-perf\final-round
if exist %R%\pgodata rmdir /s /q %R%\pgodata
if exist %R%\pgotrain rmdir /s /q %R%\pgotrain
mkdir %R%\pgotrain\voordelta %R%\pgotrain\sorp
copy /y C:\swan-win-perf\upstream-vergelijking\deck\voordelta.dep %R%\pgotrain\voordelta\ > nul
copy /y C:\swan-win-perf\upstream-vergelijking\deck\voordelta.swn %R%\pgotrain\voordelta\INPUT > nul
copy /y %R%\decks\sorp_physics\* %R%\pgotrain\sorp\ > nul
cd /d %R%\pgotrain\voordelta
start /affinity 1000 /wait /b %R%\swan_pgo_gen.exe > nul 2>&1
if exist norm_end (echo voordelta trained) else (echo voordelta training FAILED)
cd /d %R%\pgotrain\sorp
start /affinity 1000 /wait /b %R%\swan_pgo_gen.exe > nul 2>&1
if exist norm_end (echo sorp trained) else (echo sorp training FAILED)
dir /s /b %R%\pgodata\*.gcda | find /c ".gcda"
