@echo off
rem Section D1: the probe build (timers around waiting in the sweep) on Voordelta, on 1, 2, 4 and 6
rem threads, each thread on its own performance core, three runs each. p1_probe.txt of every run is
rem kept in results\D-probe. Columns: thread, time waiting for the upwind row, time in the sweep loop,
rem time at the barrier after the sweep, time in master-only code, time in the parallel region.
setlocal enabledelayedexpansion
set PATH=C:\msys64\ucrt64\bin;%PATH%
set R=C:\swan-win-perf\final-round
set D=C:\swan-win-perf\upstream-vergelijking\deck
set OMP_PLACES=cores
set OMP_PROC_BIND=close
if not exist %R%\results\D-probe mkdir %R%\results\D-probe
for %%N in (1 2 4 6) do (
  if %%N==1 set MASK=1000
  if %%N==2 set MASK=5000
  if %%N==4 set MASK=55000
  if %%N==6 set MASK=55401
  set OMP_NUM_THREADS=%%N
  for %%K in (1 2 3) do (
    if exist %R%\probework rmdir /s /q %R%\probework
    mkdir %R%\probework
    copy /y %D%\voordelta.dep %R%\probework\ > nul
    copy /y %D%\voordelta.swn %R%\probework\INPUT > nul
    cd /d %R%\probework
    start /affinity !MASK! /wait /b %R%\build-probe\bin\swan.exe > nul 2>&1
    if exist norm_end (copy /y p1_probe.txt %R%\results\D-probe\p1_probe_%%N_%%K.txt > nul & echo threads %%N run %%K ok) else (echo threads %%N run %%K FAILED)
    cd /d %R%
  )
)
