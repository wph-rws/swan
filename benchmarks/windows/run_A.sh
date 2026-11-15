#!/bin/bash
# Section A of the final round: the fork at 2466aa4 against upstream 43e9bbb and against the fork at
# e68e9be, on the Voordelta example, under native Windows on the performance cores.
# Called from WSL; every series is one call of speed_vs_upstream.ps1.
cd /mnt/c/swan-win-perf/final-round
PS1='C:\swan-win-perf\upstream-vergelijking\speed_vs_upstream.ps1'
R='C:\swan-win-perf\final-round'
U='C:\swanuvgl'
DECK='C:\swan-win-perf\upstream-vergelijking\deck'
series() {  # label fork upstream kind workers upstreamworkers residualrounds repetitions
   local cmd="& '$PS1' -ForkExe '$2' -UpstreamExe '$3' -DeckDir '$DECK' -Kind $4 -Workers $5 -UpstreamWorkers $6 -ResidualRounds $7 -Repetitions $8 -Label '$1' -OutDir '$R\\results\\$1'"
   echo "=== $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/A-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/A-$1.log | tr -d '\r' | cut -c1-150)"
}
F="$R\\build-final_omp\\bin\\swan.exe"
FS="$R\\build-final_serial\\bin\\swan.exe"
FM="$R\\build-final_mpi\\bin\\swan.exe"
O="$R\\build-old_omp\\bin\\swan.exe"
OS="$R\\build-old_serial\\bin\\swan.exe"
OM="$R\\build-old_mpi\\bin\\swan.exe"
series serial-default       "$FS" "$U\\build_serial_default\\bin\\swan.exe"   openmp 1       1       0 3
series serial-o3            "$FS" "$U\\build_serial_o3\\bin\\swan.exe"        openmp 1       1       0 3
series openmp-default       "$F"  "$U\\build_default_noivopts\\bin\\swan.exe" openmp 1,2,4,6 1,2,4,6 0 3
series openmp-o3            "$F"  "$U\\build_o3_noivopts\\bin\\swan.exe"      openmp 1,2,4,6 1,2,4,6 7 3
series mpi-default          "$FM" "$U\\build_mpi_default\\bin\\swan.exe"      mpi    1,2,4,6 1,2,4,6 0 3
series mpi-o3               "$FM" "$U\\build_mpi_o3\\bin\\swan.exe"           mpi    1,2,4,6 1,2,4,6 0 3
series final-vs-old-serial  "$FS" "$OS"                                        openmp 1       1       0 5
series final-vs-old-openmp  "$F"  "$O"                                         openmp 1,2,4,6 1,2,4,6 0 5
series final-vs-old-mpi     "$FM" "$OM"                                        mpi    1,2,4,6 1,2,4,6 0 5
echo A_DONE
