#!/bin/bash
# Section C3 of the final round: one thread of the OpenMP build of the fork at 2466aa4 on a
# performance core (logical CPU 12) and on an efficiency core (logical CPU 4), Voordelta, in turn.
cd /mnt/c/swan-win-perf/final-round
PS1='C:\swan-win-perf\upstream-vergelijking\speed_vs_upstream.ps1'
R='C:\swan-win-perf\final-round'
VD='C:\swan-win-perf\upstream-vergelijking\deck'
F="$R\\build-final_omp\\bin\\swan.exe"
alone() {  # label cpu repetitions
   local cmd="& '$PS1' -ForkExe '$F' -DeckDir '$VD' -Kind openmp -Workers 1 -CpuList $2 -ResidualRounds 0 -ResidualWorkers 1 -Repetitions $3 -Label '$1' -OutDir '$R\\results\\C-$1'"
   echo "=== $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/C-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/C-$1.log | tr -d '\r' | cut -c1-150)"
}
for round in 1 2 3; do
   alone pcore-$round 12 2
   alone ecore-$round 4 2
done
echo C3_DONE
