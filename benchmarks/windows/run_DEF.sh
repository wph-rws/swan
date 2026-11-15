#!/bin/bash
# Sections D, E and F of the final round, under native Windows on the performance cores.
#   D1 probe build: where the threads of the sweep spend their time (probe.cmd)
#   D2 N independent serial runs at the same time (concurrent.ps1)
#   E  time of the deck with QUADRUPLET 2 (the default), 3 and 1: Voordelta and the so-rp physics deck
#   F  output of the Windows build for the comparison with the Linux build (compare_F.py does the rest)
cd /mnt/c/swan-win-perf/final-round
PS1='C:\swan-win-perf\upstream-vergelijking\speed_vs_upstream.ps1'
R='C:\swan-win-perf\final-round'
F="$R\\build-final_omp\\bin\\swan.exe"

echo "=== D1 probe  $(date +%H:%M:%S)"
cmd.exe /c "probe.cmd" 2>&1 | tr -d '\r' > logs/D-probe.log
tail -2 logs/D-probe.log

echo "=== D2 concurrent serial runs  $(date +%H:%M:%S)"
powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "& '$R\\concurrent.ps1'" > logs/D-concurrent.log 2>&1
tail -4 logs/D-concurrent.log | tr -d '\r'

alone() {  # label deckdir blockfiles tablefile repetitions
   local cmd="& '$PS1' -ForkExe '$F' -DeckDir '$2' -DeckFile INPUT -BlockFiles $3 -TableFile '$4' -Kind openmp -Workers 1,4 -ResidualRounds 0 -Repetitions $5 -Label '$1' -OutDir '$R\\results\\E-$1'"
   echo "=== E $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/E-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/E-$1.log | tr -d '\r' | cut -c1-150)"
}
for q in 2 3 1; do
   alone voordelta_q$q "$R\\decks\\voordelta_q$q" "'voordelta_hs.blk','voordelta_depth.blk'" voordelta_sites.tbl 5
done
for q in 2 3 1; do
   alone sorp_q$q "$R\\decks\\sorp_q$q" "'hs.blk'" p.tbl 7
done

echo "=== F Windows output  $(date +%H:%M:%S)"
cmd.exe /c "run_F.cmd" 2>&1 | tr -d '\r' | tail -5
echo DEF_DONE
