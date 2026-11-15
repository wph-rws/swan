#!/bin/bash
# Section C of the final round: build variants of the fork at 2466aa4 under native Windows.
#   1. the portable Release build against -DSWAN_NATIVE=ON and against -march=x86-64-v3 -ffp-contract=off
#   2. profile-guided optimisation (serial builds): trained on Voordelta and the so-rp physics deck
#   3. one thread on an efficiency core (logical CPU 4) against a performance core (12)
cd /mnt/c/swan-win-perf/final-round
PS1='C:\swan-win-perf\upstream-vergelijking\speed_vs_upstream.ps1'
R='C:\swan-win-perf\final-round'
E="$R\\src\\final\\examples"
VD='C:\swan-win-perf\upstream-vergelijking\deck'
pair() {  # label reference-exe other-exe deckdir deckfile blockfiles tablefile workers repetitions
   local cmd="& '$PS1' -ForkExe '$3' -UpstreamExe '$2' -DeckDir '$4' -DeckFile '$5' -BlockFiles $6 -TableFile '$7' -Kind openmp -Workers $8 -UpstreamWorkers $8 -ResidualRounds 0 -Repetitions $9 -Label '$1' -OutDir '$R\\results\\C-$1'"
   echo "=== $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/C-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/C-$1.log | tr -d '\r' | cut -c1-150)"
}
alone() {  # label exe cpulist
   local cmd="& '$PS1' -ForkExe '$2' -DeckDir '$VD' -Kind openmp -Workers 1 -CpuList $3 -ResidualRounds 0 -Repetitions 5 -Label '$1' -OutDir '$R\\results\\C-$1'"
   echo "=== $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/C-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/C-$1.log | tr -d '\r' | cut -c1-150)"
}
F="$R\\build-final_omp\\bin\\swan.exe"
FS="$R\\build-final_serial\\bin\\swan.exe"
VB="'voordelta_hs.blk','voordelta_depth.blk'"

# 1. AVX2 variants (the script calls the first program "upstream": here that is the portable build)
pair native-voordelta "$F" "$R\\build-final_native\\bin\\swan.exe" "$VD" voordelta.swn "$VB" voordelta_sites.tbl 1,4 5
pair v3-voordelta     "$F" "$R\\build-final_v3\\bin\\swan.exe"     "$VD" voordelta.swn "$VB" voordelta_sites.tbl 1,4 5
pair native-sorp      "$F" "$R\\build-final_native\\bin\\swan.exe" "$R\\decks\\sorp_physics" INPUT "'hs.blk'" p.tbl 1,4 7
pair v3-sorp          "$F" "$R\\build-final_v3\\bin\\swan.exe"     "$R\\decks\\sorp_physics" INPUT "'hs.blk'" p.tbl 1,4 7

# 2. PGO: train, rebuild with the profile, compare with the plain serial build
cmd.exe /c "pgo_train.cmd" 2>&1 | tr -d '\r' | tail -3
cmd.exe /c "build.cmd pgo_use" 2>&1 | tr -d '\r' | tail -1
pair pgo-voordelta "$FS" "$R\\swan_pgo_use.exe" "$VD" voordelta.swn "$VB" voordelta_sites.tbl 1 5
pair pgo-sorp      "$FS" "$R\\swan_pgo_use.exe" "$R\\decks\\sorp_physics" INPUT "'hs.blk'" p.tbl 1 7
pair pgo-slope     "$FS" "$R\\swan_pgo_use.exe" "$E\\testmodels\\slope" INPUT "'hs.blk','tm.blk'" none.tbl 1 5

# 3. E-core against P-core, one thread
alone pcore "$F" 12
alone ecore "$F" 4
echo C_DONE
