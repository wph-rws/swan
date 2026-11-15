#!/bin/bash
# Section B of the final round: the other routes, the fork at 2466aa4 against the fork at e68e9be,
# OpenMP builds on one and on four threads, under native Windows on the performance cores.
cd /mnt/c/swan-win-perf/final-round
PS1='C:\swan-win-perf\upstream-vergelijking\speed_vs_upstream.ps1'
R='C:\swan-win-perf\final-round'
F="$R\\build-final_omp\\bin\\swan.exe"
O="$R\\build-old_omp\\bin\\swan.exe"
deck() {  # label deckdir deckfile blockfiles tablefile repetitions
   local cmd="& '$PS1' -ForkExe '$F' -UpstreamExe '$O' -DeckDir '$2' -DeckFile '$3' -BlockFiles $4 -TableFile '$5' -Kind openmp -Workers 1,4 -UpstreamWorkers 1,4 -ResidualRounds 0 -Repetitions $6 -Label '$1' -OutDir '$R\\results\\B-$1'"
   echo "=== $1  $(date +%H:%M:%S)"
   powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "$cmd" > logs/B-$1.log 2>&1
   echo "    exit $?  $(tail -1 logs/B-$1.log | tr -d '\r' | cut -c1-150)"
}
E="$R\\src\\final\\examples"
deck sorp_physics    "$R\\decks\\sorp_physics"        INPUT                    "'hs.blk'"                                     p.tbl                            5
deck nonstat_regular "$E\\nonstationary\\regular"     nonstationary_regular.swn "'nonstationary_regular_hs.blk'"               nonstationary_regular_center.tbl 3
deck nonstat_sandl   "$R\\decks\\nonstat_sandl"       INPUT                    "'nonstationary_regular_hs.blk'"               nonstationary_regular_center.tbl 3
deck st6             "$E\\source_terms_st6"           st6_testpoints.swn       "'st6_testpoints_sources.spc'"                 st6_testpoints.tbl               5
deck current_strong  "$R\\decks\\current_strong"      INPUT                    "'hs.blk','p.sp2'"                             none.tbl                         5
deck obstacle        "$R\\decks\\obstacle"            INPUT                    "'hs.blk'"                                     none.tbl                         5
deck sphys           "$E\\testmodels\\sphys"          INPUT                    "'hs.blk','tm.blk'"                            none.tbl                         3
deck slope           "$E\\testmodels\\slope"          INPUT                    "'hs.blk','tm.blk'"                            none.tbl                         3
deck unstructured    "$R\\decks\\voordelta_unstr"     INPUT                    "'voordelta_hs.blk','voordelta_depth.blk'"     voordelta_sites.tbl              2
echo B_DONE
