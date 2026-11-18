# The timing series of 17 and 18 November 2026 (benchmarks/speed-vs-upstream.md,
# "Work taken out on 17 and 18 November 2026"). Run under Windows with nothing
# else running:
#
#   powershell -NoProfile -ExecutionPolicy Bypass -Command "& 'C:\temp\swan-speed-1117\sweep_2026_11_17.ps1'"
#
# It expects, beside this script:
#
#   base\, c1\, c2\, c3\, c4\    one directory per source state, each with
#                                build-omp\bin\swan.exe and build-ser\bin\swan.exe
#   decks\voordelta, decks\sorp  the decks forced to eight iterations
#   decks\voordelta-full         examples\voordelta as it is
#   decks\collinear, decks\noncollinear   from doc\measurements\dcta_decks.py
#
# and upstream in C:\temp\swanuvgl (a path without a hyphen), built at -O3 in
# build_O3_ser and, with OpenMP and -fno-ivopts -fno-ipa-cp-clone, in
# build_O3_omp.
#
# The source states are: base = 4ea92ec, c1 = dc27d92, c2 = 510d38a,
# c3 = 4ffe8af and c4 = 0ec1db1.
# Every program was built with gfortran 16.2 of MSYS2 (ucrt64), CMake and
# Ninja: -DCMAKE_BUILD_TYPE=Release, with -DOPENMP=ON for build-omp.
#
# benchmarks\windows\collect_results.py gathers the summaries into
# results\series-2026-11-17.json. The series of c4 ran on 18 November and
# is in results\series-2026-11-18.json.

$ErrorActionPreference = 'Stop'
$root = $PSScriptRoot
$script = Join-Path $root 'c3\src\benchmarks\windows\speed_vs_upstream.ps1'
$cpus = @(12, 14, 16, 18, 10, 0)

# One series: the OpenMP builds on 1, 2, 4 and 6 threads, fifteen runs per
# build in turn, and the serial builds, seven runs.
function Pair($before, $after, $deck, $blocks, $table) {
    $common = @{ CpuList = $cpus; ResidualRounds = 0; DeckFile = 'INPUT'
                 DeckDir = (Join-Path $root "decks\$deck"); BlockFiles = $blocks; TableFile = $table }
    $label = "$deck-$before-$after"
    & $script @common -Repetitions 15 -Workers @(1, 2, 4, 6) `
        -ForkExe (Join-Path $root "$after\build-omp\bin\swan.exe") `
        -UpstreamExe (Join-Path $root "$before\build-omp\bin\swan.exe") `
        -Label "$label-omp" -OutDir (Join-Path $root "results\$label-omp")
    & $script @common -Repetitions 7 -Workers @(1) `
        -ForkExe (Join-Path $root "$after\build-ser\bin\swan.exe") `
        -UpstreamExe (Join-Path $root "$before\build-ser\bin\swan.exe") `
        -Label "$label-ser" -OutDir (Join-Path $root "results\$label-ser")
}

$v = @('voordelta_hs.blk', 'voordelta_depth.blk'), 'voordelta_sites.tbl'
$s = @('hs.blk', 'p.sp1'), 'p.tbl'
$d = @('height.blk', 'triad_dcta.spc'), 'triad_dcta.tbl'

# each commit against the one before, and the three together
Pair base c1 voordelta $v[0] $v[1]
Pair c1 c2 voordelta $v[0] $v[1]
Pair base c3 voordelta $v[0] $v[1]
Pair base c1 sorp $s[0] $s[1]
Pair c1 c2 sorp $s[0] $s[1]
Pair base c3 sorp $s[0] $s[1]
Pair c2 c3 collinear $d[0] $d[1]
Pair c2 c3 noncollinear $d[0] $d[1]

# the rotation of the full circle in the non-collinear DCTA
Pair c3 c4 noncollinear $d[0] $d[1]

# the whole Voordelta run: against upstream at -O3, and against 4ea92ec
$full = @{ CpuList = $cpus; Repetitions = 5; ResidualRounds = 0
           DeckDir = (Join-Path $root 'decks\voordelta-full') }
& $script @full -Workers @(1) -ForkExe (Join-Path $root 'c3\build-ser\bin\swan.exe') `
    -UpstreamExe 'C:\temp\swanuvgl\build_O3_ser\bin\swan.exe' `
    -Label 'upstream-ser' -OutDir (Join-Path $root 'results\upstream-ser')
& $script @full -Workers @(1, 2, 4, 6) -ForkExe (Join-Path $root 'c3\build-omp\bin\swan.exe') `
    -UpstreamExe 'C:\temp\swanuvgl\build_O3_omp\bin\swan.exe' `
    -Label 'upstream-omp' -OutDir (Join-Path $root 'results\upstream-omp')
& $script @full -Workers @(1, 2, 4, 6) -ForkExe (Join-Path $root 'c3\build-omp\bin\swan.exe') `
    -UpstreamExe (Join-Path $root 'base\build-omp\bin\swan.exe') `
    -Label 'full-base-c3-omp' -OutDir (Join-Path $root 'results\full-base-c3-omp')
& $script @full -Workers @(1) -ForkExe (Join-Path $root 'c3\build-ser\bin\swan.exe') `
    -UpstreamExe (Join-Path $root 'base\build-ser\bin\swan.exe') `
    -Label 'full-base-c3-ser' -OutDir (Join-Path $root 'results\full-base-c3-ser')
