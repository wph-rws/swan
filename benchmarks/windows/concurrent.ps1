<#
Section D2: N independent serial runs of Voordelta at the same time, each on its own performance
core, for N = 1, 2, 4 and 6. The time per run tells how much slower the hardware gets when N cores
are busy (clock frequency, memory), without any waiting between threads.

Each process is started suspended in effect: it is created, its affinity is set at once, and the
time is taken from process start to process exit as Windows records them.
#>
param(
    [string] $Exe = 'C:\swan-win-perf\final-round\build-final_serial\bin\swan.exe',
    [string] $DeckDir = 'C:\swan-win-perf\upstream-vergelijking\deck',
    [string] $OutDir = 'C:\swan-win-perf\final-round\results\D-concurrent',
    [int[]] $Counts = @(1, 2, 4, 6),
    [int[]] $CpuList = @(12, 14, 16, 18, 10, 0),
    [int] $Repetitions = 3
)
$ErrorActionPreference = 'Stop'
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null
$rows = @()
foreach ($n in $Counts) {
    foreach ($rep in 1..$Repetitions) {
        $procs = @()
        foreach ($k in 0..($n - 1)) {
            $work = Join-Path $OutDir "work_$k"
            if (Test-Path $work) { Remove-Item -Recurse -Force $work }
            New-Item -ItemType Directory -Force -Path $work | Out-Null
            Copy-Item (Join-Path $DeckDir 'voordelta.dep') $work
            Copy-Item (Join-Path $DeckDir 'voordelta.swn') (Join-Path $work 'INPUT')
        }
        foreach ($k in 0..($n - 1)) {
            $work = Join-Path $OutDir "work_$k"
            $p = Start-Process -FilePath $Exe -WorkingDirectory $work -PassThru -NoNewWindow `
                 -RedirectStandardOutput (Join-Path $work 'stdout.log')
            $null = $p.Handle   # keep the handle, or ExitTime is not available after the exit
            $p.ProcessorAffinity = [IntPtr]([int64]1 -shl $CpuList[$k])
            $procs += $p
        }
        foreach ($p in $procs) { $p.WaitForExit() }
        $k = 0
        foreach ($p in $procs) {
            $seconds = ($p.ExitTime - $p.StartTime).TotalSeconds
            $ok = Test-Path (Join-Path (Join-Path $OutDir "work_$k") 'norm_end')
            $rows += [pscustomobject]@{ concurrent = $n; repetition = $rep; cpu = $CpuList[$k]; seconds = [math]::Round($seconds, 3); ok = $ok }
            $k++
        }
        "{0} at a time, repetition {1}: {2}" -f $n, $rep, (($rows | Where-Object { $_.concurrent -eq $n -and $_.repetition -eq $rep } | ForEach-Object { $_.seconds }) -join ' ')
    }
}
$rows | Export-Csv -NoTypeInformation -Path (Join-Path $OutDir 'concurrent.csv')
foreach ($n in $Counts) {
    $t = $rows | Where-Object { $_.concurrent -eq $n } | ForEach-Object { $_.seconds } | Sort-Object
    "{0} at a time: median {1:N2} s, fastest {2:N2} s, slowest {3:N2} s" -f $n, $t[[int]($t.Count / 2)], $t[0], $t[-1]
}
