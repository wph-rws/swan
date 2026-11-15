<#
.SYNOPSIS
Times the fork against upstream SWAN on native Windows, and measures what the
balance residual costs, on logical CPUs that you choose.

.DESCRIPTION
Under WSL2 a logical CPU number does not say on which kind of core (performance
or efficiency) a thread runs, so timings there mix the two. Native Windows
does keep a process on the logical CPUs it is given. This script uses that.

The script does three things, each optional:

1. Speed. For every number of workers it runs the upstream program and the
   fork right after each other, on the same logical CPUs: the first N entries
   of -CpuList. Which of the two goes first alternates between repetitions.
   Each run starts in a fresh copy of the deck directory. With
   -UpstreamWorkers, upstream runs only at those numbers of workers and the
   fork runs alone at the others; without -UpstreamExe the fork runs alone.
2. Balance residual. It runs the fork on the same deck with "NUMERIC STOPC"
   and with "NUMERIC STOPC RESIDUAL 0" added before COMPUTE, alternating, for
   -ResidualRounds rounds. The cost of one round is the ratio of the two
   times; the reported cost is the median of those ratios.
3. Checks. For every run it records a SHA-256 checksum of the two block files
   and a checksum of the numbers in the table file (so that a different
   header line does not count). At the end it reports whether all runs of the
   speed part gave the same answer, for both programs and every number of
   workers, and whether the residual changed the answer.

Before and after, it records the power source and how busy the machine was.
On a laptop, battery power makes these runs much slower: measure on mains.

With -Kind openmp (the default) the fork must be an OpenMP build, and
upstream an OpenMP or a serial build. The number of OpenMP threads is the
number of logical CPUs a run gets. With -Kind mpi both must be MPI builds for
MS-MPI; a run on N workers starts N processes with mpiexec, and the script
gives each its own logical CPU and records the masks it read back
(rank_affinity in the output).

Upstream 41.51 (43e9bbb) built with OpenMP by MSYS2 gfortran 16.2 gave
Hsig = 0 everywhere on the Voordelta example under Windows (November 2026).
That compiler reads some THREADPRIVATE variables at -O1 and above as ordinary
global variables (the starting copy, not that of the thread). The serial and
MPI builds were correct. Compare against those until this is resolved.

Give one logical CPU per physical core: on a processor with two threads per
core, take one of each pair. The script reads the kind of each logical CPU
from Windows (GetSystemCpuSetInformation) and writes it to the output, so that
it shows which kind of core was used.

Pass arrays with -Command, not -File: powershell.exe -File passes "1,2,4" as
one string, which becomes the number 124.

.EXAMPLE
.\speed_vs_upstream.ps1 -ForkExe C:\swan\fork\bin\swan.exe `
    -UpstreamExe C:\swan\upstream\bin\swan.exe -DeckDir C:\swan\examples\voordelta `
    -OutDir C:\swan\results\default -Label default
#>
[CmdletBinding()]
param(
    # OpenMP build of the fork. Used in the speed part and the residual part.
    [Parameter(Mandatory = $true)] [string] $ForkExe,
    # Build of upstream. Leave out to time the fork alone.
    [string] $UpstreamExe,
    # Numbers of workers at which upstream runs as well (default: all of
    # -Workers). For a serial upstream build give 1.
    [int[]] $UpstreamWorkers,
    # Leave out the speed part (to measure only the residual).
    [switch] $SkipSpeed,
    # Directory with the deck and its input files. All files are copied.
    [Parameter(Mandatory = $true)] [string] $DeckDir,
    # Name of the deck in -DeckDir. It is copied to INPUT.
    [string] $DeckFile = 'voordelta.swn',
    # Output files to compare byte for byte, and the table file whose numbers are compared.
    [string[]] $BlockFiles = @('voordelta_hs.blk', 'voordelta_depth.blk'),
    [string] $TableFile = 'voordelta_sites.tbl',
    # Numbers of workers for the speed part.
    [int[]] $Workers = @(1, 2, 4, 6),
    # Logical CPUs, in the order in which they are taken. A run on N workers
    # gets the first N.
    [int[]] $CpuList = @(12, 14, 16, 18, 10, 0),
    [int] $Repetitions = 3,
    # Rounds of the residual part; 0 skips it.
    [int] $ResidualRounds = 7,
    [int[]] $ResidualWorkers = @(1, 4),
    # Directory with the run-time libraries of the compiler (libgfortran, libgomp).
    [string] $RuntimeDir = 'C:\msys64\ucrt64\bin',
    # Kind of parallel build: 'openmp' (threads) or 'mpi' (processes). Both
    # programs of a series must be of this kind. The residual part needs
    # 'openmp'.
    [ValidateSet('openmp', 'mpi')] [string] $Kind = 'openmp',
    # mpiexec of MS-MPI, for -Kind mpi.
    [string] $MpiExec = 'C:\Program Files\Microsoft MPI\Bin\mpiexec.exe',
    # Name of this series in the output.
    [string] $Label = 'series',
    [Parameter(Mandatory = $true)] [string] $OutDir
)

$ErrorActionPreference = 'Stop'
Set-StrictMode -Version 1.0

foreach ($n in $Workers + $ResidualWorkers) {
    if ($n -gt $CpuList.Count) { throw "$n workers asked, but -CpuList has only $($CpuList.Count) entries" }
}
$ForkExe = (Resolve-Path $ForkExe).Path
if ($UpstreamExe) { $UpstreamExe = (Resolve-Path $UpstreamExe).Path }
$DeckDir = (Resolve-Path $DeckDir).Path
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null
$OutDir = (Resolve-Path $OutDir).Path
$WorkRoot = Join-Path $OutDir 'work'
$env:PATH = "$RuntimeDir;$env:PATH"

# ---------------------------------------------------------------------------
# Which kind of core each logical CPU belongs to. Windows gives every logical
# CPU an efficiency class: on a hybrid processor the performance cores have
# the higher class. CoreIndex tells which logical CPUs share a physical core.
Add-Type -TypeDefinition @"
using System; using System.Runtime.InteropServices; using System.Collections.Generic;
public static class SwanCpuSets {
    [DllImport("kernel32.dll", SetLastError = true)]
    static extern bool GetSystemCpuSetInformation(IntPtr info, uint length, out uint returned, IntPtr process, uint flags);
    public static List<int[]> Read() {
        uint n; GetSystemCpuSetInformation(IntPtr.Zero, 0, out n, IntPtr.Zero, 0);
        IntPtr buffer = Marshal.AllocHGlobal((int)n);
        var list = new List<int[]>();
        try {
            if (!GetSystemCpuSetInformation(buffer, n, out n, IntPtr.Zero, 0)) return list;
            int offset = 0;
            while (offset < n) {
                IntPtr p = IntPtr.Add(buffer, offset);
                // SYSTEM_CPU_SET_INFORMATION: LogicalProcessorIndex at byte 14,
                // CoreIndex at 15, EfficiencyClass at 18.
                list.Add(new int[] { Marshal.ReadByte(p, 14), Marshal.ReadByte(p, 15), Marshal.ReadByte(p, 18) });
                offset += Marshal.ReadInt32(p);
            }
        } finally { Marshal.FreeHGlobal(buffer); }
        return list;
    }
}
"@
$cpuSets = @{}
foreach ($s in [SwanCpuSets]::Read()) { $cpuSets[[int]$s[0]] = @{ core = [int]$s[1]; class = [int]$s[2] } }
$highestClass = ($cpuSets.Values | ForEach-Object { $_.class } | Measure-Object -Maximum).Maximum
$cpuInfo = foreach ($lp in $CpuList) {
    $s = $cpuSets[$lp]
    [pscustomobject]@{ logical_cpu = $lp; physical_core = $s.core; efficiency_class = $s.class
                       performance_core = ($s.class -eq $highestClass) }
}
$cpuInfo | Format-Table -AutoSize | Out-String | Write-Host
if (($cpuInfo | Where-Object { -not $_.performance_core })) {
    Write-Warning 'Some logical CPUs in -CpuList are not on performance cores.'
}
if (($cpuInfo.physical_core | Sort-Object -Unique).Count -lt $CpuList.Count) {
    Write-Warning 'Some logical CPUs in -CpuList share a physical core.'
}

# ---------------------------------------------------------------------------
# Power source and load. BatteryStatus 2 means the laptop is on mains power.
# The load is read through CIM, whose names do not depend on the language of
# Windows (Get-Counter does). The busiest processes are those that used the
# most processor time during the sample.
function Get-MachineState([string] $When) {
    $battery = Get-CimInstance Win32_Battery -ErrorAction SilentlyContinue
    $before = @{}
    foreach ($p in Get-Process) { if ($p.CPU) { $before[$p.Id] = $p.CPU } }
    $load = for ($i = 0; $i -lt 5; $i++) {
        (Get-CimInstance Win32_PerfFormattedData_PerfOS_Processor -Filter "Name='_Total'").PercentProcessorTime
        Start-Sleep -Seconds 1
    }
    $busiest = foreach ($p in Get-Process) {
        if ($p.CPU -and $before.ContainsKey($p.Id)) {
            [pscustomobject]@{ name = $p.ProcessName; cpu_seconds = [math]::Round($p.CPU - $before[$p.Id], 2) }
        }
    }
    [pscustomobject]@{
        when = $When
        time = (Get-Date).ToString('s')
        battery_status = $(if ($battery) { $battery.BatteryStatus } else { $null })
        on_mains = $(if ($battery) { $battery.BatteryStatus -eq 2 } else { $true })
        load_percent = @($load)
        busiest = @($busiest | Sort-Object cpu_seconds -Descending | Select-Object -First 5)
    }
}

# ---------------------------------------------------------------------------
# One run in a fresh copy of the deck directory. "start /affinity" creates the
# process on the given logical CPUs, so that no thread runs elsewhere, even
# for a moment. The time includes starting cmd, which takes a few hundredths
# of a second and is the same for every program.
function Invoke-SwanRun([string] $Exe, [int[]] $Cpus, [string] $Name, [string] $ExtraLine) {
    $work = Join-Path $WorkRoot $Name
    if (Test-Path $work) { Remove-Item -Recurse -Force $work }
    New-Item -ItemType Directory -Force -Path $work | Out-Null
    Copy-Item -Path (Join-Path $DeckDir '*') -Destination $work
    $deck = Get-Content (Join-Path $DeckDir $DeckFile)
    if ($ExtraLine) {
        $at = 0
        while ($deck[$at] -notmatch '^\s*COMP') { $at++ }
        $deck = @($deck[0..($at - 1)]) + $ExtraLine + @($deck[$at..($deck.Count - 1)])
    }
    Set-Content -Path (Join-Path $work 'INPUT') -Value $deck -Encoding Ascii

    $mask = [int64]0
    foreach ($c in $Cpus) { $mask = $mask -bor ([int64]1 -shl $c) }
    $hex = '{0:X}' -f $mask
    $rankAffinity = ''
    if ($Kind -eq 'openmp') {
        $env:OMP_NUM_THREADS = "$($Cpus.Count)"
        $clock = [Diagnostics.Stopwatch]::StartNew()
        cmd /c "cd /d `"$work`" && start `"`" /wait /b /affinity $hex `"$Exe`" > swan-stdout.txt 2>&1"
        $clock.Stop()
        $exit = $LASTEXITCODE
    } else {
        # MPI: one process per logical CPU. mpiexec starts inside the mask of
        # all the CPUs of the run, and its processes inherit that mask. MS-MPI
        # does not bind them further ("-affinity_layout 0"), so the script
        # gives each process one CPU of its own as soon as it appears, and
        # reads the masks back. Until then (a fraction of a second) the
        # processes may share the CPUs of the run.
        $env:OMP_NUM_THREADS = '1'
        $name = [IO.Path]::GetFileNameWithoutExtension($Exe)
        $startInfo = New-Object Diagnostics.ProcessStartInfo 'cmd.exe', ("/c cd /d `"$work`" && start `"`" /wait /b " +
            "/affinity $hex `"$MpiExec`" -n $($Cpus.Count) -affinity_layout 0 `"$Exe`" > swan-stdout.txt 2>&1")
        $startInfo.UseShellExecute = $false
        $launched = Get-Date
        $clock = [Diagnostics.Stopwatch]::StartNew()
        $process = [Diagnostics.Process]::Start($startInfo)
        $ranks = @()
        while (-not $process.HasExited -and $clock.Elapsed.TotalSeconds -lt 60) {
            $ranks = @(Get-Process $name -ErrorAction SilentlyContinue | Where-Object { $_.StartTime -ge $launched })
            if ($ranks.Count -ge $Cpus.Count) { break }
            Start-Sleep -Milliseconds 20
        }
        $ranks = @($ranks | Sort-Object Id)
        for ($i = 0; $i -lt [math]::Min($ranks.Count, $Cpus.Count); $i++) {
            try { $ranks[$i].ProcessorAffinity = [IntPtr]([int64]1 -shl $Cpus[$i]) } catch { }
        }
        $rankAffinity = ($ranks | ForEach-Object {
            try { $_.Refresh(); '{0:X}' -f [int64]$_.ProcessorAffinity } catch { 'ended' } }) -join ' '
        $process.WaitForExit()
        $clock.Stop()
        $exit = $process.ExitCode
    }

    $sums = @{}
    foreach ($f in $BlockFiles) {
        $path = Join-Path $work $f
        $sums[$f] = $(if (Test-Path $path) { (Get-FileHash -Algorithm SHA256 $path).Hash.Substring(0, 16) } else { 'missing' })
    }
    # The numbers of the table, without its header lines (which start with %).
    $path = Join-Path $work $TableFile
    if (Test-Path $path) {
        $numbers = foreach ($line in Get-Content $path) {
            if ($line -notmatch '^\s*%') {
                foreach ($t in ($line.Trim() -split '\s+')) {
                    if ($t) { ([double]::Parse($t, [Globalization.CultureInfo]::InvariantCulture)).ToString('R', [Globalization.CultureInfo]::InvariantCulture) }
                }
            }
        }
        $bytes = [Text.Encoding]::ASCII.GetBytes(($numbers -join ' '))
        $sha = [Security.Cryptography.SHA256]::Create()
        $sums[$TableFile] = ([BitConverter]::ToString($sha.ComputeHash($bytes)) -replace '-', '').Substring(0, 16)
    } else { $sums[$TableFile] = 'missing' }
    $errors = Test-Path (Join-Path $work 'Errfile')
    if ($exit -ne 0 -or $errors -or ($sums.Values -contains 'missing')) {
        Write-Warning "$Name ended with exit code $exit, Errfile present: $errors; see $work"
    }
    $answer = (@($BlockFiles) + $TableFile | ForEach-Object { $sums[$_] }) -join '/'
    [pscustomobject]@{
        series = $Label; build = ''; kind = ''; workers = $Cpus.Count; cpus = ($Cpus -join ' ')
        repetition = 0; seconds = [math]::Round($clock.Elapsed.TotalSeconds, 3); exit_code = $exit
        errfile = $errors; output_checksum = $answer; rank_affinity = $rankAffinity
    }
}

function Get-Median([double[]] $Values) {
    $s = $Values | Sort-Object
    if ($s.Count % 2) { $s[[int][math]::Floor($s.Count / 2)] } else { ($s[$s.Count / 2 - 1] + $s[$s.Count / 2]) / 2 }
}

# ---------------------------------------------------------------------------
$states = @(Get-MachineState 'before')
$runs = New-Object System.Collections.Generic.List[object]

if (-not $SkipSpeed) {
    if (-not $UpstreamWorkers) { $UpstreamWorkers = $Workers }
    for ($rep = 1; $rep -le $Repetitions; $rep++) {
        foreach ($n in $Workers) {
            $programs = @(, @('fork', $ForkExe))
            if ($UpstreamExe -and ($UpstreamWorkers -contains $n)) {
                $programs = @(@('upstream', $UpstreamExe), @('fork', $ForkExe))
            }
            # Assign inside the branches: the output of an if statement would
            # unroll a list that holds a single pair.
            if (($rep % 2) -or $programs.Count -eq 1) { $order = $programs } else { $order = @($programs[1], $programs[0]) }
            foreach ($p in $order) {
                $r = Invoke-SwanRun $p[1] $CpuList[0..($n - 1)] "$($p[0])-$n" $null
                $r.build = $p[0]; $r.kind = $Kind; $r.repetition = $rep
                $runs.Add($r)
                Write-Host ("{0,-9} {1} workers  rep {2}  {3,8:N2} s  {4}" -f $p[0], $n, $rep, $r.seconds, $r.output_checksum)
            }
        }
    }
}

if ($ResidualRounds -gt 0) {
    $variants = @(@('residual-off', 'NUMERIC STOPC'), @('residual-on', 'NUMERIC STOPC RESIDUAL 0'))
    foreach ($n in $ResidualWorkers) {
        for ($round = 1; $round -le $ResidualRounds; $round++) {
            if ($round % 2) { $order = $variants } else { $order = @($variants[1], $variants[0]) }
            foreach ($v in $order) {
                $r = Invoke-SwanRun $ForkExe $CpuList[0..($n - 1)] "$($v[0])-$n" $v[1]
                $r.build = 'fork'; $r.kind = $v[0]; $r.repetition = $round
                $runs.Add($r)
                Write-Host ("{0,-12} {1} workers  round {2}  {3,8:N2} s  {4}" -f $v[0], $n, $round, $r.seconds, $r.output_checksum)
            }
        }
    }
}

$states += Get-MachineState 'after'

# ---------------------------------------------------------------------------
# Summary: medians, factors, residual cost and the checks of the answers.
$speed = @()
foreach ($n in $Workers) {
    $up = @($runs | Where-Object { $_.kind -eq $Kind -and $_.build -eq 'upstream' -and $_.workers -eq $n })
    $fk = @($runs | Where-Object { $_.kind -eq $Kind -and $_.build -eq 'fork' -and $_.workers -eq $n })
    if ($fk.Count) {
        $mf = Get-Median $fk.seconds
        $mu = $(if ($up.Count) { Get-Median $up.seconds } else { $null })
        $speed += [pscustomobject]@{ workers = $n; cpus = ($CpuList[0..($n - 1)] -join ' ')
            upstream_median_s = $(if ($up.Count) { [math]::Round($mu, 2) } else { $null })
            fork_median_s = [math]::Round($mf, 2)
            factor = $(if ($up.Count) { [math]::Round($mu / $mf, 2) } else { $null })
            upstream_s = @($up | ForEach-Object { $_.seconds }); fork_s = @($fk | ForEach-Object { $_.seconds }) }
    }
}
$residual = @()
foreach ($n in $ResidualWorkers) {
    $off = @($runs | Where-Object { $_.kind -eq 'residual-off' -and $_.workers -eq $n } | Sort-Object repetition)
    $on = @($runs | Where-Object { $_.kind -eq 'residual-on' -and $_.workers -eq $n } | Sort-Object repetition)
    if ($off.Count -and $on.Count) {
        $costs = for ($i = 0; $i -lt $off.Count; $i++) { 100 * ($on[$i].seconds / $off[$i].seconds - 1) }
        $residual += [pscustomobject]@{ workers = $n; cpus = ($CpuList[0..($n - 1)] -join ' ')
            off_median_s = [math]::Round((Get-Median $off.seconds), 2)
            on_median_s = [math]::Round((Get-Median $on.seconds), 2)
            cost_percent = [math]::Round((Get-Median $costs), 1)
            cost_per_round_percent = @($costs | ForEach-Object { [math]::Round($_, 1) })
            same_answer = (@(($off + $on).output_checksum | Sort-Object -Unique).Count -eq 1) }
    }
}
$speedAnswers = @($runs | Where-Object { $_.kind -eq $Kind } | ForEach-Object { $_.output_checksum } | Sort-Object -Unique)
$failed = @($runs | Where-Object { $_.exit_code -ne 0 -or $_.errfile })
$summary = [pscustomobject]@{
    series = $Label
    fork = $ForkExe; upstream = $UpstreamExe; deck = (Join-Path $DeckDir $DeckFile)
    logical_cpus = @($cpuInfo)
    machine = $states
    speed = $speed
    speed_all_runs_same_answer = ($speedAnswers.Count -le 1)
    speed_distinct_answers = $speedAnswers
    residual = $residual
    failed_runs = $failed.Count
}
$runs | Export-Csv -NoTypeInformation -Encoding Ascii (Join-Path $OutDir "runs-$Label.csv")
$summary | ConvertTo-Json -Depth 6 | Set-Content -Encoding Ascii (Join-Path $OutDir "summary-$Label.json")

Write-Host ''
if ($speed) { $speed | Select-Object workers, cpus, upstream_median_s, fork_median_s, factor | Format-Table -AutoSize | Out-String | Write-Host }
Write-Host "All speed runs give the same answer: $($summary.speed_all_runs_same_answer)"
if ($residual) { $residual | Select-Object workers, off_median_s, on_median_s, cost_percent, same_answer | Format-Table -AutoSize | Out-String | Write-Host }
foreach ($s in $states) { Write-Host ("{0}: on mains {1}, load {2} %" -f $s.when, $s.on_mains, ($s.load_percent -join ' ')) }
if ($failed.Count) { Write-Warning "$($failed.Count) runs failed" }
Remove-Item -Recurse -Force $WorkRoot
