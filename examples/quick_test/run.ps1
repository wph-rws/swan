[CmdletBinding()]
param(
    [string] $SwanExecutable
)

$ErrorActionPreference = 'Stop'
$caseDirectory = $PSScriptRoot

if (-not $SwanExecutable) {
    $localBuild = Join-Path $caseDirectory '..\..\build\bin\swan.exe'
    if (Test-Path -LiteralPath $localBuild) {
        $SwanExecutable = $localBuild
    }
    else {
        $installedSwan = Get-Command swan.exe -ErrorAction SilentlyContinue
        if ($installedSwan) {
            $SwanExecutable = $installedSwan.Source
        }
    }
}

if (-not $SwanExecutable -or -not (Test-Path -LiteralPath $SwanExecutable)) {
    throw 'SWAN executable not found. Build the repo first or pass -SwanExecutable C:\path\to\swan.exe.'
}

$SwanExecutable = (Resolve-Path -LiteralPath $SwanExecutable).Path
$generatedFiles = @(
    'INPUT',
    'PRINT',
    'Errfile',
    'ERRPTS',
    'norm_end',
    'swaninit',
    'quick_test.prt',
    'quick_test.erf',
    'quick_test_hs.blk',
    'quick_test_center.tbl'
)

Push-Location $caseDirectory
try {
    foreach ($file in $generatedFiles) {
        if (Test-Path -LiteralPath $file) {
            Remove-Item -LiteralPath $file -Force
        }
    }

    Copy-Item -LiteralPath 'quick_test.swn' -Destination 'INPUT'

    $timer = [System.Diagnostics.Stopwatch]::StartNew()
    & $SwanExecutable
    $exitCode = $LASTEXITCODE
    $timer.Stop()

    if (Test-Path -LiteralPath 'PRINT') {
        Move-Item -LiteralPath 'PRINT' -Destination 'quick_test.prt'
    }
    if (Test-Path -LiteralPath 'Errfile') {
        Move-Item -LiteralPath 'Errfile' -Destination 'quick_test.erf'
    }

    if ($exitCode -ne 0) {
        throw "SWAN stopped with exit code $exitCode. Inspect quick_test.prt and quick_test.erf."
    }
    if (-not (Test-Path -LiteralPath 'norm_end')) {
        throw 'SWAN did not create norm_end. Inspect quick_test.prt and quick_test.erf.'
    }

    $elapsed = $timer.Elapsed
    Write-Host ("Quick test completed normally in {0:n2} seconds." -f $elapsed.TotalSeconds)
    Write-Host 'Results: quick_test_center.tbl, quick_test_hs.blk, quick_test.prt'

    if ($elapsed.TotalSeconds -gt 180) {
        Write-Warning 'The run took longer than the intended three-minute budget.'
    }
}
finally {
    if (Test-Path -LiteralPath 'INPUT') {
        Remove-Item -LiteralPath 'INPUT' -Force
    }
    Pop-Location
}
