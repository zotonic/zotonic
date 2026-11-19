# Run with powershell.exe or pwsh. All Windows/WSL side effects are mocked.
$ErrorActionPreference = 'Stop'
$launcher = Join-Path $PSScriptRoot 'launcher.ps1'
$fixture = Join-Path ([System.IO.Path]::GetTempPath()) ('zotonic-launcher-' + [guid]::NewGuid())
$oldLocalAppData = $env:LOCALAPPDATA
$oldProgramFiles = $env:ProgramFiles
$global:zotonicTestCalls = [System.Collections.Generic.List[object]]::new()
$global:zotonicTestFailStart = $false
$global:zotonicTestUid = '1000'
$global:zotonicTestPayload = ''
$global:zotonicTestEngineReady = $true

function Assert($Condition, $Message) {
    if (-not $Condition) { throw $Message }
}

function wsl.exe {
    $global:zotonicTestCalls.Add(@($args))
    $global:LASTEXITCODE = 0
    if ($args -contains '--list') { return 'Ubuntu  Running  2' }
    switch ($args[-1]) {
        'whoami' { return 'developer' }
        '-u' { return $global:zotonicTestUid }
        '-r' { return '6.6.0-microsoft-standard-WSL2' }
        'HOME' { return '/home/developer' }
        '{{.OSType}}' {
            if ($global:zotonicTestEngineReady) { return 'linux' }
            $global:LASTEXITCODE = 1
            return
        }
        '-s' { $global:zotonicTestPayload = ($input -join ''); return }
    }
    if ($args -contains 'start' -and $global:zotonicTestFailStart) { $global:LASTEXITCODE = 17 }
}

function Read-Host {
    if ($args[0] -like '*retry*') { return 'Q' }
    return ''
}
function Start-Sleep { }
function Start-Process { $global:zotonicTestCalls.Add(@('Start-Process') + $args) }
function code { $global:zotonicTestCalls.Add(@('code') + $args); $global:LASTEXITCODE = 0 }

function New-Object {
    param([string]$TypeName, [object[]]$ArgumentList, [string]$ComObject)
    if ($ComObject -eq 'WScript.Shell') {
        $shell = [pscustomobject]@{}
        $shell | Add-Member ScriptMethod CreateShortcut {
            param($Path)
            $link = [pscustomobject]@{ TargetPath = ''; Description = '' }
            $link | Add-Member ScriptMethod Save { }
            return $link
        }
        return $shell
    }
    Microsoft.PowerShell.Utility\New-Object @PSBoundParameters
}

try {
    $env:LOCALAPPDATA = $fixture
    $env:ProgramFiles = $fixture
    New-Item -ItemType Directory -Force $fixture | Out-Null
    & $launcher -Action setup
    Assert ($LASTEXITCODE -eq 0) 'Setup failed'
    $installed = Join-Path $fixture 'Zotonic/WSL'
    $config = Get-Content -Raw (Join-Path $installed 'config.json') | ConvertFrom-Json
    Assert ($config.User -eq 'developer') 'Selected user was not saved'
    Assert ($config.Checkout -eq '/home/developer/projects/zotonic') 'Linux checkout was not saved'
    Assert (Test-Path (Join-Path $installed 'Start Zotonic.cmd')) 'Start launcher was not created'
    Assert (Test-Path (Join-Path $installed 'Shell Zotonic.cmd')) 'Shell launcher was not created'
    Assert ($global:zotonicTestPayload.Contains('base64 -d')) 'Support files were not streamed to WSL'
    Assert (-not $global:zotonicTestPayload.Contains("`r")) 'Support payload has CRLF lines'
    Write-Host 'PASS: setup saves the Linux profile and creates launchers'

    $global:zotonicTestCalls.Clear()
    & $launcher -Action start
    Assert ($LASTEXITCODE -eq 0) 'Start failed'
    Assert (@($global:zotonicTestCalls | Where-Object { $_ -contains 'http://localhost:8000/' }).Count -eq 1) 'Browser not opened after readiness'
    $start = @($global:zotonicTestCalls | Where-Object { $_ -contains 'start' })[0]
    Assert ($start -contains 'developer') 'Start did not use the selected Linux user'
    Write-Host 'PASS: start targets the saved user and opens the browser'

    $global:zotonicTestCalls.Clear()
    $global:zotonicTestFailStart = $true
    & $launcher -Action start
    Assert ($LASTEXITCODE -eq 1) 'Failed startup was not propagated'
    Assert (@($global:zotonicTestCalls | Where-Object { $_ -contains 'Start-Process' }).Count -eq 0) 'Browser opened after a failed startup'
    $global:zotonicTestFailStart = $false
    Write-Host 'PASS: failed startup leaves an error and does not open the browser'

    foreach ($action in @('stop', 'shell', 'logs', 'folder', 'edit')) {
        $global:zotonicTestCalls.Clear()
        & $launcher -Action $action
        Assert ($LASTEXITCODE -eq 0) "$action failed"
        Assert ($global:zotonicTestCalls.Count -gt 0) "$action did not call its integration"
        if ($action -eq 'edit') {
            $edit = @($global:zotonicTestCalls | Where-Object { $_ -contains 'code .' })[0]
            Assert ($edit -contains 'developer') 'VS Code did not use the selected Linux user'
        }
    }
    Write-Host 'PASS: stop, shell, logs, Explorer and VS Code dispatch'

    $global:zotonicTestCalls.Clear()
    $global:zotonicTestEngineReady = $false
    & $launcher -Action start
    Assert ($LASTEXITCODE -eq 1) 'Unavailable Docker was not reported'
    Assert (@($global:zotonicTestCalls | Where-Object { $_ -contains 'start' }).Count -eq 0) 'Containers launched without a ready engine'
    $global:zotonicTestEngineReady = $true
    Write-Host 'PASS: unavailable Docker offers retry/cancel and does not launch containers'

    $global:zotonicTestUid = '0'
    & $launcher -Action setup
    Assert ($LASTEXITCODE -eq 1) 'Setup accepted root'
    Assert ((Get-Content -Raw (Join-Path $installed 'config.json') | ConvertFrom-Json).User -eq 'developer') 'Failed setup replaced the saved profile'
    Write-Host 'PASS: root setup fails without replacing the saved profile'
} finally {
    $env:LOCALAPPDATA = $oldLocalAppData
    $env:ProgramFiles = $oldProgramFiles
    Remove-Item -Recurse -Force $fixture
}
