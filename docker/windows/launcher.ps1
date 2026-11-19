# Windows PowerShell 5.1 compatible. Installed copies use a saved WSL profile.
param(
    [ValidateSet('setup', 'start', 'stop', 'folder', 'edit', 'shell', 'logs')]
    [string]$Action = 'start'
)
$ErrorActionPreference = 'Stop'
$installDir = Join-Path $env:LOCALAPPDATA 'Zotonic\WSL'
$configFile = Join-Path $installDir 'config.json'
$utf8 = New-Object System.Text.UTF8Encoding($false)

function Read-Default($Prompt, $Default) {
    $answer = Read-Host "$Prompt [$Default]"
    if ([string]::IsNullOrWhiteSpace($answer)) { return $Default }
    return $answer.Trim()
}

function Invoke-WslText($Distro, $UserName, $CommandArgs) {
    $result = & wsl.exe --distribution $Distro --user $UserName --exec @CommandArgs
    if ($LASTEXITCODE -ne 0) { throw 'The WSL command failed. Check the distribution and Linux user.' }
    return (($result -join "`n").Replace([string][char]0, '').Trim())
}

function Invoke-Control($Config, $Operation) {
    & wsl.exe --distribution $Config.Distribution --user $Config.User --exec bash $Config.Helper $Operation $Config.Checkout
    if ($LASTEXITCODE -ne 0) { throw "Zotonic '$Operation' failed. See the output above." }
}

function Test-Engine($Config) {
    try {
        $engine = & wsl.exe --distribution $Config.Distribution --user $Config.User --exec docker info --format '{{.OSType}}' 2>$null
        return ($LASTEXITCODE -eq 0 -and ($engine -join '').Trim() -eq 'linux')
    } catch { return $false }
}

function Ensure-Engine($Config) {
    if (Test-Engine $Config) { return }
    $executables = @(
        (Join-Path $env:ProgramFiles 'Docker\Docker\Docker Desktop.exe'),
        (Join-Path $env:LOCALAPPDATA 'Programs\DockerDesktop\Docker Desktop.exe')
    )
    $desktop = $executables | Where-Object { Test-Path $_ } | Select-Object -First 1
    if ($desktop) { Start-Process -FilePath $desktop }
    Write-Host 'Waiting for Docker Desktop. Enable WSL Integration for the selected Ubuntu distribution.'
    do {
        for ($attempt = 0; $attempt -lt 30; $attempt++) {
            if (Test-Engine $Config) { return }
            Start-Sleep -Seconds 2
        }
        Write-Host 'Docker is still unavailable. Check Docker Desktop for startup errors.'
        $retry = Read-Host 'Press Enter to retry, or type Q to quit'
        if ($retry -match '^[qQ]$') { throw 'Docker startup cancelled.' }
    } while ($true)
}

try {
    if (-not (Get-Command wsl.exe -ErrorAction SilentlyContinue)) {
        throw 'Install Ubuntu WSL first: in administrator PowerShell run wsl --install -d Ubuntu, then open Ubuntu to create your Linux user.'
    }

    if ($Action -eq 'setup') {
        Write-Host 'Installed WSL distributions (the selected Ubuntu must show VERSION 2):'
        & wsl.exe --list --verbose
        if ($LASTEXITCODE -ne 0) { throw 'Install Ubuntu with wsl --install -d Ubuntu, then open Ubuntu once to create your Linux user.' }
        $previous = $null
        if (Test-Path $configFile) { $previous = Get-Content -Raw $configFile | ConvertFrom-Json }
        $defaultDistro = if ($previous) { $previous.Distribution } else { 'Ubuntu' }
        $distro = Read-Default 'Ubuntu distribution name' $defaultDistro
        $defaultUser = & wsl.exe --distribution $distro --exec whoami
        if ($LASTEXITCODE -ne 0) { throw 'Open this Ubuntu distribution once to complete installation, then run setup again.' }
        if ($previous -and $previous.Distribution -eq $distro) { $defaultUser = $previous.User }
        $userName = Read-Default 'Linux username (must not be root)' (($defaultUser -join '').Trim())
        $uid = Invoke-WslText $distro $userName @('id', '-u')
        if ($uid -eq '0') { throw 'Select the normal Linux user created during Ubuntu installation, not root.' }
        $kernel = Invoke-WslText $distro $userName @('uname', '-r')
        if ($kernel -notmatch '(?i)wsl2') { throw 'This distribution must use WSL 2. In PowerShell run: wsl --set-version YOUR_DISTRIBUTION 2' }
        $linuxHome = Invoke-WslText $distro $userName @('printenv', 'HOME')
        $defaultCheckout = if ($previous -and $previous.Distribution -eq $distro -and $previous.User -eq $userName) { $previous.Checkout } else { "$linuxHome/projects/zotonic" }
        $checkout = Read-Default 'Linux checkout path (existing checkout or new clone)' $defaultCheckout
        if ($linuxHome -notmatch '^/[a-zA-Z0-9_/-]+$' -or $checkout -notmatch '^/[a-zA-Z0-9_./-]+$') {
            throw 'Use Linux paths without spaces or special characters.'
        }
        $helperDir = "$linuxHome/.local/share/zotonic-wsl"
        $config = [pscustomobject]@{ Distribution = $distro; User = $userName; Checkout = $checkout; Helper = "$helperDir/control.sh" }

        # Copy only launcher support files. The source checkout is cloned by Linux Git.
        # Send LF text on stdin so Windows paths/quoting never enter Bash commands.
        $payload = "set -eu`nmkdir -p '$helperDir'`n"
        foreach ($name in @('control.sh', 'compose.yml')) {
            $content = [System.IO.File]::ReadAllText((Join-Path $PSScriptRoot $name)).Replace("`r`n", "`n")
            $encoded = [Convert]::ToBase64String($utf8.GetBytes($content))
            $payload += "printf '%s' '$encoded' | base64 -d > '$helperDir/$name'`n"
        }
        # PowerShell adds CRLF after pipeline strings; exit before that final line.
        $payload += "exit`n"
        $payload | & wsl.exe --distribution $distro --user $userName --exec bash -s
        if ($LASTEXITCODE -ne 0) { throw 'Could not install the WSL launcher support files.' }
        Invoke-Control $config 'setup'
        Ensure-Engine $config
        Invoke-Control $config 'check'

        New-Item -ItemType Directory -Force $installDir | Out-Null
        foreach ($name in @('launcher.ps1', 'control.sh', 'compose.yml')) {
            $source = Join-Path $PSScriptRoot $name
            $target = Join-Path $installDir $name
            if ([System.IO.Path]::GetFullPath($source) -ne [System.IO.Path]::GetFullPath($target)) { Copy-Item $source $target -Force }
        }
        [System.IO.File]::WriteAllText($configFile, ($config | ConvertTo-Json), $utf8)
        $launchers = [ordered]@{
            'Start Zotonic' = 'start'; 'Stop Zotonic' = 'stop';
            'Open Zotonic Folder' = 'folder'; 'Edit Zotonic' = 'edit';
            'Shell Zotonic' = 'shell'; 'Logs Zotonic' = 'logs'; 'Setup Zotonic' = 'setup'
        }
        foreach ($label in $launchers.Keys) {
            $operation = $launchers[$label]
            $batch = "@echo off`r`npowershell.exe -NoProfile -ExecutionPolicy Bypass -File `"%~dp0launcher.ps1`" -Action $operation`r`nif errorlevel 1 pause`r`n"
            [System.IO.File]::WriteAllText((Join-Path $installDir "$label.cmd"), $batch, $utf8)
        }
        $desktopDir = [Environment]::GetFolderPath('Desktop')
        if (-not [string]::IsNullOrWhiteSpace($desktopDir)) {
            $shortcutPath = Join-Path $desktopDir 'Zotonic.lnk'
            $shortcut = (New-Object -ComObject WScript.Shell).CreateShortcut($shortcutPath)
            $shortcut.TargetPath = $installDir
            $shortcut.Description = 'Start, stop and edit Zotonic in Ubuntu WSL'
            $shortcut.Save()
        }
        Write-Host "Setup complete. In the launcher folder, double-click Start Zotonic."
        Start-Process explorer.exe -ArgumentList "`"$installDir`""
        exit 0
    }

    if (-not (Test-Path $configFile)) { throw 'Run setup-wsl.cmd once before using these Windows launchers.' }
    $config = Get-Content -Raw $configFile | ConvertFrom-Json
    switch ($Action) {
        'folder' {
            $unc = '\\wsl.localhost\' + $config.Distribution + $config.Checkout.Replace('/', '\')
            # Start the distribution before Explorer accesses its share.
            $null = Invoke-WslText $config.Distribution $config.User @('true')
            Start-Process explorer.exe -ArgumentList "`"$unc`""
        }
        'edit' {
            if (-not (Get-Command code -ErrorAction SilentlyContinue)) { throw 'Install VS Code for Windows with its WSL extension and the code command on PATH, then reopen this launcher.' }
            # Start from the selected user's WSL shell, not the distro's default
            # user (which can still be root on an imported distribution).
            & wsl.exe --distribution $config.Distribution --user $config.User --cd $config.Checkout --exec bash -lc 'code .'
            if ($LASTEXITCODE -ne 0) { throw 'VS Code could not open the WSL checkout. Check that code . works in Ubuntu and the Windows WSL extension is installed.' }
        }
        'start' {
            Ensure-Engine $config
            Invoke-Control $config 'start'
            Start-Process 'http://localhost:8000/'
        }
        default { Invoke-Control $config $Action }
    }
} catch {
    Write-Host "`n$($_.Exception.Message)" -ForegroundColor Red
    exit 1
}
