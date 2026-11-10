---
keywords:
  - how_to_guide
  - frontend_developer
  - backend_developer
  - development_and_debugging
---

# Local development with Docker on Windows

Use Docker Desktop and double-click `start-docker.cmd` in a Windows checkout.
Erlang, PostgreSQL and the other Zotonic dependencies run in Linux containers.
You do not need to open Ubuntu for this workflow, though Docker Desktop can use
WSL 2 internally as its Linux container backend.

Commands below are labelled **PowerShell** or **container shell**. The alternative
Ubuntu WSL workflow is at the end of this guide.

## 1. Install and start Docker Desktop

Install [Docker Desktop for Windows](https://docs.docker.com/desktop/setup/install/windows-install/)
and Git for Windows. Complete Docker Desktop's backend setup and start it.
Wait for the Linux container engine to be ready.

If Docker Desktop shows **Use WSL 2 based engine** under **Settings → General**,
you can enable it. The **Switch to Linux containers** menu item only appears
when applicable; it is not a setting every installation shows. Ubuntu WSL
integration is only needed for the alternative Ubuntu workflow below.

Open a normal **PowerShell** window and check:

```powershell
docker version
docker compose version
```

Expect both client and server information from `docker version`. If Docker is
not found, reopen PowerShell after installation. If the server is unavailable,
check that Docker Desktop has finished starting.

## 2. Create a Windows checkout

In **PowerShell**, run:

```powershell
New-Item -ItemType Directory -Force C:\projects
Set-Location C:\projects
git -c core.autocrlf=false clone https://github.com/zotonic/zotonic.git
Set-Location zotonic
git config core.autocrlf false
```

The repository's Git attributes preserve LF endings for Linux scripts in new
checkouts. They do not automatically repair CRLF scripts already present in an
older checkout. When moving from WSL, use a fresh clone and preserve existing
site files and database backups separately.

## 3. Double-click the launcher

In Windows Explorer, open `C:\projects\zotonic` and double-click
**start-docker.cmd**. Alternatively, run this from the checkout in **PowerShell**:

```powershell
.\start-docker.cmd
```

No administrator shell is needed. The launcher checks Docker, defaults the
container user and group IDs to `1000` unless explicitly overridden, starts
PostgreSQL, builds the image, and opens a shell inside the Zotonic container.
Downloads and the first compilation can take several minutes.

Wait for the **container shell prompt**. The checkout is now available at
`/opt/zotonic`. The launcher does not start the Zotonic server automatically;
continue with Step 4. Its window stays open after exit or failure so you can
read the output.

If the launcher reports that the Linux engine check failed, Zotonic has not
started. Open Docker Desktop from the Windows Start menu, resolve any startup
error it displays, and wait until its engine is running. Then close the launcher
and double-click `start-docker.cmd` again. Pressing a key on the launcher's error
screen only closes it; it does not retry or start Docker Desktop.

To diagnose this from **PowerShell**, run:

```powershell
docker info
docker info --format '{{.OSType}}'
```

The second command should print `linux`. A connection error means Docker's
engine is unavailable; `windows` means it is running Windows containers and
must be switched to Linux containers.

If host port `5432` is occupied, launch from **PowerShell** with:

```powershell
$env:DB_FORWARD_PORT = "5433"
.\start-docker.cmd
```

Use the same override on subsequent starts. Zotonic still connects to
`postgres:5432` inside the container network. Ports `8000` and `8443` must also
be available.

## 4. Start Zotonic inside the container

At the **container shell prompt**, run:

```bash
bin/zotonic start
bin/zotonic status
```

Keep this terminal open. In your Windows browser, open
[the local status site](http://localhost:8000/). If redirected to HTTPS, the
local development certificate is self-signed.

The container supplies the database settings automatically. The database host is
`postgres`; do not change it to `localhost` or run the native PostgreSQL setup.

## 5. Create your development site

In the same **container shell**, run:

```bash
bin/zotonic addsite -s blog -H mysite.test mysite
```

This creates `apps_user/mysite`, initializes its database schema and starts the
site. Save the admin password printed by the command.

On **Windows**, open Notepad as Administrator and edit:

```text
C:\Windows\System32\drivers\etc\hosts
```

Select **All files** in Notepad's file picker to see the extensionless `hosts`
file. Add this line and save the existing file:

```text
127.0.0.1 mysite.test
```

Open [your site](http://mysite.test:8000/) or
[its admin interface](http://mysite.test:8000/admin). Log in as `admin` with the
generated password. These credentials belong to your new site; the Zotonic
status site has a separate password.

## 6. Edit your site

Open `C:\projects\zotonic` in your Windows editor, such as VS Code. Your site
lives under `apps_user/mysite`; templates are in `priv/templates`. The checkout
is mounted into the container, so edits are available there immediately.

To explicitly rebuild, run in the **container shell**:

```bash
make
```

The Windows checkout workflow has not yet been verified on a Windows machine.
For more reliable Linux file-change notifications and better bind-mount
performance, use the Ubuntu WSL alternative below.

## 7. Stop and return later

To stop from Windows Explorer, double-click **stop-docker.cmd** in the same
checkout you used to start Zotonic. It stops the project's containers, including
the development shell and PostgreSQL, and removes their containers and network.
Your database volume and checkout files remain saved.

For an orderly application shutdown before stopping the containers, first run
in the **container shell**:

```bash
bin/zotonic stop
exit
```

The start launcher pauses before closing its window. Then double-click
**stop-docker.cmd**, or run it in **PowerShell**:

```powershell
Set-Location C:\projects\zotonic
.\stop-docker.cmd
```

You can also double-click `stop-docker.cmd` while the development container is
running; it stops the project's containers with a 30-second shutdown timeout.
For an orderly application shutdown, use `bin/zotonic stop` first as shown above.
The stop launcher keeps database volumes and checkout files, and leaves Docker
Desktop running. Its pause only keeps the result visible; pressing a key closes
that window.

Next time, start Docker Desktop and double-click `start-docker.cmd` again. At
the **container prompt**, run:

```bash
bin/zotonic start
```

Source files remain in the checkout, runtime files under `docker-data`, and
PostgreSQL data in the Docker `pgdata` volume. Avoid `docker compose down -v`
unless you intend to delete the development database.

## Troubleshooting: `env: 'bash\r': No such file or directory`

This means a Linux script has Windows CRLF line endings: the carriage return
becomes part of the interpreter name. Installing Bash does not fix it. Git
attributes protect fresh checkouts, but do not rewrite existing working files.

Close the failed launcher. In **PowerShell**, from the checkout, convert the
tracked shell scripts to LF without discarding their contents:

```powershell
Set-Location C:\projects\zotonic
git config core.autocrlf false
$utf8 = New-Object System.Text.UTF8Encoding($false)
git ls-files -- '*.sh' 'bin/zotonic' 'bin/mergepot' 'bin/zotonic-lsp' | ForEach-Object {
    $path = Join-Path (Get-Location).Path $_
    $text = [System.IO.File]::ReadAllText($path)
    [System.IO.File]::WriteAllText($path, $text.Replace("`r`n", "`n"), $utf8)
}
```

Double-click `start-docker.cmd` again. Its build step rebuilds the Docker image
with the corrected entrypoint, and the other corrected scripts are available
through the mounted checkout. Do not delete the database volume or reset the
checkout to fix line endings. If the error remains, inspect the failing hook's
file path; scripts in dependencies or user sites may need the same correction.

## Alternative: develop from Ubuntu WSL

Use this workflow instead of Steps 1–3 when you want the checkout on Ubuntu's
Linux filesystem. After opening the container shell, follow Steps 4–5 above to
start Zotonic and create your site.

### Install WSL 2 and Ubuntu

Open **PowerShell as Administrator** and run:

```powershell
wsl --install -d Ubuntu
```

Restart Windows if requested. Open the **Windows Start menu**, search for
**Ubuntu**, and launch it. On the first launch, create your Linux username and
password.

In PowerShell, update WSL and check the distribution version:

```powershell
wsl --update
wsl --list --verbose
```

Ubuntu should show version `2`. If necessary, convert it:

```powershell
wsl --set-version Ubuntu 2
```

See [Microsoft's WSL installation instructions](https://learn.microsoft.com/en-us/windows/wsl/install).

### Configure Docker Desktop for Ubuntu

Install [Docker Desktop for Windows](https://docs.docker.com/desktop/setup/install/windows-install/)
and start it.

In Docker Desktop settings:

- Enable **Use WSL 2 based engine** under **General**, if shown.
- Enable **Ubuntu** under **Resources → WSL Integration** and click **Apply**.
- If WSL Integration is missing, check the Docker taskbar menu for **Switch to
  Linux containers**. This is a conditional menu item, not a setting that every
  installation shows.

### Open the Ubuntu WSL shell from Windows

Keep Docker Desktop running. Open your Ubuntu shell from **Windows**, separately
from the Docker Desktop app. Docker Desktop runs the container engine; its
container terminal is not the Ubuntu shell used for the next steps.

Choose either of these ways to open Ubuntu:

- Open the **Windows Start menu**, search for **Ubuntu**, and launch it.
- Open **PowerShell** (a normal, non-administrator window is sufficient) and run:

  ```powershell
  wsl -d Ubuntu
  ```

If your distribution has a different name, such as `Ubuntu-24.04`, use the exact
name shown by `wsl --list --verbose` in PowerShell. Open your Ubuntu distribution,
not Docker's internal `docker-desktop` distribution or Git Bash.

You are now at an **Ubuntu shell prompt**. Run the following commands there:

```bash
docker version
docker compose version
```

`docker version` should show both client and server information. If it cannot
connect to the server, check that Docker Desktop is running and Ubuntu
integration is enabled.

If Ubuntu reports `docker: command not found`, check that WSL Integration is
enabled for the same Ubuntu distribution you opened, apply the settings, and
reopen Ubuntu. Docker Desktop supplies the Docker commands through this
integration; do not install a separate Docker Engine with `apt` for this setup.

See [Docker's WSL setup](https://docs.docker.com/desktop/features/wsl/).

### Clone Zotonic inside Ubuntu

Run in the **Ubuntu terminal**:

```bash
sudo apt update
sudo apt install -y git

mkdir -p ~/projects
cd ~/projects
git clone https://github.com/zotonic/zotonic.git
cd zotonic
```

Keep the checkout under `~/projects`, rather than `/mnt/c`. Linux filesystem
storage improves bind-mount performance and lets file-change notifications reach
the container reliably. See [Docker's WSL best practices](https://docs.docker.com/desktop/features/wsl/best-practices/).

### Build and enter the development container

From the Zotonic checkout in **Ubuntu**, run:

```bash
./start-docker.sh
```

This starts PostgreSQL, builds the development image and compiles Zotonic on the
first run. Downloads and compilation can take several minutes. Wait until the
**container's shell prompt** appears. The same terminal now runs a shell inside
the Zotonic container, with the checkout available at `/opt/zotonic`. Run the
commands from **Step 4: Start Zotonic inside the container** there.

If port `5432` is already occupied, use:

```bash
DB_FORWARD_PORT=5433 ./start-docker.sh
```

Use the same port override on subsequent starts. Zotonic still connects to
`postgres:5432` inside the container network. Ports `8000` and `8443` must also
be available on your computer.

See [Zotonic's Docker guide](https://zotonic.com/docs/1411/install-with-docker-or-podman).

### Check the Ubuntu user

Run `whoami` and `id` in Ubuntu before launching `./start-docker.sh`. Use the
normal Linux user created during Ubuntu installation, and run the launcher
without `sudo`. User or group ID `0` is rejected.

If Ubuntu opened as root, open **PowerShell** and select your normal Linux user:

```powershell
wsl -d Ubuntu -u YOUR_USERNAME
```

If `id` already shows non-zero IDs but the launcher reports ID `0`, clear explicit
overrides in **Ubuntu** and retry:

```bash
unset ZOTONIC_UID ZOTONIC_GID
./start-docker.sh
```

### Edit and return to a WSL checkout

For VS Code, install the WSL extension on Windows. In a second Ubuntu shell:

```bash
cd ~/projects/zotonic
code .
```

To open that directory in Windows Explorer, run `explorer.exe .` from the same
Ubuntu shell.

Stop Zotonic with `bin/zotonic stop` and `exit` inside the container. Back in
Ubuntu, run `./stop-docker.sh` from the checkout. You can also run this stop
script from a second Ubuntu terminal while the development container is running:

```bash
cd ~/projects/zotonic
./stop-docker.sh
```

Run this on the Ubuntu host, not inside the Zotonic container. It removes the
project's containers and network, preserves the database volume and checkout
files, and leaves Docker Desktop running.

Next time, start Docker Desktop, open Ubuntu from Windows, and run:

```bash
cd ~/projects/zotonic
./start-docker.sh
```

At the container prompt, run `bin/zotonic start`.
