---
keywords:
  - how_to_guide
  - frontend_developer
  - backend_developer
  - development_and_debugging
---

# Local development with Docker on Windows

Keep Zotonic in Ubuntu's Linux filesystem and use Windows launchers for everyday
work. This avoids the slow Windows bind mounts and missed file-change events
that can affect a checkout under `C:\projects`. Erlang, PostgreSQL and the media
tools run in Linux containers; your editor connects to Ubuntu through WSL.

## 1. Prepare Ubuntu and Docker Desktop

Install [Docker Desktop for Windows](https://docs.docker.com/desktop/setup/install/windows-install/).
Use its **WSL 2 backend** for this workflow. Docker VMM and Windows containers do
not provide the Ubuntu filesystem integration this setup needs.

If Ubuntu is not installed, open **PowerShell as Administrator**:

```powershell
wsl --install -d Ubuntu
```

Restart Windows if requested. Open **Ubuntu from the Windows Start menu** and
create a normal Linux username and password. Do not use root for development.
In that **Ubuntu shell**, install Git:

```bash
sudo apt update
sudo apt install -y git
```

In **PowerShell**, check:

```powershell
wsl --update
wsl --list --verbose
```

Ubuntu must show version `2`. If necessary, use `wsl --set-version Ubuntu 2`,
substituting the exact distribution name shown in the list.

Start Docker Desktop. Under **Settings → Resources → WSL Integration**, enable
that Ubuntu distribution and apply the settings. Enable **Use WSL 2 based
engine** under General if shown. **Switch to Linux containers** is a conditional
menu item, not an option every installation shows.

See [Microsoft's WSL instructions](https://learn.microsoft.com/en-us/windows/wsl/install)
and [Docker's WSL setup](https://docs.docker.com/desktop/features/wsl/).

## 2. Run the Windows setup once

Obtain a current Zotonic checkout or extract the repository archive on Windows.
This Windows copy is only used to run setup; it is not mounted into the container.
Windows Git is not required when using an extracted archive.

In Explorer, double-click **setup-wsl.cmd**. No administrator shell is needed.
It asks for:

- The Ubuntu distribution name, such as `Ubuntu` or `Ubuntu-24.04`.
- Your Linux username. Setup rejects root.
- The Linux checkout path, defaulting to `/home/YOUR_USERNAME/projects/zotonic`.
  Keep it inside your Linux home directory, without spaces or special characters.

Setup clones Zotonic with Linux Git if that directory does not exist. It can
reuse an existing Zotonic Git checkout without pulling, resetting, or replacing
its files. It rejects Windows-mounted paths, even if reached through a symlink.
The initial clone uses the Zotonic repository's default branch.

Setup checks Docker's Ubuntu integration, starts Docker Desktop if it can find
it, and offers **Enter to retry** or **Q to quit** if Docker is unavailable.
It installs a launcher folder under `%LOCALAPPDATA%\Zotonic\WSL` and creates a
**Zotonic** shortcut to that folder on your Windows desktop.

The folder remembers the distribution, Linux user and checkout path. Rerun
**Setup Zotonic** there to change them. This setup manages one selected checkout
at a time. Running setup again does not stop a previously selected project;
stop it before switching profiles. To install newer launcher code, rerun
`setup-wsl.cmd` from an updated repository copy.

## 3. Start Zotonic

Open the desktop **Zotonic** folder and double-click **Start Zotonic**.

The launcher starts Docker Desktop when needed, builds the development image,
starts PostgreSQL, and starts Zotonic automatically. Compilation on the first
run can take several minutes. The sources and `_build` remain inside Ubuntu.

The launcher checks the HTTP endpoint inside the Zotonic container. When it is
ready, it opens [the local status site](http://localhost:8000/) in your Windows
browser. No `bin/zotonic start` command is needed. The server continues running
in its container after the launcher window closes.

Use **Logs Zotonic** to see compilation and server output. Closing the log
window does not stop Zotonic. Startup errors stay visible. If readiness times
out, the containers are left available for inspection; use Logs or Stop.

Ports `8000`, `8443`, and the database forwarding port `5432` must be available.
If `5432` is occupied, add `DB_FORWARD_PORT=5433` to the Linux checkout's `.env`
file, preserving any existing settings. Zotonic still connects to `postgres:5432`
inside Docker. Do not change its database host to `localhost`.

If the browser redirects to HTTPS, the development certificate is self-signed.

## 4. Create your development site

Double-click **Shell Zotonic**. This opens a shell **inside the running Zotonic
container**, as the Zotonic user, with the correct configuration paths.
Run:

```bash
bin/zotonic addsite -s blog -H mysite.test mysite
```

The command creates `apps_user/mysite`, initializes its database schema and
starts the site. Save the generated admin password printed in the terminal.

On Windows, open Notepad as Administrator and edit:

```text
C:\Windows\System32\drivers\etc\hosts
```

Select **All files** in the file picker to see `hosts`. Add:

```text
127.0.0.1 mysite.test
```

Open [your site](http://mysite.test:8000/) or
[its admin](http://mysite.test:8000/admin). Log in as `admin` with the generated
password. The status site has separate credentials.

Type `exit` to close the container shell. This leaves the server running.

## 5. Edit files

Install VS Code for Windows with its **WSL extension** and the `code` command
on your Windows PATH. Double-click **Edit Zotonic** to open the Linux checkout
through VS Code's WSL connection.

Double-click **Open Zotonic Folder** to browse the checkout in Explorer. Its
address looks like:

```text
\\wsl.localhost\Ubuntu\home\YOUR_USERNAME\projects\zotonic
```

Keep the working files there rather than copying them back to a Windows drive.
Site templates are under `apps_user/mysite/priv/templates`. Zotonic receives
Linux file-change notifications for the checkout. To explicitly rebuild, open
**Shell Zotonic** and run `make`.

## 6. Stop and return later

Double-click **Stop Zotonic**. It stops and removes the managed project's
containers and network, allowing up to 60 seconds for shutdown. The PostgreSQL
volume, source files, uploaded files and configuration remain saved. Docker
Desktop stays running; it can be quit separately from its system-tray menu.

Next time, double-click **Start Zotonic**. Your site and database remain.

The repository's Windows `start-docker.cmd` and `stop-docker.cmd` also use this
saved WSL setup. They no longer mount the Windows checkout. The standalone
`./start-docker.sh` and `./stop-docker.sh` are for the separate manual container
workflow; use the Windows launchers together for this managed setup.

Avoid `docker compose down -v`: `-v` removes database volumes.

## Moving from the old Windows checkout

First stop the old containers. If using the previous interactive setup, run
`bin/zotonic stop` and `exit` in its container, then run `docker compose down`
from PowerShell in the old Windows checkout. Do not add `-v`.

Keep the old checkout and database volume until migration is complete. Setup
creates a separate Compose project whose name starts with `zotonic-wsl-` and is
based on the distribution and Linux checkout path. Existing Windows project
databases are **not automatically migrated**. Copy your site source files and
transfer the database with PostgreSQL backup/restore; preserve uploaded files
and any needed configuration under `docker-data` as well. Do not copy `_build`
from the old checkout.

Setup does not reset local edits or delete the old project. Moving or renaming
the Linux checkout changes its managed Compose project identity, so keep its
path stable once it contains data.

## Troubleshooting

### Docker is unavailable

The retry prompt waits for you to resolve Docker Desktop startup or integration
errors. Press Enter to check again, or Q to leave. In PowerShell, open the exact
Ubuntu distribution and user selected during setup:

```powershell
wsl -d Ubuntu -u YOUR_USERNAME
```

Then, in Ubuntu:

```bash
docker version
docker compose version
docker info --format '{{.OSType}}'
```

The engine type must be `linux`. A missing `docker` command usually means Ubuntu
WSL integration is disabled. Do not install a second Docker Engine with `apt`.

### User or group ID is zero

Rerun setup and choose the normal Linux user created during Ubuntu installation.
The managed launchers use that user's IDs and ignore stale UID/GID overrides.

### The credential helper fails

For `docker-credential-desktop.exe: Invalid argument`, save your WSL work, quit
Docker Desktop, and run `wsl --shutdown` in PowerShell. This stops all WSL
distributions. Start Docker Desktop and retry Start Zotonic. If it still fails,
check `cmd.exe /c ver` and `command -v docker-credential-desktop.exe` in Ubuntu;
keep the diagnostic output rather than deleting Docker credential settings.

### A script reports `bash\r` or `sh\r`

A script has Windows CRLF endings. Fresh clones made by setup use Linux Git
with `core.autocrlf=false`. If you copied scripts from the old Windows checkout,
change their line endings to LF in VS Code and retry. Git attributes do not
rewrite existing files automatically.

### Validation status

The launcher shell logic and Compose configuration can be tested without
Windows. Full Windows Desktop/WSL integration still needs verification on a
Windows machine.
