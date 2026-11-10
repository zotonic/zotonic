@echo off
setlocal
rem Run from this checkout, including when launched from Windows Explorer.
pushd "%~dp0"
if errorlevel 1 goto directory_error

where docker >nul 2>&1
if errorlevel 1 (
    echo Docker was not found. Install Docker Desktop for Windows and try again.
    goto failed
)

docker compose version
if errorlevel 1 (
    echo Docker Compose is unavailable. Check your Docker Desktop installation.
    goto failed
)

rem Zotonic needs the Linux container engine, not Windows containers.
docker info --format "{{.OSType}}" | findstr /x "linux" >nul
if errorlevel 1 (
    echo.
    echo Zotonic has not started: Docker's Linux engine check failed.
    echo Docker diagnostic information follows:
    docker info
    echo.
    echo Open Docker Desktop from the Windows Start menu.
    echo Resolve any startup error shown there and wait until the engine is running.
    echo Then close this window and double-click start-docker.cmd again.
    echo This launcher does not wait for Docker or retry automatically.
    goto failed
)

rem Windows has no Unix UID/GID. Preserve explicit overrides when supplied.
if not defined ZOTONIC_UID set "ZOTONIC_UID=1000"
if not defined ZOTONIC_GID set "ZOTONIC_GID=1000"

echo.
echo Building the development container. The first build can take several minutes.
echo At the container prompt, run: bin/zotonic start
echo To finish, run: bin/zotonic stop, then exit
echo.
docker compose -f docker-compose.yml run --build --service-ports zotonic sh
set "ZOTONIC_EXIT_CODE=%ERRORLEVEL%"
if not "%ZOTONIC_EXIT_CODE%"=="0" echo Docker exited with code %ZOTONIC_EXIT_CODE%.
goto finished

:failed
set "ZOTONIC_EXIT_CODE=1"
:finished
popd
echo.
echo Press any key to close this launcher.
pause >nul
exit /b %ZOTONIC_EXIT_CODE%

:directory_error
echo Could not open the Zotonic checkout directory.
pause
exit /b 1
