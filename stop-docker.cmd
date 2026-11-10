@echo off
setlocal
rem Stop this checkout's project, preserving database volumes and source files.
pushd "%~dp0"
if errorlevel 1 goto directory_error

echo Stopping Zotonic containers. Database data will be kept.
docker compose -f docker-compose.yml down --remove-orphans --timeout 30
set "ZOTONIC_EXIT_CODE=%ERRORLEVEL%"
if not "%ZOTONIC_EXIT_CODE%"=="0" echo Docker stop failed with code %ZOTONIC_EXIT_CODE%.
popd
echo.
echo Press any key to close this launcher.
pause >nul
exit /b %ZOTONIC_EXIT_CODE%

:directory_error
echo Could not open the Zotonic checkout directory.
pause
exit /b 1
