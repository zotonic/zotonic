@echo off
setlocal
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0docker\windows\launcher.ps1" -Action setup
set "ZOTONIC_EXIT_CODE=%ERRORLEVEL%"
echo.
echo Press any key to close this window.
pause >nul
exit /b %ZOTONIC_EXIT_CODE%
