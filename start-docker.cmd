@echo off
setlocal
rem Windows development runs in the Ubuntu checkout configured by setup-wsl.cmd.
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0docker\windows\launcher.ps1" -Action start
set "ZOTONIC_EXIT_CODE=%ERRORLEVEL%"
if not "%ZOTONIC_EXIT_CODE%"=="0" pause
exit /b %ZOTONIC_EXIT_CODE%
