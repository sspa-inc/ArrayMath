@echo off
setlocal
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp002_autotest.ps1" %*
exit /b %ERRORLEVEL%
