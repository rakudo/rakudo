@ECHO off
FOR /f "delims=" %%I in ('powershell.exe -File "%~dp0set-env-cmd-helper.ps1"') do @%%I