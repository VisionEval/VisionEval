@echo off
if "%R_HOME%" == "" set R_HOME=%userprofile%/AppData/Local/Programs/R/R-4.2.3
start %R_HOME%\bin\x64\RGui.exe --no-save
