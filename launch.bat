@echo off
if "%R_HOME%" == "" set R_HOME=C:/PROGRA~1/R/R-43~1.1
start %R_HOME%\bin\x64\RGui.exe --no-save
