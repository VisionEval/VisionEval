@echo off

rem get the R_HOME string by starting the desired R GUI
rem then run R.home(). Copy the result here.
if "%R_HOME%" == "" set R_HOME=C:/PROGRA~1/R/R-43~1.2

rem remove --no-save if you might want to save .Rhistory and .Rdata
start %R_HOME%\bin\x64\RGui.exe --no-save
