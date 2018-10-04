@echo off
REM This batch file uses Rscript to bootstrap the local installation
Rscript Install-VisionEval.R

REM The rest of what is here is catching the error conditions...
if errorlevel 3 goto NotInstalled
if errorlevel 2 goto InstallAgain
if errorlevel 1 goto UnknownError
echo Configuration completed successfully
echo Double-click VisionEval.Rdata to launch
goto WaitEnd

:UnknownError
echo Error level 1! You know what THAT means!

:InstallAgain
echo The R install script was unable to run. Try unzipping the installer again.
goto WaitEnd

:NotInstalled
echo R appears not to be installed...
goto WaitEnd

:WaitEnd
pause Press any key to close...
