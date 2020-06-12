@echo off

setlocal EnableExtensions
setlocal EnableDelayedExpansion

set R_VERSION=%1
if [%R_VERSION%] == [] set R_VERSION=3.6.1
set ROOT_KEY=\Software\R-core\R\%R_VERSION%
set MACHINE_KEY="HKLM%ROOT_KEY%"
set USER_KEY="HKCU%ROOT_KEY%"
set VALUE_NAME=InstallPath
if NOT [!R_BASE_USER!] == [] (
	echo R Version from filesystem via R_BASE_USER 1>&2
	set R_HOME=!R_BASE_USER!\R-%R_VERSION%
	if NOT EXIST !R_HOME! (
		set "R_HOME="
		echo R %R_VERSION% not found under !R_BASE_USER! 1>&2
	) else (
		echo R_HOME = !R_HOME! 1>&2
	)
)
if [!R_HOME!] == [] (
	for /f "usebackq skip=2 tokens=1-2*" %%i in (`reg query %MACHINE_KEY% /v %VALUE_NAME% 2^>nul`) do set R_HOME=%%k
)
if [!R_HOME!] == [] (
	echo R %R_VERSION% not found in machine; searching %USER_KEY% 1>&2
	for /f "usebackq skip=2 tokens=1-2*" %%x in (`reg query %USER_KEY% /v %VALUE_NAME% 2^>nul`) do set R_HOME=%%z
)
if [!R_HOME!] == [] (
	echo R Version %R_VERSION% not found in machine or user 1>&2
	start "" https://cran.r-project.org/bin/windows/base/old/%R_VERSION%
	EXIT /B 1
) else (
	echo !R_HOME!\bin\Rscript.exe
	set RSCRIPT=!R_HOME!\bin\Rscript.exe
	"!RSCRIPT!" --version 1>&2
	EXIT /B 0
)
