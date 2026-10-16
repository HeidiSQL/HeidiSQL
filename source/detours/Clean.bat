rem *****************************************
rem *        Delphi CleanUp Batch.          *
rem *                                       *
rem * Clean identcache,local,dcu,exe,       *
rem * map,drc files.                        *
rem * Clean hidden __history folder.        *
rem *                                       *
rem *        Author: Mahdi Safsafi          *
rem *****************************************

@echo off
Setlocal EnableDelayedExpansion

Del "*.identcache" /s/q
Del "*.local" /s/q
Del "*.dcu" /s/q
Del "*.exe" /s/q
Del "*.drc" /s/q
Del "*.map" /s/q

set mustdel=false
For /r %%f in (.) do (
    set "mustdel=false"
	if %%~nf==Win32 (
		if exist "%%~ff\Debug\" set "mustdel=true"
		if exist "%%~ff\Release\" set "mustdel=true"
) else if %%~nf==Win64 (
		if exist "%%~ff\Debug\" set "mustdel=true"
		if exist "%%~ff\Release\" set "mustdel=true" 
		)
if %%~nf==__history set "mustdel=true"
if !mustdel!==true (
	if exist "%%~ff" rd /s/q "%%~ff"
        )
)