@echo off

setlocal enableextensions 2>nul:
if not %errorlevel% == 0 goto extensions_failure
goto tool_tests

:extensions_failure
echo Error: Your command interpreter (cmd.exe) does not support
echo command processor extensions.  This is a requirement for
echo this build script to work.  Please upgrade your command processor.
echo.
pause > NUL:
goto :eof

:tool_tests

:test_nsis
makensis.exe /version >NUL: 2>NUL:
if %errorlevel% == 0 goto init

:nsis_not_found
echo Error: NSIS compiler 'makensis.exe' was not found in PATH.
echo.
echo After install NSIS, modify your system path to include the
echo location of this file.
echo.
echo See also:
echo http://nsis.sourceforge.net/
echo.
pause > NUL:
goto :eof

:init
set start_time=%DATE% %TIME%
set start_dir=%CD%
cd portable_launcher
set base_dir=%CD%

set compiler=makensis.exe
rem /v2 = no info, only warnings
set params=/v2

set clean_only=false

:param_loop
if %0. == . goto param_done
if %0. == --clean. set clean_only=true
shift
goto param_loop

:param_done
echo Base directory:          %base_dir%
echo Compiler directory:      %compiler_dir%

:start
rem Delete old binaries
echo Cleaning build directories.
cd /d "%base_dir%"
del /S HeidiSQLPortable.exe
if %clean_only% == true goto end
goto build

:build
echo.

rem Build main executable
echo Compiling main project.
"%compiler%" %params% HeidiSQLPortable.nsi
if not %errorlevel% == 0 goto end

echo.
echo Started:   %start_time%
echo Finished:  %DATE% %TIME%
echo.

:end
cd /d "%start_dir%" 2>NUL:
