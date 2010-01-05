@echo off

setlocal enableextensions 2>nul:
if not %errorlevel% == 0 goto extensions_failure

setlocal enabledelayexpansion 2>nul:
rem if not %errorlevel% == 0 goto delayexpansion_failure
goto tool_tests

:extensions_failure
echo Error: Your command interpreter (cmd.exe) does not support
echo command processor extensions.  This is a requirement for
echo this build script to work.  Please upgrade your command processor.
echo.
pause > NUL:
goto :eof

:delayexpansion_failure
echo Notice: Your command interpreter (cmd.exe) does not support
echo delayed variable expansion.  This is not currently a requirement
echo for this build script, but may be in the future.
echo.

:tool_tests

:test_dcc32
dcc32.exe --version >NUL: 2>NUL:
if %errorlevel% == 0 goto test_libs

:dcc32_not_found
echo Error: Delphi compiler 'dcc32.exe' was not found in PATH.
echo.
echo Please install Delphi.  When installing, Delphi should modify
echo your system path to include the location of this file.
echo.
echo See also:
echo http://www.codegear.com/downloads/free/delphi
echo.
pause > NUL:
goto :eof

:locate_dcc32
if not "%compiler_dir%" == "none" goto :eof
pushd %~f1
if exist dcc32.exe set compiler_dir=found
if "%compiler_dir%" == "found" cd ..
if "%compiler_dir%" == "found" set compiler_dir=%CD%
popd
goto :eof

:test_libs
SET compiler_dir=none
for %%p in ("%PATH:;=" "%") do call :locate_dcc32 %%p
if "%compiler_dir%" == "none" (goto dcc32_not_found) else (goto init)


:init
set start_dir=%CD%
cd ..
set base_dir=%CD%

echo Base directory:          %base_dir%
goto :start

:install
echo Installing component %1, package %2.bpl.
rem
rem    There is a workaround for bug #23225 in IdeBplInstall.exe.
rem    (technically a regression since Delphi 7 worked OK.)
rem
rem    see: http://qc.borland.com/wc/qcmain.aspx?d=23225
rem
del /f /q "%USERPROFILE%\My Documents\Borland Studio Projects\Bpl\%2.bpl" 2>NUL:
"%start_dir%\IdeBplInstall\IdeBplInstall.exe" "%base_dir%\components\%1\build" "%2.bpl" "%1"
set err=%errorlevel%
echo.
goto :eof

:start
echo.

rem Install SynEdit
call :install synedit SynEditD
if not %err% == 0 goto end


rem Install VirtualTreeView
call :install virtualtreeview VirtualTreesD
if not %err% == 0 goto end


rem Install PNGComponents
call :install pngcomponents PngComponentsD
if not %err% == 0 goto end


echo.
echo Finished.
echo.


:end
cd /d "%start_dir%" 2>NUL:
