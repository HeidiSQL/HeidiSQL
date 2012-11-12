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
if "%compiler_dir%" == "none" (goto dcc32_not_found) else (goto test_packages)

:test_packages
set package_dir=none
dcc32.exe --version | find "22.0" >NUL:
if %errorlevel% == 0 set package_dir=delphiXE
if "%package_dir%" == "none" (goto unknown_version) else (goto init)

:unknown_version
echo Error: Unknown version of Delphi compiler 'dcc32.exe'!
echo.
echo At present, only Delphi XE is supported.
echo.
pause >NUL:
goto :eof

:init
set start_time=%DATE% %TIME%
set start_dir=%CD%
cd ..
set base_dir=%CD%
set mad_dir=%base_dir%\..\madCollection
set compiler=dcc32.exe
set params=--no-config
set params=%params% -DDEBUG -GD
set params=%params% -aWinTypes=Windows;WinProcs=Windows;DbiProcs=BDE;DbiTypes=BDE;DbiErrs=BDE
set params=%params% -B
set params=%params% -i"%base_dir%\source"
set params=%params% -r"%base_dir%\components\synedit\resources;%base_dir%\components\virtualtreeview\Resources"
set params=%params% -u"%compiler_dir%\lib\win32\release;%base_dir%\components\virtualtreeview\build;%base_dir%\components\synedit\build;%base_dir%\components\graphicex;%base_dir%\components\synapse;%mad_dir%\madExcept\BDS8;%mad_dir%\madDisAsm\BDS8;%mad_dir%\madBasic\BDS8"
set params=%params% -N0"..\..\build" 
set params=%params% -LE"..\..\build"
set params=%params% -LN"..\..\build"

rem -Q = Quiet compile
rem      This is a workaround for avoiding error D21153
rem      see here: http://qc.borland.com/wc/qcmain.aspx?d=44731
set params=%params% -Q

set clean_only=false

:param_loop
if %0. == . goto param_done
if %0. == --clean. set clean_only=true
shift
goto param_loop

:param_done
echo Base directory:          %base_dir%
echo Compiler directory:      %compiler_dir%
goto start

:start
rem Delete old binaries
echo Cleaning build directories.
cd /d "%base_dir%"
del /S *.dcu
del /S *.dcp
del /S *.bpl
del /S out\heidisql.exe
if %clean_only% == true goto end
goto build

:compile
echo Compiling component %1, package %2.
cd /d "%base_dir%\components\%1\packages\%package_dir%\"
"%compiler%" %params% %2.dpk
set err=%errorlevel%
echo.
goto :eof

:build
echo.

rem Build SynEdit
call :compile synedit SynEditR
if not %err% == 0 goto end
call :compile synedit SynEditD
if not %err% == 0 goto end


rem Build VirtualTreeView
call :compile virtualtreeview VirtualTreesR
if not %err% == 0 goto end
call :compile virtualtreeview VirtualTreesD
if not %err% == 0 goto end


rem Build main executable
echo Compiling main project.
cd /d "%base_dir%\packages\%package_dir%\"
..\..\extra\SetVersion\SetVersion.exe ..\..\res\version.rc
brcc32 ..\..\res\version.rc
brcc32 ..\..\res\icon.rc
brcc32 ..\..\res\manifest.rc
brcc32 ..\..\res\updater.rc
"%compiler%" %params% -e"%base_dir%\out" heidisql.dpr
if not %errorlevel% == 0 goto end

rem Get translation files from Transifex server
cd /d "%base_dir%"
extra\internationalization\tx.exe pull -a
rem Compile .po translation files
for /f %%p IN ('dir /S /B out\locale\*.po') do extra\internationalization\msgfmt.exe -o %%~dpp\%%~np.mo %%p
rem Patch executable with .mo files
rem Must be done before madExcept writes a new crc header, otherwise it will complain about a corrupt .exe
rem See http://tech.dir.groups.yahoo.com/group/dxgettext/message/3623
extra\internationalization\assemble.exe out\heidisql.exe --dxgettext

rem Patch executable with exception handler
cd /d "%base_dir%\packages\%package_dir%\"
"%mad_dir%\madExcept\Tools\madExceptPatch.exe" "%base_dir%\out\heidisql.exe" heidisql.mes

rem Create installer
rem cd /d "%base_dir%\out
"C:\Program Files (x86)\Inno Setup 5\ISCC.exe" /finstaller "%base_dir%\out\heidisql.iss"

echo.
echo Started:   %start_time%
echo Finished:  %DATE% %TIME%
echo.


:end
cd /d "%start_dir%" 2>NUL:
